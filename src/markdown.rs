mod list;
use crate::comment::{hide_sharp_behind_comment, trim_custom_comment_prefix, CodeBlockAttribute};
use crate::Config;
use itertools::Itertools;
use list::{ListMarker, OrderedListMarker, UnorderedListMarker};
use pulldown_cmark::escape::StrWrite;
use pulldown_cmark::{
    CodeBlockKind, Event, HeadingLevel, LinkDef, LinkType, OffsetIter, Options, Parser, Tag,
};
use std::borrow::Cow;
use std::iter::Peekable;
use std::ops::Range;

/// Rewrite markdown input.
///
/// The main goal of this function is to reformat rust code blocks in markdown text. However, there
/// will also be some light reformatting of other markdown items outside of code blocks like
/// adjusting the number of newlines after headings, paragraphs, tables, lists, blockquotes, etc.
///
/// **Note:** The content of indented codeblocks will not be formatted, but indentation may change.
pub(crate) fn rewrite_markdown<'a, 'c>(input: &'a str, config: &'c Config) -> Cow<'a, str> {
    let formatter = MarkdownFormatter::new(input, config);
    match formatter.format() {
        Ok(text) => text.into(),
        Err(_) => input.into(),
    }
}

struct MarkdownFormatter<'i, 'c> {
    /// Raw markdown input
    input: &'i str,
    /// rustfmt config
    config: &'c Config,
    /// Keep track of whether the last Event was an HTML snippet
    was_last_html: bool,
    /// Iterator Supplying Markdown Events
    events: Peekable<OffsetIter<'i, 'i>>,
    rewrite_buffer: String,
    /// Stores code that we might try to format
    code_block_buffer: String,
    /// The current list marker when formatting an ordered or unorederd list.
    /// For unordered lists this is one of `*`, `+`, or `-`,
    /// while for ordered lists this is a number from 0-9 char integer followed by a `.` or `)`.
    list_marker: ListMarker,
    /// Stack that keeps track of indentation.
    indentation: Vec<Cow<'static, str>>,
    /// Stack that keeps track of whether we're formatting inside of another element.
    nested_context: Vec<Tag<'i>>,
    /// A set of reference link definitions that will be output after formatting.
    /// Reference style links contain 3 parts:
    /// 1. Text to display
    /// 2. URL
    /// 3. (Optional) Title
    /// ```markdown
    /// [title]: link "optional title"
    /// ```
    reference_links: Vec<(String, String, Option<String>)>,
    /// keep track of a header written with an underline.
    /// ```markdown
    /// Header
    /// ======
    /// ```
    underline_header: Option<&'i str>,
    /// next Start event should push indentation
    needs_indent: bool,
}

impl<'i, 'c> MarkdownFormatter<'i, 'c> {
    fn new(input: &'i str, config: &'c Config) -> Self {
        let mut options = Options::all();
        options.remove(Options::ENABLE_SMART_PUNCTUATION);

        let parser = Parser::new_ext(input, options);

        let reference_links = parser
            .reference_definitions()
            .iter()
            .sorted_by(|(_, link_a), (_, link_b)| link_a.span.start.cmp(&link_b.span.start))
            .map(|(link_lable, LinkDef { dest, title, .. })| {
                (
                    link_lable.to_string(),
                    dest.to_string(),
                    title.clone().map(|s| s.to_string()),
                )
            })
            .collect::<Vec<_>>();

        Self {
            input,
            config,
            was_last_html: false,
            events: parser.into_offset_iter().peekable(),
            rewrite_buffer: String::with_capacity(input.len() * 2),
            code_block_buffer: String::with_capacity(256),
            list_marker: ListMarker::Unordered(UnorderedListMarker::Asterisk),
            indentation: vec![],
            nested_context: vec![],
            reference_links,
            underline_header: None,
            needs_indent: false,
        }
    }

    /// Peek at the next Markdown Event
    fn peek(&mut self) -> Option<&Event<'i>> {
        self.events.peek().map(|(e, _)| e)
    }

    /// Check if the next Event is an `Event::End`
    fn is_next_end_event(&mut self) -> bool {
        matches!(self.peek(), Some(Event::End(_)))
    }

    /// Check if we should write newlines and indentation before the next Start Event
    fn check_needs_indent(&mut self) {
        if let Some(Event::Start(_)) = self.peek() {
            self.needs_indent = true
        }
    }

    /// Check if we're formatting a fenced code block
    fn in_fenced_code_block(&self) -> bool {
        matches!(
            self.nested_context.last(),
            Some(Tag::CodeBlock(CodeBlockKind::Fenced(_)))
        )
    }

    /// Check if we're formatting an indented code block
    fn in_indented_code_block(&self) -> bool {
        matches!(
            self.nested_context.last(),
            Some(Tag::CodeBlock(CodeBlockKind::Indented))
        )
    }

    /// Check if we're formatting in a nested context
    fn is_nested(&self) -> bool {
        !self.nested_context.is_empty()
    }

    /// Write out reference links at the end of the file
    fn rewrite_reference_links(mut self) -> std::io::Result<String> {
        // use std::mem::take to work around the borrow checker
        let reference_links = std::mem::take(&mut self.reference_links);

        if !reference_links.is_empty() {
            self.write_newlines(2)?;
        }

        for (label, dest, title) in reference_links {
            self.write_newlines(1)?;
            if let Some(title) = title {
                write!(self.rewrite_buffer, r#"[{label}]: {dest} "{title}""#)?;
            } else {
                write!(self.rewrite_buffer, "[{label}]: {dest}")?;
            }
        }
        Ok(self.rewrite_buffer)
    }

    fn write_indentation(&mut self, use_rewrite_buffer: bool, trim_trailing_whiltespace: bool) {
        let last_non_complete_whitespace_indent = self
            .indentation
            .iter()
            .rposition(|indent| !indent.chars().all(char::is_whitespace));

        let position = if trim_trailing_whiltespace {
            let Some(position) = last_non_complete_whitespace_indent else {
                // All indents are just whitespace. We don't want to push blank lines
                return;
            };
            position
        } else {
            self.indentation.len()
        };

        for (i, indent) in self.indentation.iter().take(position + 1).enumerate() {
            let is_last = i == position;
            let buffer = if use_rewrite_buffer {
                &mut self.rewrite_buffer
            } else {
                &mut self.rewrite_buffer
            };

            if is_last && trim_trailing_whiltespace {
                buffer.push_str(indent.trim())
            } else {
                buffer.push_str(&indent)
            }
        }
    }

    fn write_newlines(&mut self, max_newlines: usize) -> std::io::Result<()> {
        if self.rewrite_buffer.is_empty() {
            return Ok(());
        }
        let newlines = self
            .rewrite_buffer
            .chars()
            .rev()
            .take_while(|c| *c == '\n')
            .count();

        let nested = self.is_nested();
        let newlines_to_write = max_newlines.saturating_sub(newlines);
        let next_is_end_event = self.is_next_end_event();

        for i in 0..newlines_to_write {
            let is_last = i == newlines_to_write - 1;

            self.rewrite_buffer.push('\n');

            if nested {
                self.write_indentation(true, !is_last);
            }
        }
        if !nested {
            self.write_indentation(true, next_is_end_event);
        }
        Ok(())
    }

    fn flush_and_format_code_buffer(&mut self, info_string: &str) -> std::io::Result<()> {
        let empty_buffer = self.code_block_buffer.trim().is_empty();
        let is_formattable_code = CodeBlockAttribute::new(info_string).is_formattable_rust();

        // use std::mem::take to work around the borrow checker
        let code_block_buffer = std::mem::take(&mut self.code_block_buffer);

        let code = if !empty_buffer && is_formattable_code {
            // First, comment out hidden rustdoc lines as they would prevent us from properly
            // parsing and formatting the code snippet.
            let with_hidden_rustdoc_lines = code_block_buffer
                .lines()
                .map(|line| hide_sharp_behind_comment(line))
                .join("\n");

            if let Some(formatted) =
                crate::format_code_block(&with_hidden_rustdoc_lines, &self.config, false)
            {
                trim_custom_comment_prefix(&formatted.snippet)
            } else {
                trim_custom_comment_prefix(&code_block_buffer)
            }
        } else {
            code_block_buffer
        };

        // join with indentation lines
        for line in code.trim().lines() {
            self.rewrite_buffer.push('\n');
            self.write_indentation(true, line.is_empty());
            self.rewrite_buffer.push_str(line)
        }

        Ok(())
    }

    fn format(mut self) -> std::io::Result<String> {
        while let Some((event, range)) = self.events.next() {
            let is_current_html = matches!(&event, Event::Html(_));
            match event {
                Event::Start(tag) => {
                    self.start_tag(tag, range)?;
                }
                Event::End(tag) => {
                    self.end_tag(tag.clone(), range)?;
                }
                Event::Text(text) => {
                    if self.in_fenced_code_block() {
                        self.code_block_buffer.push_str(&text);
                        continue;
                    }

                    if self.in_indented_code_block() {
                        self.write_indentation(true, false);
                        self.rewrite_buffer.push_str(&text);
                        continue;
                    }

                    if self.input[..range.start].ends_with('\\') {
                        // recover escape characters
                        self.rewrite_buffer.push('\\');
                    }
                    self.rewrite_buffer.write_str(&text)?;
                }
                Event::Code(text) => {
                    let code_marker = find_marker(self.input, &range, |c| c != '`');
                    let fs = if text.starts_with('`') { " " } else { "" };
                    let bs = if text.ends_with('`') { " " } else { "" };
                    write!(
                        self.rewrite_buffer,
                        "{code_marker}{fs}{text}{bs}{code_marker}"
                    )?;
                }
                Event::SoftBreak => {
                    self.rewrite_buffer.write_str("\n")?;
                    self.write_indentation(true, false);
                }
                Event::HardBreak => {
                    self.rewrite_buffer.write_str("  ")?;
                }
                Event::Html(_text) => {
                    if !self.was_last_html && !self.is_nested() {
                        self.write_newlines(2)?;
                    }
                    write!(self.rewrite_buffer, "{}", &self.input[range])?;
                    self.check_needs_indent();
                }
                Event::Rule => {
                    // TODO(ytmimi) support space characters. See these examples:
                    // https://github.github.com/gfm/#example-17
                    // https://github.github.com/gfm/#example-21
                    self.write_newlines(2)?;
                    rewrite_marker(self.input, &range, &mut self.rewrite_buffer)?;
                    self.needs_indent = true;
                }
                Event::FootnoteReference(text) => {
                    write!(self.rewrite_buffer, "[^{text}]")?;
                }
                Event::TaskListMarker(done) => {
                    if done {
                        write!(self.rewrite_buffer, "[x]")?;
                    } else {
                        write!(self.rewrite_buffer, "[ ]")?;
                    }
                }
            }
            self.was_last_html = is_current_html;
        }
        debug_assert!(self.nested_context.is_empty());
        self.rewrite_reference_links()
    }

    fn start_tag(&mut self, tag: Tag<'i>, range: Range<usize>) -> std::io::Result<()> {
        use Tag::*;
        match tag {
            Paragraph => {
                if self.needs_indent {
                    self.write_newlines(2)?;
                    self.needs_indent = false
                }
                self.nested_context.push(tag);
            }
            Heading(level, _, _) => {
                if !self.is_nested() {
                    self.write_newlines(2)?;
                }
                let full_header = self.input[range].trim();

                if full_header.contains('\n') && full_header.ends_with(['=', '-']) {
                    // support for alternative syntax for H1 and H2
                    // <https://www.markdownguide.org/basic-syntax/#alternate-syntax>
                    let header_marker = full_header.split('\n').last().unwrap().trim();
                    self.underline_header.replace(header_marker);
                    // underlined headers are handled in `end_tag`
                    return Ok(());
                }

                let header = match level {
                    HeadingLevel::H1 => "# ",
                    HeadingLevel::H2 => "## ",
                    HeadingLevel::H3 => "### ",
                    HeadingLevel::H4 => "#### ",
                    HeadingLevel::H5 => "##### ",
                    HeadingLevel::H6 => "###### ",
                };

                let empty_header = full_header
                    .trim_start_matches(header)
                    .trim_end_matches(|c: char| c.is_whitespace() || matches!(c, '#' | '\\'))
                    .trim()
                    .is_empty();

                if empty_header {
                    write!(self.rewrite_buffer, "{}", header.trim())?;
                } else {
                    write!(self.rewrite_buffer, "{header}")?;
                }
            }
            BlockQuote => {
                if self.needs_indent {
                    self.write_newlines(2)?;
                    self.needs_indent = false
                }
                self.nested_context.push(tag);
                self.indentation.push("> ".into());

                if matches!(self.peek(), Some(Event::End(BlockQuote))) {
                    // Special case handling for empty block quotes
                    self.rewrite_buffer.push('>')
                } else {
                    self.rewrite_buffer.push_str("> ")
                }
            }
            CodeBlock(ref kind) => {
                if self.needs_indent {
                    self.write_newlines(2)?;
                    self.needs_indent = false
                }
                match kind {
                    CodeBlockKind::Fenced(info_sting) => {
                        rewrite_marker(self.input, &range, &mut self.rewrite_buffer)?;
                        self.rewrite_buffer.push_str(&info_sting);
                    }
                    CodeBlockKind::Indented => {
                        // TODO(ytmimi) support tab as an indent
                        self.indentation.push("    ".into());
                    }
                }
                self.nested_context.push(tag);
            }
            List(value) => {
                if self.needs_indent {
                    self.write_newlines(2)?;
                    self.needs_indent = false
                }

                let list_marker = if let Some(number) = value {
                    let marker = match self.input[range]
                        .chars()
                        .find(|c| ['.', ')'].contains(c))
                        .expect("we should find ordered list markers")
                    {
                        '.' => OrderedListMarker::Period,
                        ')' => OrderedListMarker::Parenthesis,
                        _ => unreachable!(),
                    };
                    ListMarker::Ordered {
                        number: number as usize,
                        marker,
                    }
                } else {
                    let marker = match self.input[range]
                        .chars()
                        .find(|c| ['*', '+', '-'].contains(c))
                        .expect("we should find unorder list marker")
                    {
                        '*' => UnorderedListMarker::Asterisk,
                        '+' => UnorderedListMarker::Plus,
                        '-' => UnorderedListMarker::Hyphen,
                        _ => unreachable!(),
                    };
                    ListMarker::Unordered(marker)
                };

                self.list_marker = list_marker;
                self.nested_context.push(tag);
            }
            Item => {
                if self.needs_indent {
                    self.write_newlines(1)?;
                    self.needs_indent = false
                }

                let empty_list_item = matches!(self.peek(), Some(Event::End(Item)));
                match &self.list_marker {
                    ListMarker::Ordered { number, marker } if empty_list_item => {
                        write!(
                            self.rewrite_buffer,
                            "{number}{}",
                            <&OrderedListMarker as Into<char>>::into(marker)
                        )?;
                    }
                    ListMarker::Ordered { number, marker } => {
                        write!(
                            self.rewrite_buffer,
                            "{number}{} ",
                            <&OrderedListMarker as Into<char>>::into(marker)
                        )?;
                    }
                    ListMarker::Unordered(marker) if empty_list_item => {
                        write!(
                            self.rewrite_buffer,
                            "{}",
                            <&UnorderedListMarker as Into<char>>::into(marker)
                        )?;
                    }
                    ListMarker::Unordered(marker) => {
                        write!(
                            self.rewrite_buffer,
                            "{} ",
                            <&UnorderedListMarker as Into<char>>::into(marker)
                        )?;
                    }
                }

                self.nested_context.push(tag);
                self.list_marker.increment_count();
                self.indentation.push(self.list_marker.indentation());
            }
            FootnoteDefinition(label) => {
                write!(self.rewrite_buffer, "[^{label}]: ")?;
            }
            Emphasis => {
                rewrite_marker_with_limit(self.input, &range, &mut self.rewrite_buffer, Some(1))?;
            }
            Strong => {
                rewrite_marker_with_limit(self.input, &range, &mut self.rewrite_buffer, Some(2))?;
            }
            Strikethrough => {
                rewrite_marker(self.input, &range, &mut self.rewrite_buffer)?;
            }
            Link(link_type, ..) => {
                let email_or_auto = matches!(link_type, LinkType::Email | LinkType::Autolink);
                let opener = if email_or_auto { '<' } else { '[' };
                self.rewrite_buffer.push(opener);
            }
            Image(..) => {
                write!(self.rewrite_buffer, "![")?;
            }
            _ => {
                todo!("Table formatting is not yet implemented");
                // handle tables
            }
        }
        Ok(())
    }

    fn end_tag(&mut self, tag: Tag<'i>, range: Range<usize>) -> std::io::Result<()> {
        use Tag::*;
        match tag {
            Paragraph => {
                let popped_tag = self.nested_context.pop();
                debug_assert_eq!(popped_tag, Some(tag));
            }
            Heading(_, fragment_identifier, classes) => {
                match (fragment_identifier, classes.is_empty()) {
                    (Some(id), false) => {
                        let classes = rewirte_header_classes(classes)?;
                        write!(self.rewrite_buffer, " {{#{id}{classes}}}")?;
                    }
                    (Some(id), true) => {
                        write!(self.rewrite_buffer, " {{#{id}}}")?;
                    }
                    (None, false) => {
                        let classes = rewirte_header_classes(classes)?;
                        write!(self.rewrite_buffer, " {{{}}}", classes.trim())?;
                    }
                    (None, true) => {}
                }

                if let Some(marker) = self.underline_header.take() {
                    write!(self.rewrite_buffer, "\n{marker}")?;
                }
            }
            BlockQuote => {
                let popped_tag = self.nested_context.pop();
                debug_assert_eq!(popped_tag, Some(tag));

                let popped_indentation = self
                    .indentation
                    .pop()
                    .expect("we pushed a blockquote marker in start_tag");
                if let Some(indentation) = self.indentation.last_mut() {
                    if indentation == ">" {
                        *indentation = popped_indentation
                    }
                }
            }
            CodeBlock(ref kind) => {
                match kind {
                    CodeBlockKind::Fenced(info_string) => {
                        self.flush_and_format_code_buffer(info_string)?;
                        // write closing code fence
                        self.write_newlines(1)?;
                        rewrite_marker(self.input, &range, &mut self.rewrite_buffer)?;
                    }
                    CodeBlockKind::Indented => {
                        // Maybe we'll consider formatting indented code blocks??

                        let popped_indentation = self
                            .indentation
                            .pop()
                            .expect("we added 4 spaces in start_tag");
                        debug_assert_eq!(popped_indentation, "    ");
                    }
                }
                let popped_tag = self.nested_context.pop();
                debug_assert_eq!(popped_tag, Some(tag));
            }
            List(_) => {
                let popped_tag = self.nested_context.pop();
                debug_assert_eq!(popped_tag, Some(tag));
            }
            Item => {
                let popped_tag = self.nested_context.pop();
                debug_assert_eq!(popped_tag, Some(tag));
                let popped_indentation = self
                    .indentation
                    .pop()
                    .expect("we list item indentation in start_tag");
                debug_assert_eq!(popped_indentation, self.list_marker.indentation());

                // if the next event is a Start(Item), then we need to set needs_indent
                self.needs_indent = matches!(self.peek(), Some(Event::Start(Item)));
            }
            FootnoteDefinition(_label) => {}
            Emphasis => {
                rewrite_marker_with_limit(self.input, &range, &mut self.rewrite_buffer, Some(1))?;
            }
            Strong => {
                rewrite_marker_with_limit(self.input, &range, &mut self.rewrite_buffer, Some(2))?;
            }
            Strikethrough => {
                rewrite_marker(self.input, &range, &mut self.rewrite_buffer)?;
            }
            Link(link_type, url, _) => {
                use LinkType::*;
                match link_type {
                    Inline => write!(self.rewrite_buffer, "]({url})")?,
                    Reference | ReferenceUnknown => write!(self.rewrite_buffer, "][{url}]")?,
                    Collapsed | CollapsedUnknown => write!(self.rewrite_buffer, "][]")?,
                    Shortcut | ShortcutUnknown => write!(self.rewrite_buffer, "]")?,
                    Autolink | Email => write!(self.rewrite_buffer, ">")?,
                }
            }
            Image(_, url, title) => {
                if title.is_empty() {
                    write!(self.rewrite_buffer, "]({url})")?;
                } else {
                    write!(self.rewrite_buffer, "]({url} \"{}\")", title.trim())?;
                }
            }
            _ => {
                todo!("Table formatting is not yet implemented");
                // handle tables
            }
        }
        self.check_needs_indent();
        Ok(())
    }
}

/// Find some marker that denotes the start of a markdown construct.
/// for example, `**` for bold or `_` for italics.
fn find_marker<'i, P>(input: &'i str, range: &Range<usize>, predicate: P) -> &'i str
where
    P: FnMut(char) -> bool,
{
    let end = if let Some(position) = input[range.start..].chars().position(predicate) {
        range.start + position
    } else {
        range.end
    };
    &input[range.start..end]
}

/// Find some marker, but limit the size
fn rewrite_marker_with_limit<'i>(
    input: &'i str,
    range: &Range<usize>,
    buffer: &mut String,
    size_limit: Option<usize>,
) -> std::io::Result<()> {
    let marker_char = input[range.start..].chars().next().unwrap();
    let marker = find_marker(input, &range, |c| c != marker_char);
    if let Some(mark_max_width) = size_limit {
        write!(buffer, "{}", &marker[..mark_max_width])
    } else {
        write!(buffer, "{marker}")
    }
}

/// Finds a marker in the source text and writes it to the buffer
fn rewrite_marker<'i>(
    input: &'i str,
    range: &Range<usize>,
    buffer: &mut String,
) -> std::io::Result<()> {
    rewrite_marker_with_limit(input, range, buffer, None)
}

/// Rewrite a list of h1, h2, h3, h4, h5, h6 classes
fn rewirte_header_classes(classes: Vec<&str>) -> std::io::Result<String> {
    let item_len = classes.iter().map(|i| i.len()).sum::<usize>();
    let capacity = item_len + classes.len() * 2;
    let mut result = String::with_capacity(capacity);
    for class in classes {
        write!(result, " .{class}")?;
    }
    Ok(result)
}

#[cfg(test)]
mod test {
    use super::rewrite_markdown;

    macro_rules! test {
        ($input:expr) => {
            test!($input, $input)
        };
        ($input:expr, $output:expr) => {
            let config = crate::Config::default();
            let formatted = super::rewrite_markdown($input, &config);
            assert_eq!(formatted, $output);
        };
    }

    mod rule {
        #[test]
        fn test_line_breaks_asterisk() {
            test!("***");
            test!("*******");
            test!("************");
            test!("hello ************");
            test!("hello\n************", "hello\n\n************");
            test!("hello\n\n************");
            test!("hello\n\n\n************", "hello\n\n************");
        }

        #[test]
        fn test_line_break_dash() {
            test!("---");
            test!("-------");
            test!("------------");
            test!("hello ------------");
            test!("hello\n\n------------");
            test!("hello\n\n\n------------", "hello\n\n------------");
        }

        #[test]
        fn test_line_break_underscore() {
            test!("___");
            test!("_______");
            test!("____________");
            test!("hello ____________");
            test!("hello\n____________", "hello\n\n____________");
            test!("hello\n\n____________");
            test!("hello\n\n\n____________", "hello\n\n____________");
        }
    }

    mod code {
        #[test]
        fn inline_code() {
            test!("`hello world!`");
        }

        #[test]
        fn multi_tick_inline_code() {
            test!("``hello world!``");
        }

        #[test]
        fn nested_inline_code() {
            test!("`` `hello world!` ``");
        }
    }

    mod header {
        #[test]
        fn heading() {
            test!("# Heading");
            test!("## Heading");
            test!("### Heading");
            test!("#### Heading");
            test!("##### Heading");
            test!("###### Heading");
            test!("# foo *bar* \\*baz\\*");
            test!("#                  foo                     ", "# foo");
            test!(" ### foo", "### foo");
            test!("  ### foo", "### foo");
            test!("   ### foo", "### foo");
            test!("## foo ##", "## foo");
            test!("# foo ##################################", "# foo");
            test!("##### foo ##", "##### foo");
            test!("### foo ###     ", "### foo");
            test!("### foo ### b");
            test!("# foo#");
            test!("### foo \\###");
            test!("## foo #\\##");
            test!("# foo \\#");
            test!("## ", "##");
            test!("#", "#");
            test!("### ###", "###");
        }

        #[test]
        fn h1_h2_alternative_heading_t() {
            test!("Heading\n=");
            test!("Heading\n===");
            test!("Heading\n-------");
            test!("Heading\n-----");
            test!("Foo *bar*\n=========");
            test!("Foo *bar*\n---------");
            test!("Foo *bar\nbaz*\n====");
            test!("  Foo *bar\nbaz*  \n====", "Foo *bar\nbaz*\n====");
            test!("   Foo\n---", "Foo\n---");
            test!("  Foo\n-----", "Foo\n-----");
            test!("  Foo\n  -----", "Foo\n-----");
            test!("Foo\n   ----      ", "Foo\n----");
            test!("Foo  \n-----", "Foo\n-----");
            test!("Foo\\----");
            test!("\\> foo\n------");

            // This test case is resembles those `test_line_break_dash`
            // and it's correctly parsed as an h2 header instead of a line break
            test!("hello\n------------");
        }

        #[test]
        fn with_fragment_id_and_classes() {
            test!("## Heading {#heading-id}");
            test!("## Heading {.class1}");
            test!("## Heading {.class1 .class2}");
            test!("## Heading {#heading-id .class1 .class2}");
        }

        #[test]
        fn h1_h2_alternative_heading_with_fragment_id_and_classes() {
            test!("Heading {#heading-id}\n=");
            test!("Heading {.class1}\n===");
            test!("Heading {.class1 .class2}\n-------");
            test!("Heading {#heading-id .class1 .class2}\n-----");
        }
    }

    mod paragraph {
        #[test]
        fn simple_paragraph() {
            test!("hello world!");
            test!("hello world\\!");
            test!("\\hello world!");
        }

        #[test]
        fn multi_line_paragraph() {
            test!("hello\nworld!");
        }

        #[test]
        fn multiple_paragraph() {
            test!("hello\n\nworld!");
        }
    }

    mod emphasis {
        #[test]
        fn bold() {
            // examples from https://www.markdownguide.org/basic-syntax/#bold
            test!("I just love **bold text**.");
            test!("I just love __bold text__.");
            test!("Love**is**bold");
        }

        #[test]
        fn italic() {
            // examples from https://www.markdownguide.org/basic-syntax/#italic
            test!("Italicized text is the *cat's meow*.");
            test!("Italicized text is the _cat's meow_.");
            test!("A*cat*meow");
        }

        #[test]
        fn bold_and_italic() {
            // examples from https://www.markdownguide.org/basic-syntax/#bold-and-italic
            test!("This text is ***really important***.");
            test!("This text is ___really important___.");
            test!("This text is __*really important*__.");
            test!("This text is **_really important_**.");
            test!("This is really***very***important text.");
        }

        #[test]
        fn strike_through() {
            // example from https://github.github.com/gfm/#strikethrough-extension-
            test!("~~Hi~~ Hello, ~there~ world!");
            test!("This will ~~~not~~~ strike.");
        }
    }

    mod html {
        #[test]
        fn single_line() {
            test!("<br>");
            test!("<hr />");
            test!("<p>hello world!</p>");
            test!("<ol><li>hello</li><li>world</li></ol>");
        }

        #[test]
        fn multi_line() {
            test!("<ol>\n<li>hello</li>\n<li>world!</li>\n</ol>");
            test!("<ol>\n<li>**hello**</li>\n<li>**world!**</li>\n</ol>");
        }

        #[ignore]
        #[test]
        fn github_flavored_markdown_examples() {
            test!(
                r"<table><tr><td>
<pre>
**Hello**,

_world_.
</pre>
</td></tr></table>
"
            );
        }
    }

    mod link {
        #[test]
        fn inline_link() {
            test!("[foo](bar)");
            test!("[`foo`](bar)");
        }

        #[test]
        fn reference_link() {
            test!("[foo][bar]");
            test!("[`foo`][bar]");
        }

        #[test]
        fn collapsed_link() {
            test!("[foo][]");
            test!("[`foo`][]");
        }

        #[test]
        fn shortcut_link() {
            test!("[foo]");
            test!("[`foo`]");
        }

        #[test]
        fn autolink_link() {
            test!("<http://foo.bar/baz>");
            test!("<`http://foo.bar/baz`>");
        }

        #[test]
        fn email_link() {
            test!("<john@example.org>");
            test!("<`john@example.org`>");
        }
    }

    mod image {
        #[test]
        fn without_title() {
            test!("![alt text](Isolated.png)");
            test!("![`alt text`](Isolated.png)");
            test!("![image info](./pictures/image.png)");
            test!("![`image info`](./pictures/image.png)");
            test!("![image](files://C:/Users/username/Desktop/Isolated.png)");
            test!("![`image`](files://C:/Users/username/Desktop/Isolated.png)");
        }

        #[test]
        fn with_title() {
            test!(r#"![alt text](Isolated.png "Title")"#);
            test!(
                r#"![alt text](Isolated.png "  Title")"#,
                r#"![alt text](Isolated.png "Title")"#
            );
        }

        #[test]
        fn github_flavored_markdown_examples() {
            // https://github.github.com/gfm/#example-581
            test!(r#"![foo](/url "title")"#);

            // https://github.github.com/gfm/#example-582
            test!(r#"![foo *bar*]"#);
            test!(r#"[foo *bar*]: train.jpg "train & tracks""#);

            // https://github.github.com/gfm/#example-583
            test!("![foo ![bar](/url)](/url2)");

            // https://github.github.com/gfm/#example-584
            test!("![foo [bar](/url)](/url2)");

            // https://github.github.com/gfm/#example-585
            test!("![foo *bar*][]");
            test!(r#"[foo *bar*]: train.jpg "train & tracks""#);

            // https://github.github.com/gfm/#example-586
            test!("![foo *bar*][foobar]");
            test!(r#"[FOOBAR]: train.jpg "train & tracks""#);

            // https://github.github.com/gfm/#example-587
            test!("![foo](train.jpg)");

            // https://github.github.com/gfm/#example-588
            test!(
                r#"My ![foo bar](/path/to/train.jpg  "title"   )"#,
                r#"My ![foo bar](/path/to/train.jpg "title")"#
            );

            // https://github.github.com/gfm/#example-589
            test!("![foo](<url>)", "![foo](url)");

            // https://github.github.com/gfm/#example-590
            test!("![](/url)");

            // https://github.github.com/gfm/#example-591
            test!("![foo][bar]");
            test!("[bar]: /url");

            // https://github.github.com/gfm/#example-592
            test!("![foo][bar]");
            test!("[BAR]: /url");

            // https://github.github.com/gfm/#example-593
            test!("![foo][]");
            test!(r#"[foo]: /url "title""#);

            // https://github.github.com/gfm/#example-594
            test!("![*foo* bar][]");
            test!(r#"[*foo* bar]: /url "title""#);

            // https://github.github.com/gfm/#example-595
            test!("![Foo][]");
            test!(r#"[foo]: /url "title""#);

            // https://github.github.com/gfm/#example-596
            test!("![foo]\n[]");
            test!(r#"[foo]: /url "title""#);

            // https://github.github.com/gfm/#example-597
            test!("![foo]");
            test!(r#"[foo]: /url "title""#);

            // https://github.github.com/gfm/#example-598
            test!("![*foo* bar]");
            test!(r#"[*foo* bar]: /url "title""#);

            // https://github.github.com/gfm/#example-599
            test!("![[foo]]");
            test!(r#"[[foo]]: /url "title""#);

            // https://github.github.com/gfm/#example-600
            test!("![Foo]");
            test!(r#"[foo]: /url "title""#);

            // https://github.github.com/gfm/#example-601
            // this isn't an image link
            test!("!\\[foo]");

            // https://github.github.com/gfm/#example-602
            // this isn't an image link
            test!("\\![foo]");
        }
    }

    mod footnote {
        #[test]
        fn footnote() {
            test!("Here is a simple footnote[^1]. With some additional text after it.");
        }
    }

    mod blockquote {
        // #[ignore]
        #[test]
        fn single_line() {
            test!(">", ">");
            test!("> ", ">");
            test!(">  ", ">");
            test!(">>", "> >");
            test!(">>>", "> > >");

            test!("> bar");
            test!("> # Foo");
            test!(">baz", "> baz");
            test!(">baz\nfoobar", "> baz\n> foobar");
            test!(">baz\nfoobar", "> baz\n> foobar");
            test!("   > # Foo", "> # Foo");
            test!("   > bar", "> bar");
            test!(" > baz", "> baz");
            test!("> bar\nbaz", "> bar\n> baz");
            test!("> bar\nbaz\n> foo", "> bar\n> baz\n> foo");
            test!("> foo\n> ---");
            test!(">\n> foo\n>  ", "> foo");

            test!("> foo\n\n> bar");
            test!("> foo\n>\n> bar");
            test!("foo\n> bar", "foo\n\n> bar");
            test!("> aaa\n***\n> bbb", "> aaa\n\n***\n\n> bbb");
            test!("> bar\nbaz", "> bar\n> baz");
            test!("> bar\n\nbaz");
            test!("> bar\n>\nbaz", "> bar\n\nbaz");
            test!("> > > foo\nbar", "> > > foo\n> > > bar");
            test!(">>> foo\n> bar\n>>baz", "> > > foo\n> > > bar\n> > > baz");
        }
    }

    mod list {
        #[test]
        fn unordered_lists() {
            test!("*");
            test!("+");
            test!("-");
            test!(" *", "*");
            test!(" +", "+");
            test!(" -", "-");
            test!("  *", "*");
            test!("  +", "+");
            test!("  -", "-");
            test!("   *", "*");
            test!("   +", "+");
            test!("   -", "-");
            test!("* foo");
            test!("* foo\nbar", "* foo\n  bar");
            // https://github.github.com/gfm/#example-233
            // a list followed by a paraphraph
            test!("- one\n\n two", "- one\n\ntwo");
            // https://github.github.com/gfm/#example-234
            test!("- one\n\n  two");

            // https://github.github.com/gfm/#example-237
            test!("   > > +  one\n>>\n>>     two", "> > + one\n> >\n> >   two");

            // https://github.github.com/gfm/#example-238
            test!(">>- one\n>>\n  >  > two", "> > - one\n> >\n> > two");

            // https://github.github.com/gfm/#example-256
            test!("-\n  foo", "- foo");

            // https://github.github.com/gfm/#example-258
            test!("-\n\n  foo", "-\n\nfoo");

            // https://github.github.com/gfm/#example-259
            test!("- foo\n-\n- bar");

            // https://github.github.com/gfm/#example-260
            test!("- foo\n-   \n- bar", "- foo\n-\n- bar");
            test!("- foo\n-         \n- bar", "- foo\n-\n- bar");
        }

        #[test]
        fn ordered_lists() {
            test!("1.");
            test!("1)");
            test!(" 1.", "1.");
            test!(" 1)", "1)");
            test!("  1.", "1.");
            test!("  1)", "1)");
            test!("   1.", "1.");
            test!("   1.", "1.");
            test!("1. foo");
            test!("1. foo\nbar", "1. foo\n   bar");

            // Counting for ordered lists
            test!("1. foo\n1. bar", "1. foo\n2. bar");
            test!("9. foo\n10. bar\nbaz", "9. foo\n10. bar\n    baz");

            // https://github.github.com/gfm/#example-233
            // a list followed by a paraphraph
            test!("1. one\n\n two", "1. one\n\ntwo");
            // https://github.github.com/gfm/#example-234
            test!("1. one\n\n   two");

            // https://github.github.com/gfm/#example-237
            test!(
                "   > > 1.  one\n>>\n>>     two",
                "> > 1. one\n> >\n> >    two"
            );

            // https://github.github.com/gfm/#example-238
            test!(">>1. one\n>>\n  >  > two", "> > 1. one\n> >\n> > two");

            // https://github.github.com/gfm/#example-256
            test!("1.\n  foo", "1. foo");

            // https://github.github.com/gfm/#example-258
            test!("1.\n\n  foo", "1.\n\nfoo");

            // https://github.github.com/gfm/#example-259
            test!("1. foo\n2.\n3. bar");

            // https://github.github.com/gfm/#example-260
            test!("1. foo\n2.   \n3. bar", "1. foo\n2.\n3. bar");
            test!("1. foo\n2.         \n3. bar", "1. foo\n2.\n3. bar");
        }
    }

    mod code_block {
        #[ignore]
        #[test]
        fn indented() {
            test!("    ", "");
            test!("    a simple indented code block");
            test!("    a simple\n      indented code block");

            // https://github.github.com/gfm/#example-80

            // https://github.github.com/gfm/#example-104
            test!("    ```\n    aaa\n    ```");
        }

        #[test]
        fn fenced() {
            test!("```\n<\n >\n```"); // https://github.github.com/gfm/#example-89
            test!("~~~\n<\n >\n~~~"); // https://github.github.com/gfm/#example-90
            test!("```\naaa\n~~~\n```"); // https://github.github.com/gfm/#example-92
            test!("~~~\naaa\n```\n~~~"); // https://github.github.com/gfm/#example-93

            // The closing code fence must be at least as long as the opening fence
            // https://github.github.com/gfm/#example-94
            test!("````\naaa\n```\n``````", "````\naaa\n```\n````");
            // https://github.github.com/gfm/#example-95
            test!("~~~~\naaa\n~~~\n~~~~");

            test!("```\n\n  ```", "```\n```"); // https://github.github.com/gfm/#example-99
            test!("```\n```"); // https://github.github.com/gfm/#example-100
            // https://github.github.com/gfm/#example-101
            test!(" ``` aaa\naaa\n```", "```aaa\naaa\n```");
            // https://github.github.com/gfm/#example-102
            test!("  ```\naaa\n  aaa\naaa\n  ```", "```\naaa\naaa\naaa\n```");
            // https://github.github.com/gfm/#example-103
            test!(
                "   ```\n   aaa\n    aaa\n  aaa\n   ```",
                "```\naaa\n aaa\naaa\n```"
            );
            // https://github.github.com/gfm/#example-105
            test!("```\naaa\n  ```", "```\naaa\n```");
            // https://github.github.com/gfm/#example-106
            test!("   ```\naaa\n  ```", "```\naaa\n```");

            // Fenced code blocks can interrupt paragraphs, and can be followed directly by
            // paragraphs, without a blank line between
            // https://github.github.com/gfm/#example-110
            test!("foo\n```\nbar\n```\nbaz", "foo\n\n```\nbar\n```\n\nbaz");

            // Other blocks can also occur before and after fenced code blocks without an
            // intervening blank line
            // https://github.github.com/gfm/#example-111
            test!(
                "foo\n---\n~~~\nbar\n~~~\n# baz",
                "foo\n---\n\n~~~\nbar\n~~~\n\n# baz"
            );

            // https://github.github.com/gfm/#example-112
            test!("```ruby\ndef foo(x)\n  return 3\nend\n```");
            // https://github.github.com/gfm/#example-113
            test!(
                "~~~~    ruby startline=3 $%@#$\ndef foo(x)\n  return 3\nend\n~~~~~~~",
                "~~~~ruby startline=3 $%@#$\ndef foo(x)\n  return 3\nend\n~~~~"
            );
            // https://github.github.com/gfm/#example-114
            test!("````;\n````");

            // Info strings for backtick code blocks cannot contain backticks
            // https://github.github.com/gfm/#example-115
            test!("``` aa ```\nfoo", "```aa```\nfoo");

            // Info strings for tilde code blocks can contain backticks and tildes
            // https://github.github.com/gfm/#example-116
            test!("~~~ aa ``` ~~~\nfoo\n~~~", "~~~aa ``` ~~~\nfoo\n~~~");

            // https://github.github.com/gfm/#example-117
            test!("```\n``` aaa\n```");
        }

        #[test]
        fn unclosed_code_fence() {
            // The text just before <https://github.github.com/gfm/#example-96> states:
            //     Unclosed code blocks are closed by the end of the document
            //     (or the enclosing block quote or list item)

            // https://github.github.com/gfm/#example-96
            test!("```", "```\n```");
            // https://github.github.com/gfm/#example-97
            test!("`````\n```\naaa", "`````\n```\naaa\n`````");
            // https://github.github.com/gfm/#example-98
            test!("> ```\n> aaa\nbbb", "> ```\n> aaa\n> ```\n\nbbb");
            // https://github.github.com/gfm/#example-107
            // This is not a closing fence, because it is indented 4 spaces
            test!("```\naaa\n    ```", "```\naaa\n    ```\n```");

            // https://github.github.com/gfm/#example-108
            // Code fences (opening and closing) cannot contain internal spaces
            // this is an inline code snippet followed by a paragraph
            test!("``` ```\naaa");

            // https://github.github.com/gfm/#example-109
            // The closing code fence has a space so it's not valid
            test!("~~~~~~\naaa\n~~~ ~~", "~~~~~~\naaa\n~~~ ~~\n~~~~~~");
        }
    }

    mod mixed {
        #[test]
        fn header_and_paragraph() {
            test!("# Header\nsome text", "# Header\n\nsome text");
            test!("Header\n=\nsome text", "Header\n=\n\nsome text");
        }

        #[test]
        fn paragraph_and_html() {
            test!("hey<h2>sub heading</h2>", "hey<h2>sub heading</h2>");
            test!("hey <h2>sub heading</h2>", "hey <h2>sub heading</h2>");
            test!("hey\n<h2>sub heading</h2>", "hey\n\n<h2>sub heading</h2>");
            test!("hey\n\n<h2>sub heading</h2>", "hey\n\n<h2>sub heading</h2>");
        }
    }
}
