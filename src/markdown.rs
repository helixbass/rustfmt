use std::iter::Peekable;
use std::ops::Range;

use crate::comment::{hide_sharp_behind_comment, trim_custom_comment_prefix, CodeBlockAttribute};
use crate::Config;

use itertools::Itertools;
use pulldown_cmark::{CodeBlockKind, Event, LinkDef, Parser, Tag};
use pulldown_cmark_to_cmark::{cmark_resume_with_options, Options, State};

/// Rewrite markdown input.
///
/// The main goal of this function is to reformat rust code blocks in markdown text. However, there
/// will also be some light reformatting of other markdown items outside of code blocks like
/// adjusting the number of newlines after headings, paragraphs, tables, lists, blockquotes, etc.
///
/// **Note:** The content of indented codeblocks will not be formatted, but indentation may change.
pub(crate) fn rewrite_markdown(input: &str, config: &Config) -> String {
    let mut result = String::with_capacity(input.len() * 2);
    let parser = Parser::new(input);
    // Grab the reference links from the parser so we can rewrite them into the result at the end
    let reference_links = parser
        .reference_definitions()
        .iter()
        .sorted_by(|(_, link_a), (_, link_b)| link_a.span.start.cmp(&link_b.span.start))
        .map(|(link_lable, LinkDef { dest, .. })| {
            (link_lable.to_string(), dest.to_string(), "".to_string())
        })
        .collect::<Vec<_>>();

    let mut fmt_options = None;
    let mut fmt_state = State::default();

    let mut events = parser.into_offset_iter().peekable();
    while events.peek().is_some() {
        let current_fmt_options = fmt_options.unwrap_or_else(default_fmt_options);

        let (sub_events, next_fmt_options) =
            collect_events_until_fmt_options_update(input, &mut events, &current_fmt_options);
        // Update the formatting options we'll use on the next iteration.
        fmt_options = next_fmt_options;

        if sub_events.is_empty() {
            // if the first `Event` in the parser required us to update the fmt_options, then
            // sub_events will be an empty list.
            continue;
        }

        let md_code_formatter = CodeBlockFormatter::new(sub_events.into_iter(), config);
        match cmark_resume_with_options(
            md_code_formatter,
            &mut result,
            Some(fmt_state),
            current_fmt_options,
        ) {
            Ok(state) => {
                // Store the state so we can use it on the next iteration if we're not done
                fmt_state = state;
            }
            Err(_) => {
                // Something went wrong just return the original input unchanged
                return input.to_owned();
            }
        }
    }

    // Sort of hackey, but the shortcuts list will only contain links to items that were
    // referenced in the markdown document so we replace them with the links we got from the
    // parser, since the parser gives us a complete list of all the reference links.
    // If we wanted to add a feature to remove dead links, then we could just use the links
    // from `fmt_state.shortcuts`.
    let _ = std::mem::replace(&mut fmt_state.shortcuts, reference_links);

    // Calling finalize adds reference style links to the end of the result buffer
    if let Err(_) = fmt_state.finalize(&mut result) {
        // Something went wrong just return the original input unchanged
        return input.to_owned();
    }
    result
}

/// Collect `Events` until we encounter one that requiers us to update the formatting options.
///
/// For example, an unordered list that uses a different bullet marker than the one currently
/// configured, or using `_` as the emphasis character when `*` is configured.
///
/// Return the collected events and the new formatting options.
fn collect_events_until_fmt_options_update<'e, E>(
    orig: &str,
    events: &mut Peekable<E>,
    fmt_options: &Options<'static>,
) -> (Vec<Event<'e>>, Option<Options<'static>>)
where
    E: Iterator<Item = (Event<'e>, Range<usize>)>,
{
    let mut sub_events = vec![];
    let mut next_fmt_options = None;

    while let Some((event, range)) = events.peek() {
        match event {
            Event::Start(Tag::List(None)) => {
                // We're peeking at the start of an unordered list. Unordered lists bullets can be
                // one of `-`, `+`, or `*`.
                // See the commonmark list spec for more details:
                // https://spec.commonmark.org/0.30/#lists
                let item = &orig[range.clone()];
                let bullet = item.chars().take(1).next().unwrap_or('*');
                if fmt_options.list_token != bullet {
                    let mut options = fmt_options.clone();
                    options.list_token = bullet;
                    next_fmt_options.replace(options);
                    break;
                };
            }
            Event::Start(Tag::Emphasis) => {
                // We're peeking at the start of text emphasis. Emphasis chars can be `*` or `_`.
                // See the commonmark emphasis and strong emphasis spec for more details:
                // https://spec.commonmark.org/0.30/#emphasis-and-strong-emphasis
                let item = &orig[range.clone()];
                let emphasis = item.chars().take(1).next().unwrap_or('*');
                if fmt_options.emphasis_token != emphasis {
                    let mut options = fmt_options.clone();
                    options.emphasis_token = emphasis;
                    next_fmt_options.replace(options);
                    break;
                };
            }
            _ => {}
        }

        if let Some((event, _range)) = events.next() {
            sub_events.push(event);
        }
    }
    (sub_events, next_fmt_options)
}

/// default markdown formatting options used by rustfmt
fn default_fmt_options() -> Options<'static> {
    let mut fmt_options = Options::default();
    fmt_options.code_block_token_count = 3;
    fmt_options
}

struct CodeBlockFormatter<'c, 'e, E>
where
    E: Iterator<Item = Event<'e>>,
{
    events: Peekable<E>,
    config: &'c Config,
    format_code_block: bool,
    indented_code_block: bool,
}

impl<'c, 'e, E> CodeBlockFormatter<'c, 'e, E>
where
    E: Iterator<Item = Event<'e>>,
{
    fn new(events: E, config: &'c Config) -> Self {
        let events = events.peekable();
        Self {
            events,
            config,
            format_code_block: false,
            indented_code_block: false,
        }
    }
}

impl<'c, 'e, E> Iterator for CodeBlockFormatter<'c, 'e, E>
where
    E: Iterator<Item = Event<'e>>,
{
    type Item = E::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let mut event = self.events.next()?;

        match &event {
            Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(ref attributes))) => {
                // We've encoutered the start of a fenced code block.
                // The next `Event::Text` will contain the content of the code block.
                self.format_code_block = CodeBlockAttribute::new(attributes).is_formattable_rust();
            }
            Event::Text(ref code) if self.format_code_block => {
                // We've reached a code block that we'll try to format!
                //
                // First, comment out hidden rustdoc lines as they would prevent us from properly
                // parsing and formatting the code snippet.
                let with_hidden_rustdoc_lines = code
                    .lines()
                    .map(|line| hide_sharp_behind_comment(line))
                    .join("\n");

                if let Some(formatted) =
                    crate::format_code_block(&with_hidden_rustdoc_lines, &self.config, false)
                {
                    let code_block = trim_custom_comment_prefix(&formatted.snippet);
                    event = Event::Text(code_block.into());
                }
            }
            Event::End(Tag::CodeBlock(CodeBlockKind::Fenced(_))) if self.format_code_block => {
                // We've reached the end of the code block so reset format_code_block
                self.format_code_block = false
            }
            Event::Start(Tag::CodeBlock(CodeBlockKind::Indented)) => {
                // Change the indented code block to a paragraph Event so that we won't try to add
                // code fence chars when rendering to markdown with `pulldown_cmark_to_cmark`
                self.indented_code_block = true;
                event = Event::Start(Tag::Paragraph)
            }
            Event::Text(ref code) if self.indented_code_block => {
                // Add indentation back to the indented code block text.
                // We're adding 4 spaces since the commonmark spec says that:
                //     An indented code block is composed of one or more indented chunks separated
                //     by blank lines. An indented chunk is a sequence of non-blank lines, each
                //     preceded by four or more spaces of indentation.
                // See https://spec.commonmark.org/0.30/#indented-code-blocks for more details
                let is_last_code_line = matches!(
                    self.events.peek(),
                    Some(Event::End(Tag::CodeBlock(CodeBlockKind::Indented)))
                );

                event = if is_last_code_line {
                    // remove trailing newlines from the last code line to not interfere with
                    // the whitespace added by `pulldown_cmark_to_cmark`.
                    Event::Text(("    ".to_owned() + code.trim_end()).into())
                } else {
                    Event::Text(("    ".to_owned() + code).into())
                };
            }
            Event::End(Tag::CodeBlock(CodeBlockKind::Indented)) if self.indented_code_block => {
                // Change the indented code block to a paragraph Event so that we won't try to add
                // code fence chars when rendering to markdown with `pulldown_cmark_to_cmark`
                self.indented_code_block = false;
                event = Event::End(Tag::Paragraph)
            }
            _ => {}
        }
        Some(event)
    }
}

#[cfg(test)]
mod test {
    use super::rewrite_markdown;
    use crate::Config;

    #[test]
    fn format_markdown_code_block() {
        let input = "\
# This is a markdown header
+ this is a markdown list
  +   this is a sublist. See how we automatically realign
  misaligned paragraphs, which is nice!
* but if we change the bullet
- that will start a new
+ list
```rust
fn     main()    {     println!(\"hello world!\");   }
```
here is the same code block without a code fence and it won't be reformatted

    fn     main()    {     println!(\"hello world!\");   }
";

        let expected = "\
# This is a markdown header

+ this is a markdown list
  + this is a sublist. See how we automatically realign
    misaligned paragraphs, which is nice!

* but if we change the bullet

- that will start a new

+ list

```rust
fn main() {
    println!(\"hello world!\");
}
```

here is the same code block without a code fence and it won't be reformatted

    fn     main()    {     println!(\"hello world!\");   }";
        let config = Config::default();
        let formatted = rewrite_markdown(input, &config);
        assert_eq!(formatted, expected)
    }
}
