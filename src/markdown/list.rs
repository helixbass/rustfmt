use std::borrow::Cow;
// Including all these spaces might be overkill, but it probably doesn't hurt.
// In practice we'll see far fewer digits in an ordered list.
//
// <https://github.github.com/gfm/#list-items> mentions that:
//
//     An ordered list marker is a sequence of 1–9 arabic digits (0-9), followed by either a .
//     character or a ) character. (The reason for the length limit is that with 10 digits we
//     start seeing integer overflows in some browsers.)
//
const LIST_INDENTATION: &'static str = "                    ";

#[derive(Debug)]
pub(super) enum ListMarker {
    // TODO(ytmimi) consider leading 0s in ordered lists
    Ordered {
        number: usize,
        marker: OrderedListMarker,
    },
    Unordered(UnorderedListMarker),
}

impl ListMarker {
    pub(super) fn increment_count(&mut self) {
        match self {
            Self::Ordered { number, marker: _ } => {
                *number += 1;
            }
            Self::Unordered(_) => {}
        }
    }

    pub(super) fn indentation(&self) -> Cow<'static, str> {
        let indent_index = self.indentation_len();

        if indent_index <= LIST_INDENTATION.len() {
            Cow::from(&LIST_INDENTATION[..indent_index])
        } else {
            // would be extreamly rare to hit his case
            Cow::from(" ".repeat(indent_index))
        }
    }

    fn indentation_len(&self) -> usize {
        match self {
            Self::Ordered { number, marker: _ } => {
                let char_len = number.checked_ilog10().unwrap_or(0) + 1;
                // + 2 to for '. '
                (char_len + 2) as usize
            }
            Self::Unordered(_) => 2,
        }
    }
}

#[derive(Debug)]
pub(super) enum OrderedListMarker {
    Period,
    Parenthesis,
}

impl From<OrderedListMarker> for char {
    fn from(value: OrderedListMarker) -> Self {
        match value {
            OrderedListMarker::Period => '.',
            OrderedListMarker::Parenthesis => ')',
        }
    }
}

impl From<&OrderedListMarker> for char {
    fn from(value: &OrderedListMarker) -> Self {
        match value {
            OrderedListMarker::Period => '.',
            OrderedListMarker::Parenthesis => ')',
        }
    }
}

#[derive(Debug)]
pub(super) enum UnorderedListMarker {
    Asterisk,
    Plus,
    Hyphen,
}

impl From<UnorderedListMarker> for char {
    fn from(value: UnorderedListMarker) -> Self {
        match value {
            UnorderedListMarker::Asterisk => '*',
            UnorderedListMarker::Plus => '+',
            UnorderedListMarker::Hyphen => '-',
        }
    }
}
impl From<&UnorderedListMarker> for char {
    fn from(value: &UnorderedListMarker) -> Self {
        match value {
            UnorderedListMarker::Asterisk => '*',
            UnorderedListMarker::Plus => '+',
            UnorderedListMarker::Hyphen => '-',
        }
    }
}
