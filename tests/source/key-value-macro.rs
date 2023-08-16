rule! {
    key =>  value,
}

// multiple keys and values
rule! {
    key =>  value,
    key2 =>  value2,
}

// weird spacing
rule! {
  key=>value ,
}

// no trailing comma
rule! {
    key => value
}

// nested curly brackets
rule! {
    key => {
        nested_key =>  value,
    }
}

// nested square brackets
rule! {
    key => [
        nested_key =>  value,
    ]
}

// nested array literal
rule! {
    key => [
        nested_key =>  [
            "foo",
            "bar"
        ]
    ]
}

// nested block
rule! {
    key => [
        nested_key =>  {
            "foo"
        }
    ]
}

// basic real example
pub fn no_debugger_rule() -> Arc<dyn Rule> {
    rule! {
        name =>  "no-debugger",
        languages => [Javascript],
        messages => [
            unexpected => "Unexpected 'debugger' statement.",
        ],
        listeners => [
            r#"(
              (debugger_statement) @c
            )"# => |node, context| {
                context.report(violation! {
                    node => node,
                    message_id => "unexpected",
                });
            },
        ]
    }
}

// unparseable value
pub fn default_case_rule() -> Arc<dyn Rule> {
    rule! {
        name =>  "default-case",
        languages => [Javascript],
        messages => [
            missing_default_case => "Expected a default case.",
        ],
        options_type => Options,
        state => {
            [per-run]
            comment_pattern: Cow<'static, Regex> = options.comment_pattern.map_or_else(
                || Cow::Borrowed(&*DEFAULT_COMMENT_PATTERN),
                |comment_pattern| Cow::Owned(Regex::new(&comment_pattern).unwrap())
            ),
        },
        listeners => [
            r#"
              (switch_statement) @c
            "# => |node, context| {
                let body = node.field("body");
                let cases = body.non_comment_named_children().collect::<Vec<_>>();
                if cases.is_empty() {
                    return;
                }
                let has_default = cases.iter().any(|v| v.kind() == SwitchDefault);
                if has_default {
                    return;
                }

                if context.get_comments_after(*cases.last().unwrap())
                    .last()
                    .filter(|comment| {
                        self.comment_pattern.is_match(get_comment_contents(*comment, context).trim())
                    })
                    .is_none() {
                    context.report(violation! {
                        node => node,
                        message_id => "missing_default_case",
                    });
                }
            },
        ],
    }
}
