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
