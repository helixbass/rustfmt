rule! {
    key => value,
}

// multiple keys and values
rule! {
    key => value,
    key2 => value2,
}

// weird spacing
rule! {
    key => value,
}

// no trailing comma
rule! {
    key => value,
}

// nested curly brackets
rule! {
    key => {
        nested_key => value,
    },
}

// nested square brackets
rule! {
    key => [
        nested_key => value,
    ],
}
