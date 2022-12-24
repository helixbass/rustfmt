# Some rust code
rustfmt will make minimal edits outside of code blocks like updating newlines after headers and before code blocks.
```rust
fn main()   {
                println!(            "Hello world!"
                )
}
```

Here's an indented code block that won't be formatted

    fn main()   {
                    println!(            "Hello world!"
                    )
    }
