# Some rust code

rustfmt will make minimal edits outside of code blocks like updating newlines after headers and before code blocks.

```rust
fn main() {
    println!("Hello world!")
}
```

Here's an indented code block that won't be formatted

    fn main()   {
                    println!(            "Hello world!"
                    )
    }

Hey check out the [commonmark spec]!

Look we can also link to rust types like [`Debug`] and [`Vec`].
Some additional text with [brackets]. what if I manually \[esacpe the bracket\]? looks like they stay escaped!

[commonmark spec]: https://spec.commonmark.org/0.30/
[a dead link]: https://this/link/isnt/used
[`Debug`]: core::fmt::Debug
