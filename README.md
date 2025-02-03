# Fury, a gradual, safe systems language

## Goals

- Teachability | Learnability | Readability
- Efficiency | Productivity
- Systems and application programming

See a simple circular linked list example in fury,
```rs
struct Node {
    data: i64
    next: Node?
}

fun main() {
    mut node3 = new Node(data: 3, next: none)
    let node2 = new Node(data: 2, next: node3)
    let node1 = new Node(data: 1, next: node2)
    node3.next = node1

    mut curr: Node? = node2
    let end = curr

    // println(curr.data)
    mut total = curr.data

    while curr.next != end {
        curr = curr.next
        total -= curr.data
    }

    println(total)
}
```

Also see the [philosophy](docs/philosophy.md) for more info.

## Status

The Fury project is still in its early days of development. While we have developed a compiler in tandem with designing the language, both should be considered experimental and pre-alpha in quality. Significant changes to both the language and the compiler should be expected.

## Running the test suite

To run the Fury test suite, be sure you have `clang` installed.

```bash
zig build run -- <file-path>    # codegen a output.c file
zig build gcc                  # emit executable using gcc
zig build exec                 # run executable
zig build coverage             # run entire test suite
```

The Fury compiler outputs C code, which you can then build with a C-compatible compiler. The test suite assumes `clang` is the compiler that is available. (Note for Windows users: Visual Studio can install 'clang' as an optional install)

## The Fury language

For more information on the Fury language, check out [the Fury language documentation](docs/language.md)

## Roadmap

Our main goal with Fury is to build out an implementation of its initial design and to test it.

As we dogfood, we will likely change the design of the language where we see it doesn't meet the goals of the project.

We'll likely also work towards support in IDEs (using things like LSP) to make it easier to write larger Fury projects and get interactive feedback from the tools.
