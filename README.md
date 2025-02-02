# Fury, a gradual, safe systems language

## Goals

- Teachability | Learnability | Readability
- Efficiency | Productivity
- Systems and application programming

Also see the [philosophy](docs/philosophy.md) for more info.

## Participating in the closed alpha/beta

## Status

The Fury project is still in its early days of development. While we have developed a compiler in tandem with designing the language, both should be considered experimental and pre-alpha in quality. Significant changes to both the language and the compiler should be expected.

## Running the test suite

To run the Fury test suite, be sure to have the following installed:

* A reasonably good `clang` in your path

The Fury compiler outputs C code, which you can then build with a C-compatible compiler. The test suite assumes `clang` is the compiler that is available. (Note for Windows users: Visual Studio can install 'clang' as an optional install)

## The Fury language

For more information on the Fury language, check out [the Fury language documentation](docs/language.md)

## Roadmap

Our main goal with Fury is to build out an implementation of its initial design and to test it.

As we dogfood, we will likely change the design of the language where we see it doesn't meet the goals of the project.

We'll likely also work towards support in IDEs (using things like LSP) to make it easier to write larger Fury projects and get interactive feedback from the tools.
