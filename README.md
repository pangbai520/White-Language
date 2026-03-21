# White Language

![License](https://img.shields.io/badge/license-Apache--2.0-red.svg)
![Version](https://img.shields.io/github/v/tag/pangbai520/White-Language?label=version&color=green&sort=semver)
![Status](https://img.shields.io/badge/status-Bootstrapped-success.svg)

White Language (suffix: `.wl`) is a statically-typed, self-hosted system programming language designed for predictability, clean syntax, and seamless C interoperability. 

Built from the ground up and fully bootstrapped (the White Language Compiler `wlc` is written entirely in White Language), it aims to hit the sweet spot between low-level memory control and high-level ergonomics.

## Key Features

* **Self-Hosted & Independent**: The compiler has successfully passed the bootstrapping milestone. White Language compiles White Language.
* **Predictable Memory Management**: Employs Automatic Reference Counting (ARC) for complex types (`String`, `Struct`, `Vector`) to ensure memory safety without the non-deterministic pauses of a Garbage Collector.
* **Seamless C Interoperability (FFI)**: Bind to C functions effortlessly with the `extern "C"` syntax. No complex wrappers or boilerplate required.
* **Modern, Clean Syntax**: Features trailing return types (`-> Type`), straightforward variable declarations (`let`, `const`), and an intuitive module import system.
* **First-class Pointers**: While ARC handles high-level data, you still have full access to raw memory manipulation using `ptr`, `ref`, and `deref` when performance or hardware interaction demands it.

## A Taste of White Language

Here is a real example showing ARC, structural data, and built-in functions:

```rust
import "builtin"

// Define a struct
struct Node(
   value -> Int,
   next  -> Node
)

func main() -> Int {
   // Memory is automatically managed by ARC
   let head -> Node = Node(value=10, next=null);
   let second -> Node = Node(value=20, next=null);
   
   head.next = second;
   
   // Type-safe null checking
   if (head.next is !null) {
      // Built-in string concatenation and printing
      builtin.print("Second node value is: " + head.next.value);
   }
   
   return 0; 
   // All memory is deterministically freed upon exiting the scope
}
```

## Why White Language?

* **Versus C**: White Language eliminates the need for header files and manual `malloc`/`free` for strings and vectors thanks to built-in ARC, making it vastly safer for everyday data manipulation while retaining the ability to drop down to raw pointers.
* **Versus Rust**: White Language avoids the steep learning curve of the borrow checker. You get deterministic memory management via ARC without spending hours fighting the compiler over lifetimes, making prototyping much faster.
* **Versus Python/Scripting**: White Language is strictly statically typed and compiled ahead-of-time (AOT) to native machine code. It catches errors at compile-time and executes orders of magnitude faster.

## Limitations & Known Issues

White Language is a passion project and a work in progress. If you are considering using it for production, please be aware of the following architectural and developmental limitations:

1. **No Cycle Collection in ARC**: The current Automatic Reference Counting implementation is naive. It does not support weak references. If you create cyclic references (e.g., a Struct A pointing to Struct B, which points back to Struct A), memory leaks *will* occur.
2. **Immature Type System**: 
   * **No Generics**: True generic programming (templates/monomorphization) is severely limited.
   * **No Algebraic Data Types (ADTs)**: There is currently no support for Rust-style `Enum`s or pattern matching.
   * **No Traits/Interfaces**: Polymorphism and interface-based dispatch are not yet implemented.
3. **Limited Standard Library**: The standard library (`std`) is still in its infancy. It lacks robust cross-platform abstractions for networking, multithreading, async I/O, and advanced file system operations.
4. **Basic Error Handling**: White Language does not currently have a robust `try/catch` exception mechanism or Monadic error handling (like `Result<T, E>`). Error management heavily relies on manual checking and C-style return codes.
5. **No Built-in Package Manager**: There is no equivalent to `cargo` or `npm`. Dependency management relies on physical file paths and environment variables (`WL_PATH`).

## Tooling & Ecosystem

We believe a language is only as good as its tooling. White Language comes with first-class developer experience tools out of the box:

* **VS Code Extension (LSP)**: We provide a dedicated VS Code extension powered by **Langium**. It offers real-time syntax highlighting, semantic validation, autocomplete, and error diagnostics directly in your editor.
* **Official Website & Binaries (Coming Soon)**: Prefer not to build from source? Our official portal is currently under active development. Once launched, it will serve as the primary hub for downloading pre-compiled, native executable binaries for Windows, macOS, and Linux.

## Building and Bootstrapping

Since White Language is self-hosted, you need an existing White Language compiler (Stage 1 or the pre-compiled binary installer) to build the compiler from source.

### 1. Set Environment Variable
Ensure your `WL_PATH` is set to the root directory of the White Language standard library:
```bash
export WL_PATH=/path/to/WhiteLanguage
```

### 2. Compile the Compiler

Use the existing compiler to compile the `wlc.wl` driver and its core components:

```bash
wlc src/wlc.wl wlc_new
```

*(Use `wlc.exe` on Windows).*

### 3. Verify Build

Test the newly built compiler:

```bash
./wlc_new --help
```

You should see the `White Language Compiler (v0.1.2)` output.

## License

White Language is licensed under the Apache License 2.0 - see the [LICENSE](LICENSE) file for details.