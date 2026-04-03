# White Language

![License](https://img.shields.io/badge/license-Apache--2.0-red.svg)
![Version](https://img.shields.io/github/v/tag/pangbai520/White-Language?label=version&color=green&sort=semver)
![Status](https://img.shields.io/badge/status-Bootstrapped-success.svg)

White Language (suffix: `.wl`) is a statically-typed, self-hosted system programming language designed for predictability, clean syntax, and seamless C interoperability.

Built from the ground up and fully bootstrapped (the White Language Compiler `wlc` is written entirely in White Language), it aims to hit the sweet spot between low-level memory control and high-level ergonomics.

## Key Features

* **Self-Hosted & Independent**: The compiler has successfully passed the bootstrapping milestone. White Language compiles White Language.
* **Modern Compiler Architecture**: Under the hood, `wlc` operates on a completely flat, `Vector`-driven Abstract Syntax Tree (AST), ensuring blazing fast compilation times and rock-solid memory safety during the compilation phase.
* **Predictable Memory Management**: Employs Automatic Reference Counting (ARC) for complex types (`String`, `Struct`, `Vector`) to ensure memory safety without the non-deterministic pauses of a Garbage Collector.
* **Seamless C Interoperability (FFI)**: Bind to C functions effortlessly with the `extern "C"` syntax. No complex wrappers or boilerplate required.
* **First-class Pointers**: While ARC handles high-level data, you still have full access to raw memory manipulation using pointers when ultimate performance or hardware interaction is demanded.
* **Industrial-Grade CLI**: Comes with a standard, Unix-philosophy CLI interface (supporting `-c`, `-S`, `--emit-llvm`, `-O3`, and custom `--ldflags`), making it perfectly suited for modern build systems.

## A Taste of White Language

Here is a real example showing ARC, closures, and lexical capture:

```rust
import "builtin"

func make_counter(start -> Int) -> Function(Int) {
    let count -> Int = start;

    func increment() -> Int {
        count += 1;
        return count;
    }
    
    return increment;
}

func main() -> Int {
    let counter -> Function(Int) = make_counter(10);
    
    builtin.print(counter()); // Output: 11
    builtin.print(counter()); // Output: 12
    
    return 0;
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
5. **No Built-in Package Manager (Yet)**: There is no equivalent to `cargo` or `npm`. Dependency management currently relies on physical file paths and environment variables (`WL_PATH`). However, a decentralized package manager WhiteLang Package manager (`wlp`) is on the roadmap.

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
wlc src/wlc.wl -o wlc_new -O3
```

*(Use `wlc.exe` and `-o wlc_new.exe` on Windows).*

### 3. Verify Build

Test the newly built compiler:

```bash
./wlc_new
```

You should see the `White Language Compiler (v0.1.6)` output.

## License

White Language is licensed under the Apache License 2.0 - see the [LICENSE](LICENSE) file for details.