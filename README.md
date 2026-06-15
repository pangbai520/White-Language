# White Language

![License](https://img.shields.io/badge/license-Apache--2.0-red.svg)
![Version](https://img.shields.io/github/v/tag/pangbai520/White-Language?label=version&color=green&sort=semver)
![Status](https://img.shields.io/badge/status-Bootstrapped-success.svg)

White Language (suffix: `.wl`) is a statically-typed, self-hosted system programming language designed for predictability, clean syntax, and seamless C interoperability.

Built from the ground up and fully bootstrapped (the White Language Compiler `wlc` is written entirely in White Language), it balances C-like memory control with modern syntax and object-oriented safety.

## Key Features
* **Bootstrapped Compiler**: The compiler is fully self-hosted. White Language is written in White Language and compiled by its own previous generation.
* **OOP with VTable Dispatch**: Supports class inheritance (`class Sub(Parent)`), method overriding, and `super` calls. Dynamic dispatch is handled via internal VTables.
* **First-Class Functions & Types**: Functions, methods, and classes are first-class citizens. You can assign them to variables or pass them as arguments using intuitive syntax without dealing with function pointers.
* **Predictable ARC & Destructors**: Uses Automatic Reference Counting (ARC) for memory management. Objects are reclaimed immediately when their reference count drops to zero, with support for `deinit()` destructors to handle custom cleanup logic.
* **Namespace Isolation & Modern Module System**: Features a strict module system with `import *` support and compile-time privacy. Symbols starting with `__` are strictly private to their module, ensuring clean namespace management.
* **Seamless C FFI**: Directly call C functions using `extern "C"` blocks. It maps White Language types to C calling conventions with zero boilerplate.
* **Developer CLI**: A straightforward compiler driver supporting various build stages (`-c`, `-S`), LLVM IR emission (`--emit-llvm`), and optimization levels (`-O3`).

## A Taste of White Language

Here is a real example showing VTable polymorphism, `super` dispatch, and automatic RAII resource management:

```rust
import "builtin"
import "dict" // this will auto-imported by compiler, but listed here for clarity

// struct definition
struct Point(x -> Int, y -> Int) {
    this.x = 0;
    this.y = 0;
}

// classes and inheritance
class Entity {
    let name -> String = "Base";

    init(n -> String) {
        self.name = n;
    }

    method identify() -> Void {
        builtin.print("I am " + self.name);
    }

    deinit() {
        builtin.print("Entity " + self.name + " destroyed");
    }
}

class Player(Entity) {
    let score -> Int = 0;

    init(n -> String, s -> Int) {
        super.init(n);
        self.score = s;
    }

    // overriding
    method identify() -> Void {
        builtin.print("Player: " + self.name + " | Score: " + self.score);
    }
}

func global_add(a -> Int, b -> Int) -> Int {
    return a + b;
}

func main() -> Int {
    // first-class functions
    let math_func -> Function(Int) = global_add;
    let result -> Int = math_func(10, 20);

    // collections (Arrays, Vectors, Dicts)
    let static_arr -> Int[3] = [1, 2, 3];
    let dynamic_vec -> Vector(String) = ["White", "Language"];
    dynamic_vec.append("2026");
    let my_map -> Dict = {
        "lang": "White Language", 
        "ver": "latest", 
        "is_fast": true
    };

    // OOP and first-class methods
    let p -> Player = Player("CC", 999);
    let id_method -> Method(Void) = p.identify;
    id_method(); // triggers player's identify
    return 0;
}
```

## Why White Language?

* **Versus C**: You get built-in OOP, a modern module system, and ARC memory management while maintaining the ability to use raw pointers. No more header files or manual `malloc`/`free` for every single object.
* **Versus Rust**: No borrow checker. White Language provides deterministic destruction via ARC and traditional inheritance, offering a significantly flatter learning curve for developers who prefer familiar OOP patterns.
* **Versus Scripting Languages**: White Language provides static type safety and compiles to native machine code via AOT (Ahead-of-Time) and runs significantly faster than interpreted languages like Python.

## Limitations & Known Issues

White Language is a passion project and a work in progress. If you are considering using it for production, please be aware of the following architectural and developmental limitations:

1.  **No Cycle Collection in ARC**: The current Automatic Reference Counting implementation is naive. It does not support weak references. If you create cyclic references (e.g., a Struct A pointing to Struct B, which points back to Struct A), memory leaks *will* occur.
2.  **Immature Type System**:
* **No Generics**: True generic programming (templates/monomorphization) is severely limited.
* **No Algebraic Data Types (ADTs)**: There is currently no support for Rust-style `Enum`s or pattern matching.
* **No Interfaces/Traits**: While class-based inheritance and VTable polymorphism are fully supported, multiple inheritance or interface-based dispatch is not yet implemented.
3.  **Limited Standard Library**: The standard library (`std`) is still in its infancy. It lacks robust cross-platform abstractions for networking, multithreading, async I/O, and advanced file system operations.
4.  **Basic Error Handling**: White Language does not currently have a robust `try/catch` exception mechanism or Monadic error handling (like `Result<T, E>`). Error management heavily relies on manual checking and C-style return codes.
5.  **No Built-in Package Manager (Yet)**: There is no equivalent to `cargo` or `npm`. Dependency management currently relies on physical file paths and environment variables (`WL_PATH`). However, a decentralized package manager WhiteLang Package manager (`wlp`) is on the roadmap.

## Tooling & Ecosystem

* **VS Code Extension**: A dedicated extension providing syntax highlighting and basic diagnostics, leveraging the **Langium**-based grammar for accurate parsing.
* **Official Website & Binaries**: Prefer not to build from source? Our official portal is now live! You can download pre-compiled, native executable binaries for Windows, macOS, and Linux directly from our website.

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

You should see the `White Language Compiler (v0.2.7)` output.

## License

White Language is licensed under the Apache License 2.0 - see the [LICENSE](LICENSE) file for details.