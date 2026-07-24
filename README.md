# White Language

![License](https://img.shields.io/badge/license-Apache--2.0-red.svg)
![Version](https://img.shields.io/github/v/tag/pangbai520/White-Language?label=version&color=green&sort=semver)
![Status](https://img.shields.io/badge/status-bootstrapped-success.svg)

White Language is a compiled, statically typed language with the `.wl` file
extension. The compiler emits LLVM IR and lets Clang do the final code
generation.

`wlc` is written in White Language. It has been self-hosting for a while now:
the compiler in a release builds the compiler in the next release. I normally
build it twice and compare the generated IR before calling a compiler change
done.

## A quick example

```rust
import "builtin"
import "dict" // this will auto-imported by compiler, but listed here for clarity

interface Named {
    method name() -> String;
}

class Entity with Named {
    let entity_name -> String = "";

    init(name -> String) {
        self.entity_name = name;
    }

    method name() -> String {
        return self.entity_name;
    }

    method describe() -> Void {
        print("Entity(" + self.entity_name + ")");
    }

    deinit() {
        print("dropping " + self.entity_name);
    }
}

class Player(Entity) {
    let score -> Int = 0;

    init(name -> String, score -> Int) {
        super.init(name);
        self.score = score;
    }

    method describe() -> Void {
        print(self.entity_name + ": " + self.score);
    }
}

func add(left -> Int, right -> Int) -> Int {
    return left + right;
}

func main() -> Int {
    let operation -> Function(Int, Int, Int) = add;
    print(operation(20, 22));

    let numbers -> Vector(Int) = [10, 20, 30];
    let copy -> Array(Int) = numbers[0:2];
    let view -> Array(Int) = ref numbers[0:2];
    view[0] = 99;

    print(copy[0]);       // 10
    print(numbers[0]);    // 99

    let values -> Dict = {
        "language": "White Language",
        "version": "12345"
    };
    print(values["language"]);

    let player -> Player = Player("CC", 999);
    let describe -> Method(Void) = player.describe;
    describe();
    return 0;
}
```

Classes use virtual dispatch, methods and functions are ordinary
values, and the array slice rules shown above are the rules used by normal
programs.

## Some language details

White has signed and unsigned integers from 8 to 128 bits, fixed arrays,
vectors, slices, strings, dictionaries, classes, structs, enums, interfaces,
closures and raw pointers.

Managed values use atomic reference counts. When the last owning reference goes
away, fields are released and `deinit` runs immediately. The compiler also
emits cleanup for returns, loop exits and error propagation. ARC only protects
object lifetime; it does not make a mutable `Vector` or `Dict` safe to modify
from several threads at once.

Operations which can fail return `T?` (or `Void?`):

```rs
import "file"
import Error from "builtin/errors"

func read_config(path -> String) -> String? {
    let input -> file.File = file.open(path)?;
    let content -> String = input.read_all()?;
    input.close_checked()?;
    return content;
}

func main() -> Int {
    let config -> String = read_config("config.txt")?;
    catch(err) {
        if (err == Error.FileNotFound) {
            print("config.txt does not exist");
        } else {
            print("could not read config: " + err);
        }
        return 1;
    }

    print(config);
    return 0;
}
```

`err` is the `Error` enum, not an integer error code. If a function is itself
fallible, a `?` without a following `catch` propagates the error.

Slices are left-closed and right-open. An ordinary slice is a shallow copy:

```rs
let copy -> Array(Int) = values[1:3];
```

Adding `ref` makes it a shared view:

```rs
let view -> Array(Int) = ref values[1:3];
```

The view retains its backing storage, so growing the original vector does not
leave the view pointing at freed memory. Strings have the same copy syntax.
The only zero-copy String form currently implemented is the full
`ref text[:]` alias; bounded String views are not there yet.

## Calling native code

There are block and single-function forms:

```rs
extern "C" in "mylib" {
    func native_add(left -> Int, right -> Int) -> Int;
}

extern func native_version() -> Int from "C" in "mylib";
```

`"C"` and `"system"` are currently supported. `in "mylib"` asks the linker for
that library, while `-L` tells it where to look:

```sh
wlc app.wl -L ./native/lib
```

On Windows, putting `mylib.dll` next to the source file is not enough for the
link step. Clang still needs a `.lib` or `libmylib.a` import library. The DLL is
used later, when the finished program starts.

Functions declared with `extern` keep their native symbol names. Normal White
functions are mangled; `@ExportLib` can be used when a shared library needs to
export an unmangled entry point.

## Building

`WL_PATH` points to the root of the White Language installation, not its `bin`
directory:

```text
WhiteLanguage/
├── bin/
├── runtime/
├── std/
└── tools/
```

On Linux or macOS:

```bash
export WL_PATH=/path/to/WhiteLanguage
wlc hello.wl
./hello
```

On Windows:

Installer will automatically configure environment variables so we don't need to configure them manually.

```bash
wlc hello.wl
.\hello.exe
```

The options I use most often are:

```text
-o <file>       choose the output name
-O0 ... -O3     set the optimization level
-c              emit an object file
-S              emit assembly
--emit-llvm     emit LLVM IR
--shared        build a DLL/.so/.dylib
-L <dir>        add a library search path
--keep-temps    keep intermediate files
```

`wlc --help` lists the rest.

### Rebuilding `wlc`

You need an existing White compiler to build the compiler source:

```bash
wlc src/wlc.wl -O3 -o wlc_new
```

or on Windows:

```bash
wlc src/wlc.wl -O3 -o wlc_new.exe
```

The existing compiler gets its standard library and runtime from `WL_PATH`.
When changing syntax or compiler intrinsics, remember that this first build is
still being parsed by the old compiler. A change which requires its own new
syntax needs to be staged rather than committed as a bootstrap loop.

## Runtime notes

Windows builds use native Windows APIs for startup, allocation, console output,
files and processes. The runtime provides its own EXE and DLL entry points, so
White programs do not need MSVCRT/UCRT for those jobs.

I have not tried to force the same design onto POSIX. Linux and macOS already
have a stable libc/POSIX environment, and the White runtime uses it for the
parts where that is the sensible option.

The C file in `runtime/` is now mostly an ABI and platform boundary. Code which
does not need to live there, such as integer formatting, is being moved into
White Language.

## Repository layout

```text
src/            compiler source
std/            standard library
runtime/        startup and native ABI glue
tests/          language, diagnostic, FFI and integration tests
```

## Tooling & ecosystem

There is a VS Code extension built around a Langium grammar. It currently
provides syntax highlighting and basic diagnostics. The extension exists and
works as an early development tool, but most of new syntax are not supported and 
it is not where most of the work is going at the moment.

There is also an official website and download portal for prebuilt Windows,
Linux and macOS binaries. 

Work on the site and extension is temporarily paused
while the compiler and standard library settle down, but neither project has been
abandoned.

A package manager `wlp` is planned (Maybe?).

## Things which are not finished

The compiler currently has the following known issues:

- ARC cannot collect cycles and there are no weak references yet.
- Strings contain UTF-8, but indexing and slicing are still byte-based. They
  can split a multi-byte character.
- The generic system is limited and is not yet something I would compare with
  Rust generics or C++ templates.
- `Dict` currently has String keys and uses an internal Variant representation
  for its values.
- Networking, threads, async I/O and a larger filesystem API are still missing
  from the standard library.
- Raw pointers and a wrong `extern` declaration can still produce undefined
  behaviour. They are unsafe interfaces, even though there is no `unsafe`
  keyword around them today.
- The internal White ABI can change between compiler releases.
- Windows x86-64 has had the most testing. Other architectures need more work,
  especially around native ABI layouts.

The project is suitable for experimenting with the language, working on the
compiler, and writing small native programs. I would not currently recommend
dropping it into production infrastructure and expecting the compatibility
guarantees of an established language.

## License

White Language is licensed under the [Apache License 2.0](LICENSE).