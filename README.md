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
import Error from "errors"

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
            print("could not read config, error code " + Int(err));
        }
        return 1;
    }

    print(config);
    return 0;
}
```

`catch(err)` receives an error value carrying both its error domain and numeric
code. Compare it directly with a concrete member such as
`Error.FileNotFound`. `Int(err)` extracts only the numeric code. If a function
is itself fallible, a `?` without a following `catch` propagates the complete
error value.

Libraries can define errors without adding members to the standard `Error`
type:

```rs
error JsonError {
    UnexpectedToken,
    InvalidEscape
}

func parse_json() -> Int? {
    throw JsonError.UnexpectedToken;
}
```

Error domains are kept during `?` propagation and rethrowing, so identically
numbered members from different libraries do not compare equal.

Standard input, output and error are available through `io`:

```rs
import "io"

func main() -> Int {
    io.stdout.write_all("name: ")?;
    catch(err) { return 1; }

    let name -> String = io.stdin.read_line()?;
    catch(err) { return 1; }

    io.stdout.write_line("hello, " + name)?;
    catch(err) { return 1; }
    return 0;
}
```

`read_bytes` may return fewer bytes than requested. `read_full` either fills the
request or reports `Error.EndOfFile`; `write_all` handles short writes. Use
these APIs for pipes and binary protocols. `print` is the convenient formatted
front end and deliberately keeps its old `Void` contract, so it cannot report
an output failure to its caller.

For interactive input, the prelude provides a small prompt wrapper:

```wl
let name -> String = input.read("name: ")?;
```

`input.read_bytes`, `input.read_full`, `input.read_until`, `input.read_all` and
`input.skip_bytes` add a prompt to their matching `io.stdin` operation.

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
-O0 ... -O3     optimize for runtime speed
-Os / -Oz       optimize for binary size
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
wlc src/wlc.wl -Oz -o wlc_new
```

or on Windows:

```bash
wlc src/wlc.wl -Oz -o wlc_new.exe
```

The existing compiler gets its standard library and runtime from `WL_PATH`.
When changing syntax or compiler intrinsics, remember that this first build is
still being parsed by the old compiler. A change which requires its own new
syntax needs to be staged rather than committed as a bootstrap loop.

The release compiler is built with `-Oz`. On the compiler's large generated IR
this produces a substantially smaller binary and spends less time in LLVM,
without making the compiler frontend slower.

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

- [wlls](https://github.com/pangbai520/White-Language-LangServer) — language server for diagnostics, navigation, and semantic highlighting.

- [website](https://www.white-lang.org) - Our official website.


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
- Windows x86-64, Linux x86-64, and macOS ARM64 are covered by the release pipeline and the main language test suite.

The project is suitable for experimenting with the language, working on the
compiler, and writing small native programs. I would not currently recommend
dropping it into production infrastructure and expecting the compatibility
guarantees of an established language.

## License

White Language is licensed under the [Apache License 2.0](LICENSE).