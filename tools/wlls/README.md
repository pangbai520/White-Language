# wlls (White Lang Language Server)

`wlls` is the language server for White Language.

It uses the same lexer, parser, syntax tree, and diagnostics implementation as the White Language compiler. There is no separate editor grammar to keep in sync with the compiler.

## What it does

The server currently provides:

- syntax diagnostics
- document symbols
- go to definition
- semantic tokens
- in-memory document updates

`wlls` is used by editor extensions to understand White Lang code.

## Usage

Start the server over standard input and output:

```sh
wlls --stdio
```

Editors normally start this process themselves. Messages use protocol version
1 and are framed with a `Content-Length` header. A client must first send
`initialize`, then open documents before requesting diagnostics, symbols,
definitions, or semantic tokens.

Run the following command to see the available command-line options:

```sh
wlls --help
```

## Building

Build from the root of the White Language repository:

```sh
wlc tools/wlls/wlls.wl -o wlls
```

On Windows:

```sh
wlc tools/wlls/wlls.wl -o wlls.exe
```

The server is kept in the compiler repository because it directly reuses the compiler frontend. Changes to White Language syntax should be made in the compiler, not reimplemented inside `wlls`.

## License

wlls is licensed under the [Apache License 2.0](../../LICENSE), the same license as White Language.
