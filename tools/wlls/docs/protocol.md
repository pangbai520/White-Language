# Using wlls from an editor

`wlls` runs as a child process and talks over stdin and stdout:

```text
wlls --stdio
```

The wire format is small and deliberately boring. It is not the Language Server Protocol yet. Every message is a json object prefixed with its utf-8 byte length:

```text
Content-Length: 43\r\n
\r\n
{"protocol":1,"id":1,"method":"initialize"}
```

Do not read stdout one line at a time after the header. Read exactly `Content-Length` bytes for the body. Keep stderr separate so a diagnostic from the process cannot corrupt the protocol stream.

Requests are handled in order. The client chooses an integer `id`, and the
response carries the same value:

```json
{"protocol":1,"id":1,"result":null}
```

Failures use an `error` object instead:

```json
{
  "protocol": 1,
  "id": 4,
  "error": {
    "code": "documentNotOpen",
    "message": "Open the document before querying it."
  }
}
```

## Starting a session

Send `initialize` first:

```json
{"protocol":1,"id":1,"method":"initialize"}
```

Its response contains the protocol version and the capabilities supported by that build. A plugin should inspect this response instead of assuming every server has every feature.

Open a document by sending its complete text:

```json
{
  "protocol": 1,
  "id": 2,
  "method": "textDocument/open",
  "path": "F:/project/main.wl",
  "version": 1,
  "text": "func main() -> Int { return 0; }"
}
```

`textDocument/change` has the same fields. It currently replaces the complete document; incremental edits are not supported. Increase `version` after every change. A version older than the copy held by the server is rejected.

When the file closes:

```json
{
  "protocol": 1,
  "id": 3,
  "method": "textDocument/close",
  "path": "F:/project/main.wl"
}
```

End the process cleanly with:

```json
{"protocol":1,"id":99,"method":"shutdown"}
```

## Diagnostics

```json
{
  "protocol": 1,
  "id": 4,
  "method": "textDocument/diagnostics",
  "path": "F:/project/main.wl"
}
```

The result is an array. Positions are zero-based and `character` is a utf-16 column, which is what VS Code expects:

```json
[
  {
    "severity": "error",
    "code": "E1001",
    "category": "InvalidSyntax",
    "message": "Expected ';' after statement.",
    "range": {
      "start": {"line": 2, "character": 16},
      "end": {"line": 2, "character": 17}
    }
  }
]
```

## Document symbols

```json
{
  "protocol": 1,
  "id": 5,
  "method": "textDocument/documentSymbols",
  "path": "F:/project/main.wl"
}
```

Symbols contain `name`, `kind`, `range`, and `children`. Classes and structs, for example, carry their fields and methods in `children`, so an editor can build an Outline view without walking the source itself.

## Go to definition

```json
{
  "protocol": 1,
  "id": 6,
  "method": "textDocument/definition",
  "path": "F:/project/main.wl",
  "line": 12,
  "character": 8
}
```

The result is either `null` or:

```json
{
  "path": "F:/project/math.wl",
  "range": {
    "start": {"line": 0, "character": 5},
    "end": {"line": 0, "character": 8}
  }
}
```

Imported definitions can only be resolved when the target document has already been opened in the same server session. A project plugin should therefore open the relevant White Language files before expecting cross-file navigation.

## Semantic tokens

```json
{
  "protocol": 1,
  "id": 7,
  "method": "textDocument/semanticTokens",
  "path": "F:/project/main.wl"
}
```

The server returns full tokens with absolute positions:

```json
[
  {
    "line": 0,
    "character": 5,
    "length": 4,
    "type": "function",
    "modifiers": ["declaration"]
  }
]
```

The token types and modifiers are listed in the `initialize` response. The coordinates are not LSP delta encoded; convert them before passing them to an editor API that expects delta encoding.

## Plugin process loop

A plugin only needs one long-lived `wlls` process per workspace:

1. spawn `wlls --stdio`;
2. send `initialize` and wait for its response;
3. forward open documents and full-text changes;
4. ask for diagnostics, symbols, definitions, or tokens as needed;
5. match responses to pending requests by `id`;
6. send `shutdown` when the workspace closes.

Queue writes to stdin. Two messages written at the same time can interleave and break framing. Requests may be sent back-to-back, but this version of the server processes them synchronously and does not support cancellation.

If the process exits, reject all pending requests, report stderr, and start a fresh process. Documents held by the old process are gone and must be opened again.

The plugin should not parse White Language to compensate for a missing server feature. Syntax and semantic decisions belong in `wlls`, which shares the compiler frontend. The editor side should stay responsible for process management, document synchronization, and translating responses into its own
UI types.
