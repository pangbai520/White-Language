// Test: WLLS_PROTOCOL
// File: tools/wlls/tests/test_protocol.wl
// Focus: wlls handshake, document synchronization, and symbol requests.
import "builtin"
import "../internal/server/_pkg.wl" as server

func main() -> Int {
    let service -> server.Server = server.Server();

    let response -> String = service.handle("{");
    if (response !=
        "{\"protocol\":1,\"id\":0,\"error\":{\"code\":\"invalidRequest\"," +
        "\"message\":\"Request body is not valid JSON.\"}}") {
        builtin.print("FAIL: wlls malformed JSON");
        return 1;
    }

    response = service.handle(
        "{\"protocol\":1,\"id\":1,\"method\":\"initialize\"}"
    );
    if (!response.starts_with("{\"protocol\":1,\"id\":1,\"result\":")) {
        builtin.print("FAIL: wlls initialize");
        return 1;
    }

    response = service.handle(
        "{\"protocol\":1,\"id\":2,\"method\":\"textDocument/open\"," +
        "\"path\":\"memory.wl\",\"version\":1," +
        "\"text\":\"func alpha() -> Int { return 1; }\"}"
    );
    if (response != "{\"protocol\":1,\"id\":2,\"result\":null}") {
        builtin.print("FAIL: wlls document open");
        return 1;
    }

    response = service.handle(
        "{\"protocol\":1,\"id\":3,\"method\":\"textDocument/documentSymbols\"," +
        "\"path\":\"memory.wl\"}"
    );
    let expected -> String =
        "{\"protocol\":1,\"id\":3,\"result\":[{\"name\":\"alpha\",\"kind\":\"function\"," +
        "\"range\":{\"start\":{\"line\":0,\"character\":5}," +
        "\"end\":{\"line\":0,\"character\":10}},\"children\":[]}]}";
    if (response != expected) {
        builtin.print("FAIL: wlls document symbols");
        return 1;
    }

    response = service.handle(
        "{\"protocol\":1,\"id\":4,\"method\":\"textDocument/change\"," +
        "\"path\":\"memory.wl\",\"version\":0,\"text\":\"\"}"
    );
    if (!response.starts_with(
        "{\"protocol\":1,\"id\":4,\"error\":{\"code\":\"staleDocument\""
    )) {
        builtin.print("FAIL: wlls stale document guard");
        return 1;
    }

    response = service.handle(
        "{\"protocol\":1,\"id\":5,\"method\":\"textDocument/change\"," +
        "\"path\":\"memory.wl\",\"version\":2,\"text\":\"func broken( -> Int {\"}"
    );
    response = service.handle(
        "{\"protocol\":1,\"id\":6,\"method\":\"textDocument/diagnostics\"," +
        "\"path\":\"memory.wl\"}"
    );
    if (!response.starts_with(
        "{\"protocol\":1,\"id\":6,\"result\":[{\"severity\":\"error\""
    )) {
        builtin.print("FAIL: wlls diagnostics");
        return 1;
    }

    response = service.handle(
        "{\"protocol\":1,\"id\":7,\"method\":\"textDocument/change\"," +
        "\"path\":\"memory.wl\",\"version\":3," +
        "\"text\":\"func healthy() -> Int { return 0; }\"}"
    );
    response = service.handle(
        "{\"protocol\":1,\"id\":8,\"method\":\"textDocument/diagnostics\"," +
        "\"path\":\"memory.wl\"}"
    );
    if (response != "{\"protocol\":1,\"id\":8,\"result\":[]}") {
        builtin.print("FAIL: wlls diagnostic recovery");
        return 1;
    }

    response = service.handle(
        "{\"protocol\":1,\"id\":9,\"method\":\"textDocument/change\"," +
        "\"path\":\"memory.wl\",\"version\":4," +
        "\"text\":\"const BASE -> Int = 1; func main() -> Int { return BASE; }\"}"
    );
    response = service.handle(
        "{\"protocol\":1,\"id\":10,\"method\":\"textDocument/definition\"," +
        "\"path\":\"memory.wl\",\"line\":0,\"character\":51}"
    );
    if (response !=
        "{\"protocol\":1,\"id\":10,\"result\":{\"path\":\"memory.wl\"," +
        "\"range\":{\"start\":{\"line\":0,\"character\":6}," +
        "\"end\":{\"line\":0,\"character\":10}}}}") {
        builtin.print("FAIL: wlls definition");
        return 1;
    }

    response = service.handle(
        "{\"protocol\":1,\"id\":11,\"method\":\"textDocument/open\"," +
        "\"path\":\"project/math.wl\",\"version\":1," +
        "\"text\":\"func add(left -> Int, right -> Int) -> Int { return left + right; }\"}"
    );
    response = service.handle(
        "{\"protocol\":1,\"id\":12,\"method\":\"textDocument/open\"," +
        "\"path\":\"project/main.wl\",\"version\":1," +
        "\"text\":\"import add from \\\"math.wl\\\"\\n" +
        "func main() -> Int { return add(1, 2); }\"}"
    );
    response = service.handle(
        "{\"protocol\":1,\"id\":13,\"method\":\"textDocument/definition\"," +
        "\"path\":\"project/main.wl\",\"line\":1,\"character\":28}"
    );
    if (response !=
        "{\"protocol\":1,\"id\":13,\"result\":{\"path\":\"project/math.wl\"," +
        "\"range\":{\"start\":{\"line\":0,\"character\":5}," +
        "\"end\":{\"line\":0,\"character\":8}}}}") {
        builtin.print("FAIL: wlls cross-file definition");
        return 1;
    }

    response = service.handle(
        "{\"protocol\":1,\"id\":14,\"method\":\"textDocument/semanticTokens\"," +
        "\"path\":\"memory.wl\"}"
    );
    if (!response.starts_with(
        "{\"protocol\":1,\"id\":14,\"result\":["
    )) {
        builtin.print("FAIL: wlls semantic tokens");
        return 1;
    }

    response = service.handle(
        "{\"protocol\":1,\"id\":15,\"method\":\"shutdown\"}"
    );
    if (!service.shutdown_requested ||
        response != "{\"protocol\":1,\"id\":15,\"result\":null}") {
        builtin.print("FAIL: wlls shutdown");
        return 1;
    }

    builtin.print("PASS: wlls protocol");
    return 0;
}
