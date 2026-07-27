// language server request loop
import "../protocol/_pkg.wl" as protocol
import "../workspace/_pkg.wl" as workspace
import "../frontend/_pkg.wl" as source
import "../analysis/_pkg.wl" as analysis

class Server {
    let workspace -> workspace.Workspace = null;
    let initialized -> Bool = false;
    let shutdown_requested -> Bool = false;

    init() {
        self.workspace = workspace.Workspace();
    }

    method response(id -> Int, result -> String) -> String {
        return "{\"protocol\":1,\"id\":" + id + ",\"result\":" + result + "}";
    }

    method error_response(id -> Int, code -> String, message -> String) -> String {
        return "{\"protocol\":1,\"id\":" + id +
               ",\"error\":{\"code\":" + protocol.quote(code) +
               ",\"message\":" + protocol.quote(message) + "}}";
    }

    method handle(message -> String) -> String {
        let request -> protocol.Request = protocol.decode_request(message)?;
        catch(err) {
            return self.error_response(0, "invalidRequest", "Request body is not valid JSON.");
        }

        let id -> Int = request.int("id", 0);
        let version -> Int = request.int("protocol", 0);
        let method_name -> String = request.string("method");
        if (version != 1) {
            return self.error_response(id, "unsupportedProtocol", "Expected language server protocol version 1.");
        }
        if (method_name is null) {
            return self.error_response(id, "invalidRequest", "Request method is missing.");
        }

        if (method_name == "initialize") {
            self.initialized = true;
            return self.response(
                id,
                "{\"name\":\"wlls\",\"protocol\":1,\"capabilities\":" +
                "{\"documentSync\":true,\"diagnostics\":true," +
                "\"documentSymbols\":true,\"definition\":true," +
                "\"semanticTokens\":{\"full\":true,\"delta\":false," +
                "\"tokenTypes\":[\"keyword\",\"type\",\"class\",\"struct\"," +
                "\"interface\",\"enum\",\"enumMember\",\"function\",\"method\"," +
                "\"parameter\",\"variable\",\"property\",\"string\",\"number\"," +
                "\"comment\",\"operator\",\"annotation\"]," +
                "\"tokenModifiers\":[\"declaration\",\"definition\",\"readonly\"," +
                "\"static\",\"defaultLibrary\"]}}}"
            );
        }
        if (!self.initialized) {
            return self.error_response(id, "notInitialized", "Initialize the language server before sending requests.");
        }
        if (method_name == "shutdown") {
            self.shutdown_requested = true;
            return self.response(id, "null");
        }

        let path -> String = request.string("path");
        if (path is null || path.length() == 0) {
            return self.error_response(id, "invalidParams", "Document path is missing.");
        }

        if (method_name == "textDocument/open" || method_name == "textDocument/change") {
            let text -> String = request.string("text");
            let document_version -> Int = request.int("version", 0);
            if (text is null) {
                return self.error_response(id, "invalidParams", "Document text is missing.");
            }
            let current -> workspace.Document = self.workspace.find(path);
            if (current is !null && document_version < current.version) {
                return self.error_response(id, "staleDocument", "Document version is older than the open version.");
            }
            self.workspace.open(path, document_version, text);
            return self.response(id, "null");
        }
        if (method_name == "textDocument/close") {
            self.workspace.close(path);
            return self.response(id, "null");
        }
        if (method_name == "textDocument/diagnostics" ||
            method_name == "textDocument/documentSymbols" ||
            method_name == "textDocument/definition" ||
            method_name == "textDocument/semanticTokens") {
            let document -> workspace.Document = self.workspace.find(path);
            if (document is null) {
                return self.error_response(id, "documentNotOpen", "Open the document before querying it.");
            }
            let checked -> source.FrontendResult = document.result;

            if (method_name == "textDocument/diagnostics") {
                let encoded -> String =
                    analysis.encode_diagnostics(checked.diagnostics);
                return self.response(id, encoded);
            }

            if (!checked.valid) {
                return self.error_response(
                    id,
                    "analysisFailed",
                    "Document contains " + checked.diagnostics.length() + " syntax error(s)."
                );
            }
            let encoded -> String = "";
            if (method_name == "textDocument/documentSymbols") {
                encoded = analysis.encode_symbols(checked.syntax.symbols);
            } else if (method_name == "textDocument/definition") {
                let line -> Int = request.int("line", -1);
                let character -> Int = request.int("character", -1);
                if (line < 0 || character < 0) {
                    return self.error_response(
                        id,
                        "invalidParams",
                        "Definition requests require a non-negative line and character."
                    );
                }
                let definition -> source.SymbolDefinition =
                    self.workspace.frontend.definition(
                        document.path,
                        line,
                        character
                );
                encoded = analysis.encode_definition(definition);
            } else {
                encoded = analysis.encode_semantic_tokens(
                    analysis.semantic_tokens(
                        checked,
                        self.workspace.frontend,
                        document.path
                    )
                );
            }
            return self.response(id, encoded);
        }
        return self.error_response(id, "methodNotFound", "Unknown language server method '" + method_name + "'.");
    }
}

func run() -> Int? {
    let server -> Server = Server();
    while (!server.shutdown_requested) {
        let message -> String = protocol.read_message()?;
        if (message.length() == 0) { return 0; }
        let response -> String = server.handle(message);
        protocol.write_message(response)?;
    }
    return 0;
}
