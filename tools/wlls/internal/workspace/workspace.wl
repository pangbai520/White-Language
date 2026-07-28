// in-memory documents owned by one language server session
import Dict from "dict"
import "../frontend/_pkg.wl" as source

class Document {
    let path -> String;
    let version -> Int;
    let text -> String;
    let result -> source.FrontendResult;

    init(
        path -> String,
        version -> Int,
        text -> String,
        result -> source.FrontendResult
    ) {
        self.path = path;
        self.version = version;
        self.text = text;
        self.result = result;
    }
}

class Workspace {
    let documents -> Dict;
    let frontend -> source.FrontendWorkspace;

    init() {
        self.documents = Dict(16);
        self.frontend = source.FrontendWorkspace();
    }

    method find(path -> String) -> Document {
        return self.documents[path];
    }

    method open(path -> String, version -> Int, text -> String) -> Document {
        let result -> source.FrontendResult =
            self.frontend.update(path, version, text);
        let document -> Document = self.find(path);
        if (document is null) {
            document = Document(path, version, text, result);
            self.documents.put(path, document);
        } else {
            document.version = version;
            document.text = text;
            document.result = result;
        }
        return document;
    }

    method close(path -> String) -> Bool {
        if (!self.documents.contains_key(path)) { return false; }
        self.documents.remove(path);
        self.frontend.remove(path);
        return true;
    }
}
