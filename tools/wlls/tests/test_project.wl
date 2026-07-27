// Test: FRONTEND_PROJECT
// File: tools/wlls/tests/test_project.wl
// Focus: Resolving named, qualified, and star imports across open source files.

import "builtin"
import "../internal/frontend/_pkg.wl" as source

func main() -> Int {
    let project -> source.FrontendWorkspace = source.FrontendWorkspace();
    project.update(
        "project/math.wl",
        1,
        "func add(left -> Int, right -> Int) -> Int { return left + right; }\n"
    );

    project.update(
        "project/named.wl",
        1,
        "import add from \"math.wl\"\n" +
        "func main() -> Int { return add(1, 2); }\n"
    );
    let named -> source.SymbolDefinition =
        project.definition("project/named.wl", 1, 28);
    if (named is null) {
        builtin.print("FAIL: named import missing");
        return 1;
    }
    if (named.name != "add" ||
        named.range.file != "project/math.wl" ||
        named.range.start.utf16_column != 5 ||
        project.type_name("project/named.wl", 1, 28) != "Int") {
        builtin.print("FAIL: named import definition");
        return 1;
    }

    project.update(
        "project/qualified.wl",
        1,
        "import \"math.wl\" as math\n" +
        "func main() -> Int { return math.add(1, 2); }\n"
    );
    let qualified -> source.SymbolDefinition =
        project.definition("project/qualified.wl", 1, 33);
    if (qualified is null) {
        builtin.print("FAIL: qualified import missing");
        return 1;
    }
    if (qualified.name != "add" ||
        qualified.range.file != "project/math.wl") {
        builtin.print("FAIL: qualified import definition");
        return 1;
    }

    project.update(
        "project/star.wl",
        1,
        "import * from \"math.wl\"\n" +
        "func main() -> Int { return add(1, 2); }\n"
    );
    let star -> source.SymbolDefinition =
        project.definition("project/star.wl", 1, 28);
    if (star is null ||
        star.name != "add" ||
        star.range.file != "project/math.wl") {
        builtin.print("FAIL: star import definition");
        return 1;
    }

    project.update(
        "project/constants.wl",
        1,
        "const LIMIT -> Int = 8;\n"
    );
    project.update(
        "project/multi_star.wl",
        1,
        "import * from \"math.wl\"\n" +
        "import * from \"constants.wl\"\n" +
        "func main() -> Int { return LIMIT; }\n"
    );
    let later_star -> source.SymbolDefinition =
        project.definition("project/multi_star.wl", 2, 28);
    if (later_star is null ||
        later_star.name != "LIMIT" ||
        later_star.range.file != "project/constants.wl") {
        builtin.print("FAIL: multiple star imports");
        return 1;
    }

    builtin.print("PASS: frontend project");
    return 0;
}
