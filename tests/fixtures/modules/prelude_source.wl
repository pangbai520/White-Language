// Support: MODULE_PRELUDE_SOURCE
// File: tests/fixtures/modules/prelude_source.wl
// Focus: Using prelude symbols from an imported module without explicit imports.

func check_module_prelude() -> Bool {
    let values -> Dict = Dict(1);
    values.put("ready", true);
    print("PASS: imported module prelude");
    return values.contains_key("ready") && Error.InvalidArgument != Error.None;
}
