// Test: PRIVATE_PACKAGE_MODULE
// File: tests/diagnostics/failures/test_private_package_module.wl
// Focus: Private package modules are not part of the package namespace

import "../../fixtures/modules/private_package/_pkg.wl" as sample

func main() -> Int {
    return sample.__secret.reveal();
}
