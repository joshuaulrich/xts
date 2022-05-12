# run package unit tests
if (requireNamespace("tinytest", quietly = TRUE)) {
    tinytest::test_package("xts")
}

