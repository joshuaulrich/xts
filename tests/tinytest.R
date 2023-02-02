# run package unit tests
if (requireNamespace("tinytest", quietly = TRUE)) {
    use_color <- as.logical(Sys.getenv("_XTS_TINYTEST_COLOR_", FALSE))
    verbosity <- as.integer(Sys.getenv("_XTS_TINYTEST_VERBOSE_", 2))
    cat("using color:", use_color, "\nverbosity:", verbosity, "\n")
    tinytest::test_package("xts", color = use_color, verbose = verbosity)
}

