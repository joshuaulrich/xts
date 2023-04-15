if (isTRUE(Sys.getenv("XTS_TEST_LONG_VECTOR_SUPPORT", FALSE))) {
    n <- 1.1e9
    x <- xts::.xts(matrix(seq_len(n*2), nrow = n, ncol = 2), seq_len(n))
    
    y <- merge(x, x[,2])
    y <- zoo::coredata(x)
    y <- lag(x)
    y <- diff(x)  # due to lag.xts()
    y <- x[ seq_len(n*2),]
    y <- x[-seq_len(n*2),]
    xts::isOrdered(seq_len(n*2))
    xts:::naCheck(seq_len(n*2))
    y <- rbind(x, x)     ####  negative length vectors are not allowed
    y <- xts::make.index.unique(x)
    
    # zoo::na.locf(x)   ### segfaults
    # na.omit(x)        ### killed
}
