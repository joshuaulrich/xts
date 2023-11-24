##
## Unit Test for timeSeries class from Rmetrics timeSeries package
## 
## 

if (requireNamespace("timeSeries", quietly = TRUE)) {
  data(sample_matrix)
  
  ###############################################################################
  ###############################################################################
  # 
  #  Multivariate timeSeries tests
  #
  ###############################################################################
  ###############################################################################
  
  ###############################################################################
  # create timeSeries object from sample_matrix
  sample.timeSeries <- timeSeries::timeSeries(sample_matrix,charvec=as.Date(rownames(sample_matrix)))
  ###############################################################################
  
  ###############################################################################
  # create corresponding 'xts' object
  sample.xts <- as.xts(sample.timeSeries)
  ###############################################################################
  
  ###############################################################################
  # test subsetting functionality of xts
  
  info_msg <- "test.convert_timeSeries_to_xts"
  expect_identical(sample.xts,as.xts(sample.timeSeries), info = info_msg)

  info_msg <- "test.convert_timeSeries_to_xts_j1"
  expect_identical(sample.xts[,1],as.xts(sample.timeSeries)[,1], info = info_msg)

  info_msg <- "test.convert_timeSeries_to_xts_i1"
  expect_identical(sample.xts[1,],as.xts(sample.timeSeries)[1,], info = info_msg)

  info_msg <- "test.convert_timeSeries_to_xts_i1j1"
  expect_identical(sample.xts[1,1],as.xts(sample.timeSeries)[1,1], info = info_msg)
  
  # end subsetting functionality
  ###############################################################################
  
  
  ###############################################################################
  # test 'reclass'
  
  info_msg <- "test.timeSeries_reclass"
  expect_identical(sample.timeSeries,reclass(try.xts(sample.timeSeries)), info = info_msg)

  info_msg <- "test.timeSeries_reclass_subset_reclass_j1"
  expect_identical(sample.timeSeries[,1],reclass(try.xts(sample.timeSeries))[,1], info = info_msg)

  info_msg <- "test.timeSeries_reclass_subset_as.xts_j1"
  spl <- sample.timeSeries[,1:2]
  respl <- reclass(try.xts(sample.timeSeries)[,1:2])
  # timeSeries fails to maintain @positions correctly if one column is selected
  
  expect_identical(spl,respl, info = info_msg)
  #expect_identical(1,1, info = info_msg)

  info_msg <- "test.timeSeries_reclass_subset_timeSeries_j1"
  spl <- sample.timeSeries[,1:2]
  respl <- reclass(try.xts(sample.timeSeries[,1:2]))
  # timeSeries fails to maintain @positions correctly if one column is selected
  
  expect_identical(spl,respl, info = info_msg)
  # expect_identical(1,1, info = info_msg)
  
  # end 'reclass' 
  ###############################################################################
  
  ###############################################################################
  ###############################################################################
  # 
  #  Univariate timeSeries tests
  #
  ###############################################################################
  ###############################################################################
  
  ###############################################################################
  # create timeSeries object from sample_matrix
  sample.timeSeries.univariate <- timeSeries::timeSeries(sample_matrix[,1],charvec=as.Date(rownames(sample_matrix)))
  ###############################################################################
  
  ###############################################################################
  # create corresponding 'xts' object
  sample.xts.univariate <- as.xts(sample.timeSeries.univariate)
  ###############################################################################
  
  ###############################################################################
  # test subsetting functionality of xts
  
  info_msg <- "test.convert_timeSeries.univariate_to_xts"
  expect_identical(sample.xts.univariate,as.xts(sample.timeSeries.univariate), info = info_msg)

  info_msg <- "test.convert_timeSeries.univariate_to_xts_j1"
  expect_identical(sample.xts.univariate[,1],as.xts(sample.timeSeries.univariate)[,1], info = info_msg)

  info_msg <- "test.convert_timeSeries.univariate_to_xts_i1"
  expect_identical(sample.xts.univariate[1,],as.xts(sample.timeSeries.univariate)[1,], info = info_msg)

  info_msg <- "test.convert_timeSeries.univariate_to_xts_i1j1"
  expect_identical(sample.xts.univariate[1,1],as.xts(sample.timeSeries.univariate)[1,1], info = info_msg)
  
  # end subsetting functionality
  ###############################################################################
}  # requireNamespace
