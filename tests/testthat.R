Sys.setenv("R_TESTS" = "")
if(require(testthat) & require(hts))
  test_check("hts")
