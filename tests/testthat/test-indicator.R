test_indicator <- indicator (
  x <- data.frame ( 
    geo = rep(c("NL", "BE", "LU"), 4), 
    time = rep(c(2016:2019),3), 
    values = runif(12, 1,100)
  ), 
  shortcode = "observatory_test_1", 
  description = "A test indicator with random numbers", 
  date_created = as.Date ( "2020-08-24"),
  date_earliest  = min (x$time, na.rm=TRUE),
  date_latest  =  max(x$time, na.rm=TRUE),
  keyword1 = "test",  keyword2 = "random",  keyword3 = "Benelux"
)


test_that("indicator created", {
  expect_true( "indicator" %in% class ( test_indicator) )
  expect_true( attr(test_indicator, "keyword2") == "random" )
})

