library(xts)
test_that("multiplication works", {

  df = data.frame(a = c(1:3),b = c(2:4))

  foo = function(x){data.frame(a = x$a,c=x$a + x$b)}

  df2 = rowMap(df,foo)

  expect_equal(as.vector(df2[["c"]])[2], 5)

  expect_class(df2,"data.frame")

  df3 = df[df$a > 9,]

  expect_error(rowMap(df3,foo))


  df4 = xts::as.xts(df,order.by = as.Date(1:nrow(df),origin = lubridate::origin))
  df5 = rowMap(df4,foo)

  expect_equal(as.vector(df5$c)[2], 5)

  expect_class(df5,"xts")

  df6 = df4[df$a > 9,]

  expect_error(rowMap(df6,foo))

  df7 = as.data.table(df)
  df8 = rowMap(df7,foo)

  expect_equal(as.vector(df8[["c"]])[2], 5)

  expect_class(df8,"data.table")

})


