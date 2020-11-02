test_that("df2stack", {
  n=5
  x=data.frame(x=1:n,
               y=rep(1:2,each=2,len=n),
               pres=rbinom(n,size=1,p=0.1),
               v1=rnorm(n),
               v2=rnorm(n),
               v3=rnorm(n))

  x2=df2stack(x)
  expect_equal(coordinates(x2)[,1],1:5)
})
