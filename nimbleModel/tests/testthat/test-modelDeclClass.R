context("modelDeclClass")

test_that("modelDeclClass works",
{
    test1 <- modelDeclClass$new()
    ## get dnorm in getAllDistributionsInfo
    test1$setup(code = quote(a ~ dnorm(0, 1)),
                context = NULL,
                sourceLineNum = 2)
    test1$genSymbolicParentNodes(constantsNamesList = list(),
                                 nimFunNames = list())
    expect_identical(test1$symbolicParentNodes,
                     NULL)
}
)

test_that("modelDeclClass works",
{
    test1 <- modelDeclClass$new()
    ## get dnorm in getAllDistributionsInfo
    test1$setup(code = quote(a[i] ~ dnorm(b * const * mu[i+1], sigma)),
                context = modelContextClass$new(list(quote(for(i in 1:10){}))),
                sourceLineNum = 2)
    test1$genSymbolicParentNodes(constantsNamesList = list(quote(const)),
                                 nimFunNames = list())
    expect_identical(test1$symbolicParentNodes,
                     list(quote(b),
                          quote(mu[i+1]),
                          quote(sigma)
                          )
                     )
}
)
