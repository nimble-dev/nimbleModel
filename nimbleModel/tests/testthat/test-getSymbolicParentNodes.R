context("getSymbolicParentNodes")

test_that("getSymbolicParentNodes works",
{
    expect_equal(
        test1 <- getSymbolicParentNodes(
            quote(foo(a, x[i] + w)),
            constNames = list(),
            indexNames = list(quote(i)),
            nimbleFunctionNames = list(quote(foo)),
            addDistNames = FALSE
        )
       ,
        test2 <- list(quote(a),
                      quote(x[i]),
                      quote(w))
    )
}
)
