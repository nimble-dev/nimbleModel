context("getSymbolicParentNodes")

test_that("getSymbolicParentNodes works", {
    expect_equal(
        getSymbolicParentNodes(
            quote(foo(a, x[i] * y[i+1] + w)),
            constNames = list(),
            indexNames = list(quote(i)),
            nimbleFunctionNames = list(quote(foo)),
            addDistNames = FALSE
        ),
        list(quote(a),
             quote(x[i]),
             quote(y[i+1]),
             quote(w))
    )
}))
