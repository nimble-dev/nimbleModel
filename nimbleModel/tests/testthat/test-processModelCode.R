context("processModelCode")

test_that("processModelCode works",
{
    modelCode <- quote({
        a <- dnorm(0, 1)
    })
    modelDef <- modelDefClass$new(modelCode)
    modelDef$processModelCode()
}
)
