context("processModelCode")

test_that("processModelCode works in simplest case",
{
    modelCode <- quote({
        a ~ dnorm(0, 1)
    })
    modelDef <- modelDefClass$new(modelCode)
    modelDef$processModelCode()
    expect_equal(
        modelDef$declInfo[[1]]$symbolicParentNodes,
        NULL
    )
}
)

test_that("processModelCode works",
{
    modelCode <- quote({
        for(i in 1:10)
            logit(a[i]) ~ dnorm(mu[i], tau)
    })
    modelDef <- modelDefClass$new(modelCode)
    modelDef$processModelCode()
    expect_equal(
        modelDef$declInfo[[1]]
       ,
       {
           test2 <- modelDeclClass$new()
           test2$setup(
               quote(logit(a[i]) ~ dnorm(mu[i], tau)),
               modelContextClass$new(
                   list(
                       quote(for(i in (1):(10)){})
                   )),
               2)
           test2
       }
    )
}
)

test_that("makeDowntreamRules works",
{
    modelCode <- quote({
        for(i in 1:10)
            logit(a[i]) ~ dnorm(mu[i], tau)
    })
    modelDef <- modelDefClass$new(modelCode)
    modelDef$processModelCode()
    modelDef$declInfo[[1]]$makeDownstreamRules()
    
    expect_equal(
        modelDef$declInfo[[1]]
       ,
       {
           test2 <- modelDeclClass$new()
           test2$setup(
               quote(logit(a[i]) ~ dnorm(mu[i], tau)),
               modelContextClass$new(
                   list(
                       quote(for(i in (1):(10)){})
                   )),
               2)
           test2
       }
    )
}
)
