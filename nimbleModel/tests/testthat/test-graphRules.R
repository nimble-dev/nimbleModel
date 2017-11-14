context("graphRules")

test_that("makeSeparableIndexSets works", {
    singleContext1 <-
        modelSingleContext(indexVarExpr = quote(i),
                           indexRangeExpr = quote(1:10),
                           forCode = quote(for(i in 1:10){})[1:3])
    
    singleContext2 <-
        modelSingleContext(indexVarExpr = quote(j),
                           indexRangeExpr = quote(1:5),
                           forCode = quote(for(j in 1:5){})[1:3])
    
    singleContext3 <-
        modelSingleContext(indexVarExpr = quote(k),
                           indexRangeExpr = quote(1:5),
                           forCode = quote(for(k in 1:5){})[1:3])
    
    
    singleContext2ni <-
        modelSingleContext(indexVarExpr = quote(j),
                           indexRangeExpr = quote(1:n[i]),
                           forCode = quote(for(j in 1:n[i]){})[1:3])
    
    
    context_i <- modelContextClass$new(list(singleContext1))
    
    context_ij <- modelContextClass$new(list(singleContext1,
                                             singleContext2))
    
    context_ijk <- modelContextClass$new(list(singleContext1,
                                              singleContext2,
                                              singleContext3))
    
    context_ijni<- modelContextClass$new(list(singleContext1,
                                              singleContext2ni))
    
    expect_identical(makeSeparableIndexSets(quote(y[j, i + j]),
                                            quote(x[i, j]),
                                            context_ij)$indexVarNameSets,
                     list(c(i = "i", j = "j")))
    
    expect_identical(makeSeparableIndexSets(quote(y[j, i]),
                                            quote(x[i, 3]),
                                            context_ij)$indexVarNameSets,
                     list(c(i = "i"), c(j = "j")))
    
    expect_identical(makeSeparableIndexSets(quote(y[j, i]),
                                            quote(x[i, 1:3]),
                                            context_ij)$indexVarNameSets,
                     list(c(i = "i"), c(j = "j")))

    expect_identical(makeSeparableIndexSets(quote(y[j, i]),
                                            quote(x[i, ]),
                                            context_ij)$indexVarNameSets,
                     list(c(i = "i"), c(j = "j")))
    
    expect_identical(makeSeparableIndexSets(quote(y[j, i]),
                                            quote(x[i, const]),
                                            context_ij)$indexVarNameSets,
                     list(c(i = "i"), c(j = "j")))
    
    expect_identical(makeSeparableIndexSets(quote(y[j, j, k]),
                                            quote(x[k, i, j]),
                                            context_ijk)$indexVarNameSets,
                     list(c(i = "i"), c(j = "j"), c(k = "k")))
}
)
