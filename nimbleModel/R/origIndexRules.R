originalIndexRuleClass <- R6Class(
    classname = "originalIndexRuleClass",
    portable = FALSE,
    public = list(
        graphRule = NULL,
        initialize = function(LHS,
                              context,
                              constants = list()) {
            if(length(context$indexVarNames)) {
                dummyLHS <- parse(text = paste0("w[",
                                                paste(context$indexVarNames, collapse = ","), "]"))[[1]]
                } else dummyLHS <- quote(w)
            graphRule <<-
                makeGraphIndexRules(dummyLHS,
                                    LHS,
                                    context,
                                    constants)
        },
        apply = function(fromVarRange) {
            applyGraphIndexRules(
                fromVarRange,
                graphRule
            )
        }
    )
)

## open issues:

## y[5:7] ~ dmnorm()
## is there an orig index rule?
## if not, how handle finding calc for y[6:8] (i.e, offset/partial)

## some tests

if(FALSE) {
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:10){}))
    
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:5){}))

    context_i <- modelContextClass$new(list(singleContext1))
    
    context_ij <- modelContextClass$new(list(singleContext1,
                                             singleContext2))
    

rules <- originalIndexRuleClass$new(LHS = quote(y[i+1]),
                                 context = context_i)

    expect_equal(
        rules$apply(
            varRangeClass$new(list(
                              indexRange(quote(3:6))))),
        varRangeClass$new(list(
                          indexRange(quote(2:5))))
    )


    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:3){}))
    
    context_i <- modelContextClass$new(list(singleContext1))
 k <- c(5,1,3)
    rules <- originalIndexRuleClass$new(LHS = quote(y[k[i]]),
                                    context = context_i,
                                    constants = list(k = k))

    expect_equal(
        rules$apply(
            varRangeClass$new(list(
                              indexRange(quote(3:5))))),
        varRangeClass$new(list(
                          indexRange(matrix(c(3,1), ncol = 1))))
    )


    rules <- originalIndexRuleClass$new(LHS = quote(y[j, i+1]),
                                    context = context_ij)

    expect_equal(
        rules$apply(
            varRangeClass$new(list(
                              indexRange(quote(3:5)),
                              indexRange(quote(2:3))))),
        varRangeClass$new(list(
                          indexRange(quote(1:2)),
                          indexRange(quote(3:5))))
    )

    expect_equal(
        rules$apply(
            varRangeClass$new(list(
                              indexRange(matrix(c(8,4,3,2), ncol = 2))))),
        varRangeClass$new(list(
                          indexRange(matrix(c(1,4),nrow = 1))))
    )

    n <- c(1,3,2)
    singleContext1 <-
        modelSingleContext(forCode = quote(for(i in 1:3){}))
    
    singleContext2 <-
        modelSingleContext(forCode = quote(for(j in 1:n[i]){}))

    context_ijni <- modelContextClass$new(list(singleContext1,
                                             singleContext2))

rules <- originalIndexRuleClass$new(LHS = quote(y[j, i+1]),
                                    context = context_ijni)
    expect_equal(
        rules$apply(
            varRangeClass$new(list(
                              indexRange(quote(1:2)),
                              indexRange(quote(1:3))))),
        varRangeClass$new(list(
                          indexRange(matrix(c(1,2,2,1,1,2), ncol = 2))))
    )
}
