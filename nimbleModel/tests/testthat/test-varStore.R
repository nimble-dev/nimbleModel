# https://stackoverflow.com/questions/37462850/r-r6-operator-overloading

## initial examples -- need to be converted into tests and expanded

## creation and subsetting
a = varStoreClass$new(c(3,6,9, 12, 15))
a[1:2]
a[c(1,3)]
a = varStoreClass$new(rep(6,3))
a[1:2]
a[1:2, expand = TRUE]
a[c(1,3), expand = TRUE]
a = varStoreClass$new(matrix(rnorm(4), 2, 2))
a[1,2]
a[1,1:2]
a[,1]
a = varStoreClass$new(5, c(2,2,2))

a = varStoreClass$new(matrix(7, 500, 500))

## subset replacement
a = varStoreClass$new(rep(6,3))
a[2] <- 4
a[2:3] <- 4
a = varStoreClass$new(c(3,6,9, 12, 15))
a[c(3,5)] <- 7
a[] <- 9

a = varStoreClass$new(matrix(rnorm(4), 2, 2))
a[2,] <- 3
a[1:2,1] <- 5
a[,2] <- 5
