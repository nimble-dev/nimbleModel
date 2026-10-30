# Runners: each runs the iterations for one instruction.
#   At each iteration, the "f" called is calc_one, sim_one, etc.
run_instr <- function(instr, f) {
  switch(as.character(instr$instr_type),
    "0" = calc_0(instr, f),
    "1" = calc_1_seq(instr, f),
    "2" = calc_1_mat(instr, f),
    "3" = calc_1_matp(instr, f),
    "4" = calc_2_seq_seq(instr, f),
    "5" = calc_2_seq_mat(instr, f),
    "6" = calc_2_mat_seq(instr, f),
    "7" = calc_2_mat_mat(instr, f),
    "8" = calc_2_seq_matp(instr, f),
    "9" = calc_2_matp_seq(instr, f),
    "10" = calc_2_matp_matp(instr, f),
    "11" = calc_2_x_y_ord(instr, f),
    "12" = calc_3_allseq(instr, f),
    "13" = calc_3_generic(instr, f),
    "14" = calc_4_allseq(instr, f),
    "15" = calc_4_generic(instr, f),
    "16" = calc_5_allseq(instr, f),
    "17" = calc_5_generic(instr, f),
    "18" = calc_1_matp_ord(instr, f),
    stop("declaration for type ", instr$instr_type, " no implemented")
  )
}

calc_0 <- function(instr, f) {
  return(f(0))
}
calc_1_seq <- function(instr, f) {
  logProb <- 0
  iStart <- instr$values[[1]][1]
  for (i in iStart:(iStart + instr$lens[1] - 1)) {
    logProb <- logProb + f(i)
  }
  return(logProb)
}
calc_1_mat <- function(instr, f) {
  logProb <- 0
  for (i in 1:instr$lens[1]) {
    logProb <- logProb + f(instr$values[[1]][i])
  }
  return(logProb)
}
calc_1_matp <- function(instr, f) {
  logProb <- 0
  for (i in 1:instr$lens[1]) {
    logProb <- logProb + f(instr$values[[1]][(instr$dims[1] * (i - 1) + 1):(instr$dims[1] * i)])
  }
  return(logProb)
}
calc_1_matp_ord <- function(instr, f) {
  logProb <- 0
  idx <- rep(0, instr$dims[1])
  for (i in 1:instr$lens[1]) {
    idx[instr$slots] <- instr$values[[1]][(instr$dims[1] * (i - 1) + 1):(instr$dims[1] * i)]
    logProb <- logProb + f(idx)
  }
  return(logProb)
}
calc_2_seq_seq <- function(instr, f) {
  logProb <- 0
  idx <- rep(0, 2)
  iStart1 <- instr$values[[1]][1]
  iStart2 <- instr$values[[2]][1]
  for (i in iStart1:(iStart1 + instr$lens[1] - 1)) {
    idx[1] <- i
    for (j in iStart2:(iStart2 + instr$lens[2] - 1)) {
      idx[2] <- j
      logProb <- logProb + f(idx)
    }
  }
  return(logProb)
}
calc_2_seq_mat <- function(instr, f) {
  logProb <- 0
  idx <- rep(0, 2)
  iStart1 <- instr$values[[1]][1]
  for (i in iStart1:(iStart1 + instr$lens[1] - 1)) {
    idx[1] <- i
    for (j in 1:instr$lens[2]) {
      idx[2] <- instr$values[[2]][j]
      logProb <- logProb + f(idx)
    }
  }
  return(logProb)
}
calc_2_mat_seq <- function(instr, f) {
  logProb <- 0
  idx <- rep(0, 2)
  iStart2 <- instr$values[[2]][1]
  for (i in 1:instr$lens[1]) {
    idx[1] <- instr$values[[1]][i]
    for (j in iStart2:(iStart2 + instr$lens[2] - 1)) {
      idx[2] <- j
      logProb <- logProb + f(idx)
    }
  }
  return(logProb)
}
calc_2_mat_mat <- function(instr, f) {
  logProb <- 0
  idx <- rep(0, 2)
  for (i in 1:instr$lens[1]) {
    idx[1] <- instr$values[[1]][i]
    for (j in 1:instr$lens[2]) {
      idx[2] <- instr$values[[2]][j]
      logProb <- logProb + f(idx)
    }
  }
  return(logProb)
}
calc_2_seq_matp <- function(instr, f) {
  logProb <- 0
  idx <- rep(0, instr$nDim)
  iStart1 <- instr$values[[1]][1]
  for (i in iStart1:(iStart1 + instr$lens[1] - 1)) {
    idx[instr$slots[1]] <- i
    for (j in 1:instr$lens[2]) {
      idx[instr$slots[2:instr$nDim]] <- instr$values[[2]][(instr$dims[2] * (j - 1) + 1):(instr$dims[2] * j)]
      logProb <- logProb + f(idx)
    }
  }
  return(logProb)
}
calc_2_matp_seq <- function(instr, f) {
  logProb <- 0
  idx <- rep(0, instr$nDim)
  iStart2 <- instr$values[[2]][1]
  for (i in 1:instr$lens[1]) {
    idx[instr$slots[1:instr$dims[1]]] <- instr$values[[1]][(instr$dims[1] * (i - 1) + 1):(instr$dims[1] * i)]
    for (j in iStart2:(iStart2 + instr$lens[2] - 1)) {
      idx[instr$slots[instr$nDim]] <- j
      logProb <- logProb + f(idx)
    }
  }
  return(logProb)
}
calc_2_matp_matp <- function(instr, f) {
  logProb <- 0
  idx <- rep(0, instr$nDim)
  for (i in 1:instr$lens[1]) {
    idx[instr$slots[1:instr$dims[1]]] <- instr$values[[1]][(instr$dims[1] * (i - 1) + 1):(instr$dims[1] * i)]
    for (j in 1:instr$lens[2]) {
      idx[instr$slots[(instr$dims[1] + 1):instr$nDim]] <- instr$values[[2]][(instr$dims[2] * (j - 1) + 1):(instr$dims[2] * j)]
      logProb <- logProb + f(idx)
    }
  }
  return(logProb)
}
calc_2_x_y_ord <- function(instr, f) {
  logProb <- 0
  idx <- rep(0, 2)
  iStart1 <- instr$values[[1]][1] - 1
  iStart2 <- instr$values[[2]][1] - 1
  index_types1 <- instr$index_types[1]
  index_types2 <- instr$index_types[2]
  for (i in 1:instr$lens[1]) {
    if (index_types1 == 1) idx[2] <- iStart1 + i else idx[2] <- instr$values[[1]][i]
    for (j in 1:instr$lens[2]) {
      if (index_types2 == 1) idx[1] <- iStart2 + j else idx[1] <- instr$values[[2]][j]
      logProb <- logProb + f(idx)
    }
  }
  return(logProb)
}
calc_3_allseq <- function(instr, f) {
  logProb <- 0
  idx <- rep(0, 3)
  iStart1 <- instr$values[[1]][1]
  iStart2 <- instr$values[[2]][1]
  iStart3 <- instr$values[[3]][1]
  for (i in iStart1:(iStart1 + instr$lens[1] - 1)) {
    idx[1] <- i
    for (j in iStart2:(iStart2 + instr$lens[2] - 1)) {
      idx[2] <- j
      for (k in iStart3:(iStart3 + instr$lens[3] - 1)) {
        idx[3] <- k
        logProb <- logProb + f(idx)
      }
    }
  }
  return(logProb)
}
calc_3_generic <- function(instr, f) {
  logProb <- 0
  idx <- rep(0, instr$nDim)
  iStart1 <- instr$values[[1]][1] - 1
  iStart2 <- instr$values[[2]][1] - 1
  iStart3 <- instr$values[[3]][1] - 1
  index_types1 <- instr$index_types[1]
  index_types2 <- instr$index_types[2]
  index_types3 <- instr$index_types[3]
  cumdim2 <- instr$dims[1] + instr$dims[2]
  for (i in 1:instr$lens[1]) {
    if (index_types1 == 1) {
      idx[instr$slots[1]] <- iStart1 + i
    } else {
      idx[instr$slots[1:instr$dims[1]]] <- instr$values[[1]][(instr$dims[1] * (i - 1) + 1):(instr$dims[1] * i)]
    }
    for (j in 1:instr$lens[2]) {
      if (index_types2 == 1) {
        idx[instr$slots[instr$dims[1] + 1]] <- iStart2 + j
      } else {
        idx[instr$slots[(instr$dims[1] + 1):cumdim2]] <- instr$values[[2]][(instr$dims[2] * (j - 1) + 1):(instr$dims[2] * j)]
      }
      for (k in 1:instr$lens[3]) {
        if (index_types3 == 1) {
          idx[instr$slots[instr$nDim]] <- iStart3 + k
        } else {
          idx[instr$slots[(cumdim2 + 1):instr$nDim]] <- instr$values[[3]][(instr$dims[3] * (k - 1) + 1):(instr$dims[3] * k)]
        }
        logProb <- logProb + f(idx)
      }
    }
  }
  return(logProb)
}
calc_4_allseq <- function(instr, f) {
  logProb <- 0
  idx <- rep(0, 4)
  iStart <- sapply(instr$values, `[`, 1)
  for (i1 in iStart[1]:(iStart[1] + instr$lens[1] - 1)) {
    idx[1] <- i1
    for (i2 in iStart[2]:(iStart[2] + instr$lens[2] - 1)) {
      idx[2] <- i2
      for (i3 in iStart[3]:(iStart[3] + instr$lens[3] - 1)) {
        idx[3] <- i3
        for (i4 in iStart[4]:(iStart[4] + instr$lens[4] - 1)) {
          idx[4] <- i4
          logProb <- logProb + f(idx)
        }
      }
    }
  }
  return(logProb)
}
calc_4_generic <- function(instr, f) {
  logProb <- 0
  idx <- rep(0, instr$nDim)
  iStart <- sapply(instr$values, function(x) x[1] - 1)
  cumdim2 <- instr$dims[1] + instr$dims[2]
  cumdim3 <- instr$dims[1] + instr$dims[2] + instr$dims[3]
  for (i1 in 1:instr$lens[1]) {
    if (instr$index_types[1] == 1) {
      idx[instr$slots[1]] <- iStart[1] + i1
    } else {
      idx[instr$slots[1:instr$dims[1]]] <- instr$values[[1]][(instr$dims[1] * (i1 - 1) + 1):(instr$dims[1] * i1)]
    }
    for (i2 in 1:instr$lens[2]) {
      if (instr$index_types[2] == 1) {
        idx[instr$slots[instr$dims[1] + 1]] <- iStart[2] + i2
      } else {
        idx[instr$slots[(instr$dims[1] + 1):cumdim2]] <- instr$values[[2]][(instr$dims[2] * (i2 - 1) + 1):(instr$dims[2] * i2)]
      }
      for (i3 in 1:instr$lens[3]) {
        if (instr$index_types[3] == 1) {
          idx[instr$slots[cumdim2 + 1]] <- iStart[3] + i3
        } else {
          idx[instr$slots[(cumdim2 + 1):cumdim3]] <- instr$values[[3]][(instr$dims[3] * (i3 - 1) + 1):(instr$dims[3] * i3)]
        }
        for (i4 in 1:instr$lens[4]) {
          if (instr$index_types[4] == 1) {
            idx[instr$slots[cumdim3 + 1]] <- iStart[4] + i4
          } else {
            idx[instr$slots[(cumdim3 + 1):instr$nDim]] <- instr$values[[4]][(instr$dims[4] * (i4 - 1) + 1):(instr$dims[4] * i4)]
          }
          logProb <- logProb + f(idx)
        }
      }
    }
  }
  return(logProb)
}
calc_5_allseq <- function(instr, f) {
  logProb <- 0
  idx <- rep(0, 4)
  iStart <- sapply(instr$values, `[`, 1)
  for (i1 in iStart[1]:(iStart[1] + instr$lens[1] - 1)) {
    idx[1] <- i1
    for (i2 in iStart[2]:(iStart[2] + instr$lens[2] - 1)) {
      idx[2] <- i2
      for (i3 in iStart[3]:(iStart[3] + instr$lens[3] - 1)) {
        idx[3] <- i3
        for (i4 in iStart[4]:(iStart[4] + instr$lens[4] - 1)) {
          idx[4] <- i4
          for (i5 in iStart[5]:(iStart[5] + instr$lens[5] - 1)) {
            idx[5] <- i5
            logProb <- logProb + f(idx)
          }
        }
      }
    }
  }
  return(logProb)
}
calc_5_generic <- function(instr, f) {
  logProb <- 0
  idx <- rep(0, instr$nDim)
  iStart <- sapply(instr$values, function(x) x[1] - 1)
  cumdim2 <- instr$dims[1] + instr$dims[2]
  cumdim3 <- instr$dims[1] + instr$dims[2] + instr$dims[3]
  cumdim4 <- instr$dims[1] + instr$dims[2] + instr$dims[3] + instr$dims[4]
  for (i1 in 1:instr$lens[1]) {
    if (instr$index_types[1] == 1) {
      idx[instr$slots[1]] <- iStart[1] + i1
    } else {
      idx[instr$slots[1:instr$dims[1]]] <- instr$values[[1]][(instr$dims[1] * (i1 - 1) + 1):(instr$dims[1] * i1)]
    }
    for (i2 in 1:instr$lens[2]) {
      if (instr$index_types[2] == 1) {
        idx[instr$slots[instr$dims[1] + 1]] <- iStart[2] + i2
      } else {
        idx[instr$slots[(instr$dims[1] + 1):cumdim2]] <- instr$values[[2]][(instr$dims[2] * (i2 - 1) + 1):(instr$dims[2] * i2)]
      }
      for (i3 in 1:instr$lens[3]) {
        if (instr$index_types[3] == 1) {
          idx[instr$slots[cumdim2 + 1]] <- iStart[3] + i3
        } else {
          idx[instr$slots[(cumdim2 + 1):cumdim3]] <- instr$values[[3]][(instr$dims[3] * (i3 - 1) + 1):(instr$dims[3] * i3)]
        }
        for (i4 in 1:instr$lens[4]) {
          if (instr$index_types[4] == 1) {
            idx[instr$slots[cumdim3 + 1]] <- iStart[4] + i4
          } else {
            idx[instr$slots[(cumdim3 + 1):cumdim4]] <- instr$values[[4]][(instr$dims[4] * (i4 - 1) + 1):(instr$dims[4] * i4)]
          }
          for (i5 in 1:instr$lens[5]) {
            if (instr$index_types[5] == 1) {
              idx[instr$slots[cumdim4 + 1]] <- iStart[5] + i5
            } else {
              idx[instr$slots[(cumdim4 + 1):instr$nDim]] <- instr$values[[5]][(instr$dims[5] * (i5 - 1) + 1):(instr$dims[5] * i5)]
            }
            logProb <- logProb + f(idx)
          }
        }
      }
    }
  }
  return(logProb)
}
