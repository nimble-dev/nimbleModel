#' @export
declFunBase_nClass <- nClass(
  classname = "declFunBase_nClass",
  Rpublic = list(
    calculate = function(instr) {
      calc_op(instr, "calc_one")
    },
    calculateDiff = function(instr) {
      calc_op(instr, "calcDiff_one")
    },
    getLogProb = function(instr) {
      calc_op(instr, "getLogProb_one")
    },
    calc_op = function(instr, fn) {
      if(instr$type == 0) return(calc_0(instr, fn))
      if(instr$type == 1) return(calc_1_seq(instr, fn))
      if(instr$type == 2) return(calc_1_mat(instr, fn))
      if(instr$type == 3) return(calc_1_matp(instr, fn))
      if(instr$type == 4) return(calc_2_seq_seq(instr, fn))
      if(instr$type == 5) return(calc_2_seq_mat(instr, fn))
      if(instr$type == 6) return(calc_2_mat_seq(instr, fn))
      if(instr$type == 7) return(calc_2_mat_mat(instr, fn))
      if(instr$type == 8) return(calc_2_seq_matp(instr, fn))
      if(instr$type == 9) return(calc_2_matp_seq(instr, fn))
      if(instr$type == 10) return(calc_2_matp_matp(instr, fn))
      if(instr$type == 11) return(calc_2_x_y_ord(instr, fn))
      if(instr$type == 12) return(calc_3_allseq(instr, fn))
      if(instr$type == 13) return(calc_3_generic(instr, fn))
      if(instr$type == 14) return(calc_4_allseq(instr, fn))
      if(instr$type == 15) return(calc_4_generic(instr, fn))
      if(instr$type == 16) return(calc_5_allseq(instr, fn))
      if(instr$type == 17) return(calc_5_generic(instr, fn))
      if(instr$type == 18) return(calc_1_matp_ord(instr, fn))
      stop("declaration for type ", instr$type, " no implemented")
    },
    calc_0 = function(instr, fn) {
      return(self[[fn]](0))
    },
    calc_1_seq =
    function(instr, fn) {
      logProb <- 0
      iStart <- instr$values[[1]][1]
      for(i in iStart:(iStart+instr$lens[1]-1))
        logProb <- logProb + self[[fn]](i)
      return(logProb)
    },
    calc_1_mat =
      function(instr, fn) {
        logProb <- 0
        for(i in 1:instr$lens[1])
          logProb <- logProb + self[[fn]](instr$values[[1]][i])
        return(logProb)
      },
    calc_1_matp =
      function(instr, fn) {
        logProb <- 0
        for(i in 1:instr$lens[1])
          logProb <- logProb + self[[fn]](instr$values[[1]][(instr$dims[1]*(i-1) + 1):(instr$dims[1]*i)])
        return(logProb)
      },
    calc_1_matp_ord =
      function(instr, fn) {
        logProb <- 0
        idx <- rep(0, instr$dims[1])
        for(i in 1:instr$lens[1]) {
          idx[instr$slots] <- instr$values[[1]][(instr$dims[1]*(i-1) + 1):(instr$dims[1]*i)]
          logProb <- logProb + self[[fn]](idx)
        }
        return(logProb)
      },
    calc_2_seq_seq =
        function(instr, fn) {
            logProb <- 0
            idx <- rep(0, 2)
            iStart1 <- instr$values[[1]][1]
            iStart2 <- instr$values[[2]][1]
            for(i in iStart1:(iStart1 + instr$lens[1] - 1)) {
                idx[1] <- i
                for(j in iStart2:(iStart2 + instr$lens[2] - 1)) {
                    idx[2] <- j
                    logProb <- logProb + self[[fn]](idx)
                }
            }
            return(logProb)
        },
    calc_2_seq_mat =
        function(instr, fn) {
            logProb <- 0
            idx <- rep(0, 2)
            iStart1 <- instr$values[[1]][1]
            for(i in iStart1:(iStart1 + instr$lens[1] - 1)) {
                idx[1] <- i
                for(j in 1:instr$lens[2]) {
                    idx[2] <- instr$values[[2]][j]
                    logProb <- logProb + self[[fn]](idx)
                }
            }
            return(logProb)
        },
    calc_2_mat_seq =
        function(instr, fn) {
            logProb <- 0
            idx <- rep(0, 2)
            iStart2 <- instr$values[[2]][1]
            for(i in 1:instr$lens[1]) {
                idx[1] <- instr$values[[1]][i]
                for(j in iStart2:(iStart2 + instr$lens[2] - 1)) {
                    idx[2] <- j
                    logProb <- logProb + self[[fn]](idx)
                }
            }
            return(logProb)
        },
    calc_2_mat_mat =
        function(instr, fn) {
            logProb <- 0
            idx <- rep(0, 2)
            for(i in 1:instr$lens[1]) {
                idx[1] <- instr$values[[1]][i]
                for(j in 1:instr$lens[2]) {
                    idx[2] <- instr$values[[2]][j]
                    logProb <- logProb + self[[fn]](idx)
                }
            }
            return(logProb)
        },
    calc_2_seq_matp =
        function(instr, fn) {
            logProb <- 0
            idx <- rep(0, instr$nDim)
            iStart1 <- instr$values[[1]][1]
            for(i in iStart1:(iStart1 + instr$lens[1] - 1)){
                idx[instr$slots[1]] <- i
                for(j in 1:instr$lens[2]) {
                    idx[instr$slots[2:instr$nDim]] <- instr$values[[2]][(instr$dims[2]*(j-1) + 1):(instr$dims[2]*j)]
                    logProb <- logProb + self[[fn]](idx)
                }
            }
            return(logProb)
        },
    calc_2_matp_seq =
        function(instr, fn) {
            logProb <- 0
            idx <- rep(0, instr$nDim)
            iStart2 <- instr$values[[2]][1]
            for(i in 1:instr$lens[1]) {
                idx[instr$slots[1:instr$dims[1]]] <- instr$values[[1]][(instr$dims[1]*(i-1) + 1):(instr$dims[1]*i)]
                for(j in iStart2:(iStart2 + instr$lens[2] - 1)) {
                    idx[instr$slots[instr$nDim]] <- j
                    logProb <- logProb + self[[fn]](idx)
                }
            }
            return(logProb)
        },
    calc_2_matp_matp =
        function(instr, fn) {
            logProb <- 0
            idx <- rep(0, instr$nDim)
            for(i in 1:instr$lens[1]) {
                idx[instr$slots[1:instr$dims[1]]] <- instr$values[[1]][(instr$dims[1]*(i-1) + 1):(instr$dims[1]*i)]
                for(j in 1:instr$lens[2]) {
                    idx[instr$slots[(instr$dims[1]+1):instr$nDim]] <- instr$values[[2]][(instr$dims[2]*(j-1) + 1):(instr$dims[2]*j)]
                    logProb <- logProb + self[[fn]](idx)
                }
            }
            return(logProb)
        },
    calc_2_x_y_ord =
        function(instr, fn) {
            logProb <- 0
            idx <- rep(0, 2)
            iStart1 <- instr$values[[1]][1]-1
            iStart2 <- instr$values[[2]][1]-1
            index_types1 <- instr$index_types[1]
            index_types2 <- instr$index_types[2]
            for(i in 1:instr$lens[1]) {
                if(index_types1 == 1) idx[2] <- iStart1 + i else idx[2] <- instr$values[[1]][i]
                for(j in 1:instr$lens[2]) {
                    if(index_types2 == 1) idx[1] <- iStart2 + j else idx[1] <- instr$values[[2]][j]
                    logProb <- logProb + self[[fn]](idx)
                }
            }
            return(logProb)
        },
    calc_3_allseq =
        function(instr, fn) {
            logProb <- 0
            idx <- rep(0, 3)
            iStart1 <- instr$values[[1]][1]
            iStart2 <- instr$values[[2]][1]
            iStart3 <- instr$values[[3]][1]
            for(i in iStart1:(iStart1 + instr$lens[1] - 1)) {
                idx[1] <- i
                for(j in iStart2:(iStart2 + instr$lens[2] - 1)) {
                    idx[2] <- j
                    for(k in iStart3:(iStart3 + instr$lens[3] - 1)) {
                        idx[3] <- k
                        logProb <- logProb + self[[fn]](idx)
                    }
                }
            }
            return(logProb)
        },
    calc_3_generic =
        function(instr, fn) {
            logProb <- 0
            idx <- rep(0, instr$nDim)
            iStart1 <- instr$values[[1]][1]-1
            iStart2 <- instr$values[[2]][1]-1
            iStart3 <- instr$values[[3]][1]-1
            index_types1 <- instr$index_types[1]
            index_types2 <- instr$index_types[2]
            index_types3 <- instr$index_types[3]
            cumdim2 <- instr$dims[1]+instr$dims[2]
            for(i in 1:instr$lens[1]) {
                if(index_types1 == 1) {
                    idx[instr$slots[1]] <- iStart1 + i
                } else idx[instr$slots[1:instr$dims[1]]] <- instr$values[[1]][(instr$dims[1]*(i-1)+1):(instr$dims[1]*i)]
                for(j in 1:instr$lens[2]) {
                    if(index_types2 == 1) {
                        idx[instr$slots[instr$dims[1]+1]] <- iStart2 + j
                    } else idx[instr$slots[(instr$dims[1]+1):cumdim2]] <- instr$values[[2]][(instr$dims[2]*(j-1)+1):(instr$dims[2]*j)]
                    for(k in 1:instr$lens[3]) {
                        if(index_types3 == 1) {
                            idx[instr$slots[instr$nDim]] <- iStart3 + k
                        } else idx[instr$slots[(cumdim2+1):instr$nDim]] <- instr$values[[3]][(instr$dims[3]*(k-1)+1):(instr$dims[3]*k)]
                        logProb <- logProb + self[[fn]](idx)
                    }
                }
            }
            return(logProb)
        },
     calc_4_allseq =
        function(instr, fn) {
            logProb <- 0
            idx <- rep(0, 4)
            iStart <- sapply(instr$values, `[`, 1)
            for(i1 in iStart[1]:(iStart[1] + instr$lens[1] - 1)) {
                idx[1] <- i1
                for(i2 in iStart[2]:(iStart[2] + instr$lens[2] - 1)) {
                    idx[2] <- i2
                    for(i3 in iStart[3]:(iStart[3] + instr$lens[3] - 1)) {
                        idx[3] <- i3
                        for(i4 in iStart[4]:(iStart[4] + instr$lens[4] - 1)) {
                            idx[4] <- i4
                            logProb <- logProb + self[[fn]](idx)
                        }
                    }
                }
            }
            return(logProb)
        },
    calc_4_generic =
        function(instr, fn) {
            logProb <- 0
            idx <- rep(0, instr$nDim)
            iStart <- sapply(instr$values, function(x) x[1]-1)
            cumdim2 <- instr$dims[1]+instr$dims[2]
            cumdim3 <- instr$dims[1]+instr$dims[2]+instr$dims[3]
            for(i1 in 1:instr$lens[1]) {
                if(instr$index_types[1] == 1) {
                    idx[instr$slots[1]] <- iStart[1] + i1
                } else idx[instr$slots[1:instr$dims[1]]] <- instr$values[[1]][(instr$dims[1]*(i1-1)+1):(instr$dims[1]*i1)]
                for(i2 in 1:instr$lens[2]) {
                    if(instr$index_types[2] == 1) {
                        idx[instr$slots[instr$dims[1]+1]] <- iStart[2] + i2
                    } else idx[instr$slots[(instr$dims[1]+1):cumdim2]] <- instr$values[[2]][(instr$dims[2]*(i2-1)+1):(instr$dims[2]*i2)]
                    for(i3 in 1:instr$lens[3]) {
                        if(instr$index_types[3] == 1) {
                            idx[instr$slots[cumdim2+1]] <- iStart[3] + i3
                        } else idx[instr$slots[(cumdim2+1):cumdim3]] <- instr$values[[3]][(instr$dims[3]*(i3-1)+1):(instr$dims[3]*i3)]
                        for(i4 in 1:instr$lens[4]) {
                            if(instr$index_types[4] == 1) {
                                idx[instr$slots[cumdim3+1]] <- iStart[4] + i4
                                } else idx[instr$slots[(cumdim3+1):instr$nDim]] <- instr$values[[4]][(instr$dims[4]*(i4-1)+1):(instr$dims[4]*i4)]
                            logProb <- logProb + self[[fn]](idx)
                        }
                    }
                }
            }
            return(logProb)
        },
    calc_5_allseq =
        function(instr, fn) {
            logProb <- 0
            idx <- rep(0, 4)
            iStart <- sapply(instr$values, `[`, 1)
            for(i1 in iStart[1]:(iStart[1] + instr$lens[1] - 1)) {
                idx[1] <- i1
                for(i2 in iStart[2]:(iStart[2] + instr$lens[2] - 1)) {
                    idx[2] <- i2
                    for(i3 in iStart[3]:(iStart[3] + instr$lens[3] - 1)) {
                        idx[3] <- i3
                        for(i4 in iStart[4]:(iStart[4] + instr$lens[4] - 1)) {
                            idx[4] <- i4
                            for(i5 in iStart[5]:(iStart[5] + instr$lens[5] - 1)) {
                                idx[5] <- i5
                                logProb <- logProb + self[[fn]](idx)
                            }
                        }
                    }
                }
            }
            return(logProb)
        },
     calc_5_generic =
        function(instr, fn) {
            logProb <- 0
            idx <- rep(0, instr$nDim)
            iStart <- sapply(instr$values, function(x) x[1]-1)
            cumdim2 <- instr$dims[1]+instr$dims[2]
            cumdim3 <- instr$dims[1]+instr$dims[2]+instr$dims[3]
            cumdim4 <- instr$dims[1]+instr$dims[2]+instr$dims[3]+instr$dims[4]
            for(i1 in 1:instr$lens[1]) {
                if(instr$index_types[1] == 1) {
                    idx[instr$slots[1]] <- iStart[1] + i1
                } else idx[instr$slots[1:instr$dims[1]]] <- instr$values[[1]][(instr$dims[1]*(i1-1)+1):(instr$dims[1]*i1)]
                for(i2 in 1:instr$lens[2]) {
                    if(instr$index_types[2] == 1) {
                        idx[instr$slots[instr$dims[1]+1]] <- iStart[2] + i2
                    } else idx[instr$slots[(instr$dims[1]+1):cumdim2]] <- instr$values[[2]][(instr$dims[2]*(i2-1)+1):(instr$dims[2]*i2)]
                    for(i3 in 1:instr$lens[3]) {
                        if(instr$index_types[3] == 1) {
                            idx[instr$slots[cumdim2+1]] <- iStart[3] + i3
                        } else idx[instr$slots[(cumdim2+1):cumdim3]] <- instr$values[[3]][(instr$dims[3]*(i3-1)+1):(instr$dims[3]*i3)]
                        for(i4 in 1:instr$lens[4]) {
                            if(instr$index_types[4] == 1) {
                                idx[instr$slots[cumdim3+1]] <- iStart[4] + i4
                                } else idx[instr$slots[(cumdim3+1):instr$nDim]] <- instr$values[[4]][(instr$dims[4]*(i4-1)+1):(instr$dims[4]*i4)]
                            for(i5 in 1:instr$lens[5]) {
                                if(instr$index_types[5] == 1) {
                                    idx[instr$slots[cumdim4+1]] <- iStart[5] + i5
                                    } else idx[instr$slots[(cumdim4+1):instr$nDim]] <- instr$values[[5]][(instr$dims[5]*(i5-1)+1):(instr$dims[5]*i5)]
                                logProb <- logProb + self[[fn]](idx)
                            }
                        }
                    }
                }
            }
            return(logProb)
        },
    simulate = function(instr) {
        if(instr$type == 0) return(sim_0(instr))
        if(instr$type == 1) return(sim_1_seq(instr))
        if(instr$type == 2) return(sim_1_mat(instr))
        if(instr$type == 3) return(sim_1_matp(instr))
        if(instr$type == 4) return(sim_2_seq_seq(instr))
        if(instr$type == 5) return(sim_2_seq_mat(instr))
        if(instr$type == 6) return(sim_2_mat_seq(instr))
        if(instr$type == 7) return(sim_2_mat_mat(instr))
        if(instr$type == 8) return(sim_2_seq_matp(instr))
        if(instr$type == 9) return(sim_2_matp_seq(instr))
        if(instr$type == 10) return(sim_2_matp_matp(instr))
        if(instr$type == 11) return(sim_2_x_y_ord(instr))
        if(instr$type == 12) return(sim_3_allseq(instr))
        if(instr$type == 13) return(sim_3_generic(instr))
        if(instr$type == 14) return(sim_4_allseq(instr))
        if(instr$type == 15) return(sim_4_generic(instr))
        if(instr$type == 16) return(sim_5_allseq(instr))
        if(instr$type == 17) return(sim_5_generic(instr))
        if(instr$type == 18) return(sim_1_matp_ord(instr))
        stop("declaration for type ", instr$type, " no implemented")
      },
    sim_0 = function(instr) {
        sim_one(0)
      },
    sim_1_seq = function(instr) {
        iStart <- instr$values[[1]][1]
        for(i in iStart:(iStart+instr$lens[1]-1))
            sim_one(i)
    },
    sim_1_mat = function(instr) {
        for(i in 1:instr$lens[1])
            sim_one(instr$values[[1]][i])
    },
    sim_1_matp = function(instr) {
        for(i in 1:instr$lens[1])
          sim_one(instr$values[[1]][(instr$dims[1]*(i-1) + 1):(instr$dims[1]*i)])
    },
    sim_1_matp_ord =
      function(instr) {
        logProb <- 0
        idx <- rep(0, instr$dims[1])
        for(i in 1:instr$lens[1]) {
          idx[instr$slots] <- instr$values[[1]][(instr$dims[1]*(i-1) + 1):(instr$dims[1]*i)]
          sim_one(idx)
        }
      },
    sim_2_seq_seq =
        function(instr) {
            idx <- rep(0, 2)
            iStart1 <- instr$values[[1]][1]-1
	    iStart2 <- instr$values[[2]][1]-1
            for(i in 1:instr$lens[1]) {
                idx[1] <- iStart1 + i
                for(j in 1:instr$lens[2]) {
                    idx[2] <- iStart2 + j
                    sim_one(idx)
                }
            }
        },
    sim_2_seq_mat =
        function(instr, fn) {
            idx <- rep(0, 2)
            iStart1 <- instr$values[[1]][1]
            for(i in iStart1:(iStart1 + instr$lens[1] - 1)) {
                idx[1] <- i
                for(j in 1:instr$lens[2]) {
                    idx[2] <- instr$values[[2]][j]
                    sim_one(idx)
                }
            }
        },
    sim_2_mat_seq =
        function(instr, fn) {
            idx <- rep(0, 2)
            iStart2 <- instr$values[[2]][1]
            for(i in 1:instr$lens[1]) {
                idx[1] <- instr$values[[1]][i]
                for(j in iStart2:(iStart2 + instr$lens[2] - 1)) {
                    idx[2] <- j
                    sim_one(idx)
                }
            }
        },
    sim_2_mat_mat =
        function(instr, fn) {
            idx <- rep(0, 2)
            for(i in 1:instr$lens[1]) {
                idx[1] <- instr$values[[1]][i]
                for(j in 1:instr$lens[2]) {
                    idx[2] <- instr$values[[2]][j]
                    sim_one(idx)
                }
            }
        },
    sim_2_seq_matp =
        function(instr, fn) {
            idx <- rep(0, instr$nDim)
            iStart1 <- instr$values[[1]][1]
            for(i in iStart1:(iStart1 + instr$lens[1] - 1)){
                idx[instr$slots[1]] <- i
                for(j in 1:instr$lens[2]) {
                    idx[instr$slots[2:instr$nDim]] <- instr$values[[2]][(instr$dims[2]*(j-1) + 1):(instr$dims[2]*j)]
                    sim_one(idx)
                }
            }
        },
    sim_2_matp_seq =
        function(instr, fn) {
            idx <- rep(0, instr$nDim)
            iStart2 <- instr$values[[2]][1]
            for(i in 1:instr$lens[1]) {
                idx[instr$slots[1:instr$dims[1]]] <- instr$values[[1]][(instr$dims[1]*(i-1) + 1):(instr$dims[1]*i)]
                for(j in iStart2:(iStart2 + instr$lens[2] - 1)) {
                    idx[instr$slots[instr$nDim]] <- j
                    sim_one(idx)
                }
            }
        },
    sim_2_matp_matp =
        function(instr, fn) {
            idx <- rep(0, instr$nDim)
            for(i in 1:instr$lens[1]) {
                idx[instr$slots[1:instr$dims[1]]] <- instr$values[[1]][(instr$dims[1]*(i-1) + 1):(instr$dims[1]*i)]
                for(j in 1:instr$lens[2]) {
                    idx[instr$slots[(instr$dims[1]+1):instr$nDim]] <- instr$values[[2]][(instr$dims[2]*(j-1) + 1):(instr$dims[2]*j)]
                    sim_one(idx)
                }
            }
        },
    sim_2_x_y_ord =
        function(instr, fn) {
            idx <- rep(0, 2)
            iStart1 <- instr$values[[1]][1]-1
            iStart2 <- instr$values[[2]][1]-1
            index_types1 <- instr$index_types[1]
            index_types2 <- instr$index_types[2]
            for(i in 1:instr$lens[1]) {
                if(index_types1 == 1) idx[2] <- iStart1 + i else idx[2] <- instr$values[[1]][i]
                for(j in 1:instr$lens[2]) {
                    if(index_types2 == 1) idx[1] <- iStart2 + j else idx[1] <- instr$values[[2]][j]
                    sim_one(idx)
                }
            }
        },
    sim_3_allseq =
        function(instr, fn) {
            idx <- rep(0, 3)
            iStart1 <- instr$values[[1]][1]
            iStart2 <- instr$values[[2]][1]
            iStart3 <- instr$values[[3]][1]
            for(i in iStart1:(iStart1 + instr$lens[1] - 1)) {
                idx[1] <- i
                for(j in iStart2:(iStart2 + instr$lens[2] - 1)) {
                    idx[2] <- j
                    for(k in iStart3:(iStart3 + instr$lens[3] - 1)) {
                        idx[3] <- k
                        sim_one(idx)
                    }
                }
            }
        },
    sim_3_generic =
        function(instr, fn) {
            idx <- rep(0, instr$nDim)
            iStart1 <- instr$values[[1]][1]-1
            iStart2 <- instr$values[[2]][1]-1
            iStart3 <- instr$values[[3]][1]-1
            index_types1 <- instr$index_types[1]
            index_types2 <- instr$index_types[2]
            index_types3 <- instr$index_types[3]
            cumdim2 <- instr$dims[1]+instr$dims[2]
            for(i in 1:instr$lens[1]) {
                if(index_types1 == 1) {
                    idx[instr$slots[1]] <- iStart1 + i
                } else idx[instr$slots[1:instr$dims[1]]] <- instr$values[[1]][(instr$dims[1]*(i-1)+1):(instr$dims[1]*i)]
                for(j in 1:instr$lens[2]) {
                    if(index_types2 == 1) {
                        idx[instr$slots[instr$dims[1]+1]] <- iStart2 + j
                    } else idx[instr$slots[(instr$dims[1]+1):cumdim2]] <- instr$values[[2]][(instr$dims[2]*(j-1)+1):(instr$dims[2]*j)]
                    for(k in 1:instr$lens[3]) {
                        if(index_types3 == 1) {
                            idx[instr$slots[instr$nDim]] <- iStart3 + k
                        } else idx[instr$slots[(cumdim2+1):instr$nDim]] <- instr$values[[3]][(instr$dims[3]*(k-1)+1):(instr$dims[3]*k)]
                        sim_one(idx)
                    }
                }
            }
        },
     sim_4_allseq =
        function(instr, fn) {
            idx <- rep(0, 4)
            iStart <- sapply(instr$values, `[`, 1)
            for(i1 in iStart[1]:(iStart[1] + instr$lens[1] - 1)) {
                idx[1] <- i1
                for(i2 in iStart[2]:(iStart[2] + instr$lens[2] - 1)) {
                    idx[2] <- i2
                    for(i3 in iStart[3]:(iStart[3] + instr$lens[3] - 1)) {
                        idx[3] <- i3
                        for(i4 in iStart[4]:(iStart[4] + instr$lens[4] - 1)) {
                            idx[4] <- i4
                            sim_one(idx)
                        }
                    }
                }
            }
        },
    sim_4_generic =
        function(instr, fn) {
            idx <- rep(0, instr$nDim)
            iStart <- sapply(instr$values, function(x) x[1]-1)
            cumdim2 <- instr$dims[1]+instr$dims[2]
            cumdim3 <- instr$dims[1]+instr$dims[2]+instr$dims[3]
            for(i1 in 1:instr$lens[1]) {
                if(instr$index_types[1] == 1) {
                    idx[instr$slots[1]] <- iStart[1] + i1
                } else idx[instr$slots[1:instr$dims[1]]] <- instr$values[[1]][(instr$dims[1]*(i1-1)+1):(instr$dims[1]*i1)]
                for(i2 in 1:instr$lens[2]) {
                    if(instr$index_types[2] == 1) {
                        idx[instr$slots[instr$dims[1]+1]] <- iStart[2] + i2
                    } else idx[instr$slots[(instr$dims[1]+1):cumdim2]] <- instr$values[[2]][(instr$dims[2]*(i2-1)+1):(instr$dims[2]*i2)]
                    for(i3 in 1:instr$lens[3]) {
                        if(instr$index_types[3] == 1) {
                            idx[instr$slots[cumdim2+1]] <- iStart[3] + i3
                        } else idx[instr$slots[(cumdim2+1):cumdim3]] <- instr$values[[3]][(instr$dims[3]*(i3-1)+1):(instr$dims[3]*i3)]
                        for(i4 in 1:instr$lens[4]) {
                            if(instr$index_types[4] == 1) {
                                idx[instr$slots[cumdim3+1]] <- iStart[4] + i4
                                } else idx[instr$slots[(cumdim3+1):instr$nDim]] <- instr$values[[4]][(instr$dims[4]*(i4-1)+1):(instr$dims[4]*i4)]
                            sim_one(idx)
                        }
                    }
                }
            }
        },
    sim_5_allseq =
        function(instr, fn) {
            idx <- rep(0, 4)
            iStart <- sapply(instr$values, `[`, 1)
            for(i1 in iStart[1]:(iStart[1] + instr$lens[1] - 1)) {
                idx[1] <- i1
                for(i2 in iStart[2]:(iStart[2] + instr$lens[2] - 1)) {
                    idx[2] <- i2
                    for(i3 in iStart[3]:(iStart[3] + instr$lens[3] - 1)) {
                        idx[3] <- i3
                        for(i4 in iStart[4]:(iStart[4] + instr$lens[4] - 1)) {
                            idx[4] <- i4
                            for(i5 in iStart[5]:(iStart[5] + instr$lens[5] - 1)) {
                                idx[5] <- i5
                                sim_one(idx)
                            }
                        }
                    }
                }
            }
        },
     sim_5_generic =
        function(instr, fn) {
            idx <- rep(0, instr$nDim)
            iStart <- sapply(instr$values, function(x) x[1]-1)
            cumdim2 <- instr$dims[1]+instr$dims[2]
            cumdim3 <- instr$dims[1]+instr$dims[2]+instr$dims[3]
            cumdim4 <- instr$dims[1]+instr$dims[2]+instr$dims[3]+instr$dims[4]
            for(i1 in 1:instr$lens[1]) {
                if(instr$index_types[1] == 1) {
                    idx[instr$slots[1]] <- iStart[1] + i1
                } else idx[instr$slots[1:instr$dims[1]]] <- instr$values[[1]][(instr$dims[1]*(i1-1)+1):(instr$dims[1]*i1)]
                for(i2 in 1:instr$lens[2]) {
                    if(instr$index_types[2] == 1) {
                        idx[instr$slots[instr$dims[1]+1]] <- iStart[2] + i2
                    } else idx[instr$slots[(instr$dims[1]+1):cumdim2]] <- instr$values[[2]][(instr$dims[2]*(i2-1)+1):(instr$dims[2]*i2)]
                    for(i3 in 1:instr$lens[3]) {
                        if(instr$index_types[3] == 1) {
                            idx[instr$slots[cumdim2+1]] <- iStart[3] + i3
                        } else idx[instr$slots[(cumdim2+1):cumdim3]] <- instr$values[[3]][(instr$dims[3]*(i3-1)+1):(instr$dims[3]*i3)]
                        for(i4 in 1:instr$lens[4]) {
                            if(instr$index_types[4] == 1) {
                                idx[instr$slots[cumdim3+1]] <- iStart[4] + i4
                                } else idx[instr$slots[(cumdim3+1):instr$nDim]] <- instr$values[[4]][(instr$dims[4]*(i4-1)+1):(instr$dims[4]*i4)]
                            for(i5 in 1:instr$lens[5]) {
                                if(instr$index_types[5] == 1) {
                                    idx[instr$slots[cumdim4+1]] <- iStart[5] + i5
                                    } else idx[instr$slots[(cumdim4+1):instr$nDim]] <- instr$values[[5]][(instr$dims[5]*(i5-1)+1):(instr$dims[5]*i5)]
                                sim_one(idx)
                            }
                        }
                    }
                }
            }
        },
      getParam = function(instr, param) {
        getParam_one(0, param) # use the "_0" case to check initial wiring
      }
  ),
  Cpublic = list(
    ## model = 'modelBase_nClass',
    ping = nFunction(
      name = "ping",
      function() {return(TRUE); returnType(logical())},
      compileInfo = list(virtual=TRUE)
    ),
    calculate_cpp = nFunction(
      name = "calculate_cpp",
      function(instr) {
        stop("Uncompiled version of calculate_cpp should not be called.")
      },
      returnType = 'numericScalar',
      compileInfo = list(virtual=TRUE,
                         C_fun = function(instr = 'instr_nClass') {
                             cppLiteral('Rprintf("declFunBase_nClass virtual base calculate_cpp should never be called (something is wrong)\\n");')
                             return(0)
                         })
    ),
    calculateDiff_cpp = nFunction(
      name = "calculateDiff_cpp",
      function(instr) {
        stop("Uncompiled version of calculateDiff_cpp should not be called.")
      },
      returnType = 'numericScalar',
      compileInfo = list(virtual=TRUE,
                         C_fun = function(instr = 'instr_nClass') {
                             cppLiteral('Rprintf("declFunBase_nClass virtual base calculateDiff_cpp should never be called (something is wrong)\\n");')
                             return(0)
                         })
    ),
    getLogProb_cpp = nFunction(
      name = "getLogProb_cpp",
      function(instr) {
        stop("Uncompiled version of getLogProb_cpp should not be called.")
      },
      returnType = 'numericScalar',
      compileInfo = list(virtual=TRUE,
                         C_fun = function(instr = 'instr_nClass') {
                             cppLiteral('Rprintf("declFunBase_nClass virtual base getLogProb_cpp should never be called (something is wrong)\\n");')
                             return(0)
                         })
    ),
    simulate_cpp = nFunction(
      name = "simulate_cpp",
      function(instr) {
        stop("Uncompiled version of simulate_cpp should not be called.")
      },
      returnType = 'void',
      compileInfo = list(virtual=TRUE,
                         C_fun = function(instr = 'instr_nClass') {
                             cppLiteral('Rprintf("declFunBase_nClass virtual base simulate_cpp should never be called (something is wrong)\\n");')
                         })
    ),
    getParam_cpp = nFunction(
      name = "getParam_cpp",
      function(instr, param) {
        stop("Uncompiled version of getParam_cpp should not be called.")
      },
      returnType = 'ETaccessor',
      compileInfo = list(virtual=TRUE,
                         C_fun = function(instr = 'instr_nClass', param = 'integerScalar') {
                             cppLiteral('Rprintf("declFunBase_nClass virtual base getParam_cpp should never be called (something is wrong)\\n");')
                         })
    )
  ),
  ## We haven't dealt with ensuring a virtual destructor when any method is virtual
  ## For now I did it manually by editing the .h and .cpp
  predefined = quote(system.file(file.path("include","nimbleModel", "predef"), package="nimbleModel") |>
               file.path("declFunBase_nC")),
  compileInfo=list(interface="full",
                   createFromR = FALSE,
                   exportName = "declFunBase_nClass_new",
                   needed_units = list("instr_nClass"),
                   interfaceExclude = c("getParam_cpp"),
                   packageNames = c(uncompiled="declFunBase_nClass_R", compiled="declFunBase_nClass")
                   )
)
