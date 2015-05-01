
# Patching a gap in FKF
# Using Durbin and Koopman's smoother
# See http://books.google.com/books?id=fOq39Zh0olQC&lpg=PP1&pg=PA88#v=onepage&q&f=false

KalmanSmoother <- function(yt, ss, kf)
{
  n <- dim(yt)[2]
  Tprime <- t(ss$bigT)

  astar <- matrix(ncol = n, nrow = nrow(kf$att))
  Pstar <- array(NA, dim = c(rep(nrow(kf$att), 2), n))
  
  astar[,n] <- kf$att[,n]
  Pstar[,,n] <- kf$Ptt[,,n]
  littleR <- rep(0, nrow(kf$att))
  bigN <- matrix(0, nrow(kf$att), nrow(kf$att))
  for (i in seq.int(n, 1)) {
    Pstar[,,i] <- kf$Ptt[,,i]
    astar[,i] <- kf$att[,i]
    availableCoordinates <- which(!is.na(yt[,i]))
    if (any(availableCoordinates)) {
      K <- ss$bigT %*% kf$Pt[,,i] %*% t(Z[availableCoordinates,]) %*% solve(kf$Ft[availableCoordinates,availableCoordinates,i])
      L <- ss$bigT - K %*% Z[availableCoordinates,]
      littleR <- (t(Z[availableCoordinates,]) %*%
                    solve(kf$Ft[availableCoordinates,availableCoordinates,i], kf$vt[availableCoordinates,i])) +
                  t(L) %*% littleR
      bigN <- (t(Z[availableCoordinates,]) %*% solve(kf$Ft[availableCoordinates,availableCoordinates,i], Z[availableCoordinates,]) +
                 t(L) %*% bigN %*% L)
      
    } else {
      littleR <- t(ss$bigT) %*% littleR
      bigN <- t(ss$bigT) %*% bigN %*% ss$bigT
    }
    astar[,i] <- kf$at[,i] + kf$Pt[,,i] %*% littleR
    Pstar[,,i] <- kf$Pt[,,i] - kf$Pt[,,i] %*% bigN %*% kf$Pt[,,i]
  }
  return(list(astar = astar, Pstar = Pstar))
}
