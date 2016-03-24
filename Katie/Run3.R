# 4. simulation

#katie.func <- function() {
  time1 <- (Sys.time())
  # Rprof(filename="test", line.profiling = TRUE)
  for (n in 1:years){
    print(n); print(Sys.time())
    for (q in 1:packs){
      muB[q,1:BP] <- births_func(S_SL, E_SL, I_SL, R_SL, gender.split, prop.rep, prop.survive, sealions)  
    }
    total.born[n] <- .rowSums((.rowSums(muB, packs, annum, na.rm=FALSE)), 1, packs, na.rm=FALSE)
    for (t in 2:(annum+1)){
      # Random dogs introduced every 91 days approximately
      I_D[seed[n], ((n-1)*365 + 1)] <- inf[n] # put back n-1
      tt <- t + 365*(n-1)
      for(i in 1:lattice_size[1]){ # starts at pack one and moves through the packs
        dog.dfval <- 0 # sets the initial lambda i to be 0 for dogs
        sealion.dfval <- 0 # sets the initial lambda i to be 0 for sea lions
        for(j in 1:lattice_size[2]){  # works through the columns starting at 1
          #dogs
          x <- I_D[j, tt-1] * dog.dist[i,j]
          # this calculates the value that takes into account other packs
          dog.dfval <- dog.dfval + x #Used to sum all of the numbers
          xx <- I_SL[j, tt-1] * sealion.dist[i,j]
          sealion.dfval <- sealion.dfval + xx
        }
        #dogs
        lambda_D[i] <- 1 - exp(-((beta_D*dog.dfval) + (beta_prime*sealion.dfval)))
        #sealions
        lambda_SL[i] <-1 - exp(-((beta_SL*sealion.dfval) + (beta_prime*dog.dfval)))
      }
      d.dat <- cbind(S_D[,tt-1], E_D[,tt-1], I_D[,tt-1], R_D[,tt-1], D_D[,tt-1], lambda_D)
      sl.dat <- cbind(S_SL[,tt-1], E_SL[,tt-1], I_SL[,tt-1], R_SL[,tt-1], D_SL[,tt-1], lambda_SL, muB[,t-1])
      for(k in 1:lattice_size[1]){# only needs to rotate through i because it doesn't need to reference any other packs, that is done in lambda
        # dogs
        S_D[k, tt] <- S_D_func(d.dat[k,], muD.dogs, muB.dogs, vac_D) # records number of susceptibles in the current time step
        E_D[k, tt] <- E_func(d.dat[k,], sigma, muD.dogs) # records number of exposed in the current time step
        I_D[k, tt] <- I_func(d.dat[k,], sigma, gamma.D, delta.D, muD.dogs) # records number of infected in current time step
        R_D[k, tt] <- R_func(d.dat[k,], gamma.D, delta.D, muD.dogs, vac_D) # records number of recovered individuals in new time step
        D_D[k, tt] <- D_func(d.dat[k,], delta.D, muD.dogs)
        TD_D[k, tt] <- TD_func(d.dat[k,], delta.D, muD.dogs)
        # sea lions
        S_SL[k, tt] <- S_SL_func(sl.dat[k,], muD.sealions, vac_SL) # records number of susceptibles in the current time step
        E_SL[k, tt] <- E_func(sl.dat[k,], sigma, muD.sealions) # records number of exposed in the current time step
        I_SL[k, tt] <- I_func(sl.dat[k,], sigma, gamma.SL, delta.SL, muD.sealions) # records number of infected in current time step
        R_SL[k, tt] <- R_func(sl.dat[k,], gamma.SL, delta.SL, muD.sealions, vac_SL) # records number of recovered individuals in new time step
        D_SL[k, tt] <- D_func(sl.dat[k,], delta.SL, muD.sealions)
        TD_SL[k, tt] <- TD_func(sl.dat[k,], delta.SL, muD.sealions)
      }
    }
  }
  # Rprof()
  
  print(Sys.time())
  (time2 <- (Sys.time()))
  (total.time <- time2-time1)
  
#}

daymon <- format(Sys.time(), "%b%d")
naming.file <- paste("dogs.once","eps", epsilon_D, "rows", island.rows,"packsize", dogs, daymon,"RData", sep=".")
naming.graph <- paste("dogs.once","eps", epsilon_D,"rows", island.rows,"packsize", dogs, daymon,"pdf", sep=".")
save(S_D, E_D, I_D, R_D, D_D, TD_D,S_SL, E_SL, I_SL, R_SL, D_SL, TD_SL, epsilon_D, island.rows, dogs, sealions, total.born,total.time,seed, time, inf, file = naming.file)
#load("/Users/KatieDixon/Documents/Stuff I Need/Research/Fall 2015/R codes/0.6.16.Jan22.RData")
# 
# 5. graph it
myS <- .colSums(S_D, packs, max(time), na.rm = FALSE)
myE <- .colSums(E_D, packs, max(time), na.rm = FALSE)
myI <- .colSums(I_D, packs, max(time), na.rm = FALSE)
myR <- .colSums(R_D, packs, max(time), na.rm = FALSE)
myD <- .colSums(D_D, packs, max(time), na.rm = FALSE)
myTD <- .colSums(TD_D, packs, max(time), na.rm = FALSE)
myDcum <- cumsum(.colSums(D_D, packs, max(time), na.rm = FALSE))
myTDcum <- cumsum(.colSums(TD_D, packs, max(time), na.rm = FALSE))
mytotalpopD <- myS + myR

myS2 <- .colSums(S_SL, packs, max(time), na.rm = FALSE)
myE2 <- .colSums(E_SL, packs, max(time), na.rm = FALSE)
myI2 <- .colSums(I_SL, packs, max(time), na.rm = FALSE)
myR2 <- .colSums(R_SL, packs, max(time), na.rm = FALSE)
myD2 <- .colSums(D_SL, packs, max(time), na.rm = FALSE)
myTD2 <- .colSums(TD_SL, packs, max(time), na.rm = FALSE)
myD2cum <- cumsum(.colSums(D_SL, packs, max(time), na.rm = FALSE))
myTD2cum <- cumsum(.colSums(TD_SL, packs, max(time), na.rm = FALSE))
myE2cum <- cumsum(.colSums(E_SL, packs, max(time), na.rm = FALSE))
mytotalpopSL <- myS2 + myR2

myTime <- 1:time

pdf(file=naming.graph, width=25, height=15)
par(mfrow=c(2,2) , oma = rep(2,4) , mar = c(3,4,3,4))
# DOG PLOT
plot(myTime, myS, type = "l", xlab = "Time(days)", ylab = "Total DOGS", lwd =2, col="dark green" , ylim = c(0,max(myS, myI, myE, myR)), main=paste("epsilon =",epsilon_D, "packs =",(island.rows*island.columns), "packsize =", dogs, sep=" "), cex=5)
abline(v = seq(from = 1 , to = time , by = 365) , col = "grey50" , lty = 2 , lwd = 2)
abline(v = seq(from = 122 , to = time , by = 365) , col = "grey50" , lty = 2)
#lines(myE, type="l", col="red", lwd = 2)
#lines(myI, type = "l", col="blue", lwd = 2)
lines(myR, type = "l", col="violet", lwd = 2)
lines(myD, type = "l", col="black", lwd = 2)
par(new = T)
plot(myTime , myTDcum, type = "n" , xaxt = "n" , yaxt = "n" , xlab = "" , ylab = "")
lines(myDcum, type = "l", col="black", lwd = 2 , lty = 3)
axis(4)
mtext("DOG deaths" , side = 4 , line = 2.5)
legend("topleft", legend = c("Susceptible","Exposed","Infected","Recovered","Dead") , col = c("dark green" , "red" , "blue" , "violet" , "black") , lty = 1, cex = 0.9 , lwd = 1.5, bg="white")

# I and E and S
plot(myTime, myS, type = "l", xlab = "Time(days)", ylab = "Total DOGS", lwd =2, col="dark green" , ylim = c(0,.1*max(myI)), main=paste("epsilon =",epsilon_D, "packs =",(island.rows*island.columns), "packsize =", dogs, sep=" "), cex=5)
abline(v = seq(from = 1 , to = time , by = 365) , col = "grey50" , lty = 2 , lwd = 2)
abline(v = seq(from = 122 , to = time , by = 365) , col = "grey50" , lty = 2)
lines(myE, type="l", col="red", lwd = 2)
lines(myI, type = "l", col="blue", lwd = 2)


# SEALION PLOT
plot(myTime, myS2, type = "l", xlab = "Time(days)", ylab = "Total SEA LIONS", lwd = 2, col="dark green" , ylim = c(0,max(myS2, myI2, myE2, myR2,na.rm = T)))
abline(h = myS2[1] , col = "grey50" , lty = 2)
abline(v = seq(from = 1 , to = time , by = annum) , col = "grey50" , lty = 2 , lwd = 2)
abline(v = seq(from = BP , to = time , by = annum) , col = "grey50" , lty = 2)
#lines(myE2, type = "l", col="red", lwd = 2)
#lines(myI2, type = "l", col="blue", lwd = 2)
lines(myR2, type = "l", col="violet", lwd = 2)
lines(myD2, type = "l", col="black", lwd = 2)
lines(myTD2, type = "l", col="green", lwd = 2)
par(new = T)
plot(myTime , myTD2cum, type = "n" , xaxt = "n" , yaxt = "n" , xlab = "" , ylab = "")
lines(myD2cum, type = "l", col="black", lwd = 2 , lty = 3)
#lines(myTD2cum, type = "l", col="grey", lwd = 2 , lty = 3)
axis(4)
mtext("SEA LION deaths" , side = 4 , line = 2.5)
#legend("topright" , legend = c("S" , "E" , "I" , "R" , "D" , "TD") , col = c("dark green" , "red" , "blue" , "violet" , "black" , "green") , lty = 1 , bg = rgb(1,1,1,0.75,,1) , cex = 0.75 , lwd = 1.5)

# I and E and S
plot(myTime, myS2, type = "l", xlab = "Time(days)", ylab = "Total DOGS", lwd =2, col="dark green" , ylim = c(0,.1*max(myI2)), main=paste("epsilon =",epsilon_D, "packs =",(island.rows*island.columns), "packsize =", dogs, sep=" "), cex=5)
abline(v = seq(from = 1 , to = time , by = 365) , col = "grey50" , lty = 2 , lwd = 2)
abline(v = seq(from = 122 , to = time , by = 365) , col = "grey50" , lty = 2)
lines(myE2, type="l", col="red", lwd = 2)
lines(myI2, type = "l", col="blue", lwd = 2)
dev.off()