#################################################
# Canine Distemper in Sea lion population model #
#                                               #
#                    INDEX                      #
#                0. parameters                  #
#              1. populate arrays               #
#               2. calculations                 #
#         3. initializing caluculations         #
#               4.  simulation                  #
#                5. graph it                    #
#                                               #
#################################################

rm(list=ls()) # all clear!
set.seed(pi) # add your favourtite number!

# function to example the corners of large 2D datasets, set rowcol = T to see the preiferal rows and columns
check <- function(x , size = 3 , rowcol = F){
  print("Upperleft")
  print(x[1:size,1:size])
  print("Bottomleft")
  print(x[(nrow(x)-(size-1)):nrow(x),1:size])
  print("Upperright")
  print(x[1:size,(ncol(x)-(size-1)):ncol(x)])
  print("Bottomright")
  print(x[(nrow(x)-(size-1)):nrow(x),(ncol(x)-(size-1)):ncol(x)])
  if (rowcol == T) {
    print("Firstrow")
    print(x[1,])
    print("Lastrow")
    print(x[nrow(x),])
    print("Firscolumn")
    print(x[,1])
    print("Lastcolumn")
    print(x[,ncol(x)])
  }
}

print(Sys.time())
epsilons <- c(0, 0.25, 0.5, 1, 2, 3)
size <- c(1, 10, 50, 100, 500)

# 0. INITIAL CONDITIONS
# model parameters
# epsilon_D <- silon # spatial decay for dogs
# epsilon_SL <- silon # spatial decay for sea lions
scalek <- 1 # km (each pack occupies a (scale X scale) km^2 grid cell)
beta_D <- 0.16 # intraspecific contact rate for dogs, kindofgotten from almberg et al. for the coyotes?
beta_SL <- 0.4 # intraspecific contact rate for sea lions, gottem from phocine distemper paper 1998
beta_prime <- 0#2.3e-2 # interspecies contact rate, from Brock
sigma <- 1/7 # 1/average latent period
gamma.SL <- 1/6 # 1/average infectious period
gamma.D <- 1/9
delta.D <- 0.1111111 # death due to disease # 50% chance or mortality over two days 0.5/(0.5+1/2) = 0.5
delta.SL <- 0.094 # 0.094 is correct, PDV paper had a 36% mortality rate for each case
sdd <- 1 - delta.D # average daily survival of infected individuals
muD.dogs <- 1.48e-3 # average daily death of non-infected individuals
muD.sealions <- 4.83e-4 # gotten from phocine distemper paper # 20% overall
muB.dogs <- 1.48e-3
# time
years <- 25
annum <- 365
time <- years*annum+1
BP <- 112
# space
island.rows <- 10
island.columns <- 10
total.cells <- island.rows*island.columns
# populations parameters
packs <- total.cells
#total.dogs <- 200 # controls the density of dogs per pack
#total.sealions <- 200
#packs.dogs <- (sqrt(total.cells)-2)^2 # number of packs of dogs that occupy all cells
#dogs <- ceiling(total.dogs/packs.dogs) # number of dogs per pack
#dogs <- 500
#packs.sealions <- total.cells - packs.dogs
#sealions <- ceiling(total.sealions/packs.sealions) # number of sea lions per pack
#sealions <- 500
# birth pulse parameters
gender.split <- 0.5 # the proportion of the population that is female
prop.rep <- 5/6 # the proportion of the population that has reproductive potential
prop.survive <- 0.66 #p proportion of sea lion pups that survive birth #Melin et al. 2010
tt <- 1
total.born<- vector(length=10, mode="numeric")


# 1. populate arrays
# Matrices for recording dog data
S_D <- array(0 , dim = c(packs, time)) # Susceptibles 
E_D <- array(0 , dim = c(packs, time)) # Exposed
I_D <- array(0 , dim = c(packs, time)) # Infected
R_D <- array(0 , dim = c(packs, time)) # Recovered
D_D <- array(0 , dim = c(packs, time)) # Death
TD_D <- array(0 , dim = c(packs, time)) # total death
# Adds susceptibles in each pack
# for(i in 1:packs){
#   S_D[i, 1] <- dogs
# }

# Matrices for recording sea lion data
S_SL <- array(0 , dim = c(packs, time)) # susceptibles 
E_SL <- array(0 , dim = c(packs, time)) # exposed
I_SL <- array(0 , dim = c(packs, time)) # infected
R_SL <- array(0 , dim = c(packs, time)) # recovered
D_SL <- array(0 , dim = c(packs, time)) # death
TD_SL <- array(0 , dim = c(packs, time)) # total death
# for(i in 1:packs){
#   S_SL[i, 1] <- sealions
# }

muB <- array(0 , dim = c(packs, 365))
#muB[,1:BP] <- 0 # this is redundant

vac_SL <- array(0 , dim = c(1, 365))
vac_D <- array(0 , dim = c(1, 365))
#vac_D[364] <- 0.25  
#vac_SL[] <- x

#adds sea lions on the perimeter, erases dogs from perimeter
# for(i in 1:(sqrt(packs)+1)) 
# { S_SL[i, 1] <- sealions
# S_D[i, 1] <- 0  
# }
# for(j in 2:(sqrt(packs)-1))
# { x = (j * sqrt(packs)) 
# S_SL[x, 1] <- sealions
# S_D[x, 1] <- 0
# y = (j * (sqrt(packs)) +1) 
# S_SL[y, 1] <- sealions
# S_D[y, 1] <- 0
# }
# for(i in (packs - sqrt(packs) + 1):(packs)) 
# { S_SL[i, 1] <- sealions
# S_D[i, 1] <- 0
# }
#create an array for muB

# lambda arrays
lambda_D <- matrix(0, nrow = packs, ncol = 1) # this is the lambda matrix which resets for every value of time
lambda_SL <- matrix(0, nrow = packs, ncol = 1) # this is the lambda matrix which resets for every value of time

# 2. FUNCTIONS
S_D_func <- #function for S dogs
  function(dat, muD, muB.1, vac) { 
    Sval_2 <- dat[1] - muD*dat[1]
    total <- Sval_2 + dat[2] + dat[3] + dat[4]
    Sval_3 <- Sval_2 + muB.1*total
    Sval_4 <- Sval_3 - dat[6]*Sval_3
    Sval_4 - vac[1, t-1]*Sval_4
  }
S_SL_func <- #function for S sealions
  function(dat, muD, vac) { 
    Sval_2 <- dat[1] - muD*dat[1]# dat1 is S
    Sval_3 <- Sval_2 + dat[7] #dat7 is muB
    Sval_4 <- Sval_3 - dat[6]*Sval_3 #dat6 is lambda
    Sval_4 - vac[1, t-1]*Sval_4    
  }
E_func <- # function for the individuals in the exposed class, not get contageous
  function(dat, sigma, muD){
    Eval_2 <- dat[2] + muD*dat[2]
    Eval_3 <- Eval_2 - sigma*Eval_2
    Sval_E <- dat[1] - muD*dat[1]
    Eval_3 + dat[6]*Sval_E
  }
I_func <- # function for individuals in the infectious class
  function (dat, sigma, gamma, delta, muD) {
    Ival_2 <- dat[3] - muD*dat[3]
    Ival_3 <- Ival_2 - delta*Ival_2
    Ival_4 <- Ival_3 - gamma*Ival_3 
    Eval_I <- dat[2] + muD*dat[2]
    Ival_4 + sigma*Eval_I
  }
R_func <- # function for individuals in the recovered class
  function(dat, gamma, delta, muD, vac) {
    Ival_R <- dat[3] - muD*dat[3]
    Ival_R2 <- Ival_R - delta*Ival_R
    Rval_2 <- dat[4] - muD*dat[4]
    Rval_2 + gamma*Ival_R2 + vac[1, t-1]*dat[1]
  }
D_func <- 
  function(dat, delta, muD) {
    Ival.D <- dat[3] - muD*dat[3]
    delta*Ival.D
  }
TD_func <- 
  function(dat, delta, muD) {
    Ival.D <- dat[3] -muD*dat[3]
    delta*Ival.D + muD*dat[1] + muD*dat[2] + muD*dat[3] + muD*dat[4]
  }
my_distance_set1 <- function(scale, iRow, iCol, jRow, jCol) {
  scale*sqrt((iRow-jRow)^2 + (iCol-jCol)^2);
}
my_distance_set2 <- function(x, scale) {
  dist <- array(data = 0  , dim = c(x, x))
  for (i in 1:x){
    iRow <- ceiling(i/sqrt(x))
    iCol <- i - (iRow-1)*sqrt(x)
    for (j in 1:x) {
      jRow <- ceiling(j/sqrt(x))
      jCol <- j - (jRow-1)*sqrt(x)
      dist[i, j] <- my_distance_set1(scale, iRow, iCol, jRow, jCol)
    }
  }
  dist
}

births_func <-
  function(S, E, I, R, gender.split, prop.rep, prop.survive,k) {
    total.alive <- S[q, tt] + E[q, tt] + I[q, tt] + R[q, tt]
    new.pups <- total.alive*gender.split*prop.rep*(1-(total.alive/(2.5*k)))
    if(new.pups <0){
      new.pups=0
    }
    live.pups <- prop.survive*new.pups
    per.day <- live.pups/BP
    per.day
  }

# 3. initializing caluculations
dog_dist <- my_distance_set2(total.cells, scalek)
sealion_dist <- my_distance_set2(total.cells, scalek)
lattice_size <- dim(dog_dist) # calculates the size to use for i and j (currently 9 and 9)