# SetPt2

for(i in 1:packs){
  S_D[i, 1] <- dogs
}

seed <- array(0 , dim = c(1,1)) #normally this is zero but I have one pack

seed <- array(0 , dim = c(1,1)) #normally this is zero but I have one pack
for (u in 1:years) {
  innoc.vec <- seq(island.rows+2,((island.rows*(island.columns))-(island.columns+1)), island.rows)
  innoc.seed.list <- vector("list" , length(innoc.vec))
  for(i in 1:length(innoc.vec)){
    innoc.seed.list[[i]] <- innoc.vec[i]:(innoc.vec[i]+(island.rows-3))
    innoc.seed <- unlist(innoc.seed.list)
  }
  seed[u] <- sample(innoc.seed , 1)
}

inf <- rep(c(1,0), c(1,50))
#inf <- rep(c(1,0,0,0,0), 7)

# I_D[seed[1],1] <- 1
# S_D[seed[1],1] <- dogs -1


# for(i in 1:packs){
#   S_SL[i, 1] <- sealions
# }

for(i in 1:(sqrt(packs)+1)) 
{ S_SL[i, 1] <- sealions
S_D[i, 1] <- 0  
}
for(j in 2:(sqrt(packs)-1))
{ x = (j * sqrt(packs)) 
S_SL[x, 1] <- sealions
S_D[x, 1] <- 0
y = (j * (sqrt(packs)) +1) 
S_SL[y, 1] <- sealions
S_D[y, 1] <- 0
}
for(i in (packs - sqrt(packs) + 1):(packs)) 
{ S_SL[i, 1] <- sealions
S_D[i, 1] <- 0
}

dog.dist <- exp(-dog_dist*epsilon_D)
sealion.dist <- exp(-sealion_dist*epsilon_SL)
