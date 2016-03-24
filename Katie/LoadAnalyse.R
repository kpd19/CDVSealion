##################################################
#                                                #
# run functions over .RData files in a directory #
#           * write some functions *             #
#         * put the functions in a list *        #
#        * specify the working directory *       #
#        * run import/analyze/export loop *      #
#         * an example plot for funzies *        #
#                                                #
##################################################

# Write some functions

end.mean <- function(arr){
  sum(arr[,ncol(arr)]) / nrow(arr)
}

end.var <- function(arr){
  sum((arr[,ncol(arr)] - end.mean(arr))^2)/(nrow(arr)-1)
}

end.cv <- function(arr){
  ((end.var(arr))^0.5)/end.mean(arr)
}

total.mean <- function(arr){
  sum(arr) / nrow(arr)
}

total.var <- function(arr){
  sum(((arr - total.mean(arr))^2)) / nrow(arr)
}

total.cv <- function(arr){
  ((total.var(arr))^0.5)/total.mean(arr)
}


# 1. list of functions
function.list <- list(end.mean, end.var, end.cv, total.mean, total.var, total.cv)
function.list.names <- c("end.mean", "end.var", "end.cv", "total.mean", "total.var", "total.cv")

# 2. name variables
arr.list.names <- c("S_D", "S_SL", "E_D", "E_SL", "I_D", "I_SL", "R_D", "R_SL", "D_D", "D_SL", "TD_D", "TD_SL")

# 3. directory with .Rdata files to import
file.dir <- "~/Desktop/Katie 10x10"

# 4. loads filenames form directory
files <- c(list.files(file.dir, pattern = ".RData"))

# 5. create matrix that is varialbes, stats, files long (3 dimensions)
stats.arr <- array(data = NA, dim = c(length(arr.list.names), length(function.list), length(files)))
dimnames(stats.arr) <- list(arr.list.names, function.list.names, files)

# 6. loop over files with loop over functions across variables
for (i in 1:length(files)){
  load(paste(file.dir, "/", files[i], sep = ""))	
  arr.list <- list(S_D, S_SL, E_D, E_SL, I_D, I_SL, R_D, R_SL, D_D, D_SL, TD_D, TD_SL)
  
  for (j in 1:length(function.list)){
    stats.arr[, j, i] <- unlist(lapply(arr.list, function.list[[j]]))
  }
  # print(stats.arr[,,j])
}

# 7. example plot
dim(stats.arr)
pack.size.seq.1 <- seq(from = 1, to = dim(stats.arr)[3], by = 5)
pack.size.seq.10 <- seq(from = 2, to = dim(stats.arr)[3], by = 5)
pack.size.seq.100 <- seq(from = 3, to = dim(stats.arr)[3], by = 5)
pack.size.seq.50 <- seq(from = 4, to = dim(stats.arr)[3], by = 5)
pack.size.seq.500 <- seq(from = 5, to = dim(stats.arr)[3], by = 5)

ones <- stats.arr[which(rownames(stats.arr)=="D_SL"), which(colnames(stats.arr)=="end.mean"), pack.size.seq.1]
tens <- stats.arr[which(rownames(stats.arr)=="D_SL"), which(colnames(stats.arr)=="end.mean"), pack.size.seq.10]
fifty <- stats.arr[which(rownames(stats.arr)=="D_SL"), which(colnames(stats.arr)=="end.mean"), pack.size.seq.50]
huns <- stats.arr[which(rownames(stats.arr)=="D_SL"), which(colnames(stats.arr)=="end.mean"), pack.size.seq.100]
fivehun <- stats.arr[which(rownames(stats.arr)=="D_SL"), which(colnames(stats.arr)=="end.mean"), pack.size.seq.500]

ymin <- min(stats.arr[which(rownames(stats.arr)=="D_SL"), which(colnames(stats.arr)=="end.mean"),])
ymax <- max(stats.arr[which(rownames(stats.arr)=="D_SL"), which(colnames(stats.arr)=="end.mean"),])

plot.col <- c("red", "orange", "green", "blue", "purple")

plot(rep(1, 5), log(ones), xlim = c(0, 500), ylim = c(log(ymin), log(ymax)), col = plot.col[1], cex = 1.25)
points(rep(10, 5), log(tens), col = plot.col[2], cex = 1.25)
points(rep(50, 5), log(fifty), col = plot.col[3], cex = 1.25)
points(rep(100, 5), log(huns), col = plot.col[4], cex = 1.25)
points(rep(500, 5), log(fivehun), col = plot.col[5], cex = 1.25)