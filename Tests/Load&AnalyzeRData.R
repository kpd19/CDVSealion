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

# 0. write some functions
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

	max.y1.pack <- function(arr){
		max(arr[,1:365])
	}

	max.y1.island <- function(arr){
		max(colSums(arr[,1:365]))
	}
	

# 1. list of functions
	function.list <- list(end.mean, end.var, end.cv, total.mean, total.var, total.cv, max.y1.pack, max.y1.island)
	function.list.names <- c("end.mean", "end.var", "end.cv", "total.mean", "total.var", "total.cv", "max.y1.pack", "max.y1.island")

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
	}