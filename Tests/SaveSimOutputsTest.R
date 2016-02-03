###  this saves simulation outputs as R data files, makes folders, and saves by parameter names

# 0. create a directory to save the files
	save.dir <- "/Users/christophermoore/Dropbox/Projects/*DixonCDV/TestOut"

# 1. create some fake data
	arr.test <- array(data = rep(1:16,1), dim = c(4,4,1))
	arr.test.big <- array(data = rep(1:16,1), dim = c(5,5,1))
	arr.test.bigger <- array(data = rep(1:16,1), dim = c(6,6,1))


	# 1.0 parameter list and length for loop
		# 1.0.0. multiplier parameter
		arr.mult <- 5:7
		mult.length <- length(arr.mult)

		# 1.0.1. multiplier parameter
		arr.add <- c(6, 1, 7)
		add.length <- length(arr.add)

		# 1.0.2. size parameter (where we will save to folders)
		arr.size <- list(arr.test, arr.test.big, arr.test.bigger)
		size.length <- length(arr.size)

# 2. loop over parameters and save data as .rda
	# 2.1 WITH CREATING FOLDERS (by grid size)
	for(k in 1:size.length){
		cells <- dim(arr.size[[k]])[1]*dim(arr.size[[k]])[2]
		dir.create(paste(save.dir, "/", cells, ".cells" , sep = ""))
		for(j in 1:add.length){
			arr.run.add <- arr.size[[k]] + arr.add[j]
			for(i in 1:mult.length){
				arr.run.mult <- arr.run.add*arr.mult[i]
				name <- paste(j, ".add", i, ".mult", sep = "")
				save(arr.run.mult, file = paste(save.dir,  "/", cells[1], ".cells", "/" , name, ".rda", sep = ""))
			}
		}
	 }

	# 2.2 WITHOUT CREATING FOLDERS
	for(k in 1:size.length){
		cells <- dim(arr.size[[k]])[1]*dim(arr.size[[k]])[2]
		for(j in 1:add.length){
			arr.run.add <- arr.size[[k]] + arr.add[j]
			for(i in 1:mult.length){
				arr.run.mult <- arr.run.add*arr.mult[i]
				name <- paste(j, ".add", i, ".mult", sep = "")
				save(arr.run.mult, file = paste(save.dir,  "/", cells[1], ".cells" , name, ".rda", sep = ""))
			}
		}
	 }