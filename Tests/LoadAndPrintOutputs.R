##################################################
#             load CDV simualtion,               #
# save .pdg of a single file (e.g., timeseries), #
#        and print timesteps to a folder         #
#                                                #
##################################################


# working directory with outputs
	file.dir <- "~/Dropbox/Projects/*DixonCDV/TestOut"
	fig.dir <- paste(file.dir, "/", "Figures", sep = "")

# checks if Figures directory exists; if not, it creates one
	if(file.exists(paste(fig.dir,sep = "")) != T){
		dir.create(paste(fig.dir, sep = ""))
		message("Created Figures directory")
	} else {
		message("Figures directory already exists")
	}

# loads filenames form directory
	files <- c(list.files(file.dir, pattern = ".rda"))

# make a fake multidimentional array
	fake.data <- runif(25)
	fake.array <- array(data = fake.data + rnorm(225,0,.01), dim = c(5,5,10))

# loop over loading file, printing singe image, and making a folder with multiple images
	for(i in 1:length(files)){
		input <- get(load(paste(file.dir, "/", files[i], sep = "")))
		file.name <- gsub('.rda', "", files[i]) # removes .rda from filename

		# for a single image
		png(paste(fig.dir, "/", file.name, ".png", sep = ""))
		image(input[,,1])
		dev.off()

		# for multiple images in a folder (e.g., what you'd want to save for the movies)
		mult.img.dir <- paste(fig.dir, "/", file.name, sep = "")
		dir.create(mult.img.dir)
		input <- fake.array
		for(j in 1:dim(fake.array)[3]){
		png(paste(mult.img.dir, "/", "t", j, ".png", sep = ""))
		image(input[,,j])
		dev.off()
		}
	
	}