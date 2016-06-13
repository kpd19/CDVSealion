# int.mat   takes the dog matrix at a single time
# peri.mat   takes the sea lion matrix at a single time
# coords   takes a matrix of coordinates for the cells in a hexgrid
# NA.cells   takes a vector of cells that NA-ize the square matrix into a hexagon
# cell.radius   takes a number representative of the radius from the center to the six points of a hexagon
# grid.radius  takes the number of rings from the cetner cell
# hex.corrds   takes a vector of hexagonal coordiantes of a cell from (0,0)
# cell.vals   takes a logical statement about the presennce of cell values
# d.colors   takes a vector of color values for the dogs
# sl.colors   takes a vector of color values for the sea lions

hex.graph <- function(int.mat, peri.mat, coords = NULL, NA.cells = NULL, cell.radius = 1, grid.radius = NULL, hex.coords = NULL, cell.vals = F, d.colors = c("grey", "red"), sl.colors = c("grey", "blue"), ...){

	if (is.null(grid.radius)){
	mat.rows <- length(int.mat)
	# write a while loop for these next three lines later
	hex.lengths <- c(1, 7, 19, 37, 61, 91, 127, 169, 217, 271, 331, 397, 469, 547, 631, 721, 817, 919, 1027, 1141)
	if(mat.rows > 1141)
	if(mat.rows != nrow(peri.mat)){stop("The number of rows in int.mat and peri.mat must be equal")}

	grid.r <- which(mat.rows == hex.lengths) - 1 # calls the element that is r -1
	if (length(grid.r) != 1) {stop("The number of cells (the matrix rows) needs to be a multiple of the number of cells in a hexagonal grid (e.g., 1, 7, 19, 37).")}
	} else {
		grid.r <- grid.radius
	}

	cell.radius <- cell.radius
	grid.diam <- (grid.r*2) +1

	q.dist <- 1.5*cell.radius #columns are 1.5 radii apart
	r.dist <- sqrt(3)*cell.radius #rows are 3^0.5 radii apart

	if (is.null(coords)){
		grid.coords <- array(data = c(
			rep(seq(q.dist, q.dist*grid.diam, q.dist), each = grid.diam),
			rep(seq(r.dist, r.dist* grid.diam, r.dist), times = grid.diam)
			), dim = c(grid.diam, grid.diam, 2)) #grid with distances away from 0 (first element in thrid dimention is column distances, second is row)
	} else {
		grid.coords <- coords
	}

	n.cells <- 3*(grid.r+1)^2 - 3*(grid.r + 1) + 1

	mat <- grid.coords

	r.seq <- seq(1, grid.diam, 2)
	mat[, r.seq, 2] <- mat[, r.seq, 2] + sqrt(3)/2

	dist.mat <- sqrt((mat[,, 1] - mat[(grid.r +1), (grid.r +1) , 1])^2 +(mat[,, 2] - mat[(grid.r + 1), (grid.r + 1) , 2])^2)

	if (is.null(NA.cells)){
		NA.cells <- which(dist.mat > dist.mat[1, (grid.r + 1)]+0.0000001, arr.ind = F)
	}
	
	pres.cells <- which(!1:(grid.diam ^2) %in% NA.cells)

	mat[,,1][NA.cells] <- NA
	mat[,,2][NA.cells] <- NA

	pres.int <- which(dist.mat <= dist.mat[2, (grid.r+1 )]+0.0000001 , arr.ind = F)
	pres.out <- which(dist.mat > dist.mat[2, (grid.r + 1)]+0.0000001 & dist.mat < dist.mat[1, (grid.r + 1)]+0.0000001, arr.ind = F)

	d.mat <- array(data = NA, dim = c(grid.diam, grid.diam, 2))
	d.mat[,,1][pres.int] <- mat[,,1][pres.int]
	d.mat[,,2][pres.int] <- mat[,,2][pres.int]

	sl.mat <- array(data = NA, dim = c(grid.diam, grid.diam, 2))
	sl.mat[,,1][pres.out] <- mat[,,1][pres.out]
	sl.mat[,,2][pres.out] <- mat[,,2][pres.out]

 if (is.null(hex.coords)) {
		s <- sqrt(3)
		x <- c(cell.radius, cell.radius/2, -cell.radius/2, -cell.radius, -cell.radius/2, cell.radius/2)
		y <- c(0, -cell.radius*s/2, -cell.radius*s/2, 0, cell.radius*s/2, cell.radius*s/2)
		hex.coords <-  list(x = x, y = y)
	}

	d.mat.comp <- matrix(data = NA, nrow = grid.diam, ncol = grid.diam)
	d.mat.comp[pres.cells] <- int.mat
	d.mat.comp[pres.out] <- NA
	d.mat.comp

	sl.mat.comp <- matrix(data = NA, nrow = grid.diam, ncol = grid.diam)
	sl.mat.comp[pres.cells] <- peri.mat
	sl.mat.comp[pres.int] <- NA
	sl.mat.comp


		brs.int <- length(pres.int)
		brs.out <- length(pres.out)

		if (all(int.mat == 0) == T){
			d.col.vir <- d.colors[1]
			} else {
				d.col.vir <- colorRampPalette(d.colors)(brs.int)[as.numeric(cut(d.mat.comp, breaks = brs.int))]
			}
		if (all(peri.mat == 0) == T) {
			sl.col.vir <- sl.colors[1]
			
		} else {
			sl.col.vir <- colorRampPalette(sl.colors)(brs.out)[as.numeric(cut(sl.mat.comp, breaks = brs.out))]
		}

		d.col.mat <- matrix(data = NA, nrow = grid.diam, ncol = grid.diam)
		sl.col.mat <- matrix(data = NA, nrow = grid.diam, ncol = grid.diam)
		d.col.mat[pres.int] <- d.col.vir[!is.na(d.col.vir)]
		sl.col.mat[pres.out] <- sl.col.vir[!is.na(sl.col.vir)]


for (i in 1:grid.diam){
	for (j in 1:grid.diam){
		if(i ==1 && j == 1) {plot(mat[i , j, 1], mat[i , j, 2], type = "n", xlim = c( min(mat[,, 1], na.rm = T)-1, max(mat[,, 1], na.rm = T)+1), ylim = c(0+sqrt(3)/2, max(mat[,, 2]+sqrt(3)/2, na.rm = T)), ann = F, axes = F, frame = T)
			}
		polygon(d.mat[i , j, 1] + hex.coords$x, d.mat[i , j, 2] + hex.coords$y, col = d.col.mat[i, j], ...)
		polygon(sl.mat[i , j, 1] + hex.coords$x, sl.mat[i , j, 2] + hex.coords$y, col = sl.col.mat[i, j], ...)
		if (cell.vals == T) {text(d.mat[i , j, 1], d.mat[i , j, 2], labels = sprintf("%.2f", d.mat.comp[i,j]))}
		if (cell.vals == T) {text(sl.mat[i , j, 1], sl.mat[i , j, 2], labels = sprintf("%.2f", sl.mat.comp[i,j]))}
	}
}

	} # close function