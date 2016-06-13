# Hexagonal plotting
This function allows you to plot your data as a hexagonal grid.  The list of arguments inclues
*`int.mat`   takes the dog matrix at a single time
*`peri.mat`   takes the sea lion matrix at a single time
*`coords`   takes a matrix of coordinates for the cells in a hexgrid
*`NA.cells`   takes a vector of cells that NA-ize the square matrix into a hexagon
*`cell.radius`   takes a number representative of the radius from the center to the six points of a hexagon
*`grid.radius`  takes the number of rings from the cetner cell
*`hex.corrds`   takes a vector of hexagonal coordiantes of a cell from (0,0)
*`cell.vals`   takes a logical statement about the presennce of cell values
*`d.colors`   takes a vector of color values for the dogs
*`sl.colors`   takes a vector of color values for the sea lions
*`...` takes arguments that will be passed to 

The only two required arguments are `int.mat` and `peri.mat`.  I'd add `grid.radius` too, but it's not essential, nor does it really save much time.
