# multi-panel plots #

e.levels <- unique(info.arr[,which(colnames(info.arr) == "epsilon")]) # epsilon levels
e.order <- order(e.levels) # finds order of epsilon values
ps.levels <- unique(info.arr[,which(colnames(info.arr) == "pack.size")]) #pack size levels
ps.order <- order(ps.levels) #finds order of pack size values
n.sims <- nrow(info.arr) # number of simulations
plot.order <- c("S_D", "E_D", "S_SL", "E_SL",
				"I_D", "R_D", "I_SL", "R_SL",
				"D_D", "TD_D", "D_SL", "TD_SL") # order for panels
plot.seq <- match(plot.order, row.names(stats.arr)) #makes a sequence from panel order
x.seq <- seq(1, n.sims, by = n.sims/length(e.levels)) # names an initial sequence
col.ramp <- colorRampPalette(c("red", "blue")) # makes a color ramp
col.pal <- col.ramp(length(ps.levels)) # makes the colors

print.wd <- "~/Dropbox/Projects/*DixonCDV/TestPrints" # set a print directory
for (k in 1:dim(stats.arr)[2]){
pdf(paste(print.wd, "/", dimnames(stats.arr)[[2]][k], ".pdf", sep = ""), width = 6, height = 5)
par(mfrow = c(3, 4), mar = rep(0.15, 4), oma = c(4, 1,1.5, 1))
for (i in 1:nrow(stats.arr)){
	y.min <- min(stats.arr[plot.seq[i], k,])
	y.max <- max(stats.arr[plot.seq[i], k,])
	plot(e.levels, stats.arr[plot.seq[i] , k, x.seq], xaxt = "n", yaxt = "n", ylim = c(y.min, y.max), type = "n")

	for (j in 1:length(ps.levels)){
	l.seq <- seq(ps.order[j], n.sims, by = n.sims/length(e.levels))
	lines(e.levels[e.order], stats.arr[plot.seq[i] , k, l.seq[e.order]], col = col.pal[j], lwd = 1.5)
	}
if (i > 8) {axis(1)
	}
	text(x = min(e.levels), y = y.max*0.99, labels = plot.order[i], adj = c(.1, .5))
}
mtext(outer = T, text = "Epsilon", side = 1, line = 2.5)
mtext(outer = T, text = c("Dogs", "Seal Lions"), side = 3, line = 0, at = c(0.25, 0.75))
dev.off()
}