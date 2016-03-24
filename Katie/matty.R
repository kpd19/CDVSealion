plot.rows <- 3
plot.cols <- 3

par(mfrow=c(plot.rows,plot.cols), oma = rep(1,4), mar = rep(0,4))
for (f in 1:9)
{
  sq.matty <- matrix(data = R_SL[,50*f] , nrow = island.rows , ncol = island.columns)
  rbPal <- colorRampPalette(c("light grey" , "red"))
  col.breaks <- 50
  col.pal <- matrix(rbPal(col.breaks)[as.numeric(cut(sq.matty,breaks = col.breaks))] , nrow = island.rows , byrow = F)
  
  plot(1:island.rows , 1:island.columns , type = "n", xlim = c(0 , island.rows + 1) , ylim = c(0 , island.columns + 1), frame = F, axes = F, xlab = "", ylab = "", lwd = 2)
  for (i in 1:island.rows){
    for (j in 1:island.columns){
      points(i , j , cex = 6.75 , col = col.pal[i,j] , pch = 15)
    }
  }
}