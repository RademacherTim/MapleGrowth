# Script to calculate basic stats for the growth data

# plot growth against estimated age of the tree --------------------------------

# plot growth against estimated size of the tree -------------------------------

# plot growth versus latitude --------------------------------------------------
plot(x = rwEYST$lat, y = rwEYST$rwEYST, pch = 19, col = "#10647011",
     xlab = "Latitude", ylab = "Croissance radiale (mm)", axes = FALSE, 
     xlim = c(35, 50))
axis(side = 1, seq (35, 50, by = 5))
axis(side = 2, las = 1)
abline(h = 5, col = "#91b9a4", lty = 2, lwd = 2)

# plot a historgram of growth --------------------------------------------------
d_rwEYST <- density(rwEYST$rwEYST)
hist(d_rwEYST)
