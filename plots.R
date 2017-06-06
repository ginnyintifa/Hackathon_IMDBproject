library(grid)
movies <- read.csv("www/movie_score.csv", header = TRUE, 
                   stringsAsFactors = FALSE, 
                   na.strings = c("\\N", "", " ", "NA"))[, -1]
actors <- read.csv("www/actor_score.csv", header = TRUE, 
                   stringsAsFactors = FALSE)[, -1]
edges <- read.csv("www/edges_movies.csv", header = TRUE)

# Make heatmap for movie scores ----
val <- movies[, c(9:22)]
assignScore <- function(z, theta, startFrom = 1) {
  theta <- sort(theta)
  sapply(z,
         function(z) {
           J <- length(theta) + 1
           if (z <= theta[1]) {
             1
           } else if (z > theta[J - 1]) {
             J
           } else {
             for (j in 2:(J - 1)) {
               if ((z > theta[j - 1]) & (z <= theta[j])) {
                 return(j)
               }
             }
           }
         }) - 1 + startFrom
}
setColor <- function(x, breaks, col = "darkorange") {
  colVec <- colorRampPalette(c("seashell", col))(length(breaks) + 1)
  colVec[assignScore(z = x, theta = breaks)]
}
breaksMat <- cbind(1:9, 
                   (1:9) * 1, 
                   (1:9) * 0.5, 
                   (1:9) * 10, 
                   (1:9) * 10, 
                   (1:9) * 10, 
                   (1:9) * 30, 
                   (1:9) * 5,
                   (1:9) * 5000, 
                   (1:9) * 27,
                   (1:9) * 5, 
                   (1:9) * 20,
                   (1:9) * 5, 
                   (1:9) * 0.1)
colMat <- sapply(1:ncol(val), function(j) {
  if (j == ncol(val)) {
    # col <- "steelblue"
    col = "blue"
  } else {
    # col <- "darkorange"
    col <- "red"
  }
  setColor(x = val[, j], breaks = breaksMat[, j], col = col)
})
colMatOrder <- colMat[sort.list(val$overall.rating, decreasing = TRUE), ]
colnames(colMatOrder) <- c(
  "All critics rating", "Top critics rating", "Audience rating", 
  "All critics score", "Top critics score", "Audience score", 
  "No. all critics reviews", "No. top critics reviews", "No. audience ratings", 
  "No. all critics fresh", "No. top critics fresh", 
  "No. all critics fresh", "No. all critics rotten", "Meta score"
)
png("www/heatmapOrder.png", width = 1200, height = 500)
pushViewport(plotViewport(margins = c(6, 1, 1, 8), 
                          xscale = c(0.5, nrow(colMatOrder) + 0.5), 
                          yscale = c(0.5, ncol(colMatOrder) + 0.5)))
for (i in 1:nrow(colMatOrder)) {
  for (j in 1:ncol(colMatOrder)) {
    x <- i
    y <- j
    grid.rect(x = x, y = y, width = 1, height = 1, default.units = "native", 
              gp = gpar(col = NA, fill = colMatOrder[i, j]))
  }
}
grid.text(label = colnames(colMatOrder), 
          y = unit(1:ncol(colMatOrder), "native"), 
          x = unit(1, "npc"), just = "left")
colVecRed <- colorRampPalette(c("seashell", "red"))(10)
colVecBlue <- colorRampPalette(c("seashell", "blue"))(10)
grid.rect(y = rep(-1, 10), x = 1:10, width = 1, height = 1, 
          default.units = "line", gp = gpar(fill = colVecRed))
grid.rect(y = rep(-2, 10), x = 1:10, width = 1, height = 1, 
          default.units = "line", gp = gpar(fill = colVecBlue))
grid.text(seq(from = 0.1, to = 0.9, by = 0.1), rot = -90, just = "left", 
          x = rep(1:9 + 0.5), y = rep(-3, 9), default.units = "line")
popViewport()
dev.off()
