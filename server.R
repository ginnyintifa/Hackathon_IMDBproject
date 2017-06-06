library(shiny)
library(visNetwork)

function(input, output) {
  actors <- read.csv("www/actor_score.csv", header = TRUE, 
                     stringsAsFactors = FALSE)[, -1]
  actor_edges <- read.csv("www/edges_movies.csv", header = TRUE, 
                          stringsAsFactors = FALSE)[, -1]
  id <- sort(unique(c(actor_edges$actorIDFrom, actor_edges$actorIDTo)))
  ae <- data.frame(from = factor(actor_edges$actorID, levels = id),
                   to = factor(actor_edges$actorIDTo, levels = id))
  actor_nodes <- actors[actors$actorID %in%
                          c(actor_edges$actorIDFrom, actor_edges$actorIDTo), ]
  an <- data.frame(id = factor(actor_nodes$actorID, levels = id),
                   label = actor_nodes$actorName, 
                   score = actor_nodes$actor.movieNO)
  an$type <- sapply(an$id, function(id) {
    actor_edges$topquality[which(actor_edges$actorID == id)][1]
  })
  
  output$actorScore <- renderPlot({
    plot(x = actors$actor.movieNO, y = actors$actor.rating, pch = 20, 
         xlab = "Number of movies an actor is in", 
         ylab = "Meta score for actors", cex = 2, 
         col = adjustcolor("steelblue", alpha.f = 0.5))
  })
  output$plotNetwork <- renderVisNetwork({
    an$shape <- "dot"
    an$shadow <- TRUE # Nodes will drop shadow
    an$color <- c("steelblue", "darkorange")[an$type]
    an$size <- actor_nodes$actor.rating * 10
    # an$title <- an$score
    an$title <- an$label
    an$borderWidth <- 2 # Node border width
    
    visNetwork(an, ae, width = "100%", height = "1000px", stabilization = TRUE)
  })
}
