library(shiny)
library(visNetwork)

shinyUI(navbarPage(
  tags$strong("Movies"),
  tabPanel(
    strong("About the data"), 
    mainPanel(
      fluidRow(
        h2("Our study plan"),
        HTML("<div><img src='new_rotten.pdf' width='1000px'/></div>")
      )
    )
  ), 
  tabPanel(
    strong("Our scores"), 
    mainPanel(
      h2("Our meta score for movies"),
      HTML("<img src='heatMapOrder.png'/>"), 
      fluidRow(
        column(
          10, 
          h2("Our score for actors"), 
          plotOutput("actorScore", width = "700px", height = "500px")
        ), 
        column(
          2,
          HTML("<img src='Gustav_Frolich.jpeg'/>"), 
          HTML("<img src='samuel_jackson.jpeg'/>"), 
          HTML("<img src='joyce_tailer.jpeg'/>")
        )
      )
    )
  ), 
  tabPanel(
    strong("Network"), 
    mainPanel(
      h2("Network of actors"), 
      visNetworkOutput("plotNetwork", width = "100%", height = "800px")
    )
  )
))
  
