setwd("F:\\RProjects\\camilla")
#Specify list of packages and install any missing packages and load
list.of.packages <- c("DT", "shiny", "tidyverse", "sna", "ggplot2", "GGally", "fmsb", "shinyWidgets"
                      ,"networkD3" ,"formattable", "plotly", "readxl", "dplyr", "tidyr", "data.table", "d3r")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, library, character.only = TRUE)

bd = read.csv("IMDB-Movie-Data.csv")
df = data.frame("Genre" = unique(unlist(bd[c("Genre1","Genre2", "Genre3")])))
df = data.frame("Genre" = df[!(is.na(df$Genre) | df$Genre==""), ])

af = data.frame("Actor" = unique(unlist(bd[c("Actor1","Actor2", "Actor3", "Actor4")])))
af = data.frame("Actor" = af[!(is.na(af$Actor) | af$Actor==""), ])

ui <- fluidPage(
  navbarPage("IMDB Explorer",
             
        tabPanel("Movie Exploration",
              
              sidebarLayout(
                sidebarPanel(
                  
                  sliderInput("max",
                              "Number of films:",
                              min = 1,
                              max = 50,
                              value = 10),
                  
                  selectInput(inputId = "metric",
                              label = "Choose a metric:",
                              choices = c("Rating"="Rating"
                                          ,"Votes"="Votes",
                                          "Revenue"="Revenue",
                                          "Metascore"="Metascore")),
                  
                  selectInput(inputId = "genre",
                              label = "Choose a genre:",
                              choices = df$Genre           
                  ),
                  
                  selectInput(inputId = "year",
                              label = "Choose a year",
                              choices = seq(2008,2016,1)           
                  ),
                  
                  uiOutput("selMov")
                ),
                
                
                
                # Show a plot 
                mainPanel(
                  h2("Top Movies"),
                  plotlyOutput("barplotTop"), 
                
                  h2("Radarplot of selected movies"),
                  plotlyOutput("radarP")
            
                )
              )
        ),
       
       
        tabPanel("Actor Exploration",
                
                sidebarLayout(
                  sidebarPanel(
                    
                    selectInput(inputId = "ActorSel",
                                label = "Select Actor",
                                choices = af$Actor           
                    )
                    
                  ),
                  
                  mainPanel(
      
                    formattableOutput("actorStats") ,
                    h2("Network Plot"),
                    forceNetworkOutput("actorNetworks", width = "100%", height = "700px")
            
                  )
                )
              )
             
  )
  
)





server <- function(input, output, session) {
  
  #Filter by max, metric, genre, year
  getFD <- reactive({
    
    max = input$max
    metric = input$metric
    genre = input$genre
    year = input$year
    
    sub_data = bd[which((bd$Genre1==genre | bd$Genre2==genre | bd$Genre3==genre)& bd$Year==year),]
    
    ## name of movies with order of popularity using a metric
    sub_data = sub_data[,c("Title", metric)]
    sub_data = sub_data[order(-sub_data[,2]),]
    sub_data = head(sub_data, max)
  
    return(sub_data)
  })
  

  output$actorNetworks <- renderForceNetwork({
    
    actorName = input$ActorSel
    #actorName = "Vin Diesel"
    sub_data = bd[which((bd$Actor1==actorName | bd$Actor2==actorName |
                           bd$Actor3==actorName  | bd$Actor4==actorName)),]
    
    sub_data = data.frame(sub_data, stringsAsFactors = FALSE)
    
    
    directorDF = data.frame("Director" = unique(unlist(sub_data[c("Director")])))
    directorDF = data.frame("Director" = directorDF[!(is.na(directorDF$Director) | directorDF$Director==""), ])
    
  
    co_actorsDF = data.frame("CoActors" = unique(unlist(sub_data[c("Actor1","Actor2", "Actor3", "Actor4")])))
    co_actorsDF = data.frame("CoActors" = co_actorsDF[!(is.na(co_actorsDF$CoActors) 
                                                        | co_actorsDF$CoActors=="" 
                                                        | co_actorsDF$CoActors==actorName ), ])
    
    src <- rep(actorName,nrow(directorDF)+nrow(co_actorsDF))
    target <- c(as.character(directorDF$Director),as.character(co_actorsDF$CoActors))
    networkData <- data.frame(src, target, stringsAsFactors = FALSE)
    
    
    nodes <- data.frame(name = unique(c(src, target)), stringsAsFactors = FALSE)
    nodes$id <- 0:(nrow(nodes) - 1)
  
    # create a data frame of the edges that uses id 0:9 instead of their names
    edges <- networkData %>%
      left_join(nodes, by = c("src" = "name")) %>%
      select(-src) %>%
      rename(source = id) %>%
      left_join(nodes, by = c("target" = "name")) %>%
      select(-target) %>%
      rename(target = id)
    
    edges$width <- 1
    
    # make a grouping variable that will match to colours
    nodes$group <- ifelse(nodes$name %in% directorDF$Director, "Director", "Actor")
    
    
    ColourScale <- 'd3.scaleOrdinal()
    .domain(["Director", "Actor"])
    .range(["#FF6900", "#694489"]);'
    
    # simple with default colours
    forceNetwork(Links = edges, Nodes = nodes, 
                 Source = "source",
                 Target = "target",
                 NodeID ="name",
                 Group = "group",
                 Value = "width",
                 opacity = 0.9,
                 zoom = TRUE,
                 legend = TRUE,
                  colourScale = JS(ColourScale))
    

  })
  
  
  output$actorStats <- renderFormattable({
    
    actorName = input$ActorSel
    sub_data = bd[which((bd$Actor1==actorName | bd$Actor2==actorName |
                           bd$Actor3==actorName  | bd$Actor4==actorName)),]
    
    sub_data[is.na(sub_data)] <- 0
    
    avgRating = mean(sub_data$Rating)
    avgVotes = mean(sub_data$Votes)
    avgMetascore = mean(sub_data$Metascore)
    avgRevenue = mean(sub_data$Revenue)
    
    SCD = data.frame(
      "Avg Rating" = avgRating,
      "Avg Votes" = avgVotes,
      "Avg Metascore" = avgMetascore,
      "Avg Revenue" = avgRevenue
    )
    formattable(SCD, align = "r")
    
  })
  
  output$barplotTop <- renderPlotly({
    
    metric = input$metric
    sub_data = getFD()
    sub_data = data.frame(sub_data, stringsAsFactors = FALSE)

    sub_data[,1] <- factor(sub_data[,1] , levels = unique(sub_data[,1] )[order(sub_data[,2] , decreasing = TRUE)])

    p <- plot_ly(
      x = sub_data[,1],
      y = sub_data[,2],
      type = "bar",
      color = sub_data[,1]
    ) %>% 
      layout(showlegend = TRUE,
             xaxis = list(title = ""),
             yaxis = list(title = metric))
    
    return(p)
  })
  
  
  
  #This outputs selection input
  output$selMov <- renderUI({
    
    sub_data = getFD()
    sub_data = data.frame(sub_data, stringsAsFactors = FALSE)
    sub_data[,1] <- factor(sub_data[,1] , levels = unique(sub_data[,1] )[order(sub_data[,2] , decreasing = TRUE)])

    #Picker Input contains the list of movies
    pickerInput("selRadarMovies","Select Movies for Radar Plot", choices=as.character(sub_data$Title),
                options = list(`actions-box` = TRUE), multiple = T)

  })

  output$radarP <- renderPlotly({

    selMoviesList = input$selRadarMovies

    p <- plot_ly(
      type = 'scatterpolar',
      fill = 'toself'
    ) 
    
    if (length(selMoviesList)>0){
          
        max = input$max
        metric = input$metric
        genre = input$genre
        year = input$year
        
        bd[is.na(bd)] <- 0
        bd$Rating = (bd$Rating - min(bd$Rating)) / (max(bd$Rating)- min(bd$Rating))
        bd$Votes = (bd$Votes - min(bd$Votes)) / (max(bd$Votes)- min(bd$Votes))
        bd$Revenue = (bd$Revenue - min(bd$Revenue)) / (max(bd$Revenue)- min(bd$Revenue))
        bd$Metascore = (bd$Metascore - min(bd$Metascore)) / (max(bd$Metascore)- min(bd$Metascore))
        
        sub_data = bd[which((bd$Genre1==genre | bd$Genre2==genre | bd$Genre3==genre)& bd$Year==year),]
        sub_data = dplyr::filter(sub_data, Title %in% selMoviesList)
    
        sub_data = sub_data[,c("Title", "Rating", "Votes", "Revenue", "Metascore")]
        print(sub_data)
    
        for(i in 1:nrow(sub_data)){
          p <- add_trace(p,
              r = c(sub_data[i,2],sub_data[i,3],sub_data[i,4],sub_data[i,5],sub_data[i,2]),
              theta = c('Rating','Votes','Revenue', 'Metascore', 'Rating') ,
              name = sub_data[i,1]
            ) 
        }
        
        p <- p %>%
          layout(
            polar = list(
              radialaxis = list(
                visible = T,
                range = c(0,1)
              )
            )
          )
        
    }
    
    return(p)
    
  })
  

  
}


shinyApp(ui, server)