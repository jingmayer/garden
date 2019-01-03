library(shiny)
library(igraph)
library(gplots)
library(sqldf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(visNetwork)

shinyServer(function(input, output) {
	
	#define the topics and place the value returns in vector of options
	output$Box1 <- renderUI ({
 	inFile <- input$file1
    if (is.null(inFile))
    return(NULL)   
    edge <- read.csv(inFile$datapath, header=TRUE)
   	edge$type <- edge$group
   	edge <- sqldf("select type, count(type) from edge group by 1")
   	options <- as.vector(edge$type)
    if (is.null(options))
    options <- character(0)
   selectInput("sector","network diagram for a discussion topic:",c(options))
})

  
  #filter the data based upon selectionInput and produce its visNetwork

  output$visnetwork <- renderVisNetwork({
   	inFile <- input$file1
    if (is.null(inFile))
    return(NULL)
   	edges <- read.csv(inFile$datapath, header=TRUE)
   	edges<-data.frame(edges)
    edges$type <- edges$group
   	selection <- as.vector(input$sector)
    if (is.null(selection))
    return(NULL)
  	edges <- fn$sqldf(sprintf( "select * from edges where type = '%s'",selection))
  	edges<-edges[c(1,2)]
    edges$from <- as.character(edges$from)
  	edges$to <- as.character(edges$to)
  	edges <- data.frame(edges)
  	nodes<-data.frame(id = c(edges[,"from"], edges[,"to"]))
  	nodes$id <- as.character(nodes$id)
  	nodes$label <- as.character(nodes$id)
  	nodes <- nodes[!duplicated(nodes[c(1,2)]), ]
  	nodes <- nodes[order(nodes$id),c(1,2)]
   	visNetwork(nodes, edges, width = "100%") %>% visEdges(arrows = 'from') %>% visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>% visInteraction(navigationButtons = TRUE)
})

#community
	output$visgroup <- renderVisNetwork({
   	inFile <- input$file1
    if (is.null(inFile))
    return(NULL)
   	edges <- read.csv(inFile$datapath, header=TRUE)
   	edges<-data.frame(edges)
    edges$type <- edges$group
   	selection <- as.vector(input$sector)
    if (is.null(selection))
    return(NULL)
  	edges <- fn$sqldf(sprintf( "select * from edges where type = '%s'",selection))
  	edges<-edges[c(1,2)]
    edges$from <- as.character(edges$from)
  	edges$to <- as.character(edges$to)
  	edges <- data.frame(edges)
    el<-as.matrix(edges)
	g <- graph.edgelist(el, directed=TRUE)
	wc <- cluster_walktrap(g)
	df <- data.frame(cbind(group=membership(wc)))
	nodes<-tibble::rownames_to_column(df, var="id")
	nodes$label<-nodes$id
	set.seed(123)
	visNetwork(nodes, edges, width = "100%") %>% 
 	visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>% visInteraction(navigationButtons = TRUE) %>% visLegend()
})


})
