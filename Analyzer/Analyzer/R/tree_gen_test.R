require(igraph)
require(zoo)
require(rgl)
require(rglPlotCustom)


#reads data in from csv to data.frame
readin <- function()
{
  theFile <- "I:\\Capstone\\Current_Program\\Crawler\\Link_data\\nsuok.edu\\nsuok.csv"
  data <- read.table(file = theFile, sep=",", header=FALSE, fill=TRUE)
  return (data)
}

plot_chart <- function(data)
{
  #replaces each NA with the most recent non-NA
  data[1:ncol(data)] <- sapply(data[1:ncol(data)], na.locf, na.rm=FALSE)
  #remove any remaining NA
  data <- na.omit(data)
  
  #combine data by columns
  edges <- rbind(data[1:2], setNames(data[2:3], names(data[1:2])))
  
  #generate base chart of data
  chart <- graph.data.frame(edges)
  
  #remove curved lines from graph
  E(chart)$curved <- 0
  
  #win.graph(150,100,4)
  
  #plot data as 3D tree
  #vertex.size sets size of points at each vertex (set to 0 to avoid overlapping text)
  #edge.arrow.size sets size of arrow leading to each point(set to 0 for readability)
  #layout set to "PENDING"
  #asp set to 0 so model will cover entire window
  rglplot2(chart, vertex.size = 0, vertex.label.cex=0.6,#vertex.label=NA,
          edge.arrow.size = 0, edge.width = 0.25,
          layout = layout.fruchterman.reingold(chart, dim=3),
          asp = 0)  
}

test <- function()
{
  t <- readin()
  plot_chart(t)
}