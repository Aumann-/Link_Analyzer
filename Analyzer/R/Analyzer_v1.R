require(TeachingDemos)
require(igraph)
require(zoo)
require(rgl)
require(rglPlotCustom)

#reads data in from csv to data.frame
readin <- function(c)
{
  if(as.numeric(c) == 1)
  {
    theFile <- "I:\\Capstone\\Current_Program\\Crawler\\Link_data\\nsuok.edu\\nsuok.csv"
    data <- read.table(file = theFile, sep=",", header=FALSE, fill=TRUE)
  }
  
  if(as.numeric(c) == 2)
  {
  theFile <- "I:\\Capstone\\Current_Program\\Crawler\\Link_data\\nsuok.edu\\duplicates.csv"
  data <- read.table(file = theFile, header = TRUE, sep=",")
  }
  
  return (data)
}

plot_dup <- function(t)
{
  #open graph window
  win.graph(40,25,8)
  #change margins for plot (bottom, left, top, right)
  #defaults= 5.1, 4.1, 4.1, 2.1
  par(mar=c(5.1,4,4.1,2))
  #generate plot with interactive points for info
  HWidentify(t$Link, t$Occurrence, label = paste(t$Link, "-", t$Occurrence), main="Link Occurrences",
             sub=paste(nrow(t), " Links"), xlab = "Mouseover for Link",
             ylab = "Occurrence", clean=TRUE,
             adj=c(0.5, 3), xaxt="n")
  #After right clicking to escape window, generates trail lines from points for ID
  lines(t$Link, t$Occurrence, type="h", lwd = 2)
}

plot_crawled <- function(data)
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
  print("1. Crawled file.")
  print("2. Duplicates/tocrawl file.")
  c <- readline("Choice: ")
  
  a <- readin(c)
  
  if(as.numeric(c) == 1)
  {
    plot_crawled(a)
  }
  
  if(as.numeric(c) == 2)
  {
    plot_dup(a)
  }
  
}