require(shiny)
require(TeachingDemos)
require(igraph)
require(zoo)
require(rgl)
require(rglPlotCustom)

shinyServer(function(input, output){ 
  
  #function to export tree as PNG after rendering as an image
  observe({
    if (input$exportPNG == 0)
    {
      return()
    }
    
    rgl.snapshot("tree.png", "png", top = TRUE)
  })
  
  #function to export as OBJ
  observe({
    if (input$exportOBJ == 0)
    {
      return()
    }
    
    writeOBJ("tree.obj")
  })
  
  #function to generate tree and display data in table
  output$contents <- renderTable({
    input$submit #set dependency on submit button
    mydata = NULL
    
    isolate({
      theFile <- input$file #get file info from input
      mydata <- NULL
    
      if (is.null(theFile)) #check if file is null
      {
        mydata <- NULL        
      } else
      {
        #determine how to read file based on radio buttons
        switch(input$fileType,
             "Crawled" = (mydata <- read.table(file = theFile$datapath, sep=",", header=FALSE, fill=TRUE)),
             "Duplicates" = (mydata <- read.table(file = theFile$datapath, header = TRUE, sep=",")),
             "Tocrawl" = (mydata <- read.table(file = theFile$datapath, header = TRUE, sep=",")))
      }
      
      #check if button has been presses
      if (input$submit == 0) #if not
      {
        return() #do nothing
      } else
      {
        #if pressed, check which function to call
        switch(input$fileType,
               "Crawled" = plot_crawled(mydata, input$fontSize, input$displayLabel),
               "Duplicates" = plot_dup(mydata),
               "Tocrawl" = plot_dup(mydata),
               NULL)
      } #close else
      
      mydata
    }) #close isolate  
  }) #close renderTable
  
  #function to generate interactive scatterplot for duplicates/tocrawl files
  plot_dup <- function(mydata)
  {
    #open graph window
    win.graph(40,25,8)
    #change margins for plot (bottom, left, top, right)
    #defaults= 5.1, 4.1, 4.1, 2.1
    par(mar=c(5.1,4,4.1,2))
    #generate plot with interactive points for info
    HWidentify(mydata$Link, mydata$Occurrence, label = paste(mydata$Link, "-", mydata$Occurrence), main="Link Occurrences",
               sub=paste(nrow(mydata), " Links"), xlab = "Mouseover for Link",
               ylab = "Occurrence", clean=TRUE,
               adj=c(0.5, 3), xaxt="n")
    
    #Current test to wite plot to saved image
    #dev.copy(jpeg, filename="I:\\test.jpg")
  }
  
  #function to generate 3D tree for crawled files
  plot_crawled <- function(mydata, size, displayLabel)
  {
    #replaces each NA with the most recent non-NA
    mydata[1] <- na.locf(mydata[1], na.rm=FALSE)
    
    #remove any remaining NA
    for(i in ncol(mydata):2)
    {
      mydata[[i-1]] <- ifelse(!is.na(mydata[[i]]), na.locf(mydata[[i-1]], na.rm=FALSE),
                              mydata[[i-1]])
    }

    #combine mydata by columns
    edges <- rbind(na.omit(mydata[1:2]), 
                   do.call('rbind', 
                           lapply(1:(ncol(mydata)-2), function(i)
                             na.omit(setNames(mydata[(1+i):(2+i)],
                                              names(mydata[1:2])))))
    )
    
    #generate base chart of mydata
    chart <- graph.data.frame(edges)
    
    #remove curved lines from graph
    E(chart)$curved <- 0
    
    #plot mydata as 3D tree
    #vertex.size sets size of points at each vertex (set to 0 to avoid overlapping text)
    #edge.arrow.size sets size of arrow leading to each point(set to 0 for readability)
    #vertex.label.cex sets font size of vertex labels
    #vertex.label=NA disables showing of vertex labels
    #layout set to fruchterman.reingold for 3D spherical tree
    #asp set to 0 so model will cover entire window
    #if displayLabel checkbox is checked, show labels using size from sliders
    if(displayLabel == TRUE)
    {
      rglplot2(chart, 
              vertex.size = 0,  
              vertex.label.cex=size,
              edge.arrow.size = 0, edge.width = 0.25,
              layout = layout.fruchterman.reingold(chart, dim=3),
              asp = 0)
    } else #if unchecked, don't display labels
    {
      rglplot2(chart, 
               vertex.size = 2, 
               vertex.label=NA, 
               edge.arrow.size = 0, edge.width = 0.5,
               layout = layout.fruchterman.reingold(chart, dim=3),
               asp = 0) 
    }
  }
  
})