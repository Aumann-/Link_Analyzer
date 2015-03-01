require(TeachingDemos)

#reads data in from csv to data.frame
readin <- function()
{
   theFile <- "E:\\Capstone\\Current_Program\\Crawler\\Link_data\\nsuok.edu\\duplicates.csv"
   data <- read.table(file = theFile, header = TRUE, sep=",")
   return (data)
}

gen_plot <- function(t)
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

#function to auto-call other functions for simplicity
test <- function()
{
  a <- readin()
  gen_plot(a)
}