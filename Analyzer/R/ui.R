require(shiny)

shinyUI(fluidPage
        (
          titlePanel("Link Analyzer"),
          
          sidebarLayout
          (
            sidebarPanel
            (
              fluidRow
              (
                fileInput("file", label = h3("Choose File"), multiple = FALSE), 
                helpText("Files are limited to 5MB.")
              ),
              
              fluidRow
              (
                radioButtons("fileType", label = h3("File Type"),
                             choices = list("Crawled",
                                            "Duplicates",
                                            "Tocrawl"),
                             selected = 0)
              ),
              
              conditionalPanel(
                condition = "input.fileType == 'Crawled'",
                
                h3("Crawled File Options"),
                #helpText("These options are only used when working with a crawled file type."),
                br(),
                
                fluidRow
                (
                    checkboxInput("displayLabel", label = "Display Vertex Labels", value = TRUE)
                ),
                
                fluidRow
                (
                  sliderInput("fontSize",
                              label = "Crawled Font Multiplier",
                              min = 0.1, max = 2, value = c(1)),
                  helpText("Only changes if 'Display Vertex Labels' is checked.")
                )
              ),
              fluidRow
              (
                actionButton("submit", label = "Submit")
              ),
              fluidRow
              (
                actionButton("reset", label = "Reset Graphics Devices"),
                helpText("Use to reset the graphics devices after rendering a plot.")
              ),
              conditionalPanel(
                condition = "input.fileType == 'Crawled'",
                
                fluidRow
                (
                  actionButton("exportPNG", label = "Export as .PNG"),
                  helpText("Export the currently open 3D model as a 2D .PNG image file.")
                ),
                fluidRow
                (
                  actionButton("exportOBJ", label = "Export as .STL"),
                  helpText("Export the currently open 3D model as a .STL model file.
                           *This feature takes a long time to complete.
                           Please do not close the render window or the application for a minimum of 5 minutes.")
                )
                
              )
            ),
            
            mainPanel
            (     
              fluidRow
              (
                tableOutput("contents")
              )
            )
          )
        ))