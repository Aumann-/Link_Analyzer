require(shiny)

shinyUI(fluidPage
        (
          titlePanel("Test"),
          
          sidebarLayout
          (
            sidebarPanel
            (
              fluidRow
              (
                fileInput("file", label = h3("Choose File"), multiple = FALSE)  
              ),
              
              fluidRow
              (
                radioButtons("fileType", label = h3("File Type"),
                             choices = list("Crawled",
                                            "Duplicates",
                                            "Tocrawl"),
                             selected = "Crawled")
              ),
              
              fluidRow
              (
                sliderInput("fontSize",
                            label = "Crawled Font Multiplier",
                            min = 0.1, max = 2, value = c(1))
              ),
              fluidRow
              (
                actionButton("submit", label = "Submit")
              )
            ),
            
            mainPanel
            (
              fluidRow
              (
                textOutput("test")  
              ),
              fluidRow
              (
                tableOutput("contents")
              )
            )
          )
        ))