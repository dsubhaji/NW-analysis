library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Growth Network Analysis"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      
      fileInput('file1', 'Choose CSV File from local drive, adjusting parameters if necessary',
                accept=c('text/csv', 'text/comma-separated-values,text/plain')),
      
      fileInput('file2', 'Choose CSV File from local drive, adjusting parameters if necessary',
                accept=c('text/csv', 'text/comma-separated-values,text/plain')),
      
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   'Comma'),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   'Double Quote'),
      tags$head(tags$style(type="text/css",
                           "label.radio { display: inline-block; margin:0 10 0 0;  }",
                           ".radio input[type=\"radio\"] { float: none; }")),
      # Specification of range within an interval
      sliderInput("range", "Random Affiliation:",
                  min = 0, max = 1, value = c(0.1,0.5)),
      
      selectInput("var",
                  label="Choose the number of steps",
                  choices = list("0.05","0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9"),
                  selected = "0.1")
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Data File",
                 
                 tableOutput('contents1'),               
                 value = 1),
        tabPanel("Data File 2",
                 tableOutput('contents2'),
                 value=2),
        
        tabPanel("Clusturing Coefficient",
                 plotOutput("clusterPlot"),
                 value = 3),
        tabPanel("Degree Centrality",
                 plotOutput("degreePlot"),
                 value = 4),
        tabPanel("Betweenness Centrality",
                 plotOutput("betweennessPlot"),
                 value=5),
        id="tabs1")
      
      
    )
  )
  
  
))
