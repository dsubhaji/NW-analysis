shinyUI(fluidPage(
  titlePanel("Network Family Metrics Visualizer"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
                multiple = TRUE,
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"')
      
        
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Degree Centrality",
                 plotOutput("Degree"),               
                 value = 1),
        tabPanel("Average Clustering Coefficient",
                 plotOutput("cluster"),
                 value=2),
        tabPanel("Diameter",
                 plotOutput("diameter"),               
                 value = 3),
        tabPanel("Average Degree",
                 plotOutput("avgdegree"),
                 value=4),
        tabPanel("Modularity",
                 plotOutput("modularity"),
                 value=5),
        tabPanel("Density",
                 plotOutput("density"),
                 value=6),
        tabPanel("Average Separation",
                 plotOutput("avgsep"),
                 value=7),
        id="tabs1")
    )
  )))
