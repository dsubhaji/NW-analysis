library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
  Data <- reactive({
    
    
    # input$file1 will be NULL initially. After the user selects and uploads a 
    # file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
    # columns. The 'datapath' column will contain the local filenames where the 
    # data can be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    df.raw <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    #info <- list(df.raw)
    return(df.raw)
  })
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    Data()
  })
  
  output$clusterPlot <- renderPlot({
    if (is.null(input$file1)) { return() }
    #cluster<-read.table(Data())
    min <- input$range[1]
    max <- input$range[2]
    #len <- dim(cluster)[1]
    
    n <- as.numeric(input$var)
    
    new <- subset(Data() , Data()$prob>=min & Data()$prob<=max)
    
    plot(new$prob,new$cluster, main="Growth Model Plots",xlab="Random Affilation",ylab="Clusturing Coeficient",xaxt="n",yaxt="n",col="blue",type='l')
    
    #plot(cluster$V2,cluster$V1, main="Growth Model Plots",xlab="Random Affilation",ylab="Clusturing Coeficient",xaxt="n",yaxt="n")
    axis(side=1, at=seq(min,max,by=n))
    axis(side=2, at=seq(0,1,by=0.01))
    
  })
  
  output$degreePlot <- renderPlot({
    if (is.null(input$file1)) { return() }
    #cluster<-read.table(Data())
    min <- input$range[1]
    max <- input$range[2]
    #len <- dim(cluster)[1]
    
    n <- as.numeric(input$var)
    
    new <- subset(Data() , Data()$prob>=min & Data()$prob<=max)
    
    plot(new$prob,new$degree, main="Growth Model Plots",xlab="Random Affilation",ylab="Degree(The graph level centrality index)",xaxt="n",yaxt="n", col="Red",type='l')
    
    #plot(cluster$V2,cluster$V1, main="Growth Model Plots",xlab="Random Affilation",ylab="Clusturing Coeficient",xaxt="n",yaxt="n")
    axis(side=1, at=seq(min,max,by=n))
    axis(side=2, at=seq(0,1,by=0.01))
    
  })
  output$betweennessPlot <- renderPlot({
    if (is.null(input$file1)) { return() }
    #cluster<-read.table(Data())
    min <- input$range[1]
    max <- input$range[2]
    #len <- dim(cluster)[1]
    
    n <- as.numeric(input$var)
    
    new <- subset(Data() , Data()$prob>=min & Data()$prob<=max)
    
    plot(new$prob,new$betweenness, main="Growth Model Plots",xlab="Random Affilation",ylab="Betweenness(The graph level centrality index)",xaxt="n",yaxt="n", col="dark violet",type='l')
    
    #plot(cluster$V2,cluster$V1, main="Growth Model Plots",xlab="Random Affilation",ylab="Clusturing Coeficient",xaxt="n",yaxt="n")
    axis(side=1, at=seq(min,max,by=n))
    axis(side=2, at=seq(0,1,by=0.01))
    
  })
  
})
