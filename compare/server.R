library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot 
  
  
  output$contents1 <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile1 <- input$file1
    
    if (is.null(inFile1))
      return(NULL)
    
    read.csv(inFile1$datapath, header=input$header,sep=input$sep, 
             quote=input$quote)
  })
  
  output$contents2 <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile2 <- input$file2
    
    if (is.null(inFile2))
      return(NULL)
    read.csv(inFile2$datapath, header=input$header,sep=input$sep, 
             quote=input$quote)
    
  })
  
  output$clusterPlot <- renderPlot({
    
    inFile1 <- input$file1
    inFile2 <- input$file2
    
    if (is.null(input$file1)) { return() }
    #cluster<-read.table(Data())
    min <- input$range[1]
    max <- input$range[2]
    #len <- dim(cluster)[1]
    
    if (is.null(input$file2)) { return() }
    
    n <- as.numeric(input$var)
    
    file1 <- read.csv(inFile1$datapath, header=input$header,sep=input$sep, 
                      quote=input$quote)
    
    file2 <- read.csv(inFile2$datapath, header=input$header,sep=input$sep, 
                      quote=input$quote)
    
    new1 <- subset(file1 , file1$prob>=min & file1$prob<=max)
    new2 <- subset(file2 , file2$prob>=min & file2$prob<=max)
    
    plot(new1$prob,new1$cluster, main="Growth Model Plots",xlab="Random Affilation",ylab="Clusturing Coeficient",col="blue",type='b')
    par(new=T)
    plot(new2$prob,new2$cluster,xaxt='n',yaxt='n',col="Red",type='b',ann=FALSE)
    
    #plot(cluster$V2,cluster$V1, main="Growth Model Plots",xlab="Random Affilation",ylab="Clusturing Coeficient",xaxt="n",yaxt="n")
    axis(side=1, at=seq(min,max,by=n))
    #axis(side=2, at=seq(0,1,by=0.01))
    
  })
  
  output$degreePlot <- renderPlot({
    
    inFile1 <- input$file1
    inFile2 <- input$file2
    
    if (is.null(input$file1)) { return() }
    #cluster<-read.table(Data())
    min <- input$range[1]
    max <- input$range[2]
    #len <- dim(cluster)[1]
    
    n <- as.numeric(input$var)
    
    file1 <- read.csv(inFile1$datapath, header=input$header,sep=input$sep, 
                      quote=input$quote)
    
    file2 <- read.csv(inFile2$datapath, header=input$header,sep=input$sep, 
                      quote=input$quote)
    
    new1 <- subset(file1 , file1$prob>=min & file1$prob<=max)
    new2 <- subset(file2 , file2$prob>=min & file2$prob<=max)
    
    plot(new1$prob,new1$degree, main="Growth Model Plots",xlab="Random Affilation",ylab="Clusturing Coeficient",col="blue",type='b')
    par(new=T)
    plot(new2$prob,new2$degree,xaxt='n',yaxt='n',col="Red",type='b',ann=FALSE)
    
    #plot(cluster$V2,cluster$V1, main="Growth Model Plots",xlab="Random Affilation",ylab="Clusturing Coeficient",xaxt="n",yaxt="n")
    axis(side=1, at=seq(min,max,by=n))
    #axis(side=2, at=seq(0,1,by=0.01))
    
  })
  output$betweennessPlot <- renderPlot({
    
    inFile1 <- input$file1
    inFile2 <- input$file2
    
    if (is.null(input$file1)) { return() }
    #cluster<-read.table(Data())
    min <- input$range[1]
    max <- input$range[2]
    #len <- dim(cluster)[1]
    
    n <- as.numeric(input$var)
    
    file1 <- read.csv(inFile1$datapath, header=input$header,sep=input$sep, 
                      quote=input$quote)
    
    file2 <- read.csv(inFile2$datapath, header=input$header,sep=input$sep, 
                      quote=input$quote)
    
    new1 <- subset(file1 , file1$prob>=min & file1$prob<=max)
    new2 <- subset(file2 , file2$prob>=min & file2$prob<=max)
    
    plot(new1$prob,new1$betweenness, main="Growth Model Plots",xlab="Random Affilation",ylab="Clusturing Coeficient",col="blue",type='b')
    par(new=T)
    plot(new2$prob,new2$betweenness,xaxt='n',yaxt='n',col="Red",type='b',ann=FALSE)
    
    #plot(cluster$V2,cluster$V1, main="Growth Model Plots",xlab="Random Affilation",ylab="Clusturing Coeficient",xaxt="n",yaxt="n")
    axis(side=1, at=seq(min,max,by=n))
    #axis(side=2, at=seq(0,1,by=0.01))
    
  })
  
})
