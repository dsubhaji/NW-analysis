library(shiny)

shinyServer(function(input, output) {
  
  output$Degree <- renderPlot({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    len1 <-(input$file1)[[1]]
    len<-length(len1)
    
    
    #par(mfrow = c(len/2,2) ,oma=c(1,1,0,0), mar=rep(2,4), tcl=-0.5, mgp=c(2,1,0) )
    for(i in 1:len)
    {
      #print(i)
      #i <- 1
      
      file <- read.csv(input$file1[[i, 'datapath']] , header=input$header, sep=input$sep, 
                       quote=input$quote)
      
      max <- dim(file)[1]
      min <- 0
      
      
      plot(file$X,file$degree,lwd = 2, xlab = "Network",ylab="Degree", main = paste("Line plot of file ",i),col="RosyBrown",type="l")
      axis(side=1, at=seq(min,max,by=1))
      #hist(file$degree, xlab = i, col="RosyBrown", main=paste("Histogram of file ",i))      
    }    
  })
  
  output$cluster <- renderPlot({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    len1 <-(input$file1)[[1]]
    len<-length(len1)
    
    
    
    #par(mfrow = c(len/2,2) ,oma=c(1,1,0,0), mar=rep(2,4), tcl=-0.5, mgp=c(2,1,0) )
    
    for(i in 1:len)
    {
      #print(i)
      #i <- 1
      max <- dim(file)[1]
      min <- 0
      
      file <- read.csv(input$file1[[i, 'datapath']] , header=input$header, sep=input$sep, 
                       quote=input$quote)
      
      plot(file$X,file$clustcoeff,lwd = 2,xlab = "Network",ylab="Clustering Coefficient", main = paste("Line plot of file ",i),col="salmon",type="l")
      axis(side=1, at=seq(min,max,by=1))
      #hist(file$clustcoeff,xlab = i ,col="salmon",main=paste("Histogram of file ",i))
      
    }
  })
  output$diameter <- renderPlot({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    len1 <-(input$file1)[[1]]
    len<-length(len1)
    
    
    #par(mfrow = c(len/2,2) ,oma=c(1,1,0,0), mar=rep(2,4), tcl=-0.5, mgp=c(2,1,0) )
    
    for(i in 1:len)
    {
      #print(i)
      #i <- 1
      
      file <- read.csv(input$file1[[i, 'datapath']] , header=input$header, sep=input$sep, 
                       quote=input$quote)
      
      max <- dim(file)[1]
      min <- 0
      plot(file$X,file$diameter,lwd = 2,xlab = "Network",ylab="Diameter", main = paste("Line plot of file ",i),col="turquoise",type="l")
      axis(side=1, at=seq(min,max,by=1))
      #  hist(file$diameter,xlab = i,col="turquoise",main=paste("Histogram of file ",i))
      
    }    
  })
  
  output$avgdegree <- renderPlot({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    len1 <-(input$file1)[[1]]
    len<-length(len1)
    
    
    
    #par(mfrow = c(len/2,2) ,oma=c(1,1,0,0), mar=rep(2,4), tcl=-0.5, mgp=c(2,1,0) )
    
    for(i in 1:len)
    {
      #print(i)
      #i <- 1
      
      file <- read.csv(input$file1[[i, 'datapath']] , header=input$header, sep=input$sep, 
                       quote=input$quote)
      
      max <- dim(file)[1]
      min <- 0
      
      plot(file$X,file$avg_degree,lwd = 2,xlab = "Network",ylab="Average Degree", main = paste("Line plot of file ",i),col="SeaGreen",type="l")
      axis(side=1, at=seq(min,max,by=1))
      #hist(file$avg_degree,xlab = i ,col="SeaGreen",main=paste("Histogram of file ",i))
      
    }    
  })
  
  output$modularity <- renderPlot({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    len1 <-(input$file1)[[1]]
    len<-length(len1)
    
    
    
    #par(mfrow = c(len/2,2) ,oma=c(1,1,0,0), mar=rep(2,4), tcl=-0.5, mgp=c(2,1,0) )
    
    for(i in 1:len)
    {
      #print(i)
      #i <- 1
      
      file <- read.csv(input$file1[[i, 'datapath']] , header=input$header, sep=input$sep, 
                       quote=input$quote)
      
      max <- dim(file)[1]
      min <- 0
      
      plot(file$X,file$modularity,lwd = 2,xlab = "Network",ylab="Modularity", main = paste("Line plot of file ",i),col="sky blue",type="l")
      axis(side=1, at=seq(min,max,by=1))
      #hist(file$modularity,xlab = i,col="sky blue",main=paste("Histogram of file ",i))
      
    }    
  })
  
  output$density <- renderPlot({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    len1 <-(input$file1)[[1]]
    len<-length(len1)
    
    
    
    #par(mfrow = c(len/2,2) ,oma=c(1,1,0,0), mar=rep(2,4), tcl=-0.5, mgp=c(2,1,0) )
    
    for(i in 1:len)
    {
      #print(i)
      #i <- 1
      
      file <- read.csv(input$file1[[i, 'datapath']] , header=input$header, sep=input$sep, 
                       quote=input$quote)
      
      max <- dim(file)[1]
      min <- 0
      
      plot(file$X,file$density,lwd = 2,xlab = "Network",ylab="Density", main = paste("Line plot of file ",i),col="grey",type="l")
      axis(side=1, at=seq(min,max,by=1))
      #hist(file$density,xlab = i,col="grey",main=paste("Histogram of file ",i))
      
    }    
  })
  
  output$avgsep <- renderPlot({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    len1 <-(input$file1)[[1]]
    len<-length(len1)
    
    
    
    #par(mfrow = c(len/2,2) ,oma=c(1,1,0,0), mar=rep(2,4), tcl=-0.5, mgp=c(2,1,0) )
    
    for(i in 1:len)
    {
      #print(i)
      #i <- 1
      
      file <- read.csv(input$file1[[i, 'datapath']] , header=input$header, sep=input$sep, 
                       quote=input$quote)
      
      max <- dim(file)[1]
      min <- 0
      
      plot(file$X,file$avg_path_length,lwd = 2,xlab = "Network",ylab="Average Separation", main = paste("Line plot of file ",i),col="grey",type="l")
      #hist(file$avg_path_length,xlab = i,col="grey",main=paste("Histogram of file ",i))
      
    }    
  })
})