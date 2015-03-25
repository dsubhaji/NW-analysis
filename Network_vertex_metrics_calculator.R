library('tools')
library('igraph')
library('Matrix')
library('blockmodeling')
choice<- readline(prompt="Enter your choice, 1. Directed Network\n2.Undirected graph\n")
directory <- readline(prompt="Enter The Folder Location: ")
filenames <- list.files(path=directory,pattern="*.net")
man<-typeof(filenames)
len=length(filenames)

prompt <- "Select the metrics you want to calculate (space-separated list) \n
1.Degree\n2.Betweenness\n3.Clustering Coeff\n4.Closeness\n5.Eigen Centrality\n6.PageRank \n"
EXP <- as.integer(strsplit(readline(prompt), " ")[[1]])
print(choice)
if (choice==1){
  

for(i in 1:len)
{
  
  ptr <- filenames[i]  
  temp<- file_path_sans_ext(ptr)
  #print(ptr)
  if(file.exists(ptr))
  {
    setwd(file.path(paste(directory)))
  }
  else
  {
    dir.create(file.path(paste(directory)))
    setwd(file.path(paste(directory)))
  }
  name <- paste(directory,ptr,sep="")
  x <- read.graph(name,format="pajek")
  
  degree <- degree(x,v=V(x),mode=c("all"))
  
  betweenness<-betweenness(x,v=V(x), directed=TRUE)
  
  closeness <- closeness(x, vids=V(x),mode=c("all"),normalized=TRUE)
  
  pagerank_temp<-page.rank(x,vids=V(x),directed=TRUE)
  
  pagerank<-pagerank_temp$vector
  
  clustcoeff <-transitivity(x,type=c("local"))
  
  eigencentrality_temp <-evcent(x,directed=TRUE)
  
  eigencentrality<-eigencentrality_temp$vector    
  
  file1=paste(temp,"-metrics",".csv",sep="")
  
 
  write.table(cbind(degree,betweenness,clustcoeff,closeness,eigencentrality,pagerank), 
              file1, row.names = FALSE,
              col.names=c('degree','betweenness','clustcoeff','closeness','eigencentrality','pagerank'),sep=",",na="0")
  
  
  file <- read.csv(file1)
  
  small <- subset(file,,EXP)
  
  write.csv(small,file1)
  
  }
}
if (choice==2)
{
  for(i in 1:len)
  {
    
    ptr <- filenames[i]  
    temp<- file_path_sans_ext(ptr)
    if(file.exists(ptr))
    {
      setwd(file.path(paste(directory)))
    }
    else
    {
      dir.create(file.path(paste(directory)))
      setwd(file.path(paste(directory)))
    }
    name <- paste(directory,ptr,sep="")
    x <- read.graph(name,format="pajek")
    
    degree <- degree(x,v=V(x))
    
    betweenness<-betweenness(x,v=V(x), directed=FALSE)
    
    closeness <- closeness(x, vids=V(x),normalized=TRUE)
    
    pagerank_temp<-page.rank(x,vids=V(x),directed=FALSE)
    
    pagerank<-pagerank_temp$vector
    
    clustcoeff <-2*transitivity(x,type=c("local"))
    
    eigencentrality_temp <-evcent(x, directed=FALSE)
    
    eigencentrality<-eigencentrality_temp$vector    
    
    file1=paste(temp,"-metrics",".csv",sep="")
    
    write.table(cbind(degree,betweenness,clustcoeff,closeness,eigencentrality,pagerank), 
                file1, row.names = FALSE,
                col.names=c('degree','betweenness','clustcoeff','closeness','eigencentrality','pagerank'),sep=",", na="0")
    
    
    file <- read.csv(file1)
    
    small <- subset(file,,EXP)
    
    write.csv(small,file1)
    
  }
  
}

