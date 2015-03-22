library('tools')
library('igraph')
library('Matrix')
library('blockmodeling')
directory <- readline(prompt="Enter The Folder Location: ")
filenames <- list.files(path=directory,pattern="*.net")
man<-typeof(filenames)
len=length(filenames)
  
prompt <- "Select the metrics you want to calculate (space-separated list) \n
1.Degree\n2.Betweenness\n3.Clustering Coeff\n4.Closeness\n5.Eigen Centrality\n6.PageRank \n"
EXP <- as.integer(strsplit(readline(prompt), " ")[[1]])

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

    closeness <- closeness(x, vids=V(x),mode=c("all"),normalized=TRUE)

    pagerank_temp<-page.rank(x,vids=V(x),directed=FALSE)

    pagerank<-pagerank_temp$vector

    clustcoeff <-transitivity(x,type=c("global"))

    eigencentrality_temp <-evcent(x)

    eigencentrality<-eigencentrality_temp$vector    
        
    file1=paste(temp,"-metrics",".csv",sep="")

    write.table(cbind(degree,betweenness,clustcoeff,closeness,eigencentrality,pagerank), 
                file1, row.names = FALSE,
                col.names=c('degree','betweenness','clustcoeff','closeness','eigencentrality','pagerank'),sep=",", na="0")
    

    file <- read.csv(file1)

    small <- subset(file,,EXP)

    write.csv(small,file1)
    
}
