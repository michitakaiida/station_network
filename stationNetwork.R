library(XML)
library(foreach)
library(igraph)
library(linkcomm)

doc <- xmlRoot(xmlTreeParse("http://www.ekidata.jp/api/p/13.xml"))
xmlData <- xmlSApply(doc, function(x) xmlSApply(x, xmlValue))
lineList <- (xmlData[,3:length(xmlData[1,])])[1,]

getNstation <-function(x){
  stationList <-xmlRoot(xmlTreeParse(paste("http://www.ekidata.jp/api/n/", x, ".xml", sep = "")))
  sList <- xmlSApply(stationList, function(x) xmlSApply(x, xmlValue))
  sList.d.tmp <- as.data.frame(cbind(as.character(sList[3,]),as.character(sList[4,])))
  names(sList.d.tmp) <- c("right","left")
  
  return (as.data.frame(sList.d.tmp))
}

total <- foreach(i = 1:length(as.character(lineList)),.combine = "rbind") %do% {getNstation(as.character(lineList)[i])}

g <- graph.data.frame(total , directed = TRUE) 
eb <- spinglass.community(g)
V(g)$color <- eb$membership

set.seed(0)
plot(g,edge.width=E(g)$Freq/3,layout=layout.fruchterman.reingold(g,niter=1500,area=vcount(g)^2.3,repulserad=vcount(g)^2.8)
     ,edge.arrow.size=0.1,edge.color="gray80",vertex.frame.color="white",vertex.label.color="gray50",vertex.label.font=-1,
     vertex.label.cex=0.5,vertex.color=V(g)$color,vertex.size=2)

plot(g,edge.width=E(g)$Freq/100,layout=layout.fruchterman.reingold(g,niter=1500,area=vcount(g)^2.3,repulserad=vcount(g)^2.8)
     ,edge.arrow.size=0.01,edge.color="gray80",vertex.frame.color="white",vertex.label.color="gray50",vertex.label.font=10,
     vertex.label.cex=0.5,vertex.color=V(g)$color,vertex.size=2)

##
g2<-getLinkCommunities(total,directed=T)
plot(g2)

plot(g2,type="graph",ewidth=E(g)$Freq/3,layout = layout.fruchterman.reingold,
     vsize = 5,edge.arrow.size=0.0001,vertex.label.font=1,vlabel.cex=0.3,vertex.size=0.01) 

for(i in 1:length(g2$clusters)){
  plot(g2,type="graph",ewidth=E(g)$Freq/3,layout = layout.fruchterman.reingold,clusterids=i,
       vsize = 5,edge.arrow.size=0.0001,vertex.label.font=100,vlabel.cex=0.3,vertex.size=0.01,
       margin=-10000)
}

for(i in 1:10){
  plot(g2,type="graph",ewidth=E(g)$Freq/3,layout = layout.fruchterman.reingold,
       vsize = 5,edge.arrow.size=0.0001,vertex.label.font=100,vlabel.cex=0.3,vertex.size=0.01,
       shownodesin=i)  
}