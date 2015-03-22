library(plyr)
require(networkD3)

mk.g = function(cor.mat,var.names,group.labs=NULL,lambda=NULL,nlambda=NULL){
  out.huge = huge(cor.mat,method = "glasso",lambda=lambda,
                  nlambda=nlambda,verbose = FALSE)
  adj.mat = as.matrix(out.huge$path[[1]])
  colnames(adj.mat)=rownames(adj.mat)=var.names
  g = graph.adjacency(adj.mat, mode = "undirected", diag = FALSE )
  if(!is.null(group.labs)){
    V(g)$label = group.labs
    cols <- rainbow(length(table(group.labs)))
    V(g)$color <- cols[factor(group.labs)]
  }
  g = delete.vertices(g, which(degree(g) < 1))
}

plot.g.fn = function(cor.mat,lambda=0.53,var.names,group.labs=NULL){
  g = mk.g(cor.mat=cor.mat, var.names=var.names,
           group.labs=group.labs, lambda=lambda)
  layout.grid = layout.auto(g)
  par(mar=c(0.5,0.5,0.5,0.5))
  plot(g, layout = layout.grid, edge.color = "gray50", 
       edge.label=NA,margin=c(0,0.2,0,0),
       vertex.size = 3, vertex.label = NA, 
       main = "")
  if(!is.null(V(g)$label)){
    labcol = unique(cbind(V(g)$label,V(g)$color))
    legend("topleft",legend=labcol[,1],
           col=labcol[,2], pch=19, bty="n")
  }
}

plot.g.d3 =  function(cor.mat,lambda=0.53,height=640,width=640,charge=-30,
                      var.names,group.labs=NULL){
  g = mk.g(cor.mat=cor.mat,lambda=lambda,var.names = var.names,group.labs=group.labs)
  name = as.character(V(g)$name)
  Links = get.data.frame(g,what="edges")
  name.lookup = data.frame(name=name,id=0:(length(name)-1))
  numSource = merge(data.frame(name=Links$from), name.lookup, all.x=TRUE, sort = FALSE)
  numTarget <- join(data.frame(name=Links$to), name.lookup, by = "name")
  numLinks = data.frame(from=numSource$id,to = numTarget$id)
  Nodes = data.frame(name,group=1,stringsAsFactors=TRUE)
  if(!is.null(group.labs)){
    fname = paste(as.character(V(g)$label)," (",as.character(V(g)$name),")",sep="")
    group = as.character(V(g)$label)
    Nodes = data.frame(name,group,fname,stringsAsFactors=TRUE)
    forceNetwork(Links=numLinks,Nodes=Nodes, Source = "from",Target="to",NodeID="fname",
                 colourScale = "d3.scale.category20()",charge=charge,
                 height=height,width=width,linkColour="#ccc",
                 Group="group",opacity = 0.9)
  } else {
    forceNetwork(Links=numLinks,Nodes, Source = "from",Target="to",NodeID="name",
                 colourScale = "d3.scale.category20()",charge=charge,
                 height=height,width=width,linkColour="#ccc",
                 Group="group",opacity = 0.9)
  }
}
