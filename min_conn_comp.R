#**Based on `igraph`**
  
#  This function permits to obtain the minimal subgraph that connects 
# your `sources` nodes. 
# All you have to do is to provide a undirected connected component 
# containing all the sources nodes and the algorithm will detect
# the shortest path connecting them and collapse these edges into a new subgraph.
# `igraph` package is required.

min_conn_comp=function(G,sources){
  D=distances(G,v=sources,to=sources)
  GD=graph_from_adjacency_matrix(D,mode="undirected",weighted = TRUE)
  MST=mst(GD)
  plot.igraph(MST,vertex.label=V(MST)$name,edge.color='black')
  Gmin=graph.empty(directed=FALSE)
  for(e in E(MST)){
    vs=ends(MST,e)
    print(paste('Computing SP from ',vs[1],' to ',vs[2]))
    sp=shortest_paths(G,from=vs[1],to=vs[2],output = 'both')
    newv=sp$vpath
    print(newv)
    Gmin=Gmin + vertices(V(G)[newv[[1]]]$name)
    Gmin=Gmin+path(V(G)[newv[[1]]]$name)
    Gmin=delete.vertices(simplify(Gmin),degree(Gmin)==0)
  }
}