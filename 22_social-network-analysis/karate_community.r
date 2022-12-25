library (igraph)
library(dplyr)


# importando bases
karate <- read.csv("dados/karate.csv", header=T, as.is=T, sep=";")

#transformando em grafo
rede_karate <- graph_from_data_frame(d=karate, directed=F) 

#plotando o grafo
plot(rede_karate)

################
# COMUNIDADES  #
################

#as comunidades no R podem ser executadas por alguns algoritmos implementados na lib igraph
#cluster_edge_betweenness  (https://igraph.org/r/doc/cluster_edge_betweenness.html)
#cluster_fast_greedy (https://igraph.org/r/doc/cluster_fast_greedy.html)
#cluster_walktrap (https://igraph.org/r/doc/cluster_walktrap.html)
#cluster_spinglass (https://igraph.org/r/doc/cluster_spinglass.html)
#cluster_leading_eigen (https://igraph.org/r/doc/cluster_leading_eigen.html)
#cluster_label_prop (https://igraph.org/r/doc/cluster_label_prop.html)
#cluster_louvain (https://igraph.org/r/doc/cluster_louvain.html)
#cluster_optimal (https://igraph.org/r/doc/cluster_optimal.html)

comunidades <- cluster_edge_betweenness(rede_karate) 
V(rede_karate)$id_comunidade <- membership(comunidades)
plot(comunidades, rede_karate) 

comunidades1 <- cluster_fast_greedy(rede_karate) 
V(rede_karate)$id_comunidade1 <- membership(comunidades1)
plot(comunidades1, rede_karate) 

comunidades2 <- cluster_optimal(rede_karate) 
V(rede_karate)$id_comunidade2 <- membership(comunidades2)
plot(comunidades2, rede_karate)

comunidades3 <- cluster_louvain(rede_karate) 
V(rede_karate)$id_comunidade3 <- membership(comunidades3)
plot(comunidades3, rede_karate)


nodes_karate <- igraph::as_data_frame(rede_karate, what="vertices")



# desenhando a rede por tamanho do betweeenneess
bet <- betweenness(rede_karate)
plot(comunidades3, rede_karate, vertex.size=bet*0.1)

#calculando e armazenando metricas da rede geral
V(rede_karate)$degree_sc <- degree(rede_karate)
V(rede_karate)$cc_sc <- transitivity(rede_karate, type = "local")
V(rede_karate)$closeness_sc <- closeness(rede_karate, mode="all", weights=NA) 
V(rede_karate)$betweenneess_sc <- betweenness(rede_karate, directed=F, weights=NA)


#calculando e armazenando metricas por comunidade
redefiltro_ <- list()

for (i in min(V(rede_karate)$id_comunidade3):max(V(rede_karate)$id_comunidade3)) {
  net_cid <- delete_vertices(rede_karate, V(rede_karate)$id_comunidade3!=i)
  V(net_cid)$degree_cc <- degree(net_cid)
  V(net_cid)$cc_cc <- transitivity(net_cid, type = "local")
  V(net_cid)$closeness_cc <- closeness(net_cid, mode="all", weights=NA) 
  V(net_cid)$betweenneess_cc <- betweenness(net_cid, directed=F, weights=NA)
  redefiltro_[[i]] <- igraph::as_data_frame(net_cid, what="vertices")
  print(redefiltro_[[i]])
}

nodes_metricas_por_comun <- bind_rows(redefiltro_)
