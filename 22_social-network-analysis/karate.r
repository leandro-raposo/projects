library (igraph)
library (readr)

#exemplo de leitura extensao gml
# dri <- read.graph("seucaminho/ADRIANA.gml",format=c("gml"))

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

modularity(comunidades)

comunidades1 <- cluster_fast_greedy(rede_karate) 
V(rede_karate)$id_comunidade1 <- membership(comunidades1)
plot(comunidades1, rede_karate) 

modularity(comunidades1)

comunidades2 <- cluster_optimal(rede_karate) 
V(rede_karate)$id_comunidade2 <- membership(comunidades2)
plot(comunidades2, rede_karate)

modularity(comunidades2)

comunidades3 <- cluster_louvain(rede_karate) 
V(rede_karate)$id_comunidade3 <- membership(comunidades3)
plot(comunidades3, rede_karate)

modularity(comunidades3)
nodes_karate <- igraph::as_data_frame(rede_karate, what="vertices")
