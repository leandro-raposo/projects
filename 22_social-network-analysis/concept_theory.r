
library (igraph)
library(tidyverse)

# desenhando a rede do exemplo
g <- graph(edges = c("A","B",  "B","C",  "B","D",  "D","C", "C","E"), directed = F)

#vendo os nos (entidades / vertices)
V(g)

#vendo os links (edges)
E(g)

# esses sao informacoes que estao dentro do objeto igraph e que podem conter mais 
# informacoes, por exemplo, pesos
# inserindo pesos no meu grafo (neste caso, todas 1)
V(g)$weight  <- c(1,1,1,1,1)

# observando os atributos carregados
g
# observe que agora temos o atributo weight (v/n) - referente ao vertice (no/entidade) 
# e que eh n de numerico


#######################
# Plotando  meu grafo #
#######################
plot(g)

#######################
# Firulando meu grafo #
#######################

# exemplo com firulas
plot(g, vertex.color="pink", vertex.size=35, 
     vertex.frame.color="red", vertex.label.color="green", 
     vertex.label.cex=1, vertex.label.dist=4, edge.curved=0.2) 


#' firulas nos grafos
#' vertex.color="pink" --------- cor do no
#' vertex.size=35 -------------- tamanho do no
#' vertex.frame.color="red" ---- cor da borda do no
#' vertex.label.color="green" -- cor da label
#' vertex.label.cex=1 ---------- tamanho da label
#' vertex.label.dist=4 --------- tamanho da label
#' edge.curved=0.2 ------------- intensidade da curva

######################
# Extraindo metricas #
######################

### metricas da rede (geral)
#densidade do grafo
edge_density(g)

#diametro da rede
diameter(g)

#raio da rede
radius(g)

#comprimeiro medio do caminho
mean_distance(g)

### metrica dos nos (vertices)

# degree
degree(g)
# o degree ponderado eh dado pelo argumento strength(g, weights= V(g)$weight)

# histograma dos degrees (caso queira ver a distribuicao, como vimos no Gephi)
hist(degree(g), breaks=15, main="Histograma do degree")

# clustering coeficient
transitivity(g, type = "local")

#closeness
closeness(g, mode="all", weights=NA, normalized=T) 
# no caso do normalized=T, lembre-se que será como 
# no Gephi... 1/ (media dos caminhos curtos saindo desse no) 
# logo, quanto menor mais "rapido" o no desloca a qualquer outro no
closeness(g, mode="all", weights=NA)  #o default eh normalized=F
# no caso do normalized=F, O divide o valor do comando anterior gerou
# dividido pelo numero de caminhos curtos 

# betweeness
betweenness(g, directed=F, weights=NA)

# excentricidade
eccentricity(g)

# o famoso e tao desejado bridge (ponte ou ponto de articulacao)
articulation.points(g)

##################################################
# Com visualizar todas as medidas num data frame #
##################################################
## voce tem que armazenar as medidas como atributos
# armazenando todas as medidas e transformando em data frame

V(g)$degree <- degree(g)
V(g)$cc <- transitivity(g, type = "local")
V(g)$closeness <- closeness(g, mode="all", weights=NA, normalized=T) 
V(g)$betweenneess <-betweenness(g, directed=F, weights=NA)
V(g)$excentricidade <-eccentricity(g)
V(g)$bridge <- 0
V(g)$bridge[ articulation_points(g) ] <- 1

tabela_nos <- igraph::as_data_frame(g, what="vertices")

############################################
# Firulas que ajudam a contar historias!!  #
############################################

############################################
# Firulando meu grafo a partir das medidas #
############################################

# para plotar aumentando o tamanho dos nos a partir do degree, por exemplo
degree_nos <- degree(g, mode="all") #aqui simplesmente dei um nome pro objeto degree
plot(g, vertex.size=degree_nos*20) #coloquei o tamanho do no a partir do valor do 
#degree vezes 9 (eu que escolhi, para ficar mais visivel)

# posso, por exemplo, pintar de outra cor os nos que sao pontes
V(g)$color <- "pink"
V(g)$color[ articulation_points(g) ] <- "red"
plot(g, layout = layout.fruchterman.reingold)

##############################################
# Olhem isso que legal e facilitador da vida #
#          caminhos curtos                   #
##############################################

# tamanho das distancias mais curtas entre nos
distances(g, weights=NA) # ignora pesos, caso os tenha

#desenhando distancias entre dois pontos (que eu especifico) - olha que animal
caminho_curto <- shortest_paths(g, 
                                from = V(g)[name=="A"], 
                                to  = V(g)[name=="E"],
                                output = "both") # both representa nos e links

# Gerando a variavel de cor da borda para tracar o caminho mais curto:
ecol <- rep("gray80", ecount(g))
ecol[unlist(caminho_curto$epath)] <- "green"

# Gerando variavel de largura do link para plotar o caminho mais curto:
ew <- rep(2, ecount(g))
ew[unlist(caminho_curto$epath)] <- 4

# Gerando variavel de cor do no para plotar o caminho mais curto:
vcol <- rep("gray40", vcount(g))
vcol[unlist(caminho_curto$vpath)] <- "gold"

# agora plotando com as marcacoes geradas
plot(g, vertex.color=vcol, edge.color=ecol, edge.width=ew)
