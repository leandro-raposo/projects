library(igraph)

exercicio <- graph(edges = c("A","B",  "A","C", "A","D", "B","D", "D","E", "E","F", "F","G"), directed = F)

#carregue os pesos dos links conforme desenho que fizemos
E(exercicio)$weight  <- c(1,2,1,1,1,3,2)

# observando os atributos carregados
exercicio

#######################
# Plotando  meu grafo #
#######################
plot(exercicio)

# faca um grafico com nos de cor azul e bordas rosas, com links curvados, labels escritas em preto 

plot(exercicio, vertex.color="blue", vertex.size=15, 
     vertex.frame.color="red", vertex.label.color="black", 
     vertex.label.cex=1, vertex.label.dist=4, edge.curved=0.5) 

#############################################################################
# Extraia todas as metricas e verifique se batem com as que calculou na mao #
#############################################################################

### metricas da rede (geral)
#densidade do grafo
edge_density(exercicio)

#diametro da rede
diameter(exercicio)

#raio da rede
radius(exercicio)

#comprimeiro medio do caminho
mean_distance(exercicio)

### metrica dos nos (vertices)

# degree
degree(exercicio)

# degree ponderado
strength(exercicio, weights= E(exercicio)$weight)

# clustering coeficient
transitivity(exercicio, type = "local")

#closeness sem peso
closeness(exercicio, mode="all", weights=NA, normalized=T) 

#closeness com peso
closeness(exercicio, mode="all", normalized=T, weights= E(exercicio)$weight)  #o default eh normalized=F

# betweeness sem levar o peso em consideraao
betweenness(exercicio, directed=F, weights=NA)

# betweeness levando o peso em consideracao
betweenness(exercicio, directed=F, weights= E(exercicio)$weight)

# excentricidade
eccentricity(exercicio)

# quais nos sao pontes?
articulation.points(exercicio)

# plotar o grafo aumentando o tamanho dos nos a partir do betweenneess
bt_nos <- betweenness(exercicio, directed=F, weights= E(exercicio)$weight)
plot(exercicio, vertex.size=bt_nos*3) 

### crie um data frame com todas as medidas calculadas e transforme ele em um data frame

V(exercicio)$degree <- degree(exercicio)
V(exercicio)$degree_ponderado <- strength(exercicio, weights= E(exercicio)$weight)
V(exercicio)$cc <- transitivity(exercicio, type = "local")
V(exercicio)$closeness_sp <- closeness(exercicio, mode="all", weights=NA, normalized=T) 
V(exercicio)$closeness_cp <- closeness(exercicio, mode="all", weights= E(exercicio)$weight, normalized=T) 
V(exercicio)$betweenneess_sp <- betweenness(exercicio, directed=F, weights=NA)
V(exercicio)$betweenneess_cp <-betweenness(exercicio, directed=F, weights= E(exercicio)$weight)
V(exercicio)$excentricidade <-eccentricity(exercicio)
V(exercicio)$bridge <- 0
V(exercicio)$bridge[ articulation_points(exercicio) ] <- 1

tabela_nos_exercicio <- as_data_frame(exercicio, what="vertices")

# desenhe o grafo, destacando o caminho mais curto entre o no C e E

# tamanho das distancias mais curtas entre nos
distances(exercicio, weights=E(exercicio)$weight) # ignora pesos, caso os tenha

#desenhando distancias entre dois pontos (que eu especifico) - olha que animal
caminho_curto <- shortest_paths(exercicio, 
                                from = V(exercicio)[name=="C"], 
                                to  = V(exercicio)[name=="E"],
                                output = "both", weights=E(exercicio)$weight) # both representa n?s e links

# Gerando a variavel de cor da borda para tracar o caminho mais curto:
ecol <- rep("gray80", ecount(exercicio))
ecol[unlist(caminho_curto$epath)] <- "green"

# Gerando variavel de largura do link  para plotar o caminho mais curto:
ew <- rep(2, ecount(exercicio))
ew[unlist(caminho_curto$epath)] <- 4

# Gerando variavel de cor do no para plotar o caminho mais curto:
vcol <- rep("gray40", vcount(exercicio))
vcol[unlist(caminho_curto$vpath)] <- "gold"

# agora plotando com as marcacoes geradas
plot(exercicio, vertex.color=vcol, edge.color=ecol, edge.width=ew)
