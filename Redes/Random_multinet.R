#Cargamos la librería y generamos un set de 100 números
library(igraph)
x<-1:100

#A partir del conjunto x, generamos una serie de redes mediante el
#método de Barabasi, que genera redes free-scale, donde solo un número
#pequeño de nodos mantiene casi la totalidad de las conexiones.

a1<-sample(x, sample(x,1))
g1<-barabasi.game(a1) %>%
  set_vertex_attr("name", value = LETTERS[1:length(a1)])
plot(g1)

a2<-sample(x, sample(x,1))
length(a2)
g2<-barabasi.game(a2) %>%
  set_vertex_attr("name", value = LETTERS[1:length(a2)])
plot(g2)

a3<-sample(x, sample(x,1))
g3<-barabasi.game(a3) %>%
  set_vertex_attr("name", value = LETTERS[1:length(a3)])
plot(g3)

a4<-sample(x, sample(x,1))
g4<-barabasi.game(a4) %>%
  set_vertex_attr("name", value = LETTERS[1:length(a4)])
plot(g4)

#Cargamos la librer?a multinet, y generamos un objeto multinet vac?o.
library(multinet)
library(corrplot)
ntB<-ml_empty()
l <- layout_circular_ml(ntB)

#Se agregan las capas a partir de redes igraph.
add_igraph_layer_ml(ntB, g1, "Layer1")
add_igraph_layer_ml(ntB, g2, "Layer2")
add_igraph_layer_ml(ntB, g3, "Layer3")
add_igraph_layer_ml(ntB, g4, "Layer4")

#Se analizan los atributos de la red generada (capas y nodos).
num_layers_ml(ntB)
layers_ml(ntB)
num_actors_ml(ntB)
actors_ml(ntB)

#Graficamos la red.
l <- layout_multiforce_ml(ntB, w_inter = 0, gravity = 1)
plot(ntB,
     vertex.labels = "",
     grid = c(2,3),
     layout = l,
     legend.x="bottomright", legend.inset = c(.03,.03)
)

#Calculamos el valor de la transitividad.
transitivity(as.list(ntB)[[1]])

#Generamos gr?ficos de correlaci?n correspondientes a:
#Presencia de nodos r?plica en las distintas capas.
comp <- layer_comparison_ml(ntB, method = "jaccard.actors")
comp
comp2<-as.matrix(comp)
corrplot(comp2)
#Comparaci?n entre el degree de los nodos entre las capas.
comp <- layer_comparison_ml(ntB, method = "pearson.degree")
comp
comp2<-as.matrix(comp)
corrplot(comp2)
#Comparaci?n de las comunidades (relaci?n de un nodo con un conjunto concreto
#de nodos) entre las capas.
comp <- layer_comparison_ml(ntB, method = "jaccard.edges")
comp
comp2<-as.matrix(comp)
corrplot(comp2)
#Como es de esperarse, la distribuci?n del degree es asim?trica, y la mayor?a
#de conexiones est? concentrada en unos pocos nodos.
hist(degree_ml(ntB), col = "blue")

#Con valores de k>2, las comunidades tienden a desaparecer.
#Posiblemente esto sea el resultado del bajo n?mero de conexiones presentes
#en la gran mayor?a de nodos, y ya que el algoritmo tiende a descartar
#conjuntos de nodos que se encuentren inconexos entre s?, las comunidades
#tender?n a ser peque?as.
#Si se carga el script repetidas veces, la mayor?a de las veces las comunidades
#ser?n peque?as o inexistentes en algunas capas.
ml_clust <- clique_percolation_ml(ntB, k=2, m=2)
plot(ntB,
     com = ml_clust,
     vertex.labels = "",
     layout=l, grid = c(2,3),
     legend.x="bottomright",
     legend.inset = c(.05, .05)
)

#Generamos un set de 30 n?meros.
prob<-1:30

#Generamos redes aleatorias mediante el m?todo de Erdos-Rendyl.
#Seleccionamos una probabilidad aleatoria de que las conexiones se generen
#y un n?mero aleatorio de nodos.
a1<-sample(x, sample(x,1))
g1<-erdos.renyi.game(a1,1/sample(prob)) %>%
  set_vertex_attr("name", value = LETTERS[1:length(a1)])
plot(g1)

a2<-sample(x, sample(x,1))
g2<-erdos.renyi.game(a2,1/sample(prob)) %>%
  set_vertex_attr("name", value = LETTERS[1:length(a2)])
plot(g2)

a3<-sample(x, sample(x,1))
g3<-erdos.renyi.game(a3,1/sample(prob)) %>%
  set_vertex_attr("name", value = LETTERS[1:length(a3)])
plot(g3)

a4<-sample(x, sample(x,1))
g4<-erdos.renyi.game(a4,1/sample(prob)) %>%
  set_vertex_attr("name", value = LETTERS[1:length(a4)])
plot(g4)

#A diferencia del ejemplo anterior, la probabilidad asociada a la generaci?n
#de conexiones provoca que algunos de los nodos queden inconexos si dicha
#probabilidad es lo suficientemente baja.
#A pesar de esto, el coeficiente de clusterizaci?n arroja valores similares
#a los dados por el m?todo de Barabasi a?n tras numerosas iteraciones.
transitivity(as.list(ntE)[[1]])

#Generamos un objeto vac?o multinet.
ntE<-ml_empty()
l <- layout_circular_ml(ntE)

#Se agregan las capas a partir de redes igraph.
add_igraph_layer_ml(ntE, g1, "Layer1")
add_igraph_layer_ml(ntE, g2, "Layer2")
add_igraph_layer_ml(ntE, g3, "Layer3")
add_igraph_layer_ml(ntE, g4, "Layer4")

#Verificamos los nodos y las capas.
layers_ml(ntE)
actors_ml(ntE)

#Con suficientes iteraciones, en algunos casos todas las capas poseen casi
#los mismos nodos.
comp <- layer_comparison_ml(ntE, method = "jaccard.actors")
comp
comp2<-as.matrix(comp)
corrplot(comp2)
#Comparaci?n entre el degree de los nodos entre las capas.
comp <- layer_comparison_ml(ntE, method = "pearson.degree")
comp
comp2<-as.matrix(comp)
corrplot(comp2)
#En la mayor?a de los casos, los nodos de una capa interact?an con
#un conjunto de nodos distinto al de la capa de referencia, por lo cual
#la correlaci?n suele ser baja.
comp <- layer_comparison_ml(ntE, method = "jaccard.edges")
comp
comp2<-as.matrix(comp)
corrplot(comp2)
#La presencia de signos de interrogaci?n podr?a deberse a la generaci?n de
#redes con casi la totalidad de sus nodos inconexos como producto de una
#baja probabilidad de interacci?n.

#Sucede lo mismo que con el m?todo de Barabasi, a valores altos de k, las
#comunidades desaparecen.
ml_clust <- clique_percolation_ml(ntE, k=2, m=2)
plot(ntE,
     com = ml_clust,
     vertex.labels = "",
     grid = c(2,3),
     legend.x="bottomright",
     legend.inset = c(.05, .05))

