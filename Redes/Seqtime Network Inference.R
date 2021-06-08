library(igraph)
library(seqtime)
#Se genera una matriz NxN con una conectividad c asociada
N=80
A=generateA(N, c=0.1)
class(A)
rownames(A)=c(1:N)
colnames(A)=rownames(A)
#Se genera una matriz de interacci?n. Si un objeto i posee una correlaci?n
#positiva con un objeto j, la intersecci?n es verde. De lo contrario es roja.
#La diagonal es roja, pues sus valores son negativos y representan la
#competencia intraespec?fica.
plotA(A, header="Known interaction matrix")
library(ggplot2)
library(reshape2)
mtxIn=plotA(A,method="ggplot")
network=plotA(A,method="image")
#Podemos visualizar la matriz como una red. Cada nodo posee una competencia
#intraespec?fica.Podemos ver las interacciones asim?tricas entre los OTUs.
network=plotA(A,method="network")
#Podemos convertirlo en un objeto igraph
g1<-graph_from_adjacency_matrix(
  A,
  mode = "directed",
  weighted = TRUE,
  diag = TRUE,
  add.colnames = NULL,
  add.rownames = NA
)
plot.igraph(g1)
degree.distribution(g1)
hist(degree(g1))
E(g1)$weight

#Podemos generar un modelo de Ricker, mismo que representa la evoluci?n
#a pasos discretos de una poblaci?n N(t+1) con respecto a la t anterior.
#Cada una de los colores corresponde a las poblaciones de cada OTUs, mismos
#que evolucionan en respesta a las interacciones dadas de la red.

#Se fundamenta en el modelo Lotka-Volterra
out.ricker=ricker(N,A=A)
tsplot(out.ricker,main="Ricker")

#Se carga el algoritmo LIMITS, mismo que estima una matriz de interacci?n
#de las series de tiempo en base el modelo de Ricker.
#Nos permite conocer que tan bien puede inferir la matriz de interacci?n
#conocida a partir de las series temporales.
Aest=limits(out.ricker)$Aest

#Comparamos la matriz de interacci?n conocida y la estimada.
par(mfrow=c(1,2))
plotA(A,header="known")
plotA(Aest,header="inferred")
par(mfrow=c(1,1))
#SE puede calcular la correlaci?n cruzada o covarianza entre ambas matrices.
crossCor=cor(A,Aest)
mean(diag(crossCor), na.rm=TRUE)

#Finalmente, podemos graficar algunos estimadores de calidad para la inferencia
#de la matriz de interacci?n. El gr?fico de calidad muestra la correlaci?n
#entre los puntos de tiempo actuales y futuros de uno a cinco pasos adelante
#(autocor) y la correlaci?n entre la serie de tiempo original y una serie de
#tiempo generada a partir de la matriz de interacci?n inferida paso a paso (cor).

#Una autocorrelaci?n de retardo 1 (es decir, k = 1 en lo anterior) es la
#correlaci?n entre valores que est?n separados por un per?odo de tiempo.
#De manera m?s general, una autocorrelaci?n de retardo k es la correlaci?n
#entre valores que est?n separados por k per?odos de tiempo.

#Podemos observar que las series de tiempo predichas apenas superan la
#correlaci?n de retardo 1.
limitsqual=limitsQuality(out.ricker,A=Aest,plot=TRUE)


#Para hacer una comparaci?n, realizamos una simulaci?n en un modelo de Hubbell.
#Este modelo neutro no toma en cuenta las interacciones de la matriz A, sino
#que pone a todos los OTUs a competir entre ellos por igual.
out.hubbell=simHubbell(N=N, M=N,I=1500,d=N, m=0.1, tskip=500, tend=1000)
tsplot(out.hubbell,main="Hubbell")

#Inferimos series temporales a partir del modelo neutro simulado con Hubbell.
Aesth=limits(out.hubbell)$Aest

#La gr?fica de calidad muestra que la matriz de interacci?n inferida conduce
#a una alta correlaci?n cruzada entre las series de tiempo predichas y
#originales, aunque la din?mica en la serie de tiempo neutral no est?
#determinada por interacciones espec?ficas de especies (todas las interacciones
#son negativas y de igual fuerza, ya que ninguna especie tiene ventaja sobre
#otra en la competencia por un espacio en la comunidad local).
limitsqualh=limitsQuality(out.hubbell,A=Aesth, plot=TRUE)

#Por lo tanto, una alta correlaci?n entre las series de tiempo originales y
#las predichas puede inducir a error como indicador de la calidad de la matriz
#de interacci?n inferida.