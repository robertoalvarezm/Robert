library(seqtime)
library(ggplot2)
library(reshape2)

#El modelo gLV (generalized Lotka-Volterra) asume que todas las especies tienen
#la misma importancia ecol?gica.

#Generamos una matriz de interacci?n mediante un algoritmo Klemm-Eguiluz, que
#genera una red free-scale.
#N=n?mero de OTUs; S=n?mero de muestras.
N = 50
S = 40
A = generateA(N, "klemm", pep=10, c =0.05)
#Graficamos la matriz de Klemm-Equiluz.
plotA(A, header="Klemm-Eguiluz interaction matrix")

#Se normalizan las abundancias de los OTUs como valores relativos.
#Las abundancias est?n dadas por la distribuci?n de Poisson.
dataset = generateDataSet(S, A)
dataset = seqtime::normalize(dataset)
dataset = melt(dataset)
colnames(dataset) = c("Species", "Sample", "Abundance")
ggplot(data=dataset, aes(x=dataset$Sample, y=dataset$Abundance, width=1)) +
  geom_bar(aes(y = dataset$Abundance, x= dataset$Sample, fill=dataset$Species),
  data=dataset, stat="identity", show.legend=F) + theme(aspect.ratio=.4) +
  theme_classic()+ ylab("Relative abundance") + xlab("Sample")

#Podemos tambi?n generar abundancias relativas en las muestras con valores
#ajustados al introducir una variable de "perturbaciones ambientales".
env = envGrowthChanges(N, strength=0.8)
dataset = generateDataSet(S, A, env.matrix=env, perturb.count=c(20,20))
dataset = seqtime::normalize(dataset)
dataset = melt(dataset)
colnames(dataset) = c("Species", "Sample", "Abundance")
ggplot(data=dataset, aes(x=dataset$Sample, y=dataset$Abundance, width=1)) +
  geom_bar(aes(y = dataset$Abundance, x= dataset$Sample, fill=dataset$Species),
  data=dataset, stat="identity", show.legend=F) + theme(aspect.ratio=.4) +
  theme_classic()+ ylab("Relative abundance") + xlab("Sample")

