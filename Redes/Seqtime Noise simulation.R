library(tuneR)

#Simulaci?n de distintos tipos de ruido.
# Ruido blanco: es una se?al aleatoria (proceso estoc?stico) que se caracteriza
#por el hecho de que sus valores de se?al en dos tiempos diferentes no guardan
#correlaci?n estad?stica. Como consecuencia de ello, su densidad espectral de
#potencia (PSD, siglas en ingl?s de power spectral density) es una constante,
#es decir, su gr?fica es plana.
w <- tuneR::noise(kind = c("white"))
w

#El ruido marr?n: Es el ruido blanco integrado. Tambi?n llamado ruido rojo,
#describe el movimiento browniano. Est? definido por S(w)=S_0/w^2, donde S_0
#corresponde a la amplitud de la onda, que es una variante aleatoria fluctuante.
#(ie. random walk)
b <- cumsum(rnorm(length(w@left)))
b

# Ruido rosa: Posee una dencidad espectral inversamente proporcional a la
#frecuancia p=1/fr.
p <- tuneR::noise(kind = c("pink"))
p

#Visualizaci?n:
par(mfrow=c(3,1))
plot(w,main="white noise")
plot(b,main="brown noise")
plot(p,main="pink noise")

#Estimaci?n del exponente o coeficiente de Hurst para cada tipo de ruido.
#El coeficiente o exponente de Hurst es una medida de independencia de las
#series de tiempo.Hurst descubri? que muchos fen?menos naturales exhiben un
#comportamiento que puede ser caracterizado por un proceso aleatorio sesgado,
#en el cual existe "memoria de largo plazo" entre las observaciones, es decir,
#que los eventos de un periodo influyen en todos los siguientes.

#Si 0 ??? H < 0.5 corresponde a un comportamiento de anti-persistencia o
#anti-correlacional en la serie de tiempo (un periodo de crecimiento es seguido
#de otro de decrecimiento) que se caracteriza por un mayor contenido de alta
#frecuencia. Un incremento en los sucesos del pasado,supone un descenso en los
#sucesos futuros y viceversa. Un sistema anti-persistente tiende a regresar
#constantemente al lugar de procedencia y tienen la particularidad de ser
#se?ales muy irregulares. A este tipo de comportamientos se le conoce tambi?n
#como Ruido Rosa y se caracterizan porque abundan en la naturaleza y se
#encuentran relacionados con procesos de turbulencia.

#Si 0.5 < H ??? 1 implica series de tiempo que muestran procesos persistentes o
#correlacionados (un periodo de crecimiento es seguido de otro an?logo) y
#presentan un aspecto suave.

#Si H=1 indica un comportamiento determinista, sin aleatoreidad. Lo llamamos
#ruido negro, normalmente relacionado con procesos predecibles que ocurren
#de forma c?clica o periodica.

library(pracma)
#El ruido blanco carece de correlaci?n alguna con el aumento de la se?al, raz?n
#por la cual su coeficiente de Hurst suele rondar alrededor de 0.5.
Hwhite <- hurstexp(w@left, d = 128)
#Posee valores cercanos a 1.
Hbrown <- hurstexp(b, d = 128)
#Sus valores sueles ser <0.5. 
Hpink <- hurstexp(p@left, d = 128)
