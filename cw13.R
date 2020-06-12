#ANALIZA SKŁADOWYCH GŁÓWNYCH

# zadanie 1 ---------------------------------------------------------------

dane <- USArrests
dane <- dane[,-3]

#1.1
#Przygotowanie danych do analizy
var(dane)
#Skalowanie
dane_scale <- scale(dane)
var(dane_scale)
#Analiza składowych głównych
pca <- prcomp(dane, scale=TRUE)

#1.2
summary(pca)
#procent wariancji - drugi wiersz

#1.3
#współrzędne obserwacji
head(pca$x)

#1.4
#interpretacja ładunków
pca$rotation
#wykres
#???

#1.5
#wykres osypiska
plot(pca)

#1.6
biplot(pca)

#1.7
#drzewo rozpinające
library(ape)
plot(mst(dist(dane_scale)), x1 = pca$x[, 1], x2 = pca$x[, 2])

# zadanie 2 ---------------------------------------------------------------
#??

