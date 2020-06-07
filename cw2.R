# zadanie 1 ---------------------------------------------------------------

lista <- list(c('Ania', 'Śniadek'), 3.14, sqrt, seq(0.02, 1, by = 0.02))
#wyświetlanie listy
str(lista)
#usuwanie elementu z listy
lista[c(1,3)] <- NULL
str(lista)
#funckja gamma Eulera
lapply(lista, gamma)

# zadanie 2 ---------------------------------------------------------------
#install.packages("Matrix")

macierz <- matrix(c(1,2,1,5,0,2,3,5,1), nrow=3, ncol=3)
#rząd
Matrix::rankMatrix(macierz)
#wyznacznik
det(macierz)
#odwrotność 
solve(macierz)
#wartości oraz wektory własne
eigen(macierz)
#sumy arytmetyczne dla wierszy i kolumn
apply(macierz, 1, sum)
apply(macierz, 2, sum)
#średnie arytmetyczne dla wierszy i kolumn
apply(macierz, 1, mean)
apply(macierz, 2, mean)
#pomnożenie przez odwrotność
macierz %*% solve(macierz)

# zadanie 3 ---------------------------------------------------------------

(potegi <- (seq(1, 100, by = 1)^2))
jednosci <- potegi %% 10
table(jednosci)

# zadanie 4 ---------------------------------------------------------------

#tabliczka mnożenia dla liczb mniejszych od 6
outer(1:5, 1:5, FUN=function(x, y) paste(x, '*', y, '=', x*y))

# zadanie 5 ---------------------------------------------------------------

plik <- read.csv("dane1.csv", header = TRUE, sep=";")
(plik)
#tylko parzyste wiersze
#???
#pacjentki starsze niz 50 i wezly.chlonne = 1
(plik[(plik$Wiek > 50) & (plik$Wezly.chlonne == 1), ])

#zadanie 6
#install.packages('weathermetrics')

ramka <- data.frame(
  #month = format(ISOdate(2004,1:12,1),"%B"),
  month = c('Styczeń', 'Luty', 'Marzec', 'Kwiecień', 'Maj', 'Czerwiec', 'Lipiec', 'Sierpień', 'Wrzsień', 'Październik', 'Listopad', 'Grudzień'),
  NY_F = c(32, 33, 41, 52, 62, 72, 77, 75, 68, 58, 47, 35)
)

#dodanie kolumny plus zamiana F na C
library(weathermetrics)
ramka = cbind(ramka[1:12, ], NY_C = fahrenheit.to.celsius(ramka$NY_F, round = 2))
#zmiana nazwy kolumn
names(ramka) <- c("month", "NY_Fahrenheit", "NY_Celsiusz")
#usuwanie kolumny
ramka[,-2]
#zapisywanie danych
save(ramka, file="NY_temp.RData")