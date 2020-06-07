#STATYSTYKA OPISOWA

# zadanie 1 ---------------------------------------------------------------

#1.1
ankieta <- read.table("http://ls.home.amu.edu.pl/data_sets/ankieta.txt", header = TRUE)
(ankieta)

#1.2
#rozkład empiryczny zmiennej wynik
data.frame(cbind(liczebnosc = table(ankieta$wynik),
                 procent = prop.table(table(ankieta$wynik))))

#1.3
#rozkład empiryczny zmiennej wynik tylko dla szkoly podstawowej
(podstawowa <- ankieta[ankieta$szkola == "p",])
data.frame(cbind(liczebnosc = table(podstawowa$wynik),
                 procent = prop.table(table(podstawowa$wynik))))

#1.4 
#wykres słupkowy - liczebność
barplot(table(ankieta$wynik),
        xlab = "Liczba zgłoszeń", ylab = "Liczebność",
        col = c("black", "red", "green", "blue", "#00FFFF"),
        main = "Rozkład empiryczny zmiennej wynik")

#wykres słupkowy - prawdopodobieństwo
barplot(prop.table(table(ankieta$wynik)),
        xlab = "Liczba zgłoszeń", ylab = "Prawdopodobieństwo",
        col = c("black", "red", "green", "blue", "#00FFFF"),
        main = "Rozkład empiryczny zmiennej wynik")

#wykres kołowy
pie(table(ankieta$wynik))

#1.5
#podział na mężczyzn i kobiety
barplot(table(ankieta$wynik, ankieta$plec), beside=TRUE,
        col = c("black", "red", "green", "blue", "#00FFFF"))


# zadanie 2 ---------------------------------------------------------------

#2.1
load("Centrala.RData")
(Centrala)

#2.2
#rozkład empiryczny liczby zgłoszeń
data.frame(cbind(liczebność = table(Centrala$Liczba),
                 procent = prop.table(table(Centrala$Liczba))))

#2.3
#wykres słupkowy - liczebność
barplot(table(Centrala$Liczba),
        xlab="Liczba zgłoszeń", ylab="Liczebność",
        col = c("black", "red", "green", "blue", "#00FFFF"),
        main="Rozkład empiryczny liczby zgłoszeń")

#wykres słupkowy - prawdopodobieństwo
barplot(prop.table(table(Centrala$Liczba)),
        xlab="Liczba zgłoszeń", ylab="Prawdopodobieństwo",
        col = c("black", "red", "green", "blue", "#00FFFF"),
        main="Rozkład empiryczny liczby zgłoszeń")

#wykres kołowy
pie(table(Centrala$Liczba))

#2.4
#średnia
(mean(Centrala$Liczba))
#mediana
(median(Centrala$Liczba))
#odchylenie standardowe
(sd(Centrala$Liczba))
#współczynnik zmienności
(sd(Centrala$Liczba) / mean(Centrala$Liczba) * 100)


# zadanie 3 ---------------------------------------------------------------

mat <- cbind(c(0.9,1.0,2.7,4.9,12.2),c(6.2,4.6,9.2,8.2,2.8),c(2.1,6.4,5.9,5.0,5.9),c(4.1,3.8,7.4,1.2,8.2),c(7.3,5.0,3.0,10.1,0.5))
wiatr <- matrix(mat, nrow=5, ncol=5)
(wiatr)

#3.1
#rozkład empiryczny z przedziałami
data.frame(cbind(liczebność = table(cut(wiatr, breaks = seq(0, 14, 2))),
                 procent = prop.table(table(cut(wiatr, breaks = seq(0, 14, 2))))))

#3.2
#histogram
hist(wiatr,
     xlab = "Średnia szybkość wiatru", ylab = "Frequency",
     main="Rozkład empiryczny średniej szybkości witaru")
rug(jitter(wiatr))

#histogram z estymatorem jądrowym gęstości
hist(wiatr,
     xlab = "Średnia szybkość wiatru", ylab = "Density",
     main="Rozkład empiryczny średniej szybkości witaru",
     probability = TRUE,
     col = "lightblue")
lines(density(wiatr), col="green", lwd=2)

#wykres pudełkowy (ramkowy)
boxplot(c(mat),
        ylab="Średnia szybkość wiatru",
        main="Rozkład empiryczny średniej szybkości witaru")

#3.3
#install.packages("e1071")
#średnia
(mean(wiatr))
#mediana
(median(wiatr))
#odchylenie standardowe
(sd(wiatr))
#współczynnik zmienności
(sd(wiatr) / mean(wiatr) * 100)
#współczynnik asymetrii
library(e1071)
skewness(wiatr)
#kurtoza
kurtosis(wiatr)


# zadanie 4 ---------------------------------------------------------------

wspolczynnik_zmiennosci <- function(x, na.rm = FALSE){
        if(is.numeric(x) == FALSE) stop("arguent nie jest liczbą")
        else if(na.rm == FALSE) return(NA)
        
        x <- x[!is.na(x)]
        return(sd(x) / mean(x) * 100)
}

x <- c(1, NA, 3)
wspolczynnik_zmiennosci(x)
wspolczynnik_zmiennosci(x, na.rm = TRUE)
wspolczynnik_zmiennosci()
wspolczynnik_zmiennosci(c("x", "y"))
           