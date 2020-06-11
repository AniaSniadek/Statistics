#MODEL STATYSTYCZNY I ESTYMACJA PUNKTOWA

# zadanie 1 ---------------------------------------------------------------
load(url("http://ls.home.amu.edu.pl/data_sets/czas_oczek_tramwaj.RData"))
(czas_oczek_tramwaj)

#estymator średniej i odchylenia standardowego
a_est <- mean(czas_oczek_tramwaj) - sqrt(3) * sd(czas_oczek_tramwaj) * sqrt((length(czas_oczek_tramwaj) - 1) / (length(czas_oczek_tramwaj)))
b_est <- mean(czas_oczek_tramwaj) + sqrt(3) * sd(czas_oczek_tramwaj) * sqrt((length(czas_oczek_tramwaj) - 1) / (length(czas_oczek_tramwaj)))

library(EnvStats)
p_est <- EnvStats::eunif(czas_oczek_tramwaj, method = "mme")

# zadanie 2 ---------------------------------------------------------------
load("Centrala.RData")
(Centrala)

#2.1
#dyskretna zmienna ilościowa
#wykres słupkowy
barplot(prop.table(table(Centrala$Liczba)),
        main = "Rozkład empiryczny liczby zgłoszeń")
#sugeruje rozkład Poissona

#2.2
#estymator
library(EnvStats)
(p_est <- EnvStats::epois(Centrala$Liczba)$parameters)

#2.3
#empiryczne i teoretyczne - porównanie
probs <- dpois(x = sort(unique(Centrala$Liczba)), lambda = p_est)
sum(probs)

counts <- matrix(c(prop.table(table(Centrala$Liczba)), probs), nrow = 2, byrow = TRUE)
rownames(counts) <- c("empiryczny", "teoretyczny")
colnames(counts) <- sort(unique(Centrala$Liczba))
counts

#wykres słupkowy
barplot(counts, 
        xlab = "Liczba zgłoszeń", ylab = "Prawdopodobieństwo",
        main = "Rozkłady empiryczny i teoretyczny liczby zgłoszeń",
        col = c("red", "blue"), legend = rownames(counts), beside = TRUE)

#wykres kwantyl-kwantyl
qqplot(rpois(length(Centrala$Liczba), lambda = p_est), Centrala$Liczba,
       xlab = "Kwantyle teoretyczne", ylab = "Kwantyle empiryczne",
       main="Wykres kwantyl-kwantyl dla liczby zgłoszeń")
qqline(Centrala$Liczba, distribution = function(probs){
  qpois(probs, lambda = p_est)
})

#2.6
# prawd. empiryczne
mean(Centrala$Liczba < 4)
# prawd. teoretyczne
ppois(3, lambda = mean(Centrala$Liczba))

# zadanie 4 ---------------------------------------------------------------

wiatr = c(0.9, 6.2, 2.1, 4.1, 7.3, 1.0, 4.6, 6.4, 3.8, 5.0, 2.7, 9.2, 5.9, 7.4, 3.0, 4.9, 8.2, 5.0, 1.2, 10.1, 12.2, 2.8, 5.9, 8.2, 0.5)
(wiatr)

#4.1
#sugeruję rozkład Rayleigha

#4.2
#ENW
(ENW = mean(wiatr^2))

#4.3
#histogram z estymatorem jądrowym gęstości
lambda <- ENW

hist(wiatr, 
     xlab = "Średnia szybkość wiatru", 
     main = "Rozkład empiryczny i teoretyczny średniej szybkości wiatru",
     probability = TRUE, 
     col = "lightgreen")
lines(density(wiatr), col = "red", lwd = 2)
curve(VGAM::drayleigh(x, sqrt(lambda / 2)), 
      add = TRUE, col = "blue", lwd = 2)
legend(x = 5, y = 0.04, legend = c("empiryczny", "teoretyczny"), col = c("red", "blue"), lwd = 2)

#4.4
#wykres kwantyl-kwantyl 
EnvStats::qqPlot(wiatr, 
                 distribution = "unif", 
                 param.list = list(min = min(wiatr), max = max(wiatr)),
                 add.line = TRUE,
                 xlab = "Kwantyle teoretyczne", ylab = "Kwantyle empiryczne",
                 main="Wykres kwantyl-kwantyl dla średniej szybkości wiatru")

#4.6
#prawd. empiryczne
mean(wiatr >= 4 & wiatr <= 8)
#prawd. teoretyczne
#??

#4.7
#??