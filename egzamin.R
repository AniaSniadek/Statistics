
# zadanie 3 ---------------------------------------------------------------
plik <- read.csv("weight-height.csv")
(plik)

#Podział danych na men i women
(men <- plik[plik$Gender == "Male",])
(women <- plik[plik$Gender == "Female",])

#Sprawdzanie p-value przez shapiro.test
(wynikM <- shapiro.test(men$Height))
(wynikF <- shapiro.test(women$Height))

#test f-sndecor - sprawdzanie jednorodności wariancji 
(var(men$Height))
(var(women$Height))
(wynikMF <- var.test(men$Height, women$Height, alternative = "greater"))
#p-value < 0.05, więc wariancja nie jest jednorodna, czyli var.equal = FALSE

#test t-studenta do sprawdzenia wartości statystyki testowej
(mean(men$Height))
(mean(women$Height))
(wynikMF2 <- t.test(men$Height, women$Height, var.equal = FALSE, alternative = "greater"))
#wartość statystyki testowej: 95.60

# zadanie 4 ---------------------------------------------------------------
plik2 <- read.csv("computers.csv")
(plik2)

#komputery tylko z ekranem 14 cali
(dane <- plik2[plik2$screen == 14,])

#rozkład zmiennej RAM - w procentach
(prop.table(table(dane$ram)))
