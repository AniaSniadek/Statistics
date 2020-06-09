#REGRESJA LINIOWA

# zadanie 1 ---------------------------------------------------------------
#Zakładając liniową zależność między rokiem a liczbą przypadków, wykonaj kompleksową analizę regresji

rok <- c(1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002)
liczba_przyadkow <- c(39.7, 38.2, 34.7, 33.1, 30.1, 28.4, 26.3, 24.7)
dane <- data.frame(rok = rok, liczba_przyadkow = liczba_przyadkow)
(dane)

#1.1
#wykres rozrzutu
plot(dane, main="Wykres rozrzutu", pch=16)

#1.2
#model
(model <- lm(liczba_przyadkow ~ rok, data=dane))
#prosta regresji
plot(dane, main="Wykres rozrzutu", pch=16)
abline(model, col="red", lwd=2)
#estymacja parametrów
coef(model)
confint(model)

#1.3
#podsumowanie modelu
summary(model)

#1.4
#wartości dopasowane przez model
fitted(model)
#reszty
residuals(model)

#1.5
#przedziały ufności dla predykcji
temp_rok <- data.frame(rok = seq(min(dane$rok) - 10, 
                                             max(dane$rok) + 10, 
                                             length = 100))
pred <- predict(model, temp_rok, interval = "prediction")
plot(dane, main = "Wykres rozrzutu", pch = 16)
abline(model, col = "red", lwd = 2)
lines(temp_rok$rok, pred[, 2], lty = 2, col = "red")
lines(temp_rok$rok, pred[, 3], lty = 2, col = "red")

#1.6
#predykcja liczby przypadków w latach 2003-2007
nowy_rok <- data.frame(rok = c(2003, 2004, 2005, 2006, 2007))
predict(model, nowy_rok, interval = "prediction")

#wykres rozrzutu
#???

# zadanie 2 ---------------------------------------------------------------

load("braking.RData")
(dane <- braking)

#2.1
#wykres rozrzutu
plot(dane, main="Wykres rozrzutu", pch=16)

#2.2
#model
(model <- lm(distance ~ speed, data=dane))
#prosta regresji
plot(dane, main="Wykres rozrzutu", pch=16)
abline(model, col="red", lwd=2)
#estymacja parametrów
coef(model)
confint(model)

#2.3
#podsumowanie modelu
summary(model)

#2.4
#wartości dopasowane przez model
fitted(model)
#reszty
residuals(model)

#2.5
#przedziały ufności dla predykcji
temp_speed <- data.frame(speed = seq(min(dane$speed) - 10, 
                                 max(dane$speed) + 10, 
                                 length = 100))
pred <- predict(model, temp_speed, interval = "prediction")
plot(dane, main = "Wykres rozrzutu", pch = 16)
abline(model, col = "red", lwd = 2)
lines(temp_speed$speed, pred[, 2], lty = 2, col = "red")
lines(temp_speed$speed, pred[, 3], lty = 2, col = "red")

#2.6
#?????