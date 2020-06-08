#ANALIZA WARIANCJI

# zadanie 1 ---------------------------------------------------------------

dane <- read.table("http://ls.home.amu.edu.pl/data_sets/kontekst.txt")
(dane)
colnames(dane) <- c("number", "context")

#1.1
#średnia liczb zapamiętaych słów w grupach
aggregate(dane$number,
         list(CONTEXT = dane$context),
         FUN = mean)

#wykres ramkowy
boxplot(number ~ context, data = dane, 
        xlab = "Kontekst", ylab = "Liczba słów")

#1.2
#test analizy wariancji 
summary(aov(number ~ context, data = dane))

#1.3
#sprawdź założenia modelu jednoczynnikowej analizy wariancji

#shapiro.test
(test1 <- shapiro.test(lm(number ~ context, data = dane)$residuals))
(test1$p.value)
#wykres
qqnorm(dane$number)
qqline(dane$number)
#barlett.test
(test2 <- bartlett.test(number ~ context, data = dane))
(test2$p.value)
#fligner.test
(test3 <- fligner.test(number ~ context, data = dane))
(test3$p.value)
#leveneTest
library(carData)
library(car)
(test4 <- leveneTest(number ~ context, data = dane))
(test5 <- leveneTest(number ~ context, data = dane, center = "mean"))

#1.4
#testy post hoc
attach(dane)
pairwise.t.test(number, context, data = dane)

model_aov <- aov(number ~ context, data = dane)
TukeyHSD(model_aov)
plot(TukeyHSD(model_aov))

library(agricolae)
HSD.test(model_aov, "context", console = TRUE)
SNK.test(model_aov, "context", console = TRUE)
LSD.test(model_aov, "context", p.adj = "holm", console = TRUE)

#1.5
#???

# zadanie 2 ---------------------------------------------------------------

#2.1
dane <- read.table("http://ls.home.amu.edu.pl/data_sets/Eysenck.txt",  header=TRUE)
#usunięcie pierwszej kolumny
dane = dane[,-1]
colnames(dane) <- c("wynik", "instrukcja")
(dane)

#2.2
#średnia wartości cech
aggregate(dane$wynik,
          list(CONTEXT = dane$instrukcja),
          FUN = mean)

#wykres ramkowy
boxplot(wynik ~ instrukcja, data = dane, 
        xlab = "Instrukcja", ylab = "Wynik")

#2.3
#test analizy wariancji 
summary(aov(wynik ~ instrukcja, data = dane))

#2.4
#sprawdź założenia modelu jednoczynnikowej analizy wariancji

#shapiro.test
(test1 <- shapiro.test(lm(wynik ~ instrukcja, data = dane)$residuals))
(test1$p.value)
#wykres
qqnorm(dane$wynik)
qqline(dane$wynik)
#barlett.test
(test2 <- bartlett.test(wynik ~ instrukcja, data = dane))
(test2$p.value)
#fligner.test
(test3 <- fligner.test(wynik ~ instrukcja, data = dane))
(test3$p.value)
#leveneTest
library(carData)
library(car)
(test4 <- leveneTest(wynik ~ instrukcja, data = dane))
(test5 <- leveneTest(wynik ~ instrukcja, data = dane, center = "mean"))

#2.5
#testy post hoc
attach(dane)
pairwise.t.test(wynik, instrukcja, data = dane)

model_aov <- aov(wynik ~ instrukcja, data = dane)
TukeyHSD(model_aov)
plot(TukeyHSD(model_aov))

library(agricolae)
HSD.test(model_aov, "instrukcja", console = TRUE)
SNK.test(model_aov, "instrukcja", console = TRUE)
LSD.test(model_aov, "instrukcja", p.adj = "holm", console = TRUE)

#2.6
#??
