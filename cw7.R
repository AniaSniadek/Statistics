#TESTOWANIE HIPOTEZ STATYSTYCZNYCH

# zadanie 1 ---------------------------------------------------------------
#zweryfikuj hipotezę, że średnia głębokość morza w tym regionie wynosi 870m
#test t-studenta dla jednej próby

glebokosc <- c(862, 870, 876, 866, 871, 865, 861, 873, 871, 872)

#shapiro.test
(wynik <- shapiro.test(glebokosc))
#p-value
(wynik$p.value)

#wykres
qqnorm(glebokosc)
qqline(glebokosc)

#średnia
(mean(glebokosc))

#test t-studenta (srednia < 870, więc alternative less)
(wynik2 <- t.test(glebokosc, mu = 870, alternative = 'less'))
(wynik2$p.value)

# zadanie 2 ---------------------------------------------------------------
#wniosek CTA
#test t-studenta dla dwóch prób niezależnych i test f-snedcora

a <- c(78.2, 78.5, 75.6, 78.5, 78.5, 77.4, 76.6)
b <- c(76.1, 75.2, 75.8, 77.3, 77.3, 77.0, 74.4, 76.2, 73.5, 77.4)

#wykres ramkowy
boxplot(a, b)

#shapiro.test a
(wynikA <- shapiro.test(a))
#p-value
(wynikA$p.value)

#wykres a
qqnorm(a)
qqline(a)

#shapiro.test b
(wynikB <- shapiro.test(b))
(wynikB$p.value)

#wykres b
qqnorm(b)
qqline(b)

#f-sndecor
(var(a))
(var(b))

#test f-sndecor (var(a) < var(b) więc alternative less)
(wynikAB <- var.test(a, b, alternative = "less"))
(wynikAB$p.value)

#średnia a i b
(mean(a))
(mean(b))

#t.student (var.equal=TRUE, gdy p.value z f-sndecor > 0.05)
(wynikAB2 <- t.test(a, b, var.equal = TRUE, alternative = "greater"))
(wynikAB2$p.value)

# zadanie 3 ---------------------------------------------------------------
#Zweryfikuj, czy film znacznie poprawia stosunek do szkół publicznych
#test t-studenta dla prób zależnych

grupaPrzed <- c(84, 87, 87, 90, 90, 90, 90, 93, 93, 96)
grupaPo <- c(89, 92, 98, 95, 95, 92, 95, 92, 98, 101)

#wykres ramkowy
boxplot(grupaPrzed, grupaPo)

#shapiro.test grupaPrzed
(wynikA <- shapiro.test(grupaPrzed))
(wynikA$p.value)

#wykres
qqnorm(grupaPrzed)
qqline(grupaPrzed)

#skapiro.test grupaPo
(wynikB <- shapiro.test(grupaPo))
(wynikB$p.value)

#wykres
qqnorm(grupaPo)
qqline(grupaPo)

#średnia
(mean(grupaPrzed))
(mean(grupaPo))

#t.student (paired=TRUE dla grup zależnych)
(wynikTest <- t.test(grupaPrzed, grupaPo, alternative = 'less', paired = TRUE))
(wynikTest$p.value)

# zadanie 4 ---------------------------------------------------------------
#Czy możemy stwierdzić, że średni wzrost mężczyzn jest znacznie większy niż wzrost kobiet?
#test t-studenta dla dwóch grup niezależnych i test f-snedcora
  
men <- c(171, 176, 179, 189, 176, 182, 173, 179, 184, 186, 189, 167, 177)
women <- c(161, 162, 163, 162, 166, 164, 168, 165, 168, 157, 161, 172)

#wykres ramkowy
boxplot(men, women)

#shapiro.test men
wynikMen <- shapiro.test(men)
(wynikMen$p.value)

#wykres
qqnorm(men)
qqline(men)

#shapiro.test women
wynikWomen <- shapiro.test(women)
(wynikWomen$p.value)

#wykres
qqnorm(women)
qqline(women)

#f-sndecor
var(men)
var(women)

#test f-sndecor
wynik <- var.test(men, women, alternative = "greater")
(wynik$p.value)

#średnia
mean(men)
mean(women)

#t-student (var.equal=FALSE czyli domyślne więc nie piszemy bo p.value z f-sndecor < 0,05)
wynik2 <- t.test(men, women, alternative = "greater")
(wynik2$p.value)