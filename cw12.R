#ANALIZA KORELACJI

# zadanie 1 ---------------------------------------------------------------

dane = mtcars
(dane)

#1.1
#wykres rozrzutu
plot(dane$mpg, dane$wt, xlab = "mpg", ylab = "wt", pch=16)

#1.2
#założenie testu istotności
shapiro.test(dane$mpg)$p.value
qqnorm(dane$mpg)
qqline(dane$mpg, col = "red")
shapiro.test(dane$wt)$p.value
qqnorm(dane$wt)
qqline(dane$wt, col = "red")

#1.3
#test istotności
cor.test(dane$mpg, dane$wt, method = "pearson")$p.value
#współczynnik korelacji - punktowo
cor(dane$mpg, dane$wt)
#lub cor.test(dane$mpg, dane$wt, method = "pearson").$est
#współczynnik korelacji - przedziałowo
cor.test(dane$mpg, dane$wt, method = "pearson")$conf.int

#1.4
#test istotności Kendall
cor.test(dane$mpg, dane$wt, method = "kendall")$p.value
#współczynnik korelacji
cor.test(dane$mpg, dane$wt, method = "kendall")$est

#test istotności Spearman
cor.test(dane$mpg, dane$wt, method = "spearman")$p.value
#współczynnik korelacji
cor.test(dane$mpg, dane$wt, method = "spearman")$est
