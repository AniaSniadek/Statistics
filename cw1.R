# zadanie 2 ---------------------------------------------------------------

x <- rep(TRUE, times = 3)
x[4:7] <- rep(FALSE, times = 4)
x[8:9] <- rep(TRUE, times = 2)
x[10:14] <- rep(FALSE, times = 5)
x <- c(rep(TRUE, 3), rep(FALSE, 4), rep(TRUE, 2), rep(FALSE, 5))

#zamiana na wektor numeryczny
as.numeric(x)

# zadanie 3 ---------------------------------------------------------------

y <- seq(1, 20, by = 1)
y[21:30] <- rep(0, times = 10)
y[31:50] <- seq(2, 40, by = 2)

#odwrócenie wektora
z <- rev(y)
y <- c(y, z)

# zadanie 4 ---------------------------------------------------------------

l <- c(letters[5], letters[10], letters[15], letters[20], letters[25])

# zadanie 5 ---------------------------------------------------------------
#???

# zadanie 6 ---------------------------------------------------------------

o <- c(6,3,4,5,2,3)
#odwrócenie elementów od największego do najmniejszego
o[order(o, decreasing = TRUE)]
#odwrócenie elementów od najmniejszego do największego
o[order(o, decreasing = FALSE)]

# zadanie 7 ---------------------------------------------------------------

wektor <- c(-1.876,-1.123,-0.123,0,0.123,1.123,1.876)
#znak liczby
znak <- sign(wektor)
#zaokroglenie do 2 miejsc po przecinku
zaokraglenie <- round(wektor, 2)
#liczba całkowita
calkowita <- floor(zaokraglenie)

# zadanie 8 ---------------------------------------------------------------
#???

# zadanie 9 ---------------------------------------------------------------
#??????

# zadanie 10 ---------------------------------------------------------------
#??????

# zadanie 11 ---------------------------------------------------------------
#??????

# zadanie 12 ---------------------------------------------------------------
#??????