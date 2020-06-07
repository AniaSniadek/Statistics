# zadanie 1 ---------------------------------------------------------------

x <- 1:5
wynik <- 1
#iloczyn elementów wektora za pomocą pętli for, repeat, while
for(i in x) {
  wynik <- wynik * i
}
print(wynik)

i <- 1
repeat {
  wynik <- wynik * i
  i <- i + 1
  if(i > length(x)) break
}
print(wynik)

i <- 1
while(i <= length(x)){
  wynik <- wynik * i
  i <- i + 1
}
print(wynik)

# zadanie 2 ---------------------------------------------------------------

#choose(20,5)(n r) <- n po r
licznik <- 0
for (i in 1:100)
{
  for (j in 1:100)
  {
    if (choose(i,j) > 1000000){
      licznik = licznik + 1
    }
  }
}
print(licznik)

# zadanie 3 ---------------------------------------------------------------

#funkcja sprawdzająca czy x jest palindromem
palindrom <- function(x) {
  all(x == rev(x))
}
palindrom(c(1,2,3,3,2,1))
palindrom(c(1,2,3,2,2,1))

# zadanie 4 ---------------------------------------------------------------

#zamiana stopni na radiany
radiany <- function(x) {
  wynik <- (x * 2 * pi) / 360
  return(wynik)
}
cat(radiany(0), radiany(30), radiany(45), radiany(60), radiany(90))

#ramka danych z sin, cos, tg, ctg
sinus <- function(x) {sin(x*pi/180)}
cosinus <- function(x) {cos(x*pi/180)}
tangens <- function(x) {tan(x*pi/180)}
cotangens <- function(x) {1/tan(x*pi/180)}
ramka <- data.frame(
  sin = c(sinus(0), sinus(30), sinus(45), sinus(60), sinus(90)),
  cos = c(cosinus(0), cosinus(30), cosinus(45), cosinus(60), cosinus(90)),
  tg = c(tangens(0), tangens(30), tangens(45), tangens(60), tangens(90)),
  ctg = c(cotangens(0), cotangens(30), cotangens(45), cotangens(60), cotangens(90))
)
ramka


# zadanie 5 ---------------------------------------------------------------

#zwracanie 3 najmniejszych i 3 najwiekszych elementów w wektorze
x <- c(2, 6, 1, 5, 7, 3, 4)
y <- c(2, 6)
extreme_3 <- function(x) {
  x = sort(x)
  #error message
  if(length(x) < 3) stop("za krótki element")
  return(c(head(x,3), tail(x,3)))
}
(extreme_3(x))
(extreme_3(y))