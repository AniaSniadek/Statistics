#REGRESJA WIELOKROTNA I KROKOWA

# zadanie 1 ---------------------------------------------------------------
dane <- read.csv("Automobile.csv", na = "?")

#1.1
dane <- na.omit(dane)

#1.2
tmp = subset(
  dane,
  select=c(
    "horsepower", 
    "city.mpg", 
    "peak.rpm", 
    "curb.weight", 
    "num.of.doors", 
    "price"
  )
)
pairs(tmp)
model <- lm(formula = price ~ horsepower + city.mpg + peak.rpm + curb.weight + num.of.doors, data = tmp)

#estymatory
coef(model)
#przedziały ufności
confint(model)

#podsumowanie modelu
summary(model)

#wartości dopasowane przez model
fitted(model)
#wartości reszt
residuals(model)
