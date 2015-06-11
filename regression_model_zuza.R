## Posilkowalam sie ksiazka "Introduction to R" Szymona Drobniaka
# robilam wg wzoru z rozdzialu "Simple linear models" - although it was not simple ; p


# Wczytuje dane
bipniecia <- read.csv2("input/reg_model_Zuza.csv")

# Dodaje zmienną average_BPM^2, log(),log10() i  sqrt()

bipniecia$avg_BPM_sqrd <-bipniecia$average_BPM^2
bipniecia$log_BPM <- log(bipniecia$average_BPM)
bipniecia$sqrt_BPM <- sqrt(bipniecia$average_BPM)
bipniecia$log10_BPM <- log10(bipniecia$average_BPM)

# Tworzę model regresji liniowej, ale trochę oszukujemy R i dodajemy kilka
# innych zmiennch, które są przekształceniem pierwszego iXa.

bipniecia.lm1 <- lm(temperature~average_BPM + avg_BPM_sqrd +
                        log_BPM + sqrt_BPM, data = bipniecia)


summary(bipniecia.lm1) 

# Wygląda to nie najgorzej - generalnie w 50 % przypadków reszty
# z regresji to +-.3 stopnia. Maksymalnie różnica jest o jeden stopień.
# Może z większą ilością danych lepiej dobierze wspólczynniki, ale wydaje mi się
# że nie jest źle.


# Plot

# Wykres rozrzutu - x = bipnięcia, y = temperatura
plot(temperature~average_BPM, data = bipniecia)

# Dorzucenie lini przewidzianej przez model regresji
lines(bipniecia$average_BPM, bipniecia.lm1$fitted.values, col="blue")





## Kilka innych modeli z mniejszą ilością zmiennych, ale wypadają trochę gorzej

bipniecia.lm2 <- lm(temperature~average_BPM +
                        log_BPM + sqrt_BPM, data = bipniecia)
lines(bipniecia$average_BPM, bipniecia.lm2$fitted.values, col="red")

bipniecia.lm3 <- lm(temperature~average_BPM +
                         sqrt_BPM, data = bipniecia)
lines(bipniecia$average_BPM, bipniecia.lm3$fitted.values, col="green")

bipniecia.lm4 <- lm(temperature~average_BPM +
                        log_BPM, data = bipniecia)
lines(bipniecia$average_BPM, bipniecia.lm4$fitted.values, col="black")


## Chcialabym wiedziec, po co to jest? Ale przekopiowalam to z przykladu z ksiazki 
# z mysla, ze zobacze, co mi to wyrzuci. Cos wyrzucilo, ale nie wiem, co.


## A Książka nie tłumaczy co to robi?? ; ]
x2 <- seq(0, 50, 2)
y2 <- predict(bipniecia.lm, list(temperature=x2), int="c")