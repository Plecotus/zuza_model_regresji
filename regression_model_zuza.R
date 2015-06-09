## Posilkowalam sie ksiazka "Introduction to R" Szymona Drobniaka
# robilam wg wzoru z rozdzialu "Simple linear models"

## Zmienna "average_BPM" figuruje jako numeric

bipniecia <- read.csv2("reg_model_Zuza.csv", sep = ";", header = T)

bipniecia.lm <- with(bipniecia, lm(temperature~average_BPM))

summary(bipniecia.lm)

predict(bipniecia.lm) [2:4]

bipniecia$temperature [2:4] - predict(bipniecia.lm) [2:4]

# Teraz plot wyglada ladnie
with(bipniecia, plot(temperature~average_BPM, type="l", las = 1))

## Chcialabym wiedziec, po co to jest? Ale przekopiowalam to z przykladu z ksiazki 
# z mysla, ze zobacze, co mi to wyrzuci. Cos wyrzucilo, ale nie wiem, co.
x2 <- seq(0, 50, 2)
y2 <- predict(bipniecia.lm, list(temperature=x2), int="c")