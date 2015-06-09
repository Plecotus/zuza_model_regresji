## Posilkowalam sie ksiazka "Introduction to R" Szymona Drobniaka
# robilam wg wzoru z rozdzialu "Simple linear models"

bipniecia <- read.csv("reg_model_Zuza.csv", sep = ";", header = T)

bipniecia.lm <- with(bipniecia, lm(temperature~average_BPM))

summary(bipniecia.lm)

predict(bipniecia.lm)

class(bipniecia.lm)

attributes(bipniecia.lm)

predict(bipniecia.lm) [2:4]

bipniecia$temperature [2:4] - predict(bipniecia.lm) [2:4]

with(bipniecia, plot(temperature~average_BPM, type="l", las = 1))

x2 <- seq(0, 50, 2)
y2 <- predict(bipniecia.lm, list(temperature=x2), int="c")

## I na tym etapie sie wysypalam, bo nie wiem co dalej robic :)