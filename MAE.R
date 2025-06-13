#llamamos librerias
library(dplyr)
library(car)
library(explore)
library(psych)
library(corrplot)
list.files()

######################Regresión######################################
reg <- read.csv("https://github.com/d-correalr/MAE/Train real state.csv")
colnames(reg)
dim(reg)
head(reg)
str(reg)
describe(reg)
boxplot(reg$SalePrice)

boxplot(reg[, c("Floor", "N_APT", "N_manager", "N_elevators")],
        main = "Características del edificio",
        col = "grey",
        las = 2,
        notch = TRUE)

boxplot(reg[, c("TimeToBusStop", "TimeToSubway")],
        main = "Accesibilidad al transporte público",
        col = "lightgreen",
        las = 2,
        notch = TRUE)


################Clasificación######################################
list.files()
clf <- read.csv("https://github.com/d-correalr/MAE/Train bank.csv")

colnames(clf)
dim(clf)
colnames(clf)
head(clf)
str(clf)
describe(clf)

table(clf$Subscription)
barplot(table(clf$Subscription),
        main = "Distribución de la variable Subscription",
        names.arg = c("No", "Sí"),
        col = c("tomato", "skyblue"),
        ylab = "Frecuencia")

boxplot(clf[, c("Age")],
        main = "Caracteristicas personas",
        col = "grey",
        las = 2,
        notch = TRUE)
