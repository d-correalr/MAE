#llamamos librerias
library(dplyr)
library(car)
library(explore)
library(psych)
library(corrplot)
library(ggplot2)
list.files()

######################Regresión######################################
reg <- read.csv("https://raw.githubusercontent.com/d-correalr/MAE/main/Train%20real%20state.csv")
#verificación inicial del dataset
colnames(reg)
dim(reg)
head(reg)
str(reg)
describe(reg)

#verificamos Nas

colSums(is.na(reg))

#no necesitamos la columna de índice, por lo que eliminamos del datset

reg <- reg%>% select(-X)

#hacemos histogramas de la variable objetivo

ggplot(reg, aes(x = SalePrice)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribución del precio de venta", x = "Precio", y = "Frecuencia")

#iniciamos con el análisis de correlación para variables númericas

numeric_vars <- reg %>%
  select(where(is.numeric)) %>%
  select(-SalePrice)

cor_matrix <- cor(reg[, sapply(reg, is.numeric)], use = "complete.obs")

  
# correlación con la variable objetivo

cor_target <- cor_matrix[, "SalePrice"]
cor_target_sorted <- sort(cor_target, decreasing = TRUE)

print("Correlaciones con SalePrice:")
print(cor_target_sorted)

# Miramos matriz de correlación
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7, number.cex = 0.7)




# miramos las variables categoricas

#  número de categorías y frecuencia
cat_vars <- c("HallwayType", "HeatingType", "AptManageType", 
              "TimeToBusStop", "TimeToSubway", "SubwayStation")

for (var in cat_vars) {
  cat("🔹 Variable:", var, "\n")
  print(table(reg[[var]]))
  cat("\n")
}

# 2. Boxplot de SalePrice por categoría
library(ggplot2)

for (var in cat_vars) {
  p <- ggplot(reg, aes_string(x = var, y = "SalePrice")) +
    geom_boxplot(fill = "lightblue") +
    theme_minimal() +
    labs(title = paste("SalePrice por", var), x = var, y = "SalePrice")
  print(p)
}

 
library(tidyverse)

# Revisión previa
reg <- reg %>% select(-X)

# 1. Agrupar TimeToSubway en niveles
reg <- reg %>%
  mutate(TimeToSubway_group = case_when(
    TimeToSubway == "0~5min" ~ "cerca",
    TimeToSubway %in% c("5min~10min", "10min~15min") ~ "media",
    TRUE ~ "lejos"
  ))

# 2. Crear variables dummies para las categóricas
reg_mod <- reg %>%
  select(SalePrice, Size.sqf., Floor, YearBuilt, N_elevators,
         N_Parkinglot.Basement., N_FacilitiesInApt,
         HallwayType, HeatingType, AptManageType, TimeToSubway_group) %>%
  mutate(across(c(HallwayType, HeatingType, AptManageType, TimeToSubway_group), as.factor))

# 3. Ajustar modelo de regresión lineal
modelo_lm <- lm(SalePrice ~ ., data = reg_mod)
summary(modelo_lm)





################Clasificación######################################
list.files()
clf <- read.csv("https://raw.githubusercontent.com/d-correalr/MAE/main/Train%20bank.csv")

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
