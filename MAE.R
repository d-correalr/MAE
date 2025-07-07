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

#### exportar para Kaggle

# Leer el conjunto de prueba
test <- read.csv("https://raw.githubusercontent.com/d-correalr/MAE/main/Test%20real%20state.csv")
test <- test %>%
  rename(Id = X)


# Realizar las mismas transformaciones que al conjunto de entrenamiento
test <- test %>%
  mutate(TimeToSubway_group = case_when(
    TimeToSubway == "0~5min" ~ "cerca",
    TimeToSubway %in% c("5min~10min", "10min~15min") ~ "media",
    TRUE ~ "lejos"
  )) %>%
  select(Id, Size.sqf., Floor, YearBuilt, N_elevators,
         N_Parkinglot.Basement., N_FacilitiesInApt,
         HallwayType, HeatingType, AptManageType, TimeToSubway_group) %>%
  mutate(across(c(HallwayType, HeatingType, AptManageType, TimeToSubway_group), as.factor))


#Predicción
predicciones <- predict(modelo_lm, newdata = test)

envio <- data.frame(
  Id = test$Id,
  Predicted = round(predicciones, 0)
)

write.table(envio, 
            file = "predicciones_submission.csv", 
            sep = ",", 
            row.names = FALSE, 
            col.names = TRUE, 
            quote = FALSE)

##Leaps

library(leaps)

# Crear matriz de predictores (excluye intercepto)
X <- model.matrix(SalePrice ~ ., data = reg_mod)[, -1]
y <- reg_mod$SalePrice

# Ejecutar selección exhaustiva
seleccion <- regsubsets(x = X, y = y, nvmax = NULL, method = "exhaustive")

# Guardar el resumen
resumen <- summary(seleccion)

# Visualizar R² ajustado por número de variables
plot(seleccion, scale = "adjr2")
which.max(resumen$adjr2)  # Índice del mejor modelo por R² ajustado

# También puedes visualizar por BIC o Cp
plot(seleccion, scale = "bic")
which.min(resumen$bic)

plot(seleccion, scale = "Cp")
which.min(resumen$cp)

# Ver las variables seleccionadas en el mejor modelo
resumen$which[which.max(resumen$adjr2), ]

##modelo lm con variables identificadas por Leaps
modelo_leaps <- lm(SalePrice ~ Size.sqf. + Floor + YearBuilt + N_elevators +
                     N_Parkinglot.Basement. + N_FacilitiesInApt +
                     HallwayTypeterraced + HeatingTypeindividual_heating +
                     AptManageTypeself_management + TimeToSubway_groupmedia,
                   data = reg_matrix_df)  # Este data frame debe tener las dummies creadas
summary(modelo_leaps)
### preparamos para exportar
# Leer test y renombrar Id
test <- read.csv("https://raw.githubusercontent.com/d-correalr/MAE/main/Test%20real%20state.csv") %>%
  rename(Id = X)

# Transformar y conservar Id
test <- test %>%
  mutate(TimeToSubway_group = case_when(
    TimeToSubway == "0~5min" ~ "cerca",
    TimeToSubway %in% c("5min~10min", "10min~15min") ~ "media",
    TRUE ~ "lejos"
  )) %>%
  select(Id, Size.sqf., Floor, YearBuilt, N_elevators,
         N_Parkinglot.Basement., N_FacilitiesInApt,
         HallwayType, HeatingType, AptManageType, TimeToSubway_group) %>%
  mutate(across(c(HallwayType, HeatingType, AptManageType, TimeToSubway_group), as.factor))

# Generar el mismo formato de columnas que en entrenamiento
X_test <- model.matrix(~ ., data = test)[, -1]
X_test_df <- as.data.frame(X_test)

predicciones_leaps <- predict(modelo_leaps, newdata = X_test_df)

envio <- data.frame(
  Id = test$Id,
  Predicted = round(predicciones_leaps, 0)
)

write.table(envio,
            file = "predicciones_submission_leaps.csv",
            sep = ",",
            row.names = FALSE,
            col.names = TRUE,
            quote = FALSE)


########### Tranformación de variables-log

reg_matrix_df$LogPrice <- log(reg_matrix_df$SalePrice)

###nuevo modelo con log

modelo_log <- lm(LogPrice ~ Size.sqf. + Floor + YearBuilt + N_elevators +
                   N_Parkinglot.Basement. + N_FacilitiesInApt +
                   HallwayTypeterraced + HeatingTypeindividual_heating +
                   AptManageTypeself_management + TimeToSubway_groupmedia,
                 data = reg_matrix_df)

summary(modelo_log)


## generamos envio

# Predicción en escala logarítmica
log_preds <- predict(modelo_log, newdata = X_test_df)

# Volver a la escala original del precio
predicciones_finales <- exp(log_preds)

envio <- data.frame(
  Id = test$Id,
  Predicted = round(predicciones_finales, 0)
)

write.table(envio,
            file = "predicciones_submission_log.csv",
            sep = ",",
            row.names = FALSE,
            col.names = TRUE,
            quote = FALSE)

### mejoras al leaps incluyendo interacciones entre tamaño y año de construcción

# Ajustar modelo con mejoras
modelo_final <- lm(SalePrice ~ Size.sqf. + Floor + YearBuilt + N_elevators +
                     N_FacilitiesInApt +
                     HallwayTypeterraced + HeatingTypeindividual_heating +
                     AptManageTypeself_management +
                     Size.sqf.:YearBuilt,  # interacción
                   data = reg_matrix_df)

summary(modelo_final)

##preparar para envío

# Ajustar la misma interacción en test
X_test_df$`Size.sqf.:YearBuilt` <- X_test_df$`Size.sqf.` * X_test_df$YearBuilt

# Predicciones
predicciones_final <- predict(modelo_final, newdata = X_test_df)

# Archivo para submit
envio <- data.frame(
  Id = test$Id,
  Predicted = round(predicciones_final, 0)
)

write.table(envio,
            file = "predicciones_submission_final.csv",
            sep = ",",
            row.names = FALSE,
            col.names = TRUE,
            quote = FALSE)

#### modelo con 4 interacciones

modelo_interacciones <- lm(
  LogPrice ~ Size.sqf. + Floor + YearBuilt + N_elevators +
    N_Parkinglot.Basement. + N_FacilitiesInApt +
    HallwayTypeterraced + HeatingTypeindividual_heating +
    AptManageTypeself_management + TimeToSubway_groupmedia +
    # Interacciones
    Size.sqf. * Floor +
    Size.sqf. * HeatingTypeindividual_heating +
    YearBuilt * Floor +
    AptManageTypeself_management * N_FacilitiesInApt,
  data = reg_matrix_df
)

summary(modelo_interacciones)

#creamos matriz de test con las interacciones
test_ids <- test$Id

# Creamos las variables dummy igual que en el set de entrenamiento
test_matrix_df <- test %>%
  mutate(
    LogPrice = NA,  
    HallwayTypemixed = ifelse(HallwayType == "mixed", 1, 0),
    HallwayTypeterraced = ifelse(HallwayType == "terraced", 1, 0),
    HeatingTypeindividual_heating = ifelse(HeatingType == "individual_heating", 1, 0),
    AptManageTypeself_management = ifelse(AptManageType == "self_management", 1, 0),
    TimeToSubway_groupmedia = ifelse(TimeToSubway %in% c("10min~15min", "15min~20min"), 1, 0)
  )

#agregamos interacciones
test_matrix_df <- test_matrix_df %>%
  mutate(
    # Interacciones seleccionadas
    SizeFloor = Size.sqf. * Floor,
    SizeHeat = Size.sqf. * HeatingTypeindividual_heating,
    YearFloor = YearBuilt * Floor,
    AptManageFac = AptManageTypeself_management * N_FacilitiesInApt
  )

# Predecimos en escala log
pred_log <- predict(modelo_interacciones, newdata = test_matrix_df)

# Convertimos a escala original
pred <- exp(pred_log)

# Asociamos con ID original 
submit <- data.frame(
  Id = test_ids,  
  Predicted = round(pred)  
)

# Guardamos CSV correctamente
write.csv(submit, "submission_interacciones4.csv", row.names = FALSE)

## modelo con solo dos interacciones, tamaño piso y manejo de apartamento

# Crear interacciones en training
reg_matrix_df$SizeFloor <- reg_matrix_df$`Size.sqf.` * reg_matrix_df$Floor
reg_matrix_df$AptManageFac <- reg_matrix_df$AptManageTypeself_management * reg_matrix_df$N_FacilitiesInApt

# Modelo optimizado
modelo_optimo <- lm(SalePrice ~ Size.sqf. + Floor + YearBuilt + N_elevators +
                      N_FacilitiesInApt +
                      HallwayTypeterraced + HeatingTypeindividual_heating +
                      AptManageTypeself_management +
                      SizeFloor + AptManageFac,
                    data = reg_matrix_df)

summary(modelo_optimo)

##preparamos para exportar

test_matrix_df$SizeFloor <- test_matrix_df$`Size.sqf.` * test_matrix_df$Floor
test_matrix_df$AptManageFac <- test_matrix_df$AptManageTypeself_management * test_matrix_df$N_FacilitiesInApt

## realizamos predicciones para probar

pred_optimo <- predict(modelo_optimo, newdata = test_matrix_df)

submit <- data.frame(
  Id = test_ids,
  Predicted = round(pred_optimo)
)

write.csv(submit, "submission_dosinterac.csv", row.names = FALSE)

### Modelo Sep con cooks

modelo_step <- step(modelo_optimo, direction = "both", trace = 0)
summary(modelo_step)

plot(modelo_optimo, which = 4)  # Cook's Distance

influencia <- cooks.distance(modelo_optimo)
reg_clean <- reg_matrix_df[influencia < 0.5, ]
modelo_limpio <- update(modelo_optimo, data = reg_clean)
summary(modelo_limpio)

# Calcular distancia de Cook
influencia <- cooks.distance(modelo_optimo)

# Visualizar (opcional)
plot(influencia, type = "h", main = "Distancia de Cook", ylab = "Cook's D")
abline(h = 0.5, col = "red", lty = 2)

# Eliminar observaciones con Cook's D > 0.5
reg_filtrado <- reg_matrix_df[influencia < 0.5, ]

modelo_filtrado <- lm(SalePrice ~ Size.sqf. + Floor + YearBuilt + N_elevators +
                        N_FacilitiesInApt +
                        HallwayTypeterraced + HeatingTypeindividual_heating +
                        AptManageTypeself_management +
                        SizeFloor + AptManageFac,
                      data = reg_filtrado)

summary(modelo_filtrado)

# Asegúrate de que test_matrix_df tenga las variables de interacción ya creadas

pred_filtrado <- predict(modelo_filtrado, newdata = test_matrix_df)

submit <- data.frame(
  Id = test_ids,
  Predicted = round(pred_filtrado)
)

write.csv(submit, "submission_filtrado.csv", row.names = FALSE)


#### modelo entregado
library(dplyr)
library(Metrics)

# Copias limpias
train <- reg_matrix_df
test <- test_matrix_df

# Aplicar todas las transformaciones
train <- train %>%
  mutate(
    SizeFloor = `Size.sqf.` * Floor,
    AptManageFac = AptManageTypeself_management * N_FacilitiesInApt,
    SizeElev = `Size.sqf.` * N_elevators,
    FloorElev = Floor * N_elevators,
    Size2 = `Size.sqf.`^2,
    Floor_sqrt = sqrt(Floor),
    Age = max(YearBuilt) - YearBuilt,
    FacilitiesLevel = cut(N_FacilitiesInApt, breaks = c(-Inf, 4, 8, Inf), labels = c("bajo", "medio", "alto")),
    HighFloor = ifelse(Floor >= 15, 1, 0),
    SizeFloorYear = `Size.sqf.` * Floor * YearBuilt,
    ElevatorDummy = ifelse(N_elevators > 0, 1, 0),
    HeatSize = HeatingTypeindividual_heating * `Size.sqf.`
  )

test <- test %>%
  mutate(
    SizeFloor = `Size.sqf.` * Floor,
    AptManageFac = AptManageTypeself_management * N_FacilitiesInApt,
    SizeElev = `Size.sqf.` * N_elevators,
    FloorElev = Floor * N_elevators,
    Size2 = `Size.sqf.`^2,
    Floor_sqrt = sqrt(Floor),
    Age = max(train$YearBuilt) - YearBuilt,
    FacilitiesLevel = cut(N_FacilitiesInApt, breaks = c(-Inf, 4, 8, Inf), labels = c("bajo", "medio", "alto")),
    HighFloor = ifelse(Floor >= 15, 1, 0),
    SizeFloorYear = `Size.sqf.` * Floor * YearBuilt,
    ElevatorDummy = ifelse(N_elevators > 0, 1, 0),
    HeatSize = HeatingTypeindividual_heating * `Size.sqf.`
  )

# Lista de fórmulas
formulas <- list(
  SalePrice ~ Size.sqf. + Floor + YearBuilt + N_elevators + N_FacilitiesInApt +
    HallwayTypeterraced + HeatingTypeindividual_heating + AptManageTypeself_management +
    SizeFloor + AptManageFac + SizeElev,
  
  . ~ . + FloorElev,
  . ~ . + Size2,
  . ~ . - Floor + Floor_sqrt,
  . ~ . - YearBuilt + Age,
  SalePrice ~ Size.sqf. + Floor + YearBuilt + N_elevators +
    FacilitiesLevel + HallwayTypeterraced + HeatingTypeindividual_heating +
    AptManageTypeself_management + SizeFloor + AptManageFac,
  
  . ~ . + HighFloor,
  . ~ . + SizeFloorYear,
  . ~ . + ElevatorDummy,
  . ~ . + HeatSize
)

# Ajustar modelos, calcular métricas y exportar predicciones
results <- data.frame(Model = character(), R2 = numeric(), MAE = numeric(), MSE = numeric(), stringsAsFactors = FALSE)

for (i in 1:10) {
  f <- if (i == 1 || i == 6) formulas[[i]] else update(formulas[[1]], formulas[[i]])
  
  modelo <- lm(f, data = train)
  pred_train <- predict(modelo, newdata = train)
  mae_val <- mae(train$SalePrice, pred_train)
  mse_val <- mse(train$SalePrice, pred_train)
  r2_val <- summary(modelo)$adj.r.squared
  
  # Predicción en test
  pred_test <- predict(modelo, newdata = test)
  pred_df <- data.frame(Id = test_ids, Predicted = round(pred_test))
  write.csv(pred_df, sprintf("submission_model%d.csv", i), row.names = FALSE)
  
  # Guardar métricas
  results[i, ] <- c(paste0("Modelo ", i), round(r2_val, 4), round(mae_val, 2), round(mse_val, 2))
}

# Mostrar resumen
print(results)

###################### este es
# Modelo 8: Interacción triple entre Size.sqf., Floor y YearBuilt
modelo_8 <- lm(
  formula = SalePrice ~ Size.sqf. * Floor * YearBuilt +
    N_elevators +
    N_Parkinglot.Basement. +
    N_FacilitiesInApt +
    HallwayTypeterraced +
    HeatingTypeindividual_heating +
    AptManageTypeself_management +
    TimeToSubway_groupmedia,
  data = reg_matrix_df
)

# Resumen del modelo
summary(modelo_8)

#linealidad y homocedasticidad
# Gráfico de residuos vs valores ajustados
plot(modelo_8$fitted.values, modelo_8$residuals,
     xlab = "Valores ajustados",
     ylab = "Residuos",
     main = "Residuos vs Ajustados")
abline(h = 0, col = "red")

## normalidad de los errores
# Q-Q Plot
qqnorm(modelo_8$residuals)
qqline(modelo_8$residuals, col = "red")

# Prueba de Shapiro-Wilk
shapiro.test(modelo_8$residuals)

#multicolinealidad
library(car)
vif(modelo_8)

# independencia de los errores
library(lmtest)
dwtest(modelo_8)

#calculo intervalo de confianza
confint(modelo_8, level = 0.95)

IC_modelo8 <- confint(modelo_8, level = 0.95)
write.csv(IC_modelo8, "intervalos_confianza_modelo8.csv", row.names = TRUE)

IC_modelo8[IC_modelo8[, 1] * IC_modelo8[, 2] > 0, ]


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
