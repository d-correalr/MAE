#llamamos librerias
library(dplyr)
library(car)
library(explore)
library(psych)
library(corrplot)
library(ggplot2)
list.files()
library(tidyverse)
library(caret)
library(MASS)
library(pROC)
library(car)
library(glmnet)
library(MuMIn)


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
train <- read.csv("https://raw.githubusercontent.com/d-correalr/MAE/main/Train%20bank.csv")
test <- read.csv("https://raw.githubusercontent.com/d-correalr/MAE/main/Test%20bank.csv")

colnames(train)
dim(train)
colnames(train)
head(train)
str(train)
describe(train)

table(train$Subscription)
barplot(table(train$Subscription),
        main = "Distribución de la variable Subscription",
        names.arg = c("No", "Sí"),
        col = c("tomato", "skyblue"),
        ylab = "Frecuencia")

boxplot(train[, c("Age")],
        main = "Caracteristicas personas",
        col = "grey",
        las = 2,
        notch = TRUE)
## preparación

target <- "Subscription"

# Preparación
train$Subscription <- as.factor(train$Subscription)
train$ID <- train$X
test$ID <- test$X

train$X <- NULL
test$X <- NULL



####################

# Modelo 1: Regresión logística con todas las variables
modelo1 <- glm(Subscription ~ ., data = train, family = "binomial")
pred1 <- predict(modelo1, newdata = test, type = "response")
envio1 <- data.frame(Id = test$ID, Predicted = round(pred1, 0))
write.table(envio1, file = "submission_model1_logistic.csv", sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
####################

# Modelo 2: GLM con solo variables numéricas
modelo2 <- glm(Subscription ~ Age + Balance..euros. + Last.Contact.Day +
                 Last.Contact.Duration + Campaign + Pdays + Previous,
               data = train, family = "binomial")
pred2 <- predict(modelo2, newdata = test, type = "response")
envio2 <- data.frame(Id = test$ID, Predicted = round(pred2, 0))
write.table(envio2, file = "submission_model2_numeric.csv", sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
####################

# Modelo 3: GLM con solo variables categóricas
modelo3 <- glm(Subscription ~ Job + Marital.Status + Education + Credit +
                 Housing.Loan + Personal.Loan + Contact + Last.Contact.Month + Poutcome,
               data = train, family = "binomial")
pred3 <- predict(modelo3, newdata = test, type = "response")
envio3 <- data.frame(Id = test$ID, Predicted = round(pred3, 0))
write.table(envio3, file = "submission_model3_categoricas.csv", sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
####################

# Modelo 4: GLM con 3 variables más importantes (ejemplo)
modelo4 <- glm(Subscription ~ Contact + Poutcome + Last.Contact.Duration,
               data = train, family = "binomial")
pred4 <- predict(modelo4, newdata = test, type = "response")
envio4 <- data.frame(Id = test$ID, Predicted = round(pred4, 0))
write.table(envio4, file = "submission_model4_top3.csv", sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
####################

# Modelo 5: GLM con variables manualmente elegidas
modelo5 <- glm(Subscription ~ Age + Education + Contact + Campaign + Poutcome,
               data = train, family = "binomial")
pred5 <- predict(modelo5, newdata = test, type = "response")
envio5 <- data.frame(Id = test$ID, Predicted = round(pred5, 0))
write.table(envio5, file = "submission_model5_mixto.csv", sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
####################

# Modelo 6: LDA con todas las variables
library(MASS)
modelo6 <- lda(Subscription ~ ., data = train)
pred6 <- predict(modelo6, newdata = test)$posterior[,2]
envio6 <- data.frame(Id = test$ID, Predicted = round(pred6, 0))
write.table(envio6, file = "submission_model6_lda.csv", sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
####################

# Modelo 7: LDA con variables seleccionadas
modelo7 <- lda(Subscription ~ Age + Contact + Last.Contact.Duration + Poutcome,
               data = train)
pred7 <- predict(modelo7, newdata = test)$posterior[,2]
envio7 <- data.frame(Id = test$ID, Predicted = round(pred7, 0))
write.table(envio7, file = "submission_model7_lda_selec.csv", sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
####################

# Modelo 8: Regresión logística con Contact + Pdays + Job
modelo8 <- glm(Subscription ~ Contact + Pdays + Job, data = train, family = "binomial")
pred8 <- predict(modelo8, newdata = test, type = "response")
envio8 <- data.frame(Id = test$ID, Predicted = round(pred8, 0))
write.table(envio8, file = "submission_model8_simple.csv", sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
####################

# Modelo 9: GLM con variables de marketing (Campaign, Contact, Previous, Poutcome)
modelo9 <- glm(Subscription ~ Campaign + Contact + Previous + Poutcome,
               data = train, family = "binomial")
pred9 <- predict(modelo9, newdata = test, type = "response")
envio9 <- data.frame(Id = test$ID, Predicted = round(pred9, 0))
write.table(envio9, file = "submission_model9_marketing.csv", sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
####################

# Modelo 10: LDA con solo Contact y Poutcome
modelo10 <- lda(Subscription ~ Contact + Poutcome, data = train)
pred10 <- predict(modelo10, newdata = test)$posterior[,2]
envio10 <- data.frame(Id = test$ID, Predicted = round(pred10, 0))
write.table(envio10, file = "submission_model10_lda_contact.csv", sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)


############ probamos modelos con base en el 7
#7b
modelo7b <- lda(Subscription ~ Age + Contact + Last.Contact.Duration + Poutcome + Education + Campaign + Previous, data = train)
pred7b <- predict(modelo7b, newdata = test)$posterior[,2]
envio7b <- data.frame(Id = test$ID, Predicted = round(pred7b, 0))
write.table(envio7b, file = "submission_model7b_lda_plus_vars.csv", sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)

#7c

train$LogDuration <- log1p(train$Last.Contact.Duration)
test$LogDuration <- log1p(test$Last.Contact.Duration)

modelo7c <- lda(Subscription ~ Age + Contact + LogDuration + Poutcome, data = train)
pred7c <- predict(modelo7c, newdata = test)$posterior[,2]
envio7c <- data.frame(Id = test$ID, Predicted = round(pred7c, 0))
write.table(envio7c, file = "submission_model7c_lda_log.csv", sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)

#7d

# Verificar distribución
table(train$Job)

# Reemplazar trabajos con baja frecuencia
rare_jobs <- names(which(table(train$Job) < 300))
train$JobClean <- ifelse(train$Job %in% rare_jobs, "other", train$Job)
test$JobClean <- ifelse(test$Job %in% rare_jobs, "other", test$Job)

train$JobClean <- as.factor(train$JobClean)
test$JobClean <- as.factor(test$JobClean)

modelo7d <- lda(Subscription ~ Age + Contact + Last.Contact.Duration + Poutcome + JobClean, data = train)
pred7d <- predict(modelo7d, newdata = test)$posterior[,2]
envio7d <- data.frame(Id = test$ID, Predicted = round(pred7d, 0))
write.table(envio7d, file = "submission_model7d_lda_jobclean.csv", sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)

#7e

library(MASS)
common_vars <- c("Age", "Contact", "Last.Contact.Duration", "Poutcome", "Education", "Campaign", "Previous")
formula_step <- as.formula(paste("Subscription ~", paste(common_vars, collapse = "+")))
modelo7e <- step(glm(formula_step, data = train, family = "binomial"), trace = FALSE)
pred7e <- predict(modelo7e, newdata = test, type = "response")
envio7e <- data.frame(Id = test$ID, Predicted = round(pred7e, 0))
write.table(envio7e, file = "submission_model7e_log_step.csv", sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)


library(dplyr)

# Aseguramos factor para target
train$Subscription <- as.factor(train$Subscription)

cat("\n========================\nNUMÉRICAS\n========================\n")

# 1. Estadísticas de variables numéricas por clase
numericas <- c("Age", "Balance..euros.", "Last.Contact.Day", "Last.Contact.Duration", "Campaign", "Pdays", "Previous")

for (var in numericas) {
  cat("\n---", var, "---\n")
  print(
    train %>%
      group_by(Subscription) %>%
      summarise(
        Mean = mean(.data[[var]], na.rm = TRUE),
        Median = median(.data[[var]], na.rm = TRUE),
        SD = sd(.data[[var]], na.rm = TRUE),
        Q1 = quantile(.data[[var]], 0.25, na.rm = TRUE),
        Q3 = quantile(.data[[var]], 0.75, na.rm = TRUE),
        .groups = "drop"
      )
  )
}

cat("\n========================\nCATEGÓRICAS\n========================\n")

# 2. Proporción de clase 1 por nivel de variables categóricas
categoricas <- c("Job", "Marital.Status", "Education", "Credit", "Housing.Loan", "Personal.Loan", "Contact", "Last.Contact.Month", "Poutcome")

for (var in categoricas) {
  cat("\n---", var, "---\n")
  print(
    train %>%
      group_by(.data[[var]]) %>%
      summarise(
        n = n(),
        suscripciones = sum(Subscription == 1),
        total = n(),
        proporción = round(mean(Subscription == 1), 3),
        .groups = "drop"
      ) %>%
      arrange(desc(proporción))
  )
}

############## 7C revamp

library(MASS)

# Transformaciones
train$LogDuration <- log1p(train$Last.Contact.Duration)
test$LogDuration <- log1p(test$Last.Contact.Duration)

train$LogBalance <- log1p(train$Balance..euros. + abs(min(train$Balance..euros.)) + 1)
test$LogBalance <- log1p(test$Balance..euros. + abs(min(train$Balance..euros.)) + 1)

train$LowCampaign <- ifelse(train$Campaign <= 2, 1, 0)
test$LowCampaign <- ifelse(test$Campaign <= 2, 1, 0)

# Dummies de variables categóricas importantes
train$SuccessContact <- ifelse(train$Poutcome == "success", 1, 0)
test$SuccessContact <- ifelse(test$Poutcome == "success", 1, 0)

train$Cellular <- ifelse(train$Contact == "cellular", 1, 0)
test$Cellular <- ifelse(test$Contact == "cellular", 1, 0)

train$Month_HighConv <- ifelse(train$Last.Contact.Month %in% c("mar", "sep", "dec"), 1, 0)
test$Month_HighConv <- ifelse(test$Last.Contact.Month %in% c("mar", "sep", "dec"), 1, 0)

train$IsStudent <- ifelse(train$Job == "student", 1, 0)
test$IsStudent <- ifelse(test$Job == "student", 1, 0)

# Modelo LDA refinado
modelo7c <- lda(Subscription ~ LogDuration + LogBalance + LowCampaign + SuccessContact + 
                  Cellular + Month_HighConv + IsStudent, data = train)

# Predicción
pred7c <- predict(modelo7c, newdata = test)$posterior[,2]
envio7c <- data.frame(Id = test$ID, Predicted = round(pred7c, 0))

# Exportar
write.table(envio7c, file = "submission_model7c_lda_refinado.csv", sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)

########### comparamos modelos

library(MASS)
library(pROC)

# Modelo 7b
modelo7b <- lda(Subscription ~ Age + Contact + Last.Contact.Duration + Poutcome + Education + Campaign + Previous,
                data = train)
prob7b <- predict(modelo7b)$posterior[,2]
pred7b <- ifelse(prob7b > 0.5, 1, 0)

# Modelo 7c
modelo7c <- lda(Subscription ~ LogDuration + LogBalance + LowCampaign + SuccessContact +
                  Cellular + Month_HighConv + IsStudent,
                data = train)
prob7c <- predict(modelo7c)$posterior[,2]
pred7c <- ifelse(prob7c > 0.5, 1, 0)

# Función de evaluación sin caret
evaluar <- function(y_true, prob, pred, nombre) {
  auc <- roc(y_true, prob)$auc
  
  TP <- sum(y_true == 1 & pred == 1)
  TN <- sum(y_true == 0 & pred == 0)
  FP <- sum(y_true == 0 & pred == 1)
  FN <- sum(y_true == 1 & pred == 0)
  
  accuracy <- (TP + TN) / length(y_true)
  precision <- ifelse((TP + FP) == 0, 0, TP / (TP + FP))
  recall <- ifelse((TP + FN) == 0, 0, TP / (TP + FN))
  f1 <- ifelse((precision + recall) == 0, 0, 2 * precision * recall / (precision + recall))
  specificity <- ifelse((TN + FP) == 0, 0, TN / (TN + FP))
  
  data.frame(Modelo = nombre,
             AUC = round(auc, 4),
             Accuracy = round(accuracy, 4),
             F1 = round(f1, 4),
             Sensibilidad = round(recall, 4),
             Especificidad = round(specificity, 4))
}

y_true <- as.numeric(as.character(train$Subscription))

res7b <- evaluar(y_true, prob7b, pred7b, "Modelo 7b")
res7c <- evaluar(y_true, prob7c, pred7c, "Modelo 7c Mejorado")

resultados <- rbind(res7b, res7c)
print(resultados)


###7d
library(MASS)

# ========================
# Variables transformadas
# ========================
train$LogDuration <- log1p(train$Last.Contact.Duration)
test$LogDuration <- log1p(test$Last.Contact.Duration)

# Dummy: ¿hubo éxito en contacto anterior?
train$SuccessContact <- ifelse(train$Poutcome == "success", 1, 0)
test$SuccessContact <- ifelse(test$Poutcome == "success", 1, 0)

# Dummy: ¿contactado por celular?
train$Cellular <- ifelse(train$Contact == "cellular", 1, 0)
test$Cellular <- ifelse(test$Contact == "cellular", 1, 0)

# ========================
# Modelo 7d
# ========================
modelo7d <- lda(Subscription ~ Age + Contact + LogDuration + Poutcome + Education +
                  Campaign + Previous + SuccessContact + Cellular,
                data = train)

# ========================
# Predicción
# ========================
prob7d <- predict(modelo7d, newdata = test)$posterior[,2]
envio7d <- data.frame(Id = test$ID, Predicted = round(prob7d, 0))
write.table(envio7d, file = "submission_model7d_lda_combinado.csv", sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)


# En entrenamiento
prob7d_train <- predict(modelo7d, newdata = train)$posterior[,2]
pred7d_train <- ifelse(prob7d_train > 0.5, 1, 0)
res7d <- evaluar(y_true, prob7d_train, pred7d_train, "Modelo 7d Combinado")

# Comparar con anteriores
resultados <- rbind(res7b, res7c, res7d)
print(resultados)

###### 7b sigue siendo el mejor

library(pROC)

# Ya deberías tener esto cargado:
# modelo7b, prob7b, train$Subscription como factor

# Vector real
y_true <- as.numeric(as.character(train$Subscription))

# Función de evaluación
evaluar_umbral <- function(prob, y_true, threshold) {
  pred <- ifelse(prob > threshold, 1, 0)
  
  TP <- sum(y_true == 1 & pred == 1)
  TN <- sum(y_true == 0 & pred == 0)
  FP <- sum(y_true == 0 & pred == 1)
  FN <- sum(y_true == 1 & pred == 0)
  
  accuracy <- (TP + TN) / length(y_true)
  precision <- ifelse((TP + FP) == 0, 0, TP / (TP + FP))
  recall <- ifelse((TP + FN) == 0, 0, TP / (TP + FN))
  f1 <- ifelse((precision + recall) == 0, 0, 2 * precision * recall / (precision + recall))
  specificity <- ifelse((TN + FP) == 0, 0, TN / (TN + FP))
  auc <- roc(y_true, prob)$auc
  
  data.frame(Umbral = threshold,
             AUC = round(as.numeric(auc), 4),
             Accuracy = round(accuracy, 4),
             F1 = round(f1, 4),
             Sensibilidad = round(diasrecall, 4),
             Especificidad = round(specificity, 4))
}

# Evaluar múltiples umbrales
umbrales <- seq(0.5, 0.3, by = -0.05)

resultados_umbral <- do.call(rbind, lapply(umbrales, function(u) evaluar_umbral(prob7b, y_true, u)))
print(resultados_umbral)

###ajustamos umbrales

# Predicción en test
prob_test_7b <- predict(modelo7b, newdata = test)$posterior[,2]
pred_test_7b <- ifelse(prob_test_7b > 0.30, 1, 0)

# Generar archivo de envío
envio_7b_umbral <- data.frame(Id = test$ID, Predicted = pred_test_7b)
write.table(envio_7b_umbral, file = "submission_model7b_umbral.csv", sep = ",",
            row.names = FALSE, col.names = TRUE, quote = FALSE)



set.seed(123)

library(MASS)
library(pROC)

# Crear pliegues estratificados
k <- 10
folds <- cut(seq(1, nrow(train)), breaks = k, labels = FALSE)
folds <- sample(folds)  # aleatorizar

# Guardar resultados por fold
metricas_cv <- data.frame()

for (i in 1:k) {
  cat("Fold", i, "\n")
  
  idx_valid <- which(folds == i)
  train_fold <- train[-idx_valid, ]
  valid_fold <- train[idx_valid, ]
  
  # Entrenar LDA
  modelo_fold <- lda(Subscription ~ Age + Contact + Last.Contact.Duration + Poutcome +
                       Education + Campaign + Previous,
                     data = train_fold)
  
  # Predecir en validación
  prob_valid <- predict(modelo_fold, newdata = valid_fold)$posterior[,2]
  pred_valid <- ifelse(prob_valid > 0.30, 1, 0)
  y_true <- as.numeric(as.character(valid_fold$Subscription))
  
  # Calcular métricas
  TP <- sum(y_true == 1 & pred_valid == 1)
  TN <- sum(y_true == 0 & pred_valid == 0)
  FP <- sum(y_true == 0 & pred_valid == 1)
  FN <- sum(y_true == 1 & pred_valid == 0)
  
  accuracy <- (TP + TN) / length(y_true)
  precision <- ifelse((TP + FP) == 0, 0, TP / (TP + FP))
  recall <- ifelse((TP + FN) == 0, 0, TP / (TP + FN))
  f1 <- ifelse((precision + recall) == 0, 0, 2 * precision * recall / (precision + recall))
  specificity <- ifelse((TN + FP) == 0, 0, TN / (TN + FP))
  auc <- roc(y_true, prob_valid)$auc
  
  metricas_cv <- rbind(metricas_cv, data.frame(Fold = i,
                                               AUC = round(auc, 4),
                                               Accuracy = round(accuracy, 4),
                                               F1 = round(f1, 4),
                                               Sensibilidad = round(recall, 4),
                                               Especificidad = round(specificity, 4)))
}

# Resultado final
cat("\n==== Promedios de Validación Cruzada ====\n")
print(round(colMeans(metricas_cv[,-1]), 4))



# ==== Generar Submission final con umbral 0.30 ====

# Predicción probabilística con modelo final
prob_7b <- predict(modelo_7b, newdata = test_final, type = "response")

# Aplicar umbral 0.30
pred_binarias_7b <- ifelse(prob_7b >= 0.30, 1, 0)

# Crear dataframe para envío
submission <- data.frame(
  Id = test$ID,
  Predicted = pred_binarias_7b
)

# Guardar CSV
write.table(
  submission,
  file = "submission_7b_umbral_030.csv",
  sep = ",",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE
)


# ================================
# Modelo 7b: Regresión logística con interacciones
# ================================
modelo_7b <- glm(
  Subscription ~ Age + Education + Job + Marital.Status + 
    Credit + Housing.Loan + Personal.Loan + Contact + 
    Last.Contact.Month + Last.Contact.Day + Last.Contact.Duration + 
    Campaign + Pdays + Previous + Poutcome +
    Education:Contact + Job:Poutcome + Age:Last.Contact.Duration + 
    Credit:Personal.Loan + Education:Campaign,
  data = train,
  family = "binomial"
)

# Asegurarse de que test tenga solo las variables requeridas
vars_7b <- all.vars(formula(modelo_7b))
vars_7b <- setdiff(vars_7b, "Subscription")
test_final <- test[, vars_7b, drop = FALSE]

# ==== Predicción y Submission ====
prob_7b <- predict(modelo_7b, newdata = test_final, type = "response")
pred_binarias_7b <- ifelse(prob_7b >= 0.30, 1, 0)

submission <- data.frame(
  Id = test$ID,
  Predicted = pred_binarias_7b
)

write.table(
  submission,
  file = "submission_7b_umbral_030.csv",
  sep = ",",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE
)


###################va ganando
# ================================
# Modelo 7e: GLM Mejorado con nuevas variables e interacciones
# ================================

# ---- Crear variables derivadas en train ----
train$EdadGrupo <- cut(train$Age, breaks = c(0, 30, 40, 50, 60, 100), labels = FALSE, include.lowest = TRUE)
train$ContactadoAntes <- ifelse(train$Pdays == -1, 0, 1)
train$DuracionCampaña <- train$Last.Contact.Duration / (train$Campaign + 1)

# ---- Asegurar que estén en test también ----
test$EdadGrupo <- cut(test$Age, breaks = c(0, 30, 40, 50, 60, 100), labels = FALSE, include.lowest = TRUE)
test$ContactadoAntes <- ifelse(test$Pdays == -1, 0, 1)
test$DuracionCampaña <- test$Last.Contact.Duration / (test$Campaign + 1)

# ---- Entrenar el modelo ----
modelo_7e <- glm(
  Subscription ~ Age + Education + Job + Marital.Status +
    Credit + Housing.Loan + Personal.Loan + Contact +
    Last.Contact.Month + Last.Contact.Day + Last.Contact.Duration +
    Campaign + Pdays + Previous + Poutcome +
    EdadGrupo + ContactadoAntes + DuracionCampaña +
    Education:Job + Poutcome:Campaign + Last.Contact.Month:Contact,
  data = train,
  family = "binomial"
)

# ---- Predicción ----
vars_7e <- all.vars(formula(modelo_7e))
vars_7e <- setdiff(vars_7e, "Subscription")
test_final <- test[, vars_7e, drop = FALSE]

prob_7e <- predict(modelo_7e, newdata = test_final, type = "response")
pred_binarias_7e <- ifelse(prob_7e >= 0.25, 1, 0)

# ---- Submission ----
submission <- data.frame(
  Id = test$ID,
  Predicted = pred_binarias_7e
)

write.table(
  submission,
  file = "submission_7e_umbral_025.csv",
  sep = ",",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE
)

num_cols <- sapply(train, is.numeric)
colSums(!is.finite(train[, num_cols]))


############################################# 
# ================================
# Modelo 7f: GLM con limpieza, transformaciones e interacciones
# ================================

# ---- Limpiar y transformar Balance ----
train$Balance.Limpio <- ifelse(is.na(train$Balance..euros.) | train$Balance..euros. < -1, 0, train$Balance..euros.)
test$Balance.Limpio  <- ifelse(is.na(test$Balance..euros.)  | test$Balance..euros. < -1, 0, test$Balance..euros.)

train$LogBalance <- log1p(train$Balance.Limpio)
test$LogBalance  <- log1p(test$Balance.Limpio)

# ---- Otras variables derivadas ----
train$EdadGrupo <- cut(train$Age, breaks = c(0, 30, 40, 50, 60, 100), labels = FALSE, include.lowest = TRUE)
test$EdadGrupo  <- cut(test$Age, breaks = c(0, 30, 40, 50, 60, 100), labels = FALSE, include.lowest = TRUE)

train$ContactadoAntes <- ifelse(train$Pdays == -1, 0, 1)
test$ContactadoAntes  <- ifelse(test$Pdays == -1, 0, 1)

train$DuracionCampaña <- train$Last.Contact.Duration / (train$Campaign + 1)
test$DuracionCampaña  <- test$Last.Contact.Duration / (test$Campaign + 1)

train$DuracionBin <- cut(train$Last.Contact.Duration, breaks = c(0, 100, 300, 600, 1000, Inf), labels = FALSE)
test$DuracionBin  <- cut(test$Last.Contact.Duration, breaks = c(0, 100, 300, 600, 1000, Inf), labels = FALSE)

# ---- Agrupamiento de profesiones poco frecuentes ----
rare_jobs <- names(which(table(train$Job) < 300))
train$JobSimplified <- ifelse(train$Job %in% rare_jobs, "other", train$Job)
test$JobSimplified  <- ifelse(test$Job %in% rare_jobs, "other", test$Job)
train$JobSimplified <- factor(train$JobSimplified)
test$JobSimplified  <- factor(test$JobSimplified)

# ---- Eliminar filas con NA restantes en train ----
train <- na.omit(train)

# ---- Entrenar modelo ----
modelo_7f <- glm(
  Subscription ~ Age + Education + JobSimplified + Marital.Status +
    Credit + Housing.Loan + Personal.Loan + Contact +
    Last.Contact.Month + Last.Contact.Day + Last.Contact.Duration +
    Campaign + Pdays + Previous + Poutcome +
    LogBalance + EdadGrupo + ContactadoAntes + DuracionCampaña + DuracionBin +
    Campaign:Previous + DuracionCampaña:Campaign + ContactadoAntes:Pdays,
  data = train,
  family = "binomial"
)

# ---- Predicción ----
vars_7f <- all.vars(formula(modelo_7f))
vars_7f <- setdiff(vars_7f, "Subscription")
test_final <- test[, vars_7f, drop = FALSE]

prob_7f <- predict(modelo_7f, newdata = test_final, type = "response")
pred_binarias_7f <- ifelse(prob_7f >= 0.22, 1, 0)

# ---- Submission ----
submission <- data.frame(
  Id = test$ID,
  Predicted = pred_binarias_7f
)

write.table(
  submission,
  file = "submission_7f_umbral_022.csv",
  sep = ",",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE
)

