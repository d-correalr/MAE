# ------------------------------
# División del set de entrenamiento (80/20)
# ------------------------------
set.seed(123)  # Reproducibilidad
indice <- sample(1:nrow(train), size = 0.8 * nrow(train))  # 80% entrenamiento

entrenamiento <- train[indice, ]
validacion <- train[-indice, ]

# ------------------------------
# Entrenamiento del modelo 
# ------------------------------
modelo_validacion <- glm(
  Subscription ~ Age + Education + Job + Marital.Status +
    Credit + Housing.Loan + Personal.Loan + Contact +
    Last.Contact.Month + Last.Contact.Day + Last.Contact.Duration +
    Campaign + Pdays + Previous + Poutcome +
    LogBalance + EdadGrupo + ContactadoAntes + DuracionCampaña + DuracionBin +
    Campaign:Previous + DuracionCampaña:Campaign + ContactadoAntes:Pdays,
  data = entrenamiento,
  family = "binomial"
)

# ------------------------------
# Predicción sobre set de validación
# ------------------------------
validacion$Prob <- predict(modelo_validacion, newdata = validacion, type = "response")
validacion$Predicho <- ifelse(validacion$Prob > 0.25, 1, 0)

# ------------------------------
# Matriz de confusión
# ------------------------------
matriz_conf <- table(Predicho = validacion$Predicho, Real = validacion$Subscription)
print(matriz_conf)

# ------------------------------
# Métricas de evaluación
# ------------------------------
VP <- matriz_conf["1", "1"]  # Verdaderos Positivos
VN <- matriz_conf["0", "0"]  # Verdaderos Negativos
FP <- matriz_conf["1", "0"]  # Falsos Positivos
FN <- matriz_conf["0", "1"]  # Falsos Negativos

accuracy <- (VP + VN) / sum(matriz_conf)
sensibilidad <- VP / (VP + FN)
especificidad <- VN / (VN + FP)

cat("\n--- Métricas ---\n")
cat("Accuracy: ", round(accuracy, 4), "\n")
cat("Sensibilidad (Recall): ", round(sensibilidad, 4), "\n")
cat("Especificidad: ", round(especificidad, 4), "\n")


# Asegúrate de tener los paquetes
if (!require(pROC)) install.packages("pROC")
if (!require(PRROC)) install.packages("PRROC")
library(pROC)
library(PRROC)

# 1. Obtener probabilidades del set de entrenamiento (validación interna)
probabilidades <- predict(modelo_7i_final, newdata = train, type = "response") 

# 2. Curva ROC
roc_curve <- roc(train$Subscription, probabilidades)
plot(roc_curve, col = "blue", lwd = 2, main = "Curva ROC - Modelo 7i_final (conjunto de entrenamiento)")
auc_value <- auc(roc_curve)
legend("bottomright", legend = paste("AUC =", round(auc_value, 3)), col = "blue", lwd = 2)

# 3. Curva Precision-Recall
probabilidades <- predict(modelo_7i_final, newdata = train, type = "response")
sum(is.na(probabilidades))  # Cuántos NA hay

# Crea una máscara para filtrar las filas válidas
mascara_valida <- !is.na(probabilidades)

# Aplica la máscara a probabilidades y variable real
prob_validas <- probabilidades[mascara_valida]
subscrip_validas <- train$Subscription[mascara_valida]

fg <- prob_validas[subscrip_validas == 1]
bg <- prob_validas[subscrip_validas == 0]

library(PRROC)
pr_curve <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = TRUE)
plot(pr_curve, main = "Curva Precisión vs. Recall - Modelo 7i_final (conjunto de entrenamiento)")





