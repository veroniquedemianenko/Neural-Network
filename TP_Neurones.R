# CAILLERIE Emilie, DEMIANENKO Véronique

library(caret)
library(ROCR)
library(rpart)
library(rpart.plot)

data<-read.csv("Desktop/data/tp réseau de neurones/Echantillons de données-20231127/HD_Complete_Data.csv", sep = ";", dec = ",")
data$Oldpeak<-as.numeric(data$Oldpeak)

# Question 1

for (i in 4:5) { # moyenne sur les valeurs = 0 pour les colonnes RestingBP, Cholesterol. 
  moy<- mean(colo)
  data[, i][data[, i] == 0] <- moy
}

View(data)

# Analyse des données 

summary(data)

for (i in c(1, 4, 5, 8, 10)) { # Données quantitatives
  col <- colnames(data)[i]
  boxplot(data[[i]], main = paste("Boxplot de", col))
}
data_quant <- data[, c(1, 4, 5, 8, 10)]
data_qual <- data[, c(2, 3, 6, 7, 9, 11, 12)] # Données qualitatives 

barplots_list <- list()

for (col in colnames(data_qual)) {
  freq_table <- table(data_qual[[col]])
  barplot(freq_table, main = paste("Barplot de", col), ylim = c(0, max(freq_table) + 2))
  barplots_list[[col]] <- barplot(freq_table, plot = FALSE)
  
}

colMeans(data_quant,na.rm=TRUE) # Moyenne de chaque colonne
sapply(data_quant,function(col)sd(col,na.rm=TRUE)) # Écart-type de chaque colonne 
sapply(data_quant,function(col)var(col,na.rm=TRUE)) # Variance de chaque colonne 

# Pas de standardisation


# Question 3

set.seed(2023)
index <- sample(1:nrow(data),round(0.80*nrow(data)))
train <- data[index,]
test <- data[-index,]
nrow(train)
nrow(test)

y <- data$HeartDisease

X <- subset(data, select = -HeartDisease)

set.seed(2023)
indices <- createDataPartition(y, p = 0.8, list = FALSE)

X_train <- X[indices, ]
y_train <- y[indices]
y_train <- as.factor(y_train)
X_test <- X[-indices, ]
y_test <- y[-indices]

train_bin <- table(y_train)
test_bin <- table(y_test)
train_bin
test_bin

# On a des proportions à peu près similaires pour l'apprentissage et pour le test (pour nbre(1)/nbre(0) avec respectivement 1.26 et 1.15) pour 1.24 pour l'ensemble des données
# Il faut modifier les valeurs de la colonne 2 et 9 afin qu'elles soient binaires : 

data[, 9][data[, 9] == "Y"] <- 1
data[, 9][data[, 9] == "N"] <- 0

data[, 2][data[, 2] == "F"] <- 1
data[, 2][data[, 2] == "M"] <- 0


###############


# Vu que le dataset est large, créons un ensemble de validation à partir de train : 

y_val <- train$HeartDisease

X_val <- subset(train, select = -HeartDisease)

indices_val <- createDataPartition(y_val, p = 0.8, list = FALSE)


X_trainval <- X_val[indices_val, ]
y_trainval <- y_val[indices_val]
X_testval <- X_val[-indices_val, ]
y_testval <- y_val[-indices_val]

train_binval <- table(y_trainval)
test_binval <- table(y_testval)
#train_binval 
#test_binval

#########


# voir cours pour les hyperparamètres 

#ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE)

# Appliquer la recherche par grille avec la fonction train
#rf_model <- train(X_train, y_train, method = "rf", trControl = ctrl, tuneGrid = grid)

# Afficher les résultats
#print(rf_model)

# Évaluer le modèle sur les données de test
#y_testpred <- predict(rf_model, newdata = X_test)
#conf_matrix <- confusionMatrix(y_testpred, y_test)
#print(conf_matrix)



fulltree <- rpart(HeartDisease ~., data=train, method="class")
rpart.plot(fulltree, main = "Arbre initial")

printcp(fulltree)       
varImp(fulltree) 

# Elagage
cart_fit <- rpart::rpart( HeartDisease ~., data=train)
min_ind <- which.min(cart_fit$cptable[, "xerror"])
min_cp <- cart_fit$cptable[min_ind, "CP"]
pruned_tree <- rpart::prune(cart_fit, cp = min_cp)
rpart.plot(pruned_tree, main = "Arbre élagué")

# Validation croisée

hyper_grid <- expand.grid(cp = seq(0.01, 0.5, by = 0.01))
train_control <- trainControl(method = "cv", number = 5)
model <- train(HeartDisease ~ ., data = train, method = "rpart", trControl = train_control, tuneGrid = hyper_grid)
model
# On utilise cp = 0.01, donc le modèle initial est optimal



Predprob <- predict(fulltree, newdata = test,type = "prob")
Predprob = as.data.frame(Predprob)

Prediction <- prediction(Predprob[2],test$HeartDisease)
performance <- performance(Prediction, "tpr","fpr")

plot(performance,main = "ROC Curve",col = 2,lwd = 2)
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")

aucDT <- performance(Prediction, measure = "auc")
aucDT <- aucDT@y.values[[1]]
aucDT


# Taux d'erreur et matrice de confusion
predicted <- predict(fulltree, test, type="class")

error1 = sum(test$HeartDisease != predicted)/length(predicted) 
error1

predicted <- ordered(predicted)
actual <- ordered(test$HeartDisease)

matrice_confusion = table(predicted,actual, dnn=c("Predicted","reelle")) 
matrice_confusion



error2 = 1-((matrice_confusion[1,1]+matrice_confusion[2,2])/sum(matrice_confusion))
error2

## Question 9 

library(stats)

reg_log <- glm(HeartDisease ~ ., data = train, family = binomial)
summary(reg_log)

pred_log <- predict(reg_log, newdata = test, type = "response")
pred_log <- na.omit(pred_log)

pred_hd <- numeric()
for( i in 1:length(pred_log)){
  if(pred_log[i] >= 0.5){
    pred_hd[[i]] <- 1
  }
  else{
    pred_hd[[i]] <- 0
  }
}

predictions <- cbind(pred_log,pred_hd)
predictions <- as.data.frame(predictions)
View(predictions)

matconf_reglog <- table(predictions$pred_hd, na.omit(test)$HeartDisease)
matconf_reglog








# Problème V

serie <- read.csv("Desktop/data/tp réseau de neurones/Echantillons de données-20231127/Danube ammonium level Time Series.csv", sep = ";", dec =",")
#View(serie)
serie$Months <- as.Date(serie$Months, format="%d/%m/%Y")

mat_serie<-as.matrix(serie)
obs <- mat_serie[, 2]
obs<-as.numeric(obs)
obs_cr <- scale(obs)
obs_min <- min(obs)
obs_max <- max(obs)
obs_norm <- (obs - obs_min) / (obs_max - obs_min)
mat_serie[, 2] <- obs_norm
mat_serie


train_s<-serie[1:252,]
#View(train_s)
test_s<-serie[253:264,]
#View(test_s)

# échantillons rnn train
sequence_train <- embed(train_s$Ammonium, 2)
serie_rnn_train <- data.frame(t = sequence_train[, 2], t_plus_1 = sequence_train[, 1])
serie_rnn_train


# échantillons rnn test

sequence_test <- embed(test_s$Ammonium, 2)
serie_rnn_test <- data.frame(t = sequence_test[, 2], t_plus_1 = sequence_test[, 1])
serie_rnn_test

time_steps <- 917
X_train <- list()
y_train <- list()

for (i in 1:(nrow(train_s) - time_steps + 1)) {
  end_ix <- i + time_steps - 1
  seq_x <- serie_rnn_train$t[i:end_ix]
  seq_y <- serie_rnn_train$t_plus_1[end_ix]
  
  X_train[[i]] <- seq_x
  y_train[[i]] <- seq_y
}

reshaped_train <- list(
  X = array_reshape(do.call(rbind, X_train), c(length(X_train), time_steps, 1)),
  y = unlist(y_train)
)

X_test <- list()
y_test <- list()

for (i in 1:(nrow(test_s) - time_steps + 1)) {
  end_ix <- i + time_steps - 1
  seq_x <- serie_rnn_test$t[i:end_ix]
  seq_y <- serie_rnn_test$t_plus_1[end_ix]
  
  X_test[[i]] <- seq_x
  y_test[[i]] <- seq_y
}

reshaped_test <- list(
  X = array_reshape(do.call(rbind, X_test), c(length(X_test), time_steps, 1)),
  y = unlist(y_test)
)


predictions <- predict(serie_rnn_test, reshaped_test$X)

# Dénormalisation des prédictions et vraies valeurs
pred_denorm <- predictions * (obs_max - obs_min) + obs_min
tval_denorm <- reshaped_test$y * (obs_max - obs_min) + obs_min

# Mesures de performance
rmse <- sqrt(mean((pred_denorm - tval_denorm)^2))
mse <- mean((pred_denorm - tval_denorm)^2)

# Affichage des résultats
cat("RMSE:", rmse, "\n")
cat("MSE:", mse, "\n")

# Graphique de comparaison entre les prédictions et les vraies valeurs
plot(tval_denorm, type = "l", col = "blue", ylim = c(min(tval_denorm, pred_denorm), max(tval_denorm, pred_denorm)))
lines(pred_denorm, col = "red")
legend("topright", legend = c("Vraies valeurs", "Prédictions"), col = c("blue", "red"), lty = 1)

# Étape 7: Comparaison avec un modèle de série chronologique classique

library(forecast)

# Création d'une série temporelle
serie_temporelle <- ts(obs, start = c(1996, 1), end = c(2017, 12), frequency = 12)

# Entraînement d'un modèle ARIMA
modele_arima <- auto.arima(serie_temporelle)

# Prédictions avec le modèle ARIMA
predictions_arima <- forecast(modele_arima, h = 12)

# Dénormalisation des prédictions ARIMA
predictions_arima_denorm <- pred_arima$mean * (obs_max - obs_min) + obs_min

# Mesures de performance pour ARIMA
rmse_arima <- sqrt(mean((pred_arima_denorm - tval_denorm)^2))
mse_arima <- mean((pred_arima_denorm - tval_denorm)^2)

# Affichage des résultats ARIMA
cat("\nPerformance du modèle ARIMA:\n")
cat("RMSE ARIMA:", rmse_arima, "\n")
cat("MSE ARIMA:", mse_arima, "\n")



