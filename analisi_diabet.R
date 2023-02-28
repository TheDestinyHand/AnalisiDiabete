library(RWeka)
#caricamento dataset
data = read.arff(file.choose())

##-- Analisi preliminari --##
#dimensione del dataset
dim(data)
#visualizziamo le prime 5 righe
head(data, 5)
#procediamo con analisi descrittive
summary(data)

##--Analisi univariata --##
#istogrammi
par(mfrow = c(3,3))
for(i in 1:8){
  hist(data[,i], main = names(data)[i])
}
#boxplot
for(i in 1:8){
  boxplot(data[,i], main = names(data)[i])
}
par(mfrow = c(1,1))

##--Analisi bivariata--##
#scatterplot
pairs(data[,1:8])
#barplot per la risposta
table(data[,9])
barplot(table(data[,9]))
#Matrice di correlazione
cor(data[,1:8])
#Heatmap della matrice di correlazione
library(ggplot2)
ggplot(data = reshape2::melt(cor(data[,1:8])),
       aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  labs(x = "", y = "")

##--Modelling--##
#Creazione del training set e del test set
set.seed(123)
indici <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
training_set <- data[indici == 1, ]
test_set <- data[indici == 2, ]
#Creazione del modello di regressione logistica
modello <- glm(class ~ ., data = training_set, family = binomial)
#Visualizzazione dei coefficienti del modello
summary(modello)
#Previsione del test set
probabilita <- predict(modello, type = "response", newdata = test_set[-9])
previsioni <- ifelse(probabilita > 0.5, 1, 0)
#Valutazione delle prestazioni del modello
library(caret)
confusionMatrix(table(previsioni, test_set$class))


#Creazione del modello
library(rpart)
modello <- rpart(class ~ ., data = training_set, method = "class")
#Previsione del test set
previsioni <- predict(modello, newdata = test_set[-9], type = "class")
#Valutazione delle prestazioni del modello
confusionMatrix(table(previsioni, test_set$class))



#Creazione del modello
library(randomForest)
modello <- randomForest(class ~ ., data = training_set)
#Previsione del test set
previsioni <- predict(modello, newdata = test_set[-9])
#Valutazione delle prestazioni del modello
confusionMatrix(table(previsioni, test_set$class))



#Creazione del modello
library(randomForest)
modello <- randomForest(class ~ ., data = training_set)
#Previsione del test set
previsioni <- predict(modello, newdata = test_set[-9])
#Valutazione delle prestazioni del modello
confusionMatrix(table(previsioni, test_set$class))