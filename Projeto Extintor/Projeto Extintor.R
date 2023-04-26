setwd("D:/Envios git/Projetos R/Projeto Extintor")
getwd()

#dataset https://www.muratkoklu.com/datasets/vtdhnd07.php

library(readxl)
library(ggplot2)
library(randomForest)
library(caret)
library(caTools)
library(class)
library(gmodels)
library(ROCR)
library(e1071)
library(neuralnet)


# Carrega o dataset
dados <- read_excel("Acoustic_Extinguisher_Fire_Dataset.xlsx")
View(dados)


#Verificar existencia de valores NA
sum(is.na(dados))

#visualizar informações dos dados
str(dados)
summary(dados)

#transformar colunas char em fator
#dados[sapply(dados, is.character)] <- lapply(dados[sapply(dados, is.character)], as.factor)
dados$FUEL <- as.factor(dados$FUEL)



#visualizar proporção das classes
table(dados$STATUS)

#boxplot por variável
nomes <- names(dados)
lapply(nomes, function(x){
  if(is.numeric(unlist(dados[x]))){
    boxplot(dados[x],main = x)}})



#remover variavel nomes
rm(nomes)

# Modelo randomForest para criar um plot de importância das variáveis

modelo <- randomForest( STATUS ~ .,
                        data = dados, 
                        ntree = 100, nodesize = 10, importance = T)

varImpPlot(modelo)

#remover variavel modelo
rm(modelo)

#transformar classe em fator e Fuel em numerico
dados$STATUS <- as.factor(dados$STATUS)
dados$FUEL <- as.numeric(dados$FUEL)

#gasoline=1, kerosene=2, lpg=3 , thinner =4

#Split de dados
amostra <- sample.split(dados$STATUS, SplitRatio = 0.80)

# Criando dados de treino - 80% dos dados
treino = subset(dados, amostra == TRUE)

# Criando dados de teste - 20% dos dados
teste = subset(dados, amostra == FALSE)

#remover variavel amostra
rm(amostra)

#criar um datasetcom os dados normalizados

#Criar um dataset normalizado
dados_normalizados <- dados






# Criando um função de normalização
normalizar <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}



# Normalizando os dados
dados_normalizados <- as.data.frame(lapply(dados_normalizados[-7], normalizar))
View(dados_normalizados)
dados_normalizados$STATUS <- dados$STATUS

#remover função da memória
rm(normalizar)




#split dos dados normalizados

amostra <- sample.split(dados_normalizados$STATUS, SplitRatio = 0.80)

# Criando dados de treino - 80% dos dados
treino_norm = subset(dados_normalizados, amostra == TRUE)

# Criando dados de teste - 20% dos dados
teste_norm = subset(dados_normalizados, amostra == FALSE)

#remover variavel amostra
rm(amostra)

#------------------------------------
#Criação do primeiro modelo com KNN
modelo_knn <- knn(train = treino_norm[,-7],
                  test= teste_norm[,-7],cl= treino_norm$STATUS,k=3)


modelo_knn

#matriz de confusão
confusionMatrix(modelo_knn,teste_norm$STATUS)
?confusionMatrix
CrossTable(x = teste_norm$STATUS, y = modelo_knn, prop.chisq = FALSE)

# Gerando a curva ROC
pred <- prediction(as.numeric(modelo_knn), as.numeric(teste_norm$STATUS))
perf <- performance(pred, "tpr","fpr") 
plot(perf, col = rainbow(10))

#removendo as variáveis
rm(pred, perf)

#Acurácia de 0,96 e p-value de 2e-16 
#percentual de acertos
mean(teste_norm$STATUS ==modelo_knn)*100

#----------------------------------------------------------
#Criação do segundo modelo com KNN e dados sem normalização
#----------------------------------------------------------

modelo_svm <- svm(STATUS ~ ., 
                     data = treino, 
                     type = 'C-classification', 
                     kernel = 'radial') 
modelo_svm
# Previsões nos dados de teste
previsto <- predict(modelo_svm, teste)


#matriz de confusão
confusionMatrix(previsto,teste$STATUS)
CrossTable(x = teste$STATUS, y = previsto, prop.chisq = FALSE)

# Gerando a curva ROC
pred <- prediction(as.numeric(previsto), as.numeric(teste$STATUS))
perf <- performance(pred, "tpr","fpr") 
plot(perf, col = rainbow(10))

#removendo as variáveis
rm(pred, perf)

#Acurácia de 0,93 e p-value de 2e-16 
#percentual de acertos
mean(teste$STATUS ==previsto)*100


#----------------------------------------------------------
#Criação do segundo modelo com KNN e dados normalizados
#----------------------------------------------------------
modelo_svm2 <- svm(STATUS ~ ., 
                  data = treino_norm, 
                  type = 'C-classification', 
                  kernel = 'radial') 
modelo_svm2
# Previsões nos dados de teste
previsto_svm2 <- predict(modelo_svm2, teste_norm)


#matriz de confusão
confusionMatrix(previsto_svm2,teste_norm$STATUS)
CrossTable(x = teste_norm$STATUS, y = previsto_svm2, prop.chisq = FALSE)

# Gerando a curva ROC
pred <- prediction(as.numeric(previsto_svm2), as.numeric(teste_norm$STATUS))
perf <- performance(pred, "tpr","fpr") 
plot(perf, col = rainbow(10))

#removendo as variáveis
rm(pred, perf)

#Acurácia de 0,93 e p-value de 2e-16 
#percentual de acertos
mean(teste_norm$STATUS ==previsto_svm2)*100

#----------------------------------------------------------
#Criação do terceiro modelo com floresta alatória
#----------------------------------------------------------
modelo_rf <- randomForest( STATUS ~ .,
                        data = dados, 
                        ntree = 100, nodesize = 10)

modelo_rf
previsto_rf <- predict(modelo_rf, teste)
#matriz de confusão
confusionMatrix(previsto_rf,teste$STATUS)
CrossTable(x = teste$STATUS, y = previsto_rf, prop.chisq = FALSE)

# Gerando a curva ROC
pred <- prediction(as.numeric(previsto_rf), as.numeric(teste$STATUS))
perf <- performance(pred, "tpr","fpr") 
plot(perf, col = rainbow(10))

#removendo as variáveis
rm(pred, perf)

#Acurácia de 0,97 e p-value de 2e-16 
#percentual de acertos
mean(teste$STATUS ==previsto_rf)*100

#O melhor modelo foi o de arvores aleatorias


#remove tudo da memoria
rm(dados,dados_normalizados, modelo_knn, modelo_svm,modelo_svm2,modelo_rf,
   treino,treino_norm, teste, teste_norm, previsto,previsto_rf, previsto_svm2)


