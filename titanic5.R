# --------------------------------------------
# --------------------------------------------

library("mlr")
library("stringr")

# --------------------------------------------
# --------------------------------------------

train = read.csv("data/train.csv")
train$Survived = as.factor(train$Survived)
test = read.csv("data/test.csv")

#-------------------------------------
#-------------------------------------

train.pre = train
test.pre  = test

#-------------------------------------
# Idade em segmentos [ok]
#-------------------------------------

#impute antes de dividir por faixas
train.pre$Age[which(is.na(train.pre$Age) & train.pre$Pclass == 1)] = 37 
train.pre$Age[which(is.na(train.pre$Age) & train.pre$Pclass == 2)] = 29 
train.pre$Age[which(is.na(train.pre$Age) & train.pre$Pclass == 3)] = 24 

test.pre$Age[which(is.na(test.pre$Age) & test.pre$Pclass == 1)] = 37 
test.pre$Age[which(is.na(test.pre$Age) & test.pre$Pclass == 2)] = 29 
test.pre$Age[which(is.na(test.pre$Age) & test.pre$Pclass == 3)] = 24 

train.pre$AgeFaixa = NA
train.pre$AgeFaixa[which(train.pre$Age <=11)] = "0-11"
train.pre$AgeFaixa[which(train.pre$Age > 11 & train.pre$Age <= 18)] = "11-18"
train.pre$AgeFaixa[which(train.pre$Age > 18 & train.pre$Age <= 22)] = "18-22"
train.pre$AgeFaixa[which(train.pre$Age > 22 & train.pre$Age <= 27)] = "22-27"
train.pre$AgeFaixa[which(train.pre$Age > 27 & train.pre$Age <= 33)] = "27-33"
train.pre$AgeFaixa[which(train.pre$Age > 33 & train.pre$Age <= 40)] = "33-40"
train.pre$AgeFaixa[which(train.pre$Age > 40 & train.pre$Age <= 66)] = "40-66"
train.pre$AgeFaixa[which(train.pre$Age > 66)] = "66+"
train.pre$AgeFaixa[which(is.na(train.pre$Age))] = "NoAge"
train.pre$AgeFaixa = as.factor(train.pre$AgeFaixa)

test.pre$AgeFaixa = NA
test.pre$AgeFaixa[which(test.pre$Age <=11)] = "0-11"
test.pre$AgeFaixa[which(test.pre$Age > 11 & test.pre$Age <= 18)] = "11-18"
test.pre$AgeFaixa[which(test.pre$Age > 18 & test.pre$Age <= 22)] = "18-22"
test.pre$AgeFaixa[which(test.pre$Age > 22 & test.pre$Age <= 27)] = "22-27"
test.pre$AgeFaixa[which(test.pre$Age > 27 & test.pre$Age <= 33)] = "27-33"
test.pre$AgeFaixa[which(test.pre$Age > 33 & test.pre$Age <= 40)] = "33-40"
test.pre$AgeFaixa[which(test.pre$Age > 40 & test.pre$Age <= 66)] = "40-66"
test.pre$AgeFaixa[which(test.pre$Age > 66)] = "66+"
test.pre$AgeFaixa[which(is.na(test.pre$Age))] = "NoAge"
test.pre$AgeFaixa = as.factor(test.pre$AgeFaixa)

#garantindo os mesmos nivels
levels(train.pre$AgeFaixa) == levels(test.pre$AgeFaixa)

#-------------------------------------
# Cabin como letra única
#-------------------------------------

train.pre$Cabin = as.character(train.pre$Cabin)
train.pre$Cabin[which(train.pre$Cabin == "")] = "NoCabin"
values = lapply(1:nrow(train.pre), function(i) {
  cabin = train.pre$Cabin[i]
  if(cabin == "NoCabin") {
    return("NoCabin")
  }else {
    return(stringr::str_sub(cabin, 1, 1))
  }
})
train.pre$CabinFaixa = unlist(values)
train.pre$CabinFaixa = as.factor(train.pre$CabinFaixa)

test.pre$Cabin = as.character(test.pre$Cabin)
test.pre$Cabin[which(test.pre$Cabin == "")] = "NoCabin"
values = lapply(1:nrow(test.pre), function(i) {
  cabin = test.pre$Cabin[i]
  if(cabin == "NoCabin") {
    return("NoCabin")
  }else {
    return(stringr::str_sub(cabin, 1, 1))
  }
})
test.pre$CabinFaixa = unlist(values)
test.pre$CabinFaixa = as.factor(test.pre$CabinFaixa)
test.pre$CabinFaixa = factor(test.pre$CabinFaixa, levels = levels(train.pre$CabinFaixa))

#garantindo os mesmos nivels
levels(train.pre$CabinFaixa) == levels(test.pre$CabinFaixa)

#-------------------------------------
# Sibling + Parents -> NotAlone
#-------------------------------------

train.pre$ParSib = train.pre$SibSp + train.pre$Parch 
test.pre$ParSib  = test.pre$SibSp + test.pre$Parch

train.pre$NotAlone = 0
train.pre$NotAlone[which(train.pre$ParSib >= 1)] = 1

test.pre$NotAlone = 0
test.pre$NotAlone[which(test.pre$ParSib >= 1)] = 1

#-------------------------------------
# Fare em faixas
#-------------------------------------

#TODO: automatizar a geração das faixas

train.pre$FareFaixa = NA
train.pre$FareFaixa[which(train.pre$Fare <=7.91)] = "<7.91"
train.pre$FareFaixa[which(train.pre$Fare > 7.91  & train.pre$Fare <= 14.455)] = "7.91-14.45"
train.pre$FareFaixa[which(train.pre$Fare > 14.455  & train.pre$Fare <= 31)] = "14.45-31"
train.pre$FareFaixa[which(train.pre$Fare > 31 & train.pre$Fare <=99)] = "31-99"
train.pre$FareFaixa[which(train.pre$Fare > 99 & train.pre$Fare <= 250)] = "99-250"
train.pre$FareFaixa[which(train.pre$Fare > 250)] = "250+"
train.pre$FareFaixa = as.factor(train.pre$FareFaixa)

test.pre$FareFaixa = NA
test.pre$FareFaixa[which(test.pre$Fare <=7.91)] = "<7.91"
test.pre$FareFaixa[which(test.pre$Fare > 7.91  & test.pre$Fare <= 14.455)] = "7.91-14.45"
test.pre$FareFaixa[which(test.pre$Fare > 14.455  & test.pre$Fare <= 31)] = "14.45-31"
test.pre$FareFaixa[which(test.pre$Fare > 31 & test.pre$Fare <= 99)] = "31-99"
test.pre$FareFaixa[which(test.pre$Fare > 99 & test.pre$Fare <= 250)] = "99-250"
test.pre$FareFaixa[which(test.pre$Fare > 250)] = "250+"

# 2 valores ausente no teste
test.pre$FareFaixa[which(is.na(test.pre$FareFaixa))] = "<7.91"
test.pre$FareFaixa = as.factor(test.pre$FareFaixa)

#garantindo os mesmos niveis
levels(train.pre$FareFaixa) == levels(test.pre$FareFaixa)

#-----------------------------------------------
# Função extrair o titutlo
#-----------------------------------------------

Names = train.pre$Name
Titles = str_extract(string = Names, pattern = "([A-Za-z]+)\\.")
Titles[which(Titles == "Mlle.")] = "Miss."
Titles[which(Titles == "Ms.")] = "Miss."
Titles[which(Titles == "Mme.")] = "Mrs."
ren = c("Lady.", "Countess.", "Capt.", "Col.", "Don.", "Dr.", "Major.", "Rev.", "Sir.",
        "Jonkheer.", "Dona.")
Titles[which(Titles %in% ren)] = "Rare"
train.pre$Titles = Titles
train.pre$Titles = as.factor(train.pre$Titles)

Names = test$Name
Titles = str_extract(string = Names, pattern = "([A-Za-z]+)\\.")
Titles[which(Titles == "Mlle.")] = "Miss."
Titles[which(Titles == "Ms.")] = "Miss."
Titles[which(Titles == "Mme.")] = "Mrs."
ren = c("Lady.", "Countess.", "Capt.", "Col.", "Don.", "Dr.", "Major.", "Rev.", "Sir.",
        "Jonkheer.", "Dona.")
Titles[which(Titles %in% ren)] = "Rare"
test.pre$Titles = Titles
test.pre$Titles = as.factor(test.pre$Titles)
levels(test.pre$Titles) == levels(train.pre$Titles)

# --------------------------------------------
# New Features
# --------------------------------------------

#train.pre$AgePerClass = train.pre$Age * train.pre$Pclass
#test.pre$AgePerClass = train.pre$Age * test.pre$Pclass

#-----------------------------------------------
# Drop nos atributos que foram transformados
#-----------------------------------------------

drop = c("PassengerId", "Name", "Age", "Cabin", "Fare", "SibSp", "Parch", "ParSib", "Ticket")
train.pre2 = train.pre[, -which(colnames(train.pre) %in% drop)]
test.pre2  = test.pre [, -which(colnames(test.pre) %in% drop)]

#-----------------------------------------------
# Renomeando os factors para numeros
#-----------------------------------------------

print("Convertendo colunas no treino")
for(i in 2:ncol(train.pre2)){
  if(class(train.pre2[,i]) != "numeric"){
    print(colnames(train.pre2)[i])
    train.pre2[,i] = as.numeric(train.pre2[,i])
  }
}

print("Convertendo colunas no teste")
for(i in 1:ncol(test.pre2)){
  if(class(test.pre2[,i]) != "numeric"){
    print(colnames(train.pre2)[i])
    test.pre2[,i] = as.numeric(test.pre2[,i])
  }
}

# --------------------------------------------
# --------------------------------------------

# cria tarefa de classificacao e especifica a coluna preditiva
task = mlr::makeClassifTask(data = train.pre2, target = "Survived")

# cria o algoritmo, no caso uma arvore de decisao
lr  = mlr::makeLearner(cl = "classif.logreg", predict.type = "prob")
nb  = mlr::makeLearner(cl = "classif.naiveBayes", predict.type = "prob")
dt  = mlr::makeLearner(cl = "classif.rpart", predict.type = "prob")
rf  = mlr::makeLearner(cl = "classif.randomForest", predict.type = "prob")
svm = mlr::makeLearner(cl = "classif.svm", predict.type = "prob")
knn = mlr::makeLearner(cl = "classif.kknn", predict.type = "prob")
xgb = mlr::makeLearner(cl = "classif.xgboost", predict.type = "prob")
mlp = mlr::makeLearner(cl = "classif.mlp", predict.type = "prob")
algos = list(lr, nb, dt, knn, svm, xgb, rf, mlp)
#algos = list(dt, knn, rf)

# define o procedimento de validacao e avaliacao: 10x 10-CV
rdesc = mlr::makeResampleDesc(method = "RepCV", folds = 10, rep = 5, stratify = TRUE)

# medida de desempenho eh a acuracia simples
measures = list(acc, ber, bac, auc)

# rodando o experimento
result = mlr::benchmark(learners = algos, task, resampling = rdesc, measures = measures, show.info = TRUE)
print(result)

# ------------------------------------------------------------
# predticions -> test set
# ------------------------------------------------------------

ids = test$PassengerId

aux = lapply(algos, function(algo){
  print(algo$id)
  model = mlr::train(learner = algo, task = task)
  preds = mlr:::predictLearner(.learner = algo, .model = model, .newdata = test.pre2)
  
  values = lapply(1:nrow(preds), function(i) {
    line = preds[i,]
    return(colnames(preds)[which.max(line)])
  })
  values = as.numeric(unlist(values))
  
  df = cbind(ids, values)
  df = as.data.frame(df)
  colnames(df) = c("PassengerId","Survived")
  write.csv(df, file = paste0("submissionBase5_", algo$id, ".csv"), row.names = FALSE)
})


# ------------------------------------------------------------
# ------------------------------------------------------------