# --------------------------------------------
# --------------------------------------------

library("mlr")

# --------------------------------------------
# --------------------------------------------

train = read.csv("data/train.csv")
train$Survived = as.factor(train$Survived)
test = read.csv("data/test.csv")

drop = c("PassengerId", "Name", "Ticket")

train.pre = train[, -which(colnames(train) %in% drop)]
test.pre  = test [, -which(colnames(test) %in% drop)]

#-------------------------------------
# Idade em segmentos [ok]
#-------------------------------------

#impute antes de dividir por faixas

train.pre$AgeFaixa = NA
train.pre$AgeFaixa[which(train.pre$Age <=12)] = "0-12"
train.pre$AgeFaixa[which(train.pre$Age > 12 & train.pre$Age <= 20)] = "12-20"
train.pre$AgeFaixa[which(train.pre$Age > 20 & train.pre$Age <= 30)] = "21-30"
train.pre$AgeFaixa[which(train.pre$Age > 30 & train.pre$Age <= 40)] = "31-40"
train.pre$AgeFaixa[which(train.pre$Age > 40 & train.pre$Age <= 50)] = "41-50"
train.pre$AgeFaixa[which(train.pre$Age > 50 & train.pre$Age <= 60)] = "51-60"
train.pre$AgeFaixa[which(train.pre$Age > 60 & train.pre$Age <= 70)] = "61-70"
train.pre$AgeFaixa[which(train.pre$Age > 70)] = "70+"
train.pre$AgeFaixa[which(is.na(train.pre$Age))] = "NoAge"
train.pre$AgeFaixa = as.factor(train.pre$AgeFaixa)

test.pre$AgeFaixa = NA
test.pre$AgeFaixa[which(test.pre$Age <=12)] = "0-12"
test.pre$AgeFaixa[which(test.pre$Age > 12 & test.pre$Age <= 20)] = "12-20"
test.pre$AgeFaixa[which(test.pre$Age > 20 & test.pre$Age <= 30)] = "21-30"
test.pre$AgeFaixa[which(test.pre$Age > 30 & test.pre$Age <= 40)] = "31-40"
test.pre$AgeFaixa[which(test.pre$Age > 40 & test.pre$Age <= 50)] = "41-50"
test.pre$AgeFaixa[which(test.pre$Age > 50 & test.pre$Age <= 60)] = "51-60"
test.pre$AgeFaixa[which(test.pre$Age > 60 & test.pre$Age <= 70)] = "61-70"
test.pre$AgeFaixa[which(test.pre$Age > 70)] = "70+"
test.pre$AgeFaixa[which(is.na(test.pre$Age))] = "NoAge"
test.pre$AgeFaixa = as.factor(test.pre$AgeFaixa)

#garantindo os mesmos nivels
#levels(train.pre$AgeFaixa) == levels(test.pre$AgeFaixa)

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
# Sibling + Parents
#-------------------------------------

train.pre$ParSib = train.pre$SibSp + train.pre$Parch 
test.pre$ParSib  = test.pre$SibSp + test.pre$Parch

#-------------------------------------
# Fare em faixas
#-------------------------------------

#impute antes de dividir por faixas
#if   Pclass == 1: impute 40 #37
#elif Pclass == 2: impute 30 #29
#else PClass == 3  ipute 20  #24

train.pre$FareFaixa = NA
train.pre$FareFaixa[which(train.pre$Fare <=25)] = "0-25"
train.pre$FareFaixa[which(train.pre$Fare > 25  & train.pre$Fare <= 50)] = "25-50"
train.pre$FareFaixa[which(train.pre$Fare > 50  & train.pre$Fare <= 100)] = "50-100"
train.pre$FareFaixa[which(train.pre$Fare > 100 & train.pre$Fare <=200)] = "100-200"
train.pre$FareFaixa[which(train.pre$Fare > 200 & train.pre$Fare <= 300)] = "200-300"
train.pre$FareFaixa[which(train.pre$Fare > 300 & train.pre$Fare <= 60)] = "300+"
train.pre$FareFaixa = as.factor(train.pre$FareFaixa)

test.pre$FareFaixa = NA
test.pre$FareFaixa[which(test.pre$Fare <=25)] = "0-25"
test.pre$FareFaixa[which(test.pre$Fare > 25  & test.pre$Fare <= 50)] = "25-50"
test.pre$FareFaixa[which(test.pre$Fare > 50  & test.pre$Fare <= 100)] = "50-100"
test.pre$FareFaixa[which(test.pre$Fare > 100 & test.pre$Fare <=200)] = "100-200"
test.pre$FareFaixa[which(test.pre$Fare > 200 & test.pre$Fare <= 300)] = "200-300"
test.pre$FareFaixa[which(test.pre$Fare > 300 & test.pre$Fare <= 60)] = "300+"

# 2 valores ausente no teste
test.pre$FareFaixa[which(is.na(test.pre$FareFaixa))] = "0-25"
test.pre$FareFaixa = as.factor(test.pre$FareFaixa)

#garantindo os mesmos nivels
#levels(train.pre$FareFaixa) == levels(test.pre$FareFaixa)

#-----------------------------------------------
# Drop nos atributos que foram transformados
#-----------------------------------------------

drop2 = c("Age", "Cabin", "Fare", "SibSp", "Parch")

train.pre2 = train.pre[, -which(colnames(train.pre) %in% drop2)]
test.pre2  = test.pre [, -which(colnames(test.pre) %in% drop2)]

#-----------------------------------------------
# data imputation
#-----------------------------------------------

obj.train = mlr::impute(obj = train.pre2, target = "Survived", 
    classes = list(numeric = mlr::imputeMedian(), integer = mlr::imputeMedian(), 
                  factor  = mlr::imputeMode()))
train.imp = obj.train$data

obj.test = mlr::impute(obj = test.pre2, classes = list(numeric = mlr::imputeMedian(), 
    numeric = mlr::imputeMedian(), integer = mlr::imputeMedian(), 
    factor  = mlr::imputeMode()))
test.imp = obj.test$data

#-----------------------------------------------
# Renomeando os factors
#-----------------------------------------------

print("Convertendo colunas no treino")
for(i in 2:ncol(train.imp)){
  if(class(train.imp[,i]) != "numeric"){
    print(colnames(train.imp)[i])
    train.imp[,i] = as.numeric(train.imp[,i])
  }
}

print("Convertendo colunas no teste")
for(i in 1:ncol(test.imp)){
  if(class(test.imp[,i]) != "numeric"){
    print(colnames(train.imp)[i])
    test.imp[,i] = as.numeric(test.imp[,i])
  }
}

# --------------------------------------------
# --------------------------------------------

# cria tarefa de classificacao e especifica a coluna preditiva
task = mlr::makeClassifTask(data = train.imp, target = "Survived")

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
rdesc = mlr::makeResampleDesc(method = "RepCV", folds = 10, rep = 2, stratify = TRUE)

# medida de desempenho eh a acuracia simples
measures = list(acc, ber, bac, auc)

# rodando o experimento
result = mlr::benchmark(learners = algos, task, resampling = rdesc, measures = measures, show.info = TRUE)
print(result)

# ------------------------------------------------------------
# predticions -> test set
# ------------------------------------------------------------

ids = test$PassengerId

model  = mlr::train(learner = dt, task = task)
preds = mlr:::predictLearner(.learner = dt, .model = model, .newdata = test.imp)

values = lapply(1:nrow(preds), function(i) {
  line = preds[i,]
  return(colnames(preds)[which.max(line)])
})
values = as.numeric(unlist(values))

df = cbind(ids, values)
df = as.data.frame(df)
colnames(df) = c("PassengerId","Survived")
write.csv(df, file = "submissionBase2.csv", row.names = FALSE)

# ------------------------------------------------------------
# ------------------------------------------------------------