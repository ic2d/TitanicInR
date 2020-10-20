# --------------------------------------------
# --------------------------------------------

library("mlr")

# --------------------------------------------
# --------------------------------------------

print(" - Reading data")
train = read.csv("data/train.csv")
train$Survived = as.factor(train$Survived)
test = read.csv("data/test.csv")

print(" - Droping ids")
drop = c("PassengerId", "Name", "Ticket")
train.pre = train[, -which(colnames(train) %in% drop)]
test.pre  = test [, -which(colnames(test) %in% drop)]

#-------------------------------------
# Idade
#-------------------------------------

print(" - Impute idade")
train.pre$Age[which(is.na(train.pre$Age) & train.pre$Pclass == 1)] = 37 
train.pre$Age[which(is.na(train.pre$Age) & train.pre$Pclass == 2)] = 29 
train.pre$Age[which(is.na(train.pre$Age) & train.pre$Pclass == 3)] = 24 

test.pre$Age[which(is.na(test.pre$Age) & test.pre$Pclass == 1)] = 37 
test.pre$Age[which(is.na(test.pre$Age) & test.pre$Pclass == 2)] = 29 
test.pre$Age[which(is.na(test.pre$Age) & test.pre$Pclass == 3)] = 24 


#-------------------------------------
# Cabin como letra única
#-------------------------------------

print(" - Cabines com letras")
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

print(" - Siblings + Parents")
train.pre$ParSib = train.pre$SibSp + train.pre$Parch 
test.pre$ParSib  = test.pre$SibSp + test.pre$Parch

#-------------------------------------
# Fare base o PClass
#-------------------------------------

m.value = median(test.pre[which(test.pre$Pclass == 3), "Fare"], na.rm = TRUE)
test.pre[which(is.na(test.pre$Fare)), "Fare"] = m.value

#-----------------------------------------------
# Drop nos atributos que foram transformados
#-----------------------------------------------

print(" - Drop nos atributos transformados")
drop2 = c("Cabin", "SibSp", "Parch")

train.pre2 = train.pre[, -which(colnames(train.pre) %in% drop2)]
test.pre2  = test.pre [, -which(colnames(test.pre) %in% drop2)]

train.pre2$ParSib = as.numeric(train.pre2$ParSib)
test.pre2$ParSib = as.numeric(test.pre2$ParSib)

#-----------------------------------------------
# Renomeando os factors
#-----------------------------------------------

# Pclass como factor
train.pre2$Pclass = as.factor(train.pre2$Pclass)
test.pre2$Pclass  = as.factor(test.pre$Pclass)

train.data = train.pre2
test.data = test.pre2

#print("Convertendo colunas no treino")
for(i in 2:ncol(train.data)){
  if(class(train.data[,i]) != "numeric"){
    print(colnames(train.data)[i])
    train.data[,i] = as.numeric(train.data[,i])
  }
}

for(i in 1:ncol(test.data)){
  if(class(test.data[,i]) != "numeric"){
    print(colnames(test.data)[i])
    test.data[,i] = as.numeric(test.data[,i])
  }
}

# --------------------------------------------
# --------------------------------------------

print(" - Criando tarefa Separada por sexo")

#male = 2
#female = 1


# cria tarefa de classificacao e especifica a coluna preditiva
sub.train = train.data[which(train.data$Sex == 1),]
sub.train$Sex = NULL
task.fm = mlr::makeClassifTask(id = "female_model", data = sub.train, target = "Survived")

sub2.train = train.data[which(train.data$Sex == 2),]
sub2.train$Sex = NULL
task.ms = mlr::makeClassifTask(id = "male_model", data = sub2.train, target = "Survived")

tasks = list(task.fm, task.ms)

print(" - Criando learners")
# cria o algoritmo, no caso uma arvore de decisao
lr  = mlr::makeLearner(cl = "classif.logreg", predict.type = "prob")
nb  = mlr::makeLearner(cl = "classif.naiveBayes", predict.type = "prob")
dt  = mlr::makeLearner(cl = "classif.rpart", predict.type = "prob")
rf  = mlr::makeLearner(cl = "classif.randomForest", predict.type = "prob")
svm = mlr::makeLearner(cl = "classif.svm", predict.type = "prob")
knn = mlr::makeLearner(cl = "classif.kknn", predict.type = "prob")
xgb = mlr::makeLearner(cl = "classif.xgboost", predict.type = "prob")
algos = list(lr, nb, dt, knn, svm, xgb, rf)


print(" - Rodando o experimento")
rdesc = mlr::makeResampleDesc(method = "RepCV", folds = 10, rep = 5, stratify = TRUE)
measures = list(acc, ber, bac, auc)
result = mlr::benchmark(learners = algos, tasks = tasks, resampling = rdesc, measures = measures, show.info = TRUE)

print(result)

# ------------------------------------------------------------
# ------------------------------------------------------------