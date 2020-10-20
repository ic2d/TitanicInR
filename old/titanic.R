# --------------------------------------------
# --------------------------------------------

# Ideias
# - Deck
# - Titulo (mr, etc)
# - Idades (criança, etc)
# - Viaja sozinho ou não
#test["TravelAlone"] = test["SibSp"] + test["Parch"]
#knn - 3

# --------------------------------------------
# --------------------------------------------

library("mlr")

# --------------------------------------------
# --------------------------------------------

train = read.csv("data/train.csv")
train$Survived = as.factor(train$Survived)
test = read.csv("data/test.csv")

train$Age = round(train$Age)
test$Age  = round(test$Age)

drop = c("PassengerId", "Name", "Ticket", "Cabin")
train.pre = train[, -which(colnames(train) %in% drop)]
test.pre  = test [, -which(colnames(test) %in% drop)]

# data imputation
obj.train = mlr::impute(obj = train.pre, target = "Survived", 
  classes = list(numeric = mlr::imputeMedian(), integer = mlr::imputeMedian(), 
  factor  = mlr::imputeMode()))
train.imp = obj.train$data

obj.test = mlr::impute(obj = test.pre, classes = list(numeric = mlr::imputeMedian(), 
  numeric = mlr::imputeMedian(), integer = mlr::imputeMedian(), 
  factor  = mlr::imputeMode()))
test.imp = obj.test$data

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
algos = list(lr, nb, dt, knn, svm, xgb, rf)

# define o procedimento de validacao e avaliacao: 10x 10-CV
rdesc = mlr::makeResampleDesc(method = "RepCV", folds = 10, rep = 10, stratify = TRUE)

# medida de desempenho eh a acuracia simples
measures = list(acc, ber, bac)

# rodando o experimento
result = mlr::benchmark(learners = algos, task, resampling = rdesc, measures = measures, show.info = TRUE)
print(result)

# ------------------------------------------------------------
# predticions -> test set
# ------------------------------------------------------------

ids = test$PassengerId

model  = mlr::train(learner = xgb, task = task)
preds = mlr:::predictLearner(.learner = xgb, .model = model, .newdata = test.imp)

values = lapply(1:nrow(preds), function(i) {
  line = preds[i,]
  return(colnames(preds)[which.max(line)])
})
values = as.numeric(unlist(values))

df = cbind(ids, values)
df = as.data.frame(df)
colnames(df) = c("PassengerId","Survived")
write.csv(df, file = "submissionTestXGB.csv", row.names = FALSE)

# ------------------------------------------------------------
# ------------------------------------------------------------