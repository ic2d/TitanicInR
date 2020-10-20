# --------------------------------------------
# --------------------------------------------

# Survival (class) {0 = No, 1 = Yes}
# pclass - Tickect class {1st, 2nd, 3rd} 
# sex - homem / mulher
# age - idade
# sibsp # of siblings / spouses aboard the Titanic
# parch # of parents / children aboard the Titanic
# ticket -> ticket number
# fare -> passemger fare
# cabin -> cabin number 
# embarked -> por of embarkation {C - cherbourg, Q - Queenstown, S - Southampton}

# --------------------------------------------
# --------------------------------------------

library("mlr")
library("ggplot2")

# --------------------------------------------
# reading data
# --------------------------------------------

train = read.csv("data/train.csv")
train$Survived = as.factor(train$Survived)

# --------------------------------------------
# Sexo
# --------------------------------------------

sub = train[, c("Survived", "Sex")]
sex.tb = table(sub)
sex.df = as.data.frame(sex.tb)

g = ggplot(sex.df, aes(x = Sex, y = Freq, group = Survived, colour = Survived, fill = Survived))
g = g + geom_bar(stat='identity', position = "dodge") + theme_bw()
ggsave(g, file = "analysis/analysis_sex.pdf", width = 8, height = 6)

# --------------------------------------------
# Pclass
# --------------------------------------------

sub = train[, c("Survived", "Pclass")]
pc.tb = table(sub)
pc.df = as.data.frame(pc.tb)

g1 = ggplot(pc.df, aes(x = Pclass, y = Freq, group = Survived, colour = Survived, fill = Survived))
g1 = g1 + geom_bar(stat='identity', position = "dodge") + theme_bw()
ggsave(g1, file = "analysis/analysis_pclass.pdf", width = 8, height = 6)

# --------------------------------------------
# Embarked
# --------------------------------------------

sub = train[, c("Survived", "Embarked")]
em.tb = table(sub)
em.df = as.data.frame(em.tb)

g2 = ggplot(em.df, aes(x = Embarked, y = Freq, group = Survived, colour = Survived, fill = Survived))
g2 = g2 + geom_bar(stat='identity', position = "dodge") + theme_bw()
ggsave(g2, file = "analysis/analysis_embarked.pdf", width = 8, height = 6)

# --------------------------------------------
# Age
# --------------------------------------------

sub = train[, c("Survived", "Age")]
sub$AgeFaixa = NA
sub$AgeFaixa[which(sub$Age <=12)] = "0-12"
sub$AgeFaixa[which(sub$Age > 12 & sub$Age <= 20)] = "12-20"
sub$AgeFaixa[which(sub$Age > 20 & sub$Age <= 30)] = "21-30"
sub$AgeFaixa[which(sub$Age > 30 & sub$Age <= 40)] = "31-40"
sub$AgeFaixa[which(sub$Age > 40 & sub$Age <= 50)] = "41-50"
sub$AgeFaixa[which(sub$Age > 50 & sub$Age <= 60)] = "51-60"
sub$AgeFaixa[which(sub$Age > 60 & sub$Age <= 70)] = "61-70"
sub$AgeFaixa[which(sub$Age > 70)] = "70+"

age.tb = table(sub[, c(1,3)])
age.df = as.data.frame(age.tb)
g3 = ggplot(age.df, aes(x = AgeFaixa, y = Freq, group = Survived, colour = Survived, fill = Survived))
g3 = g3 + geom_bar(stat='identity', position = "dodge") + theme_bw()
ggsave(g3, file = "analysis/analysis_age.pdf", width = 8, height = 6)

# --------------------------------------------
# Cabin = letra unica x survival
# --------------------------------------------

sub = train[, c("Survived", "Cabin")]
sub$Cabin = as.character(sub$Cabin)
sub$Cabin[which(sub$Cabin == "")] = "NoCabin"

values = lapply(1:nrow(sub), function(i) {
  cabin = sub$Cabin[i]
  if(cabin == "NoCabin") {
    return("NoCabin")
  }else {
    return(stringr::str_sub(cabin, 1, 1))
  }
})
sub$CabinFaixa = unlist(values)
sub$CabinFaixa = as.factor(sub$CabinFaixa)

cab.tb = table(sub[, c(1,3)])
cab.df = as.data.frame(cab.tb)
g4 = ggplot(cab.df, aes(x = CabinFaixa, y = Freq, group = Survived, colour = Survived, fill = Survived))
g4 = g4 + geom_bar(stat='identity', position = "dodge") + theme_bw()
ggsave(g4, file = "analysis/analysis_cabin.pdf", width = 8, height = 6)

# --------------------------------------------
# sibsp # of siblings / spouses aboard the Titanic
# --------------------------------------------

sub = train[, c("SibSp", "Survived")]
sb.tb = table(sub)
sb.df = as.data.frame(sb.tb)
g5 = ggplot(sb.df, aes(x = SibSp, y = Freq, group = Survived, colour = Survived, fill = Survived))
g5 = g5 + geom_bar(stat='identity', position = "dodge") + theme_bw()
ggsave(g5, file = "analysis/analysis_SibSP.pdf", width = 8, height = 6)
g5 

# --------------------------------------------
# parch # of parents / children aboard the Titanic
# --------------------------------------------

sub = train[, c("Parch", "Survived")]
par.tb = table(sub)
par.df = as.data.frame(par.tb)
g6 = ggplot(par.df, aes(x = Parch, y = Freq, group = Survived, colour = Survived, fill = Survived))
g6 = g6 + geom_bar(stat='identity', position = "dodge") + theme_bw()
ggsave(g6, file = "analysis/analysis_Parch.pdf", width = 8, height = 6)
g6

# --------------------------------------------
# SibSP + Parch
# --------------------------------------------

sub = train[, c("Parch", "SibSp", "Survived")]
sub$ParSib = sub$SibSp + sub$Parch
prs.tb = table(sub[, c(3,4)])

prs.df = as.data.frame(prs.tb)
g7 = ggplot(prs.df, aes(x = ParSib, y = Freq, group = Survived, colour = Survived, fill = Survived))
g7 = g7 + geom_bar(stat='identity', position = "dodge") + theme_bw()
ggsave(g7, file = "analysis/analysis_ParSib.pdf", width = 8, height = 6)
g7 

# --------------------------------------------
# Fare
# --------------------------------------------

sub = train[, c("Fare", "Survived")]
sub$FareFaixa = NA
sub$FareFaixa[which(sub$Fare <=25)] = "0-25"
sub$FareFaixa[which(sub$Fare > 25  & sub$Fare <= 50)] = "25-50"
sub$FareFaixa[which(sub$Fare > 50  & sub$Fare <= 100)] = "50-100"
sub$FareFaixa[which(sub$Fare > 100 & sub$Fare <=200)] = "100-200"
sub$FareFaixa[which(sub$Fare > 200 & sub$Fare <= 300)] = "200-300"
sub$FareFaixa[which(sub$Fare > 300 & sub$Fare <= 60)] = "300+"

far.tb = table(sub[,c(2,3)])
far.df = as.data.frame(far.tb)

g8 = ggplot(far.df, aes(x = FareFaixa, y = Freq, group = Survived, colour = Survived, fill = Survived))
g8 = g8 + geom_bar(stat='identity', position = "dodge") + theme_bw()
ggsave(g8, file = "analysis/analysis_FareFaixa.pdf", width = 8, height = 6)
g8

# --------------------------------------------
# --------------------------------------------
