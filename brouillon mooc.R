#Importation des données & chargement prettyR
data<-satisfaction
library(prettyR)

#Traitement des variables catégorielles
#Pour la clarté, les professions ne sont pas recodées.
data$service <- factor(data$service, labels=c(paste('Serv.',seq(1:8),sep=" ")))
data$sexe <- factor(data$sexe, labels=c('Homme','Femme'))
data$profession <- factor(data$profession)

#Création des tableaux
#Les professions non-déclarées sont concervées
tab.service <- table(data$service, useNA="always")
tab.sexe <- table(data$sexe, useNA="always")
tab.prof <- table(data$profession, useNA="always")

#Réponse 1, arrondie à .00
round(prop.table(tab.service)*100, 2)
round(prop.table(tab.sexe)*100, 2)
round(prop.table(tab.prof)*100, 2)
#Pourcentages sans les NA
round(prop.table(tab.prof[1:8])*100, 2)

#Réponse 2
data.num <- data[c(-1,-2,-4)]
describe(data.num,num.desc=c("mean","median","sd","min","max","valid.n"))

#Réponse 3
hist(data$score.relation, main="Histogramme du score de relation ", xlab="Score")

#Réponse 4
boxplot(data$score.relation~data$sexe, main="Score de relation selon sexe", xlab="Sexe", ylab="Score")
