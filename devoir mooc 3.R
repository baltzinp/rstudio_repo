# MOOC devoir 2

# importer les données
satisfaction <- read_csv2("/Users/baltzingerphilippe/Downloads/satisfaction_hopital.csv")

data <- read_csv2("satisfaction_hopital.csv")

# Transformez la variable « recommander » en une variable binaire « recommander.b » :

## je commence par créer la variable recommander.b en définissant chaque occurence à NA
data$recommander.b = NA

## je fais une boucle pour coder cette nouvelle variabel
for(i in 1:length(data$recommander)){
  if(!is.na(data$recommander[i])){
    if(data$recommander[i]== 0) {data$recommander.b[i] = 1}
    else if(data$recommander[i] ==1) {data$recommander.b[i] = 1}
    else {data$recommander.b[i] =2}
  }
}

## je vérifie le résultat
table(data$recommander, data$recommander.b,deparse.level = 2, useNA = "always")


#  A l’aide d’un odds-ratio, estimez la force de l’association entre « recommander.b » et « sexe ». Estimez un intervalle de confiance de cet odds-ratio.

Epi::twoby2(1-data$recommander.b, 1-data$sexe)


# Calculez la corrélation (de Pearson) entre « score.relation » et « age ». Testez statistiquement cette corrélation (le script doit inclure la vérification éventuelle des conditions de validité de la méthode utilisée).

## je visualise les données

plot(data$score.relation, data$age)
hist(data$age)
hist(data$score.relation)

## Je calcule la corrélation entre les deux variables 

cor(data$score.relation, data$age, method = "pearson", use = "complete.obs")

## Je teste cette corrélation. Au moins une des deux variables a une distribution normale (cf histogram de age), on peut donc utiliser la methode de pearson

cor.test(data$score.relation, data$age, method = "pearson")

## Il n'y a pas de corrélation statistiquement significative entre les deux variables p>0,05

# La moyenne du score de relation est-il significativement différent chez les hommes et chez les femmes ? (le script doit inclure la vérification éventuelle des conditions de validité de la méthode utilisée)

## je visualise les données
hist(data$score.relation)
hist(data$score.relation[data$sexe==1]) # pour les hommes, ne suit pas une loi normale
hist(data$score.relation[data$sexe ==0]) # pour les femmes, ne suit pas une loi normale

## j'analyse les effectifs

table(data$sexe)

## j'analyse les écarts types, moyenne, médiane

by(data$score.relation, data$sexe, sd, na.rm = T) # pas de différence significative
by(data$score.relation, data$sexe, mean, na.rm = T) # pas de différence significative
by(data$score.relation, data$sexe, median, na.rm = T) # pas de différence significative

## le écarts types sont quasi identiques mais la variable score.relation ne suit pas une loi normale. J'utilise le test de wilcoxon
wilcox.test(data$score.relation ~data$sexe)

## il n'y a pas de différence significative dans le score de relation entre hommes et femmes