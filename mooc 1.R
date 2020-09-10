##MOOC Introduction à la statistique avec R
# devoir 1

# importe la table de données
satisfaction <- read_csv2("/Users/baltzingerphilippe/Downloads/satisfaction_hopital.csv")

# examine la construction de la table de données
str(satisfaction)

## Question 1
# transforme les variables catégorielles en facteurs
satisfaction$service <- factor(satisfaction$service)
satisfaction$profession <-  factor(satisfaction$profession, labels = c("agriculteur exploitant", "artisan, commerçant, chef d'entreprise",
                                                                       "cadre, profession intellectuelle ou artistique, profession libérale",
                                                                       "profession intermédiaire de l'enseignement et autres",
                                                                       "employé", "ouvrier", "étudiant, militaire, chômeur sans avoir jamais travaillé", "autre"))
satisfaction$recommander <- factor(satisfaction$recommander, labels = c("Non", "Oui", "Oui probablement"))

# détermine les effectifs et fréquences associées
round(prop.table(table(satisfaction$service))*100, 0)
round(prop.table(table(satisfaction$profession))*100, 0)
round(prop.table(table(satisfaction$recommander))*100, 0)

# affiche sous forme de bar plot les résultats précédents
barplot(round(prop.table(table(satisfaction$service))*100, 0), xlab = "service")
barplot(round(prop.table(table(satisfaction$profession))*100, 0), xlab = "profession")
barplot(round(prop.table(table(satisfaction$recommander))*100, 0), xlab = "Souhaite recommander")

## Question 2
# analyse les variables quantitatives
