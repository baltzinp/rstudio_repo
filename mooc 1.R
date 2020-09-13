##MOOC Introduction à la statistique avec R
# devoir 1

# vérifie si les librairies nécessaires sont installées. Si oui, les charge, sinon, les installe puis les charge. 
packages = c("tidyverse", "ggplot2", "prettyR")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# importe la table de données
satisfaction <- read_csv2("satisfaction_hopital.csv")

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
satisfaction$sexe <- factor(satisfaction$sexe, labels = c("Femme", "Homme"))

# détermine les effectifs et fréquences associées
round(prop.table(table(satisfaction$service))*100, 0)
round(prop.table(table(satisfaction$profession))*100, 0)
round(prop.table(table(satisfaction$recommander))*100, 0)

# affiche sous forme de bar plot les résultats précédents
barplot(round(prop.table(table(satisfaction$service))*100, 0), xlab = "service")
barplot(round(prop.table(table(satisfaction$profession))*100, 0), xlab = "profession")
barplot(round(prop.table(table(satisfaction$recommander))*100, 0), xlab = "Souhaite recommander")

## Question 2
# analyse les variables quantitatives à l'aide du tidyverse (librairie à installer et à charger)
satisfaction %>% 
  select_if(is.numeric) %>% 
  prettyR::describe(num.desc = c("mean", "median", "sd", "min", "max", "valid.n"))


## Question 3 
#Réalisation d'un histogramme du score de relation à l'aide de la librairie ggplot2 (à installer et à charger)

satisfaction %>% 
  ggplot(aes(score.relation))+
  geom_histogram(binwidth = 1, fill = 3, color = 1)+
  labs(title = "Distribution du score de relation", x = "Score de relation", y = "Nombre de répondants")


## Question 4    
# Distribution du score de relation selon le sexe

satisfaction %>% 
  ggplot(aes(score.relation, fill = sexe))+
  geom_boxplot()+
  labs(x = "Score de relation", title = "Distribution du score de relation en fonction du sexe")
    
    
    
    
    
    
    
    
  

