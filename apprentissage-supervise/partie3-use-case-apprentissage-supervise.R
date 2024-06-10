#------------------------------------#
# DONNEES DE TEST ET D'APPRENTISSAGE #
#------------------------------------#

# Chargement des données
achat <- read.csv("Data Achat.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors=T)
str(achat)
View(achat)
table(achat$Achat)

# Construction des ensembles d'apprentissage et de test
achat_EA <- achat[1:400,]
achat_ET <- achat[401:600,]

# Suppression variable ID
achat_EA <- subset(achat_EA, select = -ID) 

# Autre solution par référence au numéro de colonne : achat_EA <- achat_EA[,-1]

# Affichages
View(achat_EA)
View(achat_ET)
summary(achat_EA)
summary(achat_ET)

#--------------------------------#
# APPRENTISSAGE DE L'ARBRE RPART #
#--------------------------------#

# Installation de la librairies rpart
install.packages("rpart")

# Activation de la librairies rpart
library(rpart)

# Construction de l'arbre de décision
tree1 <- rpart(Achat~., achat_EA)

# Affichage de l'arbre par les fonctions de R Base 
plot(tree1)
text(tree1, pretty=0)

#-----------------------#
# TEST DE L'ARBRE RPART #
#-----------------------#

# Application de l'arbre de décision a l'ensemble de test 'achat_ET'
test_tree1 <- predict(tree1, achat_ET, type="class")

# Affichage du vecteur de prédictions de la classe des exemples de test
test_tree1

# Affichage du nombre de prédictions pour chacune des classes
table(test_tree1)

# Ajout des prédictions comme une nouvelle colonne 'Prediction' dans le data frame 'achat_ET'
achat_ET$Prediction <- test_tree1
View(achat_ET)

# Affichage de liste des exemples de test correctement prédits
View(achat_ET[achat_ET$Achat==achat_ET$Prediction, ])

# Calcul du nombre de succès : nombre d'exemples avec classe réelle et prédiction identiques
nbr_succes <- nrow(achat_ET[achat_ET$Achat==achat_ET$Prediction,])
nbr_succes

# Calcul du taux de succès : nombre de succès sur nombre d'exemples de test
taux_succes <- nbr_succes/nrow(achat_ET)
taux_succes

# Calcul du nombre d’échecs : nombre d'exemples avec classe réelle et prédiction différentes
nbr_echecs <- nrow(achat_ET[achat_ET$Achat!=achat_ET$Prediction,])
nbr_echecs

# Calcul du taux d’échecs : nombre d’échecs sur nombre d'exemples de test
taux_echecs <- nbr_echecs/nrow(achat_ET)
taux_echecs

#-------------------------------#
# PREDICTIONS PAR L'ARBRE RPART #
#-------------------------------#

# Chargement des exemples prospects dans un data frame 'achat_pro'
achat_pro <- read.csv("Data Achat Prospects.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors=T)

# Application de l'arbre de décision aux prospects dans 'achat_pro' : classe prédite
pred_tree1 <- predict(tree1, achat_pro, type="class")

# Affichage des résultats (prédictions)
pred_tree1

# Affichage du nombre de prédictions pour chaque classe
table(pred_tree1)

# Ajout dans le data frame achat_pro d'une colonne Prediction contenant la classe prédite 
achat_pro$Prediction <- pred_tree1

# Création d'un data frame contenant les prédictions 'Oui'
achat_pro_oui <- achat_pro[achat_pro$Prediction=="Oui",]
View(achat_pro_oui)

# Création d'un data frame contenant les prédictions 'Non'
achat_pro_non <- achat_pro[achat_pro$Prediction=="Non",]
View(achat_pro_non)
