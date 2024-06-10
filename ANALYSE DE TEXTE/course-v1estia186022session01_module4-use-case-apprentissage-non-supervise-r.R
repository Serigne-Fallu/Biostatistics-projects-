#----------------------------------#
# CHARGEMENT DES DONNEES GROCERIES #
#----------------------------------#

# Chargement du fichier CSV
groceries <- read.table("Data Groceries.csv", header=TRUE, dec=".", sep="\t", stringsAsFactors=T)
ncol(groceries)
colnames(groceries)

# Installation de la librairies 'arules'
install.packages("arules")

# Activation de la librairies 'arules'
library(arules)

# Représentation des données au format transactionnel
groceries_tr <- as(groceries, "transactions")
summary(groceries_tr)

# Histogramme d'effectifs des 20 items les plus fréquents
itemFrequencyPlot(groceries_tr, topN=20)
itemFrequencyPlot(groceries_tr, topN=20,type="absolute")
itemFrequencyPlot(groceries_tr, support=0.10)

# Affichage des nombres réels avec 3 décimales
options(digits=3)

#---------------------------------#
# EXTRACTION D'ITEMSETS FREQUENTS #
#---------------------------------#

# Nombre d'itemsets potentiellement fréquents
2^ncol(groceries_tr)

# Extraction des itemsets fréquents pour minsupport = 2%
fi <- apriori(groceries, parameter = list(supp = 0.02, target = "frequent itemsets"))

# Affichage des itemsets fréquents dans 'fi'
inspect(fi)

# Affichage d'un histogramme des tailles des itemsets fréquents dans 'fi'
barplot(table(size(fi)), xlab="Taille itemset", ylab="Nombre")

# Tri des itemsets fréquents par ordre décroissant de leur support
fi <- sort(fi, by="support")

# Affichage des 30 itemsets les plus fréquents
inspect(head(fi, n=30))
# Commande alternative
inspect(fi[1:30,])

# Affichage des itemsets fréquents constitues de plus de deux items
inspect(fi[size(fi)>2])

#------------------------------------#
# EXTRACTION DE REGLES D'ASSOCIATION #
#------------------------------------#

# Extraction des règles d'association pour minsupport = 1% et minconfiance = 40%
rules1 <- apriori(groceries, parameter = list(supp = 0.01, conf = 0.4, target = "rules"))
inspect(rules1)

# Ordonnancement des règles par mesures de confiance et support respectivement
rules1 <- sort(rules1, by = c("confidence", "support"))
inspect(rules1)

# Règles d'association de taille maximale de 2 items pour minsupport = 1% et minconfiance = 40%
rules2 <- apriori(groceries, parameter = list(supp = 0.01, conf = 0.4, target = "rules", maxlen=2))
inspect(rules2)

# Règles d'association de taille maximale de 2 items pour minsupport = 0,1% et minconfiance = 50%
rules3 <- apriori(groceries, parameter = list(supp = 0.001, conf = 0.5, target = "rules", maxlen=2))
rules3 <- sort(rules3, by=c("confidence", "support"))
inspect(rules3)

# Ciblage de l'item 'Whole.milk=Y' en antécédent des règles
wm1 <- apriori(data=groceries, parameter=list(supp=0.01, conf=0.2), appearance=list(lhs="Whole.milk=Y"), control=list(verbose=F))
wm1 <- sort(wm1, by = c("confidence", "support"))
inspect(wm1)

# Ciblage de l'item 'Whole.milk=Y' en conséquence des règles
wm2 <- apriori(data=groceries, parameter=list(supp=0.01, conf=0.55), appearance=list(rhs="Whole.milk=Y"), control=list(verbose=F))
wm2 <- sort(wm2, by = c("confidence", "support"))
inspect(wm2)

#-----------------------------------------------#
# AFFICHAGES GRAPHIQUES DE REGLES D'ASSOCIATION #
#-----------------------------------------------#

# Installation/mise a jour de la librairies 'arulesViz'
install.packages("arulesViz")

# Activation de la librairies 'arulesViz'
library(arulesViz)

# Affichage des règles de 'wm1' sous forme de graphe
plot(wm1, method="graph", shading="confidence")

# Affichage des règles de 'wm2' sous forme de graphe interactif
plot(wm2, method="graph", shading="confidence", engine='interactive')

# Affichage des règles de 'wm2' sous forme de boulier
plot(wm2, method="grouped", shading="confidence", engine='interactive')

# Affichage des règles de 'wm2' sous forme de matrice
plot(rules3, method="matrix", shading="confidence", engine='interactive')

#-----------------------------#
# CHARGEMENT DES DONNEES IRIS #
#-----------------------------#

# Chargement du fichier CSV
iris <- read.table("Data Iris.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = T)

# Installation/mise a jour de la librairies 'ggplot2'
install.packages("ggplot2")

# Activation de la librairies 'ggplot2'
library(ggplot2)

# Nuages de points des longueur et largeur des sépales
qplot(Sepal.Length, Sepal.Width, data=iris, color = Species)

# Nuages de points des longueur et largeur des pétales
qplot(Petal.Length, Petal.Width, data=iris, color = Species)

# Définition de la suite aléatoire pour reproductibilité des résultats d'initialisation aléatoire
set.seed(1234)

#------------------------#
# CLUSTERING PAR K-MEANS #
#------------------------#

# Calcul des k-means pour k = 3 avec sélection de la meilleure parmi 20 initialisations 
km3 <- kmeans(iris[, 1:4], 3, nstart = 20)

# Ajout du numero de cluster d'affectation de chaque instance au data frame 'iris'
iris$Cluster <- as.factor(km3$cluster)

# Affichage des numeros de cluster d'affectation de chacune des 150 instances
print(iris$Cluster)

# Comparaison des effectifs des clusters et espèces
table(iris$Cluster, iris$Species)

# Nuage de points des longueur et largeur des sépales avec clusters en symboles
qplot(Sepal.Length, Sepal.Width, data=iris, pch=iris$Cluster) + geom_point(size = 3)

# Nuage de points des longueur et largeur des pétales avec clusters en symboles
qplot(Petal.Length, Petal.Width, data=iris, pch=iris$Cluster) + geom_point(size = 3)

#---------------------------------------------------#
# EVALUATION EXTERNE DES CLUSTERS SELON LES ESPECES #
#---------------------------------------------------#

# Histogramme des effectifs des clusters avec les classes en couleurs
qplot(iris$Cluster, data=iris, fill=iris$Species)

# Nuage de points des longueur et largeur des sépales avec espèces en couleurs et clusters en symboles
qplot(Sepal.Length, Sepal.Width, data=iris, color=Species, pch=iris$Cluster) + geom_point(size = 3)

# Nuage de points des longueur et largeur des pétales avec espèces en couleurs et clusters en symboles
qplot(Petal.Length, Petal.Width, data=iris, color=Species, pch=iris$Cluster) + geom_point(size = 3)
