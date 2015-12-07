# TD 3 :  Régression linéaire multiple
# 14/10/15

# 1) Préliminaires

setwd("/home/etudiant/Bureau")
gavote<-read.table(file = "/home/etudiant/Bureau/gavote.txt", header = TRUE)

# Accéder à une colonne, manipuler un objet
# On sélectionne des colonnes
gavote[1:10, c(2:7)]
# On regarde si des noms de colonnes correspondant à des strings
colnames(gavote) %in% c("equip", "gore", "rural")
# On sélectionne des colonnes par nom
gavote[1:10, colnames(gavote) %in% c("equip", "gore", "rural")]
# On prend l'exclusion de la sélection
gavote[1:10, -c(2:7)]

# Créer la variable undercount
gavote$undercount <- (gavote$ballots - gavote$votes)/gavote$ballots

# Créer la varioable pergore, perbush, perother
pergore <- gavote$votes/gavote$gore
perbush <- gavote$votes/gavote$bush
perother <- gavote$votes/gavote$other

head(subset(gavote, select = c("gore", "bush", "other"))/gavote$votes)
head(rowSums(subset(gavote, select = c("gore", "bush", "other"))/gavote$votes))

gavote[, colnames(gavote) %in% c("gore", "bush", "other")] <- subset(gavote, select = c("gore", "bush", "other"))/gavote$votes
colnames(gavote)[6:8] <- paste("per", c("gore", "bush", "other"), sep = "")
gavote$votes <- NULL
gavote$ballots <- NULL
head(gavote)

# 2) Analyse descriptive

# On regarde la moyenne pour les colonnes numériques
apply(gavote[,6:9], 2, summary)
apply(gavote[,6:9], 2, mean)

# Quelques représentations graphiques
plot(gavote$undercount)
boxplot(gavote$undercount~gavote$atlanta)
boxplot(gavote$undercount~gavote$rural)
boxplot(gavote$perbush~gavote$atlanta)

# Histograme des votes pas pris en compte par municipalité
hist(gavote$undercount, breaks = 40)

# On calcule la moyenne sur pour tout ce qui n'est pas un facteur dans la matrice/data frame
my.factor<- sapply(gavote, is.factor)
apply(gavote[, !my.factor], 2, mean)

# Visualisation des données sous Ggplot
#install.packages("ggplot2")
dplot <- cbind(stack(subset(gavote, select = c("pergore", "perbush", "perother"))), rep(gavote$econ, 3))
head(dplot)
ggplot(dplot, aes(x=factor(var), y=values, fill=factor(econ)))+ geom_boxplot()
ggplot(dplot, aes(x=econ, fill=factor(econ)))+ geom_boxplot()


# Corrélations : on représente un vecteur, on voit l'angle entre les 2 vecteur avec le cosinus.
#Si les deux sont très corrélés (fonction cor), on a cosinus proche de 1
cor(gavote$undercount, gavote$perAA) # Ici 0.2, peu corrélé



cor(gavote[, !my.factor])

# library GGally Librairie qui aide la représentation sous GGplot2
ggpairs(gavote[, my.factor])
ggpairs(gavote[, !my.factor])


heatmap(abs(cor(gavote[, !my.factor])))

# 3)

