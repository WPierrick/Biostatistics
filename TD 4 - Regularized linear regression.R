# TD 4 Régression régularisée
# 05/10/15

setwd(dir = "/home/etudiant/Bureau/")
getwd()
install.packages("FactoMineR")
library(FactoMineR)
install.packages("GGally")
library(GGally)

# Chargement du jeu de données
diabetes <- read.table("diabetes_simple_effects.txt", header = TRUE)
head(diabetes)

# 1.1) Création des sous ensembles de données
# Dans le subset, le nom du data frame
#le sous ensemble de lignes égales à vrais
# On précise le nom des colonnes qu'on conserve ou qu'on veut pas
diabetes.train <- subset(diabetes,train, -train)
diabetes.test <- subset(diabetes,!train, -train)

diabetes.sub <- subset(diabetes, train & age > 45, -train)

# 1.2) Analyse descriptive
hist(diabetes.train)
plot(diabetes.train)
hist(diabetes.train)
x <- diabetes.train[, -1] # Variable explicatives
y <- diabetes.train[, 1] # Variable à expliquer

# Distribution de la variable à expliquer
par(mfrow=c(1,2))
hist(y, freq = FALSE, xlab = "", col="cyan", main = "")
lines(density(y), col="red")
plot(ecdf(y), xlab = "", main = "")
title(outer=TRUE, main="\n Distribution de la progression de la maladie")

# ACP sur les descripteurs
pca.out <- PCA(x, graph = FALSE)
par(mfrow = c(1,2))
plot(pca.out, choix = "var", axes = c(1,2))
bbarplot(pca.out$eig$"Percentage of variance")

# Graphes pair à pair
ggscatmat(x, columns=c(1,3:6))

# Corrélation entre les prédicteurs
heatmap(abs(cor(x)))

# Autres plots
plot(diabetes.train$prog[order(diabetes.train$prog)])


# 2.1) Régression linéaire multiple
M1<- lm(diabetes.train)
par(mfrow=c(2,2))
plot(M1)
summary(M1)
plot(M1)
anova(M1)


null<- lm(prog ~1, diabetes.train) #  On estime prog en fonction de l'intercept (comme une équation linéaire)
full<- lm(prog ~ ., diabetes.train) # On estime prog en fonction de tous les autres parametres (tous les prédicteurs)

anova(null,full)
summary(full)

par(mfrow=c(2,2))
plot(full, which = c(1,2,3,4))

# 2.2) Recherche exhaustive
library(leaps)
p<- ncol(diabetes.train) - 1
out <- regsubsets(prog ~ ., data = diabetes.train, nvmax = p, nbest = 2^p, really.big = TRUE)
# Nbest : nombre de modèles que je veux qu'il me renvoie
# Nvmax : nombre de variables max
bss <- summary(out)

# On récupère la taille de tous les modèles pour pouvoir faire le meilleur modèle pour chaque taille de modèle (Somme des carrés ajustés, BIC, R2, CP)
# On découpe tout par classe de modèle et on prend le min ou max
bss.size <- as.numeric(rownames(bss$which))
best.rss <- tapply(bss$rss, bss.size, min)
best.bic <- tapply(bss$bic, bss.size, min)
best.ar2 <- tapply(bss$adjr2, bss.size, max)
best.cp <- tapply(bss$cp, bss.size, min)

#Somme des carrés résiduels pour tous les modèles possibles (Residual Sum of Square)
par(mfrow=c(2,2))

# On fait 4 graphes qui correspondent à chacun de mes critères
#On a 4 graphes avec l'évolution de la somme des caarrés résiduels, le BIC qui remonte (bst modèle à 6 variables), le R2 qui est proche du BIC et CP aussi
plot(1:p, best.rss, ylim=range(bss$rss), col="red", type = "l", main = "RSS")
points(bss.size, bss$rss, col="grey", pch = 20)

plot(1:p, best.bic, ylim=range(bss$bic), col="red", type = "l", main = "BIC")
points(bss.size, bss$bic, col="grey", pch = 20)

plot(1:p, best.ar2, ylim=range(bss$adjr2), col="red", type = "l", main = "R2")
points(bss.size, bss$adjr2, col="grey", pch = 20)

plot(1:p, best.cp, ylim=range(bss$cp), col="red", type = "l", main = "Cp")
points(bss.size, bss$cp, col="grey", pch = 20)

# 2.3) Sélection Stepwise

n <- nrow(diabetes.train)

# On donne le range des estimate entre 0 et tous les criteres (null et full)
scope <- list(lower = terms(null), upper = terms(full))
# k : nombre de pénalité du critère : 2 par défaut pour une AIC, pour une BIC
fwd.AIC <- step(null, scope, direction = "both", k=2, trace=FALSE)
fwd.BIC <- step(null, scope, direction = "both", k=log(n), trace=FALSE)

anova(fwd.AIC)
anova(fwd.BIC)

summary(fwd.AIC)
summary(fwd.BIC)

# 3) Méthodes pénalisées
# 3.1) Régression Ridge

install.packages("glmnet")
library("glmnet")
x <- diabetes.train[, colnames(diabetes.train) != "prog"]
y <- diabetes.train[, colnames(diabetes.train) == "prog"]
x <- as.matrix(x)
ridge <- glmnet(x,y,alpha = 0)
lasso <- glmnet(x,y,alpha = 1)
par(mfrow=c(1,2))
plot(ridge, label=TRUE)
plot(lasso, label=TRUE)

# On veut trouver un seul modèle de la régularisation qui semble le mieux
# On prend les valeurs de lambda qui rendent minimales les erreurs de prédiction
# En faisant ça, on peut sélectionner le modèle le plus pertinent

cv.ridge <- cv.glmnet(x, y, nfolds = 10, alpha = 0)
cv.lasso <- cv.glmnet(x, y, nfolds = 10, alpha = 1)

par(mfrow=c(1,2))
plot(cv.lasso)
plot(cv.ridge)
# La premiere barre verticale erreur minimale
# La seconde c'est la valeur de lambda qui reste avec le moins de variables
# (lambda se (dans one standard error) )), une sorte de compromis entre les deux
# On prend l'erreur de lambda qui rend minimale l'erreur de l'erreur de prédiction
# Les nombres en haut sont les degrés de liberté (nombre de paramètres utilisés dans le modèle)
# Pour le lasso, le lambda est à quel point on régularise. Lambda très grand pour une variable, plus on ajoute des modèle, plus on s'ajuste
# C'est dans ce graphe qu'on choisit le compromis biais / variance.
# Il y a une plage de valeurs de lambda. Pour voir la valeur lambda min, cd.lasso lambda min et labmda.se
cv.lasso$lambda.min
cv.lasso$lambda.1se

# On dose le niveau de 2 facons différentes. Ridge moyenne des var corrélées, lasso sélection de variable
# On estime comment evolue l'erreur de prédiction le long des valeurs de lambda avec la validation croisée
# On choisi une valeur de lambda en fonction
par(mfrow=c(2,2))
plot(ridge, main="Ridge", xvar="lambda")
abline(v=c(log(cv.ridge$lambda.min), log(cv.ridge$lambda.1se)))
plot(lasso, main="Lasso", xvar="lambda")
abline(v=c(log(cv.lasso$lambda.min), log(cv.lasso$lambda.1se)))
plot(cv.ridge)
plot(cv.lasso)
# Ici on a 4 variables qui expliquent de manière importante la progression de la maladie

# En utilisant la fonction predict de glmnet
coef(cv.lasso$glmnet.fit, s=cv.lasso$lambda.lse)

# Le lasso fait la sélection de variable, la ridge assemble ensemble les prédicteurs

# AIC/BIC/nBIC
# On veut calculer l'erreur d'apprentissage pour toutes les valeurs d'apprentissage ajustés du lasso en fonction du lambda
p <- ncol(diabetes.train) - 1 # p c'est le nombre de prédicteurs
erD.lasso <- colMeans((y - predict(lasso,x))^2)

plot(lasso$lambda, n*log(erD.lasso), type = "l",log = "x")
BIC <- n * log(erD.lasso) + log(n)* lasso$df
AIC <- n * log(erD.lasso) + 2 * lasso$df
nBIC <- n * log(erD.lasso) + (log(n)+2*log(p)) * lasso$df
lines(lasso$lambda, BIC, col="red", lty=2)
lines(lasso$lambda, AIC, col="blue", lty=3)
lines(lasso$lambda, nBIC, col="green", lty=4)
legend("topleft", c("ErrD", "BIC", "AIC", "nBIC"), lty = 1:4, col = c("black","red", "blue", "green"))
# Tous les criteres ont le terme en noir en commun (erreur sur l'ensemble d'apprentissage
# Mais les autres courbes rajoutent des termes pour faire un compromis entre 
# "A quel point je suis bon sur le modèle d'apprentissage" et "à quel point mon modèle n'est pas trop compliqué"

# Choix de modèles pour la ridge et le lasso
lambda.lasso.min <- cv.lasso$lambda.min
lambda.lasso.lse <- cv.lasso$lambda.lse
lambda.lasso.BIC <- lasso$lambda[which.min(BIC)] # L'indice d'un vecteur donne la valeur minimale d'entrée du vecteur
lambda.lasso.nBIC <- lasso$lambda[which.min(nBIC)]
lambda.ridge.min <- cv.ridge$lambda.min
lambda.ridge.lse <- cv.ridge$lambda.lse

# 4) Evaluation des modèles sur l'ensemble Test
y.test <- diabetes.test$prog
#On veut la matrice des prédicteurs. prog c'est notre vecteur de réponse
x.test <- as.matrix(diabetes.test[, colnames(diabetes.test) != "prog"])

# Predict c'est notre y chapeau, notre prédiction
predict(lasso, x.test, s=lambda.lasso.min)
#plot(predict(lasso, x.test, s=lambda.lasso.min)
err.lasso.min <- mean(y.test - predict(lasso, x.test, s=lambda.lasso.min)^2)
err.lasso.lse <- mean(y.test - predict(lasso, x.test, s=lambda.lasso.lse)^2)
err.lasso.BIC <- mean(y.test - predict(lasso, x.test, s=lambda.lasso.BIC)^2)
err.lasso.nBIC <- mean(y.test - predict(lasso, x.test, s=lambda.lasso.nBIC)^2)


# Ces courbes sont faisables pour les méthodes qui calculent des paramètres : ridge et lasso
# Courbe d'évolution de l'erreur de prédiction
# On confronte les lambdas estimés avec l'erreur de test
# Les traits correspondent à des modèles 
err.lasso.test <- colMeans((y.test - predict(lasso, x.test))^2)
plot(lasso$lambda, err.lasso.test, log = "x", type = "l")
abline(v=c(lambda.lasso.min, lambda.lasso.lse, lambda.lasso.BIC, lambda.lasso.nBIC), lty = 1:4, col = 1:4)
legend("topleft", c("CV.min", "CV.lse", "BIC", "nBIC"), lty = 1:4, col = 1:4)

# On calcule les erreurs de test pour les autres méthodes
err.ridge.min <- mean((y.test - predict(ridge, x.test, s=lambda.ridge.min))^2)

### PARTIE A MODIFIER 
'''
plot(lasso$lambda, err.lasso.test, log = "x", type = "l")
abline(v=c(lambda.lasso.min, lambda.lasso.lse, lambda.lasso.BIC, lambda.lasso.nBIC), lty = 1:4, col = 1:4)
legend("topleft", c("CV.min", "CV.lse", "BIC", "nBIC"), lty = 1:4, col = 1:4)

err.lasso.test <- colMeans((y.test - predict(lasso, x.test))^2)
plot(lasso$lambda, err.lasso.test, log = "x", type = "l")
abline(v=c(lambda.lasso.min, lambda.lasso.lse, lambda.lasso.BIC, lambda.lasso.nBIC), lty = 1:4, col = 1:4)
legend("topleft", c("CV.min", "CV.lse", "BIC", "nBIC"), lty = 1:4, col = 1:4)
### PARTIE A MODIFIER

plot(ridge$lambda, err.ridge.test)

# On applique la méthode prédict a l'objet fwd.AIC
err.fwd.AIC <- mean ((y.test - predict(fwd.AIC, diabetes.test))^2)
err.fwd.BIC <- mean ((y.test - predict(fwd.BIC, diabetes.test))^2)
err.null <-  mean ((y.test - predict(fwd.AIC, diabetes.test))^2)
err.full <-  mean ((y.test - predict(fwd.AIC, diabetes.test))^2)

# On met dans un vecteur toutes nos erreurs
results <- c(fwd.AIC = err.fwd.AIC,
             fwd.BIC = err.fwd.BIC,
             null = err.null,
             OLS = err.full,
             lasso.cv.min = err.lasso.min,
             lasso.cv.lse = err.lasso.lse,
             lasso.BIC = err.lasso.BIC,
             lasso.nBIC = err.lasso.nBIC,
             ridge.cv.min = err.ridge.min,
             ridge.cv.lse = err.ridge.lse)