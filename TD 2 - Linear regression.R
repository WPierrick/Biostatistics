# TD Régression linéaire simple

# http://julien.cremeriefamily.info/teachings_M1GENIOME_Reg.html
setwd("/home/etudiant/Bureau")
brochet <- read.table("Brochet.txt", header = TRUE)

hist(brochet$age, breaks = 20)
ggplot(brochet, aes(x=Age, y=TxDDT))+geom_point()
libray(ggplot2)
ggplot(brochet, aes(x=Age, y=TxDDT))+ geom_boxplot()
pairs(brochet)
plot(brochet$Age,brochet$TxDDT)
with(brochet,plot(Age,TxDDT))# Equivalent de ggplot plus haut
with(brochet,plot(TxDDT~age))# Idem, équivalent ggplot
#with(brochet, tapply(TxDDT,Age,var) # On crée un vecteur numerique et une catégorie factorielle.
# On regarde pour chaque classe d'age

# 2) Modèle linéaire sous R
# a)
M1<- lm(TxDDT~Age, brochet) # Modèle linéaire sous R : Taux de DDT en fonction de l'age
# b)
# Pour evaluer à la main, c'est (covariance de l'age et du taux de ddt) divisé par (la variance de l'age) et on obtient le Beta0
residuals(M1)
str(M1)
with(brochet, cov(Age,TxDDT)/var(Age))
beta1 <- with(brochet, cov(Age,TxDDT)/var(Age))
beta0 <- with(brochet, mean(TxDDT) - mean(Age)*beta1)
beta1
beta0
attach(brochet)
summary(M1)
sum(residuals(M1)^2/(M1$df.residual))
sqrt(sum(residuals(M1)^2/(M1$df.residual)))
df.residual(M1)
nrow(brochet) - 2 # Def S  étoile
#Explication summary : Distribution des résidus
# Coefficients estimés : pour le décalage à l'originee -0.23 et la pente 0.17
# t value statistique de test de student (estimate / std error). C'est censé suivre une student sous H0
# Et donc on regarde la P valeur *** très significatif
# R ajuste : pourcentage de la variabilité expliqué par le modèle
# F statistique : test du modèle le plus simple (intercept) par le modèle le plus compliqué (intercept + age)
# Si on veut juste une analyse de la variance, anova de M1, avec le modèle le plus compliqué
predict(M1) # Valeurs prédites par le modèle
plot(brochet$TxDDT, predict(M1)) # On regarde les valeurs prédites avec les valeurs observées
abline(0,1)
predict (M1, newdata = data.frame(Age=10), intervals="prediction")
head(predict(M1, interval="conf"))
head(predict(M1, interval="pred"))


# d) A partir de brochet on représente en X age et Y tx ddt on fait un modele lineaire et on rajoute les points
ggplot(brochet, aes(x=Age,y=TxDDT)) + stat_smooth(method="lm",formula=y-x) + geom_point() 

# Idem qu'avec ggplot mais en le faisant à la main
plot(brochet$Age, brochet$TxDDT)
plot(brochet$Age, brochet$TxDDT, xlab="", ylab="")
abline(a=coefficients(M1)[1], b=coefficients(M1)[2])
lines(brochet$Age, predict(M1, interval = "conf")[, 3])
lines(brochet$Age, predict(M1, interval = "conf")[, 2])

# e) Plot des résidus
M2 <-lm(brochet$TxDDT ~ brochet$Age+1)
plot(M2, which=1:2) # On a des résidus qui sont bien par rapport au modèle, par contre le modèle n'est pas le bon

# 3) Modèle quadratique
# a-b)
M3 <-lm(brochet$TxDDT ~ I(brochet$Age^2))

# c)
summary(M3)
plot(M3)
anova(M3) # Analyse de la variance

# d)
install.packages("ggplot2")
library(ggplot2)
ggplot(brochet, aes(x=Age,y=TxDDT)) + stat_smooth(method="lm",formula=y~I(x^2)) + geom_point()

# 4) Modèle de transformation logarithmique
# a-b)
M4 <-lm(log(brochet$TxDDT) ~ I(brochet$Age^2))

# c)
summary(M4)
plot(M4)
anova(M4) # Analyse de la variance

# d)
ggplot(brochet, aes(x=Age,y=log(TxDDT)) + stat_smooth(method="lm",formula=y~I(x^2)) + geom_point()

# 5)       
# a-b)
M5 <-lm(log(brochet$TxDDT) ~ brochet$Age + I(brochet$Age^2))

# c)
summary(M5)
plot(M5)
anova(M5) # Analyse de la variance

# d)
ggplot(brochet, aes(x=Age,y=log(TxDDT))) + stat_smooth(method="lm",formula=y~x+I(x^2)) + geom_point()
 
