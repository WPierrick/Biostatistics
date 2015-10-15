# TD 2 : Régression linéaire simple

# Utiliser RMarkdown, pour sortir un fichier avec mise en forme
# Dans RMarkdown, Ctrl / Alt / i pour créer une balise de code
# Utiliser des ### Pour les niveaux des titres **bold** et *italic*


# Révisions

# 1) Manipulation de vecteurs
k <- 0:20
factorial(k)
x<-2^k/factorial(k)
cumsum(x) # Ca dit compbien pour chaque indic de la somme, quelle est la valeur courante de la somme
x < 1e-8
x[x> 1e-8]
sum(x[x > 1e-8])


# 2) Simuler les données
hist(rnorm(n=10000, mean=2, sd=1), breaks = 100)
n <- 100 # Taille échantillon
X <- rnorm(n, sqrt(2)) #
#X <- rnorm(n, mu.x, 1)
#Bruit gaussien les moyennes ne sont pas corrélées entre elles et les variances normales
epsilon <- rnorm(n,0, 0.1) # Variance du bruit de 1/10eme
#Y <- X * 9.8 + epsilon
Y <- X*9.8 + rnorm(100,0,5)
plot(X,Y)

# 3) Lire et écrire un fichier
donnees <- data.frame(x=X, y=Y)
write.table(donnees, file="monfichier.txt") #
read.table(file="nomfichier.txt") # Il lit directement ce qu'il y a dans le fichier


# 4) Lire et écrire un fichier de données
save(donnees, file="monfichier.Rdata") # Il sauvegarde non pas le contenu du tableau de données, mais le tableau avec son nom
load("monfichier.RData")
head(donnees)

# 5) Nuage de points
#ggplot travaille avec les data frame. On spécifie les variables qu'on veut réprésenter.
# On demande de représenter les données selon une certaine géométrie
qplot(x,y,geom="point")

# 6) Histogramme
installed.packages("ggplot2")
library(ggplot2)
ggplot(donnees)

# 7) Boucle for
#Loi de Khi2 a 1 ddl c'est une loi normale centrée réduite
n <- 10000; n <- rchisq(n, df=6) # 6 DDL
est <- vector("numeric",n)
head(est)
for (i in 1:n) {
  est[i]<- mean(x[1:i])
} # On recalcule pour chaque valeur de n

plot(1:n, est)
est2 <- cumsum(x)/1:n