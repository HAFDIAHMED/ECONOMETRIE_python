#vider la mémoire
rm(list=ls())

#Q.3 -- changement de dossier
setwd("c:/votre dossier de travail/...")

#Q.4 -- charger le fichier
m.prim <- read.table(file="autos.txt",header=T,sep="\t",dec=".")

#Q.5 -- nombre d'observations et de variables
print(nrow(m.prim))
print(ncol(m.prim))

#Q.6 -- liste des variables
print(str(m.prim))

#Q.7 -- stat descriptives
print(summary(m.prim))

#Q.8 -- stat pour price
print(mean(m.prim$price))
print(sd(m.prim$price))
print(quantile(m.prim$price,probs=c(0.25,0.75)))

#Q.9 -- fréquence fuel_type
print(table(m.prim$fuel_type))

#Q.10 -- fréquence aspiration
print(table(m.prim$aspiration))

#Q.11 -- tableau croisé
print(table(m.prim$fuel_type,m.prim$aspiration))

#Q.12 -- filtrage
m <- m.prim[m.prim$fuel_type=="gas" & m.prim$aspiration=="std" & m.prim$price < 30000,]
print(nrow(m))

#Q.13 -- nuage de points
plot(m$engine_size,m$price)

#Q.14 -- corrélation
ccor <- cor(m$engine_size,m$price)
print(ccor)

#Q.15 -- t calculé
tcor <- ccor/sqrt((1-ccor^2)/(nrow(m)-2))
print(tcor)

#p-value
print(2.0*(1.0-pt(abs(tcor),nrow(m)-2)))

#Q.16 -- transformation de Fisher
z <- 0.5*log((1+ccor)/(1-ccor))
print(z)

#quantile de la loi normale
u <- qnorm(0.975)
print(u)

#ecart-type de z
ecz <- sqrt(1/(nrow(m)-3))
print(ecz)

#intervalle de z
zb <- z - u * ecz
zh <- z + u * ecz

#transformation inverse
rb <- tanh(zb)
rh <- tanh(zh)

#intervalle
print(paste(rb,rh," , "))

#Q.17 -- transformation en rangs
rengine <- rank(m$engine_size)
rprice <- rank(m$price)

#graphique
plot(rengine,rprice)

#corrélation
print(cor(rengine,rprice))

#Q.18 -- voiture atypiques (petit moteur, prix élevé)
bizarre <- (rengine < 30 & rprice > 75)
print(m[bizarre,c("make","engine_size","price")])

#Q.19 -- graphique
plot(m$horsepower,m$price)

#Q.20 -- qui est ce point atypique
print(m[m$horsepower > 200,c("make","horsepower","price")])

#retrait du point atypique
m1 <- m[m$horsepower < 200,]

#Q.21 -- régression simple
r1 <- lm(price ~ horsepower, data = m1)
print(r1)

#Q.22 -- attributs
print(attributes(r1))

#vecteur des coefficients
print(r1$coefficients)

#Q.23 -- graphique avec droite de régression
plot(m1$horsepower,m1$price)
abline(r1$coefficients[1],r1$coefficients[2])

#Q.24 et 25 -- summary de la régression
sr1 <- summary(r1) 
print(sr1)

#Q.26 -- attributs
print(attributes(sr1))

#coefficients
print(sr1$coefficients)

#ses dimensions
print(dim(sr1$coefficients))

#Q.27 -- quantile de la loi de student
ttheo <- qt(0.95,nrow(m1)-2)
print(ttheo)

#intervalle de confiance
a.basse <- sr1$coefficients[2,1]-ttheo*sr1$coefficients[2,2]
a.haute <- sr1$coefficients[2,1]+ttheo*sr1$coefficients[2,2]
print(paste(a.basse,a.haute,sep=" , "))

#Q.28 -- valeurs ajustées
p1 <- r1$fitted.values

#min global
min.glob <- min(m1$price,p1)

#max global
max.glob <- max(m1$price,p1)

#graphique Y vs. Y^
plot(m1$price,p1,xlim=c(min.glob,max.glob),ylim=c(min.glob,max.glob))
abline(0,1)

#Q.29 -- graphique des résidus
plot(m1$price,r1$residuals)
abline(0,0)

#Q.30 -- graphique des résidus avec l'exogène en abscisse
plot(m1$horsepower,r1$residuals)
abline(0,0)

#Q.31 -- prédiction ponctuelle
pprice <- r1$coefficients[2] * 100 + r1$coefficients[1]
print(pprice)

#Q.32 -- variance de l'erreur de prédiction
n <- nrow(m1)
x_barre <- mean(m1$horsepower)
vprice <- (sr1$sigma^2)*(1+1/n+(100-x_barre)^2/sum((m1$horsepower-x_barre)^2))
print(vprice)

#quantile de la loi de Student
qs <- qt(0.95,n-1-1)
print(qs)

#bornes
bbprice <- pprice - qs * sqrt(vprice)
bhprice <- pprice + qs * sqrt(vprice)
cat("(",bbprice,',',bhprice,")")

#Q.33 -- matérialisation de l'intervalle
#graphique avec droite de régression
plot(m1$horsepower,m1$price)
abline(r1$coefficients[1],r1$coefficients[2])
#points à prédire
points(100,pprice,col="red",pch=19)
#intervalle
lines(c(100,100),c(bbprice,bhprice),col="blue")
lines(c(100,100),c(bbprice,bhprice),col="blue",type="b",pch=20)
