## install.packages("FactoMineR")
## install.packages("ade4")
library(FactoMineR)
library(ade4)

##############
# EXERCICE 1 #
##############

temperature <-read.table("http://factominer.free.fr/book/temperature.csv",
                         header=TRUE, sep=";", dec=".", row.names=1)
temperature <-read.table("temperature.csv", header=TRUE,sep=";", dec=".", row.names=1)

is.factor(temperature[,1])
lines(as.numeric(temperature[1,1:12]), ylim= range(temperature[,1:12]))
lines(as.numeric(temperature[2,1:12]))
lines(as.numeric(temperature[3,1:12]), col="red")
lines(as.numeric(temperature[4,1:12]), col="blue")

# Description des données
typeof(temperature[1,1:16])
dim(temperature)
names(temperature)
rownames(temperature)

#' La commande suivante effectue l'ACP du jeu de données. les individus 24 à 35 sont passés en individus supplémentaires (pas des capitales). On passe en variables supplémentaires toutes les variables qui ne sont pas des moyennes mensuelles 
#?PCA
res <- PCA(temperature,ind.sup=24:35,quanti.sup=13:16,quali.sup=17)
#' On trace le nuage des individus (capitales)
plot.PCA(res,choix="ind",habillage=17)
#' l'argument habillage permet de colorier les individus en fonction d'une variable supplémentaire (ici, 17 : Area, ie. la région (nord, sud, est, ouest). 
#' 1 - Comment interpréter les deux premiers axes au vu de ces résultats? 
#' --> un axe nord/sud et un axe est/ouest 

#' Le package permet une description `automatique' des axes à l'aide des variables (classement en fonction de la corrélation)
dimdesc(res)
#' 2 - Quelles sont les variables les plus corrélées aux deux premieres composantes ?
#' --> la température annuelle est la plus liée à l'axe 1 (nord/sud), et tous les mois sont positivement corrélés (effet taille) mais surtout ceux des saisons intermédiaires  
#' --> c'est l'amplitude qui est la plus correlee à l'axe 2 (climat continental / océanique) 

#' Les valeurs propres de la matrice de variance (valeurs, pourcentage, pourcentage cumulé) sont dans l'élément eig de la liste renvoyée par PCA
res$eig
#' 3 - Quelle est la  part de variance expliquée par les 2 premieres composantes? est-il utile de considérer les composantes suivantes pour ce jeu de données?
#' --> 98.3
#' --> non

#' Résultats sur les individus
res$ind
plot.PCA(res, choix = c("ind"), invisible=c("ind.sup", "quali", "quanti.sup"))
#' 4 - Donner deux capitales contribuant significativement à  l'axe 1, aux deux extrémités. Idem pour l'axe 2. 
#' --> regarder les contrib ou les cos2 elevees avec des signes opposés pour coord
#' --> axe 1: Athens,  Elsinki. Axe 2: Dublin/ Kiev ou budapest

res$ind.sup 
plot.PCA(res, choix = c("ind"), invisible = c("ind")) 
#' 5 - idem avec les individus supplémentaires (pas les capitales)
#' --> Seville / St Petersbrg, St Petersburg/ Edinburgh


#' Résultats sur les variables
plot.PCA(res, choix = "var")##, invisible = "quanti.sup")
res$var
#' 6 - Quelles sont les mois  contribuant le plus à l'inertie sur l'axe 1? sur l'axe 2?
#' Dans le package $\|a\| = \sqrt{lambda}$, les contributions sont le carré des coordonnées, divisés par la valeur propre de l'axe. 
#' --> axe 1: Octobre, Septembre, Avril; axe 2: Juin, Janvier.

ccoord <- res$var$coord[,1] # Première dimension = premier axe (Coordonnées dans l'espace des variables) ! 
ccor <- res$var$cor[,1] # Corrélation entre chaque variable et la composante correspondante à l'axe (identique aux coordonnées à cause de la norme de |a|)
# Pourquoi ? On a V*a = lambda*a <=> Xt*X*a = lambda*a <=> Xt*c = lambda*a -> Corrélation = prendre la j-ème coordonnée ! 
ccos2 <- res$var$cos2[,1] # Cosinus au carré = corrélation au carré ! 
ccontrib <- res$var$contrib[,1] # Contribution = Corrélation au carré / valeur propre de l'axe -> Attention, pourcentage (a été multiplié par 100)
# Avec le calcul qui suit, on vérifie bien que la contribution d'une variable à un axe correspond à sa corrélation avec cet axe au carré divisé par la valeur propre associée 
sum(ccor^2)
sum(ccontrib)
sum(ccoord^2) / res$eig[1,1]
ccoord^2/res$eig[1]  - ccontrib/100


# Résultats pour les variables quantitatives additionnelles
res$quanti.sup
plot.PCA(res, choix = c("var"), invisible = c("ind"))
#' 7 - à quelles composante principale pouvez vous rattacher prioritairement chacune des variables supplémentaires?
#' --> annual: 1, amplitude:2 , latitude:1, longitude:2. 

#' 8 - Concernant les  variables qualitatives supplémentaire: chaque catégorie est identifiée au barycentre des individus qui la possèdent. 
res$quali.sup
plot.PCA(res, choix = "ind", invisible = c("ind", "ind.sup"))
#' à quelle composante la catégorie 'East' est-elle le plus corrélée ? quel est le signe de cette corrélation? 
#' -> East : 2, positif. West : 2, négatif. North : 1, négatif. South : 1, positif.

#'9 - En se basant uniquement sur les deux premieres composantes de l'acp, peut-on anticiper le signe de la corrélation entre 'amplitude' et 'janvier' ?
res$var$coord[1,c(1,2)]   ## Janvier est positif sur l'axe 1 et négatif sur l'axe 2
res$quanti.sup$coord[2,c(1,2)] ## C'est l'inverse pour Amplitude
#'--> Le produit scalaire des 2 variables en ne prenant que les 2 axes principaux est négatif donc la corrélation négative
#' Confirmation
cor(temperature[1:23,1:16]) # correl de -0.76
#' Les pays avec hiver comparativement doux (température plus élevées) ont moins d'amplitude de température: climat océanique. 
         
         
#' En plus: Calculer sans utiliser le package les axes principaux et les valeurs propres.
#' Comparer avec la sortie de factominer
n <- 23
DD <- temperature[1:n,1:12] # Individus et variables à prendre en compte
cc <- apply(DD,2,mean) # 
X1 <- t(t(DD) - cc) # On centre les données
apply(X1,2,mean)    # Et on vérifie ..
vv <- apply(X1,2,function(v){sum(v^2)/n})
vv
Rsd <- apply(DD,2,sd)
Rsd -sqrt(vv) 
         
X2 <- t(t(X1)/sqrt(vv)) # On standardise les données
t(X2)%*%X2/n # Matrice de corrélation

# Autre méthode pour obtenir le même résultat avec les fonctions de R         
X <- scale(temperature[1:23,1:12])*sqrt(23/22) ## Standard deviation -> On utilise le dénominateur n-1
X2-X
apply(X,2,var)
nV <- t(X)%*%X
V <- nV/23 ## Matrice de corrélation 

# On peut maintenant calculer les valeurs/vecteurs propres
Veig <- eigen(V, symmetric=TRUE)

#' Valeurs propres
Veig$values
res$eig ## Ce sont les mêmes ! 
         
#' Première composante
C1 <- X %*% Veig$vectors[,1] 
C1 + res$ind$coord[,1] ## c1 est (hasard) l'opposé du vecteur renvoyé par factomineR
         
#' à l'aide des formules de transition on obtient le premier axe en utilisant la première composante fournie par factominer.
#'--> on utilise a = 1/lambda * t(X)Dc puis coord(Paris) = <Paris, a1>.
n <- 23
1/(res$eig[1] *n) * t(X) %*% res$ind$coord[,1]
Veig$vectors[,1]
1/(res$eig[1] *n) * t(X) %*% res$ind$coord[,1] + Veig$vectors[,1] ## Encore, a1 est (hasard) l'opposé du vecteur renvoyé par factomineR

##############
# EXERCICE 2 #
##############

##' Avec données JO:
#' Lignes: disciplines olympiques
#' Colonnes: pays
#' Chaque cellule : nombre de médailles (or/argent/bronze) gagnées par un pays entre 92 et 2008 (5 JO, donc 15 medailles par discipline)
#' Données originales: chaque lignes = 1 médaille, 2 chiffres 1 (pays concerné et discipline). on a bien une table de contingence (creuse)
#' Il y a 24 disciplines. 
data(JO)
?JO

#' On verifie qu'il y a bien 15 medailles par discipline:  
apply(JO, 1, sum)
#' La table de contingence est particulière car chaque profil ligne a le même poids 
resJO <- CA(JO)
summary(resJO)
#' Le test du chi2 rejette largemement l'indépendence, mais n'est pas significatif car la condition n_{ij} > 5 pour tout (i,j)  n'est pas satisfaite. 
#' Profils lignes
rowprof <- JO / apply(JO, 1, sum)
apply(rowprof,1,sum) # On vérifie que les lignes somment à 1

#' Profils colonnes
colprof <- t(t(JO) / apply(JO, 2, sum))
apply(colprof,2,sum) # On vérifie que les colonnes somment à 1

#' Inspectez les valeurs propres de l'analyse. Combien faudrait-il garder de dimensions pour expliquer 50% de la variance? 
round(resJO$eig,1)
min(which(resJO$eig[,3]>=50))
#' 2 - Vérifiez que l'inertie totale  = 1/n * stat
chisq.test(JO)
n <- sum(JO) 
n * sum(resJO$eig[,1]) # n * somme des valeurs propres = résultat du test !  

#' Coordonnées des lignes sur les axes
resJO$row$coord

#' poids: fréquences marginales
rowW <-  apply(JO,1,sum)/n
rowW # Les lignes ont les mêmes poids - c'est logique, même nombre de médailles par discipline
colW <-  apply(JO,2,sum)/n
colW # Mais pas par pays ...

#' 3 - Vérifiez que le  barycentre des profils lignes est le profil marginal
sum((t(rowW) %*% rowprof - colW)^2) # Barycentre des profils lignes (somme pondérée par les poids des lignes) = poids des colonnes !

#' .. et vérifiez que le barycentre des projections des profils lignes sur les axes est le vecteur nul)
t(rowW) %*% resJO$row$coord # Somme pondérée des coordonnées des lignes dans les axes (=composantes) est le vecteur nul 

#' 4 - Variance pondérée des coordonnées des lignes sur le premier axe = Variance de la première composante =  lambda1 ?
sum(t(rowW) * resJO$row$coord[,1]^2 )
resJO$eig[1,1]

#' 5 - Covariance entre les coordonnées des lignes sur les 2 axes ?
sum(t(rowW) * resJO$row$coord[,1] * resJO$row$coord[,2]) # Produit scalaire (pondéré par les profils lignes !) entre les deux premières composantes.
## C'est parce que les axes obtenus sont orthogonaux ! 

##' 6 - Contributions des lignes aux axes
resJO$row$coord # Coordonées des lignes sur les axes 
rowW * resJO$row$coord[,1]^2 / resJO$eig[1,1] # Au carré, multipliées par les poids des lignes = contributions !
sum(rowW * resJO$row$coord[,2]^2 / resJO$eig[2,1]) # On fait la somme des contributions divisées par les valeurs propres -> Total à 1 !
rowW * resJO$row$coord[,1]^2 / resJO$eig[1,1]- resJO$row$contrib[,1]/100 # On retrouve le même résultat directement ici ! (Attention, contrib est en pourcentage ici aussi.)

#' 7 - Interprétation des résultats
#' Axes de l'acp lignes à partir des contributions des modalités lignes ?
plot.CA(resJO, invisible= "col")
#' Axe 1: Disciplines de courses d'endurance contre le reste (avec gradient pour l'endurance)
#' Axe 2: Lancers et marche  contre sprint (avec gradient pour le sprint). 

#' Rajouter les pays. Peut-on dresser des profils de pays en termes de points forts disciplinaires ? 
plot.CA(resJO)

# Coordonnées des pays sur les deux axes:
dimdesc(resJO)$"Dim 1"$col
dimdesc(resJO)$"Dim 2"$col
#' --> eri, eth, bdi, mar.
#' Erithrée, Kenya, Ethiopie: à gauche de l'axe 1 (avec les courses d'endurance) !

#' Quantitativement: 
#' on vérifie comment sont construites les contributions des colonnes (pays) à l'axe.
#' -> calculer les contributions des pays à l'axe 1 en utilisant leurs poids, les coordonnées des colonnes et la première valeur propre. 
ctrs <- colW * resJO$col$coord[,1]^2/resJO$eig[1,1]
ctrs - resJO$col$contrib[,1]/100  # On vérifie que c'est bien le pourcentage donné par contrib

#' Contributions les plus importantes des pays à l'axe 1 ?  
ctrs[rev(order(ctrs))] 
#' Les pays ayant remporté le plus de médailles en endurance / autres:
#' ken          eth          mar          usa          gbr
#' estc-ce que usa est du côté de endurance ou de 'autres' ? 
which(colnames(JO)=='usa') # USA est la colonne 57
#' 57
resJO$col$coord[57,]
#'--> Même si USA contribue beaucoup au côté endurance, le pays se situe de l'autre côté de l'axe
#' Idem avec gbr
which(colnames(JO)=='gbr')
resJO$col$coord[23,]

##############
# EXERCICE 3 #
##############

#' on charge les données 'banque' du package ade4 
data(banque)
dim(banque)
names(banque)
head(banque)
?banque

#' Ces données
## résultent d’une enquête auprès de 810 clients d’une banque et décrivent les
## clients suivant certaines caractéristiques. Nous retenons ici les variables
## age, sexe, interdit,  credhab, credcon  (4,5,6,12,13)

#' 1 - Représenter les données pour chacune des variables.
plot(banque$age)
plot(banque$sexe)
plot(banque$interdit)
plot(banque$credcon)
plot(banque$credhab)
## 2 - Le tableau disjonctif est donné par la commande acm.disjonctif.
## (a) Sans calcul, donnez la somme totale du tableau et de chaque ligne.
## (b) Étudiez les effectifs de chaque modalité, et repérez les éventuelles
## modalités rares.
ids <- c(4,5,6,12,13)
idSup <- setdiff(1:21, ids)
tabdisj <- acm.disjonctif(banque[,ids])
colnames(tabdisj)
head(tabdisj)
#' -> Somme des lignes = nombre de variables du tableau = 5
#' -> Somme totale = nombre d'individus *5 
apply(tabdisj,1,sum)
#' On visualise les effectifs des colonnes
apply(tabdisj,2,sum)
colnames(tabdisj)
#' Modalités rares?
#'--> interdit: "oui", crédit habitation: "oui" , crédit conso: "fai" et "for", age: "< 25"

#' 3 - Effectuer l'ACM avec factoMineR

resMCA <- MCA(banque[,ids])
#' Que représentent les 3 graphes ?
#' --> 1) Carte des catégories,
#'     2) Carte des individus,
#'     3) Carte des corrélations au carré  entre variables et composantes. 

#' Calculez l’inertie totale:
sum(resMCA$eig[,1]) # Inertie totale = Somme des valeurs propres
chisq.test(tabdisj)$statistic / sum(tabdisj) # Vérification similaire à l'exercice 2 
#' Contributions des modalités de chaque variable à l'axe 1 ? 
ctrs <- resMCA$var$contrib[,1] # Pourcentages !
#' Quelles catégories contribuent le plus a l'inertie de l'axe 1 (3 premieres) ? 
ctrs[rev(order(ctrs))]
#' --> credit habitation, femme, age <25
#' idem pour axe 2?
ctrs <-  resMCA$var$contrib[,2]
ctrs[rev(order(ctrs))]
#' interdit oui, age <35, age <75


#' Dressez une typologie des clients, ie. interprétez les axes à l'aide de la varte des catégories et des variables
#' Axe 1: clients avec crédit habitation, gros crédit conso, hommes, ages actifs (45,55) ( = très consommateurs de servies bancaires) contre jeunes, femmes, retraites, en interdit banquaire (peu consommateurs)
#' Axe 2: clients à risque (interdit, age <35, petit crédit conso)  contre peu risqués (pas d'interdit, retraités, gros crédit)

#' Refaire l'analyse en passant en argument de MCA toutes les variables et en précisant que les variables autres que celles considérées ci-dessus sont des variables supplémentaires. Cela confirme-t-il l'interpretation des resultats?

resMCA <- MCA(banque, quali.sup = idSup)
graphics.off()
plot.MCA(resMCA, choix = "ind", invisible = "ind", selectMod = "cos2 30", unselect = "gray50")
#'--> axe 1: assurance vie, agriculteurs à droite(consommateurs),
#' étudiants, inactifs, à gauche (peu consommateurs)
#' --> axe 2: ouvriers,  soldes négatifs (n1) en haut,  retraités, compte d'epargne en bas. 