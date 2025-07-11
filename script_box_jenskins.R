# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> IMPORTATION DES LIBRAIRIES >>>>>>>>>>>>>>>>>>>>>>>>
library(readxl)
library(tseries)
library(urca)
library(FinTS)
library(fGarch)
library(aTSA)
library(vars)
library(forecast)
library(mice)
library(seastests)
library(tidyverse)
library(psych)

# Definition du répertoire 
setwd("C:\Users\Julien\Desktop\JULIEN\Livre_ISE2\S2\SERIE_TEMP\NOTRE_TP_ST\REDACTION_PROJET\PROJET_ST_JULIEN_CHRISTELLE_LIANNE")
getwd()



# Importation de la base de donnees 


data=read_excel("growth.xlsx",sheet = 1, col_names = TRUE, col_types = NULL, na = "")


# Analyses préliminaires 

# Nature des variables 

glimpse(data)


# Statistiques descriptives 
describe(data$pib)

sd(data$pib)/mean(data$pib) # Coefficient de variations

# Transformation en series temporelle 
pib      = ts(data$pib, start = 1970, end = 2020)

###########################################################################

### ANALYSE UNIVARIEE DU PIB : Methode de Box et Jenskins

# Methodologie : 

# – Familiarisation avec les données,
# – Analyse préliminaire,
# – Identification ou spécification du modèle,
# – Estimation des paramètres,
# – Validation par tests ou test d’adéquation du modèle,
# – Prévision,
# – interprétation des résultats.

# 1- Familiarisation avec les données 

# Analyse descriptive : 

colSums(is.na(data)) # La formation brute du capital fixe contient de Valeurs manquantes


mean(pib)
median(pib)
sd(pib)
min(pib)
max(pib)

mean(pib)/sd(pib)  # Coefficient de Vatriation 

describe(pib) # Pour avoir le resume avec en plus le kurtosis et le skewness





boxplot(pib, col ="aquamarine")     # Un petit nombre de valeurs aberrantes

# Evolution du PIB  du Cameroun 
dev.new()
ts.plot(pib, col ="midnightblue", xlab = "Années", ylab = "Taux de croissance", lwd = 2)
abline(h=mean(pib), col="red")

# 2- Analyse préliminaire 

# - Etude de la stationnarité

# - Test de racines unitaires 

# Test de Dickey-Fuller
tseries::adf.test(pib, k=0) # on accpete H1, le PIB est stationnaire

# Dickey-Fuller augmente avec présence de tendance
st_moy = ur.df(pib,type="trend")
summary(st_moy)




# Avec dérive
st_var = ur.df(pib,type="drift")
summary(st_var)


####################################################################################

############## Resume ###########

# Tous ces resultats montrent que le PIB est bien stationnaire. Par conséquent la série 
# est intégré I(0).


# 3- Identification et spécification du modèle


# ACF pour identifier l'ordre du MA(q) et PACF pour identifier l'ordre du AR(p) :

dev.new()
spectrum(pib, main ="Périodogramme", xlab ="") # Densite spectrale


dev.new()
pacf(pib, main = "", xlab ="") # p = 1,2,3

dev.new()
acf(pib, main = "", xlab = "") # q = 1,2,3,4


# Modeles possibles : 

# AR(1) ; AR(2); AR(3);
# MA(1) ; MA(2); MA(3); MA(4);
# ARMA(1,1) ; ARMA(1,2); ARMA(1,3); ARMA(1,4);
# ARMA(2,1) ; ARMA(2,2); ARMA(2,3), ARMA(2,4); 
# ARMA(3,1) ; ARMA(3,2); ARMA(3,3), ARMA(3,4);


# 4- Modelisation : estimation des paramètres

########## AR #############

ar.1 = arima(pib, c(1,0,0))
summary(ar.1)
ar.1$coef

ar.2 = arima(pib, c(2,0,0))
summary(ar.2)
ar.2$coef

ar.3 = arima(pib, c(3,0,0))
summary(ar.3)
ar.3$coef

############### MA  ##############

ma.1 = arima(pib, order=c(0,0,1))
summary(ma.1)
ma.1$coef

ma.2 = arima(pib, c(0,0,2))
summary(ma.2)
ma.2$coef

ma.3 = arima(pib, c(0,0,3))
summary(ma.3)
ma.3$coef

ma.4 = arima(pib, c(0,0,4))
summary(ma.4)
ma.4$coef


########### ARMA #############

arma.1.1 = arima(pib, c(1,0,1))
summary(arma.1.1 )
arma.1.1$coef


arma.1.2 = arima(pib, c(1,0,2))
summary(arma.1.2)
arma.1.2$coef

arma.1.3 = arima(pib, c(1,0,3))
summary(arma.1.3)
arma.1.3$coef

arma.1.4 = arima(pib, c(1,0,4))
summary(arma.1.4)
arma.1.4$coef

################

arma.2.1 = arima(pib, c(2,0,1))
summary(arma.2.1)
arma.2.1$coef


arma.2.2 = arima(pib, c(2,0,2))
summary(arma.2.2)
arma.2.2$coef

arma.2.3 = arima(pib, c(2,0,3))
summary(arma.2.3)
arma.2.3$coef

arma.2.4 = arima(pib, c(2,0,4))
summary(arma.2.4)
arma.2.4$coef

####################

arma.3.1 = arima(pib, c(3,0,1))
summary(arma.3.1)
arma.3.1$coef


arma.3.2 = arima(pib, c(3,0,2))
summary(arma.3.2)
arma.3.2$coef

arma.3.3 = arima(pib, c(3,0,3))
summary(arma.3.3)
arma.3.3$coef

arma.3.4 = arima(pib, c(3,0,4))
summary(arma.3.4)
arma.3.4$coef

######################################

# 5– Tests de significativité des coefficients des modèles

abs(ma.1$coef/sqrt(diag(ma.1$var.coef))) > 1.96 # BON ##########
abs(ma.2$coef/sqrt(diag(ma.2$var.coef))) > 1.96 # NON
abs(ma.3$coef/sqrt(diag(ma.3$var.coef))) > 1.96 # NON
abs(ma.4$coef/sqrt(diag(ma.4$var.coef))) > 1.96 # NON 
abs(ar.1$coef/sqrt(diag(ar.1$var.coef))) > 1.96 # BON #########
abs(ar.2$coef/sqrt(diag(ar.2$var.coef))) > 1.96 # NON
abs(ar.3$coef/sqrt(diag(ar.3$var.coef))) > 1.96 # NON


abs(arma.1.1$coef/sqrt(diag(arma.1.1$var.coef))) > 1.96  # NON
abs(arma.1.2$coef/sqrt(diag(arma.1.2$var.coef))) > 1.96  # NON 
abs(arma.1.3$coef/sqrt(diag(arma.1.3$var.coef))) > 1.96  # NON
abs(arma.1.4$coef/sqrt(diag(arma.1.4$var.coef))) > 1.96  # NON

abs(arma.2.1$coef/sqrt(diag(arma.2.1$var.coef))) > 1.96  # NON
abs(arma.2.2$coef/sqrt(diag(arma.2.2$var.coef))) > 1.96  # NON
abs(arma.2.3$coef/sqrt(diag(arma.2.3$var.coef))) > 1.96  # NON
abs(arma.2.4$coef/sqrt(diag(arma.2.4$var.coef))) > 1.96  # NON

abs(arma.3.1$coef/sqrt(diag(arma.3.1$var.coef))) > 1.96  # NON
abs(arma.3.2$coef/sqrt(diag(arma.3.2$var.coef))) > 1.96  # BON #########
abs(arma.3.3$coef/sqrt(diag(arma.3.3$var.coef))) > 1.96  # NON
abs(arma.3.4$coef/sqrt(diag(arma.3.4$var.coef))) > 1.96  # NON


# Les meilleurs modeles sont : MA(1); AR(1) et ARMA(3,2)

# Calcul des criteres d'information 

# AIC
AIC(ma.1, k=log(44))
AIC(ar.1, k=log(44))
AIC(arma.3.2, k=log(44))

# BIC
BIC(ma.1)
BIC(ar.1)
BIC(arma.3.2)



# L'AIC et le BIC sont minimises par le ARMA(3,2), il s'agit de notre meilleur modele 


# 6- Analyses des résidus
res = arma.3.2$residuals

# Test de nullité de la moyenne
mean(res)
sqrt(var(res))

mean(res) > 1.96*sqrt(var(res)) # faux alors on accepte H0: moy = 0



###### Tests d'autocorrelation des erreurs #########"

# test de Box-Pierce
T = length(pib)
K = round(T/3)
K # K = 15
Box.test(res, lag=1, type="Box-Pierce") 
# p-value = 0.9647, on accepte H0: absence d'autocorrelation


# test de Ljung-Box
Box.test(res, lag=1, type="Ljung-Box") 
# p-value = 0.9647, on accepte H0: absence d'autocorrelation

par(mfrow = c(1,2))
acf(res, xlab = "Ordre", main = "") # c'est un bruit blanc
pacf(res, xlab = "ordre", main ="")

ts.diag(arma.3.2)

tsdiag(arma.3.2)

#######################  Tests de normalite ####################

# Test de jarque bera
jarque.bera.test(res) ## On accepte l'hypothese de normalite des residus



# Test d'adequation Kolmogorov
ks.test(res, rnorm) # Il y a adequation entre la loi des residus et la loi normale


# Test du skewness 
library(moments)
skewness(res)
agostino.test(res) # confirme la presence de Skewness


dev.new()     
hist(res, col="aquamarine", lwd=2, main="", xlab="Valeurs", ylab="Fr?quence")
qqnorm(res, col="blue", lwd=1)
qqline(res, lwd=2, col="red") # droite d'Henry
abline(h = 0)


################ Tests d'hétéroscédasticité #######################

white.test(res) # Les residus sont homoscedastiques

dev.new()
plot(res, col="midnightblue", type="l", lwd = 2)
abline(h = mean(res), col = "red")


plot(density(res), main ="", xlab ="", 
     col="midnightblue", type="l", lwd = 2)


######## Test de stabilité des coefficients (Test de Chow) ###################

# on va decouper la base en deux parties suivant un critere (choc). 

# Nous considérons un choc pour l'annee 

base_n1 = pib[1:25] # Si T <= 25 c'est a dire de 1970 à 1994

base_n2 = pib[26:51] # T > 25 c'est a dire de 1995 à 2020


# Coefficients du modele de base 
arma.3.2$coef


# Coefficients des nouveaux modeles 

# Modele 1 
model_n1 = arima(base_n1, c(3,0,2))
model_n1$coef
summary(model_n1)

# Modele 2
model_n2 = arima(base_n2, c(3,0,2))
model_n2$coef

# On calcul la SCR de chaque modele. Meme si elle sont deja integre dans la modelisation
# Pour des besoins de coherance on calcul nous meme. 

k   = 6 # nombre de parametres du ARMA(3,2)
T   =length(pib)
n.1 = length(base_n1)
n.2 = length(base_n2)


SCR   = (1/T)*(t(arma.3.2$residuals)%*%arma.3.2$residuals)
SCR

SCR.1 = (1/n.1)*(t(model_n1$residuals)%*%model_n1$residuals)
SCR.1

SCR.2 = (1/n.2)*(t(model_n2$residuals)%*%model_n2$residuals)
SCR.2

# on calcul la statistique du test : 

Fisher.theo = ((SCR-(SCR.1+SCR.2))/k)/((SCR.1+SCR.2)/((n.1+n.2)-2*k))
Fisher.theo    # suit une fisher a (k, n-2k) degre de liberte.

Fisher.lu = qf(0.95, k, (n.1 +n.2)-2*k) 

# F theo < F lu, on accpete donc H0, les coefficients sont sensiblement identiques
# Autrement dit, les coefficients sont stables.

# 7- Interpretation des resultats et Prévions 

library(stats)

# Previsions du PIB jusqu'en 2022

prevision = predict(arma.3.2, n.ahead=2)
prevision$se

# graphe des prévisions
library(forecast)
dev.new()

Arim
Arma.3.2 = arima(pib, c(3,0,2))
forecast(Arma.3.2, h=5)
forec
plot(forecast(Arma.3.2, h=5), col="red",
     main="", xlab="années",ylab="Taux de croissance du PIB", bty="7", lwd = 3)

# Intervalle de confiance 
born.inf = prevision$pred - 1.96*prevision$se
born.sup = prevision$pred + 1.96*prevision$se

##################### Test de d'heteroscedasticite conditionnelle : ARCH ############

library(FinTS)
ArchTest(res)
