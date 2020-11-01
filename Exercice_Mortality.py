# -*- coding: utf-8 -*-

# ** Importation et vérification des données

#Q.3 -- indiquer le répertoire de travail
import os
os.chdir("D:/_Travaux/university/Cours_Universite/Supports_de_cours/DataAnalysis/RegressionMultiple/PublicationWeb/TD/Seance_5")

#vérifier
print(os.getcwd())

#Q.4 -- Chargement des données, utilisation du package pandas.
import pandas

#version de pandas
print(pandas.__version__)

#Q.5 -- lecture du fichier -- attention aux options
df = pandas.read_excel("mortality.xlsx",sheet_name="data", header=0,index_col=0)

#Q.6 -- affichage des premières lignes
print(df.head())

#dimension
print("dimension du jeu de données : ",df.shape)

#vérification du type des données
print("liste des variables : ",df.info())

#statistiques descriptives
print(df.describe())

#labels
print(df.index)

#Q.7 -- graphique croisant toutes les variables
#il y a des points atypiques notamment sur NOX et POP
#il y a des variables corrélées avec MORTALITY, par ex. JULYTEMP(+), RAIN (+), EDUCATION (-)
pandas.plotting.scatter_matrix(df)

# **Q.8 à 10 -- Régression linéaire multiple et inspection des résultats. Affichage des principaux indicateurs de la régression. **
#importation de statsmodels
import statsmodels.formula.api as smf


#instanciation de l'objet régression
reg = smf.ols("mortality ~ julyTemp+rain+education+pop+income+nox", data = df)

#estimation des paramètres
res = reg.fit()

#affichage des résultats
print(res.summary())
# ** Vérification de la normalité : La p-value de Jarque-Bera [Prob(JB) = 0.416] est supérieure à 10%. Les résidus sont compatibles avec l'hypothèse de normalité. **

#Q.11 -- qq plot
import statsmodels.api as sm
sm.qqplot(res.resid)

#*** graphiques des résidus ***
#Q.12 -- endogène vs. résidus -- problème visiblement
#on continue nous mais dans une étude réelle il faudrait s'en inquiéter
import matplotlib.pyplot as plt
plt.scatter(df.mortality,res.resid,c="red")

#Q.13 -- chaque exogène vs. résidus
#inspecter s'il y a des formes de régularités
#voir aussi s'il y a des points atypiques
for j in range(1,7):
    plt.scatter(df.iloc[:,j],res.resid)
    plt.show()

#Q.14 -- *** nullité simultanée de pop, income, nox - solution (a) ***
import numpy as np
#matrice des coef. à tester
R = np.array([[0,0,0,0,1,0,0],[0,0,0,0,0,1,0],[0,0,0,0,0,0,1]])
#test F
print(res.f_test(R))

#Q.14 -- *** nullité  simultanée de pop, income, nox - solution (b) ***
#régression sans les variables à tester (régression sous H0)
reg0 = smf.ols("mortality ~ julyTemp+rain+education", data = df)
res0 = reg0.fit()
#comparaison des R2
n = df.shape[0]
p = df.shape[1]-1
q = 3

#stat de test - comp. des R2 avec (res) et sans (res0) les variables à tester
F = ((res.rsquared-res0.rsquared)/q)/((1-res.rsquared)/(n-p-1))
print(F)

#p-value
from scipy.stats import f
print(1-f.cdf(F,q,n-p-1))

#Q.15 -- affichage des leviers, résidus studentisés
infl = res.get_influence()
print(infl.summary_frame().filter(["hat_diag","student_resid"]))

#Q.16 -- représentation graphique
sm.graphics.influence_plot(res)

#Q.17 -- nombre d'observations
n = df.shape[0]
print("nb observations : ",n)

#nombre d'explicatives
p = df.shape[1]-1
print("nb exogènes : ",p)

#*** atypiques au sens du levier ***
#levier des observations
levier = infl.hat_matrix_diag
print("valeurs leviers\n",levier)

#seuil
seuil_levier = 2*(p+1)/n
print("seuil pour levier : ",seuil_levier)

#atypiques
atyp_levier = levier > seuil_levier
print("villes atypiques levier\n",df.index[atyp_levier])

#Q.18 -- *** atypiques au sens du résidu studentisé ***
#résidu studentisé
rstud = infl.resid_studentized_external
print("rstudent\n",rstud)

#seuil
import scipy
seuil_rstud = scipy.stats.t.ppf(0.95,df=n-p-2)
print("seuil 10% rstudent (bilatéral) : ",seuil_rstud)

#détection
import numpy as np
atyp_rstud = np.abs(rstud) > seuil_rstud

#affichage
print("villes mal modélisées rstudent\n",df.index[atyp_rstud])

#Q.19 -- pbm sur l'un des 2 critères (combinaison avec OU)
pbm = np.logical_or(atyp_levier,atyp_rstud)

#affichage
print("pbm sur l'un des 2 critères\n",df.index[pbm])

#Q.20 -- nouvel ensemble de données
dfclean = df.iloc[np.logical_not(pbm),0:7]
print("nouvelles dim data : ",dfclean.shape)

nclean = dfclean.shape[0]
print("nclean = ",nclean)

#Q.21 -- ** Relancer la régression après avoir évacué les observations à problème (10 obs., ça fait beaucoup à enlever quand même, on voit bien ici que ce type d'approche est très discutable)**

#instanciation de l'objet régression
reg2 = smf.ols("mortality ~ julyTemp+rain+education+pop+income+nox", data = dfclean)

#estimation
res2 = reg2.fit()

#Q.22 -- affichage des résultats
print(res2.summary())


#Q.23 -- ** Détection de la colinéarité. Utilisation du critère VIF.**

#matrice des corrélations
import scipy
mc = scipy.corrcoef(dfclean.iloc[:,1:7],rowvar=0)
print(mc)

#VIF
vif = np.linalg.inv(mc)
print(np.diag(vif))


#Q.24 -- **Tester la nullité simultanée de 2 coefficients associés aux variables julyTemp et income. On peut passer par le calcul matriciel (cf. TUTO, page7). Mais on peut aussi par une simple comparaison de R2 de 2 régressions : avec et sans les variables incriminées (cf. livre "Econométrie", section 10.4).**

#régression sans julyTemp et income
reg3 = smf.ols("mortality ~ rain+education+pop+nox", data = dfclean)
res3 = reg3.fit()
print(res3.summary())

#comparaison des R2 - statistique F (eq. 10.1)
q = 2
F =((res2.rsquared-res3.rsquared)/q)/((1-res2.rsquared)/(nclean-p-1))
print("F statistic = ",F)

#calcul de la p-value
print("p-value = ",1-scipy.stats.f.cdf(F,q,nclean-p-1))

#bien sûr, si on passe par le test F (TUTO, page 7), on doit obtenir un résultat identique
R = np.array([[0,1,0,0,0,0,0],[0,0,0,0,0,1,0]])
#test
print(res2.f_test(R))

#Q.25 -- carac. du nouveau modèle
print(res3.summary())

#Q.26 -- qqplot des résidus de la nouvelle régression
sm.qqplot(res3.resid)

#Q.27 -- *** Prédiction pour la ville de Colombus ***

#carac. de Colombus
colombus = np.array([1,37,11.9,124833,9])
print(colombus)

#prédiction ponctuelle
ychapeau = reg3.predict(res3.params,colombus)
print(ychapeau)

#vérification prédiction - combinaison linéaire
print(np.sum(res3.params*colombus))

#Q.28 -- *** prédiction par intervalle ***
#inverse de X'X
inv_xtx = reg3.normalized_cov_params
print("(X'X)^-1 : ",inv_xtx)

#levier
lev = np.dot(np.dot(colombus,inv_xtx),np.transpose(colombus))
print("levier de Colombus = ",lev)

#variance de l'erreur de prédiction
v_err = res3.scale * (1 + lev)
print(v_err)

#quantile de la loi de student
ttheo = scipy.stats.t.ppf(0.95,nclean-4-1)
print(ttheo)

#borne.basse
yc_b = ychapeau - ttheo * np.sqrt(v_err)
yc_h = ychapeau + ttheo * np.sqrt(v_err)

print(yc_b,yc_h)
