
rm(list=ls())
library(readr)
meteo_train <- read_csv("../data/meteo.train.csv")
meteo_test <- read_csv("../data/meteo.test.csv")

#View(meteo_train)
#View(meteo_test)

#names(meteo_train)
#names(meteo_test)


summary(meteo_train)

install.packages(corrplot)

library(corrplot)

names(meteo_train)

# Partie 1 : 
# Regardant la matrice de correlation entre ces covariables dans cet ordre : 
# 1 - High.Cloud.Cover.daily.max..high.cld.lay.
# 2 - High.Cloud.Cover.daily.mean..high.cld.lay.
# 3 - High.Cloud.Cover.daily.min..high.cld.lay.
# 4 - Low.Cloud.Cover.daily.max..low.cld.lay.
# 5 - Low.Cloud.Cover.daily.mean..low.cld.lay.
# 6 - Low.Cloud.Cover.daily.min..low.cld.lay.
# 7 - Mean.Sea.Level.Pressure.daily.max..MSL.
# 8 - Mean.Sea.Level.Pressure.daily.mean..MSL.
# 9 - Mean.Sea.Level.Pressure.daily.min..MSL.
# 10 - Medium.Cloud.Cover.daily.max..mid.cld.lay.
# 11 - Medium.Cloud.Cover.daily.mean..mid.cld.lay.
# 12 - Medium.Cloud.Cover.daily.min..mid.cld.lay.
# 13 - Relative.Humidity.daily.max..2.m.above.gnd.
# 14 - Relative.Humidity.daily.mean..2.m.above.gnd.
# 15 - Relative.Humidity.daily.min..2.m.above.gnd.
# 16 - Shortwave.Radiation.daily.sum..sfc.
# 17 - Snowfall.amount.raw.daily.sum..sfc.
# 18 - Sunshine.Duration.daily.sum..sfc.
# 19 - Temperature.daily.max..2.m.above.gnd.
# 20 - Temperature.daily.mean..2.m.above.gnd.
# 21 - Temperature.daily.min..2.m.above.gnd.
# 22 - Total.Cloud.Cover.daily.max..sfc.
# 23 - Total.Cloud.Cover.daily.mean..sfc.
# 24 - Total.Cloud.Cover.daily.min..sfc.
# 25 - Total.Precipitation.daily.sum..sfc.
# 26 - Wind.Gust.daily.max..sfc.
# 27 - Wind.Gust.daily.mean..sfc.
# 28 - Wind.Gust.daily.min..sfc.
# 29 - Wind.Speed.daily.max..10.m.above.gnd.
# 30 - Wind.Speed.daily.max..80.m.above.gnd.
# 31 - Wind.Speed.daily.max..900.mb.
# 32 - Wind.Speed.daily.mean..10.m.above.gnd.
# 33 - Wind.Speed.daily.mean..80.m.above.gnd.
# 34 - Wind.Speed.daily.mean..900.mb.
# 35 - Wind.Speed.daily.min..10.m.above.gnd.
# 36 - Wind.Speed.daily.min..80.m.above.gnd.
# 37 - Wind.Speed.daily.min..900.mb.



meteo_train_cor=cbind(meteo_train$High.Cloud.Cover.daily.max..high.cld.lay.,
                      meteo_train$High.Cloud.Cover.daily.mean..high.cld.lay.,
                      meteo_train$High.Cloud.Cover.daily.min..high.cld.lay.,
                      meteo_train$Low.Cloud.Cover.daily.max..low.cld.lay.,
                      meteo_train$Low.Cloud.Cover.daily.mean..low.cld.lay.,
                      meteo_train$Low.Cloud.Cover.daily.min..low.cld.lay.,
                      meteo_train$Mean.Sea.Level.Pressure.daily.max..MSL.,
                      meteo_train$Mean.Sea.Level.Pressure.daily.mean..MSL.,
                      meteo_train$Mean.Sea.Level.Pressure.daily.min..MSL.,
                      meteo_train$Medium.Cloud.Cover.daily.max..mid.cld.lay.,
                      meteo_train$Medium.Cloud.Cover.daily.mean..mid.cld.lay.,
                      meteo_train$Medium.Cloud.Cover.daily.min..mid.cld.lay.,
                      meteo_train$Relative.Humidity.daily.max..2.m.above.gnd.,
                      meteo_train$Relative.Humidity.daily.mean..2.m.above.gnd.,
                      meteo_train$Relative.Humidity.daily.min..2.m.above.gnd.,
                      meteo_train$Shortwave.Radiation.daily.sum..sfc.,
                      meteo_train$Snowfall.amount.raw.daily.sum..sfc.,
                      meteo_train$Sunshine.Duration.daily.sum..sfc.,
                      meteo_train$Temperature.daily.max..2.m.above.gnd.,
                      meteo_train$Temperature.daily.mean..2.m.above.gnd.,
                      meteo_train$Temperature.daily.min..2.m.above.gnd.,
                      meteo_train$Total.Cloud.Cover.daily.max..sfc.,
                      meteo_train$Total.Cloud.Cover.daily.mean..sfc.,
                      meteo_train$Total.Cloud.Cover.daily.min..sfc.,
                      meteo_train$Total.Precipitation.daily.sum..sfc.,
                      meteo_train$Wind.Gust.daily.max..sfc.,
                      meteo_train$Wind.Gust.daily.mean..sfc.,
                      meteo_train$Wind.Gust.daily.min..sfc.,
                      meteo_train$Wind.Speed.daily.max..10.m.above.gnd.,
                      meteo_train$Wind.Speed.daily.max..80.m.above.gnd.,
                      meteo_train$Wind.Speed.daily.max..900.mb.,
                      meteo_train$Wind.Speed.daily.mean..10.m.above.gnd.,
                      meteo_train$Wind.Speed.daily.mean..80.m.above.gnd.,
                      meteo_train$Wind.Speed.daily.mean..900.mb.,
                      meteo_train$Wind.Speed.daily.min..10.m.above.gnd.,
                      meteo_train$Wind.Speed.daily.min..80.m.above.gnd.,
                      meteo_train$Wind.Speed.daily.min..900.mb.
)

meteo_train_cor

matcorr1_meteo_train=cor(meteo_train_cor)
taille_matcorr1_meteo_train=sqrt(length(cor(matcorr1_meteo_train)))

taille_matcorr1_meteo_train

corrplot(cor(meteo_train_cor), method = "ellipse")

res_matcorr1_meteo_train=c(0,0);
for(i in 1:taille_matcorr1_meteo_train){
  for(j in i:taille_matcorr1_meteo_train){
    if (abs(matcorr1_meteo_train[i,j])>=0.9 & abs(matcorr1_meteo_train[i,j])!=1) res_matcorr1_meteo_train = rbind(res_matcorr1_meteo_train,c(i,j)) else NULL
  }
  
}



res_matcorr1_meteo_train

# Les covariables coorel‚e entre elles sont : 
# 5 - Low.Cloud.Cover.daily.mean..low.cld.lay. et corr‚l‚ avec # 23 - Total.Cloud.Cover.daily.mean..sfc.
# 7 - Mean.Sea.Level.Pressure.daily.max..MSL. et corr‚l‚ avec # 8 - Mean.Sea.Level.Pressure.daily.mean..MSL.
# 7 - Mean.Sea.Level.Pressure.daily.max..MSL. et corr‚l‚ avec # 9 - Mean.Sea.Level.Pressure.daily.min..MSL.
# 8 - Mean.Sea.Level.Pressure.daily.mean..MSL. et corr‚l‚ avec # 9 - Mean.Sea.Level.Pressure.daily.min..MSL.
# 18 - Sunshine.Duration.daily.sum..sfc. et corr‚l‚ avec # 23 - Total.Cloud.Cover.daily.mean..sfc.
# 19 - Temperature.daily.max..2.m.above.gnd. et corr‚l‚ avec # 20 - Temperature.daily.mean..2.m.above.gnd.
# 19 - Temperature.daily.max..2.m.above.gnd. et corr‚l‚ avec # 21 - Temperature.daily.min..2.m.above.gnd.
# 20 - Temperature.daily.mean..2.m.above.gnd. et corr‚l‚ avec # 21 - Temperature.daily.min..2.m.above.gnd.
# 27 - Wind.Gust.daily.mean..sfc. et corr‚l‚ avec # 32 - Wind.Speed.daily.mean..10.m.above.gnd.
# 27 - Wind.Gust.daily.mean..sfc. et corr‚l‚ avec # 33 - Wind.Speed.daily.mean..80.m.above.gnd.
# 29 - Wind.Speed.daily.max..10.m.above.gnd. et corr‚l‚ avec # 30 - Wind.Speed.daily.max..80.m.above.gnd.
# 29 - Wind.Speed.daily.max..10.m.above.gnd. et corr‚l‚ avec # 32 - Wind.Speed.daily.mean..10.m.above.gnd.
# 31 - Wind.Speed.daily.max..900.mb. et corr‚l‚ avec # 34 - Wind.Speed.daily.mean..900.mb.
# 32 - Wind.Speed.daily.mean..10.m.above.gnd. et corr‚l‚ avec # 33 - Wind.Speed.daily.mean..80.m.above.gnd.
# 35 - Wind.Speed.daily.min..10.m.above.gnd. et corr‚l‚ avec # 36 - Wind.Speed.daily.min..80.m.above.gnd.

# par transitivit‚, parmis ces covariables corr‚l‚es entre elles, je gardes : 
# 8 - Mean.Sea.Level.Pressure.daily.mean..MSL.
# 35 - Wind.Speed.daily.min..10.m.above.gnd.
# 34 - Wind.Speed.daily.mean..900.mb.
# 27 - Wind.Gust.daily.mean..sfc.
# 23 - Total.Cloud.Cover.daily.mean..sfc.
# 20 - Temperature.daily.mean..2.m.above.gnd.

# et je ne prend pas les suivantes dans mes modŠles : 
# 5 - Low.Cloud.Cover.daily.mean..low.cld.lay.
# 9 - Mean.Sea.Level.Pressure.daily.min..MSL.
# 7 - Mean.Sea.Level.Pressure.daily.max..MSL.
# 36 - Wind.Speed.daily.min..80.m.above.gnd.
# 33 - Wind.Speed.daily.mean..80.m.above.gnd.
# 32 - Wind.Speed.daily.mean..10.m.above.gnd.
# 31 - Wind.Speed.daily.max..900.mb.
# 30 - Wind.Speed.daily.max..80.m.above.gnd.
# 29 - Wind.Speed.daily.max..10.m.above.gnd.
# 21 - Temperature.daily.min..2.m.above.gnd.
# 19 - Temperature.daily.max..2.m.above.gnd.
# 18 - Sunshine.Duration.daily.sum..sfc.

# Les variables li‚e … la direction du vent sont de nature cat‚gorielle (mˆme si c'est quantifi‚ : ), je prfŠre en premier temps de les analyser s‚par‚ment entre elles

meteo_train_cor2=cbind(meteo_train$Wind.Direction.daily.mean..10.m.above.gnd.,
                       meteo_train$Wind.Direction.daily.mean..80.m.above.gnd.,
                       meteo_train$Wind.Direction.daily.mean..900.mb.)


matcorr1_meteo_train2=cor(meteo_train_cor2)
taille_matcorr1_meteo_train2=sqrt(length(cor(matcorr1_meteo_train2)))

taille_matcorr1_meteo_train2

corrplot(cor(meteo_train_cor2), method = "ellipse")

res_matcorr1_meteo_train2=c(0,0);
for(i in 1:taille_matcorr1_meteo_train2){
  for(j in i:taille_matcorr1_meteo_train2){
    if (abs(matcorr1_meteo_train2[i,j])>=0.9 & abs(matcorr1_meteo_train2[i,j])!=1) res_matcorr1_meteo_train2 = rbind(res_matcorr1_meteo_train2,c(i,j)) else NULL
  }
  
}

# Les covariables coorel‚e entre elles sont : 
# 1 - Wind.Direction.daily.mean..80.m.above.gnd. et corr‚l‚ avec # 2 - Wind.Direction.daily.mean..80.m.above.gnd.

# je garderai en final les deux covariables suivantes : 
#1 - Wind.Direction.daily.mean..10.m.above.gnd.
#3 - Wind.Direction.daily.mean..900.mb.

# Ce qui parrait coh‚rent avec les covariables choisies pr‚c‚dement : 
# 35 - Wind.Speed.daily.min..10.m.above.gnd.
# 34 - Wind.Speed.daily.mean..900.mb.


# Ainsi notre premier modŠle est le suivant : 
model1_temperature_meteo_train = lm(temp.demain ~ 
                                      High.Cloud.Cover.daily.max..high.cld.lay. +
                                      High.Cloud.Cover.daily.mean..high.cld.lay. +
                                      High.Cloud.Cover.daily.min..high.cld.lay. +
                                      Low.Cloud.Cover.daily.max..low.cld.lay. +
                                      Low.Cloud.Cover.daily.min..low.cld.lay. +
                                      Mean.Sea.Level.Pressure.daily.mean..MSL. +
                                      Medium.Cloud.Cover.daily.max..mid.cld.lay. +
                                      Medium.Cloud.Cover.daily.mean..mid.cld.lay. +
                                      Medium.Cloud.Cover.daily.min..mid.cld.lay. +
                                      Relative.Humidity.daily.max..2.m.above.gnd. +
                                      Relative.Humidity.daily.mean..2.m.above.gnd. +
                                      Relative.Humidity.daily.min..2.m.above.gnd. +
                                      Shortwave.Radiation.daily.sum..sfc. +
                                      Snowfall.amount.raw.daily.sum..sfc. +
                                      Temperature.daily.mean..2.m.above.gnd. +
                                      Total.Cloud.Cover.daily.max..sfc. +
                                      Total.Cloud.Cover.daily.mean..sfc. +
                                      Total.Cloud.Cover.daily.min..sfc. +
                                      Total.Precipitation.daily.sum..sfc. +
                                      Wind.Gust.daily.max..sfc. +
                                      Wind.Gust.daily.mean..sfc. +
                                      Wind.Gust.daily.min..sfc. +
                                      Wind.Speed.daily.mean..900.mb. + 
                                      Wind.Direction.daily.mean..900.mb. +
                                      Wind.Speed.daily.min..10.m.above.gnd.*Wind.Direction.daily.mean..10.m.above.gnd. +
                                      Wind.Speed.daily.min..900.mb.*Wind.Direction.daily.mean..900.mb. ,
                                    data =
                                      meteo_train)

summary(model1_temperature_meteo_train)

par(mfrow=c(2,2))
plot(model1_temperature_meteo_train)

# Sur la base de ces graphes, les hypothŠses de notre modŠle lin‚aire sont bien v‚rfi‚s : 
# Passant … la simplification du modŠle

# Regardant le nouveau modŠle simplifi‚ en se basant uniquement sur les covariables significatives (p-value<0.1)
model2_temperature_meteo_train = lm(temp.demain ~ 
                                      High.Cloud.Cover.daily.max..high.cld.lay.  + 
                                      High.Cloud.Cover.daily.mean..high.cld.lay. + 
                                      Mean.Sea.Level.Pressure.daily.mean..MSL.   + 
                                      Shortwave.Radiation.daily.sum..sfc.        + 
                                      Snowfall.amount.raw.daily.sum..sfc.        + 
                                      Temperature.daily.mean..2.m.above.gnd.     + 
                                      Wind.Gust.daily.max..sfc.                  + 
                                      Wind.Direction.daily.mean..10.m.above.gnd. + 
                                      Wind.Speed.daily.min..900.mb.                ,
                                    data =
                                      meteo_train)

summary(model2_temperature_meteo_train)

# comparant les deux modŠle par l'anova : 
anova(model1_temperature_meteo_train,model2_temperature_meteo_train)

# le deuxiŠme modŠle est trop simplifi‚, il n'a pas assez d'information 
# Regardant le model 2_bis contenant uniquement les covariables du modŠle1 qui ne sont pas significatives

model2_bis_temperature_meteo_train = lm(temp.demain ~ 
                                          High.Cloud.Cover.daily.min..high.cld.lay.      + 
                                          Low.Cloud.Cover.daily.max..low.cld.lay.        + 
                                          Low.Cloud.Cover.daily.min..low.cld.lay.        + 
                                          Medium.Cloud.Cover.daily.max..mid.cld.lay.     + 
                                          Medium.Cloud.Cover.daily.mean..mid.cld.lay.    + 
                                          Medium.Cloud.Cover.daily.min..mid.cld.lay.     + 
                                          Relative.Humidity.daily.max..2.m.above.gnd.    + 
                                          Relative.Humidity.daily.mean..2.m.above.gnd.   + 
                                          Relative.Humidity.daily.min..2.m.above.gnd.    + 
                                          Total.Cloud.Cover.daily.max..sfc.              + 
                                          Total.Cloud.Cover.daily.mean..sfc.             + 
                                          Total.Cloud.Cover.daily.min..sfc.              + 
                                          Total.Precipitation.daily.sum..sfc.            + 
                                          Wind.Gust.daily.mean..sfc.                     + 
                                          Wind.Gust.daily.min..sfc.                      + 
                                          Wind.Speed.daily.mean..900.mb.*Wind.Direction.daily.mean..900.mb.                 + 
                                          Wind.Speed.daily.min..10.m.above.gnd.*Wind.Direction.daily.mean..10.m.above.gnd.            ,
                                        data =
                                          meteo_train)

summary(model2_bis_temperature_meteo_train)


#A partir de ce modŠle, je prend uniquement les covariables significatives que j'ajouterai au modŠle 2 pour cr‚er un nouveau modŠle : 
model3_temperature_meteo_train = lm(temp.demain ~ 
                                      High.Cloud.Cover.daily.max..high.cld.lay.  + 
                                      High.Cloud.Cover.daily.mean..high.cld.lay. + 
                                      Mean.Sea.Level.Pressure.daily.mean..MSL.   + 
                                      Shortwave.Radiation.daily.sum..sfc.        + 
                                      Snowfall.amount.raw.daily.sum..sfc.        + 
                                      Temperature.daily.mean..2.m.above.gnd.     + 
                                      Wind.Gust.daily.max..sfc.                  + 
                                      Wind.Speed.daily.min..900.mb.              +
                                      Medium.Cloud.Cover.daily.mean..mid.cld.lay.                                       +
                                      Relative.Humidity.daily.max..2.m.above.gnd.                                       +
                                      Relative.Humidity.daily.mean..2.m.above.gnd.                                      +
                                      Total.Cloud.Cover.daily.mean..sfc.                                                +
                                      Total.Precipitation.daily.sum..sfc.                                               +
                                      Wind.Gust.daily.mean..sfc.                                                        +
                                      Wind.Gust.daily.min..sfc.                                                         +
                                      Wind.Speed.daily.mean..900.mb.                                                    +
                                      Wind.Speed.daily.min..10.m.above.gnd.*Wind.Direction.daily.mean..10.m.above.gnd.  +
                                      Wind.Speed.daily.mean..900.mb.*Wind.Direction.daily.mean..900.mb.      ,
                                    data =
                                      meteo_train)

summary(model3_temperature_meteo_train)
plot(model3_temperature_meteo_train)

# Regardant alors par l'anova si notre nouveau modŠle est int‚ressant : 

anova(model1_temperature_meteo_train,model3_temperature_meteo_train)

# Notre nouveau modŠle est effectivement int‚ressant : p-value>0.5

# Passant … la simplification automatique par la m‚thode 'step'
#install.packages("MASS")

library(MASS)

SForward_model3_temperature_meteo_train=step(lm(temp.demain~1,data=meteo_train),
                                             temp.demain ~ 
                                               High.Cloud.Cover.daily.max..high.cld.lay.  + 
                                               High.Cloud.Cover.daily.mean..high.cld.lay. + 
                                               Mean.Sea.Level.Pressure.daily.mean..MSL.   + 
                                               Shortwave.Radiation.daily.sum..sfc.        + 
                                               Snowfall.amount.raw.daily.sum..sfc.        + 
                                               Temperature.daily.mean..2.m.above.gnd.     + 
                                               Wind.Gust.daily.max..sfc.                  + 
                                               Wind.Speed.daily.min..900.mb.              +
                                               Medium.Cloud.Cover.daily.mean..mid.cld.lay.                                       +
                                               Relative.Humidity.daily.max..2.m.above.gnd.                                       +
                                               Relative.Humidity.daily.mean..2.m.above.gnd.                                      +
                                               Total.Cloud.Cover.daily.mean..sfc.                                                +
                                               Total.Precipitation.daily.sum..sfc.                                               +
                                               Wind.Gust.daily.mean..sfc.                                                        +
                                               Wind.Gust.daily.min..sfc.                                                         +
                                               Wind.Speed.daily.mean..900.mb.                                                    +
                                               Wind.Speed.daily.min..10.m.above.gnd.*Wind.Direction.daily.mean..10.m.above.gnd.  +
                                               Wind.Speed.daily.mean..900.mb.*Wind.Direction.daily.mean..900.mb.      ,
                                             data =
                                               meteo_train, 
                                             direction = "forward")


Sboth_model3_temperature_meteo_train=step(lm(temp.demain~1,data=meteo_train),
                                          temp.demain ~ 
                                            High.Cloud.Cover.daily.max..high.cld.lay.  + 
                                            High.Cloud.Cover.daily.mean..high.cld.lay. + 
                                            Mean.Sea.Level.Pressure.daily.mean..MSL.   + 
                                            Shortwave.Radiation.daily.sum..sfc.        + 
                                            Snowfall.amount.raw.daily.sum..sfc.        + 
                                            Temperature.daily.mean..2.m.above.gnd.     + 
                                            Wind.Gust.daily.max..sfc.                  + 
                                            Wind.Speed.daily.min..900.mb.              +
                                            Medium.Cloud.Cover.daily.mean..mid.cld.lay.                                       +
                                            Relative.Humidity.daily.max..2.m.above.gnd.                                       +
                                            Relative.Humidity.daily.mean..2.m.above.gnd.                                      +
                                            Total.Cloud.Cover.daily.mean..sfc.                                                +
                                            Total.Precipitation.daily.sum..sfc.                                               +
                                            Wind.Gust.daily.mean..sfc.                                                        +
                                            Wind.Gust.daily.min..sfc.                                                         +
                                            Wind.Speed.daily.mean..900.mb.                                                    +
                                            Wind.Speed.daily.min..10.m.above.gnd.*Wind.Direction.daily.mean..10.m.above.gnd.  +
                                            Wind.Speed.daily.mean..900.mb.*Wind.Direction.daily.mean..900.mb.      ,
                                          data =
                                            meteo_train, 
                                          direction = "both")

Sbackward_model3_temperature_meteo_train = step(model3_temperature_meteo_train,direction="backward")

# Les trois m‚thode de choix de modŠle ne donnent pas les mˆmes r‚sultats.
# Cependant, 
# les modŠles SForward_model3_temperature_meteo_train et Sboth_model3_temperature_meteo_train sont imbriqu‚s,
# les modŠles SForward_model3_temperature_meteo_train et Sbackward_model3_temperature_meteo_train sont imbriqu‚s,
# l'anova nous donne : 
anova(SForward_model3_temperature_meteo_train,
      Sboth_model3_temperature_meteo_train)

anova(Sbackward_model3_temperature_meteo_train,
      Sboth_model3_temperature_meteo_train)

# donc on peut garder le modŠle plus simple entre ces trois modŠles : Sboth_model3_temperature_meteo_train

# Notre dernier modŠle choisi est bien valide : 
plot(Sboth_model3_temperature_meteo_train)

# Passant … la pr‚diction

#Je cr‚e une nouvelle colonne meteo_test$temp.demain_pred
meteo_test$temp.demain_pred = round(predict(Sboth_model3_temperature_meteo_train, newdata = meteo_test, 
),2)

#La moyenne est bien pesque ‚gale … 0
mean(meteo_test$temp.demain_pred - 
       meteo_test$temp.demain)

#Le test de Box Pierce ne rejete pas la nulet‚ de la diff‚rence
Box.test(meteo_test$temp.demain_pred - 
           meteo_test$temp.demain, type ="Box-Pierce")

#La variance de la diff‚rence n'est pas ‚lev‚e
var(meteo_test$temp.demain_pred - 
      meteo_test$temp.demain)

#Les valeurs observ‚es et pr‚dites sont bien corr‚l‚es
cor(cbind(meteo_test$temp.demain_pred,
          meteo_test$temp.demain))

corrplot(cor(cbind(meteo_test$temp.demain_pred,
                   meteo_test$temp.demain)), method = "ellipse")

#Apperc‡ des valeurs pr‚dite et valeurs observ‚e : 
cbind(meteo_test$temp.demain_pred,
      meteo_test$temp.demain)

#trac‚ des valeurs observ‚es et pr‚dites
library(stringr)
meteo_test$Le_Jour=
  as.Date(paste(meteo_test$Year,
                str_pad(meteo_test$Month, 2, pad = "0"),
                str_pad(meteo_test$Day, 2, pad = "0"),sep="") ,
          format="%Y%m%d")

plot(meteo_test$Le_Jour,meteo_test$temp.demain,
     col="green",lwd=2,ylab="Temp‚rature en øC",xlab="Temps", type="l")
points(meteo_test$Le_Jour,meteo_test$temp.demain_pred,col="blue",lwd=2,type="l")
legend("topleft",c("Temp‚ratures observ‚es","Temp‚ratures pr‚dites"),
       col=c("green","blue"),lty=rep(1,2),lwd = rep(2,2),cex=0.7)









# Partie 2 : Pr‚diction de la pluie
# On commencera notre premier modŠle glm par les mˆmes covariables que dans le premier modŠle de la partie 1
model_glm1_temperature_meteo_train = glm(pluie.demain ~ 
                                           High.Cloud.Cover.daily.max..high.cld.lay. +
                                           High.Cloud.Cover.daily.mean..high.cld.lay. +
                                           High.Cloud.Cover.daily.min..high.cld.lay. +
                                           Low.Cloud.Cover.daily.max..low.cld.lay. +
                                           Low.Cloud.Cover.daily.min..low.cld.lay. +
                                           Mean.Sea.Level.Pressure.daily.mean..MSL. +
                                           Medium.Cloud.Cover.daily.max..mid.cld.lay. +
                                           Medium.Cloud.Cover.daily.mean..mid.cld.lay. +
                                           Medium.Cloud.Cover.daily.min..mid.cld.lay. +
                                           Relative.Humidity.daily.max..2.m.above.gnd. +
                                           Relative.Humidity.daily.mean..2.m.above.gnd. +
                                           Relative.Humidity.daily.min..2.m.above.gnd. +
                                           Shortwave.Radiation.daily.sum..sfc. +
                                           Snowfall.amount.raw.daily.sum..sfc. +
                                           Temperature.daily.mean..2.m.above.gnd. +
                                           Total.Cloud.Cover.daily.max..sfc. +
                                           Total.Cloud.Cover.daily.mean..sfc. +
                                           Total.Cloud.Cover.daily.min..sfc. +
                                           Total.Precipitation.daily.sum..sfc. +
                                           Wind.Gust.daily.max..sfc. +
                                           Wind.Gust.daily.mean..sfc. +
                                           Wind.Gust.daily.min..sfc. +
                                           Wind.Speed.daily.mean..900.mb. + 
                                           Wind.Direction.daily.mean..900.mb. +
                                           Wind.Speed.daily.min..10.m.above.gnd.*Wind.Direction.daily.mean..10.m.above.gnd. +
                                           Wind.Speed.daily.min..900.mb.*Wind.Direction.daily.mean..900.mb. ,
                                         data =
                                           meteo_train,
                                         family = binomial)

summary(model_glm1_temperature_meteo_train)


# Analysons les sorties
# Null d‚viance: 1635.4  on 1179  degrees of freedom
# Residual deviance: 1267.4  on 1150  degrees of freedom
# On commence par comparer notre modŠle au modŠle sans covariable
pchisq(1635.4 - 1267.4, 1179 - 1150, lower = F)
# On obtient une p-valeur trŠs faible : on rejette le modŠle sans covariable. Notre modŠle est donc utile.

# Comparons maintenant notre modŠle au modŠle satur‚
pchisq(1267.4, 1150, lower = F)
# L… aussi, la p-valeur est faible : on rejette donc notre modŠle et on pr‚fŠre le modŠle satur‚. Autrement dit, notre modŠle n'est pas suffisant.

# vu le nombre ‚lev‚ des covariables, on procŠdera par s‚l‚ction automatique de modŠle : 
names(meteo_train)

model_glm2_temperature_meteo_train = step(glm(pluie.demain ~ . - X1 - Year - Month - Day - Hour - Minute - pluie.demain - temp.demain , data = meteo_train, family = binomial))
summary(model_glm2_temperature_meteo_train)

# Analysons les sorties de notre nouveau modŠle 
# Null d‚viance: 1635.4  on 1179  degrees of freedom
# Residual deviance: 1249.6  on 1162  degrees of freedom
# On commence par comparer notre modŠle au modŠle sans covariable
pchisq(1635.4 - 1249.6, 1179 - 1162, lower = F)
# On obtient une p-valeur trŠs faible : on rejette le modŠle sans covariable. Notre modŠle est donc utile.

# Comparons maintenant notre modŠle au modŠle satur‚
pchisq(1249.6, 1162, lower = F)
# p-valeur ‚lev‚e : on accepte notre nouveau modŠle, qui suffit … expliquer les variations.


#Effectuant la mˆme chose, mais avec le modŠle probit cette fois:
model_glmprobit1_temperature_meteo_train = glm(pluie.demain ~ 
                                                 High.Cloud.Cover.daily.max..high.cld.lay. +
                                                 High.Cloud.Cover.daily.mean..high.cld.lay. +
                                                 High.Cloud.Cover.daily.min..high.cld.lay. +
                                                 Low.Cloud.Cover.daily.max..low.cld.lay. +
                                                 Low.Cloud.Cover.daily.min..low.cld.lay. +
                                                 Mean.Sea.Level.Pressure.daily.mean..MSL. +
                                                 Medium.Cloud.Cover.daily.max..mid.cld.lay. +
                                                 Medium.Cloud.Cover.daily.mean..mid.cld.lay. +
                                                 Medium.Cloud.Cover.daily.min..mid.cld.lay. +
                                                 Relative.Humidity.daily.max..2.m.above.gnd. +
                                                 Relative.Humidity.daily.mean..2.m.above.gnd. +
                                                 Relative.Humidity.daily.min..2.m.above.gnd. +
                                                 Shortwave.Radiation.daily.sum..sfc. +
                                                 Snowfall.amount.raw.daily.sum..sfc. +
                                                 Temperature.daily.mean..2.m.above.gnd. +
                                                 Total.Cloud.Cover.daily.max..sfc. +
                                                 Total.Cloud.Cover.daily.mean..sfc. +
                                                 Total.Cloud.Cover.daily.min..sfc. +
                                                 Total.Precipitation.daily.sum..sfc. +
                                                 Wind.Gust.daily.max..sfc. +
                                                 Wind.Gust.daily.mean..sfc. +
                                                 Wind.Gust.daily.min..sfc. +
                                                 Wind.Speed.daily.mean..900.mb. + 
                                                 Wind.Direction.daily.mean..900.mb. +
                                                 Wind.Speed.daily.min..10.m.above.gnd.*Wind.Direction.daily.mean..10.m.above.gnd. +
                                                 Wind.Speed.daily.min..900.mb.*Wind.Direction.daily.mean..900.mb. ,
                                               data =
                                                 meteo_train,
                                               family = binomial(link="probit"))

summary(model_glmprobit1_temperature_meteo_train)


# Analysons les sorties
# Null d‚viance: 1635.4  on 1179  degrees of freedom
# Residual deviance: 1270.2  on 1150  degrees of freedom
# On commence par comparer notre modŠle au modŠle sans covariable
pchisq(1635.4 - 1270.2, 1179 - 1150, lower = F)
# On obtient une p-valeur trŠs faible : on rejette le modŠle sans covariable. Notre modŠle est donc utile.

# Comparons maintenant notre modŠle au modŠle satur‚
pchisq(1270.2, 1150, lower = F)
# L… aussi, la p-valeur est faible : on rejette donc notre modŠle et on pr‚fŠre le modŠle satur‚. Autrement dit, notre modŠle n'est pas suffisant.

# vu le nombre ‚lev‚ des covariables, on procŠdera par s‚l‚ction automatique de modŠle : 
names(meteo_train)

model_glmprobit2_temperature_meteo_train = step(glm(pluie.demain ~ . - X1 - Year - Month - Day - Hour - Minute - pluie.demain - temp.demain , data = meteo_train, family = binomial(link="probit")))
summary(model_glmprobit2_temperature_meteo_train)

# Analysons les sorties de notre nouveau modŠle 
# Null d‚viance: 1635.4  on 1179  degrees of freedom
# Residual deviance: 1251.0  on 1161  degrees of freedom
# On commence par comparer notre modŠle au modŠle sans covariable
pchisq(1635.4 - 1251.0, 1179 - 1161, lower = F)
# On obtient une p-valeur trŠs faible : on rejette le modŠle sans covariable. Notre modŠle est donc utile.

# Comparons maintenant notre modŠle au modŠle satur‚
pchisq(1251.0, 1161, lower = F)
# p-valeur ‚lev‚e : on accepte notre nouveau modŠle, qui suffit … expliquer les variations.

# Comparant le modŠle probit avec le premier modŠle : 
summary(model_glm2_temperature_meteo_train) ## AIC = 1285.6
summary(model_glmprobit2_temperature_meteo_train) ## AIC = 1289

# les deux modŠles sont proches, mais regardant leurs score de pr‚dictions : On utilisera la m‚thode de validation crois‚e : 
# On commence par d‚finir un ‚chantillion de 33% de des obseravtions pour le test : 
meteo_for_train = sample(c(T, F), nrow(meteo_train), replace = T, prob = c(.67, .33))

#red‚ffinissant nos deux modŠles sur notre base d'entrainement
model_glm2train_temperature_meteo_train=glm(formula = pluie.demain ~ Temperature.daily.mean..2.m.above.gnd. + 
                                              Mean.Sea.Level.Pressure.daily.mean..MSL. + Snowfall.amount.raw.daily.sum..sfc. + 
                                              Total.Cloud.Cover.daily.mean..sfc. + Wind.Speed.daily.mean..80.m.above.gnd. + 
                                              Wind.Direction.daily.mean..80.m.above.gnd. + Wind.Direction.daily.mean..900.mb. + 
                                              Wind.Gust.daily.mean..sfc. + Temperature.daily.min..2.m.above.gnd. + 
                                              Mean.Sea.Level.Pressure.daily.max..MSL. + Mean.Sea.Level.Pressure.daily.min..MSL. + 
                                              Total.Cloud.Cover.daily.min..sfc. + High.Cloud.Cover.daily.max..high.cld.lay. + 
                                              Medium.Cloud.Cover.daily.max..mid.cld.lay. + Wind.Speed.daily.max..10.m.above.gnd. + 
                                              Wind.Speed.daily.min..10.m.above.gnd. + Wind.Speed.daily.min..80.m.above.gnd., 
                                            family = binomial, data = meteo_train[meteo_for_train, ])

model_glmprobit2train_temperature_meteo_train=glm(formula = pluie.demain ~ Temperature.daily.mean..2.m.above.gnd. + 
                                                    Mean.Sea.Level.Pressure.daily.mean..MSL. + Snowfall.amount.raw.daily.sum..sfc. + 
                                                    Total.Cloud.Cover.daily.mean..sfc. + Wind.Speed.daily.mean..80.m.above.gnd. + 
                                                    Wind.Direction.daily.mean..80.m.above.gnd. + Wind.Direction.daily.mean..900.mb. + 
                                                    Wind.Gust.daily.mean..sfc. + Temperature.daily.min..2.m.above.gnd. + 
                                                    Mean.Sea.Level.Pressure.daily.max..MSL. + Mean.Sea.Level.Pressure.daily.min..MSL. + 
                                                    Total.Cloud.Cover.daily.min..sfc. + High.Cloud.Cover.daily.max..high.cld.lay. + 
                                                    Medium.Cloud.Cover.daily.max..mid.cld.lay. + Low.Cloud.Cover.daily.max..low.cld.lay. + 
                                                    Wind.Speed.daily.max..10.m.above.gnd. + Wind.Speed.daily.min..10.m.above.gnd. + 
                                                    Wind.Speed.daily.min..80.m.above.gnd., family = binomial(link = "probit"), 
                                                  data = meteo_train[meteo_for_train, ])


# on effectue la pr‚diction des deux modŠles sur la base de test
# Pr‚diction modŠle logistique : 
pred_glm2train = predict(model_glm2train_temperature_meteo_train, meteo_train[!meteo_for_train, ], type = "response")
pred_glm2train
# et on ‚value l'erreur de pr‚diction
pred_glm2train_b= (pred_glm2train >= 0.5)
pred_glm2train_b
table(pred_glm2train_b, meteo_train[!meteo_for_train, ]$pluie.demain) #Table des pr‚dictions compar‚es aux vraies valeurs :
# on a 48 faux n‚gatifs et 61 faux affirmatifs
mean(pred_glm2train_b == (meteo_train[!meteo_for_train, ]$pluie.demain == "TRUE")) # 73% de bonnes pr‚dictions

# Pr‚diction modŠle probit : 
pred_glmprobit2train = predict(model_glmprobit2train_temperature_meteo_train, meteo_train[!meteo_for_train, ], type = "response")
pred_glmprobit2train
# et on ‚value l'erreur de pr‚diction
pred_glmprobit2train_b= (pred_glmprobit2train >= 0.5)
pred_glmprobit2train_b
table(pred_glmprobit2train_b, meteo_train[!meteo_for_train, ]$pluie.demain) #Table des pr‚dictions compar‚es aux vraies valeurs :
# on a 52 faux n‚gatifs et 61 faux affirmatifs
mean(pred_glmprobit2train_b == (meteo_train[!meteo_for_train, ]$pluie.demain == "TRUE")) # 72% de bonnes pr‚dictions


# Regardant maintenant le choix du seuil pour am‚liorer la pr‚diction
seuil = seq(0, 100, by=1)
system.time({
  seuil_opti_glm2train = rep(NA, length(seuil))
  for(i in 1:length(seuil)){
    pred_glm2train_bs = (pred_glm2train >= seuil[i]/100)
    seuil_opti_glm2train[i] = 	sum(pred_glm2train_bs & meteo_train[!meteo_for_train, ]$pluie.demain == "FALSE") + 
      sum(!pred_glm2train_bs & meteo_train[!meteo_for_train, ]$pluie.demain == "TRUE")
  }
})
round(seuil[which.min(seuil_opti_glm2train)]/100,2)


seuil = seq(0, 100, by=1)
system.time({
  seuil_opti_glmprobit2train = rep(NA, length(seuil))
  for(i in 1:length(seuil)){
    pred_glmprobit2train_bs = (pred_glmprobit2train >= seuil[i]/100)
    seuil_opti_glmprobit2train[i] = 	sum(pred_glmprobit2train_bs & meteo_train[!meteo_for_train, ]$pluie.demain == "FALSE") + 
      sum(!pred_glmprobit2train_bs & meteo_train[!meteo_for_train, ]$pluie.demain == "TRUE")
  }
})
round(seuil[which.min(seuil_opti_glmprobit2train)]/100,2)


# Pr‚diction modŠle logistique : 
# et on ‚value l'erreur de pr‚diction
pred_glm2train_b= (pred_glm2train >= round(seuil[which.min(seuil_opti_glm2train)]/100,2))
table(pred_glm2train_b, meteo_train[!meteo_for_train, ]$pluie.demain) #Table des pr‚dictions compar‚es aux vraies valeurs :
# on a 29 faux n‚gatifs et 73 faux affirmatifs
mean(pred_glm2train_b == (meteo_train[!meteo_for_train, ]$pluie.demain == "TRUE")) # 75% de bonnes pr‚dictions

# Pr‚diction modŠle probit : 
# et on ‚value l'erreur de pr‚diction
pred_glmprobit2train_b= (pred_glmprobit2train >= round(seuil[which.min(seuil_opti_glmprobit2train)]/100,2))
table(pred_glmprobit2train_b, meteo_train[!meteo_for_train, ]$pluie.demain) #Table des pr‚dictions compar‚es aux vraies valeurs :
# on a 31 faux n‚gatifs et 71 faux affirmatifs
mean(pred_glmprobit2train_b == (meteo_train[!meteo_for_train, ]$pluie.demain == "TRUE")) # 75% de bonnes pr‚dictions



#/* */
#/* */

















#On recommencera plusieurs fois avec une boucle for pour le choix optimal des seuils des deux modŠles :
system.time({
  res_itteratif_seuil=c(0,"prediction_logistique",1,0,"seuil_logistique",1,0)
  for(k in 1:389){
    print(cbind("d‚but itteration :",k))
    
    #Redeffinition al‚atoires des deux base : test and train
    meteo_for_train = sample(c(T, F), nrow(meteo_train), replace = T, prob = c(.67, .33))
    
    #red‚ffinissant nos deux modŠles sur notre base d'entrainement
    model_glm2train_temperature_meteo_train=glm(formula = pluie.demain ~ Temperature.daily.mean..2.m.above.gnd. + 
                                                  Mean.Sea.Level.Pressure.daily.mean..MSL. + Snowfall.amount.raw.daily.sum..sfc. + 
                                                  Total.Cloud.Cover.daily.mean..sfc. + Wind.Speed.daily.mean..80.m.above.gnd. + 
                                                  Wind.Direction.daily.mean..80.m.above.gnd. + Wind.Direction.daily.mean..900.mb. + 
                                                  Wind.Gust.daily.mean..sfc. + Temperature.daily.min..2.m.above.gnd. + 
                                                  Mean.Sea.Level.Pressure.daily.max..MSL. + Mean.Sea.Level.Pressure.daily.min..MSL. + 
                                                  Total.Cloud.Cover.daily.min..sfc. + High.Cloud.Cover.daily.max..high.cld.lay. + 
                                                  Medium.Cloud.Cover.daily.max..mid.cld.lay. + Wind.Speed.daily.max..10.m.above.gnd. + 
                                                  Wind.Speed.daily.min..10.m.above.gnd. + Wind.Speed.daily.min..80.m.above.gnd., 
                                                family = binomial, data = meteo_train[meteo_for_train, ])
    
    model_glmprobit2train_temperature_meteo_train=glm(formula = pluie.demain ~ Temperature.daily.mean..2.m.above.gnd. + 
                                                        Mean.Sea.Level.Pressure.daily.mean..MSL. + Snowfall.amount.raw.daily.sum..sfc. + 
                                                        Total.Cloud.Cover.daily.mean..sfc. + Wind.Speed.daily.mean..80.m.above.gnd. + 
                                                        Wind.Direction.daily.mean..80.m.above.gnd. + Wind.Direction.daily.mean..900.mb. + 
                                                        Wind.Gust.daily.mean..sfc. + Temperature.daily.min..2.m.above.gnd. + 
                                                        Mean.Sea.Level.Pressure.daily.max..MSL. + Mean.Sea.Level.Pressure.daily.min..MSL. + 
                                                        Total.Cloud.Cover.daily.min..sfc. + High.Cloud.Cover.daily.max..high.cld.lay. + 
                                                        Medium.Cloud.Cover.daily.max..mid.cld.lay. + Low.Cloud.Cover.daily.max..low.cld.lay. + 
                                                        Wind.Speed.daily.max..10.m.above.gnd. + Wind.Speed.daily.min..10.m.above.gnd. + 
                                                        Wind.Speed.daily.min..80.m.above.gnd., family = binomial(link = "probit"), 
                                                      data = meteo_train[meteo_for_train, ])
    
    
    # on effectue la pr‚diction des deux modŠles sur la base de test
    # Pr‚diction modŠle logistique : 
    pred_glm2train = predict(model_glm2train_temperature_meteo_train, meteo_train[!meteo_for_train, ], type = "response")
    # Pr‚diction modŠle probit : 
    pred_glmprobit2train = predict(model_glmprobit2train_temperature_meteo_train, meteo_train[!meteo_for_train, ], type = "response")
    # Regardant maintenant le choix du seuil pour am‚liorer la pr‚diction
    seuil = seq(0, 100, by=1)
    #system.time({
    seuil_opti_glm2train = rep(NA, length(seuil))
    for(i in 1:length(seuil)){
      pred_glm2train_bs = (pred_glm2train >= seuil[i]/100)
      seuil_opti_glm2train[i] = 	sum(pred_glm2train_bs & meteo_train[!meteo_for_train, ]$pluie.demain == "FALSE") + 
        sum(!pred_glm2train_bs & meteo_train[!meteo_for_train, ]$pluie.demain == "TRUE")
    }
    #})
    
    #system.time({
    seuil_opti_glmprobit2train = rep(NA, length(seuil))
    for(i in 1:length(seuil)){
      pred_glmprobit2train_bs = (pred_glmprobit2train >= seuil[i]/100)
      seuil_opti_glmprobit2train[i] = 	sum(pred_glmprobit2train_bs & meteo_train[!meteo_for_train, ]$pluie.demain == "FALSE") + 
        sum(!pred_glmprobit2train_bs & meteo_train[!meteo_for_train, ]$pluie.demain == "TRUE")
    }
    #})
    
    
    # Pr‚diction modŠle logistique : 
    # et on ‚value l'erreur de pr‚diction
    pred_glm2train_b= (pred_glm2train >= round(seuil[which.min(seuil_opti_glm2train)]/100,2))
    #mean(pred_glm2train_b == (meteo_train[!meteo_for_train, ]$pluie.demain == "TRUE")) 
    
    # Pr‚diction modŠle probit : 
    # et on ‚value l'erreur de pr‚diction
    pred_glmprobit2train_b= (pred_glmprobit2train >= round(seuil[which.min(seuil_opti_glmprobit2train)]/100,2))
    #mean(pred_glmprobit2train_b == (meteo_train[!meteo_for_train, ]$pluie.demain == "TRUE")) 
    
    
    res_itteratif_seuil=rbind(res_itteratif_seuil, c(k,"prediction_logistique",
                                                     round(mean(pred_glm2train_b == (meteo_train[!meteo_for_train, ]$pluie.demain == "TRUE")),2) ,
                                                     round(mean(pred_glmprobit2train_b == (meteo_train[!meteo_for_train, ]$pluie.demain == "TRUE")),2) ,
                                                     "seuil_logistique",
                                                     round(seuil[which.min(seuil_opti_glm2train)]/100,2),
                                                     round(seuil[which.min(seuil_opti_glmprobit2train)]/100,2)))
    
    print(cbind("fin itteration :",k))
    
  }
})


res_itteratif_seuil[,c(1,3,4,6,7)]=as.numeric(res_itteratif_seuil[,c(1,3,4,6,7)])
summary(res_itteratif_seuil)



#Concernant le modŠle logistique, le min, le max et la moyenne des scores de pr‚diction sont respectivement :  
min(as.numeric(res_itteratif_seuil[(res_itteratif_seuil[,1]!=0),3]))
max(as.numeric(res_itteratif_seuil[(res_itteratif_seuil[,1]!=0),3]))
round(mean(as.numeric(res_itteratif_seuil[(res_itteratif_seuil[,1]!=0),3])),2)
res_pred_logit = round(quantile(as.numeric(res_itteratif_seuil[(res_itteratif_seuil[,1]!=0),3]),0.75),2)
res_pred_logit


#Concernant le modŠle probit, le min, le max et la moyenne des scores de pr‚diction sont respectivement :  
min(as.numeric(res_itteratif_seuil[(res_itteratif_seuil[,1]!=0),4]))
max(as.numeric(res_itteratif_seuil[(res_itteratif_seuil[,1]!=0),4]))
round(mean(as.numeric(res_itteratif_seuil[(res_itteratif_seuil[,1]!=0),4])),2)
res_pred_probit = round(quantile(as.numeric(res_itteratif_seuil[(res_itteratif_seuil[,1]!=0),4]),0.75),2)
res_pred_probit


#La moyenne des seuils du modŠle logistique, permetant d'atteindre la moyenne des scores de pr‚diction est :  
seuil_pred_logit_min=round(min(as.numeric(
  res_itteratif_seuil[(res_itteratif_seuil[,1]!=0 & res_itteratif_seuil[,3]>=res_pred_logit),6])
),2)

seuil_pred_logit_max=round(max(as.numeric(
  res_itteratif_seuil[(res_itteratif_seuil[,1]!=0 & res_itteratif_seuil[,3]>=res_pred_logit),6])
),2)

seuil_pred_logit_mean=round(mean(as.numeric(
  res_itteratif_seuil[(res_itteratif_seuil[,1]!=0 & res_itteratif_seuil[,3]>=res_pred_logit),6])
),2)

seuil_pred_logit_quantile25=round(quantile(as.numeric(
  res_itteratif_seuil[(res_itteratif_seuil[,1]!=0 & res_itteratif_seuil[,3]>=res_pred_logit),6])
  ,0.25),2)

seuil_pred_logit_min
seuil_pred_logit_mean
seuil_pred_logit_quantile25
seuil_pred_logit_max


#La moyenne des seuils du modŠle logistique, permetant d'atteindre la moyenne des scores de pr‚diction est :  
seuil_pred_probit_min=round(min(as.numeric(
  res_itteratif_seuil[(res_itteratif_seuil[,1]!=0 & res_itteratif_seuil[,4]>=res_pred_probit),6])
),2)

seuil_pred_probit_mean=round(mean(as.numeric(
  res_itteratif_seuil[(res_itteratif_seuil[,1]!=0 & res_itteratif_seuil[,4]>=res_pred_probit),6])
),2)

seuil_pred_probit_max=round(max(as.numeric(
  res_itteratif_seuil[(res_itteratif_seuil[,1]!=0 & res_itteratif_seuil[,4]>=res_pred_probit),6])
),2)

seuil_pred_probit_quantile25=round(quantile(as.numeric(
  res_itteratif_seuil[(res_itteratif_seuil[,1]!=0 & res_itteratif_seuil[,4]>=res_pred_probit),6])
  ,0.25),2)


seuil_pred_probit_min
seuil_pred_probit_mean
seuil_pred_probit_quantile25
seuil_pred_probit_max


# Enfin, du fait que le score maximal de pr‚diction est atteint par les deux modŠles , je choisi le modŠle logistique qui a un AIC plus faible avec le seuil associ‚ … seuil_pred_logit_quantile25=0.41 : 


model_glm2train_temperature_meteo_train=glm(formula = pluie.demain ~ Temperature.daily.mean..2.m.above.gnd. + 
                                              Mean.Sea.Level.Pressure.daily.mean..MSL. + Snowfall.amount.raw.daily.sum..sfc. + 
                                              Total.Cloud.Cover.daily.mean..sfc. + Wind.Speed.daily.mean..80.m.above.gnd. + 
                                              Wind.Direction.daily.mean..80.m.above.gnd. + Wind.Direction.daily.mean..900.mb. + 
                                              Wind.Gust.daily.mean..sfc. + Temperature.daily.min..2.m.above.gnd. + 
                                              Mean.Sea.Level.Pressure.daily.max..MSL. + Mean.Sea.Level.Pressure.daily.min..MSL. + 
                                              Total.Cloud.Cover.daily.min..sfc. + High.Cloud.Cover.daily.max..high.cld.lay. + 
                                              Medium.Cloud.Cover.daily.max..mid.cld.lay. + Wind.Speed.daily.max..10.m.above.gnd. + 
                                              Wind.Speed.daily.min..10.m.above.gnd. + Wind.Speed.daily.min..80.m.above.gnd., 
                                            family = binomial, data = meteo_train)

model_glmprobit2train_temperature_meteo_train=glm(formula = pluie.demain ~ Temperature.daily.mean..2.m.above.gnd. + 
                                                    Mean.Sea.Level.Pressure.daily.mean..MSL. + Snowfall.amount.raw.daily.sum..sfc. + 
                                                    Total.Cloud.Cover.daily.mean..sfc. + Wind.Speed.daily.mean..80.m.above.gnd. + 
                                                    Wind.Direction.daily.mean..80.m.above.gnd. + Wind.Direction.daily.mean..900.mb. + 
                                                    Wind.Gust.daily.mean..sfc. + Temperature.daily.min..2.m.above.gnd. + 
                                                    Mean.Sea.Level.Pressure.daily.max..MSL. + Mean.Sea.Level.Pressure.daily.min..MSL. + 
                                                    Total.Cloud.Cover.daily.min..sfc. + High.Cloud.Cover.daily.max..high.cld.lay. + 
                                                    Medium.Cloud.Cover.daily.max..mid.cld.lay. + Low.Cloud.Cover.daily.max..low.cld.lay. + 
                                                    Wind.Speed.daily.max..10.m.above.gnd. + Wind.Speed.daily.min..10.m.above.gnd. + 
                                                    Wind.Speed.daily.min..80.m.above.gnd., family = binomial(link = "probit"), 
                                                  data = meteo_train)


# Le r‚sultat de pr‚diction sur le jeu de test est :
meteo_test$pluie.demain_pred = predict(model_glm2train_temperature_meteo_train, meteo_test, type = "response")>=
  seuil_pred_logit_quantile25


# Le score associ‚ est : 0.7
round(mean(meteo_test$pluie.demain_pred == (meteo_test$pluie.demain == "TRUE")),2) 


#R‚capitulatif des r‚sultats de l'‚tude

#Export des donn‚es en csv
write.table(meteo_test[,c(1:6,49,50)],"meteo_prediction_SEFFANE.csv", row.names = FALSE, sep = ";")

# trac‚ des temp‚ratures r‚‚lles et pr‚dites
par(mfrow=c(1,1))
library(stringr)
meteo_test$Le_Jour=
  as.Date(paste(meteo_test$Year,
                str_pad(meteo_test$Month, 2, pad = "0"),
                str_pad(meteo_test$Day, 2, pad = "0"),sep="") ,
          format="%Y%m%d")

plot(meteo_test$Le_Jour,meteo_test$temp.demain,
     col="green",lwd=2,ylab="Temp‚rature en øC",xlab="Temps", type="l")
points(meteo_test$Le_Jour,meteo_test$temp.demain_pred,col="blue",lwd=2,type="l")
legend("topleft",c("Temp‚ratures observ‚es","Temp‚ratures pr‚dites"),
       col=c("green","blue"),lty=rep(1,2),lwd = rep(2,2),cex=0.7)






#/* */
#/* */
#/* */
#/* */
#/* */
#/* */
#/* */
#/* */
#/* */
#/* */
#/* */
#/* */
#/* */
#/* */
#/* */
#/* */
#/* */
#/* */
#/* */
#/* */
#/* */
#/* */
#/* */
#/* */
#/* */
#/* */
#/* */
#/* */
#/* */
#/* */

# Partie 2 : Pr‚diction de la pluie
# On commencera notre premier modŠle glm par les mˆmes covariables que dans le premier modŠle de la partie 1
model_glm1_temperature_meteo_train = glm(pluie.demain ~ 
                                           High.Cloud.Cover.daily.max..high.cld.lay. +
                                           High.Cloud.Cover.daily.mean..high.cld.lay. +
                                           High.Cloud.Cover.daily.min..high.cld.lay. +
                                           Low.Cloud.Cover.daily.max..low.cld.lay. +
                                           Low.Cloud.Cover.daily.min..low.cld.lay. +
                                           Mean.Sea.Level.Pressure.daily.mean..MSL. +
                                           Medium.Cloud.Cover.daily.max..mid.cld.lay. +
                                           Medium.Cloud.Cover.daily.mean..mid.cld.lay. +
                                           Medium.Cloud.Cover.daily.min..mid.cld.lay. +
                                           Relative.Humidity.daily.max..2.m.above.gnd. +
                                           Relative.Humidity.daily.mean..2.m.above.gnd. +
                                           Relative.Humidity.daily.min..2.m.above.gnd. +
                                           Shortwave.Radiation.daily.sum..sfc. +
                                           Snowfall.amount.raw.daily.sum..sfc. +
                                           Temperature.daily.mean..2.m.above.gnd. +
                                           Total.Cloud.Cover.daily.max..sfc. +
                                           Total.Cloud.Cover.daily.mean..sfc. +
                                           Total.Cloud.Cover.daily.min..sfc. +
                                           Total.Precipitation.daily.sum..sfc. +
                                           Wind.Gust.daily.max..sfc. +
                                           Wind.Gust.daily.mean..sfc. +
                                           Wind.Gust.daily.min..sfc. +
                                           Wind.Speed.daily.mean..900.mb. + 
                                           Wind.Direction.daily.mean..900.mb. +
                                           Wind.Speed.daily.min..10.m.above.gnd.*Wind.Direction.daily.mean..10.m.above.gnd. +
                                           Wind.Speed.daily.min..900.mb.*Wind.Direction.daily.mean..900.mb. ,
                                         data =
                                           meteo_train,
                                         family = binomial)

summary(model_glm1_temperature_meteo_train)


# Analysons les sorties
# Null d‚viance: 1635.4  on 1179  degrees of freedom
# Residual deviance: 1267.4  on 1150  degrees of freedom
# On commence par comparer notre modŠle au modŠle sans covariable
pchisq(1635.4 - 1267.4, 1179 - 1150, lower = F)
# On obtient une p-valeur trŠs faible : on rejette le modŠle sans covariable. Notre modŠle est donc utile.

# Comparons maintenant notre modŠle au modŠle satur‚
pchisq(1267.4, 1150, lower = F)
# L… aussi, la p-valeur est faible : on rejette donc notre modŠle et on pr‚fŠre le modŠle satur‚. Autrement dit, notre modŠle n'est pas suffisant.

# vu le nombre ‚lev‚ des covariables, on procŠdera par s‚l‚ction automatique de modŠle : 
names(meteo_train)

model_glm2_temperature_meteo_train = step(glm(pluie.demain ~ . - X1 - Year - Month - Day - Hour - Minute - pluie.demain - temp.demain , data = meteo_train, family = binomial))
summary(model_glm2_temperature_meteo_train)

# Analysons les sorties de notre nouveau modŠle 
# Null d‚viance: 1635.4  on 1179  degrees of freedom
# Residual deviance: 1249.6  on 1162  degrees of freedom
# On commence par comparer notre modŠle au modŠle sans covariable
pchisq(1635.4 - 1249.6, 1179 - 1162, lower = F)
# On obtient une p-valeur trŠs faible : on rejette le modŠle sans covariable. Notre modŠle est donc utile.

# Comparons maintenant notre modŠle au modŠle satur‚
pchisq(1249.6, 1162, lower = F)
# p-valeur ‚lev‚e : on accepte notre nouveau modŠle, qui suffit … expliquer les variations.


#Effectuant la mˆme chose, mais avec le modŠle probit cette fois:
model_glmprobit1_temperature_meteo_train = glm(pluie.demain ~ 
                                                 High.Cloud.Cover.daily.max..high.cld.lay. +
                                                 High.Cloud.Cover.daily.mean..high.cld.lay. +
                                                 High.Cloud.Cover.daily.min..high.cld.lay. +
                                                 Low.Cloud.Cover.daily.max..low.cld.lay. +
                                                 Low.Cloud.Cover.daily.min..low.cld.lay. +
                                                 Mean.Sea.Level.Pressure.daily.mean..MSL. +
                                                 Medium.Cloud.Cover.daily.max..mid.cld.lay. +
                                                 Medium.Cloud.Cover.daily.mean..mid.cld.lay. +
                                                 Medium.Cloud.Cover.daily.min..mid.cld.lay. +
                                                 Relative.Humidity.daily.max..2.m.above.gnd. +
                                                 Relative.Humidity.daily.mean..2.m.above.gnd. +
                                                 Relative.Humidity.daily.min..2.m.above.gnd. +
                                                 Shortwave.Radiation.daily.sum..sfc. +
                                                 Snowfall.amount.raw.daily.sum..sfc. +
                                                 Temperature.daily.mean..2.m.above.gnd. +
                                                 Total.Cloud.Cover.daily.max..sfc. +
                                                 Total.Cloud.Cover.daily.mean..sfc. +
                                                 Total.Cloud.Cover.daily.min..sfc. +
                                                 Total.Precipitation.daily.sum..sfc. +
                                                 Wind.Gust.daily.max..sfc. +
                                                 Wind.Gust.daily.mean..sfc. +
                                                 Wind.Gust.daily.min..sfc. +
                                                 Wind.Speed.daily.mean..900.mb. + 
                                                 Wind.Direction.daily.mean..900.mb. +
                                                 Wind.Speed.daily.min..10.m.above.gnd.*Wind.Direction.daily.mean..10.m.above.gnd. +
                                                 Wind.Speed.daily.min..900.mb.*Wind.Direction.daily.mean..900.mb. ,
                                               data =
                                                 meteo_train,
                                               family = binomial(link="probit"))

summary(model_glmprobit1_temperature_meteo_train)


# Analysons les sorties
# Null d‚viance: 1635.4  on 1179  degrees of freedom
# Residual deviance: 1270.2  on 1150  degrees of freedom
# On commence par comparer notre modŠle au modŠle sans covariable
pchisq(1635.4 - 1270.2, 1179 - 1150, lower = F)
# On obtient une p-valeur trŠs faible : on rejette le modŠle sans covariable. Notre modŠle est donc utile.

# Comparons maintenant notre modŠle au modŠle satur‚
pchisq(1270.2, 1150, lower = F)
# L… aussi, la p-valeur est faible : on rejette donc notre modŠle et on pr‚fŠre le modŠle satur‚. Autrement dit, notre modŠle n'est pas suffisant.

# vu le nombre ‚lev‚ des covariables, on procŠdera par s‚l‚ction automatique de modŠle : 
names(meteo_train)

model_glmprobit2_temperature_meteo_train = step(glm(pluie.demain ~ . - X1 - Year - Month - Day - Hour - Minute - pluie.demain - temp.demain , data = meteo_train, family = binomial(link="probit")))
summary(model_glmprobit2_temperature_meteo_train)

# Analysons les sorties de notre nouveau modŠle 
# Null d‚viance: 1635.4  on 1179  degrees of freedom
# Residual deviance: 1251.0  on 1161  degrees of freedom
# On commence par comparer notre modŠle au modŠle sans covariable
pchisq(1635.4 - 1251.0, 1179 - 1161, lower = F)
# On obtient une p-valeur trŠs faible : on rejette le modŠle sans covariable. Notre modŠle est donc utile.

# Comparons maintenant notre modŠle au modŠle satur‚
pchisq(1251.0, 1161, lower = F)
# p-valeur ‚lev‚e : on accepte notre nouveau modŠle, qui suffit … expliquer les variations.

# Comparant le modŠle probit avec le premier modŠle : 
summary(model_glm2_temperature_meteo_train) ## AIC = 1285.6
summary(model_glmprobit2_temperature_meteo_train) ## AIC = 1289

# les deux modŠles sont proches, mais regardant leurs score de pr‚dictions : On utilisera la m‚thode de validation crois‚e : 
# On commence par d‚finir un ‚chantillion de 33% de des obseravtions pour le test : 
meteo_for_train = sample(c(T, F), nrow(meteo_train), replace = T, prob = c(.67, .33))

#red‚ffinissant nos deux modŠles sur notre base d'entrainement
model_glm2train_temperature_meteo_train=glm(formula = pluie.demain ~ Temperature.daily.mean..2.m.above.gnd. + 
                                              Mean.Sea.Level.Pressure.daily.mean..MSL. + Snowfall.amount.raw.daily.sum..sfc. + 
                                              Total.Cloud.Cover.daily.mean..sfc. + Wind.Speed.daily.mean..80.m.above.gnd. + 
                                              Wind.Direction.daily.mean..80.m.above.gnd. + Wind.Direction.daily.mean..900.mb. + 
                                              Wind.Gust.daily.mean..sfc. + Temperature.daily.min..2.m.above.gnd. + 
                                              Mean.Sea.Level.Pressure.daily.max..MSL. + Mean.Sea.Level.Pressure.daily.min..MSL. + 
                                              Total.Cloud.Cover.daily.min..sfc. + High.Cloud.Cover.daily.max..high.cld.lay. + 
                                              Medium.Cloud.Cover.daily.max..mid.cld.lay. + Wind.Speed.daily.max..10.m.above.gnd. + 
                                              Wind.Speed.daily.min..10.m.above.gnd. + Wind.Speed.daily.min..80.m.above.gnd., 
                                            family = binomial, data = meteo_train[meteo_for_train, ])

model_glmprobit2train_temperature_meteo_train=glm(formula = pluie.demain ~ Temperature.daily.mean..2.m.above.gnd. + 
                                                    Mean.Sea.Level.Pressure.daily.mean..MSL. + Snowfall.amount.raw.daily.sum..sfc. + 
                                                    Total.Cloud.Cover.daily.mean..sfc. + Wind.Speed.daily.mean..80.m.above.gnd. + 
                                                    Wind.Direction.daily.mean..80.m.above.gnd. + Wind.Direction.daily.mean..900.mb. + 
                                                    Wind.Gust.daily.mean..sfc. + Temperature.daily.min..2.m.above.gnd. + 
                                                    Mean.Sea.Level.Pressure.daily.max..MSL. + Mean.Sea.Level.Pressure.daily.min..MSL. + 
                                                    Total.Cloud.Cover.daily.min..sfc. + High.Cloud.Cover.daily.max..high.cld.lay. + 
                                                    Medium.Cloud.Cover.daily.max..mid.cld.lay. + Low.Cloud.Cover.daily.max..low.cld.lay. + 
                                                    Wind.Speed.daily.max..10.m.above.gnd. + Wind.Speed.daily.min..10.m.above.gnd. + 
                                                    Wind.Speed.daily.min..80.m.above.gnd., family = binomial(link = "probit"), 
                                                  data = meteo_train[meteo_for_train, ])


# on effectue la pr‚diction des deux modŠles sur la base de test
# Pr‚diction modŠle logistique : 
pred_glm2train = predict(model_glm2train_temperature_meteo_train, meteo_train[!meteo_for_train, ], type = "response")
#pred_glm2train
# et on ‚value l'erreur de pr‚diction
pred_glm2train_b= (pred_glm2train >= 0.5)
#pred_glm2train_b
table(pred_glm2train_b, meteo_train[!meteo_for_train, ]$pluie.demain) #Table des pr‚dictions compar‚es aux vraies valeurs :
# on a 48 faux n‚gatifs et 61 faux affirmatifs
mean(pred_glm2train_b == (meteo_train[!meteo_for_train, ]$pluie.demain == "TRUE")) # 73% de bonnes pr‚dictions

# Pr‚diction modŠle probit : 
pred_glmprobit2train = predict(model_glmprobit2train_temperature_meteo_train, meteo_train[!meteo_for_train, ], type = "response")
#pred_glmprobit2train
# et on ‚value l'erreur de pr‚diction
pred_glmprobit2train_b= (pred_glmprobit2train >= 0.5)
#pred_glmprobit2train_b
table(pred_glmprobit2train_b, meteo_train[!meteo_for_train, ]$pluie.demain) #Table des pr‚dictions compar‚es aux vraies valeurs :
# on a 52 faux n‚gatifs et 61 faux affirmatifs
mean(pred_glmprobit2train_b == (meteo_train[!meteo_for_train, ]$pluie.demain == "TRUE")) # 72% de bonnes pr‚dictions


# Regardant maintenant le choix du seuil pour am‚liorer la pr‚diction
seuil = seq(0, 100, by=1)
system.time({
  seuil_opti_glm2train = rep(NA, length(seuil))
  for(i in 1:length(seuil)){
    pred_glm2train_bs = (pred_glm2train >= seuil[i]/100)
    seuil_opti_glm2train[i] = 	sum(pred_glm2train_bs & meteo_train[!meteo_for_train, ]$pluie.demain == "FALSE") + 
      sum(!pred_glm2train_bs & meteo_train[!meteo_for_train, ]$pluie.demain == "TRUE")
  }
})
round(seuil[which.min(seuil_opti_glm2train)]/100,2)


seuil = seq(0, 100, by=1)
system.time({
  seuil_opti_glmprobit2train = rep(NA, length(seuil))
  for(i in 1:length(seuil)){
    pred_glmprobit2train_bs = (pred_glmprobit2train >= seuil[i]/100)
    seuil_opti_glmprobit2train[i] = 	sum(pred_glmprobit2train_bs & meteo_train[!meteo_for_train, ]$pluie.demain == "FALSE") + 
      sum(!pred_glmprobit2train_bs & meteo_train[!meteo_for_train, ]$pluie.demain == "TRUE")
  }
})
round(seuil[which.min(seuil_opti_glmprobit2train)]/100,2)


# Pr‚diction modŠle logistique : 
# et on ‚value l'erreur de pr‚diction
pred_glm2train_b= (pred_glm2train >= round(seuil[which.min(seuil_opti_glm2train)]/100,2))
table(pred_glm2train_b, meteo_train[!meteo_for_train, ]$pluie.demain) #Table des pr‚dictions compar‚es aux vraies valeurs :
# on a 29 faux n‚gatifs et 73 faux affirmatifs
mean(pred_glm2train_b == (meteo_train[!meteo_for_train, ]$pluie.demain == "TRUE")) # 75% de bonnes pr‚dictions

# Pr‚diction modŠle probit : 
# et on ‚value l'erreur de pr‚diction
pred_glmprobit2train_b= (pred_glmprobit2train >= round(seuil[which.min(seuil_opti_glmprobit2train)]/100,2))
table(pred_glmprobit2train_b, meteo_train[!meteo_for_train, ]$pluie.demain) #Table des pr‚dictions compar‚es aux vraies valeurs :
# on a 31 faux n‚gatifs et 71 faux affirmatifs
mean(pred_glmprobit2train_b == (meteo_train[!meteo_for_train, ]$pluie.demain == "TRUE")) # 75% de bonnes pr‚dictions



#/* */
#/* */

















#On recommencera plusieurs fois avec une boucle for pour le choix optimal des seuils des deux modŠles :
system.time({
  res_itteratif_seuil=c(0,"prediction_logistique",1,0,"seuil_logistique",1,0)
  for(k in 1:389){
    print(cbind("d‚but itteration :",k))
    
    #Redeffinition al‚atoires des deux base : test and train
    meteo_for_train = sample(c(T, F), nrow(meteo_train), replace = T, prob = c(.67, .33))
    
    #red‚ffinissant nos deux modŠles sur notre base d'entrainement
    model_glm2train_temperature_meteo_train=glm(formula = pluie.demain ~ Temperature.daily.mean..2.m.above.gnd. + 
                                                  Mean.Sea.Level.Pressure.daily.mean..MSL. + Snowfall.amount.raw.daily.sum..sfc. + 
                                                  Total.Cloud.Cover.daily.mean..sfc. + Wind.Speed.daily.mean..80.m.above.gnd. + 
                                                  Wind.Direction.daily.mean..80.m.above.gnd. + Wind.Direction.daily.mean..900.mb. + 
                                                  Wind.Gust.daily.mean..sfc. + Temperature.daily.min..2.m.above.gnd. + 
                                                  Mean.Sea.Level.Pressure.daily.max..MSL. + Mean.Sea.Level.Pressure.daily.min..MSL. + 
                                                  Total.Cloud.Cover.daily.min..sfc. + High.Cloud.Cover.daily.max..high.cld.lay. + 
                                                  Medium.Cloud.Cover.daily.max..mid.cld.lay. + Wind.Speed.daily.max..10.m.above.gnd. + 
                                                  Wind.Speed.daily.min..10.m.above.gnd. + Wind.Speed.daily.min..80.m.above.gnd., 
                                                family = binomial, data = meteo_train[meteo_for_train, ])
    
    model_glmprobit2train_temperature_meteo_train=glm(formula = pluie.demain ~ Temperature.daily.mean..2.m.above.gnd. + 
                                                        Mean.Sea.Level.Pressure.daily.mean..MSL. + Snowfall.amount.raw.daily.sum..sfc. + 
                                                        Total.Cloud.Cover.daily.mean..sfc. + Wind.Speed.daily.mean..80.m.above.gnd. + 
                                                        Wind.Direction.daily.mean..80.m.above.gnd. + Wind.Direction.daily.mean..900.mb. + 
                                                        Wind.Gust.daily.mean..sfc. + Temperature.daily.min..2.m.above.gnd. + 
                                                        Mean.Sea.Level.Pressure.daily.max..MSL. + Mean.Sea.Level.Pressure.daily.min..MSL. + 
                                                        Total.Cloud.Cover.daily.min..sfc. + High.Cloud.Cover.daily.max..high.cld.lay. + 
                                                        Medium.Cloud.Cover.daily.max..mid.cld.lay. + Low.Cloud.Cover.daily.max..low.cld.lay. + 
                                                        Wind.Speed.daily.max..10.m.above.gnd. + Wind.Speed.daily.min..10.m.above.gnd. + 
                                                        Wind.Speed.daily.min..80.m.above.gnd., family = binomial(link = "probit"), 
                                                      data = meteo_train[meteo_for_train, ])
    
    
    # on effectue la pr‚diction des deux modŠles sur la base de test
    # Pr‚diction modŠle logistique : 
    pred_glm2train = predict(model_glm2train_temperature_meteo_train, meteo_train[!meteo_for_train, ], type = "response")
    # Pr‚diction modŠle probit : 
    pred_glmprobit2train = predict(model_glmprobit2train_temperature_meteo_train, meteo_train[!meteo_for_train, ], type = "response")
    # Regardant maintenant le choix du seuil pour am‚liorer la pr‚diction
    seuil = seq(0, 100, by=1)
    #system.time({
    seuil_opti_glm2train = rep(NA, length(seuil))
    for(i in 1:length(seuil)){
      pred_glm2train_bs = (pred_glm2train >= seuil[i]/100)
      seuil_opti_glm2train[i] = 	sum(pred_glm2train_bs & meteo_train[!meteo_for_train, ]$pluie.demain == "FALSE") + 
        sum(!pred_glm2train_bs & meteo_train[!meteo_for_train, ]$pluie.demain == "TRUE")
    }
    #})
    
    #system.time({
    seuil_opti_glmprobit2train = rep(NA, length(seuil))
    for(i in 1:length(seuil)){
      pred_glmprobit2train_bs = (pred_glmprobit2train >= seuil[i]/100)
      seuil_opti_glmprobit2train[i] = 	sum(pred_glmprobit2train_bs & meteo_train[!meteo_for_train, ]$pluie.demain == "FALSE") + 
        sum(!pred_glmprobit2train_bs & meteo_train[!meteo_for_train, ]$pluie.demain == "TRUE")
    }
    #})
    
    
    # Pr‚diction modŠle logistique : 
    # et on ‚value l'erreur de pr‚diction
    pred_glm2train_b= (pred_glm2train >= round(seuil[which.min(seuil_opti_glm2train)]/100,2))
    #mean(pred_glm2train_b == (meteo_train[!meteo_for_train, ]$pluie.demain == "TRUE")) 
    
    # Pr‚diction modŠle probit : 
    # et on ‚value l'erreur de pr‚diction
    pred_glmprobit2train_b= (pred_glmprobit2train >= round(seuil[which.min(seuil_opti_glmprobit2train)]/100,2))
    #mean(pred_glmprobit2train_b == (meteo_train[!meteo_for_train, ]$pluie.demain == "TRUE")) 
    
    
    res_itteratif_seuil=rbind(res_itteratif_seuil, c(k,"prediction_logistique",
                                                     round(mean(pred_glm2train_b == (meteo_train[!meteo_for_train, ]$pluie.demain == "TRUE")),2) ,
                                                     round(mean(pred_glmprobit2train_b == (meteo_train[!meteo_for_train, ]$pluie.demain == "TRUE")),2) ,
                                                     "seuil_logistique",
                                                     round(seuil[which.min(seuil_opti_glm2train)]/100,2),
                                                     round(seuil[which.min(seuil_opti_glmprobit2train)]/100,2)))
    
    print(cbind("fin itteration :",k))
    
  }
})


res_itteratif_seuil[,c(1,3,4,6,7)]=as.numeric(res_itteratif_seuil[,c(1,3,4,6,7)])
summary(res_itteratif_seuil)



#Concernant le modŠle logistique, le min, le max , la moyenne et le 3eme quantile des scores de pr‚diction sont respectivement :  
min(as.numeric(res_itteratif_seuil[(res_itteratif_seuil[,1]!=0),3]))
max(as.numeric(res_itteratif_seuil[(res_itteratif_seuil[,1]!=0),3]))
round(mean(as.numeric(res_itteratif_seuil[(res_itteratif_seuil[,1]!=0),3])),2)
res_pred_logit = round(quantile(as.numeric(res_itteratif_seuil[(res_itteratif_seuil[,1]!=0),3]),0.75),2)
res_pred_logit


#Concernant le modŠle probit, le min, le max , la moyenne et le 3eme quantile des scores de pr‚diction sont respectivement :  
min(as.numeric(res_itteratif_seuil[(res_itteratif_seuil[,1]!=0),4]))
max(as.numeric(res_itteratif_seuil[(res_itteratif_seuil[,1]!=0),4]))
round(mean(as.numeric(res_itteratif_seuil[(res_itteratif_seuil[,1]!=0),4])),2)
res_pred_probit = round(quantile(as.numeric(res_itteratif_seuil[(res_itteratif_seuil[,1]!=0),4]),0.75),2)
res_pred_probit


#le min, le max , la moyenne et le 1er quantile des seuils du modŠle logistique, permetant d'atteindre le 3eme quantile des scores de pr‚diction est :  
seuil_pred_logit_min=round(min(as.numeric(
  res_itteratif_seuil[(res_itteratif_seuil[,1]!=0 & res_itteratif_seuil[,3]>=res_pred_logit),6])
),2)

seuil_pred_logit_max=round(max(as.numeric(
  res_itteratif_seuil[(res_itteratif_seuil[,1]!=0 & res_itteratif_seuil[,3]>=res_pred_logit),6])
),2)

seuil_pred_logit_mean=round(mean(as.numeric(
  res_itteratif_seuil[(res_itteratif_seuil[,1]!=0 & res_itteratif_seuil[,3]>=res_pred_logit),6])
),2)

seuil_pred_logit_quantile25=round(quantile(as.numeric(
  res_itteratif_seuil[(res_itteratif_seuil[,1]!=0 & res_itteratif_seuil[,3]>=res_pred_logit),6])
  ,0.25),2)

seuil_pred_logit_min
seuil_pred_logit_mean
seuil_pred_logit_quantile25
seuil_pred_logit_max


#le min, le max , la moyenne et le 1er quantile des seuils du modŠle probit, permetant d'atteindre le 3eme quantile des scores de pr‚diction est :  
seuil_pred_probit_min=round(min(as.numeric(
  res_itteratif_seuil[(res_itteratif_seuil[,1]!=0 & res_itteratif_seuil[,4]>=res_pred_probit),6])
),2)

seuil_pred_probit_mean=round(mean(as.numeric(
  res_itteratif_seuil[(res_itteratif_seuil[,1]!=0 & res_itteratif_seuil[,4]>=res_pred_probit),6])
),2)

seuil_pred_probit_max=round(max(as.numeric(
  res_itteratif_seuil[(res_itteratif_seuil[,1]!=0 & res_itteratif_seuil[,4]>=res_pred_probit),6])
),2)

seuil_pred_probit_quantile25=round(quantile(as.numeric(
  res_itteratif_seuil[(res_itteratif_seuil[,1]!=0 & res_itteratif_seuil[,4]>=res_pred_probit),6])
  ,0.25),2)


seuil_pred_probit_min
seuil_pred_probit_mean
seuil_pred_probit_quantile25
seuil_pred_probit_max


# Enfin, du fait que le score maximal de pr‚diction est atteint par les deux modŠles , je choisi le modŠle logistique qui a un AIC plus faible avec le seuil associ‚ … seuil_pred_logit_quantile25=0.41 : 


model_glm2train_temperature_meteo_train=glm(formula = pluie.demain ~ Temperature.daily.mean..2.m.above.gnd. + 
                                              Mean.Sea.Level.Pressure.daily.mean..MSL. + Snowfall.amount.raw.daily.sum..sfc. + 
                                              Total.Cloud.Cover.daily.mean..sfc. + Wind.Speed.daily.mean..80.m.above.gnd. + 
                                              Wind.Direction.daily.mean..80.m.above.gnd. + Wind.Direction.daily.mean..900.mb. + 
                                              Wind.Gust.daily.mean..sfc. + Temperature.daily.min..2.m.above.gnd. + 
                                              Mean.Sea.Level.Pressure.daily.max..MSL. + Mean.Sea.Level.Pressure.daily.min..MSL. + 
                                              Total.Cloud.Cover.daily.min..sfc. + High.Cloud.Cover.daily.max..high.cld.lay. + 
                                              Medium.Cloud.Cover.daily.max..mid.cld.lay. + Wind.Speed.daily.max..10.m.above.gnd. + 
                                              Wind.Speed.daily.min..10.m.above.gnd. + Wind.Speed.daily.min..80.m.above.gnd., 
                                            family = binomial, data = meteo_train)

model_glmprobit2train_temperature_meteo_train=glm(formula = pluie.demain ~ Temperature.daily.mean..2.m.above.gnd. + 
                                                    Mean.Sea.Level.Pressure.daily.mean..MSL. + Snowfall.amount.raw.daily.sum..sfc. + 
                                                    Total.Cloud.Cover.daily.mean..sfc. + Wind.Speed.daily.mean..80.m.above.gnd. + 
                                                    Wind.Direction.daily.mean..80.m.above.gnd. + Wind.Direction.daily.mean..900.mb. + 
                                                    Wind.Gust.daily.mean..sfc. + Temperature.daily.min..2.m.above.gnd. + 
                                                    Mean.Sea.Level.Pressure.daily.max..MSL. + Mean.Sea.Level.Pressure.daily.min..MSL. + 
                                                    Total.Cloud.Cover.daily.min..sfc. + High.Cloud.Cover.daily.max..high.cld.lay. + 
                                                    Medium.Cloud.Cover.daily.max..mid.cld.lay. + Low.Cloud.Cover.daily.max..low.cld.lay. + 
                                                    Wind.Speed.daily.max..10.m.above.gnd. + Wind.Speed.daily.min..10.m.above.gnd. + 
                                                    Wind.Speed.daily.min..80.m.above.gnd., family = binomial(link = "probit"), 
                                                  data = meteo_train)


# Le r‚sultat de pr‚diction sur le jeu de test est :
meteo_test$pluie.demain_pred = predict(model_glm2train_temperature_meteo_train, meteo_test, type = "response")>=
  seuil_pred_logit_quantile25


# Le score associ‚ est : 0.7
round(mean(meteo_test$pluie.demain_pred == (meteo_test$pluie.demain == "TRUE")),2) 


#R‚capitulatif des r‚sultats de l'‚tude

#Export des donn‚es en csv
write.table(meteo_test[,c(1:6,49,50)],"meteo_prediction_SEFFANE.csv", row.names = FALSE, sep = ";")

# trac‚ des temp‚ratures r‚‚lles et pr‚dites
library(stringr)
meteo_test$Le_Jour=
  as.Date(paste(meteo_test$Year,
                str_pad(meteo_test$Month, 2, pad = "0"),
                str_pad(meteo_test$Day, 2, pad = "0"),sep="") ,
          format="%Y%m%d")

plot(meteo_test$Le_Jour,meteo_test$temp.demain,
     col="green",lwd=2,ylab="Temp‚rature en øC",xlab="Temps", type="l")
points(meteo_test$Le_Jour,meteo_test$temp.demain_pred,col="blue",lwd=2,type="l")
legend("topleft",c("Temp‚ratures observ‚es","Temp‚ratures pr‚dites"),
       col=c("green","blue"),lty=rep(1,2),lwd = rep(2,2),cex=0.7)


# croisement des donn‚es observ‚es et pr‚dites concernant la pluie
table(meteo_test$pluie.demain, meteo_test$pluie.demain_pred)

# score de pr‚diction : 
round(mean(meteo_test$pluie.demain_pred == (meteo_test$pluie.demain == "TRUE")),2) 
