setwd("~/Dropbox/condition/ANALYSE")

Rep= read.table(file = "Repetability.txt", header = TRUE)
Rep<-Rep[which(Rep$FecondIncomplet==0),]
#attach(Rep)
names(Rep)

#install.packages("rptR")
library(rptR)

M1<- lm(longueur~individus,na.action=na.omit,data=Rep)
anova(M1)


#Analysis of Variance Table#

#Response: longueur
#           Df  Sum Sq Mean Sq F value    Pr(>F)    
#individus 342 235.505 0.68861  23.695 < 2.2e-16 ***
#Residuals 339   9.852 0.02906                      
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 


#NoNARep<-Rep[which(!Rep$individus=="22PNlPNa" & !Rep$individus=="23PNlPNa" & !Rep$individus=="24PNlPNa" & !Rep$individus=="25PNlPNa"),]



#rpt.aov(NoNARep$longueur,NoNARep$individus,CI=0.95,npermut=10000)

length(Rep$longueur)
length(Rep$individus)


rpt(longueur~(1|individus),data=Rep,grname = "individus",nboot=10000,datatype="Gaussian")


#Bootstrap Progress:
#   |++++| 100% elapsed = 01m 56s#
#

#Repeatability estimation using the lmm method #

#Repeatability for individus
#R  = 0.921
#SE = 0.009
#CI = [0.902, 0.936]
#P  = 1.93e-134 [LRT]
#     NA [Permutation]#

#Warning message:
#In rptGaussian(formula, grname, data, CI, nboot, npermut, parallel,  :
#  4 rows containing missing values were removed






M1<- lm(largeur~individus,na.action=na.omit,data=Rep)
anova(M1)


#Analysis of Variance Table#

#Response: largeur
#           Df Sum Sq  Mean Sq F value    Pr(>F)    
#individus 342 70.782 0.206964  3.6871 < 2.2e-16 ***
#Residuals 339 19.029 0.056132                      
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 


#rpt.aov(NoNARep$largeur,NoNARep$individus,CI=0.95,npermut=10000)

#Repeatability calculation using the ANOVA method#

#R  = 0.576
#SE = 0.036
#CI = [0.505, 0.648]
#P  = 8.17e-32 [P.aov]
#     1e-04 [P.permut]


rpt(largeur~(1|individus),data=Rep,grname = "individus",nboot=10000,datatype="Gaussian")

#Bootstrap Progress:
#   |++++| 100% elapsed = 01m 54s#
#

#Repeatability estimation using the lmm method #

#Repeatability for individus
#R  = 0.572
#SE = 0.037
#CI = [0.494, 0.641]
#P  = 4.75e-30 [LRT]
#     NA [Permutation]#

#Warning message:
#In rptGaussian(formula, grname, data, CI, nboot, npermut, parallel,  :
#  4 rows containing missing values were removed





M1<- lm(epaisseur~individus,na.action=na.omit)
anova(M1)
#Analysis of Variance Table#

#Response: epaisseur
#           Df Sum Sq  Mean Sq F value    Pr(>F)    
#individus 342 85.178 0.249058  10.097 < 2.2e-16 ***
#Residuals 325  8.017 0.024667                      
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 



rpt(epaisseur~(1|individus),data=Rep,grname = "individus",nboot=10000,datatype="Gaussian")

#Bootstrap Progress:
#   |++++| 100% elapsed = 01m 57s#
#

#Repeatability estimation using the lmm method #

#Repeatability for individus
#R  = 0.829
#SE = 0.018
#CI = [0.79, 0.861]
#P  = 2.54e-81 [LRT]
#     NA [Permutation]#

#Warning message:
#In rptGaussian(formula, grname, data, CI, nboot, npermut, parallel,  :
#  18 rows containing missing values were removed



#detach(Rep)
######

setwd("~/Dropbox/condition/ANALYSE")

INDICES= read.csv(file = "CALCULINDICES.csv", header = TRUE)

INDICES<-INDICES[which(INDICES$FecondIncomplet==0),]
#attach(INDICES)
names(INDICES)
 [1] "individu"        "longueur.1"      "épaisseur.1"     "largeur.1"      
 [5] "longueur.2"      "épaisseur.2"     "largeur.2"       "sacrifie"       
 [9] "milieu"          "TreatLarve"      "TreatAdult"      "moyLong"        
[13] "moyepaiss"       "moylargeur"      "volume"          "poids"          
[17] "MSL"             "VSL"             "OLSresidue"      "Fecondite"      
[21] "Larve"           "FecondIncomplet"


summary(INDICES$milieu)


png("/home/mark/Dropbox/condition/ANALYSE/largeurLongueur.png",width=2100, height=2970,res=300)
par(mfrow=c(2,2))
plot(INDICES$moylargeur~INDICES$moyLong)
plot(INDICES$moylargeur~INDICES$poids)
plot(INDICES$moyLong~INDICES$poids)

dev.off()


cor.test(log10(INDICES$moylargeur),log10(INDICES$moyLong))

cor.test(INDICES$moylargeur,INDICES$poids)

cor.test(INDICES$moyLong,INDICES$poids)


treatment<-paste(INDICES$TreatLarve:INDICES$TreatAdult)

png("/home/mark/Dropbox/condition/ANALYSE/loglargeurLongueur.png",width=2100, height=2970,res=300)

plot(log(INDICES$moyLong)~log(INDICES$poids),ylab="log (mass (g))",xlab="log (elytron length (mm))")

dev.off()





cor.test(log(INDICES$poids),INDICES$OLS,method="pearson")
cor.test(log(INDICES$moyLong),INDICES$OLS,method="pearson")

cor.test(log(INDICES$poids),INDICES$MSL,method="pearson")
cor.test(log(INDICES$moyLong),INDICES$MSL,method="pearson")

cor.test(log(INDICES$poids),INDICES$VSL,method="pearson")
cor.test(log(INDICES$moyLong),INDICES$VSL,method="pearson")

#install.packages("lmodel2")
library(lmodel2)


#install.packages("remotes")
#remotes::install_github("MohoWu/utilr")

library(utilr)



M1<-lm(poids~moyLong,data=INDICES)
M2<-lm(log(poids)~log(moyLong),data=INDICES)
summary(M1)
summary(M2)
INDICES$OLSresidue<-residuals(M2)




M2<-lmodel2(log10(volume)~log10(moyLong),data=INDICES)
M2

INDICES$lpoids<-log10(INDICES$poids)
INDICES$lmoyLong<-log10(INDICES$moyLong)

INDICES$lvolume<-log10(INDICES$volume)


M1<-lmodel2(lpoids~lmoyLong,data=INDICES)
M1

residuals(M1)


min(log(INDICES$volume))
max(log(INDICES$volume))

min(log10(INDICES$volume))
max(log10(INDICES$volume))


min(log(INDICES$moyLong))
max(log(INDICES$moyLong))

min(log10(INDICES$moyLong))
max(log10(INDICES$moyLong))

names(INDICES)


library("utilr")
plot_lmodel2(INDICES, "lmoyLong", "lpoids", method = c("OLS","SMA"), mod.line = F, group = NA,  same.panel = FALSE, xpos = 1, ypos = 1, pt.col = "black",pt.shape = 1)


#install.packages("ggpubr")

names(INDICES)

library("ggpubr")
ggscatter(INDICES, x = "poids", y = "OLSresidue", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "log(Mass)", ylab = "OLSresid")

## Get Coefficients
# Only want the results of the regressions saved as reg
reg <- (M1$regression.results)[1-3,]
CI<-(M1$confidence.intervals)[1-3,]

reg<-merge(reg,CI,by="Method")

# Rename columns in reg so they're easy to use
names(reg) <- c("method", "intercept", "slope", "angle", "p-value","low.intercept","high.intercept","low.slope","high.slope")
# Check that the regressions look like so we know what will be plots
print(reg)
library(smatr)
M4 <- sma(poids~moyLong ,log="xy",data=INDICES)
plot(M4)
M4

M4 <- sma(log(poids)~log(moyLong),data=INDICES)
plot(M4)
M4



M5 <- ma(poids~moyLong ,log="xy",data=INDICES)
M5

INDICES$SMAresid<-residuals(M4)
INDICES$MAresid<-residuals(M5)


M1



reg

p1<-ggplot() + geom_point(data = INDICES, aes(x=lmoyLong,y=lpoids)) +geom_abline(data = reg, aes(intercept = intercept, slope = slope, colour =method), show.legend = TRUE)+theme_classic(base_size = 14)+ scale_color_manual(values=c("black", "red"))+ labs(x = "log (Elytron length)", y = "log (Mass)")+ggtitle("a.")
p1

p2<-ggscatter(INDICES, x = "lpoids", y = "OLSresidue", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "log(Mass)", ylab = "OLSresid", cor.coef.size = 4)+ggtitle("b.")+  theme_classic(base_size = 14)
p2


p3<-ggscatter(INDICES, x = "lpoids", y = "MSL", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "log(Mass)", ylab = "SMI", cor.coef.size = 4)+ggtitle("c.")+  theme_classic(base_size = 14)


p4<-ggscatter(INDICES, x = "lmoyLong", y = "OLSresidue", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "log(Elytron length)", ylab = "OLSresid", cor.coef.size = 4)+ggtitle("d.")+  theme_classic(base_size = 14)
p4

p5<-ggscatter(INDICES, x = "lmoyLong", y = "MSL", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "log(Elytron length)", ylab = "SMI", cor.coef.size = 4)+ggtitle("e.")+  theme_classic(base_size = 14)
p5


library(grid)


png("/home/mark/Dropbox/condition/ANALYSE/FigureCorrelationPoidsBCI.png",width=2100, height=2970,res=300)
pushViewport(viewport(layout = grid.layout(3, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

print(p1, vp = vplayout(1, 1:2))
print(p2, vp = vplayout(2, 1))
print(p3, vp = vplayout(2, 2))
print(p4, vp = vplayout(3, 1))
print(p5, vp = vplayout(3, 2))

dev.off()








INDICES$lpoids<-log10(INDICES$poids)
INDICES$lmoyLong<-log10(INDICES$moyLong)

INDICES$lvolume<-log10(INDICES$volume)

M1<-lmodel2(lpoids~lmoyLong,data=INDICES)
M1

M2<-lmodel2(log10(volume)~log10(moyLong),data=INDICES)
M2

names(INDICES)

plot_lmodel2(INDICES, "lmoyLong", "lpoids", method = c("OLS","SMA"), mod.line = F, group = NA,  same.panel = FALSE, xpos = 1, ypos = 1, pt.col = "black",pt.shape = 1)


#install.packages("ggpubr")

names(INDICES)

library("ggpubr")
ggscatter(INDICES, x = "poids", y = "OLSresidue", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "log(Mass)", ylab = "OLSresid")

## Get Coefficients
# Only want the results of the regressions saved as reg
reg <- (M1$regression.results)
CI<-(M1$confidence.intervals)

reg<-merge(reg,CI,by="Method")
# Rename columns in reg so they're easy to use
names(reg) <- c("method", "intercept", "slope", "angle", "p-value","low.intercept","high.intercept","low.slope","high.slope")
# Check that the regressions look like so we know what will be plots
print(reg)




p1<-ggplot() + geom_point(data = INDICES, aes(x=lmoyLong,y=lpoids)) +geom_abline(data = reg, aes(intercept = intercept, slope = slope, colour =method), show.legend = TRUE)+theme_classic(base_size = 14)+ scale_color_manual(values=c("gray","black","red"))+ labs(x = "log (Elytron length)", y = "log (Mass)")+ggtitle("a.")
p1


MSLMA<- INDICES$poids*((mean(INDICES$moyLong)/INDICES$moyLong)^3.536462)

INDICES$MSLMA<-MSLMA

p2<-ggscatter(INDICES, x = "lpoids", y = "OLSresidue", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "log(Mass)", ylab = "OLS residuals", cor.coef.size = 4)+ggtitle("c.")+  theme_classic(base_size = 14)

p2
p3<-ggscatter(INDICES, x = "lpoids", y = "SMAresid", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "log(Mass)", ylab = "SMA residuals", cor.coef.size = 4)+ggtitle("d.")+  theme_classic(base_size = 14)
p3
p3a<-ggscatter(INDICES, x = "lpoids", y = "MAresid", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "log(Mass)", ylab = "MA residuals", cor.coef.size = 4)+ggtitle("e.")+  theme_classic(base_size = 14)
p3a


p4<-ggscatter(INDICES, x = "lmoyLong", y = "OLSresidue", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "log(Elytron length)", ylab = "OLS residuals", cor.coef.size = 4)+ggtitle("f.")+  theme_classic(base_size = 14)
p4

p5<-ggscatter(INDICES, x = "lmoyLong", y = "SMAresid", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "log(Elytron length)", ylab = "SMA residuals", cor.coef.size = 4)+ggtitle("g.")+  theme_classic(base_size = 14)
p5


p5a<-ggscatter(INDICES, x = "lmoyLong", y = "MAresid", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "log(Elytron length)", ylab = "MA residuals", cor.coef.size = 4)+ggtitle("h.")+  theme_classic(base_size = 14)
p5a


INDICES$lSMAresid<-log10(INDICES$SMAresid+1)
INDICES$lMSL<-log10(INDICES$MSL)


p7a<-ggscatter(INDICES, x = "lMSL", y = "SMAresid", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "log(SMI)", ylab = "SMA residuals", cor.coef.size = 4)+ggtitle("b.")+  theme_classic(base_size = 14)


png("/home/mark/Dropbox/condition/ANALYSE/FigureCorrelationPoidsTailleBCIa.png",width=2970, height=2970,res=300)

pushViewport(viewport(layout = grid.layout(3, 3)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

print(p1, vp = vplayout(1, 1:2))
print(p7a, vp = vplayout(1, 3))
print(p2, vp = vplayout(2, 1))
print(p3, vp = vplayout(2, 2))
print(p3a, vp = vplayout(2, 3))
print(p4, vp = vplayout(3, 1))
print(p5, vp = vplayout(3, 2))
print(p5a, vp = vplayout(3, 3))

dev.off()




p6a<-ggscatter(INDICES, x = "lMSL", y = "SMAresid", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "log (SMI)", ylab = "SMA residuals", cor.coef.size = 10)+  theme_classic(base_size = 30)
p6a



png("/home/mark/Dropbox/condition/ANALYSE/SMAresidSMI.png",width=2970, height=2970,res=300)

p6a

dev.off()













M1<-lmodel2(log10(volume)~log10(moyLong),data=INDICES)
M1



## Get Coefficients
# Only want the results of the regressions saved as reg
reg <- (M1$regression.results)[1-3,]
CI<-(M1$confidence.intervals)[1-3,]

reg<-merge(reg,CI,by="Method")
# Rename columns in reg so they're easy to use
names(reg) <- c("method", "intercept", "slope", "angle", "p-value","low.intercept","high.intercept","low.slope","high.slope")
# Check that the regressions look like so we know what will be plots
print(reg)





p1<-ggplot() + geom_point(data = INDICES, aes(lmoyLong,lvolume)) +geom_abline(data = reg, aes(intercept = intercept, slope = slope, colour =method), show.legend = TRUE)+theme_classic(base_size = 14)+ scale_color_manual(values=c("black", "red"))+ labs(y = "log (Volume)", x = "log (Elytron length)")+ggtitle("a.")
p1


MOLSV<-lm(log10(volume)~log10(moyLong),data=INDICES)
plot(MOLSV)
ROLSV<-resid(MOLSV)
INDICES$OLSvol<-ROLSV


p2<-ggscatter(INDICES, x = "lvolume", y = "OLSvol", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "log(Volume)", ylab = "OLS residuals of\nlog(volume)~log(elytron length)", cor.coef.size = 4)+ggtitle("b.")+  theme_classic(base_size = 14)
p2

p3<-ggscatter(INDICES, x = "lvolume", y = "VSL", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "log(Volume)", ylab = "SVI", cor.coef.size = 4)+ggtitle("c.")+  theme_classic(base_size = 14)
p3

p4<-ggscatter(INDICES, x = "lmoyLong", y = "OLSvol", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "log(Elytron length)", ylab = "OLS residuals of\nlog(volume)~log(elytron length)", cor.coef.size = 4)+ggtitle("d.")+  theme_classic(base_size = 14)
p4


p5<-ggscatter(INDICES, x = "lmoyLong", y = "VSL", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "log(Elytron length)", ylab = "SVI", cor.coef.size = 4)+ggtitle("e.")+  theme_classic(base_size = 14)
p5




png("/home/mark/Dropbox/condition/ANALYSE/FigureCorrelationVolumeTailleBCI.png",width=2100, height=2970,res=300)

pushViewport(viewport(layout = grid.layout(3, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

print(p1, vp = vplayout(1, 1:2))
print(p2, vp = vplayout(2, 1))
print(p3, vp = vplayout(2, 2))
print(p4, vp = vplayout(3, 1))
print(p5, vp = vplayout(3, 2))

dev.off()




## Get Coefficients
# Only want the results of the regressions saved as reg
reg <- (M1$regression.results)
CI<-(M1$confidence.intervals)

reg<-merge(reg,CI,by="Method")
# Rename columns in reg so they're easy to use
names(reg) <- c("method", "intercept", "slope", "angle", "p-value","low.intercept","high.intercept","low.slope","high.slope")
# Check that the regressions look like so we know what will be plots
print(reg)




p1<-ggplot() + geom_point(data = INDICES, aes(x=lmoyLong,y=lvolume)) +geom_abline(data = reg, aes(intercept = intercept, slope = slope, colour =method), show.legend = TRUE)+theme_classic(base_size = 14)+ scale_color_manual(values=c("gray","black","red"))+ labs(x = "log (Elytron length)", y = "log (Volume)")+ggtitle("a.")
p1

M2

VSLMA<- INDICES$volume*((mean(INDICES$moyLong)/INDICES$moyLong)^4.983438)

INDICES$VSLMA<-VSLMA

M4 <- sma(volume~moyLong ,log="xy",data=INDICES)
M4
M5 <- ma(volume~moyLong ,log="xy",data=INDICES)
M5

INDICES$SMAresidVol<-residuals(M4)
INDICES$MAresidVol<-residuals(M5)


p2<-ggscatter(INDICES, x = "lvolume", y = "OLSvol", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "log(Volume)", ylab = "OLS residuals", cor.coef.size = 4)+ggtitle("c.")+  theme_classic(base_size = 14)

p2
p3<-ggscatter(INDICES, x = "lvolume", y = "SMAresidVol", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "log(Volume)", ylab = "SMA residuals", cor.coef.size = 4)+ggtitle("d.")+  theme_classic(base_size = 14)
p3
p3a<-ggscatter(INDICES, x = "lvolume", y = "MAresidVol", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "log(Volume)", ylab = "MA residuals", cor.coef.size = 4)+ggtitle("e.")+  theme_classic(base_size = 14)
p3a


p4<-ggscatter(INDICES, x = "lmoyLong", y = "OLSvol", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "log(Elytron length)", ylab = "OLS residuals", cor.coef.size = 4)+ggtitle("f.")+  theme_classic(base_size = 14)
p4

p5<-ggscatter(INDICES, x = "lmoyLong", y = "SMAresidVol", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "log(Elytron length)", ylab = "SMA residuals", cor.coef.size = 4)+ggtitle("g.")+  theme_classic(base_size = 14)
p5


p5a<-ggscatter(INDICES, x = "lmoyLong", y = "MAresidVol", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "log(Elytron length)", ylab = "MA residuals", cor.coef.size = 4)+ggtitle("h.")+  theme_classic(base_size = 14)
p5a


INDICES$lSMAresidVol<-log10(INDICES$SMAresidVol+1)
INDICES$lVSL<-log10(INDICES$VSL)


p7a<-ggscatter(INDICES, x = "lVSL", y = "SMAresidVol", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "log(SVI)", ylab = "SMA residuals", cor.coef.size = 4)+ggtitle("b.")+  theme_classic(base_size = 14)
p7a

png("/home/mark/Dropbox/condition/ANALYSE/FigureCorrelationVolumeTailleBCIa.png",width=2970, height=2970,res=300)

pushViewport(viewport(layout = grid.layout(3, 3)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

print(p1, vp = vplayout(1, 1:2))
print(p7a, vp = vplayout(1, 3))
print(p2, vp = vplayout(2, 1))
print(p3, vp = vplayout(2, 2))
print(p3a, vp = vplayout(2, 3))
print(p4, vp = vplayout(3, 1))
print(p5, vp = vplayout(3, 2))
print(p5a, vp = vplayout(3, 3))

dev.off()






M1<-lm(log(INDICES$poids)~log(INDICES$moyLong))
par(mfrow=c(2,2))
plot(M1)
R1<-resid(M1)
plot(R1~INDICES$poids)






plot(log(INDICES$poids)~log(INDICES$moyLong))
abline(lm(log(INDICES$poids)~log(INDICES$moyLong)))
plot(INDICES$volume~INDICES$moyLong)





library(smatr)
m1<-sma(INDICES$poids~INDICES$moyLong*INDICES$TreatLarve,log="xy",data=INDICES)
plot(m1)
m1

m1<-sma(INDICES$poids~INDICES$moyLong*INDICES$TreatAdult,log="xy",data=INDICES)
plot(m1)
m1

m1<-sma(INDICES$volume~INDICES$moyLong*INDICES$TreatLarve,log="xy",data=INDICES)
plot(m1)
m1

m1<-sma(INDICES$volume~INDICES$moyLong*INDICES$TreatAdult,log="xy",data=INDICES)
plot(m1)
m1


tapply(INDICES$milieu,INDICES$milieu,length)
#  NlNa  NlPNa  PNlNa PNlPNa 
#    97     85     71     74 



library(MuMIn)
M2<-lm(log(poids)~log(moyLong)*TreatLarve*TreatAdult,data=INDICES,na.action=na.fail)
dredge(M2)
step(M2,test="F")


M2<-lm(log(volume)~log(moyLong)*TreatLarve*TreatAdult,data=INDICES,na.action=na.fail)
dredge(M2)




summary(INDICES)


M1<-lm(poids~moyLong,data=INDICES)
M2<-lm(log(poids)~log(moyLong),data=INDICES)
summary(M1)
summary(M2)
OLSresidue<-residuals(M2)
#plot(M1)
length(OLSresidue)

INDICES$OLSresidue<-OLSresidue

OLS<-data.frame(list(INDICES$individu,OLSresidue))
OLS
colnames(OLS)<-c("individu","OLSresidue")
OLS




write.table(OLS,"OLSresidue.txt",sep="\t",row.names=F,quote=F) 

#install.packages("lmodel2")
library(lmodel2)

M2<- lmodel2(log(poids)~log(moyLong),nperm=9999,data=INDICES)

plot(M2,"OLS",main="OLS regression vs SMA regression",ylab="ln(Mass)", xlab="ln(Tarsus)")
par(new=TRUE)
plot(M2,"SMA",main="",ylab="", xlab="")

#install.packages("smatr")
library(smatr)

M1 <- sma(poids~moyLong*milieu,data=INDICES)
M2 <- sma(poids~moyLong*milieu ,log="xy",data=INDICES)
M3 <- sma(poids~moyLong*TreatLarve ,log="xy",data=INDICES)
M4 <- sma(poids~moyLong*TreatAdult ,log="xy",data=INDICES)
print(M1)
print(M2)

print(M3)
print(M4)



plot(M1)
plot(M2)


M1 <- sma(poids~moyLong,data=INDICES)
M2 <- sma(poids~moyLong ,log="xy",data=INDICES)
print(M1)
print(M2)

#Call: sma(formula = poids ~ moyLong, data = INDICES) #

#Fit using Standardized Major Axis #

#------------------------------------------------------------
#Coefficients:
#            elevation    slope
#estimate    -223.3095 35.64995
#lower limit -245.6295 33.39542
#upper limit -200.9895 38.05669#

#H0 : variables uncorrelated
#R-squared : 0.6410971 
#P-value : < 2.22e-16 #

#> print(M2)
#Call: sma(formula = poids ~ moyLong, data = INDICES, log = "xy") #

#Fit using Standardized Major Axis #

#These variables were log-transformed before fitting: xy #

#Confidence intervals (CI) are at 95%#

#------------------------------------------------------------
#Coefficients:
#             elevation    slope
#estimate    -0.8362235 2.959333
#lower limit -1.0236654 2.774214
#upper limit -0.6487816 3.156805#

#H0 : variables uncorrelated
#R-squared : 0.649112 
#P-value : < 2.22e-16 

MSL<- INDICES$poids*((mean(INDICES$moyLong)/INDICES$moyLong)^2.959333)

INDICES$MSL<-MSL

MSL<-data.frame(list(INDICES$individu,MSL))
MSL
colnames(MSL)<-c("individu","MSL")
MSL


write.table(MSL,"MSL.txt",sep="\t",row.names=F,quote=F) 

M1 <- sma(volume~moyLong*milieu,data=INDICES)
M2 <- sma(volume~moyLong*milieu ,log="xy",data=INDICES)
print(M1)
print(M2)

plot(M1)
plot(M2)

M1 <- sma(volume~moyLong,data=INDICES)
M2 <- sma(volume~moyLong ,log="xy",data=INDICES)
print(M1)
print(M2)

#Call: sma(formula = volume ~ moyLong, data = INDICES, log = "xy") #

#Fit using Standardized Major Axis #

#These variables were log-transformed before fitting: xy #

#Confidence intervals (CI) are at 95%#

#------------------------------------------------------------
#Coefficients:
#            elevation    slope
#estimate    -2.107818 3.920726
#lower limit -2.377104 3.655526
#upper limit -1.838531 4.205166#

#H0 : variables uncorrelated
#R-squared : 0.5874182 
#P-value : < 2.22e-16 



VSL<- INDICES$volume*((mean(INDICES$moyLong)/INDICES$moyLong)^3.920726)

INDICES$VSL<-VSL

head(INDICES)


M1<-lm(VSL~volume,data=INDICES)
anova(M1)
summary(M1)
par(mfrow=c(2,2))
plot(M1)
dev.off()
plot(INDICES$VSL~INDICES$volume)
abline(M1)

cor.test(VSL,INDICES$volume)

#        Pearson's product-moment correlation#

#data:  VSL and INDICES$volume
#t = 6.2717, df = 325, p-value = 1.138e-09
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.2282489 0.4219936
#sample estimates:
#      cor 
#0.3285735 


VSL<-data.frame(list(INDICES$individu,VSL))
VSL
colnames(VSL)<-c("individu","VSL")
VSL


write.table(VSL,"VSL.txt",sep="\t",row.names=F,quote=F) 

plot(MSL$MSL,VSL$VSL)















densite<-INDICES$poids/INDICES$volume

png("/home/mark/Dropbox/condition/ANALYSE/PoidsVolume.png",width=2100, height=2970,res=300)
plot(INDICES$poids~INDICES$volume)
dev.off()

library(smatr)

M1 <- sma(densite~moyLong*milieu,data=INDICES)
M2 <- sma(densite~moyLong*milieu ,log="xy",data=INDICES)
print(M1)
print(M2)

plot(M1)
plot(M2)

M1 <- sma(densite~moyLong,data=INDICES)
M2 <- sma(densite~moyLong ,log="xy",data=INDICES)
print(M1)
print(M2)

#Call: sma(formula = densite ~ moyLong, data = INDICES, log = "xy") #

#Fit using Standardized Major Axis #

#These variables were log-transformed before fitting: xy #

#Confidence intervals (CI) are at 95%#

#------------------------------------------------------------
#Coefficients:
#            elevation     slope
#estimate     2.863763 -2.586991
#lower limit  2.595171 -2.875528
#upper limit  3.132356 -2.327407#

#H0 : variables uncorrelated
#R-squared : 0.05756883 
#P-value : 1.1524e-05 

png("/home/mark/Dropbox/condition/ANALYSE/DensityLength.png",width=2100, height=2970,res=300)
plot(M2)
dev.off()


DSL<- densite*((mean(INDICES$moyLong)/INDICES$moyLong)^-2.586991)

INDICES$DSL<-DSL
INDICES$densite<-densite


head(INDICES)


M1<-lm(DSL~densite,data=INDICES)
anova(M1)
summary(M1)
par(mfrow=c(2,2))
plot(M1)
dev.off()
plot(INDICES$DSL~INDICES$densite)
abline(M1)

cor.test(DSL,INDICES$densite)

#        Pearson's product-moment correlation#

#data:  DSL and INDICES$densite
#t = 6.2717, df = 325, p-value = 1.138e-09
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.2282489 0.4219936
#sample estimates:
#      cor 
#0.3285735 


DSL<-data.frame(list(INDICES$individu,DSL))
DSL
colnames(DSL)<-c("individu","DSL")
DSL


write.table(DSL,"DSL.txt",sep="\t",row.names=F,quote=F) 




plot(INDICES$MSL,INDICES$DSL)






















M1<-lm(poids~TreatLarve*TreatAdult,na.action=na.fail,data=INDICES)
anova(M1)
step(M1,test="F")
p1<-boxplot(INDICES$poids~INDICES$milieu)
means <- tapply(INDICES$poids,INDICES$milieu,mean)
points(means,col="red",pch=18)
means
par(mfrow=c(2,2))
plot(M1)
dev.off()


#install.packages("MuMIn")
library(MuMIn)
ms1 <- dredge(M1,rank="AICc", extra="R^2")
ms1
model.avg(ms1)
confset.95p <- get.models(ms1, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)



M1<-lm(poids~TreatLarve+TreatAdult,na.action=na.fail,data=INDICES)
summary(M1)

#Call:
#lm(formula = poids ~ TreatLarve + TreatAdult, data = INDICES, 
#    na.action = na.fail)#

#Residuals:
#    Min      1Q  Median      3Q     Max 
#-53.061 -13.503  -0.625  12.293  88.347 #

#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    112.788      1.899  59.400   <2e-16 ***
#TreatLarvePNl    5.872      2.341   2.509   0.0126 *  
#TreatAdultPNa    4.065      2.326   1.747   0.0816 .  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#

#Residual standard error: 21.01 on 324 degrees of freedom
#Multiple R-squared:  0.02919,   Adjusted R-squared:  0.0232 
#F-statistic: 4.871 on 2 and 324 DF,  p-value: 0.008235



tapply(INDICES$volume,INDICES$TreatLarve,length)
# Nl PNl 
#182 145 

tapply(INDICES$volume,INDICES$TreatAdult,length)
# Na PNa 
#168 159

ci.smd(ncp=2.509 , n.1=182, n.2=145, conf.level=1-.05) 
#$Lower.Conf.Limit.smd
#[1] 0.05985009#

#$smd
#[1] 0.2792897#

#$Upper.Conf.Limit.smd
#[1] 0.4983036



ci.smd(ncp=1.747 , n.1=168, n.2=159, conf.level=1-.05) 
#$Lower.Conf.Limit.smd
#[1] -0.02421889

#$smd
#[1] 0.1932919

#$Upper.Conf.Limit.smd
#[1] 0.4105067



M1<-lm(poids~TreatLarve,na.action=na.fail,data=INDICES)
summary(M1)

#Call:
#lm(formula = poids ~ TreatLarve, data = INDICES, na.action = na.fail)#

#Residuals:
#    Min      1Q  Median      3Q     Max 
#-55.135 -12.785   0.013  12.965  90.513 #

#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    114.687      1.562  73.419   <2e-16 ***
#TreatLarvePNl    6.048      2.346   2.578   0.0104 *  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#

#Residual standard error: 21.07 on 325 degrees of freedom
#Multiple R-squared:  0.02004,   Adjusted R-squared:  0.01703 
#F-statistic: 6.648 on 1 and 325 DF,  p-value: 0.01037

ci.smd(ncp=2.578 , n.1=182, n.2=145, conf.level=1-.05) 
#$Lower.Conf.Limit.smd
#[1] 0.06746652#

#$smd
#[1] 0.2869704#

#$Upper.Conf.Limit.smd
#[1] 0.5060372


write.table(ms1, "~/Dropbox/condition/ANALYSE/poids.txt", sep="\t")


M1<-lm(moyLong~TreatLarve,na.action=na.fail,data=INDICES)
anova(M1)
step(M1,test="F")
p2<-boxplot(INDICES$moyLong~INDICES$TreatLarve)
means <- tapply(INDICES$moyLong,INDICES$TreatLarve,mean)
points(means,col="red",pch=18)
par(mfrow=c(2,2))
plot(M1)
dev.off()

ms1 <- dredge(M1,rank="AICc", extra="R^2")
ms1
model.avg(ms1)
confset.95p <- get.models(ms1, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

tapply(INDICES$moyLong,INDICES$TreatLarve,length)

# Nl PNl 
#182 145

tapply(INDICES$moyLong,INDICES$TreatLarve,mean)

      Nl      PNl 
9.464093 9.671828


9.671828-9.464093

0.207735/9.464093*100


INDICES$traitement<-paste(INDICES$TreatLarve,INDICES$TreatAdult)
INDICES

M1<-lm(moyLong~TreatLarve,na.action=na.fail,data=INDICES)
summary(M1)


#Call:
#lm(formula = moyLong ~ TreatLarve, data = INDICES, na.action = na.fail)#

#Residuals:
#     Min       1Q   Median       3Q      Max 
#-1.97683 -0.35909 -0.01683  0.33091  2.07091 #

#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    9.46409    0.04359 217.100  < 2e-16 ***
#TreatLarvePNl  0.20773    0.06547   3.173  0.00165 ** 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#

#Residual standard error: 0.5881 on 325 degrees of freedom
#Multiple R-squared:  0.03005,   Adjusted R-squared:  0.02707 
#F-statistic: 10.07 on 1 and 325 DF,  p-value: 0.001652


#install.packages("MBESS")
library(MBESS)
ci.smd(ncp=3.173 , n.1=182, n.2=145, conf.level=1-.05) #

#$Lower.Conf.Limit.smd
#[1] 0.1330797

#$smd
#[1] 0.3532029

#$Upper.Conf.Limit.smd
#[1] 0.5727907



write.table(ms1, "~/Dropbox/condition/ANALYSE/long.txt", sep="\t")



M1<-lm(volume~TreatLarve*TreatAdult,na.action=na.fail,data=INDICES)
anova(M1)
step(M1,test="F")
p3<-boxplot(INDICES$volume~INDICES$milieu)
means <- tapply(INDICES$volume,INDICES$milieu,mean)
points(means,col="red",pch=18)
par(mfrow=c(2,2))
plot(M1)
dev.off()

ms1 <- dredge(M1,rank="AICc", extra="R^2")
ms1
model.avg(ms1)
confset.95p <- get.models(ms1, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)


M1<-lm(volume~TreatLarve+TreatAdult,na.action=na.fail,data=INDICES)
summary(M1)
#Call:
#lm(formula = volume ~ TreatLarve + TreatAdult, data = INDICES, 
#    na.action = na.fail)#

#Residuals:
#    Min      1Q  Median      3Q     Max 
#-32.497  -7.677  -0.229   7.031  63.027 #

#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     47.367      1.054  44.948  < 2e-16 ***
#TreatLarvePNl   12.930      1.299   9.954  < 2e-16 ***
#TreatAdultPNa    5.170      1.291   4.004 7.72e-05 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#

#Residual standard error: 11.66 on 324 degrees of freedom
#Multiple R-squared:  0.2682,    Adjusted R-squared:  0.2637 
#F-statistic: 59.38 on 2 and 324 DF,  p-value: < 2.2e-16

tapply(INDICES$volume,INDICES$TreatLarve,length)
# Nl PNl 
#182 145 

tapply(INDICES$volume,INDICES$TreatAdult,length)
# Na PNa 
#168 159


ci.smd(ncp=9.954 , n.1=182, n.2=145, conf.level=1-.05) 
#$Lower.Conf.Limit.smd
#[1] 0.873085#

#$smd
#[1] 1.108031#

#$Upper.Conf.Limit.smd
#[1] 1.341486


ci.smd(ncp=4.004 , n.1=168, n.2=159, conf.level=1-.05) 
#$Lower.Conf.Limit.smd
#[1] 0.2231677#

#$smd
#[1] 0.4430112#

#$Upper.Conf.Limit.smd
#[1] 0.662189




write.table(ms1, "~/Dropbox/condition/ANALYSE/vol.txt", sep="\t")






M1<-lm(OLSresidue~TreatLarve*TreatAdult,na.action=na.fail,data=INDICES)
anova(M1)
step(M1,test="F")
p4<-boxplot(INDICES$OLSresidue~INDICES$milieu)
means <- tapply(INDICES$OLSresidue,INDICES$milieu,mean)
points(OLSresidue,col="red",pch=18)
par(mfrow=c(2,2))
plot(M1)

ms1 <- dredge(M1,rank="AICc", extra="R^2")
ms1
model.avg(ms1)
confset.95p <- get.models(ms1, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

write.table(ms1, "~/Dropbox/condition/ANALYSE/OLS.txt", sep="\t")


M1<-lm(MSL~TreatLarve*TreatAdult,na.action=na.fail,data=INDICES)
anova(M1)
step(M1,test="F")
p5<-boxplot(INDICES$MSL~INDICES$milieu)
means <- tapply(MSL,milieu,mean)
points(MSL,col="red",pch=18)
par(mfrow=c(2,2))
plot(M1)
dev.off()

ms1 <- dredge(M1,rank="AICc", extra="R^2")
ms1
model.avg(ms1)
confset.95p <- get.models(ms1, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

write.table(ms1, "~/Dropbox/condition/ANALYSE/MSL.txt", sep="\t")


M1<-lm(VSL~TreatLarve*TreatAdult,na.action=na.fail,data=INDICES)
anova(M1)
step(M1,test="F")
p6<-boxplot(INDICES$VSL~INDICES$milieu)
means <- tapply(INDICES$VSL,INDICES$milieu,mean)
points(VSL,col="red",pch=18)
par(mfrow=c(2,2))
plot(M1)
dev.off()


ms1 <- dredge(M1,rank="AICc", extra="R^2")
ms1
model.avg(ms1)
confset.95p <- get.models(ms1, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)


write.table(ms1, "~/Dropbox/condition/ANALYSE/VSL.txt", sep="\t")



M1<-lm(VSL~TreatLarve,na.action=na.fail,data=dosages)


summary(M1)
#Call:
#lm(formula = VSL ~ TreatLarve, data = dosages, na.action = na.fail)#

#Residuals:
#     Min       1Q   Median       3Q      Max 
#-18.6768  -5.2111  -0.1596   5.8961  22.4293 #

#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    51.398      1.100  46.741  < 2e-16 ***
#TreatLarvePN    4.988      1.806   2.762  0.00701 ** 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#

#Residual standard error: 8.229 on 87 degrees of freedom
#Multiple R-squared:  0.08062,   Adjusted R-squared:  0.07005 
#F-statistic: 7.629 on 1 and 87 DF,  p-value: 0.007006


library(MBESS) 

ci.smd(ncp=8.631 , n.1=113, n.2=85, conf.level=1-.05) #to calculate cohen D and 95CI

names(INDICES)

Long<-ggplot(INDICES, aes(y=moyLong,x=TreatLarve))+geom_boxplot(outlier.shape = NA)+geom_jitter(position = position_jitter(0.1),aes(shape=TreatLarve),size=3)+theme_bw()+scale_shape_manual(values=c(1,19),name="", breaks=c("N","PN"),labels=c("Rich larval feeding treatment","Poor larval feeding treatment"))+ labs(x = "Larval feeding treatment", y = "Elytron length (cm)")+ scale_x_discrete(labels=c("Nl" = "Rich larval\n(R.L.)","PNl" = "Poor larval\n(P.L.)"))+ggtitle("a.")+  theme(axis.text.x =element_text(size=10),text = element_text(size=15))
Long


Poids<-ggplot(INDICES, aes(y=poids,x=milieu))+geom_boxplot(outlier.shape = NA,aes(colour=TreatAdult,shape=TreatLarve),show.legend = FALSE)+geom_jitter(position = position_jitter(0.1),aes(colour=TreatAdult,shape=TreatLarve),size=3)+theme_bw()+scale_colour_manual(values=c("black","red"),name="Feeding treatment", breaks=c("N","PN"),labels=c("Rich adult feeding treatment","Poor adult feeding treatment"))+scale_shape_manual(values=c(1,19),name="", breaks=c("N","PN"),labels=c("Rich larval feeding treatment","Poor larval feeding treatment"))+ labs(x = "Feeding treatment", y = "Mass (mg)")+ scale_x_discrete(labels=c("NlNa" = "Rich larval\nand\nrich adult\n(R.L./R.A.)", "NlPNa" = "Rich larval\nand\npoor adult\n(R.L./P.A.)","PNlNa" = "Poor larval\nand\nrich adult\n(P.L./R.A.)","PNlPNa" = "Poor larval\nand\npoor adult\n(P.L./P.A.)"))+ggtitle("b.")+  theme(axis.text.x =element_text(size=10),text = element_text(size=15))+guides(linetype=guide_legend(FALSE))
Poids



Vol<-ggplot(INDICES, aes(y=volume,x=milieu))+geom_boxplot(outlier.shape = NA,aes(colour=TreatAdult),show.legend = FALSE)+geom_jitter(position = position_jitter(0.1),aes(colour=TreatAdult,shape=TreatLarve),size=3)+theme_bw()+scale_colour_manual(values=c("black","red"),name="Feeding treatment", breaks=c("N","PN"),labels=c("Rich adult feeding treatment","Poor adult feeding treatment"))+scale_shape_manual(values=c(1,19),name="", breaks=c("N","PN"),labels=c("Rich larval feeding treatment","Poor larval feeding treatment"))+ labs(x = "Feeding treatment", y = "Volume (cm3)")+ scale_x_discrete(labels=c("NlNa" = "Rich larval\nand\nrich adult\n(R.L./R.A.)", "NlPNa" = "Rich larval\nand\npoor adult\n(R.L./P.A.)","PNlNa" = "Poor larval\nand\nrich adult\n(P.L./R.A.)","PNlPNa" = "Poor larval\nand\npoor adult\n(P.L./P.A.)"))+ggtitle("c.")+  theme(axis.text.x =element_text(size=10),text = element_text(size=15))
Vol





OLS<-ggplot(INDICES, aes(y=OLSresidue,x=milieu))+geom_boxplot(outlier.shape = NA,aes(colour=TreatAdult,shape=TreatLarve),show.legend = FALSE)+geom_jitter(position = position_jitter(0.1),aes(colour=TreatAdult,shape=TreatLarve),size=3)+theme_bw()+scale_colour_manual(values=c("black","red"),name="Feeding treatment", breaks=c("N","PN"),labels=c("Rich adult feeding treatment","Poor adult feeding treatment"))+scale_shape_manual(values=c(1,19),name="", breaks=c("N","PN"),labels=c("Rich larval feeding treatment","Poor larval feeding treatment"))+ labs(x = "Feeding treatment", y = "OLSresid")+ scale_x_discrete(labels=c("NlNa" = "Rich larval\nand\nrich adult\n(R.L./R.A.)", "NlPNa" = "Rich larval\nand\npoor adult\n(R.L./P.A.)","PNlNa" = "Poor larval\nand\nrich adult\n(P.L./R.A.)","PNlPNa" = "Poor larval\nand\npoor adult\n(P.L./R.A.)"))+ggtitle("d.")+  theme(axis.text.x =element_text(size=10),text = element_text(size=15))+guides(linetype=guide_legend(FALSE))
OLS



SMI<-ggplot(INDICES, aes(y=MSL,x=milieu))+geom_boxplot(outlier.shape = NA,aes(colour=TreatAdult),show.legend = FALSE)+geom_jitter(position = position_jitter(0.1),aes(colour=TreatAdult,shape=TreatLarve),size=3)+theme_bw()+scale_colour_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("N","PN"),labels=c("Rich adult feeding treatment","Poor adult feeding treatment"))+scale_shape_manual(values=c(1,19),name="", breaks=c("N","PN"),labels=c("Rich larval feeding treatment","Poor larval feeding treatment"))+ labs(x = "Feeding treatment", y = "SMI (mg)")+ scale_x_discrete(labels=c("NlNa" = "Rich larval\nand\nrich adult\n(R.L./R.A.)", "NlPNa" = "Rich larval\nand\npoor adult\n(R.L./P.A.)","PNlNa" = "Poor larval\nand\nrich adult\n(P.L./R.A.)","PNlPNa" = "Poor larval\nand\npoor adult\n(P.L./P.A.)"))+ggtitle("e.")+  theme(axis.text.x =element_text(size=10),text = element_text(size=15))
SMI


SVI<-ggplot(INDICES, aes(y=VSL,x=milieu))+geom_boxplot(outlier.shape = NA,aes(colour=TreatAdult),show.legend = FALSE)+geom_jitter(position = position_jitter(0.1),aes(colour=TreatAdult,shape=TreatLarve),size=3)+theme_bw()+scale_colour_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("N","PN"),labels=c("Rich adult feeding treatment","Poor adult feeding treatment"))+scale_shape_manual(values=c(1,19),name="", breaks=c("N","PN"),labels=c("Rich larval feeding treatment","Poor larval feeding treatment"))+ labs(x = "Feeding treatment", y = "SVI (cm3)")+ scale_x_discrete(labels=c("NlNa" = "Rich larval\nand\nrich adult\n(R.L./R.A.)", "NlPNa" = "Rich larval\nand\npoor adult\n(R.L./P.A.)","PNlNa" = "Poor larval\nand\nrich adult\n(P.L./R.A.)","PNlPNa" = "Poor larval\nand\npoor adult\n(P.L./P.A.)"))+ggtitle("f.")+  theme(axis.text.x =element_text(size=10),text = element_text(size=15))
SVI


library(gridExtra)



library(gtable)

legend=gtable_filter(ggplotGrob(Poids), "guide-box")
grid.arrange(Long,OLS,Poids,SMI,Vol,SVI,Dens,DSL,ncol=2)





png("/home/mark/Dropbox/condition/ANALYSE/FigureBoxplotCondMilieu.png",height=1485, width=1050,res=100)

grid.arrange(arrangeGrob(Long+ theme(legend.position="none"),OLS+ theme(legend.position="none"),Poids+ theme(legend.position="none"),SMI+ theme(legend.position="none"),Vol+ theme(legend.position="none"),SVI+ theme(legend.position="none"),ncol=2))

dev.off()




Dens<-ggplot(INDICES, aes(y=densite,x=milieu))+geom_boxplot(outlier.shape = NA,aes(colour=TreatAdult),show.legend = FALSE)+geom_jitter(position = position_jitter(0.1),aes(colour=TreatAdult,shape=TreatLarve),size=3)+theme_bw()+scale_colour_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("N","PN"),labels=c("Rich adult feeding treatment","Poor adult feeding treatment"))+scale_shape_manual(values=c(1,19),name="", breaks=c("N","PN"),labels=c("Rich larval feeding treatment","Poor larval feeding treatment"))+ labs(x = "Feeding treatment", y = "Density g/cm3")+ scale_x_discrete(labels=c("NlNa" = "Rich larval\nand\nrich adult\n(R.L./R.A.)", "NlPNa" = "Rich larval\nand\npoor adult\n(R.L./P.A.)","PNlNa" = "Poor larval\nand\nrich adult\n(P.L./R.A.)","PNlPNa" = "Poor larval\nand\npoor adult\n(P.L./P.A.)"))+ggtitle("a.")+  theme(axis.text.x =element_text(size=10),text = element_text(size=15))
Dens




DSL<-ggplot(INDICES, aes(y=DSL,x=milieu))+geom_boxplot(outlier.shape = NA,aes(colour=TreatAdult),show.legend = FALSE)+geom_jitter(position = position_jitter(0.1),aes(colour=TreatAdult,shape=TreatLarve),size=3)+theme_bw()+scale_colour_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("N","PN"),labels=c("Rich adult feeding treatment","Poor adult feeding treatment"))+scale_shape_manual(values=c(1,19),name="", breaks=c("N","PN"),labels=c("Rich larval feeding treatment","Poor larval feeding treatment"))+ labs(x = "Feeding treatment", y = "DSL (g/cm3)")+ scale_x_discrete(labels=c("NlNa" = "Rich larval\nand\nrich adult\n(R.L./R.A.)", "NlPNa" = "Rich larval\nand\npoor adult\n(R.L./P.A.)","PNlNa" = "Poor larval\nand\nrich adult\n(P.L./R.A.)","PNlPNa" = "Poor larval\nand\npoor adult\n(P.L./P.A.)"))+ggtitle("b.")+  theme(axis.text.x =element_text(size=10),text = element_text(size=15))
DSL


png("/home/mark/Dropbox/condition/ANALYSE/FigureBoxplotCondMilieuDens.png",height=1050, width=1485,res=100)

grid.arrange(arrangeGrob(Dens+ theme(legend.position="none"),DSL+ theme(legend.position="none"),ncol=2))

dev.off()






png("/home/mark/Dropbox/condition/ANALYSE/FigureBoxplotCondMilieu.png",width=2100, height=2970,res=300)
#pdf("/home/mark/Dropbox/condition/ANALYSE/FigureBoxplotCondMilieu.pdf",paper="a4",width=8.267, height=11.692)
par(mfrow=c(3,2))

boxplot(INDICES$poids~INDICES$milieu, names=c("R.L/R.A","R.L/P.A","P.L/R.A","P.L/P.A"), ylab="Mass (g)",outline=FALSE,ylim=c(min(INDICES$poids),max(INDICES$poids)), main="A.")
stripchart(INDICES$poids~INDICES$milieu, vertical = TRUE,add = TRUE,names="",pch = 16,method="jitter")

boxplot(INDICES$OLSresidue~INDICES$milieu, names=c("R.L/R.A","R.L/P.A","P.L/R.A","P.L/P.A"), ylab="OLS residulals",outline=FALSE,ylim=c(min(INDICES$OLSresidue),max(INDICES$OLSresidue)), main="D.")
stripchart(INDICES$OLSresidue~INDICES$milieu, vertical = TRUE,add = TRUE,names="",pch = 16,method="jitter")


boxplot(INDICES$moyLong~INDICES$milieu, names=c("R.L/R.A","R.L/P.A","P.L/R.A","P.L/P.A"), ylab="Elytron length (mm)",outline=FALSE,ylim=c(min(INDICES$moyLong),max(INDICES$moyLong)), main="B.")
stripchart(INDICES$moyLong~INDICES$milieu, vertical = TRUE,add = TRUE,names="",pch = 16,method="jitter")

boxplot(INDICES$MSL~INDICES$milieu, names=c("R.L/R.A","R.L/P.A","P.L/R.A","P.L/P.A"), ylab="MSL (g)",outline=FALSE,ylim=c(min(INDICES$MSL),max(INDICES$MSL)), main="E.")
stripchart(INDICES$MSL~INDICES$milieu, vertical = TRUE,add = TRUE,names="",pch = 16,method="jitter")


boxplot(INDICES$volume~INDICES$milieu, names=c("R.L/R.A","R.L/P.A","P.L/R.A","P.L/P.A"), ylab="Volume (mm3)", xlab="Food treatmeant",outline=FALSE,ylim=c(min(INDICES$volume),max(INDICES$volume)), main="C.")
stripchart(INDICES$volume~INDICES$milieu, vertical = TRUE,add = TRUE,names="",pch = 16,method="jitter")

boxplot(INDICES$VSL~INDICES$milieu, names=c("R.L/R.A","R.L/P.A","P.L/R.A","P.L/P.A"), ylab="SVI (mm3)", xlab="Food treatmeant",outline=FALSE,ylim=c(min(INDICES$VSL),max(INDICES$VSL)), main="F.")
stripchart(INDICES$VSL~INDICES$milieu, vertical = TRUE,add = TRUE,names="",pch = 16,method="jitter")

dev.off()




png("/home/mark/Dropbox/condition/ANALYSE/FigureBoxplotCondLarve.png",width=2100, height=2970,res=300)
#pdf("/home/mark/Dropbox/condition/ANALYSE/FigureBoxplotCondMilieu.pdf",paper="a4",width=8.267, height=11.692)
par(mfrow=c(3,2))

boxplot(INDICES$poids~INDICES$TreatLarve, names=c("R.L","P.L"), ylab="Mass (g)",outline=FALSE,ylim=c(min(INDICES$poids),max(INDICES$poids)), main="A.")
stripchart(INDICES$poids~INDICES$TreatLarve, vertical = TRUE,add = TRUE,names="",pch = 16,method="jitter")

boxplot(INDICES$OLSresidue~INDICES$TreatLarve, names=c("R.L","P.L"), ylab="OLS residulals",outline=FALSE,ylim=c(min(INDICES$OLSresidue),max(INDICES$OLSresidue)), main="D.")
stripchart(INDICES$OLSresidue~INDICES$TreatLarve, vertical = TRUE,add = TRUE,names="",pch = 16,method="jitter")


boxplot(INDICES$moyLong~INDICES$TreatLarve, names=c("R.L","P.L"), ylab="Elytron length (mm)",outline=FALSE,ylim=c(min(INDICES$moyLong),max(INDICES$moyLong)), main="B.")
stripchart(INDICES$moyLong~INDICES$TreatLarve, vertical = TRUE,add = TRUE,names="",pch = 16,method="jitter")

boxplot(INDICES$MSL~INDICES$TreatLarve, names=c("R.L","P.L"), ylab="MSL (g)",outline=FALSE,ylim=c(min(INDICES$MSL),max(INDICES$MSL)), main="E.")
stripchart(INDICES$MSL~INDICES$TreatLarve, vertical = TRUE,add = TRUE,names="",pch = 16,method="jitter")


boxplot(INDICES$volume~INDICES$TreatLarve, names=c("R.L","P.L"), ylab="Volume (mm3)", xlab="Food treatmeant",outline=FALSE,ylim=c(min(INDICES$volume),max(INDICES$volume)), main="C.")
stripchart(INDICES$volume~INDICES$TreatLarve, vertical = TRUE,add = TRUE,names="",pch = 16,method="jitter")

boxplot(INDICES$VSL~INDICES$TreatLarve, names=c("R.L","P.L"), ylab="SVI (mm3)", xlab="Food treatmeant",outline=FALSE,ylim=c(min(INDICES$VSL),max(INDICES$VSL)), main="F.")
stripchart(INDICES$VSL~INDICES$TreatLarve, vertical = TRUE,add = TRUE,names="",pch = 16,method="jitter")

dev.off()



pdf("/home/mark/Dropbox/condition/ANALYSE/FigureBoxplotCondMilieu.pdf",paper="a4",width=4.13, height=11.692)
par(mfrow=c(3,1))

boxplot(INDICES$OLSresidue~INDICES$TreatLarve, names=c("R.L","P.L"),ylab="OLS residulals",xlab="Food treatment",outline=FALSE,ylim=c(min(INDICES$OLSresidue),max(INDICES$OLSresidue)), main="A.")
stripchart(INDICES$OLSresidue~INDICES$TreatLarve, vertical = TRUE,add = TRUE,names="",pch = 16,method="jitter")

boxplot(INDICES$MSL~INDICES$TreatLarve, names=c("R.L","P.L"),ylab="MSL (g)",xlab="Food treatment",outline=FALSE,ylim=c(min(INDICES$MSL),max(INDICES$MSL)), main="B.")
stripchart(INDICES$MSL~INDICES$TreatLarve, vertical = TRUE,add = TRUE,names="",pch = 16,method="jitter")


boxplot(INDICES$VSL~INDICES$TreatLarve, names=c("R.L","P.L"),ylab="SVI (mm3)",xlab="Food treatment",outline=FALSE,ylim=c(min(INDICES$VSL),max(INDICES$VSL)), main="C.")
stripchart(INDICES$VSL~INDICES$TreatLarve, vertical = TRUE,add = TRUE,names="",pch = 16,method="jitter")

dev.off()


OLSresidue
MSL
VSL

Indices<-(merge(OLS,MSL,by="individu"))
Indices<-(merge(Indices,VSL,by="individu"))
head(Indices)


write.table(Indices, "~/Dropbox/condition/ANALYSE/indices.txt", sep="\t")


##########################
# selection tlarve*SMI??



library(ggplot2)
ggplot(data=INDICES,aes(x = MSL,fill=INDICES$TreatLarve))+geom_histogram(binwidth=0.5)

ggplot(data=INDICES,aes(x = MSL,color=TreatLarve:TreatAdult))+geom_density()+theme_classic()

library(dplyr)
#library(plyr)
cdat <- INDICES %>%
  group_by(TreatLarve) %>%
  summarise(MSL.mean = mean(MSL),
            sem = sd(MSL)/sqrt(length(MSL)),
            upper.95=quantile(MSL,probs=c(0.975)),
            lower.95=quantile(MSL,probs=c(0.025)),
            ci.low = mean(MSL) - 1.96*sem,
            ci.upp = mean(MSL) + 1.96*sem)
cdat

#mu <- ddply(INDICES, "TreatLarve", summarise, grp.mean=mean(MSL))

#mu.95.upper <- ddply(INDICES, "TreatLarve", summarise, upper.95=quantile(INDICES$MSL,probs=c(0.975)))

#mu.95.lower <- ddply(INDICES, "TreatLarve", summarise, lower.95=quantile(INDICES$MSL,probs=c(0.025)))

#mu.merge<-merge(mu.95.lower,mu.95.upper,by="TreatLarve")
#mu.merge
#mu.merge<-merge(mu,mu.merge,by="TreatLarve")
#mu.merge

#install.packages("dplyr")
#library(plyr)

cdat.dens <- ggplot_build(ggplot(INDICES, aes(x=MSL, colour=TreatLarve)) + geom_density())$data[[1]] %>%
  mutate(TreatLarve = ifelse(group == 1, "Nl", "PNl"))%>%left_join(cdat) %>%
  select(y, x, TreatLarve, MSL.mean, sem, ci.low, ci.upp) %>%
  group_by(TreatLarve) %>%
  mutate(dens.mean = approx(x, y, xout = MSL.mean)[[2]],
         dens.cilow = approx(x, y, xout = ci.low)[[2]],
         dens.ciupp = approx(x, y, xout = ci.upp)[[2]]) %>%
  select(-y, -x) %>%
  slice(1)

ribbon <- ggplot_build(ggplot(INDICES, aes(x=MSL, colour=TreatLarve)) + geom_density())$data[[1]] %>%
  mutate(TreatLarve = ifelse(group == 1, "Nl", "PNl")) %>%
  left_join(cdat.dens) %>%
  group_by(TreatLarve) %>%
  filter(x >= ci.low & x <= ci.upp) %>%
  select(TreatLarve, x, y)


ribbon <- rbind(data.frame(TreatLarve = c("Nl", "PNl"), x = c(cdat.dens$ci.low[cdat.dens$TreatLarve=="Nl"], cdat.dens$ci.low[cdat.dens$TreatLarve=="PNl"]), y = c(0, 0)), as.data.frame(ribbon),               data.frame(TreatLarve = c("Nl", "PNl"), x = c(cdat.dens$ci.upp[cdat.dens$TreatLarve=="Nl"], cdat.dens$ci.upp[cdat.dens$TreatLarve=="PNl"]), y = c(0, 0)))

names(ribbon)



annotate(geom="text", x=120, y=0.054, label=" ",size=rel(3))


'Fligner-Killeen Test of Homogeneity of Variances:\nchi-squared = 9.712, p-value = 0.002\n\nGeneralized least squares (GLS): F'
~"Fligner-Killeen Test of Homogeneity of Variances:\nchi-squared = 9.712, p-value = 0.002 \n\nGeneralized least squares (GLS):"

label2<-expression(~"\tF"[1][", "][325]~"= 0.833; p = 0.362")

DensityMSL<-ggplot() + 
geom_density(data = INDICES, aes(x = MSL, colour = TreatLarve)) + 
geom_polygon(data = ribbon, aes(x = x, y =y, fill = TreatLarve), alpha = .2) + 
geom_segment(data = cdat.dens, aes(x = MSL.mean, xend = MSL.mean, y = 0, yend = dens.mean, colour = TreatLarve), linetype = "dashed", size = 1)+
theme_bw()+
scale_colour_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("Nl","PNl"),labels=c("Rich larval","Poor larval"))+
scale_fill_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("Nl","PNl"),labels=c("Rich larval","Poor larval"))+
annotate(geom = 'text', x = -Inf, y = Inf, label = "\tFligner-Killeen Test of Homogeneity of Variances:\n\tchi-squared = 9.712, p-value = 0.002 \n\tGeneralized least squares (GLS):", hjust = 0, vjust = 1.25,size=rel(4))+
annotate(geom = 'text', x = -Inf, y = Inf, label = label2, hjust = 0, vjust = 5.75,size=rel(4))+
theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1)), legend.title=element_text(size= rel(1)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+xlab("SMI (mg)")+ylab("Density")+ylim(0,0.069)
DensityMSL

library(nlme)
M1<-gls(MSL~TreatLarve,data=INDICES, weights = varIdent(form = ~1 | TreatLarve),na.action=na.fail,method="ML")
drop1(M1,test="Chi")
anova(M1)




library(dplyr)
cdat <- INDICES %>%
  group_by(TreatLarve) %>%
  summarise(poids.mean = mean(poids),
            sem = sd(poids)/sqrt(length(poids)),
            upper.95=quantile(poids,probs=c(0.975)),
            lower.95=quantile(poids,probs=c(0.025)),
            ci.low = mean(poids) - 1.96*sem,
            ci.upp = mean(poids) + 1.96*sem)
cdat


cdat.dens <- ggplot_build(ggplot(INDICES, aes(x=poids, colour=TreatLarve)) + geom_density())$data[[1]] %>%
  mutate(TreatLarve = ifelse(group == 1, "Nl", "PNl"))%>%left_join(cdat) %>%
  dplyr::select(y, x, TreatLarve, poids.mean, sem, ci.low, ci.upp) %>%
  dplyr::group_by(TreatLarve) %>%
  dplyr::mutate(dens.mean = approx(x, y, xout = poids.mean)[[2]],
         dens.cilow = approx(x, y, xout = ci.low)[[2]],
         dens.ciupp = approx(x, y, xout = ci.upp)[[2]]) %>%
   dplyr::select(-y, -x) %>%
  slice(1)

ribbon <- ggplot_build(ggplot(INDICES, aes(x=poids, colour=TreatLarve)) + geom_density())$data[[1]] %>%
  mutate(TreatLarve = ifelse(group == 1, "Nl", "PNl")) %>%
  left_join(cdat.dens) %>%
  group_by(TreatLarve) %>%
  filter(x >= ci.low & x <= ci.upp) %>%
  dplyr::select(TreatLarve, x, y)


ribbon <- rbind(data.frame(TreatLarve = c("Nl", "PNl"), x = c(cdat.dens$ci.low[cdat.dens$TreatLarve=="Nl"], cdat.dens$ci.low[cdat.dens$TreatLarve=="PNl"]), y = c(0, 0)), as.data.frame(ribbon),               data.frame(TreatLarve = c("Nl", "PNl"), x = c(cdat.dens$ci.upp[cdat.dens$TreatLarve=="Nl"], cdat.dens$ci.upp[cdat.dens$TreatLarve=="PNl"]), y = c(0, 0)))

names(ribbon)

label2<-expression(~"\tF"[1][", "][325]~"= 6.648; p = 0.010")

DensityMass<-ggplot() + 
geom_density(data = INDICES, aes(x = poids, colour = TreatLarve)) + geom_polygon(data = ribbon, aes(x = x, y =y, fill = TreatLarve), alpha = .2) + 
geom_segment(data = cdat.dens, aes(x = poids.mean, xend = poids.mean, y = 0, yend = dens.mean, colour = TreatLarve), linetype = "dashed", size = 1)+theme_bw()+
scale_colour_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("Nl","PNl"),labels=c("Rich larval","Poor larval"))+
scale_fill_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("Nl","PNl"),labels=c("Rich larval","Poor larval"))+
annotate(geom = 'text', x = -Inf, y = Inf, label = "\tFligner-Killeen Test of Homogeneity of Variances:\n\tchi-squared = 0.289, p-value = 0.591 \n\tLinear model (LM):", hjust = 0, vjust = 1.25,size=rel(4))+
annotate(geom = 'text', x = -Inf, y = Inf, label = label2, hjust = 0, vjust = 5.75,size=rel(4))+
theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+xlab("Mass (mg)")+ylab("Density")+ylim(0,0.035)

DensityMass


M1<-lm(poids~TreatLarve,data=INDICES,na.action=na.fail)
drop1(M1,test="F")
anova(M1)
dredge(M1)
summary(M1)

partial.d<-function(t.val,df,n1,n2){
  d<-t.val*(n1+n2)/(sqrt(n1*n2)*sqrt(df))
  names(d)<-"effect size d"
  return(d)
}

tapply(INDICES$poids,INDICES$TreatLarve,length)

partial.d(2.578,325,182,145)
#effect size d 
#     0.287852 

coef(summary(M1))[2,3]

d.part <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula,data=d)
  return(partial.d(coef(summary(fit))[2,3],325,182,145))
}

# bootstrapping with 10000 replications
(results <- boot(data=INDICES, statistic=d.part,R=10000, formula=poids~TreatLarve))

#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = INDICES, statistic = d.part, R = 10000, formula = poids ~ 
#    TreatLarve)#
#

#Bootstrap Statistics :
#     original      bias    std. error
#t1* 0.2866783 0.002301956   0.1147747


library(boot)
(Boot<-boot.ci(results, type="bca"))
#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   ( 0.0613,  0.5141 )  
#Calculations and Intervals on Original Scale



library(MBESS) 

ci.smd(ncp=2.578 , n.1=182, n.2=145, conf.level=1-.05)
#$Lower.Conf.Limit.smd
#[1] 0.06746652

#$smd
#[1] 0.2869704

#$Upper.Conf.Limit.smd
#[1] 0.5060372





cdat <- INDICES %>%
  group_by(TreatLarve) %>%
  summarise(moyLong.mean = mean(moyLong),
            sem = sd(moyLong)/sqrt(length(moyLong)),
            upper.95=quantile(moyLong,probs=c(0.975)),
            lower.95=quantile(moyLong,probs=c(0.025)),
            ci.low = mean(moyLong) - 1.96*sem,
            ci.upp = mean(moyLong) + 1.96*sem)
cdat


cdat.dens <- ggplot_build(ggplot(INDICES, aes(x=moyLong, colour=TreatLarve)) + geom_density())$data[[1]] %>%
  mutate(TreatLarve = ifelse(group == 1, "Nl", "PNl"))%>%left_join(cdat) %>%
  select(y, x, TreatLarve, moyLong.mean, sem, ci.low, ci.upp) %>%
  group_by(TreatLarve) %>%
  mutate(dens.mean = approx(x, y, xout = moyLong.mean)[[2]],
         dens.cilow = approx(x, y, xout = ci.low)[[2]],
         dens.ciupp = approx(x, y, xout = ci.upp)[[2]]) %>%
  select(-y, -x) %>%
  slice(1)

ribbon <- ggplot_build(ggplot(INDICES, aes(x=moyLong, colour=TreatLarve)) + geom_density())$data[[1]] %>%
  mutate(TreatLarve = ifelse(group == 1, "Nl", "PNl")) %>%
  left_join(cdat.dens) %>%
  group_by(TreatLarve) %>%
  filter(x >= ci.low & x <= ci.upp) %>%
  select(TreatLarve, x, y)


ribbon <- rbind(data.frame(TreatLarve = c("Nl", "PNl"), x = c(cdat.dens$ci.low[cdat.dens$TreatLarve=="Nl"], cdat.dens$ci.low[cdat.dens$TreatLarve=="PNl"]), y = c(0, 0)), as.data.frame(ribbon),               data.frame(TreatLarve = c("Nl", "PNl"), x = c(cdat.dens$ci.upp[cdat.dens$TreatLarve=="Nl"], cdat.dens$ci.upp[cdat.dens$TreatLarve=="PNl"]), y = c(0, 0)))

names(ribbon)



label2<-expression(~"\tF"[1][", "][325]~"= 10.069; p = 0.002")

DensityLong<-ggplot() + 
geom_density(data = INDICES, aes(x = moyLong, colour = TreatLarve)) + geom_polygon(data = ribbon, aes(x = x, y =y, fill = TreatLarve), alpha = .2) + 
geom_segment(data = cdat.dens, aes(x = moyLong.mean, xend = moyLong.mean, y = 0, yend = dens.mean, colour = TreatLarve), linetype = "dashed", size = 1)+theme_bw()+
scale_colour_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("Nl","PNl"),labels=c("Rich larval","Poor larval"))+
scale_fill_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("Nl","PNl"),labels=c("Rich larval","Poor larval"))+
annotate(geom = 'text', x = -Inf, y = Inf, label = "\tFligner-Killeen Test of Homogeneity of Variances:\n\t chi-squared = 0.101, p-value = 0.378 \n\tLinear model (LM):", hjust = 0, vjust = 1.25,size=rel(4))+
annotate(geom = 'text', x = -Inf, y = Inf, label = label2, hjust = 0, vjust = 5.75,size=rel(4))+
theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+xlab("Elytron length (mm)")+ylab("Density")+ylim(0,1.1)

DensityLong

M1<-lm(moyLong~TreatLarve,data=INDICES,na.action=na.fail)
drop1(M1,test="F")
anova(M1)
summary(M1)

DensityMSL
DensityMass
DensityLong


partial.d<-function(t.val,df,n1,n2){
  d<-t.val*(n1+n2)/(sqrt(n1*n2)*sqrt(df))
  names(d)<-"effect size d"
  return(d)
}

tapply(INDICES$moyLong,INDICES$TreatLarve,length)

partial.d(3.173,325,182,145)
#effect size d 
#     0.354288 

coef(summary(M1))[2,3]

d.part <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula,data=d)
  return(partial.d(coef(summary(fit))[2,3],325,182,145))
}

# bootstrapping with 10000 replications
(results <- boot(data=INDICES, statistic=d.part,R=10000, formula=moyLong~TreatLarve))
#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = INDICES, statistic = d.part, R = 10000, formula = moyLong ~ 
#    TreatLarve)#
#

#Bootstrap Statistics :
#     original      bias    std. error
#t1* 0.3543114 0.004592481     0.11815


library(boot)
(Boot<-boot.ci(results, type="bca"))
#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   ( 0.1253,  0.5827 )  
#Calculations and Intervals on Original Scale



library(MBESS) 

ci.smd(ncp=3.173 , n.1=182, n.2=145, conf.level=1-.05)



M1<-lm(MSL~TreatLarve,data=INDICES,na.action=na.fail)
drop1(M1,test="F")
anova(M1)
summary(M1)

DensityMSL
DensityMass
DensityLong


partial.d<-function(t.val,df,n1,n2){
  d<-t.val*(n1+n2)/(sqrt(n1*n2)*sqrt(df))
  names(d)<-"effect size d"
  return(d)
}

tapply(INDICES$moyLong,INDICES$TreatLarve,length)

partial.d(-0.896,325,182,145)
#effect size d 
#   -0.1000448 

coef(summary(M1))[2,3]

d.part <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula,data=d)
  return(partial.d(coef(summary(fit))[2,3],325,182,145))
}

# bootstrapping with 10000 replications
(results <- boot(data=INDICES, statistic=d.part,R=10000, formula=MSL~TreatLarve))

#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = INDICES, statistic = d.part, R = 10000, formula = MSL ~ 
#    TreatLarve)#
#

#Bootstrap Statistics :
#      original     bias    std. error
#t1* -0.1000919 0.09933445   0.1124249


library(boot)
(Boot<-boot.ci(results, type="bca"))
#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   (-0.4163,  0.0184 )  
#Calculations and Intervals on Original Scale
#Warning : BCa Intervals used Extreme Quantiles
#Some BCa intervals may be unstable
#Warning message:
#In norm.inter(t, adj.alpha) : extreme order statistics used as endpoints



library(MBESS) 

ci.smd(ncp=3.173 , n.1=182, n.2=145, conf.level=1-.05)



library(grid)

library(gridExtra)
library(gtable)


legend = gtable_filter(ggplotGrob(DensityMSL), "guide-box")




png("/home/mark/Dropbox/condition/ANALYSE/Density.png",width=2100, height=2970,res=300)

pushViewport(viewport(layout = grid.layout(3, 14)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

print(DensityMSL+ theme(legend.position="none"), vp = vplayout(1, 1:9))
print(DensityMass+ theme(legend.position="none"), vp = vplayout(2, 1:9))
print(DensityLong+ theme(legend.position="none"), vp = vplayout(3, 1:9))
legend$vp = viewport(layout.pos.row = 2, layout.pos.col = 10:14)
grid.draw(legend)

dev.off()


png("/home/mark/Dropbox/condition/ANALYSE/Density.png",width=2100, height=2970,res=300)

pushViewport(viewport(layout = grid.layout(4, 14)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

print(DensityMSL+ theme(legend.position="none"), vp = vplayout(1:2, 1:9))
print(DensityLong+ theme(legend.position="none"), vp = vplayout(3:4, 1:9))
legend$vp = viewport(layout.pos.row = 2:3, layout.pos.col = 10:14)
grid.draw(legend)

dev.off()




ggplot(data=INDICES,aes(x = scale(MSL),color=TreatLarve))+geom_density()+theme_classic()




ggplot(data=INDICES,aes(x = poids,color=INDICES$TreatLarve))+geom_density()+theme_classic()
ggplot(data=INDICES,aes(x = poids,color=INDICES$TreatLarve:TreatAdult))+geom_density()+theme_classic()


ggplot(data=INDICES,aes(x = moyLong,color=INDICES$TreatLarve))+geom_density()+theme_classic()


names(INDICES)

library(olsrr)

INDICES$lMSL<-log10(INDICES$MSL)
INDICES$lpoids<-log10(INDICES$poids)
INDICES$lmoyLong<-log10(INDICES$moyLong)


ols_test_bartlett(INDICES,MSL,TreatLarve)
ols_test_bartlett(INDICES,poids,TreatLarve)
ols_test_bartlett(INDICES,moyLong,TreatLarve)



M1<-lm(MSL~TreatLarve,data=INDICES)
step(M1)
ols_test_breusch_pagan(M1, rhs = TRUE, multiple = TRUE)

ols_test_score(M1, rhs = TRUE)

ols_test_f(M1, rhs = TRUE)


M1<-lm(poids~TreatLarve+TreatAdult,data=INDICES)
ols_test_breusch_pagan(M1, rhs = TRUE, multiple = TRUE)

M1<-lm(moyLong~TreatLarve,data=INDICES)
ols_test_breusch_pagan(M1, rhs = TRUE, multiple = TRUE)





ks.test(scale(INDICES$MSL[INDICES$TreatLarve=="Nl"]),scale(INDICES$MSL[INDICES$TreatLarve=="PNl"]), simulate=TRUE, B=10000)


ks.test(scale(INDICES$poids[INDICES$TreatLarve=="Nl"]),scale(INDICES$poids[INDICES$TreatLarve=="PNl"]), simulate=TRUE, B=10000)

ks.test(scale(INDICES$moyLong[INDICES$TreatLarve=="Nl"]),scale(INDICES$moyLong[INDICES$TreatLarve=="PNl"]), simulate=TRUE, B=10000)

install.packages("sm")
library("sm")




sm.density.compare(INDICES$MSL, INDICES$TreatLarve, model="equal", nboot= 500, ngrid= 100)

sm.density.compare(INDICES$poids, INDICES$TreatLarve, model="equal", nboot= 500, ngrid= 100)

sm.density.compare(INDICES$moyLong, INDICES$TreatLarve, model="equal", nboot= 500, ngrid= 100)

install.packages("gsg")
library(gsg)


M1<-lm(MSL~TreatLarve*TreatAdult,data=INDICES,na.action=na.fail)


M1<-gls(MSL~TreatLarve*TreatAdult,data=INDICES,na.action=na.fail)
library(nlme)
M2<-gls(MSL~TreatLarve*TreatAdult,data=INDICES, weights = varIdent(form = ~1 | TreatLarve),na.action=na.fail)
summary(M2)
AICc(M1,M2)



M2b<-gls(MSL~TreatLarve*TreatAdult,data=INDICES, weights = varIdent(form = ~1 | TreatLarve),method="ML",na.action=na.fail)
summary(M2)



anova(M1,M2)
anova(M1,M2b)



dredge(M1)
dredge(M2b)





M1<-lm(MSL~TreatLarve,data=INDICES)
library(nlme)
M2<-gls(MSL~TreatLarve,data=INDICES, weights = varIdent(form = ~1 | TreatLarve))


fligner.test(MSL~TreatLarve,data=INDICES)
fligner.test(moyLong~TreatLarve,data=INDICES)
fligner.test(poids~TreatLarve,data=INDICES)






n<-250
z<-cbind(rnorm(n,0,1),rnorm(n,0,1))
W<-rpois(n,exp(2-0.6*z[,1]^2))
d<-as.data.frame(cbind(W,z))
names(d)<-c("W","z1","z2")
fit.func<-gppr(y="W",xterms=c("z1","z2"),data=d,family="poisson",nterms=2,max.terms=2)


gppr.gradients(mod= fit.func,phenotype=c("z1","z2"),se.method='n',standardize=FALSE)


fit.func<-gppr(y="MSL",xterms="moyLong",data=INDICES,family="gaussian",nterms=2,max.terms=2)

gppr.gradients(mod= fit.func,phenotype="moyLong",se.method='n',standardize=FALSE)


#detach(INDICES)
#############################################
setwd("~/Dropbox/condition/ANALYSE")

dosages= read.csv(file = "DOSAGES.csv", header = TRUE)
#attach(dosages)
names(dosages)

 [1] "individu"     "traitement"   "TreatLarve"   "TreatAdult"   "lipides"     "lipid"
 [6] "glucose"      "glycogene"    "sumLGly"      "sumenergie"   "SsumLGly"    
[11] "Ssumenergie"  "poids"        "volume"       "SLipides"     "SGlucose"    
[16] "SGlycogene"   "MSL"          "VSL"          "OLSresidue"   "longueur.1"  
[21] "X.paisseur.1" "largeur.1"    "longueur.2"   "X.paisseur.2" "largeur.2"   
[26] "moyLong"      "moyepaiss"    "moylargeur"  

summary(dosages$traitement)

length(dosages$lipides)

relLipides<-dosages$lipides/dosages$moyLong

tapply()

dosages$sumsugars<-dosages$glucose+dosages$glycogene 

dosages$sumLGly==dosages$sumsugars

plot(relLipides~dosages$moyLong,ylim=c(0,0.5),xlim=c(8,11.5))
plot(dosages$OLSresidue~dosages$moyLong)
plot(dosages$MSL~dosages$moyLong)

M1<-lm(OLSresidue~moyLong,data=dosages)
M2<-lm(MSL~moyLong,data=dosages)
summary(M1)
summary(M2)


plot(dosages$SLipides~dosages$moyLong)
plot(dosages$SGlucose~dosages$moyLong)
plot(dosages$SGlycogene~dosages$moyLong)

#install.packages("smatr")
library(smatr)

M1 <- sma(poids~moyLong,log="xy",data=dosages)
M2 <- sma(lipides~moyLong,log="xy",data=dosages)
M3 <- sma(glucose~moyLong,log="xy",data=dosages)
M4 <- sma(glycogene~moyLong,log="xy",data=dosages)

par(mfrow=c(2,2))
plot(M1)
plot(M2)
plot(M3)
plot(M4)


print(M1)
            elevation    slope
estimate    -1.033579 3.135961
lower limit -1.418035 2.769597
upper limit -0.649123 3.550789

print(M2)
             elevation     slope
estimate     -8.806555  8.793586
lower limit -10.265842  7.435173
upper limit  -7.347269 10.400183

print(M3)
            elevation    slope
estimate    -13.66676 13.54824
lower limit -15.86033 11.50181
upper limit -11.47319 15.95878

print(M4)
             elevation     slope
estimate     -9.641431  9.693634
lower limit -11.135838  8.293588
upper limit  -8.147024 11.330023




Lipides<-log10(dosages$lipides)
Lipides


M1<-lm(Lipides~TreatLarve*TreatAdult,na.action=na.fail,data=dosages)
anova(M1)
step(M1,test="F")
p4<-boxplot(Lipides~dosages$traitement)
means <- tapply(Lipides,dosages$traitement,mean)
points(Lipides,col="red",pch=18)
par(mfrow=c(2,2))
plot(M1)
dev.off()


library(MuMIn)
ms1 <- dredge(M1,rank="AICc", extra="R^2")
ms1
model.avg(ms1)
confset.95p <- get.models(ms1, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

write.table(ms1, "~/Dropbox/condition/ANALYSE/lipides.txt", sep="\t")



M1<-glm(Lipides~TreatLarve+TreatAdult,na.action=na.fail,data=dosages)

summary(M1)


#Call:
#glm(formula = Lipides ~ TreatLarve + TreatAdult, data = dosages, 
#    na.action = na.fail)#

#Deviance Residuals: 
#     Min        1Q    Median        3Q       Max  
#-1.35278  -0.31994  -0.05423   0.31577   1.27453  #

#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  -0.67974    0.07749  -8.772 1.43e-13 ***
#TreatLarvePN  0.32913    0.11289   2.915  0.00453 ** 
#TreatAdultPN  0.49434    0.11110   4.449 2.57e-05 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#

#(Dispersion parameter for gaussian family taken to be 0.2567011)#

#    Null deviance: 30.753  on 88  degrees of freedom
#Residual deviance: 22.076  on 86  degrees of freedom
#AIC: 136.49#

#Number of Fisher Scoring iterations: 2



#TreatLarve TreatAdult   
#N :56      N :53  
#PN:33      PN:36


library(MBESS) 
ci.smd(ncp=2.915 , n.1=56, n.2=33, conf.level=1-.05)
#$Lower.Conf.Limit.smd
#[1] 0.1974849#

#$smd
#[1] 0.6397092#

#$Upper.Conf.Limit.smd
#[1] 1.078425



ci.smd(ncp=4.449 , n.1=53, n.2=36, conf.level=1-.05)
#$Lower.Conf.Limit.smd
#[1] 0.5117209#

#$smd
#[1] 0.9608779#

#$Upper.Conf.Limit.smd
#[1] 1.405057



lSLipides<-log10(dosages$SLipides)

M1<-lm(lSLipides~TreatLarve*TreatAdult,na.action=na.fail,data=dosages)
anova(M1)
step(M1,test="F")
p4<-boxplot(lSLipides~dosages$traitement)
means <- tapply(lSLipides,dosages$traitement,mean)
points(lSLipides,col="red",pch=18)
par(mfrow=c(2,2))
plot(M1)
dev.off()


library(MuMIn)
ms1 <- dredge(M1,rank="AICc", extra="R^2")
ms1
model.avg(ms1)
confset.95p <- get.models(ms1, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

write.table(ms1, "~/Dropbox/condition/ANALYSE/lSlipides.txt", sep="\t")




lglucose<-log10(dosages$glucose)

M1<-lm(lglucose~TreatLarve*TreatAdult,na.action=na.fail,data=dosages)
anova(M1)
step(M1,test="F")
p5<-boxplot(lglucose~dosages$traitement)
means <- tapply(lglucose,dosages$traitement,mean)
points(lglucose,col="red",pch=18)
par(mfrow=c(2,2))
plot(M1)
dev.off()

ms1 <- dredge(M1,rank="AICc", extra="R^2")
ms1
model.avg(ms1)
confset.95p <- get.models(ms1, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

write.table(ms1, "~/Dropbox/condition/ANALYSE/lglucose.txt", sep="\t")




M1<-glm(lglucose~TreatLarve+TreatAdult,na.action=na.fail,data=dosages)

summary(M1)


#Call:
#glm(formula = lglucose ~ TreatLarve + TreatAdult, data = dosages, 
#    na.action = na.fail)#

#Deviance Residuals: 
#     Min        1Q    Median        3Q       Max  
#-0.93608  -0.11947   0.02019   0.15789   0.50113  #

#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  -0.63256    0.03875 -16.325  < 2e-16 ***
#TreatLarvePN  0.54906    0.05645   9.727 1.63e-15 ***
#TreatAdultPN  0.22514    0.05555   4.053 0.000111 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#

#(Dispersion parameter for gaussian family taken to be 0.06418003)#

#    Null deviance: 13.7685  on 88  degrees of freedom
#Residual deviance:  5.5195  on 86  degrees of freedom
#AIC: 13.12#

#Number of Fisher Scoring iterations: 2




#TreatLarve TreatAdult   
#N :56      N :53  
#PN:33      PN:36


library(MBESS) 
ci.smd(ncp=9.727 , n.1=56, n.2=33, conf.level=1-.05)
#$Lower.Conf.Limit.smd
#[1] 1.596425#

#$smd
#[1] 2.134632#

#$Upper.Conf.Limit.smd
#[1] 2.664699



ci.smd(ncp=4.053 , n.1=53, n.2=36, conf.level=1-.05)
#$Lower.Conf.Limit.smd
#[1] 0.4302647

#$smd
#[1] 0.8753514

#$Upper.Conf.Limit.smd
#[1] 1.315827





lSGlucose<-log10(dosages$SGlucose)

M1<-lm(lSGlucose~TreatLarve*TreatAdult,na.action=na.fail,data=dosages)
anova(M1)
step(M1,test="F")
p5<-boxplot(lSGlucose~dosages$traitement)
means <- tapply(lSGlucose,dosages$traitement,mean)
points(lSGlucose,col="red",pch=18)
par(mfrow=c(2,2))
plot(M1)
dev.off()

ms1 <- dredge(M1,rank="AICc", extra="R^2")
ms1
model.avg(ms1)
confset.95p <- get.models(ms1, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

write.table(ms1, "~/Dropbox/condition/ANALYSE/lSGlucose.txt", sep="\t")

M1<-lm(lSGlucose~TreatLarve,na.action=na.fail,data=dosages)
summary(M1)

#Call:
#lm(formula = lSGlucose ~ TreatLarve, data = dosages, na.action = na.fail)#

#Residuals:
#     Min       1Q   Median       3Q      Max 
#-1.38709 -0.15366  0.00232  0.20604  0.60996 #

#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  -0.40137    0.04332  -9.265 1.29e-14 ***
#TreatLarvePN  0.20654    0.07115   2.903  0.00468 ** 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#

#Residual standard error: 0.3242 on 87 degrees of freedom
#Multiple R-squared:  0.08831,   Adjusted R-squared:  0.07783 
#F-statistic: 8.427 on 1 and 87 DF,  p-value: 0.004683



#TreatLarve TreatAdult   
#N :56      N :53  
#PN:33      PN:36

library(MBESS) 
ci.smd(ncp=2.903, n.1=56, n.2=33, conf.level=1-.05)
#$Lower.Conf.Limit.smd
#[1] 0.194942#

#$smd
#[1] 0.6370757#

#$Upper.Conf.Limit.smd
#[1] 1.075714



lglycogen<-log10(dosages$glycogen)

M1<-glm(lglycogen~TreatLarve*TreatAdult,na.action=na.fail,data=dosages)
anova(M1)
step(M1,test="F")
p6<-boxplot(dosages$glycogen~dosages$traitement)
means <- tapply(lglycogen,dosages$traitement,mean)
points(lglycogen,col="red",pch=18)
par(mfrow=c(2,2))
plot(M1)
dev.off()
hist(lglycogen,breaks=30)



ms1 <- dredge(M1,rank="AICc", extra="R^2")
ms1
model.avg(ms1)
confset.95p <- get.models(ms1, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

write.table(ms1, "~/Dropbox/condition/ANALYSE/lglycogen.txt", sep="\t")



M1<-glm(lglycogen~traitement,na.action=na.fail,data=dosages)

summary(M1)


#Call:
#glm(formula = lglycogen ~ traitement, data = dosages, na.action = na.fail)#

#Deviance Residuals: 
#     Min        1Q    Median        3Q       Max  
#-0.63010  -0.10246  -0.00322   0.10458   0.45472  #

#Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      -0.36130    0.02831 -12.763  < 2e-16 ***
#traitementNlPNa   0.32929    0.04860   6.776 1.53e-09 ***
#traitementPNlNa   0.50223    0.05152   9.748 1.66e-15 ***
#traitementPNlPNa  0.50230    0.05045   9.956 6.31e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#

#(Dispersion parameter for gaussian family taken to be 0.02965003)#

#    Null deviance: 7.0484  on 88  degrees of freedom
#Residual deviance: 2.5203  on 85  degrees of freedom
#AIC: -54.65#

#Number of Fisher Scoring iterations: 2


tapply(dosages$glycogen, dosages$traitement, length)
#  NlNa  NlPNa  PNlNa PNlPNa 
#    37     19     16     17 



library(MBESS) 
ci.smd(ncp=6.776 , n.1=37, n.2=19, conf.level=1-.05)
#$Lower.Conf.Limit.smd
#[1] 1.246229

#$smd
#[1] 1.912448

#$Upper.Conf.Limit.smd
#[1] 2.566039


ci.smd(ncp=9.748 , n.1=37, n.2=16, conf.level=1-.05)
#$Lower.Conf.Limit.smd
#[1] 2.095021

#$smd
#[1] 2.916706

#$Upper.Conf.Limit.smd
#[1] 3.72304

ci.smd(ncp=9.956 , n.1=37, n.2=17, conf.level=1-.05)
#$Lower.Conf.Limit.smd
#[1] 2.108131

#$smd
#[1] 2.917134

#$Upper.Conf.Limit.smd
#[1] 3.71123


ftrait<-factor(dosages$traitement, levels=c("NlPNa","PNlNa","PNlPNa","NlNa"))

M1<-glm(lglycogen~ftrait,na.action=na.fail,data=dosages)

summary(M1)
#Call:
#glm(formula = lglycogen ~ ftrait, data = dosages, na.action = na.fail)#

#Deviance Residuals: 
#     Min        1Q    Median        3Q       Max  
#-0.63010  -0.10246  -0.00322   0.10458   0.45472  #

#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  -0.03201    0.03950  -0.810  0.42002    
#ftraitPNlNa   0.17294    0.05843   2.960  0.00399 ** 
#ftraitPNlPNa  0.17301    0.05749   3.010  0.00344 ** 
#ftraitNlNa   -0.32929    0.04860  -6.776 1.53e-09 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#

#(Dispersion parameter for gaussian family taken to be 0.02965003)#

#    Null deviance: 7.0484  on 88  degrees of freedom
#Residual deviance: 2.5203  on 85  degrees of freedom
#AIC: -54.65#

#Number of Fisher Scoring iterations: 2


tapply(dosages$glycogen, dosages$traitement, length)
#  NlNa  NlPNa  PNlNa PNlPNa 
#    37     19     16     17 



library(MBESS) 
ci.smd(ncp=2.960 , n.1=19, n.2=16, conf.level=1-.05)
#$Lower.Conf.Limit.smd
#[1] 0.2901485#

#$smd
#[1] 1.004359#

#$Upper.Conf.Limit.smd
#[1] 1.705115

ci.smd(ncp=3.010 , n.1=19, n.2=17, conf.level=1-.05)
#$Lower.Conf.Limit.smd
#[1] 0.302101#

#$smd
#[1] 1.004885#

#$Upper.Conf.Limit.smd
#[1] 1.694608




lSGlycogene<-log10(dosages$SGlycogene)

M1<-glm(lSGlycogene~TreatLarve*TreatAdult,na.action=na.fail,data=dosages)
anova(M1)
step(M1,test="F")
p6<-boxplot(dosages$SGlycogene~dosages$traitement)
means <- tapply(lSGlycogene,dosages$traitement,mean)
points(lSGlycogene,col="red",pch=18)
par(mfrow=c(2,2))
plot(M1)
hist(lSGlycogene,breaks=30)
dev.off()



ms1 <- dredge(M1,rank="AICc", extra="R^2")
ms1
model.avg(ms1)
confset.95p <- get.models(ms1, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

write.table(ms1, "~/Dropbox/condition/ANALYSE/lSGlycogene.txt", sep="\t")





M1<-glm(lSGlycogene~TreatLarve,na.action=na.fail,data=dosages)
summary(M1)

#Call:
#glm(formula = lSGlycogene ~ TreatLarve, data = dosages, na.action = na.fail)#

#Deviance Residuals: 
#     Min        1Q    Median        3Q       Max  
#-0.56088  -0.13974   0.00023   0.12686   0.47225  #

#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  -0.13882    0.02955  -4.698 9.73e-06 ***
#TreatLarvePN  0.11715    0.04852   2.414   0.0179 *  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#

#(Dispersion parameter for gaussian family taken to be 0.04888509)#

#    Null deviance: 4.538  on 88  degrees of freedom
#Residual deviance: 4.253  on 87  degrees of freedom
#AIC: -12.079#

#Number of Fisher Scoring iterations: 2



#TreatLarve TreatAdult   
#N :56      N :53  
#PN:33      PN:36

library(MBESS) 
ci.smd(ncp=2.414, n.1=56, n.2=33, conf.level=1-.05)

#$Lower.Conf.Limit.smd
#[1] 0.09104414#

#$smd
#[1] 0.5297626#

#$Upper.Conf.Limit.smd
#[1] 0.9655346



lsumLGly<-log10(dosages$sumLGly)

M1<-glm(lsumLGly~TreatLarve*TreatAdult,na.action=na.fail,data=dosages)
anova(M1)
step(M1,test="F")
p6<-boxplot(lsumLGly~dosages$traitement)
means <- tapply(lsumLGly,dosages$traitement,mean)
points(lsumLGly,col="red",pch=18)
par(mfrow=c(2,2))
plot(M1)
hist(log(lsumLGly),breaks=10)
dev.off()


ms1 <- dredge(M1,rank="AICc", extra="R^2")
ms1
model.avg(ms1)
confset.95p <- get.models(ms1, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

write.table(ms1, "~/Dropbox/condition/ANALYSE/sumLGly.txt", sep="\t")




M1<-glm(lsumLGly~traitement,na.action=na.fail,data=dosages)

summary(M1)

Call:
glm(formula = lsumLGly ~ traitement, data = dosages, na.action = na.fail)

#Deviance Residuals: 
#     Min        1Q    Median        3Q       Max  
#-0.54789  -0.10591   0.01289   0.10431   0.36877  #

#Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      -0.16655    0.02763  -6.029 4.13e-08 ***
#traitementNlPNa   0.30968    0.04743   6.529 4.59e-09 ***
#traitementPNlNa   0.54687    0.05028  10.876  < 2e-16 ***
#traitementPNlPNa  0.57983    0.04924  11.776  < 2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#

#(Dispersion parameter for gaussian family taken to be 0.02823961)#

#    Null deviance: 7.9836  on 88  degrees of freedom
#Residual deviance: 2.4004  on 85  degrees of freedom
#AIC: -58.987#

#Number of Fisher Scoring iterations: 2


tapply(dosages$glycogen, dosages$traitement, length)
#  NlNa  NlPNa  PNlNa PNlPNa 
#    37     19     16     17 



library(MBESS) 
ci.smd(ncp=6.529 , n.1=37, n.2=19, conf.level=1-.05)
#$Lower.Conf.Limit.smd
#[1] 1.18368

#$smd
#[1] 1.842736

#$Upper.Conf.Limit.smd
#[1] 2.489373

ci.smd(ncp=10.876 , n.1=37, n.2=16, conf.level=1-.05)
#$Lower.Conf.Limit.smd
#[1] 2.385905

#$smd
#[1] 3.254215

#$Upper.Conf.Limit.smd
#[1] 4.107071

ci.smd(ncp=11.776 , n.1=37, n.2=17, conf.level=1-.05)
#$Lower.Conf.Limit.smd
#[1] 2.567013

#$smd
#[1] 3.450399

#$Upper.Conf.Limit.smd
#[1] 4.318807





ftrait<-factor(dosages$traitement, levels=c("NlPNa","PNlNa","PNlPNa","NlNa"))

M1<-glm(lsumLGly~ftrait,na.action=na.fail,data=dosages)

summary(M1)
#Call:
#glm(formula = lsumLGly ~ ftrait, data = dosages, na.action = na.fail)#

#Deviance Residuals: 
#     Min        1Q    Median        3Q       Max  
#-0.54789  -0.10591   0.01289   0.10431   0.36877  #

#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   0.14313    0.03855   3.713 0.000366 ***
#ftraitPNlNa   0.23719    0.05702   4.160 7.59e-05 ***
#ftraitPNlPNa  0.27015    0.05610   4.815 6.33e-06 ***
#ftraitNlNa   -0.30968    0.04743  -6.529 4.59e-09 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#

#(Dispersion parameter for gaussian family taken to be 0.02823961)#

#    Null deviance: 7.9836  on 88  degrees of freedom
#Residual deviance: 2.4004  on 85  degrees of freedom
#AIC: -58.987#

#Number of Fisher Scoring iterations: 2#




tapply(dosages$glycogen, dosages$traitement, length)
#  NlNa  NlPNa  PNlNa PNlPNa 
#    37     19     16     17 



library(MBESS) 
ci.smd(ncp=4.160 , n.1=19, n.2=16, conf.level=1-.05)
#$Lower.Conf.Limit.smd
#[1] 0.6564538

#$smd
#[1] 1.411531

#$Upper.Conf.Limit.smd
#[1] 2.149555


ci.smd(ncp=4.815 , n.1=19, n.2=17, conf.level=1-.05)
#$Lower.Conf.Limit.smd
#[1] 0.8416036

#$smd
#[1] 1.607483

#$Upper.Conf.Limit.smd
#[1] 2.355562



lSsumLGly<-log10(dosages$SsumLGly)

M1<-glm(lSsumLGly~TreatLarve*TreatAdult,na.action=na.fail,data=dosages)
anova(M1)
step(M1,test="F")
p6<-boxplot(lSsumLGly~dosages$traitement)
means <- tapply(lSsumLGly,dosages$traitement,mean)
points(lSsumLGly,col="red",pch=18)
par(mfrow=c(2,2))
plot(M1)
hist(log(lSsumLGly),breaks=10)
dev.off()


ms1 <- dredge(M1,rank="AICc", extra="R^2")
ms1
model.avg(ms1)
confset.95p <- get.models(ms1, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

write.table(ms1, "~/Dropbox/condition/ANALYSE/SsumLGly.txt", sep="\t")



M1<-glm(lSsumLGly~TreatLarve,na.action=na.fail,data=dosages)

summary(M1)

#Call:
#glm(formula = lSsumLGly ~ TreatLarve, data = dosages, na.action = na.fail)#

#Deviance Residuals: 
#     Min        1Q    Median        3Q       Max  
#-0.70690  -0.11132  -0.02281   0.12759   0.45564  #

#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   0.05640    0.02920   1.931 0.056721 .  
#TreatLarvePN  0.16781    0.04796   3.499 0.000738 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#

#(Dispersion parameter for gaussian family taken to be 0.04775918)#

#    Null deviance: 4.7398  on 88  degrees of freedom
#Residual deviance: 4.1550  on 87  degrees of freedom
#AIC: -14.153#

#Number of Fisher Scoring iterations: 2


library(MBESS) 
ci.smd(ncp=3.499 , n.1=56, n.2=33, conf.level=1-.05)

#$Lower.Conf.Limit.smd
#[1] 0.3208499#

#$smd
#[1] 0.7678705#

#$Upper.Conf.Limit.smd
#[1] 1.21076




pdf("/home/mark/Dropbox/condition/ANALYSE/FigureBoxplotComponentMilieu.pdf",paper="a4",width=8.267, height=11.692)
par(mfrow=c(2,2))
boxplot(dosages$SLipides~dosages$TreatLarve, names=c("R.L","P.L"), ylab="Scaled lipides",main="A.")
boxplot(lSGlucose~dosages$TreatLarve, names=c("R.L","P.L"), ylab="log (scaled glucose)",main="B.")
boxplot(lSGlycogene~dosages$TreatLarve, names=c("R.L","P.L"), ylab="log (scaled glycogen)", xlab="Food treatment",main="C.")
boxplot(lSsumLGly~dosages$TreatLarve, names=c("R.L","P.L"), ylab="log (sum of scaled glucose and glycogen)", xlab="Food treatment",,main="D.")
dev.off()


jpeg("/home/mark/Dropbox/condition/ANALYSE/FigureBoxplotComponentMilieu.jpeg",width=2100, height=2100,res=300)
par(mfrow=c(2,2))

boxplot(lSLipides~dosages$TreatLarve, names=c("R.L","P.L"),ylab="Scaled lipides",main="A.",xlab="Food treatment",outline=FALSE,ylim=c(min(lSLipides),max(lSLipides)))
stripchart(lSLipides~dosages$TreatLarve, vertical = TRUE,add = TRUE,names="",pch = 16,method="jitter")


boxplot(lSGlucose~dosages$TreatLarve, names=c("R.L","P.L"),ylab="log (scaled glucose)",main="B.",xlab="Food treatment",outline=FALSE,ylim=c(min(lSGlucose),max(lSGlucose)))
stripchart(lSGlucose~dosages$TreatLarve, vertical = TRUE,add = TRUE,names="",pch = 16,method="jitter")


boxplot(lSGlycogene~dosages$TreatLarve, names=c("R.L","P.L"),ylab="log (scaled glycogen)",main="C.",xlab="Food treatment",outline=FALSE,ylim=c(min(lSGlycogene),max(lSGlycogene)))
stripchart(lSGlycogene~dosages$TreatLarve, vertical = TRUE,add = TRUE,names="",pch = 16,method="jitter")


boxplot(lSsumLGly~dosages$TreatLarve, names=c("R.L","P.L"),ylab="log (sum of scaled glucose and glycogen)",main="D.",xlab="Food treatment",outline=FALSE,ylim=c(min(lSsumLGly),max(lSsumLGly)))
stripchart(lSsumLGly~dosages$TreatLarve, vertical = TRUE,add = TRUE,names="",pch = 16,method="jitter")

dev.off()





png("/home/mark/Dropbox/condition/ANALYSE/FigureBoxplotTousComponentMilieu.png",width=2100, height=2970,res=300)
par(mfrow=c(4,2))

boxplot(Lipides~dosages$traitement, names=c("R.L/R.A","R.L/P.A","P.L/R.A","P.L/P.A"),ylab="log (lipides)",main="A.",xlab="Food treatment",outline=FALSE,ylim=c(min(Lipides),max(Lipides)))
stripchart(Lipides~dosages$traitement, vertical = TRUE,add = TRUE,names="",pch = 16,method="jitter")


boxplot(lSLipides~dosages$traitement, names=c("R.L/R.A","R.L/P.A","P.L/R.A","P.L/P.A"),ylab="log (scaled lipides)",main="E.",xlab="Food treatment",outline=FALSE,ylim=c(min(lSLipides),max(lSLipides)))
stripchart(lSLipides~dosages$traitement, vertical = TRUE,add = TRUE,names="",pch = 16,method="jitter")


boxplot(lglucose~dosages$traitement, names=c("R.L/R.A","R.L/P.A","P.L/R.A","P.L/P.A"),ylab="log (glucose)",main="B.",xlab="Food treatment",outline=FALSE,ylim=c(min(lglucose),max(lglucose)))
stripchart(lglucose~dosages$traitement, vertical = TRUE,add = TRUE,names="",pch = 16,method="jitter")


boxplot(lSGlucose~dosages$traitement, names=c("R.L/R.A","R.L/P.A","P.L/R.A","P.L/P.A"),ylab="log (scaled glucose)",main="F.",xlab="Food treatment",outline=FALSE,ylim=c(min(lSGlucose),max(lSGlucose)))
stripchart(lSGlucose~dosages$traitement, vertical = TRUE,add = TRUE,names="",pch = 16,method="jitter")


boxplot(lglycogen~dosages$traitement, names=c("R.L/R.A","R.L/P.A","P.L/R.A","P.L/P.A"),ylab="log (glycogen)",main="C.",xlab="Food treatment",outline=FALSE,ylim=c(min(lglycogen),max(lglycogen)))
stripchart(lglycogen~dosages$traitement, vertical = TRUE,add = TRUE,names="",pch = 16,method="jitter")


boxplot(lSGlycogene~dosages$traitement, names=c("R.L/R.A","R.L/P.A","P.L/R.A","P.L/P.A"),ylab="log (scaled glycogen)",main="G.",xlab="Food treatment",outline=FALSE,ylim=c(min(lSGlycogene),max(lSGlycogene)))
stripchart(lSGlycogene~dosages$traitement, vertical = TRUE,add = TRUE,names="",pch = 16,method="jitter")


boxplot(lsumLGly~dosages$traitement, names=c("R.L/R.A","R.L/P.A","P.L/R.A","P.L/P.A"),ylab="log (sum of glucose and glycogen)",main="D.",xlab="Food treatment",outline=FALSE,ylim=c(min(lsumLGly),max(lsumLGly)))
stripchart(lsumLGly~dosages$traitement, vertical = TRUE,add = TRUE,names="",pch = 16,method="jitter")


boxplot(lSsumLGly~dosages$traitement, names=c("R.L/R.A","R.L/P.A","P.L/R.A","P.L/P.A"),ylab="log (sum of scaled glucose and glycogen)",main="H.",xlab="Food treatment",outline=FALSE,ylim=c(min(lSsumLGly),max(lSsumLGly)))
stripchart(lSsumLGly~dosages$traitement, vertical = TRUE,add = TRUE,names="",pch = 16,method="jitter")

dev.off()


library(ggplot2)

JitterPlot = qplot(traitement,Lipides, data=dosages, geom=c("boxplot", "jitter"), main="",xlab="Feeding treatment", ylab="log (lipides)")+theme_bw()
JitterPlot 

names(dosages)




Lip<-ggplot(dosages, aes(y=Lipides,x=traitement))+geom_boxplot(outlier.shape = NA,aes(colour=TreatAdult,shape=TreatLarve),show.legend = FALSE)+geom_jitter(position = position_jitter(0.1),aes(colour=TreatAdult,shape=TreatLarve),size=3)+theme_bw()+scale_colour_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("N","PN"),labels=c("Rich adult feeding treatment","Poor adult feeding treatment"))+scale_shape_manual(values=c(1,19),name="", breaks=c("N","PN"),labels=c("Rich larval feeding treatment","Poor larval feeding treatment"))+ labs(x = "Feeding treatment", y = "log(lipids)")+ scale_x_discrete(labels=c("NlNa" = "Rich larval\nand\nrich adult\n(R.L./R.A.)", "NlPNa" = "Rich larval\nand\npoor adult\n(R.L./P.A.)","PNlNa" = "Poor larval\nand\nrich adult\n(P.L./R.A.)","PNlPNa" = "Poor larval\nand\npoor adult\n(P.L./P.A.)"))+ggtitle("a.")+  theme(axis.text.x =element_text(size=10),text = element_text(size=15))+guides(linetype=guide_legend(FALSE))
Lip

sLip<-ggplot(dosages, aes(y=lSLipides,x=traitement))+geom_boxplot(outlier.shape = NA,aes(colour=TreatAdult),show.legend = FALSE)+geom_jitter(position = position_jitter(0.1),aes(colour=TreatAdult,shape=TreatLarve),size=3)+theme_bw()+scale_colour_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("N","PN"),labels=c("Rich adult feeding treatment","Poor adult feeding treatment"))+scale_shape_manual(values=c(1,19),name="", breaks=c("N","PN"),labels=c("Rich larval feeding treatment","Poor larval feeding treatment"))+ labs(x = "Feeding treatment", y = "log (scaled lipides)")+ scale_x_discrete(labels=c("NlNa" = "Rich larval\nand\nrich adult\n(R.L./R.A.)", "NlPNa" = "Rich larval\nand\npoor adult\n(R.L./P.A.)","PNlNa" = "Poor larval\nand\nrich adult\n(P.L./R.A.)","PNlPNa" = "Poor larval\nand\npoor adult\n(P.L./P.A.)"))+ggtitle("c.")+  theme(axis.text.x =element_text(size=10),text = element_text(size=15))
sLip


sugar<-ggplot(dosages, aes(y=lsumLGly,x=traitement))+geom_boxplot(outlier.shape = NA,aes(colour=TreatAdult),show.legend = FALSE)+geom_jitter(position = position_jitter(0.1),aes(colour=TreatAdult,shape=TreatLarve),size=3)+theme_bw()+scale_colour_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("N","PN"),labels=c("Rich adult feeding treatment","Poor adult feeding treatment"))+scale_shape_manual(values=c(1,19),name="", breaks=c("N","PN"),labels=c("Rich larval feeding treatment","Poor larval feeding treatment"))+ labs(x = "Feeding treatment", y = "log(sugars)")+ scale_x_discrete(labels=c("NlNa" = "Rich larval\nand\nrich adult\n(R.L./R.A.)", "NlPNa" = "Rich larval\nand\npoor adult\n(R.L./P.A.)","PNlNa" = "Poor larval\nand\nrich adult\n(P.L./R.A.)","PNlPNa" = "Poor larval\nand\npoor adult\n(P.L./P.A.)"))+ggtitle("b.")+  theme(axis.text.x =element_text(size=10),text = element_text(size=15))
sugar


Ssugar<-ggplot(dosages, aes(y=lSsumLGly,x=traitement))+geom_boxplot(outlier.shape = NA,aes(colour=TreatAdult),show.legend = FALSE)+geom_jitter(position = position_jitter(0.1),aes(colour=TreatAdult,shape=TreatLarve),size=3)+theme_bw()+scale_colour_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("N","PN"),labels=c("Rich adult feeding treatment","Poor adult feeding treatment"))+scale_shape_manual(values=c(1,19),name="", breaks=c("N","PN"),labels=c("Rich larval feeding treatment","Poor larval feeding treatment"))+ labs(x = "Feeding treatment", y = "log(scaled sugars)")+ scale_x_discrete(labels=c("NlNa" = "Rich larval\nand\nrich adult\n(R.L./R.A.)", "NlPNa" = "Rich larval\nand\npoor adult\n(R.L./P.A.)","PNlNa" = "Poor larval\nand\nrich adult\n(P.L./R.A.)","PNlPNa" = "Poor larval\nand\npoor adult\n(P.L./P.A.)"))+ggtitle("d.")+  theme(axis.text.x =element_text(size=10),text = element_text(size=15))
Ssugar


library(gridExtra)



library(gtable)

legend=gtable_filter(ggplotGrob(Lip), "guide-box")
grid.arrange(Lip,sLip,sugar,Ssugar)


png("/home/mark/Dropbox/condition/ANALYSE/FigureBoxplotTousComponentMilieuFinal.png",height=1050, width=1485,res=100)
grid.arrange(arrangeGrob(Lip + theme(legend.position="none"),sugar + theme(legend.position="none"), sLip + theme(legend.position="none"),Ssugar + theme(legend.position="none")))

dev.off()



#grid.arrange(arrangeGrob(Lip + theme(legend.position="none"), sLip + theme(legend.position="none"),sugar + theme(legend.position="none"),Ssugar + theme(legend.position="none")),legend,nrow=1,widths=c(9,2))


library(smatr)


M1 <- sma(lipides~moyLong*traitement,data=dosages)
M2 <- sma(lipides~moyLong*traitement ,log="xy",data=dosages)
print(M1)
print(M2)

plot(M2)


M1 <- sma(lipides~moyLong,data=dosages)
M2 <- sma(lipides~moyLong ,log="xy",data=dosages)
print(M1)
print(M2)

#Call: sma(formula = lipides ~ moyLong, log = "xy") #

#Fit using Standardized Major Axis #

#These variables were log-transformed before fitting: xy #

#Confidence intervals (CI) are at 95%#

#------------------------------------------------------------
#Coefficients:
#             elevation     slope
#estimate     -8.806555  8.793586
#lower limit -10.265842  7.435173
#upper limit  -7.347269 10.400183#

#H0 : variables uncorrelated
#R-squared : 0.3740812 
#P-value : 1.9282e-10 

SLipides<- dosages$lipides*((mean(dosages$moyLong)/dosages$moyLong)^ 8.793586)

write.table(SLipides,"SLipides.txt",sep="\t",row.names=F,quote=F) 


M1 <- sma(dosages$glucose~dosages$moyLong*dosages$traitement)
M2 <- sma(dosages$glucose~dosages$moyLong*dosages$traitement ,log="xy")
print(M1)
print(M2)
plot(M1)
plot(M2)



M1 <- sma(dosages$glucose~dosages$moyLong)
M2 <- sma(dosages$glucose~dosages$moyLong ,log="xy")
print(M1)
print(M2)

#Call: sma(formula = glucose ~ moyLong, log = "xy") #

#Fit using Standardized Major Axis #

#These variables were log-transformed before fitting: xy #

#Confidence intervals (CI) are at 95%#

#------------------------------------------------------------
#Coefficients:
#            elevation    slope
#estimate    -13.66676 13.54824
#lower limit -15.86033 11.50181
#upper limit -11.47319 15.95878#

#H0 : variables uncorrelated
#R-squared : 0.4041838 
#P-value : 2.1782e-11 #



SGlucose<- dosages$glucose*((mean(dosages$moyLong)/dosages$moyLong)^13.54824)

write.table(SGlucose,"SGlucose.txt",sep="\t",row.names=F,quote=F) 


M1 <- sma(glycogene~moyLong*traitement,data=dosages)
M2 <- sma(glycogene~moyLong*traitement ,log="xy",data=dosages)
print(M1)
print(M2)

plot(M1)
plot(M2)

M1 <- sma(glycogene~moyLong,data=dosages)
M2 <- sma(glycogene~moyLong ,log="xy",data=dosages)
print(M1)
print(M2)

#Call: sma(formula = glycogene ~ moyLong, log = "xy") #

#Fit using Standardized Major Axis #

#These variables were log-transformed before fitting: xy #

#Confidence intervals (CI) are at 95%#

#------------------------------------------------------------
#Coefficients:
#             elevation     slope
#estimate     -9.641431  9.693634
#lower limit -11.135838  8.293588
#upper limit  -8.147024 11.330023#

#H0 : variables uncorrelated
#R-squared : 0.4598029 
#P-value : 2.8844e-13 



SGlycogene<- dosages$glycogene*((mean(dosages$moyLong)/dosages$moyLong)^9.693634)



write.table(SGlycogene,"SGlycogene.txt",sep="\t",row.names=F,quote=F) 


M1 <- sma(sumLGly~moyLong,data=dosages)
M2 <- sma(sumLGly~moyLong ,log="xy",data=dosages)
print(M1)
print(M2)

#Call: sma(formula = sumLGly ~ moyLong, log = "xy") #

#Fit using Standardized Major Axis #

#These variables were log-transformed before fitting: xy #

#Confidence intervals (CI) are at 95%#

#------------------------------------------------------------
#Coefficients:
#             elevation    slope
#estimate    -10.041007 10.31670
#lower limit -11.579638  8.87129
#upper limit  -8.502376 11.99760#

#H0 : variables uncorrelated
#R-squared : 0.4944295 
#P-value : 1.5543e-14 

SsumLGly<- dosages$sumLGly*((mean(dosages$moyLong)/dosages$moyLong)^10.31670)

write.table(SsumLGly,"SsumLGly.txt",sep="\t",row.names=F,quote=F) 


M1 <- sma(sumenergie~moyLong*traitement,data=dosages)
M2 <- sma(sumenergie~moyLong*traitement,log="xy",data=dosages)
print(M1)
print(M2)

plot(M1)
plot(M2)



M1 <- sma(sumenergie~moyLong,data=dosages)
M2 <- sma(sumenergie~moyLong,log="xy",data=dosages)
print(M1)
print(M2)

#Call: sma(formula = sumenergie ~ moyLong, log = "xy") #

#Fit using Standardized Major Axis #

#These variables were log-transformed before fitting: xy #

#Confidence intervals (CI) are at 95%#

#------------------------------------------------------------
#Coefficients:
#            elevation     slope
#estimate    -8.350993  8.805937
#lower limit -9.613159  7.616514
#upper limit -7.088827 10.181106#

#H0 : variables uncorrelated
#R-squared : 0.5330365 
#P-value : 4.4409e-16 

Ssumenergie<- dosages$sumenergie*((mean(dosages$moyLong)/dosages$moyLong)^8.805937)

write.table(Ssumenergie,"Ssumenergie.txt",sep="\t",row.names=F,quote=F) 

M1<-lm(SLipides~traitement,data=dosages)


plot(log(dosages$glucose)~log(dosages$glycogene))

plot(log(dosages$glucose)~log(dosages$glycogene))

plot(dosages$lipides~dosages$sumenergie)

plot(log(dosages$lipides)~log(dosages$sumenergie))




cor.test(log(dosages$glucose),log(dosages$glycogene))

cor.test(log(dosages$lipides),log(dosages$sumenergie))



M1<-sma(dosages$lipides~dosages$sumenergie*traitement,data=dosages)
plot(M1)
print(M1)



M1<-sma(log(dosages$lipides)~log(dosages$sumenergie)*traitement,data=dosages)
plot(M1)
print(M1)



M1<-sma(log(dosages$lipides)~log(dosages$sumenergie),data=dosages)

M1
plot(M1)
abline(lm(log(dosages$lipides)~log(dosages$sumenergie)))


library(mgcv)
M2<-gam(log(dosages$lipides)~s(log(dosages$sumenergie),k=3))
plot(M2)
anova(M2)
summary(M2)


M3<-lm(log(dosages$lipides)~log(dosages$sumenergie))


AICc(M1)
AICc(M2)
AICc(M3)



cor(dosages$poids,dosages$SLipides,method="pearson")
cor(dosages$OLSresidue,dosages$SLipides,method="pearson")
cor(dosages$MSL,dosages$SLipides,method="pearson")
cor(dosages$VSL,dosages$SLipides,method="pearson")

cor(dosages$poids,dosages$SGlucose,method="pearson")
cor(dosages$OLSresidue,dosages$SGlucose,method="pearson")
cor(dosages$MSL,dosages$SGlucose,method="pearson")
cor(dosages$VSL,dosages$SGlucose,method="pearson")

cor(dosages$poids,dosages$SGlycogene,method="pearson")
cor(dosages$OLSresidue,dosages$SGlycogene,method="pearson")
cor(dosages$MSL,dosages$SGlycogene,method="pearson")
cor(dosages$VSL,dosages$SGlycogene,method="pearson")

cor(dosages$poids,dosages$SsumLGly,method="pearson")
cor(dosages$OLSresidue,dosages$SsumLGly,method="pearson")
cor(dosages$MSL,dosages$SsumLGly,method="pearson")
cor(dosages$VSL,dosages$SsumLGly,method="pearson")

cor(dosages$poids,dosages$Ssumenergie,method="pearson")
cor(dosages$OLSresidue,dosages$Ssumenergie,method="pearson")
cor(dosages$MSL,dosages$Ssumenergie,method="pearson")
cor(dosages$VSL,dosages$Ssumenergie,method="pearson")


par(mfrow=c(1,2))

plot(dosages$moyLong,dosages$lipides)
plot(log(dosages$moyLong),log(dosages$lipides))
plot(dosages$poids,dosages$lipides)
plot(log(dosages$poids),log(dosages$lipides))
plot(dosages$volume,dosages$lipides)
plot(log(dosages$volume),log(dosages$lipides))
plot(dosages$OLSresidue,dosages$lipides)
plot(dosages$OLSresidue,log(dosages$lipides))
plot(log(dosages$OLSresidue),log(dosages$lipides))

plot(dosages$MSL,log(dosages$lipides))
plot(log(dosages$MSL),log(dosages$lipides))
plot(dosages$VSL,dosages$lipides)
plot(log(dosages$VSL),log(dosages$lipides))

plot(dosages$moyLong,dosages$glucose)
plot(log(dosages$moyLong),log(dosages$glucose))
plot(dosages$poids,dosages$glucose)
plot(log(dosages$poids),log(dosages$glucose))
plot(dosages$volume,dosages$glucose)
plot(log(dosages$volume),log(dosages$glucose))
plot(dosages$OLSresidue,dosages$glucose)
plot(log(dosages$OLSresidue),log(dosages$glucose))
plot(dosages$MSL,dosages$glucose)
plot(log(dosages$MSL),log(dosages$glucose))
plot(dosages$VSL,dosages$glucose)
plot(log(dosages$VSL),log(dosages$glucose))

plot(dosages$moyLong,dosages$glycogene)
plot(log(dosages$moyLong),log(dosages$glycogene))
plot(dosages$poids,dosages$glycogene)
plot(log(dosages$poids),log(dosages$glycogene))
plot(dosages$volume,dosages$glycogene)
plot(log(dosages$volume),log(dosages$glycogene))
plot(dosages$OLSresidue,dosages$glycogene)
plot(log(dosages$OLSresidue),log(dosages$glycogene))
plot(dosages$MSL,dosages$glycogene)
plot(log(dosages$MSL),log(dosages$glycogene))
plot(dosages$VSL,dosages$glycogene)
plot(log(dosages$VSL),log(dosages$glycogene))

plot(dosages$moyLong,dosages$sumLGly)
plot(log(dosages$moyLong),log(dosages$sumLGly))
plot(dosages$poids,dosages$sumLGly)
plot(log(dosages$poids),log(dosages$sumLGly))
plot(dosages$volume,dosages$sumLGly)
plot(log(dosages$volume),log(dosages$sumLGly))
plot(dosages$OLSresidue,dosages$sumLGly)
plot(log(dosages$OLSresidue),log(dosages$sumLGly))
plot(dosages$MSL,dosages$sumLGly)
plot(log(dosages$MSL),log(dosages$sumLGly))
plot(dosages$VSL,dosages$sumLGly)
plot(log(dosages$VSL),log(dosages$sumLGly))

plot(dosages$moyLong,dosages$sumenergie)
plot(log(dosages$moyLong),log(dosages$sumenergie))
plot(dosages$poids,dosages$sumenergie)
plot(log(dosages$poids),log(dosages$sumenergie))
plot(dosages$volume,dosages$sumenergie)
plot(log(dosages$volume),log(dosages$sumenergie))
plot(dosages$OLSresidue,dosages$sumenergie)
plot(log(dosages$OLSresidue),log(dosages$sumenergie))
plot(dosages$MSL,dosages$sumenergie)
plot(log(dosages$MSL),log(dosages$sumenergie))
plot(dosages$VSL,dosages$sumenergie)
plot(log(dosages$VSL),log(dosages$sumenergie))




plot(dosages$moyLong,dosages$SLipides)
plot(dosages$poids,dosages$SLipides)
plot(dosages$volume,dosages$SLipides)
plot(dosages$OLSresidue,dosages$SLipides)
plot(dosages$MSL,dosages$SLipides)
plot(dosages$VSL,dosages$SLipides)

plot(dosages$moyLong,dosages$SGlucose)
plot(dosages$poids,dosages$SGlucose)
plot(dosages$volume,dosages$SGlucose)
plot(dosages$OLSresidue,dosages$SGlucose)
plot(dosages$MSL,dosages$SGlucose)
plot(dosages$VSL,dosages$SGlucose)

plot(dosages$moyLong,dosages$SGlycogene)
plot(dosages$poids,dosages$SGlycogene)
plot(dosages$volume,dosages$SGlycogene)
plot(dosages$OLSresidue,dosages$SGlycogene)
plot(dosages$MSL,dosages$SGlycogene)
plot(dosages$VSL,dosages$SGlycogene)

plot(dosages$moyLong,dosages$SsumLGly)
plot(dosages$poids,dosages$SsumLGly)
plot(dosages$volume,dosages$SsumLGly)
plot(dosages$OLSresidue,dosages$SsumLGly)
plot(dosages$MSL,dosages$SsumLGly)
plot(dosages$VSL,dosages$SsumLGly)

plot(dosages$moyLong,dosages$Ssumenergie)
plot(dosages$poids,dosages$Ssumenergie)
plot(dosages$volume,dosages$Ssumenergie)
plot(dosages$OLSresidue,dosages$Ssumenergie)
plot(dosages$MSL,dosages$Ssumenergie)
plot(dosages$VSL,dosages$Ssumenergie)




cor.test(log(dosages$poids),log(dosages$VSL),method="pearson")



cor.test(log(dosages$poids),log(dosages$lipides),method="pearson")
cor.test(log(dosages$poids),log(dosages$glucose),method="pearson")
cor.test(log(dosages$poids),log(dosages$glycogene),method="pearson")
cor.test(log(dosages$poids),log(dosages$sumLGly),method="pearson")
cor.test(log(dosages$poids),log(dosages$sumenergie),method="pearson")

cor.test(log(dosages$poids),log(dosages$SLipides),method="pearson")
cor.test(log(dosages$poids),log(dosages$SGlucose),method="pearson")
cor.test(log(dosages$poids),log(dosages$SGlycogene),method="pearson")
cor.test(log(dosages$poids),log(dosages$SsumLGly),method="pearson")
cor.test(log(dosages$poids),log(dosages$Ssumenergie),method="pearson")


cor.test(dosages$glucose,dosages$glycogen,method="pearson")


library(mgcv)

library(purrr)
library(voxel)
names(dosages)

M1<-gam(glucose~s(glycogene,by=traitement)+traitement,data=dosages)

plot(M1)


lglucose<-log(dosages$glucose)
lglycogene<-log(dosages$glycogene)



M1<-gam(lglucose~s(lglycogene),data=dosages)


PlotGAM<-plotGAM(M1, smooth.cov = "lglycogene") + geom_point(data= dosages,aes(x=lglycogene, y = lglucose,color=traitement),size=4)
PlotGAM


M1<-sma(dosages$glucose~dosages$glycogen*traitement,log="xy",data=dosages)
plot(M1)

png("/home/mark/Dropbox/condition/ANALYSE/GlucoseGlycogen.png",width=2100, height=2100,res=300)
plot(log10(dosages$glucose)~log10(dosages$glycogen),ylab="Glucose",xlab="Glycogen")
dev.off()



tapply(dosages$glucose,dosages$traitement,length)

plot(dosages$poids~dosages$SLipides)

M1<-sma(dosages$glucose~dosages$glycogen,log="xy",data=dosages)
M1<-sma(dosages$glucose~dosages$glycogen,data=dosages)


plot(M1)


densite<-dosages$poids/dosages$volume

Z1 <- cbind(lipides=log(dosages$lipides),glucose=log(dosages$glucose),glycogene=log(dosages$glycogene),sumLGly=log(dosages$sumLGly),sumenergie=log(dosages$sumenergie),SsumLGly=log(dosages$SsumLGly),Ssumenergie=log(dosages$Ssumenergie),poids=log(dosages$poids),volume=log(dosages$volume),densite=log(densite),SLipides=log(dosages$SLipides),SGlucose=log(dosages$SGlucose),SGlycogene=log(dosages$SGlycogene),OLSresidue=dosages$OLSresidue,MSL=dosages$MSL,VSL=dosages$VSL,DSL=dosages$DSL)



source(file="/home/mark/Dropbox/MicrobiomeHM/MicrobiomeMetal/HighstatLib.r") 



png("/home/mark/Dropbox/condition/ANALYSE/logCorrelations.png",width=5000, height=3000,res=300)

pairs(Z1, upper.panel = panel.smooth,lower.panel = panel.cor)

dev.off()


corvif(Z1)



Z1 <- cbind(lipides=dosages$lipides,glucose=dosages$glucose,glycogene=dosages$glycogene,sumLGly=dosages$sumLGly,sumenergie=dosages$sumenergie,SsumLGly=dosages$SsumLGly,Ssumenergie=dosages$Ssumenergie,poids=dosages$poids,volume=dosages$volume,densite=densite,SLipides=dosages$SLipides,SGlucose=dosages$SGlucose,SGlycogene=dosages$SGlycogene,OLSresidue=dosages$OLSresidue,MSL=dosages$MSL,VSL=dosages$VSL,DSL=dosages$DSL)



source(file="/home/mark/Dropbox/MicrobiomeHM/MicrobiomeMetal/HighstatLib.r") 



png("/home/mark/Dropbox/condition/ANALYSE/Correlations.png",width=5000, height=3000,res=300)

pairs(Z1, upper.panel = panel.smooth,lower.panel = panel.cor)

dev.off()







cor.test(dosages$SGlucose,dosages$SGlycogene,method="pearson")



cor.test(dosages$poids,dosages$lipides,method="pearson")

#        Pearsons product-moment correlation#

#data:  poids and lipides 
#t = 7.5144, df = 87, p-value = 4.75e-11
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# 0.4820934 0.7390599 
#sample estimates:
#      cor 
#0.6273633 

cor.test(log(dosages$poids),log(dosages$lipides),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(dosages$poids) and log(dosages$lipides)
#t = 8.7625, df = 87, p-value = 1.376e-13
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.5556683 0.7815112
#sample estimates:
#      cor 
#0.6846914 


library("ppcor")

?pcor()
pcor()

pcor.test(log(dosages$poids),log(dosages$lipides),dosages$moyLong)
pcor.test(log(dosages$poids),log(dosages$sumLGly),dosages$moyLong)
pcor.test(log(dosages$poids),log(dosages$SLipides),dosages$moyLong)
pcor.test(log(dosages$poids),log(dosages$SsumLGly),dosages$moyLong)

pcor.test(log(dosages$moyLong),log(dosages$lipides),dosages$poids)
pcor.test(log(dosages$moyLong),log(dosages$sumLGly),dosages$poids)
pcor.test(log(dosages$moyLong),log(dosages$SLipides),dosages$poids)
pcor.test(log(dosages$moyLong),log(dosages$SsumLGly),dosages$poids)


pcor_ci.test <-
function (x, y, z, method = c("pearson", "kendall", "spearman"), conf.level = 0.95, ...) {
    d1 <- deparse(substitute(x))
    d2 <- deparse(substitute(y))
    d3 <- deparse(substitute(z))
    data.name <- paste0(d1, " and ", d2, "; controlling: ", d3)
    method <- match.arg(method)
    Method <- paste0("Partial correlation (", method, ")")
    alternative <- "true partial correlation is not equal to 0"

    x <- as.vector(x)
    y <- as.vector(y)
    z <- as.data.frame(z)
    xyz <- data.frame(x, y, z)
    pcor <- ppcor::pcor(xyz, method = method)
    estimate <- pcor$est[1, 2]
    p.value <- pcor$p.value[1, 2]
    parameter <- c(n = pcor$n, gp = pcor$gp)
    statistic <- c(Stat = pcor$statistic[1, 2])

    fit1 <- lm(x ~ z, data = xyz)
    fit2 <- lm(y ~ z, data = xyz)
    cortest <- cor.test(resid(fit1), resid(fit2), method = method, conf.level = conf.level, ...)
    ci <- cortest$conf.int

    ht <- list(
        statistic = statistic,
        parameter = parameter,
        p.value = p.value,
        estimate = c(partial.cor = estimate),
        alternative = alternative,
        method = Method,
        data.name = data.name,
        conf.int = ci
    )
    class(ht) <- "htest"
    ht
}

pcor_ci.test(log(dosages$poids),log(dosages$lipides),dosages$moyLong)
pcor_ci.test(log(dosages$poids),log(dosages$sumLGly),dosages$moyLong)
pcor_ci.test(log(dosages$poids),log(dosages$SLipides),dosages$moyLong)
pcor_ci.test(log(dosages$poids),log(dosages$SsumLGly),dosages$moyLong)

pcor_ci.test(log(dosages$moyLong),log(dosages$lipides),dosages$poids)
pcor_ci.test(log(dosages$moyLong),log(dosages$sumLGly),dosages$poids)
pcor_ci.test(log(dosages$moyLong),log(dosages$SLipides),dosages$poids)
pcor_ci.test(log(dosages$moyLong),log(dosages$SsumLGly),dosages$poids)




cor.test(dosages$poids,dosages$glucose,method="pearson")

#        Pearsons product-moment correlation#

#data:  poids and glucose 
#t = 10.2182, df = 87, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# 0.6267035 0.8206045 
#sample estimates:
#      cor 
#0.7385661 

cor.test(log(dosages$poids),log(dosages$glucose),method="pearson")

#        Pearson's product-moment correlation

#data:  log(dosages$poids) and log(dosages$glucose)
#t = 11.184, df = 87, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.666266 0.841618
#sample estimates:
#      cor 
#0.7679643 




cor.test(dosages$poids,dosages$glycogene,method="pearson")

#        Pearsons product-moment correlation#

#data:  poids and glycogene 
#t = 8.696, df = 87, p-value = 1.883e-13
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# 0.5520605 0.7794772 
#sample estimates:
#     cor 
#0.681917 


cor.test(log(dosages$poids),log(dosages$glycogene),method="pearson")

#        Pearson's product-moment correlation

#data:  log(dosages$poids) and log(dosages$glycogene)
#t = 7.8998, df = 87, p-value = 7.905e-12
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.5061660 0.7531777
#sample estimates:
#      cor 
#0.6462952 



cor.test(dosages$poids,dosages$sumLGly,method="pearson")

#        Pearsons product-moment correlation#

#data:  poids and sumLGly 
#t = 10.236, df = 87, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# 0.6274880 0.8210264 
#sample estimates:
#      cor 
#0.7391532 

cor.test(log(dosages$poids),log(dosages$sumLGly),method="pearson")

#        Pearson's product-moment correlation

#data:  log(dosages$poids) and log(dosages$sumLGly)
#t = 10.256, df = 87, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.6283384 0.8214834
#sample estimates:
#      cor 
#0.7397895 




cor.test(dosages$poids,dosages$sumenergie,method="pearson")

#        Pearsons product-moment correlation#

#data:  poids and sumenergie 
#t = 11.1468, df = 87, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# 0.6648523 0.8408761 
#sample estimates:
#      cor 
#0.7669212 


cor.test(log(dosages$poids),log(dosages$sumenergie),method="pearson")

#        Pearson's product-moment correlation

#data:  log(dosages$poids) and log(dosages$sumenergie)
#t = 11.786, df = 87, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.6882891 0.8530898
#sample estimates:
#      cor 
#0.7841457 



cor.test(dosages$poids,dosages$SLipides,method="pearson")

#        Pearsons product-moment correlation#

#data:  poids and SLipides 
#t = -1.4434, df = 87, p-value = 0.1525
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# -0.35004139  0.05714346 
#sample estimates:
#       cor 
#-0.1529333 


cor.test(log(dosages$poids),log(dosages$SLipides),method="pearson")

#        Pearson's product-moment correlation

#data:  log(dosages$poids) and log(dosages$SLipides)
#t = -1.355, df = 87, p-value = 0.1789
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.34178356  0.06648786
#sample estimates:
#       cor 
#-0.1437594 



cor.test(dosages$poids,dosages$SGlucose,method="pearson")

#        Pearsons product-moment correlation#

#data:  poids and SGlucose 
#t = -1.3097, df = 87, p-value = 0.1937
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# -0.3375329  0.0712701 
#sample estimates:
#       cor 
#-0.1390505 


cor.test(log(dosages$poids),log(dosages$SGlucose),method="pearson")

#        Pearson's product-moment correlation

#data:  log(dosages$poids) and log(dosages$SGlucose)
#t = -0.4752, df = 87, p-value = 0.6358
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.2564204  0.1590614
#sample estimates:
#        cor 
#-0.05088074 



cor.test(dosages$poids,dosages$SGlycogene,method="pearson")

#        Pearsons product-moment correlation#

#data:  poids and SGlycogene 
#t = -2.5091, df = 87, p-value = 0.01396
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# -0.4440097 -0.0544634 
#sample estimates:
#       cor 
#-0.2597737 

cor.test(log(dosages$poids),log(dosages$SGlycogene),method="pearson")

#        Pearson's product-moment correlation

#data:  log(dosages$poids) and log(dosages$SGlycogene)
#t = -1.9611, df = 87, p-value = 0.05306
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.397002275  0.002611416
#sample estimates:
#       cor 
#-0.2057572 



cor.test(dosages$poids,dosages$SsumLGly,method="pearson")

#        Pearsons product-moment correlation#

#data:  poids and SsumLGly 
#t = -1.453, df = 87, p-value = 0.1498
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# -0.35092471  0.05613967 
#sample estimates:
#       cor 
#-0.1539166 

cor.test(log(dosages$poids),log(dosages$SsumLGly),method="pearson")

#        Pearson's product-moment correlation

#data:  log(dosages$poids) and log(dosages$SsumLGly)
#t = -0.87055, df = 87, p-value = 0.3864
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.2954676  0.1176037
#sample estimates:
#        cor 
#-0.09292907



cor.test(dosages$poids,dosages$Ssumenergie,method="pearson")

#        Pearsons product-moment correlation#

#data:  poids and Ssumenergie 
#t = -0.6972, df = 87, p-value = 0.4875
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# -0.2784785  0.1358203 
#sample estimates:
#        cor 
#-0.07454499 

cor.test(log(dosages$poids),log(dosages$Ssumenergie),method="pearson")

#        Pearson's product-moment correlation

#data:  log(dosages$poids) and log(dosages$Ssumenergie)
#t = -0.34613, df = 87, p-value = 0.7301
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.2434604  0.1725050
#sample estimates:
#        cor 
#-0.03708394


cor.test(dosages$moyLong,dosages$lipides,method="pearson")
cor.test(dosages$moyLong,dosages$glucose,method="pearson")
cor.test(dosages$moyLong,dosages$glycogene,method="pearson")
cor.test(dosages$moyLong,dosages$sumLGly,method="pearson")
cor.test(dosages$moyLong,dosages$sumenergie,method="pearson")


cor.test(dosages$moyLong,dosages$SLipides,method="pearson")
cor.test(dosages$moyLong,dosages$SGlucose,method="pearson")
cor.test(dosages$moyLong,dosages$SGlycogene,method="pearson")
cor.test(dosages$moyLong,dosages$SsumLGly,method="pearson")
cor.test(dosages$moyLong,dosages$Ssumenergie,method="pearson")



cor.test(dosages$moyLong,dosages$lipides,method="pearson")

#        Pearsons product-moment correlation#

#data:  moyLong and lipides 
#t = 6.407, df = 87, p-value = 7.35e-09
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# 0.4057846 0.6927641 
#sample estimates:
#      cor 
#0.5661938 


cor.test(log(dosages$moyLong),log(dosages$lipides),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(dosages$moyLong) and log(dosages$lipides)
#t = 7.2108, df = 87, p-value = 1.928e-10
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.4622429 0.7272460
#sample estimates:
#      cor 
#0.6116218 



cor.test(dosages$moyLong,dosages$glucose,method="pearson")

#        Pearsons product-moment correlation#

#data:  moyLong and glucose 
#t = 8.2307, df = 87, p-value = 1.679e-12
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# 0.5258557 0.7645583 
#sample estimates:
#      cor 
#0.6616529 

cor.test(log(dosages$moyLong),log(dosages$glucose),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(dosages$moyLong) and log(dosages$glucose)
#t = 7.6823, df = 87, p-value = 2.178e-11
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.4927362 0.7453295
#sample estimates:
#      cor 
#0.6357545 




cor.test(log(dosages$moyLong),log(dosages$glucose),method="pearson")

#        Pearson's product-moment correlation

#data:  log(dosages$moyLong) and log(dosages$glucose)
#t = 7.6823, df = 87, p-value = 2.178e-11
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.4927362 0.7453295
#sample estimates:
#      cor 
#0.6357545



cor.test(dosages$moyLong,dosages$glycogene,method="pearson")

#        Pearsons product-moment correlation#

#data:  moyLong and glycogene 
#t = 8.6801, df = 87, p-value = 2.029e-13
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# 0.5511938 0.7789879 
#sample estimates:
#      cor 
#0.6812499 


cor.test(log(dosages$moyLong),log(dosages$glycogene),method="pearson")


#        Pearson's product-moment correlation#

#data:  log(dosages$moyLong) and log(dosages$glycogene)
#t = 8.6054, df = 87, p-value = 2.885e-13
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.5470888 0.7766664
#sample estimates:
#      cor 
#0.6780877 



cor.test(log(dosages$moyLong),log(dosages$glycogene),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(dosages$moyLong) and log(dosages$glycogene)
#t = 8.6054, df = 87, p-value = 2.885e-13
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.5470888 0.7766664
#sample estimates:
#      cor 
#0.6780877 



cor.test(dosages$moyLong,dosages$sumLGly,method="pearson")

#        Pearsons product-moment correlation#

#data:  moyLong and sumLGly 
#t = 9.1247, df = 87, p-value = 2.487e-14
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# 0.5747428 0.7921857 
#sample estimates:
#      cor 
#0.6992978 

cor.test(log(dosages$moyLong),log(dosages$sumLGly),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(dosages$moyLong) and log(dosages$sumLGly)
#t = 9.224, df = 87, p-value = 1.561e-14
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.5798050 0.7949965
#sample estimates:
#      cor 
#0.7031568 


cor.test(dosages$moyLong,dosages$sumenergie,method="pearson")

#        Pearsons product-moment correlation#

#data:  moyLong and sumenergie 
#t = 9.5264, df = 87, p-value = 3.775e-15
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# 0.5947826 0.8032590 
#sample estimates:
#      cor 
#0.7145321 

cor.test(log(dosages$moyLong),log(dosages$sumenergie),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(dosages$moyLong) and log(dosages$sumenergie)
#t = 9.9654, df = 87, p-value = 4.755e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.6154076 0.8145070
#sample estimates:
#      cor 
#0.7300935




cor.test(dosages$volume,dosages$lipides,method="pearson")
cor.test(dosages$volume,dosages$glucose,method="pearson")
cor.test(dosages$volume,dosages$glycogene,method="pearson")
cor.test(dosages$volume,dosages$sumLGly,method="pearson")
cor.test(dosages$volume,dosages$sumenergie,method="pearson")

cor.test(dosages$volume,dosages$SLipides,method="pearson")
cor.test(dosages$volume,dosages$SGlucose,method="pearson")
cor.test(dosages$volume,dosages$SGlycogene,method="pearson")
cor.test(dosages$volume,dosages$SsumLGly,method="pearson")
cor.test(dosages$volume,dosages$Ssumenergie,method="pearson")



cor.test(dosages$volume,dosages$lipides,method="pearson")

#        Pearsons product-moment correlation#

#data:  volume and lipides 
#t = 8.6122, df = 87, p-value = 2.793e-13
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# 0.5474685 0.7768814 
#sample estimates:
#      cor 
#0.6783804 

cor.test(log(dosages$volume),log(dosages$lipides),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(dosages$volume) and log(dosages$lipides)
#t = 9.2791, df = 87, p-value = 1.204e-14
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.5825818 0.7965344
#sample estimates:
#      cor 
#0.7052705



cor.test(dosages$volume,dosages$glucose,method="pearson")

#        Pearsons product-moment correlation#

#data:  volume and glucose 
#t = 14.2483, df = 87, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# 0.7610134 0.8898730 
#sample estimates:
#      cor 
#0.8366696 


cor.test(log(dosages$volume),log(dosages$glucose),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(dosages$volume) and log(dosages$glucose)
#t = 14.486, df = 87, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.7667917 0.8927255
#sample estimates:
#     cor 
#0.840784

cor.test(dosages$volume,dosages$glycogene,method="pearson")

#        Pearsons product-moment correlation#

#data:  volume and glycogene 
#t = 14.2025, df = 87, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# 0.7598769 0.8893108 
#sample estimates:
#      cor 
#0.8358593 

cor.test(log(dosages$volume),log(dosages$glycogene),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(dosages$volume) and log(dosages$glycogene)
#t = 12.717, df = 87, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.7188203 0.8687343
#sample estimates:
#      cor 
#0.8063649 


cor.test(dosages$volume,dosages$sumLGly,method="pearson")

#        Pearsons product-moment correlation#

#data:  volume and sumLGly 
#t = 16.52, df = 87, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# 0.8092957 0.9134021 
#sample estimates:
#      cor 
#0.8707886 

cor.test(log(dosages$volume),log(dosages$sumLGly),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(dosages$volume) and log(dosages$sumLGly)
#t = 16.043, df = 87, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.8003436 0.9090914
#sample estimates:
#      cor 
#0.8645069 

cor.test(dosages$volume,dosages$sumenergie,method="pearson")

#        Pearsons product-moment correlation#

#data:  volume and sumenergie 
#t = 17.1961, df = 87, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# 0.8210657 0.9190343 
#sample estimates:
#      cor 
#0.8790172 

cor.test(log(dosages$volume),log(dosages$sumenergie),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(dosages$volume) and log(dosages$sumenergie)
#t = 18.596, df = 87, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.8424209 0.9291522
#sample estimates:
#      cor 
#0.8938592 


cor.test(dosages$volume,dosages$SLipides,method="pearson")

#        Pearsons product-moment correlation#

#data:  volume and SLipides 
#t = -0.9626, df = 87, p-value = 0.3384
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# -0.3044080  0.1079043 
#sample estimates:
#       cor 
#-0.1026594 

cor.test(log(dosages$volume),log(dosages$SLipides),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(dosages$volume) and log(dosages$SLipides)
#t = -1.1342, df = 87, p-value = 0.2598
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.32089906  0.08980505
#sample estimates:
#       cor 
#-0.1207092 



cor.test(dosages$volume,dosages$SGlucose,method="pearson")

#        Pearsons product-moment correlation#

#data:  volume and SGlucose 
#t = -0.2949, df = 87, p-value = 0.7688
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# -0.2382897  0.1778259 
#sample estimates:
#        cor 
#-0.03160116 

cor.test(log(dosages$volume),log(dosages$SGlucose),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(dosages$volume) and log(dosages$SGlucose)
#t = 0.3185, df = 87, p-value = 0.7509
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.1753761  0.2406733
#sample estimates:
#      cor 
#0.0341271 

cor.test(dosages$volume,dosages$SGlycogene,method="pearson")

#        Pearsons product-moment correlation#

#data:  volume and SGlycogene 
#t = -0.754, df = 87, p-value = 0.4529
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# -0.2840686  0.1298571 
#sample estimates:
#       cor 
#-0.0805788 


cor.test(log(dosages$volume),log(dosages$SGlycogene),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(dosages$volume) and log(dosages$SGlycogene)
#t = -0.06151, df = 87, p-value = 0.9511
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.2145565  0.2019397
#sample estimates:
#         cor 
#-0.006594387 



cor.test(dosages$volume,dosages$SsumLGly,method="pearson")

#        Pearsons product-moment correlation#

#data:  volume and SsumLGly 
#t = 0.1366, df = 87, p-value = 0.8916
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# -0.1942014  0.2222266 
#sample estimates:
#       cor 
#0.01464774 


cor.test(log(dosages$volume),log(dosages$SsumLGly),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(dosages$volume) and log(dosages$SsumLGly)
#t = 0.64129, df = 87, p-value = 0.523
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.1416892  0.2729493
#sample estimates:
#       cor 
#0.06859156



cor.test(log(densite),log(dosages$SsumLGly),method="pearson")

#       Pearson's product-moment correlation#

#data:  log(densite) and log(dosages$SsumLGly)
#t = -2.4133, df = 87, p-value = 0.01791
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.43599413 -0.04454779
#sample estimates:
#       cor 
#-0.2504807


cor.test(dosages$volume,dosages$Ssumenergie,method="pearson")

#        Pearsons product-moment correlation#

#data:  volume and Ssumenergie 
#t = 0.7608, df = 87, p-value = 0.4488
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# -0.1291452  0.2847340 
#sample estimates:
#       cor 
#0.08129809 


cor.test(dosages$volume,dosages$Ssumenergie,method="pearson")




densite<-dosages$poids/dosages$volume
densiteLipides<-dosages$lipides/dosages$volume
densiteSumenergie<-dosages$sumenergie/dosages$volume



cor.test(densite,dosages$lipides,method="pearson")

#        Pearson's product-moment correlation#

#data:  densite and dosages$lipides
#t = -2.9712, df = 87, p-value = 0.003835
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.4813484 -0.1016884
#sample estimates:
#       cor 
#-0.3035175 


cor.test(log(densite),log(dosages$lipides),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(densite) and log(dosages$lipides)
#t = -2.9584, df = 87, p-value = 0.003982
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.4803444 -0.1003958
#sample estimates:
#       cor 
#-0.3023314 


cor.test(log(densite),log(densiteLipides),method="pearson")

#       Pearson's product-moment correlation#

#data:  log(densite) and log(densiteLipides)
#t = -0.19653, df = 87, p-value = 0.8447
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.2283206  0.1880160
#sample estimates:
#        cor 
#-0.02106553 



cor.test(densite,dosages$glucose,method="pearson")

#        Pearson's product-moment correlation#

#data:  densite and dosages$glucose
#t = -4.3829, df = 87, p-value = 3.261e-05
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.5819926 -0.2381142
#sample estimates:
#       cor 
#-0.4252817 


cor.test(log(densite),log(dosages$glucose),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(densite) and log(dosages$glucose)
#t = -4.4088, df = 87, p-value = 2.957e-05
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.5836558 -0.2404887
#sample estimates:
#       cor 
#-0.4273426 


cor.test(densite,dosages$glycogene,method="pearson")

#        Pearson's product-moment correlation#

#data:  densite and dosages$glycogene
#t = -5.7207, df = 87, p-value = 1.473e-07
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.6592939 -0.3530002
#sample estimates:
#       cor 
#-0.5228219 

cor.test(log(densite),log(dosages$glycogene),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(densite) and log(dosages$glycogene)
#t = -5.905, df = 87, p-value = 6.668e-08
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.6686689 -0.3675911
#sample estimates:
#       cor 
#-0.5348995 



cor.test(densite,dosages$sumLGly,method="pearson")

#        Pearson's product-moment correlation#

#data:  densite and dosages$sumLGly
#t = -5.3043, df = 87, p-value = 8.501e-07
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.6370115 -0.3189084
#sample estimates:
#      cor 
#-0.494334 


cor.test(log(densite),log(dosages$sumLGly),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(densite) and log(dosages$sumLGly)
#t = -5.508, df = 87, p-value = 3.632e-07
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.6481041 -0.3357778
#sample estimates:
#       cor 
#-0.5084777

cor.test(densite,dosages$sumenergie,method="pearson")

#        Pearson's product-moment correlation#

#data:  densite and dosages$sumenergie
#t = -4.9139, df = 87, p-value = 4.159e-06
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.6146897 -0.2855623
#sample estimates:
#       cor 
#-0.4660999 


cor.test(log(densite),log(dosages$sumenergie),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(densite) and log(dosages$sumenergie)
#t = -5.372, df = 87, p-value = 6.418e-07
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.6407396 -0.3245557
#sample estimates:
#       cor 
#-0.4990792 



cor.test(log(densite),log(densiteSumenergie),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(densite) and log(densiteSumenergie)
#t = -3.37, df = 87, p-value = 0.001123
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.5118358 -0.1415603
#sample estimates:
#       cor 
#-0.3397994 




cor.test(densite,dosages$SLipides,method="pearson")

#        Pearson's product-moment correlation#

#data:  densite and dosages$SLipides
#t = -0.28488, df = 87, p-value = 0.7764
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.2372759  0.1788662
#sample estimates:
#        cor 
#-0.03052764 

cor.test(log(densite),log(dosages$SLipides),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(densite) and log(dosages$SLipides)
#t = 0.13916, df = 87, p-value = 0.8896
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.1939409  0.2224839
#sample estimates:
#       cor 
#0.01491837 


cor.test(densite,dosages$SGlucose,method="pearson")

#        Pearson's product-moment correlation#

#data:  densite and dosages$SGlucose
#t = -1.1355, df = 87, p-value = 0.2593
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.32101944  0.08967192
#sample estimates:
#       cor 
#-0.1208415 

cor.test(log(densite),log(dosages$SGlucose),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(densite) and log(dosages$SGlucose)
#t = -1.2331, df = 87, p-value = 0.2209
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.33030281  0.07936135
#sample estimates:
#       cor 
#-0.1310615 

cor.test(densite,dosages$SGlycogene,method="pearson")

#        Pearson's product-moment correlation#

#data:  densite and dosages$SGlycogene
#t = -2.2057, df = 87, p-value = 0.03004
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.41833282 -0.02296873
#sample estimates:
#       cor 
#-0.2301247 

cor.test(log(densite),log(dosages$SGlycogene),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(densite) and log(dosages$SGlycogene)
#t = -2.6517, df = 87, p-value = 0.009515
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.45576043 -0.06913988
#sample estimates:
#       cor 
#-0.2734591 #


cor.test(densite,dosages$SsumLGly,method="pearson")

#        Pearson's product-moment correlation#

#data:  densite and dosages$SsumLGly
#t = -2.3021, df = 87, p-value = 0.02372
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.42658789 -0.03300936
#sample estimates:
#       cor 
#-0.2396188 

cor.test(log(densite),log(dosages$SsumLGly),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(densite) and log(dosages$SsumLGly)
#t = -2.4133, df = 87, p-value = 0.01791
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.43599413 -0.04454779
#sample estimates:
#       cor 
#-0.2504807 


cor.test(densite,dosages$Ssumenergie,method="pearson")

#        Pearson's product-moment correlation#

#data:  densite and dosages$Ssumenergie
#t = -2.3746, df = 87, p-value = 0.01977
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.43273939 -0.04054342
#sample estimates:
#      cor 
#-0.246717 

cor.test(log(densite),log(dosages$Ssumenergie),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(densite) and log(dosages$Ssumenergie)
#t = -2.4072, df = 87, p-value = 0.01819
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.43548433 -0.04391975
#sample estimates:
#       cor 
#-0.2498908 


cor.test(dosages$poids,dosages$SLipides,method="pearson")
cor.test(dosages$OLSresidue,dosages$SLipides,method="pearson")
cor.test(dosages$MSL,dosages$SLipides,method="pearson")
cor.test(dosages$VSL,dosages$SLipides,method="pearson")

cor.test(dosages$poids,dosages$SGlucose,method="pearson")
cor.test(dosages$OLSresidue,dosages$SGlucose,method="pearson")
cor.test(dosages$MSL,dosages$SGlucose,method="pearson")
cor.test(dosages$VSL,dosages$SGlucose,method="pearson")

cor.test(dosages$poids,dosages$SGlycogene,method="pearson")
cor.test(dosages$OLSresidue,dosages$SGlycogene,method="pearson")
cor.test(dosages$MSL,dosages$SGlycogene,method="pearson")
cor.test(dosages$VSL,dosages$SGlycogene,method="pearson")

cor.test(dosages$poids,dosages$SsumLGly,method="pearson")
cor.test(dosages$OLSresidue,dosages$SsumLGly,method="pearson")
cor.test(dosages$MSL,dosages$SsumLGly,method="pearson")
cor.test(dosages$VSL,dosages$SsumLGly,method="pearson")

cor.test(dosages$poids,dosages$Ssumenergie,method="pearson")
cor.test(dosages$OLSresidue,dosages$Ssumenergie,method="pearson")
cor.test(dosages$MSL,dosages$Ssumenergie,method="pearson")
cor.test(dosages$VSL,dosages$Ssumenergie,method="pearson")


cor.test(dosages$poids,dosages$SLipides,method="pearson")

#        Pearsons product-moment correlation#

#data:  poids and SLipides 
#t = -1.4434, df = 87, p-value = 0.1525
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# -0.35004139  0.05714346 
#sample estimates:
#       cor 
#-0.1529333 

cor.test(log(dosages$poids),log(dosages$SLipides),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(dosages$poids) and log(dosages$SLipides)
#t = -1.355, df = 87, p-value = 0.1789
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.34178356  0.06648786
#sample estimates:
#       cor 
#-0.1437594 


cor.test(dosages$OLSresidue,dosages$SLipides,method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$OLSresidue and dosages$SLipides
#t = 2.6253, df = 87, p-value = 0.01023
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.06642791 0.45359918
#sample estimates:
#      cor 
#0.2709365 



cor.test(dosages$OLSresidue,log(dosages$SLipides),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$OLSresidue and log(dosages$SLipides)
#t = 3.2165, df = 87, p-value = 0.001823
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.1263289 0.5002985
#sample estimates:
#      cor 
#0.3260088 



par(mfrow=c(1,2))
plot(dosages$OLSresidue,dosages$SLipides)
plot(log(dosages$OLSresidue),log(dosages$SLipides))

par(mfrow=c(1,2))
plot(dosages$MSL,dosages$SLipides)
plot(log(dosages$MSL),log(dosages$SLipides))

par(mfrow=c(1,2))
plot(dosages$VSL,dosages$SLipides)
plot(log(dosages$VSL),log(dosages$SLipides))

par(mfrow=c(1,2))
plot(dosages$OLSresidue,dosages$SLipides)
plot(log(dosages$OLSresidue),log(dosages$SLipides))



cor.test(dosages$OLSresidue,log(dosages$SLipides),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$OLSresidue and log(dosages$SLipides)
#t = 3.2165, df = 87, p-value = 0.001823
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.1263289 0.5002985
#sample estimates:
#      cor 
#0.3260088 




cor.test(dosages$MSL,dosages$SLipides,method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$MSL and dosages$SLipides
#t = 3.9897, df = 87, p-value = 0.0001375
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.2015260 0.5559964
#sample estimates:
#      cor 
#0.3932773 


cor.test(dosages$MSL,log(dosages$SLipides),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$MSL and log(dosages$SLipides)
#t = 4.7254, df = 87, p-value = 8.76e-06
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.2689923 0.6033982
#sample estimates:
#      cor 
#0.4519321


cor.test(dosages$VSL,dosages$SLipides,method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$VSL and dosages$SLipides
#t = 5.5402, df = 87, p-value = 3.172e-07
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.3384106 0.6498235
#sample estimates:
#      cor 
#0.5106767 

cor.test(dosages$VSL,log(dosages$SLipides),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$VSL and log(dosages$SLipides)
#t = 5.4996, df = 87, p-value = 3.762e-07
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.3350935 0.6476567
#sample estimates:
#      cor 
#0.5079058 


cor.test(dosages$DSL,log(dosages$SLipides),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$DSL and log(dosages$SLipides)
#t = -3.7548, df = 87, p-value = 0.0003127
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.5397197 -0.1791103
#sample estimates:
#       cor 
#-0.3734374 


cor.test(dosages$poids,dosages$SGlucose,method="pearson")

#        Pearsons product-moment correlation#

#data:  poids and SGlucose 
#t = -1.3097, df = 87, p-value = 0.1937
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# -0.3375329  0.0712701 
#sample estimates:
#       cor 
#-0.1390505 

cor.test(log(dosages$poids),log(dosages$SGlucose),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(dosages$poids) and log(dosages$SGlucose)
#t = -0.4752, df = 87, p-value = 0.6358
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.2564204  0.1590614
#sample estimates:
#        cor 
#-0.05088074 


cor.test(dosages$OLSresidue,dosages$SGlucose,method="pearson")

#        Pearson's product-moment correlation

#data:  dosages$OLSresidue and dosages$SGlucose
#t = 3.7648, df = 87, p-value = 0.0003022
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.1800657 0.5404190
#sample estimates:
#      cor 
#0.3742867 


cor.test(dosages$OLSresidue,log(dosages$SGlucose),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$OLSresidue and log(dosages$SGlucose)
#t = 4.9164, df = 87, p-value = 4.118e-06
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.2857754 0.6148341
#sample estimates:
#      cor 
#0.4662816 


cor.test(dosages$MSL,dosages$SGlucose,method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$MSL and dosages$SGlucose
#t = 5.2645, df = 87, p-value = 1.002e-06
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.3155762 0.6348046
#sample estimates:
#      cor 
#0.4915292 

cor.test(dosages$MSL,log(dosages$SGlucose),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$MSL and log(dosages$SGlucose)
#t = 6.4029, df = 87, p-value = 7.486e-09
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.4054813 0.6925753
#sample estimates:
#      cor 
#0.5659471 


cor.test(dosages$VSL,dosages$SGlucose,method="pearson")

#        Pearson's product-moment correlation

#data:  dosages$VSL and dosages$SGlucose
#t = 9.6281, df = 87, p-value = 2.325e-15
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.5996759 0.8059411
#sample estimates:
#      cor 
#0.7182348 

cor.test(dosages$VSL,log(dosages$SGlucose),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$VSL and log(dosages$SGlucose)
#t = 9.8144, df = 87, p-value = 9.674e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.6084567 0.8107329
#sample estimates:
#      cor 
#0.7248623 


cor.test(dosages$DSL,log(dosages$SGlucose),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$DSL and log(dosages$SGlucose)
#t = -5.2113, df = 87, p-value = 1.247e-06
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.6318245 -0.3110888
#sample estimates:
#       cor 
#-0.4877462 

cor.test(dosages$poids,dosages$SGlycogene,method="pearson")

#        Pearsons product-moment correlation#

#data:  poids and SGlycogene 
#t = -2.5091, df = 87, p-value = 0.01396
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# -0.4440097 -0.0544634 
#sample estimates:
#       cor 
#-0.2597737 

cor.test(dosages$OLSresidue,dosages$SGlycogene,method="pearson")

#        Pearson's product-moment correlation

#data:  dosages$OLSresidue and dosages$SGlycogene
#t = 1.1704, df = 87, p-value = 0.245
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.0859777  0.3243547
#sample estimates:
#      cor 
#0.1245084 

cor.test(dosages$OLSresidue,log(dosages$SGlycogene),method="pearson")


#        Pearson's product-moment correlation#

#data:  dosages$OLSresidue and log(dosages$SGlycogene)
#t = 1.6012, df = 87, p-value = 0.113
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.04049416  0.36459951
#sample estimates:
#      cor 
#0.1691894 

cor.test(dosages$MSL,dosages$SGlycogene,method="pearson")

#        Pearson's product-moment correlation

#data:  dosages$MSL and dosages$SGlycogene
#t = 2.214, df = 87, p-value = 0.02944
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.02384177 0.41905320
#sample estimates:
#      cor 
#0.2309518 

cor.test(dosages$MSL,log(dosages$SGlycogene),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$MSL and log(dosages$SGlycogene)
#t = 2.5222, df = 87, p-value = 0.01348
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.05581502 0.44509747
#sample estimates:
#      cor 
#0.2610375

cor.test(dosages$VSL,dosages$SGlycogene,method="pearson")

#        Pearson's product-moment correlation

#data:  dosages$VSL and dosages$SGlycogene
#t = 6.8545, df = 87, p-value = 9.822e-10
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.4379204 0.7125532
#sample estimates:
#      cor 
#0.5921712 

cor.test(dosages$VSL,log(dosages$SGlycogene),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$VSL and log(dosages$SGlycogene)
#t = 7.4981, df = 87, p-value = 5.123e-11
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.4810472 0.7384412
#sample estimates:
#      cor 
#0.6265367 


cor.test(dosages$DSL,log(dosages$SGlycogene),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$DSL and log(dosages$SGlycogene)
#t = -6.6597, df = 87, p-value = 2.37e-09
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.7041279 -0.4241520
#sample estimates:
#       cor 
#-0.5810805#


cor.test(dosages$poids,dosages$SsumLGly,method="pearson")

#        Pearsons product-moment correlation#

#data:  poids and SsumLGly 
#t = -1.453, df = 87, p-value = 0.1498
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# -0.35092471  0.05613967 
#sample estimates:
#       cor 
#-0.1539166 

cor.test(log(dosages$poids),log(dosages$SsumLGly),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(dosages$poids) and log(dosages$SsumLGly)
#t = -0.87055, df = 87, p-value = 0.3864
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.2954676  0.1176037
#sample estimates:
#        cor 
#-0.09292907

cor.test(dosages$OLSresidue,dosages$SsumLGly,method="pearson")

#        Pearson's product-moment correlation

#data:  dosages$OLSresidue and dosages$SsumLGly
#t = 2.5731, df = 87, p-value = 0.01177
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.06105824 0.44930647
#sample estimates:
#      cor 
#0.2659334 


cor.test(dosages$OLSresidue,log(dosages$SsumLGly),method="pearson")


#        Pearson's product-moment correlation#

#data:  dosages$OLSresidue and log(dosages$SsumLGly)
#t = 3.3816, df = 87, p-value = 0.001081
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.1427094 0.5127009
#sample estimates:
#      cor 
#0.3408364 



cor.test(dosages$MSL,dosages$SsumLGly,method="pearson")

#        Pearson's product-moment correlation

#data:  dosages$MSL and dosages$SsumLGly
#t = 3.5711, df = 87, p-value = 0.0005819
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.1613064 0.5265968
#sample estimates:
#      cor 
#0.3575519 


cor.test(dosages$MSL,log(dosages$SsumLGly),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$MSL and log(dosages$SsumLGly)
#t = 4.3632, df = 87, p-value = 3.511e-05
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.2363147 0.5807303
#sample estimates:
#      cor 
#0.4237185 


cor.test(dosages$VSL,dosages$SsumLGly,method="pearson")

#        Pearson's product-moment correlation

#data:  dosages$VSL and dosages$SsumLGly
#t = 9.038, df = 87, p-value = 3.755e-14
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.5702637 0.7896910
#sample estimates:
#      cor 
#0.6958772 


cor.test(dosages$VSL,log(dosages$SsumLGly),method="pearson")


#        Pearson's product-moment correlation#

#data:  dosages$VSL and log(dosages$SsumLGly)
#t = 9.6668, df = 87, p-value = 1.938e-15
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.6015182 0.8069487
#sample estimates:
#      cor 
#0.7196271 


cor.test(dosages$DSL,log(dosages$SsumLGly),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$DSL and log(dosages$SsumLGly)
#t = -6.1188, df = 87, p-value = 2.627e-08
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.6791842 -0.3841349
#sample estimates:
#       cor 
#-0.5485115 


cor.test(dosages$poids,dosages$Ssumenergie,method="pearson")

#        Pearsons product-moment correlation#

#data:  poids and Ssumenergie 
#t = -0.6972, df = 87, p-value = 0.4875
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# -0.2784785  0.1358203 
#sample estimates:
#        cor 
#-0.07454499 


cor.test(log(dosages$poids),log(dosages$Ssumenergie),method="pearson")

#        Pearson's product-moment correlation#

#data:  log(dosages$poids) and log(dosages$Ssumenergie)
#t = -0.34613, df = 87, p-value = 0.7301
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.2434604  0.1725050
#sample estimates:
#        cor 
#-0.03708394 


cor.test(dosages$OLSresidue,dosages$Ssumenergie,method="pearson")

#        Pearsons product-moment correlation#

#data:  OLSresidue and Ssumenergie 
#t = 3.5932, df = 87, p-value = 0.0005406
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# 0.1634571 0.5281914 
#sample estimates:
#      cor 
#0.3594769 


cor.test(dosages$OLSresidue,log(dosages$Ssumenergie),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$OLSresidue and log(dosages$Ssumenergie)
#t = 4.2277, df = 87, p-value = 5.81e-05
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.2238120 0.5719138
#sample estimates:
#      cor 
#0.4128267 

cor.test(dosages$MSL,dosages$Ssumenergie,method="pearson")

#        Pearsons product-moment correlation#

#data:  MSL and Ssumenergie 
#t = 4.6002, df = 87, p-value = 1.425e-05
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# 0.2578121 0.5957031 
#sample estimates:
#      cor 
#0.4423202 

cor.test(dosages$MSL,log(dosages$Ssumenergie),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$MSL and log(dosages$Ssumenergie)
#t = 5.2732, df = 87, p-value = 9.667e-07
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.3163018 0.6352856
#sample estimates:
#      cor 
#0.4921403 


cor.test(dosages$VSL,dosages$Ssumenergie,method="pearson")

#        Pearsons product-moment correlation#

#data:  VSL and Ssumenergie 
#t = 10.7428, df = 87, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# 0.6488798 0.8324483 
#sample estimates:
#      cor 
#0.7550975 


























cor.test(dosages$DSL,dosages$lipides,method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$DSL and dosages$lipides
#t = 2.1776, df = 87, p-value = 0.03214
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.02004076 0.41591317
#sample estimates:
#      cor 
#0.2273486 



cor.test(dosages$DSL,dosages$glucose,method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$DSL and dosages$glucose
#t = 1.9379, df = 87, p-value = 0.05587
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.005046195  0.394949232
#sample estimates:
#      cor 
#0.2034243 



cor.test(dosages$DSL,dosages$glycogene,method="pearson")


#        Pearson's product-moment correlation#

#data:  dosages$DSL and dosages$glycogene
#t = 1.4045, df = 87, p-value = 0.1637
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.06125881  0.34641236
#sample estimates:
#      cor 
#0.1488975 


cor.test(dosages$DSL,dosages$sumLGly,method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$DSL and dosages$sumLGly
#t = 1.7364, df = 87, p-value = 0.08603
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.0262379  0.3769097
#sample estimates:
#      cor 
#0.1830189


cor.test(dosages$DSL,dosages$sumenergie,method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$DSL and dosages$sumenergie
#t = 2.0928, df = 87, p-value = 0.03928
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.01118041 0.40855676
#sample estimates:
#      cor 
#0.2189274 #




cor.test(dosages$DSL,dosages$SLipides,method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$DSL and dosages$SLipides
#t = -3.8504, df = 87, p-value = 0.0002247
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.5464106 -0.1882796
#sample estimates:
#       cor 
#-0.3815746 

cor.test(dosages$DSL,dosages$SGlucose,method="pearson")


#        Pearson's product-moment correlation#

#data:  dosages$DSL and dosages$SGlucose
#t = -5.231, df = 87, p-value = 1.15e-06
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.6329276 -0.3127481
#sample estimates:
#       cor 
#-0.4891458 


cor.test(dosages$DSL,dosages$SGlycogene,method="pearson")


#        Pearson's product-moment correlation#

#data:  dosages$DSL and dosages$SGlycogene
#t = -6.1942, df = 87, p-value = 1.886e-08
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.6828038 -0.3898739
#sample estimates:
#       cor 
#-0.5532131#




cor.test(dosages$DSL,dosages$SsumLGly,method="pearson")



#        Pearson's product-moment correlation#

#data:  dosages$DSL and dosages$SsumLGly
#t = -5.8845, df = 87, p-value = 7.285e-08
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.6676422 -0.3659859
#sample estimates:
#       cor 
#-0.5335741 


cor.test(dosages$DSL,log(dosages$SsumLGly),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$DSL and log(dosages$SsumLGly)
#t = -6.1188, df = 87, p-value = 2.627e-08
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.6791842 -0.3841349
#sample estimates:
#       cor 
#-0.5485115

cor.test(dosages$DSL,dosages$Ssumenergie,method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$DSL and dosages$Ssumenergie
#t = -5.6272, df = 87, p-value = 2.194e-07
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.6544274 -0.3454844
#sample estimates:
#       cor 
#-0.5165739 






























par(mfrow=c(2,2))
boxplot(SLipides~traitement,main="Scaled lipides")
means<-by(SLipides,traitement,mean)
points(1:4,means,pch=23,cex=0.75,bg="red")
boxplot(SGlucose~traitement,main="Scaled glucose")
means<-by(SGlucose,traitement,mean)
points(1:4,means,pch=23,cex=0.75,bg="red")
boxplot(SGlycogene~traitement,main="Scaled glycogen")
means<-by(SGlycogene,traitement,mean)
points(1:4,means,pch=23,cex=0.75,bg="red")
boxplot(SsumLGly~traitement, main="Scaled sum glycoogen+gluscose")
means<-by(SsumLGly,traitement,mean)
points(1:4,means,pch=23,cex=0.75,bg="red")

boxplot(log(SLipides)~traitement,main="Scaled lipides")
boxplot(log(SGlucose)~traitement,main="Scaled glucose")
boxplot(log(SGlycogene)~traitement,main="Scaled glycogen")
boxplot(log(SsumLGly)~traitement, main="Scaled sum glycoogen+gluscose")



hist(SLipides)
hist(SGlucose)
hist(SGlycogene)
hist(SsumLGly)

TreatLarve	TreatAdult


M1<-lm(log(SLipides)~TreatLarve*TreatAdult)
par(mfrow=c(2,2))
plot(M1)
step(M1,test="F")

M1<-lm(log(SLipides)~TreatLarve+TreatAdult)
step(M1,test="F")

M1<-lm(log(SGlucose)~TreatLarve*TreatAdult)
plot(M1)
step(M1,test="F")

M1<-lm(log(SGlycogene)~TreatLarve*TreatAdult)
plot(M1)
step(M1,test="F")

M1<-lm(log(SsumLGly)~TreatLarve*TreatAdult)
plot(M1)
step(M1,test="F")

M1<-glm(SLipides~TreatLarve*TreatAdult,family=quasipoisson)
anova(M1,test="F")
M1<-glm(SLipides~TreatLarve+TreatAdult,family=quasipoisson)
anova(M1,test="F")
M1<-glm(SLipides~TreatLarve,family=quasipoisson)
anova(M1,test="F")


M1<-glm(SGlucose~TreatLarve*TreatAdult,family=quasipoisson)
#plot(M1)
anova(M1,test="F")
M1<-glm(SGlucose~TreatLarve+TreatAdult,family=quasipoisson)
anova(M1,test="F")
M1<-glm(SGlucose~TreatLarve,family=quasipoisson)
anova(M1,test="F")

M1<-glm(SGlycogene~TreatLarve*TreatAdult,family=quasipoisson)
anova(M1,test="F")
M1<-glm(SGlycogene~TreatLarve+TreatAdult,family=quasipoisson)
anova(M1,test="F")
M1<-glm(SGlycogene~TreatLarve,family=quasipoisson)
anova(M1,test="F")

M1<-glm(SsumLGly~TreatLarve*TreatAdult,family=quasipoisson)
anova(M1,test="F")
M1<-glm(SsumLGly~TreatLarve+TreatAdult,family=quasipoisson)
anova(M1,test="F")
M1<-glm(SsumLGly~TreatLarve,family=quasipoisson)
anova(M1,test="F")



cor.test(dosages$poids,dosages$lipides,method="pearson")
cor.test(dosages$OLSresidue,dosages$lipides,method="pearson")
cor.test(dosages$MSL,dosages$lipides,method="pearson")
cor.test(dosages$VSL,dosages$lipides,method="pearson")

cor.test(dosages$poids,dosages$glucose,method="pearson")
cor.test(dosages$OLSresidue,dosages$glucose,method="pearson")
cor.test(dosages$MSL,dosages$glucose,method="pearson")
cor.test(dosages$VSL,dosages$glucose,method="pearson")

cor.test(dosages$poids,dosages$glycogene,method="pearson")
cor.test(dosages$OLSresidue,dosages$glycogene,method="pearson")
cor.test(dosages$MSL,dosages$glycogene,method="pearson")
cor.test(dosages$VSL,dosages$glycogene,method="pearson")

cor.test(dosages$poids,dosages$sumLGly,method="pearson")
cor.test(dosages$OLSresidue,dosages$sumLGly,method="pearson")
cor.test(dosages$MSL,dosages$sumLGly,method="pearson")
cor.test(dosages$VSL,dosages$sumLGly,method="pearson")

cor.test(dosages$poids,dosages$sumenergie,method="pearson")
cor.test(dosages$OLSresidue,dosages$sumenergie,method="pearson")
cor.test(dosages$MSL,dosages$sumenergie,method="pearson")
cor.test(dosages$VSL,dosages$sumenergie,method="pearson")

 

cor.test(dosages$OLSresidue,dosages$lipides,method="pearson")

#        Pearson's product-moment correlation

#data:  dosages$OLSresidue and dosages$lipides
#t = 3.0604, df = 87, p-value = 0.002939
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.1106857 0.4883092
#sample estimates:
#      cor 
#0.3117561 


cor.test(dosages$OLSresidue,log(dosages$lipides),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$OLSresidue and log(dosages$lipides)
#t = 3.7662, df = 87, p-value = 0.0003008
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.1802033 0.5405197
#sample estimates:
#     cor 
#0.374409 

cor.test(log(dosages$OLSresidue),log(dosages$lipides),method="pearson")





cor.test(dosages$MSL,dosages$lipides,method="pearson")

#        Pearson's product-moment correlation

#data:  dosages$MSL and dosages$lipides
#t = 1.1339, df = 87, p-value = 0.26
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.08983807  0.32086920
#sample estimates:
#      cor 
#0.1206765 


cor.test(dosages$MSL,log(dosages$lipides),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$MSL and log(dosages$lipides)
#t = 1.5986, df = 87, p-value = 0.1135
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.04076309  0.36436592
#sample estimates:
#      cor 
#0.1689277

cor.test(dosages$VSL,dosages$lipides,method="pearson")

#        Pearsons product-moment correlation#

#data:  VSL and lipides 
#t = 1.5995, df = 87, p-value = 0.1133
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# -0.04067228  0.36444480 
#sample estimates:
#      cor 
#0.1690161 

cor.test(dosages$VSL,log(dosages$lipides),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$VSL and log(dosages$lipides)
#t = 1.5917, df = 87, p-value = 0.1151
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.04148882  0.36373531
#sample estimates:
#      cor 
#0.1682214 


cor.test(dosages$DSL,log(dosages$lipides),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$DSL and log(dosages$lipides)
#t = 2.8299, df = 87, p-value = 0.005781
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.08735075 0.47015604
#sample estimates:
#     cor 
#0.290326 



cor.test(dosages$OLSresidue,dosages$glucose,method="pearson")

#        Pearson's product-moment correlation

#data:  dosages$OLSresidue and dosages$glucose
#t = 4.0591, df = 87, p-value = 0.0001073
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.2080721 0.5606990
#sample estimates:
#      cor 
#0.3990376 

cor.test(dosages$OLSresidue,log(dosages$glucose),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$OLSresidue and log(dosages$glucose)
#t = 5.1738, df = 87, p-value = 1.454e-06
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.3079121 0.6297091
#sample estimates:
#      cor 
#0.4850642 


cor.test(dosages$MSL,dosages$glucose,method="pearson")

#        Pearson's product-moment correlation

#data:  dosages$MSL and dosages$glucose
#t = 1.4834, df = 87, p-value = 0.1416
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.05292035  0.35375278
#sample estimates:
#      cor 
#0.1570674 

cor.test(dosages$MSL,log(dosages$glucose),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$MSL and log(dosages$glucose)
#t = 2.4461, df = 87, p-value = 0.01646
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.04794426 0.43874676
#sample estimates:
#      cor 
#0.2536682 

cor.test(dosages$VSL,dosages$glucose,method="pearson")

#        Pearson's product-moment correlation

#data:  dosages$VSL and dosages$glucose
#t = 2.8144, df = 87, p-value = 0.006042
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.0857763 0.4689194
#sample estimates:
#      cor 
#0.2888728 

cor.test(dosages$VSL,log(dosages$glucose),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$VSL and log(dosages$glucose)
#t = 3.364, df = 87, p-value = 0.001144
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.1409704 0.5113915
#sample estimates:
#     cor 
#0.339267 

cor.test(dosages$DSL,log(dosages$glucose),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$DSL and log(dosages$glucose)
#t = 1.9352, df = 87, p-value = 0.05621
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.005331317  0.394708551
#sample estimates:
#     cor 
#0.203151

cor.test(dosages$OLSresidue,dosages$glycogene,method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$OLSresidue and dosages$glycogene
#t = 2.695, df = 87, p-value = 0.008447
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.07357852 0.45928790
#sample estimates:
#      cor 
#0.2775818 


cor.test(dosages$OLSresidue,log(dosages$glycogene),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$OLSresidue and log(dosages$glycogene)
#t = 2.1321, df = 87, p-value = 0.03581
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.01529349 0.41197812
#sample estimates:
#      cor 
#0.2228405 


cor.test(dosages$MSL,dosages$glycogene,method="pearson")

#        Pearson's product-moment correlation

#data:  dosages$MSL and dosages$glycogene
#t = 0.25581, df = 87, p-value = 0.7987
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.1818802  0.2343338
#sample estimates:
#       cor 
#0.02741496


cor.test(dosages$MSL,log(dosages$glycogene),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$MSL and log(dosages$glycogene)
#t = -0.18588, df = 87, p-value = 0.853
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.2272379  0.1891174
#sample estimates:
#        cor 
#-0.01992409 #


cor.test(dosages$VSL,dosages$glycogene,method="pearson")

#        Pearson's product-moment correlation

#data:  dosages$VSL and dosages$glycogene
#t = 2.6202, df = 87, p-value = 0.01037
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.06589751 0.45317596
#sample estimates:
#      cor 
#0.2704428 

cor.test(dosages$VSL,log(dosages$glycogene),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$VSL and log(dosages$glycogene)
#t = 2.1369, df = 87, p-value = 0.03542
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.01578706 0.41238794
#sample estimates:
#      cor 
#0.2233096 

cor.test(dosages$DSL,log(dosages$glycogene),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$DSL and log(dosages$glycogene)
#t = 1.446, df = 87, p-value = 0.1518
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.05687193  0.35028040
#sample estimates:
#      cor 
#0.1531993 


cor.test(dosages$OLSresidue,dosages$sumLGly,method="pearson")

#        Pearson's product-moment correlation

#data:  dosages$OLSresidue and dosages$sumLGly
#t = 3.5012, df = 87, p-value = 0.0007333
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.1544707 0.5215117
#sample estimates:
#      cor 
#0.3514223 

cor.test(dosages$OLSresidue,log(dosages$sumLGly),method="pearson")


#        Pearson's product-moment correlation#

#data:  dosages$OLSresidue and log(dosages$sumLGly)
#t = 3.4816, df = 87, p-value = 0.0007817
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.1525569 0.5200833
#sample estimates:
#      cor 
#0.3497033 


cor.test(dosages$MSL,dosages$sumLGly,method="pearson")

#        Pearson's product-moment correlation

#data:  dosages$MSL and dosages$sumLGly
#t = 0.89142, df = 87, p-value = 0.3752
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.1154068  0.2974990
#sample estimates:
#       cor 
#0.09513651

cor.test(dosages$MSL,log(dosages$sumLGly),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$MSL and log(dosages$sumLGly)
#t = 0.91001, df = 87, p-value = 0.3653
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.1134486  0.2993065
#sample estimates:
#       cor 
#0.09710236


cor.test(dosages$VSL,dosages$sumLGly,method="pearson")

#        Pearson's product-moment correlation

#data:  dosages$VSL and dosages$sumLGly
#t = 2.8382, df = 87, p-value = 0.005646
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.0881956 0.4708190
#sample estimates:
#      cor 
#0.2911055 

cor.test(dosages$VSL,log(dosages$sumLGly),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$VSL and log(dosages$sumLGly)
#t = 2.6686, df = 87, p-value = 0.009086
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.07086789 0.45713516
#sample estimates:
#     cor 
#0.275065 

cor.test(dosages$DSL,log(dosages$sumLGly),method="pearson")

#        Pearson's product-moment correlation#

#data:  dosages$DSL and log(dosages$sumLGly)
#t = 1.8724, df = 87, p-value = 0.06451
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.01192877  0.38912405
#sample estimates:
#      cor 
#0.1968169

cor.test(dosages$poids,dosages$sumenergie,method="pearson")

#        Pearsons product-moment correlation#

#data:  poids and sumenergie 
#t = 11.1468, df = 87, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# 0.6648523 0.8408761 
#sample estimates:
#      cor 
#0.7669212 


cor.test(dosages$OLSresidue,dosages$sumenergie,method="pearson")

#        Pearsons product-moment correlation#

#data:  OLSresidue and sumenergie 
#t = 3.6986, df = 87, p-value = 0.0003789
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# 0.1736885 0.5357419 
#sample estimates:
#      cor 
#0.3686119 

cor.test(dosages$MSL,dosages$sumenergie,method="pearson")

#        Pearsons product-moment correlation#

#data:  MSL and sumenergie 
#t = 1.0782, df = 87, p-value = 0.2839
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# -0.09571688  0.31553977 
#sample estimates:
#      cor 
#0.1148288 

cor.test(dosages$VSL,dosages$sumenergie,method="pearson")

#        Pearsons product-moment correlation#

#data:  VSL and sumenergie 
#t = 2.6095, df = 87, p-value = 0.01068
#alternative hypothesis: true correlation is not equal to 0 
#95 percent confidence interval:
# 0.06479773 0.45229785 
#sample estimates:
#      cor 
#0.2694188 

##########################

setwd("~/Dropbox/condition/ANALYSE")

Fecond= read.csv(file = "FeconditeMixNew.csv", header = TRUE)
#attach(Fecond)
names(Fecond)

 [1] "individu"   "eggs"       "larve"      "Diff"       "Rank"      
 [6] "milieu"     "tLarvaire"  "tAdulte"    "Bin"        "eggs.trunc"
[11] "TreatLarve" "TreatAdult" "moyLong"    "moyepaiss"  "moylargeur"
[16] "volume"     "poids"      "MSL"        "VSL"        "OLS" 

summary(Fecond$milieu)

boxplot(Fecond$eggs~Fecond$milieu)
hist(Fecond$eggs)
boxplot(Fecond$OLS~Fecond$milieu)

library(lme4)


M0<-glmer(Bin~TreatAdult*TreatLarve*Rank+(1|individu),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),family=binomial,na.action=na.fail,data=Fecond)
summary(M0)


M1<-glmer(Bin~TreatAdult*TreatLarve*Rank*OLS+(1|individu),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),family=binomial,na.action=na.fail,data=Fecond)

library(MuMIn)


ms1 <- dredge(M1)
ms1
head(ms1,25)
model.avg(ms1)
confset.95p <- get.models(ms1, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p,dispersion=19.65)
summary(avgmod.95p)
confint(avgmod.95p)


#install.packages("mgcv")
#install.packages("lme4")
#install.packages("car")
#install.packages("R2admb")
#install.packages("ggplot2")
#install.packages("MuMIn")
#install.packages("glmmADMB",repos=c("http://glmmadmb.r-forge.r-project.org/repos",getOption("repos")),    type="source")

M1<-glmer(Bin~TreatLarve*Rank+TreatAdult*Rank+TreatLarve*OLS+TreatAdult*OLS+(1|individu),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),family=binomial,na.action=na.fail,data=Fecond)
ms1 <- dredge(M1)
ms1



library(mgcv)
library(R2admb)
library(glmmADMB)
library(ggplot2)
library(MuMIn)

M1<-glmmadmb(Bin~TreatAdult*TreatLarve*Rank*OLS+(1|individu),family="binomial",data=Fecond)

ms1 <- dredge(M1)
ms1
head(ms1,20)
model.avg(ms1)
confset.95p <- get.models(ms1, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)


fit_zipoiss <-glmmadmb(eggs ~ milieu + OLS + Rank + (1 | individu), family = "poisson", zeroInflation = TRUE,data=Fecond)
fit_zipoiss2 <-glmmadmb(eggs ~ OLS + Rank + (1 | individu), family = "poisson", zeroInflation = TRUE,data=Fecond)
fit_zipoiss3 <-glmmadmb(eggs ~ OLS + Rank + (1 | individu), family = "poisson", zeroInflation = TRUE,data=Fecond)



library(MuMIn)
fit_zipoiss <- glmmadmb(eggs~tAdulte+OLS+Rank+(1|individu), zeroInflation=TRUE, family="poisson")
summary(fit_zipoiss)
library(car)

Anova(fit_zipoiss,type="III")

ms1 <- dredge(fit_zipoiss)
ms1
model.avg(ms1)
confset.95p <- get.models(ms1, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)



fit_zinb2 <- glmmadmb(eggs~tAdulte*Rank*OLS+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=200,maxfn=1000,imaxfn=500,maxph=5))
summary(fit_zinb2)

AICc(fit_zinb2)



fit_zinb <- glmmadmb(eggs~tAdulte*tLarvaire*Rank*OLS+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=200,maxfn=1000,imaxfn=500,maxph=5))
summary(fit_zinb)

ms1 <- dredge(fit_zinb,rank="AICc")
ms1

AICc(fit_zinb)

#fit_zinb <- glmmadmb(eggs~tAdulte*tLarvaire*Rank*OLS+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond)





write.table(ms1,"MixedAICc.txt",sep="\t",row.names=F,quote=F) 



fit <- glm.nb(eggs~tAdulte*tLarvaire*Rank*OLS,data=Fecond,na.action=na.fail)
summary(fit)

ms1 <- dredge(fit,rank="AICc")
head(ms1,20)
write.table(ms1,"NotMixedAICc.txt",sep="\t",row.names=F,quote=F) 




fit_zinb1 <- glmmadmb(eggs~OLS+Rank+tAdulte+Rank:tAdulte+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb2 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tLarvaire+Rank:tAdulte+Rank:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb3 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tLarvaire+Rank:tAdulte+Rank:tLarvaire+tAdulte:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb4 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tAdulte+OLS:tLarvaire+Rank:tAdulte+Rank:tLarvaire+tAdulte:tLarvaire+OLS:tAdulte:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb5 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tAdulte+OLS:tLarvaire+Rank:tAdulte+Rank:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb6 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tLarvaire+Rank:tAdulte+Rank:tLarvaire+OLS:Rank:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb7 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tAdulte+OLS:tLarvaire+Rank:tAdulte+Rank:tLarvaire+tAdulte:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb8 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tAdulte+OLS:tLarvaire+Rank:tAdulte+Rank:tLarvaire+tAdulte:tLarvaire+OLS:Rank:tLarvaire+OLS:tAdulte:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb9 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tLarvaire+Rank:tAdulte+Rank:tLarvaire+tAdulte:tLarvaire+OLS:Rank:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb10 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tAdulte+OLS:tLarvaire+Rank:tAdulte+Rank:tLarvaire+tAdulte:tLarvaire+OLS:Rank:tAdulte+OLS:tAdulte:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb11 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tAdulte+OLS:tLarvaire+Rank:tAdulte+Rank:tLarvaire+OLS:Rank:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb12 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tAdulte+OLS:tLarvaire+Rank:tAdulte+Rank:tLarvaire+OLS:Rank:tAdulte+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb13 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+Rank:tAdulte+Rank:tLarvaire+tAdulte:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb14 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tAdulte+Rank:tAdulte+Rank:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb15 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tAdulte+OLS:tLarvaire+Rank:tAdulte+Rank:tLarvaire+tAdulte:tLarvaire+OLS:Rank:tAdulte+OLS:Rank:tLarvaire+OLS:tAdulte:tLarvaire+Rank:tAdulte:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb16 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tAdulte+OLS:tLarvaire+Rank:tAdulte+Rank:tLarvaire+tAdulte:tLarvaire+OLS:Rank:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb17 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tAdulte+OLS:tLarvaire+Rank:tAdulte+Rank:tLarvaire+tAdulte:tLarvaire+OLS:Rank:tAdulte+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb18 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tAdulte+OLS:tLarvaire+Rank:tAdulte+Rank:tLarvaire+OLS:Rank:tAdulte+OLS:Rank:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb19 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tAdulte+Rank:tAdulte+Rank:tLarvaire+tAdulte:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb20 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tAdulte+Rank:tAdulte+Rank:tLarvaire+OLS:Rank:tAdulte+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb21 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tAdulte+OLS:tLarvaire+Rank:tAdulte+Rank:tLarvaire+tAdulte:tLarvaire+OLS:Rank:tAdulte+OLS:Rank:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb22 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tAdulte+Rank:tAdulte+Rank:tLarvaire+tAdulte:tLarvaire+OLS:Rank:tAdulte+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond,admb.opts=admbControl(shess=FALSE,noinit=FALSE))
fit_zinb23 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond,admb.opts=admbControl(shess=FALSE,noinit=FALSE))
fit_zinb24 <- glmmadmb(eggs~OLS+Rank+tAdulte+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb25 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:tLarvaire+tAdulte:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb26 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:tAdulte+OLS:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb27 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:tLarvaire+Rank:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=400,maxfn=2000,imaxfn=1000,maxph=10))
fit_zinb28 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:tAdulte+OLS:tLarvaire+tAdulte:tLarvaire+OLS:tAdulte:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=400,maxfn=2000,imaxfn=1000,maxph=10))
fit_zinb29 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb30 <- glmmadmb(eggs~OLS+Rank+tAdulte+OLS:tAdulte+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb31 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:tAdulte+OLS:tLarvaire+Rank:tLarvaire+tAdulte:tLarvaire+OLS:tAdulte:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)

fit_zinb32 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:tAdulte+OLS:tLarvaire+tAdulte:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=2000,maxfn=10000,imaxfn=5000,maxph=10))



fit_zinb33 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:tAdulte+OLS:tLarvaire+Rank:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=2000,maxfn=10000,imaxfn=5000,maxph=10))


fit_zinb34 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tLarvaire+tAdulte:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=2000,maxfn=10000,imaxfn=5000,maxph=10))


fit_zinb35 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tAdulte+OLS:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb36 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+tAdulte:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb37 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tLarvaire+Rank:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond,admb.opts=admbControl(shess=FALSE,noinit=FALSE))
fit_zinb38 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tAdulte+OLS:tLarvaire+tAdulte:tLarvaire+OLS:tAdulte:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb39 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)

fit_zinb40 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tAdulte+OLS:tLarvaire+Rank:tLarvaire+tAdulte:tLarvaire+OLS:tAdulte:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=2000,maxfn=10000,imaxfn=5000,maxph=10))

fit_zinb41 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tAdulte+OLS:tLarvaire+Rank:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond,admb.opts=admbControl(shess=FALSE,noinit=FALSE))
fit_zinb42 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:tAdulte+Rank:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=400,maxfn=2000,imaxfn=1000,maxph=10))
fit_zinb43 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:tAdulte+tAdulte:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb44 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+tAdulte:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb45 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tLarvaire+Rank:tLarvaire+OLS:Rank:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)

fit_zinb46 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+Rank:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=2000,maxfn=10000,imaxfn=5000,maxph=10))

fit_zinb47 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tAdulte+OLS:tLarvaire+Rank:tLarvaire+tAdulte:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=2000,maxfn=10000,imaxfn=5000,maxph=10))

fit_zinb48 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tLarvaire+Rank:tLarvaire+tAdulte:tLarvaire+OLS:Rank:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=2000,maxfn=10000,imaxfn=5000,maxph=10))

fit_zinb49 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tAdulte+OLS:tLarvaire+Rank:tLarvaire+tAdulte:tLarvaire+OLS:Rank:tLarvaire+OLS:tAdulte:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb50 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+Rank:tLarvaire+tAdulte:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)

fit_zinb51 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tAdulte+OLS:tLarvaire+Rank:tLarvaire+OLS:Rank:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=2000,maxfn=10000,imaxfn=5000,maxph=10))

fit_zinb52 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tAdulte+Rank:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=2000,maxfn=10000,imaxfn=5000,maxph=10))

fit_zinb53 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tAdulte+OLS:tLarvaire+Rank:tLarvaire+tAdulte:tLarvaire+OLS:Rank:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=2000,maxfn=10000,imaxfn=5000,maxph=10))

fit_zinb54 <- glmmadmb(eggs~OLS+Rank+tAdulte+tLarvaire+OLS:Rank+OLS:tAdulte+Rank:tLarvaire+tAdulte:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=2000,maxfn=10000,imaxfn=5000,maxph=10))


fit_zinb55 <- glmmadmb(eggs~Rank+tAdulte+tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=2000,maxfn=10000,imaxfn=5000,maxph=10))

fit_zinb56 <- glmmadmb(eggs~Rank+tAdulte+tLarvaire+Rank:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb57 <- glmmadmb(eggs~Rank+tAdulte+tLarvaire+Rank:tLarvaire+tAdulte:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb58 <- glmmadmb(eggs~OLS+Rank+tLarvaire+OLS:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb59 <- glmmadmb(eggs~OLS+Rank+tLarvaire+Rank:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb60 <- glmmadmb(eggs~OLS+Rank+tLarvaire+OLS:tLarvaire+Rank:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb61 <- glmmadmb(eggs~OLS+Rank+tLarvaire+OLS:Rank+OLS:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)
fit_zinb62 <- glmmadmb(eggs~OLS+Rank+tLarvaire+OLS:Rank+Rank:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)

fit_zinb63 <- glmmadmb(eggs~OLS+Rank+tLarvaire+OLS:Rank+OLS:tLarvaire+Rank:tLarvaire+OLS:Rank:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=2000,maxfn=10000,imaxfn=5000,maxph=10))

fit_zinb64 <- glmmadmb(eggs~Rank+tLarvaire+Rank:tLarvaire+(1|individu),zeroInflation=TRUE,family="nbinom1",data=Fecond)



(logLik1<-logLik(fit_zinb1)[1])
(logLik2<-logLik(fit_zinb2)[1])
(logLik3<-logLik(fit_zinb3)[1])
(logLik4<-logLik(fit_zinb4)[1])
(logLik5<-logLik(fit_zinb5)[1])
(logLik6<-logLik(fit_zinb6)[1])
(logLik7<-logLik(fit_zinb7)[1])
(logLik8<-logLik(fit_zinb8)[1])
(logLik9<-logLik(fit_zinb9)[1])
(logLik10<-logLik(fit_zinb10)[1])
(logLik11<-logLik(fit_zinb11)[1])
(logLik12<-logLik(fit_zinb12)[1])
(logLik13<-logLik(fit_zinb13)[1])
(logLik14<-logLik(fit_zinb14)[1])
(logLik15<-logLik(fit_zinb15)[1])
(logLik16<-logLik(fit_zinb16)[1])
(logLik17<-logLik(fit_zinb17)[1])
(logLik18<-logLik(fit_zinb18)[1])
(logLik19<-logLik(fit_zinb19)[1])
(logLik20<-logLik(fit_zinb20)[1])
(logLik21<-logLik(fit_zinb21)[1])
(logLik22<-logLik(fit_zinb22)[1])
(logLik23<-logLik(fit_zinb23)[1])
(logLik24<-logLik(fit_zinb24)[1])
(logLik25<-logLik(fit_zinb25)[1])
(logLik26<-logLik(fit_zinb26)[1])
(logLik27<-logLik(fit_zinb27)[1])
(logLik28<-logLik(fit_zinb28)[1])
(logLik29<-logLik(fit_zinb29)[1])
(logLik30<-logLik(fit_zinb30)[1])
(logLik31<-logLik(fit_zinb31)[1])

(logLik32<-logLik(fit_zinb32)[1])#


(logLik33<-logLik(fit_zinb33)[1])#

(logLik34<-logLik(fit_zinb34)[1])#

(logLik35<-logLik(fit_zinb35)[1])
(logLik36<-logLik(fit_zinb36)[1])
(logLik37<-logLik(fit_zinb37)[1])
(logLik38<-logLik(fit_zinb38)[1])
(logLik39<-logLik(fit_zinb39)[1])

(logLik40<-logLik(fit_zinb40)[1])#

(logLik41<-logLik(fit_zinb41)[1])
(logLik42<-logLik(fit_zinb42)[1])
(logLik43<-logLik(fit_zinb43)[1])
(logLik44<-logLik(fit_zinb44)[1])
(logLik45<-logLik(fit_zinb45)[1])

(logLik46<-logLik(fit_zinb46)[1])#

(logLik47<-logLik(fit_zinb47)[1])#

(logLik48<-logLik(fit_zinb48)[1])#

(logLik49<-logLik(fit_zinb49)[1])
(logLik50<-logLik(fit_zinb50)[1])

(logLik51<-logLik(fit_zinb51)[1])#

(logLik52<-logLik(fit_zinb52)[1])#

(logLik53<-logLik(fit_zinb53)[1])#

(logLik54<-logLik(fit_zinb54)[1])#

(logLik55<-logLik(fit_zinb55)[1])#

(logLik56<-logLik(fit_zinb56)[1])
(logLik57<-logLik(fit_zinb57)[1])
(logLik58<-logLik(fit_zinb58)[1])
(logLik59<-logLik(fit_zinb59)[1])
(logLik60<-logLik(fit_zinb60)[1])
(logLik61<-logLik(fit_zinb61)[1])
(logLik62<-logLik(fit_zinb62)[1])

(logLik63<-logLik(fit_zinb63)[1])#

(logLik64<-logLik(fit_zinb64)[1])

logLik<-c(logLik1,logLik2,logLik3,logLik4,logLik5,logLik6,logLik7,logLik8,logLik9,logLik10,logLik11,logLik12,logLik13,logLik14,logLik15,logLik16,logLik17,logLik18,logLik19,logLik20,logLik21,logLik22,logLik23,logLik24,logLik25,logLik26,logLik27,logLik28,logLik29,logLik30,logLik31,logLik32,logLik33,logLik34,logLik35,logLik36,logLik37,logLik38,logLik39,logLik40,logLik41,logLik42,logLik43,logLik44,logLik45,logLik46,logLik47,logLik48,logLik49,logLik50,logLik51,logLik52,logLik53,logLik54,logLik55,logLik56,logLik57,logLik58,logLik59,logLik60,logLik61,logLik62,logLik63,logLik64)

logLik




(df1<-fit_zinb1$npar)
(df2<-fit_zinb2$npar)
(df3<-fit_zinb3$npar)
(df4<-fit_zinb4$npar)
(df5<-fit_zinb5$npar)
(df6<-fit_zinb6$npar)
(df7<-fit_zinb7$npar)
(df8<-fit_zinb8$npar)
(df9<-fit_zinb9$npar)
(df10<-fit_zinb10$npar)
(df11<-fit_zinb11$npar)
(df12<-fit_zinb12$npar)
(df13<-fit_zinb13$npar)
(df14<-fit_zinb14$npar)
(df15<-fit_zinb15$npar)
(df16<-fit_zinb16$npar)
(df17<-fit_zinb17$npar)
(df18<-fit_zinb18$npar)
(df19<-fit_zinb19$npar)
(df20<-fit_zinb20$npar)
(df21<-fit_zinb21$npar)
(df22<-fit_zinb22$npar)
(df23<-fit_zinb23$npar)
(df24<-fit_zinb24$npar)
(df25<-fit_zinb25$npar)
(df26<-fit_zinb26$npar)
(df27<-fit_zinb27$npar)
(df28<-fit_zinb28$npar)
(df29<-fit_zinb29$npar)
(df30<-fit_zinb30$npar)
(df31<-fit_zinb31$npar)

(df32<-fit_zinb32$npar)#


(df33<-fit_zinb33$npar)#

(df34<-fit_zinb34$npar)#

(df35<-fit_zinb35$npar)
(df36<-fit_zinb36$npar)
(df37<-fit_zinb37$npar)
(df38<-fit_zinb38$npar)
(df39<-fit_zinb39$npar)

(df40<-fit_zinb40$npar)

(df41<-fit_zinb41$npar)
(df42<-fit_zinb42$npar)
(df43<-fit_zinb43$npar)
(df44<-fit_zinb44$npar)
(df45<-fit_zinb45$npar)

(df46<-fit_zinb46$npar)#

(df47<-fit_zinb47$npar)#

(df48<-fit_zinb48$npar)#

(df49<-fit_zinb49$npar)
(df50<-fit_zinb50$npar)

(df51<-fit_zinb51$npar)#

(df52<-fit_zinb52$npar)#

(df53<-fit_zinb53$npar)#

(df54<-fit_zinb54$npar)#

(df55<-fit_zinb55$npar)#

(df56<-fit_zinb56$npar)
(df57<-fit_zinb57$npar)
(df58<-fit_zinb58$npar)
(df59<-fit_zinb59$npar)
(df60<-fit_zinb60$npar)
(df61<-fit_zinb61$npar)
(df62<-fit_zinb62$npar)

(df63<-fit_zinb63$npar)#

(df64<-fit_zinb64$npar)




df<-c(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15,df16,df17,df18,df19,df20,df21,df22,df23,df24,df25,df26,df27,df28,df29,df30,df31,df32,df33,df34,df35,df36,df37,df38,df39,df40,df41,df42,df43,df44,df45,df46,df47,df48,df49,df50,df51,df52,df53,df54,df55,df56,df57,df58,df59,df60,df61,df62,df63,df64)

df





(AICc1<-AICc(fit_zinb1))
(AICc2<-AICc(fit_zinb2))
(AICc3<-AICc(fit_zinb3))
(AICc4<-AICc(fit_zinb4))
(AICc5<-AICc(fit_zinb5))
(AICc6<-AICc(fit_zinb6))
(AICc7<-AICc(fit_zinb7))
(AICc8<-AICc(fit_zinb8))
(AICc9<-AICc(fit_zinb9))
(AICc10<-AICc(fit_zinb10))
(AICc11<-AICc(fit_zinb11))
(AICc12<-AICc(fit_zinb12))
(AICc13<-AICc(fit_zinb13))
(AICc14<-AICc(fit_zinb14))
(AICc15<-AICc(fit_zinb15))
(AICc16<-AICc(fit_zinb16))
(AICc17<-AICc(fit_zinb17))
(AICc18<-AICc(fit_zinb18))
(AICc19<-AICc(fit_zinb19))
(AICc20<-AICc(fit_zinb20))
(AICc21<-AICc(fit_zinb21))
(AICc22<-AICc(fit_zinb22))
(AICc23<-AICc(fit_zinb23))
(AICc24<-AICc(fit_zinb24))
(AICc25<-AICc(fit_zinb25))
(AICc26<-AICc(fit_zinb26))
(AICc27<-AICc(fit_zinb27))
(AICc28<-AICc(fit_zinb28))
(AICc29<-AICc(fit_zinb29))
(AICc30<-AICc(fit_zinb30))
(AICc31<-AICc(fit_zinb31))
(AICc32<-AICc(fit_zinb32))
(AICc33<-AICc(fit_zinb33))
(AICc34<-AICc(fit_zinb34))
(AICc35<-AICc(fit_zinb35))
(AICc36<-AICc(fit_zinb36))
(AICc37<-AICc(fit_zinb37))
(AICc38<-AICc(fit_zinb38))
(AICc39<-AICc(fit_zinb39))
(AICc40<-AICc(fit_zinb40))
(AICc41<-AICc(fit_zinb41))
(AICc42<-AICc(fit_zinb42))
(AICc43<-AICc(fit_zinb43))
(AICc44<-AICc(fit_zinb44))
(AICc45<-AICc(fit_zinb45))
(AICc46<-AICc(fit_zinb46))
(AICc47<-AICc(fit_zinb47))
(AICc48<-AICc(fit_zinb48))
(AICc49<-AICc(fit_zinb49))
(AICc50<-AICc(fit_zinb50))
(AICc51<-AICc(fit_zinb51))
(AICc52<-AICc(fit_zinb52))
(AICc53<-AICc(fit_zinb53))
(AICc54<-AICc(fit_zinb54))
(AICc55<-AICc(fit_zinb55))
(AICc56<-AICc(fit_zinb56))
(AICc57<-AICc(fit_zinb57))
(AICc58<-AICc(fit_zinb58))
(AICc59<-AICc(fit_zinb59))
(AICc60<-AICc(fit_zinb60))
(AICc61<-AICc(fit_zinb61))
(AICc62<-AICc(fit_zinb62))
(AICc63<-AICc(fit_zinb63))
(AICc64<-AICc(fit_zinb64))

AICc<-c(AICc1,AICc2,AICc3,AICc4,AICc5,AICc6,AICc7,AICc8,AICc9,AICc10,AICc11,AICc12,AICc13,AICc14,AICc15,AICc16,AICc17,AICc18,AICc19,AICc20,AICc21,AICc22,AICc23,AICc24,AICc25,AICc26,AICc27,AICc28,AICc29,AICc30,AICc31,AICc32,AICc33,AICc34,AICc35,AICc36,AICc37,AICc38,AICc39,AICc40,AICc41,AICc42,AICc43,AICc44,AICc45,AICc46,AICc47,AICc48,AICc49,AICc50,AICc51,AICc52,AICc53,AICc54,AICc55,AICc56,AICc57,AICc58,AICc59,AICc60,AICc61,AICc62,AICc63,AICc64)

min(AICc)



head(ms1,20)









str(fit_zinb1)

(Fix1<-fit_zinb1$formula )
(Fix2<-fit_zinb2$formula )
(Fix3<-fit_zinb3$formula )
(Fix4<-fit_zinb4$formula )
(Fix5<-fit_zinb5$formula )
(Fix6<-fit_zinb6$formula )
(Fix7<-fit_zinb7$formula )
(Fix8<-fit_zinb8$formula )
(Fix9<-fit_zinb9$formula )
(Fix10<-fit_zinb10$formula )
(Fix11<-fit_zinb11$formula )
(Fix12<-fit_zinb12$formula )
(Fix13<-fit_zinb13$formula )
(Fix14<-fit_zinb14$formula )
(Fix15<-fit_zinb15$formula )
(Fix16<-fit_zinb16$formula )
(Fix17<-fit_zinb17$formula )
(Fix18<-fit_zinb18$formula )
(Fix19<-fit_zinb19$formula )
(Fix20<-fit_zinb20$formula )
(Fix21<-fit_zinb21$formula )
(Fix22<-fit_zinb22$formula )
(Fix23<-fit_zinb23$formula )
(Fix24<-fit_zinb24$formula )
(Fix25<-fit_zinb25$formula )
(Fix26<-fit_zinb26$formula )
(Fix27<-fit_zinb27$formula )
(Fix28<-fit_zinb28$formula )
(Fix29<-fit_zinb29$formula )
(Fix30<-fit_zinb30$formula )
(Fix31<-fit_zinb31$formula )

(Fix32<-fit_zinb32$formula )#


(Fix33<-fit_zinb33$formula )#

(Fix34<-fit_zinb34$formula )#

(Fix35<-fit_zinb35$formula )
(Fix36<-fit_zinb36$formula )
(Fix37<-fit_zinb37$formula )
(Fix38<-fit_zinb38$formula )
(Fix39<-fit_zinb39$formula )

(Fix40<-fit_zinb40$formula )

(Fix41<-fit_zinb41$formula )
(Fix42<-fit_zinb42$formula )
(Fix43<-fit_zinb43$formula )
(Fix44<-fit_zinb44$formula )
(Fix45<-fit_zinb45$formula )

(Fix46<-fit_zinb46$formula )#

(Fix47<-fit_zinb47$formula )#

(Fix48<-fit_zinb48$formula )#

(Fix49<-fit_zinb49$formula )
(Fix50<-fit_zinb50$formula )

(Fix51<-fit_zinb51$formula )#

(Fix52<-fit_zinb52$formula )#

(Fix53<-fit_zinb53$formula )#

(Fix54<-fit_zinb54$formula )#

(Fix55<-fit_zinb55$formula )#

(Fix56<-fit_zinb56$formula )
(Fix57<-fit_zinb57$formula )
(Fix58<-fit_zinb58$formula )
(Fix59<-fit_zinb59$formula )
(Fix60<-fit_zinb60$formula )
(Fix61<-fit_zinb61$formula )
(Fix62<-fit_zinb62$formula )

(Fix63<-fit_zinb63$formula )#

(Fix64<-fit_zinb64$formula )



Fix<- c(Fix1,Fix2,Fix3,Fix4,Fix5,Fix6,Fix7,Fix8,Fix9,Fix10,Fix11,Fix12,Fix13,Fix14,Fix15,Fix16,Fix17,Fix18,Fix19,Fix20,Fix21,Fix22,Fix23,Fix24,Fix25,Fix26,Fix27,Fix28,Fix29,Fix30,Fix31,Fix32,Fix33,Fix34,Fix35,Fix36,Fix37,Fix38,Fix39,Fix40,Fix41,Fix42,Fix43,Fix44,Fix45,Fix46,Fix47,Fix48,Fix49,Fix50,Fix51,Fix52,Fix53,Fix54,Fix55,Fix56,Fix57,Fix58,Fix59,Fix60,Fix61,Fix62,Fix63,Fix64)
Fix



Fix<-vapply(Fix, paste, collapse = ", ", character(1L))




Table<-cbind(Fix,df,logLik,AICc)
Table

write.table(Table,"/home/mark/Dropbox/condition/ANALYSE/FinalAnalysis/TableAICcZI.txt",sep="\t",row.names=F,quote=F) 






head(Fecond)

fit_zinb <- glmmadmb(eggs~tAdulte*tLarvaire*Rank*poids+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=200,maxfn=1000,imaxfn=500,maxph=5))
summary(fit_zinb)


fit_zinb <- glmmadmb(eggs~tAdulte*tLarvaire*Rank*poids+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=2000,maxfn=10000,imaxfn=5000,maxph=10))

ms1 <- dredge(fit_zinb,rank="AICc")
ms1


write.table(ms1,"/home/mark/Dropbox/condition/ANALYSE/FinalAnalysis/TableAICcZIPoids.txt",sep="\t",row.names=F,quote=F) 


AICc(fit_zinb)


summary(fit_zinb)







fit_zinb_notMixed <- glm.nb(eggs~tAdulte*tLarvaire*Rank*poids,data=Fecond,na.action = na.fail)

ms1NM <- dredge(fit_zinb_notMixed,rank="AICc")
ms1NM


write.table(ms1NM,"/home/mark/Dropbox/condition/ANALYSE/FinalAnalysis/TableAICcZIPoidsNotMixed.txt",sep="\t",row.names=F,quote=F) 






#poids,Rank,tAdulte,tLarvaire,Rank:tAdulte,


fit_zinb1 <- glmmadmb(eggs~ poids+Rank+tAdulte+tLarvaire+Rank:tAdulte+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond)


#poids,Rank,tAdulte,tLarvaire,poids:tLarvaire,Rank:tAdulte,


fit_zinb2 <- glmmadmb(eggs~ poids+Rank+tAdulte+tLarvaire+poids:tLarvaire+Rank:tAdulte+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond)


#poids,Rank,tAdulte,tLarvaire,poids:Rank,Rank:tAdulte,


fit_zinb3 <- glmmadmb(eggs~poids+Rank+tAdulte+tLarvaire+poids:Rank+Rank:tAdulte+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=200,maxfn=1000,imaxfn=500,maxph=5))


#poids,Rank,tAdulte,tLarvaire,poids:tLarvaire,Rank:tAdulte,Rank:tLarvaire,

fit_zinb4 <- glmmadmb(eggs~poids+Rank+tAdulte+tLarvaire+poids:tLarvaire+Rank:tAdulte+Rank:tLarvaire+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond)


#poids,Rank,tAdulte,tLarvaire,poids:Rank,Rank:tAdulte,Rank:tLarvaire,


fit_zinb5 <- glmmadmb(eggs~poids+Rank+tAdulte+tLarvaire+poids:Rank+Rank:tAdulte+Rank:tLarvaire+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond)



#poids,Rank,tAdulte,tLarvaire,poids:tAdulte,Rank:tAdulte,Rank:tLarvaire,

fit_zinb6 <- glmmadmb(eggs~poids+Rank+tAdulte+tLarvaire+poids:tAdulte+Rank:tAdulte+Rank:tLarvaire+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond)




#poids,Rank,tAdulte,tLarvaire,poids:Rank,poids:tLarvaire,Rank:tAdulte,

fit_zinb7 <- glmmadmb(eggs~poids+Rank+tAdulte+tLarvaire+poids:Rank+poids:tLarvaire+Rank:tAdulte+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond)


#poids,Rank,tAdulte,tLarvaire,poids:tAdulte,poids:tLarvaire,Rank:tAdulte,

fit_zinb8 <- glmmadmb(eggs~poids+Rank+tAdulte+tLarvaire+poids:tAdulte+poids:tLarvaire+Rank:tAdulte+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond)


#poids,Rank,tAdulte,tLarvaire,poids:Rank,poids:tLarvaire,Rank:tAdulte,Rank:tLarvaire,

fit_zinb9 <- glmmadmb(eggs~poids+Rank+tAdulte+tLarvaire+poids:Rank+poids:tLarvaire+Rank:tAdulte+Rank:tLarvaire+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond)



#poids,Rank,tAdulte,tLarvaire,poids:Rank,poids:tAdulte,poids:tLarvaire,Rank:tAdulte,Rank:tLarvaire,tAdulte:tLarvaire,poids:tAdulte:tLarvaire,

fit_zinb10 <- glmmadmb(eggs~poids+Rank+tAdulte+tLarvaire+poids:Rank+poids:tAdulte+poids:tLarvaire+Rank:tAdulte+Rank:tLarvaire+tAdulte:tLarvaire+poids:tAdulte:tLarvaire+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond)


#poids,Rank,tAdulte,tLarvaire,poids:Rank,poids:tAdulte,poids:tLarvaire,Rank:tAdulte,Rank:tLarvaire,tAdulte:tLarvaire,

fit_zinb11 <- glmmadmb(eggs~poids+Rank+tAdulte+tLarvaire+poids:Rank+poids:tAdulte+poids:tLarvaire+Rank:tAdulte+Rank:tLarvaire+tAdulte:tLarvaire+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond)



#poids,Rank,tAdulte,tLarvaire,poids:Rank,poids:tAdulte,poids:tLarvaire,Rank:tAdulte,Rank:tLarvaire,poids:Rank:tAdulte,

fit_zinb12 <- glmmadmb(eggs~poids+Rank+tAdulte+tLarvaire+poids:Rank+poids:tAdulte+poids:tLarvaire+Rank:tAdulte+Rank:tLarvaire+poids:Rank:tAdulte+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond)


#poids,Rank,tAdulte,poids:Rank,poids:tAdulte,								

fit_zinb13 <- glmmadmb(eggs~poids+Rank+tAdulte+poids:Rank+poids:tAdulte+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond)





(logLik1<-logLik(fit_zinb1)[1])
(logLik2<-logLik(fit_zinb2)[1])
(logLik3<-logLik(fit_zinb3)[1])
(logLik4<-logLik(fit_zinb4)[1])
(logLik5<-logLik(fit_zinb5)[1])
(logLik6<-logLik(fit_zinb6)[1])
(logLik7<-logLik(fit_zinb7)[1])
(logLik8<-logLik(fit_zinb8)[1])
(logLik9<-logLik(fit_zinb9)[1])
(logLik10<-logLik(fit_zinb10)[1])
(logLik11<-logLik(fit_zinb11)[1])
(logLik12<-logLik(fit_zinb12)[1])
(logLik13<-logLik(fit_zinb13)[1])


(df1<-fit_zinb1$npar)
(df2<-fit_zinb2$npar)
(df3<-fit_zinb3$npar)
(df4<-fit_zinb4$npar)
(df5<-fit_zinb5$npar)
(df6<-fit_zinb6$npar)
(df7<-fit_zinb7$npar)
(df8<-fit_zinb8$npar)
(df9<-fit_zinb9$npar)
(df10<-fit_zinb10$npar)
(df11<-fit_zinb11$npar)
(df12<-fit_zinb12$npar)
(df13<-fit_zinb13$npar)







logLik<-c(logLik1,logLik2,logLik3,logLik4,logLik5,logLik6,logLik7,logLik8,logLik9,logLik10,logLik11,logLik12,logLik13)

logLik




df<-c(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13)

df





(AICc1<-AICc(fit_zinb1))
(AICc2<-AICc(fit_zinb2))
(AICc3<-AICc(fit_zinb3))
(AICc4<-AICc(fit_zinb4))
(AICc5<-AICc(fit_zinb5))
(AICc6<-AICc(fit_zinb6))
(AICc7<-AICc(fit_zinb7))
(AICc8<-AICc(fit_zinb8))
(AICc9<-AICc(fit_zinb9))
(AICc10<-AICc(fit_zinb10))
(AICc11<-AICc(fit_zinb11))
(AICc12<-AICc(fit_zinb12))
(AICc13<-AICc(fit_zinb13))



AICc<-c(AICc1,AICc2,AICc3,AICc4,AICc5,AICc6,AICc7,AICc8,AICc9,AICc10,AICc11,AICc12,AICc13)

min(AICc)


#str(fit_zinb1)

(Fix1<-fit_zinb1$formula)
(Fix2<-fit_zinb2$formula)
(Fix3<-fit_zinb3$formula)
(Fix4<-fit_zinb4$formula)
(Fix5<-fit_zinb5$formula)
(Fix6<-fit_zinb6$formula)
(Fix7<-fit_zinb7$formula)
(Fix8<-fit_zinb8$formula)
(Fix9<-fit_zinb9$formula)
(Fix10<-fit_zinb10$formula)
(Fix11<-fit_zinb11$formula)
(Fix12<-fit_zinb12$formula)
(Fix13<-fit_zinb13$formula)


Fix<- c(Fix1,Fix2,Fix3,Fix4,Fix5,Fix6,Fix7,Fix8,Fix9,Fix10,Fix11,Fix12,Fix13)
Fix



Fix<-vapply(Fix, paste, collapse = ", ", character(1L))




Table2<-cbind(Fix,df,logLik,AICc)
Table2

write.table(Table2,"/home/mark/Dropbox/condition/ANALYSE/FinalAnalysis/extraTableAICcZIPoids.txt",sep="\t",row.names=F,quote=F) 







fit_zinb <- glmmadmb(eggs~tAdulte*tLarvaire*Rank*MSL+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=2000,maxfn=10000,imaxfn=5000,maxph=10))

ms1 <- dredge(fit_zinb,rank="AICc")
ms1




write.table(ms1,"/home/mark/Dropbox/condition/ANALYSE/FinalAnalysis/TableAICcZISMI.txt",sep="\t",row.names=F,quote=F) 


fit_zinb1 <- glmmadmb(eggs~MSL+Rank+tAdulte+MSL:Rank+MSL:tAdulte+Rank:tAdulte+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond)

fit_zinb2 <- glmmadmb(eggs~MSL+Rank+tAdulte+tLarvaire+MSL:tAdulte+Rank:tAdulte+tAdulte:tLarvaire+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond)

fit_zinb3 <- glmmadmb(eggs~MSL+Rank+tAdulte+tLarvaire+MSL:tLarvaire+Rank:tAdulte+Rank:tLarvaire+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond)

fit_zinb4 <- glmmadmb(eggs~MSL+Rank+tAdulte+tLarvaire+MSL:Rank+Rank:tAdulte+Rank:tLarvaire+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond)

fit_zinb5 <- glmmadmb(eggs~MSL+Rank+tAdulte+tLarvaire+MSL:tAdulte+MSL:tLarvaire+Rank:tAdulte+Rank:tLarvaire+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond)

fit_zinb6 <- glmmadmb(eggs~MSL+Rank+tAdulte+tLarvaire+MSL:tAdulte+MSL:tLarvaire+Rank:tAdulte+Rank:tLarvaire+tAdulte:tLarvaire+MSL:tAdulte:tLarvaire+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond)

fit_zinb7 <- glmmadmb(eggs~MSL+Rank+tAdulte+tLarvaire+MSL:Rank+MSL:tAdulte+Rank:tAdulte+Rank:tLarvaire+MSL:Rank:tAdulte+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond)

fit_zinb8 <- glmmadmb(eggs~MSL+Rank+tAdulte+tLarvaire+MSL:Rank+MSL:tAdulte+MSL:tLarvaire+Rank:tAdulte+MSL:Rank:tAdulte+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond)

fit_zinb9 <- glmmadmb(eggs~MSL+Rank+tAdulte+tLarvaire+MSL:Rank+MSL:tAdulte+MSL:tLarvaire+Rank:tAdulte+Rank:tLarvaire+tAdulte:tLarvaire+MSL:tAdulte:tLarvaire+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond)

fit_zinb10 <- glmmadmb(eggs~MSL+Rank+tAdulte+tLarvaire+MSL:Rank+MSL:tAdulte+MSL:tLarvaire+Rank:tAdulte+Rank:tLarvaire+MSL:Rank:tAdulte+MSL:Rank:tLarvaire+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond)

fit_zinb11 <- glmmadmb(eggs~MSL+Rank+tAdulte+tLarvaire+MSL:Rank+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=200,maxfn=1000,imaxfn=500,maxph=5))

fit_zinb1
fit_zinb2
fit_zinb3
fit_zinb4
fit_zinb5
fit_zinb6
fit_zinb7
fit_zinb8
fit_zinb9
fit_zinb10
fit_zinb11




(logLik1<-logLik(fit_zinb1)[1])
(logLik2<-logLik(fit_zinb2)[1])
(logLik3<-logLik(fit_zinb3)[1])
(logLik4<-logLik(fit_zinb4)[1])
(logLik5<-logLik(fit_zinb5)[1])
(logLik6<-logLik(fit_zinb6)[1])
(logLik7<-logLik(fit_zinb7)[1])
(logLik8<-logLik(fit_zinb8)[1])
(logLik9<-logLik(fit_zinb9)[1])
(logLik10<-logLik(fit_zinb10)[1])
(logLik11<-logLik(fit_zinb11)[1])



(df1<-fit_zinb1$npar)
(df2<-fit_zinb2$npar)
(df3<-fit_zinb3$npar)
(df4<-fit_zinb4$npar)
(df5<-fit_zinb5$npar)
(df6<-fit_zinb6$npar)
(df7<-fit_zinb7$npar)
(df8<-fit_zinb8$npar)
(df9<-fit_zinb9$npar)
(df10<-fit_zinb10$npar)
(df11<-fit_zinb11$npar)



logLik<-c(logLik1,logLik2,logLik3,logLik4,logLik5,logLik6,logLik7,logLik8,logLik9,logLik10,logLik11)

logLik




df<-c(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11)

df


(AICc1<-AICc(fit_zinb1))
(AICc2<-AICc(fit_zinb2))
(AICc3<-AICc(fit_zinb3))
(AICc4<-AICc(fit_zinb4))
(AICc5<-AICc(fit_zinb5))
(AICc6<-AICc(fit_zinb6))
(AICc7<-AICc(fit_zinb7))
(AICc8<-AICc(fit_zinb8))
(AICc9<-AICc(fit_zinb9))
(AICc10<-AICc(fit_zinb10))
(AICc11<-AICc(fit_zinb11))


AICc<-c(AICc1,AICc2,AICc3,AICc4,AICc5,AICc6,AICc7,AICc8,AICc9,AICc10,AICc11)

min(AICc)


#str(fit_zinb1)

(Fix1<-fit_zinb1$formula)
(Fix2<-fit_zinb2$formula)
(Fix3<-fit_zinb3$formula)
(Fix4<-fit_zinb4$formula)
(Fix5<-fit_zinb5$formula)
(Fix6<-fit_zinb6$formula)
(Fix7<-fit_zinb7$formula)
(Fix8<-fit_zinb8$formula)
(Fix9<-fit_zinb9$formula)
(Fix10<-fit_zinb10$formula)
(Fix11<-fit_zinb11$formula)


Fix<- c(Fix1,Fix2,Fix3,Fix4,Fix5,Fix6,Fix7,Fix8,Fix9,Fix10,Fix11)
Fix



Fix<-vapply(Fix, paste, collapse = ", ", character(1L))




Table2<-cbind(Fix,df,logLik,AICc)
Table2

write.table(Table2,"/home/mark/Dropbox/condition/ANALYSE/FinalAnalysis/extraTableAICcZIPSMI.txt",sep="\t",row.names=F,quote=F) 







library(snow)
clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
clust <- try(makeCluster(getOption("cl.cores", 8), type = clusterType))




print(system.time(pdd <- pdredge(fit_zinb, cluster = clust)))




summary(fit_zinb)

#fit_zinb <-glmmadmb(eggs ~ milieu + OLS + Rank + (1 | individu), family = "nbinom1", zeroInflation = TRUE,data=Fecond,debug=TRUE,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=200,maxfn=1000,imaxfn=500,maxph=5))

fit_zinb <- glmmadmb(eggs~Rank*tLarvaire*OLS+Rank*tAdulte*OLS+OLS*Rank+tLarvaire*tAdulte*Rank+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=200,maxfn=1000,imaxfn=500,maxph=5))





dput(round(coef(fit_zinb),3))
#c(`(Intercept)` = 3.156, tLarvairePNl = -0.179, tAdultePNa = -0.068, OLS = 0.597, Rank = -0.096, `tLarvairePNl:tAdultePNa` = 0.456, `tLarvairePNl:OLS` = 0.552, `tAdultePNa:OLS` = 1.481, `tLarvairePNl:Rank` = 0.052, `tAdultePNa:Rank` = -0.086, `OLS:Rank` = 0.01, `tLarvairePNl:tAdultePNa:OLS` = -0.51, `tLarvairePNl:tAdultePNa:Rank` = -0.112, `tLarvairePNl:OLS:Rank` = -0.085, `tAdultePNa:OLS:Rank` = 0.172, `tLarvairePNl:tAdultePNa:OLS:Rank` = -0.677)

library(car)
Anova(fit_zinb,type="III")

ms1 <- dredge(fit_zinb)
ms1
model.avg(ms1)
confset.95p <- get.models(ms1, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)


step(fit_zinb)
drop(fit_zinb)
drop1(fit_zinb)


Anova(fit_zinb,type="III")
summary(fit_zinb)

ms1 <- dredge(fit_zinb)
ms1
model.avg(ms1)
confset.95p <- get.models(ms1, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

write.table(ms1, "~/Dropbox/condition/ANALYSE/ms1New.txt", sep="\t")
ms1

Summary<-summary(avgmod.95p)
write.table(Summary, "~/Dropbox/condition/ANALYSE/Summary.txt", sep="\t")

Conf<-confint(avgmod.95p)
write.table(Conf, "~/Dropbox/condition/ANALYSE/CI.txt", sep="\t")



testDredge <- glm.nb(eggs~Rank*tLarvaire*OLS+Rank*tAdulte*OLS+OLS*Rank+tLarvaire*tAdulte*Rank,data=Fecond,na.action=na.fail)



test1 <- dredge(testDredge)
test1
model.avg(test1)
confset.95p <- get.models(test1, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

write.table(test1, "~/Dropbox/condition/ANALYSE/test1New.txt", sep="\t")
test1



plot(fit_zinb)


library(glmmADMB)
fit_zinb <- glmmadmb(eggs~tAdulte*Rank+OLS+(1|individu), zeroInflation=TRUE, family="nbinom1",data=Fecond)

library(visreg)
summary(fit_zinb)

library(coefplot2)
coefplot2(fit_zinb)

library(ggplot2)


Fecond$sOLS<-scale(Fecond$OLS)

FitOLS <- glmmadmb(formula = eggs~tAdulte*Rank+sOLS+(1|individu),zeroInflation=TRUE, family="nbinom1", data =Fecond)
confint(FitOLS)

newdat.lme = data.frame(tAdulte = Fecond$tAdulte,
                        Rank = Fecond$Rank,
                        sOLS = median(Fecond$sOLS))

head(newdat.lme)

newdat.lme$predlme = predict(FitOLS, newdata = newdat.lme, level = 0)

ggplot(Fecond, aes(x = Rank, y = log(eggs), color = tAdulte) ) +
     geom_rug(sides = "b", size = 1) +
     geom_line(data = newdat.lme, aes(y = predlme), size = 1)

des = model.matrix(formula("~tAdulte * Rank + sOLS"), newdat.lme)

predvar = diag( des %*% vcov(FitOLS) %*% t(des) )

head(newdat.lme)
newdat.lme$lower = with(newdat.lme, predlme - 2*sqrt(predvar) )
newdat.lme$upper = with(newdat.lme, predlme + 2*sqrt(predvar) )


range(newdat.lme$lower)
1.184363 2.870066
range(newdat.lme$upper)
2.211534 3.304036
mean(newdat.lme$lower)
2.29053
mean(newdat.lme$upper)
2.842194



png("/home/mark/Dropbox/condition/ANALYSE/FigureMixedZI.png",width=2100, height=2100,res=300)

ggplot(Fecond, aes(x = Rank, y = eggs, color = tAdulte) ) + geom_point(aes(x=Rank, y = eggs)) + geom_ribbon(data = newdat.lme, aes(y = NULL, ymin = exp(lower), ymax = exp(upper),color = NULL, fill = tAdulte), alpha = .15) + geom_line(data = newdat.lme, aes(y = exp(predlme)), size = .75) + theme_bw()+ggtitle("Mixed effect model: Zero-inflated negative binomial (controlling for OLSresid)")+scale_colour_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("Na","PNa"),labels=c("R.A.","P.A."))+scale_fill_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("Na","PNa"),labels=c("R.A.","P.A."))+ labs(x = "Reproductive event", y = "Number of eggs")

dev.off()


png("/home/mark/Dropbox/condition/ANALYSE/FigureMixedZIAlternatif.png",width=2100, height=2100,res=300)

ggplot(Fecond, aes(x = Rank, y = eggs, color = tAdulte) ) + geom_jitter(aes(x=Rank, y = eggs),width = 0.25) + geom_ribbon(data = newdat.lme, aes(y = NULL, ymin = exp(lower), ymax = exp(upper),color = NULL, fill = tAdulte), alpha = .15) + geom_line(data = newdat.lme, aes(y = exp(predlme)), size = .75) + theme_bw()+scale_colour_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("Na","PNa"),labels=c("Rich adult","Poor adult"))+scale_fill_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("Na","PNa"),labels=c("Rich adult","Poor adult"))+ labs(x = "Rank", y = "Number of eggs")

dev.off()


MixedOLS<-ggplot(Fecond, aes(x = Rank, y = eggs, color = tAdulte) ) + geom_jitter(aes(x=Rank, y = eggs),width = 0.25,size=4) + geom_ribbon(data = newdat.lme, aes(y = NULL, ymin = exp(lower), ymax = exp(upper),color = NULL, fill = tAdulte), alpha = .15) + geom_line(data = newdat.lme, aes(y = exp(predlme)), size = 2) + theme_bw()+scale_colour_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("Na","PNa"),labels=c("Rich adult","Poor adult"))+scale_fill_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("Na","PNa"),labels=c("Rich adult","Poor adult"))+ labs(x = "Rank", y = "Number of eggs")+theme(legend.position="bottom", legend.box = "horizontal",plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))

MixedOLS






newdat.lme = data.frame(tAdulte = Fecond$tAdulte,
                        Rank = median(Fecond$Rank),
                        sOLS = Fecond$sOLS)

head(newdat.lme)

newdat.lme$predlme = predict(FitOLS, newdata = newdat.lme, level = 0)

ggplot(Fecond, aes(x = sOLS, y = log(eggs), color = tAdulte) ) +
     geom_rug(sides = "b", size = 1) +
     geom_line(data = newdat.lme, aes(y = predlme), size = 1)

des = model.matrix(formula("~tAdulte * Rank + sOLS"), newdat.lme)

predvar = diag( des %*% vcov(FitOLS) %*% t(des) )

head(newdat.lme)
newdat.lme$lower = with(newdat.lme, predlme - 2*sqrt(predvar) )
newdat.lme$upper = with(newdat.lme, predlme + 2*sqrt(predvar) )



MOLS<-ggplot(Fecond, aes(x = sOLS, y = eggs, color = tAdulte) ) + geom_jitter(aes(x=sOLS, y = eggs),width = 0.25,size=4) + geom_ribbon(data = newdat.lme, aes(y = NULL, ymin = exp(lower), ymax = exp(upper),color = NULL, fill = tAdulte), alpha = .15) + geom_line(data = newdat.lme, aes(y = exp(predlme)), size = 2) + theme_bw()+ggtitle("b.")+scale_colour_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("Na","PNa"),labels=c("Rich adult","Poor adult"))+scale_fill_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("Na","PNa"),labels=c("Rich adult","Poor adult"))+ labs(x = "mean centered (OLSresid)", y = "Number of eggs")+ theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))


MOLS















Fecond$spoids<-scale(Fecond$poids)


Fitpoids <- glmmadmb(formula = eggs~tAdulte*Rank+spoids+(1|individu),zeroInflation=TRUE, family="nbinom1", data =Fecond)
confint(Fitpoids)

newdat.lme = data.frame(tAdulte = Fecond$tAdulte,
                        Rank = Fecond$Rank,
                        spoids = median(Fecond$spoids))

head(newdat.lme)

newdat.lme$predlme = predict(Fitpoids, newdata = newdat.lme, level = 0)

ggplot(Fecond, aes(x = Rank, y = log(eggs), color = tAdulte) ) +
     geom_rug(sides = "b", size = 1) +
     geom_line(data = newdat.lme, aes(y = predlme), size = 1)

des = model.matrix(formula("~tAdulte * Rank + spoids"), newdat.lme)

predvar = diag( des %*% vcov(Fitpoids) %*% t(des) )


newdat.lme$lower = with(newdat.lme, predlme - 2*sqrt(predvar) )
newdat.lme$upper = with(newdat.lme, predlme + 2*sqrt(predvar) )

range(newdat.lme$lower)
0.8681413 2.4821924
range(newdat.lme$upper)
2.504003 3.683785
mean(newdat.lme$lower)
1.923371
mean(newdat.lme$upper)
3.214168




MixedMass<-ggplot(Fecond, aes(x = Rank, y = eggs, color = tAdulte) ) + geom_jitter(aes(x=Rank, y = eggs),width = 0.25) + geom_ribbon(data = newdat.lme, aes(y = NULL, ymin = exp(lower), ymax = exp(upper),color = NULL, fill = tAdulte), alpha = .15) + geom_line(data = newdat.lme, aes(y = exp(predlme)), size = .75) + theme_bw()+ggtitle("Mixed effect model: Zero-inflated negative binomial (controlling for mass)")+scale_colour_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("Na","PNa"),labels=c("Rich adult","Poor adult"))+scale_fill_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("Na","PNa"),labels=c("Rich adult","Poor adult"))+ labs(x = "Rank", y = "Number of eggs")






newdat.lme = data.frame(tAdulte = Fecond$tAdulte,
                        Rank = median(Fecond$Rank),
                        spoids = Fecond$spoids)

head(newdat.lme)

newdat.lme$predlme = predict(Fitpoids, newdata = newdat.lme, level = 0)

ggplot(Fecond, aes(x = spoids, y = log(eggs), color = tAdulte) ) +
     geom_rug(sides = "b", size = 1) +
     geom_line(data = newdat.lme, aes(y = predlme), size = 1)

des = model.matrix(formula("~tAdulte * Rank + spoids"), newdat.lme)

predvar = diag( des %*% vcov(Fitpoids) %*% t(des) )

head(newdat.lme)
newdat.lme$lower = with(newdat.lme, predlme - 2*sqrt(predvar) )
newdat.lme$upper = with(newdat.lme, predlme + 2*sqrt(predvar) )



MPOIDS<-ggplot(Fecond, aes(x = spoids, y = eggs, color = tAdulte) ) + geom_jitter(aes(x=spoids, y = eggs),width = 0.25,size=4) + geom_ribbon(data = newdat.lme, aes(y = NULL, ymin = exp(lower), ymax = exp(upper),color = NULL, fill = tAdulte), alpha = .15) + geom_line(data = newdat.lme, aes(y = exp(predlme)), size = 2) + theme_bw()+ggtitle("c.")+scale_colour_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("Na","PNa"),labels=c("Rich adult","Poor adult"))+scale_fill_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("Na","PNa"),labels=c("Rich adult","Poor adult"))+ labs(x = "mean centered (mass)", y = "Number of eggs")+ theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
















library(car)
vif(FitOLS)
vif(FitMSL)
vif(FitMSL)
vif(Fit)


Fecond$sMSL<-scale(Fecond$MSL)
scale(Fecond$MSL)

FitMSLnotS <- glmmadmb(formula = eggs~tAdulte*Rank+MSL+(1|individu),zeroInflation=TRUE, family="nbinom1", data =Fecond)
summary(FitMSLnotS)

FitMSL <- glmmadmb(formula = eggs~tAdulte*Rank+sMSL+(1|individu),zeroInflation=TRUE, family="nbinom1", data =Fecond)
confint(FitMSL)
summary(FitMSL)

newdat.lme = data.frame(tAdulte = Fecond$tAdulte,
                        Rank = Fecond$Rank,
                        sMSL = median(Fecond$sMSL))

AICc(FitMSLnotS)
AICc(FitMSL)


head(newdat.lme)

newdat.lme$predlme = predict(FitMSL, newdata = newdat.lme, level = 0)

ggplot(Fecond, aes(x = Rank, y = log(eggs), color = tAdulte) ) +
     geom_rug(sides = "b", size = 1) +
     geom_line(data = newdat.lme, aes(y = predlme), size = 1)

des = model.matrix(formula("~tAdulte * Rank + sMSL"), newdat.lme)

predvar = diag( des %*% vcov(FitMSL) %*% t(des) )


newdat.lme$lower = with(newdat.lme, predlme - 2*sqrt(predvar) )
newdat.lme$upper = with(newdat.lme, predlme + 2*sqrt(predvar) )

range(newdat.lme$lower)
[1] 0.6244201 2.1797294
range(newdat.lme$upper)
[1] 2.774889 3.968927
mean(newdat.lme$lower)
[1] 1.641416
mean(newdat.lme$upper)
[1] 3.48873


MixedSMI<-ggplot(Fecond, aes(x = Rank, y = eggs, color = tAdulte) ) + geom_jitter(aes(x=Rank, y = eggs),width = 0.25,size=4) + geom_ribbon(data = newdat.lme, aes(y = NULL, ymin = exp(lower), ymax = exp(upper),color = NULL, fill = tAdulte), alpha = .15) + geom_line(data = newdat.lme, aes(y = exp(predlme)), size = 2) + theme_bw()+ggtitle("Mixed effect model: Zero-inflated negative binomial (controlling for SMI)")+scale_colour_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("Na","PNa"),labels=c("Rich adult","Poor adult"))+scale_fill_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("Na","PNa"),labels=c("Rich adult","Poor adult"))+ labs(x = "Rank", y = "Number of eggs")+ theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))


x11()







newdat.lme = data.frame(tAdulte = Fecond$tAdulte,
                        Rank = median(Fecond$Rank),
                        sMSL = Fecond$sMSL)

head(newdat.lme)

newdat.lme$predlme = predict(FitMSL, newdata = newdat.lme, level = 0)

ggplot(Fecond, aes(x = sMSL, y = log(eggs), color = tAdulte) ) +
     geom_rug(sides = "b", size = 1) +
     geom_line(data = newdat.lme, aes(y = predlme), size = 1)

des = model.matrix(formula("~tAdulte * Rank + sMSL"), newdat.lme)

predvar = diag( des %*% vcov(FitMSL) %*% t(des) )

head(newdat.lme)
newdat.lme$lower = with(newdat.lme, predlme - 2*sqrt(predvar) )
newdat.lme$upper = with(newdat.lme, predlme + 2*sqrt(predvar) )



MMSL<-ggplot(Fecond, aes(x = sMSL, y = eggs, color = tAdulte) ) + geom_jitter(aes(x=sMSL, y = eggs),width = 0.25,size=4) + geom_ribbon(data = newdat.lme, aes(y = NULL, ymin = exp(lower), ymax = exp(upper),color = NULL, fill = tAdulte), alpha = .15) + geom_line(data = newdat.lme, aes(y = exp(predlme)), size = 2) + theme_bw()+ggtitle("d.")+scale_colour_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("Na","PNa"),labels=c("Rich adult","Poor adult"))+scale_fill_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("Na","PNa"),labels=c("Rich adult","Poor adult"))+ labs(x = "mean centered (SMI)", y = "Number of eggs")+ theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))









Fit <- glmmadmb(formula = eggs~tAdulte*Rank+(1|individu),zeroInflation=TRUE, family="nbinom1", data =Fecond)
confint(Fit)

newdat.lme = data.frame(tAdulte = Fecond$tAdulte,
                        Rank = Fecond$Rank)

head(newdat.lme)

newdat.lme$predlme = predict(Fit, newdata = newdat.lme, level = 0)

ggplot(Fecond, aes(x = Rank, y = log(eggs), color = tAdulte) ) +
     geom_rug(sides = "b", size = 1) +
     geom_line(data = newdat.lme, aes(y = predlme), size = 1)

des = model.matrix(formula("~tAdulte * Rank"), newdat.lme)

predvar = diag( des %*% vcov(Fit) %*% t(des) )


newdat.lme$lower = with(newdat.lme, predlme - 2*sqrt(predvar) )
newdat.lme$upper = with(newdat.lme, predlme + 2*sqrt(predvar) )

range(newdat.lme$lower)
[1] 1.014011 2.885111
range(newdat.lme$upper)
[1] 2.378391 3.339049
mean(newdat.lme$lower)
[1] 2.244485
mean(newdat.lme$upper)
[1] 2.901028

Mixed<-ggplot(Fecond, aes(x = Rank, y = eggs, color = tAdulte) ) + geom_jitter(aes(x=Rank, y = eggs),width = 0.25) + geom_ribbon(data = newdat.lme, aes(y = NULL, ymin = exp(lower), ymax = exp(upper),color = NULL, fill = tAdulte), alpha = .15) + geom_line(data = newdat.lme, aes(y = exp(predlme)), size = .75) + theme_bw()+ggtitle("Mixed effect model: Zero-inflated negative binomial (controlling for SMI)")+scale_colour_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("Na","PNa"),labels=c("Rich adult","Poor adult"))+scale_fill_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("Na","PNa"),labels=c("Rich adult","Poor adult"))+ labs(x = "Rank", y = "Number of eggs") + theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 


## newdata for prediction
Fecond$yhat <- predict(Fit, Fecond, type = "response")

## graph with CIs
ztnb<-ggplot(Fecond, aes(x = Rank, y = yhat,color=tAdulte))+ geom_line(size=1)+geom_point(aes(x=Rank, y=eggs))+theme_bw()
ztnb



library(boot)
set.seed(10)
res <- boot(Fecond, f, R = 1200, newdata = Fecond, parallel = "snow", ncpus = 8)

## basic parameter estimates with percentile and bias adjusted CIs
parms <- t(sapply(c(1:nrow(FecondTrunc$yhat)), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "basic"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
    basicLL = basic[4], basicLL = basic[5]))
}))

## add row names
#row.names(parms) <- names(coef(fit_zinb))
## print results
parms


## exponentiated parameter estimates with percentile and bias adjusted CIs
expparms <- t(sapply(1 + (1:nrow(FecondTrunc)), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "basic"), h = exp)
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
    basicLL = basic[4], basicLL = basic[5]))
}))

## add row names
row.names(expparms) <- names(coef(fit_zinb))
## print results
expparms


ggplot(FecondTrunc, aes(x = OLSresidue, y = yhat))  +
  geom_point() +
  geom_line() 


## get the bootstrapped percentile CIs
yhat <- t(sapply(6 +(1:nrow(Fecond$yhat)), function(i) {
  out <- boot.ci(res, index = i, type = c("perc"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5]))
}))



ztnb<-ggplot(Fecond, aes(x = Rank, y = yhat,color=tAdulte))+ geom_line(size=1) 
ztnb




length(data.frame(yhat)$Est)
length(FecondTrunc$yhat)

length(data.frame(yhat)$Est)
length(FecondTrunc$yhat)


head(FecondTrunc)

head(FecondTrunc$yhat)


head(yhat)

## merge CIs with predicted values
FecondTrunc <- cbind(FecondTrunc, yhat)

head(FecondTrunc,20)





library(gtable)
library(gridExtra)
library(ggpubr)
library(grid)

legend = gtable_filter(ggplotGrob(MOLS), "guide-box")



png("/home/mark/Dropbox/condition/ANALYSE/MIXEDOLSMASSSMI.png",width=6300, height=2970,res=300)

grid.arrange(arrangeGrob(MOLS + theme(legend.position="none"), MPOIDS + theme(legend.position="none"),MMSL + theme(legend.position="none"),nrow=1),legend,widths=unit.c(unit(1, "npc") - legend$width, legend$width))

dev.off()


grid.arrange(arrangeGrob(MOLS + theme(legend.position="none"), MPOIDS + theme(legend.position="none"),MMSL + theme(legend.position="none"),nrow=1),legend,widths=unit.c(unit(1, "npc") - legend$width, legend$width))

MixedOLS





legend = gtable_filter(ggplotGrob(MixedOLS), "guide-box")


png("/home/mark/Dropbox/condition/ANALYSE/RANKMIXEDOLSMASSSMI.png",width=2000, height=2000,res=100)

pushViewport(viewport(layout = grid.layout(19, 6)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
legend$vp = viewport(layout.pos.row = 19, layout.pos.col = 2:5)
grid.draw(legend)
print(MixedOLS+ggtitle("a.")+ theme(legend.position="none"), vp = vplayout(1:8, 2:5))
print(MOLS + theme(legend.position="none"), vp = vplayout(9:18, 1:2))
print(MPOIDS + theme(legend.position="none"), vp = vplayout(9:18, 3:4))
print(MMSL + theme(legend.position="none"), vp = vplayout(9:18, 5:6))

dev.off()





visreg(fit_zinb)



library(ggplot2)
g0<-ggplot(Fecond,aes(x=Rank,y=eggs,color=tAdulte))
glm3<-g0+geom_point()+geom_smooth(method = "gam", formula = y ~ x,method.args=list(family="nb()"))
glm3


g0<-ggplot(Fecond,aes(x=Rank,y=eggs,color=milieu))
glm3<-g0+geom_point()+geom_smooth(method = "gam", formula = y ~ x,method.args=list(family="nb()"))
glm3


g0<-ggplot(Fecond,aes(x=OLS,y=eggs,color=milieu))
glm3<-g0+geom_point()+geom_smooth(method = "gam", formula = y ~ x,method.args=list(family="nb()"))
glm3



## graph with CIs


+ geom_ribbon(aes(ymin = pLL, ymax = pUL), alpha = .25) + geom_line(size=1)+geom_point(aes(x = OLSresidue, y = reproduction.1)) + labs(x = "OLSresid", y = "Number of eggs (first egg laying event)") + ggtitle("Zero-truncated negative binomial") + theme_bw()
ztnb



pdf("/home/mark/Dropbox/condition/ANALYSE/QuiDevraitMixed.pdf",paper="a4",width=8.267, height=11.692)
multiplot(p1, p2, cols=1)
dev.off()


detach(Fecond)


jpeg("/home/mark/Dropbox/condition/ANALYSE/QuiDevraitMixed.jpeg",paper="a4",width=2100, height=3000,res=300)
multiplot(p1, p2, cols=1)
dev.off()


##########################
setwd("~/Dropbox/condition/ANALYSE/FinalAnalysis")

Fecond= read.csv(file = "FeconditePropreNew.csv", header = TRUE)
Fecond<-Fecond[which(!Fecond$Days.laid=="NA" & !Fecond$Mort==1),]
Fecond$Days.laid

#detach(Fecond)
#attach(Fecond)
names(Fecond)
head(Fecond)


 [1] "individu"      "milieu"        "eggs"          "larves"       
 [5] "oeufspaseclos" "Mort"          "TreatLarve"    "TreatAdult"   
 [9] "OLSresidue"    "MSL"           "VSL"           "poids"        
[13] "moyLong"       "volume"        "moylargeur"    "moyepaiss"  

library(MuMIn)

hist(Fecond$eggs,breaks=40)
hist(Fecond$OLSresidue,breaks=40)
hist(Fecond$MSL,breaks=40)
hist(Fecond$VSL,breaks=40)
hist(Fecond$poids,breaks=40)
hist(Fecond$volume,breaks=40)
hist(Fecond$moyLong,breaks=40)

qqnorm(Fecond$eggs,breaks=40)
qqnorm(Fecond$OLSresidue,breaks=40)
qqnorm(Fecond$MSL,breaks=40)
qqnorm(Fecond$VSL,breaks=40)
qqnorm(Fecond$poids,breaks=40)
qqnorm(Fecond$volume,breaks=40)
qqnorm(Fecond$moyLong,breaks=40)


#install.packages("coda")
#install.packages("lme4")
#install.packages("reshape")
#install.packages("coefplot2",repos="http://www.math.mcmaster.ca/bolker/R")
#install.packages("boot")
#install.packages("MuMIn")


dfun <- function(object){with(object,sum((weights*residuals^2)[weights>0])/df.residual)}
x.quasipoisson <- function(...) {
res <- quasipoisson(...)
res$aic <- poisson(...)$aic
res
}

M1b<-glm(eggs~TreatLarve*TreatAdult*moyLong,family=quasipoisson,na.action=na.fail,data=Fecond)
summary(M1b)
par(mfrow=c(2,2))
plot(M1b)
dev.off()

library(coefplot2)
coefplot2(M1b)


library(boot)
CI<-confint(M1b, method="boot", nsim=10000,parallel = "snow")
CI


M1<-glm(eggs~TreatLarve*TreatAdult,family=quasipoisson,na.action=na.fail,data=Fecond)
summary(M1)
#Call:
#glm(formula = Eggs2 ~ TreatLarve * TreatAdult, family = quasipoisson, 
#    na.action = na.fail)#

#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-4.2339  -1.2031  -0.1399   0.9640   4.3835  #

#Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                2.47666    0.05213  47.510  < 2e-16 ***
#TreatLarvePN               0.09248    0.07691   1.202  0.23043    
#TreatAdultPN              -0.28356    0.08638  -3.283  0.00119 ** 
#TreatLarvePN:TreatAdultPN -0.10637    0.12369  -0.860  0.39066    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#

#(Dispersion parameter for quasipoisson family taken to be 2.29624)#

#    Null deviance: 627.80  on 235  degrees of freedom
#Residual deviance: 556.67  on 232  degrees of freedom
#AIC: NA#

#Number of Fisher Scoring iterations: 4


calcules des estimates
TreatLarveN / TreatAdultN = 2.47666
TreatLarvePN / TreatAdultN = 2.47666 + 0.09248 = 2.56914
TreatLarveN / TreatAdultPN = 2.47666 - 0.28356 = 2.1931
TreatLarvePN / TreatAdultPN = 2.47666 -0.10637 = 2.37029

CI<-confint(M1)
CI

#                               2.5 %     97.5 %
#(Intercept)                2.3727129  2.5771150
#TreatLarvePN              -0.0586465  0.2429890
#TreatAdultPN              -0.4542649 -0.1154481
#TreatLarvePN:TreatAdultPN -0.3487240  0.1362963

TreatLarveN / TreatAdultN IC= 2.3727129, 2.5771150
TreatLarvePN / TreatAdultN IC= 2.47666 -0.0586465, 2.47666 + 0.2429890 = 2.418013, 2.719649
TreatLarveN / TreatAdultPN IC= 2.47666 -0.4542649 , 2.47666 -0.1154481= 2.022395, 2.361212
TreatLarvePN / TreatAdultPN IC= 2.47666 -0.3487240, 2.47666 + 0.1362963  = 2.127936 , 2.612956


densite<-Fecond$poids/Fecond$moyLong

M1<-glm(eggs~TreatLarve*TreatAdult*moyLong,family=poisson,na.action=na.fail,data=Fecond)
M2<-glm(eggs~TreatLarve*TreatAdult*poids,family=poisson,na.action=na.fail,data=Fecond)
M3<-glm(eggs~TreatLarve*TreatAdult*volume,family=poisson,na.action=na.fail,data=Fecond)
M4<-glm(eggs~TreatLarve*TreatAdult*OLSresidue,family=poisson,na.action=na.fail,data=Fecond)
M5<-glm(eggs~TreatLarve*TreatAdult*VSL,family=poisson,na.action=na.fail,data=Fecond)
M6<-glm(eggs~TreatLarve*TreatAdult*MSL,family=poisson,na.action=na.fail,data=Fecond)
M8<-glm(eggs~TreatLarve*TreatAdult*densite,family=poisson,na.action=na.fail,data=Fecond)
M9<-glm(eggs~TreatLarve*TreatAdult*DSL,family=poisson,na.action=na.fail,data=Fecond)

M7<-glm(eggs~TreatLarve*TreatAdult*moyLong+TreatLarve*TreatAdult*OLSresidue+TreatLarve*TreatAdult*volume+TreatLarve*TreatAdult*MSL+TreatLarve*TreatAdult*poids+TreatLarve*TreatAdult*VSL,family=poisson,na.action=na.fail,data=Fecond)

Mtest<-glm(eggs~TreatLarve*TreatAdult*moyLong+TreatLarve*TreatAdult*OLSresidue+TreatLarve*TreatAdult*volume+TreatLarve*TreatAdult*MSL+TreatLarve*TreatAdult*poids+TreatLarve*TreatAdult*VSL,family=poisson,na.action=na.fail,data=Fecond)

Mtest<-glm(eggs~TreatLarve*TreatAdult*moyLong+TreatLarve*TreatAdult*OLSresidue+TreatLarve*TreatAdult*volume+TreatLarve*TreatAdult*MSL+TreatLarve*TreatAdult*poids+TreatLarve*TreatAdult*VSL,family=poisson,na.action=na.fail,data=Fecond)


library(MuMIn)

MtestB <- update(Mtest,family="x.quasipoisson",na.action=na.fail)
gg <- dredge(MtestB,rank="QAICc", extra="R^2", chat=dfun(Mtest), subset= !(moyLong&OLSresidue)&!(moyLong&volume)&!(moyLong&MSL)&!(moyLong&poids)&!(moyLong&VSL)&!(OLSresidue&volume)&!(OLSresidue&MSL)&!(OLSresidue&poids)&!(OLSresidue&VSL)&!(MSL&poids)&!(MSL&VSL)&!(poids&VSL))
gg
model.avg(gg)
confset.95p <- get.models(gg, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)



summary(M1)
library(MuMIn)





M1b <- update(M1,family="x.quasipoisson",na.action=na.fail)
gg1 <- dredge(M1b,rank="QAICc", extra="R^2", chat=dfun(M1))
M2b <- update(M2,family="x.quasipoisson",na.action=na.fail)
gg2 <- dredge(M2b,rank="QAICc", extra="R^2", chat=dfun(M2))
M3b <- update(M3,family="x.quasipoisson",na.action=na.fail)
gg3 <- dredge(M3b,rank="QAICc", extra="R^2", chat=dfun(M3))
M4b <- update(M4,family="x.quasipoisson",na.action=na.fail)
gg4 <- dredge(M4b,rank="QAICc", extra="R^2", chat=dfun(M4))
M5b <- update(M5,family="x.quasipoisson",na.action=na.fail)
gg5 <- dredge(M5b,rank="QAICc", extra="R^2", chat=dfun(M5))
M6b <- update(M6,family="x.quasipoisson",na.action=na.fail)
gg6 <- dredge(M6b,rank="QAICc", extra="R^2", chat=dfun(M6))
M8b <- update(M8,family="x.quasipoisson",na.action=na.fail)
gg8 <- dredge(M8b,rank="QAICc", extra="R^2", chat=dfun(M6))
M9b <- update(M9,family="x.quasipoisson",na.action=na.fail)
gg9 <- dredge(M9b,rank="QAICc", extra="R^2", chat=dfun(M6))


M7b <- update(M7,family="x.quasipoisson",na.action=na.fail)
gg <- dredge(M7b,rank="QAICc", extra="R^2", chat=dfun(M7), subset= !(moyLong & (poids||volume||OLSresidue||MSL||VSL)) & !(poids & (volume||OLSresidue||MSL||VSL)) & !(volume & (OLSresidue||MSL||VSL)) & !(OLSresidue & (MSL||VSL)) & !(MSL & VSL))

head(gg)
gg
model.avg(gg)
confset.95p <- get.models(gg, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

gg1
model.avg(gg1)
confset.95p <- get.models(gg1, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

gg2
model.avg(gg2)
confset.95p <- get.models(gg2, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

gg3
model.avg(gg3)
confset.95p <- get.models(gg3, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

gg4
model.avg(gg4)
confset.95p <- get.models(gg4, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

gg5
model.avg(gg5)
confset.95p <- get.models(gg5, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

gg6
model.avg(gg6)
confset.95p <- get.models(gg6, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)


gg8
model.avg(gg8)
confset.95p <- get.models(gg8, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

gg9
model.avg(gg9)
confset.95p <- get.models(gg9, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)



gg
gg1
gg2
gg3
gg4
gg5
gg6
gg8
gg9





jpeg("/home/mark/Dropbox/condition/ANALYSE/FigureBoxplotOeufsMilieu.jpeg",width=2100, height=2100,res=300)

boxplot(Fecond$eggs~Fecond$TreatLarve:Fecond$TreatAdult,xlab="Food treatement (Larval:Adult)",ylab="Eggs",outline=FALSE,ylim=c(0,210))
stripchart(Fecond$eggs~Fecond$TreatLarve:Fecond$TreatAdult, vertical = TRUE,add = TRUE,names="",pch = 16,method="jitter")

dev.off()



names(Fecond)

library(ggplot2)

Eggs<-ggplot(Fecond, aes(y=eggs,x=milieu))+geom_boxplot(outlier.shape = NA,aes(colour=TreatAdult),show.legend = FALSE)+geom_jitter(position = position_jitter(0.1),aes(colour=TreatAdult,shape=TreatLarve),size=4)+theme_bw()+scale_colour_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("N","PN"),labels=c("Rich adult feeding treatment","Poor adult feeding treatment"))+scale_shape_manual(values=c(1,19),name="", breaks=c("N","PN"),labels=c("Rich larval feeding treatment","Poor larval feeding treatment"))+ labs(x = "Feeding treatment", y = "Total fecundity (total number of eggs)")+ scale_x_discrete(labels=c("NlNa" = "Rich larval/Rich adult", "NlPNa" = "Rich larval/Poor adult","PNlNa" = "Poor larval/Rich adult","PNlPNa" = "Poor larval/Poor adult"))+ theme(plot.title = element_text(size = rel(1.5)),axis.text.y = element_text(size = rel(1.5)),axis.text.x = element_text(size = rel(1.2)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))

Eggs+ theme(legend.position="none")




png("/home/mark/Dropbox/condition/ANALYSE/FigureBoxplotOeufsMilieuNew.png",width=2100, height=2100,res=300)

Eggs+ theme(legend.position="none")


dev.off()







boxplot(eggs~)

#install.packages("MASS")
library(MASS)

M1.nb<-glm.nb(eggs~TreatLarve*TreatAdult*moyLong,na.action=na.fail,data=Fecond)
M2.nb<-glm.nb(eggs~TreatLarve*TreatAdult*poids,na.action=na.fail,data=Fecond)
M3.nb<-glm.nb(eggs~TreatLarve*TreatAdult*volume,na.action=na.fail,data=Fecond)
M4.nb<-glm.nb(eggs~TreatLarve*TreatAdult*OLSresidue,na.action=na.fail,data=Fecond)
M5.nb<-glm.nb(eggs~TreatLarve*TreatAdult*VSL,na.action=na.fail,data=Fecond)
M6.nb<-glm.nb(eggs~TreatLarve*TreatAdult*MSL,na.action=na.fail,data=Fecond)
M7.nb<-glm.nb(eggs~TreatLarve*TreatAdult,na.action=na.fail,data=Fecond)
M8.nb<-glm.nb(eggs~TreatLarve*TreatAdult*densite,na.action=na.fail,data=Fecond)
M9.nb<-glm.nb(eggs~TreatLarve*TreatAdult*DSL,na.action=na.fail,data=Fecond)


(gg1.nb <- dredge(M1.nb,rank="AICc", extra="R^2"))
(gg2.nb <- dredge(M2.nb,rank="AICc", extra="R^2"))
(gg3.nb <- dredge(M3.nb,rank="AICc", extra="R^2"))
(gg4.nb <- dredge(M4.nb,rank="AICc", extra="R^2"))
(gg5.nb <- dredge(M5.nb,rank="AICc", extra="R^2"))
(gg6.nb <- dredge(M6.nb,rank="AICc", extra="R^2"))
(gg7.nb <- dredge(M7.nb,rank="AICc", extra="R^2"))
(gg8.nb <- dredge(M8.nb,rank="AICc", extra="R^2"))
(gg9.nb <- dredge(M9.nb,rank="AICc", extra="R^2"))

M8c.nb<-glm.nb(eggs~TreatLarve+densite,na.action=na.fail,data=Fecond)

summary(M8c.nb)


par(mfrow=c(2,2))
plot(M1)
summary.glm(M1.nb)
summary(M1.nb)


gg1.nb
model.avg(gg1.nb)
confset.95p <- get.models(gg1.nb, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

gg2.nb
model.avg(gg2.nb)
confset.95p <- get.models(gg2.nb, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

gg3.nb
model.avg(gg3.nb)
confset.95p <- get.models(gg3.nb, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

gg4.nb
model.avg(gg4.nb)
confset.95p <- get.models(gg4.nb, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

gg5.nb
model.avg(gg5.nb)
confset.95p <- get.models(gg5.nb, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

gg6.nb
model.avg(gg6.nb)
confset.95p <- get.models(gg6.nb, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)


gg7.nb
model.avg(gg7.nb)
confset.95p <- get.models(gg7.nb, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)


gg8.nb
model.avg(gg8.nb)
confset.95p <- get.models(gg8.nb, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)


gg9.nb
model.avg(gg9.nb)
confset.95p <- get.models(gg9.nb, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)





par(mfrow=c(2,2))
plot(M1b)
plot(M2b)
plot(M3b)
plot(M4b)
plot(M5b)
plot(M6b)
plot(M8b)
plot(M9b)

write.table(gg1.nb, "~/Dropbox/condition/ANALYSE/eggsLong.txt", sep="\t")
write.table(gg2.nb, "~/Dropbox/condition/ANALYSE/eggsPoids.txt", sep="\t")
write.table(gg3.nb, "~/Dropbox/condition/ANALYSE/eggsvlm.txt", sep="\t")
write.table(gg4.nb, "~/Dropbox/condition/ANALYSE/eggsOLS.txt", sep="\t")
write.table(gg5.nb, "~/Dropbox/condition/ANALYSE/eggsVSL.txt", sep="\t")
write.table(gg6.nb, "~/Dropbox/condition/ANALYSE/eggsMSL.txt", sep="\t")
write.table(gg8.nb, "~/Dropbox/condition/ANALYSE/eggsdensity.txt", sep="\t")
write.table(gg9.nb, "~/Dropbox/condition/ANALYSE/eggsDSL.txt", sep="\t")

model.avg(gg1)
confset.95p <- get.models(gg1, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

model.avg(gg2)
confset.95p <- get.models(gg2, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

model.avg(gg3)
confset.95p <- get.models(gg3, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p

model.avg(gg4)
confset.95p <- get.models(gg4, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

model.avg(gg5)
confset.95p <- get.models(gg5, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

model.avg(gg6)
confset.95p <- get.models(gg6, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)






M1<-glm(eggs~TreatLarve*TreatAdult*moyLong,na.action=na.fail,data=Fecond,family=Gamma(log))
M2<-glm(eggs~TreatLarve*TreatAdult*poids,na.action=na.fail,data=Fecond,family=Gamma(log))
M3<-glm(eggs~TreatLarve*TreatAdult*volume,na.action=na.fail,data=Fecond,family=Gamma(log))
M4<-glm(eggs~TreatLarve*TreatAdult*OLSresidue,na.action=na.fail,data=Fecond,family=Gamma(log))
M5<-glm(eggs~TreatLarve*TreatAdult*VSL,na.action=na.fail,data=Fecond,family=Gamma(log))
M6<-glm(eggs~TreatLarve*TreatAdult*MSL,na.action=na.fail,data=Fecond,family=Gamma(log))
M7<-glm(eggs~TreatLarve*TreatAdult,na.action=na.fail,data=Fecond,family=Gamma(log))


(gg1.Gamma <- dredge(M1,rank="AICc", extra="R^2"))
(gg2.Gamma <- dredge(M2,rank="AICc", extra="R^2"))
(gg3.Gamma <- dredge(M3,rank="AICc", extra="R^2"))
(gg4.Gamma <- dredge(M4,rank="AICc", extra="R^2"))
(gg5.Gamma <- dredge(M5,rank="AICc", extra="R^2"))
(gg6.Gamma <- dredge(M6,rank="AICc", extra="R^2"))
(gg7.Gamma <- dredge(M7,rank="AICc", extra="R^2"))


summary(M1)
summary(M1.nb)
summary(M2)
summary(M2.nb)
summary(M3)
summary(M3.nb)
summary(M4)
summary(M4.nb)
summary(M5)
summary(M5.nb)
summary(M6)
summary(M6.nb)
summary(M7)
summary(M7.nb)


####Les resultats entre Gamma et neg bin quasiment pareille. J'ai choisi neg bin parce que plus reconnu par les ecolos


Zr.and.se2<-function(r,n){
  Zr<-0.5*log((1+r)/(1-r))
  se<-(1/sqrt(n-3))
  names(Zr)<-"Fisher's z"
  names(se)<-"se for Zr"
  c(Zr,se)
}


partial.r<-function(t.val,df){
  r<-t.val/sqrt((t.val)^2+df)
  names(r)<-"effect size r"
  return(r)
}

library(MASS)

M1<-glm.nb(eggs~TreatAdult+moyLong,na.action=na.fail,data=Fecond)
summary(M1)  

#Call:
#glm.nb(formula = eggs ~ TreatAdult + moyLong, data = Fecond, 
#    na.action = na.fail, init.theta = 3.997191673, link = log)#

#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-4.0065  -0.7217  -0.0915   0.5857   2.4447  #

#Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   3.58980    0.57917   6.198 5.71e-10 ***
#TreatAdultPN -0.35019    0.06931  -5.053 4.36e-07 ***
#moyLong       0.10678    0.06097   1.751   0.0799 .  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#

#(Dispersion parameter for Negative Binomial(3.9972) family taken to be 1)#

#    Null deviance: 261.25  on 220  degrees of freedom
#Residual deviance: 234.03  on 218  degrees of freedom
#AIC: 2262#

#Number of Fisher Scoring iterations: 1#
#

#              Theta:  3.997 
#          Std. Err.:  0.394 #

# 2 x log-likelihood:  -2254.006 


TreatAdult2<-factor(Fecond$TreatAdult, c("PN","N"))
TreatAdult2

M2<-glm.nb(eggs~TreatAdult2+moyLong,na.action=na.fail,data=Fecond)
summary(M2)  


partial.r(1.751,218)
#effect size r 
#    0.1177674 


Zr.and.se2(0.1177674,221)
#Fisher's z  se for Zr 
#0.11831642 0.06772855 


coef(summary(M1))[3,3]

library(boot)

r.part <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- glm.nb(formula,data=d)
  return(partial.r(coef(summary(fit))[3,3],218))
}

# bootstrapping with 10000 replications
(results <- boot(data=Fecond, statistic=r.part,R=10000, formula=eggs~TreatAdult+moyLong))

#Call:
#boot(data = Fecond, statistic = r.part, R = 10000, formula = eggs ~ 
#    TreatAdult + moyLong)#
#

#Bootstrap Statistics :
#     original        bias    std. error
#t1* 0.1177959 -0.0004526272  0.06492167



library(boot)
(Boot<-boot.ci(results, type="bca"))
# get 95% confidence interval

#(Boot<-boot.ci(results, type="bca"))
#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   (-0.0119,  0.2440 )  
#Calculations and Intervals on Original Scale




Length<-as.numeric(results$t0)

minci.Length<-Boot$bca[,4]
maxci.Length<-Boot$bca[,5]


# view results
results
plot(results)




CI1<-function(ES,se){
  ci<-c((ES-(1.96)*se),(ES+(1.96)*se))
  names(ci)<-c("95% CI lower","95% CI upper")
  return(ci)
}
#  
#CI2<-function(ES,se,df){
#  ci<-c((ES-qt(0.975,df)*se),(ES+qt(0.975,df)*se))
#  names(ci)<-c("95% CI lower","95% CI upper")
#  return(ci)
#}#

  
CI1(0.11831642, 0.06772855)
#95% CI lower 95% CI upper 
# -0.01443154   0.25106438 


M3<-glm.nb(eggs~TreatAdult+poids,na.action=na.fail,data=Fecond)

summary(M3)

#Call:
#glm.nb(formula = eggs ~ TreatAdult + poids, data = Fecond, na.action = na.fail, 
#    init.theta = 4.150208508, link = log)#

#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-3.9897  -0.7579  -0.0762   0.5626   2.3301  #

#Coefficients:
#              Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   3.920414   0.201210  19.484  < 2e-16 ***
#TreatAdultPN -0.354061   0.067772  -5.224 1.75e-07 ***
#poids         0.005779   0.001665   3.472 0.000517 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#

#(Dispersion parameter for Negative Binomial(4.1502) family taken to be 1)#

#    Null deviance: 270.49  on 220  degrees of freedom
#Residual deviance: 233.97  on 218  degrees of freedom
#AIC: 2253.8#

#Number of Fisher Scoring iterations: 1#
#

#              Theta:  4.150 
#          Std. Err.:  0.410 #

# 2 x log-likelihood:  -2245.844 


ci.smd(ncp=-5.224 , n.1=111, n.2=110, conf.level=1-.05)


partial.r(3.472,218)
#effect size r 
#    0.2289096 

Zr.and.se2(0.2288828,221)

CI1(0.23301019, 0.06772855)

(results <- boot(data=Fecond, statistic=r.part,R=10000, formula=eggs~TreatAdult+poids))

#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = Fecond, statistic = r.part, R = 10000, formula = eggs ~ 
#    TreatAdult + poids)#
#

#Bootstrap Statistics :
#     original      bias    std. error
#t1* 0.2288828 0.001497025  0.06037667



plot(results)
(Boot<-boot.ci(results, type="bca"))

#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   ( 0.1045,  0.3418 )  
#Calculations and Intervals on Original Scale









partial.d<-function(t.val,df,n1,n2){
  d<-t.val*(n1+n2)/(sqrt(n1*n2)*sqrt(df))
  names(d)<-"effect size d"
  return(d)
}

tapply(Fecond$poids,Fecond$TreatLarve,length)
#  N  PN 
#110 111 


partial.d(-5.224,220,110,111)
#effect size d 
#   -0.7044112 


coef(summary(M3))[2,3]

d.part <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula,data=d)
  return(partial.d(coef(summary(fit))[2,3],220,110,111))
}

# bootstrapping with 10000 replications
(results <- boot(data=Fecond, statistic=d.part,R=10000, formula=eggs~TreatAdult+poids))

#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = Fecond, statistic = d.part, R = 10000, formula = eggs ~ 
#    TreatAdult + poids)#
#

#Bootstrap Statistics :
#      original       bias    std. error
#t1* -0.7663723 -0.003349541   0.1476262


library(boot)
(Boot<-boot.ci(results, type="bca"))

#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   (-1.0574, -0.4784 )  
#Calculations and Intervals on Original Scale











Weight<-as.numeric(results$t0)
minci.Weight<-Boot$bca[,4]
maxci.Weight<-Boot$bca[,5]


(results <- boot(data=Fecond, statistic=r.part,R=10000, formula=eggs~TreatAdult+volume))

#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = Fecond, statistic = r.part, R = 10000, formula = eggs ~ 
#    TreatAdult + volume)#
#

#Bootstrap Statistics :
#     original      bias    std. error
#t1* 0.1718071 0.000447119  0.05818483

plot(results)
(Boot<-boot.ci(results, type="bca"))

#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   ( 0.0588,  0.2874 )  
#Calculations and Intervals on Original Scale


Vol<-as.numeric(results$t0)
minci.Vol<-Boot$bca[,4]
maxci.Vol<-Boot$bca[,5]


M3<-glm.nb(eggs~TreatAdult+OLSresidue,na.action=na.fail,data=Fecond)

tapply(Fecond$eggs,Fecond$TreatAdult,length)
  N  PN 
111 110 

summary(M3)

#Call:
#glm.nb(formula = eggs ~ TreatAdult + OLSresidue, data = Fecond, 
#    na.action = na.fail, init.theta = 4.167746972, link = log)#

#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-3.9394  -0.7764  -0.0663   0.5462   2.2379  #

#Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   4.57575    0.04807  95.187  < 2e-16 ***
#TreatAdultPN -0.33754    0.06765  -4.989 6.07e-07 ***
#OLSresidue    1.17565    0.33704   3.488 0.000486 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#

#(Dispersion parameter for Negative Binomial(4.1677) family taken to be 1)#

#    Null deviance: 271.54  on 220  degrees of freedom
#Residual deviance: 233.97  on 218  degrees of freedom
#AIC: 2252.9#

#Number of Fisher Scoring iterations: 1#
#

#              Theta:  4.168 
#          Std. Err.:  0.412 #

# 2 x log-likelihood:  -2244.935 


library(MBESS) 

ci.smd(ncp=-4.989 , n.1=111, n.2=110, conf.level=1-.05)
#$Lower.Conf.Limit.smd
#[1] -0.941539#

#$smd
#[1] -0.6711998#

#$Upper.Conf.Limit.smd
#[1] -0.3994072



(results <- boot(data=Fecond, statistic=r.part,R=10000, formula=eggs~TreatAdult+OLSresidue))


#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = Fecond, statistic = r.part, R = 10000, formula = eggs ~ 
#    TreatAdult + OLSresidue)#
#

#Bootstrap Statistics :
#     original       bias    std. error
#t1* 0.2299175 6.943211e-05  0.05910606



plot(results)
Boot<-(boot<-boot.ci(results, type="bca"))

#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   ( 0.1101,  0.3410 )  
#Calculations and Intervals on Original Scale


ols<-as.numeric(results$t0)
minci.ols<-Boot$bca[,4]
maxci.ols<-Boot$bca[,5]




M3<-glm.nb(eggs~TreatAdult+MSL,na.action=na.fail,data=Fecond)

tapply(Fecond$eggs,Fecond$TreatAdult,length)
  N  PN 
111 110 

summary(M3)

#Call:
#glm.nb(formula = eggs ~ TreatAdult + MSL, data = Fecond, na.action = na.fail, 
#    init.theta = 4.07874818, link = log)#

#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-3.9392  -0.7794  -0.0498   0.5756   2.3047  #

#Coefficients:
#              Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   3.733886   0.325673  11.465  < 2e-16 ***
#TreatAdultPN -0.335222   0.068470  -4.896 9.79e-07 ***
#MSL           0.007258   0.002685   2.704  0.00686 ** 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#

#(Dispersion parameter for Negative Binomial(4.0787) family taken to be 1)#

#    Null deviance: 266.18  on 220  degrees of freedom
#Residual deviance: 233.96  on 218  degrees of freedom
#AIC: 2257.6#

#Number of Fisher Scoring iterations: 1#
#

#              Theta:  4.079 
#          Std. Err.:  0.403 #

# 2 x log-likelihood:  -2249.573 #


ci.smd(ncp=-4.896  , n.1=111, n.2=110, conf.level=1-.05)


(results <- boot(data=Fecond, statistic=r.part,R=10000, formula=eggs~TreatAdult+MSL))

#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = Fecond, statistic = r.part, R = 10000, formula = eggs ~ 
#    TreatAdult + MSL)#
#

#Bootstrap Statistics :
#     original        bias    std. error
#t1* 0.1801189 -0.0001548352  0.05885196


plot(results)
(Boot<-boot.ci(results, type="bca"))

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   ( 0.0611,  0.2930 )  
#Calculations and Intervals on Original Scale

msl<-as.numeric(results$t0)
minci.msl<-Boot$bca[,4]
maxci.msl<-Boot$bca[,5]


(results <- boot(data=Fecond, statistic=r.part,R=10000, formula=eggs~TreatAdult+VSL))

#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = Fecond, statistic = r.part, R = 10000, formula = eggs ~ 
#    TreatAdult + VSL)#
#

#Bootstrap Statistics :
#     original      bias    std. error
#t1* 0.0741907 0.001112534  0.06433879




plot(results)
(Boot<-boot.ci(results, type="bca"))

#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   (-0.0610,  0.1919 )  
#Calculations and Intervals on Original Scale





M3<-glm.nb(eggs~TreatAdult+MSL*TreatLarve,na.action=na.fail,data=Fecond)

tapply(Fecond$eggs,Fecond$TreatAdult,length)
#  N  PN 
#111 110 

summary(M3)

#Call:
#glm.nb(formula = eggs ~ TreatAdult + MSL * TreatLarve, data = Fecond, 
#    na.action = na.fail, init.theta = 4.222457082, link = log)#

#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-4.0873  -0.7304  -0.0263   0.5529   2.3063  #

#Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       2.720437   0.484374   5.616 1.95e-08 ***
#TreatAdultPN     -0.343274   0.067369  -5.095 3.48e-07 ***
#MSL               0.015344   0.003975   3.860 0.000113 ***
#TreatLarvePN      1.698041   0.640163   2.653 0.007990 ** 
#MSL:TreatLarvePN -0.013597   0.005342  -2.545 0.010919 *  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#

#(Dispersion parameter for Negative Binomial(4.2225) family taken to be 1)#

#    Null deviance: 274.84  on 220  degrees of freedom
#Residual deviance: 233.88  on 216  degrees of freedom
#AIC: 2254#

#Number of Fisher Scoring iterations: 1#
#

#              Theta:  4.222 
#          Std. Err.:  0.418 #

# 2 x log-likelihood:  -2242.044 




partial.d<-function(t.val,df,n1,n2){
  d<-t.val*(n1+n2)/(sqrt(n1*n2)*sqrt(df))
  names(d)<-"effect size d"
  return(d)
}

tapply(Fecond$poids,Fecond$TreatLarve,length)
#  N  PN 
#110 111 


partial.d(-5.095,220,110,111)
#effect size d 
#   -0.6870167 


coef(summary(M3))[2,3]

d.part <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula,data=d)
  return(partial.d(coef(summary(fit))[2,3],220,110,111))
}

# bootstrapping with 10000 replications
(results <- boot(data=Fecond, statistic=d.part,R=10000, formula=eggs~TreatAdult+MSL*TreatLarve))

#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = Fecond, statistic = d.part, R = 10000, formula = eggs ~ 
#    TreatAdult + MSL * TreatLarve)#
#

#Bootstrap Statistics :
#     original       bias    std. error
#t1* -0.713558 -0.005314423   0.1433907



library(boot)
(Boot<-boot.ci(results, type="bca"))

#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   (-0.9896, -0.4258 )  
#Calculations and Intervals on Original Scale






vsl<-as.numeric(results$t0)
minci.vsl<-Boot$bca[,4]
maxci.vsl<-Boot$bca[,5]

Densite<-Fecond$poids/Fecond$volume

(results <- boot(data=Fecond, statistic=r.part,R=10000, formula=eggs~TreatAdult+Densite))

#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = Fecond, statistic = r.part, R = 10000, formula = eggs ~ 
#    TreatAdult + densite)#
#

#Bootstrap Statistics :
#       original       bias    std. error
#t1* 0.004168205 -0.005136517  0.06307225


plot(results)
(Boot<-boot.ci(results, type="bca"))

#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   (-0.1184,  0.1276 )  
#Calculations and Intervals on Original Scale


dens<-as.numeric(results$t0)
minci.densite<-Boot$bca[,4]
maxci.densite<-Boot$bca[,5]

  






(results <- boot(data=Fecond, statistic=r.part,R=10000, formula=eggs~TreatAdult+DSL))

#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = Fecond, statistic = r.part, R = 10000, formula = eggs ~ 
#    TreatAdult + DSL)#
#

#Bootstrap Statistics :
#      original       bias    std. error
#t1* 0.07973165 -0.001632344  0.06253312


plot(results)
(Boot<-boot.ci(results, type="bca"))

#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   (-0.0450,  0.2016 )  
#Calculations and Intervals on Original Scale


dsl<-as.numeric(results$t0)
minci.dsl<-Boot$bca[,4]
maxci.dsl<-Boot$bca[,5]

  



df<-data.frame(effect.size=c(dsl,vsl,msl,ols,dens,Vol,Weight,Length), minci=c(minci.dsl,minci.vsl,minci.msl,minci.ols,minci.densite,minci.Vol,minci.Weight,minci.Length),maxci=c(maxci.dsl,maxci.vsl,maxci.msl,maxci.ols,maxci.densite,maxci.Vol,maxci.Weight,maxci.Length),lev.names=c("DSL","SVI","SMI","OLSresids","Density","volume","Mass","Elytron length"))  
df

df$lev.names2 <- factor(df$lev.names, as.character(df$lev.names))

library(ggplot2)
plot(x=df$effect.size)


p<-ggplot(df,aes(lev.names2, effect.size))
p <- p + geom_hline(yintercept=0,lty=2) + geom_errorbar(aes(ymin=minci, ymax=maxci), width=0,color="black") + geom_point(aes(size=2)) 
p <- p + guides(size=FALSE,shape=FALSE) + scale_shape_manual(values=c(16,16,16,1,1,1))
p <- p + theme_bw() + xlab("Morphological measure/Body condition index") + ylab("Effect size r")
p <- p + theme(axis.text.x=element_text(size=rel(1.2)),
               axis.title.x=element_text(size=rel(1.3)),
               axis.text.y=element_text(size=rel(1.2)),
               panel.grid.minor=element_blank(),
               panel.grid.major.x=element_blank())

p <- p+ coord_flip()+theme(text = element_text(size=15))


print(p)


png("/home/mark/Dropbox/condition/ANALYSE/FigureCoefBCIDSL.png",width=2100, height=2970,res=300)
print(p)
dev.off()



df<-data.frame(effect.size=c(vsl,msl,ols,Vol,Weight,Length), minci=c(minci.vsl,minci.msl,minci.ols,minci.Vol,minci.Weight,minci.Length),maxci=c(maxci.vsl,maxci.msl,maxci.ols,maxci.Vol,maxci.Weight,maxci.Length),lev.names=c("SVI","SMI","OLSresids","Volume","Mass","Elytron length"))  
df

df$lev.names2 <- factor(df$lev.names, as.character(df$lev.names))

library(ggplot2)
plot(x=df$effect.size)


p<-ggplot(df,aes(lev.names2, effect.size))
p <- p + geom_hline(yintercept=0,lty=2) + geom_errorbar(aes(ymin=minci, ymax=maxci), width=0,color="black") + geom_point(aes(size=2)) 
p <- p + guides(size=FALSE,shape=FALSE) + scale_shape_manual(values=c(16,16,16,1,1,1))
p <- p + theme_bw() + xlab("Morphological measure/Body condition index") + ylab("Effect size r")
p <- p + theme(axis.text.x=element_text(size=rel(1.2)),
               axis.title.x=element_text(size=rel(1.3)),
               axis.text.y=element_text(size=rel(1.2)),
               panel.grid.minor=element_blank(),
               panel.grid.major.x=element_blank())

p <- p+ coord_flip()+theme(text = element_text(size=15))
p

png("/home/mark/Dropbox/condition/ANALYSE/FigureCoefBCI.png",width=2100, height=2970,res=300)
print(p)
dev.off()


df<-data.frame(effect.size=c(msl,ols,Weight,Length), minci=c(minci.msl,minci.ols,minci.Weight,minci.Length),maxci=c(maxci.msl,maxci.ols,maxci.Weight,maxci.Length),lev.names=c("SMI","OLSresids","Mass","Elytron length"))  
df

df$lev.names2 <- factor(df$lev.names, as.character(df$lev.names))

library(ggplot2)
plot(x=df$effect.size)



p_new<-ggplot(df,aes(lev.names2, effect.size))
p_new <- p_new + geom_hline(yintercept=0,lty=2) + geom_errorbar(aes(ymin=minci, ymax=maxci), width=0,color="black") + geom_point(aes(size=2)) 
p_new <- p_new + guides(size=FALSE,shape=FALSE) + scale_shape_manual(values=c(16,16,16,1,1,1))
p_new <- p_new + theme_bw() + xlab("Morphological measure/Body condition index") + ylab(expression(paste("Effect size",italic(' r'))))
p_new <- p_new + theme(axis.text.x=element_text(size=rel(1.5)),
               axis.title.y=element_text(size=rel(1.2)),
               axis.title.x=element_text(size=rel(1.2)),
               axis.text.y=element_text(size=rel(1.5)),
               panel.grid.minor=element_blank(),
               panel.grid.major.x=element_blank())

p_new <- p_new+ coord_flip()+theme(text = element_text(size=15))
p_new
 




M1<-glm.nb(eggs~TreatAdult+OLSresidue,na.action=na.fail,data=Fecond)
library(mgcv)
M2<-gam(eggs~TreatAdult+s(OLSresidue),family=nb(),na.action=na.fail,data=Fecond)


AIC(M1)
AIC(M2)
anova(M2)
anova(M1)
dredge(M2)

g0<-ggplot(Fecond,aes(x=OLSresidue,y=eggs,color=TreatAdult))
glm3<-g0+geom_point()+geom_smooth(method = "gam", formula = y ~ s(x,k=4),method.args=list(family="nb()"))
glm3

Fecond$TreatAdult

library(MASS)
g0<-ggplot(Fecond,aes(x=OLSresidue,y=eggs,color=TreatAdult))
glm3<-g0+geom_point()+geom_smooth(method="glm.nb", se=TRUE)
glm3
glm3+scale_colour_manual(values=c("black","red"),name="Feeding treatment", breaks=c("N","PN"),labels=c("Rich adult","Poor adult")) + theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ labs(x = "OLSresid", y = "Total fecundity (total number of eggs)")+theme_bw()

g0<-ggplot(Fecond,aes(x=MSL,y=eggs,color=TreatAdult))
glm1<-g0+geom_point()+geom_smooth(method="glm.nb", se=TRUE)
glm1
glm1+scale_colour_manual(values=c("black","red"),name="Feeding treatment", breaks=c("N","PN"),labels=c("Rich adult","Poor adult")) + theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ labs(x = "MSL", y = "Total fecundity (total number of eggs)")+theme_bw()





Fit <- glm.nb(eggs~TreatAdult+OLSresidue,na.action=na.fail,data=Fecond)


newdat = data.frame(TreatAdult = Fecond$TreatAdult,OLSresidue=Fecond$OLSresidue,eggs=Fecond$eggs)

head(newdat)

pred = predict(Fit, newdata = newdat, level = 0, interval = "confidence")

newdat<-cbind(newdat,pred)

names(newdat)
newdat$lwr
head(newdat)

des = model.matrix(formula("~TreatAdult+OLSresidue"), newdat)

predvar = diag( des %*% vcov(Fit) %*% t(des) )


newdat$lower = with(newdat, pred - 2*sqrt(predvar) )
newdat$upper = with(newdat, pred + 2*sqrt(predvar) )


names(newdat)
head(newdat)



EggsOLS<-ggplot(newdat, aes(x = OLSresidue, y = eggs, color = TreatAdult)) + geom_point(aes(x=OLSresidue, y = eggs),size=4) + geom_ribbon(data = newdat, aes(y = NULL, ymin = exp(lower), ymax = exp(upper),color = NULL, fill = TreatAdult), alpha = .15) + geom_line(data = newdat, aes(y = exp(pred)), size = 1) + theme_bw()+scale_colour_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("N","PN"),labels=c("Rich adult","Poor adult")) + theme(legend.position="bottom", legend.box = "horizontal", plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ labs(x = "OLSresid", y = "Total fecundity (total number of eggs)")+scale_fill_manual(values=c("black","red"), name="fill",guide=FALSE)
EggsOLS




Fit <- glm.nb(eggs~TreatAdult+poids,na.action=na.fail,data=Fecond)
Fit

newdat = data.frame(TreatAdult = Fecond$TreatAdult,poids=Fecond$poids,eggs=Fecond$eggs)

head(newdat)

pred = predict(Fit, newdata = newdat, level = 0, interval = "confidence")

newdat<-cbind(newdat,pred)

names(newdat)
newdat$lwr
head(newdat)

des = model.matrix(formula("~TreatAdult+poids"), newdat)

predvar = diag( des %*% vcov(Fit) %*% t(des) )


newdat$lower = with(newdat, pred - 2*sqrt(predvar) )
newdat$upper = with(newdat, pred + 2*sqrt(predvar) )


names(newdat)
head(newdat)


EggsMass<-ggplot(newdat, aes(x = poids, y = eggs, color = TreatAdult)) + geom_point(aes(x=poids, y = eggs),size=4) + geom_ribbon(data = newdat, aes(y = NULL, ymin = exp(lower), ymax = exp(upper),color = NULL, fill = TreatAdult), alpha = .15) + geom_line(data = newdat, aes(y = exp(pred)), size = 1) + theme_bw()+scale_colour_manual(values=c("black","red"),name="Feeding treatment", breaks=c("N","PN"),labels=c("Rich adult","Poor adult")) + theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ labs(x = "Mass (mg)", y = "Total fecundity (total number of eggs)")+scale_fill_manual(values=c("black","red"), name="fill",guide=FALSE)

EggsMass




Fit <- glm.nb(eggs~TreatAdult+MSL,na.action=na.fail,data=Fecond)
Fit

newdat = data.frame(TreatAdult = Fecond$TreatAdult,MSL=Fecond$MSL,eggs=Fecond$eggs)

head(newdat)

pred = predict(Fit, newdata = newdat, level = 0, interval = "confidence")

newdat<-cbind(newdat,pred)

names(newdat)
newdat$lwr
head(newdat)

des = model.matrix(formula("~TreatAdult+MSL"), newdat)

predvar = diag( des %*% vcov(Fit) %*% t(des) )


newdat$lower = with(newdat, pred - 2*sqrt(predvar) )
newdat$upper = with(newdat, pred + 2*sqrt(predvar) )


names(newdat)
head(newdat)





EggsSMI<-ggplot(newdat, aes(x = MSL, y = eggs, color = TreatAdult)) + geom_point(aes(x=MSL, y = eggs),size=4) + geom_ribbon(data = newdat, aes(y = NULL, ymin = exp(lower), ymax = exp(upper),color = NULL, fill = TreatAdult), alpha = .15) + geom_line(data = newdat, aes(y = exp(pred)), size = 1) + theme_bw()+scale_colour_manual(values=c("black","red"),name="Feeding treatment", breaks=c("N","PN"),labels=c("Rich adult","Poor adult")) + theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ labs(x = "SMI (mg)", y = "Total fecundity (total number of eggs)")+scale_fill_manual(values=c("black","red"), name="fill",guide=FALSE)





Fit <- glm.nb(eggs~TreatAdult+moyLong,na.action=na.fail,data=Fecond)
Fit

newdat = data.frame(TreatAdult = Fecond$TreatAdult,moyLong=Fecond$moyLong,eggs=Fecond$eggs)

head(newdat)

pred = predict(Fit, newdata = newdat, level = 0, interval = "confidence")

newdat<-cbind(newdat,pred)

names(newdat)
newdat$lwr
head(newdat)

des = model.matrix(formula("~TreatAdult+moyLong"), newdat)

predvar = diag( des %*% vcov(Fit) %*% t(des) )


newdat$lower = with(newdat, pred - 2*sqrt(predvar) )
newdat$upper = with(newdat, pred + 2*sqrt(predvar) )


names(newdat)
head(newdat)





EggsLENGTH<-ggplot(newdat, aes(x = moyLong, y = eggs, color = TreatAdult)) + geom_point(aes(x=moyLong, y = eggs),size=4) + geom_ribbon(data = newdat, aes(y = NULL, ymin = exp(lower), ymax = exp(upper),color = NULL, fill = TreatAdult), alpha = .15) + geom_line(data = newdat, aes(y = exp(pred)), size = 1) + theme_bw()+scale_colour_manual(values=c("black","red"),name="Feeding treatment", breaks=c("N","PN"),labels=c("Rich adult","Poor adult")) + theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ labs(x = "Elytron length (mm)", y = "Total fecundity (total number of eggs)")+scale_fill_manual(values=c("black","red"), name="fill",guide=FALSE)



EggsLENGTH

EggsOLS
EggsMass
EggsSMI
Eggs+ theme(legend.position="none")


library(gtable)
library(gridExtra)
#install.packages("ggpubr")
library(ggpubr)
library(grid)

legend = gtable_filter(ggplotGrob(EggsOLS), "guide-box")



png("/home/mark/Dropbox/condition/ANALYSE/TotalFecondTraitOLSMASSSMI.png",width=6300, height=2970,res=300)

grid.arrange(arrangeGrob(EggsOLS + theme(legend.position="none"), EggsMass + theme(legend.position="none"),EggsSMI + theme(legend.position="none"),nrow=1),legend,widths=unit.c(unit(1, "npc") - legend$width, legend$width))

dev.off()




png("/home/mark/Dropbox/condition/ANALYSE/TotalFecondTraitOLS.png",width=2100, height=2100,res=300)
EggsOLS
dev.off()


EggsOLS
EggsMass
EggsSMI

Eggs+ theme(legend.position="none")


lay <- rbind(c(1,1,1),
             c(1,2,3))


gs<-grid.arrange(arrangeGrob(EggsOLS + theme(legend.position="none"), EggsMass + theme(legend.position="none"),EggsSMI + theme(legend.position="none"),nrow=1),legend,widths=unit.c(unit(1, "npc") - legend$width, legend$width))



grid.arrange(Eggs+ theme(legend.position="none"),gs, layout_matrix = rbind(c(1),c(2)))



legend = gtable_filter(ggplotGrob(EggsOLS), "guide-box")


png("/home/mark/Dropbox/condition/ANALYSE/TotalFecondTraitAll.png",width=2333.333, height=1666.667,res=100)

pushViewport(viewport(layout = grid.layout(19, 6)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
legend$vp = viewport(layout.pos.row = 19, layout.pos.col = 2:5)
grid.draw(legend)
print(Eggs+ theme(legend.position="none"), vp = vplayout(1:8, 2:5))
print(EggsOLS + theme(legend.position="none"), vp = vplayout(9:18, 1:2))
print(EggsMass + theme(legend.position="none"), vp = vplayout(9:18, 3:4))
print(EggsSMI + theme(legend.position="none"), vp = vplayout(9:18, 5:6))

dev.off()



png("/home/mark/Dropbox/condition/ANALYSE/TotalFecondTraitAllCoef.png",width=2333.333, height=1666.667,res=200)

pushViewport(viewport(layout = grid.layout(19, 6)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
legend$vp = viewport(layout.pos.row = 19, layout.pos.col = 2:5)
grid.draw(legend)
print(Eggs+ theme(legend.position="none"), vp = vplayout(1:9, 1:4))
print(p+ggtitle("b."), vp = vplayout(1:9, 5:6))
print(EggsOLS + theme(legend.position="none"), vp = vplayout(10:18, 1:2))
print(EggsMass + theme(legend.position="none"), vp = vplayout(10:18, 3:4))
print(EggsSMI + theme(legend.position="none"), vp = vplayout(10:18, 5:6))

dev.off()



legend = gtable_filter(ggplotGrob(EggsOLS), "guide-box")


png("/home/mark/Dropbox/condition/ANALYSE/TotalFecondTraitCoef_New.png",width=2500, height=1500,res=150)

pushViewport(viewport(layout = grid.layout(31, 30)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
legend$vp = viewport(layout.pos.row = 30, layout.pos.col = 11:29)
grid.draw(legend)
print(p_new+ggtitle("a."), vp = vplayout(2:29, 2:9))
print(EggsLENGTH+ggtitle("b.") + theme(legend.position="none"), vp = vplayout(2:15, 11:19))
print(EggsMass+ggtitle("c.") + theme(legend.position="none"), vp = vplayout(2:15, 21:29))
print(EggsOLS+ggtitle("d.") + theme(legend.position="none"), vp = vplayout(16:29, 11:19))
print(EggsSMI+ggtitle("e.") + theme(legend.position="none"), vp = vplayout(16:29, 21:29))
#print(Eggs+ggtitle("f.")+ theme(legend.position="none"), vp = vplayout(15:21, 3:6))
dev.off()





p

g0<-ggplot(Fecond,aes(x=moyLong,y=eggs,color=TreatAdult))
glmlong<-g0+geom_point()+geom_smooth(method="glm.nb", se=TRUE)

g0<-ggplot(Fecond,aes(x=poids,y=eggs,color=TreatAdult))
glmpoids<-g0+geom_point()+geom_smooth(method="glm.nb", se=TRUE)


g0<-ggplot(Fecond,aes(x=volume,y=eggs,color=TreatAdult))
glmvolume<-g0+geom_point()+geom_smooth(method="glm.nb", se=TRUE)


g0<-ggplot(Fecond,aes(x=OLSresidue,y=eggs,color=TreatAdult))
glmOLS<-g0+geom_point()+geom_smooth(method="glm.nb", se=TRUE)


g0<-ggplot(Fecond,aes(x=MSL,y=eggs,color=TreatAdult))
glmMSL<-g0+geom_point()+geom_smooth(method="glm.nb", se=TRUE)


g0<-ggplot(Fecond,aes(x=VSL,y=eggs,color=TreatAdult))
glmVSL<-g0+geom_point()+geom_smooth(method="glm.nb", se=TRUE)




(glmlong<-glmlong+scale_colour_manual(values=c("black","red"),name="Adult food treatment", breaks=c("N","PN"),labels=c("R.A.","P.A.")) + theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ labs(x = "Elytron length (mm)", y = "Total fecundity (total number of eggs)")+theme_bw()+ggtitle("Elytron length"))

(glmpoids<-glmpoids+scale_colour_manual(values=c("black","red"),name="Adult food treatment", breaks=c("N","PN"),labels=c("R.A.","P.A.")) + theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ labs(x = "Mass (mg)", y = "Total fecundity (total number of eggs)")+theme_bw()+ggtitle("Mass"))

(glmvolume<-glmvolume+scale_colour_manual(values=c("black","red"),name="Adult food treatment", breaks=c("N","PN"),labels=c("R.A.","P.A.")) + theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ labs(x = "Volume (mm3)", y = "Total fecundity (total number of eggs)")+theme_bw()+ggtitle("Volume"))

(glmOLS<-glmOLS+scale_colour_manual(values=c("black","red"),name="Adult food treatment", breaks=c("N","PN"),labels=c("R.A.","P.A.")) + theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ labs(x = "OLSresid", y = "Total fecundity (total number of eggs)")+theme_bw()+ggtitle("OLSresid"))


(glmMSL<-glmMSL+scale_colour_manual(values=c("black","red"),name="Adult food treatment", breaks=c("N","PN"),labels=c("R.A.","P.A.")) + theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ labs(x = "MSL (mm)", y = "Total fecundity (total number of eggs)")+theme_bw()+ggtitle("MSL (mm)"))


(glmVSL<-glmVSL+scale_colour_manual(values=c("black","red"),name="Adult food treatment", breaks=c("N","PN"),labels=c("R.A.","P.A.")) + theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ labs(x = "SVI (mm3)", y = "Total fecundity (total number of eggs)")+theme_bw()+ggtitle("SVI"))


library(gtable)
library(gridExtra)
library(ggpubr)
library(grid)

legend = gtable_filter(ggplotGrob(glmlong), "guide-box")

png("/home/mark/Dropbox/condition/ANALYSE/TotalFecondTraitBCI.png",width=2100, height=2970,res=300)

grid.arrange(arrangeGrob(glmlong + theme(legend.position="none"), glmpoids + theme(legend.position="none"),glmvolume + theme(legend.position="none"),glmOLS + theme(legend.position="none"),glmMSL + theme(legend.position="none"),glmVSL + theme(legend.position="none")),legend,widths=unit.c(unit(1, "npc") - legend$width, legend$width),nrow=1)


dev.off()



names(Fecond)


hist(Fecond$reproduction.1)


library(pscl)

M1.Infl <- zeroinfl(reproduction.1 ~ TreatLarve*TreatAdult*moyLong | TreatLarve*TreatAdult*moyLong,data = Fecond, dist = "negbin", EM = TRUE,na.action = na.fail)
M2.Infl <- zeroinfl(reproduction.1 ~ TreatLarve*TreatAdult*poids | TreatLarve*TreatAdult*poids,data = Fecond, dist = "negbin", EM = TRUE,na.action = na.fail)
M3.Infl <- zeroinfl(reproduction.1 ~ TreatLarve*TreatAdult*volume | TreatLarve*TreatAdult*volume,data = Fecond, dist = "negbin", EM = TRUE,na.action = na.fail)
M4.Infl <- zeroinfl(reproduction.1 ~ TreatLarve*TreatAdult*OLSresidue | TreatLarve*TreatAdult*OLSresidue,data = Fecond, dist = "negbin", EM = TRUE,na.action = na.fail)
M5.Infl <- zeroinfl(reproduction.1 ~ TreatLarve*TreatAdult*MSL | TreatLarve*TreatAdult*MSL,data = Fecond, dist = "negbin", EM = TRUE,na.action = na.fail)
M6.Infl <- zeroinfl(reproduction.1 ~ TreatLarve*TreatAdult*VSL | TreatLarve*TreatAdult*VSL,data = Fecond, dist = "negbin", EM = TRUE,na.action = na.fail)





(gg1.zinfl <- dredge(M1.Infl,rank="AICc", extra="R^2"))
(gg2.zinfl <- dredge(M2.Infl,rank="AICc", extra="R^2"))
(gg3.zinfl <- dredge(M3.Infl,rank="AICc", extra="R^2"))
(gg4.zinfl <- dredge(M4.Infl,rank="AICc", extra="R^2"))
(gg5.zinfl <- dredge(M5.Infl,rank="AICc", extra="R^2"))
(gg6.zinfl <- dredge(M6.Infl,rank="AICc", extra="R^2"))

head(gg1.zinfl,10)
head(gg2.zinfl,1)
head(gg3.zinfl,1)
head(gg4.zinfl,1)
head(gg5.zinfl,1)
head(gg6.zinfl,1)

head(gg4.zinfl,20)



gg4.zinfl
model.avg(gg4.zinfl)
confset.95p <- get.models(gg4.zinfl, cumsum(weight) <= 0.95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

summary(M4.Infl)

M4a.Infl <- zeroinfl(reproduction.1 ~ OLSresidue |OLSresidue,data = Fecond, dist = "negbin", EM = TRUE,na.action = na.fail)

summary(M4a.Infl)


rootogram <- function(obj, max = NULL, ...) {
   y <- model.response(model.frame(obj))
   tab <- table(factor(y, levels = 0:max(y)))
   tab2 <- colSums(predprob(obj))
   if(is.null(max)) {
     max <- if(all(tab2 >= 1)) max(y) else max(ceiling(mean(y)), min(which(tab2 < 1)) - 1)
   }
   max <- min(max, length(tab) - 1) + 1
   obsrvd <- sqrt(tab[1:max])
   expctd <- sqrt(tab2[1:max])
   res <- obsrvd - expctd
   x <- barplot(obsrvd, offset = -res, xlab = "Count", ylab = "sqrt(Frequency)")
   lines(x, expctd, col = 2, type = ifelse(max > 25, "l", "b"), pch = 19)
   abline(h = 0)
   invisible(cbind(observed = tab, expected = tab2))
}

rootogram(M4a.Infl)




#MuMIN does not work with vglm so I do it manually


FecondTrunc<-Fecond[which(Fecond$reproduction.1 > 0),]

#install.packages("VGAM")
library(VGAM)
m4a.trunc <- vglm(reproduction.1 ~ TreatLarve*TreatAdult*OLSresidue, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4b.trunc <- vglm(reproduction.1 ~ TreatLarve+OLSresidue, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4c.trunc <- vglm(reproduction.1 ~ OLSresidue, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4d.trunc <- vglm(reproduction.1 ~ TreatLarve*OLSresidue, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4e.trunc <- vglm(reproduction.1 ~ TreatLarve+TreatAdult+OLSresidue, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4f.trunc <- vglm(reproduction.1 ~ TreatAdult+OLSresidue, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4g.trunc <- vglm(reproduction.1 ~ TreatLarve*TreatAdult+OLSresidue, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4h.trunc <- vglm(reproduction.1 ~ TreatAdult+TreatLarve*OLSresidue, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4i.trunc <- vglm(reproduction.1 ~ TreatLarve+TreatAdult*OLSresidue, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4j.trunc <- vglm(reproduction.1 ~ TreatAdult*OLSresidue, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4k.trunc <- vglm(reproduction.1 ~ TreatLarve*TreatAdult+TreatLarve*OLSresidue, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4l.trunc <- vglm(reproduction.1 ~ TreatLarve*OLSresidue+TreatAdult*OLSresidue, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4m.trunc <- vglm(reproduction.1 ~ TreatLarve*TreatAdult+TreatAdult*OLSresidue, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4n.trunc <- vglm(reproduction.1 ~ TreatLarve*TreatAdult+TreatLarve*OLSresidue+TreatAdult*OLSresidue, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4o.trunc <- vglm(reproduction.1 ~ TreatLarve, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4p.trunc <- vglm(reproduction.1 ~ 1, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4q.trunc <- vglm(reproduction.1 ~ TreatLarve + TreatAdult, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4r.trunc <- vglm(reproduction.1 ~ TreatAdult, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4s.trunc <- vglm(reproduction.1 ~ TreatLarve*TreatAdult, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)



m4aAICc<-AICc(m4a.trunc)
m4bAICc<-AICc(m4b.trunc)
m4cAICc<-AICc(m4c.trunc)
m4dAICc<-AICc(m4d.trunc)
m4eAICc<-AICc(m4e.trunc)
m4fAICc<-AICc(m4f.trunc)
m4gAICc<-AICc(m4g.trunc)
m4hAICc<-AICc(m4h.trunc)
m4iAICc<-AICc(m4i.trunc)
m4jAICc<-AICc(m4j.trunc)
m4kAICc<-AICc(m4k.trunc)
m4lAICc<-AICc(m4l.trunc)
m4mAICc<-AICc(m4m.trunc)
m4nAICc<-AICc(m4n.trunc)
m4oAICc<-AICc(m4o.trunc)
m4pAICc<-AICc(m4p.trunc)
m4qAICc<-AICc(m4q.trunc)
m4rAICc<-AICc(m4r.trunc)
m4sAICc<-AICc(m4s.trunc)


AIC<-c(m4aAICc,m4bAICc,m4cAICc,m4dAICc,m4eAICc,m4fAICc,m4gAICc,m4hAICc,m4iAICc,m4jAICc,m4kAICc,m4lAICc,m4mAICc,m4nAICc,m4oAICc,m4pAICc,m4qAICc,m4rAICc,m4sAICc)


LogLikm4a<-logLik(m4a.trunc)
LogLikm4b<-logLik(m4b.trunc)
LogLikm4c<-logLik(m4c.trunc)
LogLikm4d<-logLik(m4d.trunc)
LogLikm4e<-logLik(m4e.trunc)
LogLikm4f<-logLik(m4f.trunc)
LogLikm4g<-logLik(m4g.trunc)
LogLikm4h<-logLik(m4h.trunc)
LogLikm4i<-logLik(m4i.trunc)
LogLikm4j<-logLik(m4j.trunc)
LogLikm4k<-logLik(m4k.trunc)
LogLikm4l<-logLik(m4l.trunc)
LogLikm4m<-logLik(m4m.trunc)
LogLikm4n<-logLik(m4n.trunc)
LogLikm4o<-logLik(m4o.trunc)
LogLikm4p<-logLik(m4p.trunc)
LogLikm4q<-logLik(m4q.trunc)
LogLikm4r<-logLik(m4r.trunc)
LogLikm4s<-logLik(m4s.trunc)


LogLik<-c(LogLikm4a,LogLikm4b,LogLikm4c,LogLikm4d,LogLikm4e,LogLikm4f,LogLikm4g,LogLikm4h,LogLikm4i,LogLikm4j,LogLikm4k,LogLikm4l,LogLikm4m,LogLikm4n,LogLikm4o,LogLikm4p,LogLikm4q,LogLikm4r,LogLikm4s)

LogLik


km4a<-m4a.trunc@misc$p
km4b<-m4b.trunc@misc$p
km4c<-m4c.trunc@misc$p
km4d<-m4d.trunc@misc$p
km4e<-m4e.trunc@misc$p
km4f<-m4f.trunc@misc$p
km4g<-m4g.trunc@misc$p
km4h<-m4h.trunc@misc$p
km4i<-m4i.trunc@misc$p
km4j<-m4j.trunc@misc$p
km4k<-m4k.trunc@misc$p
km4l<-m4l.trunc@misc$p
km4m<-m4m.trunc@misc$p
km4n<-m4n.trunc@misc$p
km4o<-m4o.trunc@misc$p
km4p<-m4p.trunc@misc$p
km4q<-m4q.trunc@misc$p
km4r<-m4r.trunc@misc$p
km4s<-m4s.trunc@misc$p


k<-c(km4a,km4b,km4c,km4d,km4e,km4f,km4g,km4h,km4i,km4j,km4k,km4l,km4m,km4n,km4o,km4p,km4q,km4r,km4s)
k

formulam4a<-m4a.trunc@misc$formula
formulam4b<-m4b.trunc@misc$formula
formulam4c<-m4c.trunc@misc$formula
formulam4d<-m4d.trunc@misc$formula
formulam4e<-m4e.trunc@misc$formula
formulam4f<-m4f.trunc@misc$formula
formulam4g<-m4g.trunc@misc$formula
formulam4h<-m4h.trunc@misc$formula
formulam4i<-m4i.trunc@misc$formula
formulam4j<-m4j.trunc@misc$formula
formulam4k<-m4k.trunc@misc$formula
formulam4l<-m4l.trunc@misc$formula
formulam4m<-m4m.trunc@misc$formula
formulam4n<-m4n.trunc@misc$formula
formulam4o<-m4o.trunc@misc$formula
formulam4p<-m4p.trunc@misc$formula
formulam4q<-m4q.trunc@misc$formula
formulam4r<-m4r.trunc@misc$formula
formulam4s<-m4s.trunc@misc$formula


formula<-c(formulam4a,formulam4b,formulam4c,formulam4d,formulam4e,formulam4f,formulam4g,formulam4h,formulam4i,formulam4j,formulam4k,formulam4l,formulam4m,formulam4n,formulam4o,formulam4p,formulam4q,formulam4r,formulam4s)


formula

formula<-vapply(formula, paste, collapse = ", ", character(1L))


Table<-(data.frame(cbind(formula,k,LogLik,AIC)))
Table

Table$formula


write.table(Table,"zeroTrunc.txt",sep="\t",row.names=F,quote=F) 

summary(m4a.trunc)




#install.packages("VGAM")
library(VGAM)
m4a.trunc <- vglm(reproduction.1 ~ TreatLarve*TreatAdult*MSL, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4b.trunc <- vglm(reproduction.1 ~ TreatLarve+MSL, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4c.trunc <- vglm(reproduction.1 ~ MSL, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4d.trunc <- vglm(reproduction.1 ~ TreatLarve*MSL, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4e.trunc <- vglm(reproduction.1 ~ TreatLarve+TreatAdult+MSL, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4f.trunc <- vglm(reproduction.1 ~ TreatAdult+MSL, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4g.trunc <- vglm(reproduction.1 ~ TreatLarve*TreatAdult+MSL, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4h.trunc <- vglm(reproduction.1 ~ TreatAdult+TreatLarve*MSL, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4i.trunc <- vglm(reproduction.1 ~ TreatLarve+TreatAdult*MSL, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4j.trunc <- vglm(reproduction.1 ~ TreatAdult*MSL, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4k.trunc <- vglm(reproduction.1 ~ TreatLarve*TreatAdult+TreatLarve*MSL, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4l.trunc <- vglm(reproduction.1 ~ TreatLarve*MSL+TreatAdult*MSL, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4m.trunc <- vglm(reproduction.1 ~ TreatLarve*TreatAdult+TreatAdult*MSL, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4n.trunc <- vglm(reproduction.1 ~ TreatLarve*TreatAdult+TreatLarve*MSL+TreatAdult*MSL, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4o.trunc <- vglm(reproduction.1 ~ TreatLarve, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4p.trunc <- vglm(reproduction.1 ~ 1, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4q.trunc <- vglm(reproduction.1 ~ TreatLarve + TreatAdult, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4r.trunc <- vglm(reproduction.1 ~ TreatAdult, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4s.trunc <- vglm(reproduction.1 ~ TreatLarve*TreatAdult, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)



m4aAICc<-AICc(m4a.trunc)
m4bAICc<-AICc(m4b.trunc)
m4cAICc<-AICc(m4c.trunc)
m4dAICc<-AICc(m4d.trunc)
m4eAICc<-AICc(m4e.trunc)
m4fAICc<-AICc(m4f.trunc)
m4gAICc<-AICc(m4g.trunc)
m4hAICc<-AICc(m4h.trunc)
m4iAICc<-AICc(m4i.trunc)
m4jAICc<-AICc(m4j.trunc)
m4kAICc<-AICc(m4k.trunc)
m4lAICc<-AICc(m4l.trunc)
m4mAICc<-AICc(m4m.trunc)
m4nAICc<-AICc(m4n.trunc)
m4oAICc<-AICc(m4o.trunc)
m4pAICc<-AICc(m4p.trunc)
m4qAICc<-AICc(m4q.trunc)
m4rAICc<-AICc(m4r.trunc)
m4sAICc<-AICc(m4s.trunc)


AIC<-c(m4aAICc,m4bAICc,m4cAICc,m4dAICc,m4eAICc,m4fAICc,m4gAICc,m4hAICc,m4iAICc,m4jAICc,m4kAICc,m4lAICc,m4mAICc,m4nAICc,m4oAICc,m4pAICc,m4qAICc,m4rAICc,m4sAICc)


LogLikm4a<-logLik(m4a.trunc)
LogLikm4b<-logLik(m4b.trunc)
LogLikm4c<-logLik(m4c.trunc)
LogLikm4d<-logLik(m4d.trunc)
LogLikm4e<-logLik(m4e.trunc)
LogLikm4f<-logLik(m4f.trunc)
LogLikm4g<-logLik(m4g.trunc)
LogLikm4h<-logLik(m4h.trunc)
LogLikm4i<-logLik(m4i.trunc)
LogLikm4j<-logLik(m4j.trunc)
LogLikm4k<-logLik(m4k.trunc)
LogLikm4l<-logLik(m4l.trunc)
LogLikm4m<-logLik(m4m.trunc)
LogLikm4n<-logLik(m4n.trunc)
LogLikm4o<-logLik(m4o.trunc)
LogLikm4p<-logLik(m4p.trunc)
LogLikm4q<-logLik(m4q.trunc)
LogLikm4r<-logLik(m4r.trunc)
LogLikm4s<-logLik(m4s.trunc)


LogLik<-c(LogLikm4a,LogLikm4b,LogLikm4c,LogLikm4d,LogLikm4e,LogLikm4f,LogLikm4g,LogLikm4h,LogLikm4i,LogLikm4j,LogLikm4k,LogLikm4l,LogLikm4m,LogLikm4n,LogLikm4o,LogLikm4p,LogLikm4q,LogLikm4r,LogLikm4s)

LogLik


km4a<-m4a.trunc@misc$p
km4b<-m4b.trunc@misc$p
km4c<-m4c.trunc@misc$p
km4d<-m4d.trunc@misc$p
km4e<-m4e.trunc@misc$p
km4f<-m4f.trunc@misc$p
km4g<-m4g.trunc@misc$p
km4h<-m4h.trunc@misc$p
km4i<-m4i.trunc@misc$p
km4j<-m4j.trunc@misc$p
km4k<-m4k.trunc@misc$p
km4l<-m4l.trunc@misc$p
km4m<-m4m.trunc@misc$p
km4n<-m4n.trunc@misc$p
km4o<-m4o.trunc@misc$p
km4p<-m4p.trunc@misc$p
km4q<-m4q.trunc@misc$p
km4r<-m4r.trunc@misc$p
km4s<-m4s.trunc@misc$p


k<-c(km4a,km4b,km4c,km4d,km4e,km4f,km4g,km4h,km4i,km4j,km4k,km4l,km4m,km4n,km4o,km4p,km4q,km4r,km4s)
k

formulam4a<-m4a.trunc@misc$formula
formulam4b<-m4b.trunc@misc$formula
formulam4c<-m4c.trunc@misc$formula
formulam4d<-m4d.trunc@misc$formula
formulam4e<-m4e.trunc@misc$formula
formulam4f<-m4f.trunc@misc$formula
formulam4g<-m4g.trunc@misc$formula
formulam4h<-m4h.trunc@misc$formula
formulam4i<-m4i.trunc@misc$formula
formulam4j<-m4j.trunc@misc$formula
formulam4k<-m4k.trunc@misc$formula
formulam4l<-m4l.trunc@misc$formula
formulam4m<-m4m.trunc@misc$formula
formulam4n<-m4n.trunc@misc$formula
formulam4o<-m4o.trunc@misc$formula
formulam4p<-m4p.trunc@misc$formula
formulam4q<-m4q.trunc@misc$formula
formulam4r<-m4r.trunc@misc$formula
formulam4s<-m4s.trunc@misc$formula


formula<-c(formulam4a,formulam4b,formulam4c,formulam4d,formulam4e,formulam4f,formulam4g,formulam4h,formulam4i,formulam4j,formulam4k,formulam4l,formulam4m,formulam4n,formulam4o,formulam4p,formulam4q,formulam4r,formulam4s)


formula

formula<-vapply(formula, paste, collapse = ", ", character(1L))


Table<-(data.frame(cbind(formula,k,LogLik,AIC)))
Table

Table$formula


write.table(Table,"zeroTruncMSL.txt",sep="\t",row.names=F,quote=F) 




#install.packages("VGAM")
library(VGAM)
m4a.trunc <- vglm(reproduction.1 ~ TreatLarve*TreatAdult*poids, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4b.trunc <- vglm(reproduction.1 ~ TreatLarve+poids, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4c.trunc <- vglm(reproduction.1 ~ poids, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4d.trunc <- vglm(reproduction.1 ~ TreatLarve*poids, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4e.trunc <- vglm(reproduction.1 ~ TreatLarve+TreatAdult+poids, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4f.trunc <- vglm(reproduction.1 ~ TreatAdult+poids, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4g.trunc <- vglm(reproduction.1 ~ TreatLarve*TreatAdult+poids, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4h.trunc <- vglm(reproduction.1 ~ TreatAdult+TreatLarve*poids, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4i.trunc <- vglm(reproduction.1 ~ TreatLarve+TreatAdult*poids, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4j.trunc <- vglm(reproduction.1 ~ TreatAdult*poids, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4k.trunc <- vglm(reproduction.1 ~ TreatLarve*TreatAdult+TreatLarve*poids, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4l.trunc <- vglm(reproduction.1 ~ TreatLarve*poids+TreatAdult*poids, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4m.trunc <- vglm(reproduction.1 ~ TreatLarve*TreatAdult+TreatAdult*poids, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4n.trunc <- vglm(reproduction.1 ~ TreatLarve*TreatAdult+TreatLarve*poids+TreatAdult*poids, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4o.trunc <- vglm(reproduction.1 ~ TreatLarve, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4p.trunc <- vglm(reproduction.1 ~ 1, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4q.trunc <- vglm(reproduction.1 ~ TreatLarve + TreatAdult, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4r.trunc <- vglm(reproduction.1 ~ TreatAdult, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4s.trunc <- vglm(reproduction.1 ~ TreatLarve*TreatAdult, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)



m4aAICc<-AICc(m4a.trunc)
m4bAICc<-AICc(m4b.trunc)
m4cAICc<-AICc(m4c.trunc)
m4dAICc<-AICc(m4d.trunc)
m4eAICc<-AICc(m4e.trunc)
m4fAICc<-AICc(m4f.trunc)
m4gAICc<-AICc(m4g.trunc)
m4hAICc<-AICc(m4h.trunc)
m4iAICc<-AICc(m4i.trunc)
m4jAICc<-AICc(m4j.trunc)
m4kAICc<-AICc(m4k.trunc)
m4lAICc<-AICc(m4l.trunc)
m4mAICc<-AICc(m4m.trunc)
m4nAICc<-AICc(m4n.trunc)
m4oAICc<-AICc(m4o.trunc)
m4pAICc<-AICc(m4p.trunc)
m4qAICc<-AICc(m4q.trunc)
m4rAICc<-AICc(m4r.trunc)
m4sAICc<-AICc(m4s.trunc)


AIC<-c(m4aAICc,m4bAICc,m4cAICc,m4dAICc,m4eAICc,m4fAICc,m4gAICc,m4hAICc,m4iAICc,m4jAICc,m4kAICc,m4lAICc,m4mAICc,m4nAICc,m4oAICc,m4pAICc,m4qAICc,m4rAICc,m4sAICc)


LogLikm4a<-logLik(m4a.trunc)
LogLikm4b<-logLik(m4b.trunc)
LogLikm4c<-logLik(m4c.trunc)
LogLikm4d<-logLik(m4d.trunc)
LogLikm4e<-logLik(m4e.trunc)
LogLikm4f<-logLik(m4f.trunc)
LogLikm4g<-logLik(m4g.trunc)
LogLikm4h<-logLik(m4h.trunc)
LogLikm4i<-logLik(m4i.trunc)
LogLikm4j<-logLik(m4j.trunc)
LogLikm4k<-logLik(m4k.trunc)
LogLikm4l<-logLik(m4l.trunc)
LogLikm4m<-logLik(m4m.trunc)
LogLikm4n<-logLik(m4n.trunc)
LogLikm4o<-logLik(m4o.trunc)
LogLikm4p<-logLik(m4p.trunc)
LogLikm4q<-logLik(m4q.trunc)
LogLikm4r<-logLik(m4r.trunc)
LogLikm4s<-logLik(m4s.trunc)


LogLik<-c(LogLikm4a,LogLikm4b,LogLikm4c,LogLikm4d,LogLikm4e,LogLikm4f,LogLikm4g,LogLikm4h,LogLikm4i,LogLikm4j,LogLikm4k,LogLikm4l,LogLikm4m,LogLikm4n,LogLikm4o,LogLikm4p,LogLikm4q,LogLikm4r,LogLikm4s)

LogLik


km4a<-m4a.trunc@misc$p
km4b<-m4b.trunc@misc$p
km4c<-m4c.trunc@misc$p
km4d<-m4d.trunc@misc$p
km4e<-m4e.trunc@misc$p
km4f<-m4f.trunc@misc$p
km4g<-m4g.trunc@misc$p
km4h<-m4h.trunc@misc$p
km4i<-m4i.trunc@misc$p
km4j<-m4j.trunc@misc$p
km4k<-m4k.trunc@misc$p
km4l<-m4l.trunc@misc$p
km4m<-m4m.trunc@misc$p
km4n<-m4n.trunc@misc$p
km4o<-m4o.trunc@misc$p
km4p<-m4p.trunc@misc$p
km4q<-m4q.trunc@misc$p
km4r<-m4r.trunc@misc$p
km4s<-m4s.trunc@misc$p


k<-c(km4a,km4b,km4c,km4d,km4e,km4f,km4g,km4h,km4i,km4j,km4k,km4l,km4m,km4n,km4o,km4p,km4q,km4r,km4s)
k

formulam4a<-m4a.trunc@misc$formula
formulam4b<-m4b.trunc@misc$formula
formulam4c<-m4c.trunc@misc$formula
formulam4d<-m4d.trunc@misc$formula
formulam4e<-m4e.trunc@misc$formula
formulam4f<-m4f.trunc@misc$formula
formulam4g<-m4g.trunc@misc$formula
formulam4h<-m4h.trunc@misc$formula
formulam4i<-m4i.trunc@misc$formula
formulam4j<-m4j.trunc@misc$formula
formulam4k<-m4k.trunc@misc$formula
formulam4l<-m4l.trunc@misc$formula
formulam4m<-m4m.trunc@misc$formula
formulam4n<-m4n.trunc@misc$formula
formulam4o<-m4o.trunc@misc$formula
formulam4p<-m4p.trunc@misc$formula
formulam4q<-m4q.trunc@misc$formula
formulam4r<-m4r.trunc@misc$formula
formulam4s<-m4s.trunc@misc$formula


formula<-c(formulam4a,formulam4b,formulam4c,formulam4d,formulam4e,formulam4f,formulam4g,formulam4h,formulam4i,formulam4j,formulam4k,formulam4l,formulam4m,formulam4n,formulam4o,formulam4p,formulam4q,formulam4r,formulam4s)


formula

formula<-vapply(formula, paste, collapse = ", ", character(1L))


Table<-(data.frame(cbind(formula,k,LogLik,AIC)))
Table

Table$formula


write.table(Table,"zeroTruncPoids.txt",sep="\t",row.names=F,quote=F) 



m4c.truncOLS <- vglm(reproduction.1 ~ OLSresidue, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4c.truncMASS <- vglm(reproduction.1 ~ poids, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4c.truncSMI <- vglm(reproduction.1 ~ MSL, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)

m4a.trunc <- vglm(reproduction.1 ~ TreatLarve*TreatAdult*OLSresidue, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)
#

#library(devtools)
#withr::with_makevars(c(PKG_LIBS="-liconv"), install_github("jslefche/piecewiseSEM@devel", build_vignette = TRUE), assignment="+=")

#install_github("r-lib/rlang@master", build_vignette = TRUE)#
#

## Load library
library(piecewiseSEM)#
rsquared(m4a.trunc)


library(ipeglim)
m <-  ztpreg(formula=reproduction.1~1, ztrunc=TRUE, dist="nbinom",data=FecondTrunc)
m
(fit<-summary(m, HT.est=TRUE))
mhat <- exp(fit$cfs)
print(c(fit$N, fit$cil, fit$ciu))

m2 <- vglm(reproduction.1 ~ OLSresidue, family = pospoisson(), data = FecondTrunc,na.action =na.omit)

m3 <- vglm(reproduction.1 ~ s(OLSresidue), family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)
summary(m3)

## change in deviance
(dLL <- 2 * (logLik(m4c.truncOLS) - logLik(m2)))
pchisq(dLL, df = 1, lower.tail = FALSE)
#Based on this, we would conclude that the negative binomial model is a better fit to the data

qtplot(m4c.truncOLS)





library(ggplot2)

output <- data.frame(resid = resid(m4c.truncOLS)[, 1], fitted = fitted(m4c.truncOLS))
ggplot(output, aes(fitted, resid)) + geom_jitter(position = position_jitter(width = 0.25), 
    alpha = 0.5) + stat_smooth(method = "loess")

m4c.truncOLS <- vglm(reproduction.1 ~ OLSresidue, family = posnegbinomial(), data = FecondTrunc,na.action =na.omit)
predict(m4c.truncOLS)
summary(m4c.truncOLS)

output <- data.frame(resid = resid(m4c.truncOLS)[, 1], fitted = fitted(m4c.truncOLS))
ggplot(output, aes(fitted, resid)) + geom_jitter(position = position_jitter(width = 0.25), 
    alpha = 0.5) + stat_smooth(method = "loess")

ggplot(output, aes(fitted, resid)) +
  geom_jitter(position=position_jitter(width=.25), alpha=.5) +
  stat_quantile(method="rq")

output <- within(output, {
  broken <- cut(fitted, hist(fitted, plot=FALSE)$breaks)
})

ggplot(output, aes(broken, resid)) +
 geom_boxplot() +
 geom_jitter(alpha=.25)


dput(round(coef(m4c.truncOLS),3))

f <- function(data, i, newdata) {
  require(VGAM)
  Fit <- vglm(formula = reproduction.1 ~ OLSresidue, family = posnegbinomial(),
    data = data[i, ], coefstart = c(3.061, 1.083, 1.482))
  mparams <- as.vector(t(coef(summary(Fit))[, 1:2]))
  yhat <- predict(Fit, newdata, type = "response")
  return(c(mparams, yhat))
}

## newdata for prediction
FecondTrunc$yhat <- predict(m4c.truncOLS, FecondTrunc, type = "response")

library(boot)
set.seed(10)
res <- boot(FecondTrunc, f, R = 1200, newdata = FecondTrunc, parallel = "snow", ncpus = 8)


## basic parameter estimates with percentile and bias adjusted CIs
parms <- t(sapply(c(1:nrow(FecondTrunc$yhat)), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "basic"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
    basicLL = basic[4], basicLL = basic[5]))
}))

## add row names
#row.names(parms) <- names(coef(m4c.truncOLS))
## print results
parms


## exponentiated parameter estimates with percentile and bias adjusted CIs
expparms <- t(sapply(1 + (1:nrow(FecondTrunc)), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "basic"), h = exp)
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
    basicLL = basic[4], basicLL = basic[5]))
}))

## add row names
row.names(expparms) <- names(coef(m4c.truncOLS))
## print results
expparms


ggplot(FecondTrunc, aes(x = OLSresidue, y = yhat))  +
  geom_point() +
  geom_line() 


## get the bootstrapped percentile CIs
yhat <- t(sapply(6 +(1:nrow(FecondTrunc$yhat)), function(i) {
  out <- boot.ci(res, index = i, type = c("perc"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5]))
}))


length(data.frame(yhat)$Est)
length(FecondTrunc$yhat)

length(data.frame(yhat)$Est)
length(FecondTrunc$yhat)


head(FecondTrunc)

head(FecondTrunc$yhat)


head(yhat)

## merge CIs with predicted values
FecondTrunc <- cbind(FecondTrunc, yhat)

head(FecondTrunc,20)



head(FecondTrunc)

## graph with CIs
ztnbOLS<-ggplot(FecondTrunc, aes(x = OLSresidue, y = yhat)) + geom_ribbon(aes(ymin = pLL, ymax = pUL), alpha = .25) + geom_line(size=2)+geom_point(aes(x = OLSresidue, y = reproduction.1),size=4) + labs(x = "OLSresid", y = "Number of eggs (first egg laying event)") + ggtitle("b. Zero-truncated negative binomial OLSresid") + theme_classic()+ theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
ztnbOLS


AICc(m4c.truncOLS)
AICc(m4d.trunc)
AICc(m4)
AICc(m5)

summary(m4c.truncOLS)

summary(m4c.truncMASS)


dput(round(coef(m4c.truncOLS),3))

f <- function(data, i, newdata) {
  require(VGAM)
  Fit <- vglm(formula = reproduction.1 ~ poids, family = posnegbinomial(),
    data = data[i, ], coefstart = c(2.565898, 1.039503, 0.004480))
  mparams <- as.vector(t(coef(summary(Fit))[, 1:2]))
  yhat <- predict(Fit, newdata, type = "response")
  return(c(mparams, yhat))
}


names(FecondTrunc)
## newdata for prediction
FecondTrunc$yhatMASS <- predict(m4c.truncMASS, FecondTrunc, type = "response")

library(boot)
set.seed(10)
res <- boot(FecondTrunc, f, R = 1200, newdata = FecondTrunc, parallel = "snow", ncpus = 8)
res

## basic parameter estimates with percentile and bias adjusted CIs
parms <- t(sapply(c(1:nrow(FecondTrunc$yhat)), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "basic"))
  with(out, c(EstMASS = t0, pLLMASS = percent[4], pULMASS = percent[5],
    basicLLMASS = basic[4], basicLLMASS = basic[5]))
}))

## add row names
#row.names(parms) <- names(coef(m4c.truncOLS))
## print results
parms


## exponentiated parameter estimates with percentile and bias adjusted CIs
expparms <- t(sapply(1 + (1:nrow(FecondTrunc)), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "basic"), h = exp)
  with(out, c(EstMASS = t0, pLLMASS = percent[4], pULMASS = percent[5],
    basicLLMASS = basic[4], basicLLMASS = basic[5]))
}))

## add row names
row.names(expparms) <- names(coef(m4c.truncMASS))
## print results
expparms


ggplot(FecondTrunc, aes(x = poids, y = yhatMASS))  +
  geom_point() +
  geom_line() 


## get the bootstrapped percentile CIs
yhatMASS <- t(sapply(6 +(1:nrow(FecondTrunc$yhatMASS)), function(i) {
  out <- boot.ci(res, index = i, type = c("perc"))
  with(out, c(EstMASS = t0, pLLMASS = percent[4], pULMASS = percent[5]))
}))


length(data.frame(yhatMASS)$EstMASS)
length(FecondTrunc$yhatMASS)

length(data.frame(yhatMASS)$EstMASS)
length(FecondTrunc$yhatMASS)


head(FecondTrunc)

head(FecondTrunc$yhatMASS)


head(yhatMASS)

## merge CIs with predicted values
FecondTrunc <- cbind(FecondTrunc, yhatMASS)

head(FecondTrunc,20)



head(FecondTrunc)

## graph with CIs
ztnbMASS<-ggplot(FecondTrunc, aes(x = poids, y = yhatMASS)) + geom_ribbon(aes(ymin = pLLMASS, ymax = pULMASS), alpha = .25) + geom_line(size=2)+geom_point(aes(x = poids, y = reproduction.1),size=4) + labs(x = "Mass (mg)", y = "Number of eggs (first egg laying event)") + ggtitle("d. Zero-truncated negative binomial mass") + theme_classic()+ theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
ztnbMASS





f <- function(data, i, newdata) {
  require(VGAM)
  Fit <- vglm(formula = reproduction.1 ~ MSL, family = posnegbinomial(),
    data = data[i, ], coefstart = c(2.565898, 1.039503, 0.004480))
  mparams <- as.vector(t(coef(summary(Fit))[, 1:2]))
  yhat <- predict(Fit, newdata, type = "response")
  return(c(mparams, yhat))
}


names(FecondTrunc)
## newdata for prediction
FecondTrunc$yhatSMI <- predict(m4c.truncSMI, FecondTrunc, type = "response")

library(boot)
set.seed(10)
res <- boot(FecondTrunc, f, R = 1200, newdata = FecondTrunc, parallel = "snow", ncpus = 8)
res

## basic parameter estimates with percentile and bias adjusted CIs
parms <- t(sapply(c(1:nrow(FecondTrunc$yhat)), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "basic"))
  with(out, c(EstSMI = t0, pLLSMI = percent[4], pULSMI = percent[5],
    basicLLSMI = basic[4], basicLLSMI = basic[5]))
}))

## add row names
#row.names(parms) <- names(coef(m4c.truncOLS))
## print results
parms


## exponentiated parameter estimates with percentile and bias adjusted CIs
expparms <- t(sapply(1 + (1:nrow(FecondTrunc)), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "basic"), h = exp)
  with(out, c(EstSMI = t0, pLLSMI = percent[4], pULSMI = percent[5],
    basicLLSMI = basic[4], basicLLSMI = basic[5]))
}))

## add row names
row.names(expparms) <- names(coef(m4c.truncSMI))
## print results
expparms


ggplot(FecondTrunc, aes(x = MSL, y = yhatSMI))  +
  geom_point() +
  geom_line() 


## get the bootstrapped percentile CIs
yhatSMI <- t(sapply(6 +(1:nrow(FecondTrunc$yhatSMI)), function(i) {
  out <- boot.ci(res, index = i, type = c("perc"))
  with(out, c(EstSMI = t0, pLLSMI = percent[4], pULSMI = percent[5]))
}))


length(data.frame(yhatSMI)$EstSMI)
length(FecondTrunc$yhatSMI)

length(data.frame(yhatSMI)$EstSMI)
length(FecondTrunc$yhatSMI)


head(FecondTrunc)

head(FecondTrunc$yhatSMI)


head(yhatSMI)

## merge CIs with predicted values
FecondTrunc <- cbind(FecondTrunc, yhatSMI)

head(FecondTrunc,20)



head(FecondTrunc)

## graph with CIs
ztnbSMI<-ggplot(FecondTrunc, aes(x = MSL, y = yhatSMI)) + geom_ribbon(aes(ymin = pLLSMI, ymax = pULSMI), alpha = .25) + geom_line(size=2)+geom_point(aes(x = MSL, y = reproduction.1),size=4) + labs(x = "SMI (mg)", y = "Number of eggs (first egg laying event)") + ggtitle("f. Zero-truncated negative binomial SMI") + theme_classic()+ theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
ztnbSMI






Fecond$ReprodBin<-ifelse(Fecond$reproduction.1<1,0,1)


library(MuMIn)
M1<-glm(ReprodBin~ TreatLarve*TreatAdult*OLSresidue,family=binomial,na.action=na.fail,data=Fecond)
ms1 <- dredge(M1)
ms1
model.avg(ms1)
confset.95p <- get.models(ms1, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)



write.table(ms1,"ReprodBin1stLay.txt",sep="\t",row.names=F,quote=F) 



M1<-glm(ReprodBin~ TreatLarve*TreatAdult*MSL,family=binomial,na.action=na.fail,data=Fecond)
ms1 <- dredge(M1)
ms1
model.avg(ms1)
confset.95p <- get.models(ms1, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)



write.table(ms1,"ReprodBin1stLayPoids.txt",sep="\t",row.names=F,quote=F) 

M1<-glm(ReprodBin~ TreatLarve*TreatAdult*poids,family=binomial,na.action=na.fail,data=Fecond)
ms1 <- dredge(M1)
ms1
model.avg(ms1)
confset.95p <- get.models(ms1, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)



write.table(ms1,"ReprodBin1stLayMSL.txt",sep="\t",row.names=F,quote=F) 



M1<-glm(ReprodBin~ TreatLarve*TreatAdult,family=binomial,na.action=na.fail,data=Fecond)
ms1 <- dredge(M1)
ms1
model.avg(ms1)
confset.95p <- get.models(ms1, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)
#

library(mgcv)

M1<-gam(ReprodBin~ s(OLSresidue),family=binomial,na.action=na.fail,data=Fecond)
summary(M1)
anova(M1)

ztnb

binomial_smooth <- function(...) {
  geom_smooth(method = "gam", method.args = list(family = "binomial"),color="black", ...)
}

p3<-ggplot(Fecond, aes(OLSresidue, ReprodBin)) + geom_point(size=4) + binomial_smooth(size=2)

bin<-p3 + labs(x = "OLSresid", y = "Probability of laying (first egg laying event)") + ggtitle("a. Binomial OLSresid")+ theme_classic()+ theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
bin


p4<-ggplot(Fecond, aes(poids, ReprodBin)) + geom_point(size=4) + binomial_smooth(size=2)

binMass<-p4 + labs(x = "Mass (mg)", y = "Probability of laying (first egg laying event)") + ggtitle("c. Binomial mass")+ theme_classic()+ theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
binMass




p5<-ggplot(Fecond, aes(MSL, ReprodBin)) + geom_point(size=4) + binomial_smooth(size=2)

binSMI<-p5 + labs(x = "SMI (mg)", y = "Probability of laying (first egg laying event)") + ggtitle("e. Binomial SMI")+ theme_classic()+ theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
binSMI




library(gridExtra)
grid.arrange(ztnb,bin,ncol=2)


png("/home/mark/Dropbox/condition/ANALYSE/FirstegglayingOLSMASSSMI.png",width=4200, height=4200,res=300)

grid.arrange(bin,ztnbOLS,binMass,ztnbMASS,binSMI,ztnbSMI,ncol=2,nrow=3)


dev.off()

summary(m4a.trunc)
summary(m4b.trunc)
summary(m4c.trunc)
summary(m4d.trunc)






M6b.nb<-glm.nb(eggs~TreatLarve*MSL+TreatAdult,na.action=na.fail,data=Fecond)
summary(M6b.nb)





Fit <- glm.nb(eggs~TreatLarve*MSL+TreatAdult,na.action=na.fail,data=Fecond)
Fit

newdat = data.frame(TreatAdult = Fecond$TreatAdult,MSL=Fecond$MSL,eggs=Fecond$eggs,TreatLarve=Fecond$TreatLarve)

head(newdat)

pred = predict(Fit, newdata = newdat, level = 0, interval = "confidence")

newdat<-cbind(newdat,pred)

names(newdat)
newdat$lwr
head(newdat)

des = model.matrix(formula("~TreatLarve*MSL+TreatAdult"), newdat)

predvar = diag( des %*% vcov(Fit) %*% t(des) )


newdat$lower = with(newdat, pred - 1.96*sqrt(predvar) )
newdat$upper = with(newdat, pred + 1.96*sqrt(predvar) )


names(newdat)
head(newdat)



library(ggplot2)

EggsSMI<-ggplot(newdat, aes(x = MSL, y = eggs, color=TreatLarve:TreatAdult))+ geom_point(aes(x=MSL, y = eggs),size=4)+ geom_ribbon(data = newdat, aes(y = NULL, ymin = exp(lower), ymax = exp(upper),color = NULL, fill = TreatLarve:TreatAdult), alpha = .15) + geom_line(data = newdat, aes(y = exp(pred)), size = 1) + theme_bw()+scale_colour_manual(values=c("black","grey","red","rosybrown1"),name="", breaks=c("N:N","N:PN","PN:N","PN:PN"),labels=c("Rich larval/Rich adult","Rich larval/Poor adult","Poor larval/Rich adult","Poor larval/Poor adult")) + theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ labs(x = "SMI (mg)", y = "Total fecundity (total number of eggs)")+scale_fill_manual(values=c("black","grey","red","rosybrown1"), name="fill",guide=FALSE)+ggtitle("Larval food treatment interaction with SMI")



png("/home/mark/Dropbox/condition/ANALYSE/TotalFecondTraitBCILarvaire.png",height=3100, width=3970,res=300)
EggsSMI
dev.off()



M4.nb<-glm.nb(eggs~TreatLarve*TreatAdult*OLSresidue,na.action=na.fail,data=Fecond)



Fit <- glm.nb(eggs~TreatLarve*OLSresidue+TreatAdult,na.action=na.fail,data=Fecond)
Fit

newdat = data.frame(TreatAdult = Fecond$TreatAdult,OLSresidue=Fecond$OLSresidue,eggs=Fecond$eggs,TreatLarve=Fecond$TreatLarve)

head(newdat)

pred = predict(Fit, newdata = newdat, level = 0, interval = "confidence")

newdat<-cbind(newdat,pred)

names(newdat)
newdat$lwr
head(newdat)

des = model.matrix(formula("~TreatLarve*OLSresidue+TreatAdult"), newdat)

predvar = diag( des %*% vcov(Fit) %*% t(des) )


newdat$lower = with(newdat, pred - 1.96*sqrt(predvar) )
newdat$upper = with(newdat, pred + 1.96*sqrt(predvar) )


names(newdat)
head(newdat)



library(ggplot2)

EggsOLS<-ggplot(newdat, aes(x = OLSresidue, y = eggs, color=TreatLarve:TreatAdult))+ geom_point(aes(x=OLSresidue, y = eggs),size=4)+ geom_ribbon(data = newdat, aes(y = NULL, ymin = exp(lower), ymax = exp(upper),color = NULL, fill = TreatLarve:TreatAdult), alpha = .15) + geom_line(data = newdat, aes(y = exp(pred)), size = 1) + theme_bw()+scale_colour_manual(values=c("black","grey","red","rosybrown1"),name="", breaks=c("N:N","N:PN","PN:N","PN:PN"),labels=c("Rich larval/Rich adult","Rich larval/Poor adult","Poor larval/Rich adult","Poor larval/Poor adult")) + theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ labs(x = "OLS residuals", y = "Total fecundity (total number of eggs)")+scale_fill_manual(values=c("black","grey","red","rosybrown1"), name="fill",guide=FALSE)+ggtitle("Larval food treatment interaction with OLS residuals")


png("/home/mark/Dropbox/condition/ANALYSE/TotalFecondTraitOLSLarvaire.png",height=3100, width=3970,res=300)
EggsOLS
dev.off()




library(grid)


legend=gtable_filter(ggplotGrob(EggsSMI), "guide-box")

png("/home/mark/Dropbox/condition/ANALYSE/TotalFecondTraitSMIOLSLarvaire.png",height=3100, width=3970,res=300)
pushViewport(viewport(layout = grid.layout(1, 10)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

print(EggsSMI+ theme(legend.position="none"), vp = vplayout(1, 1:4))
print(EggsOLS+ theme(legend.position="none"), vp = vplayout(1, 5:8))
legend$vp = viewport(layout.pos.row = 1, layout.pos.col = 9:10)
grid.draw(legend)

dev.off()





########################################################################################################New submission#####################################################################################################


##########################
setwd("~/Dropbox/condition/ANALYSE/FinalAnalysis")

Fecond= read.csv(file = "FeconditePropreNew.csv", header = TRUE)
Fecond<-Fecond[which(!Fecond$Days.laid=="NA" & !Fecond$Mort==1),]
Fecond$Days.laid

#detach(Fecond)
#attach(Fecond)
names(Fecond)
head(Fecond)

summary(Fecond$milieu)


library(MASS)

Fit <- glm.nb(eggs~TreatLarve*MSL+TreatAdult,na.action=na.fail,data=Fecond)
Fit

newdat = data.frame(TreatAdult = Fecond$TreatAdult,MSL=Fecond$MSL,eggs=Fecond$eggs,TreatLarve=Fecond$TreatLarve)

head(newdat)

pred = predict(Fit, newdata = newdat, level = 0, interval = "confidence")

newdat<-cbind(newdat,pred)

names(newdat)
newdat$lwr
head(newdat)

des = model.matrix(formula("~TreatLarve*MSL+TreatAdult"), newdat)

predvar = diag( des %*% vcov(Fit) %*% t(des) )


newdat$lower = with(newdat, pred - 1.96*sqrt(predvar) )
newdat$upper = with(newdat, pred + 1.96*sqrt(predvar) )


names(newdat)
head(newdat)



library(ggplot2)

EggsSMI<-ggplot(newdat, aes(x = MSL, y = eggs, color=TreatLarve:TreatAdult))+ geom_point(aes(x=MSL, y = eggs),size=4)+ geom_ribbon(data = newdat, aes(y = NULL, ymin = exp(lower), ymax = exp(upper),color = NULL, fill = TreatLarve:TreatAdult), alpha = .15) + geom_line(data = newdat, aes(y = exp(pred)), size = 1) + theme_bw()+scale_colour_manual(values=c("black","grey","red","rosybrown1"),name="", breaks=c("N:N","N:PN","PN:N","PN:PN"),labels=c("Rich larval/Rich adult","Rich larval/Poor adult","Poor larval/Rich adult","Poor larval/Poor adult")) + theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ labs(x = "SMI (mg)", y = "Total fecundity (total number of eggs)")+scale_fill_manual(values=c("black","grey","red","rosybrown1"), name="fill",guide=FALSE)

EggsSMI

png("/home/mark/Dropbox/condition/ANALYSE/TotalFecondTraitBCILarvaire.png",height=3100, width=3970,res=300)
EggsSMI
dev.off()




########Validation des BCI###########

#Fecond<-Fecond[which(Fecond$TreatLarve=="N" & Fecond$TreatAdult=="PN"),]



png("/home/mark/Dropbox/condition/ANALYSE/Density.png",width=2100, height=2970,res=300)

pushViewport(viewport(layout = grid.layout(3, 14)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

print(DensityMSL+ theme(legend.position="none"), vp = vplayout(1, 1:9))
print(DensityMass+ theme(legend.position="none"), vp = vplayout(2, 1:9))
print(DensityLong+ theme(legend.position="none"), vp = vplayout(3, 1:9))
legend$vp = viewport(layout.pos.row = 2, layout.pos.col = 10:14)
grid.draw(legend)

dev.off()




Fecond= read.csv(file = "FeconditePropreNew.csv", header = TRUE)
Fecond<-Fecond[which(!Fecond$Days.laid=="NA" & !Fecond$Mort==1),]
Fecond$Days.laid

Fecond$TreatLarve
Fecond$TreatAdult


library(MASS)
M1.nb<-glm.nb(eggs~TreatAdult+moyLong,na.action=na.fail,data=Fecond)
M2.nb<-glm.nb(eggs~TreatAdult+poids,na.action=na.fail,data=Fecond)
#M3.nb<-glm.nb(eggs~volume,na.action=na.fail,data=Fecond)
M4.nb<-glm.nb(eggs~TreatAdult+OLSresidue,na.action=na.fail,data=Fecond)
#M5.nb<-glm.nb(eggs~VSL,na.action=na.fail,data=Fecond)
M6.nb<-glm.nb(eggs~TreatAdult+MSL,na.action=na.fail,data=Fecond)

library(MuMIn)
(gg1.nb <- dredge(M1.nb,rank="AICc", extra="R^2"))
(gg2.nb <- dredge(M2.nb,rank="AICc", extra="R^2"))
#(gg3.nb <- dredge(M3.nb,rank="AICc", extra="R^2"))
(gg4.nb <- dredge(M4.nb,rank="AICc", extra="R^2"))
#(gg5.nb <- dredge(M5.nb,rank="AICc", extra="R^2"))
(gg6.nb <- dredge(M6.nb,rank="AICc", extra="R^2"))




Fit <- glm.nb(eggs~MSL*TreatLarve+TreatAdult,na.action=na.fail,data=Fecond)
Fit

newdat = data.frame(TreatAdult = Fecond$TreatAdult,MSL=Fecond$MSL,eggs=Fecond$eggs,TreatLarve=Fecond$TreatLarve)

head(newdat)

pred = predict(Fit, newdata = newdat, level = 0, interval = "confidence")

newdat<-cbind(newdat,pred)

names(newdat)
newdat$lwr
head(newdat)

des = model.matrix(formula("~MSL*TreatLarve+TreatAdult"), newdat)

predvar = diag( des %*% vcov(Fit) %*% t(des) )


newdat$lower = with(newdat, pred - 1.96*sqrt(predvar) )
newdat$upper = with(newdat, pred + 1.96*sqrt(predvar) )


names(newdat)
head(newdat)



library(ggplot2)

EggsSMI<-ggplot(newdat, aes(x = MSL, y = eggs, color=TreatLarve:TreatAdult))+ geom_point(aes(x=MSL, y = eggs),size=3)+ geom_ribbon(data = newdat, aes(y = NULL, ymin = exp(lower), ymax = exp(upper),color = NULL, fill = TreatLarve:TreatAdult), alpha = .15) + geom_line(data = newdat, aes(y = exp(pred)), size = 1) + theme_bw()+scale_colour_manual(values=c("black","grey","red","rosybrown1"),name="", breaks=c("N:N","N:PN","PN:N","PN:PN"),labels=c("Rich larval/Rich adult","Rich larval/Poor adult","Poor larval/Rich adult","Poor larval/Poor adult"),guide=guide_legend(ncol=2)) + theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ labs(x = "SMI (mg)", y = "Total fecundity\n(total number of eggs)")+scale_fill_manual(values=c("black","grey","red","rosybrown1"), name="fill",guide=FALSE)+theme(legend.position="bottom", legend.text=element_text(size = rel(1)), legend.title=element_text(size= rel(1)))

EggsSMI



hist(Fecond$MSL)





Fit <- glm.nb(eggs~moyLong+TreatLarve+TreatAdult,na.action=na.fail,data=Fecond)
Fit

dredge(Fit)

newdat = data.frame(TreatAdult = Fecond$TreatAdult,moyLong=Fecond$moyLong,eggs=Fecond$eggs,TreatLarve=Fecond$TreatLarve)

head(newdat)

pred = predict(Fit, newdata = newdat, level = 0, interval = "confidence")

newdat<-cbind(newdat,pred)

names(newdat)
newdat$lwr
head(newdat)

des = model.matrix(formula("~moyLong+TreatAdult+TreatLarve"), newdat)

predvar = diag( des %*% vcov(Fit) %*% t(des) )


newdat$lower = with(newdat, pred - 1.96*sqrt(predvar) )
newdat$upper = with(newdat, pred + 1.96*sqrt(predvar) )


names(newdat)
head(newdat)



library(ggplot2)

EggsmoyLong<-ggplot(newdat, aes(x = moyLong, y = eggs, color=TreatLarve:TreatAdult))+ geom_point(aes(x=moyLong, y = eggs),size=3)+ geom_ribbon(data = newdat, aes(y = NULL, ymin = exp(lower), ymax = exp(upper),color = NULL, fill = TreatLarve:TreatAdult), alpha = .15) + geom_line(data = newdat, aes(y = exp(pred)), size = 1) + theme_bw()+scale_colour_manual(values=c("black","grey","red","rosybrown1"),name="", breaks=c("N:N","N:PN","PN:N","PN:PN"),labels=c("Rich larval/Rich adult","Rich larval/Poor adult","Poor larval/Rich adult","Poor larval/Poor adult")) + theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ labs(x = "Elytron length (mm)", y = "Total fecundity\n(total number of eggs)")+scale_fill_manual(values=c("black","grey","red","rosybrown1"), name="fill",guide=FALSE)

EggsmoyLong


hist(Fecond$MSL)




Fit <- glm.nb(eggs~poids+TreatLarve+TreatAdult,na.action=na.fail,data=Fecond)
Fit

dredge(Fit)

newdat = data.frame(TreatAdult = Fecond$TreatAdult,poids=Fecond$poids,eggs=Fecond$eggs,TreatLarve=Fecond$TreatLarve)

head(newdat)

pred = predict(Fit, newdata = newdat, level = 0, interval = "confidence")

newdat<-cbind(newdat,pred)

names(newdat)
newdat$lwr
head(newdat)

des = model.matrix(formula("~poids+TreatAdult+TreatLarve"), newdat)

predvar = diag( des %*% vcov(Fit) %*% t(des) )


newdat$lower = with(newdat, pred - 1.96*sqrt(predvar) )
newdat$upper = with(newdat, pred + 1.96*sqrt(predvar) )


names(newdat)
head(newdat)



library(ggplot2)

Eggspoids<-ggplot(newdat, aes(x = poids, y = eggs, color=TreatLarve:TreatAdult))+ geom_point(aes(x=poids, y = eggs),size=3)+ geom_ribbon(data = newdat, aes(y = NULL, ymin = exp(lower), ymax = exp(upper),color = NULL, fill = TreatLarve:TreatAdult), alpha = .15) + geom_line(data = newdat, aes(y = exp(pred)), size = 1) + theme_bw()+scale_colour_manual(values=c("black","grey","red","rosybrown1"),name="", breaks=c("N:N","N:PN","PN:N","PN:PN"),labels=c("Rich larval/Rich adult","Rich larval/Poor adult","Poor larval/Rich adult","Poor larval/Poor adult"))+ theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ labs(x = "Mass (mg)", y = "Total fecundity\n(total number of eggs)")+scale_fill_manual(values=c("black","grey","red","rosybrown1"), name="fill",guide=FALSE)

Eggspoids





Fit <- glm.nb(eggs~moyLong+TreatLarve*MSL+TreatAdult,na.action=na.fail,data=Fecond)
Fit
summary(Fit)
dredge(Fit)

newdat = data.frame(TreatAdult = Fecond$TreatAdult,moyLong=Fecond$moyLong,eggs=Fecond$eggs,TreatLarve=Fecond$TreatLarve,MSL=mean(Fecond$MSL))

head(newdat)

pred = predict(Fit, newdata = newdat, level = 0, interval = "confidence")

newdat<-cbind(newdat,pred)

names(newdat)
newdat$lwr
head(newdat)

des = model.matrix(formula("~moyLong+TreatLarve*MSL+TreatAdult"), newdat)

predvar = diag( des %*% vcov(Fit) %*% t(des) )


newdat$lower = with(newdat, pred - 1.96*sqrt(predvar) )
newdat$upper = with(newdat, pred + 1.96*sqrt(predvar) )


names(newdat)
head(newdat)



library(ggplot2)

EggsmoyLongContMSL<-ggplot(newdat, aes(x = moyLong, y = eggs, color=TreatLarve:TreatAdult))+ geom_point(aes(x=moyLong, y = eggs),size=3)+ geom_ribbon(data = newdat, aes(y = NULL, ymin = exp(lower), ymax = exp(upper),color = NULL, fill = TreatLarve:TreatAdult), alpha = .15) + geom_line(data = newdat, aes(y = exp(pred)), size = 1) + theme_bw()+scale_colour_manual(values=c("black","grey","red","rosybrown1"),name="Feeeding treatment:", breaks=c("N:N","N:PN","PN:N","PN:PN"),labels=c("Rich larval/Rich adult","Rich larval/Poor adult","Poor larval/Rich adult","Poor larval/Poor adult"),guide=guide_legend(ncol=2)) + theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.2)), legend.title=element_text(size= rel(1.2)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position="bottom")+ labs(x = "Elytron length (mm)", y = "Total fecundity\n(total number of eggs)")+scale_fill_manual(values=c("black","grey","red","rosybrown1"), name="fill",guide=FALSE)


png("/home/mark/Dropbox/condition/ANALYSE/EggsmoyLongContMSL.png",height=2100, width=2970,res=300)
EggsmoyLongContMSL
dev.off()




M6.nb<-glm.nb(eggs~TreatLarve*TreatAdult*moyLong,na.action=na.fail,data=Fecond)
library(MuMIn)
(gg6.nb <- dredge(M6.nb,rank="AICc", extra="R^2"))

write.table(gg6.nb, "~/Dropbox/condition/ANALYSE/eggsLong.txt", sep="\t")



M6.nb<-glm.nb(eggs~TreatLarve*TreatAdult*poids,na.action=na.fail,data=Fecond)
library(MuMIn)
(gg6.nb <- dredge(M6.nb,rank="AICc", extra="R^2"))

write.table(gg6.nb, "~/Dropbox/condition/ANALYSE/eggsPoids.txt", sep="\t")



M6.nb<-glm.nb(eggs~TreatLarve*TreatAdult*MSL,na.action=na.fail,data=Fecond)
library(MuMIn)
(gg6.nb <- dredge(M6.nb,rank="AICc", extra="R^2"))

write.table(gg6.nb, "~/Dropbox/condition/ANALYSE/eggsMSL.txt", sep="\t")



partial.r<-function(t.val,df){
  r<-t.val/sqrt((t.val)^2+df)
  names(r)<-"effect size r"
  return(r)
}

M2<-glm.nb(eggs~TreatLarve*MSL+TreatAdult,na.action=na.fail,data=Fecond)
summary(M2)  

#Call:
#glm.nb(formula = eggs ~ TreatLarve * MSL + TreatAdult, data = Fecond, 
#    na.action = na.fail, init.theta = 4.222457082, link = log)#

#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-4.0873  -0.7304  -0.0263   0.5529   2.3063  #

#Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       2.720437   0.484374   5.616 1.95e-08 ***
#TreatLarvePN      1.698041   0.640163   2.653 0.007990 ** 
#MSL               0.015344   0.003975   3.860 0.000113 ***
#TreatAdultPN     -0.343274   0.067369  -5.095 3.48e-07 ***
#TreatLarvePN:MSL -0.013597   0.005342  -2.545 0.010919 *  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#

#(Dispersion parameter for Negative Binomial(4.2225) family taken to be 1)#

#    Null deviance: 274.84  on 220  degrees of freedom
#Residual deviance: 233.88  on 216  degrees of freedom
#AIC: 2254#

#Number of Fisher Scoring iterations: 1#
#

#              Theta:  4.222 
#          Std. Err.:  0.418 #

# 2 x log-likelihood:  -2242.044 


FecondLN<-Fecond[which(Fecond$TreatLarve=="N"),]
FecondLPN<-Fecond[which(Fecond$TreatLarve=="PN"),]

M2<-glm.nb(eggs~MSL+TreatAdult,na.action=na.fail,data=FecondLN)
summary(M2)  

M3<-glm.nb(eggs~MSL+TreatAdult,na.action=na.fail,data=FecondLPN)
summary(M3)  


coef(summary(M2))[2,3]

library(boot)

r.part <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- glm.nb(formula,data=d)
  return(partial.r(coef(summary(fit))[2,3],107))
}

# bootstrapping with 10000 replications
(results <- boot(data=FecondLN, statistic=r.part,R=10000, formula=eggs~MSL+TreatAdult))

#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = FecondLN, statistic = r.part, R = 10000, formula = eggs ~ 
#    MSL + TreatAdult)#
#

#Bootstrap Statistics :
#    original        bias    std. error
#t1* 0.320789 -0.0002838316  0.07633326


(Boot<-boot.ci(results, type="bca"))

#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates

#CALL : 
#boot.ci(boot.out = results, type = "bca")

#Intervals : 
#Level       BCa          
#95%   ( 0.1599,  0.4584 )  
#Calculations and Intervals on Original Scale




r.part <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- glm.nb(formula,data=d)
  return(partial.r(coef(summary(fit))[2,3],108))
}

# bootstrapping with 10000 replications
(results <- boot(data=FecondLPN, statistic=r.part,R=10000, formula=eggs~MSL+TreatAdult))

#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = FecondLPN, statistic = r.part, R = 10000, formula = eggs ~ 
#    MSL + TreatAdult)#
#

#Bootstrap Statistics :
#      original       bias    std. error
#t1* 0.04841915 -0.005216605  0.09496639


(Boot<-boot.ci(results, type="bca"))
#

#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   (-0.1340,  0.2393 )  
#Calculations and Intervals on Original Scale








M6.nb<-glm.nb(eggs~TreatLarve*TreatAdult*MSL+moyLong*TreatAdult,na.action=na.fail,data=Fecond)
library(MuMIn)
(gg6.nb <- dredge(M6.nb,rank="AICc", extra="R^2"))


M6a.nb<-glm.nb(eggs~TreatLarve*TreatAdult*MSL+moyLong,na.action=na.fail,data=Fecond)
library(MuMIn)
(gg6a.nb <- dredge(M6a.nb,rank="AICc", extra="R^2"))
library(car)
vif(M6a.nb)

cor.test(Fecond$moyLong,Fecond$MSL)

plot(Fecond$moyLong,Fecond$MSL)


write.table(gg6a.nb, "~/Dropbox/condition/ANALYSE/eggsMSLtemp.txt", sep="\t")

M6a.nb<-glm.nb(eggs~TreatLarve*MSL+TreatAdult+moyLong,na.action=na.fail,data=Fecond)
vif(M6a.nb)



M6a.nb<-glm.nb(eggs~TreatLarve*TreatAdult*MSL+TreatLarve*TreatAdult*moyLong,na.action=na.fail,data=Fecond)
(gg6a.nb <- dredge(M6a.nb,rank="AICc", extra="R^2"))
step(M6a.nb,test="Chi")

library(car)
vif(M6a.nb)



M1 <- glm.nb(eggs~TreatLarve*MSL*TreatAdult+moyLong,na.action=na.fail,data=Fecond)
M1


M2 <- glm.nb(eggs~TreatLarve*MSL*TreatAdult+poids,na.action=na.fail,data=Fecond)
M2


(gg10.nb <- dredge(M1,rank="AICc", extra="R^2"))
(gg11.nb <- dredge(M2,rank="AICc", extra="R^2"))


write.table(gg10.nb, "~/Dropbox/condition/ANALYSE/eggsLongSMI.txt", sep="\t")

write.table(gg11.nb, "~/Dropbox/condition/ANALYSE/massSMI.txt", sep="\t")





M6a.nb<-glm.nb(eggs~MSL+moyLong*TreatLarve*TreatAdult,na.action=na.fail,data=Fecond)
library(MuMIn)
(gg6a.nb <- dredge(M6a.nb,rank="AICc", extra="R^2"))
library(car)
vif(M6a.nb)







Fit <- glm.nb(eggs~TreatLarve*MSL+TreatAdult+moyLong,na.action=na.fail,data=Fecond)
Fit


library(car)
vif(Fit)



newdat = data.frame(TreatAdult = Fecond$TreatAdult,MSL=Fecond$MSL,eggs=Fecond$eggs,TreatLarve=Fecond$TreatLarve,moyLong=mean(Fecond$moyLong))

head(newdat)

pred = predict(Fit, newdata = newdat, level = 0, interval = "confidence")

newdat<-cbind(newdat,pred)

names(newdat)
newdat$lwr
head(newdat)

des = model.matrix(formula("~TreatLarve*MSL+TreatAdult+moyLong"), newdat)

predvar = diag( des %*% vcov(Fit) %*% t(des) )


newdat$lower = with(newdat, pred - 1.96*sqrt(predvar) )
newdat$upper = with(newdat, pred + 1.96*sqrt(predvar) )


names(newdat)
head(newdat)



library(ggplot2)

EggsSMILong<-ggplot(newdat, aes(x = MSL, y = eggs, color=TreatLarve:TreatAdult))+ geom_point(aes(x=MSL, y = eggs),size=3)+ geom_ribbon(data = newdat, aes(y = NULL, ymin = exp(lower), ymax = exp(upper),color = NULL, fill = TreatLarve:TreatAdult), alpha = .15) + geom_line(data = newdat, aes(y = exp(pred)), size = 1) + theme_bw()+scale_colour_manual(values=c("black","grey","red","rosybrown1"),name="", breaks=c("N:N","N:PN","PN:N","PN:PN"),labels=c("Rich larval/Rich adult","Rich larval/Poor adult","Poor larval/Rich adult","Poor larval/Poor adult"))+ theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ labs(x = "SMI (mg)", y = "Total fecundity (total number of eggs)")+scale_fill_manual(values=c("black","grey","red","rosybrown1"), name="fill",guide=FALSE)+ggtitle("Larval food treatment interaction with SMI")

EggsSMILong







Fit <- glm.nb(eggs~TreatLarve*MSL+TreatAdult+moyLong,na.action=na.fail,data=Fecond)
Fit

newdat = data.frame(TreatAdult = Fecond$TreatAdult,MSL=mean(Fecond$MSL),eggs=Fecond$eggs,TreatLarve=Fecond$TreatLarve,moyLong=Fecond$moyLong)

head(newdat)

pred = predict(Fit, newdata = newdat, level = 0, interval = "confidence")

newdat<-cbind(newdat,pred)

names(newdat)
newdat$lwr
head(newdat)

des = model.matrix(formula("~TreatLarve*MSL+TreatAdult+moyLong"), newdat)

predvar = diag( des %*% vcov(Fit) %*% t(des) )


newdat$lower = with(newdat, pred - 1.96*sqrt(predvar) )
newdat$upper = with(newdat, pred + 1.96*sqrt(predvar) )


names(newdat)
head(newdat)



library(ggplot2)

EggsSMILong2<-ggplot(newdat, aes(x = moyLong, y = eggs, color=TreatLarve:TreatAdult))+ geom_point(aes(x=moyLong, y = eggs),size=3)+ geom_ribbon(data = newdat, aes(y = NULL, ymin = exp(lower), ymax = exp(upper),color = NULL, fill = TreatLarve:TreatAdult), alpha = .15) + geom_line(data = newdat, aes(y = exp(pred)), size = 1) + theme_bw()+scale_colour_manual(values=c("black","grey","red","rosybrown1"),name="", breaks=c("N:N","N:PN","PN:N","PN:PN"),labels=c("Rich larval/Rich adult","Rich larval/Poor adult","Poor larval/Rich adult","Poor larval/Poor adult")) + theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ labs(x = "Elytron length (mm)", y = "Total fecundity (total number of eggs)")+scale_fill_manual(values=c("black","grey","red","rosybrown1"), name="fill",guide=FALSE)+ggtitle("Effect of structural size when controlling for body condition")

EggsSMILong2




library(gtable)
library(gridExtra)
library(ggpubr)
library(grid)


legend=gtable_filter(ggplotGrob(EggsSMI+theme(legend.position="right", legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)))), "guide-box")
legend2=gtable_filter(ggplotGrob(DensityLong+theme(legend.position="right", legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)))), "guide-box")

png("/home/mark/Dropbox/condition/ANALYSE/TotalFecondTraitSelection.png",width=2500, height=2970,res=200)
pushViewport(viewport(layout = grid.layout(23, 20)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)


print(DensityLong+ theme(legend.position="none")+ggtitle("a."), vp = vplayout(1:7, 1:8))
print(DensityMass+ theme(legend.position="none")+ggtitle("b."), vp = vplayout(8:14, 1:8))
print(DensityMSL+ theme(legend.position="none")+ggtitle("c."), vp = vplayout(15:21, 1:8))
legend2$vp = viewport(layout.pos.row = 22:23, layout.pos.col = 1:9)
grid.draw(legend2)


print(EggsmoyLong+ theme(legend.position="none")+ggtitle("d."), vp = vplayout(1:7, 10:19))
print(Eggspoids+ theme(legend.position="none")+ggtitle("e."),vp = vplayout(8:14, 10:19))
print(EggsSMI+ theme(legend.position="none")+ggtitle("f."), vp = vplayout(15:21, 10:19))
legend$vp = viewport(layout.pos.row = 22:23, layout.pos.col = 9:20)
grid.draw(legend)
dev.off()





##########################
setwd("/home/mark/Dropbox/condition/ANALYSE/FinalAnalysis")

Mortalite= read.csv(file = "MortalityTreament.csv", header = TRUE)
names(Mortalite)

 [1] "individu"  "milieu"    "Mort"      "tLarve"    "tAdulte"   "longueur" 
 [7] "largeur"   "epaisseur" "volume"    "poids" 

summary(Mortalite$milieu)


#Mortalite<-Mortalite[!rowSums(is.na(Mortalite)) > 0,]
#Mortalite<-Mortalite[which(Mortalite$FecondIncomplet==0),]
Mortalite<-Mortalite[!is.na(Mortalite$poids)==TRUE,]



M1<-glm(Mort~tLarve+tAdulte,family=binomial,data=Mortalite,na.action=na.fail)

drop1(M1,test="Chi")
#anova(M1,test="Chi")
step(M1,test="Chi")


tapply(Mortalite$Mort,Mortalite$tLarve:Mortalite$tAdulte,sum)

  Nl:Na  Nl:Pna  Pnl:Na Pnl:Pna 
      5      12       0       1 


tapply(Mortalite$Mort,Mortalite$tLarve:Mortalite$tAdulte,length)

Nl:Na  Nl:Pna  Pnl:Na Pnl:Pna 
     76      66      55      57 
 

NlNa<-5/76
NlPNa<-12/66
PNlNa<-0/55
PNlPNa<-1/57

proportionMort<-data.frame(rbind(NlNa,NlPna,PnlNa,PnlPna))
Treatment<-rbind("NlNa","NlPNa","PNlNa","PNlPNa")


names(proportionMort)

proportionMort$Mortalite<-proportionMort$rbind.NlNa..NlPna..PnlNa..PnlPna.

proportionMort<-cbind(proportionMort,Treatment)[,2:3]
proportionMort


str(proportionMort)

library(ggplot2)

png("/home/mark/Dropbox/condition/ANALYSE/FigureMortaliteTreatment.png",width=2100, height=2100,res=200)

ggplot(data=proportionMort,aes(y=Mortalite,x=Treatment,fill=Treatment))+geom_bar(stat="identity")+theme(axis.text.x = element_text(size = rel(1.8)),axis.text.y = element_text(size = rel(2)), axis.title.x = element_text(size = rel(2)), axis.title.y = element_text(size=rel(2)),panel.background = element_blank(),axis.line = element_line(colour = "black"),legend.position = c(.95, .95),legend.justification = c("right", "top"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size = rel(2)))+labs(y="Adult mortality during experiment",x="Feeding treatment")+ geom_text(aes(label=round(Mortalite,2)), vjust=-0.1, color="black", size=rel(8))+ scale_x_discrete(labels=c("NlNa" = "Rich larval/Rich adult", "NlPna" = "Rich larval/Poor adult", "PnlNa" = "Poor larval/Rich adult","PnlPna" = "Poor larval/Poor adult"))+scale_fill_manual(values=c("black","grey","red","rosybrown1"),labels=c("NlNa" = "Rich larval/Rich adult", "NlPna" = "Rich larval/Poor adult", "PnlNa" = "Poor larval/Rich adult","PnlPna" = "Poor larval/Poor adult"),name="Feeding treatement")

dev.off()



head(Mortalite)

library(smatr)
m1<-sma(poids~longueur,log="xy",data=Mortalite)
plot(m1)
m1

#Call: sma(formula = poids ~ longueur, data = Mortalite, log = "xy") #

#Fit using Standardized Major Axis #

#These variables were log-transformed before fitting: xy #

#Confidence intervals (CI) are at 95%#

#------------------------------------------------------------
#Coefficients:
#              elevation    slope
#estimate     0.13693965 2.870705
#lower limit -0.04363699 2.614787
#upper limit  0.31751629 3.151672#

#H0 : variables uncorrelated
#R-squared : 0.4682886 
#P-value : < 2.22e-16#




mortMSL<- Mortalite$poids*((mean(Mortalite$longueur)/Mortalite$longueur)^2.847276)

M1<-glm(Mort~tLarve*tAdulte*mortMSL,family=binomial,data=Mortalite,na.action=na.fail)
library(MuMIn)
dredge(M1)

ms1 <- dredge(M1,rank="AICc", extra="R^2")
ms1
model.avg(ms1)
confset.95p <- get.models(ms1, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

write.table(ms1,"MSLmortalite.txt",sep="\t",row.names=F,quote=F) 




M1<-glm(Mort~tLarve*tAdulte*poids,family=binomial,data=Mortalite,na.action=na.fail)
library(MuMIn)
dredge(M1,rank="AICc", extra="R^2")

ms1 <- dredge(M1,rank="AICc", extra="R^2")
ms1
model.avg(ms1)
confset.95p <- get.models(ms1, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)


write.table(ms1,"Poidsmortalite.txt",sep="\t",row.names=F,quote=F) 



M1<-glm(Mort~poids,family=binomial,data=Mortalite,na.action=na.fail)
library(MuMIn)
dredge(M1,rank="AICc", extra="R^2")

M1<-glm(Mort~mortMSL,family=binomial,data=Mortalite,na.action=na.fail)
library(MuMIn)
dredge(M1,rank="AICc", extra="R^2")

M1<-glm(Mort~longueur,family=binomial,data=Mortalite,na.action=na.fail)
library(MuMIn)
dredge(M1,rank="AICc", extra="R^2")



M1<-glm(Mort~tLarve*tAdulte*mortMSL+longueur,family=binomial,data=Mortalite,na.action=na.fail)
library(MuMIn)
dredge(M1,rank="AICc", extra="R^2")

ms1 <- dredge(M1,rank="AICc", extra="R^2")
ms1
model.avg(ms1)
confset.95p <- get.models(ms1, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)






M1<-glm(Mort~tLarve*tAdulte*longueur,family=binomial,data=Mortalite,na.action=na.fail)
library(MuMIn)
dredge(M1,rank="AICc", extra="R^2")

ms1 <- dredge(M1,rank="AICc", extra="R^2")
ms1
model.avg(ms1)
confset.95p <- get.models(ms1, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)


write.table(ms1,"longueurmortalite.txt",sep="\t",row.names=F,quote=F) 













M1<-glm(Mort~tLarve*tAdulte*volume,family=binomial,data=Mortalite,na.action=na.fail)
anova(M1,test="Chi")
step(M1,test="Chi")




M1<-glm(Mort~tLarve*tAdulte*VSL,family=binomial,data=Mortalite,na.action=na.fail)



library(MuMIn)
dredge(M1)

M1<-glm(Mort~tLarve*tAdulte+tLarve*volume+tAdulte*volume,family=binomial,data=Mortalite,na.action=na.fail)
summary(M1)

MortaliteNl<-Mortalite[which(Mortalite$tLarve=="Nl"),]
MortaliteNlPna<-MortaliteNl[which(MortaliteNl$tAdulte=="Pna"),]

M1<-glm(Mort~volume,family=binomial,data=MortaliteNlPna,na.action=na.fail)
drop1(M1,test="Chi")

anova(M1,test="Chi")
step(M1,test="Chi")


MortLong<-ggplot(MortaliteNlPna, aes(x=longueur, y=Mort)) +geom_point() + geom_smooth(method = "glm",method.args = list(family = "binomial"),aes(col="red"),se = TRUE, show.legend = FALSE)+theme_classic()+ggtitle("Rich larval, poor adult mortality\naccording to elytron length")+labs(x="Elytron length (mm)", y="Adult mortality during experiment") 
MortLong

MortMass<-ggplot(MortaliteNlPna, aes(x=poids, y=Mort)) +geom_point() + geom_smooth(method = "glm",method.args = list(family = "binomial"),aes(col="red"),se = TRUE, show.legend = FALSE)+theme_classic()+ggtitle("Rich larval, poor adult mortality\naccording to mass")+labs(x="Mass (g)", y="Adult mortality during experiment") 
MortMass

MortVolume<-ggplot(MortaliteNlPna, aes(x=volume, y=Mort)) +geom_point() + geom_smooth(method = "glm",method.args = list(family = "binomial"),aes(col="red"),se = TRUE, show.legend = FALSE)+theme_classic()+ggtitle("Rich larval, poor adult mortality\naccording to volume")+labs(x="Volume (mm3)", y="Adult mortality during experiment")  
MortVolume


MortOLS<-ggplot(MortaliteNlPna, aes(x=OLSresidue, y=Mort)) +geom_point() + geom_smooth(method = "glm",method.args = list(family = "binomial"),aes(col="red"),se = TRUE, show.legend = FALSE)+theme_classic()+ggtitle("Rich larval, poor adult mortality\naccording to OLSresiduals")+labs(x="OLSresiduals", y="Adult mortality during experiment") 
MortOLS


MortMSL<-ggplot(MortaliteNlPna, aes(x=MSL, y=Mort)) +geom_point() + geom_smooth(method = "glm",method.args = list(family = "binomial"),aes(col="red"),se = TRUE, show.legend = FALSE)+theme_classic()+ggtitle("Rich larval, poor adult mortality\naccording to SMI")+labs(x="SMI (g)", y="Adult mortality during experiment") 

MortMSL


MortVSL<-ggplot(MortaliteNlPna, aes(x=VSL, y=Mort)) +geom_point() + geom_smooth(method = "glm",method.args = list(family = "binomial"),aes(col="red"),se = TRUE, show.legend = FALSE)+theme_classic() +ggtitle("Rich larval, poor adult mortality\naccording to SVI")+labs(x="SVI (mm3)", y="Adult mortality during experiment")
MortVSL



MortLong
MortMass
MortVolume
MortOLS
MortMSL
MortVSL


library(grid)





png("/home/mark/Dropbox/condition/ANALYSE/FigureMortaliteBCI.png",width=2100, height=2970,res=300)

pushViewport(viewport(layout = grid.layout(3, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

print(MortLong, vp = vplayout(1, 1))
print(MortMass, vp = vplayout(2, 1))
print(MortVolume, vp = vplayout(3, 1))
print(MortOLS, vp = vplayout(1, 2))
print(MortMSL, vp = vplayout(2, 2))
print(MortVSL, vp = vplayout(3, 2))


dev.off()






tapply(Mortalite$Mort,Mortalite$tLarve:Mortalite$tAdulte,sum)

M1<-glm(Mort~tLarve*tAdulte*longueur,family=binomial,data=Mortalite,na.action=na.fail)


library(MuMIn)

ms1 <- dredge(M1,rank="AICc")
ms1
model.avg(ms1)
confset.95p <- get.models(ms1, cumsum(weight) <= 1)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)





summary(M1)

Mortalite$milieu<-factor(Mortalite$milieu)
M1<-glm(Mort~milieu,family=binomial,data=Mortalite,na.action=na.fail)

summary(M1)
anova(M1,test="Chi")

require(multcomp)

tuk <- glht(M1, linfct = mcp(milieu = "Tukey"))
tuk

#?glht()

summary(tuk)


         Simultaneous Tests for General Linear Hypotheses

Multiple Comparisons of Means: Tukey Contrasts


Fit: lm(formula = Mort ~ milieu, data = Mortalite, family = binomial)

Linear Hypotheses:
                    Estimate Std. Error t value Pr(>|t|)    
NlPNa - NlNa == 0    0.10390    0.04262   2.438  0.07265 .  
PNlNa - NlNa == 0   -0.07792    0.04486  -1.737  0.30630    
PNlPNa - NlNa == 0  -0.06097    0.04396  -1.387  0.50819    
PNlNa - NlPNa == 0  -0.18182    0.04639  -3.919  < 0.001 ***
PNlPNa - NlPNa == 0 -0.16487    0.04552  -3.622  0.00195 ** 
PNlPNa - PNlNa == 0  0.01695    0.04762   0.356  0.98449    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(Adjusted p values reported -- single-step method)




M1<-glm(Mort~milieu*mortMSL*mortMSL*longueur*poids,family=binomial,data=Mortalite,na.action=na.fail)
library(MuMIn)
dredge(M1)

ms1 <- dredge(M1,rank="AICc", extra="R^2")
ms1


write.table(ms1,"NouvelleStreategieMortalite.txt",sep="\t",row.names=F,quote=F) 


M1<-glm(Mort~Treatment,family=binomial,data=Mortalite,na.action=na.fail)

require(multcomp)
tuk <- glht(M1, linfct = mcp(fmilieu = "Tukey"))
tuk





##############################New stats for MEE###############
setwd("~/Dropbox/condition/ANALYSE/FinalAnalysis")

Fecond= read.csv(file = "FeconditePropreNew.csv", header = TRUE)
Fecond<-Fecond[which(!Fecond$Days.laid=="NA" & !Fecond$Mort==1),]
Fecond$Days.laid

#detach(Fecond)
#attach(Fecond)
names(Fecond)
head(Fecond)


 [1] "individu"      "milieu"        "eggs"          "larves"       
 [5] "oeufspaseclos" "Mort"          "TreatLarve"    "TreatAdult"   
 [9] "OLSresidue"    "MSL"           "VSL"           "poids"        
[13] "moyLong"       "volume"        "moylargeur"    "moyepaiss"  

library(MuMIn)


library(MASS)

M1.nb<-glm.nb(eggs~moyLong+MSL,na.action=na.fail,data=Fecond)
(gg1.nb <- dredge(M1.nb,rank="AICc", extra="R^2"))
head(gg1.nb)
drop1(M1.nb,test="Chi")
confint(M1.nb)


write.table(gg1.nb, "/home/mark/Dropbox/condition/JAE/MEE ?/eggsMSLLong.txt", sep="\t")




partial.r<-function(t.val,df){
  r<-t.val/sqrt((t.val)^2+df)
  names(r)<-"effect size r"
  return(r)
}

library(boot)
coef(summary(M1.nb))[2,3]

r.part <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- glm.nb(formula,data=d)
  return(partial.r(coef(summary(fit))[2,3],218))
}

# bootstrapping with 10000 replications
(results <- boot(data=Fecond, statistic=r.part,R=10000, formula=eggs~moyLong+MSL))


#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = Fecond, statistic = r.part, R = 10000, formula = eggs ~ 
#    moyLong + MSL)#
#

#Bootstrap Statistics :
#     original        bias    std. error
#t1* 0.1555348 -0.0004109512  0.06491348


(Boot<-boot.ci(results, type="bca"))


#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   ( 0.0199,  0.2772 )  
#Calculations and Intervals on Original Scale



LengthCMSL<-as.numeric(results$t0)
minci.LengthCMSL<-Boot$bca[,4]
maxci.LengthCMSL<-Boot$bca[,5]



r.part <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- glm.nb(formula,data=d)
  return(partial.r(coef(summary(fit))[3,3],218))
}

(results <- boot(data=Fecond, statistic=r.part,R=10000, formula=eggs~moyLong+MSL))

#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = Fecond, statistic = r.part, R = 10000, formula = eggs ~ 
#    moyLong + MSL)#
#

#Bootstrap Statistics :
#     original        bias    std. error
#t1* 0.2227464 -0.0001775004  0.05564785



(Boot<-boot.ci(results, type="bca"))


#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   ( 0.1088,  0.3256 )  
#Calculations and Intervals on Original Scale

MSLCLength<-as.numeric(results$t0)
minci.MSLCLength<-Boot$bca[,4]
maxci.MSLCLength<-Boot$bca[,5]



library(car)
vif(M1.nb)

M2.nb<-glm.nb(eggs~poids,na.action=na.fail,data=Fecond)
(gg2.nb <- dredge(M2.nb,rank="AICc", extra="R^2"))
head(gg2.nb)

vif(M2.nb)

write.table(gg2.nb, "/home/mark/Dropbox/condition/JAE/MEE ?/eggsPoids.txt", sep="\t")



r.part <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- glm.nb(formula,data=d)
  return(partial.r(coef(summary(fit))[2,3],218))
}

# bootstrapping with 10000 replications
(results <- boot(data=Fecond, statistic=r.part,R=10000, formula=eggs~poids))


#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = Fecond, statistic = r.part, R = 10000, formula = eggs ~ 
#    poids)#
#

#Bootstrap Statistics :
#    original       bias    std. error
#t1*  0.19736 0.0001109211  0.06094415


(Boot<-boot.ci(results, type="bca"))


#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   ( 0.0749,  0.3152 )  
#Calculations and Intervals on Original Scale



Poids<-as.numeric(results$t0)
minci.Poids<-Boot$bca[,4]
maxci.Poids<-Boot$bca[,5]




M3.nb<-glm.nb(eggs~moyLong,na.action=na.fail,data=Fecond)
(gg3.nb <- dredge(M3.nb,rank="AICc", extra="R^2"))
head(gg3.nb)

vif(M3.nb)

r.part <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- glm.nb(formula,data=d)
  return(partial.r(coef(summary(fit))[2,3],218))
}

# bootstrapping with 10000 replications
(results <- boot(data=Fecond, statistic=r.part,R=10000, formula=eggs~moyLong))

#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = Fecond, statistic = r.part, R = 10000, formula = eggs ~ 
#    moyLong)#
#

#Bootstrap Statistics :
#      original        bias    std. error
#t1* 0.08564932 -0.0006298081  0.06536662



(Boot<-boot.ci(results, type="bca"))

#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   (-0.0442,  0.2115 )  
#Calculations and Intervals on Original Scale


Long<-as.numeric(results$t0)
minci.Long<-Boot$bca[,4]
maxci.Long<-Boot$bca[,5]




M4.nb<-glm.nb(eggs~MSL,na.action=na.fail,data=Fecond)
(gg4.nb <- dredge(M4.nb,rank="AICc", extra="R^2"))
head(gg4.nb)

r.part <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- glm.nb(formula,data=d)
  return(partial.r(coef(summary(fit))[2,3],218))
}

# bootstrapping with 10000 replications
(results <- boot(data=Fecond, statistic=r.part,R=10000, formula=eggs~MSL))

#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = Fecond, statistic = r.part, R = 10000, formula = eggs ~ 
#    MSL)#
#

#Bootstrap Statistics :
#     original        bias    std. error
#t1* 0.1830902 -6.455767e-05  0.05584266


(Boot<-boot.ci(results, type="bca"))

#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   ( 0.0705,  0.2909 )  
#Calculations and Intervals on Original Scale



msl<-as.numeric(results$t0)
minci.msl<-Boot$bca[,4]
maxci.msl<-Boot$bca[,5]




df<-data.frame(effect.size=c(MSLCLength,msl,LengthCMSL,Long,Poids), minci=c(minci.MSLCLength,minci.msl,minci.LengthCMSL, minci.Long, minci.Poids),maxci=c(maxci.MSLCLength,maxci.msl,maxci.LengthCMSL, maxci.Long, maxci.Poids),lev.names=c("SMI\n(controlled for Elytron Length)","SMI","Elytron Length\n(controlled for SMI)","Elytron Length","Mass"))  
df



df$lev.names2 <- factor(df$lev.names, as.character(df$lev.names))

library(ggplot2)
plot(x=df$effect.size)


p_new<-ggplot(df,aes(lev.names2, effect.size))
p_new <- p_new + geom_hline(yintercept=0,lty=2) + geom_errorbar(aes(ymin=minci, ymax=maxci), width=0,color="black") + geom_point(aes(size=2)) 
p_new <- p_new + guides(size=FALSE,shape=FALSE) + scale_shape_manual(values=c(16,16,16,1,1,1))
p_new <- p_new + theme_bw() + xlab("Morphological measure/Body condition index") + ylab(expression(paste("Effect size",italic(' r'))))
p_new <- p_new + theme(axis.text.x=element_text(size=rel(1.5)),
               axis.title.y=element_text(size=rel(1.2)),
               axis.title.x=element_text(size=rel(1.2)),
               axis.text.y=element_text(size=rel(1.5)),
               panel.grid.minor=element_blank(),
               panel.grid.major.x=element_blank())

p_new <- p_new+ coord_flip()+theme(text = element_text(size=15))


png("/home/mark/Dropbox/condition/JAE/MEE ?/SuppCoefPlotEffectNew.png",width=2100, height=2100,res=300)
p_new+geom_text(aes(label=round(effect.size,3)),hjust=0.5, vjust=-0.5,size=7)
 
dev.off()





M5.nb<-glm.nb(eggs~milieu*moyLong+milieu*MSL,na.action=na.fail,data=Fecond)
library(MuMIn)
(gg5.nb <- dredge(M5.nb,rank="AICc", extra="R^2"))
head(gg5.nb)
write.table(gg5.nb, "/home/mark/Dropbox/condition/JAE/MEE ?/eggsMSLLong.txt", sep="\t")

#install.packages("rsq")
library(rsq)

summary(M5.nb)


$r.squared 


M5.nb.b<-glm.nb(eggs~moyLong+MSL+TreatAdult,na.action=na.fail,data=Fecond)

rsq.partial(M5.nb.b,adj=FALSE)


rsq(M5.nb.b,adj=FALSE,type="n")


r.squaredLR(M5.nb.b, null = NULL, null.RE = FALSE)


M5.nc<-glm.nb(eggs~moyLong+TreatAdult+TreatLarve*MSL,na.action=na.fail,data=Fecond)
summary(M5.nc)




partial.d<-function(t.val,df,n1,n2){
  d<-t.val*(n1+n2)/(sqrt(n1*n2)*sqrt(df))
  names(d)<-"effect size d"
  return(d)
}

tapply(Fecond$TreatAdult,Fecond$TreatAdult,length)

partial.d(-5.400,215,111,110)
#effect size d 
#   -0.7365614

coef(summary(M5.nc))[4,3]

d.part <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula,data=d)
  return(partial.d(coef(summary(fit))[3,3],215,111,110))
}

# bootstrapping with 10000 replications
(results <- boot(data=Fecond, statistic=d.part,R=10000, formula=eggs~moyLong+TreatAdult+TreatLarve*MSL))

#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = Fecond, statistic = d.part, R = 10000, formula = eggs ~ 
#    moyLong + TreatAdult + TreatLarve * MSL)#
#

#Bootstrap Statistics :
#      original       bias    std. error
#t1* -0.7677614 -0.009982402   0.1490804

library(boot)
(Boot<-boot.ci(results, type="bca"))

#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   (-1.0556, -0.4715 )  
#Calculations and Intervals on Original Scale



M5.nc<-glm.nb(eggs~moyLong+TreatAdult+TreatLarve+MSL,na.action=na.fail,data=Fecond)
summary(M5.nc)


d.part <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula,data=d)
  return(partial.d(coef(summary(fit))[4,3],215,111,110))
}

# bootstrapping with 10000 replications
(results <- boot(data=Fecond, statistic=d.part,R=10000, formula=eggs~moyLong+TreatAdult+TreatLarve+MSL))

#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = Fecond, statistic = d.part, R = 10000, formula = eggs ~ 
#    moyLong + TreatAdult + TreatLarve * MSL)#
#

#Bootstrap Statistics :
#     original       bias    std. error
#t1* 0.3321831 -0.003523253   0.1340011






library(boot)
(Boot<-boot.ci(results, type="bca"))







M1.nb<-glm.nb(eggs~milieu*moyLong,na.action=na.fail,data=Fecond)
library(MuMIn)
(gg1.nb <- dredge(M1.nb,rank="AICc", extra="R^2"))
write.table(gg1.nb, "/home/mark/Dropbox/condition/JAE/MEE ?/eggsLong.txt", sep="\t")
rsq.partial(M1.nb,adj=FALSE)


summary(M1.nb)


#Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   3.62380    0.57899   6.259 3.88e-10 ***
#milieuNlPNa  -0.30984    0.09861  -3.142 0.001677 ** 
#milieuPNlNa   0.06178    0.09740   0.634 0.525902    
#milieuPNlPNa -0.32686    0.09752  -3.352 0.000803 ***
#moyLong       0.09991    0.06133   1.629 0.103263    


partial.r(1.629,216)
#effect size r 
#    0.1101648 



r.part <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- glm.nb(formula,data=d)
  return(partial.r(coef(summary(fit))[5,3],218))
}


# bootstrapping with 10000 replications
(results <- boot(data=Fecond, statistic=r.part,R=10000, formula=eggs~milieu+moyLong))

#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = Fecond, statistic = r.part, R = 10000, formula = eggs ~ 
#    milieu + moyLong)#
#

#Bootstrap Statistics :
#     original       bias    std. error
#t1* 0.1096799 0.0003524072  0.06802651



(Boot<-boot.ci(results, type="bca"))


#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   (-0.0296,  0.2373 )  
#Calculations and Intervals on Original Scale


Long<-as.numeric(results$t0)
minci.Long<-Boot$bca[,4]
maxci.Long<-Boot$bca[,5]



M2.nb<-glm.nb(eggs~milieu*poids,na.action=na.fail,data=Fecond)
library(MuMIn)
(gg2.nb <- dredge(M2.nb,rank="AICc", extra="R^2"))
write.table(gg2.nb, "/home/mark/Dropbox/condition/JAE/MEE ?/eggsPoids.txt", sep="\t")

library(rsq)
rsq.partial(M2.nb,adj=FALSE)

M2.nb<-glm.nb(eggs~milieu+poids,na.action=na.fail,data=Fecond)
library(MuMIn)

summary(M2.nb)

#Coefficients:
#              Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   3.903013   0.204539  19.082  < 2e-16 ***
#milieuNlPNa  -0.336517   0.096500  -3.487 0.000488 ***
#milieuPNlNa   0.053061   0.095268   0.557 0.577553    
#milieuPNlPNa -0.318283   0.095087  -3.347 0.000816 ***
#poids         0.005700   0.001673   3.408 0.000656 ***


partial.r<-function(t.val,df){
  r<-t.val/sqrt((t.val)^2+df)
  names(r)<-"effect size r"
  return(r)
}


partial.r(3.408,216)
#effect size r 
#    0.2258914 


coef(summary(M2.nb))[5,3]

library(boot)

r.part <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- glm.nb(formula,data=d)
  return(partial.r(coef(summary(fit))[5,3],218))
}

# bootstrapping with 10000 replications
(results <- boot(data=Fecond, statistic=r.part,R=10000, formula=eggs~milieu+poids))

#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = Fecond, statistic = r.part, R = 10000, formula = eggs ~ 
#    milieu + poids)#
#

#Bootstrap Statistics :
#     original       bias    std. error
#t1* 0.2248748 -0.000921994  0.06324002


(Boot<-boot.ci(results, type="bca"))
#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   ( 0.0976,  0.3430 )  
#Calculations and Intervals on Original Scale



Poids<-as.numeric(results$t0)
minci.Poids<-Boot$bca[,4]
maxci.Poids<-Boot$bca[,5]



M3.nb<-glm.nb(eggs~milieu*MSL*moyLong*poids,na.action=na.fail,data=Fecond)
library(MuMIn)
(gg3.nb <- dredge(M3.nb,rank="AICc", extra="R^2"))
head(gg3.nb)

write.table(gg3.nb, "/home/mark/Dropbox/condition/JAE/MEE ?/eggsMSL.txt", sep="\t")

M3.nb<-glm.nb(eggs~milieu*MSL,na.action=na.fail,data=Fecond)
rsq.partial(M3.nb,adj=FALSE)



summary(M3.nb)

#Coefficients:
#              Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   3.612137   0.333966  10.816  < 2e-16 ***
#milieuNlPNa  -0.294915   0.096568  -3.054  0.00226 ** 
#milieuPNlNa   0.115736   0.095850   1.207  0.22725    
#milieuPNlPNa -0.257597   0.096581  -2.667  0.00765 ** 
#MSL           0.007784   0.002696   2.887  0.00389 ** 



partial.r(2.887,216)
#effect size r 
#    0.1927518

r.part <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- glm.nb(formula,data=d)
  return(partial.r(coef(summary(fit))[5,3],218))
}


(results <- boot(data=Fecond, statistic=r.part,R=10000, formula=eggs~milieu+MSL))


#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = Fecond, statistic = r.part, R = 10000, formula = eggs ~ 
#    milieu + MSL)#
#

#Bootstrap Statistics :
#     original        bias    std. error
#t1* 0.1919039 -2.027873e-05  0.06094048



(Boot<-boot.ci(results, type="bca"))

#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   ( 0.0707,  0.3086 )  
#Calculations and Intervals on Original Scale



msl<-as.numeric(results$t0)
minci.msl<-Boot$bca[,4]
maxci.msl<-Boot$bca[,5]





M4.nb<-glm.nb(eggs~milieu+MSL+moyLong,na.action=na.fail,data=Fecond)
library(MuMIn)
(gg4.nb <- dredge(M4.nb,rank="AICc", extra="R^2"))
write.table(gg4.nb, "/home/mark/Dropbox/condition/JAE/MEE ?/eggsLong.txt", sep="\t")
rsq.partial(M4.nb,adj=FALSE)

summary(M4.nb)
#Coefficients:
#              Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   1.670477   0.765695   2.182 0.029136 *  
#milieuNlPNa  -0.338145   0.095946  -3.524 0.000425 ***
#milieuPNlNa   0.074964   0.094805   0.791 0.429104    
#milieuPNlPNa -0.288669   0.095438  -3.025 0.002489 ** 
#MSL           0.010235   0.002788   3.670 0.000242 ***
#moyLong       0.175800   0.062695   2.804 0.005046 ** 

partial.r(3.670,216)  ####MSL
#effect size r 
#    0.2422725 

partial.r(2.804,216) ####MoyLong
#effect size r 
#    0.1874077 

r.part <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- glm.nb(formula,data=d)
  return(partial.r(coef(summary(fit))[5,3],218))
}

(results <- boot(data=Fecond, statistic=r.part,R=10000, formula=eggs~milieu+MSL+moyLong))

#Bootstrap Statistics :
#     original       bias    std. error
#t1* 0.2412523 0.0003794327  0.05956672


(Boot<-boot.ci(results, type="bca"))


#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   ( 0.1203,  0.3523 )  
#Calculations and Intervals on Original Scale


MSLCLength<-as.numeric(results$t0)
minci.MSLCLength<-Boot$bca[,4]
maxci.MSLCLength<-Boot$bca[,5]



r.part <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- glm.nb(formula,data=d)
  return(partial.r(coef(summary(fit))[6,3],218))
}
(results <- boot(data=Fecond, statistic=r.part,R=10000, formula=eggs~milieu+MSL+moyLong))

#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = Fecond, statistic = r.part, R = 10000, formula = eggs ~ 
#    milieu + MSL + moyLong)#
#

#Bootstrap Statistics :
#     original        bias    std. error
#t1* 0.1865812 -0.0008142964  0.06727314



(Boot<-boot.ci(results, type="bca"))

#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   ( 0.0465,  0.3147 )  
#Calculations and Intervals on Original Scale


LengthCMSL<-as.numeric(results$t0)
minci.LengthCMSL<-Boot$bca[,4]
maxci.LengthCMSL<-Boot$bca[,5]




df<-data.frame(effect.size=c(MSLCLength,msl,LengthCMSL,Long,Poids), minci=c(minci.MSLCLength,minci.msl,minci.LengthCMSL, minci.Long, minci.Poids),maxci=c(maxci.MSLCLength,maxci.msl,maxci.LengthCMSL, maxci.Long, maxci.Poids),lev.names=c("SMI\n(controlled for Elytron Length)","SMI","Elytron Length\n(controlled for SMI)","Elytron Length","Mass"))  
df



df$lev.names2 <- factor(df$lev.names, as.character(df$lev.names))

library(ggplot2)
plot(x=df$effect.size)


p_new<-ggplot(df,aes(lev.names2, effect.size))
p_new <- p_new + geom_hline(yintercept=0,lty=2) + geom_errorbar(aes(ymin=minci, ymax=maxci), width=0,color="black") + geom_point(aes(size=2)) 
p_new <- p_new + guides(size=FALSE,shape=FALSE) + scale_shape_manual(values=c(16,16,16,1,1,1))
p_new <- p_new + theme_bw() + xlab("Morphological measure/Body condition index") + ylab(expression(paste("Effect size",italic(' r'))))
p_new <- p_new + theme(axis.text.x=element_text(size=rel(1.5)),
               axis.title.y=element_text(size=rel(1.2)),
               axis.title.x=element_text(size=rel(1.2)),
               axis.text.y=element_text(size=rel(1.5)),
               panel.grid.minor=element_blank(),
               panel.grid.major.x=element_blank())

p_new <- p_new+ coord_flip()+theme(text = element_text(size=15))


png("/home/mark/Dropbox/condition/JAE/MEE ?/SuppCoefPlotEffectNew.png",width=2100, height=2100,res=300)
p_new+geom_text(aes(label=round(effect.size,3)),hjust=0.5, vjust=-0.5,size=7)
 
dev.off()



df<-data.frame(effect.size=c(MSLCLength,LengthCMSL,Poids), minci=c(minci.MSLCLength,minci.LengthCMSL, minci.Poids),maxci=c(maxci.MSLCLength,maxci.LengthCMSL, maxci.Poids),lev.names=c("SMI","Elytron Length","Mass"))  
df



df$lev.names2 <- factor(df$lev.names, as.character(df$lev.names))

library(ggplot2)
plot(x=df$effect.size)


p_new<-ggplot(df,aes(lev.names2, effect.size))
p_new <- p_new + geom_hline(yintercept=0,lty=2) + geom_errorbar(aes(ymin=minci, ymax=maxci), width=0,color="black") + geom_point(aes(size=2)) 
p_new <- p_new + guides(size=FALSE,shape=FALSE) + scale_shape_manual(values=c(16,16,16,1,1,1))
p_new <- p_new + theme_bw() + xlab("Morphological measure/\nBody condition index") + ylab(expression(paste("Effect size",italic(' r'))))
p_new <- p_new + theme(axis.text.x=element_text(size=rel(1.5)),
               axis.title.y=element_text(size=rel(1.2)),
               axis.title.x=element_text(size=rel(1.2)),
               axis.text.y=element_text(size=rel(1.5)),
               panel.grid.minor=element_blank(),
               panel.grid.major.x=element_blank())

p_new <- p_new+ coord_flip()+theme(text = element_text(size=15))+geom_text(aes(label=round(effect.size,3)),hjust=0.5, vjust=-0.5,size=7)


png("/home/mark/Dropbox/condition/JAE/MEE ?/CoefPlotEffectNew.png",width=2100, height=2100,res=300)
p_new
 
dev.off()






Fit <- glm.nb(eggs~milieu+poids,na.action=na.fail,data=Fecond)
Fit

newdat = data.frame(milieu = Fecond$milieu,poids=Fecond$poids,eggs=Fecond$eggs)

head(newdat)

pred = predict(Fit, newdata = newdat, level = 0, interval = "confidence")

newdat<-cbind(newdat,pred)

names(newdat)
newdat$lwr
head(newdat)

des = model.matrix(formula("~milieu+poids"), newdat)

predvar = diag( des %*% vcov(Fit) %*% t(des) )


newdat$lower = with(newdat, pred - 2*sqrt(predvar) )
newdat$upper = with(newdat, pred + 2*sqrt(predvar) )


names(newdat)
head(newdat)

factor(newdat$milieu)

   


(EggsMass<-ggplot(newdat, aes(x = poids, y = eggs, color=milieu))+ geom_point(aes(x=poids, y = eggs),size=3)+ geom_ribbon(data = newdat, aes(y = NULL, ymin = exp(lower), ymax = exp(upper),color = NULL, fill = milieu), alpha = .15) + geom_line(data = newdat, aes(y = exp(pred)), size = 1) + theme_bw()+scale_colour_manual(values=c("black","grey","red","rosybrown1"),name="", breaks=c("NlNa","NlPNa","PNlNa","PNlPNa"),labels=c("Rich larval/Rich adult","Rich larval/Poor adult","Poor larval/Rich adult","Poor larval/Poor adult")) + theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ labs(x = "Mass (mg)", y = "Total fecundity (total number of eggs)")+scale_fill_manual(values=c("black","grey","red","rosybrown1"), name="fill",guide="none"))

EggsMass




Fit <- glm.nb(eggs~milieu+MSL+moyLong,na.action=na.fail,data=Fecond)
Fit

newdat = data.frame(milieu = Fecond$milieu,MSL=median(Fecond$MSL),eggs=Fecond$eggs,moyLong=Fecond$moyLong)

head(newdat)

pred = predict(Fit, newdata = newdat, level = 0, interval = "confidence")

newdat<-cbind(newdat,pred)

names(newdat)
newdat$lwr
head(newdat)

des = model.matrix(formula("~milieu+MSL+moyLong"), newdat)

predvar = diag( des %*% vcov(Fit) %*% t(des) )


newdat$lower = with(newdat, pred - 2*sqrt(predvar) )
newdat$upper = with(newdat, pred + 2*sqrt(predvar) )


names(newdat)
head(newdat)


(EggsLENGTH<-ggplot(newdat, aes(x = moyLong, y = eggs, color=milieu))+ geom_point(aes(x=moyLong, y = eggs),size=3)+ geom_ribbon(data = newdat, aes(y = NULL, ymin = exp(lower), ymax = exp(upper),color = NULL, fill = milieu), alpha = .15) + geom_line(data = newdat, aes(y = exp(pred)), size = 1) + theme_bw()+scale_colour_manual(values=c("black","grey","red","rosybrown1"),name="", breaks=c("NlNa","NlPNa","PNlNa","PNlPNa"),labels=c("Rich larval/Rich adult","Rich larval/Poor adult","Poor larval/Rich adult","Poor larval/Poor adult")) + theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ labs(x = "Elytron length (mm)", y = "Total fecundity (total number of eggs)")+scale_fill_manual(values=c("black","grey","red","rosybrown1"), name="fill",guide="none"))





Fit <- glm.nb(eggs~milieu+MSL+moyLong,na.action=na.fail,data=Fecond)
Fit

newdat = data.frame(milieu = Fecond$milieu,MSL=Fecond$MSL,eggs=Fecond$eggs,moyLong=median(Fecond$moyLong))

head(newdat)

pred = predict(Fit, newdata = newdat, level = 0, interval = "confidence")

newdat<-cbind(newdat,pred)

names(newdat)
newdat$lwr
head(newdat)

des = model.matrix(formula("~milieu+MSL+moyLong"), newdat)

predvar = diag( des %*% vcov(Fit) %*% t(des) )


newdat$lower = with(newdat, pred - 2*sqrt(predvar) )
newdat$upper = with(newdat, pred + 2*sqrt(predvar) )


names(newdat)
head(newdat)


(EggsSMI<-ggplot(newdat, aes(x = MSL, y = eggs, color=milieu))+ geom_point(aes(x=MSL, y = eggs),size=3)+ geom_ribbon(data = newdat, aes(y = NULL, ymin = exp(lower), ymax = exp(upper),color = NULL, fill = milieu), alpha = .15) + geom_line(data = newdat, aes(y = exp(pred)), size = 1) + theme_bw()+scale_colour_manual(values=c("black","grey","red","rosybrown1"),name="Feeding treatment:", breaks=c("NlNa","NlPNa","PNlNa","PNlPNa"),labels=c("Rich larval/Rich adult","Rich larval/Poor adult","Poor larval/Rich adult","Poor larval/Poor adult")) + theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ labs(x = "SMI (mg)", y = "Total fecundity (total number of eggs)")+scale_fill_manual(values=c("black","grey","red","rosybrown1"), name="fill",guide="none"))







library(gtable)
library(gridExtra)
library(ggpubr)
library(grid)
legend = gtable_filter(ggplotGrob(EggsSMI+ theme(legend.position="bottom")), "guide-box")


png("/home/mark/Dropbox/condition/JAE/MEE ?/TotalFecondTraitCoef_New.png",width=2500, height=1500,res=150)

pushViewport(viewport(layout = grid.layout(33, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
legend$vp = viewport(layout.pos.row = 33, layout.pos.col = 1:2)
grid.draw(legend)
print(p_new+ggtitle("a."), vp = vplayout(1:16,1))
print(EggsMass+ggtitle("b.") + theme(legend.position="none"), vp = vplayout(1:16, 2))
print(EggsLENGTH+ggtitle("c.") + theme(legend.position="none"), vp = vplayout(17:32, 1))
#print(EggsOLS+ggtitle("d.") + theme(legend.position="none"), vp = vplayout(16:29, 11:19))
print(EggsSMI+ggtitle("d.") + theme(legend.position="none"), vp = vplayout(17:32, 2))
#print(Eggs+ggtitle("f.")+ theme(legend.position="none"), vp = vplayout(15:21, 3:6))
dev.off()






Fit <- glm.nb(eggs~milieu*MSL+moyLong,na.action=na.fail,data=Fecond)
summary(Fit)

#                  Estimate Std. Error z value Pr(>|z|)   
#(Intercept)       1.918191   0.969317   1.979  0.04783 * 
#milieuNlPNa      -2.268449   0.957289  -2.370  0.01780 * 
#milieuPNlNa       0.592503   0.925329   0.640  0.52197   
#milieuPNlPNa      0.021366   0.951073   0.022  0.98208   
#MSL               0.007903   0.006010   1.315  0.18847   
#moyLong           0.179534   0.061693   2.910  0.00361 **
#milieuNlPNa:MSL   0.015947   0.007884   2.023  0.04311 * 
#milieuPNlNa:MSL  -0.004453   0.007654  -0.582  0.56066   
#milieuPNlPNa:MSL -0.002798   0.007940  -0.352  0.72456 


Fecond[which(Fecond$milieu=="NlPNa"),]

Fit2 <- glm.nb(eggs~MSL+moyLong,na.action=na.fail,data=Fecond[which(Fecond$milieu=="NlPNa"),])
summary(Fit2)


coef(summary(Fit2))[2,3]

tapply(Fecond$milieu,Fecond$milieu,length)

r.part <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- glm.nb(formula,data=d)
  return(partial.r(coef(summary(fit))[2,3],51))
}

(results <- boot(data=Fecond[which(Fecond$milieu=="NlPNa"),], statistic=r.part,R=10000, formula=eggs~MSL+moyLong))

#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = Fecond[which(Fecond$milieu == "NlPNa"), ], statistic = r.part, 
#    R = 10000, formula = eggs ~ MSL + moyLong)#
#

#Bootstrap Statistics :
#     original      bias    std. error
#t1* 0.4761921 0.002006584  0.09223098



(Boot<-boot.ci(results, type="bca"))

#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   ( 0.2686,  0.6315 )  
#Calculations and Intervals on Original Scale




Fit2 <- glm.nb(eggs~MSL+moyLong,na.action=na.fail,data=Fecond[which(Fecond$milieu=="NlNa"),])
summary(Fit2)


coef(summary(Fit2))[2,3]

r.part <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- glm.nb(formula,data=d)
  return(partial.r(coef(summary(fit))[2,3],53))
}

(results <- boot(data=Fecond[which(Fecond$milieu=="NlNa"),], statistic=r.part,R=10000, formula=eggs~MSL+moyLong))


#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = Fecond[which(Fecond$milieu == "NlNa"), ], statistic = r.part, 
#    R = 10000, formula = eggs ~ MSL + moyLong)#
#

#Bootstrap Statistics :
#     original      bias    std. error
#t1* 0.1482804 0.005668921   0.1238913





(Boot<-boot.ci(results, type="bca"))

#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   (-0.1247,  0.3673 )  
#Calculations and Intervals on Original Scale










Fit2 <- glm.nb(eggs~MSL+moyLong,na.action=na.fail,data=Fecond[which(Fecond$milieu=="PNlNa"),])
summary(Fit2)
plot(Fit2)

coef(summary(Fit2))[2,3]

r.part <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- glm.nb(formula,data=d)
  return(partial.r(coef(summary(fit))[2,3],52))
}

(results <- boot(data=Fecond[which(Fecond$milieu=="PNlNa"),], statistic=r.part,R=10000, formula=eggs~MSL+moyLong))

#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = Fecond[which(Fecond$milieu == "PNlNa"), ], statistic = r.part, 
#    R = 10000, formula = eggs ~ MSL + moyLong)#
#

#Bootstrap Statistics :
#     original      bias    std. error
#t1* 0.2057181 -0.01081126   0.1559435



(Boot<-boot.ci(results, type="bca"))

#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   (-0.1139,  0.4898 )  
#Calculations and Intervals on Original Scale







Fit2 <- glm.nb(eggs~MSL+moyLong,na.action=na.fail,data=Fecond[which(Fecond$milieu=="PNlPNa"),])
summary(Fit2)


coef(summary(Fit2))[2,3]

r.part <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- glm.nb(formula,data=d)
  return(partial.r(coef(summary(fit))[2,3],53))
}

(results <- boot(data=Fecond[which(Fecond$milieu=="PNlPNa"),], statistic=r.part,R=10000, formula=eggs~MSL+moyLong))


#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = Fecond[which(Fecond$milieu == "PNlPNa"), ], statistic = r.part, 
#    R = 10000, formula = eggs ~ MSL + moyLong)#
#

#Bootstrap Statistics :
#     original       bias    std. error
#t1* 0.1262508 -0.006954421   0.1189822




(Boot<-boot.ci(results, type="bca"))

#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   (-0.1043,  0.3660 )  
#Calculations and Intervals on Original Scale








newdat = data.frame(milieu = Fecond$milieu,MSL=Fecond$MSL,eggs=Fecond$eggs,moyLong=median(Fecond$moyLong))

head(newdat)

pred = predict(Fit, newdata = newdat, level = 0, interval = "confidence")

newdat<-cbind(newdat,pred)

names(newdat)
newdat$lwr
head(newdat)

des = model.matrix(formula("~milieu+MSL+moyLong"), newdat)

predvar = diag( des %*% vcov(Fit) %*% t(des) )


newdat$lower = with(newdat, pred - 2*sqrt(predvar) )
newdat$upper = with(newdat, pred + 2*sqrt(predvar) )


names(newdat)
head(newdat)


(EggsSMIInt<-ggplot(newdat, aes(x = MSL, y = eggs, color=milieu))+ geom_point(aes(x=MSL, y = eggs),size=3)+ geom_ribbon(data = newdat, aes(y = NULL, ymin = exp(lower), ymax = exp(upper),color = NULL, fill = milieu), alpha = .15) + geom_line(data = newdat, aes(y = exp(pred)), size = 1) + theme_bw()+scale_colour_manual(values=c("black","grey","red","rosybrown1"),name="Feeding treatment:", breaks=c("NlNa","NlPNa","PNlNa","PNlPNa"),labels=c("Rich larval/Rich adult","Rich larval/Poor adult","Poor larval/Rich adult","Poor larval/Poor adult")) + theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+ labs(x = "SMI (mg)", y = "Total fecundity (total number of eggs)")+scale_fill_manual(values=c("black","grey","red","rosybrown1"), name="fill",guide="none"))


######

setwd("~/Dropbox/condition/ANALYSE")

INDICES= read.csv(file = "CALCULINDICES.csv", header = TRUE)

INDICES<-INDICES[which(INDICES$FecondIncomplet==0),]
#attach(INDICES)
names(INDICES)



MSL<- INDICES$poids*((mean(INDICES$moyLong)/INDICES$moyLong)^2.959333)

INDICES$MSL<-MSL




library(dplyr)
cdat <- INDICES %>%
  group_by(TreatLarve) %>%
  summarise(poids.mean = mean(poids),
            sem = sd(poids)/sqrt(length(poids)),
            upper.95=quantile(poids,probs=c(0.975)),
            lower.95=quantile(poids,probs=c(0.025)),
            ci.low = mean(poids) - 1.96*sem,
            ci.upp = mean(poids) + 1.96*sem)
cdat


cdat.dens <- ggplot_build(ggplot(INDICES, aes(x=poids, colour=TreatLarve)) + geom_density())$data[[1]] %>%
  mutate(TreatLarve = ifelse(group == 1, "Nl", "PNl"))%>%left_join(cdat) %>%
  select(y, x, TreatLarve, poids.mean, sem, ci.low, ci.upp) %>%
  group_by(TreatLarve) %>%
  mutate(dens.mean = approx(x, y, xout = poids.mean)[[2]],
         dens.cilow = approx(x, y, xout = ci.low)[[2]],
         dens.ciupp = approx(x, y, xout = ci.upp)[[2]]) %>%
  select(-y, -x) %>%
  slice(1)

ribbon <- ggplot_build(ggplot(INDICES, aes(x=poids, colour=TreatLarve)) + geom_density())$data[[1]] %>%
  mutate(TreatLarve = ifelse(group == 1, "Nl", "PNl")) %>%
  left_join(cdat.dens) %>%
  group_by(TreatLarve) %>%
  filter(x >= ci.low & x <= ci.upp) %>%
  select(TreatLarve, x, y)


ribbon <- rbind(data.frame(TreatLarve = c("Nl", "PNl"), x = c(cdat.dens$ci.low[cdat.dens$TreatLarve=="Nl"], cdat.dens$ci.low[cdat.dens$TreatLarve=="PNl"]), y = c(0, 0)), as.data.frame(ribbon),               data.frame(TreatLarve = c("Nl", "PNl"), x = c(cdat.dens$ci.upp[cdat.dens$TreatLarve=="Nl"], cdat.dens$ci.upp[cdat.dens$TreatLarve=="PNl"]), y = c(0, 0)))

names(ribbon)

label2<-expression(~"\tF"[1][", "][325]~"= 6.648; p = 0.010")

DensityMass<-ggplot() + 
geom_density(data = INDICES, aes(x = poids, colour = TreatLarve)) + geom_polygon(data = ribbon, aes(x = x, y =y, fill = TreatLarve), alpha = .2) + 
geom_segment(data = cdat.dens, aes(x = poids.mean, xend = poids.mean, y = 0, yend = dens.mean, colour = TreatLarve), linetype = "dashed", size = 1)+theme_bw()+
scale_colour_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("Nl","PNl"),labels=c("Rich larval","Poor larval"))+
scale_fill_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("Nl","PNl"),labels=c("Rich larval","Poor larval"))+
annotate(geom = 'text', x = -Inf, y = Inf, label = "\tFligner-Killeen Test:\n\tchi-squared = 0.289, p-value = 0.591 \n\tLinear model (LM):", hjust = 0, vjust = 1.25,size=rel(4))+
annotate(geom = 'text', x = -Inf, y = Inf, label = label2, hjust = 0, vjust = 5.75,size=rel(4))+
theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+xlab("Mass (mg)")+ylab("Density")+ylim(0,0.035)

DensityMass



cdat <- INDICES %>%
  group_by(TreatLarve) %>%
  summarise(moyLong.mean = mean(moyLong),
            sem = sd(moyLong)/sqrt(length(moyLong)),
            upper.95=quantile(moyLong,probs=c(0.975)),
            lower.95=quantile(moyLong,probs=c(0.025)),
            ci.low = mean(moyLong) - 1.96*sem,
            ci.upp = mean(moyLong) + 1.96*sem)
cdat


cdat.dens <- ggplot_build(ggplot(INDICES, aes(x=moyLong, colour=TreatLarve)) + geom_density())$data[[1]] %>%
  mutate(TreatLarve = ifelse(group == 1, "Nl", "PNl"))%>%left_join(cdat) %>%
  select(y, x, TreatLarve, moyLong.mean, sem, ci.low, ci.upp) %>%
  group_by(TreatLarve) %>%
  mutate(dens.mean = approx(x, y, xout = moyLong.mean)[[2]],
         dens.cilow = approx(x, y, xout = ci.low)[[2]],
         dens.ciupp = approx(x, y, xout = ci.upp)[[2]]) %>%
  select(-y, -x) %>%
  slice(1)

ribbon <- ggplot_build(ggplot(INDICES, aes(x=moyLong, colour=TreatLarve)) + geom_density())$data[[1]] %>%
  mutate(TreatLarve = ifelse(group == 1, "Nl", "PNl")) %>%
  left_join(cdat.dens) %>%
  group_by(TreatLarve) %>%
  filter(x >= ci.low & x <= ci.upp) %>%
  select(TreatLarve, x, y)


ribbon <- rbind(data.frame(TreatLarve = c("Nl", "PNl"), x = c(cdat.dens$ci.low[cdat.dens$TreatLarve=="Nl"], cdat.dens$ci.low[cdat.dens$TreatLarve=="PNl"]), y = c(0, 0)), as.data.frame(ribbon),               data.frame(TreatLarve = c("Nl", "PNl"), x = c(cdat.dens$ci.upp[cdat.dens$TreatLarve=="Nl"], cdat.dens$ci.upp[cdat.dens$TreatLarve=="PNl"]), y = c(0, 0)))

names(ribbon)



label2<-expression(~"\tF"[1][", "][325]~"= 10.069; p = 0.002")

DensityLong<-ggplot() + 
geom_density(data = INDICES, aes(x = moyLong, colour = TreatLarve)) + geom_polygon(data = ribbon, aes(x = x, y =y, fill = TreatLarve), alpha = .2) + 
geom_segment(data = cdat.dens, aes(x = moyLong.mean, xend = moyLong.mean, y = 0, yend = dens.mean, colour = TreatLarve), linetype = "dashed", size = 1)+theme_bw()+
scale_colour_manual(values=c("black","red"),name="Larval feeding treatment:", breaks=c("Nl","PNl"),labels=c("Rich larval","Poor larval"))+
scale_fill_manual(values=c("black","red"),name="Larval feeding treatment:", breaks=c("Nl","PNl"),labels=c("Rich larval","Poor larval"))+
annotate(geom = 'text', x = -Inf, y = Inf, label = "\tFligner-Killeen Test:\n\t chi-squared = 0.101, p-value = 0.378 \n\tLinear model (LM):", hjust = 0, vjust = 1.25,size=rel(4))+
annotate(geom = 'text', x = -Inf, y = Inf, label = label2, hjust = 0, vjust = 5.75,size=rel(4))+
theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+xlab("Elytron length (mm)")+ylab("Density")+ylim(0,1.1)

DensityLong

library(ggplot2)
ggplot(data=INDICES,aes(x = MSL,fill=INDICES$TreatLarve))+geom_histogram(binwidth=0.5)

ggplot(data=INDICES,aes(x = MSL,color=milieu))+geom_density()+theme_classic()

library(dplyr)
#library(plyr)
cdat <- INDICES %>%
  group_by(TreatLarve) %>%
  summarise(MSL.mean = mean(MSL),
            sem = sd(MSL)/sqrt(length(MSL)),
            upper.95=quantile(MSL,probs=c(0.975)),
            lower.95=quantile(MSL,probs=c(0.025)),
            ci.low = mean(MSL) - 1.96*sem,
            ci.upp = mean(MSL) + 1.96*sem)
cdat

#mu <- ddply(INDICES, "TreatLarve", summarise, grp.mean=mean(MSL))

#mu.95.upper <- ddply(INDICES, "TreatLarve", summarise, upper.95=quantile(INDICES$MSL,probs=c(0.975)))

#mu.95.lower <- ddply(INDICES, "TreatLarve", summarise, lower.95=quantile(INDICES$MSL,probs=c(0.025)))

#mu.merge<-merge(mu.95.lower,mu.95.upper,by="TreatLarve")
#mu.merge
#mu.merge<-merge(mu,mu.merge,by="TreatLarve")
#mu.merge

#install.packages("dplyr")
#library(plyr)

cdat.dens <- ggplot_build(ggplot(INDICES, aes(x=MSL, colour=TreatLarve)) + geom_density())$data[[1]] %>%
  mutate(TreatLarve = ifelse(group == 1, "Nl", "PNl"))%>%left_join(cdat) %>%
  select(y, x, TreatLarve, MSL.mean, sem, ci.low, ci.upp) %>%
  group_by(TreatLarve) %>%
  mutate(dens.mean = approx(x, y, xout = MSL.mean)[[2]],
         dens.cilow = approx(x, y, xout = ci.low)[[2]],
         dens.ciupp = approx(x, y, xout = ci.upp)[[2]]) %>%
  select(-y, -x) %>%
  slice(1)

ribbon <- ggplot_build(ggplot(INDICES, aes(x=MSL, colour=TreatLarve)) + geom_density())$data[[1]] %>%
  mutate(TreatLarve = ifelse(group == 1, "Nl", "PNl")) %>%
  left_join(cdat.dens) %>%
  group_by(TreatLarve) %>%
  filter(x >= ci.low & x <= ci.upp) %>%
  select(TreatLarve, x, y)


ribbon <- rbind(data.frame(TreatLarve = c("Nl", "PNl"), x = c(cdat.dens$ci.low[cdat.dens$TreatLarve=="Nl"], cdat.dens$ci.low[cdat.dens$TreatLarve=="PNl"]), y = c(0, 0)), as.data.frame(ribbon),               data.frame(TreatLarve = c("Nl", "PNl"), x = c(cdat.dens$ci.upp[cdat.dens$TreatLarve=="Nl"], cdat.dens$ci.upp[cdat.dens$TreatLarve=="PNl"]), y = c(0, 0)))

names(ribbon)



annotate(geom="text", x=120, y=0.054, label=" ",size=rel(3))


'Fligner-Killeen Test of Homogeneity of Variances:\nchi-squared = 9.712, p-value = 0.002\n\nGeneralized least squares (GLS): F'
~"Fligner-Killeen Test of Homogeneity of Variances:\nchi-squared = 9.712, p-value = 0.002 \n\nGeneralized least squares (GLS):"

label2<-expression(~"\tF"[1][", "][325]~"= 0.833; p = 0.362")

DensityMSL<-ggplot() + 
geom_density(data = INDICES, aes(x = MSL, colour = TreatLarve)) + 
geom_polygon(data = ribbon, aes(x = x, y =y, fill = TreatLarve), alpha = .2) + 
geom_segment(data = cdat.dens, aes(x = MSL.mean, xend = MSL.mean, y = 0, yend = dens.mean, colour = TreatLarve), linetype = "dashed", size = 1)+
theme_bw()+
scale_colour_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("Nl","PNl"),labels=c("Rich larval","Poor larval"))+
scale_fill_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("Nl","PNl"),labels=c("Rich larval","Poor larval"))+
annotate(geom = 'text', x = -Inf, y = Inf, label = "\tFligner-Killeen Test:\n\tchi-squared = 9.712, p-value = 0.002 \n\tGeneralized least squares (GLS):", hjust = 0, vjust = 1.25,size=rel(4))+
annotate(geom = 'text', x = -Inf, y = Inf, label = label2, hjust = 0, vjust = 5.75,size=rel(4))+
theme(plot.title = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1)), legend.title=element_text(size= rel(1)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+xlab("SMI (mg)")+ylab("Density")+ylim(0,0.069)
DensityMSL


legend2=gtable_filter(ggplotGrob(DensityLong+theme(legend.position="bottom", legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)))), "guide-box")



png("/home/mark/Dropbox/condition/JAE/MEE ?/SMIFecondTraitSelection.png",height=3042, width=6415.2,res=300)
pushViewport(viewport(layout = grid.layout(22, 20)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(EggsSMIInt+ theme(legend.position="none")+ggtitle("a."),vp = vplayout(1:21, 1:14))
legend$vp = viewport(layout.pos.row = 22, layout.pos.col = 1:14)
grid.draw(legend)

print(DensityMass+ theme(legend.position="none")+ggtitle("b."), vp = vplayout(1:7, 14:20))
print(DensityLong+ theme(legend.position="none")+ggtitle("c."), vp = vplayout(8:14, 14:20))
print(DensityMSL+ theme(legend.position="none")+ggtitle("d."), vp = vplayout(15:21, 14:20))
legend2$vp = viewport(layout.pos.row = 22, layout.pos.col = 14:20)
grid.draw(legend2)
dev.off()





legend=gtable_filter(ggplotGrob(EggsSMI+theme(legend.position="right", legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)))), "guide-box")
legend2=gtable_filter(ggplotGrob(DensityLong+theme(legend.position="right", legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)))), "guide-box")

png("/home/mark/Dropbox/condition/JAE/MEE ?/TotalFecondTraitSelection.png",height=4455, width=3150,res=300)
pushViewport(viewport(layout = grid.layout(47, 20)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)


print(DensityMass+ theme(legend.position="none")+ggtitle("a."), vp = vplayout(1:14, 1:8))
print(DensityLong+ theme(legend.position="none")+ggtitle("b."), vp = vplayout(15:28, 1:8))
print(DensityMSL+ theme(legend.position="none")+ggtitle("c."), vp = vplayout(29:42, 1:8))
legend2$vp = viewport(layout.pos.row = 43:46, layout.pos.col = 1:9)
grid.draw(legend2)


print(EggsMass+ theme(legend.position="none")+ggtitle("d."),vp = vplayout(1:14, 9:20))
print(EggsLENGTH+ theme(legend.position="none")+ggtitle("e."), vp = vplayout(15:28, 9:20))
print(EggsSMIInt+ theme(legend.position="none")+ggtitle("f."), vp = vplayout(29:42, 9:20))
legend$vp = viewport(layout.pos.row = 43:46, layout.pos.col = 7:20)
grid.draw(legend)
dev.off()










require(multcomp)

Fecond$fmilieu<-factor(Fecond$milieu,levels=c("PNlPNa","NlNa","NlPNa","PNlNa"))

M4.nb<-glm.nb(eggs~fmilieu+MSL+moyLong,na.action=na.fail,data=Fecond)
library(MuMIn)
tuk <- glht(M4.nb, linfct = mcp(fmilieu = "Tukey"))
tuk


library(rstatix)  # https://github.com/kassambara/rstatix
stat.test <- aov(eggs~fmilieu,data=Fecond) %>%tukey_hsd()

library(ggpubr)
ggboxplot(Fecond, x = "fmilieu", y = "eggs") +stat_pvalue_manual(stat.test, label = "p = {round(p.adj,4)}", y.position = c(210, 220, 230, 240, 250 ,260))

summary(tuk)

summary(tuk)
SumTuk<-data.frame(cbind(Estimates=summary(tuk)$test$coefficients,Std.Error=summary(tuk)$test$sigma,zvalue=summary(tuk)$test$tstat,pvalues=summary(tuk)$test$pvalues),group1=c("NlNa","NlPNa","PNlNa","NlPNa","PNlNa","PNlNa"),group2=c("PNlPNa","PNlPNa","PNlPNa","NlNa","NlNa","NlPNa"))

#SumTuk$Treatment<-rownames(SumTuk)

#install.packages("finalfit")
library(finalfit)
SumTuk$p.adj<-finalfit::p_tidy(SumTuk$pvalues, digits = 3)

#install.packages("remotes")
#remotes::install_github("svenhalvorson/SvenR")
library("SvenR")
#SumTuk$p.adj<-format_p(SumTuk$pvalues, min_digits = 3, max_digits = 3, level = 0.05, write_p = FALSE)

SumTuk
stat.test



library(ggplot2)

Eggs<-ggplot(Fecond, aes(y=eggs,x=milieu))+geom_boxplot(outlier.shape = NA,aes(colour=TreatAdult),show.legend = FALSE)+geom_jitter(position = position_jitter(0.1),aes(colour=TreatAdult,shape=TreatLarve),size=4)+theme_bw()+scale_colour_manual(values=c("black","red"),name="Feeding treatment:", breaks=c("N","PN"),labels=c("Rich adult feeding treatment","Poor adult feeding treatment"))+scale_shape_manual(values=c(1,19),name="", breaks=c("N","PN"),labels=c("Rich larval feeding treatment","Poor larval feeding treatment"))+ labs(x = "Feeding treatment", y = "Total fecundity (total number of eggs)")+ scale_x_discrete(labels=c("NlNa" = "Rich larval/Rich adult", "NlPNa" = "Rich larval/Poor adult","PNlNa" = "Poor larval/Rich adult","PNlPNa" = "Poor larval/Poor adult"))+ theme(plot.title = element_text(size = rel(1.5)),axis.text.y = element_text(size = rel(1.5)),axis.text.x = element_text(size = rel(1.2)), axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size=rel(1.5)), legend.text=element_text(size = rel(1.5)), legend.title=element_text(size= rel(1.5)),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+stat_pvalue_manual(SumTuk, label = "p {p.adj}", y.position = c(210, 225, 235, 245, 255 ,265),size=5)

Eggs+ theme(legend.position="none")




png("/home/mark/Dropbox/condition/JAE/MEE ?/FigureBoxplotOeufsMilieuNew.png",width=2100, height=2100,res=300)

Eggs+ theme(legend.position="none")


dev.off()


































sum(rsq.partial(M5.nb.b,objR=NULL,adj=FALSE,type=c('v','kl','sse','lr','n'))$partial.rsq)













M6.nb<-glm.nb(eggs~milieu*poids,na.action=na.fail,data=Fecond)
library(MuMIn)
(gg6.nb <- dredge(M6.nb,rank="AICc", extra="R^2"))
head(gg6.nb,10)
write.table(gg6.nb, "/home/mark/Dropbox/condition/JAE/MEE ?/eggsMSLPoids.txt", sep="\t")





M5.nb<-glm.nb(eggs~milieu*moyLong+milieu*MSL,na.action=na.fail,data=Fecond)
library(MuMIn)
(gg5.nb <- dredge(M5.nb,rank="AICc", extra="R^2"))
head(gg5.nb)
write.table(gg5.nb, "/home/mark/Dropbox/condition/JAE/MEE ?/eggsMSLLong.txt", sep="\t")




sum(rsq.partial(M5.nb.b,objR=NULL,adj=FALSE,type=c('v','kl','sse','lr','n'))$partial.rsq)

M6.nb<-glm.nb(eggs~TreatLarve*TreatAdult*poids,na.action=na.fail,data=Fecond)
library(MuMIn)
(gg6.nb <- dredge(M6.nb,rank="AICc", extra="R^2"))
head(gg6.nb,10)
write.table(gg6.nb, "/home/mark/Dropbox/condition/JAE/MEE ?/eggsMSLPoids.txt", sep="\t")


M6.nb.b<-glm.nb(eggs~poids+TreatAdult,na.action=na.fail,data=Fecond)

rsq.partial(M6.nb.b,adj=FALSE)






M6.nb<-glm.nb(eggs~milieu+poids,na.action=na.fail,data=Fecond)
library(MuMIn)
(gg6.nb <- dredge(M6.nb,rank="AICc", extra="R^2"))
head(gg6.nb,10)
write.table(gg6.nb, "/home/mark/Dropbox/condition/JAE/MEE ?/eggsMSLPoids.txt", sep="\t")













names(Fecond)
library(MuMIn)
M7.nb<-glm.nb(eggs~moyLong*MSL*milieu,na.action=na.fail,data=Fecond)
(gg7.nb <- dredge(M7.nb,rank="AICc", extra="R^2"))

M7.nb<-glm.nb(eggs~moyLong*MSL*TreatLarve*TreatAdult,na.action=na.fail,data=Fecond)
(gg7.nb <- dredge(M7.nb,rank="AICc", extra="R^2"))



Fecond$fmilieu<-factor(Fecond$milieu,levels=c("NlNa","NlPNa","PNlNa","PNlPNa"))
levels(Fecond$fmilieu)

#"NlNa"   "NlPNa"  "PNlNa"  "PNlPNa"

M7.nb.a<-glm.nb(eggs~moyLong+MSL*fmilieu,na.action=na.fail,data=Fecond)
summary(M7.nb.a)

#Call:
#glm.nb(formula = eggs ~ moyLong + MSL * fmilieu, data = Fecond, 
#    na.action = na.fail, init.theta = 4.45029412, link = log)#

#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-4.1181  -0.7178  -0.1061   0.5311   2.1554  #

#Coefficients:
#                   Estimate Std. Error z value Pr(>|z|)   
#(Intercept)        1.918191   0.969317   1.979  0.04783 * 
#moyLong            0.179534   0.061693   2.910  0.00361 **
#MSL                0.007903   0.006010   1.315  0.18847   
#fmilieuNlPNa      -2.268449   0.957289  -2.370  0.01780 * 
#fmilieuPNlNa       0.592503   0.925329   0.640  0.52197   
#fmilieuPNlPNa      0.021366   0.951073   0.022  0.98208   
#MSL:fmilieuNlPNa   0.015947   0.007884   2.023  0.04311 * 
#MSL:fmilieuPNlNa  -0.004453   0.007654  -0.582  0.56066   
#MSL:fmilieuPNlPNa -0.002798   0.007940  -0.352  0.72456   
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#

#(Dispersion parameter for Negative Binomial(4.4503) family taken to be 1)#

#    Null deviance: 288.47  on 220  degrees of freedom
#Residual deviance: 233.98  on 212  degrees of freedom
#AIC: 2250.9#

#Number of Fisher Scoring iterations: 1#
#

#              Theta:  4.450 
#          Std. Err.:  0.444 #

# 2 x log-likelihood:  -2230.873

coef(summary(M7.nb.a))[3,3]


partial.r<-function(t.val,df){
  r<-t.val/sqrt((t.val)^2+df)
  names(r)<-"effect size r"
  return(r)
}


r.part <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- glm.nb(formula,data=d)
  return(partial.r(coef(summary(fit))[3,3],218))
}

library(boot)
(results <- boot(data=Fecond, statistic=r.part,R=10000, formula=eggs~moyLong+MSL*fmilieu))

#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = Fecond, statistic = r.part, R = 10000, formula = eggs ~ 
#    moyLong + MSL * fmilieu)#
#

#Bootstrap Statistics :
#      original      bias    std. error
#t1* 0.08871947 0.001617366  0.06877051


(Boot<-boot.ci(results, type="bca"))

#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   (-0.0465,  0.2238 )  
#Calculations and Intervals on Original Scale



RLRA<-as.numeric(results$t0)
minci.RLRA<-Boot$bca[,4]
maxci.RLRA<-Boot$bca[,5]





Fecond$fmilieu<-factor(Fecond$milieu,levels=c("NlPNa","PNlNa","PNlPNa","NlNa"))
levels(Fecond$fmilieu)

M7.nb.b<-glm.nb(eggs~moyLong+MSL*fmilieu,na.action=na.fail,data=Fecond)
summary(M7.nb.b)


library(boot)
(results <- boot(data=Fecond, statistic=r.part,R=10000, formula=eggs~moyLong+MSL*fmilieu))

#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = Fecond, statistic = r.part, R = 10000, formula = eggs ~ 
#    moyLong + MSL * fmilieu)#
#

#Bootstrap Statistics :
#     original      bias    std. error
#t1* 0.2968668 0.001154973  0.05956274


(Boot<-boot.ci(results, type="bca"))

#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   ( 0.1739,  0.4074 )  
#Calculations and Intervals on Original Scale


RLPA<-as.numeric(results$t0)
minci.RLPA<-Boot$bca[,4]
maxci.RLPA<-Boot$bca[,5]






Fecond$fmilieu<-factor(Fecond$milieu,levels=c("PNlNa","PNlPNa","NlNa","NlPNa"))
levels(Fecond$fmilieu)

M7.nb.c<-glm.nb(eggs~moyLong+MSL*fmilieu,na.action=na.fail,data=Fecond)
summary(M7.nb.c)


library(boot)
(results <- boot(data=Fecond, statistic=r.part,R=10000, formula=eggs~moyLong+MSL*fmilieu))

#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = Fecond, statistic = r.part, R = 10000, formula = eggs ~ 
#    moyLong + MSL * fmilieu)#
#

#Bootstrap Statistics :
#      original       bias    std. error
#t1* 0.04857492 -0.001512537  0.05843435


(Boot<-boot.ci(results, type="bca"))

#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   (-0.0616,  0.1674 )  
#Calculations and Intervals on Original Scale


PLRA<-as.numeric(results$t0)
minci.PLRA<-Boot$bca[,4]
maxci.PLRA<-Boot$bca[,5]





Fecond$fmilieu<-factor(Fecond$milieu,levels=c("PNlPNa","NlNa","NlPNa","PNlNa"))
levels(Fecond$fmilieu)

M7.nb.d<-glm.nb(eggs~moyLong+MSL*fmilieu,na.action=na.fail,data=Fecond)
summary(M7.nb.d)


library(boot)
(results <- boot(data=Fecond, statistic=r.part,R=10000, formula=eggs~moyLong+MSL*fmilieu))

#ORDINARY NONPARAMETRIC BOOTSTRAP#
#

#Call:
#boot(data = Fecond, statistic = r.part, R = 10000, formula = eggs ~ 
#    moyLong + MSL * fmilieu)#
#

#Bootstrap Statistics :
#      original       bias    std. error
#t1* 0.06506825 -0.002783836  0.05894534


(Boot<-boot.ci(results, type="bca"))


#BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
#Based on 10000 bootstrap replicates#

#CALL : 
#boot.ci(boot.out = results, type = "bca")#

#Intervals : 
#Level       BCa          
#95%   (-0.0390,  0.1976 )  
#Calculations and Intervals on Original Scale



PLPA<-as.numeric(results$t0)
minci.PLPA<-Boot$bca[,4]
maxci.PLPA<-Boot$bca[,5]

RLRA
minci.RLRA
maxci.RLRA
RLPA
minci.RLPA
maxci.RLPA
PLRA
minci.PLRA
maxci.PLRA
PLPA
minci.PLPA
maxci.PLPA


maxci.RLPA,maxci.RLRA,maxci.PLRA, maxci.PLPA

"NlNa" = "Rich larval\nand\nrich adult\n(R.L./R.A.)", "NlPNa" = "Rich larval\nand\npoor adult\n(R.L./P.A.)","PNlNa" = "Poor larval\nand\nrich adult\n(P.L./R.A.)","PNlPNa" = "Poor larval\nand\npoor adult\n(P.L./P.A.)"

df<-data.frame(effect.size=c(RLPA,RLRA,PLRA,PLPA), minci=c(minci.RLPA,minci.RLRA,minci.PLRA, minci.PLPA),maxci=c(maxci.RLPA,maxci.RLRA,maxci.PLRA, maxci.PLPA),lev.names=c("Rich larval\nand\nrich adult\n(R.L./R.A.)","Rich larval\nand\npoor adult\n(R.L./P.A.)","Poor larval\nand\nrich adult\n(P.L./R.A.)","Poor larval\nand\npoor adult\n(P.L./P.A.)"))  
df



df$lev.names2 <- factor(df$lev.names, as.character(df$lev.names))

library(ggplot2)
plot(x=df$effect.size)


p_new<-ggplot(df,aes(lev.names2, effect.size))
p_new <- p_new + geom_hline(yintercept=0,lty=2) + geom_errorbar(aes(ymin=minci, ymax=maxci), width=0,color="black") + geom_point(aes(size=2)) 
p_new <- p_new + guides(size=FALSE,shape=FALSE) + scale_shape_manual(values=c(16,16,16,1,1,1))
p_new <- p_new + theme_bw() + xlab("Morphological measure/Body condition index") + ylab(expression(paste("Effect size",italic(' r'))))
p_new <- p_new + theme(axis.text.x=element_text(size=rel(1.5)),
               axis.title.y=element_text(size=rel(1.2)),
               axis.title.x=element_text(size=rel(1.2)),
               axis.text.y=element_text(size=rel(1.5)),
               panel.grid.minor=element_blank(),
               panel.grid.major.x=element_blank())

p_new <- p_new+ coord_flip()+theme(text = element_text(size=15))


png("/home/mark/Dropbox/condition/JAE/MEE ?/SuppCoefPlotEffectNew.png",width=2100, height=2100,res=300)
p_new+geom_text(aes(label=round(effect.size,3)),hjust=0.5, vjust=-0.5,size=7)
 
dev.off()





rsq.partial(M7.nb.b,adj=FALSE)

plot(M7.nb.b)

M8.nb<-glm.nb(eggs~poids*milieu,na.action=na.fail,data=Fecond)
(gg8.nb <- dredge(M8.nb,rank="AICc", extra="R^2"))
