library(faraway)
?sexab
head(sexab)
str(sexab)

#variabile risposta: ptsd
#covariata: cpa
#variabile explicativa (explanatory variable): csa

#statistiche descrittive dei dati:
by(sexab,sexab$csa, summary)

coplot(ptsd~cpa|csa,sexab,col = "red", bg = "yellow", pch = 21,
                                                    bar.bg = c(fac = "light blue"),
                                                    panel = panel.smooth)
#osservo se c'è la stessa relazione tra cpa e ptsd all'interno dei gruppi csa y/n.
# se sì si può adattare i dati ad un modello che assume lo stesso effetto della variabile educ su inc in ciascun gruppo

#relazione tra covariata e variabile risposta
plot(ptsd~csa,sexab)
#identify(sexab$ptsd~sexab$csa,n=2)#per identificare i due outliers (point and click)
plot(ptsd~cpa,pch=as.character(csa),sexab,add=F) #idem com il coplot ma meno leggibile
#coefficiente di correlazione tra covariata e variabile risposta:
#se non c'è correlazione o è completamente diversa tra i due gruppi non serve la covariata
cor(sexab$ptsd,sexab$cpa)
cor(sexab$ptsd[as.integer(sexab$csa)==1],sexab$cpa[as.integer(sexab$csa)==1])
cor(sexab$ptsd[as.integer(sexab$csa)==2],sexab$cpa[as.integer(sexab$csa)==2])
#hanno valori tutto sommato simili (boomsma 2012)

#modello generale
rslt_t <- lm(ptsd ~ cpa,data=sexab)
plot(cpa, ptsd, data=sexab)
plot(sexab$ptsd,sexab$cpa)
plot(ptsd~cpa,pch=as.character(csa),sexab,add=F)
abline(rslt_t, col="green") # plotting linear regression line
rslt_A <- lm(ptsd ~ cpa,data=sexab[as.integer(sexab$csa)==1,])
rslt_N <- lm(ptsd ~ cpa,data=sexab[as.integer(sexab$csa)==2,])
abline(rslt_A, col="red") # regression line ‘Abused’
abline(rslt_N, col="blue") # regression line ‘NotAbused’
?plot

m1<-lm(ptsd ~ cpa+csa + cpa:csa,data=sexab)
summary(m1)
#It turns out that the interaction eﬀect of cpa with csa is statistically not sig-
#  niﬁcant. Hence, the assumption of diﬀerent regression slopes in the two groups
#does not seem to be plausible. That is great, because a necessary assumption for
#the analysis of covariance (ancova) is not rejected.

#modello senza interazione
m2 <- lm(ptsd ~ cpa + csa, sexab)
summary(m2)
abline(10.248, 0.551, col="red",lty=2) # regr. line ‘Abused’
abline(10.248-6.273, 0.551, col="blue",lty=2) # ‘NotAbused’



# freund_wilson -----------------------------------------------------------

FW11X02 <- read.table("~/Documenti/importanza_1_sync/Statistica/freund_wilson/EXAMPLE/FW11X02.txt", header=T, quote="")


# esempio_web -------------------------------------------------------------

# http://www.agr.kuleuven.ac.be/vakken/statisticsbyR/ANOVAbyRr/ANCOVAinR.htm
read.table("http://www.agr.kuleuven.ac.be/vakken/statisticsbyR/datasetsTXT/CH25PR07.txt")->Ch25pr07
names(Ch25pr07)<-c('Improv','RDexp','Replic','PriorImp'); Ch25pr07
attach(Ch25pr07)
RDexp<-factor(RDexp)
tapply(Improv,RDexp,length)
coplot(Improv~PriorImp|RDexp)
plot(Improv~PriorImp)
text(PriorImp+0.15, Improv, RDexp)
plot(Improv~PriorImp,pch=as.numeric(RDexp))
ResF<-lm(Improv~ RDexp+ PriorImp + RDexp:PriorImp)
summary(ResF)
plot(ResF)

ResR1<-lm(Improv~ RDexp+ PriorImp)
ResSR<-lm(Improv~  PriorImp)
anova(ResF, ResR1)
anova(ResSR,ResR1,ResF)


# saiur 2004 --------------------------------------------------------------
compensation <- read.delim("~/Documenti/importanza_1_sync/Statistica/crawley_intro_stat/compensation.txt")
attach(compensation)
names(compensation)

plot(Grazing,Fruit)
#boxplot(Fruit~Grazing) idem

model<-lm(Fruit~Root*Grazing)
summary.aov(model)
anova(model)

model2<-lm(Fruit~Grazing+Root)
anova(model,model2)


summary.lm(model2)

levels(Grazing)

sf<-split(Fruit,Grazing)
sr<-split(Root,Grazing)
plot(Root,Fruit,type="n",ylab="Seed production",xlab="Initial root diameter")
points(sr[[1]],sf[[1]],pch=16)
points(sr[[2]],sf[[2]])

abline(-127.829,23.56)
abline(-127.829+36.103,23.56,lty=2)

tapply(Fruit,Grazing,mean)
summary(aov(Fruit~Grazing))

-127.829+36.103+23.56*mean(Root)

-127.829+23.56*mean(Root)

#crawley the r book / intro to statistical analysis using r (saiur 2004?)

species <- read.delim("./scripts/species.txt")
attach(species)
names(species)

plot(Biomass,Species,type="n")
spp<-split(Species,pH)
bio<-split(Biomass,pH)
points(bio[[1]],spp[[1]],pch=16)
points(bio[[2]],spp[[2]],pch=17)

points(bio[[3]],spp[[3]])


model1<-glm(Species~Biomass*pH,poisson)
summary(model1)

model2<-glm(Species~Biomass+pH,poisson)
anova(model1,model2,test="Chi")

xv<-seq(0,10,0.1)
levels(pH)

length(xv)

phv<-rep("high",101)
yv<-predict(model1,list(pH=factor(phv),Biomass=xv),type="response")
lines(xv,yv)

phv<-rep("mid",101)
yv<-predict(model1,list(pH=factor(phv),Biomass=xv),type="response")
lines(xv,yv)

phv<-rep("low",101)
yv<-predict(model1,list(pH=factor(phv),Biomass=xv),type="response")
lines(xv,yv)
