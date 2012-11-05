#poisson

plot(fboxi$t0_ss900,fboxi$t1_ss900,type="n",xlab="varroe/900 api pre-trattamento",ylab="varroe/900 api post-trattamento")
pre<-split(fboxi$t0_ss900,fboxi$t0_treat)
post<-split(fboxi$t1_ss900,fboxi$t0_treat)
points(pre[[1]],post[[1]],pch=16)
points(pre[[2]],post[[2]],pch=17)

model1<-glm(t1_ss900~t0_treat*t0_ss900,data=fboxi,family="poisson")
summary(model1)
model2<-glm(t1_ss900~t0_treat+t0_ss900,data=fboxi,family="poisson")
summary(model2)
anova(model1,model2,test="Chi")

#ancova: se non significativa la differenza tra le pendenze si toglie l'interazione

xv<-seq(0,max(fboxi$t0_ss900),1)
levels(fboxi$t0_treat)

length(xv)

tr1<-factor(rep(levels(fboxi$t0_treat)[1],length(xv)))
y1v<-predict(model2,list(t0_treat=tr1,t0_ss900=xv),type="response")
lines(xv,y1v)

tr2<-factor(rep(levels(fboxi$t0_treat)[2],length(xv)))
y2v<-predict(model2,list(t0_treat=tr2,t0_ss900=xv),type="response")
lines(xv,y2v)

1-(y1v/y2v)
1-(exp(coef(model2)[1])/(exp(coef(model2)[1]+coef(model2)[2])))

#trasformazione sqrt (taylor 1987)
plot(sqrt(fboxi$t0_ss900),sqrt(fboxi$t1_ss900),type="n",xlab="sqrt varroe/900 api pre-trattamento",ylab="sqrt varroe/900 api post-trattamento")
pre<-split(sqrt(fboxi$t0_ss900),fboxi$t0_treat)
post<-split(sqrt(fboxi$t1_ss900),fboxi$t0_treat)
points(pre[[1]],post[[1]],pch=16)
points(pre[[2]],post[[2]],pch=17)


summary(lm(sqrt(t1_ss900)~0+t0_treat*sqrt(t0_ss900),data=fboxi))

mod1<-lm(sqrt(t1_ss900)~0+t0_treat*sqrt(t0_ss900),data=fboxi)
mod2<-lm(sqrt(t1_ss900)~0+t0_treat+sqrt(t0_ss900),data=fboxi)
anova(mod1,mod2)

xv<-seq(0,max(fboxi$t0_ss900),1)
levels(fboxi$t0_treat)

length(xv)

tr1<-factor(rep(levels(fboxi$t0_treat)[1],length(xv)))
y1v<-predict(model2,list(t0_treat=tr1,t0_ss900=xv),type="response")
lines(xv,y1v)

tr2<-factor(rep(levels(fboxi$t0_treat)[2],length(xv)))
y2v<-predict(model2,list(t0_treat=tr2,t0_ss900=xv),type="response")
lines(xv,y2v)

1-(y1v/y2v)
1-(exp(coef(model2)[1])/(exp(coef(model2)[1]+coef(model2)[2])))


####### provare a trasformare la covariata (sqrt) mantenendo l'uscita poisson
# e vedere che effetto otteniamo


#trasformazione sqrt (taylor 1987)
plot(fboxi$t0_ss900,fboxi$t1_ss900,type="n",xlab="sqrt varroe/900 api pre-trattamento",ylab="sqrt varroe/900 api post-trattamento")
pre<-split(fboxi$t0_ss900,fboxi$t0_treat)
post<-split(fboxi$t1_ss900,fboxi$t0_treat)
points(pre[[1]],post[[1]],pch=16)
points(pre[[2]],post[[2]],pch=17,col="red")

z<-sqrt(fboxi$t1_ss900)
z1<-sqrt(fboxi$t0_ss900)
fact<-fboxi$t0_treat


summary(lm(sqrt(t1_ss900)~0+t0_treat*sqrt(t0_ss900),data=fboxi))

mod3<-lm(sqrt(t1_ss900)~t0_treat*sqrt(t0_ss900),data=fboxi)
mod2<-lm(sqrt(t1_ss900)~t0_treat+sqrt(t0_ss900),data=fboxi)
mod1<-lm(z~0+fact:z1,data=fboxi)

anova(mod3,mod2,mod1)

xv<-seq(0,max(z1),0.1)
levels(fboxi$t0_treat)

length(xv)

tr1<-factor(rep(levels(fboxi$t0_treat)[1],length(xv)))
y1v<-predict(mod1,list(fact=tr1,z1=xv),type="response")
lines(xv^2,y1v^2)


tr2<-factor(rep(levels(fboxi$t0_treat)[2],length(xv)))
y2v<-predict(mod1,list(fact=tr2,z1=xv),type="response")
lines(xv^2,y2v^2)

1-(y1v^2/y2v^2)

# tr2<-factor(rep(levels(fboxi$t0_treat)[2],length(xv)))
# y2v<-predict(mod1,list(t0_treat=tr2,t0_ss900=xv),type="response")
# lines(xv,y2v)
# abline(mod1)

1-(y1v/y2v)
1-(exp(coef(model2)[1])/(exp(coef(model2)[1]+coef(model2)[2])))
