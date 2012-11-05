#poisson
modglm_completo<-glm(t1_ss900~t0_treat+t0_ss900,data=fboxi,family="poisson")

#trasformazione arcsin
#asin(sqrt())
#sin()^2

summary(lm(sqrt(t1_ss900)~t0_treat+sqrt(t0_ss900),data=fboxi))