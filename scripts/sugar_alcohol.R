sug_alc <- read.csv("./data/efficacia_zucchero.csv")
sd(sug_alc$n_am,na.rm=T)
mean(sug_alc$n_am,na.rm=T)
boxplot(sug_alc$n_am,ylim=c(0,500))
sug_alc$sug_eff<-sug_alc$n_vd_sug/(sug_alc$n_vd_sug+sug_alc$n_vd_alc)
boxplot(sug_alc$sug_eff*100,ylim=c(0,100))
mean(sug_alc$sug_eff*100,na.rm=T)
sd(sug_alc$sug_eff*100,na.rm=T)
boxplot(sug_alc$sug_eff*100~sug_alc$id_op_alc,ylim=c(0,100))# sugar efficacy per operator
