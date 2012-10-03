#
detach()
rm(list=ls())

# data load ---------------------------------------------------------------
fbox <- read.csv("../data/fbox_def.csv", sep=",")
fbox$t0_treat<-c(rep("fb",48),rep("nt",48))   #treatments: fbox, no treatment

# 900 bees sums -----------------------------------------------------------

fbox$t0_ss900<- fbox$t0_ss300_1 + fbox$t0_ss300_2 + fbox$t0_ss300_3 #total sugar shake time 0
fbox$t1_ss900<- fbox$t1_ss300_1 + fbox$t1_ss300_2 + fbox$t1_ss300_3 #total sugar shake time 1
fbox$t2_ss900<- fbox$t2_ss300_1 + fbox$t2_ss300_2 + fbox$t2_ss300_3 #total sugar shake time 2
fbox$t3_ss900<- fbox$t3_ss300_1 + fbox$t3_ss300_2 + fbox$t3_ss300_3 #total sugar shake time 3

# hives selection -------------------------------------------------------
minvar=1.5  #varroe minime per inclusione
maxvar=15 #varroe massime per inclusione
!is.na(fbox$t0_ss900-fbox$t1_ss900)->include # NAs removed (only time 0 and 1)
fbox$t0_ss900<maxvar & fbox$t0_ss900>minvar & include ->include #excluded most infested hives and hives with infestation under 1/900

fboxi<-fbox[include,] #dataset delle casse in analisi
rm(include)
#fboxi<-fboxi[order(fboxi$ss9_bt),] ????? inutile ?????

n_nt<-dim(fboxi[fboxi$t0_treat=="nt",])[1] # num ctr hives
n_fb<-dim(fboxi[fboxi$t0_treat=="fb",])[1]    # num fbox hives


# t1-t0 growth analysis ---------------------------------------------------

# t0 infestation
boxplot(t0_ss900/9~t0_treat,data=fboxi,names=c("trattati","non trattati"),ylab="infestazione (%)")

 #fboxi$eff_log<-log(ss9_at+0.5)-log(ss9_bt+0.5)
fboxi$t1_growth<-(fboxi$t1_ss900-fboxi$t0_ss900)/fboxi$t0_ss900*100

#boxplot(t1_growth~tratt)
boxplot(fboxi$t1_growth~fboxi$t0_treat)

fboxi_nt<-fboxi[fboxi$t0_treat=="nt",]
fboxi_fb<-fboxi[fboxi$t0_treat=="fb",]

plot(fboxi_nt$t0_ss900,fboxi_nt$t1_ss900)
abline(fboxi_nt$t0_ss900,fboxi_nt$t1_ss900)
lm(t1_ss900~t0_ss900,data=fboxi_nt)
#plot(glm(ss9_at~sqrt(ss9_bt),family=poisson))


points(fboxi_fb$t0_ss900,fboxi_fb$t1_ss900,pch=3)
abline(fboxi_fb$t0_ss900,fboxi_fb$t1_ss900)

### henderson_tilton
#ht.efficacy<-function(tb,ta,cb,ca){100*(1-((ta*mean(cb))/(tb*mean(ca))))}
ht.efficacy<-function(tb,ta,cb,ca){100*(1-((ta/tb)*mean(cb/ca)))}
ht.efficacy<-function(tb,ta,cb,ca){100*(1-((ta/tb)/mean(ca/cb)))}
#ht.efficacy<-function(tb,ta,cb,ca){100*(1-((mean(ta/tb)*mean(cb/ca))))}

fboxi_fb$t1_ht.eff<-ht.efficacy(fboxi_fb$t0_ss900,fboxi_fb$t1_ss900,fboxi_nt$t0_ss900,fboxi_nt$t1_ss900)
mean(fboxi_fb$t1_ht.eff)

sin(t.test(asin(sqrt(fboxi_fb$t1_ht.eff/100))[-7])$conf.int)^2

boxplot(fboxi_fb$t1_ht.eff)
plot(fboxi_fb$t0_ss900,fboxi_fb$t1_ht.eff)

fboxi_fb$t2_ss900/fboxi_fb$t1_ss900
# 
# 
# ((mss9fbbt*m_eff_plac)-mss9fbat)/(mss9fbbt*m_eff_plac) ### efficacia del formicobox
# ## ic con il bootstrap?!?
# attach(dataplac)
# hist(ss9_bt,breaks=0:35,ylim=c(0,30),main="Infestazione al giorno 0",xlab="varroe/900 api",ylab="frequenza")
# hist(ss9_at,breaks=0:35,ylim=c(0,30),main="Infestazione al giorno 30",xlab="varroe/900 api",ylab="frequenza")
# #capire come gestire gli zeri iniziali
# detach(dataplac)
# 
# attach(fboxi)
# order(cassa)->ordine
# rbind(ss9_bt[ordine],ss9_at[ordine])->counts
# row.names(counts)<-c("prima","dopo")
# names(counts)<-cassa[ordine]
# barplot(counts, main="Infestazione delle api adulte",
#         xlab="Famiglie", col=c("white","yellow"),ylab="Varroe/900 api",
#         legend = rownames(counts), beside=TRUE)
# barplot(counts[1,], main="Infestazione delle api adulte prima del trattamento",
#         xlab="Famiglie", col=c(rep("black",dim(datafb)[1]),rep("red",dim(dataplac)[1])),ylab="Varroe/900 api",ylim=c(0,35))
# barplot(counts[2,], main="Infestazione delle api adulte un mese dopo il trattamento",
#         xlab="Famiglie", col=c(rep("black",dim(datafb)[1]),rep("red",dim(dataplac)[1])),ylab="Varroe/900 api")
# 
# 
# 
# rbind(-(ss9_bt[ordine]-ss9_at[ordine]))->counts
# barplot(counts, main="Car Distribution",
#         xlab="Number of Gears") 
# 
# barplot(fboxi$eff_log[ordine], main="",
#         xlab="")
# fboxi$aumentoperc<-(ss9_at-ss9_bt)*100/(ss9_bt+0.5)
# as.numeric(as.factor(tratt[ordine]))->coloritratt
# barplot(fboxi$aumentoperc[ordine], ylab="Variazione dell'infestazione (%)",
#         xlab="famiglie",col=coloritratt,main="Variazione dell'infestazione in un mese")
# mean(fboxi$aumentoperc[tratt=="fb"])
# mean(fboxi$aumentoperc[tratt=="plac"])
# (mean(fboxi$aumentoperc[tratt=="plac"])-(mean(fboxi$aumentoperc[tratt=="fb"])))/mean(fboxi$aumentoperc[tratt=="plac"])
# boxplot(fboxi$aumentoperc~tratt)
# boxplot(fboxi$eff_log~tratt)
# boxplot(fboxi$ss9_at~tratt)
# as.character(ora.insacco%/%1)->ora
# as.character(round(ora.insacco%%1,digits=2)*100)->minu
# plot(strptime(paste(ora,minu,sep=":"),format="%H:%M")[tratt=="fb"],fboxi$aumentoperc[tratt=="fb"],type="h",lwd=2,ylab="Variazione dell'infestazione (%)",xlab="ora di insacco")
# plot(strptime(paste(ora,minu,sep=":"),format="%H:%M")[tratt=="fb"],fboxi$ss9_at[tratt=="fb"],type="h",lwd=2,ylab="Varroe/900 api dopo il trattamento",xlab="ora di insacco")
# rm(ora,minu)
# detach(fboxi)
# 
# # Blocco di covata --------------------------------------------------------
# 
# 
# formicobox_blocco_conteggi <- read.csv("~/Documenti/importanza_1_sync/unaapi/Sperimentazioni/Formicobox/formicobox_blocco_conteggi.csv", sep=";", dec=",")
# merge(formicobox_blocco_conteggi,fbox,by.x="alveare",by.y="cassa")->fbox_blocco
# as.Date(c("2012/7/15","2012/07/18","2012/7/22","2012/07/26","2012/7/30","2012/08/3","2012/08/7","2012/08/8","2012/08/10","2012/08/13"))->Dates.cad
# fbox_blocco[fbox_blocco$tratt.x=="ctr",c(3:9,11:13)]->cadute_ctr
# fbox_blocco[fbox_blocco$tratt.x=="fbox",c(3:9,11:13)]->cadute_fbox
# plot(Dates.cad,apply(cadute_fbox,2,mean),type="l")
# plot(Dates.cad,apply(cadute_ctr,2,mean),type="l")
# mean(fbox_blocco$Z2[fbox_blocco$tratt.x=="ctr"])
# mean(fbox_blocco$Z2[fbox_blocco$tratt.x=="fbox"])
# boxplot(fbox_blocco$Z2~fbox_blocco$tratt.x)
# (fbox_blocco$Z3[fbox_blocco$tratt.x=="fbox"])/(fbox_blocco$Z2[fbox_blocco$tratt.x=="fbox"])
# sum(cadute_fbox)
# sum(cadute_ctr)
# sum(apply(cadute_fbox,2,mean))
# sum(apply(cadute_ctr,2,mean))
# #fare abbott modificato con cadute e con z3
# (fbox_blocco$Z3[fbox_blocco$tratt.x=="fbox"])/(fbox_blocco$Z2[fbox_blocco$tratt.x=="fbox"])
# 
