#
detach()
rm(list=ls())
fbox <- read.csv2("./data/formicobox_mombasiglio.csv", sep=";")
attach(fbox)
fbox$tratt<-c(rep("fb",48),rep("plac",48))   #trattamenti: formicobox, placebo
fbox$ss9_bt=ss300_bt_1+ss300_bt_2+ss300_bt_3 #totale zucchero prima trattamento
fbox$ss9_at=ss300_at_1+ss300_at_2+ss300_at_3 #totale zucchero dopo trattamento
fbox$ss9_at2=ss300_at2_1+ss300_at2_2+ss300_at2_3 #totale zucchero dopo secondo trattamento
detach(fbox)
attach(fbox)
!is.na(ss9_at-ss9_bt)->include #escluse casse con valori NA
ss9_bt<11 & include ->include #escluse casse con altissima infestazione
detach(fbox)
fboxi<-fbox[include,] #dataset delle casse in analisi
rm(include)
fboxi<-fboxi[order(fboxi$ss9_bt),]
attach(fboxi)
n_plac<-length(ss9_bt[tratt=="plac"]) #numero di casse placebo
n_fb<-length(ss9_bt[tratt=="fb"]) #numero casse formicobox
fboxi$eff_log<-log(ss9_at+0.5)-log(ss9_bt+0.5)
fboxi$aumento<-(ss9_at+1)/(ss9_bt+1)
detach(fboxi)
attach(fboxi)
boxplot(eff_log~tratt)
boxplot(aumento~tratt)
detach(fboxi)
dataplac<-fboxi[fboxi$tratt=="plac",]
datafb<-fboxi[fboxi$tratt=="fb",]
attach(dataplac)
plot(ss9_bt,ss9_at)
abline(ss9_bt,ss9_at)
#plot(glm(ss9_at~sqrt(ss9_bt),family=poisson))
detach(dataplac)
attach(datafb)
points(ss9_bt,ss9_at,pch=3)
abline(ss9_bt,ss9_at)
mean(eff_log)
exp(mean(eff_log))#non sicuro della trasformazione
mean(aumento)
mss9fbbt<-mean(ss9_bt)
mss9fbat<-mean(ss9_at)
mss9fbat/mss9fbbt->m_eff_fb
detach(datafb)
attach(dataplac)
mean(eff_log)
exp(mean(eff_log))#non sicuro della trasformazione
mean(aumento)
mss9placbt<-mean(ss9_bt)
mss9placat<-mean(ss9_at)
(mss9placat)/mss9placbt->m_eff_plac
detach(dataplac)
((mss9fbbt*m_eff_plac)-mss9fbat)/(mss9fbbt*m_eff_plac) ### efficacia del formicobox
## ic con il bootstrap?!?
attach(dataplac)
hist(ss9_bt,breaks=0:35,ylim=c(0,30),main="Infestazione al giorno 0",xlab="varroe/900 api",ylab="frequenza")
hist(ss9_at,breaks=0:35,ylim=c(0,30),main="Infestazione al giorno 30",xlab="varroe/900 api",ylab="frequenza")
#capire come gestire gli zeri iniziali
detach(dataplac)

attach(fboxi)
order(cassa)->ordine
rbind(ss9_bt[ordine],ss9_at[ordine])->counts
row.names(counts)<-c("prima","dopo")
names(counts)<-cassa[ordine]
barplot(counts, main="Infestazione delle api adulte",
        xlab="Famiglie", col=c("white","yellow"),ylab="Varroe/900 api",
        legend = rownames(counts), beside=TRUE)
barplot(counts[1,], main="Infestazione delle api adulte prima del trattamento",
        xlab="Famiglie", col=c(rep("black",dim(datafb)[1]),rep("red",dim(dataplac)[1])),ylab="Varroe/900 api",ylim=c(0,35))
barplot(counts[2,], main="Infestazione delle api adulte un mese dopo il trattamento",
        xlab="Famiglie", col=c(rep("black",dim(datafb)[1]),rep("red",dim(dataplac)[1])),ylab="Varroe/900 api")



rbind(-(ss9_bt[ordine]-ss9_at[ordine]))->counts
barplot(counts, main="Car Distribution",
        xlab="Number of Gears") 

barplot(fboxi$eff_log[ordine], main="",
        xlab="")
fboxi$aumentoperc<-(ss9_at-ss9_bt)*100/(ss9_bt+0.5)
as.numeric(as.factor(tratt[ordine]))->coloritratt
barplot(fboxi$aumentoperc[ordine], ylab="Variazione dell'infestazione (%)",
        xlab="famiglie",col=coloritratt,main="Variazione dell'infestazione in un mese")
mean(fboxi$aumentoperc[tratt=="fb"])
mean(fboxi$aumentoperc[tratt=="plac"])
(mean(fboxi$aumentoperc[tratt=="plac"])-(mean(fboxi$aumentoperc[tratt=="fb"])))/mean(fboxi$aumentoperc[tratt=="plac"])
boxplot(fboxi$aumentoperc~tratt)
boxplot(fboxi$eff_log~tratt)
boxplot(fboxi$ss9_at~tratt)
as.character(ora.insacco%/%1)->ora
as.character(round(ora.insacco%%1,digits=2)*100)->minu
plot(strptime(paste(ora,minu,sep=":"),format="%H:%M")[tratt=="fb"],fboxi$aumentoperc[tratt=="fb"],type="h",lwd=2,ylab="Variazione dell'infestazione (%)",xlab="ora di insacco")
plot(strptime(paste(ora,minu,sep=":"),format="%H:%M")[tratt=="fb"],fboxi$ss9_at[tratt=="fb"],type="h",lwd=2,ylab="Varroe/900 api dopo il trattamento",xlab="ora di insacco")
rm(ora,minu)
detach(fboxi)

# Blocco di covata --------------------------------------------------------


formicobox_blocco_conteggi <- read.csv("~/Documenti/importanza_1_sync/unaapi/Sperimentazioni/Formicobox/formicobox_blocco_conteggi.csv", sep=";", dec=",")
merge(formicobox_blocco_conteggi,fbox,by.x="alveare",by.y="cassa")->fbox_blocco
as.Date(c("2012/7/15","2012/07/18","2012/7/22","2012/07/26","2012/7/30","2012/08/3","2012/08/7","2012/08/8","2012/08/10","2012/08/13"))->Dates.cad
fbox_blocco[fbox_blocco$tratt.x=="ctr",c(3:9,11:13)]->cadute_ctr
fbox_blocco[fbox_blocco$tratt.x=="fbox",c(3:9,11:13)]->cadute_fbox
plot(Dates.cad,apply(cadute_fbox,2,mean),type="l")
plot(Dates.cad,apply(cadute_ctr,2,mean),type="l")
mean(fbox_blocco$Z2[fbox_blocco$tratt.x=="ctr"])
mean(fbox_blocco$Z2[fbox_blocco$tratt.x=="fbox"])
boxplot(fbox_blocco$Z2~fbox_blocco$tratt.x)
(fbox_blocco$Z3[fbox_blocco$tratt.x=="fbox"])/(fbox_blocco$Z2[fbox_blocco$tratt.x=="fbox"])
sum(cadute_fbox)
sum(cadute_ctr)
sum(apply(cadute_fbox,2,mean))
sum(apply(cadute_ctr,2,mean))
#fare abbott modificato con cadute e con z3
(fbox_blocco$Z3[fbox_blocco$tratt.x=="fbox"])/(fbox_blocco$Z2[fbox_blocco$tratt.x=="fbox"])

