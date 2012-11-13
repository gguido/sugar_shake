# Surviving -------------------------------------------------------------

Survival<-function(X,Y){
  #X=count before treatment
  #Y=count after treatment
  Y/X
  }

# Abbott's percent control -----------------------------------------------

abbott.pc<-function(St,Sc){
  #St=surviving proportion in treatment group
  #Sc=surviving proportion in control group
  (mean(Sc)-St)/mean(Sc)
}


# Classical Fallen Mites --------------------------------------------------

fm.efficacy<-function(txtf,tctf){
  #txtf=treated hives - fallen mites during x treatment
  #tctf=treated hives - control treatment fallen mites
  txtf/(txtf+tctf)
  }

# Fallen mites with Abbot (1925) Correction -------------------------------

fm.efficacy.abbott<-function(txtf,tctf,cxtf,cctf){
  #txtf=treated hives - fallen mites during x treatment
  #tctf=treated hives - control treatment fallen mites
  #cxtf=control hives - fallen mites during x treatment in treated hives
  #cctf=control hives - control treatment fallen mites
  ##((mean(100*cctf/(cxtf+cctf)))-(100 * tctf / ( txtf + tctf)))/(100 * tctf / ( txtf + tctf))
  ##((mean(100*cctf/(cxtf+cctf)))->Cs #Cs
  ##(100 * tctf / ( txtf + tctf)) -> Ts
  ##(Cs – Ts) / Cs
  ## mortality=1-survival => ((1-Cm)-(1-Tm))/(1-Cm) => (Tm-Cm)/(1-Cm)
  ((txtf/(txtf+tctf)) - mean(cxtf/(cxtf+cctf))) / (1- mean(cxtf/(cxtf+cctf)))
  }

# Taylor (1987) -----------------------------------------------------------


# Henderson-Tilton (1955) -------------------------------------------------
# percent mortality

ht.efficacy<-function(tb,ta,cb,ca){
  #tb=treated before treatment
  #ta=treated after treatment
  #cb=control before treatment (matter of time, control is not treated)
  #ca=control after treatment (matter of time, control is not treated)
  1-((ta/tb)/mean(ca/cb))
  }

# Henderson-Tilton (1955) - angular coefficients --------------------------

ht.coeff<-function(Bt,Bc){
  #Bt=coefficiente angolare della retta di regressione per le trattate (infestazione prima e dopo trattamento)
  #Bc=coefficiente angolare della retta di regressione per le controllo (infestazione al tempo 1 e 2)
  1-Bt/Bc
  }

# Henderson-Tilton (1955) - quantiles (experimental) ----------------------

ht.efficacy.quantiles<-function(tb,ta,cb,ca,probs){
  if(max(probs)>=1 | min(probs<=0))
  {paste("Error, probs must be strictly comprised between 0 and 1")}
  else{
    probs<-c(0,probs,1)
    ht.effq<-NULL
    for(i in 1:(length(probs)-1)){
      rank(tb,ties.method="first")/length(tb)->rtb
      rank(cb,ties.method="first")/length(cb)->rcb
      tb[rtb>probs[i] & rtb<=probs[i+1]]->tbq
      cb[rcb>probs[i] & rcb<=probs[i+1]]->cbq
      ta[rtb>probs[i] & rtb<=probs[i+1]]->taq
      ca[rcb>probs[i] & rcb<=probs[i+1]]->caq
      100*(1-((taq/tbq)/mean(caq/cbq)))->ht.effqi
      ht.effqi->ht.effq[[i]]
    }
    ht.effq
  }
}
