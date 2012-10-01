# data merge and formulation
fbox <- read.csv2("./data/formicobox_mombasiglio.csv", sep=";")
sug_sett <- read.csv("~/Documenti/importanza_1_sync/unaapi/Sperimentazioni/Formicobox/data_oo/zuccheri_settembre.csv")
names(fbox)<-c("id_hive","t0_ss300_1","t0_ss300_2","t0_ss300_3","t0_notes","t0_fbox_daytime","t1_ss300_1","t1_ss300_2","t1_ss300_3","t1_notes","t2_ss300_1","t2_ss300_2","t2_ss300_3","t2_notes","exp2")
names(sug_sett)<-c("id_hive","t3_ss300_1","t3_ss300_2","t3_ss300_3","t3_notes","t3_id_op_ss")
fbox<-merge(fbox,sug_sett,by="id_hive",all.x=T)
write.csv(fbox, "./data/fbox_def.csv")
