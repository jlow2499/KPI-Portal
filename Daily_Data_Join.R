library(dplyr)
library(stringr)

t <- read.csv("//knx1fs01/ED Reporting/Lowhorn Big Data/Golden Rule Data/t.csv", stringsAsFactors=FALSE)
ACT_HIST <- read.delim("//knx3it/EDOPSMGMT/Reports/Stamm Reports/Act_Hist/ACT_HIST.txt", stringsAsFactors=FALSE)

ACT_HIST <- rename(ACT_HIST,ARNUM = Collector, TFILE=Debtor,CODE_1=ActWhat,CODE_2=ActWho,CODE_3=ActResult,EMPNUM=EmplNo)


time <- as.data.frame(str_split_fixed(ACT_HIST$ActTMS," ",n=2))
time <- rename(time, ACT_DATE = V1,TIME = V2)

ACT_HIST <- ACT_HIST[,!names(ACT_HIST)%in%"ActTMS"]

ACT_HIST <- cbind(ACT_HIST,time)
rm(time)

ACT_HIST <- ACT_HIST[c("ARNUM","TFILE","ACT_DATE","TIME","CODE_1","CODE_2","CODE_3","EMPNUM")]

ACT_HIST$ACT_DATE <- as.character(ACT_HIST$ACT_DATE)
ACT_HIST$ACT_DATE <- as.Date(ACT_HIST$ACT_DATE,"%Y-%m-%d")
ACT_HIST$ACT_DATE <- format(ACT_HIST$ACT_DATE,"%m/%d/%Y")

t <- rbind(t,ACT_HIST)

write.csv(t,"//knx1fs01/ED Reporting/Lowhorn Big Data/Golden Rule Data/low_t.csv",row.names=FALSE)
