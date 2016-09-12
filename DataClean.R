library(scales)
library(shiny)
library(DT)
library(dplyr)
library(stringr)

CODE <- read.csv("//knx1fs01/ED Reporting/Lowhorn Big Data/Golden Rule Data/CODE.csv")
#########DILLON HIGHTOWER ADD############

q <- read.csv("//knx1fs01/ED Reporting/Lowhorn Big Data/Golden Rule Data/q.csv", stringsAsFactors=FALSE)
r <- read.csv("//knx1fs01/ED Reporting/Lowhorn Big Data/Golden Rule Data/r.csv", stringsAsFactors=FALSE)

#########################################
ARMASTER <- read.csv("//KNX1FS01/ED Reporting/Lowhorn Big Data/Golden Rule Data/ARMASTER.csv",header=TRUE,stringsAsFactors = FALSE)
COND <- read.csv("//KNX1FS01/ED Reporting/Lowhorn Big Data/Golden Rule Data/COND.csv",header=TRUE,stringsAsFactors = FALSE)
GRRHBS <- read.csv("//KNX1FS01/ED Reporting/Lowhorn Big Data/Golden Rule Data/GRRHBS.csv",header=TRUE,stringsAsFactors = FALSE)
GRRHBS<-plyr::rename(GRRHBS,c("Setup.Month"="ActMonth","AR"="desk","Setup.Date"="ActDate"))
GRRHBS$ActDate<-as.Date(GRRHBS$ActDate,format="%m/%d/%Y")
COND<-arrange(COND,desc(ED_COND_OPEN_DT))
GRRHBS<-arrange(GRRHBS,desc(ActDate))
RHBS <- GRRHBS
#########DILLON HIGHTOWER ADD############
df <- rbind(q,r)
contacts <- c("CM","A3P","AT")

df <- df[df$CODE_2 %in% contacts,]
#########################################
#removed duplicate contacts ~ every 60 minutes
df$T <- gsub("^.*? ","",df$TIME)
df$DTE <- gsub(" 0:00:00","",df$ACT_DATE)
time <- as.data.frame(str_split_fixed(df$T,":",n=3))
time <- dplyr::rename(time, Hour = V1, Minute = V2, Second = V3)

time$Hour <- as.numeric(as.character(time$Hour))
time$Minute <- as.numeric(as.character(time$Minute))
time$Second <- as.numeric(as.character(time$Second))

df <- cbind(df,time)
rm(time)

df$DTE <- as.Date(df$DTE,"%m/%d/%Y")

df <- df %>%
  arrange(Second) %>% 
  arrange(Minute) %>%
  arrange(Hour) %>%
  arrange(DTE) %>%
  arrange(TFILE) %>%
  mutate(Minute = Minute + Second/60) %>%
  mutate(Hour.Min = Hour + Minute/60) %>%
  group_by(TFILE,DTE) %>%
  mutate(Diff = Hour.Min - lag(Hour.Min,default=Hour.Min[1])) %>%
  mutate(Diff_Minutes = Diff *60)

df <- df[,-which(names(df)%in%c("T","DTE","Hour","Minute","Second","Diff"))]

df <- df %>%
  mutate(sub = ifelse(Diff_Minutes >= -60 & Diff_Minutes <= 60 & Diff_Minutes != 0,"sub","no"))

list <- c(df$sub[-1],"no")

df<-cbind(df,list)

df <- df %>%
  mutate(list = ifelse(CODE_3 == "PRM",1,list))


df <- df[df$list != 2,]

df <- df[,-which(names(df)%in%c("Hour.Min","Diff_Minutes","sub","list"))]

########################################################################################################
pf <- df[df$CODE_3 == "PF",]



pf <- pf[pf$CODE_2 %in% contacts,]

emp <-c("24985","105986","103297","21747","47254","45885","131978","190830","119359",
        "167905","CN2009","CN4450","CN2371","CN3679","CN3653","CN3857","CN3907","CN4199",
        "76924","115866","175947","163618","158836","22094","121681","93150","45845",
        "168041","148992","170717","167902","173045","95513","174646","179647","47252",
        "176843","170715","93288","43738","202198","201274","162776","199709","164007",
        "189722","166008","162723","173092","143377","172790","161094","181043","149549",
        "201071","180225","194484","195197","180239","180229","161696","15857","101241",
        "195504","202195","162827","162718","169500","185554","192285","185291","198018",
        "25607","157831","193339","189377","193344","164774","180055","113172","185292",
        "29962","99661","0","1") 
pf$EMPNUM <- as.character(pf$EMPNUM)
pf <- pf[!pf$EMPNUM %in% emp,]
pf <- pf[complete.cases(pf),]
pf$ACT_DATE <- as.Date(pf$ACT_DATE,format="%m/%d/%Y")
pf <- pf %>%
  arrange(desc(ACT_DATE))
Names <- ARMASTER

COND$ED_COND_OPEN_DT <- as.Date(COND$ED_COND_OPEN_DT,format="%m/%d/%Y")
COND$ED_COND_VER_DT <- as.Date(COND$ED_COND_VER_DT,format="%m/%d/%Y")
COND <- rename(COND,ACT_DATE = ED_COND_VER_DT, EMPNUM = ED_COND_OPEN_EMP, TFILE = CM_FILENO)
COND <- COND %>%
  arrange(desc(ACT_DATE))
COND <- COND[!COND$EMPNUM %in% emp,]
rm(emp)
COND <- mutate(COND,Rehab.Condition = "Yes")
COND <- COND[,names(COND) != "EMPNUM"]
pf <- left_join(pf,COND,by=c("TFILE"))

pf$ED_PROOF_INCOME_DT <- as.Date(pf$ED_PROOF_INCOME_DT,"%m/%d/%Y")

pf$ED_ALL_RHB_RECV_DT <- as.Date(pf$ED_ALL_RHB_RECV_DT,"%m/%d/%Y")

pf <- pf[,names(pf) != "ACT_DATE.y"]

ARMASTER <- select(ARMASTER,EMPNUM,A.R,manager,DEPT,off)
pf <- left_join(pf,ARMASTER,by="EMPNUM") %>%
  rename(Collector = A.R, Manager = manager, Department = DEPT, Office = off,Date = ACT_DATE.x)


pf$Office <- as.factor(pf$Office)
pf <- pf %>%
  arrange(Date)
pf$Month <- format(pf$Date,format = "%B %Y")
pf$Office <- plyr::revalue(pf$Office,c("C"="Columbus","K" = "Knoxville", "W" = "Westlake", "B" = "Columbus 2", "A" = "Atlanta"))
pf$Department <- as.factor(pf$Department)

dp <- c("CMG","CLR")
pf <- pf[!pf$Department %in% dp,]

pf <- pf %>%
  group_by(Date,EMPNUM) %>%
  distinct(TFILE) %>%
  ungroup()
pf$Month <- as.factor(pf$Month)


##################################ADD LEVELS EACH MONTH#######################################
##############################################################################################
pf$Month <- factor(pf$Month,levels=c("January 2015","February 2015","March 2015","April 2015",
                                     "May 2015", "June 2015", "July 2015", "August 2015",
                                     "September 2015", "October 2015", "November 2015", "December 2015",
                                     "January 2016","February 2016","March 2016","April 2016","May 2016",
                                     "June 2016","July 2016","August 2016","September 2016","October 2016",
                                     "November 2016", "December 2016"))

##############################################################################################
##############################################################################################
#pf$CODE_1 <- plyr::revalue(pf$CODE_1,c("IC"="Incoming Call","1H"="Home Phone 1","2H"="Home Phone 2", "1P"="POE Phone 1","2P"="POE Phone 2"))
#pf$CODE_2 <- plyr::revalue(pf$CODE_2,c("CM"="Borrower","A3P"="Authorized 3rd Party","AT"="Attorney"))
pf$Manager <- as.factor(pf$Manager)
pf$Collector <- as.factor(pf$Collector)




completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
pf <- completeFun(pf,"Manager")

pf <- pf %>%
  mutate(Days_for_Income_Docs = ifelse(ED_PROOF_INCOME_DT >= Date,
                                       ED_PROOF_INCOME_DT-Date,
                                       0),
         Days_for_All_RHB_Docs = ifelse(ED_ALL_RHB_RECV_DT >= Date,
                                        ED_ALL_RHB_RECV_DT-Date,
                                        0))
pf$Days_for_Income_Docs <- as.numeric(pf$Days_for_Income_Docs)
pf$Days_for_All_RHB_Docs <- as.numeric(pf$Days_for_All_RHB_Docs)
pf <- pf %>%
  mutate(Days_for_All_RHB_Docs = ifelse(is.na(Days_for_All_RHB_Docs),0,Days_for_All_RHB_Docs),
         Days_for_Income_Docs = ifelse(is.na(Days_for_Income_Docs),0,Days_for_Income_Docs))

##################################################################################################
COND <- read.csv("//KNX1FS01/ED Reporting/Lowhorn Big Data/Golden Rule Data/COND.csv",header=TRUE,stringsAsFactors = FALSE)
ARMASTER <- read.csv("//KNX1FS01/ED Reporting/Lowhorn Big Data/Golden Rule Data/ARMASTER.csv",header=TRUE,stringsAsFactors = FALSE)



contacts <- c("CM","A3P","AT")

df <- df[df$CODE_2 %in% contacts,]

emp <-c("24985","105986","103297","21747","47254","45885","131978","190830","119359",
        "167905","CN2009","CN4450","CN2371","CN3679","CN3653","CN3857","CN3907","CN4199",
        "76924","115866","175947","163618","158836","22094","121681","93150","45845",
        "168041","148992","170717","167902","173045","95513","174646","179647","47252",
        "176843","170715","93288","43738","202198","201274","162776","199709","164007",
        "189722","166008","162723","173092","143377","172790","161094","181043","149549",
        "201071","180225","194484","195197","180239","180229","161696","15857","101241",
        "195504","202195","162827","162718","169500","185554","192285","185291","198018",
        "25607","157831","193339","189377","193344","164774","180055","113172","185292",
        "29962","99661","0","1") 
df$EMPNUM <- as.character(df$EMPNUM)
df <- df[!df$EMPNUM %in% emp,]
df <- df[complete.cases(df),]
df$ACT_DATE <- as.Date(df$ACT_DATE,format="%m/%d/%Y")
df <- df %>%
  arrange(desc(ACT_DATE))
Names <- ARMASTER

COND$ED_COND_OPEN_DT <- as.Date(COND$ED_COND_OPEN_DT,format="%m/%d/%Y")
COND$ED_COND_VER_DT <- as.Date(COND$ED_COND_VER_DT,format="%m/%d/%Y")
COND <- rename(COND,ACT_DATE = ED_COND_VER_DT, EMPNUM = ED_COND_OPEN_EMP, TFILE = CM_FILENO)
COND <- COND %>%
  arrange(desc(ACT_DATE))
COND <- COND[!COND$EMPNUM %in% emp,]
rm(emp)
COND <- mutate(COND,Rehab.Condition = "Yes")
df <- left_join(df,COND,by=c("TFILE","ACT_DATE","EMPNUM"))
rm(COND)
df <- mutate(df,Rehab.Condition = ifelse(is.na(Rehab.Condition), "No", Rehab.Condition))
rh <- GRRHBS
GRRHBS <- mutate(GRRHBS, Tracker = "Tracker") %>%
  rename(ACT_DATE = ActDate)
GRRHBS <- GRRHBS %>%
  select(TFILE,ACT_DATE,Tracker)
GRRHBS$ACT_DATE <- as.Date(GRRHBS$ACT_DATE,format="%Y=%m-%d")
GRRHBS <- GRRHBS %>%
  arrange(desc(ACT_DATE))

df <- left_join(df,GRRHBS,by=c("TFILE","ACT_DATE"))
rm(GRRHBS)
df <- mutate(df,Tracker = ifelse(is.na(Tracker),"Non_On_Tracker",Tracker))
df$Tracker <- as.factor(df$Tracker)
df$CODE_1 <- as.factor(df$CODE_1)
df$CODE_2 <- as.factor(df$CODE_2)
df$CODE_3 <- as.factor(df$CODE_3)

ARMASTER <- select(ARMASTER,EMPNUM,A.R,manager,DEPT,off)
df <- left_join(df,ARMASTER,by="EMPNUM") %>%
  rename(Collector = A.R, Manager = manager, Department = DEPT, Office = off,Date = ACT_DATE)
rm(ARMASTER)
#df <- df[complete.cases(df),]
df$Office <- as.factor(df$Office)
df <- df %>%
  arrange(Date)
df$Month <- format(df$Date,format = "%B %Y")
df$Office <- plyr::revalue(df$Office,c("C"="Columbus","K" = "Knoxville", "W" = "Westlake", "B" = "Columbus 2", "A" = "Atlanta"))
df$Department <- as.factor(df$Department)

dp <- c("PRO","CMG","CLR")
df <- df[!df$Department %in% dp,]
rh <- rh %>%
  select(desk,TFILE, ActDate) %>%
  rename(ARNUM = desk, Date = ActDate) %>%
  mutate(Credit = "Yes")
rh$ARNUM <- as.character(rh$ARNUM)
df <- left_join(df,rh,by=c("ARNUM","Date","TFILE"))
rm(rh)
df <- mutate(df,Credit = ifelse(is.na(Credit),"No",Credit))
df$Credit <- as.factor(df$Credit)
df <- df %>%
  group_by(Date,EMPNUM) %>%
  distinct(TFILE) %>%
  ungroup()
df$Month <- as.factor(df$Month)
##################################ADD LEVELS EACH MONTH#######################################
##############################################################################################
df$Month <- factor(df$Month,levels=c("January 2015","February 2015","March 2015","April 2015",
                                     "May 2015", "June 2015", "July 2015", "August 2015",
                                     "September 2015", "October 2015", "November 2015", "December 2015",
                                     "January 2016","February 2016","March 2016","April 2016","May 2016",
                                     "June 2016","July 2016","August 2016","September 2016","October 2016",
                                     "November 2016", "December 2016"))

##############################################################################################
##############################################################################################
#df$CODE_1 <- plyr::revalue(df$CODE_1,c("IC"="Incoming Call","1H"="Home Phone 1","2H"="Home Phone 2", "1P"="POE Phone 1","2P"="POE Phone 2"))
#df$CODE_2 <- plyr::revalue(df$CODE_2,c("CM"="Borrower","A3P"="Authorized 3rd Party","AT"="Attorney"))
df$Manager <- as.factor(df$Manager)
df$Collector <- as.factor(df$Collector)




completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
df <- completeFun(df,"Manager")

df <- df[df$CODE_3 != "PF",]

RHBS <- select(RHBS, desk, ActDate)
RHBS <- rename(RHBS, Date = ActDate,ARNUM=desk)
RHBS$Month = format(RHBS$Date, format="%B %Y")
Names <- select(Names,A.R,manager,desk,DEPT,off)
Names <- rename(Names,ARNUM=desk,Collector=A.R,Manager=manager,Office = off,Department = DEPT)
Names$Office <- plyr::revalue(Names$Office,c("C"="Columbus","K" = "Knoxville", "W" = "Westlake", "B" = "Columbus 2", "A" = "Atlanta"))
RHBS <- left_join(RHBS,Names,by="ARNUM")
RHBS <- select(RHBS,Date,Month,Collector,Manager,Office,Department)

ODay <- RHBS %>%
  group_by(Office,Date) %>%
  summarize(Rehabs_On_Tracker = n())
OMonth <- RHBS %>%
  group_by(Office,Month) %>%
  summarize(Rehabs_On_Tracker = n())

DeptDay <- RHBS %>%
  group_by(Department,Office,Date) %>%
  summarize(Rehabs_On_Tracker = n())
DeptMonth <- RHBS %>%
  group_by(Department,Office,Month) %>%
  summarize(Rehabs_On_Tracker = n())

CMonth <- RHBS %>%
  group_by(Collector,Month) %>%
  summarize(Rehabs_On_Tracker = n())
CMonth$Month <- as.factor(CMonth$Month)
CMonth$Collector <- as.factor(CMonth$Collector)

CDay <- RHBS %>%
  group_by(Collector,Date) %>%
  summarize(Rehabs_On_Tracker = n())
CDay$Collector <- as.factor(CDay$Collector)

MMonth <- RHBS %>%
  group_by(Manager,Month) %>%
  summarize(Rehabs_On_Tracker = n())

MMonth$Month <- as.factor(MMonth$Month)
MMonth$Manager <- as.factor(MMonth$Manager)

MDay <- RHBS %>%
  group_by(Manager,Date) %>%
  summarize(Rehabs_On_Tracker = n())
MDay$Manager <- as.factor(MDay$Manager)

CODE <- plyr::rename(CODE,c("MENU.OPTION.1"="Menu Option","Description.2"="Description (cont)", "Call.Code"="Call Code"))
rm(calls)
rm(contacts)
rm(dp)
rm(Names)

setwd("//Knx3fs01/ED_BA_GROUP/Lowhorn/Golden Rule 3/Application")
runApp(host="0.0.0.0",port=5060)
