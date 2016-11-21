library(scales)
library(shiny)
library(DT)
library(dplyr)
library(stringr)
library(future)
library(dygraphs)
library(xts)


x <- c()
if(weekdays(Sys.Date())=="Monday"){
  x <- 3
}else{
  x <- 1
}


CODE <- read.csv("//knx1fs01/ED Reporting/Lowhorn Big Data/Golden Rule Data/CODE.csv")
#########DILLON HIGHTOWER ADD############

#q <- read.csv("//knx1fs01/ED Reporting/Lowhorn Big Data/Golden Rule Data/q.csv", stringsAsFactors=FALSE)
#r <- read.csv("//knx1fs01/ED Reporting/Lowhorn Big Data/Golden Rule Data/r.csv", stringsAsFactors=FALSE)
s <- future({ read.csv("//knx1fs01/ED Reporting/Lowhorn Big Data/Golden Rule Data/s.csv", stringsAsFactors=FALSE) }) %plan% multiprocess
t <- future ({ read.csv("//knx1fs01/ED Reporting/Lowhorn Big Data/Golden Rule Data/t.csv", stringsAsFactors=FALSE) }) %plan% multiprocess

#########################################
ARMASTER <- read.csv("//KNX1FS01/ED Reporting/Lowhorn Big Data/Golden Rule Data/ARMASTER.csv",header=TRUE,stringsAsFactors = FALSE)
COND <- read.csv("//KNX1FS01/ED Reporting/Lowhorn Big Data/Golden Rule Data/COND.csv",header=TRUE,stringsAsFactors = FALSE)
GRRHBS <- read.csv("//KNX1FS01/ED Reporting/Lowhorn Big Data/Golden Rule Data/GRRHBS.csv",header=TRUE,stringsAsFactors = FALSE)
GRRHBS<-plyr::rename(GRRHBS,c("Setup.Month"="ActMonth","AR"="desk","Setup.Date"="ActDate"))
GRRHBS$ActDate<-as.Date(GRRHBS$ActDate,format="%m/%d/%Y")
COND<-arrange(COND,desc(ED_COND_OPEN_DT))
GRRHBS<-arrange(GRRHBS,desc(ActDate))
RHBS <- GRRHBS
#########ADD############
df <- rbind(value(s),value(t))
DF <- df
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
#pf <- df[df$CODE_3 == "PF",]

pf <- df[df$CODE_2 %in% contacts,]

emp <-c("0","1") 
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
#COND <- COND[!COND$EMPNUM %in% emp,]

COND <- mutate(COND,Rehab.Condition = "Yes")
COND <- COND[,names(COND) != "EMPNUM"]
pf <- left_join(pf,COND,by=c("TFILE"))

pf$ED_PROOF_INCOME_DT <- as.Date(pf$ED_PROOF_INCOME_DT,"%m/%d/%Y")

pf$ED_ALL_RHB_RECV_DT <- as.Date(pf$ED_ALL_RHB_RECV_DT,"%m/%d/%Y")

pf <- pf[,names(pf) != "ACT_DATE.y"]

ARMASTER <- select(ARMASTER,EMPNUM,A.R,manager,DEPT,off,SUB)
pf <- left_join(pf,ARMASTER,by="EMPNUM") %>%
  rename(Collector = A.R, Manager = manager, Department = DEPT, Office = off,Date = ACT_DATE.x)
pf <- pf[pf$Department %in% "PRO",]
pf <- pf[pf$SUB %in% c("AR","VERF","HIR","AWG"),]

pf$Office <- as.factor(pf$Office)
pf <- pf %>%
  arrange(Date)
pf$Month <- format(pf$Date,format = "%B %Y")
pf$Office <- plyr::revalue(pf$Office,c("C"="Columbus","K" = "Knoxville", "W" = "Westlake", "B" = "Columbus 2", "A" = "Atlanta","S"="Schuerger"))
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

emp <-c("0","1") 
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

ARMASTER <- select(ARMASTER,EMPNUM,A.R,manager,DEPT,off,SUB)
df <- left_join(df,ARMASTER,by="EMPNUM") %>%
  rename(Collector = A.R, Manager = manager, Department = DEPT, Office = off,Date = ACT_DATE)
rm(ARMASTER)
#df <- df[complete.cases(df),]
df <- df[df$SUB %in% c("GAR","AR"),]
df$Office <- as.factor(df$Office)
df <- df %>%
  arrange(Date)
df$Month <- format(df$Date,format = "%B %Y")
df$Office <- plyr::revalue(df$Office,c("C"="Columbus","K" = "Knoxville", "W" = "Westlake", "B" = "Columbus 2", "A" = "Atlanta","S"="Schuerger"))
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
Names$Office <- plyr::revalue(Names$Office,c("C"="Columbus","K" = "Knoxville", "W" = "Westlake", "B" = "Columbus 2", "A" = "Atlanta","S"="Schuerger"))
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

rm(contacts)
rm(dp)
rm(Names)


######################################################################################

ARMASTER <- read.csv("//KNX1FS01/ED Reporting/Lowhorn Big Data/Golden Rule Data/ARMASTER.csv",header=TRUE,stringsAsFactors = FALSE)

emp <-c("0","1") 

DF <- DF[!DF$EMPNUM %in% emp,]



review <- c("RV","LR")

DF$ACT_DATE <- as.Date(DF$ACT_DATE,"%m/%d/%Y")

worked <- DF[!duplicated(DF[c("TFILE","ACT_DATE","EMPNUM")]),]

worked <- worked %>%
  group_by(EMPNUM,ACT_DATE) %>%
  summarize(Accounts_Worked = n())

calls <- DF[!DF$CODE_1 %in% review,]

calls <- calls %>%
  group_by(EMPNUM,ACT_DATE) %>%
  summarize(Notated_Calls = n(),
            Inbound_Calls = sum(CODE_1=="IC"),
            Outbound_Calls = Notated_Calls - Inbound_Calls,
            Messages_Left = sum(CODE_3=="MSG")+ sum(CODE_3=="LMF"),
            POE_Attempts = sum(CODE_1=="1P")+sum(CODE_1=="2P"))

activity <- left_join(worked,calls,by=c("EMPNUM","ACT_DATE"))

activity[is.na(activity)] <- 0

ARMASTER <- ARMASTER %>%
  select(EMPNUM,A.R,manager,DEPT,off,SUB)

activity <- left_join(activity, ARMASTER,by="EMPNUM")
activity <- activity[activity$SUB %in% c("HIR","VERF","HIR","AWG","GAR","AR"),]

activity <- plyr::rename(activity,c("ACT_DATE"="Date","A.R"="Collector",
                                    "manager"="Manager","DEPT"="Department",
                                    "off"="Office"))
activity$Month <- format(activity$Date, "%B %Y")

activity$Office <- as.factor(activity$Office)
activity$Department <- as.factor(activity$Department)
activity$Collector <- as.factor(activity$Collector)
activity$Manager <- as.factor(activity$Manager)
activity$Month <- as.factor(activity$Month)

activity$Office <- plyr::revalue(activity$Office,c("A"="Atlanta","AGY"="ALL","B"="Columbus 2","K"="Knoxville",
                                                   "W"="Westlake","C"="Columbus","S"="Schuerger"))

activity <- activity[activity$Manager != "",]

Tracker <- read.csv("//knx1fs01/ED Reporting/Lowhorn Big Data/Golden Rule Data/Tracker.csv", stringsAsFactors=FALSE)
ARMASTER <- read.csv("//KNX1FS01/ED Reporting/Lowhorn Big Data/Golden Rule Data/ARMASTER.csv",header=TRUE,stringsAsFactors = FALSE)
COND <- read.csv("//KNX1FS01/ED Reporting/Lowhorn Big Data/Golden Rule Data/COND.csv",header=TRUE,stringsAsFactors = FALSE)
Funded <- read.delim("//Knx3fs01/ED_BA_GROUP/Lowhorn/Monthly Transaction Detail Report/Transaction Detail Dashboard/Funded.txt", stringsAsFactors=FALSE)
Funded <- select(Funded,Debtor)
Funded <- Funded %>%
  mutate(Funded = "Yes")
Funded <- rename(Funded, TFILE=Debtor)
Funded$TFILE <- as.character(Funded$TFILE)
Funded <- Funded[!duplicated(Funded$TFILE),]

COND <- select(COND, CM_FILENO,ED_PROOF_INCOME_DT,ED_ALL_RHB_RECV_DT,ED_DOED_PAY_CNTR,ED_RHB_TYPE,ED_RHB_APP_RCV_DT)
COND <- rename(COND,TFILE=CM_FILENO)

Tracker$TFILE <- as.character(Tracker$TFILE)
COND$TFILE <- as.character(COND$TFILE)
Tracker <- left_join(Tracker,Funded,by="TFILE")
Tracker <- left_join(Tracker,COND,by="TFILE")
rm(Funded)
rm(COND)

Tracker$Funded[is.na(Tracker$Funded)] <- "No"
Tracker[is.na(Tracker)] <- ""



Tracker <- Tracker %>%
  mutate(Fallout = ifelse(Funded=="Yes","Funded",
                          ifelse(ED_DOED_PAY_CNTR=="","Fallout","In Pipeline")),
         IncomeDocs = ifelse(Funded=="Yes","Yes",ifelse(is.na(ED_PROOF_INCOME_DT)|ED_PROOF_INCOME_DT=="","No","Yes")),
         RALBack = ifelse(Funded=="Yes","Yes",ifelse(is.na(ED_RHB_APP_RCV_DT)|ED_RHB_APP_RCV_DT=="","No","Yes")),
         ReadytoFund = ifelse(is.na(ED_ALL_RHB_RECV_DT)|ED_ALL_RHB_RECV_DT=="","No","Yes")
         
  )

Tracker <- Tracker[,!names(Tracker) %in% c("ED_PROOF_INCOME_DT","ED_ALL_RHB_RECV_DT")]
Tracker$EMPNUM <- as.character(Tracker$EMPNUM)
ARMASTER <- select(ARMASTER,EMPNUM,A.R,manager,DEPT,off)
Tracker <- left_join(Tracker,ARMASTER,by="EMPNUM")
rm(ARMASTER)

Tracker <- rename(Tracker,Collector = A.R , Manager = manager, Department = DEPT, Office = off)
Tracker$Office <- as.factor(Tracker$Office)
Tracker$Office <- plyr::revalue(Tracker$Office, c("K"="Knoxville",'W'="Westlake","C"="Columbus","S"="Schuerger"))
Tracker$Collector[is.na(Tracker$Collector)] <- "NLE Employee"
Tracker$Manager[is.na(Tracker$Manager)] <- "NLE Manager"
Tracker$Department[is.na(Tracker$Department)] <- "COL"
Tracker$Office[is.na(Tracker$Office)] <- "Knoxville"

Tracker$SetupMonth <- as.Date(Tracker$SetupMonth,"%m/%d/%Y")
Tracker$SetupMonth <- format(Tracker$SetupMonth,"%B %Y")

docs <- Tracker %>%
  group_by(SetupMonth,Office, Department, Manager, Collector) %>%
  summarize(Setups = n(),
            Income_Docs = sum(IncomeDocs=="Yes"),
            IC_Percent = Income_Docs/Setups,
            RAL = sum(RALBack=="Yes"),
            RAL_Percent = RAL/Setups,
            Fallout = sum(Fallout=="Fallout"),
            Funded = sum(Funded=="Yes"),
            Funded_Percent = Funded/Setups)


docs$SetupMonth <- factor(docs$SetupMonth,levels=c(
  "January 2016","February 2016","March 2016","April 2016","May 2016",
  "June 2016","July 2016","August 2016","September 2016","October 2016",
  "November 2016", "December 2016"))
docs$Department <- as.factor(docs$Department)
docs$Manager <- as.factor(docs$Manager)
docs$Collector <- as.factor(docs$Collector)

docs$Department <- plyr::revalue(docs$Department,c("IM"="COL","PRO"="COL","CMG"="COL","CLR"="COL"))
docs$Office <- plyr::revalue(docs$Office,c("AGY"="Knoxville","ALL"="Knoxville"))


Tracker$SetupMonth <- factor(Tracker$SetupMonth,levels=c("January 2015","February 2015","March 2015","April 2015",
                                                         "May 2015", "June 2015", "July 2015", "August 2015",
                                                         "September 2015", "October 2015", "November 2015", "December 2015",
                                                         "January 2016","February 2016","March 2016","April 2016","May 2016",
                                                         "June 2016","July 2016","August 2016","September 2016","October 2016",
                                                         "November 2016", "December 2016"))


COND <- read.csv("//KNX1FS01/ED Reporting/Lowhorn Big Data/Golden Rule Data/COND.csv",header=TRUE,stringsAsFactors = FALSE)
ALL <- read.csv("//KNX1FS01/ED Reporting/Lowhorn Big Data/Golden Rule Data/all.csv",header=TRUE)
TYPE <- read.csv("//KNX1FS01/ED Reporting/Lowhorn Big Data/Golden Rule Data/type.csv",header=TRUE)
ALLAWG <- read.csv("//KNX1FS01/ED Reporting/Lowhorn Big Data/Golden Rule Data/allawg.csv",header=TRUE)
TYPEAWG <- read.csv("//KNX1FS01/ED Reporting/Lowhorn Big Data/Golden Rule Data/typeawg.csv",header=TRUE)
TYPE <- TYPE[,-1]
ALL <- ALL[,-1]
TYPEAWG <- TYPEAWG[,-1]
ALLAWG <- ALLAWG[,-1]


COND <- COND[COND$CM_CAN_DATE == "",]
COND <- COND[COND$ED_RHB_TYPE != "",]

COND <- rename(COND, Bucket = ED_DOED_PAY_CNTR, Office = CM_CLIENT)
COND$ED_RHB_TYPE <- as.factor(COND$ED_RHB_TYPE)
COND$Office <- as.factor(COND$Office)
COND$ED_RHB_TYPE <- plyr::revalue(COND$ED_RHB_TYPE,c("1"="15 %","2"="FIS","3"="15 %"))
COND$Office <- plyr::revalue(COND$Office,c("7222"="Knoxville","8035"="Knoxville","A7222"="Atlanta","B7222"="Columbus 2",
                                           "C7222"="Columbus","C8035"="Columbus","D7222"="Defiance","W7222"="Westlake","S7222"="Schuerger"))


COND <- COND[!COND$Office %in% c("Atlanta","Defiance"),]
COND$Office <- factor(COND$Office,levels=c("Knoxville","Columbus","Columbus 2","Westlake","Schuerger"))
COND <- COND %>%
  mutate(Department = ifelse(ED_ACCT_ACT_DATE =="","Collections","AWG"))
COND$Department <- as.factor(COND$Department)
CONDcol <- COND[COND$Department == "Collections",]
CONDawg <- COND[COND$Department == "AWG",]


type <- CONDcol %>%
  group_by(Bucket,ED_RHB_TYPE,Office) %>%
  summarize(Rehabs = n(),
            Income_Docs = sum(ED_PROOF_INCOME_DT != ""),
            Income_Doc_P = Income_Docs/Rehabs,
            RAL = sum(ED_RHB_APP_RCV_DT != ""),
            RAL_P = RAL/Rehabs)


all <- CONDcol %>%
  group_by(Bucket,Office) %>%
  summarize(Rehabs = n(),
            Income_Docs = sum(ED_PROOF_INCOME_DT != ""),
            Income_Doc_P = Income_Docs/Rehabs,
            RAL = sum(ED_RHB_APP_RCV_DT != ""),
            RAL_P = RAL/Rehabs)
CONDawg <- CONDawg[,!names(CONDawg) %in% "Office"]

typeawg <- CONDawg %>%
  group_by(Bucket,ED_RHB_TYPE) %>%
  summarize(Rehabs = n(),
            Income_Docs = sum(ED_PROOF_INCOME_DT != ""),
            Income_Doc_P = Income_Docs/Rehabs,
            RAL = sum(ED_RHB_APP_RCV_DT != ""),
            RAL_P = RAL/Rehabs)
allawg <- CONDawg %>%
  group_by(Bucket) %>%
  summarize(Rehabs = n(),
            Income_Docs = sum(ED_PROOF_INCOME_DT != ""),
            Income_Doc_P = Income_Docs/Rehabs,
            RAL = sum(ED_RHB_APP_RCV_DT != ""),
            RAL_P = RAL/Rehabs)

type$Office <-as.factor(type$Office)
type <- left_join(type,TYPE,by=c("Bucket","Office","ED_RHB_TYPE"))
type <- type %>%
  mutate(Income_Doc_Change = Income_Doc_P.y - Income_Doc_P.x,
         RAL_DOC_Change = RAL_P.y - RAL_P.x)
type <- type[,!names(type) %in% c("Rehabs.y","Income_Docs.y","Income_Doc_P.y","RAL.y","RAL_P.y")]
type <- rename(type, Rehabs = Rehabs.x,Income_Docs = Income_Docs.x,Income_Doc_P=Income_Doc_P.x,RAL = RAL.x,RAL_P=RAL_P.x)
type$Income_Doc_Change <- round(as.numeric(type$Income_Doc_Change),2)
type$RAL_DOC_Change <- round(as.numeric(type$RAL_DOC_Change),2)


all$Office <-as.factor(all$Office)
all <- left_join(all,ALL,by=c("Bucket","Office"))
all <- all %>%
  mutate(Income_Doc_Change = Income_Doc_P.y - Income_Doc_P.x,
         RAL_DOC_Change = RAL_P.y - RAL_P.x)
all <- all[,!names(all) %in% c("Rehabs.y","Income_Docs.y","Income_Doc_P.y","RAL.y","RAL_P.y")]
all <- rename(all, Rehabs = Rehabs.x,Income_Docs = Income_Docs.x,Income_Doc_P=Income_Doc_P.x,RAL = RAL.x,RAL_P=RAL_P.x)
all$Income_Doc_Change <- round(as.numeric(all$Income_Doc_Change),2)
all$RAL_DOC_Change <- round(as.numeric(all$RAL_DOC_Change),2)


TYPEAWG <- TYPEAWG[,!names(TYPEAWG) %in% c("Rehabs.x")]
typeawg <- left_join(typeawg,TYPEAWG,by=c("Bucket","ED_RHB_TYPE"))
typeawg <- typeawg[,!names(typeawg)%in%c("RAL_DOC_Change","Income_Doc_Change")]
typeawg <- typeawg %>%
  mutate(Income_Doc_Change = Income_Doc_P.y - Income_Doc_P.x,
         RAL_DOC_Change = RAL_P.y - RAL_P.x)

typeawg$Income_Doc_Change <- round(as.numeric(typeawg$Income_Doc_Change),2)
typeawg$RAL_DOC_Change <- round(as.numeric(typeawg$RAL_DOC_Change),2)
typeawg <- typeawg[,!names(typeawg) %in% c("Rehabs.y","Income_Docs.y","Income_Doc_P.y","RAL.y","RAL_P.y")]
typeawg <- rename(typeawg,RAL=RAL.x,RAL_P=RAL_P.x,Income_Docs=Income_Docs.x,Income_Doc_P=Income_Doc_P.x)


allawg <- left_join(allawg,ALLAWG,by=c("Bucket"))
allawg <- allawg %>%
  mutate(Income_Doc_Change = Income_Doc_P.y - Income_Doc_P.x,
         RAL_DOC_Change = RAL_P.y - RAL_P.x)

allawg$Income_Doc_Change <- round(as.numeric(allawg$Income_Doc_Change),2)
allawg$RAL_DOC_Change <- round(as.numeric(allawg$RAL_DOC_Change),2)
allawg <- allawg[,!names(allawg) %in% c("Rehabs.y","Income_Docs.y","Income_Doc_P.y","RAL.y","RAL_P.y")]
allawg <- rename(allawg,Rehabs=Rehabs.x,RAL=RAL.x,RAL_P=RAL_P.x,Income_Docs=Income_Docs.x,Income_Doc_P=Income_Doc_P.x)


rm(ALL); rm(TYPE);rm(TYPEAWG);rm(ALLAWG)
rm(COND)

all$Office <- as.factor(all$Office)
type$Office <- as.factor(type$Office)

if(x==1){
}else{
  write.csv(all,"//KNX1FS01/ED Reporting/Lowhorn Big Data/Golden Rule Data/all.csv")
  write.csv(type,"//KNX1FS01/ED Reporting/Lowhorn Big Data/Golden Rule Data/type.csv")
  write.csv(allawg,"//KNX1FS01/ED Reporting/Lowhorn Big Data/Golden Rule Data/allawg.csv")
  write.csv(typeawg,"//KNX1FS01/ED Reporting/Lowhorn Big Data/Golden Rule Data/typeawg.csv")
  
}

ARMASTER <- read.csv("//KNX1FS01/ED Reporting/Lowhorn Big Data/Golden Rule Data/ARMASTER.csv",header=TRUE,stringsAsFactors = FALSE)

ARMASTER$A.R <- as.factor(ARMASTER$A.R)

hein <- activity %>%
  group_by(Department) %>%
  summarize(Avg_Calls = round(mean(Notated_Calls),2),
            Avg_Accounts_Worked = round(mean(Accounts_Worked),2),
            Avg_Messages = round(mean(Messages_Left),2),
            Avg_POE_Attempts = round(mean(POE_Attempts),2)) 

hein <- left_join(activity, hein,by="Department")

hein <- rename(hein,Calls=Notated_Calls,Messages = Messages_Left,Accounts.Worked=Accounts_Worked,Average.Calls=Avg_Calls,Average.Accounts.Worked=Avg_Accounts_Worked,Average.Messages=Avg_Messages)


setwd("//Knx3fs01/ED_BA_GROUP/Lowhorn/Golden Rule 3/Application")
runApp(host="0.0.0.0",port=5060)
