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
  x <- 1.
}


CODE <- read.csv("//knx1fs01/ED Reporting/Lowhorn Big Data/Golden Rule Data/CODE.csv")
#########DILLON HIGHTOWER ADD############

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


df <- readRDS('C:/ACTDB/ACT_DB.rds')

df$ACT_DATE <- as.Date(df$ACT_DATE,'%m/%d/%Y')

Date <- Sys.Date()-120
Date <- format(Date,'%m/%d/%Y')
Date <- stringr::str_split_fixed(Date,pattern='/',n=3)
Date <- paste0(Date[1],'/','1','/',Date[3])
Date <- as.Date(Date,'%m/%d/%Y')

df <- df[df$ACT_DATE >= Date,]

df$ACT_DATE <- format(df$ACT_DATE,'%m/%d/%Y')

DF <- df
gap <- df

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
  mutate(Hour.Min = Hour + Minute/60)%>%
  group_by(TFILE,DTE,EMPNUM) %>%
  mutate(Diff = Hour.Min-lag(Hour.Min,1)) %>%
  mutate(Diff_Minutes = Diff * 60)

df <- df[,-which(names(df)%in%c("T","DTE","Hour","Minute","Second","Diff"))]

##########lag for employee



df <- df %>%
  mutate(sub = ifelse(Diff_Minutes >= -60 & Diff_Minutes <= 60 & Diff_Minutes != 0,"sub","no"))

list <- c(df$sub[-1],"sub")

df<-cbind(df,list)

df <- df %>%
  mutate(list = ifelse(CODE_3 %in%c("PRM","CMP"),1,list))


df$list[is.na(df$list)] <- 1


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

COND$TFILE <- as.character(COND$TFILE)
pf <- left_join(pf,COND,by=c("TFILE"))
COND$TFILE <- as.numeric(COND$TFILE)

pf$ED_PROOF_INCOME_DT <- as.Date(pf$ED_PROOF_INCOME_DT,"%m/%d/%Y")

pf$ED_ALL_RHB_RECV_DT <- as.Date(pf$ED_ALL_RHB_RECV_DT,"%m/%d/%Y")

pf <- pf[,names(pf) != "ACT_DATE.y"]

ARMASTER <- select(ARMASTER,EMPNUM,A.R,manager,DEPT,off,SUB)
pf <- left_join(pf,ARMASTER,by="EMPNUM") %>%
  rename(Collector = A.R, Manager = manager, Department = DEPT, Office = off,Date = ACT_DATE.x)
pf <- pf[pf$Department %in% "PRO",]
pf <- pf[pf$SUB %in% c("AR","VERF","HIR","AWG","ADM"),]

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
                                     "November 2016", "December 2016","January 2017","February 2017", "March 2017",
                                     "April 2017","May 2017","June 2017","July 2017", "August 2017",'September 2017',
                                     ,'October 2017'))

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
COND$TFILE <- as.character(COND$TFILE)
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
GRRHBS$TFILE <- as.character(GRRHBS$TFILE)

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
rh$TFILE <- as.character(rh$TFILE)
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
                                     "November 2016", "December 2016","January 2017","February 2017","March 2017",
                                     "April 2017","May 2017", "June 2017","July 2017", "August 2017",'September 2017',
                                     'October 2017'))

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
            Messages_Rate = Messages_Left/Notated_Calls,
            POE_Attempts = sum(CODE_1=="1P")+sum(CODE_1=="2P"),
            POE_Percent = POE_Attempts/Notated_Calls)

activity <- left_join(worked,calls,by=c("EMPNUM","ACT_DATE"))

activity <- activity %>%
  mutate(Calls_Per_Account = Notated_Calls/Accounts_Worked)

activity[is.na(activity)] <- 0

ARMASTER <- ARMASTER %>%
  select(EMPNUM,A.R,manager,DEPT,off,SUB)

activity <- left_join(activity, ARMASTER,by="EMPNUM")
activity <- activity[activity$SUB %in% c("HIR","VERF","AWG","GAR","AR",'SWAT'),]

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
  "November 2016", "December 2016","January 2017","February 2017","March 2017",
  "April 2017","May 2017","June 2017","July 2017", "August 2017",'September 2017',
  'October 2017'))
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
                                                         "November 2016", "December 2016","January 2017","February 2017","March 2017",
                                                         "April 2017","May 2017","June 2017","July 2017", "August 2017",'September 2017',
                                                         'October 2017'))


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

####new

BG <- df %>%
  group_by(Collector, Manager, Office, Department, Date) %>%
  summarize(Contacts = n(),
            Closed_Calls = sum(CODE_3 %in% c("PRM","CMP")),
            Conversion_Rate = Closed_Calls/Contacts,
            RH_Conditions_Verified = sum(Rehab.Condition=="Yes")) %>%
  ungroup() %>%
  arrange(desc(Conversion_Rate))

BG$Month <- format(BG$Date, "%B %Y")
BG <- left_join(BG,activity,by=c("Month","Date","Collector","Manager","Department","Office"))

BG <- BG %>%
  mutate(Contact_Rate = Contacts/Notated_Calls)

BG <- BG[!BG$Collector %in% "",]
BG <- BG[!BG$Manager %in% "",]

tiff <- BG %>%
  group_by(Collector,Manager,Department,Office,Month) %>%
  summarize(Contacts=sum(Contacts)/n_distinct(Date),
            Closed_Calls=sum(Closed_Calls)/n_distinct(Date),
            Conversion_Rate=Closed_Calls/Contacts,
            Accounts_Worked = sum(Accounts_Worked)/n_distinct(Date),
            Notated_Calls=sum(Notated_Calls)/n_distinct(Date),
            Inbound_Calls=sum(Inbound_Calls)/n_distinct(Date),
            Outbound_Calls=sum(Outbound_Calls)/n_distinct(Date),
            Messages_Left=sum(Messages_Left)/n_distinct(Date),
            Message_Rate=Messages_Left/Outbound_Calls,
            POE_Attempts=sum(POE_Attempts)/n_distinct(Date),
            POE_Percent=POE_Attempts/Outbound_Calls,
            Calls_Per_Account=Outbound_Calls/Accounts_Worked,
            Calls_to_Contacts = Contacts/Notated_Calls,
            AW_to_Contacts = Contacts/Accounts_Worked)


tiff$Manager <- as.factor(tiff$Manager)
tiff$Office <- as.factor(tiff$Office)
tiff$Month <- as.factor(tiff$Month)

tiff[is.na(tiff)] <- 0

#########

emp <-c("0","1")
gap$EMPNUM <- as.character(gap$EMPNUM)
gap <- gap[!gap$EMPNUM %in% emp,]
rm(emp)

gap$T <- gsub("^.*? ","",gap$TIME)
gap$DTE <- gsub(" 0:00:00","",gap$ACT_DATE)
time <- as.data.frame(str_split_fixed(gap$T,":",n=3))
time <- dplyr::rename(time, Hour = V1, Minute = V2, Second = V3)

time$Hour <- as.numeric(as.character(time$Hour))
time$Minute <- as.numeric(as.character(time$Minute))
time$Second <- as.numeric(as.character(time$Second))

gap <- cbind(gap,time)
rm(time)
gap$DTE <- as.Date(gap$DTE,"%m/%d/%Y")

gap <- gap[gap$Hour >=8,]
gap <- gap %>%
  arrange(Second) %>% 
  arrange(Minute) %>%
  arrange(Hour) %>%
  arrange(DTE) %>%
  arrange(EMPNUM) %>%
  mutate(Minute = Minute + Second/60) %>%
  mutate(Hour.Min = Hour + Minute/60) %>%
  group_by(EMPNUM,DTE) %>%
  mutate(Diff = Hour.Min - lag(Hour.Min)) %>%
  mutate(Diff_Minutes = Diff *60)

ARMASTER <- ARMASTER[ARMASTER$SUB %in% c("HIR","VERF","AWG","GAR","AR","SWAT"),]

ARMASTER <- ARMASTER %>%
  select(EMPNUM,A.R,manager,DEPT,off)

gap <- left_join(gap,ARMASTER,by="EMPNUM")

gap <- gap[!is.na(gap$manager),]
gap <- gap[!gap$CODE_2 %in% c("CM","A3P","ATY"),]
gap <- gap[!gap$CODE_3 %in% c("PRM","CMP"),]

gap <- rename(gap,Collector=A.R,Manager=manager,Department=DEPT,Office=off)
gap$Office <- as.factor(gap$Office)
gap$Month <- format(gap$DTE,format="%B %Y")
gap$Office <- plyr::revalue(gap$Office,c("K"="Knoxville","C"="Columbus","B"="Columbus2","W"="Westlake","S"="Schuerger"))
gap <- gap[!gap$Office %in% c("AGY","ALL"),]
gap$Diff_Minutes[is.na(gap$Diff_Minutes)] <- 0
gap <- gap[!gap$Manager %in% "",]
gap$Manager <- as.factor(gap$Manager)
gap$Department <- as.factor(gap$Department)
gap$Month <- as.factor(gap$Month)

EMP_GAP_Day <- gap %>%
  group_by(Collector,Manager,DTE,Department,Office) %>%
  summarise(More_Than_5_Minutes = as.numeric(ifelse(sum(Diff_Minutes>=5)>3,sum(Diff_Minutes>=5),0)),
            More_Than_15 = as.numeric(ifelse(sum(Diff_Minutes>=15)>3,sum(Diff_Minutes>=15),0)),
            Hours_Of_Gap_Time = ifelse(round(sum(Diff_Minutes)/60-1.5,2)>=0,round(sum(Diff_Minutes)/60-1.5,2),0))
EMP_GAP_MONTH<- gap %>%
  group_by(Collector,Manager,Month,Department,Office) %>%
  summarise(More_Than_5_Minutes = as.numeric(ifelse(sum(Diff_Minutes>=5)>3,sum(Diff_Minutes>=5),0)),
            More_Than_15_Minutes = as.numeric(ifelse(sum(Diff_Minutes>=15)>3,sum(Diff_Minutes>=15),0)),
            Hours_Of_Gap_Time = ifelse(round(sum(Diff_Minutes)/60-1.5,2)>0,
                                       round(sum(Diff_Minutes)/60-1.5,2),
                                       0))
MGR_GAP_Day <- gap %>%
  group_by(Manager,DTE,Department,Office) %>%
  summarise(More_Than_5_Minutes = as.numeric(ifelse(sum(Diff_Minutes>=5)>3,sum(Diff_Minutes>=5),0)),
            More_Than_15_Minutes = as.numeric(ifelse(sum(Diff_Minutes>=15)>3,sum(Diff_Minutes>=15),0)),
            Hours_Of_Gap_Time = ifelse(round(sum(Diff_Minutes)/60-1.5,2)>=0,
                                       round(sum(Diff_Minutes)/60-1.5,2),
                                       0))

MGR_GAP_MONTH<- gap %>%
  group_by(Manager,Month,Department,Office) %>%
  summarise(More_Than_5_Minutes = as.numeric(ifelse(sum(Diff_Minutes>=5)>3,sum(Diff_Minutes>=5),0)),
            More_Than_15_Minutes = as.numeric(ifelse(sum(Diff_Minutes>=15)>3,sum(Diff_Minutes>=15),0)),
            Hours_Of_Gap_Time = ifelse(round(sum(Diff_Minutes)/60 - (n_distinct(DTE)*1.5),2)>0,
                                       round(sum(Diff_Minutes)/60 - (n_distinct(DTE)*1.5),2),
                                       0))
rm(BG)


MGR_GAP_MONTH <- plyr::rename(MGR_GAP_MONTH, c("More_Than_5_Minutes"="5 Minute Gaps","More_Than_15_Minutes"="15 Minute Gaps","Hours_Of_Gap_Time"="Hours of Gap Time"))
MGR_GAP_Day <- plyr::rename(MGR_GAP_Day, c("More_Than_5_Minutes"="5 Minute Gaps","More_Than_15_Minutes"="15 Minute Gaps","DTE"="Date","Hours_Of_Gap_Time"="Hours of Gap Time"))
EMP_GAP_MONTH <- plyr::rename(EMP_GAP_MONTH, c("More_Than_5_Minutes"="5 Minute Gaps","More_Than_15_Minutes"="15 Minute Gaps","Hours_Of_Gap_Time"="Hours of Gap Time"))
EMP_GAP_Day <- plyr::rename(EMP_GAP_Day, c("More_Than_5_Minutes"="5 Minute Gaps","More_Than_15"="15 Minute Gaps","DTE"="Date","Hours_Of_Gap_Time"="Hours of Gap Time"))

RESDATA <- read.csv("//Knx3it/edopsmgmt/Reports/Res Referrals & Rankings/RES Database/RESDATA.csv",
                    stringsAsFactors=FALSE)

ARMASTER <- read.csv("//KNX1FS01/ED Reporting/Lowhorn Big Data/Golden Rule Data/ARMASTER.csv",
                     header=TRUE,stringsAsFactors = FALSE)

RESDATA$Which.MGR <- factor(RESDATA$Which.MGR)

RESDATA$Which.MGR <- plyr::revalue(RESDATA$Which.MGR,c("HOUSE"=900))

RESDATA$Which.MGR <- as.character(RESDATA$Which.MGR)  
RESDATA$Which.MGR <- as.numeric(RESDATA$Which.MGR)

ARMASTER <- ARMASTER %>%
  select(EMPNUM, A.R, desk, manager, DEPT, off, SUB)

ARMASTER <- rename(ARMASTER, Credit.AR = desk)

RESDATA <- left_join(RESDATA,ARMASTER,by="Credit.AR")
rm(ARMASTER)

res <- RESDATA %>%
  group_by(off, DEPT, manager, A.R, Month) %>%
  summarize(Good = sum(Good.or.Bad == 1),
            Bad = sum(Good.or.Bad == 0),
            Death = sum(Condition == "DEATH" & Good.or.Bad == 1),
            Incarceration = sum(Condition == "INCR" & Good.or.Bad == 1),
            Disability = sum(Condition == "DIS" & Good.or.Bad == 1))

res$manager[res$manager %in% ""] <- "GAVIN, THOMAS"
res$A.R[res$A.R %in% ""] <- "Non in AR Master"

res$off[res$off %in% ""] <- "HOUSE"
res$DEPT[res$DEPT %in% ""] <- "ADM"


res$Month <- as.Date(res$Month,"%m/%d/%Y")
res$Month <- format(res$Month, "%B %Y")

res <- rename(res, Office = off, Department = DEPT, Manager = manager, Collector = A.R)

res$Office <- as.factor(res$Office)
res$Department <- as.factor(res$Department)
res$Manager <- as.factor(res$Manager)
res$Collector <- as.factor(res$Collector)
res$Month <- as.factor(res$Month)

res$Office <- plyr::revalue(res$Office, c("B"="Columbus 2","C"="Columbus","K"="Knoxville",
                                          "S"="Schuerger","W"="Westlake","ALL"="HOUSE"))


res$Month <- factor(res$Month,levels=c("October 2016","November 2016", "December 2016","January 2017","February 2017","March 2017",
                                       "April 2017","May 2017","June 2017","July 2017", "August 2017",'September 2017',
                                       'October 2017'))


RGR <- read.csv("//KNX3IT/AWG Management/RGR/RGR Database.csv", stringsAsFactors=FALSE)
ARMASTER <- read.csv("//KNX1FS01/ED Reporting/Lowhorn Big Data/Golden Rule Data/ARMASTER.csv",header=TRUE,stringsAsFactors = FALSE)

RGR$CRAR[is.na(RGR$CRAR)] <- 811
RGR$CRAR[RGR$CRAR>=800] <- 811

ARMASTER <- ARMASTER %>%
  select(A.R, desk, manager, DEPT, off)

ARMASTER <- rename(ARMASTER, CRAR = desk, Collector = A.R, Manager = manager, Department = DEPT, Office = off)
ARMASTER$CRAR <- as.character(ARMASTER$CRAR)

RGR <- left_join(RGR,ARMASTER,by="CRAR")
rm(ARMASTER)

RGR$Month <- as.Date(RGR$Month,"%m/%d/%Y")
RGR$Month <- format(RGR$Month,"%B %Y")

RGR$Month <- as.factor(RGR$Month)
RGR$Collector <- as.factor(RGR$Collector)
RGR$Vendor.File. <- as.factor(RGR$Vendor.File.)
RGR$Manager <- as.factor(RGR$Manager)
RGR$Department <- as.factor(RGR$Department)
RGR$Office <- as.factor(RGR$Office)

RGR$Office <- plyr::revalue(RGR$Office,c("K"="Knoxville","B"="Columbus 2","C"="Columbus","S"="Schuerger","W"="Westlake"))
RGR$Manager <- as.character(RGR$Manager)
RGR$Manager[RGR$Manager %in% ""] <- "NLE Manager"
RGR$Manager <- as.factor(RGR$Manager)

RGR$Collector <- as.character(RGR$Collector)
RGR$Collector[RGR$Collector %in% ""] <- "NLE Collector"
RGR$Collector <- as.factor(RGR$Collector)

RGR$Office[is.na(RGR$Office)] <- "Knoxville"
RGR$Department[is.na(RGR$Department)] <- "AWG"
RGR$Manager[is.na(RGR$Manager)] <- "NLE Manager"
RGR$Collector[is.na(RGR$Collector)] <- "NLE Collector"

RGR$Month <- factor(RGR$Month,levels=c("October 2016","November 2016", "December 2016","January 2017","February 2017","March 2017",
                                       "April 2017","May 2017","June 2017","July 2017", "August 2017",'September 2017',
                                       'October 2017'))

NINE <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/NINE.csv", stringsAsFactors=FALSE)

NINE <- select(NINE,ED_ACCT_ACT_DATE,CM_FILENO)
NINE <- rename(NINE,Debtor=CM_FILENO)
RGR <- left_join(RGR,NINE,by="Debtor")
RGR$ED_ACCT_ACT_DATE <- as.Date(RGR$ED_ACCT_ACT_DATE,"%m/%d/%Y")
rm(NINE)
RGR$ED_ACCT_ACT_DATE[is.na(RGR$ED_ACCT_ACT_DATE)] <- "2000-01-01" 

BIFSIF <- read.csv("//Knx3fs01/ED_BA_GROUP/Lowhorn/BIFSIF.csv", stringsAsFactors=FALSE)
ARMASTER <- read.csv("//knx1fs01/ED Reporting/Lowhorn Big Data/Golden Rule Data/ARMASTER.csv", stringsAsFactors=FALSE)
ARMASTER <- ARMASTER %>%
  select(EMPNUM,A.R,manager,DEPT,off) %>%
  rename(EMPL = EMPNUM, Collector = A.R, Manager = manager, Department = DEPT, Office = off)
BIFSIF <- left_join(BIFSIF,ARMASTER,by="EMPL")
rm(ARMASTER)
BIFSIF$Month <- as.Date(BIFSIF$Month, "%m/%d/%Y")
BIFSIF$Month <- format(BIFSIF$Month, "%B %Y")
BIFSIF$Collector[is.na(BIFSIF$Collector)] <- "NLE"
BIFSIF$Manager[is.na(BIFSIF$Manager)] <- "NLE"
BIFSIF$Department[is.na(BIFSIF$Department)] <- "NLE"
BIFSIF$Office[is.na(BIFSIF$Office)] <- "Knoxville"
BIFSIF$Month <- as.factor(BIFSIF$Month)
BIFSIF$Program <- as.factor(BIFSIF$Program)
BIFSIF$Collector <- as.factor(BIFSIF$Collector)
BIFSIF$Manager <- as.factor(BIFSIF$Manager)
BIFSIF$Department <- as.factor(BIFSIF$Department)
BIFSIF$Office <- factor(BIFSIF$Office)
BIFSIF$Office <- plyr::revalue(BIFSIF$Office,c("K"="Knoxville","C"="Columbus","B"="Columbus 2","W"="Westlake","S"="Schuerger","AGY"="Knoxville","ALL"="Knoxville"))

A <- BIFSIF %>%
  group_by(Office, Department, Manager, Collector, Month) %>%
  summarize(Total_BIF_SIF = n())
B <- RGR %>%
  group_by(Office, Department, Manager, Collector, Month) %>%
  summarize(Total_RGR = n())
C <- res %>%
  group_by(Office, Department, Manager, Collector, Month) %>%
  summarize(Total_Res = sum(Good))

D <- full_join(A,B,by=c("Office","Department","Manager","Collector","Month"))
E <- full_join(D,C,by=c("Office","Department","Manager","Collector","Month"))
E[is.na(E)] <- 0
E <- E %>%
  mutate(Total = Total_BIF_SIF + Total_RGR + Total_Res)
E <- E[!E$Office %in% c("0","HOUSE"),]



RESDATA <- read.csv("//Knx3it/edopsmgmt/Reports/Res Referrals & Rankings/RES Database/RESDATA.csv",
                    stringsAsFactors=FALSE)

RESDATA$Month <- as.Date(RESDATA$Month,"%m/%d/%Y")
RESDATA$Month <- format(RESDATA$Month,"%B %Y")
ARMASTER <- read.csv("//KNX1FS01/ED Reporting/Lowhorn Big Data/Golden Rule Data/ARMASTER.csv",header=TRUE,stringsAsFactors = FALSE)
ARMASTER <- ARMASTER[ARMASTER$SUB %in% c("HIR","VERF","AWG","GAR","AR","SWAT"),]
ARMASTER <- ARMASTER %>%
  select(EMPNUM,A.R,manager,DEPT,off)

ARMASTER$A.R <- as.factor(ARMASTER$A.R)

saveRDS(activity,'//KNX3IT/AWG Management/KPI Data/Input/activity.rds')
saveRDS(all,'//KNX3IT/AWG Management/KPI Data/Input/all.rds')
saveRDS(allawg,'//KNX3IT/AWG Management/KPI Data/Input/allawg.rds')
saveRDS(ARMASTER,'//KNX3IT/AWG Management/KPI Data/Input/ARMASTER.rds')
saveRDS(BIFSIF,'//KNX3IT/AWG Management/KPI Data/Input/BIFSIF.rds')
saveRDS(CDay,'//KNX3IT/AWG Management/KPI Data/Input/CDay.rds')
saveRDS(CMonth,'//KNX3IT/AWG Management/KPI Data/Input/CMonth.rds')
saveRDS(CODE,'//KNX3IT/AWG Management/KPI Data/Input/CODE.rds')
saveRDS(DeptDay,'//KNX3IT/AWG Management/KPI Data/Input/DeptDay.rds')
saveRDS(DeptMonth,'//KNX3IT/AWG Management/KPI Data/Input/DeptMonth.rds')
saveRDS(df,'//KNX3IT/AWG Management/KPI Data/Input/df.rds')
saveRDS(docs,'//KNX3IT/AWG Management/KPI Data/Input/docs.rds')
saveRDS(E,'//KNX3IT/AWG Management/KPI Data/Input/E.rds')
saveRDS(EMP_GAP_Day,'//KNX3IT/AWG Management/KPI Data/Input/EMP_GAP_Day.rds')
saveRDS(EMP_GAP_MONTH,'//KNX3IT/AWG Management/KPI Data/Input/EMP_GAP_MONTH.rds')
saveRDS(gap,'//KNX3IT/AWG Management/KPI Data/Input/gap.rds')
saveRDS(hein,'//KNX3IT/AWG Management/KPI Data/Input/hein.rds')
saveRDS(MDay,'//KNX3IT/AWG Management/KPI Data/Input/MDay.rds')
saveRDS(MGR_GAP_Day,'//KNX3IT/AWG Management/KPI Data/Input/MGR_GAP_Day.rds')
saveRDS(MGR_GAP_MONTH,'//KNX3IT/AWG Management/KPI Data/Input/MGR_GAP_MONTH.rds')
saveRDS(MMonth,'//KNX3IT/AWG Management/KPI Data/Input/MMonth.rds')
saveRDS(ODay,'//KNX3IT/AWG Management/KPI Data/Input/ODay.rds')
saveRDS(OMonth,'//KNX3IT/AWG Management/KPI Data/Input/OMonth.rds')
saveRDS(pf,'//KNX3IT/AWG Management/KPI Data/Input/pf.rds')
saveRDS(res,'//KNX3IT/AWG Management/KPI Data/Input/res.rds')
saveRDS(RESDATA,'//KNX3IT/AWG Management/KPI Data/Input/RESDATA.rds')
saveRDS(RGR,'//KNX3IT/AWG Management/KPI Data/Input/RGR.rds')
saveRDS(tiff,'//KNX3IT/AWG Management/KPI Data/Input/tiff.rds')
saveRDS(Tracker,'//KNX3IT/AWG Management/KPI Data/Input/Tracker.rds')
saveRDS(type,'//KNX3IT/AWG Management/KPI Data/Input/type.rds')
saveRDS(typeawg,'//KNX3IT/AWG Management/KPI Data/Input/typeawg.rds')
