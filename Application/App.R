currmonth = format(Sys.Date(),"%B %Y")

x <- c()
if(weekdays(Sys.Date())=="Monday"){
  x <- 3
}else{
  x <- 1
}

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggvis)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Slicers",tabName = "slicers", icon = icon("th")),
    downloadButton("download", 'Download Daily Data')
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            column(width=4,selectInput("type","Type of Call",
                                       choices=c("Collections","Program Follow Up"))),
            column(width=4,selectInput("time","Timeframe",
                                       choices=c("Monthly","Daily"))),
            column(width=4,selectInput("group","Group",
                                       choices=c("Office","Department","Manager","Collector"))),
#            textOutput("text"),
            DT::dataTableOutput("dt")
    ),
    tabItem(tabName = "slicers",
            column(width=5,
                   box(dateInput("Day",
                                 label="Date",
#############################################DILLON HIGHTOWER UPDATE######################################                                 
                                 value=(Sys.Date()-x),
##########################################################################################################
                                 min="2014-12-31",
                                 format="m/d/yyyy")),
                   box(selectInput("Month","Month",
                                   choices=levels(df$Month),
                                   selected=currmonth)),
                   box(checkboxGroupInput("CCode","Call Code",
                                          choices=levels(df$CODE_1),
                                          selected=levels(df$CODE_1),
                                          inline=TRUE)),
                   box(checkboxGroupInput("PCode","Contact Party",
                                          choices=levels(df$CODE_2),
                                          selected=levels(df$CODE_2),
                                          inline=TRUE))),
            column(width=7,
                   box(width=12,h1("Call Code Description Table")),
                   dataTableOutput("codes"))
    )
    
  )
)


ui <- dashboardPage(
  dashboardHeader(title = "KPI Portal"),
  sidebar,
  body
)

server <- function(input, output) {
  output$codes <- renderDataTable({
    datatable(CODE,extensions = 'TableTools', rownames=FALSE,class = 'cell-border stripe',filter="top",
              options = list(
                searching=TRUE,
                autoWidth=TRUE
              ))
  })
  
  manday <- reactive ({
    a <- subset(df,Date == as.character(input$Day))
   # c <- a[a$CODE_1 %in% c(input$CCode[1],input$CCode[2],input$CCode[3],input$CCode[4],input$CCode[5],
  #                         input$CCode[6],input$CCode[7],input$CCode[7],input$CCode[8],input$CCode[9],
    #                       input$CCode[10],input$CCode[11],input$CCode[12],input$CCode[13],input$CCode[14],
     #                      input$CCode[15],input$CCode[16],input$CCode[17],input$CCode[18],input$CCode[19],
      #                     input$CCode[20],input$CCode[21],input$CCode[22],input$CCode[23],input$CCode[24],
       #                    input$CCode[25],input$CCode[26],input$CCode[27],input$CCode[28],input$CCode[29],
        #                   input$CCode[30],input$CCode[31],input$CCode[32],input$CCode[33]),]
    d <- a[a$CODE_2 %in% c(input$PCode[1],input$PCode[2],input$PCode[3]),]
    e <- d %>%
      group_by(Manager, Office, Department, Date) %>%
      summarize(Contacts = n(),
                Closed_Calls = sum(CODE_3 %in% c("PRM","CMP")),
                Conversion_Rate = Closed_Calls/Contacts,
                RH_Conditions_Verified = sum(Rehab.Condition=="Yes")) %>%
      ungroup() %>%
      arrange(desc(Conversion_Rate))
    e$Manager <- as.factor(e$Manager)
    e <- left_join(e,MDay,by=c("Date","Manager"))
    e$Manager <- as.factor(e$Manager)
    e <- e[,-4]
    e <- plyr::rename(e,c("RH_Conditions_Verified"="RH Conditions Verified","Closed_Calls"="Closed Calls","Rehabs_On_Tracker"="Rehabs On Tracker","Conversion_Rate"="Conversion Rate"))
    e
  })
  
  manmon <- reactive ({
    a <- subset(df,Month == input$Month)
   # c <- a[a$CODE_1 %in% c(input$CCode[1],input$CCode[2],input$CCode[3],input$CCode[4],input$CCode[5],
    #                       input$CCode[6],input$CCode[7],input$CCode[7],input$CCode[8],input$CCode[9],
     #                      input$CCode[10],input$CCode[11],input$CCode[12],input$CCode[13],input$CCode[14],
      ##                    input$CCode[20],input$CCode[21],input$CCode[22],input$CCode[23],input$CCode[24],
         #                  input$CCode[25],input$CCode[26],input$CCode[27],input$CCode[28],input$CCode[29],
        #                   input$CCode[30],input$CCode[31],input$CCode[32],input$CCode[33]),]
    d <- a[a$CODE_2 %in% c(input$PCode[1],input$PCode[2],input$PCode[3]),]
    e <- d %>%
      group_by(Manager, Office, Department, Month) %>%
      summarize(Contacts = n(),
                Closed_Calls = sum(CODE_3 %in% c("PRM","CMP")),
                Conversion_Rate = Closed_Calls/Contacts,
                RH_Conditions_Verified = sum(Rehab.Condition=="Yes")) %>%
      ungroup() %>%
      arrange(desc(Conversion_Rate))
    e$Manager <- as.factor(e$Manager)
    e$Month <- as.factor(e$Month)
    e <- left_join(e,MMonth,by=c("Manager","Month"))
    e$Manager <- as.factor(e$Manager)
    e <- plyr::rename(e,c("RH_Conditions_Verified"="RH Conditions Verified","Closed_Calls"="Closed Calls","Rehabs_On_Tracker"="Rehabs On Tracker","Conversion_Rate"="Conversion Rate"))
    e
  })
  
  colmon <- reactive ({
    a <- subset(df,Month == input$Month)
   # c <- a[a$CODE_1 %in% c(input$CCode[1],input$CCode[2],input$CCode[3],input$CCode[4],input$CCode[5],
    #                       input$CCode[6],input$CCode[7],input$CCode[7],input$CCode[8],input$CCode[9],
     #                      input$CCode[10],input$CCode[11],input$CCode[12],input$CCode[13],input$CCode[14],
      #                     input$CCode[15],input$CCode[16],input$CCode[17],input$CCode[18],input$CCode[19],
       #                    input$CCode[20],input$CCode[21],input$CCode[22],input$CCode[23],input$CCode[24],
        #                   input$CCode[25],input$CCode[26],input$CCode[27],input$CCode[28],input$CCode[29],
         #                  input$CCode[30],input$CCode[31],input$CCode[32],input$CCode[33]),]
    d <- a[a$CODE_2 %in% c(input$PCode[1],input$PCode[2],input$PCode[3]),]
    e <- d %>%
      group_by(Collector, Manager, Office, Department, Month) %>%
      summarize(Contacts = n(),
                Closed_Calls = sum(CODE_3 %in% c("PRM","CMP")),
                Conversion_Rate = Closed_Calls/Contacts,
                RH_Conditions_Verified = sum(Rehab.Condition=="Yes")) %>%
      ungroup() %>%
      arrange(desc(Conversion_Rate))
    e$Collector <- as.factor(e$Collector)
    e$Month <- as.factor(e$Month)
    e <- left_join(e,CMonth,by=c("Collector","Month"))
    e$Collector <- as.factor(e$Collector)
    e <- plyr::rename(e,c("RH_Conditions_Verified"="RH Conditions Verified","Closed_Calls"="Closed Calls","Rehabs_On_Tracker"="Rehabs On Tracker","Conversion_Rate"="Conversion Rate"))
    e
  })
  
  colday <- reactive ({
    a <- subset(df,Date == as.character(input$Day))
    #c <- a[a$CODE_1 %in% c(input$CCode[1],input$CCode[2],input$CCode[3],input$CCode[4],input$CCode[5],
     #                      input$CCode[6],input$CCode[7],input$CCode[7],input$CCode[8],input$CCode[9],
      #                     input$CCode[10],input$CCode[11],input$CCode[12],input$CCode[13],input$CCode[14],
       #                    input$CCode[15],input$CCode[16],input$CCode[17],input$CCode[18],input$CCode[19],
        #                   input$CCode[20],input$CCode[21],input$CCode[22],input$CCode[23],input$CCode[24],
         #                  input$CCode[25],input$CCode[26],input$CCode[27],input$CCode[28],input$CCode[29],
          #                 input$CCode[30],input$CCode[31],input$CCode[32],input$CCode[33]),]
    d <- a[a$CODE_2 %in% c(input$PCode[1],input$PCode[2],input$PCode[3]),]
    e <- d %>%
      group_by(Collector, Manager, Office, Department, Date) %>%
      summarize(Contacts = n(),
                Closed_Calls = sum(CODE_3 %in% c("PRM","CMP")),
                Conversion_Rate = Closed_Calls/Contacts,
                RH_Conditions_Verified = sum(Rehab.Condition=="Yes")) %>%
      ungroup() %>%
      arrange(desc(Conversion_Rate))
    e$Collector <- as.factor(e$Collector)
    e <- left_join(e,CDay,by=c("Collector","Date"))
    e$Collector <- as.factor(e$Collector)
    e <- e[,-5]
    e <- plyr::rename(e,c("RH_Conditions_Verified"="RH Conditions Verified","Closed_Calls"="Closed Calls","Rehabs_On_Tracker"="Rehabs On Tracker","Conversion_Rate"="Conversion Rate"))
    e
  })
  
  depday <- reactive ({
    a <- subset(df,Date == as.character(input$Day))
    #c <- a[a$CODE_1 %in% c(input$CCode[1],input$CCode[2],input$CCode[3],input$CCode[4],input$CCode[5],
     #                      input$CCode[6],input$CCode[7],input$CCode[7],input$CCode[8],input$CCode[9],
      #                     input$CCode[10],input$CCode[11],input$CCode[12],input$CCode[13],input$CCode[14],
       #                    input$CCode[15],input$CCode[16],input$CCode[17],input$CCode[18],input$CCode[19],
        #                   input$CCode[20],input$CCode[21],input$CCode[22],input$CCode[23],input$CCode[24],
         #                  input$CCode[25],input$CCode[26],input$CCode[27],input$CCode[28],input$CCode[29],
          #                 input$CCode[30],input$CCode[31],input$CCode[32],input$CCode[33]),]
    d <- a[a$CODE_2 %in% c(input$PCode[1],input$PCode[2],input$PCode[3]),]
    e <- d %>%
      group_by(Department, Office, Date) %>%
      summarize(Contacts = n(),
                Closed_Calls = sum(CODE_3 %in% c("PRM","CMP")),
                Conversion_Rate = Closed_Calls/Contacts,
                RH_Conditions_Verified = sum(Rehab.Condition=="Yes")) %>%
      ungroup() %>%
      arrange(desc(Conversion_Rate))
    e$Department <- as.factor(e$Department)
    e <- left_join(e,DeptDay,by=c("Department","Office","Date"))
    e$Department <- as.factor(e$Department)
    e <- e[,-3]
    e <- plyr::rename(e,c("RH_Conditions_Verified"="RH Conditions Verified","Closed_Calls"="Closed Calls","Rehabs_On_Tracker"="Rehabs On Tracker","Conversion_Rate"="Conversion Rate"))
    e
  })
  
  depmon <- reactive ({
    a <- subset(df,Month == input$Month)
    #c <- a[a$CODE_1 %in% c(input$CCode[1],input$CCode[2],input$CCode[3],input$CCode[4],input$CCode[5],
     #                      input$CCode[6],input$CCode[7],input$CCode[7],input$CCode[8],input$CCode[9],
      #                     input$CCode[10],input$CCode[11],input$CCode[12],input$CCode[13],input$CCode[14],
       #                    input$CCode[15],input$CCode[16],input$CCode[17],input$CCode[18],input$CCode[19],
        #                   input$CCode[20],input$CCode[21],input$CCode[22],input$CCode[23],input$CCode[24],
         #                  input$CCode[25],input$CCode[26],input$CCode[27],input$CCode[28],input$CCode[29],
          #                 input$CCode[30],input$CCode[31],input$CCode[32],input$CCode[33]),]
    d <- a[a$CODE_2 %in% c(input$PCode[1],input$PCode[2],input$PCode[3]),]
    e <- d %>%
      group_by(Department, Office, Month) %>%
      summarize(Contacts = n(),
                Closed_Calls = sum(CODE_3 %in% c("PRM","CMP")),
                Conversion_Rate = Closed_Calls/Contacts,
                RH_Conditions_Verified = sum(Rehab.Condition=="Yes")) %>%
      ungroup() %>%
      arrange(desc(Conversion_Rate))
    e$Department <- as.factor(e$Department)
    e <- left_join(e,DeptMonth,by=c("Department","Office","Month"))
    e$Department <- as.factor(e$Department)
    #e <- e[,-4]
    e <- plyr::rename(e,c("RH_Conditions_Verified"="RH Conditions Verified","Closed_Calls"="Closed Calls","Rehabs_On_Tracker"="Rehabs On Tracker","Conversion_Rate"="Conversion Rate"))
    e
  })
  
  offday <- reactive ({
    a <- subset(df,Date == as.character(input$Day))
    #c <- a[a$CODE_1 %in% c(input$CCode[1],input$CCode[2],input$CCode[3],input$CCode[4],input$CCode[5],
     #                      input$CCode[6],input$CCode[7],input$CCode[7],input$CCode[8],input$CCode[9],
      #                     input$CCode[10],input$CCode[11],input$CCode[12],input$CCode[13],input$CCode[14],
       #                    input$CCode[15],input$CCode[16],input$CCode[17],input$CCode[18],input$CCode[19],
        #                   input$CCode[20],input$CCode[21],input$CCode[22],input$CCode[23],input$CCode[24],
         #                  input$CCode[25],input$CCode[26],input$CCode[27],input$CCode[28],input$CCode[29],
          #                 input$CCode[30],input$CCode[31],input$CCode[32],input$CCode[33]),]
    d <- a[a$CODE_2 %in% c(input$PCode[1],input$PCode[2],input$PCode[3]),]
    e <- d %>%
      group_by(Office, Date) %>%
      summarize(Contacts = n(),
                Closed_Calls = sum(CODE_3 %in% c("PRM","CMP")),
                Conversion_Rate = Closed_Calls/Contacts,
                RH_Conditions_Verified = sum(Rehab.Condition=="Yes")) %>%
      ungroup() %>%
      arrange(desc(Conversion_Rate))
    e$Office <- as.factor(e$Office)
    e <- left_join(e,ODay,by=c("Office","Date"))
    e$Office <- as.factor(e$Office)
    e <- e[,-2]
    e <- plyr::rename(e,c("RH_Conditions_Verified"="RH Conditions Verified","Closed_Calls"="Closed Calls","Rehabs_On_Tracker"="Rehabs On Tracker","Conversion_Rate"="Conversion Rate"))
    e
  })
  
  offmon <- reactive ({
    a <- subset(df,Month == input$Month)
    #c <- a[a$CODE_1 %in% c(input$CCode[1],input$CCode[2],input$CCode[3],input$CCode[4],input$CCode[5],
     #                      input$CCode[6],input$CCode[7],input$CCode[7],input$CCode[8],input$CCode[9],
      #                     input$CCode[10],input$CCode[11],input$CCode[12],input$CCode[13],input$CCode[14],
       #                    input$CCode[15],input$CCode[16],input$CCode[17],input$CCode[18],input$CCode[19],
        #                   input$CCode[20],input$CCode[21],input$CCode[22],input$CCode[23],input$CCode[24],
         #                  input$CCode[25],input$CCode[26],input$CCode[27],input$CCode[28],input$CCode[29],
          #                 input$CCode[30],input$CCode[31],input$CCode[32],input$CCode[33]),]
    d <- a[a$CODE_2 %in% c(input$PCode[1],input$PCode[2],input$PCode[3]),]
    e <- d %>%
      group_by(Office, Month) %>%
      summarize(Contacts = n(),
                Closed_Calls = sum(CODE_3 %in% c("PRM","CMP")),
                Conversion_Rate = Closed_Calls/Contacts,
                RH_Conditions_Verified = sum(Rehab.Condition=="Yes")) %>%
      ungroup() %>%
      arrange(desc(Conversion_Rate))
    e$Office <- as.factor(e$Office)
    e <- left_join(e,OMonth,by=c("Office","Month"))
    e$Office <- as.factor(e$Office)
    #e <- e[,-4]
    e <- plyr::rename(e,c("RH_Conditions_Verified"="RH Conditions Verified","Closed_Calls"="Closed Calls","Rehabs_On_Tracker"="Rehabs On Tracker","Conversion_Rate"="Conversion Rate"))
    e
  })
  
  pfoffday <- reactive ({
    a <- pf
  #  c <- a[a$CODE_1 %in% c(input$CCode[1],input$CCode[2],input$CCode[3],input$CCode[4],input$CCode[5],
   #                        input$CCode[6],input$CCode[7],input$CCode[7],input$CCode[8],input$CCode[9],
    #                       input$CCode[10],input$CCode[11],input$CCode[12],input$CCode[13],input$CCode[14],
     #                      input$CCode[15],input$CCode[16],input$CCode[17],input$CCode[18],input$CCode[19],
      #                     input$CCode[20],input$CCode[21],input$CCode[22],input$CCode[23],input$CCode[24],
       #                    input$CCode[25],input$CCode[26],input$CCode[27],input$CCode[28],input$CCode[29],
        #                   input$CCode[30],input$CCode[31],input$CCode[32],input$CCode[33]),]
    d <- a[a$CODE_2 %in% c(input$PCode[1],input$PCode[2],input$PCode[3]),]
    e <- d %>%
      group_by(Office, Date) %>%
      summarize(Contacts = n(),
                Income_Paperwork_Back = sum(!is.na(ED_PROOF_INCOME_DT)),
                All_Rehab_Docs_Back = sum(!is.na(ED_ALL_RHB_RECV_DT)),
                Avg_Days_To_Get_Income_Docs = round(sum((Days_for_Income_Docs))/Income_Paperwork_Back,2),
                Avg_Days_To_Get_All_Docs = round(sum((Days_for_All_RHB_Docs))/All_Rehab_Docs_Back,2)) %>%
      ungroup()
    e <- e %>%
      mutate(Avg_Days_To_Get_All_Docs = ifelse(is.nan(Avg_Days_To_Get_All_Docs)|is.na(Avg_Days_To_Get_All_Docs),
                                               0,
                                               Avg_Days_To_Get_All_Docs),
             Avg_Days_To_Get_Income_Docs = ifelse(is.nan(Avg_Days_To_Get_Income_Docs)|is.na(Avg_Days_To_Get_Income_Docs),
                                                  0,
                                                  Avg_Days_To_Get_Income_Docs))
    e$Office <- as.factor(e$Office)
    e <- plyr::rename(e,c("Income_Paperwork_Back"="Income PPW Back","All_Rehab_Docs_Back"="RAL Back",
                          "Avg_Days_To_Get_Income_Docs"="Avg Days To Get Income Docs",
                          "Avg_Days_To_Get_All_Docs"="Avg Days to Get RAL"))
    f <- subset(e,Date == as.character(input$Day))
    f <- f[,-2]
    f
  })
  
  pfoffmon <- reactive ({
    a <- pf
    #c <- a[a$CODE_1 %in% c(input$CCode[1],input$CCode[2],input$CCode[3],input$CCode[4],input$CCode[5],
     #                      input$CCode[6],input$CCode[7],input$CCode[7],input$CCode[8],input$CCode[9],
      #                     input$CCode[10],input$CCode[11],input$CCode[12],input$CCode[13],input$CCode[14],
       #                    input$CCode[15],input$CCode[16],input$CCode[17],input$CCode[18],input$CCode[19],
        #                   input$CCode[20],input$CCode[21],input$CCode[22],input$CCode[23],input$CCode[24],
         #                  input$CCode[25],input$CCode[26],input$CCode[27],input$CCode[28],input$CCode[29],
          #                 input$CCode[30],input$CCode[31],input$CCode[32],input$CCode[33]),]
    d <- a[a$CODE_2 %in% c(input$PCode[1],input$PCode[2],input$PCode[3]),]
    e <- d %>%
      group_by(Office, Month) %>%
      summarize(Contacts = n(),
                Income_Paperwork_Back = sum(!is.na(ED_PROOF_INCOME_DT)),
                All_Rehab_Docs_Back = sum(!is.na(ED_ALL_RHB_RECV_DT)),
                Avg_Days_To_Get_Income_Docs = round(sum((Days_for_Income_Docs))/Income_Paperwork_Back,2),
                Avg_Days_To_Get_All_Docs = round(sum((Days_for_All_RHB_Docs))/All_Rehab_Docs_Back,2)) %>%
      ungroup()
    e <- e %>%
      mutate(Avg_Days_To_Get_All_Docs = ifelse(is.nan(Avg_Days_To_Get_All_Docs)|is.na(Avg_Days_To_Get_All_Docs),
                                               0,
                                               Avg_Days_To_Get_All_Docs),
             Avg_Days_To_Get_Income_Docs = ifelse(is.nan(Avg_Days_To_Get_Income_Docs)|is.na(Avg_Days_To_Get_Income_Docs),
                                                  0,
                                                  Avg_Days_To_Get_Income_Docs))
    
    e$Office <- as.factor(e$Office)
    e <- plyr::rename(e,c("Income_Paperwork_Back"="Income PPW Back","All_Rehab_Docs_Back"="RAL Back",
                          "Avg_Days_To_Get_Income_Docs"="Avg Days To Get Income Docs",
                          "Avg_Days_To_Get_All_Docs"="Avg Days to Get RAL"))
    f <- subset(e,Month == input$Month)
    f
  })
  
  pfdepmon <- reactive ({
    a <- pf
    #c <- a[a$CODE_1 %in% c(input$CCode[1],input$CCode[2],input$CCode[3],input$CCode[4],input$CCode[5],
     #                      input$CCode[6],input$CCode[7],input$CCode[7],input$CCode[8],input$CCode[9],
      #                     input$CCode[10],input$CCode[11],input$CCode[12],input$CCode[13],input$CCode[14],
       #                    input$CCode[15],input$CCode[16],input$CCode[17],input$CCode[18],input$CCode[19],
        #                   input$CCode[20],input$CCode[21],input$CCode[22],input$CCode[23],input$CCode[24],
         #                  input$CCode[25],input$CCode[26],input$CCode[27],input$CCode[28],input$CCode[29],
          #                 input$CCode[30],input$CCode[31],input$CCode[32],input$CCode[33]),]
    d <- a[a$CODE_2 %in% c(input$PCode[1],input$PCode[2],input$PCode[3]),]
    e <- d %>%
      group_by(Department, Office, Month) %>%
      summarize(Contacts = n(),
                Income_Paperwork_Back = sum(!is.na(ED_PROOF_INCOME_DT)),
                All_Rehab_Docs_Back = sum(!is.na(ED_ALL_RHB_RECV_DT)),
                Avg_Days_To_Get_Income_Docs = round(sum((Days_for_Income_Docs))/Income_Paperwork_Back,2),
                Avg_Days_To_Get_All_Docs = round(sum((Days_for_All_RHB_Docs))/All_Rehab_Docs_Back,2)) %>%
      ungroup()
    e <- e %>%
      mutate(Avg_Days_To_Get_All_Docs = ifelse(is.nan(Avg_Days_To_Get_All_Docs)|is.na(Avg_Days_To_Get_All_Docs),
                                               0,
                                               Avg_Days_To_Get_All_Docs),
             Avg_Days_To_Get_Income_Docs = ifelse(is.nan(Avg_Days_To_Get_Income_Docs)|is.na(Avg_Days_To_Get_Income_Docs),
                                                  0,
                                                  Avg_Days_To_Get_Income_Docs))
    
    e$Office <- as.factor(e$Office)
    e <- plyr::rename(e,c("Income_Paperwork_Back"="Income PPW Back","All_Rehab_Docs_Back"="RAL Back",
                          "Avg_Days_To_Get_Income_Docs"="Avg Days To Get Income Docs",
                          "Avg_Days_To_Get_All_Docs"="Avg Days to Get RAL"))
    f <- subset(e,Month == input$Month)
    f
  })
  
  pfdepday <- reactive ({
    a <- pf
    #c <- a[a$CODE_1 %in% c(input$CCode[1],input$CCode[2],input$CCode[3],input$CCode[4],input$CCode[5],
     #                      input$CCode[6],input$CCode[7],input$CCode[7],input$CCode[8],input$CCode[9],
      #                     input$CCode[10],input$CCode[11],input$CCode[12],input$CCode[13],input$CCode[14],
       #                    input$CCode[15],input$CCode[16],input$CCode[17],input$CCode[18],input$CCode[19],
        #                   input$CCode[20],input$CCode[21],input$CCode[22],input$CCode[23],input$CCode[24],
         #                  input$CCode[25],input$CCode[26],input$CCode[27],input$CCode[28],input$CCode[29],
          #                 input$CCode[30],input$CCode[31],input$CCode[32],input$CCode[33]),]
    d <- a[a$CODE_2 %in% c(input$PCode[1],input$PCode[2],input$PCode[3]),]
    e <- d %>%
      group_by(Department, Office, Date) %>%
      summarize(Contacts = n(),
                Income_Paperwork_Back = sum(!is.na(ED_PROOF_INCOME_DT)),
                All_Rehab_Docs_Back = sum(!is.na(ED_ALL_RHB_RECV_DT)),
                Avg_Days_To_Get_Income_Docs = round(sum((Days_for_Income_Docs))/Income_Paperwork_Back,2),
                Avg_Days_To_Get_All_Docs = round(sum((Days_for_All_RHB_Docs))/All_Rehab_Docs_Back,2)) %>%
      ungroup()
    e <- e %>%
      mutate(Avg_Days_To_Get_All_Docs = ifelse(is.nan(Avg_Days_To_Get_All_Docs)|is.na(Avg_Days_To_Get_All_Docs),
                                               0,
                                               Avg_Days_To_Get_All_Docs),
             Avg_Days_To_Get_Income_Docs = ifelse(is.nan(Avg_Days_To_Get_Income_Docs)|is.na(Avg_Days_To_Get_Income_Docs),
                                                  0,
                                                  Avg_Days_To_Get_Income_Docs))
    e$Office <- as.factor(e$Office)
    e <- plyr::rename(e,c("Income_Paperwork_Back"="Income PPW Back","All_Rehab_Docs_Back"="RAL Back",
                          "Avg_Days_To_Get_Income_Docs"="Avg Days To Get Income Docs",
                          "Avg_Days_To_Get_All_Docs"="Avg Days to Get RAL"))
    f <- subset(e,Date == as.character(input$Day))
    f <- f[,-3]
    f
  })
  
  pfmanday <- reactive ({
    a <- pf
   # c <- a[a$CODE_1 %in% c(input$CCode[1],input$CCode[2],input$CCode[3],input$CCode[4],input$CCode[5],
    #                       input$CCode[6],input$CCode[7],input$CCode[7],input$CCode[8],input$CCode[9],
     #                      input$CCode[10],input$CCode[11],input$CCode[12],input$CCode[13],input$CCode[14],
      #                     input$CCode[15],input$CCode[16],input$CCode[17],input$CCode[18],input$CCode[19],
       #                    input$CCode[20],input$CCode[21],input$CCode[22],input$CCode[23],input$CCode[24],
        #                   input$CCode[25],input$CCode[26],input$CCode[27],input$CCode[28],input$CCode[29],
         #                  input$CCode[30],input$CCode[31],input$CCode[32],input$CCode[33]),]
    d <- a[a$CODE_2 %in% c(input$PCode[1],input$PCode[2],input$PCode[3]),]
    e <- d %>%
      group_by(Manager,Department, Office,Date) %>%
      summarize(Contacts = n(),
                Income_Paperwork_Back = sum(!is.na(ED_PROOF_INCOME_DT)),
                All_Rehab_Docs_Back = sum(!is.na(ED_ALL_RHB_RECV_DT)),
                Avg_Days_To_Get_Income_Docs = round(sum((Days_for_Income_Docs))/Income_Paperwork_Back,2),
                Avg_Days_To_Get_All_Docs = round(sum((Days_for_All_RHB_Docs))/All_Rehab_Docs_Back,2)) %>%
      ungroup()
    e <- e %>%
      mutate(Avg_Days_To_Get_All_Docs = ifelse(is.nan(Avg_Days_To_Get_All_Docs)|is.na(Avg_Days_To_Get_All_Docs),
                                               0,
                                               Avg_Days_To_Get_All_Docs),
             Avg_Days_To_Get_Income_Docs = ifelse(is.nan(Avg_Days_To_Get_Income_Docs)|is.na(Avg_Days_To_Get_Income_Docs),
                                                  0,
                                                  Avg_Days_To_Get_Income_Docs))
    e$Office <- as.factor(e$Office)
    e <- plyr::rename(e,c("Income_Paperwork_Back"="Income PPW Back","All_Rehab_Docs_Back"="RAL Back",
                          "Avg_Days_To_Get_Income_Docs"="Avg Days To Get Income Docs",
                          "Avg_Days_To_Get_All_Docs"="Avg Days to Get RAL"))
    f <- subset(e,Date == as.character(input$Day))
    f <- f[,-4]
    f
  })
  
  pfmanmon <- reactive ({
    a <- pf
    #c <- a[a$CODE_1 %in% c(input$CCode[1],input$CCode[2],input$CCode[3],input$CCode[4],input$CCode[5],
     #                      input$CCode[6],input$CCode[7],input$CCode[7],input$CCode[8],input$CCode[9],
      #                     input$CCode[10],input$CCode[11],input$CCode[12],input$CCode[13],input$CCode[14],
       #                    input$CCode[15],input$CCode[16],input$CCode[17],input$CCode[18],input$CCode[19],
        #                   input$CCode[20],input$CCode[21],input$CCode[22],input$CCode[23],input$CCode[24],
         #                  input$CCode[25],input$CCode[26],input$CCode[27],input$CCode[28],input$CCode[29],
          #                 input$CCode[30],input$CCode[31],input$CCode[32],input$CCode[33]),]
    d <- a[a$CODE_2 %in% c(input$PCode[1],input$PCode[2],input$PCode[3]),]
    e <- d %>%
      group_by(Manager,Department, Office, Month) %>%
      summarize(Contacts = n(),
                Income_Paperwork_Back = sum(!is.na(ED_PROOF_INCOME_DT)),
                All_Rehab_Docs_Back = sum(!is.na(ED_ALL_RHB_RECV_DT)),
                Avg_Days_To_Get_Income_Docs = round(sum((Days_for_Income_Docs))/Income_Paperwork_Back,2),
                Avg_Days_To_Get_All_Docs = round(sum((Days_for_All_RHB_Docs))/All_Rehab_Docs_Back,2)) %>%
      ungroup()
    e <- e %>%
      mutate(Avg_Days_To_Get_All_Docs = ifelse(is.nan(Avg_Days_To_Get_All_Docs)|is.na(Avg_Days_To_Get_All_Docs),
                                               0,
                                               Avg_Days_To_Get_All_Docs),
             Avg_Days_To_Get_Income_Docs = ifelse(is.nan(Avg_Days_To_Get_Income_Docs)|is.na(Avg_Days_To_Get_Income_Docs),
                                                  0,
                                                  Avg_Days_To_Get_Income_Docs))
    
    e$Office <- as.factor(e$Office)
    e <- plyr::rename(e,c("Income_Paperwork_Back"="Income PPW Back","All_Rehab_Docs_Back"="RAL Back",
                          "Avg_Days_To_Get_Income_Docs"="Avg Days To Get Income Docs",
                          "Avg_Days_To_Get_All_Docs"="Avg Days to Get RAL"))
    f <- subset(e,Month == input$Month)
    f
  })
  
  programmonth <- reactive({
      a <- pf
      #c <- a[a$CODE_1 %in% c(input$CCode[1],input$CCode[2],input$CCode[3],input$CCode[4],input$CCode[5],
       #                      input$CCode[6],input$CCode[7],input$CCode[7],input$CCode[8],input$CCode[9],
        #                     input$CCode[10],input$CCode[11],input$CCode[12],input$CCode[13],input$CCode[14],
         #                    input$CCode[15],input$CCode[16],input$CCode[17],input$CCode[18],input$CCode[19],
          #                   input$CCode[20],input$CCode[21],input$CCode[22],input$CCode[23],input$CCode[24],
           #                  input$CCode[25],input$CCode[26],input$CCode[27],input$CCode[28],input$CCode[29],
            #                 input$CCode[30],input$CCode[31],input$CCode[32],input$CCode[33]),]
      d <- a[a$CODE_2 %in% c(input$PCode[1],input$PCode[2],input$PCode[3]),]
      e <- d %>%
      group_by(Collector,Manager,Department, Office, Month) %>%
      summarize(Contacts = n(),
                Income_Paperwork_Back = sum(!is.na(ED_PROOF_INCOME_DT)),
                All_Rehab_Docs_Back = sum(!is.na(ED_ALL_RHB_RECV_DT)),
                Avg_Days_To_Get_Income_Docs = round(sum((Days_for_Income_Docs))/Income_Paperwork_Back,2),
                Avg_Days_To_Get_All_Docs = round(sum((Days_for_All_RHB_Docs))/All_Rehab_Docs_Back,2)) %>%
        ungroup()
    e <- mutate(e,Avg_Days_To_Get_All_Docs = ifelse(is.nan(Avg_Days_To_Get_All_Docs)|is.na(Avg_Days_To_Get_All_Docs),
                                               0,
                                               Avg_Days_To_Get_All_Docs),
             Avg_Days_To_Get_Income_Docs = ifelse(is.nan(Avg_Days_To_Get_Income_Docs)|is.na(Avg_Days_To_Get_Income_Docs),
                                                  0,
                                                  Avg_Days_To_Get_Income_Docs))
    
    e$Office <- as.factor(e$Office)
    e <- plyr::rename(e,c("Income_Paperwork_Back"="Income PPW Back","All_Rehab_Docs_Back"="RAL Back",
                          "Avg_Days_To_Get_Income_Docs"="Avg Days To Get Income Docs",
                          "Avg_Days_To_Get_All_Docs"="Avg Days to Get RAL"))
    f <- subset(e,Month == input$Month)
    f
  })
  
  programday <- reactive({
    a <- pf
   # c <- a[a$CODE_1 %in% c(input$CCode[1],input$CCode[2],input$CCode[3],input$CCode[4],input$CCode[5],
    #                       input$CCode[6],input$CCode[7],input$CCode[7],input$CCode[8],input$CCode[9],
     #                      input$CCode[10],input$CCode[11],input$CCode[12],input$CCode[13],input$CCode[14],
      #                     input$CCode[15],input$CCode[16],input$CCode[17],input$CCode[18],input$CCode[19],
       #                    input$CCode[20],input$CCode[21],input$CCode[22],input$CCode[23],input$CCode[24],
        #                   input$CCode[25],input$CCode[26],input$CCode[27],input$CCode[28],input$CCode[29],
         #                  input$CCode[30],input$CCode[31],input$CCode[32],input$CCode[33]),]
    d <- a[a$CODE_2 %in% c(input$PCode[1],input$PCode[2],input$PCode[3]),]
    e <- d %>%
      group_by(Collector,Manager,Department, Office,Date) %>%
      summarize(Contacts = n(),
                Income_Paperwork_Back = sum(!is.na(ED_PROOF_INCOME_DT)),
                All_Rehab_Docs_Back = sum(!is.na(ED_ALL_RHB_RECV_DT)),
                Avg_Days_To_Get_Income_Docs = round(sum((Days_for_Income_Docs))/Income_Paperwork_Back,2),
                Avg_Days_To_Get_All_Docs = round(sum((Days_for_All_RHB_Docs))/All_Rehab_Docs_Back,2)) %>%
      ungroup()
    e <- mutate(e,Avg_Days_To_Get_All_Docs = ifelse(is.nan(Avg_Days_To_Get_All_Docs)|is.na(Avg_Days_To_Get_All_Docs),
                                                    0,
                                               Avg_Days_To_Get_All_Docs),
             Avg_Days_To_Get_Income_Docs = ifelse(is.nan(Avg_Days_To_Get_Income_Docs)|is.na(Avg_Days_To_Get_Income_Docs),
                                                  0,
                                                  Avg_Days_To_Get_Income_Docs))
    e$Office <- as.factor(e$Office)
    e <- plyr::rename(e,c("Income_Paperwork_Back"="Income PPW Back","All_Rehab_Docs_Back"="RAL Back",
                          "Avg_Days_To_Get_Income_Docs"="Avg Days To Get Income Docs",
                          "Avg_Days_To_Get_All_Docs"="Avg Days to Get RAL"))
    f <- subset(e,Date == as.character(input$Day))
    f <- f[,-5]
    f
  })
  
  
  lo <- reactive({
    paste(input$type,input$time,input$group)
  })
  
  output$text <- renderText({lo()})
  dt <- reactive({
   switch(lo(),
           "Collections Monthly Office"=offmon(),
           "Collections Monthly Department"=depmon(),
           "Collections Monthly Manager"=manmon(),
           "Collections Monthly Collector"=colmon(),
           "Collections Daily Office" = offday(),
           "Collections Daily Department" = depday(),
           "Collections Daily Manager" = manday(),
           "Collections Daily Collector" = colday(),
           "Program Follow Up Monthly Office" = pfoffmon(),
           "Program Follow Up Daily Office" = pfoffday(),
           "Program Follow Up Monthly Department" = pfdepmon(),
           "Program Follow Up Daily Department" = pfdepday(),
           "Program Follow Up Monthly Manager" = pfmanmon(),
           "Program Follow Up Daily Manager" = pfmanday(),
           "Program Follow Up Monthly Collector" = programmonth(),
           "Program Follow Up Daily Collector" = programday()
           
    )
  })
  
  
  output$dt <- DT::renderDataTable({
    table <- datatable(dt(),extensions = 'TableTools', rownames=FALSE,class = 'cell-border stripe',filter="top",
              options = list(
                searching=TRUE,
                autoWidth=TRUE,
                paging=FALSE,
                
                "sDom" = 'T<"clear">lfrtip',
                "oTableTools" = list(
                  "sSwfPath" = "//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls.swf",
                  "aButtons" = list(
                    "copy",
                    "print",
                    list("sExtends" = "collection",
                         "sButtonText" = "Save",
                         "aButtons" = c("csv","xls"))))))
    
    table <- if(input$type == "Collections")
                 {formatPercentage(table,"Conversion Rate",digits=2)}
                 else{table}
    
    table
     
  })
  
  
  
  data <- reactive({
    a <- subset(df,Date == as.character(input$Day))
    a
  })
  
output$download <- downloadHandler(
       filename = GoldeRuleData,
       content = function(file) {
         write.csv(data(), file)
       }
     )
  
}
shinyApp(ui, server)
