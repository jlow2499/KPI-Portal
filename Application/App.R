currmonth = format(Sys.Date()-1,"%B %Y")
prevmonth = format(Sys.Date()-30,"%B %Y")

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
    menuItem("KPI Portal",tabName="KPI",icon=icon("arrow-circle-right")),
    menuItem("Contact Report",tabName="contacts",icon=icon("phone"),
      menuSubItem("Contact Report", tabName = "dashboard", icon = icon("table")),
      menuSubItem("Download Tool",tabName="DL",icon=icon("cloud-download"))),
    menuItem("Call Detail Report",tabName="calldetail",icon=icon("phone"),
             menuSubItem("Call Detail report",tabName="calls",icon=icon("table")),
             menuSubItem("Download Tool",tabName="DL2",icon=icon("cloud-download"))),
    menuItem("Rehab Report",tabName="rehab",icon=icon("fighter-jet"),
             menuSubItem("Rehab Report",tabName="RHB",icon=icon("table")),
             menuSubItem("RAL Success by Bucket",tabName="RAL",icon=icon("table")),
             menuSubItem("Download Tool",tabName="DL3",icon=icon("cloud-download"))),
        menuItem("KPI Forms",tabName="form",icon=icon("list-alt"),
             menuSubItem("Printable Form",tabName="form1",icon=icon("print")),
             menuSubItem("Collector Graph",tabName="dygraph",icon=icon("line-chart"))
        )
    
  
)
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName="KPI",h1("Powered By:"),img(src='Capture.png')),
    tabItem(tabName = "dashboard",fluidRow(
            column(width=3,selectInput("type","Type of Call",
                                       choices=c("Collections","Program Follow Up"))),
            column(width=3,selectInput("time","Timeframe",
                                       choices=c("Monthly","Daily"))),
            column(width=3,uiOutput("darn")),
                  
            column(width=3,selectInput("group","Group",
                                       choices=c("Office","Department","Manager","Collector")))),
  
            DT::dataTableOutput("dt")
    ),
    tabItem(tabName="DL",
            column(width=4),column(width=4,
                                   box(width=8,height=150,dateInput("days",
                                                 label="Date",
                                                 value=(Sys.Date()-x),
                                                 min="2015-12-31",
                                                 format="m/d/yyyy"),
                                       downloadButton('downloadData', 'Download Daily Data')))),
    
    
    tabItem(tabName = "calls",fluidRow(
           
      column(width=4,selectInput("time3","Timeframe",
                                 choices=c("Monthly","Daily"))),
      column(width=4,uiOutput("darn3")),
      
      column(width=4,selectInput("group3","Group",
                                 choices=c("Office","Department","Manager","Collector")))),
      #textOutput("text3")
      DT::dataTableOutput("dt3")
    ),
    
    
    
    
    tabItem(tabName="DL2",
            column(width=4),column(width=4,
                                   box(width=8,height=150,dateInput("days2",
                                                                    label="Date",
                                                                    value=(Sys.Date()-x),
                                                                    min="2015-12-31",
                                                                    format="m/d/yyyy"),
                                       downloadButton('downloadData2', 'Download Data')))),
    
    
    
    tabItem(tabName = "RHB",fluidRow(
      
      column(width=6,selectInput("Month4","Month",
                                 choices=levels(docs$SetupMonth),
                                 selected=currmonth)),
      
      column(width=6,selectInput("group4","Group",
                                 choices=c("Office","Department","Manager","Collector")))),
     # textOutput("text4"),
      DT::dataTableOutput("dt4")
    ),
    
    tabItem(tabName = "RAL",
            fluidRow(column(width=3,
                            selectInput("raltype","Report View",choices=c("By Type","All Rehabs"))),
                     column(width=3,
                            selectInput('dep',"Department",choices=c("Collections","AWG"))),
                     column(width=3,selectInput("progtype","Rehab Type",
                                                choices=c("15 %","FIS"),
                                                selected="15 %")),
                     column(width=3,uiOutput("awgoff"))),
            #textOutput("thetext")
            DT::dataTableOutput("ralsuc")
            
            
            ),
    
    
   # uiOutput("typeselect"),
    
    tabItem(tabName="DL3",
            column(width=4),column(width=4,
                                   box(width=8,height=150,selectInput("Month6","Month",
                                                                      choices=levels(Tracker$SetupMonth),
                                                                      selected=prevmonth),
                                       downloadButton('downloadData3', 'Download Data')))),
    
  
    tabItem(tabName="form1",
            fluidRow(column(width=4),column(width=4,            
                     h1("Collector KPI Form")
                  )),
            fluidRow(column(width=1),column(width=3,
                            fluidRow(selectInput("time2","Timeframe",
                                                 choices=c("Monthly","Daily"))),
                            fluidRow(uiOutput("darn2"))
                            ),
                     column(width=1),
                     column(width=4,
                     selectInput("AR",h3("Collector"),
                                 choices=levels(ARMASTER$A.R),
                                 selected="PLUNK, STEVE")),
                     column(width=3,
                            selectInput("Month5","Rehab Tracker Month",
                                        choices=levels(docs$SetupMonth),
                                        selected="January 2016")
                            
                            )),
            
            column(width=4,
                   fluidRow(column(width=12,infoBox(width=12,title=h4("Contacts"),value=textOutput("contact"),icon=icon("phone")))),
                   fluidRow(column(width=12,infoBox(width=12,title=h4("Rehabs Verified"),value=textOutput("rhv"),icon=icon("phone")))),
                   fluidRow(column(width=12,infoBox(width=12,title=h4("Contact Resolution Rate"),value=textOutput("cc"),icon=icon("percent"))))
            ),
            column(width=4,
                   fluidRow(column(width=12,infoBox(width=12,title=h4("Phone Calls"),value=textOutput("phonecall"),icon=icon("phone")))),
                   fluidRow(column(width=12,infoBox(width=12,title=h4("Accounts Worked"),value=textOutput("AW"),icon=icon("edit")))),
                   fluidRow(column(width=12,infoBox(width=12,title=h4("Messages Left"),value=textOutput("ML"),icon=icon("edit")))),
                   fluidRow(column(width=12,infoBox(width=12,title=h4("Talk Time"),icon=icon("clock-o"))))
                   #fluidRow(column(width=12,selectInput("DATUM","Select KPI",choices=c('Calls',"Accounts Worked","Messages Left"))))
                   ),
            column(width=4,
                   fluidRow(column(width=12,infoBox(width=12,title=h4("Setups"),value=textOutput("setup"),icon=icon("paper-plane")))),
                   fluidRow(column(width=12,infoBox(width=12,title=h4("Income Document %"),value=textOutput("ic"),icon=icon("paper-plane")))),
                   fluidRow(column(width=12,infoBox(width=12,title=h4("RAL Returned %"),value=textOutput("ral"),icon=icon("paper-plane")))),
                   fluidRow(column(width=12,infoBox(width=12,title=h4("Funding Rate %"),value=textOutput("fund"),icon=icon("percent"))))
            )
           # fluidRow(dygraphOutput("DYGRAPH"))
            
              
            
            ),
   tabItem(tabName="dygraph",
           fluidRow(column(width=6,selectInput("COL",h3("Collector"),
                       choices=levels(ARMASTER$A.R),
                       selected="PLUNK, STEVE")),
           column(width=6,selectInput("DATUM",h3("Select KPI"),choices=c('Calls',"Accounts Worked","Messages Left")))),
           #DT::dataTableOutput("dytable")
           dygraphOutput("DYGRAPH")
          )
    )
    
  )


ui <- dashboardPage(
  dashboardHeader(title = "KPI Portal",
                  dropdownMenu(type="notifications",
                               notificationItem(text=textOutput("counter"),icon=icon("users"))
                  )),
  sidebar,
  body
)

server <- function(input, output) {
  
  



  output$darn <- renderUI({
  
    if(input$time == "Monthly"){
    
    selectInput("Month","Month",
                              choices=c("September 2016","October 2016","November 2016"),
                             selected=currmonth)}else{
                               dateInput("Day",
                                         label="Date",
                                         value=(Sys.Date()-x),
                                         min="2015-12-31",
                                         format="m/d/yyyy")
                               
                             }
})

output$darn3 <- renderUI({
  
  if(input$time3 == "Monthly"){
    
    selectInput("Month3","Month",
                choices=c("September 2016","October 2016","November 2016"),
                selected=currmonth)}else{
                  dateInput("Day3",
                            label="Date",
                            value=(Sys.Date()-x),
                            min="2015-12-31",
                            format="m/d/yyyy")
                  
                }
})







output$darn2 <- renderUI({
  
  if(input$time2 == "Monthly"){
    
    selectInput(inputId="Month2","Month",
                choices=c("September 2016","October 2016","November 2016"),
                selected=currmonth)}else{
                  dateInput("Day2",
                            label="Date",
                            value=(Sys.Date()-x),
                            min="2015-12-31",
                            format="m/d/yyyy")
                  
                }
})


  
data <- reactive({
  a <- subset(df,Date == as.character(input$days))
  a <- a[,-which(names(a)%in%c("Tracker","Credit","ARNUM"))]
  a
})

output$downloadData <- downloadHandler(
  filename = function() { 
    paste("Daily_Contact_Data", '.csv', sep='') 
  },
  content = function(file) {
    write.csv(data(), file)
  }
)

acct <- reactive({
  a <- activity[activity$Date == as.character(input$days2),]
  a
    
})

output$downloadData2 <- downloadHandler(
  filename = function() { 
    paste("Daily_Call_Data", '.csv', sep='') 
  },
  content = function(file) {
    write.csv(acct(), file)
  }
)


Track <- reactive({
  a <- Tracker[Tracker$SetupMonth == input$Month6,]
  a
})


output$downloadData3 <- downloadHandler(
  filename = function() { 
    paste("Daily_Data", '.csv', sep='') 
  },
  content = function(file) {
    write.csv(Track(), file)
  }
)

pcmon <- reactive({
  a <- activity[activity$Month == input$Month2,]
  b <- a %>%
    group_by(Collector,Month) %>%
    summarize(Calls = sum(Notated_Calls))
  b<- b[complete.cases(b),]
  c <- b[b$Collector ==input$AR,]
  d <- c$Calls
  d
  
})

pcday <- reactive({
  a <- activity[activity$Date == as.character(input$Day2),]
  b <- a %>%
    group_by(Collector,Date) %>%
    summarize(Calls = sum(Notated_Calls))
  b<- b[complete.cases(b),]
  c <- b[b$Collector ==input$AR,]
  d <- c$Calls
  d
  
})

output$phonecall <- renderText({
  if(input$time2=="Monthly"){
    pcmon()}else{
      pcday()
    }
})
  
  

conformmon <- reactive ({
  a <- subset(df,Month == input$Month2)
  e <- a %>%
    group_by(Collector, Month) %>%
    summarize(Contacts = n())
  e$Collector <- as.factor(e$Collector)
 f <- e[e$Collector == input$AR,]
  g <- f$Contacts
 g
})
  
conformday <- reactive ({
  a <- subset(df,Date == as.character(input$Day2))
  e <- a %>%
    group_by(Collector, Date) %>%
    summarize(Contacts = n())
  e$Collector <- as.factor(e$Collector)
  f <- e[e$Collector == input$AR,]
  g <- f$Contacts
  g
})

output$contact <- renderText({
  if(input$time2=="Monthly"){
    conformmon()}else{
      conformday()
    }
  })
  

rhvmon <- reactive ({
  a <- subset(df,Month == input$Month2)
  e <- a %>%
    group_by(Collector, Month) %>%
    summarize(RH_Conditions_Verified = sum(Rehab.Condition=="Yes"))
  e$Collector <- as.factor(e$Collector)
  f <- e[e$Collector == input$AR,]
  g <- f$RH_Conditions_Verified
  g
})

rhvday <- reactive ({
  a <- subset(df,Date == as.character(input$Day2))
  e <- a %>%
    group_by(Collector, Date) %>%
    summarize(RH_Conditions_Verified = sum(Rehab.Condition=="Yes"))
  e$Collector <- as.factor(e$Collector)
  f <- e[e$Collector == input$AR,]
  g <- f$RH_Conditions_Verified
  g
})

output$rhv <- renderText({
  if(input$time2=="Monthly"){
    rhvmon()}else{
      rhvday()
    }
})

ccmon <- reactive ({
  a <- subset(df,Month == input$Month2)
  e <- a %>%
    group_by(Collector, Month) %>%
    summarize(Contacts = n(),
              Closed_Calls = sum(CODE_3 %in% c("PRM","CMP")),
              Conversion_Rate = Closed_Calls/Contacts,
              RH_Conditions_Verified = sum(Rehab.Condition=="Yes"))
  e$Collector <- as.factor(e$Collector)
  f <- e[e$Collector == input$AR,]
  g <- f$Conversion_Rate
  g
})

ccday <- reactive ({
  a <- subset(df,Date == as.character(input$Day2))
  e <- a %>%
    group_by(Collector, Date) %>%
    summarize(Contacts = n(),
              Closed_Calls = sum(CODE_3 %in% c("PRM","CMP")),
              Conversion_Rate = Closed_Calls/Contacts,
              RH_Conditions_Verified = sum(Rehab.Condition=="Yes"))
  e$Collector <- as.factor(e$Collector)
  f <- e[e$Collector == input$AR,]
  g <- f$Conversion_Rate
  g
})

output$cc <- renderText({
  if(input$time2=="Monthly"){
    percent(ccmon())}else{
      percent(ccday())
    }
})




awmon <- reactive({
  a <- activity[activity$Month == input$Month2,]
  b <- a %>%
    group_by(Collector,Month) %>%
    summarize(Calls = sum(Accounts_Worked))
  b<- b[complete.cases(b),]
  c <- b[b$Collector ==input$AR,]
  d <- c$Calls
  d
  
})

awday <- reactive({
  a <- activity[activity$Date == as.character(input$Day2),]
  b <- a %>%
    group_by(Collector,Date) %>%
    summarize(Calls = sum(Accounts_Worked))
  b<- b[complete.cases(b),]
  c <- b[b$Collector ==input$AR,]
  d <- c$Calls
  d
  
})

output$AW <- renderText({
  if(input$time2=="Monthly"){
    awmon()}else{
      awday()
    }
})

mlmon <- reactive({
  a <- activity[activity$Month == input$Month2,]
  b <- a %>%
    group_by(Collector,Month) %>%
    summarize(Calls = sum(Messages_Left))
  b<- b[complete.cases(b),]
  c <- b[b$Collector ==input$AR,]
  d <- c$Calls
  d
  
})

mlday <- reactive({
  a <- activity[activity$Date == as.character(input$Day2),]
  b <- a %>%
    group_by(Collector,Date) %>%
    summarize(Calls = sum(Messages_Left))
  b<- b[complete.cases(b),]
  c <- b[b$Collector == input$AR,]
  d <- c$Calls
  d
  
})

output$ML <- renderText({
  if(input$time2=="Monthly"){
    mlmon()}else{
      mlday()
    }
})



rhbsetup <- reactive ({
  a <- subset(docs,SetupMonth == input$Month5)
  b <- a[a$Collector == input$AR,]
  g <- b$Setups
  g
})

output$setup <- renderText({
  rhbsetup()    
})

rhbicdoc <- reactive ({
  a <- subset(docs,SetupMonth == input$Month5)
  b <- a[a$Collector == input$AR,]
  g <- b$IC_Percent
  g
})

output$ic <- renderText({
  percent(rhbicdoc())  
})

rhbral <- reactive ({
  a <- subset(docs,SetupMonth == input$Month5)
  b <- a[a$Collector == input$AR,]
  g <- b$RAL_Percent
  g
})

output$ral <- renderText({
  percent(rhbral())  
})

rhbfund <- reactive ({
  a <- subset(docs,SetupMonth == input$Month5)
  b <- a[a$Collector == input$AR,]
  g <- b$Funded_Percent
  g
})

output$fund <- renderText({
  percent(rhbfund())  
})









  
  output$codes <- renderDataTable({
    datatable(CODE,extensions = 'TableTools', rownames=FALSE,class = 'cell-border stripe',filter="top",
              options = list(
                searching=TRUE,
                autoWidth=TRUE
              ))
  })
  
  manday <- reactive ({
    a <- subset(df,Date == as.character(input$Day))
    e <- a %>%
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
    e <- a %>%
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
    e <- a %>%
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
    e <- a %>%
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
    e <- a %>%
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
    e <- a %>%
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
    e <- plyr::rename(e,c("RH_Conditions_Verified"="RH Conditions Verified","Closed_Calls"="Closed Calls","Rehabs_On_Tracker"="Rehabs On Tracker","Conversion_Rate"="Conversion Rate"))
    e
  })
  
  offday <- reactive ({
    a <- subset(df,Date == as.character(input$Day))
    e <- a %>%
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
    e <- a %>%
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
    e <- plyr::rename(e,c("RH_Conditions_Verified"="RH Conditions Verified","Closed_Calls"="Closed Calls","Rehabs_On_Tracker"="Rehabs On Tracker","Conversion_Rate"="Conversion Rate"))
    e
  })
  
  pfoffday <- reactive ({
    a <- pf
    e <- a %>%
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
    e <- a %>%
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
    e <- a %>%
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
    e <- a %>%
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
    e <- a %>%
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
    e <- a %>%
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
      e <- a %>%
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
    e <- a %>%
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


calloffmon <- reactive({

  a <- activity[activity$Month == input$Month3,]
  
  b <- a %>%
    group_by(Office,Month) %>%
    summarize(Accounts_Worked = sum(Accounts_Worked),
              Notated_Calls = sum(Notated_Calls),
              Outbound_Calls=sum(Outbound_Calls),
              Inbound_Calls = sum(Inbound_Calls),
              Messages_Left = sum(Messages_Left),
              POE_Attempts = sum(POE_Attempts))
  b <- plyr::rename(b, c("Accounts_Worked"="Accounts Worked",
                         "Notated_Calls"="Notated Calls",
                         "Outbound_Calls"="Outbound Calls",
                         "Inbound_Calls"="Inbound Calls",
                         "Messages_Left"="Messages Left",
                         "POE_Attempts"="Top 5 POE Attempts"))
  b
    
})

calloffday <- reactive({

  a <- activity[activity$Date == as.character(input$Day3),]
  
  
  b <- a %>%
    group_by(Office,Date) %>%
    summarize(Accounts_Worked = sum(Accounts_Worked),
              Notated_Calls = sum(Notated_Calls),
              Outbound_Calls=sum(Outbound_Calls),
              Inbound_Calls = sum(Inbound_Calls),
              Messages_Left = sum(Messages_Left),
              POE_Attempts = sum(POE_Attempts))
  b <- plyr::rename(b, c("Accounts_Worked"="Accounts Worked",
                         "Notated_Calls"="Notated Calls",
                         "Outbound_Calls"="Outbound Calls",
                         "Inbound_Calls"="Inbound Calls",
                         "Messages_Left"="Messages Left",
                         "POE_Attempts"="Top 5 POE Attempts"))
  b <- b[,!names(b)%in%"Date"]
  b
  
})



calldeptmon <- reactive({

  a <- activity[activity$Month == input$Month3,]
  
  
  b <- a %>%
    group_by(Department,Office,Month) %>%
    summarize(Accounts_Worked = sum(Accounts_Worked),
              Notated_Calls = sum(Notated_Calls),
              Outbound_Calls=sum(Outbound_Calls),
              Inbound_Calls = sum(Inbound_Calls),
              Messages_Left = sum(Messages_Left),
              POE_Attempts = sum(POE_Attempts))
  b <- plyr::rename(b, c("Accounts_Worked"="Accounts Worked",
                         "Notated_Calls"="Notated Calls",
                         "Outbound_Calls"="Outbound Calls",
                         "Inbound_Calls"="Inbound Calls",
                         "Messages_Left"="Messages Left",
                         "POE_Attempts"="Top 5 POE Attempts"))
  b
  
})

calldeptday <- reactive({
 
  a <- activity[activity$Date == as.character(input$Day3),]
  
  
  b <- a %>%
    group_by(Department,Office,Date) %>%
    summarize(Accounts_Worked = sum(Accounts_Worked),
              Notated_Calls = sum(Notated_Calls),
              Outbound_Calls=sum(Outbound_Calls),
              Inbound_Calls = sum(Inbound_Calls),
              Messages_Left = sum(Messages_Left),
              POE_Attempts = sum(POE_Attempts))
  b <- plyr::rename(b, c("Accounts_Worked"="Accounts Worked",
                         "Notated_Calls"="Notated Calls",
                         "Outbound_Calls"="Outbound Calls",
                         "Inbound_Calls"="Inbound Calls",
                         "Messages_Left"="Messages Left",
                         "POE_Attempts"="Top 5 POE Attempts"))
  b <- b[,!names(b)%in%"Date"]
  b
  
})

callmgrmon <- reactive({

  a <- activity[activity$Month == input$Month3,]
  
  
  b <- a %>%
    group_by(Manager,Department,Office,Month) %>%
    summarize(Accounts_Worked = sum(Accounts_Worked),
              Notated_Calls = sum(Notated_Calls),
              Outbound_Calls=sum(Outbound_Calls),
              Inbound_Calls = sum(Inbound_Calls),
              Messages_Left = sum(Messages_Left),
              POE_Attempts = sum(POE_Attempts))
  b <- plyr::rename(b, c("Accounts_Worked"="Accounts Worked",
                         "Notated_Calls"="Notated Calls",
                         "Outbound_Calls"="Outbound Calls",
                         "Inbound_Calls"="Inbound Calls",
                         "Messages_Left"="Messages Left",
                         "POE_Attempts"="Top 5 POE Attempts"))
  b
  
})

callmgrday <- reactive({

  a <- activity[activity$Date == as.character(input$Day3),]
  
  
  b <- a %>%
    group_by(Manager,Department,Office,Date) %>%
    summarize(Accounts_Worked = sum(Accounts_Worked),
              Notated_Calls = sum(Notated_Calls),
              Outbound_Calls=sum(Outbound_Calls),
              Inbound_Calls = sum(Inbound_Calls),
              Messages_Left = sum(Messages_Left),
              POE_Attempts = sum(POE_Attempts))
  b <- plyr::rename(b, c("Accounts_Worked"="Accounts Worked",
                         "Notated_Calls"="Notated Calls",
                         "Outbound_Calls"="Outbound Calls",
                         "Inbound_Calls"="Inbound Calls",
                         "Messages_Left"="Messages Left",
                         "POE_Attempts"="Top 5 POE Attempts"))
  b <- b[,!names(b)%in%"Date"]
  b
  
})

callcolmon <- reactive({

  a <- activity[activity$Month == input$Month3,]
  
  
  b <- a %>%
    group_by(Collector,Manager,Department,Office,Month) %>%
    summarize(Accounts_Worked = sum(Accounts_Worked),
              Notated_Calls = sum(Notated_Calls),
              Outbound_Calls=sum(Outbound_Calls),
              Inbound_Calls = sum(Inbound_Calls),
              Messages_Left = sum(Messages_Left),
              POE_Attempts = sum(POE_Attempts))
  b <- plyr::rename(b, c("Accounts_Worked"="Accounts Worked",
                         "Notated_Calls"="Notated Calls",
                         "Outbound_Calls"="Outbound Calls",
                         "Inbound_Calls"="Inbound Calls",
                         "Messages_Left"="Messages Left",
                         "POE_Attempts"="Top 5 POE Attempts"))
  b
  
})

callcolday <- reactive({

  a <- activity[activity$Date == as.character(input$Day3),]
  
  
  b <- a %>%
    group_by(Collector,Manager,Department,Office,Date) %>%
    summarize(Accounts_Worked = sum(Accounts_Worked),
              Notated_Calls = sum(Notated_Calls),
              Outbound_Calls=sum(Outbound_Calls),
              Inbound_Calls = sum(Inbound_Calls),
              Messages_Left = sum(Messages_Left),
              POE_Attempts = sum(POE_Attempts))
  b <- plyr::rename(b, c("Accounts_Worked"="Accounts Worked",
                         "Notated_Calls"="Notated Calls",
                         "Outbound_Calls"="Outbound Calls",
                         "Inbound_Calls"="Inbound Calls",
                         "Messages_Left"="Messages Left",
                         "POE_Attempts"="Top 5 POE Attempts"))
  b <- b[,!names(b)%in%"Date"]
  b
  
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

lo3 <- reactive({
  paste(input$time3,input$group3)
})

output$text3 <- renderText({lo3()})

dt3 <- reactive({
  switch(lo3(),
         "Monthly Office" = calloffmon(),
         "Monthly Department"=calldeptmon(),
         "Monthly Manager"=callmgrmon(),
         "Monthly Collector"=callcolmon(),
         "Daily Office" = calloffday(),
         "Daily Department" = calldeptday(),
         "Daily Manager" = callmgrday(),
         "Daily Collector" = callcolday()
         
         
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

output$dt3 <- DT::renderDataTable({
  table <- datatable(dt3(),extensions = 'TableTools', rownames=FALSE,class = 'cell-border stripe',filter="top",
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
  
  table
  
})

rhboffmon <- reactive({
  
  a <- docs[docs$SetupMonth == input$Month4,]
  b <- a %>%
    group_by(SetupMonth, Office) %>%
    summarize(
      Setups = sum(Setups),
      Income_Docs = sum(Income_Docs),
      IC_Percent = Income_Docs/Setups,
      RAL = sum(RAL),
      RAL_Percent = RAL/Setups,
      Fallout = sum(Fallout),
      Funded = sum(Funded),
      Funded_Percent = Funded/Setups
    )
  b <- plyr::rename(b,c("SetupMonth"="Setup Month",
                  "Income_Docs"="Income Docs",
                  "IC_Percent"="Income Docs %",
                  "RAL"="RALs Returned",
                  "RAL_Percent"="RAL % Returned",
                  "Funded"="Funded Rehabs",
                  "Funded_Percent"="Funded %"))
  b <- b[!b$"Income Docs" %in% "",]
  b
  
})

rhbdeptmon <- reactive({
  
  a <- docs[docs$SetupMonth == input$Month4,]
  b <- a %>%
    group_by(SetupMonth, Office, Department) %>%
    summarize(
      Setups = sum(Setups),
      Income_Docs = sum(Income_Docs),
      IC_Percent = Income_Docs/Setups,
      RAL = sum(RAL),
      RAL_Percent = RAL/Setups,
      Fallout = sum(Fallout),
      Funded = sum(Funded),
      Funded_Percent = Funded/Setups
    )
  b <- plyr::rename(b,c("SetupMonth"="Setup Month",
                  "Income_Docs"="Income Docs",
                  "IC_Percent"="Income Docs %",
                  "RAL"="RALs Returned",
                  "RAL_Percent"="RAL % Returned",
                  "Funded"="Funded Rehabs",
                  "Funded_Percent"="Funded %"))
  b <- b[!b$"Income Docs" %in% "",]
  b
  
})

rhbmanmon <- reactive({
  
  a <- docs[docs$SetupMonth == input$Month4,]
  b <- a %>%
    group_by(SetupMonth, Office, Department, Manager) %>%
    summarize(
      Setups = sum(Setups),
      Income_Docs = sum(Income_Docs),
      IC_Percent = Income_Docs/Setups,
      RAL = sum(RAL),
      RAL_Percent = RAL/Setups,
      Fallout = sum(Fallout),
      Funded = sum(Funded),
      Funded_Percent = Funded/Setups
    )
  b <- plyr::rename(b,c("SetupMonth"="Setup Month",
                         "Income_Docs"="Income Docs",
                         "IC_Percent"="Income Docs %",
                         "RAL"="RALs Returned",
                         "RAL_Percent"="RAL % Returned",
                         "Funded"="Funded Rehabs",
                         "Funded_Percent"="Funded %"))
  b <- b[!b$"Income Docs" %in% "",]
  b
  
})

rhbcolmon <- reactive({
  
  a <- docs[docs$SetupMonth == input$Month4,]
  
  a <- plyr::rename(a,c("SetupMonth"="Setup Month",
                  "Income_Docs"="Income Docs",
                  "IC_Percent"="Income Docs %",
                  "RAL"="RALs Returned",
                  "RAL_Percent"="RAL % Returned",
                  "Funded"="Funded Rehabs",
                  "Funded_Percent"="Funded %"))
  a <- a[!a$"Income Docs" %in% "",]
  a
  
})

lo4 <- reactive({
  paste(input$group4)
})

output$text4 <- renderText({lo4()})

dt4 <- reactive({
  switch(lo4(),
         "Office" = rhboffmon(),
         "Department"=rhbdeptmon(),
         "Manager"=rhbmanmon(),
         "Collector"=rhbcolmon()
         
  )
})

output$dt4 <- DT::renderDataTable({
  table <- datatable(dt4(),extensions = 'TableTools', rownames=FALSE,class = 'cell-border stripe',filter="top",
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
  
  table <- formatPercentage(table,"Income Docs %",digits=2)
  
  table <- formatPercentage(table,"RAL % Returned",digits=2)
  table <- formatPercentage(table,"Funded %",digits=2)
  
})





output$awgoff <- renderUI({
  if(input$dep == "Collections"){
    selectInput("office_day","Office",choices=c("Knoxville","Columbus","Columbus 2","Westlake","Schuerger"),
                selected="Knoxville")
  }
    
})


type1 <- reactive({
  a <- all[all$Office == input$office_day,]
  a <- plyr::rename(a,c("Income_Docs"="Income Docs Returned",
                        "Income_Doc_P"="Income Docs Returned %",
                        "RAL"="RALs Returned",
                        "RAL_P"="RAL Percent",
                        "Income_Doc_Change"="Income Document Change",
                        "RAL_DOC_Change"="RAL Document Change"))
  
  a
})
type2 <- reactive({
  a <- type[type$Office == input$office_day,]
  b <- a[a$ED_RHB_TYPE == input$progtype,]
  b <- plyr::rename(b,c("Income_Docs"="Income Docs Returned",
                        "Income_Doc_P"="Income Docs Returned %",
                        "RAL"="RALs Returned",
                        "RAL_P"="RAL Percent",
                        "Income_Doc_Change"="Income Document Change",
                        "RAL_DOC_Change"="RAL Document Change",
                        "ED_RHB_TYPE"="Rehab Type"))
  b
})
type3 <- reactive({
  a <- typeawg[typeawg$ED_RHB_TYPE==input$progtype,]
  a <- plyr::rename(a,c("Income_Docs"="Income Docs Returned",
                        "Income_Doc_P"="Income Docs Returned %",
                        "RAL"="RALs Returned",
                        "RAL_P"="RAL Percent",
                        "Income_Doc_Change"="Income Document Change",
                        "RAL_DOC_Change"="RAL Document Change",
                        "ED_RHB_TYPE"="Rehab Type",
                        "Rehabs.x"="Rehabs"))
  a
})
type4 <- reactive({
  allawg <- plyr::rename(allawg,c("Income_Docs"="Income Docs Returned",
                        "Income_Doc_P"="Income Docs Returned %",
                        "RAL"="RALs Returned",
                        "RAL_P"="RAL Percent",
                        "Income_Doc_Change"="Income Document Change",
                        "RAL_DOC_Change"="RAL Document Change"))
  allawg
})

lotext <- reactive({
  paste(input$raltype,input$dep)
})
output$thetext <- renderText({lotext()})

ralsuc <- reactive({
  switch(lotext(),
         "By Type Collections" = type2(),#
         "By Type AWG"=type3(),#
         "All Rehabs Collections"=type1(),
         "All Rehabs AWG"=type4()  
  )
})

output$ralsuc <- DT::renderDataTable({
    table <- datatable(ralsuc(),extensions = 'TableTools', rownames=FALSE,class = 'cell-border stripe',filter="top",
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
    
    table <- formatPercentage(table,"Income Docs Returned %",digits=2)
    table <- formatPercentage(table,"RAL Percent",digits=2)
    table <- formatPercentage(table,"Income Document Change",digits=2)
    table <- formatPercentage(table,"RAL Document Change",digits=2)
    
    
  table
  
})

output$counter <- 
  renderText({
    if (!file.exists("counter.Rdata")) 
      counter <- -1
    else
      load(file="counter.Rdata")
    counter  <- counter + 1
    save(counter, file="counter.Rdata")     
    paste("You are visitor# ", counter)
  })


heintext <- reactive({input$DATUM})


heiny <- reactive({
  switch(heintext(),
  "Calls"=c("Date","Calls","Average.Calls"),
  "Accounts Worked"=c("Date","Accounts.Worked","Average.Accounts.Worked"),
  "Messages Left"=c("Date","Messages","Average.Messages")
)
  
})




graph <- reactive({
  a <- hein[hein$Collector %in% input$COL,]
  a <- a[,names(a)%in% heiny()]
  a <- as.xts(a,order.by=a$Date)
  a
    
})

output$DYGRAPH <- renderDygraph({
  dygraph(graph(),main="KPI to Department Average") %>%
    dyRangeSelector()
    
})




}
shinyApp(ui, server)
