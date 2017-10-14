library(scales)
library(shiny)
library(DT)
library(dplyr)
library(stringr)
library(future)
library(dygraphs)
library(xts)



gap <- readRDS('//KNX3IT/AWG Management/KPI Data/Input/res.rds')

setwd("//Knx3fs01/ED_BA_GROUP/Lowhorn/Golden Rule 3/Application")
runApp(host="0.0.0.0",port=5060)
