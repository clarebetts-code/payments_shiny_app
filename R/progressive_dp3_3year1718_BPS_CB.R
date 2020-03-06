library(shiny)
library(htmlwidgets)
library(survey)
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(scales)
library(lazyeval)
library(data.table)
library(doBy)
library(rockchalk)
library(purrr)
#library(plotly)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This version has been adapted to use 2017/18 FBS data
# But also to use the 2018 BPS data that has come in
# a different format to before.

# updated by Clare Betts January 2020

# N.B.
# Need to check why some farms increase their FBI after payment reduction - only 2
# Need to modify payment reduction code so not double counting any farms which lie on the boundry
#   i.e. between(dp_3yr, input$Level_1+1, input$Level_2) or similar

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source("K:\\TASPrototype\\Statistical issues\\R\\FBS_functions.R")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function to calculate 3 year averages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#####
av_3year <- function(dat, item, years = 2015:2017){ 
  
  years %>%
    substr(start = 3, stop = 4) %>%
    paste0("X.", ., item) %>%
    subset(dat, select = .) %>%
    rowMeans()
}
#####

setwd("K:\\TASPrototype\\FBSmastercopy\\FBS_ADHOC_DATA_REQUESTS\\EU Exit\\Payments reduction shiny app")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#    RPA Payment data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#####
#rpapaydata <- readRDS("Data\\28012020 BPS 2018.Rds")
rpapaydata <- read.csv("Data\\elm_analysis_bps_data_09jul19.csv")

# select just the columns we need, and rename the payment column
rpapaydata %>%
  dplyr::select(FRN,
                FORM_STATUS,
                Total_Payment_pounds = NET_PAY_GBP) %>%
  # add a column for the losses, set to zeros to start
  dplyr::mutate(loss = 0)  %>%
  # only those with a claim over £0
  dplyr::filter(Total_Payment_pounds > 0) -> RPApay_interim
#####


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get FBS data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#####
fbs_3yr <- readRDS("Data\\fbs_england_3yr_15_17.Rds")



# set new factors to combine some farm types and sizes

fbs_3yr %>%
  dplyr::mutate(
    # group some farm types to improve sample size
    type_red = forcats::fct_recode(type,
                                   `Pigs & Poultry` =  "Pigs",
                                   `Pigs & Poultry`="Poultry",
                                   `Cereals & General cropping`="Cereals",
                                   `Cereals & General cropping`="General cropping"),
    # merge spare time and part time
    farm_size = forcats::fct_recode(slrgroup,
                                    `Spare/Part-time` =  "Spare-time",
                                    `Spare/Part-time` =  "Part-time"),
    # put ages into age bands
    age_band = as.factor(cut(X.16age_of_farmer, 
                             breaks = c(-10000000, 40, 50, 60, 70, Inf), 
                             labels = c("Under 40", "40-49", "50-59", "60-69", "70 and over"), 
                             levels =c(1,2,3,4,5),
                             right = FALSE,
                             ordered_result = TRUE))) %>%
  # re-order factors
  dplyr::mutate(farm_size = forcats::fct_relevel(farm_size, "Spare/Part-time",
                                                 "Small",
                                                 "Medium",
                                                 "Large",
                                                 "Very large"),
         type_red = forcats::fct_relevel(type_red, "Cereals & General cropping",
                                         "Dairy",
                                         "LFA Grazing Livestock",
                                         "Lowland Grazing Livestock",
                                         "Pigs & Poultry",
                                         "Mixed",
                                         "Horticulture"),
         tenancy = forcats::fct_relevel(tenancy,
                                        "Tenanted",
                                        "Mixed - mainly tenanted",
                                        "Mixed - mainly owner occupied",
                                        "Owner occupied"),
         gor = forcats::fct_relevel(gor,
                                    "North East",
                                    "North West",
                                    "Yorkshire & Humber",
                                    "East Midlands",
                                    "West Midlands",
                                    "East of England",
                                    "South East",
                                    "South West"),
         lfa = forcats::fct_relevel(lfa, "Non-LFA",
                                    "Mainly DA",
                                    "Mainly SDA")) -> fbs_3yr

#    Calculate the 3 year averages
fbs_3yr %>%
  dplyr::mutate(fbi_3yr = av_3year(.,"farm_business_income"),
                agout_3yr = av_3year(.,"output_from_agriculture"),
                aginc_3yr = av_3year(.,"agriculture_input_costs"),
                dp_3yr = av_3year(.,"basic_payment_scheme"),
                dp_costs_3yr = av_3year(.,"BPS_costs")) -> fbs_3yr

#Remove SPS from income
fbs_3yr %>%
  dplyr::mutate(
    #SPS cost centre
    dp_cc_3yr = dp_3yr - dp_costs_3yr,
    #FBI minus SPS cost centre
    fbi_min_dp_cc_3yr = fbi_3yr - dp_cc_3yr
  ) -> fbs_3yr


#####

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The ui section sets up how the page will look
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#####
ui <- navbarPage("Progressive approach",
                 
               tabPanel("Results",
                          # sets up a sidebar with input boxes that can be changed
                          sidebarLayout( 
                           sidebarPanel(
                             # the div parts set up 2 adjacent columns each with width 140 px
                             
                             div(style="display: inline-block;vertical-align:top; width: 140px;",
                                  numericInput("Level_1", 
                                           "Upto Level 1:", 
                                           value=25000,step = 1000),
                              numericInput("Level_2",
                                           "Upto Level 2:",
                                           value=30000,step = 1000),
                              numericInput("Level_3",
                                           "Upto Level 3:",
                                           value=40000,step = 1000),
                              numericInput("Level_4",
                                           "Upto Level 4:",
                                           value=50000,step = 5000),
                              numericInput("Level_5",
                                           "Upto Level 5:",
                                           value=75000,step = 5000),
                              numericInput("Level_6",
                                           "Upto Level 6:",
                                           value=100000,step = 5000),
                              numericInput("Level_7",
                                           "Upto Level 7:",
                                           value=125000,step = 5000),
                              numericInput("Level_8",
                                           "Upto Level 8:",
                                           value=150000,step = 5000),
                              numericInput("Level_9",
                                           "Upto Level 9:",
                                           value=200000,step = 5000),
                              numericInput("Level_10",
                                           "Upto Level 10:",
                                           value=200000,step = 5000),
                              numericInput("Level_11",
                                           "Upto Level 11:",
                                           value=200000,step = 5000),
                              numericInput("Level_12",
                                           "Upto Level 12:",
                                           value=1000000000)#,
                              #actionButton("goButton", "Go!")
                              ),
                              div(style="display: inline-block;vertical-align:top; width: 140px;",
                                  numericInput("prop_1", 
                                               "Percentage 1:", 3),
                                  numericInput("prop_2", 
                                               "Percentage 2:", 3),
                                  numericInput("prop_3", 
                                               "Percentage 3:", 8),
                                  numericInput("prop_4", 
                                               "Percentage 4:", 8),
                                  numericInput("prop_5", 
                                               "Percentage 5:", 11),
                                  numericInput("prop_6", 
                                               "Percentage 6:", 11),
                                  numericInput("prop_7", 
                                               "Percentage 7:", 11),
                                  numericInput("prop_8", 
                                               "Percentage 8:", 11),
                                  numericInput("prop_9", 
                                               "Percentage 9:", 15),
                                  numericInput("prop_10", 
                                               "Percentage 10:", 15),
                                  numericInput("prop_11", 
                                               "Percentage 11:", 15),
                                  numericInput("prop_12", 
                                               "Percentage 12:", 15)
                              ),
                              hr(),
                             width = 3
                            ),
                           # the main panel is split into 2 columns for the 2 statements at the top
                           mainPanel(fluidRow(
                              h3("Reductions applied to TOTAL payment [BPS, greening & Young farmer]"),
                              column(7,
                                     h4(textOutput("caption1", container = span)),
                                     h4(textOutput("caption1b", container = span))
                              ),
                              column(5,
                                     h4(textOutput("caption1a", container = span))
                              )
                              ),
                              fluidRow(
                               h3("Reductions applied to BPS payment only [greening & Young farmer excluded]"),
                               column(7,
                                      h4(textOutput("caption1bps", container = span)),
                                      h4(textOutput("caption1bbps", container = span)),
                                      h5("Percentages are of total fund - not BPS payment")
                               ),
                               column(5,
                                      h4(textOutput("caption1abps", container = span)),
                                      h4(textOutput("caption1agreen", container = span)),
                                      h4(textOutput("caption1ayoung", container = span))
                               )
                              ),
                              fluidRow(
                                # the tabset section allows us to have the different tabs and still have side panel on view
                              tabsetPanel(
                                tabPanel("RPA data - total",
                                         h4("Reductions applied to total Direct payments: BPS,Greening and Young farmer"),
                                         plotOutput("plot3"),
                                         plotOutput("plot4"),
                                         #tableOutput("agg_data_red"),
                                         tableOutput("agg_data")
                                         ),
                                tabPanel("RPA data - BPS only",
                                         h4("Reductions applied to BPS only: not Greening or Young farmer payment"),
                                         plotOutput("plot3a"),
                                         plotOutput("plot4a")
                                ), 
                                tabPanel("FBS data", 
                                         h5("Using FBS data (Â£ million):"),
                                         tableOutput("data_var"),
                                         selectInput("fbsfac", label = h3("Choose a factor:"), 
                                          choices = list("type","type_red", "farm_size",
                                                         "tenancy","gor","lfa","fbi_band","fbi_band1","fbi_band2","age_band","farmer_gender","dp_band"), selected = "type"),
                                         column(7,tableOutput("data_var1")),
                                         "Distribution of payments and losses",
                                         column(5,plotOutput("plot2")),
                                         h5("For all farms, not just those with a loss:"),
                                         tableOutput("data_var2"),
                                         tableOutput("data_var6"),
                                         tableOutput("data_var7")),
                                tabPanel("FBS data 2",
                                        tableOutput("data_var4"),
                                        tableOutput("data_var5")
                                        
                                )
                               )

                              )
                          )
                 )
                 ),
               
                 tabPanel("Background",
                          
                          fluidRow(
                            column(6,
                                   h5("Written by Lindsey Clothier, October 2017"),
                                   p ("This calculates the impact of applying a progressive cap on Direct Payments")
                                   ))
                 )        
                 
)
#####

#-----------------------------------------------
# The server section produces all the output to be displayed
#-----------------------------------------------


server <- function(input, output) {
  # functions to apply payment reductions  
  #####
  
  band_1_reduction <- function(x){
    input$prop_1 * x/100
  }
  
  band_2_reduction <- function(x){
    input$prop_2*(x - input$Level_1)/100 + 
      input$prop_1*input$Level_1/100
  }
  
  band_3_reduction <- function(x){
    input$prop_3*(x - input$Level_2)/100 + 
      input$prop_1*input$Level_1/100 + 
      input$prop_2 * (input$Level_2 - input$Level_1)/100
  }
  
  band_4_reduction <- function(x){
    input$prop_4*(x - input$Level_3)/100 +
      input$prop_1*input$Level_1/100 +
      input$prop_2*(input$Level_2 - input$Level_1)/100 +
      input$prop_3*(input$Level_3 - input$Level_2)/100
  }
  
  band_5_reduction <- function(x){
    input$prop_5*(x - input$Level_4)/100 +
      input$prop_1*input$Level_1/100 +
      input$prop_2*(input$Level_2 - input$Level_1)/100 +
      input$prop_3*(input$Level_3 - input$Level_2)/100 +
      input$prop_4*(input$Level_4 - input$Level_3)/100
    
  }
  
  band_6_reduction <- function(x){
    input$prop_6*(x - input$Level_5)/100 +
      input$prop_1*input$Level_1/100 +
      input$prop_2*(input$Level_2 - input$Level_1)/100 +
      input$prop_3*(input$Level_3 - input$Level_2)/100 +
      input$prop_4*(input$Level_4 - input$Level_3)/100 +
      input$prop_5*(input$Level_5 - input$Level_4)/100
  }
  
  band_7_reduction <- function(x){
    input$prop_7*(x - input$Level_6)/100 +
      input$prop_1*input$Level_1/100 +
      input$prop_2*(input$Level_2 - input$Level_1)/100 +
      input$prop_3*(input$Level_3 - input$Level_2)/100 +
      input$prop_4*(input$Level_4 - input$Level_3)/100 +
      input$prop_5*(input$Level_5 - input$Level_4)/100 +
      input$prop_6*(input$Level_6 - input$Level_5)/100
  }
  
  band_8_reduction <- function(x){
    input$prop_8*(x - input$Level_7)/100 +
      input$prop_1*input$Level_1/100 +
      input$prop_2*(input$Level_2 - input$Level_1)/100 +
      input$prop_3*(input$Level_3 - input$Level_2)/100 +
      input$prop_4*(input$Level_4 - input$Level_3)/100 +
      input$prop_5*(input$Level_5 - input$Level_4)/100 +
      input$prop_6*(input$Level_6 - input$Level_5)/100 +
      input$prop_7*(input$Level_7 - input$Level_6)/100
  }
  
  band_9_reduction <- function(x){
    input$prop_9*(x - input$Level_8)/100 +
      input$prop_1*input$Level_1/100 +
      input$prop_2*(input$Level_2 - input$Level_1)/100 +
      input$prop_3*(input$Level_3 - input$Level_2)/100 +
      input$prop_4*(input$Level_4 - input$Level_3)/100 +
      input$prop_5*(input$Level_5 - input$Level_4)/100 +
      input$prop_6*(input$Level_6 - input$Level_5)/100 +
      input$prop_7*(input$Level_7 - input$Level_6)/100 +
      input$prop_8*(input$Level_8 - input$Level_7)/100
  }
  
  band_10_reduction <- function(x){
    input$prop_10*(x - input$Level_9)/100 +
      input$prop_1*input$Level_1/100 +
      input$prop_2*(input$Level_2 - input$Level_1)/100 +
      input$prop_3*(input$Level_3 - input$Level_2)/100 +
      input$prop_4*(input$Level_4 - input$Level_3)/100 +
      input$prop_5*(input$Level_5 - input$Level_4)/100 +
      input$prop_6*(input$Level_6 - input$Level_5)/100 +
      input$prop_7*(input$Level_7 - input$Level_6)/100 +
      input$prop_8*(input$Level_8 - input$Level_7)/100 +
      input$prop_9*(input$Level_9 - input$Level_8)/100
  }
  
  band_11_reduction <- function(x){
    input$prop_11*(x - input$Level_10)/100 +
      input$prop_1*input$Level_1/100 +
      input$prop_2*(input$Level_2 - input$Level_1)/100 +
      input$prop_3*(input$Level_3 - input$Level_2)/100 +
      input$prop_4*(input$Level_4 - input$Level_3)/100 +
      input$prop_5*(input$Level_5 - input$Level_4)/100 +
      input$prop_6*(input$Level_6 - input$Level_5)/100 +
      input$prop_7*(input$Level_7 - input$Level_6)/100 +
      input$prop_8*(input$Level_8 - input$Level_7)/100 +
      input$prop_9*(input$Level_9 - input$Level_8)/100 +
      input$prop_10*(input$Level_10 - input$Level_9)/100
  }
  
  band_12_reduction <- function(x){
    input$prop_12*(x - input$Level_11)/100 +
      input$prop_1*input$Level_1/100 +
      input$prop_2*(input$Level_2 - input$Level_1)/100 +
      input$prop_3*(input$Level_3 - input$Level_2)/100 +
      input$prop_4*(input$Level_4 - input$Level_3)/100 +
      input$prop_5*(input$Level_5 - input$Level_4)/100 +
      input$prop_6*(input$Level_6 - input$Level_5)/100 +
      input$prop_7*(input$Level_7 - input$Level_6)/100 +
      input$prop_8*(input$Level_8 - input$Level_7)/100 +
      input$prop_9*(input$Level_9 - input$Level_8)/100 +
      input$prop_10*(input$Level_10 - input$Level_9)/100 +
      input$prop_11*(input$Level_11 - input$Level_10)/100
  }
  
  #####
  

#This calculates the loss for each business using RPA data based on the TOTAL PAYMENT
  #observeEvent(input$goButton,{
    Loss_rpa <- reactive ({
      #####
      rpaloss <- subset(RPApay_interim, RPApay_interim$Total_Payment_pounds>0)
      rpaloss$loss <- dplyr::case_when(
        # if payment is less than first level, reduce by prop_1
        rpaloss$Total_Payment_pounds < input$Level_1 ~
          band_1_reduction(rpaloss$Total_Payment_pounds),
        # if payment is between level 1 and level 2
        dplyr::between(rpaloss$Total_Payment_pounds, input$Level_1, input$Level_2) ~
          band_2_reduction(rpaloss$Total_Payment_pounds),
        # if payment is between level 2 and level 3
        dplyr::between(rpaloss$Total_Payment_pounds, input$Level_2, input$Level_3) ~
          band_3_reduction(rpaloss$Total_Payment_pounds), 
        # if payment is between level 3 and level 4
        dplyr::between(rpaloss$Total_Payment_pounds, input$Level_3, input$Level_4) ~ 
          band_4_reduction(rpaloss$Total_Payment_pounds),
        # if payment is between level 4 and level 5
        dplyr::between(rpaloss$Total_Payment_pounds, input$Level_4, input$Level_5) ~ 
          band_5_reduction(rpaloss$Total_Payment_pounds),
        # if payment is between level 5 and level 6
        dplyr::between(rpaloss$Total_Payment_pounds, input$Level_5, input$Level_6) ~ 
          band_6_reduction(rpaloss$Total_Payment_pounds),
        # if payment is between level 6 and level 7
        dplyr::between(rpaloss$Total_Payment_pounds, input$Level_6, input$Level_7) ~ 
          band_7_reduction(rpaloss$Total_Payment_pounds),
        # if payment is between level 7 and level 8
        dplyr::between(rpaloss$Total_Payment_pounds, input$Level_7, input$Level_8) ~ 
          band_8_reduction(rpaloss$Total_Payment_pounds),
        # if payment is between level 8 and level 9
        dplyr::between(rpaloss$Total_Payment_pounds, input$Level_8, input$Level_9) ~ 
          band_9_reduction(rpaloss$Total_Payment_pounds),
        # if payment is between level 9 and level 10
        dplyr::between(rpaloss$Total_Payment_pounds, input$Level_9, input$Level_10) ~ 
          band_10_reduction(rpaloss$Total_Payment_pounds),
        # if payment is between level 10 and level 11
        dplyr::between(rpaloss$Total_Payment_pounds, input$Level_10, input$Level_11) ~ 
          band_11_reduction(rpaloss$Total_Payment_pounds),
        # if payment is between level 11 and level 12
        dplyr::between(rpaloss$Total_Payment_pounds, input$Level_11, input$Level_12) ~ 
          band_12_reduction(rpaloss$Total_Payment_pounds)
        )
    
    #Set the bands for analysis
    rpaloss$pay_band <- as.factor(cut(rpaloss$Total_Payment_pounds, 
                                                 breaks = c(-10000000,0, 5000, 10000, 15000, 20000, 25000, 30000, 40000, 50000, 75000,  100000, 125000, 150000, 200000, 250000, 300000, Inf), 
                                                 labels = c("less than zero", "0 to 5k", "5 to 10k", "10 to 15k", "15 to 20k", "20 to 25k", "25 to 30k", "30 to 40k", "40 to 50k", "50 to 75k", "75 to 100k", "100 to 125k", "125 to 150k","150 to 200k", "200k to 250k", "250k to 300k", "over 300k"), 
                                                 levels =c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17),
                                                 right = TRUE,ordered_result = TRUE))

    
    #Set band for money lost
    rpaloss$pay_band_money_lost <- as.factor(cut(rpaloss$loss, 
                                                            breaks = c(0, 5000, 10000, 15000, 20000, 25000, 30000, 40000, 50000, 75000,  100000, 125000, 150000, 200000, 250000, 300000, Inf), 
                                                            labels = c("Under 5k", "5 to 10k", "10 to 15k", "15 to 20k", "20 to 25k", "25 to 30k", "30 to 40k", "40 to 50k", "50 to 75k", "75 to 100k", "100 to 125k", "125 to 150k","150 to 200k", "200k to 250k", "250k to 300k","over 300k"), 
                                                            levels =c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17),
                                                            right = TRUE,ordered_result = TRUE))
    rpaloss$num_affected <- ifelse(rpaloss$loss==0,0,1)
    rpaloss$dummy <- 1
    rpaloss
    #####
    })


  #This calculates the loss for each business using FBS data
  Loss_fbs <- reactive ({
    #####
    fbsloss <- fbs_3yr
    fbsloss$loss <- dplyr::case_when(
      # if payment is less than first level, reduce by prop_1
      fbsloss$dp_3yr < input$Level_1 ~ 
        band_1_reduction(fbsloss$dp_3yr),
      # if payment is between level 1 and level 2
      dplyr::between(fbsloss$dp_3yr, input$Level_1, input$Level_2) ~ 
        band_2_reduction(fbsloss$dp_3yr),
      # if payment is between level 2 and level 3
      dplyr::between(fbsloss$dp_3yr, input$Level_2, input$Level_3) ~ 
        band_3_reduction(fbsloss$dp_3yr),
      # if payment is between level 3 and level 4
      dplyr::between(fbsloss$dp_3yr, input$Level_3, input$Level_4) ~ 
        band_4_reduction(fbsloss$dp_3yr),
      # if payment is between level 4 and level 5
      dplyr::between(fbsloss$dp_3yr, input$Level_4, input$Level_5) ~ 
        band_5_reduction(fbsloss$dp_3yr),
      # if payment is between level 5 and level 6
      dplyr::between(fbsloss$dp_3yr, input$Level_5, input$Level_6) ~ 
        band_6_reduction(fbsloss$dp_3yr),
      # if payment is between level 6 and level 7
      dplyr::between(fbsloss$dp_3yr, input$Level_6, input$Level_7) ~ 
        band_7_reduction(fbsloss$dp_3yr),
      # if payment is between level 7 and level 8
      dplyr::between(fbsloss$dp_3yr, input$Level_7, input$Level_8) ~ 
        band_8_reduction(fbsloss$dp_3yr),
      # if payment is between level 8 and level 9
      dplyr::between(fbsloss$dp_3yr, input$Level_8, input$Level_9) ~ 
        band_9_reduction(fbsloss$dp_3yr),
      # if payment is between level 9 and level 10
      dplyr::between(fbsloss$dp_3yr, input$Level_9, input$Level_10) ~ 
        band_10_reduction(fbsloss$dp_3yr),
      # if payment is between level 10 and level 11
      dplyr::between(fbsloss$dp_3yr, input$Level_10, input$Level_11) ~ 
        band_11_reduction(fbsloss$dp_3yr),
      # if payment is between level 11 and level 12
      dplyr::between(fbsloss$dp_3yr, input$Level_11, input$Level_12) ~ 
        band_12_reduction(fbsloss$dp_3yr)
    )
      
    fbsloss %>%
      dplyr::mutate(
        # marker for affected farms
        num_affected = ifelse(loss==0,0,1),
        # calculates new direct payment
        newdp = dp_3yr - loss,
        # calculates new direct payment costs pro rata
        new_dpcost = ifelse(dp_3yr == 0, 0, newdp * dp_costs_3yr/dp_3yr),
        # calculates new direct payment cost centre
        new_dp_cc = newdp - new_dpcost,
        # calculates new FBI
        new_fbi = fbi_3yr - dp_cc_3yr + new_dp_cc,
        # calculates change in FBI
        fbi_change = fbi_3yr-new_fbi,
        
        # marker for farms with negative FBI
        neg_fbi = ifelse(fbi_3yr < 0,1,0),
        # marker for farms with negative FBI after loss of payment
        negnew_fbi = ifelse(new_fbi < 0,1,0),
        # marker for farms with FBI < 10k
        lt_10k_fbi = ifelse(fbi_3yr < 10000,1,0),
        # marker for farms with FBI < 10k after loss of payment
        under10knew_fbi = ifelse(new_fbi < 10000,1,0),
        # marker for farms with FBI < 25k
        lt_25k_fbi = ifelse(fbi_3yr < 25000,1,0),
        # marker for farms with FBI < 25k after loss of payment
        under25knew_fbi = ifelse(new_fbi < 25000,1,0),
        # marker for farms with FBI < 50k
        lt_50k_fbi = ifelse(fbi_3yr < 50000,1,0),
        # marker for farms with FBI < 50k after loss of payment
        under50knew_fbi = ifelse(new_fbi < 50000,1,0),
        
        # sets up a factor for fams by FBI (original)
        fbi_band = as.factor(cut(fbi_3yr,             
                                 breaks = c(-Inf,0,10000,25000,Inf), 
                                 labels = c("less than zero", "0 to 10k","10 to 25k","Over 25k"), 
                                 levels =c(1,2,3,4),
                                 right = TRUE,
                                 ordered_result = TRUE)),
        # sets up a factor for fams by FBI (original)
        fbi_band1 = as.factor(cut(fbi_3yr,
                                  breaks = c(-Inf,0,10000,25000,50000,Inf), 
                                  labels = c("less than zero", "0 to 10k","10 to 25k","25 to 50k","Over 50k"), 
                                  levels =c(1,2,3,4),
                                  right = TRUE,
                                  ordered_result = TRUE)),
        # sets up a factor for fams by FBI (original)
        fbi_band2 = as.factor(cut(fbi_min_dp_cc_3yr, 
                                  breaks = c(-Inf,0,10000,20000,30000,40000,50000,100000,Inf), 
                                  labels = c("less than zero", "0 to 10k","10 to 20k","20 to 30k","30 to 40k","40 to 50k","50 to 100k","Over 100k"), 
                                  levels =c(1,2,3,4,5,6,7,8),
                                  right = TRUE,
                                  ordered_result = TRUE)),
        # sets up a factor for fams by FBI (original)
         dp_band = as.factor(cut(dp_3yr,
                                 breaks = c(-Inf,0,5000,10000,
                                            15000,20000,25000,
                                            30000,40000,
                                            50000,60000,
                                            70000,80000,
                                            100000,150000,Inf), 
                                 labels = c("No DP", "0 to 5k","5 to 10k",
                                            "10 to 15k","15 to 20k",
                                            "20 to 25k","25 to 30k",
                                            "30 to 40k","40 to 50k",
                                            "50 to 60k","60 to 70k",
                                            "70 to 80k","80 to 100k",
                                            "100 to 150k","over 150k"), 
                                 levels =c(1,2,3,4),
                                 right = TRUE,
                                 ordered_result = TRUE))
        ) -> fbsloss
    #####
  })

  
  ##sets up the survey design
  #####
  fbsdesign <- reactive({svydesign(id= ~Loss_fbs()$farms,
                                   strata= ~Loss_fbs()$stratum,
                                   fpc=~Loss_fbs()$num_pop,
                                   data= Loss_fbs(),
                                   weight= ~Loss_fbs()$newwt3yr,
                                   nest=TRUE)

  })
  #####
  
  #-----------------------------------------------
  # calculate all the means, totals and observation numbers
  #-----------------------------------------------
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #1. TOTALS
  
  # Total loss for all farms
  tots_allfarms<-reactive({svyby(~Loss_fbs()$loss,1,fbsdesign(),svytotal,na.rm.by = FALSE,vartype = c("ci"))
     })
  
  
  # tots_allfarms<-svyby(~loss,
  #                      1,
  #                      fbsdesign,
  #                      svytotal,
  #                      na.rm.by = FALSE,
  #                      vartype = c("ci"))
  
  
  
  
  
  # Total Direct Payment for all farms
  tots_dp_allfarms<-reactive({svyby(~Loss_fbs()$dp_3yr,1,fbsdesign(),svytotal,na.rm.by = FALSE,vartype = c("ci"))
  })
  
    # Total loss by chosen factor
  tots_var1 <- reactive({svyby(~loss, interp(~x, x = as.name(input$fbsfac)),fbsdesign(),svytotal,na.rm.by = FALSE,vartype = c("ci"))
        })
  
  # Total Direct Payment by chosen factor
  tots_var2 <- reactive({svyby(~dp_3yr, interp(~x, x = as.name(input$fbsfac)),fbsdesign(),svytotal,na.rm.by = FALSE,vartype = c("ci"))
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #2. MEANS
  
  # Average original Direct Payment by chosen factor
  mean_dp <- reactive({svyby(~dp_3yr, interp(~x, x = as.name(input$fbsfac)),fbsdesign(),svymean,na.rm.by = FALSE,vartype = c("ci"))
  })
  
  # Average original Direct Payment all farms
  mean_dp_all <- reactive({svyby(~dp_3yr,1,fbsdesign(),svymean,na.rm.by = FALSE,vartype = c("ci"))
  })

  # Average new Direct Payment by chosen factor
  mean_dpnew <- reactive({svyby(~newdp, interp(~x, x = as.name(input$fbsfac)),fbsdesign(),svymean,na.rm.by = FALSE,vartype = c("ci"))
  })
  
  # Average new Direct Payment all farms
  mean_dpnew_all <- reactive({svyby(~newdp,1,fbsdesign(),svymean,na.rm.by = FALSE,vartype = c("ci"))
  })
  
  # Average original FBI by chosen factor
  mean_fbi <- reactive({svyby(~fbi_3yr, interp(~x, x = as.name(input$fbsfac)),fbsdesign(),svymean,na.rm.by = FALSE,vartype = c("ci"))
  })
  
  # Average original FBI all farms
  mean_fbi_all <- reactive({svyby(~fbi_3yr,1,fbsdesign(),svymean,na.rm.by = FALSE,vartype = c("ci"))
  })
  
  # Average new FBI by chosen factor
  mean_newfbi <- reactive({svyby(~new_fbi, interp(~x, x = as.name(input$fbsfac)),fbsdesign(),svymean,na.rm.by = FALSE,vartype = c("ci"))
  })
  
  # Average new FBI by chosen factor
  mean_newfbi_all <- reactive({svyby(~new_fbi,1,fbsdesign(),svymean,na.rm.by = FALSE,vartype = c("ci"))
  })
  
  # Average original FBI farms with negative income and lose money under option
  mean_fbi_neg_loss <- reactive({svyby(~fbi_3yr, 1,subset(fbsdesign(),num_affected==1 & neg_fbi==1),svymean,na.rm.by = FALSE,vartype = c("ci"))
  })
  
  # Average new FBI farms with negative income and lose money under option
  mean_newfbi_neg_loss <- reactive({svyby(~new_fbi, 1,subset(fbsdesign(),num_affected==1 & neg_fbi==1),svymean,na.rm.by = FALSE,vartype = c("ci"))
  })
  
  # Average change FBI farms with negative income and lose money under option
  mean_incloss_neg_loss <- reactive({svyby(~fbi_change, 1,subset(fbsdesign(),num_affected==1 & neg_fbi==1),svymean,na.rm.by = FALSE,vartype = c("ci"))
  })

  # Average change FBI farms with income under 10k and lose money under option
  mean_incloss_10k_loss <- reactive({svyby(~fbi_change, 1,subset(fbsdesign(),num_affected==1 & lt_10k_fbi==1),svymean,na.rm.by = FALSE,vartype = c("ci"))
  })
  
  # Average change FBI farms with income under 25k and lose money under option
  mean_incloss_25k_loss <- reactive({svyby(~fbi_change, 1,subset(fbsdesign(),num_affected==1 & lt_25k_fbi==1),svymean,na.rm.by = FALSE,vartype = c("ci"))
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #3. PROPORTIONS OF FARMS (WEIGHTED)
  
  # Weighted proportion of farms with negative FBI by chosen factor
  mean_negfbi <- reactive({svyby(~neg_fbi, interp(~x, x = as.name(input$fbsfac)),fbsdesign(),svymean,na.rm.by = FALSE,vartype = c("ci"))
  })
  
  # Weighted proportion of farms with negative FBI after option of DP by chosen factor
  mean_negnewfbi <- reactive({svyby(~negnew_fbi, interp(~x, x = as.name(input$fbsfac)),fbsdesign(),svymean,na.rm.by = FALSE,vartype = c("ci"))
  })

  # Weighted proportion of farms with negative FBI all farms
  mean_negfbi_prop <- reactive({svyby(~neg_fbi,1,fbsdesign(),svymean,na.rm.by = FALSE,vartype = c("ci"))
  })
  
  # Weighted proportion of farms with negative FBI after any loss - all farms
  mean_negnewfbi_prop <- reactive({svyby(~negnew_fbi,1,fbsdesign(),svymean,na.rm.by = FALSE,vartype = c("ci"))
  })
  
  # Weighted proportion of farms with FBI lt 10k all farms
  mean_fbi_lt_10k_prop <- reactive({svyby(~lt_10k_fbi,1,fbsdesign(),svymean,na.rm.by = FALSE,vartype = c("ci"))
  })

  # Weighted proportion of farms with FBI lt 10k after any loss - all farms
  mean_newfbi_lt_10k_prop <- reactive({svyby(~under10knew_fbi,1,fbsdesign(),svymean,na.rm.by = FALSE,vartype = c("ci"))
  })
  
  # Weighted proportion of farms with FBI lt 25k all farms
  mean_fbi_lt_25k_prop <- reactive({svyby(~lt_25k_fbi,1,fbsdesign(),svymean,na.rm.by = FALSE,vartype = c("ci"))
  })

  # Weighted proportion of farms with FBI lt 25k after any loss - all farms
  mean_newfbi_lt_25k_prop <- reactive({svyby(~under25knew_fbi,1,fbsdesign(),svymean,na.rm.by = FALSE,vartype = c("ci"))
  })
    
  # Weighted proportion of farms with FBI lt 50k all farms
  mean_fbi_lt_50k_prop <- reactive({svyby(~lt_50k_fbi,1,fbsdesign(),svymean,na.rm.by = FALSE,vartype = c("ci"))
  })
  
  # Weighted proportion of farms with FBI lt 50k after any loss - all farms
  mean_newfbi_lt_50k_prop <- reactive({svyby(~under50knew_fbi,1,fbsdesign(),svymean,na.rm.by = FALSE,vartype = c("ci"))
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #4. NUMBERS (WEIGHTED)
  
  # Weighted NUMBER of farms with negative FBI all farms
  tot_negfbi_weight <- reactive({svyby(~neg_fbi,1,fbsdesign(),svytotal,na.rm.by = FALSE,vartype = c("ci"))
  })
 
  # Weighted NUMBER of farms with negative FBI and loss all farms
  tot_negfbi_loss_weight <- reactive({svyby(~neg_fbi,1,subset(fbsdesign(),num_affected==1),svytotal,na.rm.by = FALSE,vartype = c("ci"))
  })
  
  # Weighted NUMBER of farms with FBI lt 10k all farms
  tot_fbilt10k_weight <- reactive({svyby(~lt_10k_fbi,1,fbsdesign(),svytotal,na.rm.by = FALSE,vartype = c("ci"))
  })
  
  # Weighted NUMBER of farms with FBI less than 10k and loss all farms
  tot_fbilt10k_loss_weight <- reactive({svyby(~lt_10k_fbi,1,subset(fbsdesign(),num_affected==1),svytotal,na.rm.by = FALSE,vartype = c("ci"))
  })
  
  # Weighted NUMBER of farms with FBI lt 25k all farms
  tot_fbilt25k_weight <- reactive({svyby(~lt_25k_fbi,1,fbsdesign(),svytotal,na.rm.by = FALSE,vartype = c("ci"))
  })

  # Weighted NUMBER of farms with FBI less than 25k and loss all farms
  tot_fbilt25k_loss_weight <- reactive({svyby(~lt_25k_fbi,1,subset(fbsdesign(),num_affected==1),svytotal,na.rm.by = FALSE,vartype = c("ci"))
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #5. SAMPLE NUMBERS
  
  # Number of farms losing some payment in sample
  observations_lossall<-reactive({summaryBy(list("num_affected",1),data=Loss_fbs(),FUN=sum)
  })
  
  # Number of farms with negative FBI in sample all farms
  observations_negfbi_all<-reactive({summaryBy(list("neg_fbi",1),data=Loss_fbs(),FUN=sum)
  })
  
  # Number of farms with negative FBI after losing payment in sample all farms
  observations_newnegfbi_all<-reactive({summaryBy(list("negnew_fbi",1),data=Loss_fbs(),FUN=sum)
  })
   
  # Number of farms with negative FBI and loss in sample all farms
  observations_loss_neginc<-reactive({summaryBy(list("num_affected",1),
                                            data=subset(Loss_fbs(),
                                                        Loss_fbs()$neg_fbi==1),
                                            FUN=sum)  
  })
 
  # Number of farms with FBI lt 10k in sample all farms
  observations_lt10k_all<-reactive({summaryBy(list("lt_10k_fbi",1),data=Loss_fbs(),FUN=sum)
  })
  
  # Number of farms with FBI lt 10k after loss in sample all farms
  observations_lt10knew_all<-reactive({summaryBy(list("under10knew_fbi",1),data=Loss_fbs(),FUN=sum)
  })
  
  # Number of farms with FBI lt 10k and loss in sample all farms
  observations_loss_fbi_lt10k<-reactive({summaryBy(list("num_affected",1),
                                                  data=subset(Loss_fbs(),
                                                              Loss_fbs()$lt_10k_fbi==1),
                                                  FUN=sum) 
  })
 
  # Number of farms with FBI lt 25k in sample all farms
  observations_lt25k_all<-reactive({summaryBy(list("lt_25k_fbi",1),data=Loss_fbs(),FUN=sum)
  })
  
  # Number of farms with FBI lt 25k after loss in sample all farms
  observations_lt25knew_all<-reactive({summaryBy(list("under25knew_fbi",1),data=Loss_fbs(),FUN=sum)
  })
  
  # Number of farms with FBI lt 25k and loss in sample all farms
  observations_loss_fbi_lt25k<-reactive({summaryBy(list("num_affected",1),
                                                  data=subset(Loss_fbs(),
                                                              Loss_fbs()$lt_25k_fbi==1),
                                                  FUN=sum) 
   })
  
  # Number of farms with FBI lt 50k in sample all farms
  observations_lt50k_all<-reactive({summaryBy(list("lt_50k_fbi",1),data=Loss_fbs(),FUN=sum)
  })
  
  # Number of farms with FBI lt 25k after loss in sample all farms
  observations_lt50knew_all<-reactive({summaryBy(list("under50knew_fbi",1),data=Loss_fbs(),FUN=sum)
  })
  
  # Number of farms with a loss in sample by factor
  observations_lossvar<-reactive({summaryBy(list("num_affected",input$fbsfac),data=Loss_fbs(),FUN=sum)
  }) 
  
  # Number of farms with negative FBI in sample by factor
  observations_negfbi<-reactive({summaryBy(list("neg_fbi",input$fbsfac),data=Loss_fbs(),FUN=sum)
  })
  
  # Number of farms with negative FBI after a loss in sample by factor
  observations_negnewfbi<-reactive({summaryBy(list("negnew_fbi",input$fbsfac),data=Loss_fbs(),FUN=sum)
  })
  

  # sets up the table for all farms
  data_var1 <- reactive({
    stat_allfarms <-cbind(tots_allfarms(),observations_lossall(),tots_dp_allfarms()[,2])   # join relevant bits together
    stat_allfarms$by="All farms"                                                           # put "All farms" in the column called "by"
    setnames(stat_allfarms, "by"," ")                                                      # rename column balled "by to blank"   
    setnames(stat_allfarms,"Loss_fbs()$loss","Total payment loss")                                 # rename column with suitable title for table
    setnames(stat_allfarms, "num_affected.sum","num in sample with payment loss")                  # rename column with suitable title for table
    setnames(stat_allfarms,"tots_dp_allfarms()[, 2]","Total Orig. Dir. Payment")           # rename column with suitable title for table
    stat_allfarms[,2:4] <- stat_allfarms[,2:4]#/1000000                                     # Show data in columns 2 to 4 in million pounds
    stat_allfarms[,6] <- stat_allfarms[,6]#/1000000                                         # Show data in column 6 in million pounds
    stat_allfarms$'percent loss' <- (stat_allfarms[,2]/stat_allfarms[,6])*100              # calculate loss as a percentage of direct payment
    return(stat_allfarms)
  })
  
  data_var2 <- reactive({
    stat_byfac <- cbind(tots_var1(),observations_lossvar()[,2],tots_var2()[,2])
    setnames(stat_byfac,"loss","Total payment loss (Â£m)")
    setnames(stat_byfac,"tots_var2()[, 2]","Total Orig. Dir. Payment (Â£m)")
    setnames(stat_byfac, "observations_lossvar()[, 2]","num in sample with payment loss")
    stat_byfac[,2:4] <- stat_byfac[,2:4]#/1000000
    stat_byfac[,6] <- stat_byfac[,6]#/1000000
    stat_byfac$'percent loss' <- (stat_byfac[,2]/stat_byfac[,6])*100
    return(stat_byfac)
  }) 
  
  for_dp_chart <- reactive({
    chart <- data_var2()
    chart$lossdist <- chart$`Total payment loss (Â£m)`/sum(chart$`Total payment loss (Â£m)`) * 100
    chart$dpdist <- chart$`Total Orig. Dir. Payment (Â£m)`/sum(chart$`Total Orig. Dir. Payment (Â£m)`) * 100
    charta <- cbind(chart[,c(1,8,9)])
    charta <- reshape2::melt(charta, id.vars=input$fbsfac, measure.vars=c("lossdist","dpdist"))
    return(charta)
  })
  
  data_var3 <- reactive({
    fbi_byfac <- cbind(mean_fbi(),mean_negfbi()[,2]*100,observations_negfbi()[,2],
                       mean_newfbi()[,-1],mean_negnewfbi()[,2]*100,observations_negnewfbi()[,2])
    fbi_byfac$sample_change <- fbi_byfac[,11]-fbi_byfac[,6]
    colnames(fbi_byfac) <- c("",
                             "Farm Business Income", 
                             "FBI_CI_L", 
                             "FBI_CI_U", 
                             "Prop farms FBI <0",
                             "Num in sample neg FBI", 
                             "New Farm Business Income",
                             "FBI_CI_L", 
                             "FBI_CI_U", 
                             "New Prop farms FBI <0",
                             "Num in sample nag FBI after payment loss",
                             "sample pos to neg")
    
    fbi_all <- cbind(mean_fbi_all(),mean_negfbi_prop()[,2]*100,observations_negfbi_all(),
                     mean_newfbi_all()[,-1],mean_negnewfbi_prop()[,2]*100,observations_newnegfbi_all())
    fbi_all$sample_change <- fbi_all[,11]-fbi_all[,6]
    fbi_all[1,1] <- "All farms"
    colnames(fbi_all) <- c("",
                           "Farm Business Income", 
                           "FBI_CI_L", 
                           "FBI_CI_U", 
                           "Prop farms FBI <0",
                           "Num in sample neg FBI", 
                           "New Farm Business Income",
                           "FBI_CI_L", 
                           "FBI_CI_U", 
                           "New Prop farms FBI <0",
                           "Num in sample nag FBI after payment loss",
                           "sample pos to neg")
    
    fbitab <- rbind(fbi_byfac, fbi_all)
    return(fbitab)
  }) 
 
  
  # sets up the table for all farms
  data_var4 <- reactive({
    tots1 <- tot_negfbi_weight()[,2:4]
    setnames(tots1,"neg_fbi","Number of farms in population")
    setnames(tots1,"ci_u","ci_u1")
    setnames(tots1,"ci_l","ci_l1")
    tots2 <- tot_negfbi_loss_weight()[,2:4]
    setnames(tots2,"ci_u","ci_u2")
    setnames(tots2,"ci_l","ci_l2")
    setnames(tots2,"neg_fbi","Number of farms with loss in population")
    tots2a <-mean_incloss_neg_loss()[,2:4]
    setnames(tots2a,"ci_u","ci_u3")
    setnames(tots2a,"ci_l","ci_l3")
    setnames(tots2a,"fbi_change","Average income loss for those with loss")
    vulnerable1 <-cbind(mean_negfbi_prop(),tots1,tots2,tots2a,observations_negfbi_all(),observations_loss_neginc())   # join relevant bits together
    vulnerable1$by="Negative FBI"                                                           # put "All farms" in the column called "by"
    setnames(vulnerable1,"neg_fbi","Proportion of farms in population")                  # rename column with suitable title for table
    setnames(vulnerable1, "num_affected.sum","Num in sample with loss")                  # rename column with suitable title for table
    setnames(vulnerable1,"neg_fbi.sum","Num in sample")                                  # rename column with suitable title for table

    tots3 <- tot_fbilt10k_weight()[,2:4]
    setnames(tots3,"ci_u","ci_u1")
    setnames(tots3,"ci_l","ci_l1")
    setnames(tots3,"lt_10k_fbi","Number of farms in population")
    tots4 <- tot_fbilt10k_loss_weight()[,2:4]
    setnames(tots4,"lt_10k_fbi","Number of farms with loss in population")
    setnames(tots4,"ci_u","ci_u2")
    setnames(tots4,"ci_l","ci_l2")
    tots4a <-mean_incloss_10k_loss()[,2:4]
    setnames(tots4a,"ci_u","ci_u3")
    setnames(tots4a,"ci_l","ci_l3")
    setnames(tots4a,"fbi_change","Average income loss for those with loss")
    vulnerable2 <-cbind(mean_fbi_lt_10k_prop(),tots3,tots4,tots4a,observations_lt10k_all(),observations_loss_fbi_lt10k())   # join relevant bits together
    vulnerable2$by="FBI less than 10k"                                                           # put "All farms" in the column called "by"
    setnames(vulnerable2,"lt_10k_fbi","Proportion of farms in population")                  # rename column with suitable title for table
    setnames(vulnerable2, "num_affected.sum","Num in sample with loss")                  # rename column with suitable title for table
    setnames(vulnerable2,"lt_10k_fbi.sum","Num in sample")                                  # rename column with suitable title for table  

    tots5 <- tot_fbilt25k_weight()[,2:4]
    setnames(tots5,"lt_25k_fbi","Number of farms in population")
    setnames(tots5,"ci_u","ci_u1")
    setnames(tots5,"ci_l","ci_l1")
    tots6 <- tot_fbilt25k_loss_weight()[,2:4]
    setnames(tots6,"lt_25k_fbi","Number of farms with loss in population")
    setnames(tots6,"ci_u","ci_u2")
    setnames(tots6,"ci_l","ci_l2")
    tots6a <-mean_incloss_25k_loss()[,2:4]
    setnames(tots6a,"ci_u","ci_u3")
    setnames(tots6a,"ci_l","ci_l3")
    setnames(tots6a,"fbi_change","Average income loss for those with loss")
    vulnerable3 <-cbind(mean_fbi_lt_25k_prop(),tots5,tots6,tots6a,observations_lt25k_all(),observations_loss_fbi_lt25k())   # join relevant bits together
    vulnerable3$by="FBI less than 25k"                                                           # put "All farms" in the column called "by"
    setnames(vulnerable3,"lt_25k_fbi","Proportion of farms in population")                  # rename column with suitable title for table
    setnames(vulnerable3, "num_affected.sum","Num in sample with loss")                  # rename column with suitable title for table
    setnames(vulnerable3,"lt_25k_fbi.sum","Num in sample")                                  # rename column with suitable title for table

    vulnerable <- rbind(vulnerable1,vulnerable2,vulnerable3)
    setnames(vulnerable, "by"," ") 
    vulnerable[,2:4] <- vulnerable[,2:4]*100                                         # Show data in columns 2 to 4 as percentages
    vulnerable$Prop_losing <- vulnerable[,8]*100/vulnerable[,5]
    setnames(vulnerable, "Prop_losing","Proportion of farms losing payment") 
    return(vulnerable)
  })
  
  data_var5 <- reactive({
    means1a <- cbind(mean_negnewfbi_prop()[,2:4],observations_newnegfbi_all())
    setnames(means1a,"negnew_fbi","Proportion of farms")
    setnames(means1a,"ci_u","ci_u1")
    setnames(means1a,"ci_l","ci_l1")
    setnames(means1a,"negnew_fbi.sum","sample nums")
    table5 <-cbind(mean_negfbi_prop(),observations_negfbi_all(),means1a)
    table5$by <- "Negative FBI"
    setnames(table5,"neg_fbi","Proportion of farms")
    setnames(table5,"ci_u","ci_u1")
    setnames(table5,"ci_l","ci_l1")
    setnames(table5,"neg_fbi.sum","sample nums")
    means1b <- cbind(mean_newfbi_lt_10k_prop()[,2:4],observations_lt10knew_all())
    setnames(means1b,"under10knew_fbi","Proportion of farms")
    setnames(means1b,"ci_u","ci_u1")
    setnames(means1b,"ci_l","ci_l1")
    setnames(means1b,"under10knew_fbi.sum","sample nums")
    table6 <-cbind(mean_fbi_lt_10k_prop(),observations_lt10k_all(),means1b)
    table6$by <- "FBI less than 10k"
    setnames(table6,"lt_10k_fbi","Proportion of farms")
    setnames(table6,"ci_u","ci_u1")
    setnames(table6,"ci_l","ci_l1")
    setnames(table6,"lt_10k_fbi.sum","sample nums")
    means1c <- cbind(mean_newfbi_lt_25k_prop()[,2:4],observations_lt25knew_all())
    setnames(means1c,"under25knew_fbi","Proportion of farms")
    setnames(means1c,"ci_u","ci_u1")
    setnames(means1c,"ci_l","ci_l1")
    setnames(means1c,"under25knew_fbi.sum","sample nums")
    table7 <-cbind(mean_fbi_lt_25k_prop(),observations_lt25k_all(),means1c)
    table7$by <- "FBI less than 25k"
    setnames(table7,"lt_25k_fbi","Proportion of farms")
    setnames(table7,"ci_u","ci_u1")
    setnames(table7,"ci_l","ci_l1")
    setnames(table7,"lt_25k_fbi.sum","sample nums")
    means1d <- cbind(mean_newfbi_lt_50k_prop()[,2:4],observations_lt50knew_all())
    setnames(means1d,"under50knew_fbi","Proportion of farms")
    setnames(means1d,"ci_u","ci_u1")
    setnames(means1d,"ci_l","ci_l1")
    setnames(means1d,"under50knew_fbi.sum","sample nums")
    table8 <-cbind(mean_fbi_lt_50k_prop(),observations_lt50k_all(),means1d)
    table8$by <- "FBI less than 50k"
    setnames(table8,"lt_50k_fbi","Proportion of farms")
    setnames(table8,"ci_u","ci_u1")
    setnames(table8,"ci_l","ci_l1")
    setnames(table8,"lt_50k_fbi.sum","sample nums")
    table9 <-rbind(table5,table6,table7,table8)
    table9[,2:4] <-table9[,2:4]*100
    table9[,6:8] <-table9[,6:8]*100
    table9$sample_change <- table9[,9]-table9[,5]
    setnames(table9, "by"," ")
    setnames(table9, "sample_change","Change in sample nums")
    return(table9)
  })
  
  data_var6 <- reactive({
    #dp_allfarms <-mean_dp_all()
    #dp_allfarms$by="All farms"                                                           # put "All farms" in the column called "by"
    #setnames(dp_allfarms, "by","fac")     
    dp_tab <- mean_dp()
    #names(dp_byfac)[1]<-"fac"
    #dp_tab <- rbind(dp_byfac,dp_allfarms)
    dp_tab$`Original DP CI` <- dp_tab$ci_u-dp_tab$dp_3yr # rename column with suitable title for table
    setnames(dp_tab, "dp_3yr","Original DP")                  # rename column with suitable title for table
    #dpnew_allfarms <-mean_dpnew_all()
    #dpnew_allfarms$by="All farms"                                                           # put "All farms" in the column called "by"
    #setnames(dpnew_allfarms, "by","fac1")  # rename column balled "by to blank"   
    dpnew_tab <- mean_dpnew()
    #names(dpnew_byfac)[1]<-"fac1"
    #dpnew_tab <- rbind(dpnew_byfac,dpnew_allfarms)
    dpnew_tab$`New DP CI` <- dpnew_tab$ci_u-dpnew_tab$newdp # rename column with suitable title for table
    setnames(dpnew_tab, "newdp","New DP")                  # rename column with suitable title for table    
    dp_all<-cbind(dp_tab,dpnew_tab)
    return(dp_all)
  })
  
  data_var7 <- reactive({
    dp_allfarms <-mean_dp_all()
    dp_allfarms$by="All farms"                                                           # put "All farms" in the column called "by"
    setnames(dp_allfarms, "by","")     
    dp_allfarms$`Original DP CI` <- dp_allfarms$ci_u-dp_allfarms$dp_3yr # rename column with suitable title for table
    setnames(dp_allfarms, "dp_3yr","Original DP")                  # rename column with suitable title for table
    dpnew_allfarms <-mean_dpnew_all()
    dpnew_allfarms$by="All farms"                                                           # put "All farms" in the column called "by"
    setnames(dpnew_allfarms, "by","")  # rename column balled "by to blank"   
    dpnew_allfarms$`New DP CI` <- dpnew_allfarms$ci_u-dpnew_allfarms$newdp # rename column with suitable title for table
    setnames(dpnew_allfarms, "newdp","New DP")                  # rename column with suitable title for table    
    dp_all2<-cbind(dp_allfarms,dpnew_allfarms)
    return(dp_all2)
  })
  
  
  #~~~~~~~~~~~~~~~~~~~~~ 
  # RPA data
  #~~~~~~~~~~~~~~~~~~~~~~~~
  
  aggdata <- reactive({
    agg_data <- aggregate(Loss_rpa()$loss, by=list(Loss_rpa()$pay_band), 
                       FUN=sum, na.rm=TRUE)
    colnames(agg_data) <- c("pay_band", "loss")
    agg_data$loss_mill <- agg_data$loss/1000000
    #agg_data <- agg_data[- which(agg_data$loss == 0),]
    return(agg_data)
  })
  
      aggdata_bps <- reactive({
        agg_data <- aggregate(Loss_rpa_bps()$loss, by=list(Loss_rpa_bps()$pay_band), 
                              FUN=sum, na.rm=TRUE)
        colnames(agg_data) <- c("pay_band", "loss")
        agg_data$loss_mill <- agg_data$loss/1000000
        #agg_data <- agg_data[- which(agg_data$loss == 0),]
        return(agg_data)
      })
      
  aggdata2 <- reactive({
    agg_data2 <- aggregate(Loss_rpa()$num_affected, by=list(Loss_rpa()$pay_band_money_lost), 
                        FUN=sum, na.rm=TRUE)
    colnames(agg_data2) <- c("money_lost", "number_of_farms")
    #agg_data2 <- agg_data2[- which(agg_data2$number_of_farms == 0),]
    return(agg_data2)
  })

  aggdata2_bps <- reactive({
    agg_data2 <- aggregate(Loss_rpa_bps()$num_affected, by=list(c(Loss_rpa_bps()$pay_band_money_lost,Loss_rpa_bps()$pay_band)), 
                           FUN=sum, na.rm=TRUE)
    colnames(agg_data2) <- c("money_lost", "number_of_farms")
    #agg_data2 <- agg_data2[- which(agg_data2$number_of_farms == 0),]
    return(agg_data2)
  })
  
  output$caption1 <- renderText({
    paste("RPA Saved: Â£",format(round(sum(Loss_rpa()$loss),0),big.mark=","),
          "(",
          format(round(100*sum(Loss_rpa()$loss/sum(Loss_rpa()$Total_Payment_pounds),0))),
          "%) from ",
          format(round(sum(Loss_rpa()$num_affected),0),big.mark=","),
          "Businesses (",
          format(round(100*(sum(Loss_rpa()$num_affected))/sum(Loss_rpa()$dummy))),
          "%)"
    )
  })
 
  output$caption1bps <- renderText({
    paste("RPA Saved: Â£",format(round(sum(Loss_rpa_bps()$loss),0),big.mark=","),
          "(",
          format(round(100*sum(Loss_rpa_bps()$loss/sum(Loss_rpa_bps()$Total_Payment_pounds),0))),
          "%) from ",
          format(round(sum(Loss_rpa_bps()$num_affected),0),big.mark=","),
          "Businesses (",
          format(round(100*(sum(Loss_rpa_bps()$num_affected))/sum(Loss_rpa_bps()$dummy))),
          "%)"
    )
  })
  
  output$caption1a <- renderText({
    paste("Total paid in 2016 = Â£",
          format(round(sum(Loss_rpa()$Total_Payment_pounds),0),big.mark=","),
          "to ",
          format(round(sum(Loss_rpa()$dummy),0),big.mark=","),
          " businesses"
    )
  })
  
  output$caption1abps <- renderText({
    paste("BPS in 2016 = Â£",
          format(round(sum(Loss_rpa_bps()$BPS_pounds),0),big.mark=",")
    )
  })
  output$caption1agreen <- renderText({
    paste("Greening in 2016 = Â£",
          format(round(sum(Loss_rpa_bps()$Greening_pounds),0),big.mark=",")
    )
  })
  
  output$caption1ayoung <- renderText({
    paste("Young farmer in 2016 = Â£",
          format(round(sum(Loss_rpa_bps()$Young_Farmer_pounds),0),big.mark=",")
    )
  })
  
  output$caption1b <- renderText({
    paste("Average loss = Â£",
          format(round(sum(Loss_rpa()$loss)/sum(Loss_rpa()$num_affected),0),big.mark=",")
    )
  })
  
  output$caption1bbps <- renderText({
    paste("Average loss = Â£",
          format(round(sum(Loss_rpa_bps()$loss)/sum(Loss_rpa_bps()$num_affected),0),big.mark=",")
    )
  })
  
  output$caption2 <- renderText({
    paste("Saved: Â£",
          format(round(sum(Loss_rpa()$loss),0),big.mark=","),
          ", ",
          format(round(100*sum(Loss_rpa()$loss/sum(Loss_rpa()$Total_Payment_pounds),0))),
          "% of payments")
  })

  output$caption2bps <- renderText({
    paste("Saved: Â£",
          format(round(sum(Loss_rpa_bps()$loss),0),big.mark=","),
          ", ",
          format(round(100*sum(Loss_rpa_bps()$loss/sum(Loss_rpa_bps()$Total_Payment_pounds),0))),
          "% of payments")
  })
  
  output$caption3 <- renderText({
    paste("Businesses affected = ",
          format(round(sum(Loss_rpa()$num_affected),0),big.mark=","),
          ", ",
          format(round(100*(sum(Loss_rpa()$num_affected))/sum(Loss_rpa()$dummy))),
          "% of businesses")
  })

  output$caption3bps <- renderText({
    paste("Businesses affected = ",
          format(round(sum(Loss_rpa_bps()$num_affected),0),big.mark=","),
          ", ",
          format(round(100*(sum(Loss_rpa_bps()$num_affected))/sum(Loss_rpa_bps()$dummy))),
          "% of businesses")
  })
  
  # lapply(1:7, function(nr){
  #   output[[paste0("data_var", nr)]] <- renderTable({
  #     data_var1()
  #   }, digits=0)
  #   })        

  
  
  output$data_var <- renderTable({
    data_var1()
  }, digits=0)  
  
  output$data_var1 <- renderTable({
    data_var2()
  }, digits=0)  
 
  output$data_var2 <- renderTable({
    data_var3()
  }, digits=0) 
  
  output$data_var3 <- renderTable({
    for_dp_chart()
  }, digits=0) 

  output$data_var4 <- renderTable({
    data_var4()
  }, digits=0) 
  
 output$data_var5 <- renderTable({
    data_var5()
  }, digits=0) 

 output$data_var6 <- renderTable({
   data_var6()
 }, digits=0) 
 
 output$data_var7 <- renderTable({
   data_var7()
 }, digits=0) 

 output$agg_data <- renderTable({
   aggdata()
 }, digits=0) 
 
 output$agg_data_red <- renderTable({
   aggdatared()
 }, digits=0) 
 
   
# output$plot1 <- renderPlotly({
#    plot_ly(data_var2(),
#            labels=interp(~x, x = as.name(input$fbsfac)),
#            values=~'Total loss (Â£m)',
#            type = 'pie') %>%
#      layout(title='Proportion of loss by farm type',
#             xaxis=list(showgrid= FALSE,
#                        zeroline= FALSE,
#                        showticklabels= FALSE),
#             yaxis=list(showgrid= FALSE,
#                        zeroline= FALSE,
#                        showticklabels= FALSE))
#  })
  
  output$plot2 <- renderPlot({
    ggplot(for_dp_chart(), aes_string(x = input$fbsfac, y="value", fill="variable"))+
      geom_bar(stat="identity", position = "dodge")+
      theme_bw()+
      xlab("")+ylab("Percent")+
      geom_text(aes(label=round(value,0)), vjust=-0.5, position=position_dodge(.9), size=4)+
      theme(axis.text.x = element_text(size=10))+
      theme(axis.text.y = element_text(size=10))+
      theme(axis.title.x = element_text(size=10))+
      theme(axis.title.y = element_text(size=10))+
      scale_x_discrete(labels=function(x) str_wrap(x,width = 10))+
      scale_fill_manual( name=element_blank( ), #legend title
                         values = c( "#FFCC00", "#77BC1F" ) )+
      theme(legend.position='bottom')
         })
  
  output$plot3 <- renderPlot({
    ggplot(aggdata(), aes(x = pay_band, y=loss_mill))+
      geom_bar(stat="identity", fill="#00A33B")+
      theme_bw()+
      xlab("Total Direct Payment band")+ylab("Loss (Â£ million)")+
      geom_text(aes(label=paste0("Â£",round(loss_mill,1),"m")), vjust=-0.5,color = "black", size=4.5)+
      ggtitle("Progressive payment cap: Expenditure saved by payment band")+
      theme(axis.text.x = element_text(size=14))+
      theme(axis.text.y = element_text(size=14))+
      theme(axis.title.x = element_text(size=14))+
      theme(axis.title.y = element_text(size=14))+
      theme(plot.title = element_text(size=14,face="bold",hjust=0.5))+
      scale_x_discrete(labels=function(x) str_wrap(x,width = 7))+
      scale_y_continuous(labels=dollar_format(prefix="Â£"))
  })

  output$plot4 <- renderPlot({
    ggplot(aggdata2(), aes(x = money_lost, y=number_of_farms))+
      geom_bar(stat="identity", fill="#00A33B")+
      theme_bw()+
      xlab("Payment reduction")+ylab("Number of businesses")+
      geom_text(aes(label=number_of_farms), vjust=-0.5,color = "black", size=4.5)+
      #ggtitle("Progressive payment cap: Businesses by payment reduction")+
      theme(axis.text.x = element_text(size=14))+
      theme(axis.text.y = element_text(size=14))+
      theme(axis.title.x = element_text(size=14))+
      theme(axis.title.y = element_text(size=14))+
      theme(plot.title = element_text(size=14,face="bold",hjust=0.5))+
      scale_x_discrete(labels=function(x) str_wrap(x,width = 7))+
      scale_y_continuous(labels=comma)
  })
  
  output$plot3a <- renderPlot({
    ggplot(aggdata_bps(), aes(x = pay_band, y=loss_mill))+
      geom_bar(stat="identity", fill="#00A33B")+
      theme_bw()+
      xlab("Total Direct Payment band")+ylab("Loss (Â£ million)")+
      geom_text(aes(label=paste0("Â£",round(loss_mill,1),"m")), vjust=-0.5,color = "black", size=4.5)+
      ggtitle("Progressive payment cap: Expenditure saved by payment band")+
      theme(axis.text.x = element_text(size=14))+
      theme(axis.text.y = element_text(size=14))+
      theme(axis.title.x = element_text(size=14))+
      theme(axis.title.y = element_text(size=14))+
      theme(plot.title = element_text(size=14,face="bold",hjust=0.5))+
      scale_x_discrete(labels=function(x) str_wrap(x,width = 7))+
      scale_y_continuous(labels=dollar_format(prefix="Â£"))
  })
  
  output$plot4a <- renderPlot({
    ggplot(aggdata2_bps(), aes(x = money_lost, y=number_of_farms))+
      geom_bar(stat="identity", fill="#00A33B")+
      theme_bw()+
      xlab("Payment reduction")+ylab("Number of businesses")+
      geom_text(aes(label=number_of_farms), vjust=-0.5,color = "black", size=4.5)+
      ggtitle("Progressive payment cap: Number of businesses by payment reduction")+
      theme(axis.text.x = element_text(size=14))+
      theme(axis.text.y = element_text(size=14))+
      theme(axis.title.x = element_text(size=14))+
      theme(axis.title.y = element_text(size=14))+
      theme(plot.title = element_text(size=14,face="bold",hjust=0.5))+
      scale_x_discrete(labels=function(x) str_wrap(x,width = 7))+
      scale_y_continuous(labels=comma)
  })
  
  }


shinyApp(ui, server)
