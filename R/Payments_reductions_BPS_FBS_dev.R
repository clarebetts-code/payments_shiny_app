# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Built using R version 3.6.2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pacman::p_load(dplyr,
               forcats,
               stringr,
               survey,
               FBSCore,
               tibble,
               shiny,
               #akima,
               shinycssloaders,
               plotly,
               ggplot2,
               DT,
               mosaic)


# read in support functions
source("C:\\Users\\m994810\\Desktop\\Payments reductions shiny app\\Payments reduction app for git\\R\\Payments_reductions_support_functions.R")
#source("K:\\TASPrototype\\FBSmastercopy\\FBS_ADHOC_DATA_REQUESTS\\EU Exit\\Payments reductions shiny app\\Payments reduction app for git\\R\\Payments_reductions_support_functions.R")

# load data
# 2019 BPS data
rpa_year <- 2019
RPA_data <- readRDS("C:\\Users\\m994810\\Desktop\\Payments reductions shiny app\\Data\\20200520 BPS 2019.Rds")
# 2015/16-2017/18 FBS data
fbs_3yr <- readRDS("C:\\Users\\m994810\\Desktop\\Payments reductions shiny app\\Data\\fbs_england_3yr_15_17.Rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Do some processing on the FBS data - calculate 3 year averages
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# function to calculate 3 year average
av_3year <- function(dat, item, years = 2015:2017){ 
  
  years %>%
    substr(start = 3, stop = 4) %>%
    paste0("X.", ., item) %>%
    subset(dat, select = .) %>%
    rowMeans()
}


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
                             ordered_result = TRUE)),
    farm_size = forcats::fct_relevel(farm_size,
                                     "Spare/Part-time",
                                     "Small",
                                     "Medium",
                                     "Large",
                                     "Very large"),
    type_red = forcats::fct_relevel(type_red,
                                    "Cereals & General cropping",
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
                               "Mainly SDA"),
    
    #    Calculate the 3 year averages
    fbi_3yr = av_3year(.,"farm_business_income"),
    agout_3yr = av_3year(.,"output_from_agriculture"),
    aginc_3yr = av_3year(.,"agriculture_input_costs"),
    dp_3yr = av_3year(.,"basic_payment_scheme"),
    dp_costs_3yr = av_3year(.,"BPS_costs"),
    
    # Remove SPS from income
    # SPS cost centre
    dp_cc_3yr = dp_3yr - dp_costs_3yr,
    #FBI minus SPS cost centre
    fbi_min_dp_cc_3yr = fbi_3yr - dp_cc_3yr)  -> fbs_3yr

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define UI for application
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#####
ui <- fluidPage(

  # Application title
  titlePanel(windowTitle = "Farm Business Survey",
             title = ""),

      div(class = "mbie-brand",
          tags$a(class = "mbie-brand",
                 title = "DEFRA logo",
                 tags$img(src = "DEFRA_logo.png",
                          alt = "DEFRA"))),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    position = "left",
    fluid = FALSE,
    sidebarPanel(
      width = 3,
      selectInput("fbsfac",
                  label = h3("Choose a factor:"),
                  choices = list("type",
                                 "type_red",
                                 "farm_size",
                                 "tenancy",
                                 "gor",
                                 "lfa",
                                 "fbi_band",
                                 "fbi_band1",
                                 "fbi_exc_DP",
                                 "age_band",
                                 "dp_band"),
                  selected = "type"),
      # the div parts set up 2 adjacent columns each with width 140 px
      div(style="display: inline-block;vertical-align:top; width: 100px;",
          numericInput("Level_1",
                       "Upto Level 1:",
                        value=30000,step = 1000),
           numericInput("Level_2",
                        "Upto Level 2:",
                        value=50000,step = 1000),
           numericInput("Level_3",
                        "Upto Level 3:",
                        value=150000,step = 1000),
           numericInput("Level_4",
                        "Upto Level 4:",
                        value=1000000,step = 5000),
           numericInput("Level_5",
                        "Upto Level 5:",
                        value=10000000,step = 5000)
       ),
       div(style="display: inline-block;vertical-align:top; width: 100px;",
           numericInput("prop_1",
                        "Percentage 1:", 3),
           numericInput("prop_2",
                        "Percentage 2:", 8),
           numericInput("prop_3",
                        "Percentage 3:", 11),
           numericInput("prop_4",
                        "Percentage 4:", 15),
           numericInput("prop_5",
                        "Percentage 5:", 15)
       )
     ),
      
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Expenditure impact",
                             # two columns for the summary statements
                             h2(paste0("Source: Rural Payments Agency, payments data for ",
                                       rpa_year,
                                       " scheme year")),
                             fluidRow(
                               h3("Reductions applied to TOTAL payment [BPS, Greening & Young Farmer]"),
                               column(5,
                                      h4(spinner(textOutput("caption1a", container = span)))
                               ),
                               column(5,
                                      h4(textOutput("caption1b", container = span))
                               )
                             ),
                             h3("Total loss of payments by payment band:"),
                             spinner(plotlyOutput("table4_plot")),
                             h3("Businesses affected by payment reduction band:"),
                             spinner(plotlyOutput("table5_plot")),
                             h3("Data:"),
                             fluidRow(
                               column(4,
                                      spinner(DT::dataTableOutput("table_4"))),
                               column(4,
                                      spinner(DT::dataTableOutput("table_5")))
                             )
                             
                    ),
                    
                    tabPanel("FBI impact", 
                             h2("Source: Farm Business Survey 3 year matched dataset 2016/17 - 2018/19"),
                             h3("Average Farm Business Income (FBI) before and after reduction:"),
                             spinner(plotlyOutput("table2_plot1")),
                             h3("Proportion of the population with a FBI less than £0:"),
                             spinner(plotlyOutput("table2_plot2")),
                             h3("Data:"),
                             spinner(DT::dataTableOutput("table_2"))
                             ),
                    
                    tabPanel("Payment recieved \nimpact", 
                             h2("Source: Farm Business Survey 3 year matched dataset 2016/17 - 2018/19"),
                             h3("Average payment before and after reduction:"),
                             spinner(plotlyOutput("table3_plot1")),
                             h3("Direct payments as a proportion of FBI:"),
                             spinner(plotlyOutput("table3_plot2")),
                             h3("Data:"),
                             spinner(DT::dataTableOutput("table_3"))
                             #spinner(plotlyOutput("table3_plot"))
                             ),
 
                    tabPanel("Next...")
                    
                    
                    )
        )
    )
  )

  
#####

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define server logic 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
server <- function(input, output) {
  
  
  # function to simplify the code to calculate the totals
  total_byfac_new <- function(vars, factor = input$fbsfac, design = fbsdesign()){
    FBSCore::total_byfac(vars, factor, design)
  }
  
  # function to simplify the code to calculate the means
  mean_byfac_new <- function(vars, factor = input$fbsfac, design = fbsdesign()){
    FBSCore::mean_byfac(vars, factor, design)
  }
  
  # function to simplify the code to calculate the means
  ratio_byfac_new <- function(numerators, denominators, factor = input$fbsfac, design = fbsdesign()){
    FBSCore::ratio_byfac(numerators, denominators, factor, design)
  }
  
  
  # functions to apply payment reductions  
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
  
  
  # function to format tables nicely
  nice_table <- function(x){
    DT::datatable(x,
                  extensions = 'Buttons',
                  options = list(
                    #dom = 'Brflit',
                    dom = 'frtBip', #buttons at bottom, just before the 'showing x to x of x entries'
                    paging = FALSE,
                    searching = FALSE,
                    fixedColumns = FALSE,
                    autoWidth = TRUE,
                    #columnDefs = list(list(width = '200px', targets = c(1, 3))),
                    ordering = FALSE,
                    buttons = c('copy', 'csv')),
                  rownames = NULL) %>%
      DT::formatRound(columns=as.numeric(), digits=0)
  }
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # CALCULATIONS
  
  # This calculates the loss for each business using RPA data based on the TOTAL PAYMENT
  Loss_rpa <- reactive ({

    RPA_data %>%
      # remove any rows with NA in payment column
      tidyr::drop_na(c(NET_PAY, BPS_NET, GREENING_NET, YF_NET)) %>%
      # only those where payments were greater than 0
      subset(NET_PAY_GBP > 0) %>%
      mutate(
        loss = dplyr::case_when(
          # if payment is less than first level, reduce by prop_1
          NET_PAY_GBP < input$Level_1 ~
            band_1_reduction(NET_PAY_GBP),
          # if payment is between level 1 and level 2
          dplyr::between(NET_PAY_GBP, input$Level_1 + 0.01, input$Level_2) ~
            band_2_reduction(NET_PAY_GBP),
          # if payment is between level 2 and level 3
          dplyr::between(NET_PAY_GBP, input$Level_2 + 0.01, input$Level_3) ~
            band_3_reduction(NET_PAY_GBP),
          # if payment is between level 3 and level 4
          dplyr::between(NET_PAY_GBP, input$Level_3 + 0.01, input$Level_4) ~
            band_4_reduction(NET_PAY_GBP),
          # if payment is between level 4 and level 5
          dplyr::between(NET_PAY_GBP, input$Level_4 + 0.01, input$Level_5) ~
            band_5_reduction(NET_PAY_GBP)
        ),
        # bands for original payments
        pay_band = cut(NET_PAY_GBP, 
                       breaks = c(-Inf, 5000, 10000, 15000, 20000, 25000, 30000, 40000, 50000, 75000,  100000, 125000, 150000, 200000, 250000, 300000, Inf), 
                       labels = c("0 to 5k", "5 to 10k", "10 to 15k", "15 to 20k", "20 to 25k", "25 to 30k", "30 to 40k", "40 to 50k", "50 to 75k", "75 to 100k", "100 to 125k", "125 to 150k","150 to 200k", "200k to 250k", "250k to 300k", "over 300k"), 
                       levels =c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),
                       right = TRUE,
                       ordered_result = TRUE),
        # bands for money lost
        pay_band_money_lost = cut(loss, 
                                  breaks = c(-Inf, 0.001, 5000, 10000, 15000, 20000, 25000, 30000, 40000, 50000, 75000,  100000, 125000, 150000, 200000, 250000, 300000, Inf), 
                                  labels = c("No loss", "0 to 5k", "5 to 10k", "10 to 15k", "15 to 20k", "20 to 25k", "25 to 30k", "30 to 40k", "40 to 50k", "50 to 75k", "75 to 100k", "100 to 125k", "125 to 150k","150 to 200k", "200k to 250k", "250k to 300k","over 300k"), 
                                  levels =c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17),
                                  right = TRUE,
                                  ordered_result = TRUE),
        # number affected
        num_affected = ifelse(loss==0, 0, 1),
        
        # dummy variable to help calculate proportions
        dummy = 1
        
      )

  })
  
  # This calculates the loss for each business using FBS data
  Loss_fbs <- reactive ({
    
    fbs_3yr %>%
      dplyr::mutate(
        loss = dplyr::case_when(
          # if payment is less than first level, reduce by prop_1
          dp_3yr <= input$Level_1 ~ 
            band_1_reduction(dp_3yr),
          # if payment is between level 1 and level 2
          dplyr::between(dp_3yr, input$Level_1+1, input$Level_2) ~ 
            band_2_reduction(dp_3yr),
          # if payment is between level 2 and level 3
          dplyr::between(dp_3yr, input$Level_2+1, input$Level_3) ~ 
            band_3_reduction(dp_3yr),
          # if payment is between level 3 and level 4   
          dplyr::between(dp_3yr, input$Level_3+1, input$Level_4) ~ 
            band_4_reduction(dp_3yr),
          # if payment is between level 4 and level 5
          dplyr::between(dp_3yr, input$Level_4+1, input$Level_5) ~ 
            band_5_reduction(dp_3yr)
        ),
        
        # marker for affected farms
        num_affected = ifelse(loss==0, 0, 1),
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
        neg_fbi = ifelse(fbi_3yr < 0, 1, 0),
        # marker for farms with negative FBI after loss of payment
        negnew_fbi = ifelse(new_fbi < 0, 1, 0),
        # marker for farms with FBI < 10k
        lt_10k_fbi = ifelse(fbi_3yr < 10000, 1, 0),
        # marker for farms with FBI < 10k after loss of payment
        under10knew_fbi = ifelse(new_fbi < 10000, 1, 0),
        # marker for farms with FBI < 25k
        lt_25k_fbi = ifelse(fbi_3yr < 25000, 1, 0),
        # marker for farms with FBI < 25k after loss of payment
        under25knew_fbi = ifelse(new_fbi < 25000, 1, 0),
        # marker for farms with FBI < 50k
        lt_50k_fbi = ifelse(fbi_3yr < 50000, 1, 0),
        # marker for farms with FBI < 50k after loss of payment
        under50knew_fbi = ifelse(new_fbi < 50000, 1, 0),
        
        # sets up a factor for farms by FBI (original)
        fbi_band = cut(fbi_3yr,  
                       breaks = c(-Inf,0,10000,25000,Inf), 
                       labels = c("less than zero", "0 to 10k","10 to 25k","Over 25k"), 
                       levels =c(1,2,3,4),
                       right = TRUE,
                       ordered_result = TRUE),
        # sets up a factor for farms by FBI (original)
        fbi_band1 = cut(fbi_3yr,
                        breaks = c(-Inf,0,10000,25000,50000,Inf), 
                        labels = c("less than zero", "0 to 10k","10 to 25k","25 to 50k","Over 50k"), 
                        levels =c(1,2,3,4),
                        right = TRUE,
                        ordered_result = TRUE),
        # sets up a factor for farms by FBI (original) excluding direct payment
        fbi_exc_DP = cut(fbi_min_dp_cc_3yr, 
                        breaks = c(-Inf,0,10000,20000,30000,40000,50000,100000,Inf), 
                        labels = c("less than zero", "0 to 10k","10 to 20k","20 to 30k","30 to 40k","40 to 50k","50 to 100k","Over 100k"), 
                        levels =c(1,2,3,4,5,6,7,8),
                        right = TRUE,
                        ordered_result = TRUE),
        # sets up a factor for farms by direct payment (original)
        dp_band = cut(dp_3yr,
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
                      ordered_result = TRUE),
        # dummy variable to help calculate proportions
        dummy = 1
      )
  })
  
  # sets up the survey design
  fbsdesign <- reactive({svydesign(id= ~Loss_fbs()$farms,
                                   strata= ~Loss_fbs()$stratum,
                                   fpc=~Loss_fbs()$num_pop,
                                   data= Loss_fbs(),
                                   weight= ~Loss_fbs()$newwt3yr,
                                   nest=TRUE)
    
  })
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # OUTPUTS
  
  # summary of real RPA data
  # Direct payments
  output$caption1a <- renderText({
    paste("Total paid in ",
          rpa_year,
          " = £",
          format(round(sum(Loss_rpa()$NET_PAY_GBP),0),big.mark=","),
          "to ",
          format(length(unique(Loss_rpa()$FRN)),big.mark=","),
          " businesses. Of which ",
          round(100*(sum(Loss_rpa()$BPS_NET_GBP)/sum(Loss_rpa()$NET_PAY_GBP)), digits = 1),
          "% were basic payments, ",
          round(100*(sum(Loss_rpa()$GREENING_NET_GBP)/sum(Loss_rpa()$NET_PAY_GBP)), digits = 1),
          "% were greening payments, and",
          round(100*(sum(Loss_rpa()$YF_NET_GBP)/sum(Loss_rpa()$NET_PAY_GBP)), digits = 1),
          "% were young farmer payments."
    )
  })
  
  # summary of RPA after reduction:
  # Direct payments
  output$caption1b <- renderText({
    paste0("RPA Saved: £", format(round(sum(Loss_rpa()$loss),0), big.mark=","),
          " (",
          format(round(100*sum(Loss_rpa()$loss/sum(Loss_rpa()$NET_PAY_GBP),0))),
          "%) from ",
          format(round(sum(Loss_rpa()$num_affected),0),big.mark=","),
          " Businesses (",
          format(round(100*(sum(Loss_rpa()$num_affected))/sum(Loss_rpa()$dummy))),
          "%)"
    )
  })
  

  ## Payments recieved impact - FBS data
  table3 <- reactive({

    mean_byfac_new(c("dp_3yr", "newdp"))[,c(1:3,7:8)] %>% {
      data.frame(fbsfac = .[[1]],
             Average_DP = .[[2]],
             CI_Average_DP = .[[4]],
             Average_new_DP = .[[3]],
             CI_Average_new_DP = .[[5]])
    } %>%
      bind_cols(.,
                ratio_byfac_new(numerators = c("dp_3yr", "newdp"), denominators = c("fbi_3yr", "new_fbi")) %>% {
                  data.frame(DP_prop_FBI = .[[2]],
                         CI_DP_prop_FBI = .[[7]],
                         new_DP_prop_new_FBI = .[[3]],
                         CI_new_DP_prop_FBI = .[[8]])
                })
  })

  output$table_3 <- DT::renderDataTable({

    table3() %>%
    dplyr::rename('Direct payment (£ per farm)' = 2,
                  '\u00B1 95% CI DP' = 3,
                  'Direct payment after payment loss (£ per farm)' = 4,
                  '\u00B1 95% CI DP after payment loss' = 5,
                  'Direct payment as a proportion of FBI' = 6,
                  '\u00B1 95% CI DP as prop FBI' = 7,
                  'Direct payment as a proportion of FBI after payment loss (£ per farm)' = 8,
                  '\u00B1 95% CI DP as prop FBI after payment loss' = 9) %>%
      nice_table() %>%
      DT::formatRound(columns=c(2:5), digits=0) %>%
      DT::formatPercentage(columns = 6:9)

  })

  output$table3_plot1 <- renderPlotly({

    (table3() %>%
       {data.frame(fbsfac = rep(.[[1]], 2),
               group = rep(c("Average DP", "Average new DP"), each = length(.[[1]])),
               y = c(.[[2]], .[[4]]),
               CI = c(.[[3]], .[[5]]))} %>%
       mutate(fbsfac = factor(fbsfac, levels = c(levels(Loss_fbs()[[input$fbsfac]]), "All"))) %>%
       ggplot(aes(x= fbsfac, y = y, fill = group)) +
       geom_bar(position = "dodge", stat="identity") +
       geom_errorbar(aes(ymin = y-CI, ymax = y+CI), position = "dodge", stat="identity") +
       theme(legend.position="bottom") +
       scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
       scale_fill_manual(values=colours.helper(c("one", "two"))) +
       labs(y = "£ per farm", x = "")
     ) %>%
      ggplotly(tooltip = c("group","y","x")) %>%
      layout(legend = list(orientation = "h",
                           y = -0.2, x = 0
      ))

  })

  output$table3_plot2 <- renderPlotly({

    (table3() %>%
       {data.frame(fbsfac = rep(.[[1]], 2),
               group = rep(c("Average DP",
                             "Average new DP"),
                           each = length(.[[1]])),
               y = c(.[[6]], .[[8]]),
               CI = c(.[[7]], .[[9]]))} %>%
       mutate(fbsfac = factor(fbsfac, levels = c(levels(Loss_fbs()[[input$fbsfac]]), "All"))) %>%
       ggplot(aes(x= fbsfac, y = y, fill = group)) +
       geom_bar(position = "dodge", stat="identity") +
       geom_errorbar(aes(ymin = y-CI, ymax = y+CI), position = "dodge", stat="identity") +
       theme(legend.position="bottom") +
       scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
       scale_y_continuous(labels = scales::percent) +
       scale_fill_manual(values=colours.helper(c("one", "two"))) +
       labs(y = "Proportion of Farm Business Income (FBI)", x = "")
    ) %>%
      ggplotly(tooltip = c("group","y","x")) %>%
      layout(legend = list(orientation = "h",
                           y = -0.2, x = 0
                           ))
    })
  

  ## FBI impact - FBS data
  table2 <- reactive({
    mean_byfac_new(c("fbi_3yr", "new_fbi"))[,c(1:3,7:8)] %>% {
      data.frame(fbsfac = .[[1]],
             Average_FBI = .[[2]],
             CI_Average_FBI = .[[4]],
             Average_new_FBI = .[[3]],
             CI_Average_new_FBI = .[[5]])
    } %>%
      cbind(., 
            total_byfac_new(c("neg_fbi", "negnew_fbi", "dummy")) %>% {
              data.frame(prop_neg = .[[2]]/.[[4]],
                     new_prop_neg = .[[3]]/.[[4]])
            },
            c(mosaic::sum(Loss_fbs()$neg_fbi ~ (Loss_fbs())[,input$fbsfac]), "All" = sum(Loss_fbs()$neg_fbi)),
            c(mosaic::sum(Loss_fbs()$negnew_fbi ~ (Loss_fbs())[,input$fbsfac]), "All" = sum(Loss_fbs()$negnew_fbi))
      )
  })
      
  output$table_2 <- DT::renderDataTable({
    
    table2() %>%
      dplyr::rename('Farm Business Income (£ per farm)' = 2,
                    '\u00B1 95% CI FBI' = 3,
                    'Proportion of farms with FBI < 0' = 6,
                    'Number in sample' = 8,
                    'Farm Business Income after payment loss (£ per farm)' = 4,
                    '\u00B1 95% CI FBI after payment loss' = 5,
                    'Proportion of farms with FBI < 0 after payment loss' = 7,
                    'Number in sample after payment loss' = 9) %>%
      .[c(1,2,3,6,8,4,5,7,9)] %>% # re-order columns
      nice_table() %>%
      DT::formatRound(columns=c(2,3,6,7), digits=0) %>%
      DT::formatPercentage(columns = c(4, 8))
    
      
  })
  
  output$table2_plot1 <- renderPlotly({
    
    (table2() %>%
       {data.frame(fbsfac = rep(.[[1]], 2),
               group = rep(c("Average FBI", "Average new FBI"), each = length(.[[1]])),
               y = c(.[[2]], .[[4]]),
               CI = c(.[[3]], .[[5]]))} %>%
       mutate(fbsfac = factor(fbsfac, levels = c(levels(Loss_fbs()[[input$fbsfac]]), "All"))) %>%
       ggplot(aes(x= fbsfac, y = y, fill = group)) +  
       geom_bar(position = "dodge", stat="identity") +
       geom_errorbar(aes(ymin = y-CI, ymax = y+CI), position = "dodge", stat="identity") +
       theme(legend.position="bottom") +
       scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
       scale_fill_manual(values=colours.helper(c("one", "two"))) +
       labs(y = "£ per farm", x = "") 
    ) %>%
      ggplotly(tooltip = c("group","y","x")) %>%
      layout(legend = list(orientation = "h",
                           y = -0.2, x = 0
      ))
    
  })
  
  output$table2_plot2 <- renderPlotly({
    
    (table2() %>%
       {data.frame(fbsfac = rep(.[[1]], 2),
               group = rep(c("Average FBI", 
                             "Average new FBI"), 
                           each = length(.[[1]])),
               y = c(.[[6]], .[[7]]))} %>%
       mutate(fbsfac = factor(fbsfac, levels = c(levels(Loss_fbs()[[input$fbsfac]]), "All"))) %>%
       ggplot(aes(x= fbsfac, y = y, fill = group)) +  
       geom_bar(position = "dodge", stat="identity") +
       theme(legend.position="bottom") +
       scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
       scale_y_continuous(labels = scales::percent) +
       scale_fill_manual(values=colours.helper(c("one", "two"))) +
       labs(y = "Proportion of farms with FBI < 0", x = "") 
    ) %>%
      ggplotly(tooltip = c("group","y","x")) %>%
      layout(legend = list(orientation = "h",
                           y = -0.2, x = 0
      ))
  })
  
  
  ## Expenditure impact - RPA data
  table4 <- reactive({
    
    data.frame(pay_band = levels(Loss_rpa()$pay_band),
           expenditure_saved = mosaic::sum(Loss_rpa()$loss ~ Loss_rpa()$pay_band)) %>%
      mutate(expenditure_saved_m = expenditure_saved/1000000,
             label = paste("£", round(expenditure_saved_m, 1), "m", sep = ""),
             pay_band = factor(pay_band, levels = levels(Loss_rpa()$pay_band)))
    
  })

  output$table_4 <- DT::renderDataTable({

    table4() %>%
      select('Payment band' = 1,
             'Total loss of payments' = 2) %>%
      nice_table() %>%
      DT::formatRound(columns = 2, digits = 0)

  })

  output$table4_plot <- renderPlotly({

    (table4() %>%
       ggplot(aes(x= pay_band, y = expenditure_saved_m)) +
       geom_bar(position = "dodge", stat="identity", fill = "#416146") +
       scale_x_discrete(labels = function(x) str_wrap(x, width = 9)) +
       #scale_fill_manual(values=colours.helper(c("one"))) +
       labs(y = "Total loss of payments (£ million)", x = "Direct payment band") +
       geom_text(
         aes(label = label, y = expenditure_saved_m + 0.2),
         position = position_dodge(0.9),
         vjust = 0,
         size = 3
       )) %>%
      ggplotly(tooltip = c("group","y","x"))

  })

  table5 <- reactive({
    
    data.frame(payment_reduction_band = factor(levels(Loss_rpa()$pay_band_money_lost)),
             number_of_businesses = mosaic::sum(dummy ~ pay_band_money_lost, data = Loss_rpa())) %>%
    mutate(payment_reduction_band = factor(payment_reduction_band, levels = levels(Loss_rpa()$pay_band_money_lost)))
    
  })
  
  output$table_5 <- DT::renderDataTable({
    
    table5() %>%
      select('Payment reduction band' = 1,
             'Number of businesses' = 2) %>%
      nice_table() %>%
      DT::formatRound(columns = 2, digits = 0)
    
  })
  
  output$table5_plot <- renderPlotly({
    
    (table5() %>%
       ggplot(aes(x= payment_reduction_band, y = number_of_businesses)) +  
       geom_bar(position = "dodge", stat="identity", fill = "#416146") +
       scale_x_discrete(labels = function(x) str_wrap(x, width = 9)) +
       #scale_fill_manual(values=colours.helper(c("one"))) +
       labs(y = "Number of businesses", x = "Loss of direct payment") +
       geom_text(
         aes(label = number_of_businesses, y = number_of_businesses + 1500),
         position = position_dodge(0.9),
         vjust = 0,
         size = 3
       )) %>%
      ggplotly(tooltip = c("group","y","x")) 
    
  })
     

}

shinyApp(ui, server)
