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
#source("C:\\Users\\m994810\\Desktop\\Payments reductions shiny app\\Payments reduction app for git\\R\\Payments_reductions_support_functions.R")
source("K:\\TASPrototype\\FBSmastercopy\\FBS_ADHOC_DATA_REQUESTS\\EU Exit\\Payments reductions shiny app\\Payments reduction app for git\\R\\Payments_reductions_support_functions.R")

# load data
# 2019 BPS data
rpa_year <- 2019
# Need to adjust the payment lost band depending on sample sizes (any categories <5)
RPA_data <- readRDS("C:\\Users\\m994810\\Desktop\\Payments reductions shiny app\\Data\\20200520 BPS 2019.Rds")
#RPA_data <- readRDS("K:\\TASPrototype\\FBSmastercopy\\FBS_ADHOC_DATA_REQUESTS\\EU Exit\\Payments reductions shiny app\\Data\\20200520 BPS 2019.Rds")
# 2016/17-2018/19 FBS data
fbs_3yr <- readRDS("C:\\Users\\m994810\\Desktop\\Payments reductions shiny app\\Data\\fbs_england_3yr_16_18.Rds")
#fbs_3yr <- readRDS("K:\\TASPrototype\\FBSmastercopy\\FBS_ADHOC_DATA_REQUESTS\\EU Exit\\Payments reductions shiny app\\Data\\fbs_england_3yr_16_18.Rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Do some processing on the FBS data - calculate 3 year averages
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fbs_3yr %>%
  dplyr::mutate(
    # group some farm types to improve sample size
    type.red = forcats::fct_recode(type,
                                   `Pigs & Poultry` =  "Pigs",
                                   `Pigs & Poultry`="Poultry",
                                   `Cereals & General cropping`="Cereals",
                                   `Cereals & General cropping`="General cropping"),
    # merge spare time and part time
    farm.size = forcats::fct_recode(slrgroup,
                                    `Spare/Part-time` =  "Spare-time",
                                    `Spare/Part-time` =  "Part-time"),
    # put ages into age bands
    age.band = as.factor(cut(X.16age.of.farmer, 
                             breaks = c(-10000000, 40, 50, 60, 70, Inf), 
                             labels = c("Under 40", "40-49", "50-59", "60-69", "70 and over"), 
                             levels =c(1,2,3,4,5),
                             right = FALSE,
                             ordered_result = TRUE)),
    farm.size = forcats::fct_relevel(farm.size,
                                     "Spare/Part-time",
                                     "Small",
                                     "Medium",
                                     "Large",
                                     "Very large"),
    type.red = forcats::fct_relevel(type.red,
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
    fbi.3yr = av_3year(.,"farm.business.income"),
    agout.3yr = av_3year(.,"output.from.agriculture"),
    aginc.3yr = av_3year(.,"agriculture.input.costs"),
    dp.3yr = av_3year(.,"basic.payment.scheme"),
    dp.costs.3yr = av_3year(.,"BPS.costs"),
    
    # Remove SPS from income
    # SPS cost centre
    dp.cc.3yr = dp.3yr - dp.costs.3yr,
    #FBI minus SPS cost centre
    fbi.min.dp.cc.3yr = fbi.3yr - dp.cc.3yr)  -> fbs_3yr

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
                                 "type.red",
                                 "farm.size",
                                 "tenancy",
                                 "gor",
                                 "lfa",
                                 "fbi.band",
                                 "fbi.band1",
                                 "fbi.exc.DP",
                                 "age.band",
                                 "dp.band"),
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
                             h3("Figure 1: Total loss of payments by payment band:"),
                             spinner(plotlyOutput("table4_plot")),
                             h3("Figure 2: Businesses affected by payment reduction band:"),
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
                             h3("Figure 3: Average Farm Business Income (FBI) before and after reduction:"),
                             h4("Instances of fewer than 5 farms have been redacted and appear as zero"),
                             spinner(plotlyOutput("table2_plot1")),
                             h3("Figure 4: Proportion of the population with a FBI less than £0:"),
                             h4("Instances of fewer than 5 farms have been redacted and appear as zero"),
                             spinner(plotlyOutput("table2_plot2")),
                             h3("Figure 5: Proportion of the population with a FBI less than £10k:"),
                             h4("Instances of fewer than 5 farms have been redacted and appear as zero"),
                             spinner(plotlyOutput("table2_plot3")),
                             h3("Figure 3 Data:"),
                             h4("Instances of fewer than 5 farms have been redacted and appear as zero"),
                             spinner(DT::dataTableOutput("table_2a")),
                             h3("Figure 4 & 5 Data:"),
                             h4("Instances of fewer than 5 farms have been redacted and appear as zero"),
                             spinner(DT::dataTableOutput("table_2b"))
                    ),
                    
                    tabPanel("Payment recieved \nimpact", 
                             h2("Source: Farm Business Survey 3 year matched dataset 2016/17 - 2018/19"),
                             h3("Figure 5: Average payment before and after reduction:"),
                             h4("Instances of fewer than 5 farms have been redacted and appear as zero"),
                             spinner(plotlyOutput("table3_plot1")),
                             h3("Figure 6: Direct payments as a proportion of FBI:"),
                             h4("Instances of fewer than 5 farms have been redacted and appear as zero"),
                             spinner(plotlyOutput("table3_plot2")),
                             h3("Data:"),
                             h4("Instances of fewer than 5 farms have been redacted and appear as zero"),
                             spinner(DT::dataTableOutput("table_3"))
                             #spinner(plotlyOutput("table3_plot"))
                             )
                    
                    
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
    FBSCore::total_byfac(vars, factor, design) %>%
      small_sample_checker()
  }
  

  # function to simplify the code to calculate the means
  mean_byfac_new <- function(vars, factor = input$fbsfac, design = fbsdesign()){
    FBSCore::mean_byfac(vars, factor, design) %>%
      small_sample_checker()
  }
  
  # function to simplify the code to calculate the means
  ratio_byfac_new <- function(numerators, denominators, factor = input$fbsfac, design = fbsdesign()){
    FBSCore::ratio_byfac(numerators, denominators, factor, design) %>%
      small_sample_checker()
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
                  rownames = NULL) 
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
                                  breaks = c(-Inf, 0.001, 5000, 10000, 15000, 20000, 25000, 30000, 40000, 50000, 75000,  100000, 125000, 150000, Inf), 
                                  labels = c("No loss", "0 to 5k", "5 to 10k", "10 to 15k", "15 to 20k", "20 to 25k", "25 to 30k", "30 to 40k", "40 to 50k", "50 to 75k", "75 to 100k", "100 to 125k", "125 to 150k","over 150"), 
                                  levels =c(1,2,3,4,5,6,7,8,9,10,11,12,13,14),
                                  right = TRUE,
                                  ordered_result = TRUE),
        # number affected
        num.affected = ifelse(loss==0, 0, 1),
        
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
          dp.3yr <= input$Level_1 ~ 
            band_1_reduction(dp.3yr),
          # if payment is between level 1 and level 2
          dplyr::between(dp.3yr, input$Level_1+1, input$Level_2) ~ 
            band_2_reduction(dp.3yr),
          # if payment is between level 2 and level 3
          dplyr::between(dp.3yr, input$Level_2+1, input$Level_3) ~ 
            band_3_reduction(dp.3yr),
          # if payment is between level 3 and level 4   
          dplyr::between(dp.3yr, input$Level_3+1, input$Level_4) ~ 
            band_4_reduction(dp.3yr),
          # if payment is between level 4 and level 5
          dplyr::between(dp.3yr, input$Level_4+1, input$Level_5) ~ 
            band_5_reduction(dp.3yr)
        ),
        
        # marker for affected farms
        num.affected = ifelse(loss==0, 0, 1),
        # calculates new direct payment
        newdp = dp.3yr - loss,
        # calculates new direct payment costs pro rata
        new.dpcost = ifelse(dp.3yr == 0, 0, newdp * dp.costs.3yr/dp.3yr),
        # calculates new direct payment cost centre
        new.dp.cc = newdp - new.dpcost,
        # calculates new FBI
        new.fbi = fbi.3yr - dp.cc.3yr + new.dp.cc,
        # calculates change in FBI
        fbi.change = fbi.3yr-new.fbi,
        
        # marker for farms with negative FBI
        neg.fbi = ifelse(fbi.3yr < 0, 1, 0),
        # marker for farms with negative FBI after loss of payment
        negnew.fbi = ifelse(new.fbi < 0, 1, 0),
        # marker for farms with FBI < 10k
        lt.10k.fbi = ifelse(fbi.3yr < 10000, 1, 0),
        # marker for farms with FBI < 10k after loss of payment
        under10knew.fbi = ifelse(new.fbi < 10000, 1, 0),
        # marker for farms with FBI < 25k
        lt.25k.fbi = ifelse(fbi.3yr < 25000, 1, 0),
        # marker for farms with FBI < 25k after loss of payment
        under25knew.fbi = ifelse(new.fbi < 25000, 1, 0),
        # marker for farms with FBI < 50k
        lt.50k.fbi = ifelse(fbi.3yr < 50000, 1, 0),
        # marker for farms with FBI < 50k after loss of payment
        under50knew.fbi = ifelse(new.fbi < 50000, 1, 0),
        
        # sets up a factor for farms by FBI (original)
        fbi.band = cut(fbi.3yr,  
                       breaks = c(-Inf,0,10000,25000,Inf), 
                       labels = c("less than zero", "0 to 10k","10 to 25k","Over 25k"), 
                       levels =c(1,2,3,4),
                       right = TRUE,
                       ordered_result = TRUE),
        # sets up a factor for farms by FBI (original)
        fbi.band1 = cut(fbi.3yr,
                        breaks = c(-Inf,0,10000,25000,50000,Inf), 
                        labels = c("less than zero", "0 to 10k","10 to 25k","25 to 50k","Over 50k"), 
                        levels =c(1,2,3,4),
                        right = TRUE,
                        ordered_result = TRUE),
        # sets up a factor for farms by FBI (original) excluding direct payment
        fbi.exc.DP = cut(fbi.min.dp.cc.3yr, 
                        breaks = c(-Inf,0,10000,20000,30000,40000,50000,100000,Inf), 
                        labels = c("less than zero", "0 to 10k","10 to 20k","20 to 30k","30 to 40k","40 to 50k","50 to 100k","Over 100k"), 
                        levels =c(1,2,3,4,5,6,7,8),
                        right = TRUE,
                        ordered_result = TRUE),
        # sets up a factor for farms by direct payment (original)
        dp.band = cut(dp.3yr,
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
                                   fpc=~Loss_fbs()$num.pop,
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
          format(round(sum(Loss_rpa()$num.affected),0),big.mark=","),
          " Businesses (",
          format(round(100*(sum(Loss_rpa()$num.affected))/sum(Loss_rpa()$dummy))),
          "%)"
    )
  })
  

  ## Payments received impact - FBS data
  table3 <- reactive({

    mean_byfac_new(c("dp.3yr", "newdp"))[,c(1:3,7:8)] %>% {
      data.frame(fbsfac = .[[1]],
             Average_DP = .[[2]],
             CI_Average_DP = .[[4]],
             Average_new_DP = .[[3]],
             CI_Average_new_DP = .[[5]])
    } %>%
      bind_cols(.,
                ratio_byfac_new(numerators = c("dp.3yr", "newdp"), denominators = c("fbi.3yr", "new.fbi")) %>% {
                  data.frame(DP_prop_FBI = .[[2]],
                         CI_DP_prop_FBI = .[[7]],
                         new_DP_prop_new.fbi = .[[3]],
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
       scale_x_discrete(labels = function(x) str_wrap(x, width = 8)) +
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
       scale_x_discrete(labels = function(x) str_wrap(x, width = 8)) +
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

    mean_byfac_new(c("fbi.3yr", "new.fbi"))[,c(1:3,7:8)] %>% {
      data.frame(fbsfac = .[[1]],
                 Average_FBI = .[[2]],
                 CI_Average_FBI = .[[4]],
                 Average_new.fbi = .[[3]],
                 CI_Average_new.fbi = .[[5]])
    } %>%
      cbind(., 
            # can't get ratio_byfac to accept multiple variables
            ratio_byfac_new(numerators = "neg.fbi", "dummy")[,c(2,5)],
            c(mosaic::sum(Loss_fbs()$neg.fbi ~ (Loss_fbs())[,input$fbsfac]), "All" = sum(Loss_fbs()$neg.fbi)),
            ratio_byfac_new(numerators = "negnew.fbi", "dummy")[,c(2,5)],
            c(mosaic::sum(Loss_fbs()$negnew.fbi ~ (Loss_fbs())[,input$fbsfac]), "All" = sum(Loss_fbs()$negnew.fbi)),
            ratio_byfac_new(numerators = "lt.10k.fbi", "dummy")[,c(2,5)],
            c(mosaic::sum(Loss_fbs()$lt.10k.fbi ~ (Loss_fbs())[,input$fbsfac]), "All" = sum(Loss_fbs()$neg.fbi)),
            ratio_byfac_new(numerators = "under10knew.fbi", "dummy")[,c(2,5)],
            c(mosaic::sum(Loss_fbs()$under10knew.fbi ~ (Loss_fbs())[,input$fbsfac]), "All" = sum(Loss_fbs()$negnew.fbi))
      ) %>%
      # run a small sample checker on the nobs of raw data column
      small_sample_checker(x = ., nobs = 8)
  })
      
  output$table_2a <- DT::renderDataTable({
    
    table2() %>%
      dplyr::rename('Farm Business Income (£ per farm)' = 2,
                    '\u00B1 95% CI FBI' = 3,
                    
                    'Farm Business Income after payment loss (£ per farm)' = 4,
                    '\u00B1 95% CI FBI after payment loss' = 5) %>%
      dplyr::select(1:5) %>%
      nice_table() %>%
      DT::formatRound(columns=c(2:5), digits=0) 
   
      
  })
  
  output$table_2b <- DT::renderDataTable({
    
    table2() %>%
      dplyr::rename('Proportion of farms with FBI < £0' = 6,
                    '\u00B1 95% CI for FBI < £0' = 7,
                    'Number in sample for FBI < £0' = 8,
                    
                    'Proportion of farms with FBI < £10k' = 12,
                    '\u00B1 95% CI for FBI < £10k' = 13,
                    'Number in sample for FBI < £10k' = 14,
                    
                    'Proportion of farms with FBI < 0 after payment loss' = 9,
                    '\u00B1 95% CI for FBI < £0 after payment loss' = 10,
                    'Number in sample after payment loss' = 11,
                    
                    'Proportion of farms with FBI < £10k after payment loss' = 15,
                    '\u00B1 95% CI for FBI < £10k after payment loss' = 16,
                    'Number in sample for FBI < £10k after payment loss' = 17) %>%
      dplyr::select(1, 6, 7, 8, 12, 13, 14, 9, 10, 11, 15, 16, 17) %>% # re-order columns
      nice_table() %>%
      DT::formatPercentage(columns = c(2, 3, 5, 6, 8, 9, 11, 12))
    
    
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
       scale_x_discrete(labels = function(x) str_wrap(x, width = 8)) +
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
               y = c(.[[6]], .[[7]]),
               CI = c(.[[7]], .[[10]]))} %>%
       mutate(fbsfac = factor(fbsfac, levels = c(levels(Loss_fbs()[[input$fbsfac]]), "All"))) %>%
       ggplot(aes(x= fbsfac, y = y, fill = group)) +  
       geom_bar(position = "dodge", stat="identity") +
       geom_errorbar(aes(ymin = y-CI, ymax = y+CI), position = "dodge", stat="identity") +
       theme(legend.position="bottom") +
       scale_x_discrete(labels = function(x) str_wrap(x, width = 8)) +
       scale_y_continuous(labels = scales::percent) +
       scale_fill_manual(values=colours.helper(c("one", "two"))) +
       labs(y = "Proportion of farms with FBI < £0", x = "") 
    ) %>%
      ggplotly(tooltip = c("group","y","x")) %>%
      layout(legend = list(orientation = "h",
                           y = -0.2, x = 0
      ))
  })
  
  output$table2_plot3 <- renderPlotly({
    (table2() %>%
       {data.frame(fbsfac = rep(.[[1]], 2),
                   group = rep(c("Average FBI", 
                                 "Average new FBI"), 
                               each = length(.[[1]])),
                   y = c(.[[12]], .[[15]]),
                   CI = c(.[[13]], .[[16]]))} %>%
       mutate(fbsfac = factor(fbsfac, levels = c(levels(Loss_fbs()[[input$fbsfac]]), "All"))) %>%
       ggplot(aes(x= fbsfac, y = y, fill = group)) +  
       geom_bar(position = "dodge", stat="identity") +
       geom_errorbar(aes(ymin = y-CI, ymax = y+CI), position = "dodge", stat="identity") +
       theme(legend.position="bottom") +
       scale_x_discrete(labels = function(x) str_wrap(x, width = 8)) +
       scale_y_continuous(labels = scales::percent) +
       scale_fill_manual(values=colours.helper(c("one", "two"))) +
       labs(y = "Proportion of farms with FBI < £10k", x = "") 
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
       scale_x_discrete(labels = function(x) str_wrap(x, width = 8)) +
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
       scale_x_discrete(labels = function(x) str_wrap(x, width = 8)) +
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
