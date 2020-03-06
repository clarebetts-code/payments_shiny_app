# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
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
               ggplot2)



setwd("K:\\TASPrototype\\FBSmastercopy\\FBS_ADHOC_DATA_REQUESTS\\EU Exit\\Payments reduction shiny app")

# read in support functions
source("Payments_reductions_support_functions.R")

load(file = "shiny.app.data.Rdata")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define UI for application
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- fluidPage(
  # Application title
  titlePanel("Farm Business Income Direct Payment reductions"),
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
                                 "fbi_band2",
                                 "age_band",
                                 "farmer_gender",
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
                        value=1000000,step = 5000)
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
                    tabPanel("Next...", 
                             spinner(
                               plotlyOutput("plot1"))),
                    tabPanel("Payment \nloss", spinner(DT::dataTableOutput("table_1"))),
                    tabPanel("FBI impact", spinner(DT::dataTableOutput("table_2")))
                    
                    )
        )
    )
  )

  


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
    FBSCore::total_byfac(vars, factor, design)
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
  
  Loss_fbs <- reactive ({
    #This calculates the loss for each business using FBS data
    fbsloss <- fbs_3yr
    fbsloss$loss <- dplyr::case_when(
      # if payment is less than first level, reduce by prop_1
      fbsloss$dp_3yr <= input$Level_1 ~ 
        band_1_reduction(fbsloss$dp_3yr),
      # if payment is between level 1 and level 2
      dplyr::between(fbsloss$dp_3yr, input$Level_1+1, input$Level_2) ~ 
        band_2_reduction(fbsloss$dp_3yr),
      # if payment is between level 2 and level 3
      dplyr::between(fbsloss$dp_3yr, input$Level_2+1, input$Level_3) ~ 
        band_3_reduction(fbsloss$dp_3yr),
      # if payment is between level 3 and level 4
      dplyr::between(fbsloss$dp_3yr, input$Level_3+1, input$Level_4) ~ 
        band_4_reduction(fbsloss$dp_3yr),
      # if payment is between level 4 and level 5
      dplyr::between(fbsloss$dp_3yr, input$Level_4+1, input$Level_5) ~ 
        band_5_reduction(fbsloss$dp_3yr)
    ) 
    
    # recalculate fbi etc
    fbsloss %>%
      dplyr::mutate(
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
        
        # sets up a factor for fams by FBI (original)
        fbi_band = cut(fbi_3yr,  
                       breaks = c(-Inf,0,10000,25000,Inf), 
                       labels = c("less than zero", "0 to 10k","10 to 25k","Over 25k"), 
                       levels =c(1,2,3,4),
                       right = TRUE,
                       ordered_result = TRUE),
        # sets up a factor for fams by FBI (original)
        fbi_band1 = cut(fbi_3yr,
                        breaks = c(-Inf,0,10000,25000,50000,Inf), 
                        labels = c("less than zero", "0 to 10k","10 to 25k","25 to 50k","Over 50k"), 
                        levels =c(1,2,3,4),
                        right = TRUE,
                        ordered_result = TRUE),
        # sets up a factor for fams by FBI (original)
        fbi_band2 = cut(fbi_min_dp_cc_3yr, 
                        breaks = c(-Inf,0,10000,20000,30000,40000,50000,100000,Inf), 
                        labels = c("less than zero", "0 to 10k","10 to 20k","20 to 30k","30 to 40k","40 to 50k","50 to 100k","Over 100k"), 
                        levels =c(1,2,3,4,5,6,7,8),
                        right = TRUE,
                        ordered_result = TRUE),
        # sets up a factor for fams by FBI (original)
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
      ) -> fbsloss
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
  
  output$caption1a <- renderText({
    paste("Number of farms in sample = ",
          length(Loss_fbs()$farms)
    )
  })
  
  output$caption1b <- renderText({
    paste("Total payments lost = ",
          sum(Loss_fbs()$loss * Loss_fbs()$newwt3yr)
    )
  })
  

  table1 <-  reactive({
    
    mean_byfac_new("loss")[, c(1, 2, 5)] %>%
      bind_cols(data.frame(
        mosaic::sum(Loss_fbs()$num_affected ~ (Loss_fbs())[,input$fbsfac])
      ) %>%
        rbind(sum(Loss_fbs()$num_affected)),
      mean_byfac_new("dp_3yr")[, c(2, 5)]) %>%
      mutate('proportional loss of DP' = scales::percent((loss / dp_3yr), accuracy = 1)) 
    
    # this doesn't work in shiny but would be clearer to read:
    # mean_byfac_new(c("loss", "dp_3yr"))  %>%
      # tibble(fbsfac = .$type,
      #        Average_payment_loss = .$loss,
      #        CI_average_payment_loss = .$CI.loss,
      #        sample_payment_loss = c(mosaic::sum(fbsloss$num_affected ~ fbsloss$type), sum(fbsloss$num_affected)),
      #        Average_original_payment = .$dp_3yr,
      #        CI_average_original_payment = .$CI.dp_3yr
      # ) %>%
      # select(fbsfac,
      #        Average_payment_loss,
      #        CI_average_payment_loss,
      #        sample_payment_loss,
      #        Average_original_payment,
      #        CI_average_original_payment) %>%
      # mutate(proportional_loss = scales::percent((Average_payment_loss / Average_original_payment), accuracy = 1),
      #        CI_average_payment_loss = CI_average_payment_loss * -1,
      #        CI_average_original_payment = CI_average_original_payment * -1)
    
    })
  
  output$table_1 <- DT::renderDataTable({
    
    table1()  %>%
      dplyr::rename('Average payment loss (£)' = 2,
                    '\u00B1 95% CI payment loss' = 3,
                    'num in sample with payment loss' = 4,
                    'Average original payment (£)' = 5,
                    '\u00B1 95% CI original payment' = 6,
                    'Proportional loss of DP' = 7) %>%
      nice_table() %>%
      DT::formatRound(columns=c(2,3,5,6), digits=0)

    })
  

    output$plot1 <- renderPlotly({
      
      (table1() %>% {
        tibble(fbsfac = rep(.[[1]], 2),
               group = rep(c("Average payment loss", "Average original payment"), each = length(.[[1]])),
               y = c(.[[2]], .[[5]]),
               CI = c(.[[3]], .[[6]]))} %>%
         ggplot(aes(x= fbsfac, y = y, fill = group)) + 
         geom_bar(position = "dodge", stat="identity") +
         geom_errorbar(aes(ymin = y-CI, ymax = y+CI), position = "dodge", stat="identity") +
         scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
         scale_fill_manual(values=colours.helper(c("one", "two"))) +
         labs(y = "£ per farm)", x = "") 
      ) %>%
        ggplotly(tooltip = c("group","y","x")) %>%
        layout(legend = list(orientation = "h",
                             y = -0.2, x = 0
                             ))
                 
      
    })

  
  
  output$table_2 <- DT::renderDataTable({
    
    # column 1 & 2
    columns_1_2 <- mean_byfac_new("fbi_3yr")[, c(1, 2, 5)]
    
    # column 3
    bla1 <- total_byfac_new("neg_fbi")
    bla2 <- total_byfac_new("dummy")
    
    column_3 <- scales::percent(
      pull(
        bla1[,2] /
          bla2[,2]
      ), accuracy = 1)
    
    # column 4
    column_4 <- c(mosaic::sum(Loss_fbs()$neg_fbi ~ (Loss_fbs())[,input$fbsfac]),
                  "All farms" = sum(Loss_fbs()$neg_fbi)) 
    
    # column 5 & 6
    columns_5_6 <- mean_byfac_new("new_fbi")[, c(2, 5)]
    
    # column 7
    bla1 <- total_byfac_new("negnew_fbi")
    
    column_7 <- scales::percent(
      pull(
        bla1[,2] /
          bla2[,2]
      ), accuracy = 1)
    
    # column 8
    column_8 <- c(mosaic::sum(Loss_fbs()$negnew_fbi ~ (Loss_fbs())[,input$fbsfac]),
                  "All farms" = sum(Loss_fbs()$negnew_fbi))
    
    columns_1_2 %>%
      cbind(column_3,
            column_4,
            columns_5_6,
            column_7,
            column_8) %>%
      dplyr::rename('Farm Business Income (£ per farm)' = 2,
                    '\u00B1 95% CI FBI' = 3,
                    'Proportion of farms with FBI < 0' = 4,
                    'Number in sample FBI < 0' = 5,
                    'Farm Business Income after payment loss (£ per farm)' = 6,
                    '\u00B1 95% CI FBI after payment loss' = 7,
                    'Proportion of farms with FBI < 0 after payment loss' = 8,
                    'Number in sample FBI < 0 after payment loss' = 9) %>%
      nice_table() %>%
      DT::formatRound(columns=c(2,3,6,7), digits=0)
      
  })
  
  

}






shinyApp(ui, server)