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
               mosaic)

setwd("C:\\Users\\m994810\\Desktop\\Payments reductions shiny app")


# read in support functions
source("C:\\Users\\m994810\\Desktop\\Payments reductions shiny app\\Payments reduction app for git\\R\\Payments_reductions_support_functions.R")

load(file = "C:\\Users\\m994810\\Desktop\\Payments reductions shiny app\\Data\\shiny.app.data.Rdata")



total_byfac_new <- function(vars, factor = input$fbsfac, design = fbsdesign){
  FBSCore::total_byfac(vars, factor, design)
}

mean_byfac_new <- function(vars, factor = input$fbsfac, design = fbsdesign){
  FBSCore::mean_byfac(vars, factor, design)
}

ratio_byfac_new <- function(numerators, denominators, factor = input$fbsfac, design = fbsdesign){
  FBSCore::ratio_byfac(numerators, denominators, factor, design)
}

# functions to apply payment reductions  
#####
input <- list(Level_1 = 30000, prop_1 = 3,
              Level_2 = 50000, prop_2 = 8,
              Level_3 = 150000, prop_3 = 11,
              Level_4 = 1000000, prop_4 = 15,
              Level_5 = 10000000, prop_5 = 15,
              # Level_6 = 100000, prop_6 = 11,
              # Level_7 = 125000, prop_7 = 11,
              # Level_8 = 150000, prop_8 = 11,
              # Level_9 = 200000, prop_9 = 15,
              # Level_10 = 200000, prop_10 = 15,
              # Level_11 = 200000, prop_11 = 15,
              # Level_12 = 1000000, prop_12 = 15,
              fbsfac = "type")

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


#####

fbsloss <- fbs_3yr
fbsloss$loss <- 0
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

fbsdesign <- svydesign(id= ~farms,
                                 strata= ~stratum,
                                 fpc=~num_pop,
                                 data= fbsloss,
                                 weight= ~newwt3yr,
                                 nest=TRUE)


# This calculates the loss for each business using RPA data based on the TOTAL PAYMENT
rpaloss <- subset(RPApay_interim, RPApay_interim$Total_Payment_pounds > 0)
rpaloss$loss <- dplyr::case_when(
    # if payment is less than first level, reduce by prop_1
    rpaloss$Total_Payment_pounds < input$Level_1 ~
      band_1_reduction(rpaloss$Total_Payment_pounds),
    # if payment is between level 1 and level 2
    dplyr::between(rpaloss$Total_Payment_pounds, input$Level_1+0.01, input$Level_2) ~
      band_2_reduction(rpaloss$Total_Payment_pounds),
    # if payment is between level 2 and level 3
    dplyr::between(rpaloss$Total_Payment_pounds, input$Level_2+0.01, input$Level_3) ~
      band_3_reduction(rpaloss$Total_Payment_pounds), 
    # if payment is between level 3 and level 4
    dplyr::between(rpaloss$Total_Payment_pounds, input$Level_3+0.01, input$Level_4) ~ 
      band_4_reduction(rpaloss$Total_Payment_pounds),
    # if payment is between level 4 and level 5
    dplyr::between(rpaloss$Total_Payment_pounds, input$Level_4+0.01, input$Level_5) ~ 
      band_5_reduction(rpaloss$Total_Payment_pounds)
  )
  
rpaloss %>%
    mutate(
      # bands for original payments
      pay_band = cut(rpaloss$Total_Payment_pounds, 
                     breaks = c(0, 5000, 10000, 15000, 20000, 25000, 30000, 40000, 50000, 75000,  100000, 125000, 150000, 200000, 250000, 300000, Inf), 
                     labels = c("0 to 5k", "5 to 10k", "10 to 15k", "15 to 20k", "20 to 25k", "25 to 30k", "30 to 40k", "40 to 50k", "50 to 75k", "75 to 100k", "100 to 125k", "125 to 150k","150 to 200k", "200k to 250k", "250k to 300k", "over 300k"), 
                     levels =c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),
                     right = TRUE,
                     ordered_result = TRUE),
      # bands for money lost
      pay_band_money_lost = cut(rpaloss$loss, 
                                breaks = c(0, 0.001, 5000, 10000, 15000, 20000, 25000, 30000, 40000, 50000, 75000,  100000, 125000, 150000, 200000, 250000, 300000, Inf), 
                                labels = c("No loss", "0 to 5k", "5 to 10k", "10 to 15k", "15 to 20k", "20 to 25k", "25 to 30k", "30 to 40k", "40 to 50k", "50 to 75k", "75 to 100k", "100 to 125k", "125 to 150k","150 to 200k", "200k to 250k", "250k to 300k","over 300k"), 
                                levels =c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17),
                                right = TRUE,
                                ordered_result = TRUE),
      # number affected
      num_affected = ifelse(rpaloss$loss==0, 0, 1),
      
      # dummy variable to help calculate proportions
      dummy = 1
    ) -> rpaloss


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  TABLE 1
#####

mean_byfac_new(c("loss", "dp_3yr"))  %>% {
  tibble(fbsfac = .$type,
       Average_payment_loss = .$loss,
       CI_average_payment_loss = .$Ci.loss,
       sample_payment_loss = c(mosaic::sum(fbsloss$num_affected ~ fbsloss$type), sum(fbsloss$num_affected)),
       Average_original_payment = .$dp_3yr,
       CI_average_original_payment = .$Ci.dp_3yr)}   %>%
  mutate(
    proportional_loss = scales::percent((Average_payment_loss / Average_original_payment), accuracy = 1)
    )  -> table1

#####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  PLOT 1
#####

(table1 %>% {
  tibble(fbsfac = rep(.[[1]], 2),
         group = rep(c("Average payment loss", "Average original payment"), each = length(.[[1]])),
             y = c(.$Average_payment_loss, .$Average_original_payment),
             CI = c(.$CI_average_payment_loss, .$CI_average_original_payment))} %>%
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
           
#####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  TABLE 2
#####

# column 1 & 2
first_column <- mean_byfac_new("fbi_3yr")[,1]

columns_1_2 <- mean_byfac_new("fbi_3yr")[, c("fbi_3yr", "Ci.fbi_3yr")]


# column 3
bla1 <- total_byfac_new("neg_fbi")
bla2 <- total_byfac_new("dummy")

column_3 <- scales::percent(
  pull(
    bla1[,2] /
      bla2[,2]
    ), accuracy = 1)

# column 4
column_4 <- c(mosaic::sum(fbsloss$neg_fbi ~ fbsloss$type),
              "All farms" = sum(fbsloss$neg_fbi))

# column 5 & 6
columns_5_6 <- mean_byfac_new("new_fbi")[, c("new_fbi", "Ci.new_fbi")]

# column 7
bla1 <- total_byfac_new("negnew_fbi")

column_7 <- scales::percent(
  pull(
    bla1[,2] /
      bla2[,2]
  ), accuracy = 1)

# column 8
column_8 <- c(mosaic::sum(fbsloss$negnew_fbi ~ fbsloss$type),
              "All farms" = sum(fbsloss$negnew_fbi))

first_column %>%
 cbind(columns_1_2,
       column_3,
       column_4,
       columns_5_6,
       column_7,
       column_8) %>%
  dplyr::rename('Farm Business Income (£ per farm)' = 2,
                '\u00B1 95% CI FBI' = 3,
                'Proportion of farms with FBI < 0' = 4,
                'Number in sample' = 5,
                'Farm Business Income after payment loss (£ per farm)' = 6,
                '\u00B1 95% CI FBI after payment loss' = 7,
                'Proportion of farms with FBI < 0 after payment loss' = 8,
                'Number in sample after payment loss' = 9) %>%
  DT::datatable() %>%
  DT::formatRound(columns=c(2,3,6,7), digits=0)

#####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  TABLE 3
# direct payments as a proportion of FBI
#####

mean_byfac_new(c("fbi_3yr", "dp_3yr", "newdp", "new_fbi"))[,c(1:5,11:14)] %>% {
  tibble(fbsfac = .[[1]],
         Average_FBI = .[[2]],
         CI_Average_FBI = .[[6]],
         Average_DP = .[[3]],
         CI_Average_DP = .[[7]],
         Average_new_FBI = .[[5]],
         CI_Average_new_FBI = .[[9]],
         Average_new_DP = .[[4]],
         CI_Average_new_DP = .[[8]])
} %>%
  bind_cols(., ratio_byfac_new(numerators = c("dp_3yr", "newdp"), denominators = c("fbi_3yr", "new_fbi")) %>% {
    tibble(DP_prop_FBI = scales::percent(.[[2]], accuracy = 1),
           CI_DP_prop_FBI = scales::percent(.[[7]], accuracy = 1),
           new_DP_prop_new_FBI = scales::percent(.[[3]], accuracy = 1),
           CI_new_DP_prop_FBI = scales::percent(.[[8]], accuracy = 1))
  }) %>%
  dplyr::rename('Farm Business Income (£ per farm)' = 2,
                '\u00B1 95% CI FBI' = 3,
                'Direct payment (£ per farm)' = 4,
                '\u00B1 95% CI DP' = 5,
                'Farm Business Income after payment loss (£ per farm)' = 6,
                '\u00B1 95% CI FBI after payment loss' = 7,
                'Direct payment after payment loss (£ per farm)' = 8,
                '\u00B1 95% CI DP after payment loss' = 9,
                'Direct payment as a proportion of FBI' = 10,
                '\u00B1 95% CI DP as prop FBI' = 11,
                'Direct payment as a proportion of FBI after payment loss (£ per farm)' = 12,
                '\u00B1 95% CI DP as prop FBI after payment loss' = 13) %>%
  .[,c(1:5, 10:11, 6:9, 12:13)] %>% # re-order columns
  DT::datatable() %>%
  DT::formatRound(columns=c(2:5,8:11), digits=0) -> table3

#####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Plot 2
# RPA data
#####

(tibble(pay_band = levels(rpaloss$pay_band),
        expenditure_saved = mosaic::sum(loss ~ pay_band, data = rpaloss)) %>%
    mutate(expenditure_saved_m = expenditure_saved/1000000,
           label = paste("£", round(expenditure_saved_m, 1), "m", sep = "")) %>%
  ggplot(aes(x= pay_band, y = expenditure_saved_m)) +  
    geom_bar(position = "dodge", stat="identity", fill = "#416146") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 9)) +
    #scale_fill_manual(values=colours.helper(c("one"))) +
    labs(y = "Total loss (£ million)", x = "Direct payment band") +
    geom_text(
      aes(label = label, y = expenditure_saved_m + 0.2),
      position = position_dodge(0.9),
      vjust = 0
    )) %>%
  ggplotly(tooltip = c("group","y","x")) 
  

#####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Plot 3
# RPA data
#####
(data.frame(payment_reduction_band = factor(levels(rpaloss$pay_band_money_lost)),
        number_of_businesses = mosaic::sum(num_affected ~ pay_band_money_lost, data = rpaloss))  %>%
   ggplot(aes(x= payment_reduction_band, y = number_of_businesses)) +  
   geom_bar(position = "dodge", stat="identity", fill = "#416146") +
   scale_x_discrete(labels = function(x) str_wrap(x, width = 9)) +
   #scale_fill_manual(values=colours.helper(c("one"))) +
   labs(y = "Total loss (£ million)", x = "Loss of direct payment") +
   geom_text(
     aes(label = number_of_businesses, y = number_of_businesses + 0.2),
     position = position_dodge(0.9),
     vjust = 0
   )) %>%
  ggplotly(tooltip = c("group","y","x")) 

#####