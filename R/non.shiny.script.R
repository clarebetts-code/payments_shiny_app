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
input <- list(Level_1 = 30000, prop_1 = 0,
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
              fbsfac = "tenancy")

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

# This calculates the loss for each business using FBS data
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
    ) -> fbsloss

  
fbsdesign <- svydesign(id= ~farms,
                                 strata= ~stratum,
                                 fpc=~num_pop,
                                 data= fbsloss,
                                 weight= ~newwt3yr,
                                 nest=TRUE)


# This calculates the loss for each business using RPA data based on the TOTAL PAYMENT
RPA_data %>%
  # remove any rows with NA in payment column
  tidyr::drop_na(c(NET_PAY, BPS_NET, GREENING_NET, YF_NET)) %>%
  # only those where payments were greater than 0
  subset(NET_PAY_GBP > 0)  %>%
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
  
  ) -> rpaloss
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# captions
#####
# summary of real RPA data
caption1a <-     paste("Total paid in ",
                       rpa_year,
                       " = £",
                       format(round(sum(rpaloss$NET_PAY_GBP),0),big.mark=","),
                       "to ",
                       format(length(unique(rpaloss$FRN)),big.mark=","),
                       " businesses. Of which ",
                       round(100*(sum(rpaloss$BPS_NET_GBP)/sum(rpaloss$NET_PAY_GBP)), digits = 1),
                       "% were basic payments, ",
                       round(100*(sum(rpaloss$GREENING_NET_GBP)/sum(rpaloss$NET_PAY_GBP)), digits = 1),
                       "% were greening payments, and",
                       round(100*(sum(rpaloss$YF_NET_GBP)/sum(rpaloss$NET_PAY_GBP)), digits = 1),
                       "% were young farmer payments."
)

# summary of RPA after reduction:
caption1b <-  paste0("RPA Saved: £", format(round(sum(rpaloss$loss),0), big.mark=","),
         " (",
         format(round(100*sum(rpaloss$loss/sum(rpaloss$NET_PAY_GBP),0))),
         "%) from ",
         format(round(sum(rpaloss$num_affected),0),big.mark=","),
         " Businesses (",
         format(round(100*(sum(rpaloss$num_affected))/sum(rpaloss$dummy))),
         "%)"
  )


# summary of RPA after reduction:
caption2b <-  paste0("RPA Saved: £", format(round(sum(rpaloss$loss),0), big.mark=","),
                     " (",
                     format(round(100*sum(rpaloss$loss/sum(rpaloss$BPS_NET_GBP),0))),
                     "%) from ",
                     format(round(sum(rpaloss$num_affected),0),big.mark=","),
                     " Businesses (",
                     format(round(100*(sum(rpaloss$num_affected))/sum(rpaloss$dummy))),
                     "%)"
)
#####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  TABLE 2
#####

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
        c(mosaic::sum(neg_fbi~fbsloss[[input$fbsfac]], data = fbsloss), "All" = sum(fbsloss$neg_fbi)),
        c(mosaic::sum(negnew_fbi~fbsloss[[input$fbsfac]], data = fbsloss), "All" = sum(fbsloss$negnew_fbi))
        ) -> table2

table2 %>%
  dplyr::rename('Farm Business Income (£ per farm)' = 2,
                '\u00B1 95% CI FBI' = 3,
                'Proportion of farms with FBI < 0' = 6,
                'Number in sample' = 8,
                'Farm Business Income after payment loss (£ per farm)' = 4,
                '\u00B1 95% CI FBI after payment loss' = 5,
                'Proportion of farms with FBI < 0 after payment loss' = 7,
                'Number in sample after payment loss' = 9) %>%
  .[c(2,3,6,8,4,5,7,9)] %>% # re-order columns
  DT::datatable() %>%
  DT::formatRound(columns=c(1,2,5,6), digits=0) %>%
  DT::formatPercentage(columns = c(3, 7))

#####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  TABLE 3
# direct payments as a proportion of FBI
#####

mean_byfac_new(c("dp_3yr", "newdp"))[,c(1:3,7:8)] %>% 
 {tibble(fbsfac = .[[1]],
         Average_DP = .[[2]],
         CI_Average_DP = .[[4]],
         Average_new_DP = .[[3]],
         CI_Average_new_DP = .[[5]])
} %>%
  bind_cols(., 
            ratio_byfac_new(numerators = c("dp_3yr", "newdp"), denominators = c("fbi_3yr", "new_fbi")) 
            %>% {
              data.frame(DP_prop_FBI = .[[2]],
                     CI_DP_prop_FBI = .[[7]],
                     new_DP_prop_new_FBI = .[[3]],
                     CI_new_DP_prop_FBI = .[[8]])
              }) -> table3

# formatted table
table3 %>%
  dplyr::rename('Direct payment (£ per farm)' = 2,
                '\u00B1 95% CI DP' = 3,
                'Direct payment after payment loss (£ per farm)' = 4,
                '\u00B1 95% CI DP after payment loss' = 5,
                'Direct payment as a proportion of FBI' = 6,
                '\u00B1 95% CI DP as prop FBI' = 7,
                'Direct payment as a proportion of FBI after payment loss (£ per farm)' = 8,
                '\u00B1 95% CI DP as prop FBI after payment loss' = 9) %>%
  DT::datatable() %>%
  DT::formatRound(columns=c(2:5), digits=0) %>%
  DT::formatPercentage(columns = 6:9)

#####

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  TABLE 3 - Plot
# average DP
#####

(table3 %>%
  {data.frame(fbsfac = rep(.[[1]], 2),
           group = rep(c("Average DP", "Average new DP"), each = length(.[[1]])),
           y = c(.$Average_DP, .$Average_new_DP),
           CI = c(.$CI_Average_DP, .$CI_Average_new_DP))} %>%
   mutate(fbsfac = factor(fbsfac, levels = c(levels(fbsloss[[input$fbsfac]]), "All"))) %>%
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

#  TABLE 3 - Plot
# DP as a proportion of FBI
#####

(table3 %>%
   {data.frame(fbsfac = rep(.[[1]], 2),
           group = rep(c("Average DP", "Average new DP"), each = length(.[[1]])),
           y = c(.$DP_prop_FBI, .$new_DP_prop_new_FBI),
           CI = c(.$CI_DP_prop_FBI, .$CI_new_DP_prop_FBI))} %>%
   mutate(fbsfac = factor(fbsfac, levels = c(levels(fbsloss[[input$fbsfac]]), "All"))) %>%
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
#####

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Table 4
# RPA data
#####

data.frame(pay_band = levels(rpaloss$pay_band),
        expenditure_saved = mosaic::sum(loss ~ pay_band, data = rpaloss)) %>%
    mutate(expenditure_saved_m = expenditure_saved/1000000,
           label = paste("£", round(expenditure_saved_m, 1), "m", sep = ""),
           pay_band = factor(pay_band, levels = levels(rpaloss$pay_band))) -> table4
 
# TABlE 4 Plot

 (table4 %>%
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
  
table4 %>%
  select('Payment band' = 1,
         'Total loss of payment' = 2)%>%
  DT::datatable() %>%
  DT::formatRound(columns = 2, digits=0) 

#####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Table 5
# RPA data
#####
data.frame(payment_reduction_band = factor(levels(rpaloss$pay_band_money_lost)),
        number_of_businesses = mosaic::sum(dummy ~ pay_band_money_lost, data = rpaloss)) %>%
  mutate(payment_reduction_band = factor(payment_reduction_band, levels = levels(rpaloss$pay_band_money_lost))) -> table5
 

 (table5 %>%
   ggplot(aes(x= payment_reduction_band, y = number_of_businesses)) +  
   geom_bar(position = "dodge", stat="identity", fill = "#416146") +
   scale_x_discrete(labels = function(x) str_wrap(x, width = 9)) +
   #scale_fill_manual(values=colours.helper(c("one"))) +
   labs(y = "Number of businesses", x = "Loss of direct payment") +
   geom_text(
     aes(label = number_of_businesses, y = number_of_businesses + 0.2),
     position = position_dodge(0.9),
     vjust = 0
   )) %>%
  ggplotly(tooltip = c("group","y","x")) 

#####