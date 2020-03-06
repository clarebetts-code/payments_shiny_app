
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# does the data processing of FBS and RPA and saves to 
# shiny.app.data.Rdata
#
# written by Clare Betts January 2020

# N.B.
# Need to check why some farms increase their FBI after payment reduction - only 2
# Need to modify payment reduction code so not double counting any farms which lie on the boundry
#   i.e. between(dp_3yr, input$Level_1+1, input$Level_2) or similar
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


pacman::p_load(dplyr,
               forcats,
               stringr,
               survey,
               FBSCore,
               tibble,
               shiny)


setwd("K:\\TASPrototype\\FBSmastercopy\\FBS_ADHOC_DATA_REQUESTS\\EU Exit\\Payments reduction shiny app")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function to calculate 3 year averages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

av_3year <- function(dat, item, years = 2015:2017){ 
  
  years %>%
    substr(start = 3, stop = 4) %>%
    paste0("X.", ., item) %>%
    subset(dat, select = .) %>%
    rowMeans()
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


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

# save R data
save(RPApay_interim, fbs_3yr, file = "shiny.app.data.Rdata")

