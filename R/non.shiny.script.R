

setwd("K:\\TASPrototype\\FBSmastercopy\\FBS_ADHOC_DATA_REQUESTS\\EU Exit\\Payments reduction shiny app")


total_byfac_new <- function(vars, factor = input$fbsfac, design = fbsdesign){
  FBSCore::total_byfac(vars, factor, design)
}

mean_byfac_new <- function(vars, factor = input$fbsfac, design = fbsdesign){
  FBSCore::mean_byfac(vars, factor, design)
}



# functions to apply payment reductions  
#####
input <- list(Level_1 = 30000, prop_1 = 3,
              Level_2 = 50000, prop_2 = 8,
              Level_3 = 150000, prop_3 = 11,
              Level_4 = 1000000, prop_4 = 15,
              Level_5 = 1000000, prop_5 = 15,
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

load(file = "shiny.app.data.Rdata")


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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  TABLE 1

mean_byfac_new("loss")[, c(1, 2, 5)] %>%
  bind_cols(data.frame(
    mosaic::sum(fbsloss$num_affected ~ (fbsloss)[,input$fbsfac])
  ) %>%
    rbind(sum(fbsloss$num_affected)),
  mean_byfac_new("dp_3yr")[, c(2, 5)]) %>%
  mutate('proportional loss of DP' = scales::percent((loss / dp_3yr), accuracy = 1)) -> table1



mean_byfac_new(c("loss", "dp_3yr"))  %>%
  tibble(fbsfac = .$type,
       Average_payment_loss = .$loss,
       CI_average_payment_loss = .$CI.loss,
       sample_payment_loss = c(mosaic::sum(fbsloss$num_affected ~ fbsloss$type), sum(fbsloss$num_affected)),
       Average_original_payment = .$dp_3yr,
       CI_average_original_payment = .$CI.dp_3yr
) %>%
  select(fbsfac,
         Average_payment_loss,
         CI_average_payment_loss,
         sample_payment_loss,
         Average_original_payment,
         CI_average_original_payment) %>%
  mutate(proportional_loss = scales::percent((Average_payment_loss / Average_original_payment), accuracy = 1),
         CI_average_payment_loss = CI_average_payment_loss * -1,
         CI_average_original_payment = CI_average_original_payment * -1)  -> table1

%>%
  dplyr::rename('Average payment loss (£)' = 2,
                '\u00B1 95% CI payment loss' = 3,
                'num in sample with payment loss' = 4,
                'Average original payment (£)' = 5,
                '\u00B1 95% CI original payment' = 6,
                'Proportional loss of DP' = 7)



%>%
  DT::datatable() %>%
  DT::formatRound(columns=c(2,3,4,5), digits=0) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  PLOT 1
colours.helper <- function(x){
  len <- length(unique(x))

    if(len == 2){cols <- c("#416146",	"#7AA680")}
    if(len == 3){cols <- c("#3D5B41",	"#BDD3C0",	"#7AA680")}
    if(len == 4){cols <- c("#39553D",	"#D6E4D8",	"#5F8D66",	"#9BBBA0")}
    if(len == 5){cols <- c("#354F39",	"#ACC8B0",	"#557F5B",	"#DCE8DD",	"#7AA680")}
    if(len == 6){cols <- c("#354F39",	"#B7CFBA",	"#66986D",	"#DCE8DD",	"#4D7352",	"#8CB291")}
    if(len == 7){cols <- c("#354F39",	"#9BBBA0",	"#496D4E",	"#BDD3C0",	"#7AA680",	"#DCE8DD",	"#5F8D66")}
    if(len == 8){cols <- c("#354F39",	"#9BBBA0",	"#496D4E",	"#BDD3C0",	"#7AA680",	"#DCE8DD",	"#5F8D66", "#A3A3A3")}
    if(len == 9){cols <- c("#354F39",	"#9BBBA0",	"#496D4E",	"#BDD3C0",	"#7AA680",	"#DCE8DD",	"#5F8D66", "#A3A3A3", "#C4C4C4")}
    if(len == 10){cols <- c("#354F39",	"#9BBBA0",	"#496D4E",	"#BDD3C0",	"#7AA680",	"#DCE8DD",	"#5F8D66", "#A3A3A3", "#C4C4C4", "#A3A3A3")}

  return(cols)
}




## this will not work in shiny
(table1 %>% {
  tibble(fbsfac = rep(.[[1]], 2),
         group = rep(c("Average payment loss", "Average original payment"), each = length(.[[1]])),
             y = c(.$loss, .$dp_3yr),
             CI = c(.$CI.loss, .$CI.dp_3yr))} %>%
    ggplot(aes(x= fbsfac, y = y, fill = group)) +  
    geom_bar(position = "dodge", stat="identity") +
    geom_errorbar(aes(ymin = y-CI, ymax = y+CI), position = "dodge", stat="identity") +
    theme(legend.position="bottom") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    scale_fill_manual(values=colours.helper(c("one", "two"))) +
    labs(y = "£ per farm", x = "") 
  )%>%
  ggplotly(tooltip = c("group","y","x"),
           layout(legend = list(orientation = "h")
           ))

# try another way
process_FBSobject <- function(obj){
  CIs <- obj[, grep("^CI.", colnames(obj))]
  SEs <- obj[, grep("^se.", colnames(obj))]
  nobs <- obj[, grep("nobs", colnames(obj))]
  means <- obj[, !(colnames(obj) %in% c(colnames(CIs),
                                        colnames(SEs),
                                        colnames(nobs)))] %>%
    .[-1]
  
  
  
  groups <- obj[,1]
  
  return(list(CIs = CIs,
              SEs = SEs,
              nobs = nobs,
              means = means,
              groups = groups))
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  TABLE 2
#####

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
column_4 <- c(mosaic::sum(fbsloss$neg_fbi ~ fbsloss$type),
              "All farms" = sum(fbsloss$neg_fbi))

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
column_8 <- c(mosaic::sum(fbsloss$negnew_fbi ~ fbsloss$type),
              "All farms" = sum(fbsloss$negnew_fbi))

columns_1_2 %>%
  cbind(column_3,
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


  
 