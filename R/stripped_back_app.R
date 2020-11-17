
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
RPA_data <- readRDS("C:\\Users\\m994810\\Desktop\\Payments reductions shiny app\\Data\\20200520 BPS 2019.Rds")
#RPA_data <- readRDS("K:\\TASPrototype\\FBSmastercopy\\FBS_ADHOC_DATA_REQUESTS\\EU Exit\\Payments reductions shiny app\\Data\\20200520 BPS 2019.Rds")
# 2016/17-2018/19 FBS data
fbs_3yr <- readRDS("C:\\Users\\m994810\\Desktop\\Payments reductions shiny app\\Data\\fbs_england_3yr_16_18.Rds")
#fbs_3yr <- readRDS("K:\\TASPrototype\\FBSmastercopy\\FBS_ADHOC_DATA_REQUESTS\\EU Exit\\Payments reductions shiny app\\Data\\fbs_england_3yr_16_18.Rds")

input <- list(fbsfac = "tenancy")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
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
                  selected = "dp.band")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: table
      DT::dataTableOutput("table_3")
      
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  # function to simplify the code to calculate the means
  mean_byfac_new <- function(vars, factor = input$fbsfac, design = fbsdesign()){
    FBSCore::mean_byfac(vars, factor, design) %>%
      small_sample_checker()
  }
  
  
  # sets up the survey design
  fbsdesign <- reactive({svydesign(id= ~farms,
                                   strata= ~stratum,
                                   fpc=~num.pop,
                                   data= fbs_3yr,
                                   weight= ~newwt3yr,
                                   nest=TRUE)
    
  })
  
  
table3 <- reactive({
   mean_byfac_new(c("X.16FBI"))
  # FBSCore::mean_byfac(vars = "X.16FBI", 
  #                     factor = input$fbsfac,
  #                     design = fbsdesign())
  })  

## Payments received impact - FBS data
  output$table_3 <- DT::renderDataTable({
    table3()
  })
}



shinyApp(ui, server)
