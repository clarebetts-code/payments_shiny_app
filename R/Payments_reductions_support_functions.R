#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# functions to support the payments reductions shiny app
# which don't need to be reactive
#
# written by clare betts march 2020
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# function to add a spinner when app parts are loading
spinner<-function(output){
  shinycssloaders::withSpinner(output, 
                               color="#808000", 
                               size = 3)
}

# function to get colours right on graphs
colours.helper <- function(x){
  len <- length(unique(x))
  if(len == 1){cols <- c("#416146")}
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


# Function to calculate 3 year averages
av_3year <- function(dat, item, years = 2016:2018){ 
  
  years %>%
    substr(start = 3, stop = 4) %>%
    paste0("X.", ., item) %>%
    subset(dat, select = .) %>%
    rowMeans()
}

# function to check for small sample sizes
small_sample_checker <- function(x, nobs = "nobs"){
  low_sample_rows <- rownames(x)[x[,nobs] < 5]
  nums <- unlist(lapply(x, is.numeric)) 
  
  out <- x
  out[low_sample_rows, nums] <- 0
  
  return(out)
}