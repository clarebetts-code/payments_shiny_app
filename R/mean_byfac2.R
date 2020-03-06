



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






vars <- "fbi_3yr"
factor <- "type"
design <- fbsdesign

mean_byfac2 <- function (vars, factor, design) 
{
  fvars <- survey::make.formula(syms(vars))
  ffactor <- survey::make.formula(syms(factor))
  all_name <- paste0("All_", factor)
  
  all_mean <-survey::svyby(fvars,
                           1, 
                           design, 
                           svymean, 
                           na.rm.by = FALSE, 
                           vartype = c("se", "ci")) %>%
    # needs a mutate call to strip attributes
    dplyr::mutate(by = all_name) %>%
    tibble::as_tibble() %>% 
    dplyr::rename(`:=`(!!factor, 
                       by))
  
   
  all_nobs <- survey::svyby(fvars,
                            1,
                            design, 
                            unwtd.count, 
                            na.rm.by = FALSE) %>%
    # needs a mutate call to strip attributes
    dplyr::mutate(by = all_name) %>%
    tibble::as_tibble() %>% 
    dplyr::rename(`:=`(!!factor, 
                       by)) %>% 
    dplyr::select(-se)
  

  fac_mean <- svyby(fvars, 
                    ffactor, 
                    design, 
                    svymean, 
                    na.rm.by = FALSE, 
                    vartype = c("se", "ci")) %>%
    # needs a mutate call to strip attributes
    dplyr::mutate(dummy = 1) %>%
    dplyr::select(-dummy) %>%
    tibble::as_tibble() %>% 
    dplyr::mutate_if(is.factor, as.character)
  
  fac_nobs <- svyby(fvars, 
                    ffactor, 
                    design, 
                    unwtd.count, 
                    na.rm.by = FALSE)%>%
    # needs a mutate call to strip attributes
    dplyr::mutate(dummy = 1) %>%
    dplyr::select(-dummy, -se) %>%
    tibble::as_tibble() %>% 
    dplyr::mutate_if(is.factor, as.character)
  
  nobs <- dplyr::bind_rows(fac_nobs, all_nobs) %>% 
    dplyr::select(-!!factor) %>% 
    dplyr::rename(nobs = counts)
  
  out <- dplyr::bind_rows(fac_mean, all_mean) %>% 
    dplyr::bind_cols(nobs)
  
  for (v in vars) {
    if (length(vars) == 1) {
      t <- out %>% dplyr::select(dplyr::starts_with("ci_u"), 
                                 dplyr::starts_with("ci_l"))
    }
    else {
      t <- out %>% dplyr::select(dplyr::matches(paste0("^(ci_l|ci_u)\\.", 
                                                       v, "$"), ignore.case = FALSE))
    }
    
    colnames(t) <- c("ci_u", "ci_l")
    
    vname <- paste("CI.", v, sep = "")
    
    ans <- dplyr::mutate(t, 
                         `:=`(!!vname, 
                              (ci_u - ci_l)/2)) %>% 
      dplyr::select(-ci_u, -ci_l)
    
    out <- dplyr::bind_cols(out, ans)
  }
  out %>% 
    dplyr::select(!!factor, 
                  vars, 
                  nobs, 
                  dplyr::contains("se"), 
                  dplyr::contains("Ci."))
}
