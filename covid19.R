## Title: Exponential model to estimate # hospital and ICU beds needed for COVID-19 over time
## Authors: Taylor Chin, Rebecca Kahn (Center for Communicable Disease Dynamics, Harvard T.H. Chan School of Public Health)
## Date: 20 March 2020

library(tidyverse)
library(ggplot2)

## FUNCTION INPUTS:
## county_fips: county ID
## N_init: number of confirmed cases (initial number to start exponential model)
## prop_all_cases: proportion of all cases # confirmed cases represents
## dates: date range
## doubling_time: doubling time of # cases
## icu_cap: proportion of ICU beds that can be used
## hosp_cap: proportion of hospital beds that can be used
## catchment_prop: proportion of county a hospital serves 

preparedness_county <- function(county_fips, N_init, prop_all_cases, dates, doubling_time, icu_cap, hosp_cap, vent_cap, catchment_prop){
  
  county_pop <- demo_long %>%
    filter(fips == county_fips & variable == "n_pop_2018") %>%
    pull(value) # pull county pop (2018)   
  
  exp_cases <- exponential(N_init, prop_all_cases, dates, doubling_time)    # exponential model (function 1) for # cases/day
  prop_hosp_vector <- prop_hospitalized(county_fips)                        # proportion hospitalized (function 2- uses age structure), with +/- 0.3 for uncertainty
  list_df <- lapply(prop_hosp_vector, hosp_accounting)                      # number hospitalized and in ICU per day (function 4) - loop over all prop hospitalized values in vector
  # takes a long time to run to loop over 1000 prop hosp
  unlist_df <- do.call("rbind", list_df)                                    # unlist and rbind to make one df
  icu_bed <- return_icu_beds(demo_long, county_fips, icu_cap)               # append # ICU beds in county (helper function)
  hosp_bed <- return_hosp_beds(demo_long, county_fips, hosp_cap)            # append # Hosp beds in county (helper function)
  ventilator <- return_ventilators(demo_long, county_fips, vent_cap)
  
  ## divide by catchment proportion
  unlist_df_catch <- unlist_df %>% mutate(num_cases_catchment = num_cases*catchment_prop, 
                                          num_hosp_catchment = num_hosp*catchment_prop, 
                                          num_icu_catchment = num_icu*catchment_prop,
                                          num_vent_catchment = num_vent*catchment_prop)
  
  ## add red vertical line for when 10% population infected (exponential model no longer accurate)
  last_date_plot <- (exp_cases %>% filter(cases >= 0.1*county_pop) %>% pull(dates))[1] 
  
  final_df <- cbind(unlist_df_catch, icu_bed, hosp_bed,ventilator, last_date_plot)
  
  return(final_df)
  
}  

# ==============================   
## FUNCTION 1: exponential model 
# ==============================   

## estimate # cases per day using exponential model
## inputs: initial number cases, proportion of all cases this represents, date rate, doubling time 

exponential <- function(N_init, prop_all_cases, dates, doubling_time){
  
  cases <- NULL
  for (i in 1:length(dates)){
    
    if(missing(prop_all_cases)){
      cases[i] <- N_init*2^(i/doubling_time)   
    } else {
      cases[i] <- (N_init/prop_all_cases)*2^(i/doubling_time)   
    }
  }
  
  df <- as.data.frame(cbind(cases, dates))
  df$dates <- as.Date(df$dates, origin = lubridate::origin)
  df$day <- df$dates - first(df$dates) +1
  return(df)
}

#suffolk_exp <- exponential(N_init, prop_all_cases, dates, doubling_time)

#plot(suffolk_exp$dates, suffolk_exp$cases)


# ==============================   
## FUNCTION 2: proportion hospitalized
# ==============================  

## estimate proportion hospitalized using age structure of county and literature on % infected and % severe based on age group
# Shenzhen close contacts paper with literature estimates of attack rate and % severe: https://www.medrxiv.org/content/10.1101/2020.03.03.20028423v1.full.pdf 
# and add range around estimate for uncertainty (added 0.3 here for now)
# and produce vector of 100 values of proportion hospitalized - incorporates uncertainty
## inputs: county ID

prop_hospitalized <- function(county_fips){
  
  county <-
    demo_long %>% filter(fips == county_fips) # filter county
  
  hosp_summary <- county %>%
    group_by(age_epi) %>%
    summarise(
      num_persons = sum(value),
      prop_severe = mean(prop_severe_adj),
      prop_infected = mean(prop_infected_adj)
    ) %>%
    ## take means bc values just repeated for each age cat (can also do first etc)
    filter(!is.na(age_epi)) %>%
    mutate(
      prop_age = num_persons / sum(num_persons),
      num_severe = prop_infected * prop_severe * num_persons,
      num_infected = prop_infected * num_persons
    )
  
  prop_hosp <- hosp_summary %>%
    summarise(prop_severe_tot = sum(num_severe) / sum(num_infected)) %>% pull(prop_severe_tot)
  
  prop_hosp_lower <- prop_hosp - 0.03
  prop_hosp_upper <- prop_hosp + 0.03
  
  prop_hosp_distrib <- runif(permutacoes, prop_hosp_lower, prop_hosp_upper)   ## should ideally by >1000 draws; 50 for demonstration/reduced compute time
  return(prop_hosp_distrib)
  
  
}

# check function
#prop_hosp_vector2 <- prop_hospitalized(35)


# ==============================   
## FUNCTION 3: count # in hospital and ICU in date range
# ==============================  

## count # people hospitalized and in ICU in given date range
## accounting system to keep track of hospitalized and critical care patients over time based on 
## estimated date of admission and last date in hospital/ICU
## this is used inside FUNCTION 4 
## inputs: data frame - either hosp_track or icu_track for hosp beds and ICU beds, respectively

num_hosp_icu <- function(df_input){
  
  num_persons <- NULL
  for (i in 1:length(dates)){
    subset <- df_input[(df_input$date_start <= dates[i] & df_input$date_end >= dates[i]),]
    num_persons[i] <- nrow(subset)
  }
  
  df <- data.frame(cbind(dates, num_persons))
  colnames(df) <- c("date", "num_persons")
  df$date <- as.Date(df$date, origin = lubridate::origin)
  
  return(df)
  
}  

# check
#num_hosp_icu(hosp_track)


# ==============================   
## FUNCTION 4: model # in hospital and ICU per day
# ==============================  

## estimate # hospitalized on each day

hosp_accounting <- function(prop_hosp){
  
  # "exp_cases" is in master function (preparedness_county function)
  # exp_cases is the name of the dataframe that has # exponential cases over time (output from FUNCTION 1)
  hosp_model <- exp_cases %>% mutate(new_cases = cases - lag(cases),             ### calculates # of new cases  
                                     num_new_hosp = new_cases*prop_hosp,         ### feeds in proportion hospitalized vector (output of FUNCTION 2)
                                     num_new_hosp_round = round(num_new_hosp,0), ### round number of new hospitalized cases for accounting; CHECK IF CAN ROUND HERE
                                     num_new_icu = new_cases*k,                  ### ASSUMPTION of 5% of cases will become ICU patients  - can make into function input to be flexible
                                     num_new_icu_round = round(num_new_icu,0),   ### round number of new ICU cases for accounting; CHECK IF CAN ROUND HERE 
                                     num_new_vent = num_new_icu*0.5,                 ### We could assume that 50% of those in ICU will need mechanical ventilation
                                     num_new_vent_round = round(num_new_vent,0),     ### round number of new ventilators cases for accounting; CHECK IF CAN ROUND HERE
                                     ID = row_number())                          ### add row number ID to help see which rows replicated below
  
  hosp_model[is.na(hosp_model)] <- 0 # make NA's in first row 0
  
  ## replicate # rows based on # new hospitalizations - create a row for every hospitalized and ICU patient
  hosp_track <- hosp_model[rep(row.names(hosp_model), hosp_model$num_new_hosp_round), ]
  icu_track <- hosp_model[rep(row.names(hosp_model), hosp_model$num_new_icu_round), ]
  vent_track <- hosp_model[rep(row.names(hosp_model), hosp_model$num_new_vent_round), ]
  
  ## add date_start and date_end dates by sampling from distribution of 1) time from symptom onset to hospital admission and 2) LOS hospital
  # hospitalizations:
  hosp_track$date_start <- round(runif(nrow(hosp_track), min = 3 , max = 7),0) + hosp_track$dates      ### update with latest literature; time from symptom onset (becoming case) to hospital admission; CHECK IF CAN ROUND HERE 
  hosp_track$date_end <- round(runif(nrow(hosp_track), min = 7, max = 15),0) + hosp_track$date_start   ### update with latest literature; length of stay in hospital; CHECK IF CAN ROUND HERE 
  
  # icu:
  icu_track$date_start <- round(runif(nrow(icu_track), min = 8 , max = 15),0) + icu_track$dates       ### update with latest literature; time from symptom onset (becoming case) to ICU admission; CHECK IF CAN ROUND HERE 
  icu_track$date_end <- round(runif(nrow(icu_track), min = 7, max = 15),0) + icu_track$date_start      ### update with latest literature; length of stay in ICU; CHECK IF CAN ROUND HERE 
  
  # ventilador:
  vent_track$date_start <- round(runif(nrow(vent_track), min = 8, max = 15),0) + vent_track$dates       ### update with latest literature; time from symptom onset (becoming case) to Ventilador admission; CHECK IF CAN ROUND HERE 
  vent_track$date_end <- round(runif(nrow(vent_track), min = 7, max = 15),0) + vent_track$date_start      ### update with latest literature; length of stay in Ventilator; CHECK IF CAN ROUND HERE 
  
  
  num_hosp_day <- num_hosp_icu(hosp_track)    ### FUNCTION 3 to count number of days in hospital using date columns
  num_icu_day <- num_hosp_icu(icu_track)      ### FUNCTION 3 to count number of days in ICU using date columns
  num_vent_day <- num_hosp_icu(vent_track)
  
  ### join data
  hosp_summary_day <- 
    hosp_model %>% 
    select(dates, day, cases) %>% 
    left_join(num_hosp_day, by = c("dates" = "date")) %>% 
    left_join(num_icu_day, by = c("dates" = "date")) %>% 
    left_join(num_vent_day, by = c("dates" = "date")) %>%
    rename(num_cases = "cases",
           num_hosp = "num_persons.x", 
           num_icu = "num_persons.y",
           num_vent = "num_persons")
  
  hosp_summary_day$prop_hosp_input <- prop_hosp    ### keep track of proportion hospitalized value used from vector 
  
  return(hosp_summary_day)
  
}


# check
#hosp_accounting(prop_hosp_vector[3])


## OTHER HELPER FUNCTIONS 

### pull # ICU and # hospital beds for each county
### inputs: county-level data frame, county ID, proportion of ICU or hospital beds that are available for use
return_icu_beds <- function(demo_long, county_fips, icu_cap) { 
  
  icu_beds <- 
    demo_long %>% 
    filter(fips == county_fips, 
           variable == "n_medsurg_card_other_icu_beds_2017") %>% 
    pull(value)
  
  icu_beds_reduc <- icu_beds*icu_cap
  
  return(icu_beds_reduc)
  
}

return_hosp_beds <- function(demo_long, county_fips, hosp_cap) { 
  
  hosp_beds <- 
    demo_long %>% 
    filter(fips == county_fips, 
           variable == "n_hospital_beds_2017") %>% 
    pull(value)
  
  hosp_beds_reduc <- hosp_beds*hosp_cap
  
  return(hosp_beds_reduc)
  
}

return_ventilators <- function(demo_long, county_fips, vent_cap) { 
  
  ventilator <- 
    demo_long %>% 
    filter(fips == county_fips, 
           variable == "n_ventilator") %>% 
    pull(value)
  
  ventilador_reduc <- ventilator*vent_cap
  
  return(ventilador_reduc)
  
}

#icu_bed <- return_icu_beds(demo_long, 35, icu_cap = 1)
#hosp_bed <- return_hosp_beds(demo_long, 35, hosp_cap = 1)
#ventilator <- return_ventilators(demo_long, 35, vent_cap = 1)

#icu_bed
#hosp_bed
#ventilator

## Read in county-level data
#demo_long2 <- readRDS("/Users/lucas/Dropbox/UFMG/Academia/COVID19/Marcia/demo_long.RDS")
demo_long <- read.csv2("~/R/dados.csv")

datas <- read.csv2("~/R/datas.csv")
datas$value <- as.Date(datas$value)

## make Age 70 + same as Age 60-69 cat (assumption) - can update using latest information
## Shenzhen close contacts paper with literature estimates of attack rate and % severe: 
##  https://www.medrxiv.org/content/10.1101/2020.03.03.20028423v1.full.pdf 
demo_long$prop_severe_adj <- ifelse(demo_long$age_epi == "Age 70+ Years", 0.24, demo_long$prop_severe) 
demo_long$prop_infected_adj <- ifelse(demo_long$age_epi == "Age 70+ Years", 0.154, demo_long$prop_infected) 

## 35 São Paulo
## 33 Rio de Janeiro
## 53 Brasília
## 23 Fortaleza
## 43 Porto Alegre
## 31 Belo Horizonte
## 13 Manaus

#35,33,53,23,43,31,13,29,41,99
#53,23,

permutacoes <- 1000
capitais <- c(43,31,13,29,41,33,35,99)
libera <- c(1,1.5)
tx_complicacao <- c(0.05,0.12)

for(i in capitais) {
  
  cenario = 1
  
  for(j in libera) {
    for(k in tx_complicacao) {

      county_fips = i
      
      doubling_time <- demo_long %>%
        filter(fips == county_fips & variable == "doubling_time") %>%
        pull(value)
      
      inicio <- datas %>%
        filter(fips == county_fips & variable == "inicio") %>%
        pull(value)
      
      fim <- datas %>%
        filter(fips == county_fips & variable == "fim") %>%
        pull(value)
      
      N_init <- demo_long %>%
        filter(fips == county_fips & variable == "n_init") %>%
        pull(value)
      
      tx_normal <- demo_long %>%
        filter(fips == county_fips & variable == "tx_normal") %>%
        pull(value)
      
      tx_uti <- demo_long %>%
        filter(fips == county_fips & variable == "tx_uti") %>%
        pull(value)
      
      tx_resp <- demo_long %>%
        filter(fips == county_fips & variable == "tx_resp") %>%
        pull(value)
      
      tx_plano <- demo_long %>%
        filter(fips == county_fips & variable == "tx_plano") %>%
        pull(value)
      
      dates <- c(seq(as.Date(inicio), as.Date(fim), "days"))
      
      prop_all_cases <- 0.14
      
      icu_cap <- (1-demo_long %>%
                    filter(fips == county_fips & variable == "tx_ocup_uti") %>%
                    pull(value))*j
      
      hosp_cap <- (1-demo_long %>%
                     filter(fips == county_fips & variable == "tx_ocup_normal") %>%
                     pull(value))*j
      
      vent_cap <- icu_cap
      
      catchment_prop <- 1
      
      exp_cases <- exponential(N_init, prop_all_cases, dates, doubling_time)
      suffolk_example <- preparedness_county(county_fips, N_init, prop_all_cases, 
                                             dates, doubling_time, 
                                             icu_cap, hosp_cap, vent_cap, 
                                             catchment_prop)
      
      write.csv2(suffolk_example,paste0(i,"_",j,"_",k,".csv"),row.names = FALSE)
    }
  }
}
 
