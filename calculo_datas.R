library(tidyverse)
library(ggplot2)

## Read in county-level data
#demo_long2 <- readRDS("/Users/lucas/Dropbox/UFMG/Academia/COVID19/Marcia/demo_long.RDS")
demo_long <- read.csv2("/Users/lucas/Dropbox/UFMG/Academia/COVID19/Marcia/dados.csv")

datas <- read.csv2("/Users/lucas/Dropbox/UFMG/Academia/COVID19/Marcia/datas.csv")
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

permutacoes <- 10
capitais <- c(99)
libera <- c(1,1.5)
tx_complicacao <- c(0.05,0.12)

for(i in capitais) {
  
  cenario = 1
  
  for(j in libera) {
    for(k in tx_complicacao) {

      county_fips = i
      
      suffolk_example <- read.csv2(paste0("/Users/lucas/Dropbox/UFMG/Academia/COVID19/Marcia/estimado/",i,"_",j,"_",k,".csv"))
      suffolk_example$dates <- as.Date(suffolk_example$dates)
      
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
      
      tx_plano = 1-tx_plano
      
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
      
      #exp_cases <- exponential(N_init, prop_all_cases, dates, doubling_time)
      #suffolk_example <- preparedness_county(county_fips, N_init, prop_all_cases, 
      #                                       dates, doubling_time, 
      #                                       icu_cap, hosp_cap, vent_cap, 
      #                                       catchment_prop)
      
      #write.csv2(suffolk_example,paste0("estimado/",i,"_",j,"_",k,".csv"),row.names = FALSE)

      #diario1 <- preparedness_county1(county_fips, N_init, prop_all_cases, 
      #                                       dates, doubling_time, 
      #                                       icu_cap, hosp_cap, vent_cap, 
      #                                       catchment_prop)
      
      #print(aggregate(num_cases ~ day, subset(suffolk_example, dates == as.Date(fim)), mean)$num_cases)
      #saida <- data.frame(diario1[[1]]$dates,round(diario1[[1]]$new_cases,0),diario1[[1]]$num_new_hosp_round,diario1[[1]]$num_new_icu_round,diario1[[1]]$num_new_vent_round)
      #write.csv2(saida,paste0(i,"_",j,"_",k,".csv"))

      #
      #sum(aggregate(num_hosp ~ day,suffolk_example,mean)$num_hosp)
      #sum(aggregate(num_icu ~ day,suffolk_example,mean)$num_icu)
      #sum(aggregate(num_vent ~ day,suffolk_example,mean)$num_vent)

      teste <- suffolk_example %>%
        group_by(dates) %>%
        summarise(
          num_cases = median(num_cases),
          num_hosp = median(num_hosp),
          num_icu = median(num_icu),
          num_vent = median(num_vent),
          hosp_bed = mean(hosp_bed),
          icu_bed = mean(icu_bed),
          ventilator = mean(ventilator)
        )
      
      
      teste$hosp_bed_sus = teste$hosp_bed*tx_normal
      teste$icu_bed_sus = teste$icu_bed*tx_uti
      teste$ventilator_sus = teste$ventilator*tx_resp
      
      teste$num_hosp_ans = teste$num_hosp*tx_plano
      teste$num_hosp_80 = teste$num_hosp*0.80
      teste$num_icu_ans = teste$num_icu*tx_plano
      teste$num_icu_80 = teste$num_icu*0.80
      teste$num_vent_ans = teste$num_vent*tx_plano
      teste$num_vent_80 = teste$num_vent*0.80
      
      teste$num_hosp_ans = cumsum(teste$num_hosp_ans)
      teste$num_icu_ans = cumsum(teste$num_icu_ans)
      teste$num_vent_ans = cumsum(teste$num_vent_ans)
      teste$num_hosp_80 = cumsum(teste$num_hosp_80)
      teste$num_icu_80 = cumsum(teste$num_icu_80)
      teste$num_vent_80 = cumsum(teste$num_vent_80)
      teste$num_hosp = cumsum(teste$num_hosp)
      teste$num_icu = cumsum(teste$num_icu)
      teste$num_vent = cumsum(teste$num_vent)
      
   
      min_leito = 0
      min_uti = 0
      min_vent = 0
      
      for(l in c(1:nrow(teste))) {
        if(teste$num_hosp_ans[l] >= teste$hosp_bed_sus[l]) {
          if(min_leito == 0) {
            min_leito = as.character(teste$dates[l])            
          }
        }
        if(teste$num_icu_ans[l] >= teste$icu_bed_sus[l]){
          if(min_uti == 0) {
            min_uti = as.character(teste$dates[l])
          }
        }
        if(teste$num_vent_ans[l] >= teste$ventilator_sus[l]){
          if(min_vent == 0) {
            min_vent = as.character(teste$dates[l])
          }
        }
      }

      if(min_leito==0) {
        min_leito <-  teste$hosp_bed_sus[nrow(teste)]-teste$num_hosp_ans[nrow(teste)]
      }
      if(min_uti==0) {
        min_uti <-  teste$icu_bed_sus[nrow(teste)]-teste$num_icu_ans[nrow(teste)]
      }
      if(min_vent==0) {
        min_vent <-  teste$ventilator_sus[nrow(teste)]-teste$num_vent_ans[nrow(teste)]
      }
      
      if(cenario == 1){
        resultado <- c(cenario,min_leito,min_uti,min_vent)
        cenario <- cenario + 1
      } else {
        linha <- c(cenario,min_leito,min_uti,min_vent)
        resultado <- rbind(resultado,linha)
        cenario <- cenario + 1
      }
  
      min_leito = 0
      min_uti = 0
      min_vent = 0
      
      for(l in c(1:nrow(teste))) {
        if(teste$num_hosp_80[l] >= teste$hosp_bed_sus[l]) {
          if(min_leito == 0) {
            min_leito = as.character(teste$dates[l])            
          }
        }
        if(teste$num_icu_80[l] >= teste$icu_bed_sus[l]){
          if(min_uti == 0) {
            min_uti = as.character(teste$dates[l])
          }
        }
        if(teste$num_vent_80[l] >= teste$ventilator_sus[l]){
          if(min_vent == 0) {
            min_vent = as.character(teste$dates[l])
          }
        }
      }
      
      if(min_leito==0) {
        min_leito <-  teste$hosp_bed_sus[nrow(teste)]-teste$num_hosp_80[nrow(teste)]
      }
      if(min_uti==0) {
        min_uti <-  teste$icu_bed_sus[nrow(teste)]-teste$num_icu_80[nrow(teste)]
      }
      if(min_vent==0) {
        min_vent <-  teste$ventilator_sus[nrow(teste)]-teste$num_vent_80[nrow(teste)]
      }
      
      linha <- c(cenario,min_leito,min_uti,min_vent)
      resultado <- rbind(resultado,linha)
      cenario <- cenario + 1

      min_leito = 0
      min_uti = 0
      min_vent = 0
      
      for(l in c(1:nrow(teste))) {
        if(teste$num_hosp[l] >= teste$hosp_bed[l]) {
          if(min_leito == 0) {
            min_leito = as.character(teste$dates[l])            
          }
        }
        if(teste$num_icu[l] >= teste$icu_bed[l]){
          if(min_uti == 0) {
            min_uti = as.character(teste$dates[l])
          }
        }
        if(teste$num_vent[l] >= teste$ventilator[l]){
          if(min_vent == 0) {
            min_vent = as.character(teste$dates[l])
          }
        }
      }
      
      if(min_leito==0) {
        min_leito <-  teste$hosp_bed[nrow(teste)]-teste$num_hosp[nrow(teste)]
      }
      if(min_uti==0) {
        min_uti <-  teste$icu_bed[nrow(teste)]-teste$num_icu[nrow(teste)]
      }
      if(min_vent==0) {
        min_vent <-  teste$ventilator[nrow(teste)]-teste$num_vent[nrow(teste)]
      }
      
      linha <- c(cenario,min_leito,min_uti,min_vent)
      resultado <- rbind(resultado,linha)
      cenario <- cenario + 1
      
      # ================
      ## PLOTS
      # ================
      
      
      ## no catchment lines
      #grafico <- ggplot(data = suffolk_example %>% filter(dates <= fim)) + # 
      #  geom_point(aes(x=dates, y=num_hosp, colour = "Persons in Hospital beds"), alpha = 0.5)  + 
      #  geom_point(aes(x=dates, y=num_icu, colour = "Persons in ICU beds"), alpha = 0.5)  + 
      #  geom_point(aes(x=dates, y=num_vent, colour = "Persons in Mechanical ventilators"), alpha = 0.5)  + 
      #  geom_hline(aes(yintercept = hosp_bed, colour = "Hospital beds"), size = 1) +
      #  geom_hline(aes(yintercept = icu_bed, colour = "ICU beds"), size = 1) +
      #  geom_hline(aes(yintercept = ventilator, colour = "Mechanical ventilators"), size = 1 ) +
      #  scale_color_manual(values = c(
      #    "Persons in Hospital beds" = 'mediumblue',
      #    "Persons in ICU beds" = 'springgreen',
      #    "Hospital beds" = "darkblue",
      #    "ICU beds" = "darkgreen", 
      #    "Persons in Mechanical ventilators" = "magenta",
      #    "Mechanical ventilators" = "darkred")) +
      #   theme_classic() +
      #   scale_x_date(breaks = "1 week") +
      #   labs(y = "People", 
      #        x = "Date", 
      #        colour = "Category") +
      #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
    #  
    #  ggsave(paste0("grafico/",county_fips,"_",cenario,".png"), plot = grafico, width = 12, height = 8, units = c("in"), dpi = 300)
      
    }
  }
  write.csv(resultado,paste0("/Users/lucas/Dropbox/UFMG/Academia/COVID19/Marcia/resultado/",county_fips,".csv"),row.names = FALSE)
  print("foi")
}

