rm(list=ls())
library(tidyverse)
library(gridExtra)
library(broom)
library(mgcv)
library(scales)
library(MASS)
select<-dplyr::select
load("clean_rdata.rdata")
weekly_data <- deaths %>% 
  filter(year>=2016, year<=2021) %>% 
  group_by(SALID2, year, epi_week) %>% 
  summarise(counts=sum(deaths))
population <- popl2 %>% 
  group_by(SALID2, year) %>% 
  summarise(pop=sum(pop)) %>% 
  filter(year>=2016, year<=2021)
weekly_data_population<-full_join(weekly_data, population) %>% 
  arrange(SALID2, year, epi_week) %>% 
  ungroup() %>% 
  filter(epi_week!=53) %>% 
  ## create exposure (person-weeks) %>% 
  mutate(exposure=pop*7/365.25) %>% 
  filter(!is.na(SALID2))
# age
weekly_data_age <- deaths %>% 
  filter(year>=2016, year<=2021) %>% 
  group_by(SALID2, year, epi_week, edad_cat) %>% 
  summarise(counts=sum(deaths))
population_age <- popl2 %>% 
  group_by(SALID2, edad_cat, year) %>% 
  summarise(pop=sum(pop)) %>% 
  filter(year>=2016, year<=2021)
weekly_data_population_age<-full_join(weekly_data_age, population_age) %>% 
  arrange(SALID2, year, epi_week, edad_cat) %>% 
  ungroup() %>% 
  filter(epi_week!=53) %>% 
  ## create exposure (person-weeks) %>% 
  mutate(exposure=pop*7/365.25) %>% 
  filter(!is.na(SALID2))
# GAM L2
excess<-weekly_data_population %>% 
  group_by(SALID2) %>% 
  group_modify(~{
    #.x<-weekly_data_population %>% filter(SALID2==10310110)
    print(.y$SALID2)
    .x<-.x %>% 
      arrange(year, epi_week) %>% 
      mutate(time=row_number())
    pre_covid<-.x %>% 
      filter(year<2020)
    fit.gam <- gam(counts ~ factor(epi_week) + s(time),
                   family=nb(), offset= log(exposure),
                   data=pre_covid)
    temp <- predict.gam(fit.gam,newdata = .x, type = "response", se.fit = TRUE) %>% 
      as_tibble %>% 
      mutate(time=row_number()) %>% 
      full_join(.x, by="time") %>% 
      mutate(fit=fit*exposure, 
             se.fit=se.fit*exposure,
             uci=fit+1.96*se.fit,
             lci=fit-1.96*se.fit,
             excess=counts-fit,
             excess_rate=excess/exposure * 100000,
             excess_rel=excess/fit,
             excess_uci=counts-lci,
             excess_lci=counts-uci,
             excess_rel_uci=excess_uci/fit,
             excess_rel_lci=excess_lci/fit,
             time=year+(epi_week-1)/52)
    temp
  })
# GAM L1
excess_l1<-weekly_data_population %>% 
  left_join(region_cw) %>% 
  group_by(SALID1, year, epi_week) %>% 
  summarise(counts=sum(counts),
            exposure=sum(exposure),
            pop=sum(pop)) %>% 
  group_by(SALID1) %>% 
  group_modify(~{
    #.x<-weekly_data_population_l1 %>% filter(SALID1==103101)
    print(.y$SALID1)
    .x<-.x %>% 
      arrange(year, epi_week) %>% 
      mutate(time=row_number())
    pre_covid<-.x %>% 
      filter(year<2020)
    fit.gam <- gam(counts ~ factor(epi_week) + s(time),
                   family=nb(), offset= log(exposure),
                   data=pre_covid)
    temp <- predict.gam(fit.gam,newdata = .x, type = "response", se.fit = TRUE) %>% 
      as_tibble %>% 
      mutate(time=row_number()) %>% 
      full_join(.x, by="time") %>% 
      mutate(fit=fit*exposure, 
             se.fit=se.fit*exposure,
             uci=fit+1.96*se.fit,
             lci=fit-1.96*se.fit,
             excess=counts-fit,
             excess_rate=excess/exposure * 100000,
             excess_rel=excess/fit,
             excess_uci=counts-lci,
             excess_lci=counts-uci,
             excess_rel_uci=excess_uci/fit,
             excess_rel_lci=excess_lci/fit,
             time=year+(epi_week-1)/52)
    temp
  })
# macrozonas
excess_macro<-weekly_data_population %>% 
  left_join(region_cw) %>% 
  group_by(macrozona, year, epi_week) %>% 
  summarise(counts=sum(counts),
            exposure=sum(exposure),
            pop=sum(pop)) %>% 
  group_by(macrozona) %>% 
  group_modify(~{
    #.x<-weekly_data_population_macro %>% filter(macrozona=="North")
    print(.y$macrozona)
    .x<-.x %>% 
      arrange(year, epi_week) %>% 
      mutate(time=row_number())
    pre_covid<-.x %>% 
      filter(year<2020)
    fit.gam <- gam(counts ~ factor(epi_week) + s(time),
                   family=nb(), offset= log(exposure),
                   data=pre_covid)
    temp <- predict.gam(fit.gam,newdata = .x, type = "response", se.fit = TRUE) %>% 
      as_tibble %>% 
      mutate(time=row_number()) %>% 
      full_join(.x, by="time") %>% 
      mutate(fit=fit*exposure, 
             se.fit=se.fit*exposure,
             uci=fit+1.96*se.fit,
             lci=fit-1.96*se.fit,
             excess=counts-fit,
             excess_rate=excess/exposure * 100000,
             excess_rel=excess/fit,
             excess_uci=counts-lci,
             excess_lci=counts-uci,
             excess_rel_uci=excess_uci/fit,
             excess_rel_lci=excess_lci/fit,
             time=year+(epi_week-1)/52)
    temp
  })

save.image("clean_models.rdata")

