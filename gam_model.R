rm(list=ls())
library(tidyverse)
library(gridExtra)
library(broom)
library(mgcv)
library(scales)
library(MASS)
select<-dplyr::select
load("analisis/clean_rdata.rdata")
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
# function to predict counts from gam
fun_predict.gam<-function(model, data){
  predict.gam(model,newdata = data, type = "response", se.fit = TRUE) %>% 
    as_tibble %>% 
    mutate(time=row_number()) %>% 
    full_join(data, by="time") %>% 
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
}
# GAM L2
excess_l2<-weekly_data_population %>% 
  group_by(SALID2) %>% 
  group_modify(~{
    #.x<-weekly_data_population %>% filter(SALID2==10310110)
    print(.y$SALID2)
    .x<-.x %>% 
      ungroup() %>% 
      arrange(year, epi_week) %>% 
      mutate(time=row_number())
    pre_covid<-.x %>% 
      filter(year<2020)
    fit.gam <- gam(counts ~ factor(epi_week) + s(time),
                   family=nb(), offset= log(exposure),
                   data=pre_covid)
    temp <- fun_predict.gam(fit.gam, .x)
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
    #.x<-weekly_data_population %>% left_join(region_cw) %>% group_by(SALID1, year, epi_week) %>% summarise(counts=sum(counts),exposure=sum(exposure),pop=sum(pop)) %>% filter(SALID1==103101)
    print(.y$SALID1)
    .x<-.x %>% 
      ungroup() %>% 
      arrange(year, epi_week) %>% 
      mutate(time=row_number())
    pre_covid<-.x %>% 
      filter(year<2020)
    fit.gam <- gam(counts ~ factor(epi_week) + s(time),
                   family=nb(), offset= log(exposure),
                   data=pre_covid)
    temp <- fun_predict.gam(fit.gam, .x)
    temp
  })
# macrozonas
excess_macrozona<-weekly_data_population %>% 
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
      ungroup() %>% 
      arrange(year, epi_week) %>% 
      mutate(time=row_number())
    pre_covid<-.x %>% 
      filter(year<2020)
    fit.gam <- gam(counts ~ factor(epi_week) + s(time),
                   family=nb(), offset= log(exposure),
                   data=pre_covid)
    temp <- fun_predict.gam(fit.gam, .x)
    temp
  })
# whole country
excess<-weekly_data_population %>% 
  group_by(year, epi_week) %>% 
  summarise(counts=sum(counts),
            exposure=sum(exposure),
            pop=sum(pop)) %>% 
  group_by(NULL) %>% 
  group_modify(~{
    #.x<-weekly_data_population_macro %>% filter(macrozona=="North")
    #print(.y$macrozona)
    .x<-.x %>% 
      ungroup() %>% 
      arrange(year, epi_week) %>% 
      mutate(time=row_number())
    pre_covid<-.x %>% 
      filter(year<2020)
    fit.gam <- gam(counts ~ factor(epi_week) + s(time),
                   family=nb(), offset= log(exposure),
                   data=pre_covid)
    temp <- fun_predict.gam(fit.gam, .x)
    temp
  })

# sens analysis
## Basselini: compare with empirical
excess_empirical<-weekly_data_population %>% 
  group_by(year, epi_week) %>% 
  summarise(counts=sum(counts),
            pop=sum(exposure)) %>% 
  mutate(covid_year=ifelse(year<2020, "pre", as.character(year))) %>% 
  group_by(covid_year, epi_week) %>% 
  summarise(counts=mean(counts),
            pop=mean(pop)) %>% 
  pivot_wider(id_cols=c(epi_week), names_from=covid_year, values_from=c(counts, pop)) %>% 
  pivot_longer(cols=c(counts_2020, counts_2021,pop_2020, pop_2021), names_to = "year", values_to = "values") %>% 
  mutate(variable=ifelse(grepl("counts", year), "counts", "pop"),
         year=ifelse(grepl("2020", year), 2020, 2021)) %>% 
  pivot_wider(id_cols=c(epi_week, counts_pre, pop_pre, year),
              names_from=variable, values_from = values) %>% 
  mutate(excess=counts-counts_pre,
         excess_rel=excess/counts_pre,
         excess_rate=excess/pop*100000,
         time=as.numeric(year)+(epi_week-1)/52)
excess_empirical_macrozona<-weekly_data_population %>% 
  left_join(region_cw) %>% 
  group_by(macrozona, year, epi_week) %>% 
  summarise(counts=sum(counts),
            pop=sum(exposure)) %>% 
  mutate(covid_year=ifelse(year<2020, "pre", as.character(year))) %>% 
  group_by(macrozona,covid_year, epi_week) %>% 
  summarise(counts=mean(counts),
            pop=mean(pop)) %>% 
  pivot_wider(id_cols=c(macrozona, epi_week), names_from=covid_year, values_from=c(counts, pop)) %>% 
  pivot_longer(cols=c(counts_2020, counts_2021,pop_2020, pop_2021), names_to = "year", values_to = "values") %>% 
  mutate(variable=ifelse(grepl("counts", year), "counts", "pop"),
         year=ifelse(grepl("2020", year), 2020, 2021)) %>% 
  pivot_wider(id_cols=c(macrozona, epi_week, counts_pre, pop_pre, year),
              names_from=variable, values_from = values) %>% 
  mutate(excess=counts-counts_pre,
         excess_rel=excess/counts_pre,
         excess_rate=excess/pop*100000,
         time=as.numeric(year)+(epi_week-1)/52)
excess_empirical_l1<-weekly_data_population %>% 
  left_join(region_cw) %>% 
  group_by(SALID1, year, epi_week) %>% 
  summarise(counts=sum(counts),
            pop=sum(exposure)) %>% 
  mutate(covid_year=ifelse(year<2020, "pre", as.character(year))) %>% 
  group_by(SALID1,covid_year, epi_week) %>% 
  summarise(counts=mean(counts),
            pop=mean(pop)) %>% 
  pivot_wider(id_cols=c(SALID1, epi_week), names_from=covid_year, values_from=c(counts, pop)) %>% 
  pivot_longer(cols=c(counts_2020, counts_2021,pop_2020, pop_2021), names_to = "year", values_to = "values") %>% 
  mutate(variable=ifelse(grepl("counts", year), "counts", "pop"),
         year=ifelse(grepl("2020", year), 2020, 2021)) %>% 
  pivot_wider(id_cols=c(SALID1, epi_week, counts_pre, pop_pre, year),
              names_from=variable, values_from = values) %>% 
  mutate(excess=counts-counts_pre,
         excess_rel=excess/counts_pre,
         excess_rate=excess/pop*100000,
         time=as.numeric(year)+(epi_week-1)/52)
excess_empirical_l2<-weekly_data_population %>% 
  left_join(region_cw) %>% 
  group_by(SALID2, year, epi_week) %>% 
  summarise(counts=sum(counts),
            pop=sum(exposure)) %>% 
  mutate(covid_year=ifelse(year<2020, "pre", as.character(year))) %>% 
  group_by(SALID2,covid_year, epi_week) %>% 
  summarise(counts=mean(counts),
            pop=mean(pop)) %>% 
  pivot_wider(id_cols=c(SALID2, epi_week), names_from=covid_year, values_from=c(counts, pop)) %>% 
  pivot_longer(cols=c(counts_2020, counts_2021,pop_2020, pop_2021), names_to = "year", values_to = "values") %>% 
  mutate(variable=ifelse(grepl("counts", year), "counts", "pop"),
         year=ifelse(grepl("2020", year), 2020, 2021)) %>% 
  pivot_wider(id_cols=c(SALID2, epi_week, counts_pre, pop_pre, year),
              names_from=variable, values_from = values) %>% 
  mutate(excess=counts-counts_pre,
         excess_rel=excess/counts_pre,
         excess_rate=excess/pop*100000,
         time=as.numeric(year)+(epi_week-1)/52)
## Varying window periods
excess_sens_windows<-map_dfr(2016:2018, function(first){
  # last<-2019;first<-2016
  temp<-weekly_data_population %>% 
    group_by(year, epi_week) %>% 
    summarise(counts=sum(counts),
              exposure=sum(exposure),
              pop=sum(pop)) %>% 
    filter(year>=first) %>% 
    ungroup() %>% 
    arrange(year, epi_week) %>% 
    mutate(time=row_number())
  pre_covid<-temp %>% 
    filter(year<2020)
  fit.gam <- gam(counts ~ factor(epi_week) + s(time),
                 family=nb(), offset= log(exposure),
                 data=pre_covid)
  fun_predict.gam(fit.gam, temp) %>% 
    mutate(lower_bound=first)
  }) 
excess_sens_windows_fig<-excess_sens_windows %>% 
  mutate(model=factor(lower_bound, levels=2018:2016,
                     labels=c("Baseline 2018-2019",
                              "Baseline 2017-2019",
                              "Baseline 2016-2019 (main analysis)"))) %>% 
  select(time, fit, lci, uci, counts, model, lower_bound) %>% 
  pivot_longer(c(fit, counts), names_to = "type", values_to = "value") %>% 
  mutate(type=factor(type, levels=c("fit", "counts"),
                     labels=c("Expected (GAM)", "Observed")))
excess_sens_windows_macrozona<-map_dfr(2016:2018, function(first){
  # last<-2019;first<-2016
  weekly_data_population %>% 
    left_join(region_cw) %>% 
    group_by(macrozona, year, epi_week) %>% 
    summarise(counts=sum(counts),
              exposure=sum(exposure),
              pop=sum(pop)) %>% 
    group_by(macrozona) %>% 
    group_modify(~{
      temp<-.x %>% ungroup() %>% 
        filter(year>=first) %>% 
        ungroup() %>% 
        arrange(year, epi_week) %>% 
        mutate(time=row_number())
      pre_covid<-temp %>% 
        filter(year<2020)
      fit.gam <- gam(counts ~ factor(epi_week) + s(time),
                     family=nb(), offset= log(exposure),
                     data=pre_covid)
      fun_predict.gam(fit.gam, temp) %>% 
        mutate(lower_bound=first)
    })
})
excess_sens_windows_macrozona_fig<-excess_sens_windows_macrozona %>% 
  mutate(model=factor(lower_bound, levels=2018:2016,
                      labels=c("Baseline 2018-2019",
                               "Baseline 2017-2019",
                               "Baseline 2016-2019 (main analysis)"))) %>% 
  select(macrozona, time, fit, lci, uci, counts, model, lower_bound) %>% 
  pivot_longer(c(fit, counts), names_to = "type", values_to = "value") %>% 
  mutate(type=factor(type, levels=c("fit", "counts"),
                     labels=c("Expected (GAM)", "Observed")))
excess_sens_windows_l1<-map_dfr(2016:2018, function(first){
  # last<-2019;first<-2016
  weekly_data_population %>% 
    left_join(region_cw) %>% 
    group_by(SALID1, year, epi_week) %>% 
    summarise(counts=sum(counts),
              exposure=sum(exposure),
              pop=sum(pop)) %>% 
    group_by(SALID1) %>% 
    group_modify(~{
      temp<-.x %>% ungroup() %>% 
        filter(year>=first) %>% 
        ungroup() %>% 
        arrange(year, epi_week) %>% 
        mutate(time=row_number())
      pre_covid<-temp %>% 
        filter(year<2020)
      fit.gam <- gam(counts ~ factor(epi_week) + s(time),
                     family=nb(), offset= log(exposure),
                     data=pre_covid)
      fun_predict.gam(fit.gam, temp) %>% 
        mutate(lower_bound=first)
    })
})
excess_sens_windows_l1_fig<-excess_sens_windows_l1 %>% 
  mutate(model=factor(lower_bound, levels=2018:2016,
                      labels=c("Baseline 2018-2019",
                               "Baseline 2017-2019",
                               "Baseline 2016-2019 (main analysis)"))) %>% 
  select(SALID1, time, fit, lci, uci, counts, model, lower_bound) %>% 
  pivot_longer(c(fit, counts), names_to = "type", values_to = "value") %>% 
  mutate(type=factor(type, levels=c("fit", "counts"),
                     labels=c("Expected (GAM)", "Observed")))
excess_sens_windows_l2<-map_dfr(2016:2018, function(first){
  # last<-2019;first<-2016
  weekly_data_population %>% 
    left_join(region_cw) %>% 
    group_by(SALID2, year, epi_week) %>% 
    summarise(counts=sum(counts),
              exposure=sum(exposure),
              pop=sum(pop)) %>% 
    group_by(SALID2) %>% 
    group_modify(~{
      temp<-.x %>% ungroup() %>% 
        filter(year>=first) %>% 
        ungroup() %>% 
        arrange(year, epi_week) %>% 
        mutate(time=row_number())
      pre_covid<-temp %>% 
        filter(year<2020)
      fit.gam <- gam(counts ~ factor(epi_week) + s(time),
                     family=nb(), offset= log(exposure),
                     data=pre_covid)
      fun_predict.gam(fit.gam, temp) %>% 
        mutate(lower_bound=first)
    })
})
excess_sens_windows_l2_fig<-excess_sens_windows_l2 %>% 
  mutate(model=factor(lower_bound, levels=2018:2016,
                      labels=c("Baseline 2018-2019",
                               "Baseline 2017-2019",
                               "Baseline 2016-2019 (main analysis)"))) %>% 
  select(SALID2, time, fit, lci, uci, counts, model, lower_bound) %>% 
  pivot_longer(c(fit, counts), names_to = "type", values_to = "value") %>% 
  mutate(type=factor(type, levels=c("fit", "counts"),
                     labels=c("Expected (GAM)", "Observed")))

# last: do bootstrapping based on Basellini to propagate error to regressions
# GAM L2
# NOTE: some small L2s do not converge with the bootstrap approach
# adding a tryCatch to exclude them. Sens analysis will restrict to same units
excess_l2_bs<-weekly_data_population %>% 
  group_by(SALID2) %>% 
  group_modify(~{
    #.x<-weekly_data_population %>% filter(SALID2==10310711)
    print(.y$SALID2)
    temp_bs<-tryCatch(expr={
      .x<-.x %>% 
        ungroup() %>% 
        arrange(year, epi_week) %>% 
        mutate(time=row_number())
      pre_covid<-.x %>% 
        filter(year<2020)
      fit.gam <- gam(counts ~ factor(epi_week) + s(time),
                     family=nb(), offset= log(exposure),
                     data=pre_covid)
      ## preparing for the boostrap via deviance residuals. See Basellini https://osf.io/a64t9/
      ## fitted expected values 
      eta.hat <- predict(fit.gam)
      mu.hat <- exp(eta.hat)*pre_covid$exposure
      ## total ED for the fitted curve
      k.hat <- sum(fit.gam$edf) - 53
      ## overdispersion parameter
      theta <- fit.gam$family$getTheta(TRUE)
      ## deviance residuals
      y <- pre_covid$counts
      yy <- y
      yy[y==0] <- 1e-8
      res1 <- sign(y-mu.hat)
      res2 <- 2 * (y * log(yy/mu.hat) - (y + theta) * log((y + theta)/(mu.hat + theta)))
      res <- res1 * sqrt(res2)
      ## exclude NaN
      res <- res[!is.nan(res)]
      ## number of simulations
      ns=1000
      m<-nrow(pre_covid)
      set.seed(333)
      temp_bs<-map_dfr(1:ns, function(s){
        #print(s)
        # s<-1
        ## resample (with replacement) the residuals
        resS <- sample(x=res, size=m, replace = TRUE)
        ## computing u
        u <- (resS^2)/2-y*log(y)+(y+theta)*log(y+theta)
        ## create new set of deaths
        InvDev <- function(mu, y, u, theta){(y+theta)*log(mu+theta) - y*log(mu) - u}
        low <- y
        low[which(resS>0)] <- 0
        up <- y
        up[which(resS<0)] <- 1e8
        yS <- rep(0,m)
        for(j in 1:m){
          yS[j]  <- uniroot(InvDev,
                            lower=low[j], upper=up[j],
                            tol = 1, 
                            y=y[j], 
                            u=u[j],
                            theta=theta)$root
        }
        ## re-fit bootstraped deaths
        pre_covid$yS <- yS
        fit.gamS <- gam(yS ~ factor(epi_week) + s(time,k=round(k.hat)),
                        family=nb(), offset= log(exposure),
                        data=pre_covid)
        temp <- fun_predict.gam(fit.gamS, .x) %>% 
          select(time, fit, se.fit, counts, excess_rel,excess, excess_rate) %>% 
          mutate(iter=s)
        temp
      })
    }, error=function(er){
        data.frame(time=NA)
    })

    
    temp_bs
  })
excess_l1_bs<-weekly_data_population %>% 
  left_join(region_cw) %>% 
  group_by(SALID1, year, epi_week) %>% 
  summarise(counts=sum(counts),
            exposure=sum(exposure),
            pop=sum(pop)) %>% 
  group_by(SALID1) %>% 
  group_modify(~{
    #.x<-weekly_data_population %>% left_join(region_cw) %>% group_by(SALID1, year, epi_week) %>% summarise(counts=sum(counts),exposure=sum(exposure),pop=sum(pop)) %>% filter(SALID1==103101)
    print(.y$SALID1)
    .x<-.x %>% 
      ungroup() %>% 
      arrange(year, epi_week) %>% 
      mutate(time=row_number())
    pre_covid<-.x %>% 
      filter(year<2020)
    fit.gam <- gam(counts ~ factor(epi_week) + s(time),
                   family=nb(), offset= log(exposure),
                   data=pre_covid)
    ## preparing for the boostrap via deviance residuals. See Basellini https://osf.io/a64t9/
    ## fitted expected values 
    eta.hat <- predict(fit.gam)
    mu.hat <- exp(eta.hat)*pre_covid$exposure
    ## total ED for the fitted curve
    k.hat <- sum(fit.gam$edf) - 53
    ## overdispersion parameter
    theta <- fit.gam$family$getTheta(TRUE)
    ## deviance residuals
    y <- pre_covid$counts
    yy <- y
    yy[y==0] <- 1e-8
    res1 <- sign(y-mu.hat)
    res2 <- 2 * (y * log(yy/mu.hat) - (y + theta) * log((y + theta)/(mu.hat + theta)))
    res <- res1 * sqrt(res2)
    ## exclude NaN
    res <- res[!is.nan(res)]
    ## number of simulations
    ns=1000
    m<-nrow(pre_covid)
    set.seed(333)
    temp_bs<-map_dfr(1:ns, function(s){
      # s<-1
      ## resample (with replacement) the residuals
      resS <- sample(x=res, size=m, replace = TRUE)
      ## computing u
      u <- (resS^2)/2-y*log(y)+(y+theta)*log(y+theta)
      ## create new set of deaths
      InvDev <- function(mu, y, u, theta){(y+theta)*log(mu+theta) - y*log(mu) - u}
      low <- y
      low[which(resS>0)] <- 0
      up <- y
      up[which(resS<0)] <- 1e8
      yS <- rep(0,m)
      for(j in 1:m){
        yS[j]  <- uniroot(InvDev,
                          lower=low[j], upper=up[j],
                          tol = 0.1, 
                          y=y[j], 
                          u=u[j],
                          theta=theta)$root
      }
      ## re-fit bootstraped deaths
      pre_covid$yS <- yS
      fit.gamS <- gam(yS ~ factor(epi_week) + s(time,k=round(k.hat)),
                      family=nb(), offset= log(exposure),
                      data=pre_covid)
      temp <- fun_predict.gam(fit.gamS, .x) %>% 
        select(time, fit, se.fit) %>% 
        mutate(iter=s)
      temp
    })
    temp_bs
  })

save.image("analisis/clean_models.rdata")

