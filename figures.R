rm(list=ls())
library(tidyverse)
library(scales)
library(broom)
library(lubridate)
library(gridExtra)
library(sf)
library(rnaturalearth)
library(ggspatial)
library(rnaturalearthdata)
library(rmapshaper)
library(classInt)
load("clean_models.rdata")
select<-dplyr::select
# figure settings
date_limits<-ymd(c("2020-01-01", "2021-07-15"))
fontsize<-16
theme_k <- theme_bw() +
  theme(axis.text = element_text(color="black", size=fontsize),
        axis.title = element_text(face = "bold", color="black", size=fontsize),
        plot.title = element_text(face = "bold", color="black", size=fontsize),
        plot.tag  = element_text(face = "bold", color="black", size=fontsize),
        legend.text = element_text(face = "bold", color="black", size=fontsize),
        legend.title = element_text(face = "bold", color="black", size=fontsize),
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", color="black", size=fontsize))
# unclear where i got this from
get_legend<-function(plot){
  grobs<-ggplotGrob(plot)$grobs
  legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]  
  return(legend)
}

# descriptive tables and figures

# Table 1
t1_overall<-deaths_raw %>%
  mutate(month=month(FECHA_DEF),
         year=year(FECHA_DEF),
         year2=ifelse(year<2020, "2016-2019", as.character(year))) %>% 
  group_by(year2, month) %>% 
  summarise(deaths=n())
t1_salurbal<-deaths_raw %>%
  mutate(month=month(FECHA_DEF),
         year=year(FECHA_DEF),
         year2=ifelse(year<2020, "2016-2019", as.character(year)),
         salurbal=!is.na(SALID2)) %>% 
  group_by(year2, month, salurbal) %>% 
  summarise(deaths=n()) %>% 
  mutate(salurbal=as.character(salurbal)) %>% 
  filter(salurbal=="TRUE")
t1_sex<-deaths_raw %>%
  mutate(month=month(FECHA_DEF),
         year=year(FECHA_DEF),
         year2=ifelse(year<2020, "2016-2019", as.character(year))) %>% 
  filter(!is.na(sex)) %>% 
  group_by(year2, month, sex) %>% 
  summarise(deaths=n())
t1_age<-deaths_raw %>%
  mutate(month=month(FECHA_DEF),
         year=year(FECHA_DEF),
         year2=ifelse(year<2020, "2016-2019", as.character(year))) %>% 
  filter(!is.na(edad_cat)) %>% 
  group_by(year2, month, edad_cat) %>% 
  summarise(deaths=n())
t1<-bind_rows(
  t1_overall %>% mutate(type="overall"),
  t1_salurbal %>% rename(type=salurbal),
  t1_sex %>% rename(type=sex),
  t1_age %>% rename(type=edad_cat)
) %>% 
  filter(!is.na(type)) %>% 
  mutate(type=factor(type, levels=c("overall","TRUE", "FALSE", "Male", "Female",
                                    "<5", "5-19", "20-39", "40-59", "60-74", "75+")),
         year2=factor(year2, levels=c("2016-2019", "2020", "2021"))) %>% 
  mutate(deaths=ifelse(year2=="2016-2019", deaths/4, deaths),
         deaths=deaths/1000,
         deaths=format(deaths, nsmall=1, digits=2)) %>% 
  filter(!(year2=="2021"&month==7)) %>% 
  spread(month, deaths) %>% 
  arrange(type, year2) %>% 
  relocate(type)

pop_tot<-popl2 %>% 
  group_by(year) %>% 
  summarise(pop=sum(pop))
pop_salurbal<-popl2 %>% 
  mutate(salurbal=!is.na(SALID2)) %>% 
  group_by(year, salurbal) %>% 
  summarise(pop=sum(pop)) %>% 
  mutate(salurbal=as.character(salurbal))
pop_age<-popl2 %>% 
  group_by(year, edad_cat) %>% 
  summarise(pop=sum(pop))
pop_sex<-popl2 %>% 
  group_by(year, sex) %>% 
  summarise(pop=sum(pop))

t1_overall<-pop_tot %>% 
  mutate(year2=ifelse(year<2020, "2016-2019", as.character(year)),
         type="overall") %>%
  group_by(type, year2) %>% 
  summarise(pop=mean(pop)) 
t1_salurbal<-pop_salurbal %>% 
  mutate(year2=ifelse(year<2020, "2016-2019", as.character(year))) %>% 
  group_by(salurbal, year2) %>% 
  summarise(pop=mean(pop)) %>% 
  mutate(salurbal=as.character(salurbal)) %>% 
  rename(type=salurbal) %>% 
  filter(type=="TRUE")
t1_sex<-pop_sex %>% 
  mutate(year2=ifelse(year<2020, "2016-2019", as.character(year))) %>% 
  group_by(sex, year2) %>% 
  summarise(pop=mean(pop)) %>% 
  rename(type=sex)
t1_age<-pop_age %>% 
  mutate(year2=ifelse(year<2020, "2016-2019", as.character(year))) %>% 
  group_by(edad_cat, year2) %>% 
  summarise(pop=mean(pop)) %>% 
  rename(type=edad_cat)
t1<-t1 %>% left_join(bind_rows(t1_overall, t1_salurbal, t1_sex, t1_age) %>% 
                       filter(!is.na(type)) %>% 
                       mutate(pop=pop/1000000,
                              pop=format(pop, nsmall=2, digits=2))) %>% 
  mutate_all(~replace_na(., ""))
t1
write_csv(t1, file="results/table1.csv")

# some numbers for results
deaths_raw %>%
  mutate(month=month(FECHA_DEF),
         year=year(FECHA_DEF),
         year2=ifelse(year<2020, "2016-2019", as.character(year))) %>% 
  group_by(year2, month) %>% 
  summarise(deaths=n()) %>%
  mutate(deaths=ifelse(year2=="2016-2019", deaths/4, deaths)) %>% 
  full_join(pop_tot %>% 
              mutate(year2=ifelse(year<2020, "2016-2019", as.character(year)),
                     type="overall") %>%
              group_by(type, year2) %>% 
              summarise(pop=mean(pop)/12)) %>% 
  group_by(year2) %>% 
  summarise_all(mean) %>% 
  mutate(rate=deaths/pop*100000) %>% 
  select(year2, rate) %>% 
  mutate(rate_ref=ifelse(year2=="2016-2019", rate, NA)) %>% 
  mutate(rate_ref=sum(rate_ref, na.rm=T),
         rr=rate/rate_ref)



# app Figure 1
{
  data_overall<-deaths %>%
    group_by(year, epi_week) %>% 
    summarise(n=sum(deaths)) %>% 
    left_join(pop_tot) %>% 
    mutate(rate=(n/pop)*100000) %>% 
    select(-n, -pop) %>% 
    spread(year, rate) %>% 
    mutate(avg=(`2016` + `2017` + `2018` + `2019`)/4)
  data_overall<-data_overall %>% select(epi_week, `2020`, `2021`) %>% 
    gather(year, rate, -epi_week) %>% 
    full_join(data_overall %>% select(epi_week, avg)) %>% 
    mutate(year=as.numeric(year)) %>% 
    full_join(epi_weeks) %>% 
    select(rate, avg, date) %>% 
    gather(type, rate, -date) %>% 
    filter(!is.na(rate), !is.na(type), !is.na(date))
    
  ## salurbal Rates
  data_salurbal<-deaths %>%
    group_by(salurbal, year, epi_week) %>% 
    summarise(n=sum(deaths)) %>% 
    left_join(pop_salurbal %>% mutate(salurbal=salurbal=="TRUE")) %>% 
    mutate(rate=(n/pop)*100000) %>% 
    select(-n, -pop) %>% 
    spread(year, rate) %>% 
    mutate(avg=(`2016` + `2017` + `2018` + `2019`)/4)
  data_salurbal<-data_salurbal %>% select(salurbal, epi_week, `2020`, `2021`) %>% 
    gather(year, rate, -epi_week, -salurbal) %>% 
    full_join(data_salurbal %>% select(salurbal, epi_week, avg)) %>% 
    mutate(year=as.numeric(year)) %>% 
    full_join(epi_weeks) %>% 
    select(salurbal, rate, avg, date) %>% 
    gather(type, rate, -date, -salurbal) %>% 
    filter(salurbal==T) %>% 
    bind_rows(data_overall %>% mutate(salurbal=F)) %>% 
    mutate(type=factor(type, levels=c("avg", "rate"),
                       labels=c("Average of 2016-2019", "2020-2021")),
           salurbal=factor(salurbal, levels=c(F, T),
                           labels=c("All of Chile", "Cities of >100,000 residents"))) %>% 
    filter(!is.na(rate), !is.na(type), !is.na(date))
  
  
  ggplot(data_salurbal, aes(x=date, y=rate, color=type)) +
    geom_vline(xintercept = ymd("2020-03-10"), linetype=2, colour="black") +
    geom_line(size=1) +
    scale_x_date(breaks="3 month", date_labels = "%b%y",
                 limits = date_limits)+
    scale_y_continuous(limits=c(0, NA))+
    # scale_x_continuous(limits = c(1, 53), 
    #                    breaks = c(1, 5, 10, 14, 18, 23, 27, 31, 36, 40, 45, 49),
    #                    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
    #                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
    theme_k + theme(legend.position=c(.8, .2),legend.background = element_blank())+
    scale_colour_manual(values=c("gray70", "black"),name="") +
    geom_vline(xintercept = 11, linetype=2, colour="black", alpha=0.6) +
    labs(#title="Weekly Mortality Rate, Guatemala, January 2015 - December 2020", 
      #subtitle="Epidemiological weeks 1 to 53", 
      #caption = "*First case detected on week 11",
      y="Mortality Rate per 100,000", x="", color="")+
    facet_wrap(~salurbal)
  ggsave("results/Appendix_Fig1.pdf", 
         height = 6, width = 10, scale = 1)
  
  ## sex Rates
  data_sex<-deaths %>%
    group_by(sex, year, epi_week) %>% 
    summarise(n=sum(deaths)) %>% 
    left_join(pop_sex) %>% 
    mutate(rate=(n/pop)*100000) %>% 
    select(-n, -pop) %>% 
    spread(year, rate) %>% 
    mutate(avg=(`2016` + `2017` + `2018` + `2019`)/4)
  data_sex<-data_sex %>% select(sex, epi_week, `2020`, `2021`) %>% 
    gather(year, rate, -epi_week, -sex) %>% 
    full_join(data_sex %>% select(sex, epi_week, avg)) %>% 
    mutate(year=as.numeric(year)) %>% 
    full_join(epi_weeks) %>% 
    select(sex, rate, avg, date) %>% 
    gather(type, rate, -date, -sex) %>% 
    mutate(type=factor(type, levels=c("avg", "rate"),
                       labels=c("Average of 2016-2019", "2020-2021")),
           sex=factor(sex, levels=c("Female", "Male"),
                           labels=c("Women", "Men"))) %>% 
    filter(!is.na(rate), !is.na(type), !is.na(date))
  
  
  ggplot(data_sex, aes(x=date, y=rate, color=type)) +
    geom_vline(xintercept = ymd("2020-03-10"), linetype=2, colour="black") +
    geom_line(size=1) +
    scale_x_date(breaks="3 month", date_labels = "%b%y",
                 limits = date_limits)+
    scale_y_continuous(limits=c(0, NA))+
    # scale_x_continuous(limits = c(1, 53), 
    #                    breaks = c(1, 5, 10, 14, 18, 23, 27, 31, 36, 40, 45, 49),
    #                    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
    #                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
    theme_k + theme(legend.position=c(.8, .2),legend.background = element_blank())+
    scale_colour_manual(values=c("gray70", "black"),name="") +
    geom_vline(xintercept = 11, linetype=2, colour="black", alpha=0.6) +
    labs(#title="Weekly Mortality Rate, Guatemala, January 2015 - December 2020", 
      #subtitle="Epidemiological weeks 1 to 53", 
      #caption = "*First case detected on week 11",
      y="Mortality Rate per 100,000", x="", color="")+
    facet_wrap(~sex)
  ggsave("results/Appendix_Fig2.pdf", 
         height = 6, width = 10, scale = 1)
  ## edad_cat Rates
  data_edad_cat<-deaths %>%
    group_by(edad_cat, year, epi_week) %>% 
    summarise(n=sum(deaths)) %>% 
    left_join(pop_age) %>% 
    mutate(rate=(n/pop)*100000) %>% 
    select(-n, -pop) %>% 
    spread(year, rate) %>% 
    mutate(avg=(`2016` + `2017` + `2018` + `2019`)/4)
  data_edad_cat<-data_edad_cat %>% select(edad_cat, epi_week, `2020`, `2021`) %>% 
    gather(year, rate, -epi_week, -edad_cat) %>% 
    full_join(data_edad_cat %>% select(edad_cat, epi_week, avg)) %>% 
    mutate(year=as.numeric(year)) %>% 
    full_join(epi_weeks) %>% 
    select(edad_cat, rate, avg, date) %>% 
    gather(type, rate, -date, -edad_cat) %>% 
    mutate(type=factor(type, levels=c("avg", "rate"),
                       labels=c("Average of 2016-2019", "2020-2021")),
           edad_cat=factor(edad_cat, levels = c("<5", "5-19","20-39","40-59","60-74","75+"))) %>% 
    filter(!is.na(rate), !is.na(type), !is.na(date))
  
  af4a<-ggplot(data_edad_cat %>% filter(edad_cat%in%c("<5", "5-19","20-39")), 
               aes(x=date, y=rate, color=type)) +
    geom_vline(xintercept = ymd("2020-03-10"), linetype=2, colour="black") +
    geom_line(size=1) +
    scale_x_date(breaks="2 month", date_labels = "%b",
                 limits = date_limits)+
    scale_y_continuous(limits=c(0, NA))+
    # scale_x_continuous(limits = c(1, 53), 
    #                    breaks = c(1, 5, 10, 14, 18, 23, 27, 31, 36, 40, 45, 49),
    #                    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
    #                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
    theme_k + theme(legend.position=c(.8, .2),legend.background = element_blank())+
    scale_colour_manual(values=c("gray70", "black"),name="") +
    geom_vline(xintercept = 11, linetype=2, colour="black", alpha=0.6) +
    labs(#title="Weekly Mortality Rate, Guatemala, January 2015 - December 2020", 
      #subtitle="Epidemiological weeks 1 to 53", 
      #caption = "*First case detected on week 11",
      y="Mortality Rate per 100,000", x="", color="")+
    facet_wrap(~edad_cat)
  af4b<-ggplot(data_edad_cat %>% filter(!edad_cat%in%c("<5", "5-19","20-39")), 
               aes(x=date, y=rate, color=type)) +
    geom_vline(xintercept = ymd("2020-03-10"), linetype=2, colour="black") +
    geom_line(size=1) +
    scale_x_date(breaks="2 month", date_labels = "%b",
                 limits = date_limits)+
    scale_y_continuous(limits=c(0, NA))+
    # scale_x_continuous(limits = c(1, 53), 
    #                    breaks = c(1, 5, 10, 14, 18, 23, 27, 31, 36, 40, 45, 49),
    #                    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
    #                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
    theme_k + theme(legend.position=c(.8, .2),legend.background = element_blank())+
    guides(color=F)+
    scale_colour_manual(values=c("gray70", "black"),name="") +
    geom_vline(xintercept = 11, linetype=2, colour="black", alpha=0.6) +
    labs(#title="Weekly Mortality Rate, Guatemala, January 2015 - December 2020", 
      #subtitle="Epidemiological weeks 1 to 53", 
      #caption = "*First case detected on week 11",
      y="Mortality Rate per 100,000", x="", color="")+
    facet_wrap(~edad_cat)
  
  ggsave("results/Appendix_Fig3.pdf", 
          arrangeGrob(grobs=list(af4a, af4b)),
         height = 10, width = 15, scale = 1)
  
  
}

# cumulative 

cumulative<-excess %>% ungroup() %>% 
  filter(year>=2020) %>% 
  summarise(counts=sum(counts),
            pop=sum(exposure),
            fit=sum(fit),
            se.fit=sqrt(mean(se.fit^2)),
            fit_lci=fit-1.96*se.fit,
            fit_uci=fit+1.96*se.fit) %>% 
  mutate(excess=counts-fit,
         excess_uci=counts-fit_lci,
         excess_lci=counts-fit_uci) %>% 
  select(-se.fit) 
cumulative
# cumulative by week
cumulative_week<-excess %>% ungroup() %>% 
  filter(year>=2020) %>% 
  group_by(year, epi_week) %>% 
  summarise(counts=sum(counts),
            pop=sum(exposure),
            fit=sum(fit),
            se.fit=sqrt(mean(se.fit^2)),
            fit_lci=fit-1.96*se.fit,
            fit_uci=fit+1.96*se.fit) %>% 
  mutate(excess=counts-fit,
         excess_uci=counts-fit_lci,
         excess_lci=counts-fit_uci) %>% 
  ungroup() %>% 
  arrange(year, epi_week) %>% 
  mutate(cum_excess=cumsum(excess),
         time=year+(epi_week-1)/52) %>% 
  left_join(epi_weeks) %>% 
  select(date, year, epi_week, cum_excess) 
cumulative_week %>% filter(year==2020) %>% filter(date==max(date))
ggplot(cumulative_week, aes(x=date, y=cum_excess))+
  geom_line(size=1.5) +
  labs(y="Cumulative Excess Mortality", x="")+
  scale_x_date(breaks="3 months", date_labels = "%b/%y",
               limits=date_limits)+
  scale_color_brewer(type="qual", palette=6 , name="")+
  guides(color=guide_legend(nrow=3))+
  theme_k + theme(legend.position=c(.4, .8),legend.background = element_blank())
ggsave("results/Figure1.pdf", 
       height = 7.5, width = 11, scale = 1)


excess_cumulative_l1<-excess_l1 %>% 
  filter(year>=2020) %>% 
  arrange(epi_week) %>% 
  group_by(SALID1) %>% 
  mutate(fit=cumsum(fit),
         deaths=cumsum(counts),
         excess=deaths-fit) %>% 
  select(SALID1, year, epi_week, excess, deaths, fit, pop)
excess_cumulative_l2<-excess_l2 %>% 
  filter(year>=2020) %>% 
  arrange(epi_week) %>% 
  group_by(SALID2) %>% 
  mutate(fit=cumsum(fit),
         deaths=cumsum(counts),
         excess=deaths-fit) %>% 
  select(SALID2, year, epi_week, excess, deaths, fit, pop) 

# excess_2020=cumulative excess by week 26 of 2021
excess_2020_l2<-excess_cumulative_l2 %>% filter(epi_week==26&year==2021) %>% 
  left_join(region_cw) %>% 
  mutate(relexcess=excess/fit,
         absexcess=excess/pop) %>% 
  arrange(macrozona, relexcess)
excess_2020_l1<-excess_cumulative_l1 %>% filter(epi_week==26&year==2021) %>% 
  left_join(region_cw %>% filter(!duplicated(SALID1)) %>% 
              select(SALID1, macrozona, city_link)) %>% 
  mutate(relexcess=excess/fit,
         absexcess=excess/pop) %>% 
  arrange(macrozona, relexcess)
excess_2020<-full_join(excess_2020_l2, excess_2020_l1 %>% 
                         select(SALID1, relexcessl1=relexcess, absexcessl1=absexcess))
excess_2020$city_link<-factor(excess_2020$city_link, levels=unique(excess_2020_l1$city_link))
excess_2020<-excess_2020 %>% 
  arrange(city_link) %>% 
  mutate(city_link_num=as.numeric(city_link))


ggplot(excess_2020, aes(x=city_link_num, y=relexcess)) +
  geom_hline(yintercept = 0, lty=2)+
  #geom_boxplot(aes(group=city_link), fill=NA, outlier.colour = NA)+
  geom_segment(aes(x=city_link_num-0.25, xend=city_link_num+0.25,
                   y=relexcessl1,
                   yend=relexcessl1, color=macrozona), lty=1, size=1.5)+
  geom_point(aes(fill=macrozona, size=deaths),
             position=position_jitter(width=0.1, height=0), 
             pch=21, color="black") +
  #geom_point(aes(color=macrozona, y=relexcessl1),size=7,pch=8) +
  scale_y_continuous(labels=percent_format(accuracy=1))+
  scale_x_continuous(breaks=1:21, labels=str_wrap(unique(excess_2020$city_link), 10),
                     sec.axis = dup_axis())+
  scale_size_continuous(range=c(2, 9))+
  scale_fill_brewer(type="qual", palette=2, name="")+
  scale_color_brewer(type="qual", palette=2, name="")+
  guides(size=F)+
  labs(x="", y="Relative Excess Mortality (%)")+
  theme_k + theme(axis.text.x = element_text(color="black", size=fontsize-2))
ggsave("results/figure2.pdf", width=25, height=7.5)

# with absolute excess
excess_2020$city_link<-factor(excess_2020$city_link, levels=unique(excess_2020_l1 %>% arrange(macrozona, absexcess) %>% pull(city_link)))
excess_2020<-excess_2020 %>% 
  arrange(city_link) %>% 
  mutate(city_link_num=as.numeric(city_link))


ggplot(excess_2020, aes(x=city_link_num, y=absexcess*100000)) +
  geom_hline(yintercept = 0, lty=2)+
  #geom_boxplot(aes(group=city_link), fill=NA, outlier.colour = NA)+
  geom_segment(aes(x=city_link_num-0.25, xend=city_link_num+0.25,
                   y=absexcessl1*100000,
                   yend=absexcessl1*100000, color=macrozona), lty=1, size=1.5)+
  geom_point(aes(fill=macrozona, size=deaths),
             position=position_jitter(width=0.1, height=0), 
             pch=21, color="black") +
  #geom_point(aes(color=macrozona, y=absexcessl1),size=7,pch=8) +
  scale_y_continuous()+
  scale_x_continuous(breaks=1:21, labels=str_wrap(unique(excess_2020$city_link), 10),
                     sec.axis = dup_axis())+
  scale_size_continuous(range=c(2, 9))+
  scale_fill_brewer(type="qual", palette=2, name="")+
  scale_color_brewer(type="qual", palette=2, name="")+
  guides(size=F)+
  labs(x="", y="Absolute Excess Mortality (excess deaths/100,000)")+
  theme_k + theme(axis.text.x = element_text(color="black", size=fontsize-2))
ggsave("results/figure2_absolute.pdf", width=25, height=7.5)

ggplot(excess_2020_l2, aes(x=absexcess*100000, y=relexcess)) +
  stat_smooth(method="lm", se=F, lty=2, color="black")+
  geom_point(pch=21, fill="gray", color="black") +
  facet_wrap(~macrozona)+
  scale_y_continuous(labels=percent_format(accuracy=1))+
  scale_fill_brewer(type="qual", palette=2, name="")+
  scale_color_brewer(type="qual", palette=2, name="")+
  guides(size=F)+
  labs(y="Relative Excess Mortality (%)", x="Absolute Excess Mortality (excess deaths/100,000)")+
  theme_k + theme(axis.text.x = element_text(color="black", size=fontsize-2))
ggsave("results/Appendix_Fig5.pdf", width=12, height=10)
excess_2020_l2 %>% filter(absexcess>150/100000, relexcess>.75)
ggplot(excess_2020_l2, aes(x=fit/pop*100000, y=relexcess)) +
  stat_smooth(method="lm", se=F, lty=2, color="black")+
  geom_point(pch=21, fill="gray", color="black") +
  facet_wrap(~macrozona)+
  scale_y_continuous(labels=percent_format(accuracy=1))+
  scale_fill_brewer(type="qual", palette=2, name="")+
  scale_color_brewer(type="qual", palette=2, name="")+
  guides(size=F)+
  labs(y="Relative Excess Mortality (%)", x="2016-2019 Crude Mortality Rate (per 100,000)")+
  theme_k + theme(axis.text.x = element_text(color="black", size=fontsize-2))
ggsave("results/Appendix_Fig6.pdf", width=12, height=10)

af5a<-ggplot(excess_2020_l2, aes(x=absexcess*100000, y=relexcess)) +
  stat_smooth(method="lm", se=F, lty=2, color="black")+
  geom_point(pch=21, fill="gray", color="black") +
  #facet_wrap(~macrozona)+
  scale_y_continuous(labels=percent_format(accuracy=1))+
  scale_fill_brewer(type="qual", palette=2, name="")+
  scale_color_brewer(type="qual", palette=2, name="")+
  guides(size=F)+
  labs(y="Relative Excess Mortality (%)", x="Absolute Excess Mortality (excess deaths/100,000)", 
       title="Absolute vs Relative Excess Mortality")+
  theme_k + theme(axis.text.x = element_text(color="black", size=fontsize-2))
af5b<-ggplot(excess_2020_l2, aes(x=fit/pop*100000, y=relexcess)) +
  stat_smooth(method="lm", se=F, lty=2, color="black")+
  geom_point(pch=21, fill="gray", color="black") +
  #facet_wrap(~macrozona)+
  scale_y_continuous(labels=percent_format(accuracy=1))+
  scale_fill_brewer(type="qual", palette=2, name="")+
  scale_color_brewer(type="qual", palette=2, name="")+
  guides(size=F)+
  labs(y="Relative Excess Mortality (%)", x="2016-2019 Crude Mortality Rate (per 100,000)",
       title="Baseline Mortaltiy vs Relative Excess Mortality")+
  theme_k + theme(axis.text.x = element_text(color="black", size=fontsize-2))
af5c<-ggplot(excess_2020_l2, aes(x=fit/pop*100000, y=absexcess*100000)) +
  stat_smooth(method="lm", se=F, lty=2, color="black")+
  geom_point(pch=21, fill="gray", color="black") +
  #facet_wrap(~macrozona)+
  #scale_y_continuous(labels=percent_format(accuracy=1))+
  scale_fill_brewer(type="qual", palette=2, name="")+
  scale_color_brewer(type="qual", palette=2, name="")+
  guides(size=F)+
  labs(y="Absolute Excess Mortality (excess deaths/100,000)", x="2016-2019 Crude Mortality Rate (per 100,000)",
       title="Baseline Mortaltiy vs Absolute Excess Mortality")+
  theme_k + theme(axis.text.x = element_text(color="black", size=fontsize-2))
pall<-arrangeGrob(grobs=list(af5a, af5b, af5c), ncol=3)
ggsave("results/Appendix_Fig5_optionb.pdf", pall, width=20, height=7)



excess_2020 %>% left_join(region_cw) %>% 
  filter(grepl("Metro", macrozona)) %>% arrange(relexcess) %>% 
  select(comuna_name, relexcess) %>% 
  print(n=50)

excess_2020_l1 %>% 
  select(macrozona, city_link, relexcess) %>% 
  arrange(relexcess) %>% 
  print(n=50)

excess_macrozona %>% 
  left_join(epi_weeks) %>% 
  filter(excess_rel>.5, year>=2020) %>% 
  select(excess_rel, epi_week, date, date1, date2) %>% 
  print(n=50)
excess_macrozona %>% 
  left_join(epi_weeks) %>% 
  filter(excess_rel>.2, year>=2020) %>% 
  select(excess_rel, epi_week, date, date1, date2) %>% 
  arrange(macrozona, epi_week) %>% 
  print(n=50)
macro_total<-excess_macrozona %>% 
  filter(year>=2020) %>% 
  arrange(epi_week) %>% 
  group_by(macrozona) %>% 
  mutate(fit=cumsum(fit),
         deaths=cumsum(counts),
         excess=deaths-fit) %>% 
  mutate(excess_rel_total=excess/fit,
         excess=paste0("Total Relative\nExcess Mortality=", round(excess_rel_total*100, digits=1), "%")) %>% 
  filter(epi_week==26&year==2021)


excess_macrozona %>% left_join(region_cw) %>% 
  filter(year>=2020) %>% 
  left_join(epi_weeks) %>% 
  ggplot(aes(x=date, y=excess_rel, group=macrozona)) +
  geom_hline(yintercept = 0, lty=2)+
  #geom_ribbon(aes(ymin=excess_rel_lci, ymax=excess_rel_uci, fill=macrozona), alpha=0.3)+
  #geom_line(aes(color=macrozona))+
  geom_ribbon(aes(ymin=excess_rel_lci, ymax=excess_rel_uci), fill="gray", alpha=1)+
  geom_line(aes(), color="black")+
  geom_text(data=macro_total, aes(x=ymd("2021-01-01"), y=1.4, label=excess))+
  scale_y_continuous(labels=percent_format(accuracy=1))+
  scale_x_date(breaks="6 months", date_labels = "%b/%y",
               limits=date_limits)+
  labs(x="", y="Relative Excess Mortality (%)", color="", fill="")+
  guides(fill=F)+
  facet_wrap(~macrozona, nrow=1)+
  theme_k 
ggsave(filename="results/figure3.pdf", width=15, height=5)

# pooling to get some data for discussion
excess_macrozona %>% group_by(year, epi_week) %>% 
  summarise(excess=sum(excess),
            fit=sum(fit)) %>% 
  mutate(excess_rel_total=excess/fit*100) %>% 
  filter(year>=2020) %>% arrange(desc(excess_rel_total))
excess_macrozona %>% select(year, epi_week, excess_rel) %>% filter(year>=2020) %>% arrange(desc(excess_rel))

# map
world <- ne_countries(continent = "south america", scale = "medium", returnclass = "sf")
load("analisis/data/salurbal/shapefiles.rdata")
cats<-5
colors_map<-rev(colorRampPalette(c("red", "yellow", "darkgreen"))(cats))
map_l1ad<-shp_l1ad %>% #ms_simplify() %>% 
  st_transform(crs=st_crs(world)) %>% 
  left_join(excess_2020_l1) %>% 
  mutate(relexcess=relexcess*100,
         absexcess=absexcess*100000,
         jenks_rel=cut(relexcess, breaks=classIntervals(relexcess,n=cats, style="jenks")$brks, include.lowest = T),
         jenks_rate=cut(absexcess, breaks=classIntervals(absexcess,n=cats, style="jenks")$brks, include.lowest = T)) %>% 
  left_join(exposuresl1 %>% select(SALID1, BECTPOP2020L1AD))
bbox<-st_bbox(map_l1ad)
xlim<-c(bbox$xmin-diff(c(bbox$xmin, bbox$xmax))*0.6, bbox$xmax+diff(c(bbox$xmin, bbox$xmax))*0.6)
ylim<-c(bbox$ymin-diff(c(bbox$ymin, bbox$ymax))*0.1, bbox$ymax+diff(c(bbox$ymin, bbox$ymax))*0.1)
m1<-ggplot(data = world) +
  geom_sf(fill="lightgray") +
  geom_sf(data=map_l1ad, 
          aes(fill=jenks_rel, size=excess), color="black", pch=21)+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)+
  scale_fill_manual(values=colors_map, name="Relative Excess Mortality (%)")+
  scale_color_manual(values=colors_map, name="Relative Excess Mortality (%)")+
  scale_size_continuous(range=c(2, 8))+
  guides(fill=guide_legend(override.aes = list(size=8)), color="none",size="none")+
  coord_sf(xlim = xlim,ylim=ylim, expand = F)+
  theme_void() +
  theme(panel.background = element_rect(fill = "aliceblue"),
        plot.title=element_text(size=20, face="bold"),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14, face="bold"),
        legend.position="right")
m2<-ggplot(data = world) +
  geom_sf(fill="lightgray") +
  geom_sf(data=map_l1ad, 
          aes(fill=jenks_rate, size=excess), color="black", pch=21)+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)+
  scale_fill_manual(values=colors_map, name="Excess Mortality Rate\n(excess deaths per 100,000)")+
  scale_color_manual(values=colors_map, name="Excess Mortality Rate\n(excess deaths per 100,000)")+
  scale_size_continuous(range=c(2, 8))+
  guides(fill=guide_legend(override.aes = list(size=8)), color="none",size="none")+
  coord_sf(xlim = xlim,ylim=ylim, expand = F)+
  theme_void() +
  theme(panel.background = element_rect(fill = "aliceblue"),
        plot.title=element_text(size=20, face="bold"),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14, face="bold"),
        legend.position="right")

ggsave("results/Appendix_Fig7.pdf", arrangeGrob(grobs=list(m1, m2), ncol=2),width=10, height=10)
# 


# regression models
## linear model
## m1: unadjusted, m2: agedjusted by age
## separately for the L2s and then the L1AD
excess_l2_expo<-excess_2020 %>% left_join(exposuresl2) %>% 
  #filter(grepl("Santiago", city_link)) %>% 
  filter(santiago!="other") %>% 
  select(SALID2, santiago, popdens, ap, 
         educ, crowd, pov, educ_hs,
         relexcess,absexcess, excess, comuna_size, prop_60p) %>% 
  gather(type, value, -SALID2, -santiago, -relexcess,-absexcess, -excess, -comuna_size, -prop_60p) %>% 
  mutate(type=factor(type, levels=c("educ","educ_hs","crowd", "pov",
                                    "popdens", "ap"),
                     labels=c("Educational Attainment (University)", 
                              "Educational Attainment (High School)", 
                              "Residential Overcrowding",
                              "Poverty",
                              "Population Density", "Air Pollution")),
         santiago=factor(santiago, 
                         #levels=c(T, F),labels=c("Santiago", "Rest of Chile")))
                         levels=c("santiago", "concepcion", "valparaiso", "other"),
                         labels=c("Santiago", "Concepcion", "Valparaiso", "Rest of Chile")))
t2_l2<-excess_l2_expo %>% 
  group_by(type, santiago) %>% 
  group_modify(~{
    #.x<-excess_l2_expo %>% filter(santiago=="Santiago", type=="Residential Overcrowding")
    .x$valuesd<-as.numeric(scale(.x$value, center=T, scale=T))
    sd<-sd(.x$value)
    .x$relexcess<-.x$relexcess*100
    m1<-lm(relexcess~valuesd, data=.x)
    m2<-lm(relexcess~valuesd+prop_60p, data=.x)
    coefs<-map_dfc(list(m1, m2), function(m){
      tidy(m) %>% filter(term=="valuesd") %>% 
        select(estimate, std.error) %>% 
        mutate(lci=estimate-1.96*std.error,
               uci=estimate+1.96*std.error,
               coef=paste0(round(estimate, digits=2)," (",
                           round(lci, digits=2),";",
                           round(uci, digits=2), ")")) %>% 
        select(coef)
    }) %>% 
      rename(coef1=1, coef2=2) %>% 
      mutate(sd=sd) %>% 
      relocate(sd, .before=1)
    coefs
  }) %>% 
  arrange(santiago, type) %>% 
  select(santiago, type, sd, coef1, coef2)

excess_l1_expo<-excess_2020_l1 %>% left_join(exposuresl1) %>% 
  select(SALID1, popdens, ap, educ, educ_hs, pov, crowd, relexcess, absexcess, excess, city_size, prop_60p) %>% 
  gather(type, value, -SALID1, -relexcess, -excess,-absexcess, -prop_60p) %>% 
  mutate(type=factor(type, levels=c("educ","educ_hs","crowd", "pov",
                                    "popdens", "ap", "city_size"),
                     labels=c("Educational Attainment (University)", 
                              "Educational Attainment (High School)", 
                              "Residential Overcrowding",
                              "Poverty",
                              "Population Density", "Air Pollution",
                              "City Population Size")))
t2_l1<-excess_l1_expo %>% 
  group_by(type) %>% 
  group_modify(~{
    #.x<-excess_l1_expo %>% filter(type=="Residential Overcrowding")
    .x$valuesd<-as.numeric(scale(.x$value, center=T, scale=T))
    sd<-sd(.x$value)
    .x$relexcess<-.x$relexcess*100
    m1<-lm(relexcess~valuesd, data=.x)
    m2<-lm(relexcess~valuesd+prop_60p, data=.x)
    coefs<-map_dfc(list(m1, m2), function(m){
      tidy(m) %>% filter(term=="valuesd") %>% 
        select(estimate, std.error) %>% 
        mutate(lci=estimate-1.96*std.error,
               uci=estimate+1.96*std.error,
               coef=paste0(round(estimate, digits=2)," (",
                           round(lci, digits=2),";",
                           round(uci, digits=2), ")")) %>% 
        select(coef)
    }) %>% 
      rename(coef1=1, coef2=2) %>% 
      mutate(sd=sd) %>% 
      relocate(sd, .before=1)
    coefs
  }) %>% 
  arrange(type) %>% 
  mutate(santiago="L1") %>% 
  select(santiago, type, sd, coef1, coef2)
t2<-bind_rows(t2_l1, t2_l2) %>% 
  mutate(sd=ifelse(type=="Population Density",
                   round(sd, digits=0),
                   round(sd, digits=1)),
         sd=case_when(
           grepl("Educ|Overcr|Pov", type) ~ paste0(sd, "%"),
           grepl("Dens", type) ~ paste0(sd, " pop/km2"),
           grepl("Pollut", type) ~ paste0(sd, " ug/m3"),
           grepl("Size", type) ~ "Doubling",
           T ~ "ERROR")) %>% 
  rename(unadjusted=coef1, adjusted=coef2)

t2
t2 %>% write_csv("results/table2.csv")
# saving relative table
t2_rel<-t2

# absolute
t2_l2<-excess_l2_expo %>% 
  group_by(type, santiago) %>% 
  group_modify(~{
    #.x<-excess_l2_expo %>% filter(santiago=="Santiago", type=="Residential Overcrowding")
    .x$valuesd<-as.numeric(scale(.x$value, center=T, scale=T))
    sd<-sd(.x$value)
    .x$absexcess<-.x$absexcess*100000
    m1<-lm(absexcess~valuesd, data=.x)
    m2<-lm(absexcess~valuesd+prop_60p, data=.x)
    coefs<-map_dfc(list(m1, m2), function(m){
      tidy(m) %>% filter(term=="valuesd") %>% 
        select(estimate, std.error) %>% 
        mutate(lci=estimate-1.96*std.error,
               uci=estimate+1.96*std.error,
               coef=paste0(round(estimate, digits=2)," (",
                           round(lci, digits=2),";",
                           round(uci, digits=2), ")")) %>% 
        select(coef)
    }) %>% 
      rename(coef1=1, coef2=2) %>% 
      mutate(sd=sd) %>% 
      relocate(sd, .before=1)
    coefs
  }) %>% 
  arrange(santiago, type) %>% 
  select(santiago, type, sd, coef1, coef2)
t2_l1<-excess_l1_expo %>% 
  group_by(type) %>% 
  group_modify(~{
    #.x<-excess_l1_expo %>% filter(type=="Residential Overcrowding")
    .x$valuesd<-as.numeric(scale(.x$value, center=T, scale=T))
    sd<-sd(.x$value)
    .x$absexcess<-.x$absexcess*100000
    m1<-lm(absexcess~valuesd, data=.x)
    m2<-lm(absexcess~valuesd+prop_60p, data=.x)
    coefs<-map_dfc(list(m1, m2), function(m){
      tidy(m) %>% filter(term=="valuesd") %>% 
        select(estimate, std.error) %>% 
        mutate(lci=estimate-1.96*std.error,
               uci=estimate+1.96*std.error,
               coef=paste0(round(estimate, digits=2)," (",
                           round(lci, digits=2),";",
                           round(uci, digits=2), ")")) %>% 
        select(coef)
    }) %>% 
      rename(coef1=1, coef2=2) %>% 
      mutate(sd=sd) %>% 
      relocate(sd, .before=1)
    coefs
  }) %>% 
  arrange(type) %>% 
  mutate(santiago="L1") %>% 
  select(santiago, type, sd, coef1, coef2)
t2<-bind_rows(t2_l1, t2_l2) %>% 
  mutate(sd=ifelse(type=="Population Density",
                   round(sd, digits=0),
                   round(sd, digits=1)),
         sd=case_when(
           grepl("Educ|Overcr|Pov", type) ~ paste0(sd, "%"),
           grepl("Dens", type) ~ paste0(sd, " pop/km2"),
           grepl("Pollut", type) ~ paste0(sd, " ug/m3"),
           grepl("Size", type) ~ "Doubling",
           T ~ "ERROR")) %>% 
  rename(unadjusted=coef1, adjusted=coef2)

t2
t2 %>% write_csv("results/table2_absolute.csv")
# saving abs table
t2_abs<-t2
full_join(t2_rel %>% select(santiago, type, sd, relative=adjusted),
          t2_abs %>% select(santiago, type, sd, absolute=adjusted)) %>% 
  write_csv("results/table2_combined.csv")

# bootstrap
excess_l1_expo_bs<-excess_l1_bs %>% 
  full_join(excess_l1 %>% select(SALID1, time, counts, pop)) %>% 
  mutate(year=as.numeric(substr(time, 1, 4)),
         epi_week=as.integer(round((time-year)*52+1))) %>% 
  filter(year>=2020) %>% 
  arrange(iter, SALID1, epi_week) %>% 
  group_by(iter, SALID1) %>% 
  mutate(fit=cumsum(fit),
         deaths=cumsum(counts),
         excess=deaths-fit) %>% 
  select(SALID1, iter, year, epi_week, excess, deaths, fit, pop) %>% 
  filter(epi_week==26&year==2021) %>% 
  left_join(region_cw %>% filter(!duplicated(SALID1)) %>% 
              select(SALID1, macrozona, city_link)) %>% 
  mutate(relexcess=excess/fit,
         absexcess=excess/pop) %>% 
  arrange(macrozona, relexcess) %>% 
  left_join(exposuresl1) %>% 
  select(SALID1, iter, popdens, ap, educ, educ_hs, pov, crowd, relexcess, absexcess, excess, city_size, prop_60p) %>% 
  gather(type, value, -SALID1, -relexcess, -excess,-absexcess, -prop_60p, -iter) %>% 
  mutate(type=factor(type, levels=c("educ","educ_hs","crowd", "pov",
                                    "popdens", "ap", "city_size"),
                     labels=c("Educational Attainment (University)", 
                              "Educational Attainment (High School)", 
                              "Residential Overcrowding",
                              "Poverty",
                              "Population Density", "Air Pollution",
                              "City Population Size")))
t2_l1_bs<-excess_l1_expo_bs %>% 
  group_by(type, iter) %>% 
  group_modify(~{
    #.x<-excess_l1_expo_bs %>% filter(type=="Residential Overcrowding", iter==1)
    if (.y$iter==1) print(as.character(.y$type))
    sd<-sd(.x$value)
    .x$valuesd<-as.numeric(scale(.x$value, center=T, scale=T))
    .x$relexcess<-.x$relexcess*100
    .x$absexcess<-.x$absexcess*100000
    m1<-lm(relexcess~valuesd+prop_60p, data=.x)
    m2<-lm(absexcess~valuesd+prop_60p, data=.x)
    coefs<-map2_dfr(list(m1, m2),c("rel", "abs"),function(m, mod){
      #m<-m1
      tidy(m) %>% filter(term=="valuesd") %>% 
        select(estimate, std.error) %>% 
        mutate(model=mod)
    }) %>% 
      mutate(sd=sd) %>% 
      relocate(sd, .before=1)
    coefs
  }) %>% 
  arrange(type, iter) %>% 
  mutate(santiago="L1") %>% 
  select(santiago, iter, type, sd, estimate, std.error, model)
# for table
t2_l1_bs_b<-t2_l1_bs %>% 
  group_by(santiago, type, model) %>% 
  summarise(est=mean(estimate),
            var_btw=var(estimate),
            var_wtn=mean(std.error^2)) %>% 
  rename(estimate=est) %>% 
  mutate(std.error=sqrt(var_btw+var_wtn),
         lci=estimate-1.96*std.error,
         uci=estimate+1.96*std.error,
         coef=paste0(round(estimate, digits=2)," (",
                     round(lci, digits=2),";",
                     round(uci, digits=2), ")")) %>% 
  select(santiago, type, model, estimate, coef, lci, uci)

# compare
t2_l1_comp<-excess_l1_expo %>% 
  group_by(type) %>% 
  group_modify(~{
    #.x<-excess_l1_expo %>% filter(type=="Residential Overcrowding")
    sd<-sd(.x$value)
    .x$valuesd<-as.numeric(scale(.x$value, center=T, scale=T))
    .x$relexcess<-.x$relexcess*100
    .x$absexcess<-.x$absexcess*100000
    m1<-lm(relexcess~valuesd+prop_60p, data=.x)
    m2<-lm(absexcess~valuesd+prop_60p, data=.x)
    coefs<-map2_dfr(list(m1, m2),c("rel", "abs"),function(m, mod){
      #m<-m1
      tidy(m) %>% filter(term=="valuesd") %>% 
        select(estimate, std.error) %>% 
        mutate(model=mod)
    }) %>% 
      mutate(sd=sd) %>% 
      relocate(sd, .before=1)
    coefs
  }) %>% 
  arrange(type) %>% 
  mutate(santiago="L1",
         lci=estimate-1.96*std.error,
         uci=estimate+1.96*std.error) %>% 
  select(santiago, type, sd, estimate, lci, uci, model)
both<-bind_rows(t2_l1_comp%>% mutate(approach="orig"), 
                t2_l1_bs_b %>% select(-coef) %>% mutate(approach="bs")) %>% 
  pivot_wider(id_cols=c(santiago, type, model),
              names_from=approach,
              values_from=c(estimate, lci, uci)) %>% 
  ungroup() %>% 
  mutate(model=factor(model, levels=c("rel", "abs"),
                      labels=c("Relative", "Absolute")))
both_rel<-both %>% filter(model=="Relative")
both_abs<-both %>% filter(model=="Absolute")
f8_rel<-ggplot(both_rel, aes(x=estimate_orig, y=estimate_bs)) +
  geom_abline(intercept=0, slope=1, lty=2)+
  geom_vline(xintercept = 0, lty=2)+
  geom_hline(yintercept = 0, lty=2)+
  geom_linerange(aes(ymin=lci_bs, ymax=uci_bs, color=type))+
  geom_linerange(aes(xmin=lci_orig, xmax=uci_orig, color=type))+
  geom_point(aes(fill=type), pch=21, size=2, color="black")+
  scale_y_continuous(limits=c(both_rel %>% select(contains("lci")) %>% min, both_rel %>% select(contains("uci")) %>% max))+
  scale_x_continuous(limits=c(both_rel %>% select(contains("lci")) %>% min, both_rel %>% select(contains("uci")) %>% max))+
  scale_color_brewer(name="",type="qual", palette=2)+
  scale_fill_brewer(name="",type="qual", palette=2)+
  #facet_wrap(~model)+
  labs(x="Estimate and 95% CI (Main Analysis)",
       y="Estimate and 95% CI (Bootstrap)",
       title="Relative",
       tag="A")+
  theme_k
f8_abs<-ggplot(both_abs, aes(x=estimate_orig, y=estimate_bs)) +
  geom_abline(intercept=0, slope=1, lty=2)+
  geom_vline(xintercept = 0, lty=2)+
  geom_hline(yintercept = 0, lty=2)+
  geom_linerange(aes(ymin=lci_bs, ymax=uci_bs, color=type))+
  geom_linerange(aes(xmin=lci_orig, xmax=uci_orig, color=type))+
  geom_point(aes(fill=type), pch=21, size=2, color="black")+
  scale_y_continuous(limits=c(both_abs %>% select(contains("lci")) %>% min, both_abs %>% select(contains("uci")) %>% max))+
  scale_x_continuous(limits=c(both_abs %>% select(contains("lci")) %>% min, both_abs %>% select(contains("uci")) %>% max))+
  scale_color_brewer(name="",type="qual", palette=2)+
  scale_fill_brewer(name="",type="qual", palette=2)+
  #facet_wrap(~model)+
  labs(x="Estimate and 95% CI (Main Analysis)",
       y="Estimate and 95% CI (Bootstrap)",
       title="Absolute",
       tag="B")+
  theme_k
legend<-get_legend(f8_rel)
f8<-arrangeGrob(grobs=list(f8_rel+guides(fill="none", color="none"),
                       f8_abs+guides(fill="none", color="none")),
            ncol=2)
f8<-arrangeGrob(grobs=list(f8, legend), ncol=1, heights=c(15, 1))

ggsave(filename="results/Appendix_Fig8.pdf", f8, width=15, height=8)
# and table
t2_l1_bs_b %>% select(-estimate, -lci, -uci) %>%  
  mutate(sd=ifelse(type=="Population Density",
                   round(sd, digits=0),
                   round(sd, digits=1)),
         sd=case_when(
           grepl("Educ|Overcr|Pov", type) ~ paste0(sd, "%"),
           grepl("Dens", type) ~ paste0(sd, " pop/km2"),
           grepl("Pollut", type) ~ paste0(sd, " ug/m3"),
           grepl("Size", type) ~ "Doubling",
           T ~ "ERROR")) %>% 
  pivot_wider(id_cols=-model, names_from=model, values_from=coef) %>% 
  select(santiago, type, sd, unadjusted=unadj, adjusted=adj) %>% 
  write_csv("results/Appendix_table2.csv")

# bootstrap L2
# we were not able to bootstrap all units (10 missing)
include<-excess_l2_bs %>% filter(!is.na(fit)) %>% pull(SALID2) %>% unique


excess_l2_expo_bs<-excess_l2_bs %>% 
  filter(SALID2%in%include) %>% 
  left_join(excess_l2 %>% select(SALID2, time, pop)) %>% 
  mutate(year=as.numeric(substr(time, 1, 4)),
         epi_week=as.integer(round((time-year)*52+1))) %>% 
  filter(year>=2020) %>% 
  arrange(iter, SALID2, epi_week) %>% 
  group_by(iter, SALID2) %>% 
  mutate(fit=cumsum(fit),
         deaths=cumsum(counts),
         excess=deaths-fit) %>% 
  select(SALID2, iter, year, epi_week, excess, deaths, fit, pop) %>% 
  filter(epi_week==26&year==2021) %>% 
  left_join(region_cw %>% filter(!duplicated(SALID2)) %>% 
              select(SALID2, macrozona, city_link)) %>% 
  mutate(relexcess=excess/fit,
         absexcess=excess/pop) %>% 
  arrange(macrozona, relexcess) %>% 
  left_join(exposuresl2) %>% 
  #filter(grepl("Santiago", city_link)) %>% 
  filter(santiago!="other") %>% 
  select(SALID2, iter, santiago, popdens, ap, 
         educ, crowd, pov, educ_hs,
         relexcess,absexcess, excess, comuna_size, prop_60p) %>% 
  gather(type, value, -SALID2, -santiago, -relexcess,-absexcess, -excess, -comuna_size, -prop_60p, -iter) %>% 
  mutate(type=factor(type, levels=c("educ","educ_hs","crowd", "pov",
                                    "popdens", "ap"),
                     labels=c("Educational Attainment (University)", 
                              "Educational Attainment (High School)", 
                              "Residential Overcrowding",
                              "Poverty",
                              "Population Density", "Air Pollution")),
         santiago=factor(santiago, 
                         #levels=c(T, F),labels=c("Santiago", "Rest of Chile")))
                         levels=c("santiago", "concepcion", "valparaiso", "other"),
                         labels=c("Santiago", "Concepcion", "Valparaiso", "Rest of Chile")))
  
t2_l2_bs<-excess_l2_expo_bs %>% 
  group_by(santiago, type, iter) %>% 
  group_modify(~{
    #.x<-excess_l2_expo_bs %>% filter(santiago=="Santiago", type=="Residential Overcrowding", iter==1)
    if (.y$iter==1&.y$type=="Educational Attainment (University)") print(as.character(.y$santiago))
    if (.y$iter==1) print(as.character(.y$type))
    sd<-sd(.x$value)
    .x$valuesd<-as.numeric(scale(.x$value, center=T, scale=T))
    .x$relexcess<-.x$relexcess*100
    .x$absexcess<-.x$absexcess*100000
    m1<-lm(relexcess~valuesd+prop_60p, data=.x)
    m2<-lm(absexcess~valuesd+prop_60p, data=.x)
    coefs<-map2_dfr(list(m1, m2),c("rel", "abs"),function(m, mod){
      #m<-m1
      tidy(m) %>% filter(term=="valuesd") %>% 
        select(estimate, std.error) %>% 
        mutate(model=mod)
    }) %>% 
      mutate(sd=sd) %>% 
      relocate(sd, .before=1)
    coefs
  }) %>% 
  arrange(santiago, type, iter) %>% 
  select(santiago, iter, type, sd, estimate, std.error, model)
# for table
t2_l2_bs_b<-t2_l2_bs %>% 
  group_by(santiago, type, model) %>% 
  summarise(est=mean(estimate),
            var_btw=var(estimate),
            var_wtn=mean(std.error^2)) %>% 
  rename(estimate=est) %>% 
  mutate(std.error=sqrt(var_btw+var_wtn),
         lci=estimate-1.96*std.error,
         uci=estimate+1.96*std.error,
         coef=paste0(round(estimate, digits=2)," (",
                     round(lci, digits=2),";",
                     round(uci, digits=2), ")")) %>% 
  select(santiago, type, model, estimate, coef, lci, uci)

# compare
t2_l2_comp<-excess_l2_expo %>% 
  filter(SALID2%in%include) %>% 
  group_by(santiago, type) %>% 
  group_modify(~{
    #.x<-excess_l2_expo %>% filter(type=="Residential Overcrowding")
    sd<-sd(.x$value)
    .x$valuesd<-as.numeric(scale(.x$value, center=T, scale=T))
    .x$relexcess<-.x$relexcess*100
    .x$absexcess<-.x$absexcess*100000
    m1<-lm(relexcess~valuesd+prop_60p, data=.x)
    m2<-lm(absexcess~valuesd+prop_60p, data=.x)
    coefs<-map2_dfr(list(m1, m2),c("rel", "abs"),function(m, mod){
      #m<-m1
      tidy(m) %>% filter(term=="valuesd") %>% 
        select(estimate, std.error) %>% 
        mutate(model=mod)
    }) %>% 
      mutate(sd=sd) %>% 
      relocate(sd, .before=1)
    coefs
  }) %>% 
  arrange(santiago, type) %>% 
  mutate(lci=estimate-1.96*std.error,
         uci=estimate+1.96*std.error) %>% 
  select(santiago, type, sd, estimate, lci, uci, model)
both<-bind_rows(t2_l2_comp%>% mutate(approach="orig"), 
                t2_l2_bs_b %>% select(-coef) %>% mutate(approach="bs")) %>% 
  pivot_wider(id_cols=c(santiago, type, model),
              names_from=approach,
              values_from=c(estimate, lci, uci)) %>% 
  ungroup() %>% 
  mutate(model=factor(model, levels=c("rel", "abs"),
                      labels=c("Relative", "Absolute")))
both_rel<-both %>% filter(model=="Relative")
both_abs<-both %>% filter(model=="Absolute")
f8_rel<-ggplot(both_rel, aes(x=estimate_orig, y=estimate_bs)) +
  geom_abline(intercept=0, slope=1, lty=2)+
  geom_vline(xintercept = 0, lty=2)+
  geom_hline(yintercept = 0, lty=2)+
  geom_linerange(aes(ymin=lci_bs, ymax=uci_bs, color=type))+
  geom_linerange(aes(xmin=lci_orig, xmax=uci_orig, color=type))+
  geom_point(aes(fill=type), pch=21, size=2, color="black")+
  scale_y_continuous(limits=c(both_rel %>% select(contains("lci")) %>% min, both_rel %>% select(contains("uci")) %>% max))+
  scale_x_continuous(limits=c(both_rel %>% select(contains("lci")) %>% min, both_rel %>% select(contains("uci")) %>% max))+
  scale_color_brewer(name="",type="qual", palette=2)+
  scale_fill_brewer(name="",type="qual", palette=2)+
  facet_wrap(~santiago)+
  labs(x="Estimate and 95% CI (Main Analysis)",
       y="Estimate and 95% CI (Bootstrap)",
       title="Relative",
       tag="A")+
  theme_k
f8_abs<-ggplot(both_abs, aes(x=estimate_orig, y=estimate_bs)) +
  geom_abline(intercept=0, slope=1, lty=2)+
  geom_vline(xintercept = 0, lty=2)+
  geom_hline(yintercept = 0, lty=2)+
  geom_linerange(aes(ymin=lci_bs, ymax=uci_bs, color=type))+
  geom_linerange(aes(xmin=lci_orig, xmax=uci_orig, color=type))+
  geom_point(aes(fill=type), pch=21, size=2, color="black")+
  scale_y_continuous(limits=c(both_abs %>% select(contains("lci")) %>% min, both_abs %>% select(contains("uci")) %>% max))+
  scale_x_continuous(limits=c(both_abs %>% select(contains("lci")) %>% min, both_abs %>% select(contains("uci")) %>% max))+
  scale_color_brewer(name="",type="qual", palette=2)+
  scale_fill_brewer(name="",type="qual", palette=2)+
  facet_wrap(~santiago)+
  labs(x="Estimate and 95% CI (Main Analysis)",
       y="Estimate and 95% CI (Bootstrap)",
       title="Absolute",
       tag="B")+
  theme_k
legend<-get_legend(f8_rel)
f9<-arrangeGrob(grobs=list(f8_rel+guides(fill="none", color="none"),
                           f8_abs+guides(fill="none", color="none")),
                ncol=1)
f9<-arrangeGrob(grobs=list(f9, legend), ncol=1, heights=c(15, 1))

ggsave(filename="results/Appendix_Fig8l2.pdf", f9, width=12, height=8)
# and table
t2_l2_bs_b %>% select(-estimate, -lci, -uci) %>%  
  mutate(sd=ifelse(type=="Population Density",
                   round(sd, digits=0),
                   round(sd, digits=1)),
         sd=case_when(
           grepl("Educ|Overcr|Pov", type) ~ paste0(sd, "%"),
           grepl("Dens", type) ~ paste0(sd, " pop/km2"),
           grepl("Pollut", type) ~ paste0(sd, " ug/m3"),
           grepl("Size", type) ~ "Doubling",
           T ~ "ERROR")) %>% 
  pivot_wider(id_cols=-model, names_from=model, values_from=coef) %>% 
  select(santiago, type, sd, unadjusted=unadj, adjusted=adj) %>% 
  write_csv("results/Appendix_table2l2.csv")



# assoc figures by l2 and by l1 separately, first l1
f4_data<-excess_l1_expo %>% 
  left_join(exposuresl1 %>% select(SALID1, city_size)) %>% 
  mutate(type=factor(type, levels=c("Educational Attainment (University)", 
                                    "Educational Attainment (High School)", 
                                    "Residential Overcrowding",
                                    "Poverty",
                                    "Population Density", "Air Pollution",
                                    "City Population Size"),
                     labels=c("Educational Attainment (% w/University Education or more)", 
                              "Educational Attainment (% w/High School or more)", 
                              "% Households in Residential Overcrowding",
                              "% Living in Poverty",
                              "Population Density (pop/km2)", "Air Pollution (PM2.5 ug/m3)",
                              "City Population Size (log2 millions)")))
ggplot(f4_data, aes(x=value, y=relexcess)) +
  geom_point(aes(size=city_size), pch=21, color="black", fill="gray")+
  stat_smooth(method="lm", color="blue", se=F, lty=2)+
  #stat_smooth(method="loess", color="black")+
  scale_y_continuous(labels=percent_format(accuracy=1))+
  labs(x="", 
       y="Relative Excess Mortality (%)", color="", fill="")+
  facet_wrap(~type, strip.position = "bottom", scales="free_x", ncol=4,
             labeller = labeller(type = label_wrap_gen(35)))+
  guides(size=F)+
  theme_k  + theme(strip.placement = "outside")
ggsave(filename="results/figure4.pdf", width=17, height=12)

f5_data<-excess_l2_expo %>% 
  mutate(type=factor(type, levels=c("Educational Attainment (University)", 
                                    "Educational Attainment (High School)", 
                                    "Residential Overcrowding",
                                    "Poverty",
                                    "Population Density", "Air Pollution",
                                    "City Population Size"),
                     labels=c("Educational Attainment (% w/University Education or more)", 
                              "Educational Attainment (% w/High School or more)", 
                              "% Households in Residential Overcrowding",
                              "% Living in Poverty",
                              "Population Density (pop/km2)", "Air Pollution (PM2.5 ug/m3)",
                              "City Population Size (log2 millions)")))
f5<-f5_data %>% group_by(santiago) %>% 
  group_map(~{
    ggplot(.x, aes(x=value, y=relexcess)) +
      geom_point(aes(size=comuna_size), pch=21, color="black", fill="gray")+
      stat_smooth(method="lm", color="blue", se=F, lty=2)+
      #stat_smooth(method="loess", color="black")+
      scale_y_continuous(labels=percent_format(accuracy=1))+
      labs(x="", title=.y$santiago,
           y="Relative Excess Mortality (%)", color="", fill="")+
      facet_wrap(~type, strip.position= "bottom", scales="free_x", ncol=6,
                 labeller = labeller(type = label_wrap_gen(35)))+
      guides(size=F)+
      theme_k  + theme(strip.placement = "outside")
  })
f5<-arrangeGrob(grobs=f5, ncol=1)
ggsave(filename="results/figure5.pdf", f5, width=24, height=12)

# last, a test figure showing appendix figure 1, but per city
data_SALID1<-deaths %>%
  group_by(SALID1, year, epi_week) %>% 
  summarise(n=sum(deaths)) %>% 
  left_join(popl1 %>% group_by(SALID1, year) %>% summarise(pop=sum(pop))) %>% 
  mutate(rate=(n/pop)*100000) %>% 
  select(-n, -pop) %>% 
  spread(year, rate) %>% 
  mutate(avg=(`2016` + `2017` + `2018` + `2019`)/4)
data_SALID1<-data_SALID1 %>% select(SALID1, epi_week, `2020`, `2021`) %>% 
  gather(year, rate, -epi_week, -SALID1) %>% 
  full_join(data_SALID1 %>% select(SALID1, epi_week, avg)) %>% 
  mutate(year=as.numeric(year)) %>% 
  full_join(epi_weeks) %>% 
  select(SALID1, rate, avg, date) %>% 
  gather(type, rate, -date, -SALID1) %>% 
  mutate(type=factor(type, levels=c("avg", "rate"),
                     labels=c("Average of 2016-2019", "2020-2021"))) %>% 
  filter(!is.na(rate), !is.na(type), !is.na(date)) %>% 
  left_join(region_cw %>% filter(!duplicated(SALID1)) %>% select(SALID1, city_link)) %>% 
  filter(!is.na(SALID1)) %>% 
  # sort cities by excess l1
  mutate(city_link=factor(city_link,
                          levels=excess_2020 %>% ungroup() %>% filter(!duplicated(SALID1)) %>% 
                            arrange(relexcessl1) %>% pull(city_link)))

ggplot(data_SALID1, aes(x=date, y=rate, color=type)) +
  geom_vline(xintercept = ymd("2020-03-10"), linetype=2, colour="black") +
  geom_line(size=1) +
  scale_x_date(breaks="6 months", date_labels = "%b/%y",
               limits = date_limits)+
  scale_y_continuous(limits=c(0, NA))+
  # scale_x_continuous(limits = c(1, 53), 
  #                    breaks = c(1, 5, 10, 14, 18, 23, 27, 31, 36, 40, 45, 49),
  #                    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  #                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_k + theme(legend.position="bottom",legend.background = element_blank())+
  scale_colour_manual(values=c("gray70", "black"),name="") +
  geom_vline(xintercept = 11, linetype=2, colour="black", alpha=0.6) +
  labs(#title="Weekly Mortality Rate, Guatemala, January 2015 - December 2020", 
    #subtitle="Epidemiological weeks 1 to 53", 
    #caption = "*First case detected on week 11",
    y="Mortality Rate per 100,000", x="", color="")+
  facet_wrap(~city_link, nrow=3)
ggsave("results/Appendix_Fig4.pdf", 
       height = 15, width = 20, scale = 1.2)

# Sens window periods
ggplot(excess_sens_windows_fig, aes(x=time, y=value)) +
  geom_ribbon(aes(ymin=lci, ymax=uci), fill="gray")+
  geom_line(aes(color=type))+
  #geom_hline(aes(yintercept = 0), lty=2)+
  geom_vline(aes(xintercept = lower_bound), lty=1)+
  geom_vline(xintercept=2020, lty=2)+
  scale_color_manual(values=c("black", "red"), name="")+
  scale_x_continuous(breaks=2016:2021)+
  scale_y_continuous(limits=c(1000, 3100))+
  facet_grid(~model)+
  labs(x="Year", y="Weekly Death Counts")+
  theme_k + theme(legend.position=c(.15, .85),legend.background = element_blank())+
  guides(color=guide_legend(nrow=2))
ggsave("results/Appendix_Fig9.pdf", 
       height = 7.5, width = 14)
# sens window periods by L1 (compare)
excess_cumulative_l1_windows<-excess_sens_windows_l1 %>% 
  filter(year>=2020) %>% 
  arrange(lower_bound, SALID1, epi_week) %>% 
  group_by(lower_bound, SALID1) %>% 
  mutate(fit=cumsum(fit),
         deaths=cumsum(counts),
         excess=deaths-fit) %>% 
  select(SALID1, lower_bound, year, epi_week, excess, deaths, fit, pop) %>% 
  filter(epi_week==26&year==2021) %>% 
  left_join(region_cw %>% filter(!duplicated(SALID1)) %>% 
              select(SALID1, macrozona, city_link)) %>% 
  mutate(rel=excess/fit*100,
         abs=excess/pop*100000) %>% 
  select(city_link, macrozona, SALID1, lower_bound, rel, abs) %>% 
  pivot_wider(id_cols=c(city_link, macrozona, SALID1), names_from=lower_bound, values_from = c(rel, abs)) %>% 
  ungroup()
{
  f_rel_18<-ggplot(excess_cumulative_l1_windows, aes(x=rel_2016, y=rel_2018)) +
    geom_abline(intercept=0, slope=1, lty=2)+
    geom_vline(xintercept = 0, lty=2)+
    geom_hline(yintercept = 0, lty=2)+
    #geom_linerange(aes(ymin=lci_bs, ymax=uci_bs, color=type))+
    #geom_linerange(aes(xmin=lci_orig, xmax=uci_orig, color=type))+
    geom_point(aes(fill=macrozona), pch=21, size=2, color="black")+
    scale_y_continuous(limits=c(excess_cumulative_l1_windows %>% select(contains("rel")) %>% min, excess_cumulative_l1_windows %>% select(contains("rel")) %>% max))+
    scale_x_continuous(limits=c(excess_cumulative_l1_windows %>% select(contains("rel")) %>% min, excess_cumulative_l1_windows %>% select(contains("rel")) %>% max))+
    scale_color_brewer(name="Region",type="qual", palette=2)+
    scale_fill_brewer(name="Region",type="qual", palette=2)+
    labs(x="Relative Excess Mortality (%) using 2016-2019 Baseline (main)",
         y="Relative Excess Mortality (%) using 2018-2019 Baseline",
         tag="B")+
    theme_k
  f_rel_17<-ggplot(excess_cumulative_l1_windows, aes(x=rel_2016, y=rel_2017)) +
    geom_abline(intercept=0, slope=1, lty=2)+
    geom_vline(xintercept = 0, lty=2)+
    geom_hline(yintercept = 0, lty=2)+
    #geom_linerange(aes(ymin=lci_bs, ymax=uci_bs, color=type))+
    #geom_linerange(aes(xmin=lci_orig, xmax=uci_orig, color=type))+
    geom_point(aes(fill=macrozona), pch=21, size=2, color="black")+
    scale_y_continuous(limits=c(excess_cumulative_l1_windows %>% select(contains("rel")) %>% min, excess_cumulative_l1_windows %>% select(contains("rel")) %>% max))+
    scale_x_continuous(limits=c(excess_cumulative_l1_windows %>% select(contains("rel")) %>% min, excess_cumulative_l1_windows %>% select(contains("rel")) %>% max))+
    scale_color_brewer(name="Region",type="qual", palette=2)+
    scale_fill_brewer(name="Region",type="qual", palette=2)+
    labs(x="Relative Excess Mortality (%) using 2016-2019 Baseline (main)",
         y="Relative Excess Mortality (%) using 2017-2019 Baseline",
         tag="A")+
    theme_k
  f_abs_18<-ggplot(excess_cumulative_l1_windows, aes(x=abs_2016, y=abs_2018)) +
    geom_abline(intercept=0, slope=1, lty=2)+
    geom_vline(xintercept = 0, lty=2)+
    geom_hline(yintercept = 0, lty=2)+
    #geom_linerange(aes(ymin=lci_bs, ymax=uci_bs, color=type))+
    #geom_linerange(aes(xmin=lci_orig, xmax=uci_orig, color=type))+
    geom_point(aes(fill=macrozona), pch=21, size=2, color="black")+
    scale_y_continuous(limits=c(excess_cumulative_l1_windows %>% select(contains("abs")) %>% min, excess_cumulative_l1_windows %>% select(contains("abs")) %>% max))+
    scale_x_continuous(limits=c(excess_cumulative_l1_windows %>% select(contains("abs")) %>% min, excess_cumulative_l1_windows %>% select(contains("abs")) %>% max))+
    scale_color_brewer(name="Region",type="qual", palette=2)+
    scale_fill_brewer(name="Region",type="qual", palette=2)+
    labs(x="Excess Mortality Rate per 100,000 using 2016-2019 Baseline (main)",
         y="Excess Mortality Rate per 100,000 using 2018-2019 Baseline",
         tag="D")+
    theme_k
  f_abs_17<-ggplot(excess_cumulative_l1_windows, aes(x=abs_2016, y=abs_2017)) +
    geom_abline(intercept=0, slope=1, lty=2)+
    geom_vline(xintercept = 0, lty=2)+
    geom_hline(yintercept = 0, lty=2)+
    #geom_linerange(aes(ymin=lci_bs, ymax=uci_bs, color=type))+
    #geom_linerange(aes(xmin=lci_orig, xmax=uci_orig, color=type))+
    geom_point(aes(fill=macrozona), pch=21, size=2, color="black")+
    scale_y_continuous(limits=c(excess_cumulative_l1_windows %>% select(contains("abs")) %>% min, excess_cumulative_l1_windows %>% select(contains("abs")) %>% max))+
    scale_x_continuous(limits=c(excess_cumulative_l1_windows %>% select(contains("abs")) %>% min, excess_cumulative_l1_windows %>% select(contains("abs")) %>% max))+
    scale_color_brewer(name="Region",type="qual", palette=2)+
    scale_fill_brewer(name="Region",type="qual", palette=2)+
    labs(x="Excess Mortality Rate per 100,000 using 2016-2019 Baseline (main)",
         y="Excess Mortality Rate per 100,000 using 2017-2019 Baseline",
         tag="C")+
    theme_k
  pall<-arrangeGrob(grobs=list(f_rel_17, f_rel_18, 
                               f_abs_17, f_abs_18), ncol=2)
}
ggsave("results/Appendix_Fig10.pdf", pall, width=18, height=15)

# sens analysis using empirical
excess_cumulative_l1_empirical<-excess_empirical_l1 %>% 
  filter(time>=2020, time<=2021.481) %>% 
  arrange(SALID1, time) %>% 
  group_by(SALID1) %>% 
  mutate(fit=cumsum(counts_pre),
         deaths=cumsum(counts),
         pop=cumsum(pop)) %>% 
  mutate(excess=deaths-fit)%>% 
  select(SALID1, year, epi_week, excess, deaths, fit, pop) %>% 
  filter(epi_week==26&year==2021) %>% 
  left_join(region_cw %>% filter(!duplicated(SALID1)) %>% 
              select(SALID1, macrozona, city_link)) %>% 
  mutate(rel=excess/fit*100,
         abs=excess/pop*100000) %>% 
  select(city_link, macrozona, SALID1, rel_emp=rel, abs_emp=abs) %>%
  full_join(excess_2020_l1 %>% mutate(rel_gam=relexcess*100,
                                      abs_gam=absexcess*100000) %>% 
              select(SALID1, rel_gam, abs_gam)) %>% 
  left_join(exposuresl1 %>% select(SALID1, total_pop=BECTPOP2020L1AD)) %>% 
  ungroup()

cor_rel<-cor(excess_cumulative_l1_empirical$rel_emp, excess_cumulative_l1_empirical$rel_gam, method="spearman")
cor_abs<-cor(excess_cumulative_l1_empirical$abs_emp, excess_cumulative_l1_empirical$abs_gam, method="spearman")

comp_rel<-ggplot(excess_cumulative_l1_empirical, aes(x=rel_gam, y=rel_emp)) +
  geom_abline(intercept=0, slope=1, lty=2)+
  geom_vline(xintercept = 0, lty=2)+
  geom_hline(yintercept = 0, lty=2)+
  #geom_linerange(aes(ymin=lci_bs, ymax=uci_bs, color=type))+
  #geom_linerange(aes(xmin=lci_orig, xmax=uci_orig, color=type))+
  geom_point(aes(fill=macrozona, size=log10(total_pop)), pch=21, color="black")+
  scale_y_continuous(limits=c(excess_cumulative_l1_empirical %>% select(contains("rel")) %>% min, excess_cumulative_l1_windows %>% select(contains("rel")) %>% max))+
  scale_x_continuous(limits=c(excess_cumulative_l1_empirical %>% select(contains("rel")) %>% min, excess_cumulative_l1_windows %>% select(contains("rel")) %>% max))+
  scale_color_brewer(name="Region",type="qual", palette=2)+
  scale_fill_brewer(name="Region",type="qual", palette=2)+
  guides(size="none")+
  labs(x="Relative Excess Mortality (%) using GAM model",
       y="Relative Excess Mortality (%) using 2016-2019 averages",
       title=paste0("Correlation=", round(cor_rel, digits=3)),
       tag="A")+
  theme_k
comp_abs<-ggplot(excess_cumulative_l1_empirical, aes(x=abs_gam, y=abs_emp)) +
  geom_abline(intercept=0, slope=1, lty=2)+
  geom_vline(xintercept = 0, lty=2)+
  geom_hline(yintercept = 0, lty=2)+
  #geom_linerange(aes(ymin=lci_bs, ymax=uci_bs, color=type))+
  #geom_linerange(aes(xmin=lci_orig, xmax=uci_orig, color=type))+
  geom_point(aes(fill=macrozona, size=log10(total_pop)), pch=21, color="black")+
  scale_y_continuous(limits=c(excess_cumulative_l1_empirical %>% select(contains("abs")) %>% min, excess_cumulative_l1_windows %>% select(contains("abs")) %>% max))+
  scale_x_continuous(limits=c(excess_cumulative_l1_empirical %>% select(contains("abs")) %>% min, excess_cumulative_l1_windows %>% select(contains("abs")) %>% max))+
  scale_color_brewer(name="Region",type="qual", palette=2)+
  scale_fill_brewer(name="Region",type="qual", palette=2)+
  guides(size="none")+
  labs(x="Excess Mortality Rate per 100,000 using GAM model",
       y="Excess Mortality Rate per 100,000 using 2016-2019 averages",
       title=paste0("Correlation=", round(cor_abs, digits=3)),
       tag="B")+
  theme_k
pall<-arrangeGrob(grobs=list(comp_rel, comp_abs), ncol=2)
ggsave("results/Appendix_Fig11.pdf", pall, width=18, height=7.5)



