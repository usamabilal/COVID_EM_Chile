rm(list=ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(sas7bdat)
select<-dplyr::select
## Open the data
deaths_raw<-fread("data/DEFUNCIONES_FUENTE_DEIS_2016_2021_02092021/DEFUNCIONES_FUENTE_DEIS_2016_2021_02092021.csv", 
      header = FALSE, encoding = "Latin-1") %>% 
  ##Rename the cols
  rename(ANO_DEF=V1, FECHA_DEF=V2, GLOSA_SEXO=V3,EDAD_TIPO=V4,
         EDAD_CANT=V5, CODIGO_COMUNA_RESIDENCIA=V6,
         GLOSA_COMUNA_RESIDENCIA=V7, GLOSA_REG_RES=V8,
         DIAG1=V9, CAPITULO_DIAG1=V10,GLOSA_CAPITULO_DIAG1=V11,
         CODIGO_GRUPO_DIAG1=V12,GLOSA_GRUPO_DIAG1=V13,
         CODIGO_CATEGORIA_DIAG1=V14,GLOSA_CATEGORIA_DIAG1=V15,
         CODIGO_SUBCATEGORIA_DIAG1=V16, GLOSA_SUBCATEGORIA_DIAG1=V17,
         DIAG2=V18, CAPITULO_DIAG2=V19, GLOSA_CAPITULO_DIAG2=V20,
         CODIGO_GRUPO_DIAG2=V21,GLOSA_GRUPO_DIAG2=V22,
         CODIGO_CATEGORIA_DIAG2=V23, GLOSA_CATEGORIA_DIAG2=V24,
         CODIGO_SUBCATEGORIA_DIAG2=V25,GLOSA_SUBCATEGORIA_DIAG2=V26) %>%
  ##Change the variables format
  mutate(FECHA_DEF=as_date(FECHA_DEF),
         epi_week=epiweek(FECHA_DEF),
         year=epiyear(FECHA_DEF),
         EDAD_TIPO1=case_when(EDAD_TIPO==1 ~ "Years",
                              EDAD_TIPO==2 ~ "Months", 
                              EDAD_TIPO==3 ~ "Days",
                              EDAD_TIPO==4 ~ "Hours",
                              EDAD_TIPO==0 ~ NA_character_),
         EDAD_CANT=ifelse(is.na(EDAD_TIPO1), NA, EDAD_CANT),
         Age=ifelse(EDAD_TIPO>1 & EDAD_TIPO<=4 & !is.na(EDAD_TIPO1), 0, EDAD_CANT),
         GLOSA_SEXO=case_when(GLOSA_SEXO=="Hombre" ~ "Male",
                              GLOSA_SEXO=="Mujer" ~ "Female",
                              GLOSA_SEXO=="Indeterminado" ~ NA_character_),
         edad_cat=case_when(Age<5 ~  "<5",
                            Age>=5 & Age<20 ~ "5-19",
                            Age>=20 & Age<40 ~ "20-39",
                            Age>=40 & Age<60 ~ "40-59",
                            Age>=60 & Age<75 ~ "60-74",
                            Age>=75 ~ "75+"),
         edad_cat2=case_when(Age>=0 & Age<30 ~ "0-29",
                             Age>=30 & Age<60 ~ "30-60",
                             Age>=60 ~ "60+"),
         sex=GLOSA_SEXO,
         comuna=CODIGO_COMUNA_RESIDENCIA,
         covid=case_when(
           DIAG1%in%c("U071")~ "confirmed",
           DIAG1%in%c("U072")~ "suspected",
           T ~ "no"),
         covid_confirmed=ifelse(DIAG1%in%c("U071"), 1, 0),
         covid_confirmed_probable=ifelse(DIAG1%in%c("U071", "U072"), 1, 0)) %>% 
  # 2016 onwards up to week 26 of 2021
  filter(year>=2016, FECHA_DEF<=ymd("2021-07-03"))
deaths<-deaths_raw %>% 
  group_by(year, epi_week, sex, edad_cat, comuna) %>% 
  summarise(deaths=n(),
            covid_confirmed=sum(covid_confirmed),
            covid_confirmed_probable=sum(covid_confirmed_probable)) %>% 
  filter(year>=2016, (year<=2021&epi_week<=26)|year<=2020) %>% 
  # removing 111+67 deaths over the 5 years
  filter(!is.na(sex), !is.na(edad_cat), comuna!=99999)

template<-expand.grid(year=unique(deaths$year),
                      epi_week=1:52, # add 53 for 2020 later on
                      edad_cat=unique(deaths$edad_cat),
                      sex=unique(deaths$sex),
                      comuna=unique(deaths$comuna))
template<-bind_rows(template, template %>% filter(year==2020, epi_week==1) %>% 
                      mutate(epi_week=53))
deaths<-full_join(deaths, template) %>% 
  mutate(deaths=replace_na(deaths, 0),
         covid_confirmed=replace_na(covid_confirmed, 0),
         covid_confirmed_probable=replace_na(covid_confirmed_probable, 0)) %>%
  filter(year>=2016, (year<=2021&epi_week<=26)|year<=2020)

# get epi weeks in 2020 as reference
epi_weeks1<-deaths_raw %>% 
  filter(year(FECHA_DEF)==2020) %>% 
  arrange(epi_week, desc(FECHA_DEF)) %>% 
  filter(!duplicated(epi_week)) %>% 
  select(epi_week, date1=FECHA_DEF) %>% 
  mutate(year=2020)
epi_weeks2<-deaths_raw %>% 
  filter(year(FECHA_DEF)==2020) %>% 
  arrange(epi_week, (FECHA_DEF)) %>% 
  filter(!duplicated(epi_week)) %>% 
  select(epi_week, date2=FECHA_DEF) %>% 
  mutate(year=2020)
epi_weeks3<-deaths_raw %>% 
  filter(year(FECHA_DEF)==2021) %>% 
  arrange(epi_week, desc(FECHA_DEF)) %>% 
  filter(!duplicated(epi_week)) %>% 
  select(epi_week, date1=FECHA_DEF) %>% 
  mutate(year=2021)
epi_weeks4<-deaths_raw %>% 
  filter(year(FECHA_DEF)==2021) %>% 
  arrange(epi_week, (FECHA_DEF)) %>% 
  filter(!duplicated(epi_week)) %>% 
  select(epi_week, date2=FECHA_DEF) %>% 
  mutate(year=2021)
epi_weeks2020<-full_join(epi_weeks1, epi_weeks2) 
epi_weeks2021<-full_join(epi_weeks3, epi_weeks4)
epi_weeks2020<-epi_weeks2020 %>% 
  filter(epi_week!=53) %>% 
  bind_rows(full_join(epi_weeks2020 %>% filter(epi_week==53) %>% select(epi_week, year, date2),
                      epi_weeks2021 %>% filter(epi_week==53) %>% select(epi_week, year, date1) %>% mutate(year=2020)))

epi_weeks<-bind_rows(epi_weeks2020, epi_weeks2021 %>% filter(epi_week!=53)) %>% 
  rowwise() %>% 
  mutate(date=mean(c(date1, date2))) %>% 
  select(epi_week, year, date, date1, date2)
epi_weeks %>% print(n=100)


cw<-read.sas7bdat("data/salurbal/clcom_salurbal_11_4_19.sas7bdat") %>% 
  select(SALID1, SALID2, comuna=L2LOCALID18) %>% 
  filter(!is.na(SALID2))
load("data/salurbal/l1s.RData")
cw<-left_join(cw, l1s %>% select(SALID1, city_link)) %>% 
  mutate(comuna=as.numeric(comuna))
deaths_raw<-full_join(deaths_raw, cw) %>% 
  mutate(salurbal=!is.na(SALID2)) %>% 
  ungroup()
deaths<-full_join(deaths, cw) %>% 
  mutate(salurbal=!is.na(SALID2)) %>% 
  ungroup()

popl2<-fread("data/ine_estimaciones-y-proyecciones-2002-2035_base-2017_comunas0381d25bc2224f51b9770a705a434b74.csv") %>% 
  select(comuna=Comuna, sex=`Sexo (1=Hombre 2=Mujer)`, Age=Edad,`Poblacion 2016`:`Poblacion 2021`) %>% 
  gather(year, pop, -comuna, -sex, -Age) %>% 
  mutate(year=as.numeric(sub("Poblacion ", "", year)),
         pop=as.numeric(pop),
         sex=ifelse(sex==1, "Male", "Female"),
         edad_cat=case_when(Age<5 ~  "<5",
                            Age>=5 & Age<20 ~ "5-19",
                            Age>=20 & Age<40 ~ "20-39",
                            Age>=40 & Age<60 ~ "40-59",
                            Age>=60 & Age<75 ~ "60-74",
                            Age>=75 ~ "75+")) %>% 
  group_by(comuna, year, sex, edad_cat, pop) %>% 
  summarise(pop=sum(pop)) %>% 
  full_join(cw %>% select(comuna, SALID2))
popl1<-popl2 %>% full_join(cw %>% select(SALID2, SALID1)) %>% 
  group_by(year, SALID1, sex, edad_cat) %>% 
  summarise(pop=sum(pop))

sec<-fread("data/salurbal/SEC_INDEXSCORES_L2_02112021.csv") %>% 
  filter(YEAR==2017, ISO2=="CL") %>% 
  select(SALID2, educ2=CNSSE3_L2)
sec2<-fread("data/salurbal/SEC_Census_L2_06232021.csv") %>% 
  filter(YEAR==2017, ISO2=="CL") %>% 
  select(SALID2, crowd=CNSCROWD25BRL2, educ=CNSMINUN_L2)
sec<-full_join(sec, sec2)
secl1<-fread("data/salurbal/SEC_INDEXSCORES_L1AD_02112021.csv") %>% 
  filter(YEAR==2017, ISO2=="CL") %>% 
  select(SALID1, educ2=CNSSE3_L1AD)
secl12<-fread("data/salurbal/SEC_Census_L1AD_06232021.csv") %>% 
  filter(YEAR==2017, ISO2=="CL") %>% 
  select(SALID1, crowd=CNSCROWD25BRL1AD, educ=CNSMINUN_L1AD)
secl1<-full_join(secl1, secl12)

bec<-fread("data/salurbal/BEC_L2_20210104.csv") %>% 
  filter(ISO2=="CL") %>% 
  select(SALID1, SALID2, popdens=BECPOPDENS2020L2, BECTPOP2020L2)
ap<-fread("data/salurbal/APSL2_09022021.csv") %>% 
  filter(ISO2=="CL", YEAR==2018) %>% 
  select(SALID2, ap=APSPM25POPWTL2)
becl1<-fread("data/salurbal/BEC_L1AD_20210104.csv") %>% 
  filter(ISO2=="CL") %>% 
  select(SALID1, popdens=BECPOPDENS2020L1AD, BECTPOP2020L1AD)
apl1<-fread("data/salurbal/APSL1AD_09022021.csv") %>% 
  filter(ISO2=="CL", YEAR==2018) %>% 
  select(SALID1, ap=APSPM25POPWTL1AD)
# create % 60+ in 2020
pop60l1<-full_join(popl1 %>% 
            filter(edad_cat%in%c("60-74", "75+"), year==2020) %>% 
            group_by(SALID1) %>% 
            summarise(pop60p=sum(pop)),
          popl1 %>% 
            filter(year==2020) %>%
            group_by(SALID1) %>% 
            summarise(total_pop=sum(pop))) %>% 
  mutate(prop_60p=pop60p/total_pop) %>% 
  select(SALID1, prop_60p)
pop60l2<-full_join(popl2 %>% 
            filter(edad_cat%in%c("60-74", "75+"), year==2020) %>% 
            group_by(SALID2) %>% 
            summarise(pop60p=sum(pop)),
          popl2 %>% 
            filter(year==2020) %>%
            group_by(SALID2) %>% 
            summarise(total_pop=sum(pop))) %>% 
  mutate(prop_60p=pop60p/total_pop) %>% 
  select(SALID2, prop_60p)


exposures_l2<-full_join(bec, ap) %>% 
  full_join(sec) %>% left_join(pop60l2)
exposures_l1<-full_join(becl1, apl1) %>% 
  full_join(secl1) %>% left_join(pop60l1)
exposuresl1<-exposures_l1 %>% 
  mutate(city_size=BECTPOP2020L1AD,
         popdens_tert=as.numeric(cut(popdens, 
                                     breaks=quantile(popdens, probs=seq(0, 1, by=1/3)), include.lowest=T)),
         ap_tert=as.numeric(cut(ap, 
                                breaks=quantile(ap, probs=seq(0, 1, by=1/3)), include.lowest=T)),
         crowd_tert=as.numeric(cut(crowd, 
                                   breaks=quantile(crowd, probs=seq(0, 1, by=1/3)), include.lowest=T)),
         educ_tert=as.numeric(cut(educ, 
                                  breaks=quantile(educ, probs=seq(0, 1, by=1/3)), include.lowest=T))) %>% 
  mutate(popdens_tert=factor(popdens_tert, levels=1:3, labels=c("1st", "2nd", "3rd")),
         crowd_tert=factor(crowd_tert, levels=1:3, labels=c("1st", "2nd", "3rd")),
         ap_tert=factor(ap_tert, levels=1:3, labels=c("1st", "2nd", "3rd")),
         educ_tert=factor(educ_tert, levels=1:3, labels=c("1st", "2nd", "3rd")))
exposuresl2<-exposures_l2 %>% 
  rename(comuna_size=BECTPOP2020L2) %>% 
  mutate(santiago=case_when(
    SALID1==103110 ~ "santiago",
    SALID1==103114 ~ "concepcion",
    SALID1==103107 ~ "valparaiso",
    T ~ "other")) %>% 
  group_by(santiago) %>% 
  mutate(popdens_tert=as.numeric(cut(popdens, 
                                     breaks=quantile(popdens, probs=seq(0, 1, by=1/3)), include.lowest=T)),
         ap_tert=as.numeric(cut(ap, 
                                     breaks=quantile(ap, probs=seq(0, 1, by=1/3)), include.lowest=T)),
         crowd_tert=as.numeric(cut(crowd, 
                                breaks=quantile(crowd, probs=seq(0, 1, by=1/3)), include.lowest=T)),
         educ_tert=as.numeric(cut(educ, 
                                breaks=quantile(educ, probs=seq(0, 1, by=1/3)), include.lowest=T))) %>% 
  mutate(popdens_tert=factor(popdens_tert, levels=1:3, labels=c("1st", "2nd", "3rd")),
         crowd_tert=factor(crowd_tert, levels=1:3, labels=c("1st", "2nd", "3rd")),
         ap_tert=factor(ap_tert, levels=1:3, labels=c("1st", "2nd", "3rd")),
         educ_tert=factor(educ_tert, levels=1:3, labels=c("1st", "2nd", "3rd")))



# regions
region_cw<-deaths_raw %>% filter(!is.na(SALID2), !duplicated(SALID2)) %>% 
  mutate(macrozona=case_when(
    grepl("Arica|Tarapa|Antofag|Atacama|Coquimbo", GLOSA_REG_RES) ~ "North",
    grepl("Valpar|Higgin|Maule", GLOSA_REG_RES) ~ "Center",
    grepl("uble|(Del B)|Arauca|(Los R)|Lagos", GLOSA_REG_RES) ~ "South",
    grepl("Ais|Magallanes", GLOSA_REG_RES) ~ "South", # should be Extremo Sur, but it's just one
    grepl("Metropolitana", GLOSA_REG_RES) ~ "Santiago (Metropolitan)",
    T ~ "ERROR"
  )) %>% 
  mutate(macrozona=factor(macrozona, levels=c("North", "Center", "Santiago (Metropolitan)", "South"))) %>% 
  select(comuna, SALID2, macrozona) %>% 
  left_join(cw) %>% 
  ## add names of comunas
  left_join(deaths_raw %>% filter(!duplicated(comuna)) %>% 
              select(comuna, comuna_name=GLOSA_COMUNA_RESIDENCIA) %>% 
              filter(!is.na(comuna)))

# save everything
save(deaths_raw, deaths, popl1, popl2, exposuresl1, exposuresl2, epi_weeks,region_cw, file="clean_rdata.rdata")
