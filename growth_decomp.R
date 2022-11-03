# path
rm(list=ls()); currentdir_path=dirname(rstudioapi::getSourceEditorContext()$path); setwd(currentdir_path)
getwd()
# packages
packs =c("tidyverse","wesanderson","RcppRoll","scales","lubridate","wpp2019")
missing_packs = setdiff(packs, as.data.frame(installed.packages()[,c(1,3:4)])$Package)
if (length(missing_packs)>0){ lapply(missing_packs,install.packages,character.only=TRUE) }
lapply(packs,library,character.only=TRUE)
standard_theme=theme(plot.title=element_text(hjust=0.5,size=16), 
                     axis.text.x=element_text(size=13,angle=90,vjust=1/2),axis.text.y=element_text(size=13),
                     axis.title.x=element_text(size=15),axis.title.y=element_text(size=15),
                     legend.title=element_text(size=16),legend.text=element_text(size=12)) # text=element_text(family="Calibri")

### ### ### ### ### ### ### ### ### ### ### ### 
# decomposing GDP growth = employment + productivity + workhours

# population dynamics
cntr_list=c("United Kingdom|Germany|United States|Japan")
pop_owid = read_csv("population-past-future.csv")
pop_growth <- pop_owid %>% 
  filter(grepl(cntr_list,Entity) & Year>1950 & Year<2020 ) %>% mutate(country=Entity) %>% #
  group_by(country) %>% mutate(pop_growth=Population-lag(Population),
                               pop_growth_pct=100*(Population-lag(Population))/lag(Population),
                               pop_growth_pct_smooth=roll_mean(pop_growth_pct,n=3,align="center",fill=NA))

### ### ### ### ### ### ### ### ### ### 
# GDP per capita

gdp_per_cap <- read_csv("API_NY.GDP.PCAP.KD.ZG_DS2_en_csv_v2_4498999/API_NY.GDP.PCAP.KD.ZG_DS2_en_csv_v2_4498999.csv") %>%
  select(!c(`Indicator Name`,`Indicator Code`,`...67`)) %>% pivot_longer(!c(`Country Name`,`Country Code`)) %>%
  rename(country=`Country Name`,year=name) %>% mutate(year=as.numeric(year)) %>%
  filter(grepl(cntr_list,country) & !is.na(value)) %>% rename(gdp_per_cap_growth_pct=value) %>%
  mutate(gdp_per_cap_growth_pct_smooth=roll_mean(gdp_per_cap_growth_pct,n=3,align="center",fill=NA))

### ### ### ### ### ### ### ### ### ### 
# workforce

cntr_codes=c("DEU|GBR|USA|JPN")
employment_total <- read_csv("oecd_employment_DP_LIVE_25092022190117013.csv") %>% # EMP_TEMP_SEX_AGE_NB_A-filtered-2022-09-24.csv
  # select(c(ref_area.label,time,obs_value,sex.label)) %>% filter(time<2020) %>% mutate(obs_value=obs_value/1e3) %>%
  # filter(sex.label %in% "Sex: Total") %>% rename(country=ref_area.label,year=time,nr_employed_thous=obs_value) %>%
  filter(grepl(cntr_codes,LOCATION) & TIME>1950 & TIME<2020) %>% select(!c(`Flag Codes`,FREQUENCY,INDICATOR,SUBJECT)) %>%
  rename(country=LOCATION,nr_employed_thous=Value,year=TIME) %>% 
  mutate(country=ifelse(grepl("DEU",country),"Germany",ifelse(grepl("GBR",country),"United Kingdom",
                                                      ifelse(grepl("JPN",country),"Japan","United States")))) %>%
  group_by(country) %>% mutate(employment_growth=nr_employed_thous-lag(nr_employed_thous),
                               employment_growth_pct=100*(nr_employed_thous-lag(nr_employed_thous))/lag(nr_employed_thous),
                               # 
                               # employment_growth=ifelse(grepl("Ger",country)&year %in% c(1990,1991),NA,employment_growth),
                               # employment_growth_pct=ifelse(grepl("Ger",country)&year %in% c(1990,1991),NA,employment_growth_pct),
                               employment_growth_pct_smooth=roll_mean(employment_growth_pct,n=3,align="center",fill=NA))
# 
# plot
employment_total %>% group_by(country) %>% # mutate() %>%
  select(c(year,country,nr_employed_thous,employment_growth_pct_smooth)) %>% mutate(nr_employed_thous=nr_employed_thous/1e3) %>%
  rename(`employment (million)`=nr_employed_thous,`growth of employment (%, smoothed 3yr)`=employment_growth_pct_smooth) %>%
  pivot_longer(!c(year,country)) %>%
ggplot()  +
  geom_line(aes(x=year,y=value,color=country),size=1) + facet_wrap(~name,scales="free_y") + # ,nrow=2
  scale_x_continuous(breaks=seq(1950,2020,5),expand=expansion(0,0)) + # scale_y_continuous(breaks=(0:20)*10) +
  geom_hline(aes(yintercept=ifelse(grepl("growth",name),0,NA)),linetype="dashed",size=1/2) +
  theme_bw() + standard_theme + xlab("") + theme(legend.position="top",legend.title=element_blank()) + ylab("")

### ### ### ### ### ### ### ### ### ### 
# annual work hours/worker
annual_working_hrs <- read_csv("oecd_annual_working_hours_DP_LIVE_25092022173345711.csv") %>% # "annual-working-hours-per-worker.csv"
  # filter(grepl(cntr_list,Entity) & Year>1950 & Year<2020 )
  # filter(grepl(cntr_list,Entity) & Year>1950 & Year<2020 ) %>% 
  # mutate(country=Entity,annual_workhours=`Average annual working hours per worker`) %>%
  filter(grepl(cntr_codes,LOCATION) & TIME>1950 & TIME<2020 ) %>% select(!c(`Flag Codes`,FREQUENCY,INDICATOR,SUBJECT)) %>%
  rename(country=LOCATION,annual_workhours=Value,year=TIME) %>% 
  mutate(country=ifelse(grepl("DEU",country),"Germany",ifelse(grepl("GBR",country),"United Kingdom",
                                                              ifelse(grepl("JPN",country),"Japan","United States")))) %>%
  group_by(country) %>% mutate(
    # 1992 and 1993 look wrong for US, remove
    annual_workhours=ifelse(country %in% "United States" & year %in% c(1992,1993),NA,annual_workhours),
    workhours_change=annual_workhours-lag(annual_workhours),
                               workhours_change_pct=100*(annual_workhours-lag(annual_workhours))/lag(annual_workhours),
  workhours_change_pct_smooth=roll_mean(workhours_change_pct,n=3,align="center",fill=NA))

# plot
annual_working_hrs %>% select(c(year,country,annual_workhours,workhours_change_pct_smooth)) %>% # workhours_change,
  pivot_longer(!c(year,country)) %>%
ggplot()  +
  geom_line(aes(x=year,y=value,color=country)) + facet_wrap(~name,scales="free_y") + # ,nrow=2
  scale_x_continuous(breaks=seq(1950,2020,5),expand=expansion(0,0)) + 
  geom_hline(aes(yintercept=ifelse(grepl("annual",name),NA,0)),linetype="dashed",size=1/2) +
  theme_bw() + standard_theme + xlab("") + theme(legend.position="top",legend.title=element_blank()) + ylab("")
# 
# ggsave("annaul_workinghrs_owid.png",width=20,height=30,units="cm")

### ### ### ### ### ### ### ### ### ### 
# productivity (output hour worked)
labor_productivity_per_hour <- read_csv("oecd_lab_prod_usd_DP_LIVE_25092022154830342.csv") %>%
  # read_csv("labor-productivity-per-hour-PennWorldTable.csv") %>%
  filter(grepl(cntr_codes,LOCATION) & TIME>1950 & TIME<2020 ) %>% select(!c(`Flag Codes`,FREQUENCY,INDICATOR,SUBJECT)) %>%
  rename(country=LOCATION,lab_prod=Value,year=TIME) %>% 
  mutate(country=ifelse(grepl("DEU",country),"Germany",ifelse(grepl("GBR",country),"United Kingdom",
                                                              ifelse(grepl("JPN",country),"Japan","United States")))) %>%
  group_by(country) %>% mutate(lab_prod_growth=lab_prod-lag(lab_prod),
                               lab_prod_pct_growth=100*(lab_prod-lag(lab_prod))/lag(lab_prod),
                               lab_prod_pct_growth_smooth=roll_mean(lab_prod_pct_growth,n=3,align="center",fill=NA))
  
# plot productivity growth
labor_productivity_per_hour %>% group_by(country) %>% # arrange(Year) %>%
  select(c(year,country,lab_prod,lab_prod_pct_growth_smooth)) %>% 
  rename(`labour productivity (USD)`=lab_prod,`labour productivity growth % (smoothed)`=lab_prod_pct_growth_smooth) %>% 
  pivot_longer(!c(year,country)) %>%
ggplot()  +
  geom_line(aes(x=year,y=value,color=country),size=1) + facet_wrap(~name,scales="free_y") + # ,nrow=2
  # geom_line(aes(x=Year,y=lab_prod_pct_growth,color=country),alpha=1/2,size=1/2,linetype="dashed") +
  scale_x_continuous(breaks=seq(1950,2020,5),expand=expansion(0,0)) + 
  geom_hline(aes(yintercept=ifelse(grepl("USD",name),NA,0)),linetype="dashed",size=1/2) +
  theme_bw() + standard_theme + xlab("") + theme(legend.position="top",legend.title=element_blank()) + ylab("")

### ### ### ### ### ### ### ### ### ### 
# GDP growth

# GDP growth
gdp_growth <- read_csv("API_NY.GDP.MKTP.KD.ZG_DS2_en_csv_v2_4558542/API_NY.GDP.MKTP.KD.ZG_DS2_en_csv_v2_4558542.csv") %>%
  select(!c(`Indicator Name`,`Indicator Code`,`...67`)) %>% pivot_longer(!c(`Country Name`,`Country Code`)) %>%
  rename(country=`Country Name`,year=name) %>% mutate(year=as.numeric(year)) %>%
  filter(grepl(cntr_list,country) & !is.na(value)) %>% rename(gdp_growth_pct=value) %>%
  mutate(gdp_growth_pct_smooth=roll_mean(gdp_growth_pct,n=3,align="center",fill=NA))
  
# plot
ggplot(gdp_growth %>% filter(year<2020),aes(x=year,y=gdp_growth_pct,group=country,color=country)) + geom_line(size=1) + 
  geom_point() +
  scale_x_continuous(breaks=seq(1950,2020,5),expand=expansion(0,0)) + geom_hline(yintercept=0,size=1/2,linetype="dashed") +
  theme_bw() + standard_theme + xlab("") + ylab("% GDP growth")

# combine all
# GDP growth ≈ workforce growth + annual work hours growth + productivity growth 
summ_table_gdp_decomp = left_join(
  left_join(
    left_join(
      gdp_growth %>% select(country,year,gdp_growth_pct,gdp_growth_pct_smooth) %>% pivot_longer(!c(country,year)) %>%
        mutate(smoothing=ifelse(grepl("smooth",name),"smoothed","point")) %>% rename(gdp_growth_pct=value) %>% select(!name),
      employment_total %>% select(c(country,year,employment_growth_pct,employment_growth_pct_smooth)) %>% 
        pivot_longer(!c(country,year)) %>% mutate(smoothing=ifelse(grepl("smooth",name),"smoothed","point")) %>% 
        rename(employment_growth_pct=value) %>% select(!name) ), 
      labor_productivity_per_hour %>% select(c(country,year,lab_prod_pct_growth,lab_prod_pct_growth_smooth)) %>% 
        pivot_longer(!c(country,year)) %>% mutate(smoothing=ifelse(grepl("smooth",name),"smoothed","point")) %>% 
        rename(lab_prod_pct_growth=value) %>% select(!name) ),
      annual_working_hrs %>% select(country,year,workhours_change_pct,workhours_change_pct_smooth) %>% 
        pivot_longer(!c(country,year)) %>% mutate(smoothing=ifelse(grepl("smooth",name),"smoothed","point")) %>% 
        rename(workhours_change_pct=value) %>% select(!name) ) %>% relocate(smoothing,.after=last_col()) %>%
  mutate(comp_sums=workhours_change_pct+lab_prod_pct_growth+employment_growth_pct)
#  comp_sum_data_pct_dev=abs(comp_sums-gdp_growth_pct)/gdp_growth_pct)

# plot
summ_table_gdp_decomp %>% filter(!grepl("smooth",smoothing) & year>1975 & year<2020) %>%
  pivot_longer(!c(country,year,smoothing))  %>%
ggplot() + geom_line(aes(x=year,y=value,color=name,size=name %in% "comp_sums")) + 
  facet_wrap(~country,nrow=2,scales="free_y") + 
  scale_size_manual(values=c(3/4,2),guide='none') + scale_color_manual(values=c("darkgrey","blue","black","red","#006600")) + 
  scale_x_continuous(expand=expansion(0.01,0),breaks=seq(1985,2020,5)) +
  xlab("") + ylab("% change") + theme_bw() + standard_theme + 
  theme(legend.title=element_blank(),legend.position="top",strip.text=element_text(size=12),
        axis.text.x=element_text(size=11),axis.text.y=element_text(size=11))
# save plot growth componets
# ggsave("plots/gdp_growth_comps_decompos.png",width=30,height=20,units="cm")

# compare decomp sum vs data
summ_table_gdp_decomp %>% filter(!grepl("smooth",smoothing) & year>1985 & year<2020) %>% 
  select(!c(workhours_change_pct,lab_prod_pct_growth,employment_growth_pct)) %>% 
  rename(`GDP growth % (linear decomp.)`=comp_sums,`GDP growth % (data)`=gdp_growth_pct) %>%
  pivot_longer(!c(country,year,smoothing)) %>% 
ggplot(aes(x=year,y=value,color=name,size=grepl("decomp",name))) + 
  geom_line(size=1.1) + geom_point(size=2,alpha=1/2) + facet_wrap(~country,nrow=2) +  # ,show.legend=F 
  geom_hline(yintercept=0,size=1/2,linetype="dashed") +
  scale_size_manual(values=c(3/4,1/2),guide='none') + 
  xlab("") + ylab("% change") + scale_x_continuous(breaks=seq(1990,2020,5),expand=expansion(0,0.5)) + 
  theme_bw() + standard_theme + theme(legend.position="top",legend.title=element_blank(),strip.text=element_text(size=13)) # 
#
ggsave("plots/gdp_growth_comps_employ_prod_decompos_comparison.png",width=30,height=20,units="cm")

# scatterplot
summ_table_gdp_decomp %>% filter(year>1990 & year<2020) %>%  # !grepl("smooth",smoothing) & 
  select(!c(workhours_change_pct,lab_prod_pct_growth,employment_growth_pct)) %>% 
ggplot(aes(x=gdp_growth_pct,y=comp_sums,color=country)) + geom_point(alpha=1/2) + geom_smooth(method=lm,se=F,alpha=1/2) +
  facet_wrap(~smoothing,scales="free") + # scale_x_continuous(limits = c(-7.5,10)) +
  theme_bw() + standard_theme + theme(legend.position="top",legend.title=element_blank()) 
#
ggsave("plots/gdp_growth_comps_decompos_scatter.png",width=30,height=20,units="cm")

# calc correlations
summ_table_gdp_decomp %>% filter(year>1990 & year<2020) %>%  # !grepl("smooth",smoothing) & 
  select(!c(workhours_change_pct,lab_prod_pct_growth,employment_growth_pct)) %>%  filter(smoothing %in% "point") %>%
  group_by(country,smoothing) %>% summarise(cor(comp_sums,gdp_growth_pct,use = "complete.obs"))

# deviations
# summ_table_gdp_decomp %>% filter(year>1990 & year<2020) %>% mutate(comp_sum_data_pct_dev=gdp_growth_pct- comp_sums) %>%
#   select(!c(workhours_change_pct,lab_prod_pct_growth,employment_growth_pct,gdp_growth_pct,comp_sums)) %>% 
# ggplot(aes(x=year,y=abs(comp_sum_data_pct_dev),color=country)) + geom_point() + 
#   facet_wrap(~smoothing) + ylab("deviation") + # scale_x_continuous(limits = c(-7.5,10)) +
#   theme_bw() + standard_theme + theme(legend.position="top",legend.title=element_blank()) # 

# deviation distribution (in %, absolute values)
# summ_table_gdp_decomp %>% filter(year>1990 & year<2020) %>%  # mutate(comp_sum_data_pct_dev=gdp_growth_pct-comp_sums) %>%
#   select(!c(workhours_change_pct,lab_prod_pct_growth,employment_growth_pct)) %>%
#   group_by(country,smoothing) %>% summarise(median_abs_deviat_pct=median(abs(gdp_growth_pct-comp_sums),na.rm=T),
#                                             median_gdp_growth=median(gdp_growth_pct,na.rm=T),
#                                             median_gdp_growth_linear_approx=median(comp_sums,na.rm=T))

# of GDP growth explained by lab_product, workforce and annual_workhour growth
# not counting recession years
periods <- list("1990-2000"=c(1990,2000),"2001-2008"=c(2001,2008),"2009-2019"=c(2009,2019)); eval_periods<-list()
for (k_per in 1:length(periods)) {

  eval_periods[[k_per]] <- summ_table_gdp_decomp %>% 
    filter(gdp_growth_pct>0 & year>=periods[[k_per]][1] & year<=periods[[k_per]][2] & 
                                   !grepl("smooth",smoothing)) %>% drop_na() %>% 
  mutate(residual=gdp_growth_pct-comp_sums) %>% pivot_longer(!c(country,year,smoothing)) %>%
  group_by(country,smoothing,name) %>% 
  summarise(mean=mean(value), median=median(value)) %>% # *ifelse(metric_type %in% "share",100,1)
  group_by(country,smoothing) %>% 
  mutate(mean_share_gdp=100*mean/mean[name %in% "gdp_growth_pct"],
         median_share_gdp=100*median/median[name %in% "gdp_growth_pct"]) %>% ungroup() %>% 
  mutate(period=names(periods)[k_per],
         name=case_when(grepl("employment_growth",name) ~ "employment",
                          grepl("lab_prod",name) ~ "labour productivity",
                          grepl("workhours",name) ~ "annual work hours",
                          grepl("residual",name) ~ "residual",
                          grepl("gdp_growth_pct",name) ~ "total GDP",
                          grepl("comp_sums",name) ~ "linear comp."),
           country=ifelse(grepl("Kingdom",country),"UK",ifelse(grepl("States",country),"US",country))) %>%
    pivot_longer(!c(country,smoothing,name,period),names_to = "metric_type")
  # 
  if (k_per==length(periods)) {df_eval_periods=bind_rows(eval_periods)}
}

# bar plot for periods
# gdp=darkgrey, residual=grey, popul/employ=#006600, gdppp/labprod=red
k_sel_metr=1; sel_metric=c("mean_share_gdp","mean")[k_sel_metr]
df_eval_periods %>% 
  filter(smoothing %in% "point" & !grepl(c("comp|GDP","comp")[k_sel_metr],name) & metric_type %in% sel_metric)  %>%
ggplot(aes(y=country,x=value,fill=name)) + geom_bar(stat="identity",position=position_dodge2(padding=0.15),size=1/4,color="black") + 
  facet_wrap(~period,scales = "free_x") + labs(fill="") + scale_y_discrete(expand=expansion(0.175,0)) +
  geom_hline(yintercept=(1:3)+1/2,size=1/2,linetype="dashed") + geom_vline(xintercept=0) +
  xlab(paste0("% contribution to GDP growth",ifelse(grepl("abs",sel_metric)," (absolute)"," (relative)"))) + ylab("") +
  scale_fill_manual(values=list(c("blue","#006600","red","grey"),c("blue","#006600","red","grey","grey30"))[[k_sel_metr]]) + 
  theme_bw() + standard_theme + theme(legend.position="top",strip.text=element_text(size=14))
# save
ggsave(paste0("plots/",gsub("\\|","_",cntr_codes),"_growth_comps_",sel_metric,".png"),width=32,height=20,units="cm")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# decompose to popul growth + gdp/capita growth

gdp_decomp_pop = left_join(
    left_join(
      gdp_growth %>% select(country,year,gdp_growth_pct,gdp_growth_pct_smooth) %>% pivot_longer(!c(country,year)) %>%
        mutate(smoothing=ifelse(grepl("smooth",name),"smoothed","point")) %>% rename(gdp_growth_pct=value) %>% select(!name),
      # population
      pop_growth %>% rename(year=Year)%>% select(c(country,year,pop_growth_pct,pop_growth_pct_smooth)) %>% 
        pivot_longer(!c(country,year)) %>% mutate(smoothing=ifelse(grepl("smooth",name),"smoothed","point")) %>% 
        rename(pop_growth_pct=value) %>% select(!name) ), 
      # gdp/capita  
      gdp_per_cap %>% select(c(country,year,gdp_per_cap_growth_pct,gdp_per_cap_growth_pct_smooth)) %>% 
        pivot_longer(!c(country,year)) %>% mutate(smoothing=ifelse(grepl("smooth",name),"smoothed","point")) %>% 
        rename(gdp_per_cap_growth_pct=value) %>% select(!name) ) %>% relocate(smoothing,.after=last_col()) %>%
  mutate(comp_sums=pop_growth_pct+gdp_per_cap_growth_pct)

# summarise
periods <- list("1990-2000"=c(1990,2000),"2001-2008"=c(2001,2008),"2009-2019"=c(2009,2019)); eval_periods<-list()
for (k_per in 1:length(periods)) {
  
  eval_periods[[k_per]] <- gdp_decomp_pop %>% 
    filter(gdp_growth_pct>0 & year>=periods[[k_per]][1] & year<=periods[[k_per]][2] & 
             !grepl("smooth",smoothing)) %>% drop_na() %>% 
    mutate(residual=gdp_growth_pct-comp_sums) %>% pivot_longer(!c(country,year,smoothing)) %>%
    group_by(country,smoothing,name) %>% 
    summarise(mean=mean(value), median=median(value)) %>% # *ifelse(metric_type %in% "share",100,1)
    group_by(country,smoothing) %>% 
    mutate(mean_share_gdp=100*mean/mean[name %in% "gdp_growth_pct"],
           median_share_gdp=100*median/median[name %in% "gdp_growth_pct"]) %>% ungroup() %>% 
    mutate(period=names(periods)[k_per],
           name=case_when(grepl("pop_growth_pct",name) ~ "population",
                          grepl("gdp_per_cap_growth_pct",name) ~ "GDP/capita",
                          grepl("residual",name) ~ "residual",
                          grepl("gdp_growth_pct",name) ~ "total GDP",
                          grepl("comp_sums",name) ~ "linear comp."),
           country=ifelse(grepl("Kingdom",country),"UK",ifelse(grepl("States",country),"US",country))) %>%
    pivot_longer(!c(country,smoothing,name,period),names_to = "metric_type")
  # 
  if (k_per==length(periods)) {df_eval_periods_gdp_per_cap=bind_rows(eval_periods)}
}

# PLOT
# bar plot for periods
k_sel_metr=2; sel_metric=c("mean_share_gdp","mean")[k_sel_metr]
df_eval_periods_gdp_per_cap %>% 
  filter(smoothing %in% "point" & !grepl(c("comp|total","comp")[k_sel_metr],name) & metric_type %in% sel_metric)  %>%
ggplot(aes(y=country,x=value,fill=name)) + 
  geom_bar(stat="identity",position=position_dodge2(padding=0.15),size=1/4,color="black") + 
  facet_wrap(~period) + labs(fill="") + scale_y_discrete(expand=expansion(0.175,0)) +
  geom_hline(yintercept=(1:3)+1/2,size=1/2,linetype="dashed") + geom_vline(xintercept=0) +
  xlab(paste0("% contribution to GDP growth",ifelse(!grepl("share",sel_metric)," (absolute)"," (relative)"))) + ylab("") +
  scale_fill_manual(values=list(c("red","#006600","grey"),c("red","#006600","grey","grey30"))[[k_sel_metr]]) + 
  theme_bw() + standard_theme + theme(legend.position="top",strip.text=element_text(size=14))
# save
ggsave(paste0("plots/",gsub("\\|","_",cntr_codes),"_growth_comps_popul_gdppercap_",sel_metric,".png"),width=32,height=20,units="cm")
