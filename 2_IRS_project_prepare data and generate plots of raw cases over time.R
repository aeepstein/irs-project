library(foreign)
library(lubridate)
library(MASS)
library(lme4)
library(dplyr)
library(ggplot2)
library(ggpubr)

rm(list=ls()) 
setwd("/Users/adrienne/Box Sync/UCSF Research Projects/UMSP") 

#Load in database
db <- read.dta("./Data/Database/UMSP database updated Jan 17th 2020.dta")
colnames(db)
db$date <- ymd(db$date)
db$monthyear_date <- (paste(year(db$date), month(db$date), "01", sep = "-"))
db$monthyear_date <- ymd(db$monthyear_date)
range(db$monthyear_date)

#Load in rainfall
precip <- read.csv("./Data/precipitation for irs project.csv")
db <- merge(db, precip, by = c("site", "monthyear_date"))

#Clean up necessary variables
db$malariapos[db$malariatestresult == "Positive"] <- 1
db$malariapos[db$malariatestresult == "Negative"] <- 0
db$tested <- ifelse(db$anytestresult == "Yes", 1, 0)
db$notsuspected <- ifelse(db$malariasuspected == "No", 1, 0)
db$rdt[db$malariatestdone == "RDT"] <- 1
db$rdt[db$malariatestdone == "Microscopy"] <- 0
db$suspected <- ifelse(db$malariasuspected == "No", 0, 1)

####RQ 1: IRS STOPPED########
db_irs_stopped <- db %>% filter(site == "Aduku" |
                                  site == "Anyeke" |
                                  site == "Aboke")

db_irs_stopped <- db_irs_stopped %>% filter(monthyear_date >= "2013-04-01" & monthyear_date <= "2016-12-31")
range(db_irs_stopped$monthyear_date)

#Set up indicators for time since IRS stopped
aduku_indicators <- db_irs_stopped %>% filter(site == 'Aduku') %>% dplyr::select(site, monthyear_date)
aduku_indicators <- unique(aduku_indicators)
aduku_indicators$mths_since_stop <- seq.int(nrow(aduku_indicators))
aduku_indicators$mths_since_stop <- aduku_indicators$mths_since_stop - 13
aduku_indicators$mths_since_stop <- ifelse(aduku_indicators$mths_since_stop < 0, 0, aduku_indicators$mths_since_stop)

anyeke_indicators <- db_irs_stopped %>% filter(site == 'Anyeke') %>% dplyr::select(site, monthyear_date)
anyeke_indicators <- unique(anyeke_indicators)
anyeke_indicators$mths_since_stop <- seq.int(nrow(anyeke_indicators))
anyeke_indicators$mths_since_stop <- anyeke_indicators$mths_since_stop - 8
anyeke_indicators$mths_since_stop <- ifelse(anyeke_indicators$mths_since_stop < 0, 0, anyeke_indicators$mths_since_stop)

aboke_indicators <- db_irs_stopped %>% filter(site == 'Aboke') %>% dplyr::select(site, monthyear_date)
aboke_indicators <- unique(aboke_indicators)
aboke_indicators$mths_since_stop <- seq.int(nrow(aboke_indicators))
aboke_indicators$mths_since_stop <- aboke_indicators$mths_since_stop - 9
aboke_indicators$mths_since_stop <- ifelse(aboke_indicators$mths_since_stop < 0, 0, aboke_indicators$mths_since_stop)

indicators <- rbind(aduku_indicators, anyeke_indicators, aboke_indicators)

db_irs_stopped <- merge(db_irs_stopped, indicators, by = c('site', 'monthyear_date'))
summary(db_irs_stopped)
hist(db_irs_stopped$mths_since_stop)

db_irs_stopped_agg <- db_irs_stopped %>% group_by(monthyear_date, site, mths_since_stop, precip, precip_lag) %>%
  summarize(tested =sum(tested),
            cases =sum(malariapos, na.rm = T),
            proportion_rdt = mean(rdt, na.rm = T),
            count_rdt = sum(rdt, na.rm = T),
            notsuspect = sum(notsuspected),
            suspect = sum(suspected))
summary(db_irs_stopped_agg)

db_irs_stopped_agg$tpr <- db_irs_stopped_agg$cases/db_irs_stopped_agg$tested
db_irs_stopped_agg$tpr_adj_cases <- (db_irs_stopped_agg$suspect-db_irs_stopped_agg$tested)*db_irs_stopped_agg$tpr
db_irs_stopped_agg$cases_adjusted <- round(db_irs_stopped_agg$cases + db_irs_stopped_agg$tpr_adj_cases) 

#Plots over time
aduku_stopped <- ggplot(db_irs_stopped_agg %>% filter(site == "Aduku"), aes(monthyear_date, cases)) +
  geom_line( size = 1) +
  labs(x = "Date", y = "Malaria Cases") + ggtitle("Aduku") +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month", limits = as.Date(c('2013-04-01','2016-11-31')),expand = c(0.01,0.01)) +
  geom_hline(aes(yintercept=0), size = 0.4) +
  annotate("rect", xmin = as.Date("2013-04-01"), xmax = as.Date("2014-04-01"),
           ymin = -Inf, ymax = Inf,
           alpha = .1, fill= "blue") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2014-04-01")),color = "Last_IRS"), linetype="dashed") +
  scale_color_manual(name = "", values = c(Last_IRS = "blue"), labels = "Last round of IRS") +
  ylim(0, 2200) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size=18)) 

anyeke_stopped <- ggplot(db_irs_stopped_agg %>% filter(site == "Anyeke"), aes(monthyear_date, cases)) +
  geom_line( size = 1) +
  labs(x = "Date", y = "Malaria Cases") + ggtitle("Anyeke") +
  annotate("rect", xmin = as.Date("2014-04-01"), xmax = as.Date("2014-11-01"),
           ymin = -Inf, ymax = Inf,
           alpha = .1, fill= "blue") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2014-11-01")),color = "Last_IRS"), linetype="dashed") +
  scale_color_manual(name = "", values = c(Last_IRS = "blue"), labels = "Last round of IRS") +
  ylim(0, 2200) +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month", limits = as.Date(c('2014-04-01','2016-12-01')),expand = c(0.01,0.01)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size=18)) 

aboke_stopped <- ggplot(db_irs_stopped_agg %>% filter(site == "Aboke"), aes(monthyear_date, cases)) +
  geom_line( size = 1) +
  labs(x = "Date", y = "Malaria Cases") + ggtitle("Aboke") +
  annotate("rect", xmin = as.Date("2014-03-01"), xmax = as.Date("2014-11-01"),
           ymin = -Inf, ymax = Inf,
           alpha = .1, fill= "blue") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2014-11-01")),color = "Last_IRS"), linetype="dashed") +
  scale_color_manual(name = "", values = c(Last_IRS = "blue"), labels = "Last round of IRS") +
  ylim(0, 2200) +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month", limits = as.Date(c('2014-03-01','2016-12-01')),expand = c(0.01,0.01)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size=18)) 

ggarrange(aduku_stopped, anyeke_stopped, aboke_stopped,common.legend = TRUE,  legend="top")
ggsave(filename = "IRS stopped 3 sites raw data.tiff", units="in", width=14, height=10, dpi=300, compression = 'lzw')

#Time categories (for summary IRRs)
db_irs_stopped_agg$time_cat <- ifelse(db_irs_stopped_agg$mths_since_stop == 0, 0, NA)
db_irs_stopped_agg$time_cat[db_irs_stopped_agg$mths_since_stop >0 & db_irs_stopped_agg$mths_since_stop <10] <- 1
db_irs_stopped_agg$time_cat[db_irs_stopped_agg$mths_since_stop >=10] <- 2
db_irs_stopped_agg$month <- month(db_irs_stopped_agg$monthyear_date)

#save data
write.csv(db_irs_stopped_agg, "./IRS project/IRS stopped 3 sites.csv") 

####RQ 2: IRS RESTARTED########
db_irs_restart <- db %>% filter(site == "Aduku" |
                                  site == "Anyeke" |
                                  site == "Aboke" |
                                  site == "Awach" |
                                  site == "Lalogi" |
                                  site == "Patongo" |
                                  site == "Atiak" |
                                  site == "Padibe" |
                                  site == "Namokora")

db_irs_restart <- db_irs_restart %>% filter(monthyear_date >= "2016-01-01")
range(db_irs_restart$monthyear_date)

#Set up indicators for time 1 round of IRS
indicators_may <- db_irs_restart %>% filter(site == 'Aduku' | site == 'Awach' |
                                              site == 'Lalogi' | site == 'Namokora') %>% 
  dplyr::select(site, monthyear_date)

indicators_may_sites <- unique(indicators_may)
indicators_may <- indicators_may %>% dplyr::select(-site)
indicators_may <- unique(indicators_may)
indicators_may$mths_since_irs <- seq.int(nrow(indicators_may))
indicators_may$mths_since_irs <- indicators_may$mths_since_irs - 17
indicators_may$mths_since_irs <- ifelse(indicators_may$mths_since_irs < 0, 0, indicators_may$mths_since_irs)
indicators_may <- merge(indicators_may_sites, indicators_may, by = "monthyear_date")

indicators_feb <- db_irs_restart %>% filter(site == 'Patongo' | site == 'Anyeke' | site == 'Aboke' |
                                              site == 'Atiak') %>% 
  dplyr::select(site, monthyear_date)
indicators_feb_sites <- unique(indicators_feb)
indicators_feb <- indicators_feb %>% dplyr::select(-site)
indicators_feb <- unique(indicators_feb)
indicators_feb$mths_since_irs <- seq.int(nrow(indicators_feb))
indicators_feb$mths_since_irs <- indicators_feb$mths_since_irs - 14
indicators_feb$mths_since_irs <- ifelse(indicators_feb$mths_since_irs < 0, 0, indicators_feb$mths_since_irs)
indicators_feb <- merge(indicators_feb_sites, indicators_feb, by = "monthyear_date")

indicators_aug <- db_irs_restart %>% filter(site == 'Padibe' ) %>% 
  dplyr::select(site, monthyear_date)
indicators_aug_sites <- unique(indicators_aug)
indicators_aug <- indicators_aug %>% dplyr::select(-site)
indicators_aug <- unique(indicators_aug)
indicators_aug$mths_since_irs <- seq.int(nrow(indicators_aug))
indicators_aug$mths_since_irs <- indicators_aug$mths_since_irs - 20
indicators_aug$mths_since_irs <- ifelse(indicators_aug$mths_since_irs < 0, 0, indicators_aug$mths_since_irs)
indicators_aug <- merge(indicators_aug_sites, indicators_aug, by = "monthyear_date")

indicators <- rbind(indicators_may, indicators_feb, indicators_aug)

db_irs_restart <- merge(db_irs_restart, indicators, by = c('site', 'monthyear_date'))
summary(db_irs_restart)
hist(db_irs_restart$mths_since_irs)

#Collapse data by monthyear
db_irs_restart_agg <- db_irs_restart %>% group_by(monthyear_date, site, mths_since_irs, precip, precip_lag) %>%
  summarize(tested =sum(tested),
            cases =sum(malariapos, na.rm = T),
            proportion_rdt = mean(rdt, na.rm = T),
            count_rdt = sum(rdt, na.rm = T),
            notsuspect = sum(notsuspected),
            suspect = sum(suspected))

db_irs_restart_agg$tpr <- db_irs_restart_agg$cases/db_irs_restart_agg$tested
db_irs_restart_agg$tpr_adj_cases <- (db_irs_restart_agg$suspect-db_irs_restart_agg$tested)*db_irs_restart_agg$tpr
db_irs_restart_agg$cases_adjusted <- round(db_irs_restart_agg$cases + db_irs_restart_agg$tpr_adj_cases) 

db_irs_restart_agg$month <- month(db_irs_restart_agg$monthyear_date)

#time categories
db_irs_restart_agg$time_cat <- ifelse(db_irs_restart_agg$mths_since_irs == 0, 0, NA)
db_irs_restart_agg$time_cat <- ifelse(db_irs_restart_agg$mths_since_irs >0 & db_irs_restart_agg$mths_since_irs <6, 1,
                                      db_irs_restart_agg$time_cat )
db_irs_restart_agg$time_cat <- ifelse(db_irs_restart_agg$mths_since_irs >=6 & db_irs_restart_agg$mths_since_irs <=12, 2,
                                      db_irs_restart_agg$time_cat )
db_irs_restart_agg$time_cat <- ifelse(db_irs_restart_agg$mths_since_irs >12 & db_irs_restart_agg$mths_since_irs <23, 3,
                                      db_irs_restart_agg$time_cat )
db_irs_restart_agg$time_cat <- ifelse(db_irs_restart_agg$mths_since_irs >=23 & db_irs_restart_agg$mths_since_irs <=31, 4,
                                      db_irs_restart_agg$time_cat )
db_irs_restart_agg$time_cat <- ifelse(db_irs_restart_agg$mths_since_irs >31, 5,
                                      db_irs_restart_agg$time_cat )
#save data
write.csv(db_irs_restart_agg, "./IRS project/IRS restarted 9 sites.csv") 

#Plots over time
ggplot(db_irs_restart_agg, aes(monthyear_date, cases)) +
  geom_line(aes(colour = site), size = 1) +
  labs(x = "Date", y = "Malaria Cases")

ggplot(db_irs_restart_agg, aes(monthyear_date, tpr)) +
  geom_line(aes(colour = site), size = 1) +
  labs(x = "Date", y = "TPR")

aduku_restart <- ggplot(db_irs_restart_agg %>% filter(site == "Aduku"), aes(monthyear_date, cases)) +
  geom_line( size = 1) +
  labs(x = "Date", y = "Malaria Cases") + ggtitle("Aduku") +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month", limits = as.Date(c('2016-01-01','2019-12-01')),expand = c(0.01,0.01)) +
  geom_vline(aes(xintercept=as.numeric(as.Date("2017-05-01")),color = "Last_IRS"), linetype="dashed") +
  scale_color_manual(name = "", values = c(Last_IRS = "blue"), labels = "Single round of Actellic") +
  ylim(0, 3000) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size=18)) 

anyeke_restart <- ggplot(db_irs_restart_agg %>% filter(site == "Anyeke"), aes(monthyear_date, cases)) +
  geom_line( size = 1) +
  labs(x = "Date", y = "Malaria Cases") + ggtitle("Anyeke") +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month", limits = as.Date(c('2016-01-01','2019-12-01')),expand = c(0.01,0.01)) +
  geom_vline(aes(xintercept=as.numeric(as.Date("2017-02-01")),color = "Last_IRS"), linetype="dashed") +
  scale_color_manual(name = "", values = c(Last_IRS = "blue"), labels = "Single round of Actellic") +
  ylim(0, 3000) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size=18)) 

aboke_restart <-ggplot(db_irs_restart_agg %>% filter(site == "Aboke"), aes(monthyear_date, cases)) +
  geom_line( size = 1) +
  labs(x = "Date", y = "Malaria Cases") + ggtitle("Aboke") +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month", limits = as.Date(c('2016-01-01','2019-12-01')),expand = c(0.01,0.01)) +
  geom_vline(aes(xintercept=as.numeric(as.Date("2017-02-01")),color = "Last_IRS"), linetype="dashed") +
  scale_color_manual(name = "", values = c(Last_IRS = "blue"), labels = "Single round of Actellic") +
  ylim(0, 3000) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size=18)) 

awach_restart <-ggplot(db_irs_restart_agg %>% filter(site == "Awach"), aes(monthyear_date, cases)) +
  geom_line( size = 1) +
  labs(x = "Date", y = "Malaria Cases") + ggtitle("Awach") +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month", limits = as.Date(c('2016-01-01','2019-12-01')),expand = c(0.01,0.01)) +
  geom_vline(aes(xintercept=as.numeric(as.Date("2017-05-01")),color = "Last_IRS"), linetype="dashed") +
  scale_color_manual(name = "", values = c(Last_IRS = "blue"), labels = "Single round of Actellic") +
  ylim(0, 3000) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size=18)) 

lalogi_restart <-ggplot(db_irs_restart_agg %>% filter(site == "Lalogi"), aes(monthyear_date, cases)) +
  geom_line( size = 1) +
  labs(x = "Date", y = "Malaria Cases") + ggtitle("Lalogi") +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month", limits = as.Date(c('2016-01-01','2019-12-01')),expand = c(0.01,0.01)) +
  geom_vline(aes(xintercept=as.numeric(as.Date("2017-05-01")),color = "Last_IRS"), linetype="dashed") +
  scale_color_manual(name = "", values = c(Last_IRS = "blue"), labels = "Single round of Actellic") +
  ylim(0, 3000) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size=18)) 

patongo_restart <- ggplot(db_irs_restart_agg %>% filter(site == "Patongo"), aes(monthyear_date, cases)) +
  geom_line( size = 1) +
  labs(x = "Date", y = "Malaria Cases") + ggtitle("Patongo") +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month", limits = as.Date(c('2016-01-01','2019-12-01')),expand = c(0.01,0.01)) +
  geom_vline(aes(xintercept=as.numeric(as.Date("2017-02-01")),color = "Last_IRS"), linetype="dashed") +
  scale_color_manual(name = "", values = c(Last_IRS = "blue"), labels = "Single round of Actellic") +
  ylim(0, 3000) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size=18)) 

atiak_restart <- ggplot(db_irs_restart_agg %>% filter(site == "Atiak"), aes(monthyear_date, cases)) +
  geom_line( size = 1) +
  labs(x = "Date", y = "Malaria Cases") + ggtitle("Atiak") +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month", limits = as.Date(c('2016-01-01','2019-12-01')),expand = c(0.01,0.01)) +
  geom_vline(aes(xintercept=as.numeric(as.Date("2017-02-01")),color = "Last_IRS"), linetype="dashed") +
  scale_color_manual(name = "", values = c(Last_IRS = "blue"), labels = "Single round of Actellic") +
  ylim(0, 3000) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size=18)) 

padibe_restart <- ggplot(db_irs_restart_agg %>% filter(site == "Padibe"), aes(monthyear_date, cases)) +
  geom_line( size = 1) +
  labs(x = "Date", y = "Malaria Cases") + ggtitle("Padibe") +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month", limits = as.Date(c('2016-01-01','2019-12-01')),expand = c(0.01,0.01)) +
  geom_vline(aes(xintercept=as.numeric(as.Date("2017-08-01")),color = "Last_IRS"), linetype="dashed") +
  scale_color_manual(name = "", values = c(Last_IRS = "blue"), labels = "Single round of Actellic") +
  ylim(0, 3000) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size=18)) 

namokora_restart <- ggplot(db_irs_restart_agg %>% filter(site == "Namokora"), aes(monthyear_date, cases)) +
  geom_line( size = 1) +
  labs(x = "Date", y = "Malaria Cases") + ggtitle("Namokora") +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month", limits = as.Date(c('2016-01-01','2019-12-01')),expand = c(0.01,0.01)) +
  geom_vline(aes(xintercept=as.numeric(as.Date("2017-05-01")),color = "Last_IRS"), linetype="dashed") +
  scale_color_manual(name = "", values = c(Last_IRS = "blue"), labels = "Single round of Actellic") +
  ylim(0, 3000) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size=18)) 

ggarrange(aduku_restart, anyeke_restart, aboke_restart,
          awach_restart, lalogi_restart, patongo_restart,
          atiak_restart, padibe_restart, namokora_restart,
          common.legend = TRUE,  legend="top")
ggsave(filename = "IRS restart 9 sites raw data.tiff", units="in", width=16, height=10, dpi=300, compression = 'lzw')

####RQ 3: IRS STARTED########
db_irs_start <- db %>% filter(site == "Nagongera" |
                                site == "Amolatar" |
                                site == "Dokolo" |
                                site == "Orum" |
                                site == "Alebtong")

db_irs_start <- db_irs_start %>% filter(monthyear_date >= "2014-01-01")
range(db_irs_start$monthyear_date)

######Set up indicators for time since IRS#######
nagongera_indicators <- db_irs_start %>% filter(site == 'Nagongera') %>% dplyr::select(site, monthyear_date)
nagongera_indicators <- unique(nagongera_indicators)
nagongera_indicators$mths_since_irs_continuous <-seq.int(nrow(nagongera_indicators))
nagongera_indicators$mths_since_irs_continuous <- nagongera_indicators$mths_since_irs_continuous-13
nagongera_indicators$mths_since_irs_continuous <- ifelse(nagongera_indicators$mths_since_irs_continuous<0,0,
                                                         nagongera_indicators$mths_since_irs_continuous)

nag_baseline <- nagongera_indicators %>% filter(monthyear_date <= "2015-01-01")
nag_baseline$mths_since_irs <- 0
nag_baseline$count_previous_irs <- 0
nag_1 <- nagongera_indicators %>% filter(monthyear_date >= "2015-02-01" & monthyear_date <= "2015-06-01")
nag_1$mths_since_irs <- seq.int(nrow(nag_1))
nag_1$count_previous_irs <- 0
nag_2 <- nagongera_indicators %>% filter(monthyear_date >= "2015-07-01" & monthyear_date <= "2015-11-01")
nag_2$mths_since_irs <- seq.int(nrow(nag_2))
nag_2$count_previous_irs <- 1
nag_3 <- nagongera_indicators %>% filter(monthyear_date >= "2015-12-01" & monthyear_date <= "2016-06-01")
nag_3$mths_since_irs <- seq.int(nrow(nag_3))
nag_3$count_previous_irs <- 2
nag_4 <- nagongera_indicators %>% filter(monthyear_date >= "2016-07-01" & monthyear_date <= "2017-09-01")
nag_4$mths_since_irs <- seq.int(nrow(nag_4))
nag_4$count_previous_irs <- 3
nag_5 <- nagongera_indicators %>% filter(monthyear_date >= "2017-10-01" & monthyear_date <= "2018-07-01")
nag_5$mths_since_irs <- seq.int(nrow(nag_5))
nag_5$count_previous_irs <- 4
nag_6 <- nagongera_indicators %>% filter(monthyear_date >= "2018-08-01" & monthyear_date <= "2019-03-01")
nag_6$mths_since_irs <- seq.int(nrow(nag_6))
nag_6$count_previous_irs <- 5
nag_7 <- nagongera_indicators %>% filter(monthyear_date >= "2019-04-01" & monthyear_date <= "2019-12-01")
nag_7$mths_since_irs <- seq.int(nrow(nag_7))
nag_7$count_previous_irs <- 6
nagongera_indicators <- rbind(nag_baseline, nag_1, nag_2, nag_3, nag_4, nag_5, nag_6, nag_7)

nagongera_indicators$IRS <- 0
nagongera_indicators$IRS[nagongera_indicators$monthyear_date == "2015-01-01" |
                           nagongera_indicators$monthyear_date == "2015-06-01" |
                           nagongera_indicators$monthyear_date == "2015-11-01" |
                           nagongera_indicators$monthyear_date =="2016-06-01" |
                           nagongera_indicators$monthyear_date =="2017-08-01" |
                           nagongera_indicators$monthyear_date =="2018-07-01" | 
                           nagongera_indicators$monthyear_date =="2019-03-01"] <- 1

nagongera_indicators$count_irs_12_months <- 0 
for(i in 13:nrow(nagongera_indicators)){
  nagongera_indicators$count_irs_12_months[i] <- sum(nagongera_indicators$IRS[(i-12):(i-1)])
}

nagongera_indicators$count_irs_24_months <- 0 
for(i in 25:nrow(nagongera_indicators)){
  nagongera_indicators$count_irs_24_months[i] <- sum(nagongera_indicators$IRS[(i-24):(i-1)])
}
for(i in 1:24){
  nagongera_indicators$count_irs_24_months[i] <- sum(nagongera_indicators$IRS[1:(i-1)])
}

amolatar_indicators <- db_irs_start %>% filter(site == 'Amolatar') %>% dplyr::select(site, monthyear_date)
amolatar_indicators <- unique(amolatar_indicators)
amolatar_indicators$mths_since_irs_continuous <-seq.int(nrow(amolatar_indicators))
amolatar_indicators$mths_since_irs_continuous <- amolatar_indicators$mths_since_irs_continuous-12
amolatar_indicators$mths_since_irs_continuous <- ifelse(amolatar_indicators$mths_since_irs_continuous<0,0,
                                                        amolatar_indicators$mths_since_irs_continuous)

amo_baseline <- amolatar_indicators %>% filter(monthyear_date <= "2015-01-01")
amo_baseline$mths_since_irs <- 0
amo_baseline$count_previous_irs <- 0
amo_1 <- amolatar_indicators %>% filter(monthyear_date >= "2015-02-01" & monthyear_date <= "2015-06-01")
amo_1$mths_since_irs <- seq.int(nrow(amo_1))
amo_1$count_previous_irs <- 0
amo_2 <- amolatar_indicators %>% filter(monthyear_date >= "2015-07-01" & monthyear_date <= "2015-10-01")
amo_2$mths_since_irs <- seq.int(nrow(amo_2))
amo_2$count_previous_irs <- 1
amo_3 <- amolatar_indicators %>% filter(monthyear_date >= "2015-11-01" & monthyear_date <= "2016-10-01")
amo_3$mths_since_irs <- seq.int(nrow(amo_3))
amo_3$count_previous_irs <- 2
amo_4 <- amolatar_indicators %>% filter(monthyear_date >= "2016-11-01" & monthyear_date <= "2017-05-01")
amo_4$mths_since_irs <- seq.int(nrow(amo_4))
amo_4$count_previous_irs <- 3
amo_5 <- amolatar_indicators %>% filter(monthyear_date >= "2017-06-01" & monthyear_date <= "2018-04-01")
amo_5$mths_since_irs <- seq.int(nrow(amo_5))
amo_5$count_previous_irs <- 4
amo_6 <- amolatar_indicators %>% filter(monthyear_date >= "2018-05-01" & monthyear_date <= "2019-06-01")
amo_6$mths_since_irs <- seq.int(nrow(amo_6))
amo_6$count_previous_irs <- 5
amo_7 <- amolatar_indicators %>% filter(monthyear_date >= "2019-07-01" & monthyear_date <= "2019-12-01")
amo_7$mths_since_irs <- seq.int(nrow(amo_7))
amo_7$count_previous_irs <- 6
amolatar_indicators <- rbind(amo_baseline, amo_1, amo_2, amo_3, amo_4, amo_5, amo_6, amo_7)

amolatar_indicators$IRS <- 0
amolatar_indicators$IRS[amolatar_indicators$monthyear_date == "2015-01-01" |
                          amolatar_indicators$monthyear_date ==  "2015-06-01" |
                          amolatar_indicators$monthyear_date ==  "2015-10-01" |
                          amolatar_indicators$monthyear_date =="2016-10-01" |
                          amolatar_indicators$monthyear_date =="2017-05-01" |
                          amolatar_indicators$monthyear_date =="2018-04-01" | 
                          amolatar_indicators$monthyear_date =="2019-06-01"] <- 1
amolatar_indicators$count_irs_12_months <- 0 
for(i in 13:nrow(amolatar_indicators)){
  amolatar_indicators$count_irs_12_months[i] <- sum(amolatar_indicators$IRS[(i-12):(i-1)])
}
amolatar_indicators$count_irs_24_months <- 0 
for(i in 25:nrow(amolatar_indicators)){
  amolatar_indicators$count_irs_24_months[i] <- sum(amolatar_indicators$IRS[(i-24):(i-1)])
}
for(i in 1:24){
  amolatar_indicators$count_irs_24_months[i] <- sum(amolatar_indicators$IRS[1:(i-1)])
}

dokolo_indicators <- db_irs_start %>% filter(site == 'Dokolo') %>% dplyr::select(site, monthyear_date)
dokolo_indicators <- unique(dokolo_indicators)
dokolo_indicators$mths_since_irs_continuous <-seq.int(nrow(dokolo_indicators))
dokolo_indicators$mths_since_irs_continuous <- dokolo_indicators$mths_since_irs_continuous-12
dokolo_indicators$mths_since_irs_continuous <- ifelse(dokolo_indicators$mths_since_irs_continuous<0,0,
                                                      dokolo_indicators$mths_since_irs_continuous)


dok_baseline <- dokolo_indicators %>% filter(monthyear_date <= "2015-01-01")
dok_baseline$mths_since_irs <- 0
dok_baseline$count_previous_irs <- 0
dok_1 <- dokolo_indicators %>% filter(monthyear_date >= "2015-02-01" & monthyear_date <= "2015-06-01")
dok_1$mths_since_irs <- seq.int(nrow(dok_1))
dok_1$count_previous_irs <- 0
dok_2 <- dokolo_indicators %>% filter(monthyear_date >= "2015-07-01" & monthyear_date <= "2015-10-01")
dok_2$mths_since_irs <- seq.int(nrow(dok_2))
dok_2$count_previous_irs <- 1
dok_3 <- dokolo_indicators %>% filter(monthyear_date >= "2015-11-01" & monthyear_date <= "2016-05-01")
dok_3$mths_since_irs <- seq.int(nrow(dok_3))
dok_3$count_previous_irs <- 2
dok_4 <- dokolo_indicators %>% filter(monthyear_date >= "2016-06-01" & monthyear_date <= "2017-05-01")
dok_4$mths_since_irs <- seq.int(nrow(dok_4))
dok_4$count_previous_irs <- 3
dok_5 <- dokolo_indicators %>% filter(monthyear_date >= "2017-06-01" & monthyear_date <= "2018-04-01")
dok_5$mths_since_irs <- seq.int(nrow(dok_5))
dok_5$count_previous_irs <- 4
dok_6 <- dokolo_indicators %>% filter(monthyear_date >= "2018-05-01" & monthyear_date <= "2019-06-01")
dok_6$mths_since_irs <- seq.int(nrow(dok_6))
dok_6$count_previous_irs <- 5
dok_7 <- dokolo_indicators %>% filter(monthyear_date >= "2019-07-01" & monthyear_date <= "2019-12-01")
dok_7$mths_since_irs <- seq.int(nrow(dok_7))
dok_7$count_previous_irs <- 6
dokolo_indicators <- rbind(dok_baseline, dok_1, dok_2, dok_3, dok_4, dok_5, dok_6, dok_7)

dokolo_indicators$IRS <- 0
dokolo_indicators$IRS[dokolo_indicators$monthyear_date == "2015-01-01" |
                        dokolo_indicators$monthyear_date ==  "2015-06-01" |
                        dokolo_indicators$monthyear_date ==  "2015-10-01" |
                        dokolo_indicators$monthyear_date =="2016-05-01" |
                        dokolo_indicators$monthyear_date =="2017-05-01" |
                        dokolo_indicators$monthyear_date =="2018-04-01" | 
                        dokolo_indicators$monthyear_date =="2019-06-01"] <- 1
dokolo_indicators$count_irs_12_months <- 0 
for(i in 13:nrow(dokolo_indicators)){
  dokolo_indicators$count_irs_12_months[i] <- sum(dokolo_indicators$IRS[(i-12):(i-1)])
}
dokolo_indicators$count_irs_24_months <- 0 
for(i in 25:nrow(dokolo_indicators)){
  dokolo_indicators$count_irs_24_months[i] <- sum(dokolo_indicators$IRS[(i-24):(i-1)])
}
for(i in 1:24){
  dokolo_indicators$count_irs_24_months[i] <- sum(dokolo_indicators$IRS[1:(i-1)])
}

orum_indicators <- db_irs_start %>% filter(site == 'Orum') %>% dplyr::select(site, monthyear_date)
orum_indicators <- unique(orum_indicators)
orum_indicators$mths_since_irs_continuous <-seq.int(nrow(orum_indicators))
orum_indicators$mths_since_irs_continuous <- orum_indicators$mths_since_irs_continuous-11
orum_indicators$mths_since_irs_continuous <- ifelse(orum_indicators$mths_since_irs_continuous<0,0,
                                                    orum_indicators$mths_since_irs_continuous)

orum_baseline <- orum_indicators %>% filter(monthyear_date <= "2015-01-01")
orum_baseline$mths_since_irs <- 0
orum_baseline$count_previous_irs <- 0
orum_1 <- orum_indicators %>% filter(monthyear_date >= "2015-02-01" & monthyear_date <= "2015-06-01")
orum_1$mths_since_irs <- seq.int(nrow(orum_1))
orum_1$count_previous_irs <- 0
orum_2 <- orum_indicators %>% filter(monthyear_date >= "2015-07-01" & monthyear_date <= "2015-11-01")
orum_2$mths_since_irs <- seq.int(nrow(orum_2))
orum_2$count_previous_irs <- 1
orum_3 <- orum_indicators %>% filter(monthyear_date >= "2015-12-01" & monthyear_date <= "2016-05-01")
orum_3$mths_since_irs <- seq.int(nrow(orum_3))
orum_3$count_previous_irs <- 2
orum_4 <- orum_indicators %>% filter(monthyear_date >= "2016-06-01" & monthyear_date <= "2017-08-01")
orum_4$mths_since_irs <- seq.int(nrow(orum_4))
orum_4$count_previous_irs <- 3
orum_5 <- orum_indicators %>% filter(monthyear_date >= "2017-09-01" & monthyear_date <= "2018-07-01")
orum_5$mths_since_irs <- seq.int(nrow(orum_5))
orum_5$count_previous_irs <- 4
orum_6 <- orum_indicators %>% filter(monthyear_date >= "2018-08-01" & monthyear_date <= "2019-06-01")
orum_6$mths_since_irs <- seq.int(nrow(orum_6))
orum_6$count_previous_irs <- 5
orum_7 <- orum_indicators %>% filter(monthyear_date >= "2019-07-01" & monthyear_date <= "2019-12-01")
orum_7$mths_since_irs <- seq.int(nrow(orum_7))
orum_7$count_previous_irs <- 6
orum_indicators <- rbind(orum_baseline, orum_1, orum_2, orum_3, orum_4, orum_5, orum_6, orum_7)

orum_indicators$IRS <- 0
orum_indicators$IRS[orum_indicators$monthyear_date == "2015-01-01" |
                      orum_indicators$monthyear_date ==  "2015-06-01" |
                      orum_indicators$monthyear_date ==  "2015-11-01" |
                      orum_indicators$monthyear_date =="2016-05-01" |
                      orum_indicators$monthyear_date =="2017-08-01" |
                      orum_indicators$monthyear_date =="2018-07-01" | 
                      orum_indicators$monthyear_date =="2019-06-01"] <- 1
orum_indicators$count_irs_12_months <- 0 
for(i in 13:nrow(orum_indicators)){
  orum_indicators$count_irs_12_months[i] <- sum(orum_indicators$IRS[(i-12):(i-1)])
}
orum_indicators$count_irs_24_months <- 0 
for(i in 25:nrow(orum_indicators)){
  orum_indicators$count_irs_24_months[i] <- sum(orum_indicators$IRS[(i-24):(i-1)])
}
for(i in 1:24){
  orum_indicators$count_irs_24_months[i] <- sum(orum_indicators$IRS[1:(i-1)])
}

alebtong_indicators <- db_irs_start %>% filter(site == 'Alebtong') %>% dplyr::select(site, monthyear_date)
alebtong_indicators <- unique(alebtong_indicators)
alebtong_indicators$mths_since_irs_continuous <-seq.int(nrow(alebtong_indicators))
alebtong_indicators$mths_since_irs_continuous <- alebtong_indicators$mths_since_irs_continuous-8
alebtong_indicators$mths_since_irs_continuous <- ifelse(alebtong_indicators$mths_since_irs_continuous<0,0,
                                                        alebtong_indicators$mths_since_irs_continuous)

alebtong_baseline <- alebtong_indicators %>% filter(monthyear_date <= "2015-01-01")
alebtong_baseline$mths_since_irs <- 0
alebtong_baseline$count_previous_irs <- 0
alebtong_1 <- alebtong_indicators %>% filter(monthyear_date >= "2015-02-01" & monthyear_date <= "2015-06-01")
alebtong_1$mths_since_irs <- seq.int(nrow(alebtong_1))
alebtong_1$count_previous_irs <- 0
alebtong_2 <- alebtong_indicators %>% filter(monthyear_date >= "2015-07-01" & monthyear_date <= "2015-10-01")
alebtong_2$mths_since_irs <- seq.int(nrow(alebtong_2))
alebtong_2$count_previous_irs <- 1
alebtong_3 <- alebtong_indicators %>% filter(monthyear_date >= "2015-11-01" & monthyear_date <= "2016-11-01")
alebtong_3$mths_since_irs <- seq.int(nrow(alebtong_3))
alebtong_3$count_previous_irs <- 2
alebtong_4 <- alebtong_indicators %>% filter(monthyear_date >= "2016-12-01" & monthyear_date <= "2017-05-01")
alebtong_4$mths_since_irs <- seq.int(nrow(alebtong_4))
alebtong_4$count_previous_irs <- 3
alebtong_5 <- alebtong_indicators %>% filter(monthyear_date >= "2017-06-01" & monthyear_date <= "2018-04-01")
alebtong_5$mths_since_irs <- seq.int(nrow(alebtong_5))
alebtong_5$count_previous_irs <- 4
alebtong_6 <- alebtong_indicators %>% filter(monthyear_date >= "2018-05-01" & monthyear_date <= "2019-06-01")
alebtong_6$mths_since_irs <- seq.int(nrow(alebtong_6))
alebtong_6$count_previous_irs <- 5
alebtong_7 <- alebtong_indicators %>% filter(monthyear_date >= "2019-07-01" & monthyear_date <= "2019-12-01")
alebtong_7$mths_since_irs <- seq.int(nrow(alebtong_7))
alebtong_7$count_previous_irs <- 6
alebtong_indicators <- rbind(alebtong_baseline, alebtong_1, alebtong_2, alebtong_3, alebtong_4, alebtong_5, alebtong_6, alebtong_7)

alebtong_indicators$IRS <- 0
alebtong_indicators$IRS[alebtong_indicators$monthyear_date == "2015-01-01" |
                          alebtong_indicators$monthyear_date ==  "2015-06-01" |
                          alebtong_indicators$monthyear_date ==  "2015-10-01" |
                          alebtong_indicators$monthyear_date =="2016-11-01" |
                          alebtong_indicators$monthyear_date =="2017-05-01" |
                          alebtong_indicators$monthyear_date =="2018-04-01" | 
                          alebtong_indicators$monthyear_date =="2019-06-01"] <- 1
alebtong_indicators$count_irs_12_months <- 0 
for(i in 13:nrow(alebtong_indicators)){
  alebtong_indicators$count_irs_12_months[i] <- sum(alebtong_indicators$IRS[(i-12):(i-1)])
}
for(i in 9:12){
  alebtong_indicators$count_irs_12_months[i] <- sum(alebtong_indicators$IRS[(i-9):(i-1)])
}
alebtong_indicators$count_irs_24_months <- 0 
for(i in 25:nrow(alebtong_indicators)){
  alebtong_indicators$count_irs_24_months[i] <- sum(alebtong_indicators$IRS[(i-24):(i-1)])
}
for(i in 1:24){
  alebtong_indicators$count_irs_24_months[i] <- sum(alebtong_indicators$IRS[1:(i-1)])
}

indicators <- rbind(nagongera_indicators,
                    amolatar_indicators,
                    dokolo_indicators,
                    orum_indicators,
                    alebtong_indicators)

db_irs_start <- merge(db_irs_start, indicators, by = c('site', 'monthyear_date'))
summary(db_irs_start)
hist(db_irs_start$mths_since_irs)

#Collapse data by monthyear
db_irs_start_agg <- db_irs_start %>% group_by(monthyear_date, site, mths_since_irs_continuous,mths_since_irs, precip, precip_lag,
                                              count_previous_irs,count_irs_12_months,count_irs_24_months) %>%
  summarize(tested =sum(tested),
            cases =sum(malariapos, na.rm = T),
            proportion_rdt = mean(rdt, na.rm = T),
            count_rdt = sum(rdt, na.rm = T),
            notsuspect = sum(notsuspected),
            suspect = sum(suspected))

db_irs_start_agg$tpr <- db_irs_start_agg$cases/db_irs_start_agg$tested
db_irs_start_agg$tpr_adj_cases <- (db_irs_start_agg$suspect-db_irs_start_agg$tested)*db_irs_start_agg$tpr
db_irs_start_agg$cases_adjusted <- round(db_irs_start_agg$cases + db_irs_start_agg$tpr_adj_cases) 

db_irs_start_agg$month <- month(db_irs_start_agg$monthyear_date)

#Time cat for summary IRR
db_irs_start_agg$time_cat <- ifelse(db_irs_start_agg$mths_since_irs_continuous == 0, 0, NA)
db_irs_start_agg$time_cat <- ifelse(db_irs_start_agg$mths_since_irs_continuous >=1 & db_irs_start_agg$mths_since_irs_continuous <=12, 1,
                                    db_irs_start_agg$time_cat )
db_irs_start_agg$time_cat <- ifelse(db_irs_start_agg$mths_since_irs_continuous >12 & db_irs_start_agg$mths_since_irs_continuous <=24, 2,
                                    db_irs_start_agg$time_cat )
db_irs_start_agg$time_cat <- ifelse(db_irs_start_agg$mths_since_irs_continuous >24 & db_irs_start_agg$mths_since_irs_continuous <=36, 3,
                                    db_irs_start_agg$time_cat )
db_irs_start_agg$time_cat <- ifelse(db_irs_start_agg$mths_since_irs_continuous >36 & db_irs_start_agg$mths_since_irs_continuous <=48, 4,
                                    db_irs_start_agg$time_cat )
db_irs_start_agg$time_cat <- ifelse(db_irs_start_agg$mths_since_irs_continuous >48 & db_irs_start_agg$mths_since_irs_continuous <=60, 5,
                                    db_irs_start_agg$time_cat )

#save data
write.csv(db_irs_start_agg, "./IRS project/IRS started 5 sites.csv") 

#Plots over time
ggplot(db_irs_start_agg, aes(monthyear_date, cases)) +
  geom_line(aes(colour = site), size = 1) +
  labs(x = "Date", y = "Malaria Cases")

ggplot(db_irs_start_agg, aes(monthyear_date, tpr)) +
  geom_line(aes(colour = site), size = 1) +
  labs(x = "Date", y = "TPR")

nagongera_start <- ggplot(db_irs_start_agg %>% filter(site == "Nagongera"), aes(monthyear_date, cases)) +
  geom_line( size = 1) +
  labs(x = "Date", y = "Malaria Cases") + ggtitle("Nagongera") +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month", limits = as.Date(c('2014-01-01','2019-12-01')),expand = c(0.01,0.01)) +
  geom_vline(aes(xintercept=as.numeric(as.Date("2015-01-01")),color = "bendio"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2015-06-01")),color = "bendio"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2015-11-01")),color = "bendio"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2016-06-01")),color = "actellic"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2017-08-01")),color = "actellic"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2018-07-01")),color = "actellic"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2019-03-01")),color = "actellic"), linetype="dashed") +
  scale_color_manual(name = "", values = c(bendio = "blue", actellic = "red", sumi = "purple"), labels = c("Bendiocarb", "Actellic",
                                                                                                           "Sumishield 50W")) +
  ylim(0,1000)  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size=18)) 

amolatar_start <- ggplot(db_irs_start_agg %>% filter(site == "Amolatar"), aes(monthyear_date, cases)) +
  geom_line( size = 1) +
  labs(x = "Date", y = "Malaria Cases") + ggtitle("Amolatar") +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month", limits = as.Date(c('2014-01-01','2019-12-01')),expand = c(0.01,0.01)) +
  geom_vline(aes(xintercept=as.numeric(as.Date("2015-01-01")),color = "bendio"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2015-06-01")),color = "bendio"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2015-10-01")),color = "bendio"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2016-10-01")),color = "bendio"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2017-05-01")),color = "actellic"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2018-04-01")),color = "actellic"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2019-06-01")),color = "actellic"), linetype="dashed") +
  scale_color_manual(name = "", values = c(bendio = "blue", actellic = "red", sumi = "purple"), labels = c("Bendiocarb", "Actellic",
                                                                                                           "Sumishield 50W")) +
  ylim(0,1000) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size=18)) 

dokolo_start <- ggplot(db_irs_start_agg %>% filter(site == "Dokolo"), aes(monthyear_date, cases)) +
  geom_line( size = 1) +
  labs(x = "Date", y = "Malaria Cases") + ggtitle("Dokolo") +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month", limits = as.Date(c('2014-01-01','2019-12-01')),expand = c(0.01,0.01)) +
  geom_vline(aes(xintercept=as.numeric(as.Date("2015-01-01")),color = "bendio"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2015-06-01")),color = "bendio"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2015-10-01")),color = "bendio"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2016-05-01")),color = "actellic"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2017-05-01")),color = "actellic"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2018-04-01")),color = "actellic"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2019-06-01")),color = "sumi"), linetype="dashed") +
  scale_color_manual(name = "", values = c(bendio = "blue", actellic = "red", sumi = "purple"), labels = c("Bendiocarb", "Actellic",
                                                                                                           "Sumishield 50W")) +
  ylim(0,1000) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size=18)) 

orum_start <- ggplot(db_irs_start_agg %>% filter(site == "Orum"), aes(monthyear_date, cases)) +
  geom_line( size = 1) +
  labs(x = "Date", y = "Malaria Cases") + ggtitle("Orum") +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month", limits = as.Date(c('2014-01-01','2019-12-01')),expand = c(0.01,0.01)) +
  geom_vline(aes(xintercept=as.numeric(as.Date("2015-01-01")),color = "bendio"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2015-06-01")),color = "bendio"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2015-11-01")),color = "bendio"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2016-05-01")),color = "actellic"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2017-08-01")),color = "actellic"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2018-07-01")),color = "actellic"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2019-06-01")),color = "actellic"), linetype="dashed") +
  scale_color_manual(name = "", values = c(bendio = "blue", actellic = "red", sumi = "purple"), labels = c("Bendiocarb", "Actellic",
                                                                                                           "Sumishield 50W")) +
  ylim(0,1000) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size=18)) 

alebtong_start <- ggplot(db_irs_start_agg %>% filter(site == "Alebtong"), aes(monthyear_date, cases)) +
  geom_line( size = 1) +
  labs(x = "Date", y = "Malaria Cases") + ggtitle("Alebtong") +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month", limits = as.Date(c('2014-01-01','2019-12-01')),expand = c(0.01,0.01)) +
  geom_vline(aes(xintercept=as.numeric(as.Date("2015-01-01")),color = "bendio"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2015-06-01")),color = "bendio"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2015-10-01")),color = "bendio"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2016-11-01")),color = "bendio"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2017-05-01")),color = "actellic"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2018-04-01")),color = "actellic"), linetype="dashed") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2019-06-01")),color = "actellic"), linetype="dashed") +
  scale_color_manual(name = "", values = c(bendio = "blue", actellic = "red", sumi = "purple"), labels = c("Bendiocarb", "Actellic",
                                                                                                           "Sumishield 50W")) +
  ylim(0,1000) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size=18)) 

ggarrange(nagongera_start, amolatar_start, dokolo_start,
          orum_start, alebtong_start, 
          common.legend = TRUE,  legend="top")
ggsave(filename = "IRS start 5 sites raw data.tiff", units="in", width=16, height=8, dpi=300, compression = 'lzw')