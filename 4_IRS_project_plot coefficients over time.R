library(foreign)
library(lubridate)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(readxl)
rm(list=ls())

db_irs_stopped_agg <- read.csv("./IRS project/IRS stopped 3 sites.csv")
db_irs_stopped_agg$monthyear_date <- ymd(db_irs_stopped_agg$monthyear_date)
db_irs_restart_agg <- read.csv("./IRS project/IRS restarted 9 sites.csv")
db_irs_restart_agg$monthyear_date <- ymd(db_irs_restart_agg$monthyear_date)
db_irs_start_agg <- read.csv("./IRS project/IRS started 5 sites.csv")
db_irs_start_agg$monthyear_date <- ymd(db_irs_start_agg$monthyear_date)

######Q1#######
coefs <- read_excel("./IRS project/stop.xlsx")
coefs$..1 <- NULL
coefs$..2 <- NULL
colnames(coefs) <-  c("IRR", "pvalue", "upperbound", "lowerbound")
coefs <- as.data.frame(coefs)
coefs <- coefs[2:32, 1:ncol(coefs)]
coefs$months_since_stopping <- seq.int(nrow(coefs))

ggplot(coefs, aes(factor(months_since_stopping), IRR)) +
  geom_hline(yintercept=1, lty=2, lwd=1, colour="grey50") +
  scale_y_continuous(trans='log10', breaks = c(0.25, 0.5, 1, 2,4,8,16,32)) +
  geom_errorbar(aes(ymin=lowerbound, ymax=upperbound),
                lwd=1, width=0) +
  geom_point(size=3) +
  guides(colour=FALSE) +
  labs(x="Months since IRS was stopped", y="Incidence Rate Ratio") +
  theme_bw(base_size=18)
ggsave(filename = "IRRs IRS stop.tiff", units="in", width=12, height=8, dpi=300, compression = 'lzw')

ggplot(coefs, aes(factor(months_since_stopping), IRR)) +
  geom_hline(yintercept=1, lty=2, lwd=1, colour="grey50") +
  scale_y_continuous(breaks = c(1,5,10,15,20)) +
  geom_errorbar(aes(ymin=lowerbound, ymax=upperbound),
                lwd=1, width=0) +
  geom_point(size=3) +
  guides(colour=FALSE) +
  labs(x="Months since IRS was stopped", y="Incidence Rate Ratio") +
  theme_grey(base_size=15) 

######Q2#######
coefs <- read_excel("./restart.xlsx")
coefs$..1 <- NULL
coefs$..2 <- NULL
colnames(coefs) <-  c("IRR", "pvalue", "upperbound", "lowerbound")
coefs <- as.data.frame(coefs)
coefs <- coefs[2:34, 1:ncol(coefs)]
coefs$months_since_irs <- seq.int(nrow(coefs))

ggplot(coefs, aes(as.factor(months_since_irs), IRR)) +
  geom_hline(yintercept=1, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(ymin=lowerbound, ymax=upperbound),
                lwd=1, width=0) +
  geom_point(size=3) +
  guides(colour=FALSE) +
  labs(x="Months since 1 round of IRS in 2017", y="Incidence Rate Ratio") +
  theme_bw(base_size=18) +
  scale_y_continuous(breaks = c(0.5, 1, 1.5))
ggsave(filename = "IRRs IRS restart.tiff", units="in", width=12, height=8, dpi=300, compression = 'lzw')

######Q3#######
coefs <- read_excel("./start_5_continuous.xlsx")
coefs$..1 <- NULL
coefs$..2 <- NULL
colnames(coefs) <-  c("IRR", "pvalue", "upperbound", "lowerbound")
coefs <- as.data.frame(coefs)
coefs <- coefs[2:60, 1:ncol(coefs)]
coefs$months_since_irs <- seq.int(nrow(coefs))

ggplot(coefs, aes(as.factor(months_since_irs), IRR)) +
  geom_hline(yintercept=1, lty=2, lwd=1, colour="grey50") +
  scale_y_continuous(breaks = seq(0,3, by = 0.25)) +
  scale_x_discrete(breaks = seq(1,59, by = 2)) +
  geom_errorbar(aes(ymin=lowerbound, ymax=upperbound),
                lwd=1, width=0) +
  geom_point(size=3) +
  guides(colour=FALSE) +
  labs(x="Months since IRS was initiated", y="Incidence Rate Ratio") +
  theme_bw(base_size=18) 
ggsave(filename = "IRRs IRS start.TIFF", units="in", width=12, height=8, dpi=300, compression = 'lzw')
