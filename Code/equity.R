library(foreign)
library(tidyverse)

source("Code/Convert UGM score.R")

blockgrp <- read.dbf("Data/PDX_blockgrp_point.dbf")
blockgrp <- merge(blockgrp,block_grp)

options(scipen=999)
ACS <- read.csv("Data/ACS.csv")
ACS <- ACS[,-c(2:55,59,64)]
ACS <- ACS[,-c(18,19)]
colnames(ACS) <- c("FIPS","pop15","pop_den","area","under_18","18_34","35_64","65_over","white","black","indian","asian","hawaiian","other","two more",
                   "pop_pov_det","doing_poor","struggling","poor_struggling","doing_ok")
ACS <- ACS[-c(521),]

blockgrp <- merge(blockgrp,ACS,by=c("FIPS"))
blockgrp$Id2 <- substr(blockgrp$FIPS,1,11)

ELP <- read.csv("Data/LEP.csv")
blockgrp <- merge (blockgrp,ELP,by="Id2")

rm(ACS,ELP,block_grp)


# Equity

# Quantile analysis acc1 --------------------------------------------------

# race
blockgrp$race_nonwhite <- 1- blockgrp$white/blockgrp$pop15
q_nonwhite <- quantile(blockgrp$race_nonwhite,seq(0,1,by=0.2))
cuts_nonwhite = cut(blockgrp$race_nonwhite, q_nonwhite)
blockgrp$cuts_nonwhite <- cuts_nonwhite
library(dplyr)
change_by_race <- na.omit(blockgrp) %>% group_by(cuts_nonwhite) %>% 
  summarize(dis_bk_16=mean(acc1_base), 
            dis_bk_grw=mean(acc1_gw),
            change=mean(acc1_gw - acc1_base),
            grwth = dis_bk_grw/dis_bk_16 -1)
cor.test(blockgrp$acc1_base,blockgrp$race_nonwhite) # no signficant correlation
cor.test(blockgrp$acc1_gw,blockgrp$race_nonwhite) # no significant correlation
cor.test((blockgrp$acc1_gw/blockgrp$acc1_base -1),blockgrp$race_nonwhite) # significant correlation

# poverty
blockgrp$pov <- 1- blockgrp$doing_ok/blockgrp$pop_pov_det
q_pov <- quantile(blockgrp$pov,seq(0,1,by=0.2),na.rm=T)
cuts_pov <- cut(blockgrp$pov, q_pov)
blockgrp$cuts_pov <- cuts_pov
change_by_pov <- na.omit(blockgrp) %>% group_by(cuts_pov) %>% 
  summarise(dis_bk_16=mean(acc1_base), 
            dis_bk_grw=mean(acc1_gw),
            change=mean(acc1_gw - acc1_base),
            grwth = mean(acc1_gw/acc1_base-1))

cor.test(blockgrp$acc1_base,blockgrp$pov) # not significant
cor.test(blockgrp$acc1_gw,blockgrp$pov) # significant positive correlation
cor.test((blockgrp$acc1_gw/blockgrp$acc1_base-1),blockgrp$pov) # significant negative correlation


# LEP
blockgrp$lep <- as.numeric(as.character(blockgrp$LEP))/100
q_lep <- quantile(blockgrp$lep,seq(0,1,by=0.2),na.rm=T)
cuts_lep <- cut(blockgrp$lep, q_lep)
blockgrp$cuts_lep <- cuts_lep
change_by_lep <- na.omit(blockgrp) %>% group_by(cuts_lep) %>% 
  summarise(dis_bk_16=mean(acc1_base), 
            dis_bk_grw=mean(acc1_gw),
            change=mean(acc1_gw - acc1_base),
            grwth = mean(acc1_gw/acc1_base -1))

cor.test(blockgrp$acc1_base,blockgrp$lep) # not significant
cor.test(blockgrp$acc1_gw,blockgrp$lep) # not significant
cor.test((blockgrp$acc1_gw/blockgrp$acc1_base-1),blockgrp$lep) # not significant 


# elder
blockgrp$elder <- blockgrp$`65_over`/blockgrp$pop15
q_elder <- quantile(blockgrp$elder,seq(0,1,by=0.2),na.rm=T)
cuts_elder <- cut(blockgrp$elder, q_elder)
blockgrp$cuts_elder <- cuts_elder
change_by_elder <- na.omit(blockgrp) %>% group_by(cuts_elder) %>% 
  summarise(dis_bk_16=mean(acc1_base), 
            dis_bk_grw=mean(acc1_gw),
            change=mean(acc1_gw - acc1_base),
            grwth = mean(dis_bk_grw/dis_bk_16-1))

cor.test(blockgrp$acc1_base,blockgrp$elder) # significant positive
cor.test(blockgrp$acc1_gw,blockgrp$elder) # significant positive correlation
cor.test((blockgrp$acc1_gw/blockgrp$acc1_base-1),blockgrp$elder) # not significant 


# youth
blockgrp$youth <- blockgrp$under_18/blockgrp$pop15
q_youth <- quantile(blockgrp$youth,seq(0,1,by=0.2),na.rm=T)
cuts_youth <- cut(blockgrp$youth, q_youth)
blockgrp$cuts_youth <- cuts_youth
change_by_youth <- na.omit(blockgrp) %>% group_by(cuts_youth) %>% 
  summarise(dis_bk_16=mean(acc1_base), 
            dis_bk_grw=mean(acc1_gw),
            change=mean(acc1_gw - acc1_base),
            grwth = mean(acc1_gw/acc1_base-1))

cor.test(blockgrp$acc1_base,blockgrp$youth) # significant positive
cor.test(blockgrp$acc1_gw,blockgrp$youth) # significant positive correlation
cor.test((blockgrp$acc1_gw/blockgrp$acc1_base-1),blockgrp$youth) # not significant 



# Quintile analysis: acc3_emp -------------------------------

# race
change_by_race2 <- na.omit(blockgrp) %>% filter(acc3_emp_gw/acc3_emp_base < 5) %>% group_by(cuts_nonwhite) %>% 
  summarize(emp_Wl_16=mean(acc3_emp_base), 
            emp_Wl_gw=mean(acc3_emp_gw),
            change=mean(acc3_emp_gw - acc3_emp_base),
            grwth = mean(acc3_emp_gw/acc3_emp_base-1))

cor.test(blockgrp$acc3_emp_base,blockgrp$race_nonwhite) # no signficant correlation
cor.test(blockgrp$acc3_emp_gw,blockgrp$race_nonwhite) # no significant correlation
cor.test((blockgrp$acc3_emp_gw/blockgrp$acc3_emp_base -1),blockgrp$race_nonwhite) # significant correlation

# poverty
change_by_pov2 <- na.omit(blockgrp) %>% filter(acc3_emp_gw/acc3_emp_base < 5) %>% group_by(cuts_pov) %>% 
  summarise(emp_Wl_16=mean(acc3_emp_base), 
            emp_Wl_gw=mean(acc3_emp_gw),
            change=mean(acc3_emp_gw - acc3_emp_base),
            grwth = mean(acc3_emp_gw/acc3_emp_base-1))

cor.test(blockgrp$acc3_emp_base,blockgrp$pov) # not significant
cor.test(blockgrp$acc3_emp_gw,blockgrp$pov) # significant positive correlation
cor.test((blockgrp$acc3_emp_gw/blockgrp$acc3_emp_base-1),blockgrp$pov) # significant negative correlation


# LEP
change_by_lep2 <- na.omit(blockgrp) %>% filter(acc3_emp_gw/acc3_emp_base < 5) %>% group_by(cuts_lep) %>% 
  summarise(emp_Wl_16=mean(acc3_emp_base), 
            emp_Wl_gw=mean(acc3_emp_gw),
            change=mean(acc3_emp_gw - acc3_emp_base),
            grwth = mean(acc3_emp_gw/acc3_emp_base -1))

cor.test(blockgrp$acc3_emp_base,blockgrp$lep) # not significant
cor.test(blockgrp$acc3_emp_gw,blockgrp$lep) # not significant
cor.test((blockgrp$acc3_emp_gw/blockgrp$acc3_emp_base-1),blockgrp$lep) # not significant 


# elder
change_by_elder2 <- na.omit(blockgrp) %>% filter(acc3_emp_gw/acc3_emp_base < 5) %>% group_by(cuts_elder) %>% 
  summarise(emp_Wl_16=mean(acc3_emp_base), 
            emp_Wl_gw=mean(acc3_emp_gw),
            change=mean(acc3_emp_gw - acc3_emp_base),
            grwth = mean(acc3_emp_gw/acc3_emp_base-1))

cor.test(blockgrp$acc3_emp_base,blockgrp$elder) # significant positive
cor.test(blockgrp$acc3_emp_gw,blockgrp$elder) # significant positive correlation
cor.test((blockgrp$acc3_emp_gw/blockgrp$acc3_emp_base-1),blockgrp$elder) # not significant 


# youth
change_by_youth2 <- na.omit(blockgrp) %>% filter(acc3_emp_gw/acc3_emp_base < 5) %>% group_by(cuts_youth) %>% 
  summarise(emp_Wl_16=mean(acc3_emp_base), 
            emp_Wl_gw=mean(acc3_emp_gw),
            change=mean(acc3_emp_gw - acc3_emp_base),
            grwth = mean(acc3_emp_gw/acc3_emp_base-1))

cor.test(blockgrp$acc3_emp_base,blockgrp$youth) # significant positive
cor.test(blockgrp$acc3_emp_gw,blockgrp$youth) # significant positive correlation
cor.test((blockgrp$acc3_emp_gw/blockgrp$acc3_emp_base-1),blockgrp$youth) # not significant 


# Quintile analysis: acc3_emp_pct -------------------------------

# race
change_by_race3 <- na.omit(blockgrp) %>% group_by(cuts_nonwhite) %>% 
  summarize(emp_Wl_16=mean(acc3_emp_pct_base), 
            emp_Wl_gw=mean(acc3_emp_pct_gw),
            change=mean(acc3_emp_pct_gw - acc3_emp_pct_base),
            grwth = mean(acc3_emp_pct_gw/acc3_emp_pct_base -1))

cor.test(blockgrp$acc3_emp_pct_base,blockgrp$race_nonwhite) # no signficant correlation
cor.test(blockgrp$acc3_emp_pct_gw,blockgrp$race_nonwhite) # no significant correlation
cor.test((blockgrp$acc3_emp_pct_gw/blockgrp$acc3_emp_pct_base -1),blockgrp$race_nonwhite) # significant correlation

# poverty
change_by_pov3 <- na.omit(blockgrp) %>% group_by(cuts_pov) %>% 
  summarise(emp_Wl_16=mean(acc3_emp_pct_base), 
            emp_Wl_gw=mean(acc3_emp_pct_gw),
            change=mean(acc3_emp_pct_gw - acc3_emp_pct_base),
            grwth = mean(acc3_emp_pct_gw/acc3_emp_pct_base-1))

cor.test(blockgrp$acc3_emp_pct_base,blockgrp$pov) # not significant
cor.test(blockgrp$acc3_emp_pct_gw,blockgrp$pov) # significant positive correlation
cor.test((blockgrp$acc3_emp_pct_gw/blockgrp$acc3_emp_pct_base-1),blockgrp$pov) # significant negative correlation


# LEP
change_by_lep3 <- na.omit(blockgrp) %>% group_by(cuts_lep) %>% 
  summarise(emp_Wl_16=mean(acc3_emp_pct_base), 
            emp_Wl_gw=mean(acc3_emp_pct_gw),
            change=mean(acc3_emp_pct_gw - acc3_emp_pct_base),
            grwth = mean(acc3_emp_pct_gw/acc3_emp_pct_base -1))

cor.test(blockgrp$acc3_emp_pct_base,blockgrp$lep) # not significant
cor.test(blockgrp$acc3_emp_pct_gw,blockgrp$lep) # not significant
cor.test((blockgrp$acc3_emp_pct_gw/blockgrp$acc3_emp_pct_base-1),blockgrp$lep) # not significant 


# elder
change_by_elder3 <- na.omit(blockgrp) %>% group_by(cuts_elder) %>% 
  summarise(emp_Wl_16=mean(acc3_emp_pct_base), 
            emp_Wl_gw=mean(acc3_emp_pct_gw),
            change=mean(acc3_emp_pct_gw - acc3_emp_pct_base),
            grwth = mean(acc3_emp_pct_gw/acc3_emp_pct_base-1))

cor.test(blockgrp$acc3_emp_pct_base,blockgrp$elder) # significant positive
cor.test(blockgrp$acc3_emp_pct_gw,blockgrp$elder) # significant positive correlation
cor.test((blockgrp$acc3_emp_pct_gw/blockgrp$acc3_emp_pct_base-1),blockgrp$elder) # not significant 


# youth
change_by_youth3 <- na.omit(blockgrp) %>% group_by(cuts_youth) %>% 
  summarise(emp_Wl_16=mean(acc3_emp_pct_base), 
            emp_Wl_gw=mean(acc3_emp_pct_gw),
            change=mean(acc3_emp_pct_gw - acc3_emp_pct_base),
            grwth = mean(acc3_emp_pct_gw/acc3_emp_pct_base-1))

cor.test(blockgrp$acc3_emp_pct_base,blockgrp$youth) # significant positive
cor.test(blockgrp$acc3_emp_pct_gw,blockgrp$youth) # significant positive correlation
cor.test((blockgrp$acc3_emp_pct_gw/blockgrp$acc3_emp_pct_base-1),blockgrp$youth) # not significant 


# Graph -------------------------------------------------------------------
# https://rpubs.com/MarkusLoew/226759

library(ggplot2)
library(reshape)
library(gridExtra)

race_m <- melt(data.frame(change_by_race[,c(1:3,5)]))
p1 <- ggplot(race_m,aes(x=cuts_nonwhite))+
  geom_bar(data=subset(race_m,variable!=c("grwth")),aes(y=value,fill=variable),stat = "identity",position="dodge") +
  scale_fill_manual(values=c("yellow3", "green3"),labels=c("Existing Bike Facilities in 2016", "Proposed Greenways in 2035")) +
  geom_point(data=subset(race_m,variable==c("grwth")),aes(y=value*100,col="Change in Percentage")) +
  scale_y_continuous(name="Distance-based UGM Score[0-100]",
                     sec.axis = sec_axis(~.*1, name = "UGM Improvement [%]")) +
  scale_x_discrete(name="Quintiles",labels=c("0-20%","20-40%","40-60%","60-80%","80-100%")) +
  theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),legend.position = "none") +
  ggtitle("Percentage of Non-white Population")

pov_m <- melt(data.frame(change_by_pov[,c(1:3,5)]))
p2 <- ggplot(pov_m,aes(x=cuts_pov))+
  geom_bar(data=subset(pov_m,variable!=c("grwth")),aes(y=value,fill=variable),stat = "identity",position="dodge") +
  scale_fill_manual(values=c("yellow3", "green3"),labels=c("Existing Bike Facilities in 2010", "Proposed Greenways in 2035")) +
  geom_point(data=subset(pov_m,variable==c("grwth")),aes(y=value*100,col="Change in Percentage")) +
  scale_y_continuous(name="Distance-based UGM Score[0-100]",
                     sec.axis = sec_axis(~.*1, name = "UGM Improvement [%]")) +
  scale_x_discrete(name="Quintiles",labels=c("0-20%","20-40%","40-60%","60-80%","80-100%")) +
  theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),legend.position = "none") +
  ggtitle("Percentage of Low-Income Households")

lep_m <- melt(data.frame(change_by_lep[,c(1:3,5)]))
p3 <- ggplot(lep_m,aes(x=cuts_lep))+
  geom_bar(data=subset(lep_m,variable!=c("grwth")),aes(y=value,fill=variable),stat = "identity",position="dodge") +
  scale_fill_manual(values=c("yellow3", "green3"),labels=c("Existing Bike Facilities in 2010", "Proposed Greenways in 2035")) +
  geom_point(data=subset(lep_m,variable==c("grwth")),aes(y=value*100,col="Change in Percentage")) +
  scale_y_continuous(name="Distance-based UGM Score[0-100]",
                     sec.axis = sec_axis(~.*1, name = "UGM Improvement [%]")) +
  scale_x_discrete(name="Quintiles",labels=c("0-20%","20-40%","40-60%","60-80%","80-100%")) +
  theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),legend.position = "none") +
  ggtitle("Percentage of Limited English Proficiency Population")

elder_m <- melt(data.frame(change_by_elder[,c(1:3,5)]))
p4 <- ggplot(elder_m,aes(x=cuts_elder))+
  geom_bar(data=subset(elder_m,variable!=c("grwth")),aes(y=value,fill=variable),stat = "identity",position="dodge") +
  scale_fill_manual(values=c("yellow3", "green3"),labels=c("Existing Bike Facilities in 2010", "Proposed Greenways in 2035")) +
  geom_point(data=subset(elder_m,variable==c("grwth")),aes(y=value*100,col="Change in Percentage")) +
  scale_y_continuous(name="Distance-based UGM Score[0-100]",
                     sec.axis = sec_axis(~.*1, name = "UGM Improvement [%]")) +
  scale_x_discrete(name="Quintiles",labels=c("0-20%","20-40%","40-60%","60-80%","80-100%")) +
  theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),legend.position = "none") +
  ggtitle("Percentage of Older Adults")

youth_m <- melt(data.frame(change_by_youth[,c(1:3,5)]))
p5 <- ggplot(youth_m,aes(x=cuts_youth))+
  geom_bar(data=subset(youth_m,variable!=c("grwth")),aes(y=value,fill=variable),stat = "identity",position="dodge") +
  scale_fill_manual(values=c("yellow3", "green3"),labels=c("Existing Bike Facilities in 2016", "Proposed Greenways in 2035")) +
  geom_point(data=subset(youth_m,variable==c("grwth")),aes(y=value*100,col="Change in Percentage")) +
  scale_y_continuous(name="Distance-based UGM Score[0-100]",
                     sec.axis = sec_axis(~.*1, name = "UGM Improvement [%]")) +
  scale_x_discrete(name="Quintiles",labels=c("0-20%","20-40%","40-60%","60-80%","80-100%")) +
  theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),legend.position = "none") +
  ggtitle("Percentage of Young Persons")

grid.arrange(p1, p2, p3, p4,p5,  ncol = 2)


# Graph 2------------------------------

race_m <- melt(data.frame(change_by_race2[,c(1:3,5)]))
p11 <-ggplot(race_m,aes(x=cuts_nonwhite))+
  geom_bar(data=subset(race_m,variable!=c("grwth")),aes(y=value,fill=variable),stat = "identity",position="dodge") +
  scale_fill_manual(values=c("yellow3", "green3"),labels=c("Existing Bike Facilities in 2016", "Proposed Greenways in 2035")) +
  geom_point(data=subset(race_m,variable==c("grwth")),aes(y=value*100,col="Change in Percentage")) +
  geom_line(data=subset(race_m,variable==c("grwth")),aes(y=value*100,col="Change in Percentage")) +
  scale_y_continuous(name="Low-Stress Proximity UGM Score",
                     sec.axis = sec_axis(~.*1, name = "UGM Improvement [%]")) +
  scale_x_discrete(name="Quintiles",labels=c("0-20%","20-40%","40-60%","60-80%","80-100%")) +
  theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),legend.position = "none") +
  ggtitle("Percentage of Non-white Population")

pov_m <- melt(data.frame(change_by_pov2[,c(1:3,5)]))
p22 <- ggplot(pov_m,aes(x=cuts_pov))+
  geom_bar(data=subset(pov_m,variable!=c("grwth")),aes(y=value,fill=variable),stat = "identity",position="dodge") +
  scale_fill_manual(values=c("yellow3", "green3"),labels=c("Existing Bike Facilities in 2010", "Proposed Greenways in 2035")) +
  geom_point(data=subset(pov_m,variable==c("grwth")),aes(y=value*100,col="Change in Percentage")) +
  scale_y_continuous(name="Low-Stress Proximity UGM Score",
                     sec.axis = sec_axis(~.*1, name = "UGM Improvement [%]")) +
  scale_x_discrete(name="Quintiles",labels=c("0-20%","20-40%","40-60%","60-80%","80-100%")) +
  theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),legend.position = "none") +
  ggtitle("Percentage of Low-Income Households")

lep_m <- melt(data.frame(change_by_lep2[,c(1:3,5)]))
p33 <- ggplot(lep_m,aes(x=cuts_lep))+
  geom_bar(data=subset(lep_m,variable!=c("grwth")),aes(y=value,fill=variable),stat = "identity",position="dodge") +
  scale_fill_manual(values=c("yellow3", "green3"),labels=c("Existing Bike Facilities in 2010", "Proposed Greenways in 2035")) +
  geom_point(data=subset(lep_m,variable==c("grwth")),aes(y=value*100,col="Change in Percentage")) +
  scale_y_continuous(name="Low-Stress Proximity UGM Score",
                     sec.axis = sec_axis(~.*1, name = "UGM Improvement [%]")) +
  scale_x_discrete(name="Quintiles",labels=c("0-20%","20-40%","40-60%","60-80%","80-100%")) +
  theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),legend.position = "none") +
  ggtitle("Percentage of Limited English Proficiency Population")

elder_m <- melt(data.frame(change_by_elder2[,c(1:3,5)]))
p44 <- ggplot(elder_m,aes(x=cuts_elder))+
  geom_bar(data=subset(elder_m,variable!=c("grwth")),aes(y=value,fill=variable),stat = "identity",position="dodge") +
  scale_fill_manual(values=c("yellow3", "green3"),labels=c("Existing Bike Facilities in 2010", "Proposed Greenways in 2035")) +
  geom_point(data=subset(elder_m,variable==c("grwth")),aes(y=value*100,col="Change in Percentage")) +
  scale_y_continuous(name="Low-Stress Proximity UGM Score",
                     sec.axis = sec_axis(~.*1, name = "UGM Improvement [%]")) +
  scale_x_discrete(name="Quintiles",labels=c("0-20%","20-40%","40-60%","60-80%","80-100%")) +
  theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),legend.position = "none") +
  ggtitle("Percentage of Older Adults")

youth_m <- melt(data.frame(change_by_youth2[,c(1:3,5)]))
p55 <- ggplot(youth_m,aes(x=cuts_youth))+
  geom_bar(data=subset(youth_m,variable!=c("grwth")),aes(y=value,fill=variable),stat = "identity",position="dodge") +
  scale_fill_manual(values=c("yellow3", "green3"),labels=c("Existing Bike Facilities in 2016", "Proposed Greenways in 2035")) +
  geom_point(data=subset(youth_m,variable==c("grwth")),aes(y=value*100,col="Change in Percentage")) +
  scale_y_continuous(name="Low-Stress Proximity UGM Score",
                     sec.axis = sec_axis(~.*1, name = "UGM Improvement [%]")) +
  scale_x_discrete(name="Quintiles",labels=c("0-20%","20-40%","40-60%","60-80%","80-100%")) +
  theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),legend.position = "none") +
  ggtitle("Percentage of Young Persons")

grid.arrange(p11, p22, p33, p44,p55,  ncol = 2)

# Graph 3------------------------------

race_m <- melt(data.frame(change_by_race3[,c(1:3,5)]))
p111 <- ggplot(race_m,aes(x=cuts_nonwhite))+
  geom_bar(data=subset(race_m,variable!=c("grwth")),aes(y=value,fill=variable),stat = "identity",position="dodge") +
  scale_fill_manual(values=c("yellow3", "green3"),labels=c("Existing Bike Facilities in 2016", "Proposed Greenways in 2035")) +
  geom_point(data=subset(race_m,variable==c("grwth")),aes(y=value*100,col="Change in Percentage")) +
  scale_y_continuous(name="Low-Stress Stress-Level UGM Score",
                     sec.axis = sec_axis(~.*1, name = "UGM Improvement [%]")) +
  scale_x_discrete(name="Quintile",labels=c("0-20%","20-40%","40-60%","60-80%","80-100%")) +
  theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),legend.position = "none") +
  ggtitle("Percentage of Non-white Population")

pov_m <- melt(data.frame(change_by_pov3[,c(1:3,5)]))
p222 <- ggplot(pov_m,aes(x=cuts_pov))+
  geom_bar(data=subset(pov_m,variable!=c("grwth")),aes(y=value,fill=variable),stat = "identity",position="dodge") +
  scale_fill_manual(values=c("yellow3", "green3"),labels=c("Existing Bike Facilities in 2010", "Proposed Greenways in 2035")) +
  geom_point(data=subset(pov_m,variable==c("grwth")),aes(y=value*100,col="Change in Percentage")) +
  scale_y_continuous(name="Low-Stress Stress-Level UGM Score",
                     sec.axis = sec_axis(~.*1, name = "UGM Improvement [%]")) +
  scale_x_discrete(name="Quintile",labels=c("0-20%","20-40%","40-60%","60-80%","80-100%")) +
  theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),legend.position = "none") +
  ggtitle("Percentage of Low-Income Households")

lep_m <- melt(data.frame(change_by_lep3[,c(1:3,5)]))
p333 <- ggplot(lep_m,aes(x=cuts_lep))+
  geom_bar(data=subset(lep_m,variable!=c("grwth")),aes(y=value,fill=variable),stat = "identity",position="dodge") +
  scale_fill_manual(values=c("yellow3", "green3"),labels=c("Existing Bike Facilities in 2010", "Proposed Greenways in 2035")) +
  geom_point(data=subset(lep_m,variable==c("grwth")),aes(y=value*100,col="Change in Percentage")) +
  scale_y_continuous(name="Low-Stress Stress-Level UGM Score",
                     sec.axis = sec_axis(~.*1, name = "UGM Improvement [%]")) +
  scale_x_discrete(name="Quintile",labels=c("0-20%","20-40%","40-60%","60-80%","80-100%")) +
  theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),legend.position = "none") +
  ggtitle("Percentage of Limited English Proficiency Population")

elder_m <- melt(data.frame(change_by_elder3[,c(1:3,5)]))
p444 <- ggplot(elder_m,aes(x=cuts_elder))+
  geom_bar(data=subset(elder_m,variable!=c("grwth")),aes(y=value,fill=variable),stat = "identity",position="dodge") +
  scale_fill_manual(values=c("yellow3", "green3"),labels=c("Existing Bike Facilities in 2010", "Proposed Greenways in 2035")) +
  geom_point(data=subset(elder_m,variable==c("grwth")),aes(y=value*100,col="Change in Percentage")) +
  scale_y_continuous(name="Low-Stress Stress-Level UGM Score",
                     sec.axis = sec_axis(~.*1, name = "UGM Improvement [%]")) +
  scale_x_discrete(name="Quintile",labels=c("0-20%","20-40%","40-60%","60-80%","80-100%")) +
  theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),legend.position = "none") +
  ggtitle("Percentage of Older Adults")

youth_m <- melt(data.frame(change_by_youth3[,c(1:3,5)]))
p555 <- ggplot(youth_m,aes(x=cuts_youth))+
  geom_bar(data=subset(youth_m,variable!=c("grwth")),aes(y=value,fill=variable),stat = "identity",position="dodge") +
  scale_fill_manual(values=c("yellow3", "green3"),labels=c("Existing Bike Facilities in 2016", "Proposed Greenways in 2035")) +
  geom_point(data=subset(youth_m,variable==c("grwth")),aes(y=value*100,col="Change in Percentage")) +
  scale_y_continuous(name="Low-Stress Stress-Level UGM Score",
                     sec.axis = sec_axis(~.*1, name = "UGM Improvement [%]")) +
  scale_x_discrete(name="Quintile",labels=c("0-20%","20-40%","40-60%","60-80%","80-100%")) +
  theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),legend.position = "none") +
  ggtitle("Percentage of Young Persons")

grid.arrange(p111, p222, p333, p444,p555,  ncol = 2)

# Historically Marginalized Community -------------------------------------
blockgrp$TierI1 <- ifelse(blockgrp$race_nonwhite<0.224,0,1)
blockgrp$TierI2 <- ifelse(blockgrp$pov<0.356,0,1)
blockgrp$TierI3 <- ifelse(blockgrp$lep>0.09,1,0)
blockgrp$TierI4 <- ifelse(blockgrp$elder<0.115,0,1)
blockgrp$TierI5 <- ifelse(blockgrp$youth<0.187,0,1)
blockgrp$TierI <- ifelse(blockgrp$TierI1+blockgrp$TierI2+blockgrp$TierI3+blockgrp$TierI4+blockgrp$TierI5>0,1,0)

blockgrp$TierII1 <- ifelse(blockgrp$pop_den>8294&blockgrp$race_nonwhite>0.224,1,0)
blockgrp$TierII2 <- ifelse(blockgrp$pop_den>8294&blockgrp$pov>0.356,1,0)
blockgrp$TierII3 <- ifelse(blockgrp$pop_den>8294&blockgrp$lep>0.09,1,0)
blockgrp$TierII <- ifelse(blockgrp$TierII1+blockgrp$TierII2+blockgrp$TierII3>0,1,0)

# acc1
t.test(blockgrp[blockgrp$TierI1==0,]$acc1_base, blockgrp[blockgrp$TierI1==1,]$acc1_base)
t.test(blockgrp[blockgrp$TierI1==0,]$acc1_gw, blockgrp[blockgrp$TierI1==1,]$acc1_gw)
t.test(blockgrp[blockgrp$TierI1==1,]$acc1_base, blockgrp[blockgrp$TierI1==1,]$acc1_gw)

t.test(blockgrp[blockgrp$TierI2==0,]$acc1_base, blockgrp[blockgrp$TierI2==1,]$acc1_base)
t.test(blockgrp[blockgrp$TierI2==0,]$acc1_gw, blockgrp[blockgrp$TierI2==1,]$acc1_gw)
t.test(blockgrp[blockgrp$TierI2==1,]$acc1_base, blockgrp[blockgrp$TierI2==1,]$acc1_gw)

t.test(blockgrp[blockgrp$TierI3==0,]$acc1_base, blockgrp[blockgrp$TierI3==1,]$acc1_base)
t.test(blockgrp[blockgrp$TierI3==0,]$acc1_gw, blockgrp[blockgrp$TierI3==1,]$acc1_gw)
t.test(blockgrp[blockgrp$TierI3==1,]$acc1_base, blockgrp[blockgrp$TierI3==1,]$acc1_gw)

t.test(blockgrp[blockgrp$TierI4==0,]$acc1_base, blockgrp[blockgrp$TierI4==1,]$acc1_base)
t.test(blockgrp[blockgrp$TierI4==0,]$acc1_gw, blockgrp[blockgrp$TierI4==1,]$acc1_gw)
t.test(blockgrp[blockgrp$TierI4==1,]$acc1_base, blockgrp[blockgrp$TierI4==1,]$acc1_gw)

t.test(blockgrp[blockgrp$TierI5==0,]$acc1_base, blockgrp[blockgrp$TierI5==1,]$acc1_base)
t.test(blockgrp[blockgrp$TierI5==0,]$acc1_gw, blockgrp[blockgrp$TierI5==1,]$acc1_gw)
t.test(blockgrp[blockgrp$TierI5==1,]$acc1_base, blockgrp[blockgrp$TierI5==1,]$acc1_gw)

t.test(blockgrp[blockgrp$TierI==0,]$acc1_base, blockgrp[blockgrp$TierI==1,]$acc1_base)
t.test(blockgrp[blockgrp$TierI==0,]$acc1_gw, blockgrp[blockgrp$TierI==1,]$acc1_gw)
t.test(blockgrp[blockgrp$TierI==1,]$acc1_base, blockgrp[blockgrp$TierI==1,]$acc1_gw)

t.test(blockgrp[blockgrp$TierII1==0,]$acc1_base, blockgrp[blockgrp$TierII1==1,]$acc1_base)
t.test(blockgrp[blockgrp$TierII1==0,]$acc1_gw, blockgrp[blockgrp$TierII1==1,]$acc1_gw)
t.test(blockgrp[blockgrp$TierII1==1,]$acc1_base, blockgrp[blockgrp$TierII1==1,]$acc1_gw)

t.test(blockgrp[blockgrp$TierII2==0,]$acc1_base, blockgrp[blockgrp$TierII2==1,]$acc1_base)
t.test(blockgrp[blockgrp$TierII2==0,]$acc1_gw, blockgrp[blockgrp$TierII2==1,]$acc1_gw)
t.test(blockgrp[blockgrp$TierII2==1,]$acc1_base, blockgrp[blockgrp$TierII2==1,]$acc1_gw)

t.test(blockgrp[blockgrp$TierII3==0,]$acc1_base, blockgrp[blockgrp$TierII3==1,]$acc1_base)
t.test(blockgrp[blockgrp$TierII3==0,]$acc1_gw, blockgrp[blockgrp$TierII3==1,]$acc1_gw)
t.test(blockgrp[blockgrp$TierII3==1,]$acc1_base, blockgrp[blockgrp$TierII3==1,]$acc1_gw)

t.test(blockgrp[blockgrp$TierII==0,]$acc1_base, blockgrp[blockgrp$TierII==1,]$acc1_base)
t.test(blockgrp[blockgrp$TierII==0,]$acc1_gw, blockgrp[blockgrp$TierII==1,]$acc1_gw)
t.test(blockgrp[blockgrp$TierII==1,]$acc1_base, blockgrp[blockgrp$TierII==1,]$acc1_gw)

# acc3_emp
t.test(blockgrp[blockgrp$TierI1==0,]$acc3_emp_base, blockgrp[blockgrp$TierI1==1,]$acc3_emp_base)
t.test(blockgrp[blockgrp$TierI1==0,]$acc3_emp_gw, blockgrp[blockgrp$TierI1==1,]$acc3_emp_gw)
t.test(blockgrp[blockgrp$TierI1==1,]$acc3_emp_base, blockgrp[blockgrp$TierI1==1,]$acc3_emp_gw)

t.test(blockgrp[blockgrp$TierI2==0,]$acc3_emp_base, blockgrp[blockgrp$TierI2==1,]$acc3_emp_base)
t.test(blockgrp[blockgrp$TierI2==0,]$acc3_emp_gw, blockgrp[blockgrp$TierI2==1,]$acc3_emp_gw)
t.test(blockgrp[blockgrp$TierI2==1,]$acc3_emp_base, blockgrp[blockgrp$TierI2==1,]$acc3_emp_gw)

t.test(blockgrp[blockgrp$TierI3==0,]$acc3_emp_base, blockgrp[blockgrp$TierI3==1,]$acc3_emp_base)
t.test(blockgrp[blockgrp$TierI3==0,]$acc3_emp_gw, blockgrp[blockgrp$TierI3==1,]$acc3_emp_gw)
t.test(blockgrp[blockgrp$TierI3==1,]$acc3_emp_base, blockgrp[blockgrp$TierI3==1,]$acc3_emp_gw)

t.test(blockgrp[blockgrp$TierI4==0,]$acc3_emp_base, blockgrp[blockgrp$TierI4==1,]$acc3_emp_base)
t.test(blockgrp[blockgrp$TierI4==0,]$acc3_emp_gw, blockgrp[blockgrp$TierI4==1,]$acc3_emp_gw)
t.test(blockgrp[blockgrp$TierI4==1,]$acc3_emp_base, blockgrp[blockgrp$TierI4==1,]$acc3_emp_gw)

t.test(blockgrp[blockgrp$TierI5==0,]$acc3_emp_base, blockgrp[blockgrp$TierI5==1,]$acc3_emp_base)
t.test(blockgrp[blockgrp$TierI5==0,]$acc3_emp_gw, blockgrp[blockgrp$TierI5==1,]$acc3_emp_gw)
t.test(blockgrp[blockgrp$TierI5==1,]$acc3_emp_base, blockgrp[blockgrp$TierI5==1,]$acc3_emp_gw)

t.test(blockgrp[blockgrp$TierI==0,]$acc3_emp_base, blockgrp[blockgrp$TierI==1,]$acc3_emp_base)
t.test(blockgrp[blockgrp$TierI==0,]$acc3_emp_gw, blockgrp[blockgrp$TierI==1,]$acc3_emp_gw)
t.test(blockgrp[blockgrp$TierI==1,]$acc3_emp_base, blockgrp[blockgrp$TierI==1,]$acc3_emp_gw)

t.test(blockgrp[blockgrp$TierII1==0,]$acc3_emp_base, blockgrp[blockgrp$TierII1==1,]$acc3_emp_base)
t.test(blockgrp[blockgrp$TierII1==0,]$acc3_emp_gw, blockgrp[blockgrp$TierII1==1,]$acc3_emp_gw)
t.test(blockgrp[blockgrp$TierII1==1,]$acc3_emp_base, blockgrp[blockgrp$TierII1==1,]$acc3_emp_gw)

t.test(blockgrp[blockgrp$TierII2==0,]$acc3_emp_base, blockgrp[blockgrp$TierII2==1,]$acc3_emp_base)
t.test(blockgrp[blockgrp$TierII2==0,]$acc3_emp_gw, blockgrp[blockgrp$TierII2==1,]$acc3_emp_gw)
t.test(blockgrp[blockgrp$TierII2==1,]$acc3_emp_base, blockgrp[blockgrp$TierII2==1,]$acc3_emp_gw)

t.test(blockgrp[blockgrp$TierII3==0,]$acc3_emp_base, blockgrp[blockgrp$TierII3==1,]$acc3_emp_base)
t.test(blockgrp[blockgrp$TierII3==0,]$acc3_emp_gw, blockgrp[blockgrp$TierII3==1,]$acc3_emp_gw)
t.test(blockgrp[blockgrp$TierII3==1,]$acc3_emp_base, blockgrp[blockgrp$TierII3==1,]$acc3_emp_gw)

t.test(blockgrp[blockgrp$TierII==0,]$acc3_emp_base, blockgrp[blockgrp$TierII==1,]$acc3_emp_base)
t.test(blockgrp[blockgrp$TierII==0,]$acc3_emp_gw, blockgrp[blockgrp$TierII==1,]$acc3_emp_gw)
t.test(blockgrp[blockgrp$TierII==1,]$acc3_emp_base, blockgrp[blockgrp$TierII==1,]$acc3_emp_gw)


# acc3_emp_pct
t.test(blockgrp[blockgrp$TierI1==0,]$acc3_emp_pct_base, blockgrp[blockgrp$TierI1==1,]$acc3_emp_pct_base)
t.test(blockgrp[blockgrp$TierI1==0,]$acc3_emp_pct_gw, blockgrp[blockgrp$TierI1==1,]$acc3_emp_pct_gw)
t.test(blockgrp[blockgrp$TierI1==1,]$acc3_emp_pct_base, blockgrp[blockgrp$TierI1==1,]$acc3_emp_pct_gw)

t.test(blockgrp[blockgrp$TierI2==0,]$acc3_emp_pct_base, blockgrp[blockgrp$TierI2==1,]$acc3_emp_pct_base)
t.test(blockgrp[blockgrp$TierI2==0,]$acc3_emp_pct_gw, blockgrp[blockgrp$TierI2==1,]$acc3_emp_pct_gw)
t.test(blockgrp[blockgrp$TierI2==1,]$acc3_emp_pct_base, blockgrp[blockgrp$TierI2==1,]$acc3_emp_pct_gw)

t.test(blockgrp[blockgrp$TierI3==0,]$acc3_emp_pct_base, blockgrp[blockgrp$TierI3==1,]$acc3_emp_pct_base)
t.test(blockgrp[blockgrp$TierI3==0,]$acc3_emp_pct_gw, blockgrp[blockgrp$TierI3==1,]$acc3_emp_pct_gw)
t.test(blockgrp[blockgrp$TierI3==1,]$acc3_emp_pct_base, blockgrp[blockgrp$TierI3==1,]$acc3_emp_pct_gw)

t.test(blockgrp[blockgrp$TierI4==0,]$acc3_emp_pct_base, blockgrp[blockgrp$TierI4==1,]$acc3_emp_pct_base)
t.test(blockgrp[blockgrp$TierI4==0,]$acc3_emp_pct_gw, blockgrp[blockgrp$TierI4==1,]$acc3_emp_pct_gw)
t.test(blockgrp[blockgrp$TierI4==1,]$acc3_emp_pct_base, blockgrp[blockgrp$TierI4==1,]$acc3_emp_pct_gw)

t.test(blockgrp[blockgrp$TierI5==0,]$acc3_emp_pct_base, blockgrp[blockgrp$TierI5==1,]$acc3_emp_pct_base)
t.test(blockgrp[blockgrp$TierI5==0,]$acc3_emp_pct_gw, blockgrp[blockgrp$TierI5==1,]$acc3_emp_pct_gw)
t.test(blockgrp[blockgrp$TierI5==1,]$acc3_emp_pct_base, blockgrp[blockgrp$TierI5==1,]$acc3_emp_pct_gw)

t.test(blockgrp[blockgrp$TierI==0,]$acc3_emp_pct_base, blockgrp[blockgrp$TierI==1,]$acc3_emp_pct_base)
t.test(blockgrp[blockgrp$TierI==0,]$acc3_emp_pct_gw, blockgrp[blockgrp$TierI==1,]$acc3_emp_pct_gw)
t.test(blockgrp[blockgrp$TierI==1,]$acc3_emp_pct_base, blockgrp[blockgrp$TierI==1,]$acc3_emp_pct_gw)

t.test(blockgrp[blockgrp$TierII1==0,]$acc3_emp_pct_base, blockgrp[blockgrp$TierII1==1,]$acc3_emp_pct_base)
t.test(blockgrp[blockgrp$TierII1==0,]$acc3_emp_pct_gw, blockgrp[blockgrp$TierII1==1,]$acc3_emp_pct_gw)
t.test(blockgrp[blockgrp$TierII1==1,]$acc3_emp_pct_base, blockgrp[blockgrp$TierII1==1,]$acc3_emp_pct_gw)

t.test(blockgrp[blockgrp$TierII2==0,]$acc3_emp_pct_base, blockgrp[blockgrp$TierII2==1,]$acc3_emp_pct_base)
t.test(blockgrp[blockgrp$TierII2==0,]$acc3_emp_pct_gw, blockgrp[blockgrp$TierII2==1,]$acc3_emp_pct_gw)
t.test(blockgrp[blockgrp$TierII2==1,]$acc3_emp_pct_base, blockgrp[blockgrp$TierII2==1,]$acc3_emp_pct_gw)

t.test(blockgrp[blockgrp$TierII3==0,]$acc3_emp_pct_base, blockgrp[blockgrp$TierII3==1,]$acc3_emp_pct_base)
t.test(blockgrp[blockgrp$TierII3==0,]$acc3_emp_pct_gw, blockgrp[blockgrp$TierII3==1,]$acc3_emp_pct_gw)
t.test(blockgrp[blockgrp$TierII3==1,]$acc3_emp_pct_base, blockgrp[blockgrp$TierII3==1,]$acc3_emp_pct_gw)

t.test(blockgrp[blockgrp$TierII==0,]$acc3_emp_pct_base, blockgrp[blockgrp$TierII==1,]$acc3_emp_pct_base)
t.test(blockgrp[blockgrp$TierII==0,]$acc3_emp_pct_gw, blockgrp[blockgrp$TierII==1,]$acc3_emp_pct_gw)
t.test(blockgrp[blockgrp$TierII==1,]$acc3_emp_pct_base, blockgrp[blockgrp$TierII==1,]$acc3_emp_pct_gw)


