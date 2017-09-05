library(foreign)
block <- read.dbf("C:/Users/shiwei/PDX Google drive/NITC Greenway/Data/Portland_blocks.dbf")
colnames(block)[23] <- "dis_bk_16"
block <- block[block$dis_bk_16<99999 & block$dis_bk_grw<99999,]

library(ggplot2)
library(reshape)
block_box <- block[block$dis_bk_16<10560,]

boxplot(block_box[,c("dis_bk_16","dis_bk_grw")])

ggplot(melt(block_box[,c("dis_bk_16","dis_bk_grw")]), aes(x=variable, y=value)) + 
  geom_boxplot() + 
  theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),legend.position = "bottom") +
  ggtitle("Boxplot of Bike Accessibility Comparison") +
  labs(x="Bike Facility Network Scenarios", y="Distance to Nearest Bike Facilities")

hist(block_box$dis_bk_16,col=rgb(0,0,1,1/4))
hist(block_box$dis_bk_grw,col=rgb(1,0,0,1/4), add=T)

ggplot(melt(block_box[,c("dis_bk_16","dis_bk_grw")]), aes(value, fill = variable),binwidth = 50) + 
  geom_histogram(position = "dodge") +
  theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),legend.position = "bottom") +
  ggtitle("Histogram of Bike Accessibility Comparison") +
  labs(x="Distance to Nearest Bike Facilities", y="Number of Blocks") +
  scale_fill_manual(values=c("yellow3","green3"),labels=c("Existing Bike Facilities in 2016", "Proposed Greenways in 2035"))

# RLIS data ---------------------------------------------------------------

blockgrp <- read.dbf("C:/Users/shiwei/PDX Google drive/NITC Greenway/Data/PDX_blockgrp.dbf")

block$TRBG <- substring(block$GEOID10,6,12)

dis_bk_16 <- aggregate(dis_bk_16~TRBG,block, FUN=mean)
dis_bk_grw <- aggregate(dis_bk_grw~TRBG,block, FUN=mean)
blockgrp <- merge(blockgrp,dis_bk_16)
blockgrp <- merge(blockgrp,dis_bk_grw)

blockgrp$white <- blockgrp$WHITE/blockgrp$POP10
q <- quantile(blockgrp$white,seq(0,1,by=0.2))
cuts = cut(blockgrp$white, q)
blockgrp$whitecuts <- cuts
library(dplyr)
change_by_race <- na.omit(blockgrp) %>% group_by(whitecuts) %>% 
  summarize(dis_bk_16=mean(dis_bk_16), 
            dis_bk_grw=mean(dis_bk_grw),
            grwth = 1-dis_bk_grw/dis_bk_16)

summary(aov(dis_bk_16~whitecuts, data=blockgrp))
summary(aov(dis_bk_grw~whitecuts, data=blockgrp))
cor(blockgrp$dis_bk_16,blockgrp$white)
cor(blockgrp$dis_bk_grw,blockgrp$white)

summary(block$dis_bk_16 - block$dis_bk_grw)
summary(block$dis_bk_16)
summary(block$dis_bk_grw)


# ACS data ----------------------------------------------------------------

ACS <- read.csv("C:/Users/shiwei/PDX Google drive/NITC Greenway/Data/ACS original.csv")
ACS <- ACS[,-c(2:55,58:60,68:75,84:89,109:126)]
colnames(ACS) <- c("FIPS","pop15","area","white","black","indian","asian","hawaiian","other","two more",
                   "pop_over25","edu_less_hischl","edu_hischl","edu_college","edu_bach","edu_master","edu_prf","edu_phd",
                   "hh","inc_0_10","inc_10_15","inc_15_20","inc_20_25","inc_25_30","inc_30_35","inc_35_40","inc_40_45","inc_45_50",
                   "inc_50_60","inc_60_75","inc_75_100","inc_100_125","inc_125_150","inc_150_200","inc_200+","medinc","avg_inc",
                   "pop_pov_det","doing_poor","struggling","poor_struggling","doing_ok")
ACS <- ACS[-c(521),]

blockgrp <- merge(blockgrp,ACS,by=c("FIPS"))

# race
blockgrp$race_nonwhite <- 1- blockgrp$white/blockgrp$pop15
q_nonwhite <- quantile(blockgrp$race_nonwhite,seq(0,1,by=0.2))
cuts_nonwhite = cut(blockgrp$race_nonwhite, q_nonwhite)
blockgrp$cuts_nonwhite <- cuts_nonwhite
library(dplyr)
change_by_race <- na.omit(blockgrp) %>% group_by(cuts_nonwhite) %>% 
  summarize(dis_bk_16=mean(dis_bk_16), 
            dis_bk_grw=mean(dis_bk_grw),
           change=mean(dis_bk_16 - dis_bk_grw),
            grwth = mean(1-dis_bk_grw/dis_bk_16))

summary(aov(dis_bk_16~cuts_nonwhite, data=blockgrp)) # no differences
summary(aov(dis_bk_grw~cuts_nonwhite, data=blockgrp)) # no differences
cor.test(blockgrp$dis_bk_16,blockgrp$race_nonwhite) # no signficant correlation
cor.test(blockgrp$dis_bk_grw,blockgrp$race_nonwhite) # no significant correlation

summary(aov((dis_bk_16-dis_bk_grw)~cuts_nonwhite, data=blockgrp)) # absolute change of distance to closest facility are different for at least one pair of -tile comparison
summary(aov((1-dis_bk_grw/dis_bk_16)~cuts_nonwhite, data=blockgrp))# percentage change of distance to closest facility are different for at least one pair of -tile comparison
cor.test((blockgrp$dis_bk_16-blockgrp$dis_bk_grw),blockgrp$race_nonwhite) # significant correlation between facility improvement and race
cor.test((1-blockgrp$dis_bk_grw/blockgrp$dis_bk_16),blockgrp$race_nonwhite) # significant correlation


# median income
q_medinc <- quantile(blockgrp$med_inc,seq(0,1,by=0.2),na.rm=T)
cuts_medinc = cut(blockgrp$med_inc, q_medinc)
blockgrp$cuts_medinc <- cuts_medinc
change_by_medinc <- na.omit(blockgrp) %>% group_by(cuts_medinc) %>% 
  summarise(dis_bk_16=mean(dis_bk_16), 
            dis_bk_grw=mean(dis_bk_grw),
            change=mean(dis_bk_16 - dis_bk_grw),
            grwth = mean(1-dis_bk_grw/dis_bk_16))

summary(aov(dis_bk_16~cuts_medinc, data=blockgrp)) # differences among different income groups
summary(aov(dis_bk_grw~cuts_medinc, data=blockgrp)) # significant differences among different income groups
cor.test(blockgrp$dis_bk_16,blockgrp$med_inc) # signficant positive correlation
cor.test(blockgrp$dis_bk_grw,blockgrp$med_inc) # significant positive correlation

summary(aov((dis_bk_16-dis_bk_grw)~cuts_medinc, data=blockgrp)) # absolute change of distance to closest facility are different for at least one pair of -tile comparison
summary(aov((1-dis_bk_grw/dis_bk_16)~cuts_medinc, data=blockgrp))# percentage change of distance to closest facility are different for at least one pair of -tile comparison
cor.test((blockgrp$dis_bk_16-blockgrp$dis_bk_grw),blockgrp$med_inc) # significant negative correlation between facility improvement and income
cor.test((1-blockgrp$dis_bk_grw/blockgrp$dis_bk_16),blockgrp$med_inc) # significant negative correlation

# avg income
q_avginc <- quantile(blockgrp$avg_inc,seq(0,1,by=0.2),na.rm=T)
cuts_avginc = cut(blockgrp$avg_inc, q_avginc)
blockgrp$cuts_avginc <- cuts_avginc
change_by_avginc <- na.omit(blockgrp) %>% group_by(cuts_avginc) %>% 
  summarise(dis_bk_16=mean(dis_bk_16), 
            dis_bk_grw=mean(dis_bk_grw),
            change=mean(dis_bk_16 - dis_bk_grw),
            grwth = mean(1-dis_bk_grw/dis_bk_16))

summary(aov(dis_bk_16~cuts_avginc, data=blockgrp)) # differences among different income groups
summary(aov(dis_bk_grw~cuts_avginc, data=blockgrp)) # significant differences among different income groups
cor.test(blockgrp$dis_bk_16,blockgrp$avg_inc) # signficant positive correlation
cor.test(blockgrp$dis_bk_grw,blockgrp$avg_inc) # significant positive correlation

summary(aov((dis_bk_16-dis_bk_grw)~cuts_avginc, data=blockgrp)) # absolute change of distance to closest facility are different for at least one pair of -tile comparison
summary(aov((1-dis_bk_grw/dis_bk_16)~cuts_avginc, data=blockgrp))# percentage change of distance to closest facility are different for at least one pair of -tile comparison
cor.test((blockgrp$dis_bk_16-blockgrp$dis_bk_grw),blockgrp$avg_inc) # significant negative correlation between facility improvement and income
cor.test((1-blockgrp$dis_bk_grw/blockgrp$dis_bk_16),blockgrp$avg_inc) # significant negative correlation

# poverty
blockgrp$pov <- 1- blockgrp$doing_ok/blockgrp$pop_pov_det
q_pov <- quantile(blockgrp$pov,seq(0,1,by=0.2),na.rm=T)
cuts_pov <- cut(blockgrp$pov, q_pov)
blockgrp$cuts_pov <- cuts_pov
change_by_pov <- na.omit(blockgrp) %>% group_by(cuts_pov) %>% 
  summarise(dis_bk_16=mean(dis_bk_16), 
            dis_bk_grw=mean(dis_bk_grw),
            change=mean(dis_bk_16 - dis_bk_grw),
            grwth = mean(1-dis_bk_grw/dis_bk_16))

summary(aov(dis_bk_16~cuts_pov, data=blockgrp)) # differences among different income groups
summary(aov(dis_bk_grw~cuts_pov, data=blockgrp)) # significant differences among different income groups
cor.test(blockgrp$dis_bk_16,blockgrp$pov) # signficant positive correlation
cor.test(blockgrp$dis_bk_grw,blockgrp$pov) # significant positive correlation

summary(aov((dis_bk_16-dis_bk_grw)~cuts_pov, data=blockgrp)) # absolute change of distance to closest facility are different for at least one pair of -tile comparison
summary(aov((1-dis_bk_grw/dis_bk_16)~cuts_pov, data=blockgrp))# percentage change of distance to closest facility are different for at least one pair of -tile comparison
cor.test((blockgrp$dis_bk_16-blockgrp$dis_bk_grw),blockgrp$pov) # significant negative correlation between facility improvement and income
cor.test((1-blockgrp$dis_bk_grw/blockgrp$dis_bk_16),blockgrp$pov) # significant negative correlation

# education_col
q_edu_col <- quantile(blockgrp$edu_col,seq(0,1,by=0.2),na.rm=T)
cuts_edu_col <- cut(blockgrp$edu_col, q_edu_col)
blockgrp$cuts_edu_col <- cuts_edu_col
change_by_edu_col <- na.omit(blockgrp) %>% group_by(cuts_edu_col) %>% 
  summarise(dis_bk_16=mean(dis_bk_16), 
            dis_bk_grw=mean(dis_bk_grw),
            change=mean(dis_bk_16 - dis_bk_grw),
            grwth = mean(1-dis_bk_grw/dis_bk_16))

summary(aov(dis_bk_16~cuts_edu_col, data=blockgrp)) # differences among different income groups
summary(aov(dis_bk_grw~cuts_edu_col, data=blockgrp)) # significant differences among different income groups
cor.test(blockgrp$dis_bk_16,blockgrp$edu_col) # signficant positive correlation
cor.test(blockgrp$dis_bk_grw,blockgrp$edu_col) # significant positive correlation

summary(aov((dis_bk_16-dis_bk_grw)~cuts_edu_col, data=blockgrp)) # absolute change of distance to closest facility are different for at least one pair of -tile comparison
summary(aov((1-dis_bk_grw/dis_bk_16)~cuts_edu_col, data=blockgrp))# percentage change of distance to closest facility are different for at least one pair of -tile comparison
cor.test((blockgrp$dis_bk_16-blockgrp$dis_bk_grw),blockgrp$edu_col) # significant negative correlation between facility improvement and income
cor.test((1-blockgrp$dis_bk_grw/blockgrp$dis_bk_16),blockgrp$edu_col) # significant negative correlation

# education_bach
blockgrp$edu_bach <- blockgrp$edu_bach/blockgrp$pop_over25
q_edu_bach <- quantile(blockgrp$edu_bach,seq(0,1,by=0.2),na.rm=T)
cuts_edu_bach <- cut(blockgrp$edu_bach, q_edu_bach)
blockgrp$cuts_edu_bach <- cuts_edu_bach
change_by_edu_bach <- na.omit(blockgrp) %>% group_by(cuts_edu_bach) %>% 
  summarise(dis_bk_16=mean(dis_bk_16), 
            dis_bk_grw=mean(dis_bk_grw),
            change=mean(dis_bk_16 - dis_bk_grw),
            grwth = mean(1-dis_bk_grw/dis_bk_16))

summary(aov(dis_bk_16~cuts_edu_bach, data=blockgrp)) # differences among different income groups
summary(aov(dis_bk_grw~cuts_edu_bach, data=blockgrp)) # significant differences among different income groups
cor.test(blockgrp$dis_bk_16,blockgrp$edu_bach) # signficant positive correlation
cor.test(blockgrp$dis_bk_grw,blockgrp$edu_bach) # significant positive correlation

summary(aov((dis_bk_16-dis_bk_grw)~cuts_edu_bach, data=blockgrp)) # absolute change of distance to closest facility are different for at least one pair of -tile comparison
summary(aov((1-dis_bk_grw/dis_bk_16)~cuts_edu_bach, data=blockgrp))# percentage change of distance to closest facility are different for at least one pair of -tile comparison
cor.test((blockgrp$dis_bk_16-blockgrp$dis_bk_grw),blockgrp$edu_bach) # significant negative correlation between facility improvement and income
cor.test((1-blockgrp$dis_bk_grw/blockgrp$dis_bk_16),blockgrp$edu_bach) # significant negative correlation


# population density
blockgrp$popden <- blockgrp$pop15/blockgrp$area
q_popden <- quantile(blockgrp$popden,seq(0,1,by=0.2),na.rm=T)
cuts_popden <- cut(blockgrp$popden, q_popden)
blockgrp$cuts_popden <- cuts_popden
change_by_popden <- na.omit(blockgrp) %>% group_by(cuts_popden) %>% 
  summarise(dis_bk_16=mean(dis_bk_16), 
            dis_bk_grw=mean(dis_bk_grw),
            change=mean(dis_bk_16 - dis_bk_grw),
            grwth = mean(1-dis_bk_grw/dis_bk_16))

summary(aov(dis_bk_16~cuts_popden, data=blockgrp)) # differences among different income groups
summary(aov(dis_bk_grw~cuts_popden, data=blockgrp)) # significant differences among different income groups
cor.test(blockgrp$dis_bk_16,blockgrp$popden) # signficant positive correlation
cor.test(blockgrp$dis_bk_grw,blockgrp$popden) # significant positive correlation

summary(aov((dis_bk_16-dis_bk_grw)~cuts_popden, data=blockgrp)) # absolute change of distance to closest facility are different for at least one pair of -tile comparison
summary(aov((1-dis_bk_grw/dis_bk_16)~cuts_popden, data=blockgrp))# percentage change of distance to closest facility are different for at least one pair of -tile comparison
cor.test((blockgrp$dis_bk_16-blockgrp$dis_bk_grw),blockgrp$popden) # significant negative correlation between facility improvement and income
cor.test((1-blockgrp$dis_bk_grw/blockgrp$dis_bk_16),blockgrp$popden) # significant negative correlation


# Graph -------------------------------------------------------------------
library(ggplot2)
library(reshape)

race_m <- melt(data.frame(change_by_race[,c(1:3,5)]))
p1 <- ggplot(race_m,aes(x=cuts_nonwhite))+
  geom_bar(data=subset(race_m,variable!=c("grwth")),aes(y=value,fill=variable),stat = "identity",position="dodge") +
  scale_fill_manual(values=c("yellow3", "green3"),labels=c("Existing Bike Facilities in 2010", "Proposed Greenways in 2035")) +
  geom_point(data=subset(race_m,variable==c("grwth")),aes(y=value*3000,col="Change in Percentage")) +
  scale_y_continuous(name="Distance to Closest Facility [ft]",
                     sec.axis = sec_axis(~.*1/30, name = "Accessibility Improvement [%]")) +
  scale_x_discrete(name="Non-white Percentage Quintile",labels=c("0-20%","20-40%","40-60%","60-80%","80-100%")) +
  theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),legend.position = "bottom") +
  ggtitle("Percentage of Non-white Population")

inc_m <- melt(data.frame(change_by_medinc[,c(1:3,5)]))
p2 <- ggplot(inc_m,aes(x=cuts_medinc))+
  geom_bar(data=subset(inc_m,variable!=c("grwth")),aes(y=value,fill=variable),stat = "identity",position="dodge") +
  scale_fill_manual(values=c("yellow3", "green3"),labels=c("Existing Bike Facilities in 2010", "Proposed Greenways in 2035")) +
  geom_point(data=subset(inc_m,variable==c("grwth")),aes(y=value*3000,col="Change in Percentage")) +
  scale_y_continuous(name="Distance to Closest Facility[ft]",
                     sec.axis = sec_axis(~.*1/30, name = "Accessibility Improvement[%]")) +
  scale_x_discrete(name="Income Level Quintile",labels=c("0-20%","20-40%","40-60%","60-80%","80-100%")) +
  theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),legend.position = "bottom") +
  ggtitle("Household Median Income")

pov_m <- melt(data.frame(change_by_pov[,c(1:3,5)]))
p3 <- ggplot(pov_m,aes(x=cuts_pov))+
  geom_bar(data=subset(pov_m,variable!=c("grwth")),aes(y=value,fill=variable),stat = "identity",position="dodge") +
  scale_fill_manual(values=c("yellow3", "green3"),labels=c("Existing Bike Facilities in 2010", "Proposed Greenways in 2035")) +
  geom_point(data=subset(pov_m,variable==c("grwth")),aes(y=value*3000,col="Change in Percentage")) +
  scale_y_continuous(name="Distance to Closest Facility [ft]",
                     sec.axis = sec_axis(~.*1/30, name = "Accessibility Improvement [%]")) +
  scale_x_discrete(name="Poverty Level Quintile",labels=c("1st quintile","2nd quintile","3rd quintile","4th quintile","5th quintile")) +
  theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),legend.position = "bottom") +
  ggtitle("Percentage of HHs Below Two Times of Federal Poverty Level")

edu_m <- melt(data.frame(change_by_edu_col[,c(1:3,5)]))
p4 <- ggplot(edu_m,aes(x=cuts_edu_col))+
  geom_bar(data=subset(edu_m,variable!=c("grwth")),aes(y=value,fill=variable),stat = "identity",position="dodge") +
  scale_fill_manual(values=c("yellow3", "green3"),labels=c("Existing Bike Facilities in 2010", "Proposed Greenways in 2035")) +
  geom_point(data=subset(edu_m,variable==c("grwth")),aes(y=value*3000,col="Change in Percentage")) +
  scale_y_continuous(name="Distance to Closest Facility [ft]",
                     sec.axis = sec_axis(~.*1/30, name = "Accessibility Improvement [%]")) +
  scale_x_discrete(name="Education Level Quintile",labels=c("0-20%","20-40%","40-60%","60-80%","80-100%")) +
  theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),legend.position = "bottom") +
  ggtitle("Percentage of Population with Some College Education or Above")

grid.arrange(p1, p2, p3, p4,  ncol = 2)


# Correlation graph -------------------------------------------------------

cor=cor(agency.df[sapply(agency.df, is.numeric)]) #if there are non-numerical variables in the data frame
library("corrplot") #package corrplot https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
library('psych')
blockgrp$dis_change <- blockgrp$dis_bk_16 - blockgrp$dis_bk_grw
blockgrp$dis_pct <- 1-blockgrp$dis_bk_grw/blockgrp$dis_bk_16
x <- blockgrp[,c("race_nonwhite","med_inc","pov","edu_col","dis_bk_16","dis_bk_grw","dis_change","dis_pct")]
y <- blockgrp[,c("race_nonwhite","med_inc","pov","edu_col")]
cor1=cor(y,x)
cor.matrix=corr.test(x, y, method="spearman")
cor1.matrix=corr.p(cor1,447)
cor1.matrix$p
cor1.matrix$r[cor1.matrix$p > 0.05] <- 0
corrplot(cor1.matrix$r, col=colorRampPalette(c("#007FFF","white","#FF7F00"))(200), 
         p.mat = cor1.matrix$p, insig = "blank",tl.col = "black",tl.offset = 0.8, tl.cex=0.8, diag=F)


cor.mtest <- function(agency, conf.level = 0.95) {
  agency <- as.matrix(agency.df)
  n <- ncol(agency)
  p.agency <- lowCI.agency <- uppCI.agency <- matrix(NA, n, n)
  diag(p.agency) <- 0
  diag(lowCI.agency) <- diag(uppCI.agency) <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(agency[, i], agency[, j], conf.level = conf.level)
      p.agency[i, j] <- p.agency[j, i] <- tmp$p.value
      lowCI.agency[i, j] <- lowCI.agency[j, i] <- tmp$conf.int[1]
      uppCI.agency[i, j] <- uppCI.agency[j, i] <- tmp$conf.int[2]
    }
  }
  return(list(p.agency, lowCI.agency, uppCI.agency))
}

res1 <- cor.mtest(agency, 0.95)
res2 <- cor.mtest(agency, 0.99)

corrplot(cor, p.agency = res1[[1]], insig = "blank",insig = "blank", method = "color", type="lower",tl.col = "black",tl.offset = .8, tl.cex=.8)

corrplot(cor,type="lower",col.axis="black")

corrplot(cor, method="number", type="lower")

corrplot(cor1, insig = "blank", method = "color",tl.col = "black",tl.offset = .8, tl.cex=.8,)

cor=cor(agency.df[sapply(agency.df, is.numeric)])
x <- agency.df[1:19]
y <- agency.df[21:29]
cor1=cor(y,x)
cor1.col <- data.matrix(cbind(x))
cor1.col.names <- c("Nearest distance in 2016","Nearest distance in 2016","Change in Absolute Value","Change in Percentage")
cor1.row <- data.matrix(rbind(y))
cor1.row.name <- c("Percentage of non-white population","HH median income","Percentage of HH below 2 times of federal poverty level",
                   "Percentage of pouplation with some college education or above")
varnames=c("Nearest distance in 2016","Nearest distance in 2016","Change in Absolute Value","Change in Percentage",
           "Percentage of non-white population","HH median income","Percentage of HH below 2 times of federal poverty level",
           "Percentage of pouplation with some college education or above")
corrplot(cor1,varnames,abs=T,method="color")
corrplot(cor1,insig="blank", method = "color",tl.col = "black",tl.offset = 0.8, tl.cex=0.8,)
corrplot(cor(iris[,1:28])[1:28,1, drop=FALSE], cl.pos='n')

# Center identification ---------------------------------------------------
setwd("/Users/Wei/PDX Google drive/NITC Greenway/Data/")
library(foreign)
blockgrp <- read.dbf("PDX_blockgrp.dbf")
block <- read.dbf("Portland_blocks.dbf")
block$FIPS <- substring(block$GEOID10,1,12)
grp <- aggregate(block[,16:22], by=list(block$FIPS),sum)
grp2 <- aggregate(block, by=list(block$FIPS),length)
grp$length <- grp2$FIPS
write.csv(grp,"blockgrp_centers.csv",row.names=F)
