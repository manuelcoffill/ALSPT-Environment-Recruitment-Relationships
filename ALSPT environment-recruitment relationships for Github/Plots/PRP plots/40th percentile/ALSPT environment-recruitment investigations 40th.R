# load rec devs and datasets
library(here)

recdevs <- read.csv(here("ALSPTRecDevs.csv"))
recdevs


library(readxl)
upper <- read_excel(here("Upper_T_S_anomaly_annual_Jun-Oct.xlsx"))
upper

lower <- read_excel(here("Lower_T_S_anomaly_annual_Jun-Oct.xlsx"))
lower

## crops recdevs to match anomalies
recdevs <- recdevs[recdevs$Year > 1999 & recdevs$Year < 2024,]
recdevs

### crop anomalies to match recdev years
upper <- upper[upper$Year > 1999,]
upper

lower <- lower[lower$Year > 1999,]
lower

## add recdevs to each anomaly dataset
upper$rec <- recdevs$Deviation
lower$rec <- recdevs$Deviation

colnames(upper) <- c("Year","AnnT","AnnS","JunOctT","JunOctS","RecDev")
upper

colnames(lower) <- c("Year","AnnT","AnnS","JunOctT","JunOctS","RecDev")
lower


################################################################################
###############################################################################
################################################################################
############# SPEARMAN CORRELATION #############################################
################################################################################
## upper first

#cor.test(upper$RecDev, upper$AnnT, method = 'spearman')
# no correlation

#cor.test(upper$RecDev, upper$AnnS, method = 'spearman')
# strong positive relationship and p<0.05 :)

#cor.test(upper$RecDev, upper$JunOctT, method = 'spearman')
# strong positive relationship and p<0.05 :)

#cor.test(upper$RecDev, upper$JunOctS, method = 'spearman')
# strong positive relationship and p<0.05:)


#################################
## lower next

#cor.test(lower$RecDev, lower$AnnT, method = 'spearman')
# no correlation

#cor.test(lower$RecDev, lower$AnnS, method = 'spearman')
# strong positive relationship and p<0.05 :)

#cor.test(lower$RecDev, lower$JunOctT, method = 'spearman')
# strong positive relationship and p<0.05 :)

#cor.test(lower$RecDev, lower$JunOctS, method = 'spearman')
# strong positive relationship and p<0.05:)




################################################################################
########### poor recruitment paradigm plots ####################################
setwd(here("Plots","PRP plots/40th percentile"))
library(ggplot2)

##################### Upper first
### first upper annual temp
uppantdiv <- quantile(upper$AnnT, probs = 0.4) # division
uppantext <- upper[upper$AnnT < uppantdiv,] # extreme dataset
uppantextmedrec <- median(uppantext$RecDev) # median recruitment of extreme
uppantextmedrecsd <- sd(uppantext$RecDev) # sd of extreme

uppantnotext <- upper[upper$AnnT > uppantdiv,] # not extreme dataset
uppantnotextrec <- median(uppantnotext$RecDev) # median recruitment of not extreme
uppantnotextrecsd <- sd(uppantnotext$RecDev) # sd of not extreme

nrow(uppantext) # number of extreme observations
uppantgoodbad <- uppantext$RecDev > uppantnotextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(uppantgoodbad, na.rm=TRUE) / nrow(uppantext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


aa <- ggplot(data=upper, aes(x=AnnT,y=RecDev)) +
  geom_vline(xintercept=uppantdiv, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,uppantdiv), y = c(uppantextmedrec,uppantextmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(uppantdiv, Inf), y = c(uppantnotextrec,uppantnotextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="UMB annual bottom temperature", x="Anomaly", y="Log recruitment deviation") +
  theme_bw() +
  annotate("text", x=-0.5, y=0.8, label= "Median = 0.06
  SD = 0.53
  Good = 10%
  Poor = 90%",
           col="black", size=3) +
  annotate("text", x=1, y=-0.5, label= "Median = 0.56
  SD = 0.46",
           col="black", size=3)
aa

#ggsave("PRP_UMBAT.png", width = 10, height = 8, dpi = 1000)




##########################################################
### second upper annual sal
uppansdiv <- quantile(upper$AnnS, probs = 0.4)
uppansext <- upper[upper$AnnS < uppansdiv,]
uppansextmedrec <- median(uppansext$RecDev)
uppansextmedrecsd <- sd(uppansext$RecDev)

uppansnotext <- upper[upper$AnnS > uppansdiv,]
uppansnotextrec <- median(uppansnotext$RecDev)
uppansnotextrecsd <- sd(uppansnotext$RecDev)

nrow(uppansext)
uppansgoodbad <- uppansext$RecDev > uppansnotextrec
sum(uppansgoodbad, na.rm=TRUE) / nrow(uppansext)


ca <- ggplot(data=upper, aes(x=AnnS,y=RecDev)) +
  geom_vline(xintercept=uppansdiv, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf, uppansdiv), y = c(uppansextmedrec,uppansextmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(uppansdiv, Inf), y = c(uppansnotextrec,uppansnotextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="UMB annual bottom salinity", x="Anomaly", y="Log recruitment deviation") +
  theme_bw() +
  annotate("text", x=-2, y=0.8, label= "Median = 0.11
  SD = 0.27
  Good = 0%
  Poor = 100%",
           col="black", size=3) +
  annotate("text", x=2, y=-0.5, label= "Median = 0.64
  SD = 0.59",
           col="black", size=3)
ca

#ggsave("PRP_UMBAS.png", width = 10, height = 8, dpi = 1000)



#########
# seasonal temp
uppjunocttdiv <- quantile(upper$JunOctT, probs = 0.4) # division
uppjunocttext <- upper[upper$JunOctT < uppjunocttdiv,] # extreme dataset
uppjunocttextmedrec <- median(uppjunocttext$RecDev) # median recruitment of extreme
uppjunocttextmedrecsd <- sd(uppjunocttext$RecDev) # sd of extreme

uppjunocttnotext <- upper[upper$JunOctT > uppjunocttdiv,] # not extreme dataset
uppjunocttnotextrec <- median(uppjunocttnotext$RecDev) # median recruitment of not extreme
uppjunocttnotextrecsd <- sd(uppjunocttnotext$RecDev) # sd of not extreme

nrow(uppjunocttext) # number of extreme observations
uppjunocttgoodbad <- uppjunocttext$RecDev > uppjunocttnotextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(uppjunocttgoodbad, na.rm=TRUE) / nrow(uppjunocttext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


ba <- ggplot(data=upper, aes(x=JunOctT,y=RecDev)) +
  geom_vline(xintercept=uppjunocttdiv, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,uppjunocttdiv), y = c(uppjunocttextmedrec,uppjunocttextmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(uppjunocttdiv, Inf), y = c(uppjunocttnotextrec,uppjunocttnotextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="UMB Jun-Oct bottom temperature", x="Anomaly", y="Log recruitment deviation") +
  theme_bw() +
  annotate("text", x=-0.8, y=0.8, label= "Median = -0.08
  SD = 0.39
  Good = 0%
  Poor = 100%",
           col="black", size=3) +
  annotate("text", x=1, y=-0.5, label= "Median = 0.64
  SD = 0.46",
           col="black", size=3)

ba

#ggsave("PRP_UMBJunOctT.png", width = 10, height = 8, dpi = 1000)




##########################################################
### seasonal sal
uppjunoctsdiv <- quantile(upper$JunOctS, probs = 0.4)
uppjunoctsext <- upper[upper$JunOctS < uppjunoctsdiv,]
uppjunoctsextmedrec <- median(uppjunoctsext$RecDev)
uppjunoctsextmedrecsd <- sd(uppjunoctsext$RecDev)

uppjunoctsnotext <- upper[upper$JunOctS > uppjunoctsdiv,]
uppjunoctsnotextrec <- median(uppjunoctsnotext$RecDev)
uppjunoctsnotextrecsd <- sd(uppjunoctsnotext$RecDev)

nrow(uppjunoctsext)
uppjunoctsgoodbad <- uppjunoctsext$RecDev > uppjunoctsnotextrec
sum(uppjunoctsgoodbad, na.rm=TRUE) / nrow(uppjunoctsext)


da <- ggplot(data=upper, aes(x=JunOctS,y=RecDev)) +
  geom_vline(xintercept=uppjunoctsdiv, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf, uppjunoctsdiv), y = c(uppjunoctsextmedrec,uppjunoctsextmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(uppjunoctsdiv, Inf), y = c(uppjunoctsnotextrec,uppjunoctsnotextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="UMB Jun-Oct bottom salinity", x="Anomaly", y="Log recruitment deviation") +
  theme_bw() +
  annotate("text", x=-3, y=0.8, label= "Median = 0.11
  SD = 0.25
  Good = 0%
  Poor = 100%",
           col="black", size=3) +
  annotate("text", x=2, y=-0.5, label= "Median = 0.64
  SD = 0.60",
           col="black", size=3)

da

#ggsave("PRP_UMBJunOctS.png", width = 10, height = 8, dpi = 1000)




######################
##################### Lower next

### first upper annual temp
lowantdiv <- quantile(lower$AnnT, probs = 0.4) # division
lowantext <- lower[lower$AnnT < lowantdiv,] # extreme dataset
lowantextmedrec <- median(lowantext$RecDev) # median recruitment of extreme
lowantextmedrecsd <- sd(lowantext$RecDev) # sd of extreme

lowantnotext <- lower[lower$AnnT > lowantdiv,] # not extreme dataset
lowantnotextrec <- median(lowantnotext$RecDev) # median recruitment of not extreme
lowantnotextrecsd <- sd(lowantnotext$RecDev) # sd of not extreme

nrow(lowantext) # number of extreme observations
lowantgoodbad <- lowantext$RecDev > lowantnotextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(lowantgoodbad, na.rm=TRUE) / nrow(lowantext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


ab <- ggplot(data=lower, aes(x=AnnT,y=RecDev)) +
  geom_vline(xintercept=lowantdiv, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,lowantdiv), y = c(lowantextmedrec,lowantextmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(lowantdiv, Inf), y = c(lowantnotextrec,lowantnotextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="LMB annual bottom temperature", x="Anomaly", y="Log recruitment deviation") +
  theme_bw() +
  annotate("text", x=-0.25, y=0.8, label= "Median = 0.25
  SD = 0.54
  Good = 20%
  Poor = 80%",
           col="black", size=3) +
  annotate("text", x=1, y=-0.5, label= "Median = 0.46
  SD = 0.49",
           col="black", size=3)

ab

#ggsave("PRP_LMBAT.png", width = 10, height = 8, dpi = 1000)




##########################################################
### second lower annual sal
lowansdiv <- quantile(lower$AnnS, probs = 0.4)
lowansext <- lower[lower$AnnS < lowansdiv,]
lowansextmedrec <- median(lowansext$RecDev)
lowansextmedrecsd <- sd(lowansext$RecDev)

lowansnotext <- lower[lower$AnnS > lowansdiv,]
lowansnotextrec <- median(lowansnotext$RecDev)
lowansnotextrecsd <- sd(lowansnotext$RecDev)

nrow(lowansext)
lowansgoodbad <- lowansext$RecDev > lowansnotextrec
sum(lowansgoodbad, na.rm=TRUE) / nrow(lowansext)


cb <- ggplot(data=lower, aes(x=AnnS,y=RecDev)) +
  geom_vline(xintercept=lowansdiv, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf, lowansdiv), y = c(lowansextmedrec,lowansextmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(lowansdiv, Inf), y = c(lowansnotextrec,lowansnotextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="LMB annual bottom salinity", x="Anomaly", y="Log recruitment deviation") +
  theme_bw() +
  annotate("text", x=-3, y=0.8, label= "Median = 0.14
  SD = 0.33
  Good = 10%
  Poor = 90%",
           col="black", size=3) +
  annotate("text", x=3, y=-0.5, label= "Median = 0.56
  SD = 0.59",
           col="black", size=3)

cb

#ggsave("PRP_LMBAS.png", width = 10, height = 8, dpi = 1000)



#########
# seasonal temp
lowjunocttdiv <- quantile(lower$JunOctT, probs = 0.4) # division
lowjunocttext <- lower[lower$JunOctT < lowjunocttdiv,] # extreme dataset
lowjunocttextmedrec <- median(lowjunocttext$RecDev) # median recruitment of extreme
lowjunocttextmedrecsd <- sd(lowjunocttext$RecDev) # sd of extreme

lowjunocttnotext <- lower[lower$JunOctT > lowjunocttdiv,] # not extreme dataset
lowjunocttnotextrec <- median(lowjunocttnotext$RecDev) # median recruitment of not extreme
lowjunocttnotextrecsd <- sd(lowjunocttnotext$RecDev) # sd of not extreme

nrow(lowjunocttext) # number of extreme observations
lowjunocttgoodbad <- lowjunocttext$RecDev > lowjunocttnotextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(lowjunocttgoodbad, na.rm=TRUE) / nrow(lowjunocttext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


bb <- ggplot(data=lower, aes(x=JunOctT,y=RecDev)) +
  geom_vline(xintercept=lowjunocttdiv, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,lowjunocttdiv), y = c(lowjunocttextmedrec,lowjunocttextmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(lowjunocttdiv, Inf), y = c(lowjunocttnotextrec,lowjunocttnotextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="LMB Jun-Oct bottom temperature", x="Anomaly", y="Log recruitment deviation") +
  theme_bw() +
  annotate("text", x=-0.5, y=0.8, label= "Median = -0.19
  SD = 0.36
  Good = 0%
  Poor = 100%",
           col="black", size=3) +
  annotate("text", x=0.75, y=-0.5, label= "Median = 0.64
  SD = 0.39",
           col="black", size=3)

bb

#ggsave("PRP_LMBJunOctT.png", width = 10, height = 8, dpi = 1000)




##########################################################
### lower seasonal sal
lowjunoctsdiv <- quantile(lower$JunOctS, probs = 0.4)
lowjunoctsext <- lower[lower$JunOctS < lowjunoctsdiv,]
lowjunoctsextmedrec <- median(lowjunoctsext$RecDev)
lowjunoctsextmedrecsd <- sd(lowjunoctsext$RecDev)

lowjunoctsnotext <- lower[lower$JunOctS > lowjunoctsdiv,]
lowjunoctsnotextrec <- median(lowjunoctsnotext$RecDev)
lowjunoctsnotextrecsd <- sd(lowjunoctsnotext$RecDev)

nrow(lowjunoctsext)
lowjunoctsgoodbad <- lowjunoctsext$RecDev > lowjunoctsnotextrec
sum(lowjunoctsgoodbad, na.rm=TRUE) / nrow(lowjunoctsext)


db <- ggplot(data=lower, aes(x=JunOctS,y=RecDev)) +
  geom_vline(xintercept=lowjunoctsdiv, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf, lowjunoctsdiv), y = c(lowjunoctsextmedrec,lowjunoctsextmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(lowjunoctsdiv, Inf), y = c(lowjunoctsnotextrec,lowjunoctsnotextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="LMB Jun-Oct bottom salinity", x="Anomaly", y="Log recruitment deviation") +
  theme_bw() +
  annotate("text", x=-3, y=0.8, label= "Median = 0.25
  SD = 0.33
  Good = 10%
  Poor = 90%",
           col="black", size=3) +
  annotate("text", x=2, y=-0.5, label= "Median = 0.56
  SD = 0.61",
           col="black", size=3)

db

#ggsave("PRP_LMBJunOctS.png", width = 10, height = 8, dpi = 1000)



######### merging all PRP plots together :)))
# first remove their axes
aa <- aa + xlab(NULL) + ylab(NULL)
ab <- ab + xlab(NULL) + ylab(NULL)
ba <- ba + xlab(NULL) + ylab(NULL)
bb <- bb + xlab(NULL) + ylab(NULL)
ca <- ca + xlab(NULL) + ylab(NULL)
cb <- cb + xlab(NULL) + ylab(NULL)
da <- da + xlab(NULL) + ylab(NULL)
db <- db + xlab(NULL) + ylab(NULL)

library(ggpubr)
library(grid)

prpfinal <- ggarrange(aa,ab,ba,bb,ca,cb,da,db, ncol = 2, nrow = 4,
                   label.x = c(0.85, 0.85),  # the x positions of the labels
                   label.y = c(1, 1),
                   font.label = list(size = 30))  # t)
prpfinal
annotate_figure(prpfinal, bottom = textGrob("Anomaly", vjust = 0),
                left = textGrob("Log recruitment deviation", vjust = 0, rot=90))

#ggsave("PRP_Merged40th.png", width = 8, height = 10, dpi = 1000)



