### ALSPT PRP with AMO
library(here)
library(readxl)
library(ggplot2)

# load rec devs
recdevs <- read.csv(here("ALSPTRecDevs.csv"))
recdevs

### AMO 
# upload dataset
amo <- read_excel(here("AMO and AMV","AMO and NAO","AMO detrended unsmoothed.xlsx"))
str(amo)
head(amo)
tail(amo)

# truncated dataset to match rec devs
amo <- amo[amo$Year > 1999 & amo$Year < 2023,]
summary(amo$Year)

## crops recdevs to match anomalies
recdevs <- recdevs[recdevs$Year > 1999 & recdevs$Year < 2023,]
recdevs

## add recdevs to each anomaly dataset
amo$rec <- recdevs$Deviation


amo <- data.frame(amo$Year, amo$Annual,amo$`Jun-Oct`, amo$rec)
amo
colnames(amo) <- c("Year","Ann","JunOct","RecDev")
amo

## ready for PRP :)



################################################################################
### 20th percentile first
setwd(here("AMO and AMV","AMO and NAO","AMO PRP"))

##################### annual first
amoanndiv <- quantile(amo$Ann, probs = 0.2) # division
amoannext <- amo[amo$Ann < amoanndiv,] # extreme dataset
amoannextmedrec <- median(amoannext$RecDev) # median recruitment of extreme
amoannextmedrecsd <- sd(amoannext$RecDev) # sd of extreme

amoannnotext <- amo[amo$Ann > amoanndiv,] # not extreme dataset
amoannnotextrec <- median(amoannnotext$RecDev) # median recruitment of not extreme
amoannnotextrecsd <- sd(amoannnotext$RecDev) # sd of not extreme

nrow(amoannext) # number of extreme observations
amoanngoodbad <- amoannext$RecDev > amoannnotextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(amoanngoodbad, na.rm=TRUE) / nrow(amoannext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


aa <- ggplot(data=amo, aes(x=Ann,y=RecDev)) +
  geom_vline(xintercept=amoanndiv, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,amoanndiv), y = c(amoannextmedrec,amoannextmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(amoanndiv, Inf), y = c(amoannnotextrec,amoannnotextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="AMO annual 20th percentile", x="Anomaly", y="Log recruitment deviation") +
  theme_bw() + 
  annotate("text", x=0.025, y=0.5, label= "Median = 0.27
  SD = 0.28
  Good = 20%
  Poor = 80%",
           col="black", size=3) +
  annotate("text", x=0.2, y=0.8, label= "Median = 0.40
  SD = 0.56",
           col="black", size=3)
aa

#ggsave("AMOAnnual20th.png", width = 10, height = 8, dpi = 1000)




##################### seasonal second
amojunoctdiv <- quantile(amo$JunOct, probs = 0.2) # division
amojunoctext <- amo[amo$JunOct < amojunoctdiv,] # extreme dataset
amojunoctextmedrec <- median(amojunoctext$RecDev) # median recruitment of extreme
amojunoctextmedrecsd <- sd(amojunoctext$RecDev) # sd of extreme

amojunoctnotext <- amo[amo$JunOct > amojunoctdiv,] # not extreme dataset
amojunoctnotextrec <- median(amojunoctnotext$RecDev) # median recruitment of not extreme
amojunoctnotextrecsd <- sd(amojunoctnotext$RecDev) # sd of not extreme

nrow(amojunoctext) # number of extreme observations
amojunoctgoodbad <- amojunoctext$RecDev > amojunoctnotextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(amojunoctgoodbad, na.rm=TRUE) / nrow(amojunoctext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


ab <- ggplot(data=amo, aes(x=JunOct,y=RecDev)) +
  geom_vline(xintercept=amojunoctdiv, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,amojunoctdiv), y = c(amojunoctextmedrec,amojunoctextmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(amojunoctdiv, Inf), y = c(amojunoctnotextrec,amojunoctnotextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="AMO Jun-Oct 20th percentile", x="Anomaly", y="Log recruitment deviation") +
  theme_bw() + 
  annotate("text", x=0.05, y=0.7, label= "Median = 0.27
  SD = 0.59
  Good = 40%
  Poor = 60%",
           col="black", size=3) +
  annotate("text", x=0.3, y=0.8, label= "Median = 0.30
  SD = 0.51",
           col="black", size=3)
ab

#ggsave("AMOJunOct20th.png", width = 10, height = 8, dpi = 1000)





################################################################################
### 25th percentile next
##################### annual first
amoanndiv <- quantile(amo$Ann, probs = 0.25) # division
amoannext <- amo[amo$Ann < amoanndiv,] # extreme dataset
amoannextmedrec <- median(amoannext$RecDev) # median recruitment of extreme
amoannextmedrecsd <- sd(amoannext$RecDev) # sd of extreme

amoannnotext <- amo[amo$Ann > amoanndiv,] # not extreme dataset
amoannnotextrec <- median(amoannnotext$RecDev) # median recruitment of not extreme
amoannnotextrecsd <- sd(amoannnotext$RecDev) # sd of not extreme

nrow(amoannext) # number of extreme observations
amoanngoodbad <- amoannext$RecDev > amoannnotextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(amoanngoodbad, na.rm=TRUE) / nrow(amoannext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


ba <- ggplot(data=amo, aes(x=Ann,y=RecDev)) +
  geom_vline(xintercept=amoanndiv, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,amoanndiv), y = c(amoannextmedrec,amoannextmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(amoanndiv, Inf), y = c(amoannnotextrec,amoannnotextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="AMO annual 25th percentile", x="Anomaly", y="Log recruitment deviation") +
  theme_bw() + 
  annotate("text", x=0.025, y=0.5, label= "Median = 0.28
  SD = 0.43
  Good = 33%
  Poor = 67%",
           col="black", size=3) +
  annotate("text", x=0.2, y=0.8, label= "Median = 0.32
  SD = 0.56",
           col="black", size=3)
ba

#ggsave("AMOAnnual25th.png", width = 10, height = 8, dpi = 1000)




##################### seasonal second
amojunoctdiv <- quantile(amo$JunOct, probs = 0.25) # division
amojunoctext <- amo[amo$JunOct < amojunoctdiv,] # extreme dataset
amojunoctextmedrec <- median(amojunoctext$RecDev) # median recruitment of extreme
amojunoctextmedrecsd <- sd(amojunoctext$RecDev) # sd of extreme

amojunoctnotext <- amo[amo$JunOct > amojunoctdiv,] # not extreme dataset
amojunoctnotextrec <- median(amojunoctnotext$RecDev) # median recruitment of not extreme
amojunoctnotextrecsd <- sd(amojunoctnotext$RecDev) # sd of not extreme

nrow(amojunoctext) # number of extreme observations
amojunoctgoodbad <- amojunoctext$RecDev > amojunoctnotextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(amojunoctgoodbad, na.rm=TRUE) / nrow(amojunoctext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


bb <- ggplot(data=amo, aes(x=JunOct,y=RecDev)) +
  geom_vline(xintercept=amojunoctdiv, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,amojunoctdiv), y = c(amojunoctextmedrec,amojunoctextmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(amojunoctdiv, Inf), y = c(amojunoctnotextrec,amojunoctnotextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="AMO Jun-Oct 25th percentile", x="Anomaly", y="Log recruitment deviation") +
  theme_bw() + 
  annotate("text", x=0.05, y=0.7, label= "Median = 0.28
  SD = 0.53
  Good = 33%
  Poor = 67%",
           col="black", size=3) +
  annotate("text", x=0.3, y=0.8, label= "Median = 0.32
  SD = 0.52",
           col="black", size=3)
bb

#ggsave("AMOJunOct25th.png", width = 10, height = 8, dpi = 1000)








################################################################################
### 30th percentile next
##################### annual first
amoanndiv <- quantile(amo$Ann, probs = 0.30) # division
amoannext <- amo[amo$Ann < amoanndiv,] # extreme dataset
amoannextmedrec <- median(amoannext$RecDev) # median recruitment of extreme
amoannextmedrecsd <- sd(amoannext$RecDev) # sd of extreme

amoannnotext <- amo[amo$Ann > amoanndiv,] # not extreme dataset
amoannnotextrec <- median(amoannnotext$RecDev) # median recruitment of not extreme
amoannnotextrecsd <- sd(amoannnotext$RecDev) # sd of not extreme

nrow(amoannext) # number of extreme observations
amoanngoodbad <- amoannext$RecDev > amoannnotextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(amoanngoodbad, na.rm=TRUE) / nrow(amoannext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


ca <- ggplot(data=amo, aes(x=Ann,y=RecDev)) +
  geom_vline(xintercept=amoanndiv, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,amoanndiv), y = c(amoannextmedrec,amoannextmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(amoanndiv, Inf), y = c(amoannnotextrec,amoannnotextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="AMO annual 30th percentile", x="Anomaly", y="Log recruitment deviation") +
  theme_bw() + 
  annotate("text", x=0.025, y=0.5, label= "Median = 0.28
  SD = 0.44
  Good = 71%
  Poor = 29%",
           col="black", size=3) +
  annotate("text", x=0.2, y=0.8, label= "Median = 0.27
  SD = 0.56",
           col="black", size=3)
ca

#ggsave("AMOAnnual30th.png", width = 10, height = 8, dpi = 1000)




##################### seasonal second
amojunoctdiv <- quantile(amo$JunOct, probs = 0.30) # division
amojunoctext <- amo[amo$JunOct < amojunoctdiv,] # extreme dataset
amojunoctextmedrec <- median(amojunoctext$RecDev) # median recruitment of extreme
amojunoctextmedrecsd <- sd(amojunoctext$RecDev) # sd of extreme

amojunoctnotext <- amo[amo$JunOct > amojunoctdiv,] # not extreme dataset
amojunoctnotextrec <- median(amojunoctnotext$RecDev) # median recruitment of not extreme
amojunoctnotextrecsd <- sd(amojunoctnotext$RecDev) # sd of not extreme

nrow(amojunoctext) # number of extreme observations
amojunoctgoodbad <- amojunoctext$RecDev > amojunoctnotextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(amojunoctgoodbad, na.rm=TRUE) / nrow(amojunoctext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


cb <- ggplot(data=amo, aes(x=JunOct,y=RecDev)) +
  geom_vline(xintercept=amojunoctdiv, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,amojunoctdiv), y = c(amojunoctextmedrec,amojunoctextmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(amojunoctdiv, Inf), y = c(amojunoctnotextrec,amojunoctnotextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="AMO Jun-Oct 30th percentile", x="Anomaly", y="Log recruitment deviation") +
  theme_bw() + 
  annotate("text", x=0.05, y=0.7, label= "Median = 0.28
  SD = 0.49
  Good = 71%
  Poor = 29%",
           col="black", size=3) +
  annotate("text", x=0.3, y=0.8, label= "Median = 0.27
  SD = 0.54",
           col="black", size=3)
cb

#ggsave("AMOJunOct30th.png", width = 10, height = 8, dpi = 1000)




################################################################################
### 33th percentile next
##################### annual first
amoanndiv <- quantile(amo$Ann, probs = 0.33) # division
amoannext <- amo[amo$Ann < amoanndiv,] # extreme dataset
amoannextmedrec <- median(amoannext$RecDev) # median recruitment of extreme
amoannextmedrecsd <- sd(amoannext$RecDev) # sd of extreme

amoannnotext <- amo[amo$Ann > amoanndiv,] # not extreme dataset
amoannnotextrec <- median(amoannnotext$RecDev) # median recruitment of not extreme
amoannnotextrecsd <- sd(amoannnotext$RecDev) # sd of not extreme

nrow(amoannext) # number of extreme observations
amoanngoodbad <- amoannext$RecDev > amoannnotextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(amoanngoodbad, na.rm=TRUE) / nrow(amoannext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


da <- ggplot(data=amo, aes(x=Ann,y=RecDev)) +
  geom_vline(xintercept=amoanndiv, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,amoanndiv), y = c(amoannextmedrec,amoannextmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(amoanndiv, Inf), y = c(amoannnotextrec,amoannnotextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="AMO annual 33th percentile", x="Anomaly", y="Log recruitment deviation") +
  theme_bw() + 
  annotate("text", x=0.025, y=0.5, label= "Median = 0.28
  SD = 0.59
  Good = 38%
  Poor = 62%",
           col="black", size=3) +
  annotate("text", x=0.2, y=0.8, label= "Median = 0.32
  SD = 0.49",
           col="black", size=3)
da

#ggsave("AMOAnnual33th.png", width = 10, height = 8, dpi = 1000)




##################### seasonal second
amojunoctdiv <- quantile(amo$JunOct, probs = 0.33) # division
amojunoctext <- amo[amo$JunOct < amojunoctdiv,] # extreme dataset
amojunoctextmedrec <- median(amojunoctext$RecDev) # median recruitment of extreme
amojunoctextmedrecsd <- sd(amojunoctext$RecDev) # sd of extreme

amojunoctnotext <- amo[amo$JunOct > amojunoctdiv,] # not extreme dataset
amojunoctnotextrec <- median(amojunoctnotext$RecDev) # median recruitment of not extreme
amojunoctnotextrecsd <- sd(amojunoctnotext$RecDev) # sd of not extreme

nrow(amojunoctext) # number of extreme observations
amojunoctgoodbad <- amojunoctext$RecDev > amojunoctnotextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(amojunoctgoodbad, na.rm=TRUE) / nrow(amojunoctext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


db <- ggplot(data=amo, aes(x=JunOct,y=RecDev)) +
  geom_vline(xintercept=amojunoctdiv, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,amojunoctdiv), y = c(amojunoctextmedrec,amojunoctextmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(amojunoctdiv, Inf), y = c(amojunoctnotextrec,amojunoctnotextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="AMO Jun-Oct 33th percentile", x="Anomaly", y="Log recruitment deviation") +
  theme_bw() + 
  annotate("text", x=0.05, y=0.7, label= "Median = 0.38
  SD = 0.48
  Good = 75%
  Poor = 25%",
           col="black", size=3) +
  annotate("text", x=0.3, y=0.8, label= "Median = 0.23
  SD = 0.53",
           col="black", size=3)
db

#ggsave("AMOJunOct33th.png", width = 10, height = 8, dpi = 1000)




################################################################################
### lastly, 40th percentile
##################### annual first
amoanndiv <- quantile(amo$Ann, probs = 0.40) # division
amoannext <- amo[amo$Ann < amoanndiv,] # extreme dataset
amoannextmedrec <- median(amoannext$RecDev) # median recruitment of extreme
amoannextmedrecsd <- sd(amoannext$RecDev) # sd of extreme

amoannnotext <- amo[amo$Ann > amoanndiv,] # not extreme dataset
amoannnotextrec <- median(amoannnotext$RecDev) # median recruitment of not extreme
amoannnotextrecsd <- sd(amoannnotext$RecDev) # sd of not extreme

nrow(amoannext) # number of extreme observations
amoanngoodbad <- amoannext$RecDev > amoannnotextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(amoanngoodbad, na.rm=TRUE) / nrow(amoannext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


ea <- ggplot(data=amo, aes(x=Ann,y=RecDev)) +
  geom_vline(xintercept=amoanndiv, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,amoanndiv), y = c(amoannextmedrec,amoannextmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(amoanndiv, Inf), y = c(amoannnotextrec,amoannnotextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="AMO annual 40th percentile", x="Anomaly", y="Log recruitment deviation") +
  theme_bw() + 
  annotate("text", x=0.025, y=0.5, label= "Median = 0.28
  SD = 0.56
  Good = 67%
  Poor = 33%",
           col="black", size=3) +
  annotate("text", x=0.2, y=0.8, label= "Median = 0.27
  SD = 0.50",
           col="black", size=3)
ea

#ggsave("AMOAnnual40th.png", width = 10, height = 8, dpi = 1000)




##################### seasonal second
amojunoctdiv <- quantile(amo$JunOct, probs = 0.40) # division
amojunoctext <- amo[amo$JunOct < amojunoctdiv,] # extreme dataset
amojunoctextmedrec <- median(amojunoctext$RecDev) # median recruitment of extreme
amojunoctextmedrecsd <- sd(amojunoctext$RecDev) # sd of extreme

amojunoctnotext <- amo[amo$JunOct > amojunoctdiv,] # not extreme dataset
amojunoctnotextrec <- median(amojunoctnotext$RecDev) # median recruitment of not extreme
amojunoctnotextrecsd <- sd(amojunoctnotext$RecDev) # sd of not extreme

nrow(amojunoctext) # number of extreme observations
amojunoctgoodbad <- amojunoctext$RecDev > amojunoctnotextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(amojunoctgoodbad, na.rm=TRUE) / nrow(amojunoctext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


eb <- ggplot(data=amo, aes(x=JunOct,y=RecDev)) +
  geom_vline(xintercept=amojunoctdiv, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,amojunoctdiv), y = c(amojunoctextmedrec,amojunoctextmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(amojunoctdiv, Inf), y = c(amojunoctnotextrec,amojunoctnotextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="AMO Jun-Oct 40th percentile", x="Anomaly", y="Log recruitment deviation") +
  theme_bw() + 
  annotate("text", x=0.05, y=0.7, label= "Median = 0.28
  SD = 0.50
  Good = 67%
  Poor = 23%",
           col="black", size=3) +
  annotate("text", x=0.3, y=0.8, label= "Median = 0.27
  SD = 0.54",
           col="black", size=3)
eb

#ggsave("AMOJunOct40th.png", width = 10, height = 8, dpi = 1000)







###############################################################################
#### merging plots
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
ea <- ea + xlab(NULL) + ylab(NULL)
eb <- eb + xlab(NULL) + ylab(NULL)

library(ggpubr)
library(grid)

prpfinal <- ggarrange(aa,ab,ba,bb,ca,cb,da,db,ea,eb, ncol = 2, nrow = 5,
                      label.x = c(0.85, 0.85),  # the x positions of the labels
                      label.y = c(1, 1),
                      font.label = list(size = 30))  # t)
prpfinal
annotate_figure(prpfinal, bottom = textGrob("Anomaly", vjust = 0),
                left = textGrob("Log recruitment deviation", vjust = 0, rot=90))

#ggsave("AMO_PRP_2000to2022.png", width = 8, height = 10, dpi = 1000)








