### ALSPT PRP with NAO
library(here)
library(readxl)
library(ggplot2)

# load rec devs
recdevs <- read.csv(here("ALSPTRecDevs.csv"))
recdevs

### NAO 
# upload dataset
nao <- read_excel(here("AMO and AMV\\AMO and NAO\\Monthly mean NAO index.xlsx"))
str(nao)
head(nao)
tail(nao)

# truncated dataset to match rec devs
nao <- nao[nao$Year > 1999 & nao$Year < 2024,]
summary(nao$Year)

## crops recdevs to match anomalies
recdevs <- recdevs[recdevs$Year > 1999 & recdevs$Year < 2024,]
recdevs

## add recdevs to each anomaly dataset
nao$rec <- recdevs$Deviation


nao <- data.frame(nao$Year, nao$Annual,nao$`Jun-Oct`, nao$rec)
nao
colnames(nao) <- c("Year","Ann","JunOct","RecDev")
nao

## ready for PRP :)



################################################################################
### 20th percentile first
setwd(here("AMO and AMV","AMO and NAO","NAO PRP"))

##################### annual first
naoanndiv <- quantile(nao$Ann, probs = 0.2) # division
naoannext <- nao[nao$Ann < naoanndiv,] # extreme dataset
naoannextmedrec <- median(naoannext$RecDev) # median recruitment of extreme
naoannextmedrecsd <- sd(naoannext$RecDev) # sd of extreme

naoannnotext <- nao[nao$Ann > naoanndiv,] # not extreme dataset
naoannnotextrec <- median(naoannnotext$RecDev) # median recruitment of not extreme
naoannnotextrecsd <- sd(naoannnotext$RecDev) # sd of not extreme

nrow(naoannext) # number of extreme observations
naoanngoodbad <- naoannext$RecDev > naoannnotextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(naoanngoodbad, na.rm=TRUE) / nrow(naoannext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


aa <- ggplot(data=nao, aes(x=Ann,y=RecDev)) +
  geom_vline(xintercept=naoanndiv, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,naoanndiv), y = c(naoannextmedrec,naoannextmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(naoanndiv, Inf), y = c(naoannnotextrec,naoannnotextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="NAO annual 20th percentile", x="Anomaly", y="Log recruitment deviation") +
  theme_bw() + 
  annotate("text", x=-0.75, y=0.5, label= "Median = 0.00
  SD = 0.56
  Good = 40%
  Poor = 60%",
           col="black", size=3) +
  annotate("text", x=0.75, y=0.8, label= "Median = 0.28
  SD = 0.51",
           col="black", size=3)
aa

#ggsave("NAOAnnual20th.png", width = 10, height = 8, dpi = 1000)




##################### seasonal second
naojunoctdiv <- quantile(nao$JunOct, probs = 0.2) # division
naojunoctext <- nao[nao$JunOct < naojunoctdiv,] # extreme dataset
naojunoctextmedrec <- median(naojunoctext$RecDev) # median recruitment of extreme
naojunoctextmedrecsd <- sd(naojunoctext$RecDev) # sd of extreme

naojunoctnotext <- nao[nao$JunOct > naojunoctdiv,] # not extreme dataset
naojunoctnotextrec <- median(naojunoctnotext$RecDev) # median recruitment of not extreme
naojunoctnotextrecsd <- sd(naojunoctnotext$RecDev) # sd of not extreme

nrow(naojunoctext) # number of extreme observations
naojunoctgoodbad <- naojunoctext$RecDev > naojunoctnotextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(naojunoctgoodbad, na.rm=TRUE) / nrow(naojunoctext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


ab <- ggplot(data=nao, aes(x=JunOct,y=RecDev)) +
  geom_vline(xintercept=naojunoctdiv, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,naojunoctdiv), y = c(naojunoctextmedrec,naojunoctextmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(naojunoctdiv, Inf), y = c(naojunoctnotextrec,naojunoctnotextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="NAO Jun-Oct 20th percentile", x="Anomaly", y="Log recruitment deviation") +
  theme_bw() + 
  annotate("text", x=-1.2, y=0.7, label= "Median = 0.64
  SD = 0.55
  Good = 60%
  Poor = 40%",
           col="black", size=3) +
  annotate("text", x=1, y=0.8, label= "Median = 0.27
  SD = 0.50",
           col="black", size=3)
ab

#ggsave("NAOJunOct20th.png", width = 10, height = 8, dpi = 1000)





################################################################################
### 25th percentile next
##################### annual first
naoanndiv <- quantile(nao$Ann, probs = 0.25) # division
naoannext <- nao[nao$Ann < naoanndiv,] # extreme dataset
naoannextmedrec <- median(naoannext$RecDev) # median recruitment of extreme
naoannextmedrecsd <- sd(naoannext$RecDev) # sd of extreme

naoannnotext <- nao[nao$Ann > naoanndiv,] # not extreme dataset
naoannnotextrec <- median(naoannnotext$RecDev) # median recruitment of not extreme
naoannnotextrecsd <- sd(naoannnotext$RecDev) # sd of not extreme

nrow(naoannext) # number of extreme observations
naoanngoodbad <- naoannext$RecDev > naoannnotextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(naoanngoodbad, na.rm=TRUE) / nrow(naoannext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


ba <- ggplot(data=nao, aes(x=Ann,y=RecDev)) +
  geom_vline(xintercept=naoanndiv, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,naoanndiv), y = c(naoannextmedrec,naoannextmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(naoanndiv, Inf), y = c(naoannnotextrec,naoannnotextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="NAO annual 25th percentile", x="Anomaly", y="Log recruitment deviation") +
  theme_bw() + 
  annotate("text", x=-0.75, y=0.5, label= "Median = 0.24
  SD = 0.52
  Good = 50%
  Poor = 50%",
           col="black", size=3) +
  annotate("text", x=0.75, y=0.8, label= "Median = 0.28
  SD = 0.52",
           col="black", size=3)
ba

#ggsave("NAOAnnual25th.png", width = 10, height = 8, dpi = 1000)




##################### seasonal second
naojunoctdiv <- quantile(nao$JunOct, probs = 0.25) # division
naojunoctext <- nao[nao$JunOct < naojunoctdiv,] # extreme dataset
naojunoctextmedrec <- median(naojunoctext$RecDev) # median recruitment of extreme
naojunoctextmedrecsd <- sd(naojunoctext$RecDev) # sd of extreme

naojunoctnotext <- nao[nao$JunOct > naojunoctdiv,] # not extreme dataset
naojunoctnotextrec <- median(naojunoctnotext$RecDev) # median recruitment of not extreme
naojunoctnotextrecsd <- sd(naojunoctnotext$RecDev) # sd of not extreme

nrow(naojunoctext) # number of extreme observations
naojunoctgoodbad <- naojunoctext$RecDev > naojunoctnotextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(naojunoctgoodbad, na.rm=TRUE) / nrow(naojunoctext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


bb <- ggplot(data=nao, aes(x=JunOct,y=RecDev)) +
  geom_vline(xintercept=naojunoctdiv, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,naojunoctdiv), y = c(naojunoctextmedrec,naojunoctextmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(naojunoctdiv, Inf), y = c(naojunoctnotextrec,naojunoctnotextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="NAO Jun-Oct 25th percentile", x="Anomaly", y="Log recruitment deviation") +
  theme_bw() + 
  annotate("text", x=-1.2, y=0.7, label= "Median = 0.66
  SD = 0.49
  Good = 67%
  Poor = 33%",
           col="black", size=3) +
  annotate("text", x=1, y=0.8, label= "Median = 0.25
  SD = 0.50",
           col="black", size=3)
bb

#ggsave("NAOJunOct25th.png", width = 10, height = 8, dpi = 1000)








################################################################################
### 30th percentile next
##################### annual first
naoanndiv <- quantile(nao$Ann, probs = 0.30) # division
naoannext <- nao[nao$Ann < naoanndiv,] # extreme dataset
naoannextmedrec <- median(naoannext$RecDev) # median recruitment of extreme
naoannextmedrecsd <- sd(naoannext$RecDev) # sd of extreme

naoannnotext <- nao[nao$Ann > naoanndiv,] # not extreme dataset
naoannnotextrec <- median(naoannnotext$RecDev) # median recruitment of not extreme
naoannnotextrecsd <- sd(naoannnotext$RecDev) # sd of not extreme

nrow(naoannext) # number of extreme observations
naoanngoodbad <- naoannext$RecDev > naoannnotextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(naoanngoodbad, na.rm=TRUE) / nrow(naoannext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


ca <- ggplot(data=nao, aes(x=Ann,y=RecDev)) +
  geom_vline(xintercept=naoanndiv, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,naoanndiv), y = c(naoannextmedrec,naoannextmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(naoanndiv, Inf), y = c(naoannnotextrec,naoannnotextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="NAO annual 30th percentile", x="Anomaly", y="Log recruitment deviation") +
  theme_bw() + 
  annotate("text", x=-0.75, y=0.5, label= "Median = 0.23
  SD = 0.48
  Good = 43%
  Poor = 57%",
           col="black", size=3) +
  annotate("text", x=0.75, y=0.8, label= "Median = 0.28
  SD = 0.53",
           col="black", size=3)
ca

#ggsave("NAOAnnual30th.png", width = 10, height = 8, dpi = 1000)




##################### seasonal second
naojunoctdiv <- quantile(nao$JunOct, probs = 0.30) # division
naojunoctext <- nao[nao$JunOct < naojunoctdiv,] # extreme dataset
naojunoctextmedrec <- median(naojunoctext$RecDev) # median recruitment of extreme
naojunoctextmedrecsd <- sd(naojunoctext$RecDev) # sd of extreme

naojunoctnotext <- nao[nao$JunOct > naojunoctdiv,] # not extreme dataset
naojunoctnotextrec <- median(naojunoctnotext$RecDev) # median recruitment of not extreme
naojunoctnotextrecsd <- sd(naojunoctnotext$RecDev) # sd of not extreme

nrow(naojunoctext) # number of extreme observations
naojunoctgoodbad <- naojunoctext$RecDev > naojunoctnotextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(naojunoctgoodbad, na.rm=TRUE) / nrow(naojunoctext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


cb <- ggplot(data=nao, aes(x=JunOct,y=RecDev)) +
  geom_vline(xintercept=naojunoctdiv, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,naojunoctdiv), y = c(naojunoctextmedrec,naojunoctextmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(naojunoctdiv, Inf), y = c(naojunoctnotextrec,naojunoctnotextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="NAO Jun-Oct 30th percentile", x="Anomaly", y="Log recruitment deviation") +
  theme_bw() + 
  annotate("text", x=-1.2, y=0.7, label= "Median = 0.69
  SD = 0.48
  Good = 71%
  Poor = 29%",
           col="black", size=3) +
  annotate("text", x=1, y=0.8, label= "Median = 0.23
  SD = 0.47",
           col="black", size=3)
cb

#ggsave("NAOJunOct30th.png", width = 10, height = 8, dpi = 1000)




################################################################################
### 33th percentile next
##################### annual first
naoanndiv <- quantile(nao$Ann, probs = 0.33) # division
naoannext <- nao[nao$Ann < naoanndiv,] # extreme dataset
naoannextmedrec <- median(naoannext$RecDev) # median recruitment of extreme
naoannextmedrecsd <- sd(naoannext$RecDev) # sd of extreme

naoannnotext <- nao[nao$Ann > naoanndiv,] # not extreme dataset
naoannnotextrec <- median(naoannnotext$RecDev) # median recruitment of not extreme
naoannnotextrecsd <- sd(naoannnotext$RecDev) # sd of not extreme

nrow(naoannext) # number of extreme observations
naoanngoodbad <- naoannext$RecDev > naoannnotextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(naoanngoodbad, na.rm=TRUE) / nrow(naoannext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


da <- ggplot(data=nao, aes(x=Ann,y=RecDev)) +
  geom_vline(xintercept=naoanndiv, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,naoanndiv), y = c(naoannextmedrec,naoannextmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(naoanndiv, Inf), y = c(naoannnotextrec,naoannnotextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="NAO annual 33th percentile", x="Anomaly", y="Log recruitment deviation") +
  theme_bw() + 
  annotate("text", x=-0.75, y=0.5, label= "Median = 0.25
  SD = 0.44
  Good = 38%
  Poor = 62%",
           col="black", size=3) +
  annotate("text", x=0.75, y=0.8, label= "Median = 0.30
  SD = 0.55",
           col="black", size=3)
da

#ggsave("NAOAnnual33th.png", width = 10, height = 8, dpi = 1000)




##################### seasonal second
naojunoctdiv <- quantile(nao$JunOct, probs = 0.33) # division
naojunoctext <- nao[nao$JunOct < naojunoctdiv,] # extreme dataset
naojunoctextmedrec <- median(naojunoctext$RecDev) # median recruitment of extreme
naojunoctextmedrecsd <- sd(naojunoctext$RecDev) # sd of extreme

naojunoctnotext <- nao[nao$JunOct > naojunoctdiv,] # not extreme dataset
naojunoctnotextrec <- median(naojunoctnotext$RecDev) # median recruitment of not extreme
naojunoctnotextrecsd <- sd(naojunoctnotext$RecDev) # sd of not extreme

nrow(naojunoctext) # number of extreme observations
naojunoctgoodbad <- naojunoctext$RecDev > naojunoctnotextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(naojunoctgoodbad, na.rm=TRUE) / nrow(naojunoctext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


db <- ggplot(data=nao, aes(x=JunOct,y=RecDev)) +
  geom_vline(xintercept=naojunoctdiv, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,naojunoctdiv), y = c(naojunoctextmedrec,naojunoctextmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(naojunoctdiv, Inf), y = c(naojunoctnotextrec,naojunoctnotextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="NAO Jun-Oct 33th percentile", x="Anomaly", y="Log recruitment deviation") +
  theme_bw() + 
  annotate("text", x=-1.2, y=0.7, label= "Median = 0.66
  SD = 0.46
  Good = 75%
  Poor = 25%",
           col="black", size=3) +
  annotate("text", x=1, y=0.8, label= "Median = 0.11
  SD = 0.49",
           col="black", size=3)
db

#ggsave("NAOJunOct33th.png", width = 10, height = 8, dpi = 1000)




################################################################################
### lastly, 40th percentile
##################### annual first
naoanndiv <- quantile(nao$Ann, probs = 0.40) # division
naoannext <- nao[nao$Ann < naoanndiv,] # extreme dataset
naoannextmedrec <- median(naoannext$RecDev) # median recruitment of extreme
naoannextmedrecsd <- sd(naoannext$RecDev) # sd of extreme

naoannnotext <- nao[nao$Ann > naoanndiv,] # not extreme dataset
naoannnotextrec <- median(naoannnotext$RecDev) # median recruitment of not extreme
naoannnotextrecsd <- sd(naoannnotext$RecDev) # sd of not extreme

nrow(naoannext) # number of extreme observations
naoanngoodbad <- naoannext$RecDev > naoannnotextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(naoanngoodbad, na.rm=TRUE) / nrow(naoannext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


ea <- ggplot(data=nao, aes(x=Ann,y=RecDev)) +
  geom_vline(xintercept=naoanndiv, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,naoanndiv), y = c(naoannextmedrec,naoannextmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(naoanndiv, Inf), y = c(naoannnotextrec,naoannnotextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="NAO annual 40th percentile", x="Anomaly", y="Log recruitment deviation") +
  theme_bw() + 
  annotate("text", x=-0.75, y=0.5, label= "Median = 0.25
  SD = 0.56
  Good = 40%
  Poor = 60%",
           col="black", size=3) +
  annotate("text", x=0.75, y=0.8, label= "Median = 0.30
  SD = 0.49",
           col="black", size=3)
ea

#ggsave("NAOAnnual40th.png", width = 10, height = 8, dpi = 1000)




##################### seasonal second
naojunoctdiv <- quantile(nao$JunOct, probs = 0.40) # division
naojunoctext <- nao[nao$JunOct < naojunoctdiv,] # extreme dataset
naojunoctextmedrec <- median(naojunoctext$RecDev) # median recruitment of extreme
naojunoctextmedrecsd <- sd(naojunoctext$RecDev) # sd of extreme

naojunoctnotext <- nao[nao$JunOct > naojunoctdiv,] # not extreme dataset
naojunoctnotextrec <- median(naojunoctnotext$RecDev) # median recruitment of not extreme
naojunoctnotextrecsd <- sd(naojunoctnotext$RecDev) # sd of not extreme

nrow(naojunoctext) # number of extreme observations
naojunoctgoodbad <- naojunoctext$RecDev > naojunoctnotextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(naojunoctgoodbad, na.rm=TRUE) / nrow(naojunoctext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


eb <- ggplot(data=nao, aes(x=JunOct,y=RecDev)) +
  geom_vline(xintercept=naojunoctdiv, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,naojunoctdiv), y = c(naojunoctextmedrec,naojunoctextmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(naojunoctdiv, Inf), y = c(naojunoctnotextrec,naojunoctnotextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="NAO Jun-Oct 40th percentile", x="Anomaly", y="Log recruitment deviation") +
  theme_bw() + 
  annotate("text", x=-1.2, y=0.7, label= "Median = 0.64
  SD = 0.41
  Good = 90%
  Poor = 10%",
           col="black", size=3) +
  annotate("text", x=1, y=0.8, label= "Median = -0.05
  SD = 0.49",
           col="black", size=3)
eb

#ggsave("NAOJunOct40th.png", width = 10, height = 8, dpi = 1000)







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

#ggsave("NAO_PRP_2000to2023.png", width = 8, height = 10, dpi = 1000)








