### PRP for top 3 habitat variables using max values for jun-oct
library(here)

# load rec devs and datasets
recdevs <- read.csv(here("ALSPTRecDevs.csv"))
recdevs


library(readxl)
deltajunoctmax <- read_excel(here("Habitat","Dec 2025 files","Delta_salty_area_temp_Jun_Oct_max.xlsx"))
head(deltajunoctmax)


## crops recdevs to match anomalies
recdevs <- recdevs[recdevs$Year > 1999 & recdevs$Year < 2024,]
recdevs

### crop anomalies to match recdev years
deltajunoctmax <- deltajunoctmax[deltajunoctmax$Year > 1999,]
summary(deltajunoctmax$Year)

## add recdevs to each anomaly dataset
deltajunoctmax$rec <- recdevs$Deviation

# look at column names
colnames(deltajunoctmax)

deltajunoctmax <- deltajunoctmax[ , -c(2, 4,6,8,16,18,20,22)]
colnames(deltajunoctmax)


### rename columns
colnames(deltajunoctmax) <- c("Year","A1","A2","A3","A4","A5","A6","A7","A8","A9","A10",
                              "T1","T2","T3","T4","T5","T6","T7","T8","T9","T10",
                              "RecDev")

colnames(deltajunoctmax)

###############################################################################
### repeat for Mobile Bay
mbjunoctmax <- read_excel(here("Habitat","Dec 2025 files","MB_salty_area_temp_Jun_Oct_max.xlsx"))
head(mbjunoctmax)


### crop anomalies to match recdev years
mbjunoctmax <- mbjunoctmax[mbjunoctmax$Year > 1999,]
summary(mbjunoctmax$Year)

## add recdevs to each anomaly dataset
mbjunoctmax$rec <- recdevs$Deviation


# look at column names
colnames(mbjunoctmax)
mbjunoctmax <- mbjunoctmax[ , -c(2, 4,6,8,16,18,20,22)]
colnames(mbjunoctmax)


### rename columns
colnames(mbjunoctmax) <- c("Year","A1","A2","A3","A4","A5","A6","A7","A8","A9","A10",
                           "T1","T2","T3","T4","T5","T6","T7","T8","T9","T10",
                           "RecDev")

colnames(mbjunoctmax)





################################################################################
###############################################################################
################################################################################
################################################################################
########### poor recruitment paradigm plots ####################################
setwd(here("Habitat","PRP temp in habitat","33rd percentile"))
library(ggplot2)

##################### Delta first
### first temp in area w slainity greater or equal to 1psu
deltat1div <- quantile(deltajunoctmax$T1, probs = 0.33) # division
deltat1ext <- deltajunoctmax[deltajunoctmax$T1 < deltat1div,] # extreme dataset
deltat1extmedrec <- median(deltat1ext$RecDev) # median recruitment of extreme
deltat1extmedrecsd <- sd(deltat1ext$RecDev) # sd of extreme

deltat1notext <- deltajunoctmax[deltajunoctmax$T1 > deltat1div,] # not extreme dataset
deltat1notextrec <- median(deltat1notext$RecDev) # median recruitment of not extreme
deltat1notextrecsd <- sd(deltat1notext$RecDev) # sd of not extreme

nrow(deltat1ext) # number of extreme observations
deltat1goodbad <- deltat1ext$RecDev > deltat1notextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(deltat1goodbad, na.rm=TRUE) / nrow(deltat1ext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


aa <- ggplot(data=deltajunoctmax, aes(x=T1,y=RecDev)) +
  geom_vline(xintercept=deltat1div, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,deltat1div), y = c(deltat1extmedrec,deltat1extmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(deltat1div, Inf), y = c(deltat1notextrec,deltat1notextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="MTD area with salinity ??? 1 PSU", x="MTD max temperature in area with salinity ??? 1 PSU", y="Log recruitment deviation") +
  theme_bw() +
  annotate("text", x=30, y=0.8, label= "Median = -0.15
  SD = 0.39
  Good = 0%
  Poor = 100%",
           col="black", size=3) +
  annotate("text", x=33, y=-0.5, label= "Median = 0.55
  SD = 0.45",
           col="black", size=3)
aa

#ggsave("PRP_MTD_T1.png", width = 10, height = 8, dpi = 1000)




##########################################################
### second, temp in area w slainity greater or equal to 2psu
deltat2div <- quantile(deltajunoctmax$T2, probs = 0.33) # division
deltat2ext <- deltajunoctmax[deltajunoctmax$T2 < deltat2div,] # extreme dataset
deltat2extmedrec <- median(deltat2ext$RecDev) # median recruitment of extreme
deltat2extmedrecsd <- sd(deltat2ext$RecDev) # sd of extreme

deltat2notext <- deltajunoctmax[deltajunoctmax$T2 > deltat2div,] # not extreme dataset
deltat2notextrec <- median(deltat2notext$RecDev) # median recruitment of not extreme
deltat2notextrecsd <- sd(deltat2notext$RecDev) # sd of not extreme

nrow(deltat2ext) # number of extreme observations
deltat2goodbad <- deltat2ext$RecDev > deltat2notextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(deltat2goodbad, na.rm=TRUE) / nrow(deltat2ext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


ba <- ggplot(data=deltajunoctmax, aes(x=T2,y=RecDev)) +
  geom_vline(xintercept=deltat2div, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,deltat2div), y = c(deltat2extmedrec,deltat2extmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(deltat2div, Inf), y = c(deltat2notextrec,deltat2notextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="MTD area with salinity ??? 2 PSU", x="MTD max temperature in area with salinity ??? 2 PSU", y="Log recruitment deviation") +
  theme_bw() +
  annotate("text", x=30, y=0.8, label= "Median = -0.14
  SD = 0.40
  Good = 0%
  Poor = 100%",
           col="black", size=3) +
  annotate("text", x=33, y=-0.5, label= "Median = 0.55
  SD = 0.46",
           col="black", size=3)
ba

#ggsave("PRP_MTD_T2.png", width = 10, height = 8, dpi = 1000)




##########################################################
### third, temp in area w slainity greater or equal to 3psu
deltat3div <- quantile(deltajunoctmax$T3, probs = 0.33) # division
deltat3ext <- deltajunoctmax[deltajunoctmax$T3 < deltat3div,] # extreme dataset
deltat3extmedrec <- median(deltat3ext$RecDev) # median recruitment of extreme
deltat3extmedrecsd <- sd(deltat3ext$RecDev) # sd of extreme

deltat3notext <- deltajunoctmax[deltajunoctmax$T3 > deltat3div,] # not extreme dataset
deltat3notextrec <- median(deltat3notext$RecDev) # median recruitment of not extreme
deltat3notextrecsd <- sd(deltat3notext$RecDev) # sd of not extreme

nrow(deltat3ext) # number of extreme observations
deltat3goodbad <- deltat3ext$RecDev > deltat3notextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(deltat3goodbad, na.rm=TRUE) / nrow(deltat3ext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


ca <- ggplot(data=deltajunoctmax, aes(x=T3,y=RecDev)) +
  geom_vline(xintercept=deltat3div, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,deltat3div), y = c(deltat3extmedrec,deltat3extmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(deltat3div, Inf), y = c(deltat3notextrec,deltat3notextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="MTD area with salinity ??? 3 PSU", x="MTD max temperature in area with salinity ??? 3 PSU", y="Log recruitment deviation") +
  theme_bw() +
  annotate("text", x=30, y=0.8, label= "Median = -0.08
  SD = 0.42
  Good = 0%
  Poor = 100%",
           col="black", size=3) +
  annotate("text", x=33, y=-0.5, label= "Median = 0.55
  SD = 0.48",
           col="black", size=3)
ca

#ggsave("PRP_MTD_T3.png", width = 10, height = 8, dpi = 1000)






############################################################################
### rerun for mobile bay :)
### first temp in area w slainity greater or equal to 1psu
mbt1div <- quantile(mbjunoctmax$T1, probs = 0.33) # division
mbt1ext <- mbjunoctmax[mbjunoctmax$T1 < mbt1div,] # extreme dataset
mbt1extmedrec <- median(mbt1ext$RecDev) # median recruitment of extreme
mbt1extmedrecsd <- sd(mbt1ext$RecDev) # sd of extreme

mbt1notext <- mbjunoctmax[mbjunoctmax$T1 > mbt1div,] # not extreme dataset
mbt1notextrec <- median(mbt1notext$RecDev) # median recruitment of not extreme
mbt1notextrecsd <- sd(mbt1notext$RecDev) # sd of not extreme

nrow(mbt1ext) # number of extreme observations
mbt1goodbad <- mbt1ext$RecDev > mbt1notextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(mbt1goodbad, na.rm=TRUE) / nrow(mbt1ext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


ab <- ggplot(data=mbjunoctmax, aes(x=T1,y=RecDev)) +
  geom_vline(xintercept=mbt1div, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,mbt1div), y = c(mbt1extmedrec,mbt1extmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(mbt1div, Inf), y = c(mbt1notextrec,mbt1notextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="MB area with salinity ??? 1 PSU", x="Mobile Bay max temperature in area with salinity ??? 1 PSU", y="Log recruitment deviation") +
  theme_bw() +
  annotate("text", x=31.25, y=0.8, label= "Median = 0.07
  SD = 0.51
  Good = 38%
  Poor = 62%",
           col="black", size=3) +
  annotate("text", x=33, y=-0.5, label= "Median = 0.30
  SD = 0.49",
           col="black", size=3)
ab

#ggsave("PRP_MB_T1.png", width = 10, height = 8, dpi = 1000)




##########################################################
### second, temp in area w slainity greater or equal to 2psu
mbt2div <- quantile(mbjunoctmax$T2, probs = 0.33) # division
mbt2ext <- mbjunoctmax[mbjunoctmax$T2 < mbt2div,] # extreme dataset
mbt2extmedrec <- median(mbt2ext$RecDev) # median recruitment of extreme
mbt2extmedrecsd <- sd(mbt2ext$RecDev) # sd of extreme

mbt2notext <- mbjunoctmax[mbjunoctmax$T2 > mbt2div,] # not extreme dataset
mbt2notextrec <- median(mbt2notext$RecDev) # median recruitment of not extreme
mbt2notextrecsd <- sd(mbt2notext$RecDev) # sd of not extreme

nrow(mbt2ext) # number of extreme observations
mbt2goodbad <- mbt2ext$RecDev > mbt2notextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(mbt2goodbad, na.rm=TRUE) / nrow(mbt2ext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


bb <- ggplot(data=mbjunoctmax, aes(x=T2,y=RecDev)) +
  geom_vline(xintercept=mbt2div, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,mbt2div), y = c(mbt2extmedrec,mbt2extmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(mbt2div, Inf), y = c(mbt2notextrec,mbt2notextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="MB area with salinity ??? 2 PSU", x="Mobile Bay max temperature in area with salinity ??? 2 PSU", y="Log recruitment deviation") +
  theme_bw() +
  annotate("text", x=31.25, y=0.8, label= "Median = 0.07
  SD = 0.51
  Good = 38%
  Poor = 62%",
           col="black", size=3) +
  annotate("text", x=33, y=-0.5, label= "Median = 0.30
  SD = 0.49",
           col="black", size=3)
bb

#ggsave("PRP_MB_T2.png", width = 10, height = 8, dpi = 1000)




##########################################################
### third, temp in area w slainity greater or equal to 3psu
mbt3div <- quantile(mbjunoctmax$T3, probs = 0.33) # division
mbt3ext <- mbjunoctmax[mbjunoctmax$T3 < mbt3div,] # extreme dataset
mbt3extmedrec <- median(mbt3ext$RecDev) # median recruitment of extreme
mbt3extmedrecsd <- sd(mbt3ext$RecDev) # sd of extreme

mbt3notext <- mbjunoctmax[mbjunoctmax$T3 > mbt3div,] # not extreme dataset
mbt3notextrec <- median(mbt3notext$RecDev) # median recruitment of not extreme
mbt3notextrecsd <- sd(mbt3notext$RecDev) # sd of not extreme

nrow(mbt3ext) # number of extreme observations
mbt3goodbad <- mbt3ext$RecDev > mbt3notextrec # logical vector indicating
# extreme recruitment values above/below median recruitment of not extreme 
sum(mbt3goodbad, na.rm=TRUE) / nrow(mbt3ext) # proportion of recruitment
# values during extreme conditions above median recruitment during not extreme


cb <- ggplot(data=mbjunoctmax, aes(x=T3,y=RecDev)) +
  geom_vline(xintercept=mbt3div, col="black", lty=2, size=2) + #66th percentile
  geom_point(col="gray30",size=3) +
  geom_line(data = data.frame(x = c(-Inf,mbt3div), y = c(mbt3extmedrec,mbt3extmedrec)), aes(x = x , y = y,),
            col="maroon", size=1) +
  geom_line(data = data.frame(x = c(mbt3div, Inf), y = c(mbt3notextrec,mbt3notextrec)), aes(x = x , y = y,),
            col="steelblue", size=1) +
  labs(title="MB area with salinity ??? 3 PSU", x="Mobile Bay max temperature in area with salinity ??? 3 PSU", y="Log recruitment deviation") +
  theme_bw() +
  annotate("text", x=31.25, y=0.8, label= "Median = 0.07
  SD = 0.51
  Good = 38%
  Poor = 62%",
           col="black", size=3) +
  annotate("text", x=33, y=-0.5, label= "Median = 0.30
  SD = 0.49",
           col="black", size=3)
cb

#ggsave("PRP_MB_T3.png", width = 10, height = 8, dpi = 1000)

















######### merging all PRP plots together :)))
# first remove their axes
aa <- aa + xlab(NULL) + ylab(NULL)
ab <- ab + xlab(NULL) + ylab(NULL)
ba <- ba + xlab(NULL) + ylab(NULL)
bb <- bb + xlab(NULL) + ylab(NULL)
ca <- ca + xlab(NULL) + ylab(NULL)
cb <- cb + xlab(NULL) + ylab(NULL)

library(ggpubr)
library(grid)

prpfinal <- ggarrange(aa,ab,ba,bb,ca,cb, ncol = 2, nrow = 3,
                   label.x = c(0.85, 0.85),  # the x positions of the labels
                   label.y = c(1, 1),
                   font.label = list(size = 30))  # t)
prpfinal
annotate_figure(prpfinal, bottom = textGrob("Max temperature (°C)", vjust = 0),
                left = textGrob("Log recruitment deviation", vjust = 0, rot=90))

#ggsave("PRP_Merged33rd.png", width = 8, height = 10, dpi = 1000)



