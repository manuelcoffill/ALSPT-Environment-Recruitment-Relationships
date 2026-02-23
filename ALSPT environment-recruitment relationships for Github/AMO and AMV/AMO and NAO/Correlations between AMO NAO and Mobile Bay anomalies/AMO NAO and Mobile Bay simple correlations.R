#### correlations between AMO, NAO and Mobile bay anomalies
library(here)
library(readxl)

### AMO 
# upload dataset
amo <- read_excel(here("AMO and AMV","AMO and NAO","AMO detrended unsmoothed.xlsx"))

# truncated dataset to match rec devs
amo <- amo[amo$Year > 1999 & amo$Year < 2023,] # 2022 is last full year w data
summary(amo$Year)


### NAO 
# upload dataset
nao <- read_excel(here("AMO and AMV","AMO and NAO","Monthly mean NAO index.xlsx"))

# truncated dataset to match rec devs
nao <- nao[nao$Year > 1999 & nao$Year < 2024,]
summary(nao$Year)


############ add upper and lower
upper <- read_excel(here("Upper_T_S_anomaly_annual_Jun-Oct.xlsx"))
upper

lower <- read_excel(here("Lower_T_S_anomaly_annual_Jun-Oct.xlsx"))
lower



### crop anomalies to match recdev years
uppernao <- upper[upper$Year > 1999,]
summary(uppernao$Year)

lowernao <- lower[lower$Year > 1999,]
summary(lowernao$Year)



############## need to make extra copy of upper and lower that filters out 2023 to match amo
### then, add annnual and seasonal AMO and NAO to their respective upper and lower dfs...
upperamo <- uppernao[uppernao$Year < 2023,]
summary(upperamo$Year)

loweramo <- lowernao[uppernao$Year < 2023,]
summary(loweramo$Year)






## add AMO and NAO to each anomaly dataset
## adding 2 columns from each index, the annual and seasonal
upperamo$amoann <- amo$Annual
upperamo$amojunoct <- amo$`Jun-Oct`
loweramo$amoann <- amo$Annual
loweramo$amojunoct <- amo$`Jun-Oct`


uppernao$naoann <- nao$Annual
uppernao$naojunoct <- nao$`Jun-Oct`
lowernao$naoann <- nao$Annual
lowernao$naojunoct <- nao$`Jun-Oct`


# rename columns
colnames(upperamo) <- c("Year","AnnT","AnnS","JunOctT","JunOctS","AMOann","AMOjunoct")
head(upperamo)
colnames(loweramo) <- c("Year","AnnT","AnnS","JunOctT","JunOctS","AMOann","AMOjunoct")
head(loweramo)

colnames(uppernao) <- c("Year","AnnT","AnnS","JunOctT","JunOctS","NAOann","NAOjunoct")
head(uppernao)
colnames(lowernao) <- c("Year","AnnT","AnnS","JunOctT","JunOctS","NAOann","NAOjunoct")
head(lowernao)

### datasets ready for correlations :)





################################################################################
###############################################################################
################################################################################
############# SPEARMAN CORRELATION #############################################
################################################################################
library(ggplot2)

## AMO annual first
# temps
cor.test(upperamo$AMOann, upperamo$AnnT, method = 'spearman')
# no correlation
aa <- ggplot(upperamo, aes(AMOann, AnnT)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="UMB annual bottom temperature")
aa

cor.test(loweramo$AMOann, loweramo$AnnT, method = 'spearman')
# no correlation
ab <- ggplot(loweramo, aes(AMOann, AnnT)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="LMB annual bottom temperature")
ab

cor.test(upperamo$AMOann, upperamo$JunOctT, method = 'spearman')
# no correlation
ac <- ggplot(upperamo, aes(AMOann, JunOctT)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="UMB Jun-Oct bottom temperature")
ac

cor.test(loweramo$AMOann, loweramo$JunOctT, method = 'spearman')
# no correlation
ad <- ggplot(loweramo, aes(AMOann, JunOctT)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="LMB Jun-Oct bottom temperature")
ad

# sals
cor.test(upperamo$AMOann, upperamo$AnnS, method = 'spearman')
# no correlation
ae <- ggplot(upperamo, aes(AMOann, AnnS)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="UMB annual bottom salinity")
ae

cor.test(loweramo$AMOann, loweramo$AnnS, method = 'spearman')
# no correlation
af <- ggplot(loweramo, aes(AMOann, AnnS)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="LMB annual bottom salinity")
af

cor.test(upperamo$AMOann, upperamo$JunOctS, method = 'spearman')
# no correlation
ag <- ggplot(upperamo, aes(AMOann, JunOctS)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="UMB Jun-Oct bottom salinity")
ag

cor.test(loweramo$AMOann, loweramo$JunOctS, method = 'spearman')
# no correlation
ah <- ggplot(loweramo, aes(AMOann, JunOctS)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="LMB Jun-Oct bottom salinity")
ah


###############################################################################
######### merging AMO anual plots together :)))
# first remove their axes
aa <- aa + xlab(NULL) + ylab(NULL)
ab <- ab + xlab(NULL) + ylab(NULL)
ac <- ac + xlab(NULL) + ylab(NULL)
ad <- ad + xlab(NULL) + ylab(NULL)
ae <- ae + xlab(NULL) + ylab(NULL)
af <- af + xlab(NULL) + ylab(NULL)
ag <- ag + xlab(NULL) + ylab(NULL)
ah <- ah + xlab(NULL) + ylab(NULL)

library(ggpubr)
library(grid)

amoannfinal <- ggarrange(aa,ab,ac,ad,ae,af,ag,ah, ncol = 2, nrow = 4,
                      label.x = c(0.85, 0.85),  # the x positions of the labels
                      label.y = c(1, 1),
                      font.label = list(size = 30))  # t)
amoannfinal
annotate_figure(amoannfinal, bottom = textGrob("AMO annual anomaly", vjust = 0),
                left = textGrob("Anomaly", vjust = 0, rot=90))

#ggsave("ALSPT_AMOannual_MB_Correlations.png", width = 8, height = 10, dpi = 1000)
###############################################################################





## AMO seasonal second
# temps
cor.test(upperamo$AMOjunoct, upperamo$AnnT, method = 'spearman')
# no correlation
ba <- ggplot(upperamo, aes(AMOjunoct, AnnT)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="UMB annual bottom temperature")
ba

cor.test(loweramo$AMOjunoct, loweramo$AnnT, method = 'spearman')
# no correlation
bb <- ggplot(loweramo, aes(AMOjunoct, AnnT)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="LMB annual bottom temperature")
bb

cor.test(upperamo$AMOjunoct, upperamo$JunOctT, method = 'spearman')
# no correlation
bc <- ggplot(upperamo, aes(AMOjunoct, JunOctT)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="UMB Jun-Oct bottom temperature")
bc

cor.test(loweramo$AMOjunoct, loweramo$JunOctT, method = 'spearman')
# no correlation
bd <- ggplot(loweramo, aes(AMOjunoct, JunOctT)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="LMB Jun-Oct bottom temperature")
bd

# sals
cor.test(upperamo$AMOjunoct, upperamo$AnnS, method = 'spearman')
# no correlation
be <- ggplot(upperamo, aes(AMOjunoct, AnnS)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="UMB annual bottom salinity")
be

cor.test(loweramo$AMOjunoct, loweramo$AnnS, method = 'spearman')
# no correlation
bf <- ggplot(loweramo, aes(AMOjunoct, AnnS)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="LMB annual bottom salinity")
bf

cor.test(upperamo$AMOjunoct, upperamo$JunOctS, method = 'spearman')
# no correlation
bg <- ggplot(upperamo, aes(AMOjunoct, JunOctS)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="UMB Jun-Oct bottom salinity")
bg

cor.test(loweramo$AMOjunoct, loweramo$JunOctS, method = 'spearman')
# no correlation
bh <- ggplot(loweramo, aes(AMOjunoct, JunOctS)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="LMB Jun-Oct bottom salinity")
bh


###############################################################################
######### merging AMO seasonal plots together :)))
# first remove their axes
ba <- ba + xlab(NULL) + ylab(NULL)
bb <- bb + xlab(NULL) + ylab(NULL)
bc <- bc + xlab(NULL) + ylab(NULL)
bd <- bd + xlab(NULL) + ylab(NULL)
be <- be + xlab(NULL) + ylab(NULL)
bf <- bf + xlab(NULL) + ylab(NULL)
bg <- bg + xlab(NULL) + ylab(NULL)
bh <- bh + xlab(NULL) + ylab(NULL)


amojunoctfinal <- ggarrange(ba,bb,bc,bd,be,bf,bg,bh, ncol = 2, nrow = 4,
                         label.x = c(0.85, 0.85),  # the x positions of the labels
                         label.y = c(1, 1),
                         font.label = list(size = 30))  # t)
amojunoctfinal
annotate_figure(amojunoctfinal, bottom = textGrob("AMO Jun-Oct anomaly", vjust = 0),
                left = textGrob("Anomaly", vjust = 0, rot=90))

#ggsave("ALSPT_AMOjunoct_MB_Correlations.png", width = 8, height = 10, dpi = 1000)
################################################################################







################# Repeat the above for the NAO anomalies :)
## NAO annual first
# temps
cor.test(uppernao$NAOann, uppernao$AnnT, method = 'spearman')
# no correlation
ca <- ggplot(uppernao, aes(NAOann, AnnT)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="UMB annual bottom temperature")
ca

cor.test(lowernao$NAOann, lowernao$AnnT, method = 'spearman')
# no correlation
cb <- ggplot(lowernao, aes(NAOann, AnnT)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="LMB annual bottom temperature")
cb

cor.test(uppernao$NAOann, uppernao$JunOctT, method = 'spearman')
# no correlation
cc <- ggplot(uppernao, aes(NAOann, JunOctT)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="UMB Jun-Oct bottom temperature")
cc

cor.test(lowernao$NAOann, lowernao$JunOctT, method = 'spearman')
# no correlation
cd <- ggplot(lowernao, aes(NAOann, JunOctT)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="LMB Jun-Oct bottom temperature")
cd

# sals
cor.test(uppernao$NAOann, uppernao$AnnS, method = 'spearman')
# no correlation
ce <- ggplot(uppernao, aes(NAOann, AnnS)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="UMB annual bottom salinity")
ce

cor.test(lowernao$NAOann, lowernao$AnnS, method = 'spearman')
# no correlation
cf <- ggplot(lowernao, aes(NAOann, AnnS)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="LMB annual bottom salinity")
cf

cor.test(uppernao$NAOann, uppernao$JunOctS, method = 'spearman')
# no correlation
cg <- ggplot(uppernao, aes(NAOann, JunOctS)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="UMB Jun-Oct bottom salinity")
cg

cor.test(lowernao$NAOann, lowernao$JunOctS, method = 'spearman')
# no correlation
ch <- ggplot(lowernao, aes(NAOann, JunOctS)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="LMB Jun-Oct bottom salinity")
ch


###############################################################################
######### merging NAO anual plots together :)))
# first remove their axes
ca <- ca + xlab(NULL) + ylab(NULL)
cb <- cb + xlab(NULL) + ylab(NULL)
cc <- cc + xlab(NULL) + ylab(NULL)
cd <- cd + xlab(NULL) + ylab(NULL)
ce <- ce + xlab(NULL) + ylab(NULL)
cf <- cf + xlab(NULL) + ylab(NULL)
cg <- cg + xlab(NULL) + ylab(NULL)
ch <- ch + xlab(NULL) + ylab(NULL)

naoannfinal <- ggarrange(ca,cb,cc,cd,ce,cf,cg,ch, ncol = 2, nrow = 4,
                         label.x = c(0.85, 0.85),  # the x positions of the labels
                         label.y = c(1, 1),
                         font.label = list(size = 30))  # t)
naoannfinal
annotate_figure(naoannfinal, bottom = textGrob("NAO annual anomaly", vjust = 0),
                left = textGrob("Anomaly", vjust = 0, rot=90))

#ggsave("ALSPT_NAOannual_MB_Correlations.png", width = 8, height = 10, dpi = 1000)
###############################################################################





## NAO seasonal second
# temps
cor.test(uppernao$NAOjunoct, uppernao$AnnT, method = 'spearman')
# no correlation
da <- ggplot(uppernao, aes(NAOjunoct, AnnT)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="UMB annual bottom temperature")
da

cor.test(lowernao$NAOjunoct, lowernao$AnnT, method = 'spearman')
# no correlation
db <- ggplot(lowernao, aes(NAOjunoct, AnnT)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="LMB annual bottom temperature")
db

cor.test(uppernao$NAOjunoct, uppernao$JunOctT, method = 'spearman')
# no correlation
dc <- ggplot(uppernao, aes(NAOjunoct, JunOctT)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="UMB Jun-Oct bottom temperature")
dc

cor.test(lowernao$NAOjunoct, lowernao$JunOctT, method = 'spearman')
# no correlation
dd <- ggplot(lowernao, aes(NAOjunoct, JunOctT)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="LMB Jun-Oct bottom temperature")
dd

# sals
cor.test(uppernao$NAOjunoct, uppernao$AnnS, method = 'spearman')
# no correlation
de <- ggplot(uppernao, aes(NAOjunoct, AnnS)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="UMB annual bottom salinity")
de

cor.test(lowernao$NAOjunoct, lowernao$AnnS, method = 'spearman')
# no correlation
df <- ggplot(lowernao, aes(NAOjunoct, AnnS)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="LMB annual bottom salinity")
df

cor.test(uppernao$NAOjunoct, uppernao$JunOctS, method = 'spearman')
# no correlation
dg <- ggplot(uppernao, aes(NAOjunoct, JunOctS)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="UMB Jun-Oct bottom salinity")
dg

cor.test(lowernao$NAOjunoct, lowernao$JunOctS, method = 'spearman')
# no correlation
dh <- ggplot(lowernao, aes(NAOjunoct, JunOctS)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="LMB Jun-Oct bottom salinity")
dh


###############################################################################
######### merging NAO seasonal plots together :)))
# first remove their axes
da <- da + xlab(NULL) + ylab(NULL)
db <- db + xlab(NULL) + ylab(NULL)
dc <- dc + xlab(NULL) + ylab(NULL)
dd <- dd + xlab(NULL) + ylab(NULL)
de <- de + xlab(NULL) + ylab(NULL)
df <- df + xlab(NULL) + ylab(NULL)
dg <- dg + xlab(NULL) + ylab(NULL)
dh <- dh + xlab(NULL) + ylab(NULL)


naojunoctfinal <- ggarrange(da,db,dc,dd,de,df,dg,dh, ncol = 2, nrow = 4,
                            label.x = c(0.85, 0.85),  # the x positions of the labels
                            label.y = c(1, 1),
                            font.label = list(size = 30))  # t)
naojunoctfinal
annotate_figure(naojunoctfinal, bottom = textGrob("NAO Jun-Oct anomaly", vjust = 0),
                left = textGrob("Anomaly", vjust = 0, rot=90))

#ggsave("ALSPT_NAOjunoct_MB_Correlations.png", width = 8, height = 10, dpi = 1000)
################################################################################



################################################################################




################################################################################
## NEED TO DO CORRELATION RESULT TABLES !!!!!!!!!!!
### simplified pearson correlation plots
library(dplyr)
library(purrr)
library(broom)



### make df to store results
cordf <- matrix(NA, nrow = 8, ncol = 4)
cordf <- data.frame(cordf)
cordf
colnames(cordf) <- c("AMO annual","AMO Jun-Oct", "NAO annual","NAO Jun-Oct")
rownames(cordf) <- c("MTD annual t","MTD annual s",
                     "MB annual t","MB annual s",
                     "MTD Jun-Oct t","MTD Jun-Oct s",
                     "MB Jun-Oct t","MB Jun-Oct s")
cordf
# ready to store values


#### correlations and Pvals


#### AMO annual 1st
## upper annual t
cor.test(upperamo$AMOann, upperamo$AnnT, method = 'pearson')
cordf[1,1] <- "0.12 (P = 0.58)"
cordf

## upper annual s
cor.test(upperamo$AMOann, upperamo$AnnS, method = 'pearson')
cordf[2,1] <- "-0.06 (P = 0.36)"
cordf

## upper seasonal t
cor.test(upperamo$AMOann, upperamo$JunOctT, method = 'pearson')
cordf[5,1] <- "0.07 (P = 0.74)"
cordf

## upper seasonal s
cor.test(upperamo$AMOann, upperamo$JunOctS, method = 'pearson')
cordf[6,1] <- "-0.08 (P = 0.70)"
cordf

## lower annual t
cor.test(loweramo$AMOann, loweramo$AnnT, method = 'pearson')
cordf[3,1] <- "0.17 (P = 0.44)"
cordf

## lower annual s
cor.test(loweramo$AMOann, loweramo$AnnS, method = 'pearson')
cordf[4,1] <- "-0.15 (P = 0.48)"
cordf

## lower seasonal t
cor.test(loweramo$AMOann, loweramo$JunOctT, method = 'pearson')
cordf[7,1] <- "0.09 (P = 0.69)"
cordf

## lower seasonal s
cor.test(loweramo$AMOann, loweramo$JunOctS, method = 'pearson')
cordf[8,1] <- "-0.17 (P = 0.42)"
cordf


##### AMO seasonal next
## upper annual t
cor.test(upperamo$AMOjunoct, upperamo$AnnT, method = 'pearson')
cordf[1,2] <- "0.03 (P = 0.90)"
cordf

## upper annual s
cor.test(upperamo$AMOjunoct, upperamo$AnnS, method = 'pearson')
cordf[2,2] <- "-0.18 (P = 0.41)"
cordf

## upper seasonal t
cor.test(upperamo$AMOjunoct, upperamo$JunOctT, method = 'pearson')
cordf[5,2] <- "-0.01 (P = 0.96)"
cordf

## upper seasonal s
cor.test(upperamo$AMOjunoct, upperamo$JunOctS, method = 'pearson')
cordf[6,2] <- "-0.17 (P = 0.44)"
cordf

## lower annual t
cor.test(loweramo$AMOjunoct, loweramo$AnnT, method = 'pearson')
cordf[3,2] <- "0.08 (P = 0.73)"
cordf

## lower annual s
cor.test(loweramo$AMOjunoct, loweramo$AnnS, method = 'pearson')
cordf[4,2] <- "-0.32 (P = 0.14)"
cordf

## lower seasonal t
cor.test(loweramo$AMOjunoct, loweramo$JunOctT, method = 'pearson')
cordf[7,2] <- "0.00 (P = 0.99)"
cordf

## lower seasonal s
cor.test(loweramo$AMOjunoct, loweramo$JunOctS, method = 'pearson')
cordf[8,2] <- "-0.29 (P = 0.18)"
cordf




###### NAO next
#### NAO annual 1st
## upper annual t
cor.test(uppernao$NAOann, uppernao$AnnT, method = 'pearson')
cordf[1,3] <- "0.17 (P = 0.44)"
cordf

## upper annual s
cor.test(uppernao$NAOann, uppernao$AnnS, method = 'pearson')
cordf[2,3] <- "-0.11 (P = 0.60)"
cordf

## upper seasonal t
cor.test(uppernao$NAOann, uppernao$JunOctT, method = 'pearson')
cordf[5,3] <- "0.08 (P = 0.71)"
cordf

## upper seasonal s
cor.test(uppernao$NAOann, uppernao$JunOctS, method = 'pearson')
cordf[6,3] <- "-0.06 (P = 0.78)"
cordf

## lower annual t
cor.test(lowernao$NAOann, lowernao$AnnT, method = 'pearson')
cordf[3,3] <- "0.22 (P = 0.30)"
cordf

## lower annual s
cor.test(lowernao$NAOann, lowernao$AnnS, method = 'pearson')
cordf[4,3] <- "-0.05 (P = 0.82)"
cordf

## lower seasonal t
cor.test(lowernao$NAOann, lowernao$JunOctT, method = 'pearson')
cordf[7,3] <- "0.09 (P = 0.66)"
cordf

## lower seasonal s
cor.test(lowernao$NAOann, lowernao$JunOctS, method = 'pearson')
cordf[8,3] <- "-0.01 (P = 0.95)"
cordf


##### NAO seasonal next
## upper annual t
cor.test(uppernao$NAOjunoct, uppernao$AnnT, method = 'pearson')
cordf[1,4] <- "-0.26 (P = 0.22)"
cordf

## upper annual s
cor.test(uppernao$NAOjunoct, uppernao$AnnS, method = 'pearson')
cordf[2,4] <- "-0.26 (P = 0.22)"
cordf

## upper seasonal t
cor.test(uppernao$NAOjunoct, uppernao$JunOctT, method = 'pearson')
cordf[5,4] <- "-0.11 (P = 0.61)"
cordf

## upper seasonal s
cor.test(uppernao$NAOjunoct, uppernao$JunOctS, method = 'pearson')
cordf[6,4] <- "-0.31 (P = 0.14)"
cordf

## lower annual t
cor.test(lowernao$NAOjunoct, lowernao$AnnT, method = 'pearson')
cordf[3,4] <- "-0.23 (P = 0.28)"
cordf

## lower annual s
cor.test(lowernao$NAOjunoct, lowernao$AnnS, method = 'pearson')
cordf[4,4] <- "-0.20 (P = 0.35)"
cordf

## lower seasonal t
cor.test(lowernao$NAOjunoct, lowernao$JunOctT, method = 'pearson')
cordf[7,4] <- "-0.04 (P = 0.84)"
cordf

## lower seasonal s
cor.test(lowernao$NAOjunoct, lowernao$JunOctS, method = 'pearson')
cordf[8,4] <- "-0.27 (P = 0.20)"
cordf



#####################################
# add column with row names so that it is included when exported as excel
cordf$Anomaly <- c("MTD annual t","MTD annual s",
                   "MB annual t","MB annual s",
                   "MTD Jun-Oct t","MTD Jun-Oct s",
                   "MB Jun-Oct t","MB Jun-Oct s")
cordf

# now move this column to the front
library(dplyr)
cordf <- cordf %>%
  relocate(Anomaly)
cordf




# save
library(writexl)
#write_xlsx(cordf, path = "AMO and AMV\\AMO and NAO\\Correlations between AMO NAO and Mobile Bay anomalies\\ALSPT_AMO_NAO_MBanomalies_PearsonCorrelations_Results.xlsx")


