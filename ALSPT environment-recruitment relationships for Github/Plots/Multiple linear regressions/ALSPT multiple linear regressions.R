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

cor.test(upper$AnnS, upper$AnnT, method = 'pearson')
# no correlation
library(ggplot2)
aa <- ggplot(upper, aes(AnnT, AnnS)) +
  geom_point(col="gray30",size=3) +
  #geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="UMB annual")
aa



cor.test(upper$JunOctS, upper$JunOctT, method = 'pearson')
# strong positive relationship and p<0.05 :)
ba <- ggplot(upper, aes(JunOctT, JunOctS)) +
  geom_point(col="gray30",size=3) +
  #geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="UMB Jun-Oct")
ba



################################################################################
## lower next

cor.test(lower$AnnS, lower$AnnT, method = 'pearson')
# no correlation
ab <- ggplot(lower, aes(AnnT, AnnS)) +
  geom_point(col="gray30",size=3) +
  #geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="LMB annual")
ab




cor.test(lower$JunOctS, lower$JunOctT, method = 'pearson')
# strong positive relationship and p<0.05 :)
bb <- ggplot(lower, aes(JunOctT, JunOctS)) +
  geom_point(col="gray30",size=3) +
  #geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="LMB Jun-Oct")
bb



################################################################################
######### merging all  plots together :)))
# first remove their axes
aa <- aa + xlab(NULL) + ylab(NULL)
ab <- ab + xlab(NULL) + ylab(NULL)
ba <- ba + xlab(NULL) + ylab(NULL)
bb <- bb + xlab(NULL) + ylab(NULL)


library(ggpubr)
library(grid)

prpfinal <- ggarrange(aa,ab,ba,bb, ncol = 2, nrow = 2,
                      label.x = c(0.85, 0.85),  # the x positions of the labels
                      label.y = c(1, 1),
                      font.label = list(size = 30))  # t)
prpfinal
annotate_figure(prpfinal, bottom = textGrob("Temperature", vjust = 0),
                left = textGrob("Salinity", vjust = 0, rot=90))

#ggsave("ALSLPTTempSaltLMs.png", width = 10, height = 8, dpi = 1000)





################################################################################
#### correlations and Pvals


### make df to store results
cordf <- matrix(NA, nrow = 2, ncol = 2)
cordf <- data.frame(cordf)
cordf
colnames(cordf) <- c("UMB","LMB")
rownames(cordf) <- c("Annual","Jun-Oct")
cordf


## upper first
upperann <- cor.test(upper$AnnT, upper$AnnS, method = 'pearson')
upperann
# no correlation
upperann$estimate
upperann$p.value
cordf[1,1] <- "0.47 (P = 0.02)"
cordf

upperjunoct <- cor.test(upper$JunOctT, upper$JunOctS, method = 'pearson')
# strong positive relationship and p<0.05 :)
upperjunoct$estimate
upperjunoct$p.value
cordf[2,1] <- "0.61 (P = 0.00)"
cordf



##############################################################################
## lower next

lowann <- cor.test(lower$AnnT, lower$AnnS, method = 'pearson')
# no correlation
lowann$estimate
lowann$p.value
cordf[1,2] <- "0.27 (P = 0.20)"
cordf

lowjunoct <- cor.test(lower$JunOctT, lower$JunOctS, method = 'pearson')
# strong positive relationship and p<0.05 :)
lowjunoct$estimate
lowjunoct$p.value
cordf[2,2] <- "0.36 (P = 0.09)"
cordf




# add column with row names so that it is included when exported as excel
cordf$TemporalScale <- c("Annual","Jun-Oct")
cordf

# now move this column to the front
library(dplyr)
cordf <- cordf %>%
  relocate(TemporalScale)
cordf




# save
library(writexl)
#write_xlsx(cordf, path = "Plots\\Multiple linear regressions\\ALSPTTemptSaltPearsonCorrelations.xlsx")







################################################################################
# fitting multiple linear regression
#upper annual first
uppann <- lm(RecDev ~ AnnT + AnnS, data = upper)
uppann

# distribution of eresiduals
hist(residuals(uppann), col = "steelblue")
residuals_model <- residuals(uppann)
qqnorm(residuals_model, 
       main = "QQ Plot of Residuals (Multiple Regression)",
       pch = 19, col = "blue")
qqline(residuals_model, col = "red", lwd = 2)

# chekcing variance of resids
#create fitted value vs residual plot
plot(fitted(uppann), residuals(uppann))
#add horizontal line at 0
abline(h = 0, lty = 2)


summary(uppann)
# modle is not significant. neither are the predictors

library(rgl)
#open3d()

#plot3d(uppann, plane.col='steelblue')

#title3d("UMB annual")
#play3d(spin3d(axis = c(0, 0, 1)), duration = 30)








#upper seasonal second
uppjunoct <- lm(RecDev ~ JunOctT + JunOctS, data = upper)
uppjunoct

# distribution of eresiduals
hist(residuals(uppjunoct), col = "steelblue")
# NOT normally distributed
residuals_model <- residuals(uppjunoct)
qqnorm(residuals_model, 
       main = "QQ Plot of Residuals (Multiple Regression)",
       pch = 19, col = "blue")
qqline(residuals_model, col = "red", lwd = 2)


# chekcing variance of resids
#create fitted value vs residual plot
plot(fitted(uppjunoct), residuals(uppjunoct))
#add horizontal line at 0
abline(h = 0, lty = 2)


summary(uppjunoct)
# model is sifnigicant! temp significant @ a=0.05
# rsquared ~2.5x higher than upper annual


#open3d()
#plot3d(uppjunoct, plane.col='steelblue')
#title3d("UMB Jun-Oct")
#play3d(spin3d(axis = c(0, 0, 1)), duration = 30)




################################################################################
### lower annual
lowann <- lm(RecDev ~ AnnT + AnnS, data = lower)
lowann

# distribution of eresiduals
hist(residuals(lowann), col = "steelblue")
# Create QQ plot of residuals
residuals_model <- residuals(lowann)
qqnorm(residuals_model, 
       main = "QQ Plot of Residuals (Multiple Regression)",
       pch = 19, col = "blue")
qqline(residuals_model, col = "red", lwd = 2)

# chekcing variance of resids
#create fitted value vs residual plot
plot(fitted(lowann), residuals(lowann))
#add horizontal line at 0
abline(h = 0, lty = 2)


summary(lowann)
# model not significant


#open3d()
#plot3d(lowann, plane.col='steelblue')
#title3d("LMB annual")
#play3d(spin3d(axis = c(0, 0, 1)), duration = 30)








#lower seasonal second
lowjunoct <- lm(RecDev ~ JunOctT + JunOctS, data = lower)
lowjunoct

# distribution of eresiduals
hist(residuals(lowjunoct), col = "steelblue")
# NOT normally distributed
residuals_model <- residuals(lowjunoct)
qqnorm(residuals_model, 
       main = "QQ Plot of Residuals (Multiple Regression)",
       pch = 19, col = "blue")
qqline(residuals_model, col = "red", lwd = 2)

# chekcing variance of resids
#create fitted value vs residual plot
plot(fitted(lowjunoct), residuals(lowjunoct))
#add horizontal line at 0
abline(h = 0, lty = 2)


summary(lowjunoct)
# model significant, and temp also!
# Rsquared ~5 times higher than annual counterpart


#open3d()
#plot3d(uppjunoct, plane.col='steelblue')
#title3d("LMB Jun-Oct")
#play3d(spin3d(axis = c(0, 0, 1)), duration = 30)





################################################################################
####### ggplot contour plots
####################
library(ggplot2)
library(dplyr)


#### upper annual first
# Build grid for plane
grid <- expand.grid(
  AnnT = seq(min(upper$AnnT), max(upper$AnnT), length.out = 50),
  AnnS = seq(min(upper$AnnS), max(upper$AnnS), length.out = 50)
)

grid$y_pred <- predict(uppann, newdata = grid)

uppannplot <- ggplot() +
  geom_tile(data = grid, aes(x = AnnT, y = AnnS, fill = y_pred), alpha = 0.99) +
  geom_point(data = upper, aes(x = AnnT, y = AnnS, color = RecDev), size = 3) +
  scale_color_gradient(low = "white", high = "black") +
  scale_fill_viridis_c(option = "turbo",  guide = guide_colorbar(order = 1)) +
  labs(
    x = "Temperature", y = "Salinity", fill = "Predicted LRD",
    title = "UMB annual", color="LRD"
  ) +
  theme_bw()
uppannplot



#### upper jun oct  second
# Build grid for plane
grid <- expand.grid(
  JunOctT = seq(min(upper$JunOctT), max(upper$JunOctT), length.out = 50),
  JunOctS = seq(min(upper$JunOctS), max(upper$JunOctS), length.out = 50)
)

grid$y_pred <- predict(uppjunoct, newdata = grid)

uppjunoctplot <- ggplot() +
  geom_tile(data = grid, aes(x = JunOctT, y = JunOctS, fill = y_pred), alpha = 0.99) +
  geom_point(data = upper, aes(x = JunOctT, y = JunOctS, color = RecDev), size = 3) +
  scale_color_gradient(low = "white", high = "black", guide = "none") +
  scale_fill_viridis_c(option = "turbo",   guide = guide_colorbar(order = 1)) +
  labs(
    x = "Temperature", y = "Salinity", fill = "Predicted LRD",
    title = "UMB Jun-Oct", color="LRD"
  ) +
  theme_bw()
uppjunoctplot




####################### lower next

#### lower annual first
# Build grid for plane
grid <- expand.grid(
  AnnT = seq(min(lower$AnnT), max(lower$AnnT), length.out = 50),
  AnnS = seq(min(lower$AnnS), max(lower$AnnS), length.out = 50)
)

grid$y_pred <- predict(lowann, newdata = grid)

lowannplot <- ggplot() +
  geom_tile(data = grid, aes(x = AnnT, y = AnnS, fill = y_pred), alpha = 0.99) +
  geom_point(data = lower, aes(x = AnnT, y = AnnS, color = RecDev), size = 3) +
  scale_color_gradient(low = "white", high = "black", guide = "none") +
  scale_fill_viridis_c(option = "turbo") +
  labs(
    x = "Temperature", y = "Salinity", fill = "Predicted LRD",
    title = "LMB annual", color="LRD"
  ) +
  theme_bw()
lowannplot



#### lower jun oct  second
# Build grid for plane
grid <- expand.grid(
  JunOctT = seq(min(lower$JunOctT), max(lower$JunOctT), length.out = 50),
  JunOctS = seq(min(lower$JunOctS), max(lower$JunOctS), length.out = 50)
)

grid$y_pred <- predict(lowjunoct, newdata = grid)

lowjunoctplot <- ggplot() +
  geom_tile(data = grid, aes(x = JunOctT, y = JunOctS, fill = y_pred), alpha = 0.99) +
  geom_point(data = lower, aes(x = JunOctT, y = JunOctS, color = RecDev), size = 3) +
  scale_color_gradient(low = "white", high = "black", guide = "none") +
  scale_fill_viridis_c(option = "turbo") +
  labs(
    x = "Temperature", y = "Salinity", fill = "Predicted LRD",
    title = "LMB Jun-Oct", color="LRD"
  ) +
  theme_bw()
lowjunoctplot




######### merging all  plots together :)))
# first remove their axes
uppannplot <- uppannplot + xlab(NULL) + ylab(NULL)
lowannplot <- lowannplot + xlab(NULL) + ylab(NULL)
uppjunoctplot <- uppjunoctplot + xlab(NULL) + ylab(NULL)
lowjunoctplot <- lowjunoctplot + xlab(NULL) + ylab(NULL)


mrplots <- ggarrange(uppannplot, lowannplot,uppjunoctplot,lowjunoctplot, ncol = 2, nrow = 2,
                      label.x = c(0.85, 0.85),  # the x positions of the labels
                      label.y = c(1, 1),
                      font.label = list(size = 30))  # t)
mrplots
annotate_figure(mrplots, bottom = textGrob("Temperature", vjust = 0),
                left = textGrob("Salinity", vjust = 0, rot=90))

#ggsave("ALSLPTMultipleRegressionPlots.png", width = 10, height = 8, dpi = 1000)




################################################################################
### table to show each model's p-val, rsquared, & predictor effects

### make df to store results
mrresults <- matrix(NA, nrow = 4, ncol = 4)
mrresults <- data.frame(mrresults)
mrresults
colnames(mrresults) <- c("Pval","Rsquared","Temp","Sal")
rownames(mrresults) <- c("UMB Annual","UMB Jun-Oct", "LMB annual", "LMB Jun-Oct")
mrresults


summary(uppann)
# modle is not significant. neither are the predictors
mrresults[1,1] <- 0.18
mrresults[1,2] <- 0.15
mrresults[1,3] <- 0.83
mrresults[1,4] <- 0.13
mrresults



summary(uppjunoct)
# model is sifnigicant! temp significant @ a=0.05
# rsquared ~2.5x higher than upper annual
mrresults[2,1] <- 0.00
mrresults[2,2] <- 0.40
mrresults[2,3] <- 0.02
mrresults[2,4] <- 0.49
mrresults



summary(lowann)
# model not significant
mrresults[3,1] <- 0.48
mrresults[3,2] <- 0.07
mrresults[3,3] <- 0.93
mrresults[3,4] <- 0.26
mrresults



summary(lowjunoct)
# model significant, and temp also!
# Rsquared ~5 times higher than annual counterpart
mrresults[4,1] <- 0.02
mrresults[4,2] <- 0.32
mrresults[4,3] <- 0.01
mrresults[4,4] <- 0.61
mrresults




# add column with row names so that it is included when exported as excel
mrresults$LocationTemporalScale <- c("UMB annual","UMB Jun-Oct","LMB annual","LMB Jun-Oct")
mrresults

# now move this column to the front
library(dplyr)
mrresults <- mrresults %>%
  relocate(LocationTemporalScale)
mrresults


# save
library(writexl)
#write_xlsx(mrresults, path = "Plots\\Multiple linear regressions\\ALSPTMultipleRegressionResults.xlsx")



