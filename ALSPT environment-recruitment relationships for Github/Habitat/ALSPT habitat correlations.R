# load rec devs and datasets
library(here)

recdevs <- read.csv(here("ALSPTRecDevs.csv"))
recdevs


library(readxl)
deltaannmax <- read_excel(here("Habitat","Dec 2025 files","Delta_salty_area_temp_yearly_max.xlsx"))
head(deltaannmax)

deltajunoctmax <- read_excel(here("Habitat","Dec 2025 files","Delta_salty_area_temp_Jun_Oct_max.xlsx"))
head(deltajunoctmax)


## crops recdevs to match anomalies
recdevs <- recdevs[recdevs$Year > 1999 & recdevs$Year < 2024,]
recdevs

### crop anomalies to match recdev years
deltaannmax <- deltaannmax[deltaannmax$Year > 1999,]
summary(deltaannmax$Year)

deltajunoctmax <- deltajunoctmax[deltajunoctmax$Year > 1999,]
summary(deltajunoctmax$Year)

## add recdevs to each anomaly dataset
deltaannmax$rec <- recdevs$Deviation
deltajunoctmax$rec <- recdevs$Deviation


# look at column names
colnames(deltaannmax)

# remove the 0.5 columns for both area and temperature
deltaannmax <- deltaannmax[ , -c(2, 4,6,8,16,18,20,22)]
colnames(deltaannmax)

deltajunoctmax <- deltajunoctmax[ , -c(2, 4,6,8,16,18,20,22)]
colnames(deltajunoctmax)


### rename columns
colnames(deltaannmax) <- c("Year","A1","A2","A3","A4","A5","A6","A7","A8","A9","A10",
                           "T1","T2","T3","T4","T5","T6","T7","T8","T9","T10",
                           "RecDev")
colnames(deltaannmax)


colnames(deltajunoctmax) <- c("Year","A1","A2","A3","A4","A5","A6","A7","A8","A9","A10",
                              "T1","T2","T3","T4","T5","T6","T7","T8","T9","T10",
                              "RecDev")

colnames(deltajunoctmax)

### READY FOR CORRELATIONS :)


################################################################################
################################################################################
################################################################################
library(dplyr)
library(purrr)
library(broom)

# make datafrmaes that remove the year column. simplifies correlation analysis
deltaannmaxcor <-deltaannmax[,-1]
deltajunoctmaxcor <-deltajunoctmax[,-1]




target <- "RecDev"

results_deltaannmax <- deltaannmaxcor %>%
  select(where(is.numeric)) %>%
  select(-all_of(target)) %>%
  map_df(
    ~ tidy(cor.test(deltaannmaxcor[[target]], .x,
                    method = "pearson",
                    use = "complete.obs")),
    .id = "variable"
  ) %>%
  select(
    variable,
    rho = estimate,
    p_value = p.value
  ) %>%
  mutate(p_adj = p.adjust(p_value, method = "fdr"))

results_deltaannmax



results_deltajunoctmax <- deltajunoctmaxcor %>%
  select(where(is.numeric)) %>%
  select(-all_of(target)) %>%
  map_df(
    ~ tidy(cor.test(deltajunoctmaxcor[[target]], .x,
                    method = "pearson",
                    use = "complete.obs")),
    .id = "variable"
  ) %>%
  select(
    variable,
    rho = estimate,
    p_value = p.value
  ) %>%
  mutate(p_adj = p.adjust(p_value, method = "fdr"))

results_deltajunoctmax



### plots
library(ggplot2)
setwd(here("Habitat","Correlation plots"))

## anual
deltaannmaxcorplot <- results_deltaannmax %>%
  mutate(
    variable = reorder(variable, rho),
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE ~ ""
    )
  ) %>%
  ggplot(aes(x = rho, y = variable)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_text(aes(label = sig), nudge_x = 0.03) +
  labs(
    x = "Pearson correlation (??)",
    y = NULL,
    title = "Delta annual effect sizes and significance"
  ) +
  theme_bw()
deltaannmaxcorplot
#ggsave("ALSPTDeltaAnnualCorrelations.png", width = 10, height = 8, dpi = 1000)




## seasonal
deltajunoctmaxcorplot <- results_deltajunoctmax %>%
  mutate(
    variable = reorder(variable, rho),
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE ~ ""
    )
  ) %>%
  ggplot(aes(x = rho, y = variable)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_text(aes(label = sig), nudge_x = 0.03) +
  labs(
    x = "Pearson correlation (??)",
    y = NULL,
    title = "Delta Jun-Oct effect sizes and significance"
  ) +
  theme_bw()
deltajunoctmaxcorplot
#ggsave("ALSPTDeltaJunOctCorrelations.png", width = 10, height = 8, dpi = 1000)




#####################################
## annual top 3
aa <- ggplot(deltaannmax, aes(T1, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Annual temperature in area with salinity ??? 1 PSU",
       y="Log recruitment deviation") +
  theme_bw() +
  stat_regline_equation(label.y = 0.75, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 0.5, aes(label = ..rr.label..))
aa  

ba <- ggplot(deltaannmax, aes(T2, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Annual temperature in area with salinity ??? 2 PSU",
       y="Log recruitment deviation") +
  theme_bw() +
  stat_regline_equation(label.y = 0.75, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 0.5, aes(label = ..rr.label..))

ca <- ggplot(deltaannmax, aes(T3, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Annual temperature in area with salinity ??? 3 PSU",
       y="Log recruitment deviation") +
  theme_bw() +
  stat_regline_equation(label.y = 0.75, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 0.5, aes(label = ..rr.label..))




  ## jun-oct top 3
ab <- ggplot(deltajunoctmax, aes(T1, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Jun-Oct temperature in area with salinity ??? 1 PSU",
       y="Log recruitment deviation") +
  theme_bw() +
  stat_regline_equation(label.y = 0.75, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 0.5, aes(label = ..rr.label..))

bb <- ggplot(deltajunoctmax, aes(T2, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Jun-Oct temperature in area with salinity ??? 2 PSU",
       y="Log recruitment deviation") +
  theme_bw() +
  stat_regline_equation(label.y = 0.75, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 0.5, aes(label = ..rr.label..))

cb <- ggplot(deltajunoctmax, aes(T3, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Jun-Oct temperature in area with salinity ??? 3 PSU",
       y="Log recruitment deviation") +
  theme_bw() +
  stat_regline_equation(label.y = 0.75, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 0.5, aes(label = ..rr.label..))




######### merging all PRP plots together :)))
# first remove their axes
aa <- aa + ylab(NULL)
ab <- ab + ylab(NULL)
ba <- ba + ylab(NULL)
bb <- bb + ylab(NULL)
ca <- ca + ylab(NULL)
cb <- cb + ylab(NULL)


library(ggpubr)
library(grid)

deltafinal <- ggarrange(aa,ab,ba,bb,ca,cb, ncol = 2, nrow = 3,
                      label.x = c(0.85, 0.85),  # the x positions of the labels
                      label.y = c(1, 1),
                      font.label = list(size = 30))  # t)
deltafinal
annotate_figure(deltafinal, left = textGrob("Log recruitment deviation", vjust = 0, rot=90))
#ggsave("ALSPTDeltaTop3Correlations2026.png", width = 8, height = 10, dpi = 1000)







################################################################################
###############################################################################
### repeat for Mobile Bay
mbannmax <- read_excel(here("Habitat","Dec 2025 files","MB_salty_area_temp_yearly_max.xlsx"))
head(mbannmax)

mbjunoctmax <- read_excel(here("Habitat","Dec 2025 files","MB_salty_area_temp_Jun_Oct_max.xlsx"))
head(mbjunoctmax)


### crop anomalies to match recdev years
mbannmax <- mbannmax[mbannmax$Year > 1999,]
summary(mbannmax$Year)

mbjunoctmax <- mbjunoctmax[mbjunoctmax$Year > 1999,]
summary(mbjunoctmax$Year)

## add recdevs to each anomaly dataset
mbannmax$rec <- recdevs$Deviation
mbjunoctmax$rec <- recdevs$Deviation


# look at column names
colnames(mbannmax)

# remove the 0.5 columns for both area and temperature
mbannmax <- mbannmax[ , -c(2, 4,6,8,16,18,20,22)]
colnames(mbannmax)

mbjunoctmax <- mbjunoctmax[ , -c(2, 4,6,8,16,18,20,22)]
colnames(mbjunoctmax)


### rename columns
colnames(mbannmax) <- c("Year","A1","A2","A3","A4","A5","A6","A7","A8","A9","A10",
                           "T1","T2","T3","T4","T5","T6","T7","T8","T9","T10",
                           "RecDev")
colnames(mbannmax)


colnames(mbjunoctmax) <- c("Year","A1","A2","A3","A4","A5","A6","A7","A8","A9","A10",
                              "T1","T2","T3","T4","T5","T6","T7","T8","T9","T10",
                              "RecDev")

colnames(mbjunoctmax)

### READY FOR CORRELATIONS :)


################################################################################
################################################################################
################################################################################


# make datafrmaes that remove the year column. simplifies correlation analysis
mbannmaxcor <-mbannmax[,-1]
mbjunoctmaxcor <-mbjunoctmax[,-1]


target <- "RecDev"

results_mbannmax <- mbannmaxcor %>%
  select(where(is.numeric)) %>%
  select(-all_of(target)) %>%
  map_df(
    ~ tidy(cor.test(mbannmaxcor[[target]], .x,
                    method = "pearson",
                    use = "complete.obs")),
    .id = "variable"
  ) %>%
  select(
    variable,
    rho = estimate,
    p_value = p.value
  ) %>%
  mutate(p_adj = p.adjust(p_value, method = "fdr"))

results_mbannmax



results_mbjunoctmax <- mbjunoctmaxcor %>%
  select(where(is.numeric)) %>%
  select(-all_of(target)) %>%
  map_df(
    ~ tidy(cor.test(mbjunoctmaxcor[[target]], .x,
                    method = "pearson",
                    use = "complete.obs")),
    .id = "variable"
  ) %>%
  select(
    variable,
    rho = estimate,
    p_value = p.value
  ) %>%
  mutate(p_adj = p.adjust(p_value, method = "fdr"))

results_mbjunoctmax



### plots
## anual
mbannmaxcorplot <- results_mbannmax %>%
  mutate(
    variable = reorder(variable, rho),
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE ~ ""
    )
  ) %>%
  ggplot(aes(x = rho, y = variable)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_text(aes(label = sig), nudge_x = 0.03) +
  labs(
    x = "Pearson correlation (??)",
    y = NULL,
    title = "Mobile Bay annual effect sizes and significance"
  ) +
  theme_bw()
mbannmaxcorplot
#ggsave("ALSPTMBAnnualCorrelations.png", width = 10, height = 8, dpi = 1000)




## seasonal
mbjunoctmaxcorplot <- results_mbjunoctmax %>%
  mutate(
    variable = reorder(variable, rho),
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE ~ ""
    )
  ) %>%
  ggplot(aes(x = rho, y = variable)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_text(aes(label = sig), nudge_x = 0.03) +
  labs(
    x = "Pearson correlation (??)",
    y = NULL,
    title = "Mobile Bay Jun-Oct effect sizes and significance"
  ) +
  theme_bw()
mbjunoctmaxcorplot
#ggsave("ALSPTMBJunOctCorrelations.png", width = 10, height = 8, dpi = 1000)



################################################################################
#####################################
## annual top 3
da <- ggplot(mbannmax, aes(T1, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Annual temperature in area with salinity ??? 1 PSU",
       y="Log recruitment deviation") +
  theme_bw()

ea <- ggplot(mbannmax, aes(T2, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Annual temperature in area with salinity ??? 2 PSU",
       y="Log recruitment deviation") +
  theme_bw()

fa <- ggplot(mbannmax, aes(T3, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Annual temperature in area with salinity ??? 3 PSU",
       y="Log recruitment deviation") +
  theme_bw()




## jun-oct top 3
db <- ggplot(mbjunoctmax, aes(T1, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Jun-Oct temperature in area with salinity ??? 1 PSU",
       y="Log recruitment deviation") +
  theme_bw()

eb <- ggplot(mbjunoctmax, aes(T2, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Jun-Oct temperature in area with salinity ??? 2 PSU",
       y="Log recruitment deviation") +
  theme_bw()

fb <- ggplot(mbjunoctmax, aes(T3, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Jun-Oct temperature in area with salinity ??? 3 PSU",
       y="Log recruitment deviation") +
  theme_bw()




######### merging all PRP plots together :)))
# first remove their axes
da <- da + ylab(NULL)
db <- db + ylab(NULL)
ea <- ea + ylab(NULL)
eb <- eb + ylab(NULL)
fa <- fa + ylab(NULL)
fb <- fb + ylab(NULL)


mbfinal <- ggarrange(da,db,ea,eb,fa,fb, ncol = 2, nrow = 3,
                      label.x = c(0.85, 0.85),  # the x positions of the labels
                      label.y = c(1, 1),
                      font.label = list(size = 30))  # t)
mbfinal
annotate_figure(mbfinal, left = textGrob("Log recruitment deviation", vjust = 0, rot=90))
#ggsave("ALSPTMBTop3Correlations.png", width = 8, height = 10, dpi = 1000)





################################################################################
################################################################################
############### correlations with habitat availability variables
#### delta annual first
ga <- ggplot(deltaannmax, aes(A1, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Annual max area with salinity ??? 1 PSU",
       y="Log recruitment deviation") +
  theme_bw()
ga

ha <- ggplot(deltaannmax, aes(A2, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Annual max area with salinity ??? 2 PSU",
       y="Log recruitment deviation") +
  theme_bw()
ha

ia <- ggplot(deltaannmax, aes(A3, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Annual max area with salinity ??? 3 PSU",
       y="Log recruitment deviation") +
  theme_bw()
ia

ja <- ggplot(deltaannmax, aes(A4, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Annual max area with salinity ??? 4 PSU",
       y="Log recruitment deviation") +
  theme_bw()
ja

ka <- ggplot(deltaannmax, aes(A5, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Annual max area with salinity ??? 5 PSU",
       y="Log recruitment deviation") +
  theme_bw()
ka

la <- ggplot(deltaannmax, aes(A6, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Annual max area with salinity ??? 6 PSU",
       y="Log recruitment deviation") +
  theme_bw()
la

ma <- ggplot(deltaannmax, aes(A7, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Annual max area with salinity ??? 7 PSU",
       y="Log recruitment deviation") +
  theme_bw()
ma

na <- ggplot(deltaannmax, aes(A8, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Annual max area with salinity ??? 8 PSU",
       y="Log recruitment deviation") +
  theme_bw()
na

oa <- ggplot(deltaannmax, aes(A9, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Annual max area with salinity ??? 9 PSU",
       y="Log recruitment deviation") +
  theme_bw()
oa

pa <- ggplot(deltaannmax, aes(A10, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Annual max area with salinity ??? 10 PSU",
       y="Log recruitment deviation") +
  theme_bw()
pa


######### merging all plots together :)))
# first remove their axes
ga <- ga + ylab(NULL)
ha <- ha + ylab(NULL)
ia <- ia + ylab(NULL)
ja <- ja + ylab(NULL)
ka <- ka + ylab(NULL)
la <- la + ylab(NULL)
ma <- ma + ylab(NULL)
na <- na + ylab(NULL)
oa <- oa + ylab(NULL)
pa <- pa + ylab(NULL)

deltaannareafinal <- ggarrange(ga,ha,ia,ja,ka,la,ma,na,oa,pa, ncol = 2, nrow = 5,
                        label.x = c(0.85, 0.85),  # the x positions of the labels
                        label.y = c(1, 1),
                        font.label = list(size = 30))  # t)
deltaannareafinal
annotate_figure(deltaannareafinal, left = textGrob("Log recruitment deviation", vjust = 0, rot=90))
#ggsave("ALSPTDeltaAnnualHabitatCorrelations.png", width = 8, height = 10, dpi = 1000)



#####################################################
#### delta junoct next
qa <- ggplot(deltajunoctmax, aes(A1, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Jun-Oct max area with salinity ??? 1 PSU",
       y="Log recruitment deviation") +
  theme_bw()
qa

ra <- ggplot(deltajunoctmax, aes(A2, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Jun-Oct max area with salinity ??? 2 PSU",
       y="Log recruitment deviation") +
  theme_bw()
ra

sa <- ggplot(deltajunoctmax, aes(A3, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Jun-Oct max area with salinity ??? 3 PSU",
       y="Log recruitment deviation") +
  theme_bw()
sa

ta <- ggplot(deltajunoctmax, aes(A4, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Jun-Oct max area with salinity ??? 4 PSU",
       y="Log recruitment deviation") +
  theme_bw()
ta

ua <- ggplot(deltajunoctmax, aes(A5, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Jun-Oct max area with salinity ??? 5 PSU",
       y="Log recruitment deviation") +
  theme_bw()
ua

va <- ggplot(deltajunoctmax, aes(A6, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Jun-Oct max area with salinity ??? 6 PSU",
       y="Log recruitment deviation") +
  theme_bw()
va

wa <- ggplot(deltajunoctmax, aes(A7, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Jun-Oct max area with salinity ??? 7 PSU",
       y="Log recruitment deviation") +
  theme_bw()
wa

xa <- ggplot(deltajunoctmax, aes(A8, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Jun-Oct max area with salinity ??? 8 PSU",
       y="Log recruitment deviation") +
  theme_bw()
xa

ya <- ggplot(deltajunoctmax, aes(A9, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Jun-Oct max area with salinity ??? 9 PSU",
       y="Log recruitment deviation") +
  theme_bw()
ya

za <- ggplot(deltajunoctmax, aes(A10, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Jun-Oct max area with salinity ??? 10 PSU",
       y="Log recruitment deviation") +
  theme_bw()
za


######### merging all plots together :)))
# first remove their axes
qa <- ga + ylab(NULL)
ra <- ra + ylab(NULL)
sa <- sa + ylab(NULL)
ta <- ta + ylab(NULL)
ua <- ua + ylab(NULL)
va <- va + ylab(NULL)
wa <- wa + ylab(NULL)
xa <- xa + ylab(NULL)
ya <- ya + ylab(NULL)
za <- za + ylab(NULL)

deltajunoctareafinal <- ggarrange(qa,ra,sa,ta,ua,va,wa,xa,ya,za, ncol = 2, nrow = 5,
                               label.x = c(0.85, 0.85),  # the x positions of the labels
                               label.y = c(1, 1),
                               font.label = list(size = 30))  # t)
deltajunoctareafinal
annotate_figure(deltajunoctareafinal, left = textGrob("Log recruitment deviation", vjust = 0, rot=90))
#ggsave("ALSPTDeltaJunOctHabitatCorrelations.png", width = 8, height = 10, dpi = 1000)




###########################################################
### now repeat for MB
# annual 1st
aaa <- ggplot(mbannmax, aes(A1, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Annual max area with salinity ??? 1 PSU",
       y="Log recruitment deviation") +
  theme_bw()
aaa

aab <- ggplot(mbannmax, aes(A2, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Annual max area with salinity ??? 2 PSU",
       y="Log recruitment deviation") +
  theme_bw()
aab

aac <- ggplot(mbannmax, aes(A3, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Annual max area with salinity ??? 3 PSU",
       y="Log recruitment deviation") +
  theme_bw()
aac

aad <- ggplot(mbannmax, aes(A4, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Annual max area with salinity ??? 4 PSU",
       y="Log recruitment deviation") +
  theme_bw()
aad

aae <- ggplot(mbannmax, aes(A5, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Annual max area with salinity ??? 5 PSU",
       y="Log recruitment deviation") +
  theme_bw()
aae

aaf <- ggplot(mbannmax, aes(A6, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Annual max area with salinity ??? 6 PSU",
       y="Log recruitment deviation") +
  theme_bw()
aaf

aag <- ggplot(mbannmax, aes(A7, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Annual max area with salinity ??? 7 PSU",
       y="Log recruitment deviation") +
  theme_bw()
aag

aah <- ggplot(mbannmax, aes(A8, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Annual max area with salinity ??? 8 PSU",
       y="Log recruitment deviation") +
  theme_bw()
aah

aai <- ggplot(mbannmax, aes(A9, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Annual max area with salinity ??? 9 PSU",
       y="Log recruitment deviation") +
  theme_bw()
aai

aaj <- ggplot(mbannmax, aes(A10, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Annual max area with salinity ??? 10 PSU",
       y="Log recruitment deviation") +
  theme_bw()
aaj


######### merging all plots together :)))
# first remove their axes
aaa <- aaa + ylab(NULL)
aab <- aab + ylab(NULL)
aac <- aac + ylab(NULL)
aad <- aad + ylab(NULL)
aae <- aae + ylab(NULL)
aaf <- aaf + ylab(NULL)
aag <- aag + ylab(NULL)
aah <- aah + ylab(NULL)
aai <- aai + ylab(NULL)
aaj <- aaj + ylab(NULL)


mbannareafinal <- ggarrange(aaa,aab,aac,aad,aae,aaf,aag,aah,aai,aaj, ncol = 2, nrow = 5,
                               label.x = c(0.85, 0.85),  # the x positions of the labels
                               label.y = c(1, 1),
                               font.label = list(size = 30))  # t)
mbannareafinal
annotate_figure(mbannareafinal, left = textGrob("Log recruitment deviation", vjust = 0, rot=90))
#ggsave("ALSPTMBAnnualHabitatCorrelations.png", width = 8, height = 10, dpi = 1000)



#####################################################
#### mb junoct next
aak <- ggplot(mbjunoctmax, aes(A1, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Jun-Oct max area with salinity ??? 1 PSU",
       y="Log recruitment deviation") +
  theme_bw()
aak

aal <- ggplot(mbjunoctmax, aes(A2, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Jun-Oct max area with salinity ??? 2 PSU",
       y="Log recruitment deviation") +
  theme_bw()
aal

aam <- ggplot(mbjunoctmax, aes(A3, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Jun-Oct max area with salinity ??? 3 PSU",
       y="Log recruitment deviation") +
  theme_bw()
aam

aan <- ggplot(mbjunoctmax, aes(A4, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Jun-Oct max area with salinity ??? 4 PSU",
       y="Log recruitment deviation") +
  theme_bw()
aan

aao <- ggplot(mbjunoctmax, aes(A5, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Jun-Oct max area with salinity ??? 5 PSU",
       y="Log recruitment deviation") +
  theme_bw()
aao

aap <- ggplot(mbjunoctmax, aes(A6, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Jun-Oct max area with salinity ??? 6 PSU",
       y="Log recruitment deviation") +
  theme_bw()
aap

aaq <- ggplot(mbjunoctmax, aes(A7, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Jun-Oct max area with salinity ??? 7 PSU",
       y="Log recruitment deviation") +
  theme_bw()
aaq

aar <- ggplot(mbjunoctmax, aes(A8, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Jun-Oct max area with salinity ??? 8 PSU",
       y="Log recruitment deviation") +
  theme_bw()
aar

aas <- ggplot(mbjunoctmax, aes(A9, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Jun-Oct max area with salinity ??? 9 PSU",
       y="Log recruitment deviation") +
  theme_bw()
aas

aat <- ggplot(mbjunoctmax, aes(A10, RecDev)) +
  geom_point() +
  geom_smooth(method = "lm", col="slateblue") +
  labs(x="Jun-Oct max area with salinity ??? 10 PSU",
       y="Log recruitment deviation") +
  theme_bw()
aat


######### merging all plots together :)))
# first remove their axes
aak <- aak + ylab(NULL)
aal <- aal + ylab(NULL)
aam <- aam + ylab(NULL)
aan <- aan + ylab(NULL)
aao <- aao + ylab(NULL)
aap <- aap + ylab(NULL)
aaq <- aaq + ylab(NULL)
aar <- aar + ylab(NULL)
aas <- aas + ylab(NULL)
aat <- aat + ylab(NULL)

mbjunoctareafinal <- ggarrange(aak,aal,aam,aan,aao,aap,aaq,aar,aas,aat, ncol = 2, nrow = 5,
                                  label.x = c(0.85, 0.85),  # the x positions of the labels
                                  label.y = c(1, 1),
                                  font.label = list(size = 30))  # t)
mbjunoctareafinal
annotate_figure(mbjunoctareafinal, left = textGrob("Log recruitment deviation", vjust = 0, rot=90))
#ggsave("ALSPTMBJunOctHabitatCorrelations.png", width = 8, height = 10, dpi = 1000)












