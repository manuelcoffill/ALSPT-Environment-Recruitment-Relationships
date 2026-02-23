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

cor.test(upper$RecDev, upper$AnnT, method = 'spearman')
# no correlation
library(ggplot2)
aa <- ggplot(upper, aes(AnnT, RecDev)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="UMB annual bottom temperature")
aa


cor.test(upper$RecDev, upper$AnnS, method = 'spearman')
# strong positive relationship and p<0.05 :)
ca <- ggplot(upper, aes(AnnS, RecDev)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="UMB annual bottom salinity")
ca


cor.test(upper$RecDev, upper$JunOctT, method = 'spearman')
# strong positive relationship and p<0.05 :)
ba <- ggplot(upper, aes(JunOctT, RecDev)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="UMB Jun-Oct bottom temperature")
ba



cor.test(upper$RecDev, upper$JunOctS, method = 'spearman')
# strong positive relationship and p<0.05:)
da <- ggplot(upper, aes(JunOctS, RecDev)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="UMB Jun-Oct bottom salinity")
da



################################################################################
## lower next

cor.test(lower$RecDev, lower$AnnT, method = 'spearman')
# no correlation
ab <- ggplot(lower, aes(AnnT, RecDev)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="LMB annual bottom temperature")
ab




cor.test(lower$RecDev, lower$AnnS, method = 'spearman')
# strong positive relationship and p<0.05 :)
cb <- ggplot(lower, aes(AnnS, RecDev)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="LMB annual bottom salinity")
cb


cor.test(lower$RecDev, lower$JunOctT, method = 'spearman')
# strong positive relationship and p<0.05 :)
bb <- ggplot(lower, aes(JunOctT, RecDev)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="LMB Jun-Oct bottom temperature")
bb



cor.test(lower$RecDev, lower$JunOctS, method = 'spearman')
# strong positive relationship and p<0.05:)
db <- ggplot(lower, aes(JunOctS, RecDev)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="LMB Jun-Oct bottom salinity")
db



###############################################################################
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

#ggsave("ALSPTEnvRecCorrelations.png", width = 8, height = 10, dpi = 1000)





################################################################################
#### correlations and Pvals


### make df to store results
cordf <- matrix(NA, nrow = 4, ncol = 2)
cordf <- data.frame(cordf)
cordf
colnames(cordf) <- c("UMB","LMB")
rownames(cordf) <- c("Annual bottom temperature","Jun-Oct bottom temperature",
                     "Annual bottom salinity","Jun-Oct bottom salinity")
cordf


## upper first
upperannt <- cor.test(upper$RecDev, upper$AnnT, method = 'spearman')
upperannt
# no correlation
upperannt$estimate
upperannt$p.value
cordf[1,1] <- "0.27 (P = 0.21)"
cordf

upperanns <- cor.test(upper$RecDev, upper$AnnS, method = 'spearman')
# strong positive relationship and p<0.05 :)
upperanns$estimate
upperanns$p.value
cordf[3,1] <- "0.41 (P = 0.05)"
cordf

upperjunoctt<- cor.test(upper$RecDev, upper$JunOctT, method = 'spearman')
# strong positive relationship and p<0.05 :)
upperjunoctt$estimate
upperjunoctt$p.value
cordf[2,1] <- "0.63 (P = 0.00)"
cordf

upperjunocts <- cor.test(upper$RecDev, upper$JunOctS, method = 'spearman')
# strong positive relationship and p<0.05:)
upperjunocts$estimate
upperjunocts$p.value
cordf[4,1] <- "0.49 (P = 0.02)"
cordf

################################################################################
## lower next

lowannt <- cor.test(lower$RecDev, lower$AnnT, method = 'spearman')
# no correlation
lowannt$estimate
lowannt$p.value
cordf[1,2] <- "0.12 (P = 0.58)"
cordf

lowanns <- cor.test(lower$RecDev, lower$AnnS, method = 'spearman')
# strong positive relationship and p<0.05 :)
lowanns$estimate
lowanns$p.value
cordf[3,2] <- "0.31 (P = 0.13)"
cordf

lowjunoctt <- cor.test(lower$RecDev, lower$JunOctT, method = 'spearman')
# strong positive relationship and p<0.05 :)
lowjunoctt$estimate
lowjunoctt$p.value
cordf[2,2] <- "0.62 (P = 0.00)"
cordf

lowjunocts <- cor.test(lower$RecDev, lower$JunOctS, method = 'spearman')
# strong positive relationship and p<0.05:)
lowjunocts$estimate
lowjunocts$p.value
cordf[4,2] <- "0.28 (P = 0.18)"
cordf





# add column with row names so that it is included when exported as excel
cordf$Anomaly <- c("Annual bottom temperature","Jun-Oct bottom temperature",
                    "Annual bottom salinity","Jun-Oct bottom salinity")
cordf

# now move this column to the front
library(dplyr)
cordf <- cordf %>%
  relocate(Anomaly)
cordf




# save
library(writexl)
#write_xlsx(cordf, path = "Plots\\ALSPTSpearmanCorrelations.xlsx")





################################################################################
##3 pearson correlation plot
# combined df to simplify correlation analysis
combined <- cbind(upper$AnnT,upper$AnnS,upper$JunOctT,upper$JunOctS,
                  lower$AnnT,lower$AnnS,lower$JunOctT,lower$JunOctS,
                  upper$RecDev)
combined <- data.frame(combined)
colnames(combined) <- c("MTD ann T","MTD ann S","MTD Jun-Oct T","MTD Jun-Oct S",
                       "MB ann T","MB ann S","MB Jun-Oct T","MB Jun-Oct S",
                       "RecDev")
head(combined)


target <- "RecDev"

library(purrr)
library(broom)


results_cor <- combined %>%
  select(where(is.numeric)) %>%
  select(-all_of(target)) %>%
  map_df(
    ~ tidy(cor.test(combined[[target]], .x,
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

results_cor


### plots
library(ggplot2)
## amo
corplot <- results_cor %>%
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
  geom_point(size = 4) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_text(aes(label = sig), nudge_x = 0.03, size=6) +
  labs(
    x = "Pearson correlation (??)",
    y = NULL
    #title = "MTD and MB effect sizes and significance"
  ) +
  theme_bw()
corplot
#ggsave("ALSPTRecPearsonCorrelations1.png", width = 10, height = 8, dpi = 1000)







################################################################################
#### rerunning plots without LOESS smooths
aa <- ggplot(upper, aes(AnnT, RecDev)) +
  geom_point(col="gray30",size=3) +
  #geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="UMB annual bottom temperature")
aa


cor.test(upper$RecDev, upper$AnnS, method = 'spearman')
# strong positive relationship and p<0.05 :)
ca <- ggplot(upper, aes(AnnS, RecDev)) +
  geom_point(col="gray30",size=3) +
  #geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="UMB annual bottom salinity")
ca


cor.test(upper$RecDev, upper$JunOctT, method = 'spearman')
# strong positive relationship and p<0.05 :)
ba <- ggplot(upper, aes(JunOctT, RecDev)) +
  geom_point(col="gray30",size=3) +
  #geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="UMB Jun-Oct bottom temperature")
ba



cor.test(upper$RecDev, upper$JunOctS, method = 'spearman')
# strong positive relationship and p<0.05:)
da <- ggplot(upper, aes(JunOctS, RecDev)) +
  geom_point(col="gray30",size=3) +
  #geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="UMB Jun-Oct bottom salinity")
da



################################################################################
## lower next

cor.test(lower$RecDev, lower$AnnT, method = 'spearman')
# no correlation
ab <- ggplot(lower, aes(AnnT, RecDev)) +
  geom_point(col="gray30",size=3) +
  #geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="LMB annual bottom temperature")
ab




cor.test(lower$RecDev, lower$AnnS, method = 'spearman')
# strong positive relationship and p<0.05 :)
cb <- ggplot(lower, aes(AnnS, RecDev)) +
  geom_point(col="gray30",size=3) +
  #geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="LMB annual bottom salinity")
cb


cor.test(lower$RecDev, lower$JunOctT, method = 'spearman')
# strong positive relationship and p<0.05 :)
bb <- ggplot(lower, aes(JunOctT, RecDev)) +
  geom_point(col="gray30",size=3) +
  #geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="LMB Jun-Oct bottom temperature")
bb



cor.test(lower$RecDev, lower$JunOctS, method = 'spearman')
# strong positive relationship and p<0.05:)
db <- ggplot(lower, aes(JunOctS, RecDev)) +
  geom_point(col="gray30",size=3) +
  #geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="LMB Jun-Oct bottom salinity")
db



###############################################################################
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

#ggsave("ALSPTEnvRecCorrelationsLinearOnly.png", width = 8, height = 10, dpi = 1000)


