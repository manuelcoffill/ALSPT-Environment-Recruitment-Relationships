# load rec devs and datasets
library(here)
recdevs <- read.csv(here("ALSPTRecDevs.csv"))
recdevs


library(readxl)
### AMO 
# upload dataset
amo <- read_excel(here("AMO and AMV", "AMO and NAO", "AMO detrended unsmoothed.xlsx"))

# truncated dataset to match rec devs
amo <- amo[amo$Year > 1999 & amo$Year < 2023,]
summary(amo$Year)


### NAO 
# upload dataset
nao <- read_excel(here("AMO and AMV","AMO and NAO","Monthly mean NAO index.xlsx"))

# truncated dataset to match rec devs
nao <- nao[nao$Year > 1999 & nao$Year < 2024,]
summary(nao$Year)



## crops recdevs to match anomalies
recdevsamo <- recdevs[recdevs$Year > 1999 & recdevs$Year < 2023,]
recdevsamo

## add recdevs to each anomaly dataset
amo$rec <- recdevsamo$Deviation

amo <- data.frame(amo$Year, amo$Annual,amo$`Jun-Oct`, amo$rec)
amo
colnames(amo) <- c("Year","Ann","JunOct","RecDev")
amo
### AMO ready :)




## crops recdevs to match anomalies
recdevsnao <- recdevs[recdevs$Year > 1999 & recdevs$Year < 2024,]
recdevsnao

## add recdevs to each anomaly dataset
nao$rec <- recdevsnao$Deviation


nao <- data.frame(nao$Year, nao$Annual,nao$`Jun-Oct`, nao$rec)
nao
colnames(nao) <- c("Year","Ann","JunOct","RecDev")
nao
## NAO ready :)





################################################################################
###############################################################################
################################################################################
############# SPEARMAN CORRELATION #############################################
################################################################################
## AMO first

cor.test(amo$RecDev, amo$Ann, method = 'spearman')
# no correlation
library(ggplot2)
aa <- ggplot(amo, aes(Ann, RecDev)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="AMO annual")
aa


cor.test(amo$RecDev, amo$JunOct, method = 'spearman')
# strong positive relationship and p<0.05 :)
ba <- ggplot(amo, aes(JunOct, RecDev)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="AMO Jun-Oct")
ba




###NAO next
cor.test(nao$RecDev, nao$Ann, method = 'spearman')
ab <- ggplot(nao, aes(Ann, RecDev)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="NAO annual")
ab


cor.test(nao$RecDev, nao$JunOct, method = 'spearman')
# strong positive relationship and p<0.05 :)
bb <- ggplot(nao, aes(JunOct, RecDev)) +
  geom_point(col="gray30",size=3) +
  geom_smooth(method = "loess", col="steelblue") +
  geom_smooth(method = "lm", col="slateblue") +
  theme_bw() +
  labs(title="NAO Jun-Oct")
bb









###############################################################################
######### merging all plots together :)))
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
annotate_figure(prpfinal, bottom = textGrob("Anomaly", vjust = 0),
                left = textGrob("Log recruitment deviation", vjust = 0, rot=90))

#ggsave("ALSPT_AMO_NAO_Rec_Correlations.png", width = 10, height = 8, dpi = 1000)





################################################################################
#### correlations and Pvals


### make df to store results
cordf <- matrix(NA, nrow = 2, ncol = 2)
cordf <- data.frame(cordf)
cordf
colnames(cordf) <- c("AMO","NAO")
rownames(cordf) <- c("Annual","Jun-Oct")
cordf


## AMO first
amoann <- cor.test(amo$RecDev, amo$Ann, method = 'spearman')
amoann
# no correlation
amoann$estimate
amoann$p.value
cordf[1,1] <- "0.10 (P = 0.65)"
cordf

amojunoct <- cor.test(amo$RecDev, amo$JunOct, method = 'spearman')
# no correlation
amojunoct$estimate
amojunoct$p.value
cordf[2,1] <- "0.09 (P = 0.70)"
cordf


# NAO next
naoann <- cor.test(nao$RecDev, nao$Ann, method = 'spearman')
# strong positive relationship and p<0.05 :)
naoann$estimate
naoann$p.value
cordf[1,2] <- "0.04 (P = 0.84)"
cordf

naojunoct <- cor.test(nao$RecDev, nao$JunOct, method = 'spearman')
# strong positive relationship and p<0.05:)
naojunoct$estimate
naojunoct$p.value
cordf[2,2] <- "-0.08 (P = 0.08)"
cordf



# add column with row names so that it is included when exported as excel
cordf$Anomaly <- c("Annual","Jun-Oct")
cordf

# now move this column to the front
library(dplyr)
cordf <- cordf %>%
  relocate(Anomaly)
cordf




# save
library(writexl)
#write_xlsx(cordf, path = "AMO and AMV\\AMO and NAO\\ALSPT_AMO_NAO_Rec_Correlations_Results.xlsx")






###############################################################################
### simplified pearson correlation plots
library(dplyr)
library(purrr)
library(broom)

# make datafrmaes that remove the year column. simplifies correlation analysis
amocor <- amo[,-1]
naocor <-nao[,-1]
amocor
naocor

target <- "RecDev"

results_amocor <- amocor %>%
  select(where(is.numeric)) %>%
  select(-all_of(target)) %>%
  map_df(
    ~ tidy(cor.test(amocor[[target]], .x,
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

results_amocor



results_naocor <- naocor %>%
  select(where(is.numeric)) %>%
  select(-all_of(target)) %>%
  map_df(
    ~ tidy(cor.test(naocor[[target]], .x,
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

results_naocor



### plots
library(ggplot2)
## amo
amocorplot <- results_amocor %>%
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
    title = "AMO annual effect sizes and significance"
  ) +
  theme_bw()
amocorplot
#ggsave("ALSPTAMORecPearsonCorrelations.png", width = 10, height = 8, dpi = 1000)

# nao
naocorplot <- results_naocor %>%
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
    title = "NAO annual effect sizes and significance"
  ) +
  theme_bw()
naocorplot
#ggsave("ALSPTNAORecPearsonCorrelations.png", width = 10, height = 8, dpi = 1000)

