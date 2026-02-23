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
#### merged plot of temp and salt anomalies
library(ggplot2)
setwd(here("Plots"))

### quick cute plot of rec devs during modelled period
recdevs
ggplot(data = recdevs, aes(Year, Deviation)) +
  geom_hline(yintercept=0, col="black", lty=1, size=2) +
  geom_point(col="slateblue", size=3) + 
  geom_line(col="slateblue", size=2) +
  theme_bw() +
  labs(y="Log recruitment deviation")

#ggsave("ALSPTRecDevsLine.png", width = 10, height = 8, dpi = 1000)



ggplot(data = recdevs, aes(Year, Deviation)) +
  geom_bar(stat = "identity", col="black", fill="slateblue") +
  geom_hline(yintercept=0, col="black", lty=1, size=1) +
  scale_y_continuous(breaks = c(-1, 0, 1)) +
  labs(y="Log recruitment deviation") +
  theme_bw()

#ggsave("ALSPTRecDevsBar.png", width = 10, height = 8, dpi = 1000)




## annual temp
a <- ggplot(data = upper, aes(Year, AnnT)) +
  geom_bar(stat = "identity", col="black", fill="#0d0887") +
  geom_hline(yintercept=0, col="black", lty=1, size=1) +
  scale_y_continuous(breaks = c(-1, 0, 1)) +
  labs(title="MTD annual bottom temperature") +
  theme_bw() +
  theme(axis.title.y = element_blank(),  # Removes text labels
        axis.title.x = element_blank())  +
  coord_cartesian(ylim = c(-1.25, 1.25))
a

b <- ggplot(data = lower, aes(Year, AnnT)) +
  geom_bar(stat = "identity", col="black", fill="#0d0887") +
  geom_hline(yintercept=0, col="black", lty=1, size=1) +
  scale_y_continuous(breaks = c(-1, 0, 1)) +
  labs(title="MB annual bottom temperature") +
  theme_bw() +
  theme(axis.title.y = element_blank(),  # Removes text labels
        axis.title.x = element_blank())  +
  coord_cartesian(ylim = c(-1.25, 1.25))
b

## annual sal
c <- ggplot(data = upper, aes(Year, AnnS)) +
  geom_bar(stat = "identity", col="black", fill="#cc4778") +
  geom_hline(yintercept=0, col="black", lty=1, size=1) +
  scale_y_continuous(breaks = seq(-4, 4, 2)) +
  labs(title="MTD annual bottom salinity") +
  theme_bw() +
  theme(axis.title.y = element_blank(),  # Removes text labels
        axis.title.x = element_blank())  +
  coord_cartesian(ylim = c(-4, 4))
c

d <- ggplot(data = lower, aes(Year, AnnS)) +
  geom_bar(stat = "identity", col="black", fill="#cc4778") +
  geom_hline(yintercept=0, col="black", lty=1, size=1) +
  scale_y_continuous(breaks = seq(-6, 6, 2)) +
  labs(title="MB annual bottom salinity") +
  theme_bw() +
  theme(axis.title.y = element_blank(),  # Removes text labels
        axis.title.x = element_blank())  +
  coord_cartesian(ylim = c(-6, 6))
d

## seasonal temp
e <- ggplot(data = upper, aes(Year, JunOctT)) +
  geom_bar(stat = "identity", col="black", fill="#7e03a8") +
  geom_hline(yintercept=0, col="black", lty=1, size=1) +
  scale_y_continuous(breaks = seq(-2, 2, 2)) +
  labs(title="MTD Jun-Oct bottom temperature") +
  theme_bw() +
  theme(axis.title.y = element_blank(),  # Removes text labels
        axis.title.x = element_blank())  +
  coord_cartesian(ylim = c(-2, 2))
e

f <- ggplot(data = lower, aes(Year, JunOctT)) +
  geom_bar(stat = "identity", col="black", fill="#7e03a8") +
  geom_hline(yintercept=0, col="black", lty=1, size=1) +
  scale_y_continuous(breaks = c(-1, 0, 1)) +
  labs(title="MB Jun-Oct bottom temperature") +
  theme_bw() +
  theme(axis.title.y = element_blank(),  # Removes text labels
        axis.title.x = element_blank())  +
  coord_cartesian(ylim = c(-1.25, 1.25))
f

## seasonal sal
g <- ggplot(data = upper, aes(Year, JunOctS)) +
  geom_bar(stat = "identity", col="black", fill="#f89540") +
  geom_hline(yintercept=0, col="black", lty=1, size=1) +
  scale_y_continuous(breaks = seq(-5, 5, 2)) +
  labs(title="MTD Jun-Oct bottom salinity") +
  theme_bw() +
  theme(axis.title.y = element_blank(),  # Removes text labels
        axis.title.x = element_blank())  +
  coord_cartesian(ylim = c(-4, 4))
g

h <- ggplot(data = lower, aes(Year, JunOctS)) +
  geom_bar(stat = "identity", col="black", fill="#f89540") +
  geom_hline(yintercept=0, col="black", lty=1, size=1) +
  scale_y_continuous(breaks = seq(-5, 5, 2)) +
  labs(title="MB Jun-Oct bottom salinity") +
  theme_bw() +
  theme(axis.title.y = element_blank(),  # Removes text labels
        axis.title.x = element_blank())  +
  coord_cartesian(ylim = c(-5, 5))
h


library(ggpubr)
library(grid)


final <- ggarrange(a,b,e,f,c,d,g,h, ncol = 2, nrow = 4
                   #label.x = c(0.85, 0.85),  # the x positions of the labels
                   #label.y = c(1, 1),
                   #font.label = list(size = 30)  # t)
)

final
annotate_figure(final, bottom = textGrob("Year", vjust = 0),
                left = textGrob("Anomaly", vjust = 0, rot=90))

#ggsave("Anomalies.png", width = 8, height = 10, dpi = 1000)





################################################################################
################################################################################
################################################################################
### anomalies for full time series
upper <- read_excel(here("Upper_T_S_anomaly_annual_Jun-Oct.xlsx"))
upper

lower <- read_excel(here("Lower_T_S_anomaly_annual_Jun-Oct.xlsx"))
lower

colnames(upper) <- c("Year","AnnT","AnnS","JunOctT","JunOctS")
upper

colnames(lower) <- c("Year","AnnT","AnnS","JunOctT","JunOctS")
lower


################################################################################
#### merged plot of temp and salt anomalies
## annual temp
a <- ggplot(data = upper, aes(Year, AnnT)) +
  geom_bar(stat = "identity", col="black", fill="#0d0887") +
  geom_hline(yintercept=0, col="black", lty=1, size=1) +
  scale_y_continuous(breaks = c(-1, 0, 1)) +
  labs(title="MTD annual bottom temperature") +
  theme_bw() +
  theme(axis.title.y = element_blank(),  # Removes text labels
        axis.title.x = element_blank())  +
  coord_cartesian(ylim = c(-1.5, 1.5))
a

b <- ggplot(data = lower, aes(Year, AnnT)) +
  geom_bar(stat = "identity", col="black", fill="#0d0887") +
  geom_hline(yintercept=0, col="black", lty=1, size=1) +
  scale_y_continuous(breaks = c(-1, 0, 1)) +
  labs(title="MB annual bottom temperature") +
  theme_bw() +
  theme(axis.title.y = element_blank(),  # Removes text labels
        axis.title.x = element_blank())  +
  coord_cartesian(ylim = c(-1.25, 1.25))
b

## annual sal
c <- ggplot(data = upper, aes(Year, AnnS)) +
  geom_bar(stat = "identity", col="black", fill="#cc4778") +
  geom_hline(yintercept=0, col="black", lty=1, size=1) +
  scale_y_continuous(breaks = seq(-4, 4, 2)) +
  labs(title="MTD annual bottom salinity") +
  theme_bw() +
  theme(axis.title.y = element_blank(),  # Removes text labels
        axis.title.x = element_blank())  +
  coord_cartesian(ylim = c(-4, 4))
c

d <- ggplot(data = lower, aes(Year, AnnS)) +
  geom_bar(stat = "identity", col="black", fill="#cc4778") +
  geom_hline(yintercept=0, col="black", lty=1, size=1) +
  scale_y_continuous(breaks = seq(-6, 6, 2)) +
  labs(title="MB annual bottom salinity") +
  theme_bw() +
  theme(axis.title.y = element_blank(),  # Removes text labels
        axis.title.x = element_blank())  +
  coord_cartesian(ylim = c(-6, 6))
d

## seasonal temp
e <- ggplot(data = upper, aes(Year, JunOctT)) +
  geom_bar(stat = "identity", col="black", fill="#7e03a8") +
  geom_hline(yintercept=0, col="black", lty=1, size=1) +
  scale_y_continuous(breaks = seq(-2, 2, 1)) +
  labs(title="MTD Jun-Oct bottom temperature") +
  theme_bw() +
  theme(axis.title.y = element_blank(),  # Removes text labels
        axis.title.x = element_blank())  +
  coord_cartesian(ylim = c(-1.5, 1.5))
e

f <- ggplot(data = lower, aes(Year, JunOctT)) +
  geom_bar(stat = "identity", col="black", fill="#7e03a8") +
  geom_hline(yintercept=0, col="black", lty=1, size=1) +
  scale_y_continuous(breaks = c(-1, 0, 1)) +
  labs(title="MB Jun-Oct bottom temperature") +
  theme_bw() +
  theme(axis.title.y = element_blank(),  # Removes text labels
        axis.title.x = element_blank())  +
  coord_cartesian(ylim = c(-1.25, 1.25))
f

## seasonal sal
g <- ggplot(data = upper, aes(Year, JunOctS)) +
  geom_bar(stat = "identity", col="black", fill="#f89540") +
  geom_hline(yintercept=0, col="black", lty=1, size=1) +
  scale_y_continuous(breaks = seq(-5, 5, 2)) +
  labs(title="MTD Jun-Oct bottom salinity") +
  theme_bw() +
  theme(axis.title.y = element_blank(),  # Removes text labels
        axis.title.x = element_blank())  +
  coord_cartesian(ylim = c(-4, 4))
g

h <- ggplot(data = lower, aes(Year, JunOctS)) +
  geom_bar(stat = "identity", col="black", fill="#f89540") +
  geom_hline(yintercept=0, col="black", lty=1, size=1) +
  scale_y_continuous(breaks = seq(-5, 5, 2)) +
  labs(title="MB Jun-Oct bottom salinity") +
  theme_bw() +
  theme(axis.title.y = element_blank(),  # Removes text labels
        axis.title.x = element_blank())  +
  coord_cartesian(ylim = c(-5, 5))
h


##### merged plot
final <- ggarrange(a,b,e,f,c,d,g,h, ncol = 2, nrow = 4
                   #label.x = c(0.85, 0.85),  # the x positions of the labels
                   #label.y = c(1, 1),
                   #font.label = list(size = 30)  # t)
)

final
annotate_figure(final, bottom = textGrob("Year", vjust = 0),
                left = textGrob("Anomaly", vjust = 0, rot=90))

#ggsave("AnomaliesFullTimeSeries.png", width = 8, height = 10, dpi = 1000)

