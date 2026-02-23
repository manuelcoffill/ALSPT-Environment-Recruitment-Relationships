library(here)
library(readxl)
library(ggplot2)

# upload dataset
amo <- read_excel(here("AMO and AMV","AMO and NAO","AMO detrended unsmoothed.xlsx"))
str(amo)
head(amo)
tail(amo)
# 2022 is last full year. remove 23
amo <- amo[amo$Year < 2023,]

# truncated dataset to match rec devs
amoforrec <- amo[amo$Year > 1999 & amo$Year < 2024,]
summary(amoforrec$Year)

# plot - full amo annual
a <- ggplot(data = amo, aes(Year, Annual)) +
  geom_bar(stat = "identity", col="black", fill="#0d0887") +
  geom_hline(yintercept=0, col="black", lty=1, size=1) +
  scale_y_continuous(breaks = seq(-0.5,0.5,0.1)) +
  labs(title="AMO annual full timeseries") +
  theme_bw() +
  theme(axis.title.y = element_blank(),  # Removes text labels
        axis.title.x = element_blank())  +
  coord_cartesian(ylim = c(-0.5, 0.5))
a

# plot - 2000-2022 amo annual
b <- ggplot(data = amoforrec, aes(Year, Annual)) +
  geom_bar(stat = "identity", col="black", fill="#0d0887") +
  geom_hline(yintercept=0, col="black", lty=1, size=1) +
  scale_y_continuous(breaks = seq(-0.5,0.5,0.1)) +
  labs(title="AMO annual 2000-2022") +
  theme_bw() +
  theme(axis.title.y = element_blank(),  # Removes text labels
        axis.title.x = element_blank())  +
  coord_cartesian(ylim = c(-0.4, 0.4))
b



# plot - full amo jun-oct
c <- ggplot(data = amo, aes(Year, `Jun-Oct`)) +
  geom_bar(stat = "identity", col="black", fill="#0d0887") +
  geom_hline(yintercept=0, col="black", lty=1, size=1) +
  scale_y_continuous(breaks = c(-0.6,-0.4,-0.2,0,0.2,0.4,0.6)) +
  labs(title="AMO Jun-Oct full timeseries") +
  theme_bw() +
  theme(axis.title.y = element_blank(),  # Removes text labels
        axis.title.x = element_blank())  +
  coord_cartesian(ylim = c(-0.6, 0.6))
c

# plot - 2000-2022 amo jun-oct
d <- ggplot(data = amoforrec, aes(Year, `Jun-Oct`)) +
  geom_bar(stat = "identity", col="black", fill="#0d0887") +
  geom_hline(yintercept=0, col="black", lty=1, size=1) +
  scale_y_continuous(breaks = c(-0.6,-0.4,-0.2,0,0.2,0.4,0.6)) +
  labs(title="AMO Jun-Oct 2000-2022") +
  theme_bw() +
  theme(axis.title.y = element_blank(),  # Removes text labels
        axis.title.x = element_blank())  +
  coord_cartesian(ylim = c(-0.6, 0.6))
d



library(ggpubr)
library(grid)


final <- ggarrange(a,c,b,d, ncol = 2, nrow = 2
                   #label.x = c(0.85, 0.85),  # the x positions of the labels
                   #label.y = c(1, 1),
                   #font.label = list(size = 30)  # t)
)

final
annotate_figure(final, bottom = textGrob("Year", vjust = 0),
                left = textGrob("Index", vjust = 0, rot=90))

#ggsave("AMOIndices.png", width = 10, height = 8, dpi = 1000)





################################################################################
### NAO next
# upload dataset
nao <- read_excel(here("AMO and AMV","AMO and NAO","Monthly mean NAO index.xlsx"))
str(nao)
head(nao)
tail(nao)
# 2025 missing dec

# truncated dataset to match rec devs
naoforrec <- nao[nao$Year > 1999 & nao$Year < 2024,]
summary(naoforrec$Year)

# plot - full nao annual
a <- ggplot(data = nao, aes(Year, Annual)) +
  geom_bar(stat = "identity", col="black", fill="#0d0887") +
  geom_hline(yintercept=0, col="black", lty=1, size=1) +
  scale_y_continuous(breaks = c(-1.2,-0.8,-0.4,0,0.4,0.8,1.2)) +
  labs(title="NAO annual full timeseries") +
  theme_bw() +
  theme(axis.title.y = element_blank(),  # Removes text labels
        axis.title.x = element_blank())  +
  coord_cartesian(ylim = c(-1.2, 1.2))
a

# plot - 2000-2022 nao annual
b <- ggplot(data = naoforrec, aes(Year, Annual)) +
  geom_bar(stat = "identity", col="black", fill="#0d0887") +
  geom_hline(yintercept=0, col="black", lty=1, size=1) +
  scale_y_continuous(breaks = c(-1.2,-0.8,-0.4,0,0.4,0.8,1.2)) +
  labs(title="NAO annual 2000-2023") +
  theme_bw() +
  theme(axis.title.y = element_blank(),  # Removes text labels
        axis.title.x = element_blank())  +
  coord_cartesian(ylim = c(-1.2, 1.2))
b



# plot - full nao jun-oct
c <- ggplot(data = nao, aes(Year, `Jun-Oct`)) +
  geom_bar(stat = "identity", col="black", fill="#0d0887") +
  geom_hline(yintercept=0, col="black", lty=1, size=1) +
  scale_y_continuous(breaks = c(-1.5,-1,-0.5,0,0.5,1,1.5)) +
  labs(title="NAO Jun-Oct full timeseries") +
  theme_bw() +
  theme(axis.title.y = element_blank(),  # Removes text labels
        axis.title.x = element_blank())  +
  coord_cartesian(ylim = c(-1.5, 1.5))
c

# plot - 2000-2022 nao jun-oct
d <- ggplot(data = naoforrec, aes(Year, `Jun-Oct`)) +
  geom_bar(stat = "identity", col="black", fill="#0d0887") +
  geom_hline(yintercept=0, col="black", lty=1, size=1) +
  scale_y_continuous(breaks = c(-1.5,-1,-0.5,0,0.5,1,1.5)) +
  labs(title="NAO Jun-Oct 2000-2023") +
  theme_bw() +
  theme(axis.title.y = element_blank(),  # Removes text labels
        axis.title.x = element_blank())  +
  coord_cartesian(ylim = c(-1.5, 1.5))
d



finall <- ggarrange(a,c,b,d, ncol = 2, nrow = 2
                   #label.x = c(0.85, 0.85),  # the x positions of the labels
                   #label.y = c(1, 1),
                   #font.label = list(size = 30)  # t)
)

finall
annotate_figure(finall, bottom = textGrob("Year", vjust = 0),
                left = textGrob("Index", vjust = 0, rot=90))

#ggsave("NAOIndices.png", width = 10, height = 8, dpi = 1000)




