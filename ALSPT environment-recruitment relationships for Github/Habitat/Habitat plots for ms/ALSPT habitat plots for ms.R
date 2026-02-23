library(here)
library(readxl)
deltajunoctmax <- read_excel(here("Habitat","Dec 2025 files","Delta_salty_area_temp_Jun_Oct_max.xlsx"))
head(deltajunoctmax)



colnames(deltajunoctmax)

deltajunoctmax <- deltajunoctmax[ , -c(2, 4,6,8,16,18,20,22)]
colnames(deltajunoctmax)

### rename columns
colnames(deltajunoctmax) <- c("Year","A1","A2","A3","A4","A5","A6","A7","A8","A9","A10",
                              "T1","T2","T3","T4","T5","T6","T7","T8","T9","T10")
colnames(deltajunoctmax)

###############################################################################
### repeat for Mobile Bay
mbjunoctmax <- read_excel(here("Habitat","Dec 2025 files","MB_salty_area_temp_Jun_Oct_max.xlsx"))
head(mbjunoctmax)



# look at column names
colnames(mbjunoctmax)
mbjunoctmax <- mbjunoctmax[ , -c(2, 4,6,8,16,18,20,22)]
colnames(mbjunoctmax)


### rename columns
colnames(mbjunoctmax) <- c("Year","A1","A2","A3","A4","A5","A6","A7","A8","A9","A10",
                           "T1","T2","T3","T4","T5","T6","T7","T8","T9","T10")

colnames(mbjunoctmax)



################################################################################
### need to separate area and temps for each area
# delta 1st
split_index <- which(names(deltajunoctmax) == "A10")

deltaarea  <- deltajunoctmax[, 1:split_index]
deltatemp <- deltajunoctmax[, (split_index + 1):ncol(deltajunoctmax)]
deltatemp$Year <- seq(1976,2023,1)
head(deltaarea)
head(deltatemp)

# mb 2nd
split_index <- which(names(mbjunoctmax) == "A10")

mbarea  <- mbjunoctmax[, 1:split_index]
mbtemp <- mbjunoctmax[, (split_index + 1):ncol(mbjunoctmax)]
mbtemp$Year <- seq(1976,2023,1)
head(mbarea)
head(mbtemp)




##### now need to pivot longer to simplify ggplots
library(tidyr)
deltaarea <- pivot_longer(data = deltaarea,
             cols = c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10"))
head(deltaarea)

deltatemp <- pivot_longer(data = deltatemp,
                          cols = c("T1","T2","T3","T4","T5","T6","T7","T8","T9","T10"))
head(deltatemp)




mbarea <- pivot_longer(data = mbarea,
                          cols = c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10"))
head(mbarea)

mbtemp <- pivot_longer(data = mbtemp,
                          cols = c("T1","T2","T3","T4","T5","T6","T7","T8","T9","T10"))
head(mbtemp)


###### convert groups to factor and reorder then
deltaarea$name <- factor(deltaarea$name, levels = c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10"))
deltatemp$name <- factor(deltatemp$name, levels = c("T1","T2","T3","T4","T5","T6","T7","T8","T9","T10"))
mbarea$name <- factor(mbarea$name, levels = c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10"))
mbtemp$name <- factor(mbtemp$name, levels = c("T1","T2","T3","T4","T5","T6","T7","T8","T9","T10"))

# rename levels
levels(deltaarea$name) <- c("1","2","3","4","5","6","7","8","9","10")
levels(deltatemp$name) <- c("1","2","3","4","5","6","7","8","9","10")
levels(mbarea$name) <- c("1","2","3","4","5","6","7","8","9","10")
levels(mbtemp$name) <- c("1","2","3","4","5","6","7","8","9","10")

#### ready for plots :)

################################################################################
###############################################################################
################################################################################
################################################################################
setwd(here("Habitat","Habitat plots for ms"))
library(ggplot2)
library(nmfspalette)

## delta first
a <- ggplot(deltaarea, aes(Year, value, col=name)) +
  geom_line() +
  scale_color_viridis_d(option = "viridis") +
theme_bw() +
  labs(color = "Salinity ??? (PSU)", y="Maximum area (km²)",
       title = "MTD")
a

c <- ggplot(deltatemp, aes(Year, value, col=name)) +
  geom_line() +
  scale_color_viridis_d(option = "viridis") +
  theme_bw() +
  labs(color = "Salinity ??? (PSU)", y="Maximum temperature (°C)",
       title = "MTD")
c

## mb second
b <- ggplot(mbarea, aes(Year, value, col=name)) +
  geom_line() +
  scale_color_viridis_d(option = "viridis") +
  theme_bw() +
  labs(color = "Salinity ??? (PSU)", y="Maximum area (km²)",
       title = "MB")
b

d <- ggplot(mbtemp, aes(Year, value, col=name)) +
  geom_line() +
  scale_color_viridis_d(option = "viridis") +
  theme_bw() +
  labs(color = "Salinity ??? (PSU)", y="Maximum temperature (°C)",
       title = "MB")
d




###############################################################################
######### merging all PRP plots together :)))
# first remove their axes
a <- a + xlab(NULL) + theme(legend.position = "none")
b <- b + xlab(NULL) + ylab(NULL) + theme(legend.position = "none")
c <- c + xlab(NULL) + theme(legend.position = "none")
d <- d + xlab(NULL) + ylab(NULL) + theme(legend.position = "none")

library(ggpubr)
library(grid)

# extract legend from single plot - works in this case since theyre all the same
legend <- get_legend(
  a + theme(legend.position = "bottom")
)




#merge plots
habitatfinal <- ggarrange(a,b,c,d, ncol = 2, nrow = 2,
                   label.x = c(0.85, 0.85),  # the x positions of the labels
                   label.y = c(1, 1),
                   font.label = list(size = 30))  # t)
habitatfinal <- annotate_figure(habitatfinal, bottom = textGrob("Year", vjust = 0))
habitatfinal

# merge plots + single legend
habitatfinal2 <- ggarrange(habitatfinal, legend,
          ncol=1,
          heights = c(1, 0.15))
habitatfinal2

#ggsave("ALSPT_Habitat_Variables_JunOct.png", width = 10, height = 8, dpi = 1000)





################################################################################
################################################################################
### cropping down to 2000-2023
deltaarea <- deltaarea[deltaarea$Year > 1999,]
deltatemp <- deltatemp[deltatemp$Year > 1999,]
mbarea <- mbarea[mbarea$Year > 1999,]
mbtemp <- mbtemp[mbtemp$Year > 1999,]




## delta first
a <- ggplot(deltaarea, aes(Year, value, col=name)) +
  geom_line() +
  scale_color_viridis_d(option = "viridis") +
  theme_bw() +
  labs(color = "Salinity ??? (PSU)", y="Maximum area (km²)",
       title = "MTD")
a

c <- ggplot(deltatemp, aes(Year, value, col=name)) +
  geom_line() +
  scale_color_viridis_d(option = "viridis") +
  theme_bw() +
  labs(color = "Salinity ??? (PSU)", y="Maximum temperature (°C)",
       title = "MTD")
c

## mb second
b <- ggplot(mbarea, aes(Year, value, col=name)) +
  geom_line() +
  scale_color_viridis_d(option = "viridis") +
  theme_bw() +
  labs(color = "Salinity ??? (PSU)", y="Maximum area (km²)",
       title = "MB")
b

d <- ggplot(mbtemp, aes(Year, value, col=name)) +
  geom_line() +
  scale_color_viridis_d(option = "viridis") +
  theme_bw() +
  labs(color = "Salinity ??? (PSU)", y="Maximum temperature (°C)",
       title = "MB")
d




###############################################################################
######### merging all PRP plots together :)))
# first remove their axes
a <- a + xlab(NULL) + theme(legend.position = "none")
b <- b + xlab(NULL) + ylab(NULL) + theme(legend.position = "none")
c <- c + xlab(NULL) + theme(legend.position = "none")
d <- d + xlab(NULL) + ylab(NULL) + theme(legend.position = "none")

# extract legend from single plot - works in this case since theyre all the same
legend <- get_legend(
  a + theme(legend.position = "bottom")
)


library(ggpubr)
library(grid)

#merge plots
habitatfinal <- ggarrange(a,b,c,d, ncol = 2, nrow = 2,
                          label.x = c(0.85, 0.85),  # the x positions of the labels
                          label.y = c(1, 1),
                          font.label = list(size = 30))  # t)
habitatfinal <- annotate_figure(habitatfinal, bottom = textGrob("Year", vjust = 0))
habitatfinal

# merge plots + single legend
habitatfinal2 <- ggarrange(habitatfinal, legend,
                           ncol=1,
                           heights = c(1, 0.15))
habitatfinal2

#ggsave("ALSPT_Habitat_Variables_JunOct_2000to2023.png", width = 10, height = 8, dpi = 1000)





