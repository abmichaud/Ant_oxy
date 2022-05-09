knitr::opts_chunk$set(echo = TRUE, warning=FALSE, message=FALSE)

require(ggplot2)
require(scales)
require(varhandle)
require(scales)
require(psych)
require(scales)
library(leaflet)
library(dplyr)
library(mapproj)
library(gplots)
library(RColorBrewer)
library(ggpubr)
library(readxl)
library(tibble)


SCOC_data = read.csv("SCOC_database_version_1.csv") # read csv file
cols <- c("Region", "Water", "Latitude", "Longitude", "Location", "Depth", 
          "Depth_range", "SCOC", "ex_in_situ", "Method", "Uptake", "Sediment", "Photic_zone", "Study") 
colnames(SCOC_data) <- cols

scoc_arctic <- filter(SCOC_data, Latitude >= 60 & Water != "Lake" & Photic_zone != "yes")
scoc_ant <- filter(SCOC_data, Latitude <= -60 & Water != "Lake" & Photic_zone != "yes")
scoc_lakes <- filter(SCOC_data, Water == "Lake" & Photic_zone != "yes" & Region != "Lake Erie")
scoc_polar <- rbind(scoc_arctic, scoc_ant, scoc_lakes)
so_db <- filter(SCOC_data, Study=="no" & Water == "Southern Ocean" & Region != "Whillans Grounding Zone" & Photic_zone != "yes")
so_study <- filter(SCOC_data, Study=="yes" & Region == "Whillans Grounding Zone")
scoc_polar$Water <- as.factor(scoc_polar$Water)

## Summary statistics
waters.means <- with(scoc_polar, tapply(SCOC, Water, mean))
water.means <- as.data.frame(waters.means)

waters.means <- with(so_db, tapply(SCOC, Water, mean))
water.means <- as.data.frame(waters.means)

waters.means <- with(so_study, tapply(SCOC, Water, mean))
water.means <- as.data.frame(waters.means)

region.means <- with(scoc_polar, tapply(SCOC, Region, mean))
region.means <- as.data.frame(region.means)

ant.means <- with(scoc_ant, tapply(SCOC, Region, mean))
ant.means <- as.data.frame(ant.means)

test <- scoc_polar %>% group_by(Water) %>% summarise(mean = mean(SCOC))

depth.means <- with(scoc_polar, tapply(SCOC, Depth_range, mean))
depth.means <- as.data.frame(depth.means)

filter(scoc_polar, (Region == "Amundsen Sea") | (Region == "Bellingshausen Sea")) %>%
  summary()

## Plotting by latitude -- not used 
polar <- ggplot() + 
  geom_point(data = scoc_polar, aes(x = SCOC, y = Latitude, color=Water, shape = Study), size = 2.25) +
  scale_color_manual(name=NULL,
                     values=c("#0800F0", "#C981F0", "#E63B01", "#F08863", "#F0B300"),
                     breaks=c("Arctic Ocean", "Atlantic Ocean", "Lake", "Pacific Ocean", "Southern Ocean"),
                     labels=c("Arctic Ocean", "Atlantic Ocean", "Lake", "Pacific Ocean", "Southern Ocean")) +
  scale_shape_manual(name=NULL,
                     values=c(19, 17),
                     breaks=c("no", "yes"),
                     labels=c("Database", "This Study")) +
  theme_classic() + 
  scale_y_continuous(breaks = c(-90, -75, -60, -45, -30, -15, 0, 15, 30, 45, 60, 75, 90)) + 
  xlab(bquote(' '*SCOC~(mmol~O[2]~m^-2~d^-1*''))) + ylab("Latitude °") + 
  theme(axis.text.x = element_text(vjust=0.5, size=14)) + 
  theme(axis.text.y = element_text(vjust=0.5, size=14)) + 
  labs(color = NULL, shape = NULL) +
  scale_x_continuous(trans=log_trans(10), breaks = c(1, 10, 100, 1000)) + 
  annotation_logticks(sides = "b") + theme(text = element_text(size=16)) + 
  geom_hline(yintercept = 0, linetype="dashed")+ theme(plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm")) +
  guides(color = guide_legend(override.aes = list(size=4)), 
         shape = guide_legend(override.aes = list(size=4)))

# Figure 1 showing points and database points used
world <- map_data("world")
global <- ggplot() +
  geom_map(data = world, map = world, aes(long, lat, map_id = region), fill = "grey") +
  geom_point(data=scoc_polar, aes(x=Longitude, y=Latitude, color = Water, shape = Study), size = 2) +
  scale_color_manual(name=NULL,
                     values=c("#0800F0", "#C981F0", "#E63B01", "#F08863", "#F0B300"),
                     breaks=c("Arctic Ocean", "Atlantic Ocean", "Lake", "Pacific Ocean", "Southern Ocean"),
                     labels=c("Arctic Ocean", "Atlantic Ocean", "Lake", "Pacific Ocean", "Southern Ocean")) +
  scale_shape_manual(name=NULL,
                     values=c(19, 17),
                     breaks=c("no", "yes"),
                     labels=c("Database", "This Study")) +
  ylab("Latitude °") +
  xlab("Longitude °") +
  labs(color = NULL, shape = NULL) +
  guides(color = guide_legend(override.aes = list(size=4)), 
         shape = guide_legend(override.aes = list(size=4))) +
  theme_classic()

pdf("global_oxy_map.pdf", width = 8, height = 5)
print(global)
dev.off()

## Figure 3 box plots and depth with database data
oxy_box <- ggplot(scoc_polar, aes(x=Water, y=SCOC, fill=Study)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape=19, size=2, position = position_jitterdodge(dodge.width = 0.7, jitter.width = 0.2), 
              aes(x=Water, y=SCOC, fill=Study)) +
  stat_summary(fun = "mean", shape=15, color="red") +
  scale_fill_manual(values=c("grey", "blue"),
                     breaks=c("no", "yes"),
                     labels=c("Database", "This Study")) +
  scale_y_continuous(trans=log_trans(10), breaks = c(0.1, 1, 10, 100, 1000)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(), legend.key=element_blank(), legend.title = element_blank(),
        text = element_text(size = 15, color = "black"),
        axis.text.y = element_text(color="black", size=15),
        axis.text.x = element_text(angle = 30, hjust = 1, color="black", size=15)) +
  ylab(bquote(''*SCOC~(mmol~O[2]~m^-2~d^-1*''))) + 
  xlab(NULL) +
  labs(color = NULL)

polar_depth <- ggplot(scoc_polar) +
  geom_point(aes(x=Depth, y=SCOC, color = Study, shape = Water), size=3, stroke=1.75) +
  scale_y_log10() +
  scale_x_log10() +
  scale_shape_manual(name=NULL,
                     values=c(0, 6, 1, 5, 2),
                     breaks=c("Arctic Ocean", "Atlantic Ocean", "Lake", "Pacific Ocean", "Southern Ocean"),
                     labels=c("Arctic Ocean", "Atlantic Ocean", "Lake", "Pacific Ocean", "Southern Ocean")) +
  scale_color_manual(name=NULL,
                     values=c("grey", "blue"),
                     breaks=c("no", "yes"),
                     labels=c("Database", "This Study")) +
  #scale_stroke_manual(name=NULL,
                    #values=c(1, 2),
                    #breaks=c("no", "yes"),
                    #labels=c("Database", "This Study")) +
  ylab(bquote(' '*SCOC~(mmol~O[2]~m^-2~d^-1*''))) + xlab("Depth (m)") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(), legend.key=element_blank(),
        text = element_text(size = 15, color = "black"),
        axis.text.y = element_text(color="black", size=15),
        axis.text.x = element_text(color="black", size=15)) +
  labs(color = NULL, shape = NULL) +
  guides(color = guide_legend(override.aes = list(size=4)), 
         shape = guide_legend(override.aes = list(size=4))) 

pdf("global_oxy.pdf", width = 8.5, height = 11)
print(ggarrange(oxy_box, polar_depth,  
                labels = c("A", "B"), 
                ncol = 1, nrow = 2))
dev.off()


slmmc2a <- read.table("slmmc2a.txt", header = TRUE, sep = "\t")

slmmc2a_px <- slmmc2a$slmmc2a_px
slmmc2a_pc <- slmmc2a$slmmc2a_pc
slmmc2a_pxcon <- slmmc2a$slmmc2a_px.1
slmmc2a_pcon <- slmmc2a$slmmc2a_pcon
slmmc2a_mx <- slmmc2a$slmmc2a_mx
slmmc2a_mc <- slmmc2a$slmmc2a_mc

slmmc2c <- read.table("slmmc2c.txt", header = TRUE, sep = "\t")

slmmc2c_px <- slmmc2c$slmmc2c_px
slmmc2c_pc <- slmmc2c$slmmc2c_pc
slmmc2c_pxcon <- slmmc2c$slmmc2c_px.1
slmmc2c_pcon <- slmmc2c$slmmc2c_pcon
slmmc2c_mx <- slmmc2c$slmmc2c_mx
slmmc2c_mc <- slmmc2c$slmmc2c_mc

wgzmc8a1 <- read.table("wgzmc8a1.txt", header = TRUE, sep = "\t")

wgzmc8a1_px <- wgzmc8a1$wgzmc8a1_px
wgzmc8a1_pc <- wgzmc8a1$wgzmc8a1_pc
wgzmc8a1_pxcon <- wgzmc8a1$wgzmc8a1_px.1
wgzmc8a1_pcon <- wgzmc8a1$wgzmc8a1_pcon
wgzmc8a1_mx <- wgzmc8a1$wgzmc8a1_mx
wgzmc8a1_mc <- wgzmc8a1$wgzmc8a1_mc

wgzmc8a2 <- read.table("wgzmc8a2.txt", header = TRUE, sep = "\t")

wgzmc8a2_px <- wgzmc8a2$wgzmc8a2_px
wgzmc8a2_pc <- wgzmc8a2$wgzmc8a2_pc
wgzmc8a2_pxcon <- wgzmc8a2$wgzmc8a2_px.1
wgzmc8a2_pcon <- wgzmc8a2$wgzmc8a2_pcon
wgzmc8a2_mx <- wgzmc8a2$wgzmc8a2_mx
wgzmc8a2_mc <- wgzmc8a2$wgzmc8a2_mc

wgzmc8a3 <- read.table("wgzmc8a3.txt", header = TRUE, sep = "\t")

wgzmc8a3_px <- wgzmc8a3$wgzmc8a3_px
wgzmc8a3_pc <- wgzmc8a3$wgzmc8a3_pc
wgzmc8a3_pxcon <- wgzmc8a3$wgzmc8a3_px.1
wgzmc8a3_pcon <- wgzmc8a3$wgzmc8a3_pcon
wgzmc8a3_mx <- wgzmc8a3$wgzmc8a3_mx
wgzmc8a3_mc <- wgzmc8a3$wgzmc8a3_mc

wgzmc9a1 <- read.table("wgzmc9a1.txt", header = TRUE, sep = "\t")

wgzmc9a1_px <- wgzmc9a1$wgzmc9a1_px
wgzmc9a1_pc <- wgzmc9a1$wgzmc9a1_pc
wgzmc9a1_pxcon <- wgzmc9a1$wgzmc9a1_px.1
wgzmc9a1_pcon <- wgzmc9a1$wgzmc9a1_pcon
wgzmc9a1_mx <- wgzmc9a1$wgzmc9a1_mx
wgzmc9a1_mc <- wgzmc9a1$wgzmc9a1_mc

wgzmc9a3 <- read.table("wgzmc9a3.txt", header = TRUE, sep = "\t")

wgzmc9a3_px <- wgzmc9a3$wgzmc9a3_px
wgzmc9a3_pc <- wgzmc9a3$wgzmc9a3_pc
wgzmc9a3_pxcon <- wgzmc9a3$wgzmc9a3_px.1
wgzmc9a3_pcon <- wgzmc9a3$wgzmc9a3_pcon
wgzmc9a3_mx <- wgzmc9a3$wgzmc9a3_mx
wgzmc9a3_mc <- wgzmc9a3$wgzmc9a3_mc

wgzmc9a4 <- read.table("wgzmc9a4.txt", header = TRUE, sep = "\t")

wgzmc9a4_px <- wgzmc9a4$wgzmc9a4_px
wgzmc9a4_pc <- wgzmc9a4$wgzmc9a4_pc
wgzmc9a4_pxcon <- wgzmc9a4$wgzmc9a4_px.1
wgzmc9a4_pcon <- wgzmc9a4$wgzmc9a4_pcon
wgzmc9a4_mx <- wgzmc9a4$wgzmc9a4_mx
wgzmc9a4_mc <- wgzmc9a4$wgzmc9a4_mc

oxy_conc=expression(paste("O"[2],~"(", mu, "M)"))
oxy_consump=expression(paste("O"[2],~"Consumption (nmol cm"^"-3"*" h"^"-1"*")"))
legend_conc=expression(paste("Measured O"[2]))
legend_mod=expression(paste("Modeled O"[2]))
legend_consump = expression(paste("O"[2],~"Consumption")) 

## SLM Oxy profiles
pdf("slm_oxy_profiles.pdf", height = 6, width = 6.5)
par(mfrow=c(1,2), pin=c(5,3), mar = c(4,4,4,0.25))
plot(slmmc2a_mc, slmmc2a_mx, col="black", pch=15, ylim = c(32,0.01),  xlim = c(0,500), axes=FALSE, ann=FALSE)
axis(3)
axis(2, las=2)
lines(slmmc2a_pc, slmmc2a_px, col="black", lty=1, lwd=2)
mtext(oxy_conc, side=3, line = 2.5)
mtext("Depth (cm)", side = 2, line = 2.5, las=0)

par(new = TRUE)
plot(slmmc2a_pcon, slmmc2a_pxcon, col="darkgrey", pch=15, ylim = c(32,0.01), xlim = c(0,1), 
     xaxt = "n", yaxt = "n", ylab = "", xlab = "", axes=FALSE, ann=FALSE)
axis(side = 1)
mtext(oxy_consump, side = 1, line = 2.5)


plot(slmmc2c_mc, slmmc2c_mx, col="black", pch=15, ylim = c(32,0.01),  xlim = c(0,500), axes=FALSE, ann=FALSE)
axis(3)
axis(2, las=2)
lines(slmmc2c_pc, slmmc2c_px, col="black", lty=1, lwd=2)
mtext(oxy_conc, side=3, line = 2.5)
#mtext("Depth (cm)", side = 2, line = 2.5, las=0)

par(new = TRUE)
plot(slmmc2c_pcon, slmmc2c_pxcon, col="darkgrey", pch=15, ylim = c(32,0.01), xlim = c(0,1), 
     xaxt = "n", yaxt = "n", ylab = "", xlab = "", axes=FALSE, ann=FALSE)
axis(side = 1)
mtext(oxy_consump, side = 1, line = 2.5)
legend(0.25, 25, legend = c(legend_conc, legend_mod, legend_consump), merge = TRUE, 
       col = c("black", "black", "darkgrey"), 
       pch = c(15, NA, 15), lty = c(-1, 1, -1), lwd = 3, ncol = 1, bty = "n")

dev.off()

## WGZ Oxy profiles
pdf("wgz_oxy_profiles.pdf", height = 12, width = 10)
par(mfrow=c(2,3), pin=c(5,3), oma = c(2,2,0,0), mar = c(5,3,6,1), cex.axis = 2.25)
plot(wgzmc8a1_mc, wgzmc8a1_mx, col="black", pch=15, cex=1.25, ylim = c(5,0), xlim = c(0,400), axes=FALSE, ann=FALSE)
axis(3)
axis(2, las=2)
lines(wgzmc8a1_pc, wgzmc8a1_px, col="black", lty=1, lwd=2)
#mtext(oxy_conc, side=3, line = 3, cex = 1.3)
mtext("Depth (cm)", side = 2, line = 3, las=0, cex = 1.3)

par(new = TRUE)
plot(wgzmc8a1_pcon, wgzmc8a1_pxcon, col="darkgrey", pch=15, cex=1.25, ylim = c(5,0), xlim = c(0,10), 
     xaxt = "n", yaxt = "n", ylab = "", xlab = "", axes=FALSE, ann=FALSE)
axis(side = 1, mgp=c(0,1.5,0))
#
plot(wgzmc8a2_mc, wgzmc8a2_mx, col="black", pch=15, cex=1.25, ylim = c(5,0),  xlim = c(0,400), axes=FALSE, ann=FALSE)
axis(3)
axis(2, las=2)
lines(wgzmc8a2_pc, wgzmc8a2_px, col="black", lty=1, lwd=2)
mtext(oxy_conc, side=3, line = 4, cex = 1.3)

par(new = TRUE)
plot(wgzmc8a2_pcon, wgzmc8a2_pxcon, col="darkgrey", pch=15, cex=1.25, ylim = c(5,0), xlim = c(0,10), 
     xaxt = "n", yaxt = "n", ylab = "", xlab = "", axes=FALSE, ann=FALSE)
axis(side = 1, mgp=c(0,1.5,0))
#
plot(wgzmc8a3_mc, wgzmc8a3_mx, col="black", pch=15, cex=1.25, ylim = c(5,0),  xlim = c(0,400), axes=FALSE, ann=FALSE)
axis(3)
axis(2, las=2)
lines(wgzmc8a3_pc, wgzmc8a3_px, col="black", lty=1, lwd=2)
#mtext(oxy_conc, side=3, line = 3, cex = 1.3)

par(new = TRUE)
plot(wgzmc8a3_pcon, wgzmc8a3_pxcon, col="darkgrey", pch=15, cex=1.25, ylim = c(20,0), xlim = c(0,10), 
     xaxt = "n", yaxt = "n", ylab = "", xlab = "", axes=FALSE, ann=FALSE)
axis(side = 1, mgp=c(0,1.5,0))
legend(0.5, 15, legend = c(legend_conc, legend_mod, legend_consump), merge = TRUE, 
       col = c("black", "black", "darkgrey"), 
       pch = c(15, NA, 15), lty = c(-1, 1, -1), lwd = 3, ncol = 1, bty = "n", cex = 1.75)
#
plot(wgzmc9a1_mc, wgzmc9a1_mx, col="black", pch=15, cex=1.25, ylim = c(20,0),  xlim = c(0,400), axes=FALSE, ann=FALSE)
axis(3)
axis(2, las=2)
lines(wgzmc9a1_pc, wgzmc9a1_px, col="black", lty=1, lwd=2)
mtext("Depth (cm)", side = 2, line = 3, las=0, cex = 1.3)

par(new = TRUE)
plot(wgzmc9a1_pcon, wgzmc9a1_pxcon, col="darkgrey", pch=15, cex=1.25, ylim = c(20,0), xlim = c(0,10), 
     xaxt = "n", yaxt = "n", ylab = "", xlab = "", axes=FALSE, ann=FALSE)
axis(side = 1, mgp=c(0,1.5,0))
#mtext(oxy_consump, side = 1, line = 4, cex = 1.3)
#
plot(wgzmc9a3_mc, wgzmc9a3_mx, col="black", pch=15, cex=1.25, ylim = c(20,0),  xlim = c(0,400), axes=FALSE, ann=FALSE)
axis(3)
axis(2, las=2)
lines(wgzmc9a3_pc, wgzmc9a3_px, col="black", lty=1, lwd=2)

par(new = TRUE)
plot(wgzmc9a3_pcon, wgzmc9a3_pxcon, col="darkgrey", pch=15, cex=1.25, ylim = c(20,0), xlim = c(0,10), 
     xaxt = "n", yaxt = "n", ylab = "", xlab = "", axes=FALSE, ann=FALSE)
axis(side = 1, mgp=c(0,1.5,0))
mtext(oxy_consump, side = 1, line = 5, cex = 1.5)
#
plot(wgzmc9a4_mc, wgzmc9a4_mx, col="black", pch=15, cex=1.25, ylim = c(20,0),  xlim = c(0,400), axes=FALSE, ann=FALSE)
axis(3)
axis(2, las=2)
lines(wgzmc9a4_pc, wgzmc9a4_px, col="black", lty=1, lwd=2)

par(new = TRUE)
plot(wgzmc9a4_pcon, wgzmc9a4_pxcon, col="darkgrey", pch=15, cex=1.25, ylim = c(20,0), xlim = c(0,10), 
     xaxt = "n", yaxt = "n", ylab = "", xlab = "", axes=FALSE, ann=FALSE)
axis(side = 1, mgp=c(0,1.5,0))
#mtext(oxy_consump, side = 1, line = 4, cex = 1.3)

#
dev.off()
