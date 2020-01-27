##### Create Some pretty plots to show the Fuzzy model design ############
# rm(list = ls())

list.of.packages <- c("reshape2", "gridExtra", "cowplot")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
# install.packages("reshape2")
# install.packages("gridExtra")
# install.packages("cowplot")
library(ggpubr)
library(reshape2)
library(gridExtra)
library(cowplot)
library(tiff)

### Riparian Veg ####
Rip_veg <- as.data.frame(c(0,0,1,1.5))
colnames(Rip_veg) <- "unsuitable"

Rip_veg$barely <- c(1, 1.5, 2, 2.25)

Rip_veg$moderately <- c(2, 2.25, 3.5, 4)

Rip_veg$suitable <- c(3.5, 4, 4.75, 5)

Rip_veg$preferred <- c(4.5, 5, 5, 5)

Rip_veg$in_out <- (c(0, 1, 1, 0))

##Then rearrange your data frame

Rip_veg= melt(Rip_veg, id=c("in_out"))

#plot
Plot1 <-ggplot(Rip_veg) + geom_line(aes(y=in_out, x=value, colour=variable, linetype = variable), size = 0.7) +
  scale_colour_manual("",values=c("black","orange2","yellow3","springgreen3","royalblue3"))+
  scale_linetype_manual("", values=c(1,2,5,4,6)) +
  labs(x = "Streamside (10m) Vegetation Value", y = "") +
  scale_y_continuous(breaks=seq(0,1)) +
  theme(panel.border = element_rect(linetype = 1, fill = NA), plot.margin = margin (10,20,10,10),
        panel.background = element_rect(fill = "white", colour = "grey90", size = 0.2),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        panel.grid.minor = element_line(colour = NA),
        axis.title.x = element_text(size=7, face = "italic", family = "sans"),
        axis.title.y = element_text(size=7, face = "italic", family = "sans"),
        axis.text=element_text(size=6, family = "sans"),
        legend.position="none")
  # theme(panel.border = element_rect(linetype = 1, fill = NA), plot.margin = margin (10,5,10,10),
  #       panel.background = element_rect(fill = "grey88", colour = "black"),
  #       panel.grid.major = element_line(colour = "grey99"),
  #       panel.grid.minor = element_line(colour = NA),legend.position="none",
  #       axis.title.x = element_text(size=11),
  #       axis.title.y = element_text(size=11))

# Plot1


### Foraging Veg 

Forag_veg <- as.data.frame(c(0,0,1,1.5))
colnames(Forag_veg) <- "unsuitable"

Forag_veg$barely <- c(1, 1.5, 2, 2.25)

Forag_veg$moderately <- c(2, 2.25, 3.5, 4)

Forag_veg$suitable <- c(3.5, 4, 4.75, 5)

Forag_veg$preferred <- c(4.5, 5, 5, 5)

Forag_veg$in_out <- (c(0, 1, 1, 0))

##Then rearrange your data frame

Forag_veg= melt(Forag_veg, id=c("in_out"))

#plot
Plot2 <-ggplot(Forag_veg) + geom_line(aes(y=in_out, x=value, colour=variable,linetype = variable), size = 0.7) +
  scale_colour_manual("",values=c("black","orange2","yellow3","springgreen3","royalblue3"))+
  scale_linetype_manual("", values=c(1,2,5,4,6)) +
  labs(x = "Riparian (40m) Vegetation Value", y = "") +
  scale_y_continuous(breaks=seq(0,1)) +
  theme(panel.border = element_rect(linetype = 1, fill = NA), plot.margin = margin (10,20,10,10),
        panel.background = element_rect(fill = "white", colour = "grey90", size = 0.2),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        panel.grid.minor = element_line(colour = NA),
        axis.title.x = element_text(size=7, face = "italic", family = "sans"),
        axis.title.y = element_text(size=7, face = "italic", family = "sans"),
        axis.text=element_text(size=6, family = "sans"),
        legend.text=element_text(size=7, family = "sans", face = "italic"),
        legend.title=element_text(size=8, family = "sans", face = "bold"))
  # theme(panel.border = element_rect(linetype = 1, fill = NA), plot.margin = margin (5,20,10,10),
  #       panel.background = element_rect(fill = "grey88", colour = "black"),
  #       panel.grid.major = element_line(colour = "grey99"),
  #       panel.grid.minor = element_line(colour = NA),
  #       axis.title.x = element_text(size=11),
  #       axis.title.y = element_text(size=11))

# Plot2

### Veg FIS Results/Density plots


Veg_FIS <- as.data.frame(c(0,0,0,0.0))
colnames(Veg_FIS) <- "none"

Veg_FIS$rare <- c(0, 0, 1, 1.5)

Veg_FIS$occasional <- c(1, 1.5, 4, 8)

Veg_FIS$frequent <- c(4, 8, 12, 25)

Veg_FIS$pervasive <- c(12, 25, 45, 45)

Veg_FIS$in_out <- (c(0, 1, 1, 0))

##Then rearrange your data frame

Veg_FIS= melt(Veg_FIS, id=c("in_out"))

#plot
Plot3 <-ggplot(Veg_FIS) + geom_line(aes(y=in_out, x=value, colour=variable,linetype = variable), size = 0.8) +
  scale_colour_manual("",values=c("black","orange2","yellow3","springgreen3","royalblue3"))+
  scale_linetype_manual("", values=c(1,2,5,4,6)) +
  labs(x = "", y = "") +
  scale_y_continuous(breaks=seq(0,1)) +
  theme(panel.border = element_rect(linetype = 1, fill = NA), plot.margin = margin (10,20,10,10),
        panel.background = element_rect(fill = "white", colour = "grey90", size = 0.2),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        panel.grid.minor = element_line(colour = NA),
        axis.title.x = element_text(size=7, face = "italic", family = "sans"),
        axis.title.y = element_text(size=7, face = "italic", family = "sans"),
        axis.text=element_text(size=6, family = "sans"),
        legend.text=element_text(size=7, family = "sans", face = "italic"),
        legend.title=element_text(size=8, family = "sans", face = "bold"))
  # theme(panel.border = element_rect(linetype = 1, fill = NA), plot.margin = margin (10,20,10,10),
  #       panel.background = element_rect(fill = "grey88", colour = "black"),
  #       panel.grid.major = element_line(colour = "grey99"),
  #       panel.grid.minor = element_line(colour = NA),
  #       axis.title.x = element_text(size=11),
  #       axis.title.y = element_text(size=11))

# Plot3



Plot4 <- ggdraw() +
  draw_plot(Plot1, x = 0., y = .5, width = .46, height = .455) +
  draw_plot(Plot2, x = .46, y = .5, width = .54, height = .455) +
  draw_plot(Plot3, x = 0, y = 0, width = 1, height = 0.455) +
  draw_plot_label(label = c("Antecedent", "Consequent"), size = 10,
                  x = c(0.365, 0.365), y = c(1, 0.51)) +
  draw_plot_label(label = c("fuzzy membership","fuzzy membership"), size = 9,
                  x = c(0,0), y = c(0.57,0.07), angle = c(90,90))+
  draw_plot_label(label =  "Vegetation FIS Dam Capacity (dams/km)", size = 9,
                  x = 0.17, y = 0.04)+
  draw_line(x = c(0.475), y = c(0.96,0.955), linetype = 2)+
  draw_line(x = c(0.025,0.98), y = c(0.955), linetype = 2)+
  draw_line(x = c(0.025), y = c(0.955,0.91), linetype = 2)+
  draw_line(x = c(0.98), y = c(0.955,0.91), linetype = 2)+
  draw_line(x = c(0.025,0.98), y = c(0.515), linetype = 2)+
  draw_line(x = c(0.025), y = c(0.515,0.56), linetype = 2)+
  draw_line(x = c(0.98), y = c(0.515,0.56), linetype = 2)+
  draw_line(x = c(0.475), y = c(0.515,0.498), linetype = 2)+
  draw_line(x = c(0.025,0.98), y = c(0.455), linetype = 1)+
  draw_line(x = c(0.025), y = c(0.455,0.41), linetype = 1)+
  draw_line(x = c(0.98), y = c(0.455,0.41), linetype = 1)+
  draw_line(x = c(0.475), y = c(0.47,0.455), linetype = 1)
  

# Plot4

tiff("Exports/BVI_FuzzPlot.tiff", width = 174, height = 150, units="mm",res=600, compression = "lzw")
Plot4# Make plot
dev.off()

#---------- Combined FIS plots ------------------------------------


Veg_FISb <- as.data.frame(c(0,0,0,0))
colnames(Veg_FISb) <- "none"

Veg_FISb$rare <- c(0, 0, 0.5, 1)

Veg_FISb$occasional <- c(0.5, 1, 4, 5)

Veg_FISb$frequet <- c(4, 5, 12, 20)

Veg_FISb$pervasive <- c(12, 20, 45, 45)

Veg_FISb$in_out <- (c(0, 1, 1, 0))

##Then rearrange your data frame

Veg_FISb= melt(Veg_FISb, id=c("in_out"))

# sp_high
SP_High <- as.data.frame(c(0, 0, 1000, 1200))
colnames(SP_High) <- "persists"

SP_High$breach <- c(1000, 1200, 1200, 1600)

SP_High$occ_blowout <- c(1200, 1600, 1600, 2400)

SP_High$blowout <- c(1600, 2400, 10000, 10000)

SP_High$in_out <- (c(0, 1, 1, 0))

##Then rearrange your data frame

SP_High= melt(SP_High, id=c("in_out"))

#SP low
SP_Low <- as.data.frame(c(0, 0, 150, 175))
colnames(SP_Low) <- "can"

SP_Low$probably<- c(150, 175, 180, 190)

SP_Low$cannot <- c(180, 190, 10000, 10000)

SP_Low$in_out <- (c(0, 1, 1, 0))

##Then rearrange your data frame

SP_Low= melt(SP_Low, id=c("in_out"))

#slope

Slope <- as.data.frame(c(0, 0, 0.0002, 0.005))
colnames(Slope) <- "flat"

Slope$can <- c(0.0002, 0.005, 0.12, 0.15)

Slope$probably <- c(0.12, 0.15, 0.17, 0.23)

Slope$cannot <- c(0.17, 0.23, 1, 1)

Slope$in_out <- (c(0, 1, 1, 0))

##Then rearrange your data frame

Slope = melt(Slope, id=c("in_out"))

Slope$value <- Slope$value*100

#combFIS density

Comb_FIS <- as.data.frame(c(0,0,0,0.0))
colnames(Comb_FIS) <- "none"

Comb_FIS$rare <- c(0, 0, 0.5, 1)

Comb_FIS$occasional <- c(0.5, 1, 4, 5)

Comb_FIS$frequent <- c(4, 5, 12, 20)

Comb_FIS$pervasive <- c(12, 20, 45, 45)

Comb_FIS$in_out <- (c(0, 1, 1, 0))

##Then rearrange your data frame

Comb_FIS= melt(Comb_FIS, id=c("in_out"))


Plot5 <-ggplot(Veg_FISb) + geom_line(aes(y=in_out, x=value, colour=variable,linetype = variable), size = 0.7) +
  scale_colour_manual("",values=c("black","orange2","yellow3","springgreen3","royalblue3"))+
  scale_linetype_manual("", values=c(1,2,5,4,6)) +
  labs(x = "Veg FIS Dam Capacity (dams/km)", y = "") +
  coord_cartesian(xlim=c(0, 30))+
  scale_y_continuous(breaks=seq(0,1)) +
  theme(panel.border = element_rect(linetype = 1, fill = NA), plot.margin = margin (10,20,10,10),
        panel.background = element_rect(fill = "white", colour = "grey90", size = 0.2),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        panel.grid.minor = element_line(colour = NA),
        axis.title.x = element_text(size=7, face = "italic", family = "sans"),
        axis.title.y = element_text(size=7, face = "italic", family = "sans"),
        axis.text=element_text(size=6, family = "sans"),
        legend.text=element_text(size=7, family = "sans", face = "italic"),
        legend.title=element_text(size=8, family = "sans", face = "bold"))
  # theme(panel.border = element_rect(linetype = 1, fill = NA), plot.margin = margin (5,20,10,10),
  #       panel.background = element_rect(fill = "grey88", colour = "black"),
  #       panel.grid.major = element_line(colour = "grey99"),
  #       panel.grid.minor = element_line(colour = NA),
  #       axis.title.x = element_text(size=11),
  #       axis.title.y = element_text(size=11),
  #       axis.text=element_text(size=10))

Plot5

Plot6 <-ggplot(SP_High) + geom_line(aes(y=in_out, x=value, colour=variable,linetype = variable), size = 0.7) +
  scale_colour_manual("",values=c("royalblue3","springgreen3","orange2","black"), breaks = c("persists","breach","occ_blowout","blowout")
                      ,labels = c("persists","breach","occasional blowout","blowout"))+
  scale_linetype_manual("", values=c(1,2,5,4), breaks = c("persists","breach","occ_blowout","blowout")
                        ,labels = c("persists","breach","occasional blowout","blowout")) +
  labs(y = "") +
  xlab(bquote('Q2 Stream Power (watts/' *m*')'))+
  coord_cartesian(xlim=c(0, 3000))+
  scale_y_continuous(breaks=seq(0,1)) +
  theme(panel.border = element_rect(linetype = 1, fill = NA), plot.margin = margin (10,20,10,10),
        panel.background = element_rect(fill = "white", colour = "grey90", size = 0.2),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        panel.grid.minor = element_line(colour = NA),
        axis.title.x = element_text(size=7, face = "italic", family = "sans"),
        axis.title.y = element_text(size=7, face = "italic", family = "sans"),
        axis.text=element_text(size=6, family = "sans"),
        legend.text=element_text(size=7, family = "sans", face = "italic"),
        legend.title=element_text(size=8, family = "sans", face = "bold"))
  # theme(panel.border = element_rect(linetype = 1, fill = NA), plot.margin = margin (5,20,10,10),
  #       panel.background = element_rect(fill = "grey88", colour = "black"),
  #       panel.grid.major = element_line(colour = "grey99"),
  #       panel.grid.minor = element_line(colour = NA),
  #       axis.title.x = element_text(size=11),
  #       axis.title.y = element_text(size=11),
  #       axis.text=element_text(size=10))

# Plot6


Plot7 <-ggplot(SP_Low) + geom_line(aes(y=in_out, x=value, colour=variable,linetype = variable), size = 0.7) +
  scale_colour_manual("",values=c("royalblue3","orange2","black"))+
  scale_linetype_manual("", values=c(1,8,5)) +
  labs(y = "") +
  xlab(bquote('Q80 Stream Power (watts/' *m*')'))+
  # scale_x_continuous(breaks = round(seq(0, 1000, by = 200)),limits = c(0,1000))+
  scale_y_continuous(breaks=seq(0,1)) +
  coord_cartesian(xlim=c(0, 300))+
  theme(panel.border = element_rect(linetype = 1, fill = NA), plot.margin = margin (10,20,10,10),
        panel.background = element_rect(fill = "white", colour = "grey90", size = 0.2),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        panel.grid.minor = element_line(colour = NA),
        axis.title.x = element_text(size=7, face = "italic", family = "sans"),
        axis.title.y = element_text(size=7, face = "italic", family = "sans"),
        axis.text=element_text(size=6, family = "sans"),
        legend.text=element_text(size=7, family = "sans", face = "italic"),
        legend.title=element_text(size=8, family = "sans", face = "bold"))
  # theme(panel.border = element_rect(linetype = 1, fill = NA), plot.margin = margin (5,20,10,10),
  #       panel.background = element_rect(fill = "grey88", colour = "black"),
  #       panel.grid.major = element_line(colour = "grey99"),
  #       panel.grid.minor = element_line(colour = NA),
  #       axis.title.x = element_text(size=11),
  #       axis.title.y = element_text(size=11),
  #       axis.text=element_text(size=10))
# Plot7

Plot8 <-ggplot(Slope) + geom_line(aes(y=in_out, x=value, colour=variable,linetype = variable), size = 0.7) +
  scale_colour_manual("",values=c("royalblue3","springgreen3","orange2","black"))+
  scale_linetype_manual("", values=c(1,2,5,4)) +
  labs(x = "Slope (%)", y = "") +
  coord_cartesian(xlim=c(0, 30))+
  scale_y_continuous(breaks=seq(0,1)) +
  theme(panel.border = element_rect(linetype = 1, fill = NA), plot.margin = margin (10,20,10,10),
        panel.background = element_rect(fill = "white", colour = "grey90", size = 0.2),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        panel.grid.minor = element_line(colour = NA),
        axis.title.x = element_text(size=7, face = "italic", family = "sans"),
        axis.title.y = element_text(size=7, face = "italic", family = "sans"),
        axis.text=element_text(size=6, family = "sans"),
        legend.text=element_text(size=7, family = "sans", face = "italic"),
        legend.title=element_text(size=8, family = "sans", face = "bold"))
  # theme(panel.border = element_rect(linetype = 1, fill = NA), plot.margin = margin (5,20,10,10),
  #       panel.background = element_rect(fill = "grey88", colour = "black"),
  #       panel.grid.major = element_line(colour = "grey99"),
  #       panel.grid.minor = element_line(colour = NA),
  #       axis.title.x = element_text(size=11),
  #       axis.title.y = element_text(size=11),
  #       axis.text=element_text(size=10))

# Plot8

Plot9 <-ggplot(Comb_FIS) + geom_line(aes(y=in_out, x=value, colour=variable,linetype = variable), size = 0.8) +
  scale_colour_manual("",values=c("black","orange2","yellow3","springgreen3","royalblue3"))+
  scale_linetype_manual("", values=c(1,2,5,4,6)) +
  labs(x = "", y = "") +
  scale_y_continuous(breaks=seq(0,1)) +
  theme(panel.border = element_rect(linetype = 1, fill = NA), plot.margin = margin (10,20,10,10),
        panel.background = element_rect(fill = "white", colour = "grey90", size = 0.2),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        panel.grid.minor = element_line(colour = NA),
        axis.title.x = element_text(size=7, face = "italic", family = "sans"),
        axis.title.y = element_text(size=7, face = "italic", family = "sans"),
        axis.text=element_text(size=6, family = "sans"),
        legend.text=element_text(size=7, family = "sans", face = "italic"),
        legend.title=element_text(size=8, family = "sans", face = "bold"))
  # theme(panel.border = element_rect(linetype = 1, fill = NA), plot.margin = margin (10,20,10,10),
  #       panel.background = element_rect(fill = "grey88", colour = "black"),
  #       panel.grid.major = element_line(colour = "grey99"),
  #       panel.grid.minor = element_line(colour = NA),
  #       axis.title.x = element_text(size=11),
  #       axis.title.y = element_text(size=11))

# Plot9



Plot10 <- ggdraw() +
  draw_plot(Plot5, x = 0, y = .69, width = .5, height = .25) +
  draw_plot(Plot8, x = .5, y = .69, width = .5, height = .25) +
  draw_plot(Plot7, x = 0, y = .46, width = .5, height = 0.25) +
  draw_plot(Plot6, x = .5, y = .46, width = .5, height = 0.25) +
  draw_plot(Plot9, x = 0, y = 0, width = 1, height = 0.415) +
  draw_plot_label(label = c("Antecedent", "Consequent"), size = 10,
                  x = c(0.365, 0.365), y = c(1, 0.475)) +
  draw_plot_label(label = c("fuzzy membership","fuzzy membership"), size = 9,
                  x = c(0,0), y = c(0.57,0.07), angle = c(90,90))+
  draw_plot_label(label =  "Combined FIS Dam Capacity (dams/km)", size = 9,
                  x = 0.17, y = 0.04)+
                  
  draw_line(x = c(0.48), y = c(0.965,0.955), linetype = 2)+
  
  draw_line(x = c(0.025,0.98), y = c(0.955), linetype = 2)+
  draw_line(x = c(0.025), y = c(0.955,0.93), linetype = 2)+
  draw_line(x = c(0.98), y = c(0.955,0.93), linetype = 2)+
  
  draw_line(x = c(0.025,0.98), y = c(0.475), linetype = 2)+
  
  draw_line(x = c(0.025), y = c(0.475,0.5), linetype = 2)+
  draw_line(x = c(0.98), y = c(0.475,0.5), linetype = 2)+
  draw_line(x = c(0.48), y = c(0.475,0.46), linetype = 2)+
  
  draw_line(x = c(0.025,0.98), y = c(0.42), linetype = 1)+
  
  draw_line(x = c(0.025), y = c(0.42,0.39), linetype = 1)+
  draw_line(x = c(0.98), y = c(0.42,0.39), linetype = 1)+
  draw_line(x = c(0.48), y = c(0.44,0.42), linetype = 1)


Plot10

tiff("Exports/BDC_FuzzPlot.tiff", width = 174, height = 200, units="mm",res=600, compression = "lzw")
Plot10# Make plot
dev.off()

# plot11 <- ggdraw()+
#   draw_plot(Plot1, x = 0.2, y = 0.82, width = .28, height = .15) +
#   draw_plot(Plot2, x = 0.48, y = 0.82, width = .37, height = .15) +
#   draw_plot(Plot3, x = 0.2, y = 0.64, width = .65, height = 0.18) +
#   draw_plot(Plot5, x = 0, y = .45, width = .5, height = .18) +
#   draw_plot(Plot8, x = .48, y = .45, width = .5, height = .18) +
#   draw_plot(Plot7, x = 0, y = .27, width = .5, height = 0.18) +
#   draw_plot(Plot6, x = .48, y = .27, width = .52, height = 0.18) +
#   draw_plot(Plot9, x = 0, y = 0, width = 1, height = 0.27)
# 
#   # draw_plot_label(label = c("Antecedent", "Consequent"), size = 15,
#   #                 x = c(0.4, 0.4), y = c(1, 0.475))
# 
# 
# plot11












