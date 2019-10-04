#------------ Description --------------------
# This script runs through the data  visulation of survey data from Graham, et al. (in prep)


#----------- Load Files ---------------------
# rm(list = ls())

# full_results <- read.csv("AllDams.csv")
full_results <- read.csv("AllDams_noBamff.csv") # no bamff

#--------- Load Packages -------------------

list.of.packages <- c("dplyr","ggplot2", "aod", "cowplot", "quantreg", "rcompanion", "pscl", 
                      "boot", "nonnest2", "AER")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
if (!("countreg" %in% installed.packages()[,"Package"])) install.packages("countreg", repos="http://R-Forge.R-project.org")


library(AER)
library(countreg)
library(nonnest2)
library(pscl)
library(boot)
library(dplyr)
library(ggplot2)
library(aod)
library(cowplot)
library(quantreg)
library(rcompanion)

# ------ select AOIs --------------------------

catchments <- c("Tay", "Otter", "ComHead")
# catchments <- ("Tay")
# catchments <- ("Otter")
# catchments <- c("BoVe", "ComHead")
# catchments <- c("Otter", "BoVe", "ComHead")

Dams <- full_results[full_results$Catch %in% catchments,]

#--------- Dam frequency plot ---------------

Dams <-
  Dams %>% mutate(Categ =
                    ifelse(BDC ==0, "None",
                           ifelse(BDC < 1, "Rare",
                                  ifelse(BDC < 5, "Occasional",
                                         ifelse(BDC <15, "Frequent",
                                                "Pervasive")))))


Dam_cat <- data.frame(table(Dams$Categ))

str(Dam_cat)

new_row <- data.frame ( Var1 = "None", Freq = 0)

Dam_cat <- rbind(Dam_cat,new_row)

Dam_cat <-
  Dam_cat %>% mutate(Order =
                       ifelse(Var1 == "None", 1,
                              ifelse(Var1 == "Rare", 2, 
                                     ifelse(Var1 == "Occasional", 3,
                                            ifelse(Var1 == "Frequent", 4,
                                                   5)))))
Dam_cat$Category <- factor(Dam_cat$Var1, levels = Dam_cat$Var1[order(Dam_cat$Order)])
Dam_cat <- subset(Dam_cat, select = -c(Var1))

Dam_cat <-
  Dam_cat %>% mutate(pct = prop.table(Freq) * 100)
Dam_cat$pct <- format(round(Dam_cat$pct, 1), nsmall = 1)


plotA <- ggplot(Dam_cat, aes(x=Category, y = Freq, fill = Category)) +
  geom_bar(stat = "identity",colour="black", alpha = 0.3, size = 0.2) +
  scale_fill_manual(guide=FALSE,values = c("None" = "red",
                                           "Rare" = "orange",
                                           "Occasional" = "Yellow",
                                           "Frequent" = "green",
                                           "Pervasive" = "blue")) +
  geom_text(aes(y = Freq + 4,    # nudge above top of bar
                label = paste0(pct, '%')),    # prettify
            position = position_dodge(width = .5),
            size = 2.1) +
  labs(x = "Capacity Category", y = "Number of Dams")+
  theme(panel.border = element_rect(linetype = 1, fill = NA), plot.margin = margin (10,20,10,10),
        panel.background = element_rect(fill = "white", colour = "grey90", size = 0.2),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        panel.grid.minor = element_line(colour = NA),
        axis.title.x = element_text(size=9, face = "bold", family = "sans"),
        axis.title.y = element_text(size=9, face = "bold", family = "sans"),
        axis.text=element_text(size=7.5, family = "sans"),
        legend.text=element_text(size=8, family = "sans", face = "italic"),
        legend.title=element_text(size=8, family = "sans", face = "bold", colour = NA))
plotA
#----------- Reach plots -----------------------------------



dam_reaches <- data.frame(count(Dams, reach_no, Catchment, iGeo_Len, BDC, Categ, Str_order)) ## this one!

dam_reaches$n_dams <- dam_reaches$n
dam_reaches <- subset(dam_reaches, select = -c(n))

dam_reaches$obs_dens <- ((dam_reaches$n_dams/dam_reaches$iGeo_Len)*1000)



### frequency of reach types dammed
reaches_cat <- data.frame(table(dam_reaches$Categ))

reaches_cat <- rbind(reaches_cat,new_row)
reaches_cat <-
  reaches_cat %>% mutate(Order =
                           ifelse(Var1 == "None", 1,
                                  ifelse(Var1 == "Rare", 2, 
                                         ifelse(Var1 == "Occasional", 3,
                                                ifelse(Var1 == "Frequent", 4,
                                                       5)))))
reaches_cat$Category <- factor(reaches_cat$Var1, levels = reaches_cat$Var1[order(reaches_cat$Order)])
reaches_cat <- subset(reaches_cat, select = -c(Var1))

reaches_cat <-
  reaches_cat %>% mutate(pct = prop.table(Freq) * 100)
reaches_cat$pct <- format(round(reaches_cat$pct, 1), nsmall = 1)

PlotB <- ggplot(reaches_cat, aes(x=Category, y = Freq, fill = Category)) +
  geom_bar(stat = "identity",colour="black", alpha = 0.3, size = 0.2) +  
  scale_fill_manual(guide=FALSE,values = c("None" = "red", 
                                           "Rare" = "orange", 
                                           "Occasional" = "Yellow", 
                                           "Frequent" = "green", 
                                           "Pervasive" = "blue")) +
  geom_text(aes(y = Freq + 2,    # nudge above top of bar
                label = paste0(pct, '%')),    # prettify
            position = position_dodge(width = .5), 
            size = 2.1) +
  labs(x = "Capacity Category", y = "Number of Dammed Reaches") +
  theme(panel.border = element_rect(linetype = 1, fill = NA), plot.margin = margin (10,20,10,10),
        panel.background = element_rect(fill = "white", colour = "grey90", size = 0.2),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        panel.grid.minor = element_line(colour = NA),
        axis.title.x = element_text(size=9, face = "bold", family = "sans"),
        axis.title.y = element_text(size=9, face = "bold", family = "sans"),
        axis.text=element_text(size=7.5, family = "sans"),
        legend.text=element_text(size=8, family = "sans", face = "italic"),
        legend.title=element_text(size=8, family = "sans", face = "bold", colour = NA))

PlotB

dmas_p_cat <- ggdraw() +
  draw_plot(plotA, x = 0., y = 0, width = .5, height = 1) +
  draw_plot(PlotB, x = .5, y = 0, width = .5, height = 1) +
  draw_plot_label(label = c("A", "B"), size = 9,
                  x = c(0, 0.5), y = c(1, 1))
dmas_p_cat

tiff("Exports/n_dams_comb_plot_noBamff.tiff", width = 174, height = 100, units="mm",res=600, compression = "none")
dmas_p_cat# Make plot
dev.off()