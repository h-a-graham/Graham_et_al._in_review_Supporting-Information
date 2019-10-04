
# This Bit needs some thought - How do we validate the BHI data?
# Our feeding sign data is not ideal because it is based on 10m sections on the channel no the exact location. 
# Therefore we are left with having to validate on a reach by reach basis.
# However, the search area that was used to extact vegetation values is limited in reaches where the stream width is > ~40m.
# So do we remove these reaches and then run the analysis, do we leave them in and explain the high false positive rate?
# Either way it looks like with increasing Veg Capacity i.e. BDC without the hydro/topo factors is a helpful predictor of relative selectiviy.



list.of.packages <- c("pkgbuild", "parallel", "dplyr", "rstanarm", "reshape2", "sjPlot", "extrafont", "bayestestR", "cowplot", "ggrepel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(pkgbuild)
library(parallel)
library(dplyr)
library(rstanarm)
library(reshape2)
library(sjPlot)
library(ggplot2)
library(extrafont)
library(bayestestR)
require(cowplot)
library(ggrepel)
# font_import()
# loadfonts(device = "win")

# load rstan
library(rstan)
options(mc.cores = parallel::detectCores())      # enabling parallel processing
rstan_options(auto_write = TRUE)                 # allows you to automatically save a bare version of a compiled Stan program to the hard disk so that it does not need to be recompiled (unless you change it).
# Sys.setenv(LOCAL_CPPFLAGS = '-march=native')     # Not needed if the comment out section above is run...

# if errors occur running models try:
# file.edit("~/.R/Makevars")
# And enter the following: CXX14 = g++ -std=c++1y -Wno-unused-variable -Wno-unused-function -fPIC



All_reaches <- read.csv("AllReaches_noBamff.csv")
All_reaches <- All_reaches[!is.na(All_reaches$iVeg_40),]


#############

#Version with 40m buffer veg results

All_reaches$vegVar <- All_reaches$iVeg_40

All_reaches  <-
  All_reaches  %>% mutate(VegCateg =
                                   ifelse(vegVar <= 1, "Unsuitable",
                                        ifelse(vegVar <= 2, "Low",
                                                 ifelse(vegVar <= 3, "Mod",
                                                        ifelse(vegVar <= 4, "High",
                                                               "Preferred")))))


# VNone_All <- All_reaches[All_reaches$VegCateg == "None",]
VUns_All <- All_reaches[All_reaches$VegCateg == "Unsuitable",]
VLow_All <- All_reaches[All_reaches$VegCateg == "Low",]
VMod_All <- All_reaches[All_reaches$VegCateg == "Mod",]
VHigh_All <- All_reaches[All_reaches$VegCateg == "High",]
VPref_All <- All_reaches[All_reaches$VegCateg == "Preferred",]

All_Cat_list2 <- list( nB = nrow(VUns_All),
                     nC = nrow(VLow_All), nD = nrow(VMod_All), nE = nrow(VHigh_All), nF = nrow(VPref_All),
                     sB = nrow(VUns_All[VUns_All$Active>0 | VUns_All$n_dams > 0,]),  
                     sC = nrow(VLow_All[VLow_All$Active>0 | VLow_All$n_dams > 0,]), 
                     sD = nrow(VMod_All[VMod_All$Active>0 | VMod_All$n_dams > 0,]), 
                     sE = nrow(VHigh_All[VHigh_All$Active>0 | VHigh_All$n_dams > 0,]), 
                     sF=nrow(VPref_All[VPref_All$Active>0 | VPref_All$n_dams > 0,]))




model_string2 <- "
// Here we define the data we are going to pass into the model
data {
  // Number of trials
  
  int nB;
  int nC;
  int nD;
  int nE;
  int nF;
  // Number of successes
  
  int sB;
  int sC;
  int sD;
  int sE;
  int sF;
  
  //int n; // Number of trials     # single model version
  // int s;  // Number of successes # single model version
}

// Here we define what 'unknowns' aka parameters we have.
parameters {
  
  real<lower=0, upper=1> UnsRate;
  real<lower=0, upper=1> LowRate;
  real<lower=0, upper=1> ModRate;
  real<lower=0, upper=1> HighRate;
  real<lower=0, upper=1> PrefRate;
}

// The generative model
model {
  
  UnsRate ~ uniform(0, 1);
  LowRate ~ uniform(0, 1);
  ModRate ~ uniform(0, 1);
  HighRate ~ uniform(0, 1);
  PrefRate ~ uniform(0, 1);
  
  
  sB ~ binomial(nB, UnsRate);
  sC ~ binomial(nC, LowRate);
  sD ~ binomial(nD, ModRate);
  sE ~ binomial(nE, HighRate);
  sF ~ binomial(nF, PrefRate);
}
"

######## Run the Stan Model ################

All_Cat_samples2 <- stan(model_code = model_string2, data = All_Cat_list2)
traceplot(All_Cat_samples2)
plot(All_Cat_samples2)

All_posterior2 <- as.data.frame(All_Cat_samples2)

All_posterior2 <-  dplyr::select (All_posterior2,-lp__)


########### Get cridible intervals ########################

#Credibility scores and mean for Category Probability

UnsCI <- posterior_interval(as.matrix(All_posterior2$UnsRate), prob = 0.95, type = "central",
                             pars = NULL, regex_pars = NULL)
LowCI <- posterior_interval(as.matrix(All_posterior2$LowRate), prob = 0.95, type = "central",
                             pars = NULL, regex_pars = NULL)
ModCI <- posterior_interval(as.matrix(All_posterior2$ModRate), prob = 0.95, type = "central",
                            pars = NULL, regex_pars = NULL)
HighCI <- posterior_interval(as.matrix(All_posterior2$HighRate), prob = 0.95, type = "central",
                             pars = NULL, regex_pars = NULL)
PrefCI <- posterior_interval(as.matrix(All_posterior2$PrefRate), prob = 0.95, type = "central",
                             pars = NULL, regex_pars = NULL)

############## Get  Maximum A Posteriori (MAP) ######################################
PrefmaxL<-map_estimate(All_posterior2$PrefRate)
HighmaxL<-map_estimate(All_posterior2$HighRate)
ModmaxL<-map_estimate(All_posterior2$ModRate)
LowmaxL<-map_estimate(All_posterior2$LowRate)
UnsmaxL<-map_estimate(All_posterior2$UnsRate)

pl2 <- c(UnsCI[1], LowCI[1], ModCI[1], HighCI[1], PrefCI[1])
MAP2 <- c(UnsmaxL, LowmaxL, ModmaxL, HighmaxL, PrefmaxL)
pu2 <- c(UnsCI[2], LowCI[2], ModCI[2], HighCI[2], PrefCI[2])
Category2 <- c("Unsuit.", "Low", "Mod.", "High", "Pref.")

# pl <- c(RareCI[1], OccCI[1], FreqCI[1], PervCI[1])
# MAP <- c(raremaxL, occmaxL, freqmaxL, pervmaxL)
# pu <- c(RareCI[2], OccCI[2], FreqCI[2], PervCI[2])
# Category <- c("Rare", "Occasional", "Frequent", "Pervasive")

############### Plotting The posterior distribution - Density plots ##################################

# get key params for density plot
ggp <- ggplot(All_posterior2, aes(x = PrefRate)) + geom_density()
ggp_build <- ggplot_build(ggp)[["data"]][[1]]
prefmaxd = max(ggp_build$y)

ggp <- ggplot(All_posterior2, aes(HighRate)) + geom_density()
ggp_build <- ggplot_build(ggp)[["data"]][[1]]
highmaxd = max(ggp_build$y)

ggp <- ggplot(All_posterior2, aes(ModRate)) + geom_density()
ggp_build <- ggplot_build(ggp)[["data"]][[1]]
modmaxd = max(ggp_build$y)

ggp <- ggplot(All_posterior2, aes(LowRate)) + geom_density()
ggp_build <- ggplot_build(ggp)[["data"]][[1]]
lowmaxd = max(ggp_build$y)

ggp <- ggplot(All_posterior2, aes(UnsRate)) + geom_density()
ggp_build <- ggplot_build(ggp)[["data"]][[1]]
unsmaxd = max(ggp_build$y)


# getting max bin heights for plotting mean lines
# Set bin number:
wid.bin = 0.01
ggp <- ggplot(All_posterior2, aes(x = PrefRate)) + geom_histogram(binwidth = wid.bin)
ggp_build <- ggplot_build(ggp)
prefmax = max(ggp_build[["data"]][[1]][["count"]])
ggp <- ggplot(All_posterior2, aes(HighRate)) + geom_histogram(binwidth = wid.bin)
ggp_build <- ggplot_build(ggp)
highmax = max(ggp_build[["data"]][[1]][["count"]])
ggp <- ggplot(All_posterior2, aes(ModRate)) + geom_histogram(binwidth = wid.bin)
ggp_build <- ggplot_build(ggp)
modmax = max(ggp_build[["data"]][[1]][["count"]])
ggp <- ggplot(All_posterior2, aes(LowRate)) + geom_histogram( binwidth = wid.bin)
ggp_build <- ggplot_build(ggp)
lowmax = max(ggp_build[["data"]][[1]][["count"]])
ggp <- ggplot(All_posterior2, aes(UnsRate)) + geom_histogram(binwidth = wid.bin)
ggp_build <- ggplot_build(ggp)
unsmax = max(ggp_build[["data"]][[1]][["count"]])

maxLik2 <- c(UnsmaxL, LowmaxL, ModmaxL, HighmaxL, PrefmaxL)
ymax2 <- c(unsmaxd, lowmaxd, modmaxd, highmaxd, prefmaxd)
histmax2 <- c(unsmax, lowmax, modmax, highmax, prefmax)

# maxLik <- c(raremaxL, occmaxL, freqmaxL, pervmaxL)
# ymax <- c(raremaxd, occmaxd, freqmaxd, pervmaxd) 
# histmax <- c(raremax, occmax, freqmax, pervmax)

points_CIS2 <- data.frame(Category2, pl2, maxLik2, pu2, ymax2, histmax2)
# points_CIS[,-1] <-round(points_CIS[,-1],5)

reshapedPost2 <- melt(All_posterior2)
names(reshapedPost2)[names(reshapedPost2) == "variable"] <- "Group"


Densplot2 <- ggplot(NULL) + 
  geom_density(data = reshapedPost2, aes(x=value, fill=Group), alpha=0.4, colour = "grey50", size = 0.2) +
  
  geom_segment(data = All_posterior2, aes(x = UnsmaxL, y = 0, xend = UnsmaxL, yend = unsmaxd),linetype="dashed", size = 0.2) +
  geom_segment(data = All_posterior2, aes(x = LowmaxL, y = 0, xend = LowmaxL, yend = lowmaxd),linetype="dashed", size = 0.2) +
  geom_segment(data = All_posterior2, aes(x = ModmaxL, y = 0, xend = ModmaxL, yend = modmaxd),linetype="dashed", size = 0.2) +
  geom_segment(data = All_posterior2, aes(x = HighmaxL, y = 0, xend = HighmaxL, yend = highmaxd),linetype="dashed", size = 0.2) +
  geom_segment(data = All_posterior2, aes(x = PrefmaxL, y = 0, xend = PrefmaxL, yend = prefmaxd),linetype="dashed", size = 0.2) +
  
  geom_point(data = points_CIS2, aes(x = maxLik2, y = ymax2), size = 0.8) +
  geom_errorbarh(data = points_CIS2, aes(xmin = pl2, xmax = pu2, y = ymax2), size = 0.2) +
  
  geom_text_repel(data = points_CIS2, aes(label = sprintf("P = %s [%s, %s]", round(maxLik2,3), round(pl2,3), round(pu2,3)), 
                                         y = ymax2, x = pu2 ),  size=2.1, family = "sans", nudge_x = 0.005, segment.color = NA) +
  
  scale_fill_manual(values = c("UnsRate" = "black",
    "LowRate" = "tomato4",
    "ModRate" = "gold3",
    "HighRate" = "seagreen",
    "PrefRate" = "royalblue4"), labels = c("Unsuit. (<=1)", "Low (<=2)", "Mod. (<=3)", "High. (<=4)", "Pref. (<=5)")) +

  # scale_y_continuous(breaks = (seq(0, unsmax+10, by = 10))) +
  scale_x_continuous(breaks = (seq(0, 0.05, by = 0.01))) +
  coord_cartesian(xlim=c(0, 0.05), ylim=c(0, 600)) +
  labs(x = "Probability", y = "Density") +
  theme(panel.border = element_rect(linetype = 1, fill = NA), plot.margin = margin (10,20,10,10),
        panel.background = element_rect(fill = "white", colour = "grey90", size = 0.2),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        panel.grid.minor = element_line(colour = NA),
        axis.title.x = element_text(size=9, face = "bold", family = "sans"),
        axis.title.y = element_text(size=9, face = "bold", family = "sans"),
        axis.text=element_text(size=8, family = "sans"),
        legend.text=element_text(size=8, family = "sans", face = "italic"),
        legend.title=element_text(size=8, family = "sans", face = "bold", colour = NA))
# Densplot2
# 
tiff("Exports/Bayes_Density_40mProb.tiff", width = 174, height = 100, units="mm",res=600, compression = "lzw")
Densplot2# Make plot
dev.off()


###### Create A relative liklihood table for BHI Bayes Validation ######################################


# get liklihood vals for all poseriors:
df4Matrix <- data.frame(matrix(ncol = 1, nrow = nrow(All_posterior2)))
colnames(df4Matrix) <- "L.U.lik"
df4Matrix$L.U.lik <- All_posterior2$LowRate / All_posterior2$UnsRate
df4Matrix$M.U.lik <- All_posterior2$ModRate / All_posterior2$UnsRate
df4Matrix$H.U.lik <- All_posterior2$HighRate / All_posterior2$UnsRate
df4Matrix$P.U.lik <- All_posterior2$PrefRate / All_posterior2$UnsRate


df4Matrix$M.L.lik <- All_posterior2$ModRate / All_posterior2$LowRate
df4Matrix$H.L.lik <- All_posterior2$HighRate / All_posterior2$LowRate
df4Matrix$P.L.lik <- All_posterior2$PrefRate / All_posterior2$LowRate


df4Matrix$H.M.lik <- All_posterior2$HighRate / All_posterior2$ModRate
df4Matrix$P.M.lik <- All_posterior2$PrefRate / All_posterior2$ModRate

df4Matrix$P.H.lik <- All_posterior2$PrefRate / All_posterior2$HighRate

# convert liklihood posteriors into MAP liklihood with 95% Cridible intervals


L.U.MAP <-map_estimate(df4Matrix$L.U.lik)
L.U.CI <- posterior_interval(as.matrix(df4Matrix$L.U.lik), prob = 0.95, type = "central", pars = NULL, regex_pars = NULL)
L.U.Str <- sprintf("%s [%s, %s]", round(L.U.MAP,2), round(L.U.CI[1],2), round(L.U.CI[2],2))

M.U.MAP <-map_estimate(df4Matrix$M.U.lik)
M.U.CI <- posterior_interval(as.matrix(df4Matrix$M.U.lik), prob = 0.95, type = "central", pars = NULL, regex_pars = NULL)
M.U.Str <- sprintf("%s [%s, %s]", round(M.U.MAP,2), round(M.U.CI[1],2), round(M.U.CI[2],2))

H.U.MAP <-map_estimate(df4Matrix$H.U.lik)
H.U.CI <- posterior_interval(as.matrix(df4Matrix$H.U.lik), prob = 0.95, type = "central", pars = NULL, regex_pars = NULL)
H.U.Str <- sprintf("%s [%s, %s]", round(H.U.MAP,2), round(H.U.CI[1],2), round(H.U.CI[2],2))

P.U.MAP <-map_estimate(df4Matrix$P.U.lik)
P.U.CI <- posterior_interval(as.matrix(df4Matrix$P.U.lik), prob = 0.95, type = "central", pars = NULL, regex_pars = NULL)
P.U.Str <- sprintf("%s [%s, %s]", round(P.U.MAP,2), round(P.U.CI[1],2), round(P.U.CI[2],2))


M.L.MAP <-map_estimate(df4Matrix$M.L.lik)
M.L.CI <- posterior_interval(as.matrix(df4Matrix$M.L.lik), prob = 0.95, type = "central", pars = NULL, regex_pars = NULL)
M.L.Str <- sprintf("%s [%s, %s]", round(M.L.MAP,2), round(M.L.CI[1],2), round(M.L.CI[2],2))

H.L.MAP <-map_estimate(df4Matrix$H.L.lik)
H.L.CI <- posterior_interval(as.matrix(df4Matrix$H.L.lik), prob = 0.95, type = "central", pars = NULL, regex_pars = NULL)
H.L.Str <- sprintf("%s [%s, %s]", round(H.L.MAP,2), round(H.L.CI[1],2), round(H.L.CI[2],2))

P.L.MAP <-map_estimate(df4Matrix$P.L.lik)
P.L.CI <- posterior_interval(as.matrix(df4Matrix$P.L.lik), prob = 0.95, type = "central", pars = NULL, regex_pars = NULL)
P.L.Str <- sprintf("%s [%s, %s]", round(P.L.MAP,2), round(P.L.CI[1],2), round(P.L.CI[2],2))


H.M.MAP <-map_estimate(df4Matrix$H.M.lik)
H.M.CI <- posterior_interval(as.matrix(df4Matrix$H.M.lik), prob = 0.95, type = "central", pars = NULL, regex_pars = NULL)
H.M.Str <- sprintf("%s [%s, %s]", round(H.M.MAP,2), round(H.M.CI[1],2), round(H.M.CI[2],2))

P.M.MAP <-map_estimate(df4Matrix$P.M.lik)
P.M.CI <- posterior_interval(as.matrix(df4Matrix$P.M.lik), prob = 0.95, type = "central", pars = NULL, regex_pars = NULL)
P.M.Str <- sprintf("%s [%s, %s]", round(P.M.MAP,2), round(P.M.CI[1],2), round(P.M.CI[2],2))


P.H.MAP <-map_estimate(df4Matrix$P.H.lik)
P.H.CI <- posterior_interval(as.matrix(df4Matrix$P.H.lik), prob = 0.95, type = "central", pars = NULL, regex_pars = NULL)
P.H.Str <- sprintf("%s [%s, %s]", round(P.H.MAP,2), round(P.H.CI[1],2), round(P.H.CI[2],2))


n <- c("Unsuitable (<=1)", L.U.Str, M.U.Str, H.U.Str, P.U.Str)
r <- c("", "Low (<=2)", M.L.Str, H.L.Str, P.L.Str)
o <- c("", "", "Moderate (<=3)", H.M.Str, P.M.Str)
f <- c("", "", "", "High (<=4)", P.H.Str)
p <- c("", "", "", "", "Preferred (<=5)")

Likli_Matrix <- data.frame(n, r, o, f, p)

# Table headers/formatting requires editing in word...
tab_df(Likli_Matrix,
       file="Exports/BHI_Lik_Table.doc",col.header = FALSE, alternate.rows = TRUE) 



######## Susperseded ##################


# All_reaches$oVC_EX[is.na(All_reaches$oVC_EX)] <- 0 
# All_reaches$iVeg_40[is.na(All_reaches$iVeg_40)] <- 0
# All_reaches$iVeg_10[is.na(All_reaches$iVeg_10)] <- 0
# 
# # All_reaches <- All_reaches[!All_reaches$iGeo_Width == 38.55876,] # Delete all reaches where river is wider than search buffer allows.
# All_reaches <- All_reaches[!is.na(All_reaches$iVeg_40),]
# # version with oVC_EX
# All_reaches  <-
#   All_reaches  %>% mutate(Categ =
#                             # ifelse(oVC_EX <= 0, "None",
#                                    ifelse(oVC_EX < 1, "Rare",
#                                           ifelse(oVC_EX < 5, "Occasional",
#                                                  ifelse(oVC_EX <15, "Frequent",
#                                                         "Pervasive"))))#)
# 
# 
# # None_All <- All_reaches[All_reaches$Categ == "None",]
# Rare_All <- All_reaches[All_reaches$Categ == "Rare",]
# Occ_All <- All_reaches[All_reaches$Categ == "Occasional",]
# Fre_All <- All_reaches[All_reaches$Categ == "Frequent",]
# Perv_All <- All_reaches[All_reaches$Categ == "Pervasive",]
# 
# All_Cat_list <- list(#nA = nrow(None_All), 
#                      nB = nrow(Rare_All), nC = nrow(Occ_All), nD = nrow(Fre_All), nE = nrow(Perv_All), 
#                      #sA = nrow(None_All[None_All$Active>0,]), 
#                      sB = nrow(Rare_All[Rare_All$Active>0,]), sC = nrow(Occ_All[Occ_All$Active>0,]), 
#                      sD = nrow(Fre_All[Fre_All$Active>0,]), sE = nrow(Perv_All[Perv_All$Active>0,]))
# 
# 
# 
# model_string <- "
# // Here we define the data we are going to pass into the model
# data {
#   // Number of trials
#   // int nA;
#   int nB;
#   int nC;
#   int nD;
#   int nE;
#   // Number of successes
#   // int sA;
#   int sB;
#   int sC;
#   int sD;
#   int sE;
#   
#   //int n; // Number of trials     # single model version
#   // int s;  // Number of successes # single model version
# }
# 
# // Here we define what 'unknowns' aka parameters we have.
# parameters {
#   // real<lower=0, upper=1> NoneRate;
#   real<lower=0, upper=1> RareRate;
#   real<lower=0, upper=1> OccRate;
#   real<lower=0, upper=1> FreqRate;
#   real<lower=0, upper=1> PervRate;
# }
# 
# // The generative model
# model {
#   // NoneRate ~ uniform(0, 1);
#   RareRate ~ uniform(0, 1);
#   OccRate ~ uniform(0, 1);
#   FreqRate ~ uniform(0, 1);
#   PervRate ~ uniform(0, 1);
#   
#   // sA ~ binomial(nA, NoneRate);
#   sB ~ binomial(nB, RareRate);
#   sC ~ binomial(nC, OccRate);
#   sD ~ binomial(nD, FreqRate);
#   sE ~ binomial(nE, PervRate);
# }
# "
# 
#Run the Stan Model
# 
# All_Cat_samples <- stan(model_code = model_string, data = All_Cat_list)
# traceplot(All_Cat_samples)
# plot(All_Cat_samples)
# 
# All_posterior <- as.data.frame(All_Cat_samples)
# 
# All_posterior <- select (All_posterior,-lp__)
# 
# ########### Get cridible intervals 

# #Credibility scores and mean for Category Probability
# 
# # NoneCI <- posterior_interval(as.matrix(All_posterior$NoneRate), prob = 0.95, type = "central",
# #                              pars = NULL, regex_pars = NULL)
# RareCI <- posterior_interval(as.matrix(All_posterior$RareRate), prob = 0.95, type = "central",
#                              pars = NULL, regex_pars = NULL)
# OccCI <- posterior_interval(as.matrix(All_posterior$OccRate), prob = 0.95, type = "central",
#                             pars = NULL, regex_pars = NULL)
# FreqCI <- posterior_interval(as.matrix(All_posterior$FreqRate), prob = 0.95, type = "central",
#                              pars = NULL, regex_pars = NULL)
# PervCI <- posterior_interval(as.matrix(All_posterior$PervRate), prob = 0.95, type = "central",
#                              pars = NULL, regex_pars = NULL)
# 
# ############## Get  Maximum A Posteriori (MAP) 
# pervmaxL<-map_estimate(All_posterior$PervRate)
# freqmaxL<-map_estimate(All_posterior$FreqRate)
# occmaxL<-map_estimate(All_posterior$OccRate)
# raremaxL<-map_estimate(All_posterior$RareRate)
# # nonemaxL<-map_estimate(All_posterior$NoneRate)
# 
# # pl <- c(NoneCI[1], RareCI[1], OccCI[1], FreqCI[1], PervCI[1])
# # MAP <- c(nonemaxL, raremaxL, occmaxL, freqmaxL, pervmaxL)
# # pu <- c(NoneCI[2], RareCI[2], OccCI[2], FreqCI[2], PervCI[2])
# # Category <- c("None", "Rare", "Occasional", "Frequent", "Pervasive")
# 
# pl <- c(RareCI[1], OccCI[1], FreqCI[1], PervCI[1])
# MAP <- c(raremaxL, occmaxL, freqmaxL, pervmaxL)
# pu <- c(RareCI[2], OccCI[2], FreqCI[2], PervCI[2])
# Category <- c("Rare", "Occasional", "Frequent", "Pervasive")
# 
# ############### Plotting The posterior distribution - Density plots
# 
# # get key params for density plot
# ggp <- ggplot(All_posterior, aes(x = PervRate)) + geom_density()
# ggp_build <- ggplot_build(ggp)[["data"]][[1]]
# pervmaxd = max(ggp_build$y)
# 
# ggp <- ggplot(All_posterior, aes(FreqRate)) + geom_density()
# ggp_build <- ggplot_build(ggp)[["data"]][[1]]
# freqmaxd = max(ggp_build$y)
# 
# ggp <- ggplot(All_posterior, aes(OccRate)) + geom_density()
# ggp_build <- ggplot_build(ggp)[["data"]][[1]]
# occmaxd = max(ggp_build$y)
# 
# ggp <- ggplot(All_posterior, aes(RareRate)) + geom_density()
# ggp_build <- ggplot_build(ggp)[["data"]][[1]]
# raremaxd = max(ggp_build$y)
# 
# # ggp <- ggplot(All_posterior, aes(NoneRate)) + geom_density()
# # ggp_build <- ggplot_build(ggp)[["data"]][[1]]
# # nonemaxd = max(ggp_build$y)
# 
# 
# # getting max bin heights for plotting mean lines
# # Set bin number:
# wid.bin = 0.01
# ggp <- ggplot(All_posterior, aes(x = PervRate)) + geom_histogram(binwidth = wid.bin)
# ggp_build <- ggplot_build(ggp)
# pervmax = max(ggp_build[["data"]][[1]][["count"]])
# ggp <- ggplot(All_posterior, aes(FreqRate)) + geom_histogram(binwidth = wid.bin)
# ggp_build <- ggplot_build(ggp)
# freqmax = max(ggp_build[["data"]][[1]][["count"]])
# ggp <- ggplot(All_posterior, aes(OccRate)) + geom_histogram(binwidth = wid.bin)
# ggp_build <- ggplot_build(ggp)
# occmax = max(ggp_build[["data"]][[1]][["count"]])
# ggp <- ggplot(All_posterior, aes(RareRate)) + geom_histogram( binwidth = wid.bin)
# ggp_build <- ggplot_build(ggp)
# raremax = max(ggp_build[["data"]][[1]][["count"]])
# # ggp <- ggplot(All_posterior, aes(NoneRate)) + geom_histogram(binwidth = wid.bin)
# # ggp_build <- ggplot_build(ggp)
# # nonemax = max(ggp_build[["data"]][[1]][["count"]])
# 
# # maxLik <- c(nonemaxL, raremaxL, occmaxL, freqmaxL, pervmaxL)
# # ymax <- c(nonemaxd, raremaxd, occmaxd, freqmaxd, pervmaxd) 
# # histmax <- c(nonemax, raremax, occmax, freqmax, pervmax)
# 
# maxLik <- c(raremaxL, occmaxL, freqmaxL, pervmaxL)
# ymax <- c(raremaxd, occmaxd, freqmaxd, pervmaxd) 
# histmax <- c(raremax, occmax, freqmax, pervmax)
# 
# points_CIS <- data.frame(Category, pl, maxLik, pu, ymax, histmax)
# # points_CIS[,-1] <-round(points_CIS[,-1],5)
# 
# reshapedPost <- melt(All_posterior)
# names(reshapedPost)[names(reshapedPost) == "variable"] <- "Category"
# 
# 
# Densplot <- ggplot(NULL) + 
#   geom_density(data = reshapedPost, aes(x=value, fill=Category), alpha=0.4, colour = "grey50", size = 0.2) +
#   
#   # geom_segment(data = All_posterior, aes(x = nonemaxL, y = 0, xend = nonemaxL, yend = nonemaxd),linetype="dashed", size = 0.2) +
#   geom_segment(data = All_posterior, aes(x = raremaxL, y = 0, xend = raremaxL, yend = raremaxd),linetype="dashed", size = 0.2) +
#   geom_segment(data = All_posterior, aes(x = occmaxL, y = 0, xend = occmaxL, yend = occmaxd),linetype="dashed", size = 0.2) +
#   geom_segment(data = All_posterior, aes(x = freqmaxL, y = 0, xend = freqmaxL, yend = freqmaxd),linetype="dashed", size = 0.2) +
#   geom_segment(data = All_posterior, aes(x = pervmaxL, y = 0, xend = pervmaxL, yend = pervmaxd),linetype="dashed", size = 0.2) +
#   
#   geom_point(data = points_CIS, aes(x = maxLik, y = ymax), size = 0.8) +
#   geom_errorbarh(data = points_CIS, aes(xmin = pl, xmax = pu, y = ymax), size = 0.2) +
#   
#   geom_text_repel(data = points_CIS, aes(label = sprintf("P = %s [%s, %s]", round(maxLik,3), round(pl,3), round(pu,3)), 
#                                    y = ymax, x = pu ),  size=1.8, family = "sans", nudge_x = 0.005, segment.color = NA) +
#   
#   scale_fill_manual(values = c(#"NoneRate" = "black", 
#                                "RareRate" = "orange", 
#                                "OccRate" = "Yellow", 
#                                "FreqRate" = "green", 
#                                "PervRate" = "blue"), labels = c("Rare", "Occasional", "Frequent", "Pervasive")) +
#   
#   # scale_y_continuous(breaks = (seq(0, raremax+10, by = 10))) +
#   scale_x_continuous(breaks = (seq(0, 0.05, by = 0.005))) +
#   coord_cartesian(xlim=c(0, 0.05), ylim=c(0, 1000)) +
#   labs(x = "Probability", y = "Density") +
#   theme(panel.border = element_rect(linetype = 1, fill = NA), plot.margin = margin (10,20,10,10),
#         panel.background = element_rect(fill = "white", colour = "grey90", size = 0.2),
#         panel.grid.major = element_line(colour = "grey90", size = 0.2),
#         panel.grid.minor = element_line(colour = NA),
#         axis.title.x = element_text(size=7, face = "italic", family = "sans"),
#         axis.title.y = element_text(size=7, face = "italic", family = "sans"),
#         axis.text=element_text(size=6, family = "sans"),
#         legend.text=element_text(size=7, family = "sans", face = "italic"),
#         legend.title=element_text(size=8, family = "sans", face = "bold"))
# # Densplot
# 
# tiff("Exports/Bayes_Density_VegCap.tiff", width = 174, height = 100, units="mm",res=600, compression = "lzw")
# Densplot# Make plot
# dev.off()




