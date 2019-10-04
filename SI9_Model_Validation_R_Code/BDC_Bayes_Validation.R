### Quick note on Rstan install: I tried this with R 3.6 but there is an error which seems really fiddly to fix so I've run this in R 3.5

# INstall Stan
# See https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started 


# # check if rstan exists and delete it.
# remove.packages("rstan")
# if (file.exists(".RData")) file.remove(".RData")

# .rs.restartR() # restart R

# # install
# install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)
# 
# # install.packages("https://win-builder.r-project.org/1vUk5Gxm9QqM/rstan_2.19.1.zip", repos = NULL)
# 
# #If this line ultimately returns TRUE, then your C++ toolchain is properly installed and you can jump to the next section.
# pkgbuild::has_build_tools(debug = TRUE)
# 
# # This step is optional, but it can result in compiled Stan programs that execute much faster than they otherwise would. Simply paste the following into R once
# dotR <- file.path(Sys.getenv("HOME"), ".R")
# if (!file.exists(dotR)) dir.create(dotR)
# M <- file.path(dotR, ifelse(.Platform$OS.type == "windows", "Makevars.win", "Makevars"))
# if (!file.exists(M)) file.create(M)
# cat("\nCXX14FLAGS=-O3 -march=native -mtune=native",
#     if( grepl("^darwin", R.version$os)) "CXX14FLAGS += -arch x86_64 -ftemplate-depth-256" else
#       if (.Platform$OS.type == "windows") "CXX11FLAGS=-O3 -march=native -mtune=native" else
#         "CXX14FLAGS += -fPIC",
#     file = M, sep = "\n", append = TRUE)

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

Act_reaches  <-  All_reaches[All_reaches$Active == 1 | All_reaches$n_dams > 0,]

Act_reaches  <-
  Act_reaches  %>% mutate(Categ =
                            ifelse(BDC ==0, "None",
                                   ifelse(BDC < 1, "Rare",
                                          ifelse(BDC < 5, "Occasional",
                                                 ifelse(BDC <15, "Frequent",
                                                        "Pervasive")))))



None_Act <- Act_reaches[Act_reaches$Categ == "None",]
# None_list <- list(n = nrow(None_Act), s = nrow(None_Act[None_Act$n_dams>0,]))
# 
Rare_Act <- Act_reaches[Act_reaches$Categ == "Rare",]
# Rare_list <- list(n = nrow(Rare_Act), s = nrow(Rare_Act[Rare_Act$n_dams>0,]))
# 
Occ_Act <- Act_reaches[Act_reaches$Categ == "Occasional",]
# Occ_list <- list(n = nrow(Occ_Act), s = nrow(Occ_Act[Occ_Act$n_dams>0,]))
# 
Fre_Act <- Act_reaches[Act_reaches$Categ == "Frequent",]
# Fre_list <- list(n = nrow(Fre_Act), s = nrow(Fre_Act[Fre_Act$n_dams>0,]))
# 
Perv_Act <- Act_reaches[Act_reaches$Categ == "Pervasive",]
# Perv_list <- list(n = nrow(Perv_Act), s = nrow(Perv_Act[Perv_Act$n_dams>0,]))

All_Cat_list <- list(nA = nrow(None_Act), nB = nrow(Rare_Act), 
                     nC = nrow(Occ_Act), nD = nrow(Fre_Act), nE = nrow(Perv_Act), 
                     sA = nrow(None_Act[None_Act$n_dams>0,]), sB = nrow(Rare_Act[Rare_Act$n_dams>0,]), 
                     sC = nrow(Occ_Act[Occ_Act$n_dams>0,]), sD = nrow(Fre_Act[Fre_Act$n_dams>0,]), sE = nrow(Perv_Act[Perv_Act$n_dams>0,]))



######### The Stan model as a string.##########
model_string <- "
// Here we define the data we are going to pass into the model
data {
  // Number of trials
  int nA;
  int nB;
  int nC;
  int nD;
  int nE;
  // Number of successes
  int sA;
  int sB;
  int sC;
  int sD;
  int sE;
  
  //int n; // Number of trials     # single model version
  // int s;  // Number of successes # single model version
}

// Here we define what 'unknowns' aka parameters we have.
parameters {
  real<lower=0, upper=1> NoneRate;
  real<lower=0, upper=1> RareRate;
  real<lower=0, upper=1> OccRate;
  real<lower=0, upper=1> FreqRate;
  real<lower=0, upper=1> PervRate;
}

// The generative model
model {
  NoneRate ~ uniform(0, 1);
  RareRate ~ uniform(0, 1);
  OccRate ~ uniform(0, 1);
  FreqRate ~ uniform(0, 1);
  PervRate ~ uniform(0, 1);
  
  sA ~ binomial(nA, NoneRate);
  sB ~ binomial(nB, RareRate);
  sC ~ binomial(nC, OccRate);
  sD ~ binomial(nD, FreqRate);
  sE ~ binomial(nE, PervRate);
}
"

######## Run the Stan Model ################

All_Cat_samples <- stan(model_code = model_string, data = All_Cat_list)
traceplot(All_Cat_samples)
plot(All_Cat_samples)

All_posterior <- as.data.frame(All_Cat_samples)

All_posterior <- dplyr::select (All_posterior, -lp__)

########### Get cridible intervals ########################

#Credibility scores and mean for Category Probability

NoneCI <- posterior_interval(as.matrix(All_posterior$NoneRate), prob = 0.95, type = "central",
                             pars = NULL, regex_pars = NULL)
RareCI <- posterior_interval(as.matrix(All_posterior$RareRate), prob = 0.95, type = "central",
                             pars = NULL, regex_pars = NULL)
OccCI <- posterior_interval(as.matrix(All_posterior$OccRate), prob = 0.95, type = "central",
                            pars = NULL, regex_pars = NULL)
FreqCI <- posterior_interval(as.matrix(All_posterior$FreqRate), prob = 0.95, type = "central",
                             pars = NULL, regex_pars = NULL)
PervCI <- posterior_interval(as.matrix(All_posterior$PervRate), prob = 0.95, type = "central",
                             pars = NULL, regex_pars = NULL)

############## Get  Maximum A Posteriori (MAP) ######################################
pervmaxL<-map_estimate(All_posterior$PervRate)
freqmaxL<-map_estimate(All_posterior$FreqRate)
occmaxL<-map_estimate(All_posterior$OccRate)
raremaxL<-map_estimate(All_posterior$RareRate)
nonemaxL<-map_estimate(All_posterior$NoneRate)

pl <- c(NoneCI[1], RareCI[1], OccCI[1], FreqCI[1], PervCI[1])
MAP <- c(nonemaxL, raremaxL, occmaxL, freqmaxL, pervmaxL)
pu <- c(NoneCI[2], RareCI[2], OccCI[2], FreqCI[2], PervCI[2])
Category <- c("None", "Rare", "Occasional", "Frequent", "Pervasive")

############### Plotting The posterior distribution - Density plots ##################################

# get key params for density plot
ggp <- ggplot(All_posterior, aes(x = PervRate)) + geom_density()
ggp_build <- ggplot_build(ggp)[["data"]][[1]]
pervmaxd = max(ggp_build$y)

ggp <- ggplot(All_posterior, aes(FreqRate)) + geom_density()
ggp_build <- ggplot_build(ggp)[["data"]][[1]]
freqmaxd = max(ggp_build$y)

ggp <- ggplot(All_posterior, aes(OccRate)) + geom_density()
ggp_build <- ggplot_build(ggp)[["data"]][[1]]
occmaxd = max(ggp_build$y)

ggp <- ggplot(All_posterior, aes(RareRate)) + geom_density()
ggp_build <- ggplot_build(ggp)[["data"]][[1]]
raremaxd = max(ggp_build$y)

ggp <- ggplot(All_posterior, aes(NoneRate)) + geom_density()
ggp_build <- ggplot_build(ggp)[["data"]][[1]]
nonemaxd = max(ggp_build$y)


# getting max bin heights for plotting mean lines
# Set bin number:
wid.bin = 0.01
ggp <- ggplot(All_posterior, aes(x = PervRate)) + geom_histogram(binwidth = wid.bin)
ggp_build <- ggplot_build(ggp)
pervmax = max(ggp_build[["data"]][[1]][["count"]])
ggp <- ggplot(All_posterior, aes(FreqRate)) + geom_histogram(binwidth = wid.bin)
ggp_build <- ggplot_build(ggp)
freqmax = max(ggp_build[["data"]][[1]][["count"]])
ggp <- ggplot(All_posterior, aes(OccRate)) + geom_histogram(binwidth = wid.bin)
ggp_build <- ggplot_build(ggp)
occmax = max(ggp_build[["data"]][[1]][["count"]])
ggp <- ggplot(All_posterior, aes(RareRate)) + geom_histogram( binwidth = wid.bin)
ggp_build <- ggplot_build(ggp)
raremax = max(ggp_build[["data"]][[1]][["count"]])
ggp <- ggplot(All_posterior, aes(NoneRate)) + geom_histogram(binwidth = wid.bin)
ggp_build <- ggplot_build(ggp)
nonemax = max(ggp_build[["data"]][[1]][["count"]])

maxLik <- c(nonemaxL, raremaxL, occmaxL, freqmaxL, pervmaxL)
ymax <- c(nonemaxd, raremaxd, occmaxd, freqmaxd, pervmaxd) 
histmax <- c(nonemax, raremax, occmax, freqmax, pervmax)

points_CIS <- data.frame(Category, pl, maxLik, pu, ymax, histmax)
# points_CIS[,-1] <-round(points_CIS[,-1],5)

reshapedPost <- melt(All_posterior)
names(reshapedPost)[names(reshapedPost) == "variable"] <- "Category"


Densplot <- ggplot(NULL) + 
  geom_density(data = reshapedPost, aes(x=value, fill=Category), alpha=0.4, colour = "grey50", size = 0.2) +
  
  geom_segment(data = All_posterior, aes(x = nonemaxL, y = 0, xend = nonemaxL, yend = nonemaxd),linetype="dashed", size = 0.2) +
  geom_segment(data = All_posterior, aes(x = raremaxL, y = 0, xend = raremaxL, yend = raremaxd),linetype="dashed", size = 0.2) +
  geom_segment(data = All_posterior, aes(x = occmaxL, y = 0, xend = occmaxL, yend = occmaxd),linetype="dashed", size = 0.2) +
  geom_segment(data = All_posterior, aes(x = freqmaxL, y = 0, xend = freqmaxL, yend = freqmaxd),linetype="dashed", size = 0.2) +
  geom_segment(data = All_posterior, aes(x = pervmaxL, y = 0, xend = pervmaxL, yend = pervmaxd),linetype="dashed", size = 0.2) +
  
  geom_point(data = points_CIS, aes(x = maxLik, y = ymax), size = 0.8) +
  geom_errorbarh(data = points_CIS, aes(xmin = pl, xmax = pu, y = ymax), size = 0.2) +
  
  geom_text_repel(data = points_CIS, aes(label = sprintf("P = %s [%s, %s]", round(maxLik,3), round(pl,3), round(pu,3)), 
                                         y = ymax, x = pu ),  size=2.1, family = "sans", nudge_x = 0.02, segment.color = NA) +
  
  scale_fill_manual(values = c("NoneRate" = "black",
                                           "RareRate" = "orange",
                                           "OccRate" = "Yellow",
                                           "FreqRate" = "green",
                                           "PervRate" = "blue"), labels = c("None", "Rare", "Occasional", "Frequent", "Pervasive"),) +
  # 
  scale_y_continuous(breaks = (seq(0, raremax+10, by = 10))) +
  scale_x_continuous(breaks = (seq(0, 0.3, by = 0.05))) +
  coord_cartesian(ylim=c(0, 50)) +
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
Densplot

tiff("Exports/Bayesian_Density.tiff", width = 174, height = 100, units="mm",res=600, compression = "lzw")
Densplot# Make plot
dev.off()



####################### Exporting Tables #########################################

# # Table 1 - Probability table - not really required now as data included in density plot.
# df <- data.frame(Category, pl, maxLik, pu)
# 
# 
# df[,-1] <-round(df[,-1],3)
# 
# tab_df(df,
#        file="Exports/ProbabilityValues.doc")


# Table 2: Some kinf of matrix type table of relative liklihood.

# get liklihood vals for all poseriors:
df4Matrix <- data.frame(matrix(ncol = 1, nrow = nrow(All_posterior)))
colnames(df4Matrix) <- "R.N.lik"
df4Matrix$R.N.lik <- All_posterior$RareRate / All_posterior$NoneRate
df4Matrix$O.N.lik <- All_posterior$OccRate / All_posterior$NoneRate
df4Matrix$F.N.lik <- All_posterior$FreqRate / All_posterior$NoneRate
df4Matrix$P.N.lik <- All_posterior$PervRate / All_posterior$NoneRate


df4Matrix$O.R.lik <- All_posterior$OccRate / All_posterior$RareRate
df4Matrix$F.R.lik <- All_posterior$FreqRate / All_posterior$RareRate
df4Matrix$P.R.lik <- All_posterior$PervRate / All_posterior$RareRate


df4Matrix$F.O.lik <- All_posterior$FreqRate / All_posterior$OccRate
df4Matrix$P.O.lik <- All_posterior$PervRate / All_posterior$OccRate

df4Matrix$P.F.lik <- All_posterior$PervRate / All_posterior$FreqRate

# convert liklihood posteriors into MAP liklihood with 95% Cridible intervals


R.N.MAP <-map_estimate(df4Matrix$R.N.lik)
R.N.CI <- posterior_interval(as.matrix(df4Matrix$R.N.lik), prob = 0.95, type = "central", pars = NULL, regex_pars = NULL)
R.N.Str <- sprintf("%s [%s, %s]", round(R.N.MAP,2), round(R.N.CI[1],2), round(R.N.CI[2],2))

O.N.MAP <-map_estimate(df4Matrix$O.N.lik)
O.N.CI <- posterior_interval(as.matrix(df4Matrix$O.N.lik), prob = 0.95, type = "central", pars = NULL, regex_pars = NULL)
O.N.Str <- sprintf("%s [%s, %s]", round(O.N.MAP,2), round(O.N.CI[1],2), round(O.N.CI[2],2))

F.N.MAP <-map_estimate(df4Matrix$F.N.lik)
F.N.CI <- posterior_interval(as.matrix(df4Matrix$F.N.lik), prob = 0.95, type = "central", pars = NULL, regex_pars = NULL)
F.N.Str <- sprintf("%s [%s, %s]", round(F.N.MAP,2), round(F.N.CI[1],2), round(F.N.CI[2],2))

P.N.MAP <-map_estimate(df4Matrix$P.N.lik)
P.N.CI <- posterior_interval(as.matrix(df4Matrix$P.N.lik), prob = 0.95, type = "central", pars = NULL, regex_pars = NULL)
P.N.Str <- sprintf("%s [%s, %s]", round(P.N.MAP,2), round(P.N.CI[1],2), round(P.N.CI[2],2))


O.R.MAP <-map_estimate(df4Matrix$O.R.lik)
O.R.CI <- posterior_interval(as.matrix(df4Matrix$O.R.lik), prob = 0.95, type = "central", pars = NULL, regex_pars = NULL)
O.R.Str <- sprintf("%s [%s, %s]", round(O.R.MAP,2), round(O.R.CI[1],2), round(O.R.CI[2],2))

F.R.MAP <-map_estimate(df4Matrix$F.R.lik)
F.R.CI <- posterior_interval(as.matrix(df4Matrix$F.R.lik), prob = 0.95, type = "central", pars = NULL, regex_pars = NULL)
F.R.Str <- sprintf("%s [%s, %s]", round(F.R.MAP,2), round(F.R.CI[1],2), round(F.R.CI[2],2))

P.R.MAP <-map_estimate(df4Matrix$P.R.lik)
P.R.CI <- posterior_interval(as.matrix(df4Matrix$P.R.lik), prob = 0.95, type = "central", pars = NULL, regex_pars = NULL)
P.R.Str <- sprintf("%s [%s, %s]", round(P.R.MAP,2), round(P.R.CI[1],2), round(P.R.CI[2],2))


F.O.MAP <-map_estimate(df4Matrix$F.O.lik)
F.O.CI <- posterior_interval(as.matrix(df4Matrix$F.O.lik), prob = 0.95, type = "central", pars = NULL, regex_pars = NULL)
F.O.Str <- sprintf("%s [%s, %s]", round(F.O.MAP,2), round(F.O.CI[1],2), round(F.O.CI[2],2))

P.O.MAP <-map_estimate(df4Matrix$P.O.lik)
P.O.CI <- posterior_interval(as.matrix(df4Matrix$P.O.lik), prob = 0.95, type = "central", pars = NULL, regex_pars = NULL)
P.O.Str <- sprintf("%s [%s, %s]", round(P.O.MAP,2), round(P.O.CI[1],2), round(P.O.CI[2],2))


P.F.MAP <-map_estimate(df4Matrix$P.F.lik)
P.F.CI <- posterior_interval(as.matrix(df4Matrix$P.F.lik), prob = 0.95, type = "central", pars = NULL, regex_pars = NULL)
P.F.Str <- sprintf("%s [%s, %s]", round(P.F.MAP,2), round(P.F.CI[1],2), round(P.F.CI[2],2))


n <- c("None", R.N.Str, O.N.Str, F.N.Str, P.N.Str)
r <- c("", "Rare", O.R.Str, F.R.Str, P.R.Str)
o <- c("", "", "Occasional", F.O.Str, P.O.Str)
f <- c("", "", "", "Frequent", P.F.Str)
p <- c("", "", "", "", "Pervasive")

Likli_Matrix <- data.frame(n, r, o, f, p)

# Table headers/formatting requires editing in word...
tab_df(Likli_Matrix,
       file="Exports/BDC_Lik_Tab.doc",col.header = FALSE, alternate.rows = TRUE) 


########### Superseded #####################
# Plot to view relative likelihood distributions - not super helpful - table above more helpful

# plotList <- list()
# 
# for(i in names(df4Matrix)){
#    var = data.frame(df4Matrix[[i]])
#    colnames(var) <- "Dens"
#   ThePlot <- ggplot(var, aes(x = Dens)) +
#     geom_histogram(alpha = 0.3, fill = "purple", bins = 100) +
#     # geom_density(alpha = 0.3, fill = "purple", colour = "grey50") +
#     coord_cartesian(xlim=c(0, 40)) +
#     theme(panel.border = element_rect(linetype = 1, fill = NA), plot.margin = margin (10,20,10,10),
#           panel.background = element_rect(fill = "white", colour = "grey90", size = 0.2),
#           panel.grid.major = element_line(colour = "grey90", size = 0.2),
#           panel.grid.minor = element_line(colour = NA),
#           axis.title.x = element_text(size=7, face = "italic", family = "sans"),
#           axis.title.y = element_text(size=7, face = "italic", family = "sans"),
#           axis.text=element_text(size=6, family = "sans"),
#           legend.text=element_text(size=7, family = "sans", face = "italic"),
#           legend.title=element_text(size=8, family = "sans", face = "bold"))
#   
#   print(ThePlot)
#   plotList[[i]] <- ThePlot
  

################################# Histogram plot of posterior distributions - less helpful than density plot 
# Plot probability as histograms
# histplot <- ggplot(All_posterior) +
#   
#   geom_histogram(aes(x=NoneRate, fill = "None"), alpha = 0.4, binwidth = wid.bin) +
#   geom_histogram(aes(x=RareRate, fill = "Rare"), alpha = 0.4, binwidth = wid.bin) +
#   geom_histogram(aes(x=OccRate, fill = "Occasional"),alpha = 0.4, binwidth = wid.bin) +
#   geom_histogram(aes(x=FreqRate, fill = "Frequent"),alpha = 0.4, binwidth = wid.bin) +
#   geom_histogram(aes(x=PervRate, fill = "Pervasive"),alpha = 0.4, binwidth = wid.bin) +
#   
#   stat_bin(aes(x=NoneRate), size = 0.2, geom="step", bins = n.bins, colour = "grey20", binwidth = wid.bin, position = position_nudge(x = -wid.bin/2), alpha = 0.4) +
#   stat_bin(aes(x=RareRate), size = 0.2, geom="step", bins = n.bins, colour = "grey20", binwidth = wid.bin, position = position_nudge(x = -wid.bin/2), alpha = 0.4) +
#   stat_bin(aes(x=OccRate), size = 0.2, geom="step", bins = n.bins, colour = "grey20", binwidth = wid.bin, position = position_nudge(x = -wid.bin/2), alpha = 0.4) +
#   stat_bin(aes(x=FreqRate), size = 0.2, geom="step", bins = n.bins, colour = "grey20", binwidth = wid.bin, position = position_nudge(x = -wid.bin/2), alpha = 0.4) +
#   stat_bin(aes(x=PervRate), size = 0.2, geom="step", bins = n.bins, colour = "grey20", binwidth = wid.bin, position = position_nudge(x = -wid.bin/2), alpha = 0.4) +
#   
#   geom_segment(aes(x = nonemaxL, y = 0, xend = nonemaxL, yend = nonemax),linetype="dashed", size = 0.2) +
#   geom_segment(aes(x = raremaxL, y = 0, xend = raremaxL, yend = raremax),linetype="dashed", size = 0.2) +
#   geom_segment(aes(x = occmaxL, y = 0, xend = occmaxL, yend = occmax),linetype="dashed", size = 0.2) +
#   geom_segment(aes(x = freqmaxL, y = 0, xend = freqmaxL, yend = freqmax),linetype="dashed", size = 0.2) +
#   geom_segment(aes(x = pervmaxL, y = 0, xend = pervmaxL, yend = pervmax),linetype="dashed", size = 0.2) +
#   
#   geom_point(data = points_CIS, aes(x = maxLik, y = histmax), size = 0.8) +
#   geom_errorbarh(data = points_CIS, aes(xmin = pl, xmax = pu, y = histmax), size = 0.2) +
#   
#   
#   
#   scale_y_continuous(breaks = (seq(0, raremax+150, by = 100))) +
#   coord_cartesian(ylim=c(0, raremax+150))+
#   scale_fill_manual(values = c("None" = "black",
#                                "Rare" = "orange",
#                                "Occasional" = "Yellow",
#                                "Frequent" = "green",
#                                "Pervasive" = "blue"), 
#                     breaks=c("None","Rare","Occasional", "Frequent", "Pervasive"),
#                     name = "Category") +
#   labs(x = "Probability", y = "Count") +
#   theme(panel.border = element_rect(linetype = 1, fill = NA), plot.margin = margin (10,20,10,10),
#         panel.background = element_rect(fill = "white", colour = "grey90", size = 0.2),
#         panel.grid.major = element_line(colour = "grey90", size = 0.2),
#         panel.grid.minor = element_line(colour = NA),
#         axis.title.x = element_text(size=7, face = "italic", family = "sans"),
#         axis.title.y = element_text(size=7, face = "italic", family = "sans"),
#         axis.text=element_text(size=6, family = "sans"),
#         legend.text=element_text(size=7, family = "sans", face = "italic"),
#         legend.title=element_text(size=8, family = "sans", face = "bold"))
# 
# histplot
# 
# 
# tiff("Exports/Bayesian_Hist.tiff", width = 174, height = 100, units="mm",res=600, compression = "lzw")
# histplot# Make plot
# dev.off()




