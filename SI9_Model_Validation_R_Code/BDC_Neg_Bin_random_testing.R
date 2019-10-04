#------------ Description --------------------
# This script presents the model selection process for development of the statistical model for observed dam counts and
# modelled dam capacity in Graham, et al. (in prep). Additionally, this script shows the procedure used for the monte 
#carlo cross-validation method.

#----------- Load Files ---------------------
# rm(list = ls())

# full_results <- read.csv("AllDams.csv")
full_results <- read.csv("AllDams_noBamff.csv")


catchments <- c("Tay", "Otter", "ComHead")
# catchments <- ("Tay")
# catchments <- ("Otter")
# catchments <- c("BoVe", "ComHead")
# catchments <- c("Otter", "BoVe", "ComHead")

Dams <- full_results[full_results$Catchment %in% catchments,]



#--------- Load Packages -------------------

list.of.packages <- c("dplyr","ggplot2", "aod", "cowplot", "quantreg", "rcompanion","hexbin", "viridis", "pscl", 
                      "boot", "nonnest2", "AER", "sjPlot", "stargazer") # stargazer not needed?
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

if (!("countreg" %in% installed.packages()[,"Package"])) install.packages("countreg", repos="http://R-Forge.R-project.org")


library(pscl)
library(boot)
library(AER)
library(countreg)
library(nonnest2)
library(viridis)
library(dplyr)
library(ggplot2)
library(aod)
library(cowplot)
library(quantreg)
library(rcompanion)
library(hexbin)
library(sjPlot)
# library(stargazer)

#--- data wrangling -----
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



dam_reaches <- data.frame(count(Dams, reach_no, Catchment, iGeo_Len, BDC, Categ, Str_order)) ## this one!

dam_reaches$n_dams <- dam_reaches$n
dam_reaches <- subset(dam_reaches, select = -c(n))

dam_reaches$obs_dens <- ((dam_reaches$n_dams/dam_reaches$iGeo_Len)*1000)



dam_reaches$Categ<-factor(dam_reaches$Categ, levels=c("Rare", "Occasional", "Frequent", "Pervasive"))


dam_reaches$n_dams_mod <- ((dam_reaches$BDC/1000)*dam_reaches$iGeo_Len)


#------  Regression ------------------
# All_reaches <- read.csv("AllReaches.csv")
All_reaches <- read.csv("AllReaches_noBamff.csv")
All_reaches$Dam_PA = 0
All_reaches$Dam_PA[All_reaches$n_dams>0] = 1


All_reaches  <-
  All_reaches  %>% mutate(Categ =
                            ifelse(BDC ==0, "None",
                                   ifelse(BDC < 1, "Rare",
                                          ifelse(BDC < 5, "Occasional",
                                                 ifelse(BDC <15, "Frequent",
                                                        "Pervasive")))))


All_act_reaches  <- All_reaches[All_reaches$Active == 1 | All_reaches$n_dams > 0,]


# catchments <- c("Otter", "BoVe", "ComHead")
# catchments <- c("BoVe", "ComHead")
# catchments <- c("Tay")
# catchments <- c("Otter")
# catchments <- c("Tay", "Otter", "BoVe", "ComHead")
Act_reaches <- All_act_reaches[All_act_reaches$Catchment %in% catchments,]

# Act_reaches <- All_act_reaches[All_act_reaches$Catchment == "Otter",]

Act_reaches  <-
  Act_reaches  %>% mutate(Categ =
                    ifelse(BDC ==0, "None",
                           ifelse(BDC < 1, "Rare",
                                  ifelse(BDC < 5, "Occasional",
                                         ifelse(BDC <15, "Frequent",
                                                "Pervasive")))))


# get the observed dam density into this active reaches data frame
Act_reaches <- 
  Act_reaches %>% mutate(obs_dens = 
                           ifelse(Dam_PA == 0, 0,
                                dam_reaches$obs_dens[match(paste(Catchment, reach_no), 
                                                           paste(dam_reaches$Catch, dam_reaches$reach_no))]))
Act_reaches <- 
  Act_reaches %>% mutate(n_dams = 
                           ifelse(Dam_PA == 0, 0,
                                  dam_reaches$n_dams[match(paste(Catchment, reach_no), 
                                                             paste(dam_reaches$Catch, dam_reaches$reach_no))]))
Act_reaches$n_dams_mod <- ((Act_reaches$BDC/1000)*Act_reaches$iGeo_Len)
Act_reaches$obs_dens_round <- round(Act_reaches$obs_dens)
# order data

Act_reaches$Categ <- factor(Act_reaches$Categ ,c("None","Rare","Occasional", "Frequent", "Pervasive"))


# boxplot(obs_dens~Categ, data = Act_reaches)
# 
# plot(obs_dens~BDC, data = Act_reaches)
# plot(obs_dens_round~BDC, data = Act_reaches)
# 
# val_aov2 <- lm(Act_reaches$obs_dens~Act_reaches$BDC)
# summary(val_aov2)
# abline(val_aov2)
# par(mfrow=c(2,2))
# plot(val_aov2)
# par(mfrow=c(1,1))


# plot(n_dams~n_dams_mod, data = Act_reaches)

poisson_test <- glm(n_dams ~ BDC , data = Act_reaches, family = "quasipoisson")
summary(poisson_test) # (Dispersion parameter for quasipoisson family taken to be 2.256568) Model is over-dispersed...
confint(poisson_test)
#### exploring zero inflation possion regression ... https://stats.idre.ucla.edu/r/dae/zip/




ggplot(Act_reaches, aes(n_dams)) + geom_histogram(binwidth = 0.1) + scale_x_log10()



m1 <- zeroinfl(n_dams ~ n_dams_mod, data = Act_reaches,dist = "negbin") # computationally demanding for the bootstrap but best looking
m2 <- zeroinfl(n_dams ~ n_dams_mod, data = Act_reaches,dist = "poisson")

m3 <- hurdle(n_dams ~ n_dams_mod, data = Act_reaches,dist = "poisson", zero.dist = "binomial")
m4 <- hurdle(n_dams ~ n_dams_mod, data = Act_reaches,dist = "negbin", zero.dist = "binomial") 

m5 <- glm.nb(n_dams ~ n_dams_mod, data = Act_reaches)

AIC(m1)
AIC(m2)
AIC(m3)
AIC(m4)
AIC(m5) # AIC score for ZINB is much lower than NB Hurdle 
summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)


mnull <- update(m1, . ~ 1)
pchisq(2 * (logLik(m1) - logLik(mnull)), df = 1, lower.tail = FALSE) 
mnull <- update(m2, . ~ 1)
pchisq(2 * (logLik(m2) - logLik(mnull)), df = 1, lower.tail = FALSE)
mnull <- update(m3, . ~ 1)
pchisq(2 * (logLik(m3) - logLik(mnull)), df = 1, lower.tail = FALSE)
mnull <- update(m4, . ~ 1)
pchisq(2 * (logLik(m4) - logLik(mnull)), df = 1, lower.tail = FALSE)
mnull <- update(m5, . ~ 1)
pchisq(2 * (logLik(m5) - logLik(mnull)), df = 1, lower.tail = FALSE)  # All models better than the null model!


summary(p1 <- glm(n_dams ~ n_dams_mod , data = Act_reaches, family = poisson)) # create poisson model for comparison
vuong(p1, m1)
vuong(p1, m2) # use vuong test to compare zeroinfl and poisson models - shows significant value - 
vuong(p1, m3)
vuong(m1, m2)
vuong(m1, m3)
vuong(m2, m3)
vuong(m4, m2) # m1 appears to be the best model  because it drives the intercept through zero... 
vuong(m2, m5) # However it computationally intensive

# the standard vuong test is not appropriate for "nested" models which the zero inflation models are so..  this version is more reliable!

vuongtest(m1,m2, nested = TRUE) # models are distinguishable and m1 is better fit than m2
vuongtest(m1,m3, nested = TRUE) # models are distinguishable and m1 is better fit than m3
vuongtest(m1,m4, nested = TRUE) # models are not distinguishable but m1 is better fit than m4
vuongtest(m1,m5, nested = TRUE) # models are distinguishable and m1 is better fit
vuongtest(m1,p1, nested = TRUE) # models are distinguishable and m1 is better fit


# check for over dispersion

par(mfrow=c(3,2))
rootogram(m1, max = 15)
rootogram(m2, max = 15)
rootogram(m3, max = 15)
rootogram(m4, max = 15)
rootogram(m5, max = 15)
rootogram(p1, max = 15)# No models look particularly over dispersed... neg bin models seem generally better...
par(mfrow=c(1,1))


dispersiontest(p1,trafo=1)


#--------------#
#SELECTED MODEL#
#--------------#
# no it looks lik m4 might be the winner...
# m1 <- zeroinfl(n_dams ~ n_dams_mod, data = Act_reaches,dist = "negbin") # CURRENT MODEL CHOICE

#--------------#
#------------ Bootstrapping -------------

count.coef1 <- as.numeric(dput(coef(m1, "count"))[1]) # obtain coefficients for start location of bootstrapping
count.coef2 <- as.numeric(dput(coef(m1, "count"))[2])
zero.coef1 <- as.numeric(dput(coef(m1, "zero"))[1])
zero.coef2 <- as.numeric(dput(coef(m1, "zero"))[2])

# coef(summary(m4))
MAX <- ceiling(max(Act_reaches$n_dams_mod))
LENG <- (round(MAX, digits = 2)*1000)+1

Predict2 <- with(Act_reaches, data.frame(n_dams_mod = round(rep(seq(from = 0, to = MAX, length.out = LENG)),digits = 3)))
# For hurdle model but haivng issues with the Upper CL
# f <- function(data, i, newdata, c1, c2, c3, c4) {
#   require(pscl)
#   m <- hurdle(n_dams ~ n_dams_mod, dist = "negbin", zero.dist = "binomial",data = data[i, ], start = list(count = c(c1, c2), zero = c(c3, c4)))
#   mparams <- as.vector(t(do.call(rbind, coef(summary(m)))[, 1:2]))
#   yhat <- predict(m, newdata, type = "response")
#   return(c(mparams, yhat))
# }

# for  ZINB
f <- function(data, i, newdata, c1, c2, c3, c4) {
  require(pscl)
  m <- zeroinfl(n_dams ~ n_dams_mod, dist = "negbin",data = data[i, ], start = list(count = c(c1, c2), zero = c(c3, c4)))
  mparams <- as.vector(t(do.call(rbind, coef(summary(m)))[, 1:2]))
  yhat <- predict(m, newdata, type = "response")
  return(c(mparams, yhat))
}

set.seed(10)

res2 <- boot(data = Act_reaches, f, newdata = Predict2, R = 100,  parallel = "snow", ncpus = 8,  c1 = count.coef1, c2=count.coef2, c3=zero.coef1, c4=zero.coef2)


yhat <- t(sapply(10 +(1:nrow(Predict2)), function(i) {   # 
  out <- boot.ci(res2, index = i, type = c("perc"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5]))
}))

Predict2<- cbind(Predict2, yhat)

# tail(Predict2)

# PLOTG <- ggplot(Predict2, aes(x = n_dams_mod, y = Est.1)) + 
#   geom_point(data = Act_reaches,aes(x = n_dams_mod,y = n_dams, colour = "Observations"),alpha = 0.6, position = position_jitter(w=0.01, h=0.05),
#              size = 1.8) +
#   geom_ribbon(aes(ymin = pLL,ymax = pUL), alpha = 0.4, fill = "coral3") + 
#   geom_line(aes(colour = "ZINB Model"),size = 1, linetype = 1) +
#   geom_line(aes(x = n_dams_mod,y = pLL, colour = "CIs"),size = 0.6, linetype = 2) +
#   geom_line(aes(x = n_dams_mod,y = pUL),size = 0.6, linetype = 2, colour = "grey37") +
#   scale_y_continuous(breaks = (seq(0, 10, by = 1)),expand = c(0.001, 0.4))+
#   scale_x_continuous(breaks = round(seq(0, 5, by = 1)),limits = c(-0.01,5.5))+
#   scale_colour_manual("legend",values = c( "grey37", "royalblue1","grey37"),breaks=c("Observations","ZINB Model","CIs"), guide = guide_legend(override.aes = list(
#     linetype = c("blank", "solid", "dotted" ), shape =  c(1, NA, NA), alpha = 1)))+
#   labs(x = "Modelled beaver dam capacity (BDC) per reach", y = "Number of Dams per reach") +
#   theme(panel.border = element_rect(linetype = 1, fill = NA), plot.margin = margin (10,20,10,10),
#         panel.background = element_rect(fill = "white", colour = "black"),
#         panel.grid.major = element_line(colour = "grey90"),
#         panel.grid.minor = element_line(colour = NA),
#         axis.title.x = element_text(size=11, face = "bold"),
#         axis.title.y = element_text(size=11, face = "bold"),
#         axis.text=element_text(size=10))
# PLOTG

#-----------Plotting ---------------
PLOTGb <- ggplot(Predict2, aes(x = n_dams_mod, y = Est.1)) + 
  geom_ribbon(aes(ymin = pLL,ymax = pUL),  alpha = 0.4, fill = "darkorchid4") +
  geom_line(size = 0.6, linetype = 1, colour = "grey13") +
  geom_line(aes(x = n_dams_mod,y = pLL),size = 0.4, linetype = 2, colour = "grey37") +
  geom_line(aes(x = n_dams_mod,y = pUL),size = 0.4, linetype = 2, colour = "grey37") +
  scale_y_continuous(breaks = (seq(0, 1, by = 0.2)),limits = c(0,1))+
  scale_x_continuous(breaks = round(seq(0, 6, by = 1)),limits = c(0,5.5))+
  labs(x = "Modelled beaver dam capacity (BDC) per reach", y = "Observed number of Dams per reach") +
  theme(panel.border = element_rect(linetype = 1, fill = NA), plot.margin = margin (10,20,10,10),
        panel.background = element_rect(fill = "white", colour = "grey90", size = 0.2),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        panel.grid.minor = element_line(colour = NA),
        axis.title.x = element_text(size=9, face = "bold", family = "sans"),
        axis.title.y = element_text(size=9, face = "bold", family = "sans"),
        axis.text=element_text(size=8, family = "sans"),
        legend.position = "none")
PLOTGb


# --- Model Results ----

Act_reaches$pred_ndams <- predict(m1, type = "response")

Predict2$n_dams_modR <- round(Predict2$n_dams_mod, digits = 3)
Act_reaches$n_dams_modR <- round(Act_reaches$n_dams_mod, digits = 3)


Act_reaches$pred_ndams_BS <- Predict2$Est.1 [match(Act_reaches$n_dams_modR, Predict2$n_dams_modR)]
Act_reaches$pLL_BS <- Predict2$pLL [match(Act_reaches$n_dams_modR, Predict2$n_dams_modR)]
Act_reaches$pUL_BS <- Predict2$pUL [match(Act_reaches$n_dams_modR, Predict2$n_dams_modR)]


Act_reaches$pred_dens <- Act_reaches$pred_ndams/Act_reaches$iGeo_Len*1000
Act_reaches$pred_dens_LL <- Act_reaches$pLL_BS/Act_reaches$iGeo_Len*1000
Act_reaches$pred_dens_UL <- Act_reaches$pUL_BS/Act_reaches$iGeo_Len*1000

sum(Act_reaches$n_dams)
sum(Act_reaches$pLL_BS[!is.na(Act_reaches$pLL_BS)])
sum(Act_reaches$pred_ndams[!is.na(Act_reaches$pred_ndams)])
sum(Act_reaches$pUL_BS[!is.na(Act_reaches$pUL_BS)])

#-------- prediction testing --------------
keeps <- as.vector(c("n_dams","n_dams_mod"))
Tidy_act <- Act_reaches[,keeps]

# Alternative option...
library(foreach)
library(doParallel)
cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)


# Pred_compare_b <- list()
# n=100
# ind <- sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(0.75, 0.25))
# loop_n <- (1:100)

# attempt parallel loop


Pred_comp_fin <- foreach(i=1:1000, .combine=rbind, .packages = c("pscl", "doParallel")) %dopar% {
  print("loop number:")
  print(i)
  Pred_compare_a <- list()
  
  ind <- sample(c(TRUE, FALSE), 100, replace=TRUE, prob=c(0.7, 0.3))
  Train <- Tidy_act[ind, ]
  Test_a <- Tidy_act[!ind, ]
  mod_test <- zeroinfl(n_dams ~ n_dams_mod, data = Train, dist = "negbin")
  # mod_test <- hurdle(n_dams ~ n_dams_mod, data = Train,dist = "negbin", zero.dist = "binomial") 
  
  for (j in (1:100)) {
    
    nr <- nrow(Test_a)
    rr <- round(nr/100*j)

    Test <- Test_a[sample(nrow(Test_a), rr), ]
    
    Test$pred_ndams_test <- predict(mod_test, newdata = Test, type = "response")
    
    pred_sum <- sum(Test$pred_ndams_test)
    obs_sum <- sum(Test$n_dams)
    i_n <- i
    
    df <- data.frame(i_n, pred_sum, obs_sum)
    
    Pred_compare_a[[j]] <- df
    
    
  }
  
  Pred_compare_b <- do.call(rbind, Pred_compare_a)
  
  return(Pred_compare_b)
  
} 

stopCluster(cl)

Pred_lm <- lm(obs_sum~pred_sum, data = Pred_comp_fin)
summary(Pred_lm )
# plot(Pred_lm )

newdata <- data.frame(seq(from = 0, to = 60, length.out = 101))
colnames(newdata) <- "pred_sum"
newdata2 <- data.frame(predict(Pred_lm, newdata, interval="predict"))
newdata2 <- cbind(newdata,newdata2)

predict(Pred_lm, newdata=list(pred_sum=25), interval="predict")

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}
# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}
Pred_comp_fin$error <- Pred_comp_fin$obs_sum - Pred_comp_fin$pred_sum

# Example of invocation of functions
RMSE <- round(rmse(Pred_comp_fin$error), digits = 1)
MAE <- round(mae(Pred_comp_fin$error), digits = 1) # using the raw data

rmse(Pred_lm$residuals) # using the lm
mae(Pred_lm$residuals)

mean(Pred_comp_fin$pred_sum)
sd(Pred_comp_fin$pred_sum)
mean(Pred_comp_fin$obs_sum)
sd(Pred_comp_fin$obs_sum)

ks.test(Pred_comp_fin$pred_sum,Pred_comp_fin$obs_sum)
###
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}
Pred_comp_fin$density <- get_density(Pred_comp_fin$pred_sum, Pred_comp_fin$obs_sum, n = 100)



val.lm <- lm(pred_sum ~ 0 + obs_sum, data = Pred_comp_fin) # linear regression through origin to aid interpretation of MCCV.

Plot_A <- ggplot(data = Pred_comp_fin, aes(x = pred_sum, y = obs_sum)) + 
  geom_point(aes(colour = density), alpha = 0.04, size = 2.1) +
  scale_colour_viridis(option="A") +
  guides(colour=FALSE) +
  # geom_line(data=newdata2,aes(x = pred_sum, y = lwr), linetype = 2, col = "grey40", size = 0.5)+
  # geom_line(data=newdata2,aes(x = pred_sum, y = upr), linetype = 2, col = "grey40", size = 0.5)+
  # geom_quantile( linetype = 4, quantiles = 0.95,size = 0.5)+
  geom_abline(intercept=0, slope=val.lm$coefficients[1], color="grey50", size=0.5,  linetype = 4) +
  # geom_quantile( linetype = 2, quantiles = 0.5, col = "grey60", size = 0.5) +
  # geom_quantile( linetype = 4, quantiles = 0.05,size = 0.5)+
  # geom_smooth(method = 'lm', col = "grey1", linetype = 1, se = FALSE, size = 0.5)+                   ####NOTE: Decide on best line type
  geom_segment(aes(x = 0, xend = 60, y = 0 , yend = 60), linetype = 1, size = 0.4, col = "grey30")+
  annotate("text", x = 5, y = 37, label = sprintf("RMSE = %s", RMSE ),size = 2.1) +
  annotate("text", x = 5, y = 34, label = sprintf("MAE = %s", MAE),size = 2.1) +
  scale_y_continuous(breaks = (seq(0, 60, by = 10)))+
  scale_x_continuous(breaks = (seq(0, 60, by = 10)))+
  coord_cartesian(ylim=c(0, 60), xlim=c(0, 60)) +
  labs(x = "Predicted Number of Dams", y = "Observed Number of Dams") +
  theme(panel.border = element_rect(linetype = 1, fill = NA), plot.margin = margin (10,20,10,10),
        panel.background = element_rect(fill = "white", colour = "grey90", size = 0.2),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        panel.grid.minor = element_line(colour = NA),
        axis.title.x = element_text(size=9, face = "bold", family = "sans"),
        axis.title.y = element_text(size=9, face = "bold", family = "sans"),
        axis.text=element_text(size=8, family = "sans"),
        legend.position = "none")
Plot_A 





# NICE <- ggplot(Pred_comp_fin, aes(x = pred_sum, y = obs_sum)) + 
#   # geom_point(alpha = 0.3, size = 1.8)+
#   # stat_binhex()+
#   stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = FALSE) +       
#   scale_fill_viridis(option = "inferno") +
#   coord_cartesian(expand = FALSE) +
#   # geom_point(size = 0.6, col = 'gold1', alpha = 0.01)+
#   # scale_y_continuous(limits = (c(0, 40)))+
#   # scale_x_continuous(limits = (c(0, 40)))+
#   theme(legend.position = "none",
#         axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.line.x=element_blank(),
#         axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         axis.line.y=element_blank())
# 
# NICE # just becasue it looks pretty...

resa <- data.frame(Pred_comp_fin[,2])
colnames(resa) <- "n_dams"
resa$type <- "Predicted"
resb <- data.frame(Pred_comp_fin[,3])
colnames(resb) <- "n_dams"
resb$type <- "Observed"
Pred_Resh <- rbind(resa,resb)

Plot_B <- ggplot(Pred_Resh , aes(x = type, y = n_dams, fill = type)) + 
  geom_boxplot(alpha = 0.3)+
  theme(panel.border = element_rect(linetype = 1, fill = NA), plot.margin = margin (10,20,10,10),
        panel.background = element_rect(fill = "white", colour = "grey90", size = 0.2),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        panel.grid.minor = element_line(colour = NA),
        axis.title.x = element_text(size=9, face = "bold", family = "sans"),
        axis.title.y = element_text(size=9, face = "bold", family = "sans"),
        axis.text=element_text(size=8, family = "sans"),
        legend.position = "none")
  # theme(panel.border = element_rect(linetype = 1, fill = NA), plot.margin = margin (10,20,10,10),
  #       panel.background = element_rect(fill = "white", colour = "black"),
  #       panel.grid.major = element_line(colour = "grey90"),
  #       panel.grid.minor = element_line(colour = NA),
  #       axis.title.x = element_text(size=9, face = "bold"),
  #       axis.title.y = element_text(size=9, face = "bold"),
  #       axis.text=element_text(size=7),
  #       legend.position = "none")

Plot_B

## Create combined plot ###

comb_NEGBIN<- ggdraw() +
  draw_plot(PLOTGb, x = 0., y = 0, width = .5, height = 1) +
  draw_plot(Plot_A, x = .5, y = 0, width = .5, height = 1) +
  draw_plot_label(label = c("A", "B"), size = 9,
                  x = c(0, 0.5), y = c(1, 1), family="sans")
# comb_NEGBIN

tiff("Exports/NEGBIN_comb_plotNoBamff.tiff", width = 174, height = 100, units="mm",res=600, compression = "lzw")
comb_NEGBIN# Make plot
dev.off()




#------- Apply Model to Full datasets ----------------------- #

# All_reaches<- read.csv("BDC_Results_all_sites.csv")



# m1 <- zeroinfl(n_dams ~ n_dams_mod, data = Act_reaches,dist = "negbin")

All_reaches$max_mod_ndams <- (All_reaches$BDC/1000)*All_reaches$iGeo_Len

NewData <- data.frame(n_dams_mod = (All_reaches$BDC/1000)*All_reaches$iGeo_Len)

All_reaches$n_dams_pred <-  predict(m1, newdata = NewData, type = "response")

All_reaches$n_dams_modR <- round(All_reaches$max_mod_ndams, digits = 3)
# All_reaches$pred_ndams_BS <- Predict2$Est.1 [match(All_reaches$n_dams_modR, Predict2$n_dams_modR)]
All_reaches$pLL_BS <- Predict2$pLL [match(All_reaches$n_dams_modR, Predict2$n_dams_modR)]
All_reaches$pUL_BS <- Predict2$pUL [match(All_reaches$n_dams_modR, Predict2$n_dams_modR)]


sum(All_reaches$max_mod_ndams[All_reaches$Catchment == "Otter"]) #BDC model max estimate
sum(All_reaches$n_dams_pred[All_reaches$Catchment == "Otter"]) # ZINB calculation
sum(All_reaches$pLL_BS[All_reaches$Catchment == "Otter"], na.rm = TRUE) # lower CI
sum(All_reaches$pUL_BS[All_reaches$Catchment == "Otter"], na.rm = TRUE) # Upper CI

sum(All_reaches$max_mod_ndams[All_reaches$Catchment == "Tay"]) #BDC model max estimate
sum(All_reaches$n_dams_pred[All_reaches$Catchment == "Tay"]) # ZINB calculation
sum(All_reaches$pLL_BS[All_reaches$Catchment == "Tay"], na.rm = TRUE) # lower CI
sum(All_reaches$pUL_BS[All_reaches$Catchment == "Tay"], na.rm = TRUE) # Upper CI

sum(All_reaches$max_mod_ndams[All_reaches$Catchment == "ComHead"]) #BDC model max estimate
sum(All_reaches$n_dams_pred[All_reaches$Catchment == "ComHead"]) # ZINB calculation
sum(All_reaches$pLL_BS[All_reaches$Catchment == "ComHead"], na.rm = TRUE) # lower CI
sum(All_reaches$pUL_BS[All_reaches$Catchment == "ComHead"], na.rm = TRUE)


storeVals <- list()

# catchments <- c("Tay", "Otter", "ComHead")

for (i in catchments){
  print(i)
  df <- setNames(data.frame(matrix(ncol = 10, nrow = 6)), c("AOI", "Cap_Cat", "Channl_Leng", "percStrNet", "Act_Channel_Leng","Act_percStrNet",
                                                           "Obs_nDams", "Perc_ObsDams", "Pred_nDams", "per_nDams" ))
  # df <- data.frame(matrix(NA, nrow = 6, ncol = 9))
  # colnames(df)[colnames(df)==c("X1", "X2", )] <- c("AOI", "Cap_Cat", "Channl_Leng", "percStrNet", "Pred_nDams", "per_nDams", "Act_Pred_nDams", "", "")
  df$AOI <- i
  
  df$Cap_Cat <- c("None", "Rare", "Occasional","Frequent", "Pervasive", "All")
  
  none.len <- (sum(All_reaches$iGeo_Len[All_reaches$Catchment == i & All_reaches$Categ == "None"], na.rm = TRUE))/1000
  rare.len <- (sum(All_reaches$iGeo_Len[All_reaches$Catchment == i & All_reaches$Categ == "Rare"], na.rm = TRUE))/1000
  Occ.len <- (sum(All_reaches$iGeo_Len[All_reaches$Catchment == i & All_reaches$Categ == "Occasional"], na.rm = TRUE))/1000
  Freq.len <- (sum(All_reaches$iGeo_Len[All_reaches$Catchment == i & All_reaches$Categ == "Frequent"], na.rm = TRUE))/1000
  Perv.len <- (sum(All_reaches$iGeo_Len[All_reaches$Catchment == i & All_reaches$Categ == "Pervasive"], na.rm = TRUE))/1000
  All.len <- (sum(All_reaches$iGeo_Len[All_reaches$Catchment == i]))/1000
  df$Channl_Leng <- c(none.len, rare.len, Occ.len, Freq.len, Perv.len, All.len)
  
  
  none.percStr <- none.len/All.len*100
  rare.percStr <- rare.len/All.len*100
  Occ.percStr <- Occ.len/All.len*100
  Freq.percStr <- Freq.len/All.len*100
  Perv.percStr <- Perv.len/All.len*100
  All.percStr <- All.len/All.len*100
  df$percStrNet <- c(none.percStr, rare.percStr, Occ.percStr, Freq.percStr, Perv.percStr, All.percStr)
  
  
  Anone.len <- (sum(All_reaches$iGeo_Len[All_reaches$Catchment == i & All_reaches$Categ == "None" & All_reaches$Active==1], na.rm = TRUE))/1000
  Arare.len <- (sum(All_reaches$iGeo_Len[All_reaches$Catchment == i & All_reaches$Categ == "Rare" & All_reaches$Active==1], na.rm = TRUE))/1000
  AOcc.len <- (sum(All_reaches$iGeo_Len[All_reaches$Catchment == i & All_reaches$Categ == "Occasional" & All_reaches$Active==1], na.rm = TRUE))/1000
  AFreq.len <- (sum(All_reaches$iGeo_Len[All_reaches$Catchment == i & All_reaches$Categ == "Frequent" & All_reaches$Active==1], na.rm = TRUE))/1000
  APerv.len <- (sum(All_reaches$iGeo_Len[All_reaches$Catchment == i & All_reaches$Categ == "Pervasive" & All_reaches$Active==1], na.rm = TRUE))/1000
  AAll.len <- (sum(All_reaches$iGeo_Len[All_reaches$Catchment == i & All_reaches$Active==1]))/1000
  df$Act_Channel_Leng <- c(Anone.len, Arare.len, AOcc.len, AFreq.len, APerv.len, AAll.len)
  
  Anone.percStr <- Anone.len/AAll.len*100
  Arare.percStr <- Arare.len/AAll.len*100
  AOcc.percStr <- AOcc.len/AAll.len*100
  AFreq.percStr <- AFreq.len/AAll.len*100
  APerv.percStr <- APerv.len/AAll.len*100
  AAll.percStr <- AAll.len/AAll.len*100
  df$Act_percStrNet <- c(Anone.percStr, Arare.percStr, AOcc.percStr, AFreq.percStr, APerv.percStr, AAll.percStr)
  

  none.predDams <- sprintf("%s [%s, %s]", round(sum(All_reaches$n_dams_pred[All_reaches$Catchment == i & All_reaches$Categ == "None"], na.rm = TRUE),2),
                           round(sum(All_reaches$pLL_BS[All_reaches$Catchment == i & All_reaches$Categ == "None"], na.rm = TRUE),2),
                           round(sum(All_reaches$pUL_BS[All_reaches$Catchment == i & All_reaches$Categ == "None"], na.rm = TRUE),2))
  rare.predDams <- sprintf("%s [%s, %s]", round(sum(All_reaches$n_dams_pred[All_reaches$Catchment == i & All_reaches$Categ == "Rare"], na.rm = TRUE),2),
                           round(sum(All_reaches$pLL_BS[All_reaches$Catchment == i & All_reaches$Categ == "Rare"], na.rm = TRUE),2),
                           round(sum(All_reaches$pUL_BS[All_reaches$Catchment == i & All_reaches$Categ == "Rare"], na.rm = TRUE),2))
  Occ.predDams <- sprintf("%s [%s, %s]", round(sum(All_reaches$n_dams_pred[All_reaches$Catchment == i & All_reaches$Categ == "Occasional"], na.rm = TRUE),2),
                          round(sum(All_reaches$pLL_BS[All_reaches$Catchment == i & All_reaches$Categ == "Occasional"], na.rm = TRUE),2),
                          round(sum(All_reaches$pUL_BS[All_reaches$Catchment == i & All_reaches$Categ == "Occasional"], na.rm = TRUE),2))
  Freq.predDams <- sprintf("%s [%s, %s]", round(sum(All_reaches$n_dams_pred[All_reaches$Catchment == i & All_reaches$Categ == "Frequent"], na.rm = TRUE),2),
                           round(sum(All_reaches$pLL_BS[All_reaches$Catchment == i & All_reaches$Categ == "Frequent"], na.rm = TRUE),2),
                           round(sum(All_reaches$pUL_BS[All_reaches$Catchment == i & All_reaches$Categ == "Frequent"], na.rm = TRUE),2))
  Perv.predDams <- sprintf("%s [%s, %s]", round(sum(All_reaches$n_dams_pred[All_reaches$Catchment == i & All_reaches$Categ == "Pervasive"], na.rm = TRUE),2),
                           round(sum(All_reaches$pLL_BS[All_reaches$Catchment == i & All_reaches$Categ == "Pervasive"], na.rm = TRUE),2),
                           round(sum(All_reaches$pUL_BS[All_reaches$Catchment == i & All_reaches$Categ == "Pervasive"], na.rm = TRUE),2))
  All.predDams <- sprintf("%s [%s, %s]", round(sum(All_reaches$n_dams_pred[All_reaches$Catchment == i], na.rm = TRUE),2),
                          round(sum(All_reaches$pLL_BS[All_reaches$Catchment == i], na.rm = TRUE),2),
                          round(sum(All_reaches$pUL_BS[All_reaches$Catchment == i], na.rm = TRUE),2))
  df$Pred_nDams <- c(none.predDams, rare.predDams, Occ.predDams, Freq.predDams, Perv.predDams, All.predDams)
  
  tot_ndams <- sum(All_reaches$n_dams_pred[All_reaches$Catchment == i], na.rm = TRUE)
  none.per_nDams <- sum(All_reaches$n_dams_pred[All_reaches$Catchment == i & All_reaches$Categ == "None"], na.rm = TRUE)/tot_ndams*100
  rare.per_nDams <- sum(All_reaches$n_dams_pred[All_reaches$Catchment == i & All_reaches$Categ == "Rare"], na.rm = TRUE)/tot_ndams*100
  Occ.per_nDams <- sum(All_reaches$n_dams_pred[All_reaches$Catchment == i & All_reaches$Categ == "Occasional"], na.rm = TRUE)/tot_ndams*100
  Freq.per_nDams <- sum(All_reaches$n_dams_pred[All_reaches$Catchment == i & All_reaches$Categ == "Frequent"], na.rm = TRUE)/tot_ndams*100
  Perv.per_nDams <- sum(All_reaches$n_dams_pred[All_reaches$Catchment == i & All_reaches$Categ == "Pervasive"], na.rm = TRUE)/tot_ndams*100
  All.per_nDams <- sum(All_reaches$n_dams_pred[All_reaches$Catchment == i], na.rm = TRUE)/tot_ndams*100
  df$per_nDams <- c(none.per_nDams, rare.per_nDams, Occ.per_nDams, Freq.per_nDams, Perv.per_nDams, All.per_nDams)
  

  # none.ActpredDams <- sprintf("%s [%s, %s]", round(sum(All_reaches$n_dams_pred[All_reaches$Catchment == i & All_reaches$Categ == "None" & All_reaches$Active==1], na.rm = TRUE),2),
  #                          round(sum(All_reaches$pLL_BS[All_reaches$Catchment == i & All_reaches$Categ == "None"& All_reaches$Active==1], na.rm = TRUE),2),
  #                          round(sum(All_reaches$pUL_BS[All_reaches$Catchment == i & All_reaches$Categ == "None"& All_reaches$Active==1], na.rm = TRUE),2))
  # rare.ActpredDams <- sprintf("%s [%s, %s]", round(sum(All_reaches$n_dams_pred[All_reaches$Catchment == i & All_reaches$Categ == "Rare"& All_reaches$Active==1], na.rm = TRUE),2),
  #                          round(sum(All_reaches$pLL_BS[All_reaches$Catchment == i & All_reaches$Categ == "Rare"& All_reaches$Active==1], na.rm = TRUE),2),
  #                          round(sum(All_reaches$pUL_BS[All_reaches$Catchment == i & All_reaches$Categ == "Rare"& All_reaches$Active==1], na.rm = TRUE),2))
  # Occ.ActpredDams <- sprintf("%s [%s, %s]", round(sum(All_reaches$n_dams_pred[All_reaches$Catchment == i & All_reaches$Categ == "Occasional"& All_reaches$Active==1], na.rm = TRUE),2),
  #                         round(sum(All_reaches$pLL_BS[All_reaches$Catchment == i & All_reaches$Categ == "Occasional"& All_reaches$Active==1], na.rm = TRUE),2),
  #                         round(sum(All_reaches$pUL_BS[All_reaches$Catchment == i & All_reaches$Categ == "Occasional"& All_reaches$Active==1], na.rm = TRUE),2))
  # Freq.ActpredDams <- sprintf("%s [%s, %s]", round(sum(All_reaches$n_dams_pred[All_reaches$Catchment == i & All_reaches$Categ == "Frequent"& All_reaches$Active==1], na.rm = TRUE),2),
  #                          round(sum(All_reaches$pLL_BS[All_reaches$Catchment == i & All_reaches$Categ == "Frequent"& All_reaches$Active==1], na.rm = TRUE),2),
  #                          round(sum(All_reaches$pUL_BS[All_reaches$Catchment == i & All_reaches$Categ == "Frequent"& All_reaches$Active==1], na.rm = TRUE),2))
  # Perv.ActpredDams <- sprintf("%s [%s, %s]", round(sum(All_reaches$n_dams_pred[All_reaches$Catchment == i & All_reaches$Categ == "Pervasive"& All_reaches$Active==1], na.rm = TRUE),2),
  #                          round(sum(All_reaches$pLL_BS[All_reaches$Catchment == i & All_reaches$Categ == "Pervasive"& All_reaches$Active==1], na.rm = TRUE),2),
  #                          round(sum(All_reaches$pUL_BS[All_reaches$Catchment == i & All_reaches$Categ == "Pervasive"& All_reaches$Active==1], na.rm = TRUE),2))
  # All.ActpredDams <- sprintf("%s [%s, %s]", round(sum(All_reaches$n_dams_pred[All_reaches$Catchment == i& All_reaches$Active==1], na.rm = TRUE),2),
  #                         round(sum(All_reaches$pLL_BS[All_reaches$Catchment == i& All_reaches$Active==1], na.rm = TRUE),2),
  #                         round(sum(All_reaches$pUL_BS[All_reaches$Catchment == i& All_reaches$Active==1], na.rm = TRUE),2))
  # df$Act_Pred_nDams <- c(none.ActpredDams, rare.ActpredDams, Occ.ActpredDams, Freq.ActpredDams, Perv.ActpredDams, All.ActpredDams)
  
  
  
  none.obs_nDams <- sum(All_reaches$n_dams[All_reaches$Catchment == i & All_reaches$Categ == "None"], na.rm = TRUE)
  rare.obs_nDams <- sum(All_reaches$n_dams[All_reaches$Catchment == i & All_reaches$Categ == "Rare"], na.rm = TRUE)
  Occ.obs_nDams <- sum(All_reaches$n_dams[All_reaches$Catchment == i & All_reaches$Categ == "Occasional"], na.rm = TRUE)
  Freq.obs_nDams <- sum(All_reaches$n_dams[All_reaches$Catchment == i & All_reaches$Categ == "Frequent"], na.rm = TRUE)
  Perv.obs_nDams <- sum(All_reaches$n_dams[All_reaches$Catchment == i & All_reaches$Categ == "Pervasive"], na.rm = TRUE)
  All.obs_nDams <- sum(All_reaches$n_dams[All_reaches$Catchment == i], na.rm = TRUE)
  df$Obs_nDams <- c(none.obs_nDams, rare.obs_nDams, Occ.obs_nDams, Freq.obs_nDams, Perv.obs_nDams, All.obs_nDams)
  
  obs_ndams <- sum(All_reaches$n_dams[All_reaches$Catchment == i], na.rm = TRUE)
  none.obsper_nDams <- none.obs_nDams/obs_ndams*100
  rare.obsper_nDams <- rare.obs_nDams/obs_ndams*100
  Occ.obsper_nDams <- Occ.obs_nDams/obs_ndams*100
  Freq.obsper_nDams <- Freq.obs_nDams/obs_ndams*100
  Perv.obsper_nDams <- Perv.obs_nDams/obs_ndams*100
  All.obsper_nDams <- All.obs_nDams/obs_ndams*100
  df$Perc_ObsDams <- c(none.obsper_nDams, rare.obsper_nDams, Occ.obsper_nDams, Freq.obsper_nDams, Perv.obsper_nDams, All.obsper_nDams)
  
  
  
  storeVals[[i]] <- df
}

out_df <- dplyr::bind_rows(storeVals)
out_df <-out_df %>% mutate_if(is.numeric, round, 2)

head(out_df)


tab_df(out_df,
       file="Exports/BDC_Summary_Tab.doc",col.header = FALSE, alternate.rows = TRUE )




# Produce Markdown table of ZINB regression summary

# stargazer(m1, type = 'text')

# mean(Act_reaches$Str_order[Act_reaches$Catchment == "Tay"])
# mean(Act_reaches$Str_order[Act_reaches$Catchment == "Otter"])
# sd(Act_reaches$Str_order[Act_reaches$Catchment == "Tay"])
# sd(Act_reaches$Str_order[Act_reaches$Catchment == "Otter"])

# # ---- retrieve active reach results -----------
# 
# Act_reaches$pred_ndams_test <- predict(m1, newdata = Test, type = "response")
# 
# sum(Act_reaches$n_dams, na.rm = TRUE)
# sum(Act_reaches$pred_ndams, na.rm = TRUE)
# sum(Act_reaches$pLL_BS, na.rm = TRUE)
# sum(Act_reaches$pUL_BS, na.rm = TRUE)
# 
# 
# sum(Act_reaches$n_dams[Act_reaches$Catchment == "Tay"])
# sum(Act_reaches$pLL_BS[Act_reaches$Catchment == "Tay"])
# sum(Act_reaches$pred_ndams[Act_reaches$Catchment == "Tay"])
# sum(Act_reaches$pUL_BS[Act_reaches$Catchment == "Tay"])
# 
# sum(Act_reaches$n_dams[Act_reaches$Catchment == "Otter"])
# sum(Act_reaches$pLL_BS[Act_reaches$Catchment == "Otter"])
# sum(Act_reaches$pred_ndams[Act_reaches$Catchment == "Otter"])
# sum(Act_reaches$pUL_BS[Act_reaches$Catchment == "Otter"])
# 
# sum(Act_reaches$n_dams[Act_reaches$Categ == "Pervasive"])
# sum(Act_reaches$pLL_BS[Act_reaches$Categ == "Pervasive"])
# sum(Act_reaches$pred_dens[Act_reaches$Categ == "Pervasive"])
# sum(Act_reaches$pUL_BS[Act_reaches$Categ == "Pervasive"])
# 
# 
# sum(Act_reaches$n_dams[Act_reaches$Categ == "Frequent"])
# sum(Act_reaches$pLL_BS[Act_reaches$Categ == "Frequent"])
# sum(Act_reaches$pred_ndams[Act_reaches$Categ == "Frequent"])
# sum(Act_reaches$pUL_BS[Act_reaches$Categ == "Frequent"])
# 
# 
# sum(Act_reaches$n_dams[Act_reaches$Categ == "Occasional"])
# sum(Act_reaches$pLL_BS[Act_reaches$Categ == "Occasional"])
# sum(Act_reaches$pred_ndams[Act_reaches$Categ == "Occasional"])
# sum(Act_reaches$pUL_BS[Act_reaches$Categ == "Occasional"])
# 
# sum(Act_reaches$n_dams[Act_reaches$Categ == "Rare"])
# sum(Act_reaches$pLL_BS[Act_reaches$Categ == "Rare"])
# sum(Act_reaches$pred_ndams[Act_reaches$Categ == "Rare"])
# sum(Act_reaches$pUL_BS[Act_reaches$Categ == "Rare"])
# 
# sum(Act_reaches$n_dams[Act_reaches$Categ == "None"])
# sum(Act_reaches$pLL_BS[Act_reaches$Categ == "None"])
# sum(Act_reaches$pred_ndams[Act_reaches$Categ == "None"])
# sum(Act_reaches$pUL_BS[Act_reaches$Categ == "None"])
# 
# sum(Act_reaches$iGeo_Len[Act_reaches$Categ == "None"])/1000
# sum(Act_reaches$iGeo_Len[Act_reaches$Categ == "Rare"])/1000
# 
# mean(Act_reaches$obs_dens)
# mean(Act_reaches$pred_dens)
# mean(Act_reaches$pred_dens_LL)
# mean(Act_reaches$pred_dens_UL)