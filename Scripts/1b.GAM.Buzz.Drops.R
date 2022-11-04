################################################################
########### MODELLING Buzz data to predict drops    ############
#### on the 3 whales equiped with both Acousonde and STP tag ###
####                   Frida, Thora, Eistla              #######
####        Chambault et al , Bio Letters, under review  #######
####        by Philippine CHAMBAULT, updated: 3 Nov 2022 #######
################################################################

library(itsadug)
library(usdm)
library(mgcv)
library(lme4)
library(performance)
library(nlme)
library(mgcv)
library(sjPlot)
library(visreg)
library(reshape)
library(plyr)
library(ggplot2)
library(data.table)
library(tidyquant)
detach("package:plyr", unload = TRUE)



####################################
# load summary dives: 1 row=1 dive
####################################
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/STP_narwhals")
acou <- readRDS(paste0("./Rdata/Acousonde/Buzz_Drops/calib_2m_zoc0/",
                        "Summary_dive_drops_buzzes_3ids_calib_2m_zoc0.RDS")) %>%
  as_tibble()

summary(acou$dur)
acou

# explo graphics
#-------------------
boxplot(nbuzz~ndrop, acou)
nrow(acou[acou$ndrop!=0,])   
table(acou$ndrop, acou$ptt)

acou %>%
  group_by(ptt) %>%
  summarise(n_buzzes  = sum(nbuzz),
            max_buzz  = max(nbuzz),
            min_buzz  = min(nbuzz),
            mean_buzz = mean(nbuzz),
            max_buzz_drop  = max(nbuzz[ndrop!=0]),
            min_buzz_drop  = min(nbuzz[ndrop!=0]),
            mean_buzz_drop = mean(nbuzz[ndrop!=0]))


# nbuzz vs maxdep
#------------------
ggplot(acou, aes(y=nbuzz, x=maxdep)) +
  geom_point(aes(colour="ndrop")) +
  geom_smooth() +
  coord_flip() +
  scale_x_continuous(trans = "reverse") +
  facet_grid(.~ptt, scales="free") +
  theme_tq()

# dive duration vs nbuzzes
#---------------------------
ggplot(acou, aes(y=dur, x=maxdep)) +
  geom_point(aes(colour="ndrop")) +
  geom_smooth() +
  coord_flip() +
  scale_x_continuous(trans = "reverse") +
  facet_grid(.~ptt, scales="free")  +
  theme_tq()


# prepare dataset for GAM: remove outliers
# that generate large CI for extreme nbuzz values
#---------------------------------------------------
dat = acou %>%
  mutate(ptt = as.factor(ptt)) %>%
  filter(nbuzz<=40)
dim(dat)  
table(dat$ptt)
unique(dat$ndrop)  # binary variable so let's use a binomial distribution





####################################
# GAM
####################################

# nbuzz (random slope+intercept)
#---------------------------------
m  = gam(ndrop ~  s(nbuzz) + 
           s(ptt, bs = 're') +        # random intercept
           s(nbuzz, ptt, bs = 're'),  # random slope
         method = "REML",
         data   = dat, 
         family = binomial(link="logit"))
summary(m) # 18%, nbuzz***
plot(m,pages=1,shade=T)
saveRDS(m, "./Rdata/GAM_ndrop/GAM_nbuzz_slope_intercept.rds")

# check residuals
#-------------------
par(mfrow=c(2,2),mar=c(6,5,6,3))
qq.gam(m,cex=1,pch=20)
hist(residuals(m), xlab="Residuals", main="Histogram of residuals")
observed.y <- napredict(m$na.action, m$y)
plot(fitted(m), observed.y, pch=20, xlab = "Fitted Values",
     ylab = "Response", main = "Response vs. Fitted Values")
acf(resid(m),lag.max=200, main="Autocorrelation")





####################################
# predictions at individual level
####################################
# new dataset generation for individual level (column time, id)
ind_pred_inter <- expand.grid(nbuzz = 0:max(dat$nbuzz),
                              ptt   = unique(dat$ptt))
head(ind_pred_inter)

# add group_to_compare column
ind_pred <- ind_pred_inter %>%
  # set as data.table
  setDT(.) %>% 
  # sort by group_to_compare, time and id
  .[order(nbuzz, ptt),] %>%
  # add individual prediction
  .[, fit_ind := predict.gam(m,
                             .SD,
                             type = "response")] %>%
  # add individual SE
  .[, se_ind := predict.gam(m,
                            .SD,
                            se.fit=TRUE,
                            type = "response")$se.fit] %>%
  # sort by group_to_compare, time and id
  .[order(nbuzz, ptt),] %>%
  
  # trick to avoid calling twice this object in the console for display
  .[]
ind_pred
saveRDS(ind_pred, "./Rdata/GAM_ndrop/ind_pred_GAM_nbuzz_slope_intercept.rds")





############################
# plot ind fit+SE
############################
ggplot(ind_pred, aes(x = nbuzz, y = fit_ind)) +
  geom_line(aes(colour=ptt),lwd=1) +
  geom_ribbon(aes(ymin=fit_ind-se_ind, 
                  ymax=fit_ind+se_ind, fill=ptt), alpha=0.2) +
  # geom_line(data=pop_pred, aes(nbuzz, fit_ind), colour="black",lwd=1) +
  scale_colour_manual(values = c("blue","brown3","darkgreen")) +
  scale_fill_manual(values = c("blue","brown3","darkgreen")) +
  labs(y = "Drop probability (log)", x = "Number of buzzes", title="a)") +
  theme(legend.position = "right",#c(0.5,0.15),
        legend.key.size = unit(6,"cm"),
        legend.key.width = unit(0.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.title = element_blank(),
        legend.text = element_text(size=7),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.key = element_blank(),
        legend.background=element_rect(fill = NA),
        axis.title = element_text(size=6, hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        axis.text.y  = element_text(size=5, hjust=1),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=8, vjust=0, hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank()) +
  guides(color = guide_legend(override.aes = list(lwd = 1) ) )

