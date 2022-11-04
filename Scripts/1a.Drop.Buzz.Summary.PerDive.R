#########################################################################
####           summarize drop, buzzes, depth per dive             #######
#### unsuccessful (buzz, no drop) vs successful dives (buzz+drop) #######
####            Chambault et al , Bio Letters, under review
####            by Philippine CHAMBAULT, updated: 3 Nov 2022
#########################################################################

library(ggplot2)
library(tidyquant)
library(gridExtra)
library(tidyverse)
library(dplyr)
library(DT)
library(data.table)
library(mgcv)
library(diveMove)
library(stringr)
library(scales)
library(viridis)
library(RColorBrewer)




##################################################
# load datasets including depth, drops, buzzes
##################################################
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/STP_narwhals")

e <- read.csv("./DRYAD/Buzz_STP_drops_Eistla.txt") %>%
  as_tibble() %>%
  mutate(id = "Eistla",
         dateTime = as.POSIXct(strptime(dateTime,
                                        format="%Y-%m-%d %H:%M:%S"), tz="GMT")) %>%
  select("id","dateTime2","dateTime","depth","buzz","st","st_fit",
         "type","feed","event")
table(e$type) # 5 drops

t <- read.csv("./DRYAD/Buzz_STP_drops_Thora.txt") %>%
  as_tibble() %>%
  mutate(id = "Thora",
         dateTime = as.POSIXct(strptime(dateTime,
                                        format="%Y-%m-%d %H:%M:%S"), tz="GMT")) %>%
  select("id","dateTime2","dateTime","depth","buzz","st","st_fit",
         "type","feed","event")
t = t[!duplicated(t$dateTime),]
table(t$type)  # 21 drops

f <- read.csv("./DRYAD/Buzz_STP_drops_Frida.txt") %>%
  as_tibble() %>%
  mutate(id = "Frida",
         dateTime = as.POSIXct(strptime(dateTime,
                                        format="%Y-%m-%d %H:%M:%S"), tz="GMT")) %>%
  select("id","dateTime2","dateTime","depth","buzz","st","st_fit",
         "type","feed","event")
table(f$type) # 75 drops

data = rbind(f,t,e)
data = data[order(data$dateTime),]
table(data$id)
table(data$id, data$buzz)


#-------------------
# add column drop
#-------------------
table(data$type)
data = data %>%
  mutate(drop = case_when(type=="start" & !is.na(data$type) ~ 1,
                          TRUE ~ 0)) 
prop.table(table(data$drop))
table(data$drop, data$id)
range(data$event)

saveRDS(data,"./Rdata/Acousonde/Buzz_Drops/Buzz_drops_3ids_bef-threshold2m.rds")







###################################################################################################
# add dive number and dive phases (depth=0)
# (D) descent, (DB) descent/bottom, (B) bottom, (BA) bottom/ascent, (A) ascent, 
# (DA) descent/ascent (occurring when no bottom phase can be detected) and (X) non-dive (surface),
###################################################################################################
path = paste0("./Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/",
              "ANALYSES/STP_narwhals/Rdata/Acousonde/Buzz_Drops/buzz_drops_3ids_bef-threshold2m.rds")

offset    = 0  # zero offset corrected (already corrected from Blackwell so set to 0)
threshold = 2  # threshold depth below which an underwater phase should be considered a dive

system.time({  # 157 sec for offset(zoc)=0 and threshold=2
  for (i in 1:length(unique(data$id))) {
    id    = data[data$id==unique(data$id)[i],]
    tdr   = createTDR(id$dateTime, id$depth, file=path, speed=F)
    calib = calibrateDepth(tdr, dive.thr=threshold, 
                           zoc.method="offset",offset=offset) 
    
    saveRDS(calib, paste0("./ANALYSES/STP_narwhals/Rdata/Acousonde/Buzz_Drops/calib_",
                          threshold,"m_zoc",offset,"/",unique(id$id),
                          "_dcalib_",threshold,"m_zoc",offset,".RDS"))
  }
})






#########################################################################
# add dive number and phases to the buzz-drop dataset
#########################################################################
zoc   = paste0("calib_",threshold,"m_zoc",offset)
setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF",
             "/ANALYSES/STP_narwhals/Rdata/Acousonde/Buzz_Drops/",zoc))
files = list.files(pattern=paste0(zoc,".RDS"))
files

acou = NULL
system.time({  # 0.5 sec
  for (i in 1:length(files)) {
    dat = readRDS(files[[i]])
    d   = getDAct(dat)
    id  = str_extract(files[[i]], "[^_]+")
    
    dive2 = data[data$id==id,]
    dive2$dive = d[,1]
    dive2$activity = d[,2]
    dive2$phase = getDPhaseLab(dat)
    
    # save all files into 1 tibble
    #------------------------------
    acou = rbind(acou, dive2)
  }
})

table(acou$drop, acou$phase)
table(acou$buzz, acou$phase)
range(acou$dive)
nrow(acou[acou$dive==0,]) /nrow(acou) * 100 
summary(acou$depth)
summary(acou$depth[acou$dive!=0])

setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/STP_narwhals")
saveRDS(acou,"./Rdata/Acousonde/Buzz_Drops/Buzz_drops_3ids_threshold2m.rds")



# proportion of dive above depth threshold
#--------------------------------------------
acou$drop = as.numeric(as.character(acou$drop))
prop = acou %>% 
  group_by(id) %>%
  summarise(ndive      = max(dive),
            above      = length(dive[dive==0])/3600, # in hours
            below      = length(dive[dive>0])/3600,  # in hours
            trackdur   = n()/3600,                   # in hours
            nBuzz      = sum(buzz[buzz==1]),
            buzz_above = sum(buzz[buzz==1 & dive==0]),
            nDrop      = sum(drop[drop==1]),
            drop_above = sum(drop[drop==1 & dive==0])) %>%
  mutate(abovepc = (above / trackdur) * 100,
         belowpc = (below / trackdur) * 100) %>%
  select(id,buzz_above,drop_above,abovepc,belowpc)
data.frame(prop)


# remove no dive (surface periods)
#----------------------------------
acou = acou %>% 
  filter(dive > 0) %>%
  mutate(drop = as.numeric(as.character(drop)))
range(acou$dive)
summary(acou$depth)

acou %>%
  group_by(id) %>%
  summarise(n_dive = n_distinct(dive),
            n_buzz = sum(buzz),
            n_drop = sum(drop))





#################################################
# plot depth vs time with buzzes for each dive
#################################################
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/")
for (i in 1:3) {  # loop over each individual
  id = acou[acou$id==unique(acou$id)[i],]
  
  # select each individual
  for (d in unique(id$dive)) {
    dive = id[id$dive==d,]
    
    # plot
    ggplot(dive, aes(dateTime, -depth)) +
      geom_path(size=0.3, colour="darkgrey") +
      geom_point(size=0.5,data=dive[dive$buzz==1,],
                 aes(dateTime,-depth),colour="black") +
      geom_point(size=0.5,data=dive[dive$drop=="1",],
                 aes(dateTime,-depth),colour="red") +
      labs(x="time", y="Depth (m)",title=paste0(unique(id$id),", dive ", d),
           subtitle = paste0("duration:",signif(as.numeric(difftime(dive$dateTime[nrow(dive)],
                                                                    dive$dateTime[1], units="mins")),2),"min, ",
                             substr(dive$dateTime[1],1,10))) +
      geom_hline(yintercept=0,linetype=2, color="blue", size=0.3) +
      theme_tq()
    
    # save plot
    ggsave(paste0("./ANALYSES/STP_narwhals/Figures/Acousondes/Depth_Profile/",unique(id$id),
                  "/Calib_",threshold,"m/Dive_profile_Acousonde_calib",threshold,
                  "m_zoc",offset,"_",
                  unique(id$id),"_dive",d,".png"),
           width=5,height=4,units="in",dpi=400)
  }
}








#############################################
# generate dive summary (n buzz, ST drop...)
# 1 row=1 dive per individual
#############################################
acou2 = NULL
system.time({   # 3 sec
  for (e in unique(acou$id)) {    # loop over each whale
    id = acou[acou$id==e,]
    id = id[order(id$dateTime),]
    
    for (i in unique(id$dive) ) { # loop over each dive
      rm(dive)
      dive  = data.frame("ptt"=0,"dive"=0,"start"=0,"end"=0,"dur"=0,
                         "n"=NA,"nbuzz"=NA,"mindep"=NA,"maxdep"=NA,"meandep"=NA,
                         "mindep_buzz"=NA,"maxdep_buzz"=NA,"meandep_buzz"=NA,
                         "event"=NA,"st_drop"=NA,
                         "ndrop"=0,"dep_drop"=NA,"phase_st"=NA,"drop_start"=NA,
                         "dur_bef_drop"=NA,"dur_aft_drop"=NA,
                         "Nbuzz_bef"=NA, "Nbuzz_aft"=NA)
      st = data.frame(id[id$dive==i,])
      
      dive$ptt   = e
      dive$dive  = i
      dive$start = st$dateTime[1]
      dive$end   = st$dateTime[nrow(st)]
      dive$dur   = as.numeric(difftime(dive$end,
                                       dive$start, units="mins"))
      dive$n = nrow(st)
      dive$nbuzz   = nrow(st[st$buzz==1,])
      dive$mindep  = min(st$depth)
      dive$maxdep  = max(st$depth)
      dive$meandep = mean(st$depth)
      
      if(any(st$buzz==1)) {   # extract buzz info 
        dive$mindep_buzz  = min(st$depth[st$buzz==1])
        dive$maxdep_buzz  = max(st$depth[st$buzz==1])
        dive$meandep_buzz = mean(st$depth[st$buzz==1]) }
      
      if(any(st$drop=="1")) { # extract drop info 
        dive$ndrop    = nrow(st[st$drop=="1",]) 
        dive$dep_drop = st$depth[st$drop=="1"][1]
        dive$event    = st$event[st$drop=="1"][1]
        dive$st_drop  = min(st$st_fit, na.rm=T)
        dive$phase_st = st$phase[st$drop=="1"][1] 
        dive$drop_start = st$dateTime[st$drop=="1"][1]  
      }
      
      # if buzz during dive: calculate duration between buzz and drop
      #---------------------------------------------------------------
      if(any(st$buzz!=0) & any(st$drop=="1")) { # <0: buzz after drop, >0: buzz before drop
        dur_drop_buzz = as.numeric(difftime(st$dateTime[st$drop=="1"],
                                            st$dateTime[st$buzz==1], units="sec"))
        
        dive$dur_bef_drop = min(dur_drop_buzz[which(dur_drop_buzz>0)])      # duration btw drop and previous buzz in secs
        dive$dur_aft_drop = abs(max(dur_drop_buzz[which(dur_drop_buzz<0)])) # duration btw drop and next buzz in secs
        dive$Nbuzz_bef    = length(dur_drop_buzz[which(dur_drop_buzz>0)])
        dive$Nbuzz_aft    = length(dur_drop_buzz[which(dur_drop_buzz<0)])
      }
      
      # save dives from id e
      #-----------------------
      acou2 = rbind(acou2, dive)
    }
  }
})

acou2 = as_tibble(acou2)
summary(acou2)
table(acou2$ndrop, acou2$ptt)


# remove dives with less than < 30 sec
#-----------------------------------------
range(acou2$dive)
range(acou2$dur)                               
nrow(acou2[acou2$dur==0,]) / nrow(acou2) * 100  
nrow(acou2[acou2$dur<0.5,]) / nrow(acou2) * 100 
nrow(acou2[acou2$dur<1,]) / nrow(acou2) * 100   
acou2 = acou2 %>%
  filter(dur>=0.5)
  
range(acou2$n)  
nrow(acou2[acou2$n<60,]) / nrow(acou2) * 100   

summary(acou2$dur)
ggplot(acou2, aes(x=dur)) +
  geom_histogram(bins=10) +
  facet_wrap(.~ptt)


# save summary
#---------------
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/STP_narwhals")
saveRDS(acou2, paste0("./Rdata/Acousonde/Buzz_Drops/calib_",
                      threshold,"m_zoc",offset,"/",
                      "Summary_dive_drops_buzzes_3ids_calib_",
                      threshold,"m_zoc",offset,".RDS"))
names(acou2)










##############################################################
# calculate proportion of successful vs unsuccessful dives
##############################################################
summary(acou2$nbuzz)
table(acou2$ndrop)
acou2 %>%
  group_by(ptt) %>%
  summarise(tot_buzz = sum(nbuzz),
            tot_drop = sum(ndrop))

# subset foraging dives: buzz=1
#---------------------------------
foraging = acou2 %>%
  filter(nbuzz>0) %>%
  select(c(ptt,dive,nbuzz,ndrop))
summary(foraging$nbuzz) 

# subset successful dives: drop=1
#---------------------------------
drop = acou2 %>%
  filter(ndrop>0) %>%
  select(c(ptt,dive,nbuzz,ndrop))

# create tibble with foraging, non foraging, successful and non successful dives
#--------------------------------------------------------------------------------
sum = acou2 %>%
  group_by(ptt) %>%
  summarise(tot_dive       = n(),                            # total number of dives
            drop_nobuzz    = sum(nbuzz==0 & ndrop==1),       # no buzz AND drop
            non_foraging   = sum(nbuzz==0 & ndrop==0),       # no buzz AND no drop
            foraging       = sum(((nbuzz>0 & ndrop==0)       # buzz BUT no drop
                                  | (nbuzz>0 & ndrop==1))),  # buzz AND drop
            successful     = sum(nbuzz>0 & ndrop==1),        # buzz AND drop
            unsuccessful   = sum(nbuzz>0 & ndrop==0)) %>%    # buzz BUT no drop
  mutate(tot_dive2         = tot_dive - drop_nobuzz,
         non_foragingPC    = (non_foraging / tot_dive2) * 100,  # prop non foraging dives
         foragingPC        = (foraging / tot_dive2) * 100,      # prop foraging dives
         successfulPC      = (successful / foraging) * 100,     # prop successful dives 
         unsuccessfulPC    = (unsuccessful / foraging) * 100)   # prop unsuccessful dives
         
sum = data.frame(sum)

setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/STP_narwhals")
saveRDS(sum, paste0("./Rdata/Acousonde/Buzz_Drops/prop_successful_dives_3ids_calib_",
                    threshold,"m_zoc",offset,".RDS"))











##################################
# exploratory plots
##################################

#--------------------------------
# barplot showing dive type
# type: foraging vs no foraging
#--------------------------------
pivot =  sum %>%
  pivot_longer(c(foragingPC,non_foragingPC), 
               names_to = "type", values_to = "prop") %>%
  mutate(type = case_when(type == "non_foragingPC" ~ "non foraging",
                          type == "foragingPC" ~ "foraging"))
pivot$type = factor(pivot$type, 
                    levels=c("foraging","non foraging"))

# plot
ggplot(pivot, aes(x = ptt, y = prop, fill = type)) +
  geom_bar(stat = "identity", position = "fill", color="grey30") +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_manual(values = c("black", "grey")) +
  labs(x = "", y = "Proportion of dives",fill="Dive type") +
  theme_tq()  +
  theme(legend.position = "top",
        axis.line.x = element_line(arrow = NULL))


#---------------------------------
# barplot showing feeding success
#---------------------------------
pivot =  sum %>%
  pivot_longer(c(unsuccessfulPC,successfulPC), 
               names_to = "type", values_to = "prop") %>%
  mutate(type = case_when(type == "unsuccessfulPC" ~ "unsuccessful",
                          type == "successfulPC" ~ "successful"))

pivot$type = factor(pivot$type, 
                    levels=c("successful","unsuccessful"))

# plot
ggplot(pivot, aes(x = ptt, y = prop, fill = type)) +
  geom_bar(stat = "identity", position = "fill", color="grey30") +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_manual(values = c("black", "grey")) +
  labs(x = "", y = "Proportion of dives",fill="Dive type") +
  theme_tq()  +
  theme(legend.position = "top",
        axis.line.x = element_line(arrow = NULL))





