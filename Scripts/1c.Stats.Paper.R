###################################################
####         Table summary Paper         ##########
##  Chambault et al , Bio Letters, under review
## by Philippine CHAMBAULT, updated: 3 Nov 2022
###################################################

# detach("package:plyr", unload = TRUE)
library(tidyr)
library(dplyr)
library(ggplot2)


setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/STP_narwhals")



#########################################
# 1. Drop and buzz frequency (Fig. 1a)
#########################################

#-------------------------------
# recovery time and magnitude
#-------------------------------
buzz <- readRDS("./Rdata/Acousonde/Buzz_Drops/Buzz_drops_3ids_threshold2m.rds") %>%
  mutate(dive_type = case_when(drop == "1" ~ "drop",
                               buzz == 1   ~ "buzz",
                               TRUE ~ "non-foraging")) 
table(buzz$dive_type)  # drop, non foraging, buzz

# extract summary info per feeding event
#---------------------------------------
event = buzz %>%
  filter(event!=0) %>%
  group_by(id,event) %>%
  summarise(start  = first(dateTime),
            end    = last(dateTime),
            min_st = min(st_fit),
            max_st = max(st_fit)) %>%
  mutate(reco  = as.numeric(difftime(end,  # recovery time
                                     start, units="min")),
         magni = max_st - min_st)          # event magnitude

event
summary(event$reco)    

# discard recovery times < 5 min (too short to be a real feeding event)
event %>% filter(reco<5) 
summary(event$magni)   


# tracking duration and number of buzzes
#----------------------------------------
sum_data = buzz %>%
  group_by(id) %>%
  summarise(start  = first(dateTime),
            end    = last(dateTime),
            nbuzz  = sum(buzz)) %>%
  mutate(track_dur = difftime(end, start,units="hours"))
sum_data


#------------------------
# proportion of dives/id
#------------------------
sum <- readRDS(paste0("./Rdata/Acousonde/Buzz_Drops/calib_2m_zoc0/",
                      "Summary_dive_drops_buzzes_3ids_calib_2m_zoc0.RDS"))
sum
sum %>%
  group_by(ptt) %>%
  summarise(ndive = max(dive),
            start = first(start),
            end   = last(end),
            nbuzz = sum(nbuzz),
            ndrop = sum(ndrop))



#----------------------------
# drop and buzz frequency
#----------------------------
stp  <- read.csv("./DRYAD/STP_clean_7ids.txt") %>%
  as_tibble() %>%
  filter(!(ptt=="3962-Eistla" | ptt=="7925-Thora" | ptt=="3965-Frida")) %>% # remove the 3 Acousonde ids
  mutate(drop = case_when(type2=="start" & !is.na(type2) ~ "drop",
                          TRUE ~ "non-foraging"),
         dateTime = as.POSIXct(strptime(dateTime,
                                        format="%Y-%m-%d %H:%M:%S"), tz="GMT")) %>%
  rename(id = ptt)

summary(stp$depth[stp$drop=="drop"])      
sd(stp$depth[stp$drop=="drop"], na.rm=T)  
nrow(buzz[buzz$buzz==1 & buzz$depth<200,]) / nrow(buzz[buzz$buzz==1,]) * 100 
nrow(buzz[buzz$drop==1 & buzz$depth<200,]) / nrow(buzz[buzz$drop==1,]) * 100 

drop = rbind(stp[c("ptt","drop","depth")],
             buzz[c("ptt","drop","depth")])
nrow(drop[drop$depth<200,]) / nrow(drop) * 100 
nrow(drop[drop$depth<100,]) / nrow(drop) * 100 
nrow(drop[drop$depth<50 & drop$drop==1,]) / nrow(drop) * 100  










#########################################
# 2. Drop and buzz depths (Figs. 1b,d)
#########################################

#--------------------------------------------
# drop depth
#--------------------------------------------
table(sum$ndrop) 
mean(sum$dep_drop[sum$ndrop==1])          
sd(sum$dep_drop[sum$ndrop==1])      

# depths when buzz and drop
summary(sum$dep_drop[sum$ndrop==1 & sum$nbuzz>0])  
sd(sum$dep_drop[sum$ndrop==1 & sum$nbuzz>0])    

# % of dives with drop but within the first 50 m
nrow(sum[sum$ndrop==1 & sum$dep_drop<=50 ,]) / nrow(sum) * 100 


#--------------------------------------------
# proportion of drops w/wo buzzes
#--------------------------------------------
nrow(sum[sum$nbuzz!=0 & sum$ndrop==1,]) / nrow(sum[sum$ndrop==1,]) * 100 
nrow(sum[sum$nbuzz==0 & sum$ndrop==1,]) / nrow(sum[sum$ndrop==1,]) * 100

# Eistla
id = sum %>%
  filter(ptt == "Eistla")
nrow(id[id$nbuzz==0 & id$ndrop==1,]) / nrow(id[id$ndrop==1,]) * 100 

# Thora
id = sum %>%
  filter(ptt == "Thora")
nrow(id[id$nbuzz==0 & id$ndrop==1,]) / nrow(id[id$ndrop==1,]) * 100 

# Frida
id = sum %>%
  filter(ptt == "Frida")
nrow(id[id$nbuzz==0 & id$ndrop==1,]) / nrow(id[id$ndrop==1,]) * 100 
summary(id$dep_drop[id$ndrop==1])             
summary(id$dep_drop[id$ndrop==1 & id$nbuzz==0]) 












##############################################
# 3. Drop magnitude and recovery (SI Fig S3)
##############################################

# Acousonde+STP tags
#----------------------
buzz = buzz %>%
  dplyr::select(id,dateTime,st_fit,depth,drop,event) %>%
  dplyr::rename(stp = st_fit)

# STP only tags
#----------------------
stp = stp %>%
  rename(id = ptt) %>%
  dplyr::select(id,dateTime,stp,depth,drop,event)

# aggregate both STP and Acousonde datasets
#-------------------------------------------
stp$id = as.factor(stp$id)
stp$id = droplevels(stp$id)
stp$drop = factor(stp$drop, levels=c("non foraging","drop"))
stp$event[is.na(stp$event)] = 0

dat = rbind(stp, buzz)
table(dat$id)
dat %>%
  group_by(id) %>%
  summarise(n_events = max(event))
range(dat$event, na.rm=T) 

# summarize events: magnitude, recovery
#---------------------------------------
sum = dat %>%
  filter(!is.na(event) & event!=0) %>%
  group_by(id, event) %>%
  summarise(start   = first(dateTime),
            end     = last(dateTime),
            min_stp = min(stp, na.rm=T),
            max_stp = max(stp, na.rm=T),
            depth_drop = first(depth)) %>%
  mutate(recovery = as.numeric(difftime(end, start, units="mins")),
         magni = abs(max_stp - min_stp)) %>%
  ungroup()
sum
summary(sum$recovery)
table(sum$id)


# recovery time across ids
#----------------------------
means = sum %>%
  group_by(id) %>%
  summarise(mean_reco = mean(recovery),
            sd_reco = sd(recovery),
            mean_magni = mean(magni),
            sd_magni = sd(magni)) %>%
  ungroup()

mean(means$mean_reco)  
sd(means$mean_reco)    
kruskal.test(recovery~id, sum)    # p<0.001
ggplot(sum,aes(x=id, y=recovery)) +
  geom_boxplot()

# drop depth across ids
summary(sum$depth_drop)           # 12 NA
kruskal.test(depth_drop~id, sum)  # p<0.001
ggplot(sum,aes(x=id, y=-depth_drop)) +
  geom_boxplot()

# drop magnitude across ids
summary(sum$magni)  
kruskal.test(magni~id, sum)  # p<0.001
ggplot(sum,aes(x=id, y=magni)) +
  geom_boxplot()
means
mean(means$mean_magni)  
sd(means$mean_magni)   
sum[which.min(sum$magni),] %>% dplyr::select(id,magni) 
sum[which.max(sum$magni),] %>% dplyr::select(id,magni) 
range(sum$recovery)
range(sum$magni)







##############################################
# 4. Proportion of successful dives (Fig 2a,b)
##############################################
prop <- readRDS("./Rdata/Acousonde/Buzz_Drops/prop_successful_dives_3ids_calib_2m_zoc0.RDS")
prop

# % foraging dives
mean(prop$foragingPC) # 21.5%
sd(prop$foragingPC)   # 2.9











