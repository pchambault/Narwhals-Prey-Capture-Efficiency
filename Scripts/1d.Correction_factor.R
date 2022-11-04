##########################################################################
####    correct ndrops from sat tag using ndrops from Acousonde    #######
##########################################################################

library(ggplot2)
library(tidyquant)
library(gridExtra)
library(tidyverse)
library(dplyr)
library(viridis)
library(DT)
library(data.table)


setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/STP_narwhals")




#################
# load datasets
#################
acou <- readRDS("./Rdata/Acousonde/Buzz_Drops/Buzz_drops_3ids_threshold2m.rds") %>%
  select(id, dateTime, dateTime2, drop, buzz, depth)

sat <- readRDS("./Rdata/STP/STP_clean_7ids.rds") %>%
  as_tibble() %>%
  mutate(dateTime2 = as.character(dateTime),
         drop = case_when(type2=="start" & !is.na(type2) ~ 1,
                          TRUE ~ 0),
         ptt = case_when(ptt == "3962-Eistla" ~ "Eistla",
                         ptt == "3965-Frida" ~ "Frida",
                         ptt == "7925-Thora" ~ "Thora")) %>%
  rename(id = ptt) %>%
  filter(!is.na(id)) %>%
  select(id,dateTime,dateTime2,drop)
table(sat$drop, sat$id)

sat %>%
  group_by(id) %>%
  summarise(start = first(dateTime),
            end   = last(dateTime))






#############################################
# merge acousonde and sat tag per ID
#############################################

# acou summary 
#-------------------
start_end_acou = acou %>%
  group_by(id) %>%
  summarise(start = first(dateTime),
            end   = last(dateTime)) %>%
  ungroup()

acou2 = acou %>%
  left_join(acou, sat, by = "dateTime2") %>%
  rename(id = id.x, dateTime_sat = dateTime.x, 
         dateTime_acou = dateTime.y) %>%
  select(-id.y) %>%
  left_join(., start_end_acou, by = "id") %>%
  filter(dateTime_acou < end & dateTime_acou > start) %>%
  select(-c(start, end, dateTime_sat, drop.y,buzz.x,depth.y)) %>%
  rename(dateTime = dateTime_acou, drop = drop.x, 
         buzz = buzz.y, depth = depth.x)
acou2

acou2 %>%
  group_by(id) %>%
  summarise(start = first(dateTime2),
            end   = last(dateTime2),
            ndrop = sum(drop),
            nbuzz = sum(buzz))
start_end_acou

# sat tag summary
#----------------------
sat2 = sat %>%
  left_join(sat, acou, by = "dateTime2") %>%
  rename(id = id.x, dateTime_sat = dateTime.x,
         dateTime_acou = dateTime.y) %>%
  select(-id.y) %>%
  left_join(., start_end_acou, by = "id") %>%
  filter(dateTime_sat < end & dateTime_sat > start) %>%
  select(-c(start, end, dateTime_acou, drop.y)) %>%
  rename(dateTime = dateTime_sat, drop = drop.x)
sat2

sat2 %>%
  group_by(id) %>%
  summarise(start = first(dateTime2),
            end   = last(dateTime2),
            ndrop = sum(drop))
start_end_acou 






###############################################
# calculate correction factor = n_acou / n_sat
###############################################

# ndrops from Acousonde
#-----------------------
sum_acou = acou2 %>%
  group_by(id) %>%
  summarize(ndrop_acou = sum(drop),
            nbuzz      = sum(buzz),
            duration   = round(as.numeric(difftime(last(dateTime), 
                                                   first(dateTime), units="hours")))) %>%
  mutate(length    = c(360,380,390),      # body length of each animal in cm
         body_mass = c(650,764,828)) %>%  # body mass of each animal in kg 
  ungroup()


# ndrops from STP only
#-----------------------
sum_sat = sat2 %>%
  group_by(id) %>%
  summarize(ndrop_sat = sum(drop)) %>%
  ungroup()

# merge both tables to have ndrops from both tags
#---------------------------------------------------
options(digits=10)
sum = cbind(sum_acou[,-c(1)], start_end_acou, sum_sat[,-c(1)])
sum = sum %>%
  mutate(corr_factor = ndrop_acou / ndrop_sat,
         nbuzz_day   = round(nbuzz / (duration)*24),
         ndrop_day   = round(ndrop_acou / (duration)*24),
         sex = c(rep("F", times=3)),
         tag_type = c(rep("STP+Acou", times=3)))
sum
names(sum)
corr_factor = sum[,c(6:8,13:14,3:5,9,1,10)] # reorder columns
corr_factor
saveRDS(corr_factor, "./Rdata/corr_factor_nsat-nacou.rds")







