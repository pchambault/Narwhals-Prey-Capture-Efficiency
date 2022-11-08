###################################################
####       SI Tables for STP Paper       ##########
##  Chambault et al , Bio Letters, under review ###
## by Philippine CHAMBAULT, updated: 3 Nov 2022 ###
###################################################

detach("package:plyr", unload = TRUE)
library(tidyr)
library(dplyr)
library(kableExtra)

setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES/STP_narwhals")
options(digits=10)





#######################
# STP + Acousonde
#######################
buzz <- readRDS("./Buzz_drops_3ids_threshold2m.rds") %>%
  mutate(dive_type = case_when(drop == "1" ~ "drop",
                               buzz == 1   ~ "buzz",
                               TRUE        ~ "non-foraging"),
         drop2 = as.numeric(as.character(drop)))
buzz$dive_type = factor(buzz$dive_type, 
                        levels=c("non foraging","buzz","drop"))

# summarize data per id
#------------------------
options(digits=2)
table1 = buzz %>%
  group_by(id) %>%
  summarize(start = first(dateTime),
            end   = last(dateTime),
            n_obs = length(dateTime),
            ndrop = sum(drop2),
            nbuzz = sum(buzz),
            mean_drop_depth = format(round(mean(depth[drop==1]), 3), nsmall = 1),
            sd_drop_depth   = format(round(sd(depth[drop==1]), 3), nsmall = 1),
            mean_buzz_depth = format(round(mean(depth[buzz==1]), 3), nsmall = 1),
            sd_buzz_depth   = format(round(sd(depth[buzz==1]), 3), nsmall = 1)) %>%
  mutate(duration = as.numeric(difftime(end, start, units="hours")),
         "tag_type" = "STP+Acou") %>%
  select(tag_type, everything())  %>%
  ungroup()
names(table1)

# daily buzzes/ID
table1$daily_buzzes = format(round(table1$nbuzz / table1$duration * 24, 
                             2), nsmall = 1)
table1
mean(as.numeric(table1$daily_buzzes)) 
sd(as.numeric(table1$daily_buzzes))   

# daily drops/ID
table1$daily_drops = format(round(table1$ndrop / table1$duration * 24, 
                                   2), nsmall = 1)
table1
names(table1)





#################
# STP only
#################
stp <- read.csv("./DRYAD/STP_clean_7ids.txt") %>% # import dataset containing the STP only tags (n=7)
  as_tibble() %>% 
  mutate(drop = case_when(type2 == "start" & !is.na(type2) ~ 1,
                          TRUE ~ 0),
         dateTime = as.POSIXct(strptime(dateTime,
                                        format="%Y-%m-%d %H:%M:%S"), tz="GMT")) %>%
  rename(id = ptt) %>%
  filter(!(id=="3962-Eistla" | id=="7925-Thora" | id=="3965-Frida")) %>% # remove the 3 Acousonde ids
  mutate(id = case_when(id == "7617-15"   ~ "7617: 2015",                # rename ids
                        id == "7618-Mara" ~ "Mara: 2014",
                        id == "3965-TDR"  ~ "3965: 2013",
                        id == "7618-15"   ~ "7618: 2015"))
unique(stp$id)

# start and end per id
#------------------------
table2 = stp %>%
  group_by(id) %>%
  summarize(start = first(dateTime),
            end   = last(dateTime),
            n_obs = length(dateTime),
            ndrop = sum(drop),
            nbuzz = NA,
            mean_drop_depth = format(round(mean(depth[drop==1],na.rm=T), 3), nsmall = 1),
            sd_drop_depth   = format(round(sd(depth[drop==1],na.rm=T), 3), nsmall = 1)) %>%
  mutate(duration = as.numeric(difftime(end, start, units="hours")),
         "tag_type" = "STP",
         "mean_buzz_depth" = NA,
         "sd_buzz_depth"   = NA,
         daily_drops  = format(round(ndrop / duration * 24, 
                                     2), nsmall = 1),
         daily_buzzes = NA) %>%
  select(tag_type, everything())

names(table2)
table2$duration # 191 15 378 317 (in hours)
table2 = data.frame(table2)
names(table2)

# reorder columns
table2 = table2[,c(1:9,11:12,10,14:13)]









##########################################
##             Table 1                 ##
##########################################

# aggregate both tables
#------------------------
names(table1)
names(table2)
s1 = rbind(table1, table2)
names(s1)
s1
s1 = s1[order(s1$start),]

# calculate means+SD
s1$start = as.character(s1$start)
s1$end   = as.character(s1$end)
s1
names(s1)

# reorder columns
s1 = s1[,c(2,1,3:4,12,6,14,8:9,7,13,10:11)]

# remove #3965 because already present in H-J et al (2014) paper
s1 = s1 %>% filter(id != "3965: 2013")
s1$id[s1$id == "7617: 2015"] = "7617"
s1$id[s1$id == "7618: 2015"] = "7618"
s1$id[s1$id == "Mara: 2014"] = "Mara"
unique(s1$id)

unique(s1$id)
names(s1)
str(s1)
saveRDS(s1, "./Rdata/Table1_6ids.rds")

# add means+SD for drop and buzz depths
#----------------------------------------
names(s1)
s1b = s1 %>% 
  select(1:7) %>%
  mutate(duration = format(round(as.numeric(duration),3), nsmall = 0),
         duration = as.character(duration),
         `Depth at drops (m)` = paste0(format(round(as.numeric(s1$mean_drop_depth),3), 
                                              nsmall = 1),"±",
                                       format(round(as.numeric(s1$sd_drop_depth),3), 
                                              nsmall = 1)),
         `n buzzes` = s1$nbuzz,
         `n daily buzzes` = s1$daily_buzzes,
         `Depth at buzzes (m)` = paste0(format(round(as.numeric(s1$mean_buzz_depth),3), 
                                               nsmall = 1),"±",
                                        format(round(as.numeric(s1$sd_buzz_depth),3), 
                                               nsmall = 1)))
colnames(s1b) = c("ID","Tag type","Start","End","Duration (h)",
                  "n drops","n daily drops","Depth at drops (m)","n buzzes",
                  "n daily buzzes","Depth at buzzes (m)")
s1b


# add means+SD at the bottom of table
#----------------------------------------
s1b = rbind(s1b,c(rep("",times=4),
                  # Duration
                  paste0(format(round(mean(as.numeric(s1$duration)),3), nsmall = 1),"±",
                         format(round(sd(as.numeric(s1$duration)),3), nsmall = 1)),
                  # n drops
                  paste0(format(round(mean(as.numeric(s1$ndrop)),3), nsmall = 1),"±",
                         format(round(sd(as.numeric(s1$ndrop)),3), nsmall = 1)),
                  # daily drops
                  paste0(format(round(mean(as.numeric(s1$daily_drops)),3), nsmall = 1),"±",
                         format(round(sd(as.numeric(s1$daily_drops)),3), nsmall = 1)),
                  # drop depth
                  paste0(format(round(mean(as.numeric(s1$mean_drop_depth)),3), nsmall = 1),"±",
                         format(round(sd(as.numeric(s1$mean_drop_depth)),3), nsmall = 1)),
                  # n buzzes
                  paste0(format(round(mean(as.numeric(s1$nbuzz),na.rm=T),3), nsmall = 1),"±",
                         format(round(sd(as.numeric(s1$nbuzz),na.rm=T),3), nsmall = 1)),
                  # daily buzzes
                  paste0(format(round(mean(as.numeric(s1$daily_buzzes),na.rm=T),3), nsmall = 1),"±",
                         format(round(sd(as.numeric(s1$daily_buzzes),na.rm=T),3), nsmall = 1)),
                  # depths at buzzes
                  paste0(format(round(mean(as.numeric(s1$mean_buzz_depth),na.rm=T),3), nsmall = 1),"±",
                         format(round(sd(as.numeric(s1$mean_buzz_depth),na.rm=T),3), nsmall = 1)) ))

s1b
names(s1b)

# replace NA by "-"
s1b[is.na(s1b)] = "-"
s1b$`Depth at buzzes (m)`[c(1,3:4)] = "-"

# add column "Sex"
rownames(s1b) = NULL
s1b$Sex = c(rep("F",6),"")
s1b = s1b %>% relocate("Sex", .before = `Tag type`)
s1b
webshot::install_phantomjs()

# export table
#--------------
names(s1b)
names(s1b)[6]  = "Duration"
names(s1b)[8]  = "n daily"
names(s1b)[9]  = "Depths at "
names(s1b)[11] = "n daily "
names(s1b)[12] = "Depth at "
names(s1b)

colNames <- names(s1b)
dfUnits <- c("","","","","",
             "(h)", " ","drops",
             "drops (m)","","buzzes",
             "buzzes (m)")

# export table
#----------------
kable(s1b, col.names = dfUnits, escape = F, align = "c",bold=TRUE) %>%
  add_header_above(header = colNames, line = F, align = "c",bold=TRUE) %>%
  kable_classic(full_width=F, html_font="ArialMT") %>%
  kable_styling(latex_options = c("HOLD_position"),
                bootstrap_options = c("striped", "hover", "condensed"),
                position = "center",
                full_width = T,
                font_size = 12) %>% 
  row_spec(0, bold=T) %>%
  kable_styling(full_width = F, bootstrap_options = "striped") %>%
  save_kable("./Table1.png", 
             density=400, zoom = 3) 















##########################################
# Table 2: corrected feeding rate
#           and daily food consumption
##########################################
names(s1b)
s1b = s1b %>% select(-c(`n drops`))
s1b = s1b[,c(1:2,7,4)]
s1b = s1b[-c(nrow(s1b)),]

s2  = s1b %>%
  # filter(ID != "3965: 2013") %>%
  # select(ID,Sex,`n daily drops`,Start) %>%
  mutate("Length"    = c(341,390,400,410,380,360),
         "Body_mass" = c(557,828,898,974,764,650),
         Start       = as.numeric(substr(Start, 1, 4)))  %>%
  rename(n_daily_drops = 'n daily') 

old_ids = data.frame("ID"=c("22849","22850","22853","22638","3963",
                            "3964","6335","3965"),
                     "Sex"=c("M","M","M","F","M","M","M","M"),
                     'n_daily_drops'=c(8,2.6,4.8,14.8,13,14.4,9.7,11.8),
                     "Start"=c("2012*","2012*","2012*","2012*",
                               "2013*","2013*","2013*","2013*"),
                     'Length'=c(225,400,278,400,400,356,390,440),
                     'Body_mass'=c(546,849,370,898,849,629,793,940))

names(old_ids)
names(s2)
s2 = rbind(old_ids, s2)
s2 = s2[,c(1:2,4:6,3)]
s2 = s2 %>% 
  rename(Year = Start, `Feeding rate (/d)` = n_daily_drops) %>%
  filter(!row_number() %in% c(15))


# import correction factor calculated for the 3 Acousondes
#---------------------------------------------------------
options(digits=10)
corr <- readRDS("./corr_factor_nsat-nacou.rds")
corr = corr %>% select(-c(start,end))
corr
mean_corr = mean(corr$corr_factor)  # 1.961988304

s2 = s2 %>%
  mutate("ndrops from STP" = c(5,45,2,8,
                               13,18,19,94,
                               224,19,4,129,19,6),
         "n drops from Acousonde" = as.numeric(c(rep(NA,times=9),
                                                 "21",NA,NA,"75","5")),
         "Correction factor"      = as.numeric(c(rep(NA,times=9),
                                                 "1.11",NA,NA,"3.95","0.83")))
s2


# correct ndrops for each whale
#--------------------------------
s2$`n drops corrected` = format(round(s2$`ndrops from STP` * mean_corr,1), 
                                nsmall = 1)
s2
s2$`n drops corrected`[c(10,13,14)] = NA # replace by NA for whales with Acousonde

# tracking duration in hours
#------------------------------
dur_hours_new = s1 %>%
  select(id, duration) %>%
  rename(ID = id, hours = duration)

dur_hours_old = data.frame("ID"=unique(s2$ID[1:8]),
                           "hours" = c(15,414,10,13,24,30,47,191))
duration = rbind(dur_hours_new, dur_hours_old)
duration = duration %>% mutate(days = hours / 24) 
duration$hours = as.numeric(format(round(duration$hours,2), nsmall = 2))
s2 = left_join(s2, duration) # join by ID


# feeding rate corrected
#-------------------------
s2$`Feeding rate corrected (/d)` = format(round(as.numeric(s2$`n drops corrected`) / s2$days,1), nsmall = 1)
round(as.numeric(s2$`n drops corrected`) / s2$days)
mean_prey_mass = 0.15 # average prey mass (Gonatus fabricii) in kg (from Golikov et al 2018)
s2$`Feeding rate corrected (/d)` = as.numeric(s2$`Feeding rate corrected (/d)`)
s2$`Food intake (kg/d)` = format(round(s2$`Feeding rate corrected (/d)` * mean_prey_mass,1), nsmall = 1) # in kg
s2$`Food intake (kg/d)`[c(10,13,14)] = format(round(as.numeric(s2$`Feeding rate (/d)`[c(10,13,14)]) * mean_prey_mass,1), nsmall = 1) # in kg
s2$`Food intake (kg/d)` = as.numeric(s2$`Food intake (kg/d)`)
s2$`Food consumption (%biomass/d)` = format(round(as.numeric(s2$`Food intake (kg/d)`) / as.numeric(s2$Body_mass)* 100,1), nsmall = 1) 
s2
mean(as.numeric(s2$`Food consumption (%biomass/d)`),na.rm=T) # mean: 0.39%

names(s2)
s2 = s2 %>% 
  rename(`Body mass (kg)` = Body_mass, `Length (cm)` = Length) %>%
  select(-c(days,hours))
s2


# add means+SD at the bottom of table
#----------------------------------------
names(s2)
s2 = rbind(s2,c(rep("",times=3),
                # Length
                paste0(format(round(mean(as.numeric(s2$`Length (cm)`)),1), nsmall = 1),"±",
                       format(round(sd(as.numeric(s2$`Length (cm)`)),1), nsmall = 1)),
                # body mass
                paste0(format(round(mean(as.numeric(s2$`Body mass (kg)`)),1), nsmall = 1),"±",
                       format(round(sd(as.numeric(s2$`Body mass (kg)`)),1), nsmall = 1)),
                # feeding rate
                paste0(format(round(mean(as.numeric(s2$`Feeding rate (/d)`)),1), nsmall = 1),"±",
                       format(round(sd(as.numeric(s2$`Feeding rate (/d)`)),1), nsmall = 1)),
                # ndrops from STP
                paste0(format(round(mean(as.numeric(s2$`ndrops from STP`)),1), nsmall = 1),"±",
                       format(round(sd(as.numeric(s2$`ndrops from STP`)),1), nsmall = 1)),
                # n drops from Acousonde
                paste0(format(round(mean(as.numeric(s2$`n drops from Acousonde`),na.rm=T),1), nsmall = 1),"±",
                       format(round(sd(as.numeric(s2$`n drops from Acousonde`),na.rm=T),1), nsmall = 1)),
                # Correction factor
                paste0(format(round(mean(as.numeric(s2$`Correction factor`),na.rm=T),2), nsmall = 1),"±",
                       format(round(sd(as.numeric(s2$`Correction factor`),na.rm=T),2), nsmall = 1)),
                # n drops corrected
                paste0(format(round(mean(as.numeric(s2$`n drops corrected`),na.rm=T),1), nsmall = 1),"±",
                       format(round(sd(as.numeric(s2$`n drops corrected`),na.rm=T),1), nsmall = 1)),
                # Feeding rate corrected
                paste0(format(round(mean(as.numeric(s2$`Feeding rate corrected (/d)`),na.rm=T),1), nsmall = 1),"±",
                       format(round(sd(as.numeric(s2$`Feeding rate corrected (/d)`),na.rm=T),1), nsmall = 1)),
                # Food intake
                paste0(format(round(mean(as.numeric(s2$`Food intake (kg/d)`)),1), nsmall = 1),"±",
                       format(round(sd(as.numeric(s2$`Food intake (kg/d)`)),1), nsmall = 1)),
                # Food consumption
                paste0(format(round(mean(as.numeric(s2$`Food consumption (%biomass/d)`)),1), nsmall = 1),"±",
                       format(round(sd(as.numeric(s2$`Food consumption (%biomass/d)`)),1), nsmall = 1)) ))
s2
s2[is.na(s2)] = "-"


# export table
#--------------
names(s2)[4] = "Length"
names(s2)[5] = "Body mass"
names(s2)[6] = "Feeding rate"
names(s2)[7] = "n drops from "
names(s2)[8] = "n drops from "
names(s2)[9] = "Correction "
names(s2)[10] = "n drops  "
names(s2)[11] = "Feeding rate "
names(s2)[12] = "Food intake"
names(s2)[13] = "Food consumption"
names(s2)

colNames <- names(s2)
dfUnits <- c("","","","(cm)","(kg)",
             "(d<sup>-1</sup>)", "STP",
             "Acousonde","factor","corrected",
             "corrected (d<sup>-1</sup>)","(kg.d<sup>-1</sup>)",
             "(%biomass.d<sup>-1</sup>)")

kable(s2, col.names = dfUnits, escape = F, align = "c",bold=TRUE) %>%
  add_header_above(header = colNames, line = F, align = "c",bold=TRUE) %>%
  kable_classic(full_width=F, html_font="ArialMT") %>%
  kable_styling(latex_options = c("HOLD_position"),
                bootstrap_options = c("striped", "hover", "condensed"),
                position = "center",
                full_width = T,
                font_size = 12) %>% 
  row_spec(0, bold=T) %>%
  kable_styling(full_width = F, bootstrap_options = "striped") %>%
  save_kable("./Table2.pdf", density=400) 















###############################################################################
# test difference between feeding rate from HJ 2014 and the one in this study
###############################################################################
stat = s2 %>% 
  filter(!row_number() %in% c(15)) %>%
  select(ID, `Feeding rate (/d)`) %>%
  mutate(study = "MP_2014") %>%
  rename(rate=`Feeding rate (/d)`) %>%
  mutate(rate = as.numeric(rate)) %>%
  as_tibble() 

stat$study[c(9:14)] = "Our study"
kruskal.test(rate~study, stat)  # p=1: not statistically different!
t.test(rate~study, stat)        # p=0.64: not statistically different!
wilcox.test(rate~study, stat)   # p=1: not statistically different!

ggplot(stat,aes(x=study, y=rate)) +
  geom_boxplot() +
  geom_point()

stat %>% 
  group_by(study) %>%
  summarise(mean = mean(rate),
            sd = sd(rate),
            median = median(rate))

# some statistics
#------------------
s2$`Food consumption (%biomass/d)` = as.numeric(s2$`Food consumption (%biomass/d)`)
s2$`Feeding rate corrected (/d)` = as.numeric(s2$`Feeding rate corrected (/d)`)
mean(s2$`Feeding rate corrected (/d)`, na.rm=T)   # 19.7 drops/day
sd(s2$`Feeding rate corrected (/d)`, na.rm=T)     # 8.8 drops/day
s2$`Food intake (kg/d)` = as.numeric(s2$`Food intake (kg/d)`)
mean(s2$`Food intake (kg/d)`, na.rm=T)            # 2.7 kg/day
sd(s2$`Food intake (kg/d)`, na.rm=T)              # 1.54
mean(s2$`Food consumption (%biomass/d)`, na.rm=T) # 0.8 %/day
sd(s2$`Food consumption (%biomass/d)`, na.rm=T)   # 0.2








