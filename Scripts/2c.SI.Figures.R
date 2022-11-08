###############################################
########    SI FIGURES STP PAPER  #############
##  Chambault et al , Bio Letters, under review
## by Philippine CHAMBAULT, updated: 3 Nov 2022
###############################################


library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyquant)
library(tidyr)
library(scales)


setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES")





###################################################################
# Fig 2: Histo drops depth (from Acousondes)
###################################################################

#--------------
# a) STP only
#--------------
stp <- readRDS("./Rdata/STP/3.STP_clean_7ids.rds") %>% as_tibble()
stp

# filter tibble
#------------------
stp = stp %>%
  mutate(dive_type = case_when(type2 == "start" & !is.na(type2) ~ "drop",
                           TRUE ~ "non-foraging")) %>%
  rename(id = ptt) %>%
  filter(!(id=="3962-Eistla" | id=="7925-Thora" | id=="3965-Frida")) %>%
  select(id,dateTime,depth,dive_type)
stp$id = as.factor(stp$id)
table(stp$id[stp$dive_type=="drop"])
table(stp$dive_type, stp$id)
stp$dive_type = factor(stp$dive_type, levels=c("non-foraging","drop"))

stp %>%
  group_by(id, dive_type) %>%
  summarise(mean = mean(depth, na.rm=T))

# rename ids
#--------------
stp$id = as.character(stp$id)
unique(stp$id)
stp = stp %>%
  mutate(id = case_when(id == "7617-15"   ~ "7617: 2015",
                        id == "7618-Mara" ~ "Mara: 2014",
                        id == "3965-TDR"  ~ "3965: 2013",
                        id == "7618-15"   ~ "7618: 2015"))
stp$id = factor(stp$id, levels = c("3965: 2013","Mara: 2014",
                                   "7618: 2015","7617: 2015"))
table(stp$id, stp$dive_type)


# import summary table for each id
#-----------------------------------
table <- readRDS("./Rdata/Table1_6ids.rds")
table

# A data frame with labels for each facet
table %>% select("id","duration","ndrop")
dur <- data.frame("id" = c("3965: 2013", "Mara: 2014",
                            "7618: 2015","7617: 2015"),
                  y = c(12,30,35,1.5),
                  x = rep(800, times=4),
                  label = c("duration: 190 h","duration: 320 h",
                            "duration: 380 h", "duration: 15 h"))
dur$id = factor(dur$id, levels = c("3965: 2013","Mara: 2014",
                                     "7618: 2015","7617: 2015"))

drop <- data.frame("id" = c("3965: 2013", "Mara: 2014",
                             "7618: 2015","7617: 2015"),
                   y = c(12, 30,35,1.5),
                   x = rep(840, times=4),
                   label = c("n drops: 95", "n drops: 224",
                             "n drops: 129", "n drops: 4"))
drop$id = factor(drop$id, levels = c("3965: 2013","Mara: 2014",
                                     "7618: 2015","7617: 2015"))

# mean drop depth
line = stp %>%
  group_by(id) %>%
  summarize(x = mean(depth[dive_type=="drop"], na.rm=T))
line$id = factor(line$id, levels = c("3965: 2013","Mara: 2014",
                                     "7618: 2015","7617: 2015"))

a = ggplot(stp[stp$dive_type=="drop",], aes(x = depth)) +
  geom_histogram(colour="black",fill="gray64",
                 bins=40,lwd=0.1,position="dodge") +
  coord_flip() +
  scale_x_continuous(trans = "reverse") +
  facet_grid(.~id, scales="free") +
  labs(y = "Drop counts", x = "Dive depth at drops (m)",
       title = "A) STP") +
  geom_vline(data = line, aes(xintercept = x), lty=2,
             lwd=0.2, colour="red") +
  geom_text(data = dur, aes(x = x, y = y, label = label),
            size=1.3, colour="gray64") +
  geom_text(data = drop, aes(x = x, y = y, label = label),
            colour="red",size=1.3) +
  theme(panel.spacing = unit(0, "lines"),
        strip.background = element_rect(fill="steelblue4",size=0.2,
                                        colour="black"),
        strip.text = element_text(colour='white',size=8,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=7, hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        legend.position = c(0.1,0.2),
        axis.text  = element_text(size=6, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0,0.3,0,0.2),"cm"))  #top,right,bottom,left


#-----------------------------------------
# b) STP + Acousonde
#-----------------------------------------
buzz <- readRDS("./Rdata/Acousonde/Buzz_Drops/buzz_drops_3ids_bef-threshold2m.rds")
buzz = buzz %>%
  mutate(dive_type = case_when(drop == "1" ~ "drop",
                               buzz == 1   ~ "buzz",
                               TRUE        ~ "non-foraging"))
table(buzz$dive_type)
buzz$dive_type = factor(buzz$dive_type, 
                        levels=c("non-foraging","buzz","drop"))

table(buzz$dive_type, buzz$id)
buzz = buzz %>%
  mutate(id = case_when(id == "Thora" ~ "Thora: 2014",
                        id == "Frida" ~ "Frida: 2015",
                        id == "Eistla" ~ "Eistla: 2016",))
buzz$drop = as.numeric(as.character(buzz$drop))


# data frame with labels for each facet
#-----------------------------------------
dur <- data.frame("id" = c("Thora: 2014","Frida: 2015","Eistla: 2016"),
                  x = rep(590, times=3),
                  y = c(1.5,20,1),
                  label = c("duration: 91 h","duration: 56 h","duration: 34 h"))
dur$id = factor(dur$id,
                levels=c("Thora: 2014","Frida: 2015","Eistla: 2016"))

drop <- data.frame("id" = c("Thora: 2014","Frida: 2015","Eistla: 2016"),
                   x = rep(620, times=3),
                   y = c(1.5,20,1),
                   label = c("n drops: 21", "n drops: 75", "n drops: 5"))
drop$id = factor(drop$id,
                 levels=c("Thora: 2014","Frida: 2015","Eistla: 2016"))

line = buzz %>%
  group_by(id) %>%
  summarize(x = mean(depth[drop==1], na.rm=T))
line$id = factor(line$id,levels=c("Thora: 2014","Frida: 2015","Eistla: 2016"))

buzz2    = buzz %>% filter(dive_type=="drop")
buzz2$id = factor(buzz2$id,
                 levels=c("Thora: 2014","Frida: 2015","Eistla: 2016"),
                 ordered = T)


b = ggplot(buzz2, aes(x = depth)) +
  geom_histogram(colour="black",fill="gray64",
                 bins=40,lwd=0.1,position="dodge") +
  coord_flip() +
  scale_x_continuous(trans = "reverse") +
  facet_grid(.~id, scales="free") +
  geom_text(data = dur, aes(x = x, y = y, label = label),
            size=1.3,colour="gray64") +
  geom_text(data = drop, aes(x = x, y = y, label = label),
            colour="red",size=1.3) +
  geom_vline(data = line, aes(xintercept = x), lty=2,
             lwd=0.2, colour="red") +
  labs(y = "Drop counts", x = "Dive depth at drops (m)",
       title = "B) STP + Acousonde") +
  theme(panel.spacing = unit(0, "lines"),
        strip.background = element_rect(fill="steelblue4",size=0.2,
                                        colour="black"),
        strip.text = element_text(colour='white',size=8,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=7, hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        legend.position = c(0.1,0.2),
        axis.text  = element_text(size=6, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0,0.3,0,0.2),"cm"))  

# grid.arrange(a,b,ncol=1)

ggsave(filename=paste0("./PAPER/S2.pdf"),
       width=4.5,height=5,units="in",dpi=400,family="ArialMT",
       grid.arrange(a,b,ncol=1)) # width=5,height=2










################################################
# SI3 Figures: ST magnitude and recovery times
################################################

#---------------------------
# a-b: STP + Acousonde tags
#---------------------------
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES")
buzz <- readRDS("./Rdata/Acousonde/Buzz_Drops/buzz_drops_3ids_bef-threshold2m.rds")
summary(buzz$event)

# summarize info per event and id
#--------------------------------
sum = buzz %>%
  filter(event!=0) %>% # remove dives without ST drop
  group_by(id, event) %>%
  summarise(start = first(dateTime),
            end   = last(dateTime),
            time_start = dateTime[type=="start" & !is.na(type)],
            time_drop = dateTime[type=="drop" & !is.na(type)],
            time_end = dateTime[type=="end" & !is.na(type)],
            st_start = signif(st[type=="start" & !is.na(type)],4),
            st_drop = st[type=="drop" & !is.na(type)],
            st_end = st[type=="end" & !is.na(type)],
            drop_dep = mean(depth[type=="drop" & !is.na(type)]),
            mean_st = mean(st_fit))
sum

sum = sum %>%
  mutate(dur = as.numeric(difftime(end, start, units="mins")),
         st_range = abs(st_start - st_drop),
         reco = as.numeric(difftime(time_end, time_drop, units="mins")))
head(data.frame(sum))


library(viridis)
a = ggplot(sum, aes(x = st_range)) + 
  geom_histogram(colour="black",aes(fill=id), 
                 bins=10,lwd=0.1,position="dodge") +
  scale_fill_viridis(discrete = TRUE) +
  labs(y = "Number of drops", x = "(°C)", title = "A) STP + Acousonde:",
       subtitle = "ST amplitude",fill="ID") +
  theme(panel.spacing = unit(0, "lines"),
        strip.background = element_rect(fill="steelblue4",size=0.2,
                                        colour="black"),
        strip.text = element_text(colour='white',size=8,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=7, hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        legend.position = c(0.8,0.8),
        legend.key.size = unit(3,"cm"),
        legend.key.width = unit(0.2,"cm"),
        legend.key.height = unit(0.2,"cm"),
        legend.title = element_blank(),
        legend.text = element_text(size=5),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.key = element_blank(),
        legend.background = element_rect(fill = NA),
        axis.text  = element_text(size=6, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, colour="black"),
        plot.subtitle=element_text(size=8, vjust=0, hjust=0, face = "italic"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0,0.3,0.2,0.2),"cm"))  #top,right,bottom,left


b = ggplot(sum, aes(x = reco)) + 
  geom_histogram(colour="black",aes(fill=id), 
                 bins=10,lwd=0.1,position="dodge") +
  scale_fill_viridis(discrete = TRUE) +
  labs(y = "Number of drops", x = "(min)", title = "B) STP + Acousonde:",
  subtitle = "Recovery time",fill="ID") +
  theme(panel.spacing = unit(0, "lines"),
        strip.background = element_rect(fill="steelblue4",size=0.2,
                                        colour="black"),
        strip.text = element_text(colour='white',size=8,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=7, hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        legend.position = c(0.8,0.8),
        legend.key.size = unit(3,"cm"),
        legend.key.width = unit(0.2,"cm"),
        legend.key.height = unit(0.2,"cm"),
        legend.title = element_blank(),
        legend.text = element_text(size=5),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.key = element_blank(),
        legend.background = element_rect(fill = NA),
        axis.text  = element_text(size=6, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.subtitle=element_text(size=8, vjust=0, hjust=0, face = "italic"),
        plot.title=element_text(size=10, vjust=0, hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0,0.3,0.2,0.2),"cm"))  #top,right,bottom,left

grid.arrange(a,b,ncol=2)




#------------------------
# c-d: STP only tags
#------------------------
stp <- readRDS("./Rdata/STP/STP_clean_7ids.rds") %>% as_tibble()
stp = stp %>% 
  rename(id = ptt, type = type2, st = stp) %>%
  filter(!(id=="3962-Eistla" | id=="7925-Thora" | id=="3965-Frida")) %>%
  filter(!is.na(event))
summary(stp)
table(stp$id, stp$type)

sum = stp %>%
  group_by(id, event) %>%
  summarise(start = dateTime[type=="start" & !is.na(type)],
            end   = dateTime[type=="end" & !is.na(type)],
            st_start   = max(st, na.rm=T),
            st_drop    = min(st, na.rm=T),
            drop_dep   = mean(depth[type=="start" & !is.na(type)]))
sum

sum = sum %>%
  mutate(dur = as.numeric(difftime(end, start, units="mins")),
         st_range = abs(st_start - st_drop),
         reco = as.numeric(difftime(end, start, units="mins")))
 

# events > 4h: possibly due to data gaps
#-----------------------------------------
summary(sum$reco)        # up to 4h: due to data gaps from sat tag?
nrow(sum[sum$reco>60,])  # 15 events
nrow(sum[sum$reco>60,]) / nrow(sum) * 100 # 3%
sum[sum$reco>60,] 
head(data.frame(sum))
summary(sum$reco[sum$reco<=60]) # 1.5 to 57 min

# events shorter than 2 min: possibly missed by the STP tag
#--------------------------------------------------------------
nrow(sum[sum$reco<=2,]) / nrow(sum) * 100 # 1.3% lasting < 2min
sum[sum$reco<=2,]

sum = sum %>%
  mutate(id = case_when(
    id == "3965-TDR"  ~ "3965",
    id == "7617-15"   ~ "7617",
    id == "7618-15"   ~ "7618",
    id == "7618-Mara" ~ "Mara"))
table(sum$id)

c = ggplot(sum, aes(x = st_range)) + 
  geom_histogram(colour="black",aes(fill=id), 
                 bins=10,lwd=0.1,position="dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Number of drops", x = "(°C)", title = "C) STP",
       subtitle ="ST amplitude", fill="ID") +
  theme(panel.spacing = unit(0, "lines"),
        strip.background = element_rect(fill="steelblue4",size=0.2,
                                        colour="black"),
        strip.text = element_text(colour='white',size=8,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=7, hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        legend.position = c(0.8,0.8),
        legend.key.size = unit(3,"cm"),
        legend.key.width = unit(0.2,"cm"),
        legend.key.height = unit(0.2,"cm"),
        legend.title = element_blank(),
        legend.text = element_text(size=5),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.key = element_blank(),
        legend.background = element_rect(fill = NA),
        axis.text  = element_text(size=6, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, colour="black"),
        plot.subtitle=element_text(size=8, vjust=0, hjust=0, face = "italic"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0,0.3,0.2,0.2),"cm"))  #top,right,bottom,left


d = ggplot(sum, aes(x = reco)) + 
  geom_histogram(colour="black",aes(fill=id), 
                 bins=10,lwd=0.1,position="dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Number of drops", x = "(min)", 
       title = "D) STP", subtitle ="Recovery time", fill="ID") +
  theme(panel.spacing = unit(0, "lines"),
        strip.background = element_rect(fill="steelblue4",size=0.2,
                                        colour="black"),
        strip.text = element_text(colour='white',size=8,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=7, hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        legend.position = c(0.8,0.8),
        legend.key.size = unit(3,"cm"),
        legend.key.width = unit(0.2,"cm"),
        legend.key.height = unit(0.2,"cm"),
        legend.title = element_blank(),
        legend.text = element_text(size=5),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.key = element_blank(),
        legend.background = element_rect(fill = NA),
        axis.text  = element_text(size=6, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, colour="black"),
        plot.subtitle=element_text(size=8, vjust=0, hjust=0, face = "italic"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0,0.3,0.2,0.2),"cm"))  #top,right,bottom,left

grid.arrange(c,d,ncol=2)

ggsave(filename=paste0("./S3.pdf"),
       width=5,height=4.5,units="in",dpi=400,family="ArialMT",
       grid.arrange(a,b,c,d,ncol=2)) 



