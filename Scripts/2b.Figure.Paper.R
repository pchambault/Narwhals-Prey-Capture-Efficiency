###############################################
########     FIGURE 1 STP PAPER    ############
##  Chambault et al , Bio Letters, under review
## by Philippine CHAMBAULT, updated: 3 Nov 2022
###############################################


library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyquant)
library(tidyr)
library(scales)
library(data.table)
library(reshape2)



setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES")





###################################################################
# Fig 1: dive profile vs time (Acousondes)
###################################################################

#-----------------------------
# a) depth over time (STP+Acou)
#-----------------------------
buzz <- readRDS("./Rdata/Acousonde/Buzz_Drops/Buzz_drops_3ids_threshold2m.rds") %>%
  mutate(dive_type = case_when(drop=="1" ~ "drop",
                               buzz == 1 ~ "buzz",
                               TRUE ~ "non-foraging"),
         id = case_when(id == "Thora" ~ "Thora: 2014",
                        id == "Frida" ~ "Frida: 2015",
                        id == "Eistla" ~ "Eistla: 2016")) 
table(buzz$dive_type, buzz$id)

# reorder dive_type levels
#---------------------------
buzz$dive_type = factor(buzz$dive_type, 
                        levels=c("non-foraging","buzz","drop"))

# reorder id levels
#---------------------------
buzz$id = factor(buzz$id,
                 levels=c("Thora: 2014","Frida: 2015","Eistla: 2016"),
                 ordered = T)


# summary per whale
#-------------------
buzz$drop = as.numeric(as.character(buzz$drop))
sum = buzz %>% 
  group_by(id) %>%
  summarize(nbuzz  = sum(buzz),
            ndrops = sum(drop),
            start  = first(dateTime),
            end    = last(dateTime)) %>%
  mutate(duration = difftime(end, start, units="hours"))
sum

dive = buzz %>% 
  group_by(id) %>%
  summarise(ndive  = max(dive),
            nBuzz    = sum(buzz[buzz==1]),
            nDrop    = sum(drop[drop==1]))
dive


# data frames with labels for each facet
#-----------------------------------------
buzz = buzz %>%
  mutate(drop2 = case_when(type=="start" & !is.na(type) ~ "drop",
                          TRUE ~ "non-foraging")) 

# duration/ID
dur <- data.frame("id" = c("Thora: 2014","Frida: 2015","Eistla: 2016"), 
                  x = as.POSIXct(strptime(c("2014-08-15","2015-08-18","2016-08-26"),
                                          format="%Y-%m-%d"), tz="GMT"),
                  y = rep(740, times=3),
                  label = c("duration: 91 h","duration: 56 h","duration: 34 h"))
dur$id = factor(dur$id,
                levels=c("Thora: 2014","Frida: 2015","Eistla: 2016"))

# number of dives/ID
dive_lab <- data.frame("id" = c("Thora: 2014","Frida: 2015","Eistla: 2016"), 
                       x = as.POSIXct(strptime(c("2014-08-15","2015-08-18","2016-08-26"),
                                               format="%Y-%m-%d"), tz="GMT"),
                       y = rep(780, times=3),
                       label = c("n dives: 1,609", "n dives: 1,016", 
                                 "n dives: 392"))
dive_lab$id = factor(dive_lab$id,
                     levels=c("Thora: 2014","Frida: 2015","Eistla: 2016"))

# number of buzzes/ID
buzz_lab <- data.frame("id" = c("Thora: 2014","Frida: 2015","Eistla: 2016"), 
                       x = as.POSIXct(strptime(c("2014-08-15","2015-08-18","2016-08-26"),
                                               format="%Y-%m-%d"), tz="GMT"),
                       y = rep(820, times=3),
                       label = c("n buzzes: 4,193", "n buzzes: 887", "n buzzes: 907"))
buzz_lab$id = factor(buzz_lab$id,
                     levels=c("Thora: 2014","Frida: 2015","Eistla: 2016"))

# number of drops/ID
drop <- data.frame("id" = c("Thora: 2014","Frida: 2015","Eistla: 2016"), 
                   x = as.POSIXct(strptime(c("2014-08-15","2015-08-18","2016-08-26"),
                                           format="%Y-%m-%d"), tz="GMT"),
                   y = rep(860, times=3),
                   label = c("n drops: 21", "n drops: 75", "n drops: 5"))
drop$id = factor(drop$id,
                 levels=c("Thora: 2014","Frida: 2015","Eistla: 2016"))


# mean depth at drop/ID
mean_drop = buzz %>%
  group_by(id) %>%
  summarize(mean = mean(depth[drop==1], na.rm=T))
line <- data.frame("id" = c("Thora: 2014","Frida: 2015","Eistla: 2016"), 
                   y = c(mean_drop$mean))
line$id = factor(line$id,levels=c("Thora: 2014","Frida: 2015","Eistla: 2016"))

buzz2 = buzz %>% filter(dive_type != "non-foraging")
table(buzz2$dive_type)
unique(buzz$dive_type)

# plot panel a
#-----------------
a = ggplot(buzz, aes(y=depth, x=dateTime)) +
  geom_path(size=0.2, colour="gray64") + 
  geom_point(size=0.3, stroke=0.1, data=buzz2,
             aes(y=depth, x=dateTime,colour=dive_type)) +
  geom_point(size=0.3, stroke=0.1, 
             data=buzz2[buzz2$dive_type=="buzz",], 
             aes(y=depth, x=dateTime),colour="black") +
  geom_point(size=0.5, stroke=0.1, 
             data=buzz2[buzz2$dive_type=="drop",], 
             aes(y=depth, x=dateTime),colour="red") +
  scale_colour_manual(labels = c("buzz","drop"),
                      values = c("black","red")) +
  facet_wrap(.~id, scales="free_x") +
  scale_y_continuous(trans = "reverse", limits=c(870,0)) +
  geom_text(data = dur, aes(x = x, y = y, label = label),
            size=1.7,colour="gray64") +
  geom_text(data = dive_lab, aes(x = x, y = y, label = label),
            size=1.7, colour="gray64") +
  geom_text(data = buzz_lab, aes(x = x, y = y, label = label),
            colour="black",size=1.7) +
  geom_text(data = drop, aes(x = x, y = y, label = label),
            colour="red",size=1.7) +
  labs(x = "", y = "Dive depth (m)", colour="",title="A)") +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%d-%b") +
  geom_hline(data = line, aes(yintercept = y), lty=2, 
             lwd=0.2, colour="red") +
  theme(panel.spacing = unit(0, "lines"),
        legend.position = c(0.4,0.4),
        legend.key.size = unit(5,"cm"),
        legend.key.width = unit(0.1,"cm"),
        legend.key.height = unit(0.1,"cm"),
        legend.title = element_text(size=5),
        legend.text = element_text(size=5),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.key = element_blank(),
        strip.background = element_rect(fill="steelblue4",size=0.2,
                                        colour="white"),
        strip.text = element_text(colour='white',size=8,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=7, hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        axis.text.x  = element_text(size=5, hjust=0.5, vjust=0.5, angle=0),
        axis.text.y  = element_text(size=5, hjust=1),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0.2,0.3,0,0.2),"cm")) +
  guides(color = guide_legend(override.aes = list(size = 1) ) )





#-----------------------------------------------------
# b) zoom in some of the dives with buzzes and drops
#-----------------------------------------------------

#-----------------------------
# Eistla: buzz+drop at depth
#-----------------------------
e = buzz %>% filter(id == "Eistla: 2016")
st = e[e$dateTime2>="2016-08-26 14:20:00" 
       & e$dateTime2<"2016-08-26 14:45:00",]
st = st %>% 
  mutate(dive = NA,
         dive = case_when(type=="start" & !is.na(type) ~"drop",
                        type=="end" & !is.na(type) ~"feeding end",
                        buzz == 1 ~ "buzz"),
         id2  = "Eistla: 2016-08-26") 
table(st$dive)
st$dive = factor(st$dive, levels=c("buzz","drop","feeding end"))
e <- st[,c("id","id2","dateTime","depth","st_fit","dive","buzz","event")]



#----------------------
# Frida: surface drop
#----------------------
id = buzz %>% filter(id == "Frida: 2015")
id = id[order(id$dateTime),]
st = setDT(id)[dateTime %between% c("2015-08-17 01:20:00",
                                    "2015-08-17 02:00:00")] 
st = data.frame(st)
st = st %>% 
  mutate(dive = NA,
         dive = case_when(type=="start" & !is.na(type) ~"drop",
                          type=="end" & !is.na(type) ~"feeding end",
                          buzz == 1 ~ "buzz"),
         id2  = "Frida: 2015-08-17") 

st$dive = factor(st$dive, levels=c("buzz","drop","feeding end"))
f <- st[,c("id","id2","dateTime","depth","st_fit","dive","buzz","event")]
tail(f[!is.na(f$dive),])
unique(f$event)
tail(f[f$event==4,])  # to retain only 1 event for this dive (visual purpose)
unique(f$dive[f$event==5])
f$dive[f$event==5]  = "NA"
f$event[f$event==5] = "NA"
f = as_tibble(f)

#----------------
# Thora 12/08
#----------------
id = buzz %>% filter(id == "Thora: 2014")
id = id[order(id$dateTime),]
st = setDT(id)[dateTime %between% c("2014-08-12 08:55:00",
                                    "2014-08-12 09:30:00")] 
st = data.frame(st)

st = st %>% 
  mutate(dive = NA,
         dive = case_when(type=="start" & !is.na(type) ~"drop",
                          type=="end" & !is.na(type) ~"feeding end",
                          buzz == 1 ~ "buzz"),
         id2  = "Thora: 2014-08-12") 

table(st$dive)
st$dive = factor(st$dive, levels=c("buzz","drop","feeding end"))
st[st$dive=="feeding end" & !is.na(st$dive),]
st$dive[st$dive=="feeding end" & !is.na(st$dive) & st$depth>5] = NA
t <- st[,c("id","id2","dateTime","depth","st_fit","dive","buzz","event")]
t = as_tibble(t)

#--------------------
dat      = rbind(e,f,t)
dat$dive = as.character(dat$dive)
dat$id2  = factor(dat$id2, levels=c("Thora: 2014-08-12",
                                    "Frida: 2015-08-17",
                                    "Eistla: 2016-08-26"))

# create data for event window (rectangle: btw start and end)
#------------------------------------------------------------
rect_data <- dat %>% 
  filter(!is.na(dive) & dive != "buzz") %>% 
  dplyr::select(-c(depth,st_fit,buzz)) %>% 
  pivot_wider(id_cols = c("id2","event"),
              names_from = "dive",
              values_from = "dateTime")
rect_data
rect_data$id2 = factor(rect_data$id2, levels=c("Thora: 2014-08-12",
                                               "Frida: 2015-08-17",
                                               "Eistla: 2016-08-26"))
rect_data$recovery = as.numeric(difftime(rect_data$`feeding end`,
                                         rect_data$drop, units="mins"))
rect_data$depth = c(296,8.6,482)

dat$dive[dat$dive=="feeding end"] = NA
unique(dat$dive)
dat$dive = as.factor(dat$dive)

# add recovery time and depth on facet
#---------------------------------------
dat_text <- data.frame(
  label = c("Depth: 482 m",
            "Depth: 8.6 m", 
            "Depth: 296 m"),
  id2   = c("Thora: 2014-08-12",
            "Frida: 2015-08-17",
            "Eistla: 2016-08-26"),
  dateTime = c("2014-08-12 09:09:00",
               "2015-08-17 01:27:00",
               "2016-08-26 14:32:00"),
  y = c(520, 70, 350))
dat_text = as_tibble(dat_text)
dat_text$dateTime = as.POSIXct(strptime(dat_text$dateTime,
                                        format="%Y-%m-%d %H:%M:%S"), tz="GMT")
dat_text$id2 = factor(dat_text$id2, levels=c("Thora: 2014-08-12",
                                             "Frida: 2015-08-17",
                                             "Eistla: 2016-08-26"))

# dive profile
#-----------------------
b = ggplot(dat, aes(dateTime, depth)) +
  geom_rect(data = rect_data,
            aes(xmin = `drop`,
                ymin = 0,
                xmax = `feeding end`,
                ymax = Inf),
            fill = "cadetblue3", inherit.aes = F) +
  geom_text(
    data    = dat_text,
    mapping = aes(x = dateTime, y = y, label = label),
    hjust   = -0.1, vjust = -1, size=2, colour="red") +
  geom_path(size=0.2) + 
  geom_point(size=0.4, data=dat[!is.na(dat$dive),],
             aes(colour=dive)) +
  geom_point(size=0.4, data=dat[!is.na(dat$dive) 
                                & dat$dive=="buzz",],
             colour="black") +
  geom_point(size=0.4, data=dat[!is.na(dat$dive) 
                                & dat$dive=="drop",],
             colour="red") +
  scale_colour_manual(values = c("black","red")) +
  scale_y_continuous(trans = "reverse",limits=c(550,0)) +
  labs(x="", y="Dive depth (m)",title="B)") + # Zoomed time-series data
  facet_wrap(.~id2, scales="free_x") +
  theme(panel.spacing = unit(0, "lines"),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        # text = element_text(family = "Arial"),
        strip.background = element_rect(fill="steelblue4",size=0.2,
                                        colour="white"),
        strip.text = element_text(colour='white',size=8,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        legend.position = c(0.2,0.5),
        legend.key.size = unit(5,"cm"),
        legend.key.width = unit(0.2,"cm"),
        legend.key.height = unit(0.2,"cm"),
        legend.title = element_blank(),
        legend.text = element_text(size=6),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.key = element_blank(),
        legend.background=element_rect(fill = NA),
        axis.title = element_text(size=7, hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        axis.text.y  = element_text(size=5, hjust=1),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 0.5) ) )
b



#--------------
# ST profile
#---------------

# add recovery time and depth on facet
#---------------------------------------
dat_text <- data.frame(
  label = c("Recovery: 18 min",
            "Recovery: 29 min", 
            "Recovery: 16 min"),
  id2   = c("Thora: 2014-08-12",
            "Frida: 2015-08-17",
            "Eistla: 2016-08-26"),
  dateTime = c("2014-08-12 09:08:00",
               "2015-08-17 01:28:00",
               "2016-08-26 14:28:00"),
  y = c(31,31,31))
dat_text = as_tibble(dat_text)
dat_text$dateTime = as.POSIXct(strptime(dat_text$dateTime,
                                        format="%Y-%m-%d %H:%M:%S"), tz="GMT")
dat_text$id2 = factor(dat_text$id2, levels=c("Thora: 2014-08-12",
                                             "Frida: 2015-08-17",
                                             "Eistla: 2016-08-26"))

# ST profile
#-----------------------
c = ggplot(dat, aes(dateTime, st_fit)) +
  geom_rect(data = rect_data,
            aes(xmin = `drop`,
                ymin = -Inf,
                xmax = `feeding end`,
                ymax = Inf), 
            fill = "cadetblue3", inherit.aes = F) +
  geom_text(
    data    = dat_text,
    mapping = aes(x = dateTime, y = y, label = label),
    hjust   = -0.1, vjust = -1, size=2, colour="red") +
  geom_path(size=0.2) + 
  geom_point(size=0.4, data=dat[!is.na(dat$dive) 
                                & dat$dive=="drop",],
             colour="red") +
  ylim(30,37) +
  facet_wrap(.~id2, scales="free_x") +
  labs(x="", y="Stomach temperature (°C)") +
  scale_colour_manual(values = c("black","red","orange")) +
  scale_x_datetime(date_breaks = "10 mins", date_labels = "%H:%M") +
  theme(panel.spacing = unit(0, "lines"),
        strip.background.x = element_blank(),
        strip.text.x = element_blank(),
        # text = element_text(family = "Arial"),
        axis.title = element_text(size=6, hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        axis.text.x  = element_text(size=5, hjust=0.5, vjust=0.5, angle=0),
        axis.text.y  = element_text(size=5, hjust=1),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=7, vjust=0, hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank())
c









#######################################
# d) density depth vs duration
#######################################
acou2 <- readRDS("./Rdata/Acousonde/Buzz_Drops/calib_2m_zoc0/Summary_dive_drops_buzzes_3ids_calib_2m_zoc0.RDS")
acou2$ptt = factor(acou2$ptt, levels=c("Thora","Frida","Eistla"))
acou2 = acou2 %>% 
  dplyr::select(ptt, nbuzz, ndrop, meandep_buzz, dep_drop, dur)
names(acou2)

# First pivot
test_1 = reshape2::melt(acou2, 
                        # id column are ptt, meandep_buzz, dep_drop, dur
                        id = c(1, 4, 5, 6), 
                        # values are ndrop and nbuzz
                        measure = c(2, 3),
                        # name of the new column
                        value.name = "z")
head(test_1)

# Second pivot
test_2 = reshape2::melt(test_1,
                        # id column are ptt, dur, z
                        id = c(1, 4, 6),
                        # values are meandep_buzz and dep_drop
                        measure = c(2, 3),
                        # name of the new column
                        value.name = "y")
head(test_2)
unique(test_2$variable)
levels(test_2$variable) = c("Buzz","Drop")


# plot panel d
#----------------
d = ggplot(test_2, aes(y = y, x = dur, z = z)) +
  geom_density_2d_filled(contour_var = "ndensity", bins = 8) + 
  scale_fill_brewer(palette = 3, direction = 1) + # 3,9,12
  facet_grid(variable ~ ptt) + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(trans = "reverse",limits = c(850, 0),
                     expand = c(0, 0)) +
  labs(x = "Dive duration (min)", y = "Dive depth (m)",
       fill="Probability", title = "") +
  theme(panel.spacing = unit(0, "lines"),
        panel.border = element_rect(color = "white", fill = NA, 
                                    size = 0.2), 
        strip.background = element_rect(fill="steelblue4",size=0.2,
                                        colour="white"),
        strip.text = element_text(colour='white',size=8,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=6, hjust=0.5),
        legend.position = c(.5,.12),
        legend.key.size = unit(0.4,"line"),
        legend.key.width = unit(0.2,"cm"),
        legend.key.height = unit(0.2,"cm"),
        legend.title = element_text(size=5),
        legend.text = element_text(size=4),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.key = element_blank(),
        axis.text  = element_text(size=4, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0,0,0,0.3),"cm"))  # t, r, b, l









##############################
# export plot with all panels
##############################
library(patchwork)

((((a + theme(plot.margin = unit(c(2,0,2,-2), "pt"))) /plot_spacer()/
     (b + theme(plot.margin = unit(c(0,0,0,-2), "pt"))) /plot_spacer()/
     (c + theme(plot.margin = unit(c(2,0,-5,-2), "pt")))) + # t, r, b, l
    plot_layout(heights = c(7,-1.4,
                            5,-1.55,3.5))) | d) +
  plot_layout(widths = c(3,2)) 
ggsave(filename=paste0("./PAPER/5.BioLetters/Fig1.pdf"),
       width=6.3,height=4.5,units="in",dpi=400,family="ArialMT")


















#######################################
# Fig. 2: density depth vs duration
#######################################
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/MSCA-GF/ANALYSES")
acou2 <- readRDS(paste0("./Rdata/Acousonde/Buzz_Drops/calib_2m_zoc0/",
                       "Summary_dive_drops_buzzes_3ids_calib_2m_zoc0.RDS"))
acou2$ptt = factor(acou2$ptt, levels=c("Thora","Frida","Eistla"))
acou2 = acou2 %>% 
  dplyr::select(ptt, nbuzz, ndrop, meandep_buzz, dep_drop, dur)
names(acou2)

# First pivot
test_1 = melt(acou2, 
              # id column are ptt, meandep_buzz, dep_drop, dur
              id = c(1, 4, 5, 6), 
              # values are ndrop and nbuzz
              measure = c(2, 3),
              # name of the new column
              value.name = "z")
head(test_1)

# Second pivot
test_2 = melt(test_1,
              # id column are ptt, dur, z
              id = c(1, 4, 6),
              # values are meandep_buzz and dep_drop
              measure = c(2, 3),
              # name of the new column
              value.name = "y")
head(test_2)
unique(test_2$variable)
levels(test_2$variable) = c("Buzz","Drop")

ggplot(test_2, aes(y = y, x = dur, z = z)) +
  geom_density_2d_filled(contour_var = "ndensity", bins = 10) + 
  facet_grid(variable ~ ptt) + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(trans = "reverse",limits = c(850, 0),
                     expand = c(0, 0)) +
  labs(x = "Dive duration (min)", y = "Dive depth (m)",
       fill="Probability") +
  theme(panel.spacing = unit(0, "lines"),
        panel.border = element_rect(color = "white", fill = NA, size = 0.2), 
        strip.background = element_rect(fill="steelblue4",size=0.2,
                                        colour="white"),
        strip.text = element_text(colour='white',size=10,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=8, hjust=0.5),
        legend.position = "right",
        legend.key.size = unit(0.4,"line"),
        legend.key.width = unit(0.2,"cm"),
        legend.key.height = unit(0.2,"cm"),
        legend.title = element_text(size=6),
        legend.text = element_text(size=5),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.key = element_blank(),
        axis.text  = element_text(size=7, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0.2,0.3,0.4,0.2),"cm")) 

ggsave(filename=paste0("./PAPER/Fig2.pdf"),
       width=4.5,height=5,units="in",dpi=400,family="ArialMT")


# only very few drops below 10 m for Frida
# so not visible using the geom_density_2d_filled function
#---------------------------------------------------------
hist(acou2$dep_drop[acou2$ptt=="Frida"])
frida = acou2[acou2$ptt=="Frida",]
summary(frida$dep_drop)
nrow(frida[frida$dep_drop>10 & !is.na(frida$dep_drop),]) / nrow(frida) *100 # 4.5%

















###############################################
# Fig. 3: histo number of drops and buzzes/ID
###############################################

#------------------------------------------
# a) nber of drops/day/id
#------------------------------------------
table <- readRDS("./Rdata/TableS1_6ids.rds")
table
str(table)
table$daily_drops  = as.numeric(table$daily_drops)
table$daily_buzzes = as.numeric(table$daily_buzzes)

a = ggplot(table, aes(x=reorder(id,daily_drops), 
                      y=daily_drops, fill=tag_type)) + 
  geom_bar(stat="identity",position=position_dodge(),
           width=0.7) +
  labs(x = "Individuals", y = "ST drops/day", 
       title="A) All tags", fill="") + 
  geom_text(data=table,aes(x=id, y=daily_drops+0.3, colour=tag_type,
                           label=daily_drops),vjust=0,size=1.4) +
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2",guide = "none") +
  theme(panel.spacing = unit(0, "lines"),
        panel.border = element_rect(color = "white", fill = NA, size = 0.2), 
        strip.background = element_rect(fill="steelblue4",size=0.2,
                                        colour="white"),
        strip.text = element_text(colour='white',size=10,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=7, hjust=0.5),
        legend.position = c(0.2,0.9),
        legend.key.size = unit(0.4,"line"),
        legend.key.width = unit(0.2,"cm"),
        legend.key.height = unit(0.2,"cm"),
        legend.title = element_text(size=6),
        legend.text = element_text(size=6),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.key = element_blank(),
        axis.text  = element_text(size=6, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, colour="black"),
        axis.line.y.left = element_line(size=0.2),
        axis.line.x.bottom = element_line(size=0.2),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0.2,0.3,0.4,0.2),"cm")) 



#------------------------------------------
# b) nber of events (buzz or drop)/day
#------------------------------------------
pivot = table %>%
  filter(tag_type == "STP+Acou") %>%
  pivot_longer(c(daily_drops,daily_buzzes), 
               names_to = "event", values_to = "value") %>%
  dplyr::select(id,tag_type,event,value) %>% 
  mutate(event = factor(case_when(event == "daily_drops" ~ "drop",
                                  event == "daily_buzzes" ~ "buzz"),
                        levels=c("buzz","drop"))) %>%
  group_by(event) %>% 
  summarise(mean = mean(value),
            sd   = sd(value))
pivot

b = ggplot(pivot, aes(x=event, y=mean)) + 
  geom_bar(stat="identity",position=position_dodge(.9),
           width=0.35, aes(fill=event), colour=NA, lwd=0.2) +
  scale_fill_manual(values = c("gray36","red")) +
  geom_text(data=pivot,aes(x=event, y=mean+10, colour=event,
                           label=format(round(mean,3), nsmall = 1)),
            vjust=0,size=1.4) +
  scale_colour_manual(values = c("gray36","red"),guide = "none") +
  labs(x = "Type of event", y = "n events/day", 
       title="B) STP + Acousonde", fill="") + # Frequency of buzzes vs. ST drops
  theme(panel.spacing = unit(0, "lines"),
        panel.border = element_rect(color = "white", fill = NA, size = 0.2), 
        strip.background = element_rect(fill="steelblue4",size=0.2,
                                        colour="white"),
        strip.text = element_text(colour='white',size=7,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=7, hjust=0.5),
        legend.position = "none",
        axis.text  = element_text(size=6, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, hjust=0, colour="black"),
        axis.line.y.left = element_line(size=0.2),
        axis.line.x.bottom = element_line(size=0.2),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0.2,0.3,0.4,0.2),"cm"))

ggsave(filename=paste0("./PAPER/Fig3.pdf"),
       width=5,height=2.4,units="in",dpi=400,family="ArialMT",
       grid.arrange(a,b,ncol=2)) 














#####################################################
# Fig. 2: barplot nber of successful dives and GAM
#####################################################

#------------------------------------------
# panel b :
# proportion of foraging vs feeding dives
#------------------------------------------
library(RColorBrewer)
prop <- readRDS("./prop_successful_dives_3ids_calib_2m_zoc0.RDS")
pivot =  prop %>%
  pivot_longer(c(foragingPC,non_foragingPC), 
               names_to = "type", values_to = "prop") %>%
  mutate(type = factor(case_when(type == "non_foragingPC" ~ "non-foraging",
                                 type == "foragingPC" ~ "foraging"),
                       levels=c("foraging","non-foraging")),
         ptt = factor(ptt, levels=c("Thora","Frida","Eistla")),
         pc = prop/100)

a = ggplot(pivot, aes(x = ptt, y = prop, fill = type)) +
  geom_bar(stat = "identity", position = "fill", color="grey30",
           lwd=0.1, width=0.7) +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_manual(values = c("royalblue4","azure2")) +
  labs(x = "Individuals", y = "Proportion of dives",
       fill="Dive type", title = "A) All dives") +
  annotate(geom="text", x=1, y=0.5, label="75%",size=1.4) +  # Thora
  annotate(geom="text", x=2, y=0.5, label="80%",size=1.4) +  # Frida
  annotate(geom="text", x=3, y=0.5, label="80%",size=1.4) +  # Eistla
  annotate(geom="text", x=1, y=0.95, label="25%",
           size=1.4, colour="white") + # Thora
  annotate(geom="text", x=2, y=0.95, label="20%",
           size=1.4, colour="white") + # Frida
  annotate(geom="text", x=3, y=0.95, label="20%",
           size=1.4, colour="white") + # Eistla
  theme(axis.title = element_text(size=6, hjust=0.5),
        legend.position= "bottom",
        legend.key.size = unit(3,"cm"),
        legend.key.width = unit(0.2,"cm"),
        legend.key.height = unit(0.2,"cm"),
        legend.title = element_blank(),
        legend.text = element_text(size=5),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.key = element_blank(),
        legend.background=element_rect(fill = NA),
        panel.border = element_blank(),
        axis.ticks.length = unit(0.02,"inch"),
        axis.line.x.bottom = element_line(size=0.2),
        axis.line.y.left = element_line(size=0.2),
        axis.text    = element_text(size=6, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=8, vjust=0, hjust=0, colour="black"),
        panel.background = element_blank(),
        plot.margin = unit(c(0.6,0.2,0.4,0.3),"cm"), # t, r, b, l
        panel.grid.major = element_blank()) 




#---------------------------------------------------
# panel b: proportion of successful vs unsuccessful
#----------------------------------------------------
pivot =  prop %>%
  pivot_longer(c(successfulPC,unsuccessfulPC), 
               names_to = "type", values_to = "prop") %>%
  mutate(type = factor(case_when(type == "successfulPC" ~ "successful",
                                 type == "unsuccessfulPC" ~ "unsuccessful"),
                       levels=c("successful","unsuccessful")),
         ptt = factor(ptt, levels=c("Thora","Frida","Eistla")),
         pc = prop/100)

b = ggplot(pivot, aes(x = ptt, y = prop, fill = type)) +
  geom_bar(stat = "identity", position = "fill", color="grey30",
           lwd=0.1, width=0.7) +
  scale_y_continuous(labels = label_percent(),guide = guide_axis(position = "right")) +
  scale_fill_manual(values = c("black","grey")) +
  labs(x = "Individuals", y = "Proportion of foraging dives",
       fill="Dive type", title = "B) Foraging dives") +
  annotate(geom="text", x=1, y=0.5, label="92%",size=1.4) +  # Thora
  annotate(geom="text", x=2, y=0.5, label="86%",size=1.4) +  # Frida
  annotate(geom="text", x=3, y=0.5, label="91%",size=1.4) +  # Eistla
  annotate(geom="text", x=1, y=0.95, label="8%",
           size=1.4, colour="white") + # Thora
  annotate(geom="text", x=2, y=0.95, label="14%",
           size=1.4, colour="white") + # Frida
  annotate(geom="text", x=3, y=0.95, label="9%",
           size=1.4, colour="white") + # Eistla
  theme(axis.title = element_text(size=6, hjust=0.5),
        legend.position = "bottom",
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
        panel.border = element_blank(),
        axis.ticks.length = unit(0.02,"inch"),
        axis.line.x.bottom = element_line(size=0.2),
        axis.line.y.right = element_line(size=0.2),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title = element_text(size=8, vjust=0, hjust=0, colour="black"),
        axis.text    = element_text(size=6, hjust=0.5),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0.6,0.2,0.4,0.3),"cm")) # t, r, b, l


#--------------------------------
# panel c: GAM output
#--------------------------------
ind_pred <- readRDS("./Rdata/GAM_ndrop/ind_pred_GAM_nbuzz_slope_intercept.rds")
pop_pred <- readRDS("./Rdata/GAM_ndrop/pop_pred_GAM_nbuzz_slope_intercept.rds")
ind_pred$ptt = factor(ind_pred$ptt, levels=c("Thora","Frida","Eistla"))

c  = ggplot(ind_pred, aes(x = nbuzz, y = fit_ind)) +
  geom_line(aes(colour=ptt),lwd=0.3) +
  geom_ribbon(aes(ymin=fit_ind-se_ind, 
                  ymax=fit_ind+se_ind, fill=ptt), alpha=0.2) +
  scale_colour_manual(values = c("blue","brown3","darkgreen")) +
  scale_fill_manual(values = c("blue","brown3","darkgreen")) +
  labs(y = "ST Drop probability", x = "Number of buzzes per dive", 
       title="C) GAM output") +
  theme(legend.position = "bottom",
        legend.key.size = unit(3,"cm"),
        legend.key.width = unit(0.2,"cm"),
        legend.key.height = unit(0.2,"cm"),
        legend.title = element_blank(),
        legend.text = element_text(size=5),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.key = element_blank(),
        legend.background=element_rect(fill = NA),
        axis.title = element_text(size=6, hjust=0.5),
        panel.border = element_blank(),
        axis.ticks.length = unit(0.02,"inch"),
        axis.line.x.bottom = element_line(size=0.2),
        axis.line.y.left = element_line(size=0.2),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title = element_text(size=8, vjust=0, hjust=0, colour="black"),
        axis.text    = element_text(size=6, hjust=0.5),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0.6,0.2,0.4,0.3),"cm")) # t, r, b, l



grid.arrange(a, b, c, ncol=3)

ggsave(filename=paste0("./FIGS/Fig2.pdf"),
       width=5.8,height=2.5,units="in",dpi=400,family="ArialMT",
       grid.arrange(a, b, c,ncol=3)) # width=5,height=2


