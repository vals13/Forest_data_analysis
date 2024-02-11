library(readxl)
library(readr)
install.packages("dplyr") 
library(dplyr)
install.packages("plyr") 
library(plyr) 
install.packages("tidyr") 
library(tidyr) 
install.packages("stringr") 
library("stringr") 
install.packages("rBDAT") 
library("rBDAT") 
install.packages("rBDATPro") 
library("ggplot2") 
library("ggpubr")
library("psych")
library("forcats")

trees <- read_excel("terreno_woodify_R_trees.xlsx")
trees <- transform(trees, h = as.numeric(h))
trees

RStudio.Version()

#apply mean of h of plot to the NA
trees <- trees %>% group_by (plot) %>% 
  mutate_at(vars(h), ~replace_na(., mean(., na.rm = TRUE)))
trees <- transform(trees, h = as.numeric(h))
aggregate(count ~ specie + plot, rejuv_sp, sum)

aggregate(cbind(DBH) ~ Specie, trees, mean)
describe(trees)

trees$Specie <- gsub("tilia cordata", "Tilia cordata",trees$Specie)
trees$Specie <- gsub("\\?", "Unknown",trees$Specie)
aggregate(cbind(DBH) ~ Specie, trees, mean)

#round values to 3 decimals
trees$h <- format(round(trees$h , 3), nsmall = 3)

# get the mean DBH of trees 
summary(trees)
# make a excel to take information in excel with a pivot table (n of trees per plot)
## add areas depending on plot (ha)
trees$area <- rep(c(0.006), rep = 549)

trees <- trees %>% 
  plyr::mutate(area = ifelse(
    str_detect(trees$plot, "vip"), 0.031416, area))

mean_DBH_tees <- aggregate(cbind(DBH) ~ Specie, trees, mean) 
#apply cone volume (m3) formula to then use BEF (V = r^2 * h * pi/3)
trees$volum <- (function(a, b) {(a/200)^2 * b * pi/3}) (trees$DBH, trees$h)
 # in m3

#add volumes
trees_p <- aggregate(volum ~ Specie + plot, 
                   trees, sum, na.action = na.omit)

## species density and carbon content
#spruce = picea = 0.38 (g/m3) & 0.501 (g C/ g dry mass)
# pine 0.43 (g/m3) & 0.51 (g C/ g dry mass)
# beech = fagus= 0.56 (g/m3) & 0.486 (g C/ g dry mass)
# oak = quercus = 0.57 (g/m3) & 0.495 (g C/ g dry mass)
# birch = betula y carpinus = 0.57 (g/m3) & 0.495 (g C/ g dry mass)
# other conifers 0.37 (g/m3) & 0.51 (g C/ g dry mass)
# other broadleaf =  0.49 (g/m3) & 0.49 (g C/ g dry mass)

aggregate(cbind(volum) ~ Specie, trees, sum)

# wood density tn in m3 (http://db.worldagroforestry.org//wd/genus/sorbus)
specie <- read.csv2("trees_species_den_Cf.csv")
colnames(specie)
specie <- specie %>% select(-ref.density, -ref.Cfraction)
colnames(specie)[2] <- "wd"

trees_p$wd <- specie$wd[match(trees_p$Specie, specie$Specie)]
trees_p$Cfraction <- specie$Cfraction[match(trees_p$Specie, specie$Specie)]

# save doc for 30 year estimations and for table of data for each sp?
write.csv(trees_p, "/Users/vale/Documents/Wodify/Field work/Woodify/trees_30y_estimation.csv")

# calculate AGB volume (m3) * wood density (kg/m3) * BEF
AGB <- trees_p$volum * trees_p$wd / 1000 # (/1000) transformed to tn?

#BEF pines or spruce 1.3  (pine and picea) and for broadleaf 1.4 (all the rest of species)
AGB <- ifelse (trees_p$Specie == "Pinus sylvestris" | trees_p$Specie == "Picea abies", 
               AGB * 1.3, 
               AGB * 1.4)
trees_p <- cbind (trees_p, AGB ) # 

## add areas depending on plot (ha)
trees_p$area <- rep(c(0.006), rep = 77)

trees_p <- trees_p %>% 
  dplyr::mutate(area = ifelse(
    str_detect(trees_p$plot, "vip"), 0.031416, area))

# to estimate carbon content AGB * Cfraction / area to make it carbon stock
trees_p$Cstock <- (AGB * trees_p$Cfraction) #tn of carbon
trees_p$C_tn_ha <- (AGB * trees_p$Cfraction)/trees_p$area # ton
trees_p$vol_ha <- trees_p$volum/trees_p$area
trees_p$AGB_ha <- trees_p$AGB/trees_p$area

#edit to combine with all the rest for nadine (data frame = df_t)
t_estim_p <- trees_p %>% select(-4,-5,-7)
t_estim_p$type <- rep(c("live trees"), rep = 77)
colnames(t_estim_p) #to work in QGIS at some point

df_t <- aggregate(cbind(AGB, Cstock, C_tn_ha, vol_ha) ~ plot, trees_p, sum) #for nadine
df_t$type <- rep(c("live trees"), rep = 32)

# dont know what this is (25.08.23)

g2 <- ggplot_gtable(ggplot_build(A))
g2$layout$clip[g2$layout$name == "panel"] <- "off"
grid.draw(g2)


#share of quercus of biomas?, for report
summary(trees_p)
 (trees_p)
a <-aggregate(cbind(AGB_ha, AGB)  ~  Specie, trees_p, mean) 

summary(a) 

aggregate(volum  ~  Specie, trees_p, mean) 

# add with GPS and Shape # 
gps <- read.csv("terreno_woodify_GPS.xlsx - GPS.csv")
gps <- gps[,-1]

df_test <- list(df_t, gps)
df_test <- Reduce(function (x, y) merge (x, y, all =TRUE), df_test)

gps$plot <- str_replace(gps$plot, "cs245", "cs245vip")
rm(df_test)

#merge df_trees with atribute..
# charge shape file and merge
shape <- read.csv("deadwood_biomass_s - deadwood_biomass_s.csv")

#merge trees_p with gps and then by tree sp too 
#(to have a future shape file with at species information not plot only)
df_sp_t <- t_estim_p

df_sp_t$Este..X. <- gps$Este..X.[match(df_sp_t$plot, gps$plot)]
df_sp_t$Norte..Y. <- gps$Norte..Y.[match(df_sp_t$plot, gps$plot)]
df_sp_t$alt <-gps$alt[match(df_sp_t$plot, gps$plot)]

t_nad <- df_t
t_nad$ID_polygons <- shape$ID_polygons[match(t_nad$plot, shape$plot)] ## para combinar con rejuv

df_sp_t$ID_polygons <- shape$ID_polygons[match(df_sp_t$plot, shape$plot)]
write.csv(df_sp_t, "/Users/vale/Documents/Wodify/Field work/Woodify/trees_data_by_species.csv")

### Graphs from TREES INDICATOR ###
write.csv(trees, "trees_TdL.csv")

## plot the DBH variation within plot
install.packages("ggpubr") 
## choose dif palet color
pal18 <- c("#CCCCCC", "#ccff66", "#E69F00","#FFCCCC", "#56B4E9", 
           "#D55E00",  "#00A600", "#9999CC",
             "#000099", "#336600", "#CC79A7", "#ff6666", 
           "#F0E442", "#009E73", "#6600cc",  "#990000", 
             "#999900", "#996633","#99FF99", "#330000")
library(tidyverse)
install.packages("cowplot")
library(ggthemes)
library(cowplot)
library(grid)


p <- ggplot(trees, aes (x= reorder(plot, DBH, mean), y= DBH))+
  labs (x = "Plots", y = "DBH (cm)") + 
  geom_boxplot(fill = "#C1CDC1", show.legend = TRUE)+
  stat_summary(fun = "mean", geom = "point", shape = 1, colour ="#009966")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle =90))

p
s <- ggplot(trees, aes (x= reorder(Specie, DBH, mean), y= DBH))+
  geom_boxplot(fill = "#C1CDC1")+
  stat_summary(fun = "mean", geom = "point", shape = 1, colour ="#009966")+
  theme_minimal()+
  labs (x = "Species", y = "DBH (cm)")+
  theme(axis.text.x = element_text(angle =90), 
        axis.title.y = element_text(size=10, margin = margin(t=10)  ),
        legend.position = "right")
s
fig_1 <- ggarrange(p, s, labels = c("A", "B"), align = "h",
                   widths = c(2,1))
fig_1
mean_DBH_tees <- aggregate(cbind(DBH) ~ Specie, trees, mean)


## plot species precence in plots! show with bigger circules to show where there is more
library(RColorBrewer)
####### falta arreglar ##

trees$count <- rep(1, 549 )

# option 2 for chart bellow
geom_count(aes(color = ..n.., size = ..n..))

#count of tree species per plot
ggplot(trees, aes(x= fct_infreq(plot), y= fct_infreq(Specie)))+
  geom_count(aes(color = after_stat(n), size = after_stat(n)), show.legend = TRUE, position='identity')+
  guides(color = guide_legend(title = "Individuals\nCount"), size =guide_legend(title = "Individuals\nCount"))+ 
  theme_minimal()+ theme(axis.text.x = element_text(angle =90),
                         legend.position = c(0.9, 0.86),
                          legend.background = element_rect(fill="white"))+
  xlab("Plots") + ylab("Specie")

colnames(trees)

summary(trees_count)

fig_3 <- ggplot(trees_p, mapping = aes(fill= plot, x=Specie, y=AGB))+
  geom_bar(position='stack', stat='identity')+
  ylab("Biomass (tonnes)")+
  xlab("Species")+
  theme(axis.text.x = element_text(angle =90), 
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line (colour= "grey80"))+
  guides(fill=guide_legend(ncol= 1, bycol=TRUE, keyheight  = 0.6, keywidth = 0.5, bycol=TRUE))
fig_3

## modify table to have groups of vip plot and non-vip ones
trees_p$plot_type <- rep(c("transect"), rep = 77)

trees_p <- trees_p %>% 
  dplyr::mutate(plot_type = ifelse(
    str_detect(trees_p$plot, "vip"), "vip", plot_type))

#option 1
colnames(trees_p)
colnames(sp_trees)
colnames(rejuv_sp)
colnames(sp_trees)[1] <- "specie"
sp_trees <- aggregate(AGB_ha~ Specie, trees_p, sum)

sp_tdl <- join(sp_trees, rejuv_sp,
     type = "full")
sp_tdl <- aggregate(AGB_ha~ specie, sp_tdl, sum)
names(pal18) = sp_tdl$specie
pal18


###used one for report
a <- ggplot(trees_p, aes(x= reorder(plot, AGB_ha, FUN=sum), y=AGB_ha, fill= Specie))+
  geom_bar(position='stack', stat='identity')+
  ylab("Biomass (t/ha)")+
  xlab("Plots")+
  theme(axis.text.x = element_text(angle =90), 
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line (colour= "grey80"),
        legend.position = "bottom")+
  scale_fill_manual(values= pal18 )+
  guides(fill=guide_legend(bycol=TRUE, keyheight  = 0.6, keywidth = 0.5, bycol=TRUE))
a
b <- ggplot(trees_p, aes(x= reorder(plot, C_tn_ha, FUN=sum), y=C_tn_ha, fill= Specie))+
  geom_bar(position='stack', stat='identity')+
  ylab("Carbon stock (tC/ha)")+
  xlab("Plots")+
  theme(axis.text.x = element_text(angle =90), 
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line (colour= "grey80"),
        legend.position = "bottom")+
  scale_fill_manual(values=  pal18)+
  guides(fill=guide_legend(bycol=TRUE, keyheight  = 0.6, keywidth = 0.5, bycol=TRUE))

fig_3 <- ggarrange(a, b, labels = c("A", "B"), align = "v",
                   widths = c(1,1), common.legend = TRUE, legend="bottom",  ncol =2)
fig_3


Fig1 <- ggplot(trees_p, aes(x= reorder(plot, AGB_ha, FUN=sum), y=AGB_ha, fill= Specie))+
  geom_bar(position='stack', stat='identity')+
  ylab("Biomass (t/ha)")+
  xlab("Plots")+
  theme(axis.text.x = element_text(angle =90), 
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line (colour= "grey80"))+
  scale_fill_manual(values= c("#999999",  "#E69F00", "#56B4E9", "#ccff66",
                              "#EEB99F", "#9966ff", "#F0E442","#000099","#999900",
                              "#009E73","#ff6666", "#996633", "#CC79A7",
                              "#336600", "#990000"  ) )+
  guides(fill=guide_legend(ncol= 1, bycol=TRUE, keyheight  = 0.6, keywidth = 0.5, bycol=TRUE))
Fig1
#option 2
trees_vip <- filter(trees_p, str_detect(plot, "vip") )
trees_t <- filter(trees_p, str_detect(plot, "vip", negate = TRUE) )
vip <- ggplot(trees_vip, aes(x= reorder(plot, AGB_ha, FUN=sum), y=AGB_ha, fill= Specie))+
  geom_bar(position='stack', stat='identity')+
  ylab("Biomass (t/ha)")+
  xlab("VIP Plots")+
  theme(axis.text.x = element_text(angle =90), 
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line (colour= "grey80"))+
  scale_fill_manual(values= c("#999999",  "#E69F00", "#56B4E9", "#ccff66",
                              "#EEB99F", "#9966ff", "#F0E442","#000099","#009E73",
                              "#999900","#ff6666", "#996633", "#CC79A7",
                              "#336600", "#990000"  ) )+
  guides(fill=guide_legend(ncol= 1, bycol=TRUE, keyheight  = 0.6, keywidth = 0.5, bycol=TRUE))
vip

tra <- ggplot(trees_t, aes(x= reorder(plot, AGB_ha, FUN=sum), y=AGB_ha, fill= Specie))+
  geom_bar(position='stack', stat='identity')+
  ylab("Biomass (t/ha)")+
  xlab("Transects")+
  theme(axis.text.x = element_text(angle =90), 
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line (colour= "grey80"))+
  scale_fill_manual(values= c("#999999",  "#E69F00", "#9966ff", "#990000","#009E73",
                              "#CC79A7", "#ccff66",
                              "#EEB99F", "#F0E442","#000099",
                              "#999900","#ff6666", "#996633", 
                              "#336600"  ) )+
  guides(fill=guide_legend(ncol= 1, bycol=TRUE, keyheight  = 0.6, keywidth = 0.5, bycol=TRUE))
tra

p2 <- ggplot(trees_p,  mapping = aes(fill= Specie, x= plot, y=AGB))+
  geom_point()+
  theme(legend.position = "none")
p2

fig_1 <- ggarrange(vip, tra, labels = c("A", "B"), align = "v",
                   widths = c(1,1), common.legend = TRUE, legend="right",  ncol =2)
fig_1
#option 3 only vip data

fig_5 <- ggplot(trees_vip, mapping = aes(fill= plot, x= Specie, y=AGB))+
  geom_bar(position='stack', stat='identity')+
  ylab("Biomass (tonnes)")+
  xlab("Plots")+
  theme(axis.text.x = element_text(angle =90), 
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line (colour= "grey80"))+
  guides(fill=guide_legend(ncol= 1, bycol=TRUE, keyheight  = 0.6, keywidth = 0.5, bycol=TRUE))
fig_5

ggarrange(fig_4, fig_5, labels = c( " ", "C"), ncol = 2) 


pallete <- c("#999999", "#ccff66", "#E69F00", "#56B4E9", "#009E73", 
             "#F0E442","#00A600","#000099", "#D55E00", "#CC79A7",
             "#ff6666", "#9999CC", "#336600", "#6600cc", "#EEB99F",
             "#990000", "#999900", "#996633")

fig_6 <- ggplot(trees_p, mapping = aes(fill= Specie, x=reorder(plot, C_tn_ha, FUN=sum), y=C_tn_ha))+
  geom_bar(position='stack', stat='identity')+
  ylab("Carbon stock (tC/ha)")+
  xlab("Plots")+
  scale_fill_manual(values= c("#999999", "#ccff66", "#E69F00", "#56B4E9", 
                              "#EEB99F", "#9966ff", "#F0E442","#000099","#999900",
                              "#009E73","#ff6666", "#D55E00", "#CC79A7",
                              "#9999CC", "#336600", "#990000",  "#996633") )+
  scale_y_continuous(breaks = c(25, 50, 75, 100, 125, 150, 175, 200, 225, 250, 275))+
  theme(axis.text.x = element_text(angle =90), 
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line (colour= "grey80"))+
  guides(fill=guide_legend(ncol= 1, bycol=TRUE, keyheight  = 0.6, keywidth = 0.5, bycol=TRUE))
fig_6

summary(trees_p)
aggregate(C_tn_ha ~ Specie, trees_p, sum)

describe(trees_p)#### not working
ggplot(trees_p, mapping = aes(x=Specie, y=C_tn_ha))+
  geom_bar(position='stack', stat='identity')+
  ylab("Carbon stock (tC/ha)")+
  xlab("Species")+
  theme(axis.text.x = element_text(angle =90))

#graph of carbon stock of each species ##
### not working
ggplot(df_sp_t, mapping = aes(fill= Specie, x=ID_polygons, y=C_tn_ha))+
  geom_bar(position='stack', stat='identity')+
  ylab("Carbon stock (tn of C/ha)")+
  xlab("Polygons")+
  theme(axis.text.x = element_text(angle =90))+
  guides(fill=guide_legend(ncol= 2, bycol=TRUE))

fig_7 <- ggarrange(v, c, labels= c("A", "B"), 
                   common.legend = TRUE, 
                   legend = "right", 
                   label.x= 0 , ncol = 2)+
  guides(fill=guide_legend(ncol= 2, bycol=TRUE))
fig_7




### calculations for rejuventation ###
rejuv <- read.csv("terreno_woodify_R_rejuv.xlsx - rejuvenation.csv")
names(rejuv) <- as.character(rejuv[1,])
rejuv <- rejuv[-c(1),]

rejuv <- rejuv[-c(335:338),]
rejuv <- rejuv[-337,]
rejuv <- rejuv[-336,]

# see invetoried species and fix details
colnames(rejuv)[2] <- "specie"
colnames(rejuv)[3] <- "count"
specie$Specie <- gsub("Prunus abium", "Prunus avium",specie$Specie)
rejuv$specie <- gsub("\\?", "Unknown",rejuv$specie)

rejuv_sp <- rejuv

aggregate(count ~ specie, rejuv, sum)
rejuv <- transform(rejuv, count = as.numeric(count))

# change info for calculations
rejuv$height <- ifelse(rejuv$h == "0",
       rejuv$height <- 1.1,
       rejuv$height <- 2.5)
rejuv$diam <- ifelse(rejuv$h == "0",
                       rejuv$diam <- 3,
                       rejuv$diam <- 5)

class(rejuv$specie)

###··trying to make aggregate work #
sum(rejuv_sp$n)
aggregate(count ~ specie + plot, rejuv_sp, sum)
describe(rejuv)

library(dplyr)
rejuv_sp <- as.numeric(rejuv_sp[,3])
rejuv_sp <- as.character(rejuv_sp[,2])

rejuv_sp %>% group_by(specie) %>% summarise_each(sum)
aggregate(n ~ specie + plot,data= rejuv_sp, sum, simplify = TRUE)

rejuv_sp <- as.numeric(rejuv_sp$specie)
summary(rejuv)

#apply cone volume formula to then use BEF (V = r^2 * h * pi/3)
rejuv$vol <- (function(a, b) {(a/200)^2 * b * pi /3}) (rejuv$diam, rejuv$height)

rejuv$vol <-  (function(a, b) {a * b}) (rejuv$vol, rejuv$count)
#start other calculations
rejuv_p <- aggregate(vol ~ specie + plot, rejuv, sum)
ggplot(rejuv)
#######
## AGB for each depending on sp. use specie data frame
# wood density tn in m3 considering specie_r list (http://db.worldagroforestry.org//wd/genus/sorbus)

rejuv_p$wd <- specie$wd[match(rejuv_p$specie, specie$Specie)]
rejuv_p$Cfraction <- specie$Cfraction[match(rejuv_p$specie, specie$Specie)]

# first time density
AGB <- rejuv_p$vol* rejuv_p$wd
# now with BEF
AGB <- ifelse(rejuv_p$specie == "Pinus sylvestris" | rejuv_p$specie == "Picea abies", 
                     AGB * 1.3, 
                     AGB * 1.4) 
rejuv_p <- cbind(rejuv_p, AGB)

# Cstock of all
rejuv_p$Cstock <- rejuv_p$AGB * rejuv_p$Cfraction 

## by hectare ##
area <- rep(c(0.006), rep = 104)
rejuv_p <- cbind (rejuv_p, area)

rejuv_p <- rejuv_p %>% 
  dplyr::mutate(area = ifelse(
    str_detect(rejuv_p$plot, "vip"), 0.031416, area))

rejuv_p$C_tn_ha <- rejuv_p$Cstock/ rejuv_p$area

rejuv_p$vol_ha <- rejuv_p$vol/ rejuv_p$area
rejuv_p <-  rejuv_p[,-4]
rejuv_p <-  rejuv_p[,-4]
rejuv_p <-  rejuv_p[,-6]

# add Cstock and Vol by plot and species type
#sumary of rejuv for QGIS
colnames(rejuv_p)
colnames(df_sp_t)
colnames(t_nad)
aggregate(AGB ~ specie, rejuv_p, sum)

rejuv_p$type <- rep(c("rejuvenation"), rep =104)
df_dw <- dw
df_dw$type <- rep(c("deadwood"), rep =188)
colnames(df_dw)
colnames(df_QGIS)
remove(df_all)

df_all <- list(df_QGIS, df_dw)
df_all <- Reduce(function (x, y) merge (x, y, all =TRUE), df_all)

colnames(rejuv_p)[1] <- "Specie"
colnames(rejuv_p)[3] <- "volum"

rejuv_p$Este..X. <- gps$Este..X.[match(rejuv_p$plot, gps$plot)]
rejuv_p$Norte..Y. <- gps$Norte..Y.[match(rejuv_p$plot, gps$plot)]
rejuv_p$alt <-gps$alt[match(rejuv_p$plot, gps$plot)]
rejuv_p$ID_polygons <- shape$ID_polygons[match(rejuv_p$plot, shape$plot)] 

tapply(rejuv_p$C_tn_ha, rejuv_p$Specie, describe_df) #no funca

describe(rejuv_p)
## listo para combinar con df_sp_t
# Unir para QGIS work is missing the other 2 parts (deadwood)
colnames(rejuv_p)
colnames(df_sp_t)
df_QGIS<- list(rejuv_p, df_sp_t)
df_QGIS <- Reduce(function (x, y) merge (x, y, all =TRUE), df_QGIS)
write.csv(df_QGIS, "/Users/vale/Documents/Wodify/Field work/Woodify/df_sp_t.csv")

install.packages("psych")
library(psych)
describe(dw)

#same pero sin species for nadine
colnames(t_nad)
df_r <- aggregate(cbind(AGB, Cstock, C_tn_ha, vol_ha) ~ plot, rejuv_p, sum)
df_r$ID_polygons <- shape$ID_polygons[match(df_r$plot, shape$plot)] 
colnames(df_r)

#join for Nadines NDVI estimations
df_r <- df_r[-23,]
t_nad <- t_nad[-16,]
df_nadine <- list(df_r, t_nad)
df_nadine <- Reduce(function (x, y) merge (x, y, all =TRUE), df_nadine)
write.csv(df_nadine, "/Users/vale/Documents/Wodify/Field work/Woodify/df_nadine.csv")

ID_atributos <- read.csv("df_nadine - df_nadine.csv")
df_nadine$OBJECTID_1 <- ID_atributos$OBJECTID_1[match(df_nadine$plot, ID_atributos$plot)] 

colnames(df_nadine)
colnames(ID_atributos)
df_nadine_m <- aggregate(cbind(AGB, Cstock, C_tn_ha, vol_ha) ~ OBJECTID_1, df_nadine, mean)

atributos <- read.csv("tabla atributos shape.xlsx - tabla atributos shape.csv")
atributos <- atributos[-49,]
atributos <- atributos[-48,]
colnames(df_nadine)[1] <- "Wldrt_GIS_"

df_nad <- list(df_nadine_m, atributos)
df_nad <- Reduce(function (x, y) merge (x, y, all =TRUE), df_nad)

## para nadine## NDVI calculations
write.csv(df_nad, "/Users/vale/Documents/Wodify/Field work/Woodify/df_nadine.csv")


## plot the amount of rejuvenation hapening, type of specie and amount of browsing
colnames(rejuv_p)
rejuv <- transform(rejuv, n = as.numeric(n))
rejuv <- transform(rejuv, Browsing = as.numeric(Browsing))
reju_count <- aggregate(cbind(Browsing) ~ specie + plot + h + Browsing, rejuv, sum)

rejuv_22 <- rejuv[which(rejuv$h == "1"),]
rejuv_2 <- rejuv[which(rejuv$h == "0"),]

A <- ggplot(rejuv_2, aes(x= reorder(specie, count, sum), y = count, fill= Browsing))+
  geom_bar(stat="identity", position = 'stack')+
  scale_fill_discrete( labels=c("Absence", "Presence"))+
  theme_minimal()+ theme(axis.text.x = element_text(angle =90),
                         legend.position = "none",
                         legend.background = element_rect(fill = "white"))+
  ylab("Count")+
  xlab("Trees <2 metres")
A

b <- ggplot(rejuv_22, aes(x= reorder(specie, count, sum), y = count, fill= Browsing))+
  geom_bar(position = 'stack', stat='identity')+
scale_fill_discrete( labels=c("Absence", "Presence"))+
  theme_minimal()+ theme(axis.text.x = element_text(angle =90),
                         legend.position = c(0.2, 0.8),
                         legend.background = element_rect(fill = "white"))+
  ylab("Count")+
  xlab("Trees >2 metres")
b

fig_8 <- ggarrange(A, b, labels= c("A", "B"), 
                   common.legend = FALSE, 
                   label.x= 0 ,ncol = 2)
fig_8 
colnames(rejuv_p)
# species rejuvenation count
install.packages("forcats")
library(forcats)
ggplot(rejuv, aes(x= fct_infreq(plot), y=fct_infreq(specie)))+
  geom_count(aes(color = after_stat(n), size = after_stat(n)), show.legend = TRUE, position='identity')+
  guides(color = guide_legend(title = "Individuals\nCount"), size =guide_legend(title = "Individuals\nCount"))+ 
  theme_minimal()+ theme(axis.text.x = element_text(angle =90),
                         legend.position = c(0.95, 0.83),
                         legend.title = element_text(size=10),
                         legend.background = element_rect(fill="white"))+
  xlab("Plots") + ylab("Specie")
  

#ggplot(dw_s_vip, mapping = aes(fill= dw_type, x= plot, y= biomass ))+
  #geom_bar(position = "dodge", stat= "identity")+
  #ggtitle("Species per plot")+
  #theme_stata()
install.packages("ggforce")
library(ggforce)
colnames(rejuv_p)
rejuv_p$AGB_ha <- rejuv_p$AGB/rejuv_p$area
rejuv_sp <- aggregate(AGB_ha ~ specie, rejuv_p, mean)
pal16 <- c("#CCCCCC", "#ccff66", "#E69F00", "#56B4E9", 
           "#D55E00", "#EEB99F", "#F0E442","#00A600",
           "#000099", "#009E73", "#CC79A7", "#ff6666", 
           "#9999CC", "#336600", "#6600cc",  "#990000", 
           "#999900", "#996633")
names(pal16) = rejuv_sp$specie
pal16
ggplot(rejuv_p, aes(x= reorder(plot, AGB_ha, FUN=sum), y=AGB_ha, fill= specie))+
  geom_bar(position='stack', stat='identity')+
  ylab("Biomass (t/ha)")+
  xlab("Plots")+
  labs(fill="Species")+
  theme(axis.text.x = element_text(angle =90), 
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line (colour= "grey80"),
        legend.position = c(0.4, 0.83),
        legend.title = element_text(size=10),
        legend.background = element_rect(fill="white"))+
  scale_fill_manual(values= pal18 )+
  guides(fill=guide_legend(nrow = 4, byrow=TRUE, keyheight  = 0.6, keywidth = 0.5, bycol=TRUE))

rb <- ggplot(rejuv_p, mapping = aes(fill= specie, x=plot, y=C_tn_ha,))+
  geom_bar(position='stack', stat='identity')+
  ylab("Cstcok (tC/ha)")+ xlab("Plots")+
  theme(axis.text.x = element_text(angle =90))+

  guides(fill=guide_legend(ncol= 1, bycol=TRUE, keyheight  = 0.6, keywidth = 0.5, bycol=TRUE))

rb
install.packages("ggthemes")
library(ggthemes)
rt <- ggplot(rejuv_p, mapping = aes(fill= Specie, x=reorder(plot,C_tn_ha, FUN=sum), y=C_tn_ha))+
  geom_bar(position='stack', stat='identity')+
  scale_fill_stata(scheme = "s2color")+
  ylab("Cstock (tC/ha)")+ xlab("Plots")+
  theme(axis.text.x = element_text(angle =90))+
  guides(fill=guide_legend(title= "Species", 
                           ncol = 1, keyheight  = 0.6, keywidth = 0.5, bycol=TRUE))
rt
scale_fill_st
facet_zoom(ylim= c(0, 250))+
  
coord_flip()
describe(rejuv_p)  

install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("ggthemes")
library(ggthemes)
install.packages("circlize")
library(circlize)

circos.initialize(factors= df_final$plot, x= df_final$C_tn_ha)
circos

#### todo bien hasta aca prob.revisar lo del GPS!!! ######
## add gps coordinates ###
## add official name of polygons 

# correct wrong things in gps revisar si aun es necesario later?
library(stringr)
# merge with shape files name of polygons
##arreglars ID polygons no se esta uniendo

 
###deadwood calculations### to add to "df_QGIS"###
deadwood <- read_excel("terreno_woodify_R_dead.xlsx")
describe(deadwood)

#Laying without roots
ly_dw <- filter(deadwood, d_2 != 0 & notes != "standing" 
                &               notes != "roots" & notes !="stand con hojas" 
                & notes != "punta arb quebrado" & notes != "alive?")

#& (las que no tienen valor 0 en d_2 y no notas no dice standing)
# Stumps
ly_s_dw <- filter(deadwood, Stumps == 0 | Stumps == 1 | Stumps == 2)

# Lying but only the ones with roots
ly_r_dw <- filter(deadwood, notes == "roots")

# Standing deadwood trees
stan_dw <- filter(deadwood, d_2 == 0 | notes == "standing" 
                  | notes == "punta arb quebrado" | notes == "stand con hojas"
                  |  notes == "alive?")

# change to numeric
#lying_dw <- transform(lying_deadwood, L = as.numeric(L))
ly_r_dw <- transform(ly_r_dw, L = as.numeric(L)) 
ly_dw <- transform(ly_dw, L = as.numeric(L))
stan_dw <- transform(stan_dw, L = as.numeric(L))
stan_dw <- transform(stan_dw, d_1 = as.numeric(d_1))

# ad L values in NA of standing ones 
#stan_dw_L <- aggregate(L ~ plot,stan_dw, mean)

stan_dw <- stan_dw %>% group_by (plot) %>% 
  mutate_at(vars(L), ~replace_na(., mean(., na.rm = TRUE)))
stan_dw <- transform(stan_dw, L = as.numeric(L))


### working with ly_dw ####

#pi(d1^2 + d2^2)*h/8 (Smalian formula)
d12 <- (function(a, b) {((a/100)^2) + ((b/100)^2) }) (ly_dw$d_1, ly_dw$d_2) #in m
vol <- (function(a, b) {a * (b)* pi / 8}) (d12, ly_dw$L)  # L in m
ly_dw <- cbind (ly_dw, vol) #vol in m3

# Density of Quercus and Fagus Sylvatica is 0.58 and Picea abies 0.40(tonnes dry matter/ m3 of fresh vol
#Biomas for vol (tn ??)
AGB <- ifelse (ly_dw$type == "b", 
                 ly_dw$vol * 0.952, 
                 ly_dw$vol * 0.611)
ly_dw <- cbind (ly_dw, AGB)

# change colum names : colnames(stan_dw)[11] <- "Biom"   #new name again
##Cstock
ly_dw$Cstock <- ly_dw$AGB * 0.5

### working with ly_r_dw  ###
# add 30 cm for roots in L
L_r <- ly_r_dw$L + 0.3
ly_r_dw <- cbind(ly_r_dw, L_r)

# pi(d1^2 + d2^2)*h/8 (Smalian formula)
ly_r_dw$vol<- (function(a,b,c) {((((a/100)^2) + ((b/100)^2)) * pi * c/8)})  (ly_r_dw$d_1, ly_r_dw$d_2, ly_r_dw$L_r) 
#in m #vol in m3

# BIOMASS for ly_dw, vol
ly_r_dw$AGB <- ifelse (ly_r_dw$type == "b", 
               ly_r_dw$vol * 0.952, 
               ly_r_dw$vol * 0.611)

##Cstock
ly_r_dw$Cstock <- ly_r_dw$AGB * 0.5

### working with stumps ###

# volume and biomas calculations for stumps
# with cilinder formula V = r2*pi*L
# designated measurements for each stump categorie
#stump = 0 ->  D= 13 cm	    h -> 15 cm	
#stump = 1 ->  D= 30 cm.   	h -> 20 cm
#stump = 2 ->  D= 45 cm.   	h -> 35	cm
#ading diameter depending on stump category
                     # Install stringr package
library("stringr")

### count of stumps ###
n <- rep(c(1), rep = 220)
stumps <- cbind(ly_s_dw, n)
size <- rep(c("6 - 20"), rep = 220) #en cm - rep given the 220 obs in data frame
stumps <- cbind (stumps, size)
# columna de deje la caga, asique no usarla en stump mode #
#para graph de stumps
stumps <- stumps %>% 
  mutate(size = ifelse(
    str_detect(stumps$Stumps, "1"), "20 - 40", size))
stumps <- stumps %>% 
  mutate(size = ifelse(
    str_detect(stumps$Stumps, "2"), "40+", size))



D <- rep(c(13), rep = 220) #en cm - rep given the 220 obs in data frame
ly_s_dw <- cbind (ly_s_dw, D)

ly_s_dw <- ly_s_dw %>% 
  mutate(D = ifelse(
    str_detect(ly_s_dw$Stumps, "1"), 30, D))
ly_s_dw <- ly_s_dw %>% 
  mutate(D = ifelse(
    str_detect(ly_s_dw$Stumps, "2"), 45, D))

#adding height depending on stump category
H <- rep(c(0.15), rep = 220) #en m
ly_s_dw <- cbind (ly_s_dw, H)

ly_s_dw <- ly_s_dw %>% 
  mutate(H = ifelse(
    str_detect(ly_s_dw$Stumps, "1"), 0.20, H))
ly_s_dw <- ly_s_dw %>% 
  mutate(H = ifelse(
    str_detect(ly_s_dw$Stumps, "2"), 0.35, H))

#for volume apply cilinder formula r2*pi*H
vol <- (function (a,b) {((a/200)^2)* b * pi}) (ly_s_dw$D, ly_s_dw$H) #en m3
ly_s_dw <- cbind (ly_s_dw, vol)

# biomass calculation
AGB <- ifelse (ly_s_dw$type == "b", 
               ly_s_dw$vol * 0.952, 
               ly_s_dw$vol * 0.611)
ly_s_dw <- cbind (ly_s_dw, AGB)

##Cstock
ly_s_dw$Cstock <- ly_s_dw$AGB * 0.5


#### working with standing dead wood ####
# Cone formula vol= r2 * pi * L/3
vol <- (function(a, b) { (((a/200)^2) * b * pi )/3}) (stan_dw$d_1, stan_dw$L) #in m3
stan_dw <- cbind(stan_dw, vol)

#segun type b or c, biomas correction and expansion fatcor (b: 1.4 * vv , c:1.3 * 0.)
AGB <- ifelse (stan_dw$type == "b", 
               stan_dw$vol * 0.952, 
               stan_dw$vol * 0.611)
stan_dw <- cbind (stan_dw, AGB ) # in tones

##Cstock
stan_dw$Cstock <- stan_dw$AGB * 0.5

# sum by plot (can be plot or more than one grouping category like type (b or c))
# change cal name to biom_s to know better
ly_plot <- aggregate(cbind(vol, AGB, Cstock) ~ plot, 
                     ly_dw, sum)
ly_r_plot <- aggregate(cbind(vol, AGB, Cstock) ~ plot, 
                       ly_r_dw, sum)
ly_s_plot <- aggregate(cbind(vol, AGB, Cstock) ~ plot, 
                       ly_s_dw, sum)
stan_plot <- aggregate(cbind(vol, AGB, Cstock) ~ plot, 
                       stan_dw, sum)

#rm(stan_plot)
#add extra colum to diferenciate type of CWD
ly_plot$dw_type <- rep(c("logs"), rep = 72) #ly_dw_plot deadwood type logs (lying dw)
ly_r_plot$dw_type <- rep(c("cwd"), rep = 12) #ly_dw_plot deadwood type coarse root debris (CWD) for our case is just logs with roots
stan_plot$dw_type <- rep(c("snag"), rep = 56) #ly_dw_plot deadwood type snags (standing trees)
ly_s_plot$dw_type <- rep(c("stump"), rep = 48) #ly_dw_plot deadwood type stumps

colnames(ly_plot)
colnames(ly_r_plot)
colnames(ly_s_plot)
colnames(stan_plot)
  

#putting all together again #merge dataframes
install.packages("tidyverse")
library(tidyverse)
# merge 4 parts again -->> dibujar desde aca!
dw <- list(ly_plot, ly_r_plot,stan_plot, ly_s_plot)
dw <- Reduce(function (x, y) merge (x, y, all= TRUE), dw)


#per hectare for dw
Area <- rep(c(0.0060), rep = 188)
dw <- cbind (dw, Area)

dw <- dw %>% 
  dplyr::mutate(Area = ifelse(
    str_detect(dw$plot, "vip"), 0.031416, Area))

#vol per hc and tons/hectare
dw$AGB_ha <- dw$AGB / dw$Area
dw$C_tn_ha <- dw$Cstock / dw$Area  # in tones


df_dw <- aggregate(cbind(AGB, Cstock, C_tn_ha, AGB_ha) ~ plot, dw, sum)
df_dw$type <- rep(c("deadwood"), rep = 72)
df_trees <- aggregate(cbind(AGB, Cstock, C_tn_ha, AGB_ha) ~ plot, trees_p, sum)
df_trees$type <- rep(c("live trees"), rep = 72)
df_rejuv <- aggregate(cbind(AGB, Cstock, C_tn_ha, AGB_ha) ~ plot, rejuv_p, sum)
df_rejuv$type <- rep(c("regeneration"), rep = 72)
df_all <- merge(df_dw, df_trees, 
               type = "full")
df_all <- join(df_dw, df_trees, 
               type = "full") # not working alone, but with merge first creating the data frame it works
df_all <- join(df_all, df_rejuv, 
               type = "full")

## mix with gps and shape ## join all biomass ##
gps$plot <- str_replace(gps$plot, "cn134", "cn134road")
gps$plot <- str_replace(gps$plot, "s19", "s19road")
gps$plot <- str_replace(gps$plot, "s65", "s65road")

colnames(df_QGIS)
colnames(df_dw)

df_dw$Este..X. <- gps$Este..X.[match(df_dw$plot, gps$plot)]
df_dw$Norte..Y. <- gps$Norte..Y.[match(df_dw$plot, gps$plot)]
df_dw$alt <-gps$alt[match(df_dw$plot, gps$plot)]
df_dw$ID_polygons <- shape$ID_polygons[match(df_dw$plot, shape$plot)]

df_QGIS <- df_QGIS[-3]

#maybe not needed??##
dw <- dw[-6]
colnames(dw)[2] <- "volum"
colnames(dw)[5] <- "type"
dw$Specie <- rep(c("Deadwood"), rep = 188 )

dw_2 <- dw
colnames(dw_2)[12] <- "type"
colnames(dw_2)[5] <- "dw_type"

df_final <- list(df_dw, df_QGIS)
df_final <- Reduce(function (x, y) merge (x, y, all =TRUE), df_final)

write.csv(df_final, "/Users/vale/Documents/Wodify/Field work/Woodify/df_final_3groups.csv")
colnames(ID_atributos)

ID_atribut <- read.csv("df_final_3groups - df_final_3groups.csv")
colnames(ID_atribut)
ID_atributos <- ID_atributos[-7]
rm(IDs)

IDs <- list(ID_atributos, ID_atribut)
IDs <- Reduce(function (x, y) merge (x, y, all =TRUE), IDs)

df_final$OBJECTID_1 <- IDs$OBJECTID_1[match(df_final$plot, IDs$plot)]
write.csv(df_final, "/Users/vale/Documents/Wodify/Field work/Woodify/df_final_groups.csv")

##
#.  todo good ok.  #
##
sessionInfo()
getwd()
at <- read.csv("atribute_table.csv")
plots <- read.csv2("df_plots.csv")
colnames(at)[23] <- "ID_polygons"
merged<- merge(plots, at, by=c("Wldrt_GIS_"), all.x=T)
write.csv(merged, "/Users/vale/Downloads/tabla_tesis_tdl.csv")

### Graphs of deadwood ####
ggplot(stumps, aes(x= (reorder((size), n, sum, decreasing = TRUE)), y= n, fill = type))+
  geom_bar(stat="identity")+
  ylab("Count")+
  xlab("Stumps size")+
  theme_minimal()+
  scale_fill_hue( name = "Type", labels=c("Broadleaf", "Conifer", "Unknown" ))+
  scale_x_discrete(name = "Stump Size (cm)")+
  theme(legend.position = c(0.7, 0.83),
        legend.title = element_text(size=10),
        legend.background = element_rect(fill="white"))+
  guides(fill=guide_legend(bycol=TRUE, keyheight  = 0.6, keywidth = 0.5, bycol=TRUE))

# now doing it per ha
n_ha <- aggregate(n ~ size + plot + type, stumps, sum)
n_ha$Area <- rep(c(0.0060), rep = 62)
n_ha <- n_ha %>% 
  dplyr::mutate(Area = ifelse(
    str_detect(n_ha$plot, "vip"), 0.031416, Area))
stumps_ha <- n_ha
stumps_ha$n_ha <- n_ha$n / n_ha$Area

##still working on it, it could be devide in 3 plots for each size
stumps_ha 
stumps_1 <- stumps_ha[which(stumps_ha$size == "6 - 20"),]
stumps_2 <- stumps_ha[which(stumps_ha$size == "20 - 40"),]
stumps_3 <- stumps_ha[which(stumps_ha$size == "40+"),]

ggplot(stumps_ha, aes(x= (reorder((plot), n_ha, sum, decreasing = TRUE)), y= n_ha, fill = type))+
  geom_bar(stat="identity")+
  ggtitle("Sizes") +
  ylab("Amount per Ha")+
  xlab("Plots")+
  facet_wrap(~size) +
  theme_minimal()+
  scale_fill_hue( name = "Type", labels=c("Broadleaf", "Conifer", "Unknown" ))+
    theme(axis.text.x = element_text(angle =90), 
        legend.position = "bottom")+
  guides(fill=guide_legend(bycol=TRUE, keyheight  = 0.6, keywidth = 0.5, bycol=TRUE))


fig_9

labels=c("Broadlleaf" = "#b9e38d", 
         "Conifer" = "#a1e9f0", 
         "Anknown" = "#d9b1f0")
theme(axis.text.x = element_text(angle =90))+
  ylab("Count")+
  xlab("Species <2 metre")+
  coord_flip()

#drawings of DW * need feexing the numbers in x ? and the rest?
ggplot(dw, aes(x="", y= 
                 C_tn_ha, fill= dw_type))+
  geom_bar(width = 2 , stat="identity")+
  coord_polar("y", start=0)

#drawings of all dw together
fig.9 <- ggplot(dw, aes(x=reorder(plot, C_tn_ha, FUN=sum), y= C_tn_ha, fill=dw_type))+
  geom_bar(position="stack", stat='identity')+
  theme_minimal ()+ 
  theme(axis.text.x = element_text(angle =90), 
     legend.position = c(0.4, 0.83),
              legend.title = element_text(size=10),
              legend.background = element_rect(fill="white"))+
  guides(fill=guide_legend(title= "Type", 
                           nrow = 1, keyheight  = 0.6, keywidth = 0.5, bycol=TRUE))+
  xlab("Plots") + ylab("Cstock (tC/ha)")
theme(axis.text.x = element_text(angle =90), 
      +      legend.position = c(0.4, 0.83),
      +               legend.title = element_text(size=10),
      +               legend.background = element_rect(fill="white"))+
  +   guides(fill=guide_legend(title= "Type", 
                               +                            nrow = 1, keyheight  = 0.6, keywidth = 0.5, bycol=TRUE))+
  +   xlab("Plots") + ylab("Cstock (tC/ha)")



aggregate(C_tn_ha ~plot , dw, sum)
summary(dw)
legend.title = element_name(name ="Type"),
legend.position= c(.2, .8), 
coord_polar("y", start=50)
circlize(dw$plot, dw$C_tn_ha)

coord_flip()
ggplot(df_final, aes(plot, C_tn_ha))+
  geom_line(show.legend = FALSE)
ggplot(dw, aes(dw_type, C_tn_ha))+
  geom_boxplot(show.legend = FALSE)
coord_polar("y", start=0)
describe(dw)
###
ggplot(dw, aes(x=plot, y= C_tn_ha))+
  geom_path() geom_
ggplot(rejuv_p, mapping = aes(fill= specie, x=plot, y=C_tn_ha))+
  geom_bar(position='dodge', stat='identity')+
  scale_fill_stata(scheme = "s2color")+
  ylab("Cstock per hectares (tn/ha)")+ xlab("Plots")+
  theme(axis.text.x = element_text(angle =90), legend.position = "top")+
  guides(fill=guide_legend(title= "Species", 
                           nrow = 2, keyheight  = 0.6, keywidth = 0.5, bycol=TRUE))
facet_zoom(ylim= c(0, 250))

#drawings of all together## not possible yet
summary(df_all)
aggregate(C_tn_ha ~type, df_all, mean)
last <- ggplot(df_all, aes(x=reorder(plot, C_tn_ha, FUN=sum), y= C_tn_ha, fill= type))+
  geom_bar(position='stack',stat="identity", width =1)+
  theme(axis.text.x = element_text(angle =90))+
  ylab("Cstock (tC/ha)")+ xlab("Plots")+ theme_minimal()+
  theme(axis.text.x = element_text(angle =90), 
              legend.position = c(0.4, 0.83),
              legend.title = element_text(size=10),
              legend.background = element_rect(fill="white"))+
  scale_fill_manual("Targets", values=c("live trees" = "#b9e38d", 
                                           "deadwood" = "#458B74" , 
                                           "regeneration" = "#9999FF" ))+
  guides(fill=guide_legend(nrow = 1, keyheight  = 0.6, keywidth = 0.5, bycol=TRUE))
last

facet_zoom(ylim= c(0, 250))

ggplot(df_all, aes(x=type, y=C_tn_ha))+
  geom_boxplot()

#table of means and values for each and all

summary(df_all)
summary(df_trees)
summary(df_rejuv)
summary(df_dw)

a <- 38.05+96.57+14.95
a * 336.76

# Create a circular plot ##
data <- data.frame(
  type=c("Deadwood", "Regeneration", "Trees"),
  C_tn_ha=c(39.0295, 417.399, 90.369 )) #mean values

data$fraction <- data$C_tn_ha / sum(data$C_tn_ha)
data$percentage <- data$fraction * 100
data$percentage <- format(round(data$percentage , 2), nsmall = 2)
data$ymax <- cumsum(data$fraction)
data$ymin <- c(0, head(data$ymax, n=-1))
data$labelPosition <- (data$ymax + 
                         data$ymin) / 2 
data$label <- paste0(data$percentage, "%")

install.packages("ggrepel")
library(ggrepel)
install.packages("ggthemes")
library(ggthemes)
install.packages("paletteer")
library (paletteer)

pie <- ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=type)) +
  geom_rect() +
  geom_label(x=3.5, aes(y=labelPosition, label=label), size = 3)+
  scale_fill_manual("Targets", values=c("Trees" = "#b9e38d", 
                             "Deadwood" = "#458B74", 
                             "Regeneration" = "#9999FF"))+
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void()+
  theme(legend.position= "none" )

pie
labels = c("Trees", "Deadwood", "Regeneration")
Fig2.2 <- ggarrange(fig2, s,labels = c("A", "B"), ncol=2, widths=c(2,1))

final <- ggarrange(last, pie, labels = c("A", "B"), ncol = 2, widths=c(3,1), 
          common.legend = F)
final
geom_label_repel( x=3.5, aes(y=labelPosition, label=label), size=3) +
brewer(palette=4) 
scale_fill_paletteer_d("ggthemes::wsj_dem_rep")


ggplot(df_final, aes(x="", y= vol_ha, fill= type))+
  geom_bar(stat="identity", width =1)+
  coord_polar("y", start=0)

#count of observations in each group (type)
ggplot(df_final, aes(x=factor(1), fill= type))+
  geom_bar(width =1)+
  coord_polar("y")
###
summary (dw)


## for graphs ##
library("ggplot2")

colnames(dw)
Fig9 <- ggplot(dw, mapping = aes(fill=dw_type, x=reorder(plot, C_tn_ha, sum), y=C_tn_ha,))+
  geom_bar(position='stack', stat='identity')+
  ylab("Cstock (tC/ha)")+ xlab("Plots")+
  theme(axis.text.x = element_text(angle =90), legend.position = "bottom")+
  guides(fill=guide_legend(title= "Type", nrow = 1, keyheight  = 0.6, keywidth = 0.5, bycol=TRUE))
Fig9
facet_zoom(ylim= c(0, 110))+

df_GIS <- df_QGIS  
df_GIS$type <- str_replace(df_GIS$type, "cwd", "deadwood")
df_GIS$type <- str_replace(df_GIS$type, "snag", "deadwood")
df_GIS$type <- str_replace(df_GIS$type, "logs", "deadwood")
df_GIS$type <- str_replace(df_GIS$type, "stump", "deadwood")
summary(df_GIS)
summary(df_nad)
327.92 *365
mean(df_GIS$C_tn_ha)

ggplot(df_GIS, mapping = aes(fill=type, x=plot, y=C_tn_ha)) +
  geom_bar(position='stack', stat='identity')+   ylab("Cstock (tn of C/ha)")+ xlab("Plots")+
  theme(axis.text.x = element_text(angle =90),  legend.position = "top")+
  facet_zoom(ylim= c(0, 520))+
  guides(fill=guide_legend(title= "Type", nrow = 1, keyheight  = 0.6, keywidth = 0.5, bycol=TRUE))+
  scale_fill_manual("Components", values=c("live trees" = "#b9e38d", 
                                       "deadwood" = "#a1e9f0", 
                                       "rejuvenation" = "#d9b1f0"))
ggplot(df_GIS, mapping = aes(x=type, y=C_tn_ha)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle =90),  legend.position = "top")+
  guides(fill=guide_legend(title= "Type", nrow = 1, keyheight  = 0.6, keywidth = 0.5, bycol=TRUE))
  
describe(df_GIS)
summary(df_final)

ggplot(df_QGIS, mapping = aes(fill=type, x=plot, y=C_tn_ha)) +
    geom_bar(position='dodge', stat='identity')+   ylab("Cstock (tn of C/ha)")+ xlab("Plots")+
  theme(axis.text.x = element_text(angle =90))+
  facet_zoom(ylim= c(0, 520))+
scale_fill_manual("legend", values=c("cwd" = "#eb8060", 
                                     "live trees" = "#b9e38d", 
                                     "logs" = "#a1e9f0", 
                                     "rejuvenation" = "#d9b1f0",
                                     "snag" ="#E69F00",
                                    "stump" = "#999999"))
  
library("RColorBrewer")
fig_9 <- ggplot(trees, mapping =aes ( x=plot, y= DBH, fill=Specie))+
  geom_dotplot(stackgroups = TRUE, binaxis = "y", method = "dotdensity", binwidth = 1)+ 
  fill_palette( "#009E73","#00A600"
    pallete= c("#999999", "#ccff66", "#E69F00", "#56B4E9",
               "#F0E442","#000099", "#D55E00", "#CC79A7",
                "#ff6666", "#9999CC", "#336600", "#6600cc", "#EEB99F",
                  "#990000", "#999900", "#996633"))+
  theme(axis.text.x = element_text(angle =90))

#funtiona tambien pero da menos info, agregar forma por specie _ shape= Specie
ggplot(trees, aes(x=plot, y=DBH, fill=Specie))+
  theme(axis.text.x = element_text(angle =90))+ 
  geom_count(aes(x=plot, y=DBH, group=Specie,
                  color = after_stat(n),
                 fill = after_scale(alpha(colour, 0.4))))
  scale_fill_discrete() +theme_classic()

#funciona
fig2 <- ggplot(trees, aes(x=reorder(plot, DBH, mean), y=DBH, fill=Specie)) +
  geom_boxplot(position=position_dodge(0.8))+ theme_minimal()+
  theme(axis.text.x = element_text(angle =90),
        panel.grid = element_line(), legend.position = "bottom")+
  guides(fill=guide_legend(title= "Species", nrow = 4,
                           keyheight  = 0.6, keywidth = 0.5,
                           bycol=TRUE))+
  labs (x = "Plots", y = "DBH (cm)")
  geom_vline(xintercept = trees$plot)
fig2
#unir con box plot per sp type
Fig2.2 <- ggarrange(fig2, s,labels = c("A", "B"), ncol=2, widths=c(2,1))
Fig2.2

ggarrange(vi, tl, labels = c("A", "B"), nrow = 2, common.legend = T, legend = "right")+guides(fill =guide_legend(ncol=1))


#same but transects and VIP separated

#linea tambien funciona.. pero no con todo junto..
geom_vline(xintercept = trees$plot)

#maybe tambien:
facet_grid(cols=trees$plot)

#funciona
  ggplot(trees, aes(x=plot, y=DBH, fill=Specie)) +
  geom_dotplot(binaxis='y', stackdir='center', 
               position=position_dodge(0.8), width=2,
               dotsize=0.7) +  theme_classic()+
    theme(axis.text.x = element_text(angle =90),
          panel.grid = element_line(), legend.position = "bottom")
  
#prueba con mas detalle
install.packages("viridis")
library("viridis")

tree_vip <- filter(trees, str_detect(plot, "vip") )
  tree_t <- filter(trees, str_detect(plot, "vip", negate = TRUE) )

vi <-  ggplot(tree_vip, aes(x=plot, y=DBH, fill=Specie)) +
    geom_dotplot(binaxis='y', stackdir='center', stackratio = 0.5, 
                 position=position_dodge(0.5), width= 3,
                 dotsize=1, color="#999999") +  theme_minimal()+
    theme(axis.text.x = element_text(angle =90),panel.grid = element_line(), 
          legend.position = "bottom",legend.key.size  = unit(0.4, "cm"))+
  guides(fill =guide_legend(ncol=1))+ scale_fill_manual(values=c("Betula pendula"  = "#EEB99F", 
                     "Carpinus betulus" = "#336600",  "Castanea sativa" ="#E69F00",
                 "Corylus avellana"= "#56B4E9", "Crataegus sp" = "#F0E442",
                    "Fagus sylvatica" = "#000099", "Malus sylvestris" = "#009E73",
                   "Picea abies" ="#D55E00",  "Pinus sylvestris" = "#CC79A7", "Prunus abium"= "#ff6666",
         "Quercus petraea" = "#ccff66", "Salix caprea" = "#9999CC", 
      "Sorbus aria" = "#6600cc", "Sorbus torminalis" =    "#990000", 
            "Tilia cordata" = "#999900", "Unknown" = "#996633")) +
  labs (x = "VIPs", y = "DBH (cm)")
vi 
tr <-  ggplot(tree_t, aes(x=plot, y=DBH, fill=Specie))+
  geom_dotplot(binaxis='y', stackdir='center', stackratio = 0.5, 
               position=position_dodge(0.5), width= 2.5,
               dotsize=1, color="#999999") +  theme_minimal()+
  theme(axis.text.x = element_text(angle =90), panel.grid = element_line(), legend.position = "none")+
  scale_fill_manual(values=c("Betula pendula"  = "#EEB99F", 
                             "Carpinus betulus" = "#336600", "Castanea sativa" ="#E69F00",
                             "Corylus avellana"= "#56B4E9", "Crataegus sp" = "#F0E442",
                             "Fagus sylvatica" = "#000099", "Malus sylvestris" = "#009E73",
                             "Picea abies" ="#D55E00",  "Pinus sylvestris" = "#CC79A7", "Prunus abium"= "#ff6666",
                             "Quercus petraea" = "#ccff66", "Salix caprea" = "#9999CC", 
                             "Sorbus aria" = "#6600cc", "Sorbus torminalis" = "#990000", 
                             "Tilia cordata" = "#999900", "Unknown" = "#996633")) +
  guides(fill =guide_legend(ncol=1))+  labs (x = "Transects", y = "DBH (cm)")

tr
# grafico para extraer leyenda comun
leg <- ggplot(trees, aes(x=plot, y=DBH, fill=Specie))+
  geom_dotplot(binaxis='y', stackdir='center', stackratio = 0.5, 
               position=position_dodge(0.7), width= 3.5,
               dotsize=0.9, color="#CCCCCC") +  theme_classic()+
  theme(axis.text.x = element_text(angle =90), panel.grid = element_line(),
        legend.position = "right", legend.key.size  = unit(0.4, "cm"))+
  scale_fill_manual(values=c("Betula pendula"  = "#EEB99F", 
                             "Carpinus betulus" = "#336600", "Castanea sativa" ="#E69F00",
                             "Corylus avellana"= "#56B4E9", "Crataegus sp" = "#F0E442",
                             "Fagus sylvatica" = "#000099", "Malus sylvestris" = "#009E73",
                             "Picea abies" ="#D55E00",  "Pinus sylvestris" = "#CC79A7", "Prunus abium"= "#ff6666",
                             "Quercus petraea" = "#ccff66", "Salix caprea" = "#9999CC", 
                             "Sorbus aria" = "#6600cc", "Sorbus torminalis" = "#990000", 
                             "Tilia cordata" = "#999900", "Unknown" = "#996633"))
  guides(fill =guide_legend(nrow=3))

leg 
le  <-  get_legend(leg)
le <- as_ggplot(le)

# union de graficos 
tl <- ggarrange(tr, s,labels = c("", "C"), ncol=2, widths=c(2,1))
tl

ggarrange(vi, tl, labels = c("A", "B"), nrow = 2, common.legend = T, legend = "right")+guides(fill =guide_legend(ncol=1))


colnames(df_dw)
colnames(df_t)
colnames(rejuv_QGIs)
colnames(df_QGIS)
write.csv(QGIS, "/Users/vale/Documents/Wodify/Field work/Woodify/QGIS_nadine.csv")

#join caculation of polygons with atribute table#

write.csv(prueba, "/Users/vale/Documents/Wodify/Field work/Woodify/final_nadine.csv")
