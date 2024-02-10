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

trees <- read_csv("R_standingtrees_terreno_obk.csv", show_col_types = FALSE)

#Data standardization for calculations
names(trees) <- as.character(trees[1,]) #Copy 1st row to header:
trees <- trees[-1,] #Delete 1st row :
trees <- trees[,-1] #delete first colum that is emty

summary(trees)
describe(trees)
colnames(trees)

aggregate(cbind(DBH) ~ Specie, trees, mean)
colnames(trees)[1] <- "plot"
colnames(trees)[3] <- "Specie"

#transform DBH with coma to dots to then change to numeric
trees$DBH <- trimws(gsub(',','.',trees$DBH,fixed=T))
trees <- transform(trees, DBH = as.numeric(DBH))

#apply mean of h of species (before plot) to the NA · difference from TdL maybe =0, here the plots are more mixed stands.
trees$h <- trimws(gsub(',','.',trees$h,fixed=T))
trees <- transform(trees, h = as.numeric(h)) 

aggregate(cbind(DBH) ~ Specie, trees, mean)
aggregate(cbind(h) ~ Specie, trees, mean)

trees <- trees %>% group_by (Specie) %>%
  mutate_at(vars(h), ~replace_na(., mean(., na.rm = TRUE)))

trees$h <- format(round(trees$h , 3), nsmall = 3)
trees <- transform(trees, h = as.numeric(h)) 
trees$Specie <- gsub("FALSE DATA", "Unknown",trees$Specie)

#Still some NA spaces.
aggregate(cbind(DBH) ~ Specie, trees, mean)
trees$DBH[is.na(trees$DBH)] <- 16.412500

# Final check of the notes to see if something else need to be considered

#c01vip NA Corylus avellana 8.0 7.000 x4
#c01vip NA Corylus avellana 8.5 7.000 x5
# this 2 have something extra to be edited, 
# due to the formula to be applied its better to do it to the volume directly

summary(trees) # to get mean DBH for report
describe(trees)

#count of trees per plot? or variety within plots?

p <- ggplot(trees, aes(x=reorder(plot, DBH, mean), y=DBH, fill=Specie)) +
  geom_boxplot(position=position_dodge(0.8))+ theme_minimal()+
  theme(axis.text.x = element_text(angle =90),
        panel.grid = element_line(), legend.position = "bottom")+
  guides(fill=guide_legend(title= "Species", nrow = 4,
                           keyheight  = 0.6, keywidth = 0.5,
                           bycol=TRUE))+
  labs (x = "Plots", y = "DBH (cm)")
p
s  <-
  ggplot(trees, aes (x= reorder(Specie, DBH, mean), y= DBH))+
  geom_boxplot(fill = "#C1CDC1")+
  stat_summary(fun = "mean", geom = "point", shape = 1, colour ="#009966")+
  theme_minimal()+
  labs (x = "Species", y = "DBH (cm)")+
  theme(axis.text.x = element_text(angle =90), axis.title.y = element_text(size=10, margin = margin(t=10)  ),
        legend.position = "right")
s
fig_2 <- ggarrange(p, s, labels = c("A", "B"),  ncol=2,
                   widths = c(2,1))
fig_2

fig_3 <- ggplot(trees, aes(x= fct_infreq(plot), y= fct_infreq(Specie)))+
  geom_count(aes(color = after_stat(n), size = after_stat(n)), show.legend = TRUE, position='identity')+
  guides(color = guide_legend(title = "Individuals\nCount"), size =guide_legend(title = "Individuals\nCount"))+ 
  theme_minimal()+ theme(axis.text.x = element_text(angle =90))+
  xlab("Plots") + ylab("Specie")

fig_3
# now the start of calculations #
sp_plots <- aggregate(cbind(DBH) ~ Specie, 
                      trees, mean)

#apply cone volume (m3) formula to then use BEF (V = r^2 * h * pi/3)
trees$volum <- (function(a, b) {(a/200)^2 * b * pi/3}) (trees$DBH, trees$h) #in m3
#correct the 2 species from the notes.
colnames(trees)

###  #not fixed yet!
# fur x5 = 0.013240504 fur x4 = 0.011728613
0.013240504* 5 -> [1] 0.06620252
0.011728613 *4 -> [1] 0.04691445

trees$volum <- transform(trees, volum = as.numeric(volum))

trees$volum[trees$notes == "x5"] <- 0.06620252

trees$volum[trees$notes == "x4"] <- 0.04691445

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

aggregate(volum ~ Specie, trees_p, mean)
#highest volumes: Quercus robur 22.76773054 | Alnus glutinosa 22.47961230 |  Fagus sylvatica 21.86365162

specie <- read.csv2("trees_species_den_Cf.csv") #saved from numbers app in mac so is separated by ; not coma.

colnames(specie)
specie <- specie %>% select(-ref.density, -ref.Cfraction)
colnames(specie)[2] <- "wd"
specie <- specie[ -28,] #empty row

library(tidyverse)
specie %>% add_row(Specie= pwd = "450", Cfraction = "0.5")

trees_p$wd <- specie$wd[match(trees_p$Specie, specie$Specie)]
trees_p$Cfraction <- specie$Cfraction[match(trees_p$Specie, specie$Specie)]
trees_p <- trees_p %>% 
  dplyr::mutate(wd = ifelse(
    str_detect(trees_p$Specie, "Alnus glutinosa"), 439, wd))
trees_p <- trees_p %>% 
  dplyr::mutate(Cfraction = ifelse(
    str_detect(trees_p$Specie, "Alnus glutinosa"), 0.5, Cfraction))

# calculate AGB volume (m3) * wood density (kg/m3) * BEF
trees_p$AGB <- trees_p$volum * trees_p$wd / 1000 # (/1000) transformed to tn?

#BEF pines or spruce 1.3  (pine and picea) and for broadleaf 1.4 (all the rest of species)
trees_p$AGB <- ifelse (trees_p$Specie == "Pinus sylvestris" | trees_p$Specie == "Picea abies"
| trees_p$Specie == "Larix decidua", 
               AGB * 1.3, 
               AGB * 1.4)
#take data for the report. E.g. species with higher Biomass and cstock (then repeat after ha)
aggregate(cbind(AGB) ~ Specie, trees_p, sum)
# THE HIGHETS:  Fagus sylvatica 21.57942415 |  Quercus robur 18.96551954 | Alnus glutinosa 13.81596972

## add areas depending on plot size (ha) transect 20m and circular plot 10m radios
trees_p$area <- rep(c(0.006), rep = 60)

trees_p <- trees_p %>% 
  dplyr::mutate(area = ifelse(
    str_detect(trees_p$plot, "vip"), 0.031416, area))

## faltan carbon fraction of species still ##

# to estimate carbon content AGB * Cfraction / area to make it carbon stock
trees_p$Cstock <- (AGB * trees_p$Cfraction) #tn of carbon
trees_p$C_tn_ha <- (AGB * trees_p$Cfraction)/trees_p$area # ton
trees_p$vol_ha <- trees_p$volum/trees_p$area
trees_p$AGB_ha <- trees_p$AGB/trees_p$area

#take data for the report. E.g. species with higher Biomass and cstock (then repeat after ha)
aggregate(cbind(AGB_ha) ~ Specie, trees_p, mean)
#Acer pseudoplatanus 1024.114907 |   Quercus robur  746.092277 | Fagus sylvatica  686.892798 (for the sum of them)
# Quercus rubra 683.324324 | Pseudotsuga menziesii 504.422557 (for the mean of them)
aggregate(cbind(C_tn_ha) ~ Specie, trees_p, mean)
# Acer pseudoplatanus 342.3469831 | Quercus robur 266.4615274 |  Quercus rubra 244.0444014

pal16 <- c("#996633", "#999999", "#9999CC", "#E69F00", "#ccff66",
            "#EEB99F", "#9966ff", "#F0E442", "#56B4E9", "#336600",
           "#999900", "#ff6666", "#CC79A7", "#009E73",
  "#990000", "#000099")
trees_sp <- aggregate(DBH ~ Specie, trees, mean)
names(pal16) = trees_sp$Specie

B <- ggplot(trees_p, aes(x= reorder(plot, AGB_ha, FUN=sum), y=AGB_ha, fill= Specie))+
  geom_bar(position='stack', stat='identity')+
  ylab("Biomass (t/ha)")+
  xlab("Plots")+
  theme(axis.text.x = element_text(angle =90), 
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line (colour= "grey80"))+
  scale_fill_manual(values= pal16 )+
  guides(fill=guide_legend(ncol= 1, bycol=TRUE, keyheight  = 0.6, keywidth = 0.5, bycol=TRUE))
B
C <- ggplot(trees_p, aes(x= reorder(plot, C_tn_ha, FUN=sum), y=C_tn_ha, fill= Specie))+
  geom_bar(position='stack', stat='identity')+
  ylab("Carbon stock (tC/ha)")+
  xlab("Plots")+
  theme(axis.text.x = element_text(angle =90), 
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line (colour= "grey80"))+
  scale_fill_manual(values=  pal16)+
  guides(fill=guide_legend(ncol= 1, bycol=TRUE, keyheight  = 0.6, keywidth = 0.5, bycol=TRUE))
C
fig_1 <- ggarrange(B, C, labels = c("A", "B"), align = "v",
                   widths = c(1,1), common.legend = TRUE, legend="right",  ncol =2)
fig_1

trees_p$pool <- rep(c("standing"), rep = 60)

#edit to combine with all the rest for nadine (data frame = df_t)
t_estim_p <- trees_p %>% select(-3,-4,-5,-7,-8)
t_estim_p$type <- rep(c("live trees"), rep = 60)
colnames(t_estim_p) #to work in QGIS at some point

# I need atribute table to see plots that go together in a polygon 
# then make an average per tree sp for the polygon
#25.10.23
# I can make a graph from this or show table =)

df_t <- aggregate(cbind(AGB, Cstock, C_tn_ha, vol_ha) ~ plot, trees_p, sum) #for nadine
df_t$type <- rep(c("live trees"), rep = 32)

# add with GPS and Shape # 
gps <- read.csv("terreno_woodify_GPS.xlsx - GPS.csv")

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


### calculations for rejuventation ###

rejuv <- read.csv("R__rejuvenation_terreno_obk.xlsx - DATA.csv")
names(rejuv) <- as.character(rejuv[1,]) #Copy 1st row to header:
rejuv <- rejuv[-1,] #Delete 1st row :
rejuv <- rejuv[,-1] #delete first colum that is emty


# change info for calculations
rejuv$height <- ifelse(rejuv$h == "1",
                       rejuv$height <- 1.1,
                       rejuv$height <- 2.5)
rejuv$diam <- ifelse(rejuv$h == "1",
                     rejuv$diam <- 3,
                     rejuv$diam <- 5)
# see invetoried species and fix details
colnames(rejuv)[2] <- "specie"
colnames(rejuv)[3] <- "count"
colnames(rejuv)[1] <- "plot"

colnames(rejuv)

aggregate(count ~ specie + plot, rejuv, sum)

rejuv <- transform(rejuv, count = as.numeric(count))
rejuv <- transform(rejuv, Browsing = as.numeric(Browsing))
rejuv <- transform(rejuv, height = as.numeric(height))
rejuv <- transform(rejuv, diam = as.numeric(diam))

summary(rejuv)
describe(rejuv)
sum(rejuv$count, na.rm = TRUE)
####. 2041 trees where counted

aggregate(count ~ specie, rejuv, mean)
aggregate(count ~ plot, rejuv, mean)
## 25 dif plot 

aggregate(height ~ specie, rejuv, mean)

## corregir esto
rejuv$specie <- gsub("FALSE DATA", "Unknown",rejuv$specie)
rejuv$specie <- gsub("nn2", "Unknown",rejuv$specie)
rejuv$specie <- gsub("\\?", "Unknown",rejuv$specie)

aggregate(height ~ specie, rejuv, mean)
#17 dif. species

pal17 <- c("#996633", "#999999", "#9999CC", "#E69F00", "#ccff66",
           "#FFCCCC", "#6633CC", "#F0E442", "#56B4E9", "#336600",
           "#999900", "#ff6666", "#CC79A7", "#009E73",
           "#990000", "#000099", "#CCCCFF" )

#volum estimation same cone formula than for trees
rejuv$vol <- (function(a, b) {(a/200)^2 * b * pi /3}) (rejuv$diam, rejuv$height)

rejuv$vol <- (function(a, b) {a * b}) (rejuv$count, rejuv$vol)

#start other calculations + info for report (some numbers)
rejuv_p <- aggregate(vol ~ specie + plot, rejuv, sum)
rejuv_c <- aggregate(count ~ specie + plot, rejuv, sum)
summary(rejuv_c)
describe(rejuv_c)
aggregate(count ~ specie, rejuv, sum)

#not really working, because of extra count in count colum.
ggplot(rejuv, aes(x=fct_infreq(plot), y= rejuv$specie*rejuv$count))+
  geom_count(aes(color = after_stat(n), size = after_stat(n)), show.legend = TRUE, position='identity')+
  guides(color = guide_legend(title = "Individuals\nCount"), size =guide_legend(title = "Individuals\nCount"))+ 
  theme_minimal()+ theme(axis.text.x = element_text(angle =90))+
  xlab("Plots") + ylab("Specie")

#managed a fixed for it in theory, but bellow 100 not nice visuals
install.packages("stats")
library("stats")

ggplot(rejuv_c, aes(x= fct_infreq(plot), y= fct_infreq(specie)))+
  geom_count(aes(color = count, size = count), show.legend = TRUE, position='identity')+
  guides(color = guide_legend(title = "Individuals\nCount"), size =guide_legend(title = "Individuals\nCount"))+ 
  theme_minimal()+ theme(axis.text.x = element_text(angle =90))+
  xlab("Plots") + ylab("Specie")

## plot the amount of rejuvenation happening, type of specie and amount of browsing
colnames(rejuv)

rejuv <- transform(rejuv, Browsing = as.numeric(Browsing))
reju_count <- aggregate(count ~ specie + plot + h + Browsing, rejuv, sum)

rejuv_22 <- rejuv[which(rejuv$h == "2"),]
rejuv_2 <- rejuv[which(rejuv$h == "1"),]

a <- ggplot(rejuv_2, aes(x= reorder(specie, count, sum), y = (count), fill= Browsing))+
  geom_bar(stat="identity", position = 'dodge')+
  theme_minimal()+theme(axis.text.x = element_text(angle =90))+
  ylab("Count")+
  xlab("Trees <2 metres")
a
b <- ggplot(rejuv_22, aes(x= reorder(specie, count, sum), y = count, fill= Browsing))+
  geom_bar(stat="identity", position = 'dodge')+
  theme_minimal()+theme(axis.text.x = element_text(angle =90))+
  ylab("Count")+
  xlab("Trees >2 metres")
b
fig_6 <- ggarrange(a, b, labels= c("A", "B"), 
                   common.legend = TRUE, 
                   legend = "right", 
                   label.x= 0 ,ncol = 2)
fig_6

# first time density - match table (density and carbon fraction) with the corresponding species
## younger species normally havea different density than older trees* as well as carbon fraction most likelly
# for research reasons, check on IPCC guide lines or something else *!! ####

rejuv_p$wd <- specie$wd[match(rejuv_p$specie, specie$Specie)]
rejuv_p$Cfraction <- specie$Cfraction[match(rejuv_p$specie, specie$Specie)]

###
#### correct the dw and c frantion for the missing ones.####
###

rejuv_p <- transform(rejuv_p, wd = as.numeric(wd))
rejuv_p <- rejuv_p %>% group_by (plot) %>% 
  mutate_at(vars(wd), ~replace_na(., mean(., na.rm = TRUE)))
rejuv_p <- transform(rejuv_p, wd = as.numeric(wd))

rejuv_p <- rejuv_p %>% group_by (plot) %>% 
  mutate_at(vars(Cfraction), ~replace_na(., 0.500))
rejuv_p <- transform(rejuv_p, Cfraction = as.numeric(Cfraction))

rejuv_p$AGB <- rejuv_p$vol* rejuv_p$wd
##aply biomas expansion factor ?¿* is it needed** ##
rejuv_p$AGB <- ifelse(rejuv_p$specie == "Pinus sylvestris" | rejuv_p$specie == "Picea abies"
              | rejuv_p$specie == "Larix decidua", 
              rejuv_p$AGB * 1.3, 
              rejuv_p$AGB * 1.4) 

# Cstock of all
rejuv_p$Cstock <- rejuv_p$AGB * rejuv_p$Cfraction 

## by hectare ##
rejuv_p$area <- rep(c(0.006), rep = 99)

rejuv_p <- rejuv_p %>% 
  dplyr::mutate(area = ifelse(
    str_detect(rejuv_p$plot, "vip"), 0.031416, area))

rejuv_p$C_tn_ha <- rejuv_p$Cstock/ rejuv_p$area

rejuv_p$AGB_ha <- rejuv_p$AGB/ rejuv_p$area

colnames(rejuv_p)
cs <- ggplot(rejuv_p, aes(x= reorder(plot, C_tn_ha, FUN=sum), y=C_tn_ha, fill= specie))+
  geom_bar(position='stack', stat='identity')+
  ylab("Carbon stock (tC/ha)")+
  xlab("Plots")+
  theme(axis.text.x = element_text(angle =90), 
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line (colour= "grey80"))+
  scale_fill_manual(values= c("#996633", "#56B499", "#9999CC", "#E69F00", "#ccff66",
                              "#999999",  "#9966ff", "#F0E442", "#56B4E9", "#336600",
                              "#999900", "#009E73", "#ff6666", "#CC79A7", 
                              "#990000", "#000099", "#EEB99F") )+
  guides(fill=guide_legend(ncol= 1, bycol=TRUE, keyheight  = 0.6, keywidth = 0.5, bycol=TRUE))
cs
ab <- ggplot(rejuv_p, aes(x= reorder(plot, AGB_ha, FUN=sum), y=AGB_ha, fill= specie))+
  geom_bar(position='stack', stat='identity')+
  ylab("Aboveground biomass (t/ha)")+
  xlab("Plots")+
  theme(axis.text.x = element_text(angle =90), 
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line (colour= "grey80"))+
  scale_fill_manual(values= c("#996633", "#56B499", "#9999CC", "#E69F00", "#ccff66",
                              "#999999",  "#9966ff", "#F0E442", "#56B4E9", "#336600",
                              "#999900", "#009E73", "#ff6666", "#CC79A7", 
                              "#990000", "#000099", "#EEB99F") )+
  guides(fill=guide_legend(ncol= 1, bycol=TRUE, keyheight  = 0.6, keywidth = 0.5, bycol=TRUE))

ab
fig_7 <- ggarrange(ab, cs, labels= c("A", "B"), 
                   common.legend = TRUE, 
                   legend = "right", 
                   label.x= 0 ,ncol = 2)
fig_7
#data about this ones in numbers
aggregate(AGB_ha ~ specie, rejuv_p, mean)
#. highest sp contribution:  Abies alba 967.46336; Corylus avellana 975.09432 Y& Acer pseudoplatanus 813.19822  Picea abies 764.37375
aggregate(C_tn_ha ~ specie, rejuv_p, mean)
# Abies alba 483.73168 and  Corylus avellana 487.54716 & Acer pseudoplatanus 380.57677 and  Picea abies 382.95125

rejuv_p$type <- rep(c("rejuv"), rep = 99)


##### Dead wood data analysis ####

deadwood <- read.csv("R_deadwood_terreno_obk.xlsx - DATA.csv")
names(deadwood) <- as.character(deadwood[1,]) #Copy 1st row to header:
deadwood <- deadwood[-1,] #Delete 1st row :
deadwood <- deadwood[,-1] #delete first colum that is emty

summary(deadwood)
colnames(deadwood)
deadwood$L <- str_replace_all(deadwood$L, ",", ".")
deadwood$d_1 <- str_replace_all(deadwood$d_1, ",", ".")
deadwood$d_2 <- str_replace_all(deadwood$d_2, ",", ".")

deadwood <- transform(deadwood, L = as.numeric(L)) 
deadwood <- transform(deadwood, d_1 = as.numeric(d_1)) 
deadwood <- transform(deadwood, d_2 = as.numeric(d_2)) 


colnames(deadwood)[1] <- "plot"
aggregate(L ~ plot,stan_dw, mean)

#Laying without roots
ly_dw <- filter(deadwood, d_2 != 0 & notes != "standing" 
                &               notes != "roots" & notes !="standing (7)" 
                & stumps != 1 & stumps != 2 & stumps != 3 & stumps !="standing" & stumps != "2-3")

#& (las que no tienen valor 0 en d_2 y no notas no dice standing)
# Stumps
ly_s_dw <- filter(deadwood, stumps == 1 | stumps == 2 | stumps == 3 | stumps == "2-3")

# Lying but only the ones with roots
ly_r_dw <- filter(deadwood, notes == "roots")

# Standing deadwood trees
stan_dw <- filter(deadwood, d_2 == 0 | notes == "standing" 
                   | d_1 == 0   | notes =="standing (7)"  
                    )

#670 observations 
# stumps 225
# standin dw 64
# leying 375
# leying with roots 6
225+ 64 + 375 + 6

aggregate(L ~ plot,stan_dw, mean)

deadwood <- substitute(",", ".")

ly_r_dw <- transform(ly_r_dw, L = as.numeric(L)) 
ly_dw <- transform(ly_dw, L = as.numeric(L))
stan_dw <- transform(stan_dw, L = as.numeric(L))
stan_dw <- transform(stan_dw, d_1 = as.numeric(d_1))

### working with ly_dw ####

#pi(d1^2 + d2^2)*h/8 (Smalian formula)
d12 <- (function(a, b) {((a/100)^2) + ((b/100)^2) }) (ly_dw$d_1, ly_dw$d_2) #in m
ly_dw$vol <- (function(a, b) {a * (b)* pi / 8}) (d12, ly_dw$L)  # L in m
 #vol in m3

# Density of Quercus and Fagus Sylvatica is 0.58 and Picea abies 0.40(tonnes dry matter/ m3 of fresh vol
#Biomas for vol (tn ??)
ly_dw$AGB <- ifelse (ly_dw$type == "b", 
               ly_dw$vol * 0.952, 
               ly_dw$vol * 0.611)

#decomposition state
#for conifers :
# just diead 1 - 0.37 
# onset decomp. 2 - 0.31
# advanced state 3 -  0.14
# heavily rotter 4 - 0.12
# broadleaf:
  # 1 - 0.58
  # 2 - 0.37
  # 3 - 0.21
  # 4 - 0.12

ly_dw$AGB <- if(any(ly_dw$type =="c" & ly_dw$decay_state == "0"))  {ly_dw$AGB * 0.37 
   } else if(ly_dw$type =="c" & ly_dw$decay_state == "1") { ly_dw$AGB * 0.31 
   } else if(ly_dw$type =="c" & ly_dw$decay_state == "2") { ly_dw$AGB * 0.14
   } else if(ly_dw$type =="b" & ly_dw$decay_state == "0")  {ly_dw$AGB * 0.58 
} else if(ly_dw$type =="b" & ly_dw$decay_state == "1") { ly_dw$AGB * 0.37 
} else { ly_dw$AGB * 0.21}
  
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

ly_r_dw$AGB <- if(any(ly_r_dw$type =="c" & ly_r_dw$decay_state == "0"))  {ly_r_dw$AGB * 0.37 
} else if(ly_r_dw$type =="c" & ly_r_dw$decay_state == "1") { ly_r_dw$AGB * 0.31 
} else if(ly_r_dw$type =="c" & ly_r_dw$decay_state == "2") { ly_r_dw$AGB * 0.14
} else if(ly_r_dw$type =="b" & ly_r_dw$decay_state == "0")  {ly_r_dw$AGB * 0.58 
} else if(ly_r_dw$type =="b" & ly_r_dw$decay_state == "1") { ly_r_dw$AGB * 0.37 
} else { ly_r_dw$AGB * 0.21}

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
ly_s_dw <- ly_s_dw %>% 
  mutate(decay_state = ifelse(
    str_detect(ly_s_dw$decay_state, "2-3"), "2", decay_state))
ly_s_dw <- ly_s_dw %>% 
  mutate(type = ifelse(
    str_detect(ly_s_dw$stumps, "2-3"), "nn", type))
ly_s_dw <- ly_s_dw %>% 
  mutate(stumps = ifelse(
    str_detect(ly_s_dw$stumps, "2-3"), "3", stumps))


n <- rep(c(1), rep = 225)
stumps <- cbind(ly_s_dw, n)
size <- rep(c("6 - 20"), rep = 225) #en cm - rep given the 220 obs in data frame
stumps <- cbind (stumps, size)
stumps_d <- as.data.frame(stumps)

# columna de deje la caga, asique no usarla en stump mode #
#para graph de stumps
stumps_d <- stumps_d %>% 
  mutate(size = ifelse(
    str_detect(stumps_d$stumps, "2"), "20 - 40", size))
stumps_d <- stumps_d %>% 
  mutate(size = ifelse(
    str_detect(stumps_d$stumps, "3"), "40+", size))

# eliminate the extra rows to have only the count per size types and plot
## fix ##

stumps_d <- stumps_d[,-2]
stumps_d <- stumps_d[,-2]
stumps_d <- stumps_d[,-2]

ggballoonplot(stumps_d, x= "plot", y="size", fill = "n")


D <- rep(c(13), rep = 225) #en cm - rep given the 225 obs in data frame
ly_s_dw <- cbind (ly_s_dw, D)

ly_s_dw <- ly_s_dw %>% 
  mutate(D = ifelse(
    str_detect(ly_s_dw$stumps, "2"), 30, D))
ly_s_dw <- ly_s_dw %>% 
  mutate(D = ifelse(
    str_detect(ly_s_dw$stumps, "3"), 45, D))

#adding height depending on stump category
H <- rep(c(0.15), rep = 225) #en m
ly_s_dw <- cbind (ly_s_dw, H)

ly_s_dw <- ly_s_dw %>% 
  mutate(H = ifelse(
    str_detect(ly_s_dw$stumps, "2"), 0.20, H))
ly_s_dw <- ly_s_dw %>% 
  mutate(H = ifelse(
    str_detect(ly_s_dw$stumps, "3"), 0.35, H))

#for volume apply cilinder formula r2*pi*H
ly_s_dw$vol <- (function (a,b) {((a/200)^2)* b * pi}) (ly_s_dw$D, ly_s_dw$H) #en m3


# biomass calculation
ly_s_dw$AGB <- ifelse (ly_s_dw$type == "b", 
               ly_s_dw$vol * 0.952, 
               ly_s_dw$vol * 0.611)

ly_s_dw$AGB <- if(any(ly_s_dw$type =="c" & ly_s_dw$decay_state == "0"))  {ly_s_dw$AGB * 0.37 
} else if(ly_s_dw$type =="c" & ly_s_dw$decay_state == "1") { ly_s_dw$AGB * 0.31 
} else if(ly_s_dw$type =="c" & ly_s_dw$decay_state == "2") { ly_s_dw$AGB * 0.14
} else if(ly_s_dw$type =="b" & ly_s_dw$decay_state == "0")  {ly_s_dw$AGB * 0.58 
} else if(ly_s_dw$type =="b" & ly_s_dw$decay_state == "1") { ly_s_dw$AGB * 0.37 
} else { ly_s_dw$AGB * 0.21}
##Cstock
ly_s_dw$Cstock <- ly_s_dw$AGB * 0.5


#### working with standing dead wood ####
# Cone formula vol= r2 * pi * L/3
stan_dw$vol <- (function(a, b) { (((a/200)^2) * b * pi )/3}) (stan_dw$d_1, stan_dw$L) #in m3


#segun type b or c, biomas correction and expansion fatcor (b: 1.4 * vv , c:1.3 * 0.)
stan_dw$AGB <- ifelse (stan_dw$type == "b", 
               stan_dw$vol * 0.952, 
               stan_dw$vol * 0.611)


##Cstock
stan_dw$Cstock <- stan_dw$AGB * 0.5
colnames(deadwood)

# sum by plot (can be plot or more than one grouping category like type (b or c))
# change cal name to biom_s to know better
ly_plot <- aggregate(cbind(vol, AGB, Cstock) ~ plot + decay_state +type, 
                     ly_dw, sum)
ly_r_plot <- aggregate(cbind(vol, AGB, Cstock) ~ plot + decay_state +type, 
                       ly_r_dw, sum)
ly_s_plot <- aggregate(cbind(vol, AGB, Cstock) ~ plot + decay_state +type, 
                       ly_s_dw, sum)
stan_plot <- aggregate(cbind(vol, AGB, Cstock) ~ plot + decay_state +type, 
                       stan_dw, sum)

#rm(stan_plot)
#add extra colum to diferenciate type of CWD
ly_plot$dw_type <- rep(c("logs"), rep = 60) #ly_dw_plot deadwood type logs (lying dw)
ly_r_plot$dw_type <- rep(c("cwd"), rep = 6) #ly_dw_plot deadwood type coarse root debris (CWD) for our case is just logs with roots
stan_plot$dw_type <- rep(c("snag"), rep = 19) #ly_dw_plot deadwood type snags (standing trees)
ly_s_plot$dw_type <- rep(c("stump"), rep = 43) #ly_dw_plot deadwood type stumps

#putting all together again #merge dataframes
install.packages("tidyverse")
library(tidyverse)
# merge 4 parts again -->> dibujar desde aca!
dw <- list(ly_plot, ly_r_plot,stan_plot, ly_s_plot)
dw <- Reduce(function (x, y) merge (x, y, all= TRUE), dw)

#per hectare for dw
dw$Area <- rep(c(0.0060), rep = 128)


dw <- dw %>% 
  dplyr::mutate(Area = ifelse(
    str_detect(dw$plot, "vip"), 0.031416, Area))

#vol per hc and tons/hectare
dw$AGB_ha <- dw$vol / dw$Area
dw$C_tn_ha <- dw$Cstock / dw$Area  # in tones


df_dw <- aggregate(cbind(AGB, Cstock, C_tn_ha, AGB_ha) ~ plot, dw, sum)
df_dw$type <- rep(c("deadwood"), rep = 28)

colnames(ly_plot)
colnames(ly_r_plot)
colnames(ly_s_plot)
colnames(stan_plot)

#fix Na values
colnames(stumps_d)
summary(stumps_d)
stumps_d <- coalesce(stumps_d)
stumps_d <- transform(stumps_d, type = as.numeric(type))

stumps_d$type <- gsub("b", "Broadleaf",stumps_d$type)
stumps_d$type <- gsub("c", "Conifer",stumps_d$type)
stumps_d$type <- gsub("nn", "Unknown",stumps_d$type)
stumps_d$type <- gsub("^$", "Unknown",stumps_d$type)
stumps_d <- transform(stumps_d, type = as.character(type))


aggregate(decay_state ~ plot, deadwood, sum)
deadwood <- transform(deadwood, decay_state = as.numeric(decay_state))
aggregate(decay_state ~ plot, deadwood, mean) # total of 28 plots where measured 
describe(dw, quant = c(0.25,0.50,0.75))
describe.by(dw, group = "dw_type")

x <- ggplot(stumps_d, aes(x= reorder(size, n, sum, decreasing = TRUE), y= n, fill = type))+
  geom_bar(stat="identity")+
  ylab("Count")+
  xlab("Stump Diameter (cm)")+
  scale_fill_hue(name = "Type", labels=c("Broadlleaf", "Conifer", "Unknown"), h =c(0, 360), c =100 )+
  theme_minimal()+theme(legend.position = c(0.77, 0.8))+
  theme(legend.background = element_rect(fill="white"))

x
z <- ggplot(stumps_d, aes(x= reorder(type, n, sum, decreasing = TRUE), y= n, fill = size))+
  geom_bar(stat="identity")+
  ylab("Count")+
  xlab("Type")+
  scale_fill_hue(name = "Diameter (cm)",  h =c(0, 360), c =100 )+
  theme_minimal()+theme(legend.position = c(0.77, 0.8))+
  theme(legend.background = element_rect(fill="white"))

z
  fig_9 <- ggarrange(x, z, labels = c("A", "B"), align = "h",
                   widths = c(1,1))
fig_9

ggplot(dw, aes(x=reorder(plot, AGB_ha, FUN=sum), y= AGB_ha, fill=dw_type))+
  geom_bar(position="stack", stat='identity')+
  theme_minimal()+
  theme(axis.text.x = element_text(angle =90), legend.position = "top")+
  guides(fill=guide_legend(title= "Type", 
                           nrow = 1, keyheight  = 0.6, keywidth = 0.5, bycol=TRUE))+
  xlab("Plots") + ylab("Aboveground biomass (t/ha)")+
  theme(legend.position = c(0.5, 0.9),
        legend.background = element_rect(fill = "white", color = "black"))

ggplot(dw, aes(x=reorder(plot, C_tn_ha, FUN=sum), y= C_tn_ha, fill=dw_type))+
  geom_bar(position="stack", stat='identity')+
  theme_minimal()+
  theme(axis.text.x = element_text(angle =90), legend.position = "top")+
  guides(fill=guide_legend(title= "Type", 
                           nrow = 1, keyheight  = 0.6, keywidth = 0.5, bycol=TRUE))+
  xlab("Plots") + ylab("Carbon pools (tC/ha)")+
  theme(legend.position = c(0.5, 0.9),
        legend.background = element_rect(fill = "white", color = "black"))
describe(dw)
aggregate(C_tn_ha ~ dw_type, dw, mean)
aggregate(C_tn_ha ~ type, dw, mean)


### union of the agb and carbon of each  ###

dw$type <- rep("deadwood", 128)
trees_p$type <- rep("stand", 59)
rejuv_p$type <- rep("rejuvenation", 99)
colnames(dw)
colnames(trees_p)
colnames(rejuv_p)
trees_p <- trees_p[,-10]


r_agb <- aggregate(AGB_ha ~ plot + type, rejuv_p, sum)
dw_agb <- aggregate(AGB_ha ~ plot + type, dw, sum)
t_agb <- aggregate(AGB_ha ~ plot + type, trees_p, sum)


df <- rbind(r_agb, t_agb, dw_agb)

ggplot(df, mapping = aes(fill=type, x= reorder(plot, AGB_ha, FUN=sum), y=AGB_ha)) +
  geom_bar(position='stack', stat='identity')+   ylab("Aboveground (tn/ha)")+ xlab("Plots")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle =90),  legend.position = c(0.4, 0.9),
        legend.background = element_rect(fill = "white", color = "black"))+
  guides(fill=guide_legend(title= "Type", nrow = 1, keyheight  = 0.6, keywidth = 0.5, bycol=TRUE))+
  scale_fill_manual("Components", values=c("stand" = "#b9e38d", 
                                           "deadwood" = "#a1e9f0", 
                                           "rejuvenation" = "#d9b1f0"))
summary(df)
describe(df)

r_cha <- aggregate(C_tn_ha ~ plot + type, rejuv_p, sum)
dw_cha <- aggregate(C_tn_ha ~ plot + type, dw, sum)
t_cha <- aggregate(C_tn_ha ~ plot + type, trees_p, sum)


df_c <- rbind(r_cha, t_cha, dw_cha)
summary(df_c)
colnames(df_c)
ggplot(df_c, mapping = aes(fill=type, x= reorder(plot, C_tn_ha, FUN=sum), y=C_tn_ha)) +
  geom_bar(position='stack', stat='identity')+   ylab("Carbon Stock (tn C/ha)")+ xlab("Plots")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle =90), legend.position = "top")+
  guides(fill=guide_legend(title= "Type", nrow = 1, keyheight  = 0.6, keywidth = 0.5, bycol=TRUE))+
  scale_fill_manual("Components", values=c("stand" = "#b9e38d", 
                                           "deadwood" = "#458B74", 
                                         "rejuvenation" = "#9999FF"))

##3 for C_tn_ha (falta ver para AGB!! compare them)
summary(df) # 646.852   AGB_ha and 
summary(r_cha) # 801.108 C_tn_ha
summary(dw_cha) # 35.0380  C_n_ha
summary(t_cha) # 77.944  fof C__ha

a <- 801.108+35.0380+77.944
a * 337.76 ## what is this values??
a
## MEDIAN OF EACH
summary (r_agb) # 1623.422
summary(t_agb) # 263.507
summary (dw_agb) #    103.496 Q
describe(df)

b <- 1623.422+263.507+103.496
b * 337.76 ## what is this values??
b

###    editado hasta aca, a continuacion es el copy paste de OBK =)    ###
# Create a circular plot ##
data <- data.frame(
  type=c("Deadwood", "Regeneration", "Trees"),
  C_tn_ha=c(35.038, 801.108, 94.740 ))

data$fraction <- data$C_tn_ha / sum(data$C_tn_ha)
data$percentage <- data$fraction * 100
data$percentage <- format(round(data$percentage , 2), nsmall = 2)
data$ymax <- cumsum(data$fraction)
data$ymin <- c(0, head(data$ymax, n=-1))
data$labelPosition <- (data$ymax + data$ymin) / 2
data$label <- paste0(data$percentage, "%")

install.packages("ggrepel")
library(ggrepel)
install.packages("ggthemes")
library(ggthemes)
install.packages("paletteer")
library (paletteer)


pie <- ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=type)) +
  geom_rect() +
  geom_label(y=1, aes(x=labelPosition, label=label), size = 3)+
  scale_fill_manual(values=c("Trees" = "#b9e38d", 
                             "Deadwood" = "#458B74", 
                             "Regeneration" = "#9999FF"))+
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")
pie


# fome de ver, me trae duda los valores de rejuvenaiton..
#have to check the density values to double check units
# in the formula


#tests#####

ggplot(dw, aes(x= reorder(plot, AGB_ha, FUN=sum), y=AGB_ha, fill= specie))+
  geom_bar(position='stack', stat='identity')+
  ylab("Aboveground biomass (t/ha)")+
  xlab("Plots")+
  theme(axis.text.x = element_text(angle =90), 
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line (colour= "grey80"))+
  scale_fill_manual(values= c("#996633", "#56B499", "#9999CC", "#E69F00", "#ccff66",
                              "#999999",  "#9966ff", "#F0E442", "#56B4E9", "#336600",
                              "#999900", "#009E73", "#ff6666", "#CC79A7", 
                              "#990000", "#000099", "#EEB99F") )+
  guides(fill=guide_legend(ncol= 1, bycol=TRUE, keyheight  = 0.6, keywidth = 0.5, bycol=TRUE))

