####################################################################
# Make diagnostic plots for landis models 
library(ggplot2)
library(data.table)
library(cowplot)
library(MetBrewer)
library(raster)
library(sf)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
# Start with the flux tower diagnostics 
updated_folder <- "/Volumes/GoogleDrive/My Drive/Chapter3/Models/Landis_Flux_Tower_Diagnostics"
start_year <- 1999 

#########################################################################################################
# Assess biomass by age in each year of the simulation 

cal_log <- fread(paste0(updated_folder, "/NECN-calibrate-log.csv"))
cal_log$Year <- as.factor(cal_log$Year)
# Distribution of biomass ages in each year of simulation
ggplot(cal_log[Year %in% seq(0, 50, 5),]) + 
  geom_boxplot(aes(x=Year, y=CohortAge), fill="Light Green") + 
  theme_classic()
# Same but broken down by species 
ggplot(cal_log[Year %in% seq(0, 50, 5),]) + 
  geom_boxplot(aes(x=Year, y=CohortAge), fill="Light Green") + 
  theme_classic() + 
  facet_wrap(~SpeciesName)

# Check regneration rates by species 
pest <- fread(paste0(updated_folder, "/NECN-prob-establish-log.csv"))
ggplot(pest) + 
  geom_boxplot(aes(x=SpeciesName, y=AvgProbEst), fill="Light Green") + 
  theme_bw()
pest_summary <- pest[,.(AvgProbEst=mean(AvgProbEst, na.rm=T)), .(Time, SpeciesName)]
ggplot(pest_summary) + 
  geom_point(aes(x=Time, y=AvgProbEst), color="Dark Green") + 
  #geom_line(aes(x=Time, y=AvgProbEst), color="Black") + 
  theme_bw() + 
  facet_wrap(~SpeciesName)


#########################################################################################################
# Plot C and N Pools over time 
# plot the som c and n for all pools 
# plot the ratio of n:c for all pools over time to see how those change over time 
annual_log <- fread("/Volumes/GoogleDrive/My Drive/Chapter3/Models/Landis_Flux_Tower_Diagnostics/NECN-Succession-log.csv")
(annual_log$SOMTC[nrow(annual_log)] - annual_log$SOMTC[1])/annual_log$SOMTC[1]
(annual_log$C_SOM1soil[nrow(annual_log)] - annual_log$C_SOM1soil[1])/annual_log$C_SOM1soil[1]
(annual_log$C_SOM1surf[nrow(annual_log)] - annual_log$C_SOM1surf[1])/annual_log$C_SOM1surf[1]
(annual_log$C_SOM2[nrow(annual_log)] - annual_log$C_SOM2[1])/annual_log$C_SOM2[1]
(annual_log$C_SOM3[nrow(annual_log)] - annual_log$C_SOM3[1])/annual_log$C_SOM3[1]

ndep <- mean(annual_log[Time > 10, ]$TotalNdep)
ndep
nvol <- mean(annual_log[Time > 10, ]$Nvol)
nvol

annual_log$CN_soil1 <- annual_log$C_SOM1soil/annual_log$N_SOM1soil
annual_log$CN_surf1 <- annual_log$C_SOM1surf/annual_log$N_SOM1surf
annual_log$CN_2 <- annual_log$C_SOM2/annual_log$N_SOM2
annual_log$CN_3 <- annual_log$C_SOM3/annual_log$N_SOM3

n1surf <- ggplot(annual_log) + 
  geom_line(aes(x=Time, y=N_SOM1surf)) + 
  theme_bw() + 
  ylim(0, 600) + 
  xlab("Year")
n1soil <- ggplot(annual_log) + 
  geom_line(aes(x=Time, y=N_SOM1soil)) + 
  theme_bw()+ 
  ylim(0, 600) + 
  xlab("Year")
n2 <- ggplot(annual_log) + 
  geom_line(aes(x=Time, y=N_SOM2)) + 
  theme_bw()+ 
  ylim(0, 600) + 
  xlab("Year")
n3 <- ggplot(annual_log) + 
  geom_line(aes(x=Time, y=N_SOM3)) + 
  theme_bw()+ 
  ylim(0, 600) + 
  xlab("Year")
c1surf <- ggplot(annual_log) + 
  geom_line(aes(x=Time, y=C_SOM1surf)) + 
  theme_bw() + 
  ylim(0, 8500) + 
  xlab("Year")
c1soil <- ggplot(annual_log) + 
  geom_line(aes(x=Time, y=C_SOM1soil)) + 
  theme_bw()+ 
  ylim(0, 8500) + 
  xlab("Year")
c2 <- ggplot(annual_log) + 
  geom_line(aes(x=Time, y=C_SOM2)) + 
  theme_bw()+ 
  ylim(0, 8500) + 
  xlab("Year")
c3 <- ggplot(annual_log) + 
  geom_line(aes(x=Time, y=C_SOM3)) + 
  theme_bw()+ 
  ylim(0, 8500) + 
  xlab("Year")

plot_grid(n1surf, n1soil, n2, n3, 
          c1surf, c1soil, c2, c3, nrow=2)

ggplot(annual_log) + 
  geom_line(aes(x=Time, y=SOMTC)) + 
  theme_bw() + 
  ylim(9000, 15000) + 
  xlab("Year") + 
  ylab("SOMTC (gC m-2)")



#########################################################################################################
# Plot mineral N 
annual_log <- annual_log[!Time == 0,]

ggplot(annual_log) + 
  geom_line(aes(x=Time, y=MineralN)) + 
  theme_bw() + 
  ylim(0, 9) + 
  xlab("Year") + 
  ylab("Mineral N (gC m-2)")

mean(annual_log$MineralN)

#########################################################################################################
# Plot NEE, NPP, ET, AGB
ggplot(annual_log) + 
  geom_line(aes(x=Time, y=NEEC)) + 
  theme_bw() + 
  xlab("Year") + 
  ylab("NEE (gC m-2 yr-1)")

mean(annual_log$NEEC)

annual_log$ET <- annual_log$AnnualTrans + annual_log$AnnualEvaporation
annual_log$ET <- annual_log$ET * 10
ggplot(annual_log) + 
  geom_line(aes(x=Time, y=ET)) + 
  theme_bw() + 
  ylim(0, 800) + 
  xlab("Year") + 
  ylab("ET (mm m-2 yr-1)")

mean(annual_log$ET)

ggplot(annual_log) + 
  geom_line(aes(x=Time, y=annual_log$AG_NPPC)) + 
  theme_bw() + 
  ylim(0, 1350) + 
  xlab("Year") + 
  ylab("AG-NPP (g C m-2 yr-1)")

mean(annual_log$AG_NPPC)

ggplot(annual_log) + 
  geom_line(aes(x=Time, y=annual_log$AGB)) + 
  theme_bw() + 
  xlab("Year") + 
  ylim(0, 30000) + 
  ylab("AGB (g Biomass m-2)")

mean(annual_log$AGB)


#########################################################################################################
# LAI
cal_log <- fread(paste0(updated_folder, "/NECN-calibrate-log.csv"))
lai <- cal_log[Month ==6,.(LAI=sum(ActualLAI)), .(Year, Month)]
ggplot(lai) + 
  geom_line(aes(x=Year, y=LAI)) + 
  theme_bw() + 
  ylim(0, 12) + 
  xlab("Year") + 
  ylab("LAI (m2 m-2)")

mean(lai$LAI)
#########################################################################################################
# Species Biomass change over time 

spp <- c("QuerAlba", "AcerRubr", "LiriTuli", "BetuLent", "Cary")
total_monthly <- cal_log[,.(Transpiration=sum(Transpiration), 
                            NPP=sum(ActualLeafANPP + ActualWoodANPP),
                            Biomass=sum(CohortBiomass), 
                            GrowthLimitLAI=mean(GrowthLimitLAI), 
                            GrowthLimitSoilWater=mean(GrowthLimitSoilWater), 
                            GrowthLimitT=mean(GrowthLimitT), 
                            GrowthLimitN=mean(GrowthLimitN), 
                            GrowthLimitLAIcompetition=mean(GrowthLimitLAIcompetition)), .(Year, Month, SpeciesName)]

total_monthly$Date <- as.Date(paste0(total_monthly$Year+start_year, sprintf("%02d", total_monthly$Month), "01"), format="%Y%m%d")
total_monthly <- total_monthly[SpeciesName %in% spp,]
total_monthly$SpeciesName <- as.factor(total_monthly$SpeciesName)
total_monthly$SpeciesName <- factor(total_monthly$SpeciesName, levels=c("LiriTuli", "BetuLent", "AcerRubr", "Cary", "QuerAlba"))
total_monthly_long <- melt(total_monthly, id.vars=c("Year", 'Month', 'Date', 'SpeciesName'), measure.vars=c('Transpiration', 'NPP','Biomass','GrowthLimitLAI', 
                                                                                                            'GrowthLimitSoilWater', 'GrowthLimitT',
                                                                                                         'GrowthLimitN', 'GrowthLimitLAIcompetition'))
ggplot(total_monthly) + 
  geom_line(aes(x=Date, y=Biomass, color=SpeciesName), size=1, alpha=0.7) + 
  theme_bw() + 
  scale_color_manual(values=met.brewer("Hokusai1", n=5),labels=c("Tulip Poplar", "Sweet Birch", "Red Maple", "Hickory", "White Oak"))+
  xlab("Date") + 
  ylab("Biomass") + 
  theme(legend.position = c(0.89, 0.70)) + 
  theme(legend.title=element_blank())



##############################################################################################################
## Plot growth limitations 
ggplot(total_monthly[year(Date) > 2012 & year(Date) < 2015,]) + 
  geom_point(aes(x=Date, y=GrowthLimitLAI)) + 
  geom_line(aes(x=Date, y=GrowthLimitLAI)) + 
  facet_wrap(~SpeciesName) + 
  theme_bw()


ggplot(total_monthly[year(Date) > 2012 & year(Date) < 2015,]) + 
  geom_point(aes(x=Date, y=GrowthLimitSoilWater)) + 
  geom_line(aes(x=Date, y=GrowthLimitSoilWater)) + 
  facet_wrap(~SpeciesName) + 
  theme_bw()

ggplot(total_monthly[year(Date) > 2012 & year(Date) < 2015,]) + 
  geom_point(aes(x=Date, y=GrowthLimitT)) + 
  geom_line(aes(x=Date, y=GrowthLimitT)) + 
  facet_wrap(~SpeciesName) + 
  theme_bw()

ggplot(total_monthly[year(Date) > 2012 & year(Date) < 2015,]) + 
  geom_point(aes(x=Date, y=GrowthLimitN)) + 
  geom_line(aes(x=Date, y=GrowthLimitN)) + 
  facet_wrap(~SpeciesName) + 
  theme_bw()


##############################################################################################################
## Plot relative species water use 
total_monthly <- cal_log[,.(Transpiration=sum(Transpiration), 
                            NPP=sum(ActualLeafANPP + ActualWoodANPP),
                            Biomass=sum(CohortBiomass), 
                            GrowthLimitLAI=mean(GrowthLimitLAI), 
                            GrowthLimitSoilWater=mean(GrowthLimitSoilWater), 
                            GrowthLimitT=mean(GrowthLimitT), 
                            GrowthLimitN=mean(GrowthLimitN), 
                            GrowthLimitLAIcompetition=mean(GrowthLimitLAIcompetition)), .(Year, Month, SpeciesName)]

total_monthly$Date <- as.Date(paste0(total_monthly$Year+start_year, sprintf("%02d", total_monthly$Month), "01"), format="%Y%m%d")
total_monthly$SpeciesName <- as.factor(total_monthly$SpeciesName)
total_monthly$Fraction <- total_monthly$Transpiration/total_monthly$Biomass
summary_fraction <- total_monthly[!Fraction==0.0,.(Fraction=mean(Fraction)), .(SpeciesName)]
summary_fraction$Fraction <- summary_fraction$Fraction * 10000
species_table <- fread("/Volumes/GoogleDrive/My Drive/Chapter3/SetUp/InitialCommunities/RileyFC/final_spp_table.csv")
summary_fraction <- merge(summary_fraction, species_table[,c("LandisCode", "Type")], by.x="SpeciesName", by.y="LandisCode", all.x=T)
summary_fraction$SpeciesName <- as.factor(summary_fraction$SpeciesName)
summary_fraction$SpeciesName <- factor(summary_fraction$SpeciesName, levels=c("AcerRubr", "BetuLent", "CornFlor", "FaguGran", "LiriTuli", "NyssSylv", 
                                                                              "PinuStro",
                                                                              "Cary", "FraxAmer", "OxydArbo", "QuerAlba", "QuerRubr", "QuerVelu" ))
pal <- met.brewer('Hokusai1', n=18)
pal2 <- c(pal[c(1, 2, 3, 5, 6, 8,10, 13, 14, 15, 16, 17, 18)])
ggplot(summary_fraction) + 
  geom_bar(aes(x=SpeciesName, y=Fraction, fill=SpeciesName),stat = "identity", alpha=1) + 
  theme_classic() + 
  #scale_x_discrete(labels=c("Tulip Poplar", "Sweet Birch", "Red Maple", "Hickory", "White Oak"))+
  scale_fill_manual(values=pal2) + 
  #theme(legend.position = "none")+
  ylab("Transpiration / Biomass") + 
  xlab("") +
  coord_flip()

ggplot(summary_fraction) + 
  geom_bar(aes(x=Type, y=Fraction, fill=SpeciesName),stat = "identity", position = position_dodge(width=0.7, preserve = "single"), width=0.7) + 
  theme_classic() + 
  #scale_x_discrete(labels=c("Tulip Poplar", "Sweet Birch", "Red Maple", "Hickory", "White Oak"))+
  scale_fill_manual(values=pal2) + 
  #theme(legend.position = "none")+
  ylab("Transpiration / Biomass") + 
  xlab("") +
  coord_flip()

summary_fraction[order(Type),]

ggplot(total_monthly[Year >= 12 & Year <= 22,]) + 
  geom_line(aes(x=Date, y=Transpiration*10, color=SpeciesName), size=1, alpha=1) + 
  theme_bw() + 
  #scale_color_manual(values=met.brewer("Hokusai1", n=5),labels=c("Tulip Poplar", "Sweet Birch", "Red Maple", "Hickory", "White Oak"))+
  scale_color_manual(values=pal2) + 
  xlab("Date") + 
  ylab("Transpiration (mm month-1)") + 
  #theme(legend.position = c(0.89, 0.70)) + 
  #theme(legend.position = c(0.89, 0.70)) + 
  theme(legend.title=element_blank())


##############################################################################################################
##############################################################################################################
##############################################################################################################
## Update the input maps to be used in the flux tower model 

flux_ex_rast <- raster("/Volumes/GoogleDrive/My Drive/Chapter3/SetUp/FluxTower/Soils/SOM1surfN.tif")

# use soil organic carbon from ssurgo 
# units grams carbon per square meter 
mukey_rast <- raster("/Volumes/GoogleDrive/My Drive/Chapter3/SetUp/Soils/cwt_mapunitraster_10m_mukey.tif")
Statedf<-as.data.table(as.data.frame(mukey_rast))
colnames(Statedf)<-"mukey"
Statedf$Marker<-1:nrow(Statedf)

NCDB <- "G:/My Drive/Chapter3/SetUp/Soils/GSSURGO_NC/gSSURGO_NC.gdb"
valu1 <- as.data.table(sf::st_read(dsn = NCDB, layer = "Valu1"))
valu1 <- valu1[,c("mukey","soc0_999")]
valu1$mukey <- as.integer(valu1$mukey)

Statedf<-as.data.table(as.data.frame(mukey_rast))
colnames(Statedf)<-"mukey"
Statedf$Marker<-1:nrow(Statedf)

valu1$mukey <- as.character(valu1$mukey)
Statedf <- merge(Statedf, valu1, by='mukey', all.x=TRUE)
Statedf <- Statedf[order(Marker),]
# make the total soc into a raster 
ExampleRaster<-mukey_rast
cwt_ecoregions <- raster("G:/My Drive/Chapter3/SetUp/Ecoregions/cwt_ecoregions.tif")
mask_ecoregions <- cwt_ecoregions
mask_ecoregions[mask_ecoregions == 0] <- NA
proj<-projection(ExampleRaster)
output_matrix<-matrix(as.vector(Statedf$soc0_999),nrow=nrow(ExampleRaster),ncol=ncol(ExampleRaster),byrow=T)
new_output_raster<-raster(output_matrix,xmn=xmin(ExampleRaster),ymn=ymin(ExampleRaster),xmx=xmax(ExampleRaster),ymx=ymax(ExampleRaster), crs=proj)
total_carbon <- resample(new_output_raster, cwt_ecoregions, method='bilinear')
total_carbon <- mask(total_carbon, mask_ecoregions)
total_carbon <- median(values(total_carbon), na.rm=T)

## Fraction of total carbon in each carbon pool:
SOM1surfC= 0.12 #0.01
SOM1soilC= 0.02
SOM2C= 0.54 #0.59 
SOM3C= 0.32 #0.38


## Nitrogen fractions based on corresponding carbon pool:  
SOM1surfN= 0.1 #0.1
SOM1soilN= 0.1
SOM2N= 0.05 #0.08   # i lowered this slighly because som2 N was too high starting off 
SOM3N= 0.11 

SOM1surfC_map = flux_ex_rast
SOM1soilC_map = flux_ex_rast
SOM2C_map =flux_ex_rast
SOM3C_map = flux_ex_rast

SOM1surfN_map = flux_ex_rast
SOM1soilN_map = flux_ex_rast
SOM2N_map = flux_ex_rast
SOM3N_map = flux_ex_rast

SOM1surfC_map[1] = total_carbon * SOM1surfC
SOM1soilC_map[1] = total_carbon * SOM1soilC
SOM2C_map[1] = total_carbon * SOM2C
SOM3C_map[1] = total_carbon * SOM3C

SOM1surfN_map[1] = SOM1surfC_map * SOM1surfN
SOM1soilN_map[1] = SOM1soilC_map * SOM1soilN
SOM2N_map[1] = SOM2C_map * SOM2N
SOM3N_map[1] = SOM3C_map * SOM3N

writeRaster(SOM1surfC_map, "C:/Users/kmcquil/Documents/Landis_Flux_Tower_Diagnostics/SOM1surfC.tif", overwrite=T)
writeRaster(SOM1soilC_map, "C:/Users/kmcquil/Documents/Landis_Flux_Tower_Diagnostics/SOM1soilC.tif", overwrite=T)
writeRaster(SOM2C_map, "C:/Users/kmcquil/Documents/Landis_Flux_Tower_Diagnostics/SOM2C.tif", overwrite=T)
writeRaster(SOM3C_map, "C:/Users/kmcquil/Documents/Landis_Flux_Tower_Diagnostics/SOM3C.tif", overwrite=T)

writeRaster(SOM1surfN_map, "C:/Users/kmcquil/Documents/Landis_Flux_Tower_Diagnostics/SOM1surfN.tif", overwrite=T)
writeRaster(SOM1soilN_map, "C:/Users/kmcquil/Documents/Landis_Flux_Tower_Diagnostics/SOM1soilN.tif", overwrite=T)
writeRaster(SOM2N_map, "C:/Users/kmcquil/Documents/Landis_Flux_Tower_Diagnostics/SOM2N.tif", overwrite=T)
writeRaster(SOM3N_map, "C:/Users/kmcquil/Documents/Landis_Flux_Tower_Diagnostics/SOM3N.tif", overwrite=T)


## I need to set the depth, FC, baselfow, and stormflow so that establishment is possible 
home <-  "C:/Users/kmcquil/Documents/Landis_Flux_Tower_Diagnostics"/
fc <- raster(paste0(home, "FieldCapacity.tif"))
baseflow <- raster(paste0(home, "Baseflow.tif"))
stormflow <- raster(paste0(home, "Stormflow.tif"))
depth <- raster(paste0(home, "SoilDepth.tif"))

# set the new soil hydro raster values 
fc[1] <- 0.38 #0.38
writeRaster(fc, paste0(home, "FieldCapacity.tif"), overwrite=T)

baseflow[1] <- 0.1
writeRaster(baseflow, paste0(home, "Baseflow.tif"), overwrite=T)

stormflow[1] <-0.1
writeRaster(stormflow, paste0(home, "Stormflow.tif"), overwrite=T)

depth[1] <- 95
writeRaster(depth, paste0(home, "SoilDepth.tif"), overwrite=T)




#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
### Create all watershed diagnostic plots 

#########################################################################################################
# Check regneration rates by species 
pest <- fread("/Volumes/GoogleDrive/My Drive/Chapter3/Models/Landis_WS_Diagnostics/NECN-prob-establish-log.csv")
ggplot(pest) + 
  geom_boxplot(aes(x=SpeciesName, y=AvgProbEst), fill="Light Green") + 
  theme_bw()
pest_summary <- pest[,.(AvgProbEst=mean(AvgProbEst, na.rm=T)), .(Time, SpeciesName)]
ggplot(pest_summary) + 
  geom_point(aes(x=Time, y=AvgProbEst), color="Dark Green") + 
  geom_line(aes(x=Time, y=AvgProbEst), color="Black") + 
  theme_bw() + 
  facet_wrap(~SpeciesName)


#########################################################################################################
# Soil C and N growth rates 

annual_log <- fread("/Volumes/GoogleDrive/My Drive/Chapter3/Models/Landis_WS_Diagnostics/NECN-Succession-log.csv")
annual_log$ET <- (annual_log$AnnualTrans + annual_log$AnnualEvaporation)*10

annual_log <- annual_log[,.(SOMTC=mean(SOMTC), C_SOM1soil=mean(C_SOM1soil), C_SOM1surf=mean(C_SOM1surf), C_SOM2=mean(C_SOM2), C_SOM3=mean(C_SOM3), 
                            TotalNdep=mean(TotalNdep), Nvol=mean(Nvol), N_SOM1soil=mean(N_SOM1soil), N_SOM1surf=mean(N_SOM1surf), N_SOM2=mean(N_SOM2), N_SOM3=mean(N_SOM3), MineralN=mean(MineralN), 
                            NEEC=mean(NEEC), AG_NPPC=mean(AG_NPPC), ET=mean(ET), AGB=mean(AGB)), .(Time)]
(annual_log$SOMTC[nrow(annual_log)] - annual_log$SOMTC[1])/annual_log$SOMTC[1]
(annual_log$C_SOM1soil[nrow(annual_log)] - annual_log$C_SOM1soil[1])/annual_log$C_SOM1soil[1]
(annual_log$C_SOM1surf[nrow(annual_log)] - annual_log$C_SOM1surf[1])/annual_log$C_SOM1surf[1]
(annual_log$C_SOM2[nrow(annual_log)] - annual_log$C_SOM2[1])/annual_log$C_SOM2[1]
(annual_log$C_SOM3[nrow(annual_log)] - annual_log$C_SOM3[1])/annual_log$C_SOM3[1]

ndep <- mean(annual_log$TotalNdep)
ndep
nvol <- mean(annual_log$Nvol)
nvol

n1surf <- ggplot(annual_log) + 
  geom_line(aes(x=Time, y=N_SOM1surf)) + 
  theme_bw() + 
  ylim(0, 600) + 
  xlab("Year")
n1soil <- ggplot(annual_log) + 
  geom_line(aes(x=Time, y=N_SOM1soil)) + 
  theme_bw()+ 
  ylim(0, 600) + 
  xlab("Year")
n2 <- ggplot(annual_log) + 
  geom_line(aes(x=Time, y=N_SOM2)) + 
  theme_bw()+ 
  ylim(0, 600) + 
  xlab("Year")
n3 <- ggplot(annual_log) + 
  geom_line(aes(x=Time, y=N_SOM3)) + 
  theme_bw()+ 
  ylim(0, 600) + 
  xlab("Year")
c1surf <- ggplot(annual_log) + 
  geom_line(aes(x=Time, y=C_SOM1surf)) + 
  theme_bw() + 
  ylim(0, 8500) + 
  xlab("Year")
c1soil <- ggplot(annual_log) + 
  geom_line(aes(x=Time, y=C_SOM1soil)) + 
  theme_bw()+ 
  ylim(0, 8500) + 
  xlab("Year")
c2 <- ggplot(annual_log) + 
  geom_line(aes(x=Time, y=C_SOM2)) + 
  theme_bw()+ 
  ylim(0, 12000) + 
  xlab("Year")
c3 <- ggplot(annual_log) + 
  geom_line(aes(x=Time, y=C_SOM3)) + 
  theme_bw()+ 
  ylim(0, 8500) + 
  xlab("Year")

plot_grid(n1surf, n1soil, n2, n3, 
          c1surf, c1soil, c2, c3, nrow=2)

ggplot(annual_log) + 
  geom_line(aes(x=Time, y=SOMTC)) + 
  theme_bw() + 
  ylim(0, 18000) + 
  xlab("Year") + 
  ylab("SOMTC (gC m-2)")


#########################################################################################################
# Plot mineral N 
annual_log <- annual_log[!Time == 0,]

ggplot(annual_log) + 
  geom_line(aes(x=Time, y=MineralN)) + 
  theme_bw() + 
  xlab("Year") + 
  ylab("Mineral N (gC m-2)") + 
  ylim(0, 6)
mean(annual_log$MineralN)

#########################################################################################################
# Plot NEE, NPP, ET, AGB
ggplot(annual_log) + 
  geom_line(aes(x=Time, y=NEEC)) + 
  theme_bw() + 
  xlab("Year") + 
  ylab("NEE (gC m-2 yr-1)") + 
  ylim(-400, 25)
mean(annual_log$NEEC)

ggplot(annual_log) + 
  geom_line(aes(x=Time, y=ET)) + 
  theme_bw() + 
  xlab("Year") + 
  ylab("ET (mm m-2 yr-1)") + 
  ylim(0, 1300)
mean(annual_log$ET)

ggplot(annual_log) + 
  geom_line(aes(x=Time, y=annual_log$AG_NPPC)) + 
  theme_bw() + 
  ylim(0, 1650) + 
  xlab("Year") + 
  ylab("AG-NPP (g C m-2 yr-1)")
mean(annual_log$AG_NPPC)

ggplot(annual_log) + 
  geom_line(aes(x=Time, y=annual_log$AGB)) + 
  theme_bw() + 
  xlab("Year") + 
  ylim(0, 30000) + 
  ylab("AGB (g Biomass m-2)")

mean(annual_log$AGB)


#########################################################################################################
# LAI

get_average <- function(filepath, variable, start_year, start, end, scalar){
  
  maps <- list.files(filepath, full.names=T, pattern="*.img$")
  maps_dt <- data.table(File=maps[grep(variable, maps)])
  maps_dt$Year <- as.numeric(unlist(regmatches(basename(maps_dt$File), gregexpr("[[:digit:]]+", basename(maps_dt$File)))))
  maps_dt <- maps_dt[order(Year)]
  maps_dt$Year <- maps_dt$Year + start_year
  r_stack <- do.call("stack", lapply(maps_dt[Year >= start & Year <= end,]$File, raster))
  r_stack <- r_stack * scalar
  r_stack[r_stack == 0] <- NA
  avg <- mean(r_stack)
  #return(avg)
  return(r_stack)
  
}
updated_lai <- get_average("/Volumes/GoogleDrive/My Drive/Chapter3/Models/Landis_WS_Diagnostics/NECN/", "LAI", 1999, 2000, 2050, 1)
mean(values(updated_lai), na.rm=T)

mean_lai <- c()
for(r in 1:nlayers(updated_lai)){
  mean_lai <- c(mean_lai, mean(values(updated_lai[[r]]), na.rm=T))
}

plot(seq(1, length(mean_lai)), mean_lai, ylim=c(0, 10), 'l', ylab="LAI", xlab="Time") 

mean(mean_lai)
#########################################################################################################
# Species Biomass
start_year <- 1999
cal_log <- fread("/Volumes/GoogleDrive/My Drive/Chapter3/Models/Landis_WS_Diagnostics/NECN-calibrate-log.csv")
spp <- c("QuerAlba", "AcerRubr", "LiriTuli", "BetuLent", "Cary")
total_monthly <- cal_log[,.(Transpiration=sum(Transpiration), 
                            NPP=sum(ActualLeafANPP + ActualWoodANPP),
                            Biomass=sum(CohortBiomass), 
                            GrowthLimitLAI=mean(GrowthLimitLAI), 
                            GrowthLimitSoilWater=mean(GrowthLimitSoilWater), 
                            GrowthLimitT=mean(GrowthLimitT), 
                            GrowthLimitN=mean(GrowthLimitN), 
                            GrowthLimitLAIcompetition=mean(GrowthLimitLAIcompetition)), .(Year, Month, SpeciesName)]

total_monthly$Date <- as.Date(paste0(total_monthly$Year+start_year, sprintf("%02d", total_monthly$Month), "01"), format="%Y%m%d")
total_monthly <- total_monthly[SpeciesName %in% spp,]
total_monthly$SpeciesName <- as.factor(total_monthly$SpeciesName)
total_monthly$SpeciesName <- factor(total_monthly$SpeciesName, levels=c("LiriTuli", "BetuLent", "AcerRubr", "Cary", "QuerAlba"))
total_monthly_long <- melt(total_monthly, id.vars=c("Year", 'Month', 'Date', 'SpeciesName'), measure.vars=c('Transpiration', 'NPP','Biomass','GrowthLimitLAI', 
                                                                                                            'GrowthLimitSoilWater', 'GrowthLimitT',
                                                                                                            'GrowthLimitN', 'GrowthLimitLAIcompetition'))
ggplot(total_monthly) + 
  geom_line(aes(x=Date, y=Biomass, color=SpeciesName), size=1, alpha=0.7) + 
  theme_bw() + 
  scale_color_manual(values=met.brewer("Hokusai1", n=5),labels=c("Tulip Poplar", "Sweet Birch", "Red Maple", "Hickory", "White Oak"))+
  xlab("Date") + 
  ylab("Biomass") + 
  theme(legend.position = c(0.89, 0.40)) + 
  theme(legend.title=element_blank())

