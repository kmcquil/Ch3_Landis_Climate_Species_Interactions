########################################################################################
## Create tables and figures for the manuscript 

library(data.table)
library(raster)
library(ggplot2)
library(MetBrewer)
library(viridis)
library(cowplot)
library(rasterVis)
library(sf)

home <-"G:/My Drive/Chapter3/"
home <- "/Volumes/GoogleDrive/My Drive/Chapter3/"
#############################################################################################
#############################################################################################
## Figure 1. Study area created in arc map 
## Supplementary Figure 1. Ecoregions map created in arc map 
#############################################################################################
#############################################################################################

#############################################################################################
#############################################################################################
## Figure 2. Growing season transpiration relative to biomass for all 31 species 
## compare with the sap flux data 
#############################################################################################
#############################################################################################

start_year <- 1999
cal_log<- fread(paste0(home, "Models/Landis_WS_V1_ZR_19012023/NECN-calibrate-log.csv"), select=c("Transpiration", "ActualLeafANPP", "ActualWoodANPP", "CohortBiomass", "Year", "Month", "SpeciesName"))
cal_log$Year <- cal_log$Year + start_year
total_monthly <- cal_log[Year >=2011 & Year <= 2021,.(Transpiration=sum(Transpiration), 
                            NPP=sum(ActualLeafANPP + ActualWoodANPP),
                            Biomass=sum(CohortBiomass)), .(Year, Month, SpeciesName)]
total_monthly$Date <- as.Date(paste0(total_monthly$Year+start_year, sprintf("%02d", total_monthly$Month), "01"), format="%Y%m%d")
total_monthly$SpeciesName <- as.factor(total_monthly$SpeciesName)
total_monthly$Fraction <- total_monthly$Transpiration/total_monthly$Biomass
summary_fraction <- total_monthly[!Fraction==0.0 & (Month > 3 & Month < 10),.(Fraction=mean(Fraction),
                                                   Fraction_sd=sd(Fraction)), .(SpeciesName)]
summary_fraction$Fraction <- summary_fraction$Fraction * 10000
summary_fraction$Fraction_sd <- summary_fraction$Fraction_sd * 10000

species_table <- fread(paste0(home, "SetUp/InitialCommunities/RileyFC/final_spp_table.csv"))
summary_fraction <- merge(summary_fraction, species_table[,c("LandisCode","Common_Name", "Type", "SN_abbr")], by.x="SpeciesName", by.y="LandisCode", all.x=T)
summary_fraction$SN_abbr <- as.factor(summary_fraction$SN_abbr)
summary_fraction <- summary_fraction[(order(Type)),]
summary_fraction$SN_abbr <- factor(summary_fraction$SN_abbr, levels=summary_fraction$SN_abbr)
summary_fraction$Type <- as.factor(summary_fraction$Type)

summary_fraction[,.(Fraction=mean(Fraction)), .(Type)]

sub <- summary_fraction[grepl("Acer|Liri|Quer", summary_fraction$SpeciesName),]
sub[,.(Fraction=mean(Fraction)), .(Type)]

pal <- met.brewer('Hokusai1', n=8)[c(8,6,7)]
plot_relative_transpiration <- ggplot(summary_fraction) + 
  geom_bar(aes(x=SN_abbr, y=Fraction, fill=Type),stat = "identity", alpha=1, width=0.7) + 
  geom_errorbar(aes(x=SN_abbr, ymin=Fraction - Fraction_sd, ymax=Fraction + Fraction_sd), width=0.5, color="Black", alpha=0.9, size=0.5) + 
  theme_classic() + 
  scale_fill_manual(values=pal, name="Xylem Anatomy") + 
  scale_x_discrete(labels=summary_fraction$SN_abbr) +
  ylab(expression(T[rel]~(mm~mo^{"-1"}/g~Biomass))) + 
  xlab("") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, face="italic")) + 
  theme(legend.position = c(0.6, 0.75))

# Observed sap flux data for comparison  
sapflux <- fread(paste0(home, "Calibration/Data/SapFlux/WS32/ws32_sapflux_2017_2021.csv"))
sapflux$Date <- as.Date(paste0(0, sapflux$Date), format="%d%m%Y")
spp_metrics <- fread(paste0(home, "Calibration/Data/SapFlux/WS32/ws32_species_metrics.csv"))

# calculate growing season transpiration (May - September) 
sapflux_long <- melt(sapflux, id.vars=c("Date"), 
                     measure.vars=c("Maple", "Tulip_poplar", "Oaks", "Mountain_laurel", "Rhododendron", "Pines", "Other", "Total"), 
                     variable.name="Species", "Transpiration")

gs_transpiration <- sapflux_long[month(Date) > 3 & month(Date) < 10, 
                                 .(Transpiration=sum(Transpiration)), .(Year=year(Date), Species)]

# Standardize by basal area 
gs_transpiration <- merge(gs_transpiration, spp_metrics, by="Species", all.x=T)
gs_transpiration$Standardized_Transpiration <- gs_transpiration$Transpiration/gs_transpiration$Basal_area

spp_summary <- gs_transpiration[,.(Standardized_Transpiration=mean(Standardized_Transpiration), 
                                   Standardized_Transpiration_sd=sd(Standardized_Transpiration)), .(Species)]
spp_summary <- spp_summary[Species %in% c("Maple", "Oaks", "Tulip_poplar"),]
spp_summary$Type <- c("Diffuse", "Ring", "Diffuse")
spp_summary$Type <- as.factor(spp_summary$Type)
spp_summary$Species <- factor(spp_summary$Species, levels=c('Maple', 'Tulip_poplar', 'Oaks'))

# Plot without Pines because that is so high 
plot_relative_sapflux <- ggplot(spp_summary) + 
  geom_bar(aes(x=Species, y=Standardized_Transpiration, fill=Type),stat = "identity", width=0.7) + 
  geom_errorbar(aes(x=Species, ymin=Standardized_Transpiration - Standardized_Transpiration_sd, 
                    ymax=Standardized_Transpiration + Standardized_Transpiration_sd), width=0.5, color="Black", alpha=0.9, size=0.5) + 
  scale_fill_manual(values=pal) + 
  scale_x_discrete(labels=c("Acer sp.", "L. tulipifera", "Quercus sp.")) +
  ylab(expression(T[rel]~(mm~mo^{"-1"}/m^{"2"}~ha^{"-1"}))) + 
  xlab("") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, face="italic")) + 
  theme(legend.position = "none")

aligned <- plot_grid(plot_relative_sapflux, plot_relative_transpiration, align='h', rel_widths = c(0.5, 2.5), 
                     labels='AUTO')

tiff(paste0(home, "Figures/relative_transpiration.tif"), width=9, height=5, units='in', res=500)
aligned
dev.off()



#############################################################################################
#############################################################################################
## Figure 3. Watershed annual streamflow for each weir for updated vs original 
## with a separate plot for each watershed
#############################################################################################
#############################################################################################

# First create precipitation time series using the precipitation I gave to the model 
monthly_log <- fread(paste0(home, "Models/Landis_WS_V1_ZR_19012023/NECN-succession-monthly-log.csv"))
monthly_log$Year <- monthly_log$Time + start_year
monthly_log$VYR <- ifelse(monthly_log$Month < 6, monthly_log$Year - 1, monthly_log$Year)
modeled_p_vyr <- monthly_log[,.(Precipitation=sum(Precipitation ), 
                                NumSites=mean(NumSites)), .(VYR, ClimateRegionIndex )]
modeled_p_vyr$ClimateRegionIndex <- modeled_p_vyr$ClimateRegionIndex + 1
modeled_p_vyr$Catchment <- ifelse(modeled_p_vyr$ClimateRegionIndex %in%(c(1,3,5)), 14, 18)
# weight each region x catchment 
modeled_p_vyr$WeightedSites <- as.numeric(rep(NA, nrow(modeled_p_vyr)))

ecor <- raster(paste0(home,  "Models/Landis_WS_V1_ZR_19012023/ecoregions_6.tif")) 
# 1, 3, 5 correspond to 14 
# 2, 4, 6 correspond to 18 
ecordt <- as.data.frame(ecor)
ws18_count <- length(ecordt[ecordt$ecoregions_6 %in% c(2,4,6),])
ws14_count <- length(ecordt[ecordt$ecoregions_6 %in% c(1,3,5),])

modeled_p_vyr[Catchment == 14,]$WeightedSites <- modeled_p_vyr[Catchment == 14,]$NumSites/ws14_count
modeled_p_vyr[Catchment == 18,]$WeightedSites <- modeled_p_vyr[Catchment == 18,]$NumSites/ws18_count
mod_p_14 <- modeled_p_vyr[Catchment == 14,.(PCP=sum(Precipitation*WeightedSites)), .(VYR)]
mod_p_18 <- modeled_p_vyr[Catchment == 18,.(PCP=sum(Precipitation*WeightedSites)), .(VYR)]
mod_p_14$PCP <- mod_p_14$PCP * 10
mod_p_18$PCP <- mod_p_18$PCP * 10

# function to convert streamflow in cubic feet per second per square mile to mm/day 
get_mmday <- function(x){
  return(x * (1/2.788e+7) * 304.8 * 86400)
}

# Streamflow in CSM (cubic feet per second per square mile) - this units does not require us knowing the area of the catchment 
# fix the water year to be a vegetation year from June - May 
ws14 <- fread(paste0(home, "Calibration/Data/Streamflow/Data/ws14_daily_flow_1937_2021.csv"), fill=T)
ws14$Year <- ifelse(ws14$MO >= 11, ws14$WYR -1, ws14$WYR)
ws14$VYR <- ifelse(ws14$MO <6, ws14$Year -1, ws14$Year)
ws14$mmday <- unlist(lapply(ws14$CSM, get_mmday))

ws18 <- fread(paste0(home, "Calibration/Data/Streamflow/Data/ws18_daily_flow_1937_2021.csv"), fill=T)
ws18$CSM <- as.numeric(ws18$CSM)
ws18[is.na(CSM)]$CSM <- (ws18[which(is.na(ws18$CSM))-1]$CSM + ws18[which(is.na(ws18$CSM))+1]$CSM)/2
ws18$Year <- ifelse(ws18$MO >= 11, ws18$WYR -1, ws18$WYR)
ws18$VYR <- ifelse(ws18$MO <6, ws18$Year -1, ws18$Year)
ws18$mmday <- unlist(lapply(ws18$CSM, get_mmday))

# Calculate vegetation water year streamflow 
ws18_vyr <- ws18[,.(mmyear=sum(mmday)), .(VYR)]
ws18_vyr <- merge(ws18_vyr, mod_p_18, by="VYR", all.x=T)
ws18_vyr$ET <- ws18_vyr$PCP - ws18_vyr$mmyear

ws14_vyr <- ws14[,.(mmyear=sum(mmday)), .(VYR)]
ws14_vyr <- merge(ws14_vyr, mod_p_14, by="VYR", all.x=T)
ws14_vyr$ET <- ws14_vyr$PCP - ws14_vyr$mmyear

## Calculate the VYR ET from the updated model 
monthly_log <- fread(paste0(home, "Models/Landis_WS_V1_ZR_19012023/NECN-succession-monthly-log.csv"))
monthly_log$ET <- (monthly_log$AvgTranspiration + monthly_log$AvgEvaporation)*10
monthly_log$Year <- monthly_log$Time + start_year
monthly_log$VYR <- ifelse(monthly_log$Month < 6, monthly_log$Year - 1, monthly_log$Year)
modeled_et_vyr <- monthly_log[,.(ET=sum(ET), 
                                 NumSites=mean(NumSites)), .(VYR, ClimateRegionIndex )]
modeled_et_vyr$ClimateRegionIndex <- modeled_et_vyr$ClimateRegionIndex + 1
modeled_et_vyr$Catchment <- ifelse(modeled_et_vyr$ClimateRegionIndex %in%(c(1,3,5)), 14, 18)

# weight each region x catchment
modeled_et_vyr$WeightedSites <- as.numeric(rep(NA, nrow(modeled_et_vyr)))
modeled_et_vyr[Catchment == 14,]$WeightedSites <- modeled_et_vyr[Catchment == 14,]$NumSites/ws14_count
modeled_et_vyr[Catchment == 18,]$WeightedSites <- modeled_et_vyr[Catchment == 18,]$NumSites/ws18_count
mod_et_14 <- modeled_et_vyr[Catchment == 14,.(UP_ET=sum(ET*WeightedSites)), .(VYR)]
mod_et_18 <- modeled_et_vyr[Catchment == 18,.(UP_ET=sum(ET*WeightedSites)), .(VYR)]

# Merge in modeled ET
ws14_vyr <- merge(ws14_vyr, mod_et_14, by="VYR")
ws18_vyr <- merge(ws18_vyr, mod_et_18, by="VYR")

## Calculate the WYR ET from the original model 
monthly_log <- fread(paste0(home, "Models/Landis_WS_V1_ZR_19012023_OG/NECN-succession-monthly-log.csv"))
monthly_log$ET <- (monthly_log$Transpiration + monthly_log$AvgEvaporation)*10
monthly_log$Year <- monthly_log$Time + start_year
monthly_log$VYR <- ifelse(monthly_log$Month < 6, monthly_log$Year - 1, monthly_log$Year)
modeled_et_vyr <- monthly_log[,.(ET=sum(ET), 
                                 NumSites=mean(NumSites)), .(VYR, ClimateRegionIndex )]
modeled_et_vyr$ClimateRegionIndex <- modeled_et_vyr$ClimateRegionIndex + 1
modeled_et_vyr$Catchment <- ifelse(modeled_et_vyr$ClimateRegionIndex %in%(c(1,3,5)), 14, 18)

# weight each region x catchment
modeled_et_vyr$WeightedSites <- as.numeric(rep(NA, nrow(modeled_et_vyr)))
modeled_et_vyr[Catchment == 14,]$WeightedSites <- modeled_et_vyr[Catchment == 14,]$NumSites/ws14_count
modeled_et_vyr[Catchment == 18,]$WeightedSites <- modeled_et_vyr[Catchment == 18,]$NumSites/ws18_count
mod_et_14 <- modeled_et_vyr[Catchment == 14,.(OG_ET=sum(ET*WeightedSites)), .(VYR)]
mod_et_18 <- modeled_et_vyr[Catchment == 18,.(OG_ET=sum(ET*WeightedSites)), .(VYR)]

# Merge in modeled ET
ws14_vyr <- merge(ws14_vyr, mod_et_14, by="VYR")
ws18_vyr <- merge(ws18_vyr, mod_et_18, by="VYR")

pal <- met.brewer('Hokusai1', n=3)[c(3,1,2)]
colors <- c("Updated" = pal[2], "Original" = pal[1], "Observed"="Black")

ws14_vyr_long <- melt(ws14_vyr, id.vars=c("VYR"), measure.vars=c("ET", "UP_ET", "OG_ET"))
ws14_plot <- ggplot(ws14_vyr_long[VYR >= 2011 & VYR <= 2020,]) + 
  geom_line(aes(x=VYR, y=value, color=variable), size=1.25, alpha=0.7) + 
  geom_point(aes(x=VYR, y=value, color=variable), size =3, alpha=0.7) + 
  scale_color_manual(values= c("black", pal[2], pal[1]), name="") + 
  theme_bw() + 
  xlab("") + 
  ylab(expression(ET~(mm~yr^{-1}))) + 
  theme(legend.position="none") + 
  scale_x_continuous(breaks=seq(2011, 2020,1)) + 
  ylim(600, 1300) + 
  theme(plot.margin = unit(c(0.1, 0.1, 0.1,0), "cm")) + 
  theme(axis.text.x=element_text(angle=45,hjust=1))


ws18_vyr_long <- melt(ws18_vyr, id.vars=c("VYR"), measure.vars=c("ET", "UP_ET", "OG_ET"))
ws18_plot <- ggplot(ws18_vyr_long[VYR >= 2011 & VYR <= 2020,]) + 
  geom_line(aes(x=VYR, y=value, color=variable), size=1.25, alpha=0.7) + 
  geom_point(aes(x=VYR, y=value, color=variable), size =3, alpha=0.7) + 
  scale_color_manual(values= c("black", pal[2], pal[1]), name="") +  
  theme_bw() + 
  xlab("VYR") + 
  ylab(expression(ET~(mm~yr^{-1}))) + 
  theme(legend.position="none")+ 
  scale_x_continuous(breaks=seq(2011, 2020,1)) + 
  ylim(600, 1300)+ 
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0), "cm")) + 
  theme(axis.text.x=element_text(angle=45,hjust=1))

legend <-  ggplot(ws18_vyr_long[VYR >= 2011 & VYR <= 2020,]) + 
  geom_line(aes(x=VYR, y=value, color=variable), size=1.25, alpha=0.7) + 
  geom_point(aes(x=VYR, y=value, color=variable), size =3, alpha=0.7) + 
  scale_color_manual(values= c("black", pal[2], pal[1]), name="", labels=c("Observed", "Updated", "Original")) +  
  theme_bw() + 
  xlab("VYR") + 
  ylab(expression(ET~(mm~yr^{-1}))) + 
  theme(legend.position="right")+ 
  scale_x_continuous(breaks=seq(2011, 2020,1)) + 
  ylim(600, 1300)

legend <- get_legend(legend)

# create a plot of the VYR precip and VYR temp 
# start by gettings the average vyr precip and temp from ws18 and ws14
monthly_log <- fread(paste0(home, "Models/Landis_WS_V1_ZR_19012023/NECN-succession-monthly-log.csv"))
monthly_log$Year <- monthly_log$Time + start_year
monthly_log$VYR <- ifelse(monthly_log$Month < 6, monthly_log$Year - 1, monthly_log$Year)
modeled_p_vyr <- monthly_log[,.(Precipitation=sum(Precipitation ), Temperature=mean(AirTemp),
                                NumSites=mean(NumSites)), .(VYR, ClimateRegionIndex )]
modeled_p_vyr$ClimateRegionIndex <- modeled_p_vyr$ClimateRegionIndex + 1
# weight each region by its size 
modeled_p_vyr$WeightedSites <- as.numeric(rep(NA, nrow(modeled_p_vyr)))
total_count <- ws14_count + ws18_count
modeled_p_vyr$WeightedSites <- modeled_p_vyr$NumSites/total_count
modeled_p_vyr <- modeled_p_vyr[,.(PCP=sum(Precipitation*WeightedSites), Temperature=sum(Temperature*WeightedSites)), .(VYR)]
modeled_p_vyr$PCP <- modeled_p_vyr$PCP*10
scale <-0.005
pt_plot <- ggplot(modeled_p_vyr[VYR >= 2011 & VYR <= 2020,]) + 
  geom_bar(aes(x=VYR, y=PCP, fill="PCP"), stat="identity", alpha=1) + 
  geom_line(aes(x=VYR, y=Temperature/scale, color="Temperature"), alpha=0.7, size=1.25) + 
  geom_point(aes(x=VYR, y=Temperature/scale, color="Temperature"), alpha=0.7, size=3) + 
  scale_y_continuous(expression(italic(P)~(mm~yr^{1})), sec.axis=sec_axis(~.*scale, name=expression(italic(T )~(degree*C)))) + 
  scale_x_continuous(breaks=seq(2011, 2020,1)) + 
  scale_color_manual("", values=c("Temperature"="black")) + 
  scale_fill_manual("", "values"="grey") + 
  theme_bw() + 
  theme(legend.position='none') + 
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.25), "cm")) + 
  theme(axis.text.x=element_text(angle=45,hjust=1))
  
ET_TS_Plot <- plot_grid(ws14_plot, pt_plot, ws18_plot, ncol=2, nrow=2, rel_widths = c(1, 1), 
                        labels=c("A     WS14", "C","B     WS18"))

tiff(paste0(home, "Figures/ws_et_ts_with_met_ts.tif"), width=7, height=5, units='in', res=500)
ggdraw() + 
  draw_plot(ET_TS_Plot) + 
  draw_plot(legend, x=0.5, y=0.2, width=0.2, height=0.2)
dev.off()

#############################################################################################
#############################################################################################
## Figure 4. Mapped average annual ET from 2011 - 2021 comparing oringal and updated models. 
#############################################################################################
#############################################################################################

get_average <- function(filepath, variable, start_year, start, end, scalar){
  
  maps <- list.files(filepath, full.names=T)
  maps_dt <- data.table(File=maps[grep(variable, maps)])
  maps_dt$Year <- as.numeric(unlist(regmatches(basename(maps_dt$File), gregexpr("[[:digit:]]+", basename(maps_dt$File)))))
  maps_dt <- maps_dt[order(Year)]
  maps_dt$Year <- maps_dt$Year + start_year
  r_stack <- do.call("stack", lapply(maps_dt[Year >= start & Year <= end,]$File, raster))
  r_stack <- r_stack * scalar
  r_stack[r_stack == 0] <- NA
  avg <- mean(r_stack)
  return(avg)
  
}

og_mean <- get_average(paste0(home, "Models/Landis_WS_V1_ZR_19012023_OG/NECN/"), "ET", 1999, 2011, 2021, 10)
updated_mean <- get_average(paste0(home, "Models/Landis_WS_V1_ZR_19012023/NECN/"), "ET", 1999, 2011, 2021, 10)

coul <- rev(viridis(100))
hm <- stack(og_mean, updated_mean)
tiff(paste0(home, "Figures/et_maps.tif"), width=9, height=5, units='in', res=500)
levelplot(hm, main = "", names.attr=c("Original", "Updated"), nrow=1,ncol=2, col.regions = coul)
dev.off()

tiff(paste0(home, "Figures/et_maps_inset.tif"), width=6.5, height=5, units='in', res=500)
par(mfrow=c(1, 1), mar=c(2, 3, 1, 3))
breakpoints <- seq(580, 1340, 40)
colors <- viridis(19)
plot(hm[[2]],breaks=breakpoints, col=colors,xaxt='n', yaxt='n', main="Updated", legend=F)
par(fig=c(0.57, 0.857, 0.05, 0.41), new = T)  
plot(hm[[1]],breaks=breakpoints,col=colors, legend=F, xaxt='n', yaxt='n')
text(x=35, y=15, labels=substitute(paste(bold("Original"))))
par(mfrow=c(1, 1), mar=c(2, 0, 1, 0), new=FALSE)
plot(hm[[2]], legend.only=TRUE, legend.shrink=1, legend.width=2,
     breaks=breakpoints, col=colors, 
     axis.args=list(at=pretty(breakpoints), labels=pretty(breakpoints)),)
dev.off()


#############################################################################################
#############################################################################################
## Figure 5. ET, NEE, and LAI along environmental gradients 
#############################################################################################
#############################################################################################

# create average nee original and updated raster 
og_nee <- get_average(paste0(home, "Models/Landis_WS_V1_ZR_19012023_OG/NECN/"), "ANEE", 1999, 2011, 2021, 1)
updated_nee <- get_average(paste0(home, "Models/Landis_WS_V1_ZR_19012023/NECN/"), "ANEE", 1999, 2011, 2021, 1)

# create average lai original and updated raster 
og_lai <- get_average(paste0(home, "Models/Landis_WS_V1_ZR_19012023_OG/NECN/"), "LAI", 1999, 2011, 2021, 1)
updated_lai <- get_average(paste0(home, "Models/Landis_WS_V1_ZR_19012023/NECN/"), "LAI", 1999, 2011, 2021, 1)

new_tl_id_rast <- raster(paste0(home, "SetUp/InitialCommunities/RileyFC/national_c2014_tree_list_cropped.tif"))
# crop that to watersheds 14 and 18 
roi <- st_read(paste0(home, "SetUp/Study_Area/roi.shp"))
roi <- st_transform(roi, crs(new_tl_id_rast))
riley_roi <- mask(crop(new_tl_id_rast, roi), roi)

twi_sbr <- raster(paste0(home, "SetUp/Topography/TWI.tif")) # this was calculated in chapter 1 
roi <- st_transform(roi, crs(twi_sbr))
twi_roi <- mask(crop(twi_sbr, roi), roi)
twi_roi <- projectRaster(twi_roi, crs=crs(riley_roi), method='ngb')
twi_roi <- raster::resample(twi_roi, riley_roi)
q75 <- quantile(values(twi_roi), 0.9, na.rm=T)
upslope <- riley_roi
upslope[twi_roi >= q75] <- NA
downslope <- riley_roi
downslope[twi_roi < q75] <- NA

elevation <- raster(paste0(home, "SetUp/Topography/Elevation/cwt_elevation_10m.tif"))
elevation <- crop(elevation, roi)
elevation <- projectRaster(elevation, crs=crs(riley_roi), method='ngb')
elevation <- raster::resample(elevation, riley_roi)

diffuse <- raster(paste0(home, "SetUp/InitialCommunities/diffuse_BA.tif"))

# spatial data for et 
extent(updated_mean) <- extent(twi_roi)
crs(updated_mean) <- crs(twi_roi)

extent(og_mean) <- extent(twi_roi)
crs(og_mean) <- crs(twi_roi)

# spatial data for nee
extent(updated_nee) <- extent(twi_roi)
crs(updated_nee) <- crs(twi_roi)

extent(og_nee) <- extent(twi_roi)
crs(og_nee) <- crs(twi_roi)

# spatial data for lai 
extent(updated_lai) <- extent(twi_roi)
crs(updated_lai) <- crs(twi_roi)

extent(og_lai) <- extent(twi_roi)
crs(og_lai) <- crs(twi_roi)


twi_stack <- as.data.table(as.data.frame(stack(twi_roi,elevation, diffuse, 
                                               updated_mean, og_mean, 
                                               updated_nee, og_nee, 
                                               updated_lai, og_lai)))
colnames(twi_stack) <- c("TWI", "Elevation","Diffuse","ET", "OG_ET", 
                         "NEE", "OG_NEE", "LAI", "OG_LAI")
twi_stack$Diffuse <- twi_stack$Diffuse * 100
twi_stack$TWI_bin <- cut(twi_stack$TWI, breaks=c(6, 8, 10, 12, 14))
twi_stack$Elev_bin <- cut(twi_stack$Elevation, breaks=seq(690, 1010, 50))
twi_stack$Diff_bin <- cut(twi_stack$Diffuse, breaks=seq(0, 100, 20))

get1 <- function(x){
  if(identical(x, character(0)) ){
    return(NA)
  }else{
    return(x[1])
  }
}

twi_stack$TWI_min <- as.numeric(unlist(lapply(regmatches(as.character(twi_stack$TWI_bin), gregexpr("[[:digit:]]+", as.character(twi_stack$TWI_bin))), get1)))
twi_stack$Elev_min <- as.numeric(unlist(lapply(regmatches(as.character(twi_stack$Elev_bin), gregexpr("[[:digit:]]+", as.character(twi_stack$Elev_bin))), get1)))
twi_stack$Diff_min <- as.numeric(unlist(lapply(regmatches(as.character(twi_stack$Diff_bin), gregexpr("[[:digit:]]+", as.character(twi_stack$Diff_bin))), get1)))

twi_stack_long <- melt(twi_stack, id.vars=c("Elev_min", "Diff_min", "TWI_min"), 
                       measure.vars=c("ET", "OG_ET", "NEE", "OG_NEE", "LAI", "OG_LAI"))

twi_summary <- twi_stack_long[complete.cases(twi_stack_long),.(Avg=mean(value), 
                                                               StDev=sd(value)), .(TWI_min, variable)]

plot_twi_et <- ggplot(twi_summary[variable %in% c("ET", "OG_ET"),]) + 
  geom_point(aes(x=TWI_min, y=Avg, color=variable), alpha=0.5, size=3) + 
  geom_line(aes(x=TWI_min, y=Avg, color=variable), alpha=0.5, size=1.5) + 
  geom_errorbar(aes(x=TWI_min, ymin=Avg-StDev, ymax=Avg+StDev, color=variable), width=0.75,
                position=position_dodge(0.05), alpha=0.5, size=1.5) + 
  scale_color_manual(values= c(pal[2], pal[1]), labels=c("Updated", "Original"))+
  theme_classic() + 
  xlab("Topographic Wetness Index") + 
  ylab(expression(ET~(mm~yr^{-1})))+
  theme(legend.position = "none")

plot_twi_nee <- ggplot(twi_summary[variable %in% c("NEE", "OG_NEE"),]) + 
  geom_point(aes(x=TWI_min, y=Avg, color=variable), alpha=0.5, size=3) + 
  geom_line(aes(x=TWI_min, y=Avg, color=variable), alpha=0.5, size=1.5) + 
  geom_errorbar(aes(x=TWI_min, ymin=Avg-StDev, ymax=Avg+StDev, color=variable), width=0.75,
                position=position_dodge(0.05), alpha=0.5, size=1.5) + 
  scale_color_manual(values= c(pal[2], pal[1]), labels=c("Updated", "Original"))+
  theme_classic() + 
  xlab("Topographic Wetness Index") + 
  ylab(expression(NEE~(g~C~yr^{-1})))+
  theme(legend.position = "none")

plot_twi_lai <- ggplot(twi_summary[variable %in% c("LAI", "OG_LAI"),]) + 
  geom_point(aes(x=TWI_min, y=Avg, color=variable), alpha=0.5, size=3) + 
  geom_line(aes(x=TWI_min, y=Avg, color=variable), alpha=0.5, size=1.5) + 
  geom_errorbar(aes(x=TWI_min, ymin=Avg-StDev, ymax=Avg+StDev, color=variable), width=0.75,
                position=position_dodge(0.05), alpha=0.5, size=1.5) + 
  scale_color_manual(values= c(pal[2], pal[1]), labels=c("Updated", "Original"))+
  theme_classic() + 
  xlab("Topographic Wetness Index") + 
  ylab(expression(LAI~(m^{2}~m^{-2})))+
  theme(legend.position = "none")

elev_summary <- twi_stack_long[complete.cases(twi_stack_long),.(Avg=mean(value), 
                                                                StDev=sd(value)), .(Elev_min, variable)]
plot_elev_et <- ggplot(elev_summary[variable %in% c("ET", "OG_ET"),]) + 
  geom_point(aes(x=Elev_min, y=Avg, color=variable), alpha=0.5, size=3) + 
  geom_line(aes(x=Elev_min, y=Avg, color=variable), alpha=0.5, size=1.5) + 
  geom_errorbar(aes(x=Elev_min, ymin=Avg-StDev, ymax=Avg+StDev, color=variable), width=30,
                position=position_dodge(0.05), alpha=0.5, size=1.5) + 
  scale_color_manual(values= c(pal[2], pal[1]), labels=c("Updated", "Original"))+
  theme_classic() + 
  xlab("Elevation (m)") + 
  ylab(expression(ET~(mm~yr^{-1})))+
  theme(legend.position = "none") 

plot_elev_nee <- ggplot(elev_summary[variable %in% c("NEE", "OG_NEE"),]) + 
  geom_point(aes(x=Elev_min, y=Avg, color=variable), alpha=0.5, size=3) + 
  geom_line(aes(x=Elev_min, y=Avg, color=variable), alpha=0.5, size=1.5) + 
  geom_errorbar(aes(x=Elev_min, ymin=Avg-StDev, ymax=Avg+StDev, color=variable), width=30,
                position=position_dodge(0.05), alpha=0.5, size=1.5) + 
  scale_color_manual(values= c(pal[2], pal[1]), labels=c("Updated", "Original"))+
  theme_classic() + 
  xlab("Elevation (m)") + 
  ylab(expression(NEE~(g~C~yr^{-1})))+
  theme(legend.position = "none") 


plot_elev_lai <- ggplot(elev_summary[variable %in% c("LAI", "OG_LAI"),]) + 
  geom_point(aes(x=Elev_min, y=Avg, color=variable), alpha=0.5, size=3) + 
  geom_line(aes(x=Elev_min, y=Avg, color=variable), alpha=0.5, size=1.5) + 
  geom_errorbar(aes(x=Elev_min, ymin=Avg-StDev, ymax=Avg+StDev, color=variable), width=30,
                position=position_dodge(0.05), alpha=0.5, size=1.5) + 
  scale_color_manual(values= c(pal[2], pal[1]), labels=c("Updated", "Original"))+
  theme_classic() + 
  xlab("Elevation (m)") + 
  ylab(expression(LAI~(m^{2}~m^{-2})))+
  theme(legend.position = "none") 


diff_summary <- twi_stack_long[complete.cases(twi_stack_long),.(Avg=mean(value), 
                                                                StDev=sd(value)), .(Diff_min, variable)]
plot_diff_et <- ggplot(diff_summary[variable%in%c("ET", "OG_ET")]) + 
  geom_point(aes(x=Diff_min, y=Avg, color=variable), alpha=0.5, size=3) + 
  geom_line(aes(x=Diff_min, y=Avg, color=variable), alpha=0.5, size=1.5) + 
  geom_errorbar(aes(x=Diff_min, ymin=Avg-StDev, ymax=Avg+StDev, color=variable), width=10,
                position=position_dodge(0.05), alpha=0.5, size=1.5) + 
  scale_color_manual(values= c(pal[2], pal[1]), labels=c("Updated", "Original"))+
  theme_classic() + 
  xlab("% Diffuse porous BA") + 
  ylab(expression(ET~(mm~yr^{-1})))+
  theme(legend.position="none")

plot_diff_nee <- ggplot(diff_summary[variable%in%c("NEE", "OG_NEE")]) + 
  geom_point(aes(x=Diff_min, y=Avg, color=variable), alpha=0.5, size=3) + 
  geom_line(aes(x=Diff_min, y=Avg, color=variable), alpha=0.5, size=1.5) + 
  geom_errorbar(aes(x=Diff_min, ymin=Avg-StDev, ymax=Avg+StDev, color=variable), width=10,
                position=position_dodge(0.05), alpha=0.5, size=1.5) + 
  scale_color_manual(values= c(pal[2], pal[1]), labels=c("Updated", "Original"))+
  theme_classic() + 
  xlab("% Diffuse porous BA") + 
  ylab(expression(NEE~(g~C~yr^{-1})))+
  theme(legend.position="none")

plot_diff_lai <- ggplot(diff_summary[variable%in%c("LAI", "OG_LAI")]) + 
  geom_point(aes(x=Diff_min, y=Avg, color=variable), alpha=0.5, size=3) + 
  geom_line(aes(x=Diff_min, y=Avg, color=variable), alpha=0.5, size=1.5) + 
  geom_errorbar(aes(x=Diff_min, ymin=Avg-StDev, ymax=Avg+StDev, color=variable), width=10,
                position=position_dodge(0.05), alpha=0.5, size=1.5) + 
  scale_color_manual(values= c(pal[2], pal[1]), labels=c("Updated", "Original"))+
  theme_classic() + 
  xlab("% Diffuse porous BA") + 
  ylab(expression(LAI~(m^{2}~m^{-2})))+
  theme(legend.position="none")

legend <- ggplot(diff_summary[variable%in%c("LAI", "OG_LAI")]) + 
  geom_point(aes(x=Diff_min, y=Avg, color=variable), alpha=0.5, size=3) + 
  geom_line(aes(x=Diff_min, y=Avg, color=variable), alpha=0.5, size=1.5) + 
  geom_errorbar(aes(x=Diff_min, ymin=Avg-StDev, ymax=Avg+StDev, color=variable), width=10,
                position=position_dodge(0.05), alpha=0.5, size=1.5) + 
  scale_color_manual(values= c(pal[2], pal[1]), labels=c("Updated", "Original"), name="")+
  theme_classic() + 
  xlab("% Diffuse porous BA") + 
  ylab(expression(LAI~(m^{2}~m^{-2})))+
  theme(legend.position="bottom")
legend <- get_legend(legend)

plots <- plot_grid(plot_elev_et, plot_elev_nee, plot_elev_lai,
          plot_twi_et, plot_twi_nee, plot_twi_lai, 
          plot_diff_et, plot_diff_nee, plot_diff_lai, 
          nrow=3, byrow=T, labels="AUTO")

plots <- plot_grid(plot_elev_et, plot_elev_nee,
                   plot_twi_et, plot_twi_nee,
                   plot_diff_et, plot_diff_nee,
                   nrow=2, byrow=F, labels="AUTO")
tiff(paste0(home, "Figures/environmental_gradients_et_nee.tif"), width=9, height=5, units='in', res=500)
plot_grid(plots, legend, nrow=2, ncol=1, rel_heights=c(1, 0.05))
dev.off()

#############################################################################################
#############################################################################################
## Supplementary Table 1: Species biomass in the flux tower and watershed model 
#############################################################################################
#############################################################################################

# Calculate species biomass for each model 
ws_ic <- fread(paste0(home, "SetUp/InitialCommunities/mapcode_species_biomass.csv"))
spp_biomass <- ws_ic[,.(WS_Biomass=sum(Biomass)), .(SPCD, LandisCode)]

# This csv is from chapter 1. I associated the spcd number to the species common name and classified them as diffuse, ring, or tracheid 
sub_unique_species <- fread(paste0(home, "SetUp/InitialCommunities/RileyFC/full_sbr_top50_species_classification.csv"))
spp_biomass <- merge(spp_biomass, sub_unique_species, all.x=T, by="SPCD")
spp_biomass <- spp_biomass[order(LandisCode),]

fwrite(spp_biomass, paste0(home, "Figures/spp_biomass_table.csv"))

#############################################################################################
##############################################################################################
## Table 1. Watershed model performance statistics 
#############################################################################################
#############################################################################################

# Calculate performance metrics 
dt <- data.table(WS=as.numeric(c(14, 14, 18, 18)),
                 Model=as.character(c("Updated", "Original", "Updated", "Original")),
                 ET=as.numeric(c(NA, NA, NA, NA)), 
                 ET_R2=as.numeric(c(NA, NA, NA, NA)),
                 ET_R2_no2018=as.numeric(c(NA, NA, NA, NA)),
                 ET_MAE=as.numeric(c(NA, NA, NA, NA)),
                 ET_Frac_Error=as.numeric(c(NA, NA, NA, NA)), 
                 RMSE=as.numeric(c(NA, NA, NA, NA)), 
                 Bias=as.numeric(c(NA, NA, NA, NA)))

dt[WS==14 & Model=="Updated" ,]$ET <-mean(ws14_vyr[VYR<2021 & VYR > 2010,]$UP_ET)
dt[WS==18 & Model=="Updated",]$ET <-mean(ws18_vyr[VYR<2021 & VYR > 2010,]$UP_ET)
dt[WS==14 & Model=="Original" ,]$ET <-mean(ws14_vyr[VYR<2021 & VYR > 2010,]$OG_ET)
dt[WS==18 & Model=="Original",]$ET <-mean(ws18_vyr[VYR<2021 & VYR > 2010,]$OG_ET)

dt[WS==14 & Model=="Updated" ,]$ET_R2_no2018 <- summary(lm(ws14_vyr[(VYR<2021 & VYR > 2010) & (!VYR == 2018),]$UP_ET ~ ws14_vyr[(VYR<2021 & VYR > 2010) & (!VYR == 2018),]$ET))$r.squared
dt[WS==18 & Model=="Updated",]$ET_R2_no2018 <- summary(lm(ws18_vyr[(VYR<2021 & VYR > 2010) & (!VYR == 2018),]$UP_ET ~ ws18_vyr[(VYR<2021 & VYR > 2010) & (!VYR == 2018),]$ET))$r.squared
dt[WS==14 & Model=="Original" ,]$ET_R2_no2018 <- summary(lm(ws14_vyr[(VYR<2021 & VYR > 2010) & (!VYR == 2018),]$OG_ET ~ ws14_vyr[(VYR<2021 & VYR > 2010) & (!VYR == 2018),]$ET))$r.squared
dt[WS==18 & Model=="Original",]$ET_R2_no2018 <- summary(lm(ws18_vyr[(VYR<2021 & VYR > 2010) & (!VYR == 2018),]$OG_ET ~ ws18_vyr[(VYR<2021 & VYR > 2010) & (!VYR == 2018),]$ET))$r.squared

dt[WS==14 & Model=="Updated" ,]$ET_R2 <- summary(lm(ws14_vyr[VYR<2021 & VYR > 2010,]$UP_ET ~ ws14_vyr[VYR<2021 & VYR > 2010,]$ET))$r.squared
dt[WS==18 & Model=="Updated",]$ET_R2 <- summary(lm(ws18_vyr[VYR<2021 & VYR > 2010,]$UP_ET ~ ws18_vyr[VYR<2021 & VYR > 2010,]$ET))$r.squared
dt[WS==14 & Model=="Original" ,]$ET_R2 <- summary(lm(ws14_vyr[VYR<2021 & VYR > 2010,]$OG_ET ~ ws14_vyr[VYR<2021 & VYR > 2010,]$ET))$r.squared
dt[WS==18 & Model=="Original",]$ET_R2 <- summary(lm(ws18_vyr[VYR<2021 & VYR > 2010,]$OG_ET ~ ws18_vyr[VYR<2021 & VYR > 2010,]$ET))$r.squared

ws14_up_mae <- mean(abs(ws14_vyr[VYR<2021 & VYR > 2010,]$UP_ET - ws14_vyr[VYR<2021 & VYR > 2010,]$ET))
ws18_up_mae <- mean(abs(ws18_vyr[VYR<2021 & VYR > 2010,]$UP_ET - ws18_vyr[VYR<2021 & VYR > 2010,]$ET))

ws14_up_mae_og <- mean(abs(ws14_vyr[VYR<2021 & VYR > 2010,]$OG_ET - ws14_vyr[VYR<2021 & VYR > 2010,]$ET))
ws18_up_mae_og <- mean(abs(ws18_vyr[VYR<2021 & VYR > 2010,]$OG_ET - ws18_vyr[VYR<2021 & VYR > 2010,]$ET))


dt[WS==14 & Model=="Updated",]$ET_MAE <- ws14_up_mae
dt[WS==18 & Model=="Updated",]$ET_MAE <- ws18_up_mae
dt[WS==14 & Model=="Original" ,]$ET_MAE <- ws14_up_mae_og
dt[WS==18 & Model=="Original" ,]$ET_MAE <- ws18_up_mae_og


dt[WS==14 & Model=="Updated",]$ET_Frac_Error <- ws14_up_mae/mean(ws14_vyr[VYR<2021 & VYR > 2010,]$ET)
dt[WS==18 & Model=="Updated",]$ET_Frac_Error <- ws18_up_mae/mean(ws18_vyr[VYR<2021 & VYR > 2010,]$ET)
dt[WS==14 & Model=="Original",]$ET_Frac_Error <- ws14_up_mae_og/mean(ws14_vyr[VYR<2021 & VYR > 2010,]$ET)
dt[WS==18 & Model=="Original",]$ET_Frac_Error <- ws18_up_mae_og/mean(ws18_vyr[VYR<2021 & VYR > 2010,]$ET)

dt[WS==14 & Model=="Updated",]$RMSE <- sqrt(mean((ws14_vyr[VYR<2021 & VYR > 2010,]$ET - ws14_vyr[VYR<2021 & VYR > 2010,]$UP_ET)^2, na.rm=T))
dt[WS==18 & Model=="Updated",]$RMSE <- sqrt(mean((ws18_vyr[VYR<2021 & VYR > 2010,]$ET - ws18_vyr[VYR<2021 & VYR > 2010,]$UP_ET)^2, na.rm=T))
dt[WS==14 & Model=="Original",]$RMSE <- sqrt(mean((ws14_vyr[VYR<2021 & VYR > 2010,]$ET - ws14_vyr[VYR<2021 & VYR > 2010,]$OG_ET)^2, na.rm=T))
dt[WS==18 & Model=="Original",]$RMSE <- sqrt(mean((ws18_vyr[VYR<2021 & VYR > 2010,]$ET - ws18_vyr[VYR<2021 & VYR > 2010,]$OG_ET)^2, na.rm=T))

dt[WS==14 & Model=="Updated",]$Bias <- mean((ws14_vyr[VYR<2021 & VYR > 2010,]$ET - ws14_vyr[VYR<2021 & VYR > 2010,]$UP_ET), na.rm=T)/mean(ws14_vyr[VYR<2021 & VYR > 2010,]$ET) * 100
dt[WS==18 & Model=="Updated",]$Bias <- mean((ws18_vyr[VYR<2021 & VYR > 2010,]$ET - ws18_vyr[VYR<2021 & VYR > 2010,]$UP_ET), na.rm=T)/mean(ws18_vyr[VYR<2021 & VYR > 2010,]$ET) * 100
dt[WS==14 & Model=="Original" ,]$Bias <- mean((ws14_vyr[VYR<2021 & VYR > 2010,]$ET - ws14_vyr[VYR<2021 & VYR > 2010,]$OG_ET), na.rm=T)/mean(ws14_vyr[VYR<2021 & VYR > 2010,]$ET) * 100
dt[WS==18 & Model=="Original" ,]$Bias <- mean((ws18_vyr[VYR<2021 & VYR > 2010,]$ET - ws18_vyr[VYR<2021 & VYR > 2010,]$OG_ET), na.rm=T)/mean(ws18_vyr[VYR<2021 & VYR > 2010,]$ET) * 100

