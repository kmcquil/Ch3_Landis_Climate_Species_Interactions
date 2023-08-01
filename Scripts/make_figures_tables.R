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
## Figure 1. Study area created in arc map 
## Supplementary Figure 1. Ecoregions map created in arc map 

#############################################################################################
## Figure 2. Comparison of flux tower ET and NEE from the updated and original models 

# Flux tower observed ET and NEE
flux_obs <- fread(paste0(home, 'Calibration/Data/CwtFluxTower/cwt_monthly_fluxes.csv'))
flux_obs$date <- as.Date(flux_obs$date)
flux_obs$Year <- year(flux_obs$date)
flux_obs$Month <- month(flux_obs$date)

# Updated model 
start_time <- 1999
monthly_log <- fread(paste0(home, "Models/Landis_Flux_Tower_V1_ZR_19012023/NECN-succession-monthly-log.csv"))
monthly_log$Year <- monthly_log$Time + start_time
monthly_log$ET_Updated <- (monthly_log$AvgTranspiration + monthly_log$AvgEvaporation) * 10
flux_obs <- merge(monthly_log[,c("Time","Year", "Month", "ET_Updated", "avgNEE")], flux_obs[,c("Year", "Month", "ET", "NEE")], by=c("Year", "Month"), all.x=T)
colnames(flux_obs) <- c('Year', 'Month', "Time", "ET_Updated", "NEE_Updated",'ET_obs', 'NEE_obs')

monthly_log_og <- fread(paste0(home, "Models/Landis_Flux_Tower_V1_ZR_19012023_OG/NECN-succession-monthly-log.csv"))
monthly_log_og$Year <- monthly_log_og$Time + start_time
monthly_log_og$ET_Original <- (monthly_log_og$Transpiration + monthly_log_og$AvgEvaporation) * 10 
monthly_log_og_sub <- monthly_log_og[,c("Year", "Month", "ET_Original", "avgNEE")]
colnames(monthly_log_og_sub) <- c("Year", "Month", "ET_Original", "NEE_Original")
flux_obs <- merge(flux_obs, monthly_log_og_sub, by=c("Year", "Month"), all.x=T)

pal <- met.brewer('Hokusai1', n=3)[c(3,1,2)]
colors <- c("Updated" = pal[2], "Original" = pal[1], "Observed"="Black")
flux_obs$Date <- as.Date(paste0(flux_obs$Year, sprintf("%02d", flux_obs$Month), "01"), format="%Y%m%d")

plot_monthly_et <-ggplot(flux_obs[Year >= 2011 & Year <= 2021,]) + 
  geom_point(aes(x=Date, y=ET_obs, color="Observed"), size = 2.5) + 
  geom_line(aes(x=Date, y=ET_Original, color="Original"), size=1.1, alpha=0.7) + 
  geom_line(aes(x=Date, y=ET_Updated, color="Updated"), size=1.1, alpha=0.7) + 
  theme_bw() + 
  labs(x="Date", 
       y=expression(ET~(mm~mo^{-1})), 
       color="Legend") + 
  scale_color_manual(values=colors) + 
  theme(legend.position = "none")


scatter_monthly_et <- ggplot(flux_obs[Year >= 2011 & Year <= 2021,]) + 
  geom_point(aes(x=ET_obs, y=ET_Updated, color="Updated"), alpha=0.7) + 
  geom_point(aes(x=ET_obs, y=ET_Original, color="Original"), alpha=0.7) + 
  theme_bw() + 
  labs(x="Observed", 
       y="Modeled", 
       color="Legend") + 
  scale_color_manual(values=colors) + 
  theme(legend.position = "none") + 
  geom_abline(intercept = 0, slope=1) + 
  ylim(0, 160) + 
  xlim(0, 160)

plot_monthly_nee <- ggplot(flux_obs[Year >= 2011 & Year <= 2015,]) + 
  geom_point(aes(x=Date, y=NEE_obs, color="Observed"), size = 2.5) + 
  geom_line(aes(x=Date, y=NEE_Original, color="Original"), size=1.1, alpha=0.7) + 
  geom_line(aes(x=Date, y=NEE_Updated, color="Updated"), size=1.1, alpha=0.7) + 
  theme_bw() + 
  labs(x="Date", 
       y=expression(NEE~(g~C~m^{-2}~mo^{-1})),
       color="Legend") + 
  theme(legend.position = "none") + 
  scale_color_manual(values=colors)

scatter_monthly_nee <- ggplot(flux_obs[Year >= 2011 & Year <= 2015,]) + 
  geom_point(aes(x=NEE_obs, y=NEE_Updated, color="Updated"), alpha=0.7) + 
  geom_point(aes(x=NEE_obs, y=NEE_Original, color="Original"), alpha=0.7) + 
  theme_bw() + 
  labs(x="Observed",
       y="Modeled",
       color="Legend") + 
  scale_color_manual(values=colors) + 
  theme(legend.position = "none") + 
  geom_abline(intercept = 0, slope=1) + 
  ylim(-150, 100) + 
  xlim(-150, 100)


legend <- ggplot(flux_obs[Year >= 2011 & Year <= 2015,]) + 
  geom_point(aes(x=Date, y=NEE_obs, color="Observed"), size = 3) + 
  geom_line(aes(x=Date, y=NEE_Original, color="Original"), size=1.2, alpha=0.7) + 
  geom_line(aes(x=Date, y=NEE_Updated, color="Updated"), size=1.2, alpha=0.7) + 
  theme_bw() + 
  labs(x="Date", 
       y=expression(NEE~(g~C~mo^{-1})),
       color="") + 
  theme(legend.position = "bottom") + 
  scale_color_manual(values=colors)

legend <- get_legend(legend)

#tiff(paste0(home, "Figures/monthly_et_nee.tif"), width=9, height=5, units='in', res=500)
#p <- plot_grid(plot_monthly_et, plot_monthly_nee, align='h', axis='b',nrow=2, ncol=1, labels="AUTO")
#plot_grid(p, legend, rel_heights = c(1,0.05),nrow=2, ncol=1)
#dev.off()

p <- plot_grid(plot_monthly_et, scatter_monthly_et, 
               plot_monthly_nee, scatter_monthly_nee, 
               align='h', axis='b',nrow=2, ncol=2, rel_widths = c(2.5,1), labels="AUTO")

tiff(paste0(home, "Figures/monthly_et_nee_with_scatter_plot.tif"), width=9, height=5, units='in', res=500)
plot_grid(p, legend, rel_heights = c(1,0.05),nrow=2, ncol=1)
dev.off()






#############################################################################################
#############################################################################################
## Figure 3. Growing season transpiration relative to biomass for all 31 species 
## compare with the sap flux data 
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
#summary_fraction$SpeciesName <- as.factor(summary_fraction$SpeciesName)
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
## Figure 4. Watershed annual streamflow for each weir for updated vs original 
## with a separate plot for each watershed

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

# ET_TS_Plot <- plot_grid(ws14_plot, ws18_plot, legend,
#                         ncol=1, nrow=3, 
#                         rel_heights = c(1,1,0.1),
#                         labels=c("A     WS14", "B     WS18"))
# tiff(paste0(home, "Figures/ws_et_ts.tif"), width=7, height=5, units='in', res=500)
# ET_TS_Plot
# dev.off()
# 

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
## Figure 5. Mapped average annual ET from 2011 - 2021 comparing oringal and updated models. 

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
## Figure 6. ET, NEE, and LAI along environmental gradients 

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

# tiff(paste0(home, "Figures/environmental_gradients.tif"), width=9, height=9, units='in', res=500)
# plot_grid(plots, legend, nrow=2, ncol=1, rel_heights=c(1, 0.05))
# dev.off()

plots <- plot_grid(plot_elev_et, plot_elev_nee,
                   plot_twi_et, plot_twi_nee,
                   plot_diff_et, plot_diff_nee,
                   nrow=2, byrow=F, labels="AUTO")
tiff(paste0(home, "Figures/environmental_gradients_et_nee.tif"), width=9, height=5, units='in', res=500)
plot_grid(plots, legend, nrow=2, ncol=1, rel_heights=c(1, 0.05))
dev.off()

##########################################################################################################
# Supplementary Figure 2: Make a figure of the 1:1 relationship of the flux tower observations and watershed observations 

ws14_et <- ws14_vyr[,c("VYR", "ET")]
ws14_et$Label <- rep("WS14", nrow(ws14_et))

ws18_et <- ws18_vyr[,c("VYR", "ET")]
ws18_et$Label <- rep("WS18", nrow(ws18_et))

flux_obs$VYR <- ifelse(flux_obs$Month < 6, flux_obs$Year - 1, flux_obs$Year)
flux_annual <- flux_obs[,.(FT_ET=sum(ET_obs)), .(VYR)]

et_comp <- rbind(ws14_et[VYR >= 2011 & VYR <= 2020,], ws18_et[VYR >= 2011 & VYR <= 2020,])
et_comp <- merge(et_comp, flux_annual[VYR >= 2011 & VYR <= 2020,], by="VYR", all.x=T)

r14 <- summary(lm(et_comp[Label == "WS14",]$ET ~ et_comp[Label == "WS14",]$FT_ET))$r.squared
r18 <- summary(lm(et_comp[Label == "WS18",]$ET ~ et_comp[Label == "WS18",]$FT_ET))$r.squared

summary(lm(et_comp[Label == "WS14" & (!VYR == 2018),]$ET ~ et_comp[Label == "WS14"& (!VYR == 2018),]$FT_ET))
summary(lm(et_comp[Label == "WS18" & (!VYR == 2018),]$ET ~ et_comp[Label == "WS18" & (!VYR == 2018),]$FT_ET))

ws_vs_ft <- ggplot(et_comp) + 
  geom_point(aes(x=FT_ET, y=ET, color=Label), size=3, alpha = 0.6)+ 
  theme_classic() + 
  ylim(650, 1300) + 
  xlim(650, 1300) + 
  geom_abline(slope=1, intercept=0, color="black",linetype = "dashed") + 
  scale_color_manual(values= c(pal[2], pal[1]), name="") + 
  xlab(expression(Observed~Flux~Tower~ET~(mm~yr^{-1}))) + 
  ylab(expression(Observed~Watershed~ET~(mm~yr^{-1}))) + 
  theme(legend.position = c(0.75, 0.2))

tiff(paste0(home, "Figures/comparison_ft_ws_et.tif"), width=4, height=4, units='in', res=500)
ws_vs_ft
dev.off()

#############################################################################################
## Table 1: Species biomass in the flux tower and watershed model 

# Calculate species biomass for each model 
ft_ic <- fread(paste0(home, "SetUp/FluxTower/InitialCommunities/species_biomass.csv"))
ft_spp_biomass <- ft_ic[,.(FT_Biomass=sum(Biomass)), .(SPCD, LandisCode)]
ws_ic <- fread(paste0(home, "SetUp/InitialCommunities/mapcode_species_biomass.csv"))
ws_spp_biomass <- ws_ic[,.(WS_Biomass=sum(Biomass)), .(SPCD, LandisCode)]

# This csv is from chapter 1. I associated the spcd number to the species common name and classified them as diffuse, ring, or tracheid 
sub_unique_species <- fread(paste0(home, "SetUp/InitialCommunities/RileyFC/full_sbr_top50_species_classification.csv"))
spp_biomass <- merge(ws_spp_biomass, ft_spp_biomass, all.x=T, all.y=T, by=c("SPCD", "LandisCode"))
spp_biomass <- merge(spp_biomass, sub_unique_species, all.x=T, by="SPCD")
spp_biomass <- spp_biomass[order(LandisCode),]

fwrite(spp_biomass, paste0(home, "Figures/spp_biomass_table.csv"))


ft_spp_biomass[order(FT_Biomass)]
ws_spp_biomass[order(WS_Biomass)]

##########################################################################################################
# Table 2. Flux tower performance statistics 
# Calculate R2 and MAE of observed vs modeled ET and NEE 
stats_dt <- data.table(Variable=c("ET_Updated", "ET_Original", "NEE_Updated", "NEE_Original"),
                       R2=as.numeric(c(NA, NA, NA, NA)), 
                       R2_GS=as.numeric(c(NA, NA, NA, NA)), 
                       R2_yearly=as.numeric(c(NA, NA, NA, NA)),
                       MAE_monthly=as.numeric(c(NA, NA, NA, NA)), 
                       RMSE_monthly=as.numeric(c(NA, NA, NA, NA)), 
                       Bias_monthly=as.numeric(c(NA, NA, NA, NA)), 
                       Bias_yearly=as.numeric(c(NA, NA, NA, NA)), 
                       MAE_yearly=as.numeric(c(NA, NA, NA, NA)), 
                       Percent_error_yearly=as.numeric(c(NA, NA, NA, NA)))

stats_dt[Variable == "ET_Updated",]$R2 <- summary(lm(flux_obs$ET_Updated ~ flux_obs$ET_obs))$r.squared
stats_dt[Variable == "NEE_Updated",]$R2 <- summary(lm(flux_obs$NEE_Updated ~ flux_obs$NEE_obs))$r.squared

stats_dt[Variable == "ET_Updated",]$R2_GS <- summary(lm(flux_obs[Month >= 4 & Month<=9,]$ET_Updated ~ flux_obs[Month >= 4 & Month<=9,]$ET_obs))$r.squared
stats_dt[Variable == "NEE_Updated",]$R2_GS <- summary(lm(flux_obs[Month >= 4 & Month<=9,]$NEE_Updated ~ flux_obs[Month >= 4 & Month<=9,]$NEE_obs))$r.squared

stats_dt[Variable == "ET_Updated",]$MAE_monthly <- mean(abs(flux_obs$ET_obs - flux_obs$ET_Updated), na.rm=T)
stats_dt[Variable == "NEE_Updated",]$MAE_monthly <- mean(abs(flux_obs$NEE_obs - flux_obs$NEE_Updated), na.rm=T)

stats_dt[Variable == "ET_Updated",]$RMSE_monthly <- sqrt(mean((flux_obs$ET_obs - flux_obs$ET_Updated)^2, na.rm=T))
stats_dt[Variable == "NEE_Updated",]$RMSE_monthly <- sqrt(mean((flux_obs$NEE_obs - flux_obs$NEE_Updated)^2, na.rm=T))

# percent bias is calculated by taking the average of (actual - predicted) / abs(actual)
stats_dt[Variable == "ET_Updated",]$Bias_monthly <- mean((flux_obs$ET_obs - flux_obs$ET_Updated)/abs(flux_obs$ET_obs), na.rm=T) * 100
stats_dt[Variable == "NEE_Updated",]$Bias_monthly <- mean((flux_obs$NEE_obs - flux_obs$NEE_Updated)/abs(flux_obs$NEE_obs), na.rm=T) * 100

annual <- flux_obs[Year >= 2011 & Year <= 2021, .(ET_obs=sum(ET_obs),
                                                  ET_Original=sum(ET_Original), 
                                                  ET_Updated=sum(ET_Updated), 
                                                  NEE_obs=sum(NEE_obs), 
                                                  NEE_Original=sum(NEE_Original), 
                                                  NEE_Updated=sum(NEE_Updated)), .(Year)]
stats_dt[Variable == "ET_Updated",]$MAE_yearly <- mean(abs(annual$ET_obs - annual$ET_Updated), na.rm=T)
stats_dt[Variable == "ET_Updated",]$Percent_error_yearly <- mean(abs(annual$ET_Updated - annual$ET_obs), na.rm=T)/mean(annual$ET_obs, na.rm=T)
stats_dt[Variable == "ET_Updated",]$R2_yearly <- summary(lm(annual$ET_Updated ~ annual$ET_obs))$r.squared

stats_dt[Variable == "NEE_Updated",]$MAE_yearly <- mean(abs(annual$NEE_obs - annual$NEE_Updated), na.rm=T)
stats_dt[Variable == "NEE_Updated",]$Percent_error_yearly <- mean(abs(annual$NEE_Updated - annual$NEE_obs), na.rm=T)/mean(annual$NEE_obs, na.rm=T)
stats_dt[Variable == "NEE_Updated",]$R2_yearly <- summary(lm(annual$NEE_Updated ~ annual$NEE_obs))$r.squared

stats_dt[Variable == "ET_Updated",]$Bias_yearly <- mean((annual$ET_obs - annual$ET_Updated)/abs(annual$ET_obs), na.rm=T) * 100
stats_dt[Variable == "NEE_Updated",]$Bias_yearly <- mean((annual$NEE_obs - annual$NEE_Updated)/abs(annual$NEE_obs), na.rm=T) * 100



stats_dt[Variable == "ET_Original",]$R2 <- summary(lm(flux_obs$ET_Original ~ flux_obs$ET_obs))$r.squared
stats_dt[Variable == "NEE_Original",]$R2 <- summary(lm(flux_obs$NEE_Original ~ flux_obs$NEE_obs))$r.squared

stats_dt[Variable == "ET_Original",]$R2_GS <- summary(lm(flux_obs[Month >= 4 & Month<=9,]$ET_Original ~ flux_obs[Month >= 4 & Month<=9,]$ET_obs))$r.squared
stats_dt[Variable == "NEE_Original",]$R2_GS <- summary(lm(flux_obs[Month >= 4 & Month<=9,]$NEE_Original ~ flux_obs[Month >= 4 & Month<=9,]$NEE_obs))$r.squared

stats_dt[Variable == "ET_Original",]$MAE_monthly <- mean(abs(flux_obs$ET_obs - flux_obs$ET_Original), na.rm=T)
stats_dt[Variable == "NEE_Original",]$MAE_monthly <- mean(abs(flux_obs$NEE_obs - flux_obs$NEE_Original), na.rm=T)

##
stats_dt[Variable == "ET_Original",]$RMSE_monthly <- sqrt(mean((flux_obs$ET_obs - flux_obs$ET_Original)^2, na.rm=T))
stats_dt[Variable == "NEE_Original",]$RMSE_monthly <- sqrt(mean((flux_obs$NEE_obs - flux_obs$NEE_Original)^2, na.rm=T))

stats_dt[Variable == "ET_Original",]$Bias_monthly <- mean((flux_obs$ET_obs - flux_obs$ET_Original)/abs(flux_obs$ET_obs), na.rm=T) * 100
stats_dt[Variable == "NEE_Original",]$Bias_monthly <- mean((flux_obs$NEE_obs - flux_obs$NEE_Original)/abs(flux_obs$NEE_obs), na.rm=T) * 100


stats_dt[Variable == "ET_Original",]$MAE_yearly <- mean(abs(annual$ET_obs - annual$ET_Original), na.rm=T)
stats_dt[Variable == "ET_Original",]$Percent_error_yearly <- mean(abs(annual$ET_Original - annual$ET_obs), na.rm=T)/mean(annual$ET_obs, na.rm=T)
stats_dt[Variable == "ET_Original",]$R2_yearly <- summary(lm(annual$ET_Original ~ annual$ET_obs))$r.squared

stats_dt[Variable == "NEE_Original",]$MAE_yearly <- mean(abs(annual$NEE_obs - annual$NEE_Original), na.rm=T)
stats_dt[Variable == "NEE_Original",]$Percent_error_yearly <- mean(abs(annual$NEE_Original - annual$NEE_obs), na.rm=T)/mean(annual$NEE_obs, na.rm=T)
stats_dt[Variable == "NEE_Original",]$R2_yearly <- summary(lm(annual$NEE_Original ~ annual$NEE_obs))$r.squared

stats_dt[Variable == "ET_Original",]$Bias_yearly <- mean((annual$ET_obs - annual$ET_Original)/abs(annual$ET_obs), na.rm=T) * 100
stats_dt[Variable == "NEE_Original",]$Bias_yearly <- mean((annual$NEE_obs - annual$NEE_Original)/abs(annual$NEE_obs), na.rm=T) * 100


##############################################################################################
## Table 3. Watershed model performance statistics 

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



dt

mean(dt[Model=="Original",]$Bias)
mean(dt[Model=="Updated",]$Bias)




###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
## Investigate what's going on in vegetation year 2018 
## What's happening with vpd
## What's happening with each species 

# Open precipitation data 
rg <- fread("G:/My Drive/Chapter3/SetUp/Climate/coweeta_precipitation/Data/RG41_daily_1958_2021.csv")
colnames(rg) <- c("YEAR", "MONTH", "DAY", "PCP")
rg$PCP <- rg$PCP * 25.4

## Calculate VPD from relative humidity and look at the time period that ET was low VYR 2018 (june 2018 - may 2019)
## SVP = 610.78 x e^(T / (T +237.3) x 17.2694)
## VPD = SVP x (1 - RH/100)
cs01 <- fread("G:/My Drive/Chapter3/SetUp/Climate/coweeta_temperature/Data/cs01_daily.csv")
cs01$SVP <- (610.78 * exp(cs01$TAVG/(cs01$TAVG + 237.3) * 17.2694))/1000 # divide by 1000 to get kpa 
cs01$VPD <- cs01$SVP * (1-cs01$RHAVG/100)
cs01 <- merge(cs01, rg, by=c("YEAR", "MONTH", "DAY"), all.x=T)
cs01[is.na(PCP),]$PCP <- 0

cs01 <- cs01[YEAR > 2010,]
cs01$Date <- as.Date(paste0(cs01$YEAR, sprintf("%02d", cs01$MONTH), sprintf("%02d", cs01$DAY)), format="%Y%m%d")
cs01$VYR <- ifelse(cs01$MONTH < 6, cs01$YEAR - 1, cs01$YEAR)
cs01$JDay <- as.numeric(format(cs01$Date, "%j"))


cs01$VYR <- as.factor(cs01$VYR)
vpd <- ggplot() + 
  geom_line(aes(x=cs01$JDay, y=cs01$VPD, group=cs01$VYR), color='Grey') + 
  geom_line(aes(x=cs01[VYR == 2018,]$JDay, y=cs01[VYR==2018,]$VPD), color="Red", size=0.5) + 
  theme_classic() + 
  xlab("Day") + 
  ylab("VPD (kPa)")

wind <- ggplot() + 
  geom_line(aes(x=cs01$JDay, y=cs01$WIND, group=cs01$VYR), color='Grey') + 
  geom_line(aes(x=cs01[VYR == 2018,]$JDay, y=cs01[VYR==2018,]$WIND), color="Red", size=0.5) + 
  theme_classic() + 
  xlab("Day") + 
  ylab("Wind")

sr <- ggplot() + 
  geom_line(aes(x=cs01$JDay, y=cs01$TOTALSOLAR, group=cs01$VYR), color='Grey') + 
  geom_line(aes(x=cs01[VYR == 2018,]$JDay, y=cs01[VYR==2018,]$TOTALSOLAR), color="Red", size=0.5) + 
  theme_classic() + 
  xlab("Day") + 
  ylab("Total Solar Radiation")

tempp <- ggplot() + 
  geom_line(aes(x=cs01$JDay, y=cs01$TAVG, group=cs01$VYR), color='Grey') + 
  geom_line(aes(x=cs01[VYR == 2018,]$JDay, y=cs01[VYR==2018,]$TAVG), color="Red", size=0.5) + 
  theme_classic() + 
  xlab("Day") + 
  ylab("Temp")

plot_grid(vpd, wind, sr, tempp)



## Try looking at the monthly comparison 
cs01_monthly <- cs01[,.(VPD=mean(VPD), WIND=mean(WIND), TOTALSOLAR=mean(TOTALSOLAR), TAVG=mean(TAVG)), .(MONTH, VYR)]

vpd <- ggplot() + 
  geom_line(aes(x=cs01_monthly$MONTH, y=cs01_monthly$VPD, group=cs01_monthly$VYR), color='Grey') + 
  geom_line(aes(x=cs01_monthly[VYR==2018,]$MONTH, y=cs01_monthly[VYR==2018,]$VPD), color='Red') + 
  scale_x_continuous(breaks=seq(1, 12, 1)) + 
  theme_classic() + 
  xlab("Month") + 
  ylab("VPD")
  
wind <- ggplot() + 
  geom_line(aes(x=cs01_monthly$MONTH, y=cs01_monthly$WIND, group=cs01_monthly$VYR), color='Grey') + 
  geom_line(aes(x=cs01_monthly[VYR==2018,]$MONTH, y=cs01_monthly[VYR==2018,]$WIND), color='Red') + 
  scale_x_continuous(breaks=seq(1, 12, 1)) + 
  theme_classic() + 
  xlab("Month") + 
  ylab("Wind")

sr <- ggplot() + 
  geom_line(aes(x=cs01_monthly$MONTH, y=cs01_monthly$TOTALSOLAR, group=cs01_monthly$VYR), color='Grey') + 
  geom_line(aes(x=cs01_monthly[VYR==2018,]$MONTH, y=cs01_monthly[VYR==2018,]$TOTALSOLAR), color='Red') + 
  scale_x_continuous(breaks=seq(1, 12, 1)) + 
  theme_classic() + 
  xlab("Month") + 
  ylab("Solar Radiation")

tempp <- ggplot() + 
  geom_line(aes(x=cs01_monthly$MONTH, y=cs01_monthly$TAVG, group=cs01_monthly$VYR), color='Grey') + 
  geom_line(aes(x=cs01_monthly[VYR==2018,]$MONTH, y=cs01_monthly[VYR==2018,]$TAVG), color='Red') + 
  scale_x_continuous(breaks=seq(1, 12, 1)) + 
  theme_classic() + 
  xlab("Month") + 
  ylab("Temperature")

plot_grid(vpd, wind, sr, tempp)

## average these by vegetation year and then plot 
cs01_yearly <- cs01[MONTH >= 4 & MONTH <= 9,.(VPD=mean(VPD, na.rm=T), WIND=mean(WIND, na.rm=T), TOTALSOLAR=mean(TOTALSOLAR, na.rm=T), 
                                              TAVG=mean(TAVG, na.rm=T), PCP=sum(PCP, na.rm=T), PPFD=mean(PPFD, na.rm=T), 
                                              RHAVG=mean(RHAVG, na.rm=T)), .(VYR)]
cs01_yearly$VYR <- as.numeric(as.character((cs01_yearly$VYR)))
cs01_yearly <- cs01_yearly[VYR >= 2011 & VYR <= 2020,]


vpd <- ggplot(cs01_yearly) + 
  geom_line(aes(x=VYR, y=VPD),color='Red') + 
  geom_point(aes(x=VYR, y=VPD),color='Red') + 
  theme_classic() + 
  #ylim(0, 0.5) + 
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + 
  xlab("VYR") +
  ylab("VPD (kPa)") + 
  geom_vline(xintercept = 2018, linetype="dotted", 
             color = "black", size=0.5)

wind <- ggplot(cs01_yearly) + 
  geom_line(aes(x=VYR, y=WIND),color='Red') + 
  geom_point(aes(x=VYR, y=WIND),color='Red') + 
  theme_classic() + 
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + 
  xlab("VYR") +
  ylab(expression(Wind~Speed~(m~s^{-1}))) + 
  geom_vline(xintercept = 2018, linetype="dotted", 
             color = "black", size=0.5)

sr <- ggplot(cs01_yearly) + 
  geom_line(aes(x=VYR, y=TOTALSOLAR),color='Red') + 
  geom_point(aes(x=VYR, y=TOTALSOLAR),color='Red') + 
  theme_classic() + 
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + 
  xlab("VYR") +
  ylab("SR (Langleys)") + 
  geom_vline(xintercept = 2018, linetype="dotted", 
             color = "black", size=0.5)

tempp <- ggplot(cs01_yearly) + 
  geom_line(aes(x=VYR, y=TAVG),color='Red') + 
  geom_point(aes(x=VYR, y=TAVG),color='Red') + 
  theme_classic() + 
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + 
  xlab("VYR") +
  ylab("T (\u00B0C)")+ 
  geom_vline(xintercept = 2018, linetype="dotted", 
             color = "black", size=0.5)

pcp <- ggplot(cs01_yearly) + 
  geom_line(aes(x=VYR, y=PCP),color='Red') + 
  geom_point(aes(x=VYR, y=PCP),color='Red') + 
  theme_classic() + 
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + 
  xlab("VYR") +
  ylab(expression(P~(mm~yr^{-1})))+ 
  geom_vline(xintercept = 2018, linetype="dotted", 
             color = "black", size=0.5)


# Photosynthetically Active Photon Flux Density in micromoles of photons per meter squared per second.
ppfd <- ggplot(cs01_yearly) + 
  geom_line(aes(x=VYR, y=PPFD),color='Red') + 
  geom_point(aes(x=VYR, y=PPFD),color='Red') + 
  theme_classic() + 
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + 
  xlab("VYR") +
  ylab(expression(PPFD~(micomoles~m^{-2}~s^{-1}))) + 
  geom_vline(xintercept = 2018, linetype="dotted", 
             color = "black", size=0.5)

rh <- ggplot(cs01_yearly) + 
  geom_line(aes(x=VYR, y=RHAVG),color='Red') + 
  geom_point(aes(x=VYR, y=RHAVG),color='Red') + 
  theme_classic() + 
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + 
  xlab("VYR") +
  ylab("Rh (%)") + 
  geom_vline(xintercept = 2018, linetype="dotted", 
             color = "black", size=0.5)


plot_grid(tempp, vpd, sr, ppfd, pcp, wind, nrow=3, ncol=2)

# actual ET - make a dt of all three sources 
flux_obs$VYR <- ifelse(flux_obs$Month < 6, flux_obs$Year - 1, flux_obs$Year)
flux_annual <- flux_obs[,.(ET_Updated=sum(ET_Updated), ET_Original=sum(ET_Original), ET_obs=sum(ET_obs)), .(VYR)]
flux_annual <- flux_annual[VYR >= 2011 & VYR<= 2020,]
et_dt <- data.table(VYR=flux_annual$VYR, FluxTower=flux_annual$ET_obs, WS14=ws14_vyr[VYR >= 2011 & VYR<= 2020,]$ET, WS18=ws18_vyr[VYR >= 2011 & VYR<= 2020,]$ET)
et_dt_long <- melt(et_dt, id.vars=c("VYR"), measure.vars=c("FluxTower", "WS14", "WS18"))
  
ggplot(et_dt_long) + 
  geom_line(aes(x=VYR, y=value, color=variable)) + 
  geom_point(aes(x=VYR, y=value, color=variable)) + 
  theme_classic() + 
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + 
  xlab("VYR") +
  ylab(expression(ET~(mm~yr^{-1}))) + 
  geom_vline(xintercept = 2018, linetype="dotted", 
             color = "black", size=0.5)



###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
## Compare precipitation of the modeled average across the watershed 
## with that of the Gage right next to each watershed 

# WS14 is next to RG41 and WS18 is next to RG96
rg41 <- fread("G:/My Drive/Chapter3/SetUp/Climate/coweeta_precipitation/Data/RG41_daily_1958_2021.csv")
rg41$VYR <- ifelse(rg41$MONTH < 6, rg41$YEAR-1, rg41$YEAR)
rg41$RRG41 <- rg41$RRG41*25.4
rg41 <- rg41[,.(RG41_P=sum(RRG41, na.rm=T)), .(VYR)]
ws18_vyr <- merge(ws18_vyr, rg41, by="VYR", all.x=T)
ws18_vyr$RG41_ET <- ws18_vyr$RG41_P - ws18_vyr$mmyear
#mean(ws18_vyr[VYR >= 2011 & VYR<= 2020,]$PCP - ws18_vyr[VYR >= 2011 & VYR<= 2020,]$RG41_P) # 48 mm = so the gage less precip  
#mean(((ws18_vyr[VYR >= 2011 & VYR<= 2020,]$PCP - ws18_vyr[VYR >= 2011 & VYR<= 2020,]$RG41_P)/ws18_vyr[VYR >= 2011 & VYR<= 2020,]$RG41_P)*100) # = 2.4% greater 


rg96 <- fread("G:/My Drive/Chapter3/SetUp/Climate/coweeta_precipitation/Data/RG96_daily_1943_2021.csv")
rg96$VYR <- ifelse(rg96$MONTH < 6, rg96$YEAR-1, rg96$YEAR)
rg96$RRG96 <- rg96$RRG96*25.4
rg96 <- rg96[,.(RG96_P=sum(RRG96, na.rm=T)), .(VYR)]
ws14_vyr <- merge(ws14_vyr, rg96, by="VYR", all.x=T)
ws14_vyr$RG96_ET <- ws14_vyr$RG96_P - ws14_vyr$mmyear

#mean(ws14_vyr[VYR >= 2011 & VYR<= 2020,]$PCP - ws14_vyr[VYR >= 2011 & VYR<= 2020,]$RG96_P) # -107mm = so the gage is actually more precip 
#mean(((ws14_vyr[VYR >= 2011 & VYR<= 2020,]$PCP - ws14_vyr[VYR >= 2011 & VYR<= 2020,]$RG96_P)/ws14_vyr[VYR >= 2011 & VYR<= 2020,]$RG96_P)*100) #=-4.8% lower 

ws14_vyr_long <- melt(ws14_vyr, id.vars=c("VYR"), measure.vars=c("ET", "UP_ET", "OG_ET", "RG96_ET"))
ws14_plot <- ggplot(ws14_vyr_long[VYR >= 2011 & VYR <= 2020,]) + 
  geom_line(aes(x=VYR, y=value, color=variable), size=1.25, alpha=0.7) + 
  geom_point(aes(x=VYR, y=value, color=variable), size =3, alpha=0.7) + 
  #scale_color_manual(values= c("black", pal[2], pal[1], pal[3]), name="") + 
  theme_bw() + 
  xlab("") + 
  ylab(expression(ET~(mm~yr^{-1}))) + 
  #theme(legend.position="none") + 
  scale_x_continuous(breaks=seq(2011, 2020,1))

ws18_vyr_long <- melt(ws18_vyr, id.vars=c("VYR"), measure.vars=c("ET", "UP_ET", "OG_ET", "RG41_ET"))
ws18_plot <- ggplot(ws18_vyr_long[VYR >= 2011 & VYR <= 2020,]) + 
  geom_line(aes(x=VYR, y=value, color=variable), size=1.25, alpha=0.7) + 
  geom_point(aes(x=VYR, y=value, color=variable), size =3, alpha=0.7) + 
  #scale_color_manual(values= c("black", pal[2], pal[1]), name="") +  
  theme_bw() + 
  xlab("Vegetation Year") + 
  ylab(expression(ET~(mm~yr^{-1}))) + 
  #theme(legend.position="none")+ 
  scale_x_continuous(breaks=seq(2011, 2020,1))


###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
## Put something together for chris abot flux tower and watershed ET discrepancy

## Calculate with water year 
## Calculate with vegetation year 
## Use the precipitation right next to the watershed 
## compare to flux tower 

## Make a figure for the water year
## Make a figure for the vegetation year 
## Check that they both show this happening 
## Send to chris and see what he thinks 

# create a dataframe of daily precipitation and streamflow 

# VYR = June - May 
# WYR = Nov - Oct

flux_obs$VYR <- ifelse(flux_obs$Month < 6, flux_obs$Year - 1, flux_obs$Year)
flux_obs$WYR <- ifelse(flux_obs$Month >= 11, flux_obs$Year + 1, flux_obs$Year)
flux_annual_vyr <- flux_obs[,.(Flux_ET=sum(ET_obs)), .(VYR)]
flux_annual_wyr <- flux_obs[,.(Flux_ET=sum(ET_obs)), .(WYR)]



# function to convert streamflow in cubic feet per second per square mile to mm/day 
get_mmday <- function(x){
  return(x * (1/2.788e+7) * 304.8 * 86400)
}

rg96 <- fread("G:/My Drive/Chapter3/SetUp/Climate/coweeta_precipitation/Data/RG96_daily_1943_2021.csv")
rg96$VYR <- ifelse(rg96$MONTH < 6, rg96$YEAR-1, rg96$YEAR)
rg96$WYR <- ifelse(rg96$MONTH >= 11, rg96$YEAR + 1, rg96$YEAR)
rg96$RRG96 <- rg96$RRG96*25.4
rg96_vyr <- rg96[,.(RG96_P=sum(RRG96, na.rm=T)), .(VYR)]
rg96_wyr <- rg96[,.(RG96_P=sum(RRG96, na.rm=T)), .(WYR)]

ws18 <- fread("G:/My Drive/Chapter3/Calibration/Data/Streamflow/Data/ws18_daily_flow_1937_2021.csv", fill=T)
ws18$CSM <- as.numeric(ws18$CSM)
ws18[is.na(CSM)]$CSM <- (ws18[which(is.na(ws18$CSM))-1]$CSM + ws18[which(is.na(ws18$CSM))+1]$CSM)/2
ws18$Year <- ifelse(ws18$MO >= 11, ws18$WYR -1, ws18$WYR)
ws18$VYR <- ifelse(ws18$MO <6, ws18$Year -1, ws18$Year)
ws18$mmday <- unlist(lapply(ws18$CSM, get_mmday))
ws18_vyr <- ws18[,.(mmyear=sum(mmday)), .(VYR)]
ws18_wyr <- ws18[,.(mmyear=sum(mmday)), .(WYR)]

ws18_vyr <- merge(rg96_vyr, ws18_vyr, all.x=T, by="VYR")
ws18_vyr$WS18_ET <- ws18_vyr$RG96_P - ws18_vyr$mmyear
ws18_vyr <- merge(ws18_vyr, flux_annual_vyr, by="VYR")
ws18_wyr <- merge(rg96_wyr, ws18_wyr, all.x=T, by="WYR")
ws18_wyr$WS18_ET <- ws18_wyr$RG96_P - ws18_wyr$mmyear
ws18_wyr <- merge(ws18_wyr, flux_annual_wyr, by="WYR")

ws18_wyr[WYR >= 2011 & WYR <= 2020,]
ws18_vyr[VYR >= 2011 & VYR <= 2020,]



rg41 <- fread("G:/My Drive/Chapter3/SetUp/Climate/coweeta_precipitation/Data/RG41_daily_1958_2021.csv")
rg41$VYR <- ifelse(rg41$MONTH < 6, rg41$YEAR-1, rg41$YEAR)
rg41$WYR <- ifelse(rg41$MONTH >= 11, rg41$YEAR + 1, rg41$YEAR)
rg41$RRG41 <- rg41$RRG41*25.4
rg41_vyr <- rg41[,.(RG41_P=sum(RRG41, na.rm=T)), .(VYR)]
rg41_wyr <- rg41[,.(RG41_P=sum(RRG41, na.rm=T)), .(WYR)]

ws14 <- fread("G:/My Drive/Chapter3/Calibration/Data/Streamflow/Data/ws14_daily_flow_1937_2021.csv", fill=T)
ws14$Year <- ifelse(ws14$MO >= 11, ws14$WYR -1, ws14$WYR)
ws14$VYR <- ifelse(ws14$MO <6, ws14$Year -1, ws14$Year)
ws14$mmday <- unlist(lapply(ws14$CSM, get_mmday))
ws14_vyr <- ws14[,.(mmyear=sum(mmday)), .(VYR)]
ws14_wyr <- ws14[,.(mmyear=sum(mmday)), .(WYR)]

ws14_vyr <- merge(rg41_vyr, ws14_vyr, all.x=T, by="VYR")
ws14_vyr$WS14_ET <- ws14_vyr$RG41_P - ws14_vyr$mmyear
ws14_vyr <- merge(ws14_vyr, flux_annual_vyr, by="VYR")
ws14_wyr <- merge(rg41_wyr, ws14_wyr, all.x=T, by="WYR")
ws14_wyr$WS14_ET <- ws14_wyr$RG41_P - ws14_wyr$mmyear
ws14_wyr <- merge(ws14_wyr, flux_annual_wyr, by="WYR")


ws14_wyr[WYR >= 2011 & WYR <= 2020,]
ws14_vyr[VYR >= 2011 & VYR <= 2020,]


# combine the ws18 and ws14 for WYR 
wyr_dt <- merge(ws18_wyr[,c("WYR", "WS18_ET", "Flux_ET")], ws14_wyr[,c("WYR", "WS14_ET")], by="WYR")
wyr_dt_long <- melt(wyr_dt[WYR >= 2012 & WYR <= 2020,], id.vars=c("WYR"), measure.vars=c("WS18_ET", "WS14_ET", "Flux_ET"))
wyr_plot <- ggplot(wyr_dt_long[WYR >= 2012 & WYR <= 2020,]) + 
  geom_line(aes(x=WYR, y=value, color=variable), size=1.25, alpha=0.7) + 
  geom_point(aes(x=WYR, y=value, color=variable), size =3, alpha=0.7) + 
  #scale_color_manual(values= c("black", pal[2], pal[1], pal[3]), name="") + 
  theme_bw() + 
  xlab("WYR") + 
  ylab(expression(ET~(mm~yr^{-1}))) + 
  #theme(legend.position="none") + 
  scale_x_continuous(breaks=seq(2012, 2020,1))


vyr_dt <- merge(ws18_vyr[,c("VYR", "WS18_ET", "Flux_ET")], ws14_vyr[,c("VYR", "WS14_ET")], by="VYR")
vyr_dt_long <- melt(vyr_dt[VYR >= 2012 & VYR <= 2020,], id.vars=c("VYR"), measure.vars=c("WS18_ET", "WS14_ET", "Flux_ET"))
vyr_plot <- ggplot(vyr_dt_long[VYR >= 2012 & VYR <= 2020,]) + 
  geom_line(aes(x=VYR, y=value, color=variable), size=1.25, alpha=0.7) + 
  geom_point(aes(x=VYR, y=value, color=variable), size =3, alpha=0.7) + 
  #scale_color_manual(values= c("black", pal[2], pal[1], pal[3]), name="") + 
  theme_bw() + 
  xlab("VYR") + 
  ylab(expression(ET~(mm~yr^{-1}))) + 
  #theme(legend.position="none") + 
  scale_x_continuous(breaks=seq(2012, 2020,1))


#########################################################################3
## Plot monthly streamflow and precipitation for ws14 and ws 18 

rg41_monthly <- rg41[,.(RRG41=sum(RRG41)), .(YEAR, MONTH)]
rg41_monthly$Date <- as.Date(paste0(rg41_monthly$YEAR, sprintf("%02d",rg41_monthly$MONTH), "01"), format="%Y%m%d")

ws14_monthly <- ws14[,.(Q=sum(mmday)), .(Year, MO)]
ws14_monthly$Date <- as.Date(paste0(ws14_monthly$Year, sprintf("%02d", ws14_monthly$MO), "01"), format="%Y%m%d")

ws14_monthly <- merge(ws14_monthly[,c("Date", "Q")], rg41_monthly[,c("Date", "RRG41")], by="Date")

# water year 2018 was from June 2018 - May 2019 
ggplot(ws14_monthly[Date >= as.Date("2012-01-01") & Date <=  as.Date("2020-12-01"),]) + 
  geom_line(aes(x=Date, y=Q), color="Blue") + 
  geom_line(aes(x=Date, y=RRG41), color="Red") + 
  geom_point(aes(x=Date, y=Q), color="Blue") + 
  geom_point(aes(x=Date, y=RRG41), color="Red") + 
  theme_bw() + 
  geom_vline(xintercept = as.Date("2018-06-01"), linetype="dashed", 
             color = "black", size=1) + 
  geom_vline(xintercept = as.Date("2019-05-01"), linetype="dashed", 
             color = "black", size=1)
  




