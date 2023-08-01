##########################################################################################
##########################################################################################
## Analyze future scenarios
## Script: KM 
## Date 07/2023

library(data.table)
library(raster)
library(ggplot2)
library(MetBrewer)
library(viridis)
library(cowplot)
library(rasterVis)
library(sf)
library(SPEI)
library(stringr)
library(dplyr)
library(zoo)

home <- "/Volumes/GoogleDrive/My Drive/Chapter3/"
folder_home <- "/Volumes/GoogleDrive/My Drive/Chapter3/Models/Future"
#home <- "G:/My Drive/Chapter3/"
#folder_home <- "G:/My Drive/Chapter3/Models/Future"
base_folder_list <- list.dirs(path = folder_home, full.names = TRUE, recursive = FALSE)
base_folder_list <- base_folder_list[c(1, 5, 6, 7)]
climate_scenario_list <- c("No_Change", "RCP85-CSIRO-Mk3-6-0", "RCP85-CNRM-CM5", "RCP85-HadGEM2-ES365")
runs <- c("Run1", "Run2", "Run3", "Run4", "Run5")
start_year <- 1994


combine_logs <- function(base_folder_list, climate_scenario_list, llog){
  
  # Make a list of the run folders and the associated climate scenarios 
  for(i in 1:length(base_folder_list)){
    
    dt <- data.table(file=list.files(base_folder_list[i], full.names=T))
    dt <- dt[grepl("Run", list.files(base_folder_list[i], full.names=T)),]
    dt$Climate_SCenario <- rep(climate_scenario_list[i], nrow(dt))
    if(i == 1){
      full_dt <- dt
    }else{
      full_dt <- rbind(full_dt, dt)
    }
  }
  full_dt$Run <- as.numeric(substr(full_dt$file, nchar(full_dt$file), nchar(full_dt$file)))
  
  # Loop through each run and combine the log data. Label by climate and run 
  for(i in 1:nrow(full_dt)){
    mlog <- fread(paste0(full_dt$file[i], llog))
    mlog$Climate_Scenario <- rep(full_dt$Climate_SCenario[i], nrow(mlog))
    mlog$Run <- rep(full_dt$Run[i], nrow(mlog))
    
    if(i == 1){
      full_log <- mlog
    }else{
      full_log <- rbind(full_log, mlog)
    }
    
  }
  
  return(full_log)
}


pal <- met.brewer('Redon', n=12)[c(3,2, 10, 12)]
xa_pal <- met.brewer('Redon', n=12)[c(1, 3, 11)]

##########################################################################################
##########################################################################################
## Figure 7. Downscaled climate visualized as a 5-year rolling average 

# Create a dt of all monthly data labeled by climate scenario and run 
monthly_log <- combine_logs(base_folder_list, climate_scenario_list, "/NECN-succession-monthly-log.csv")
monthly_log$Year <- monthly_log$Time + start_year
monthly_log <- monthly_log[Year >= 2001,]

# Calculate the average temperature, vpd, and ppt for each climate scenario 
annual_climate <- monthly_log[, .(Temperature=mean(AirTemp),VPD=mean(AvgVPD)), .(Climate_Scenario, Year)]
p <- monthly_log[,.(Precipitation=mean(Precipitation)*10), .(Climate_Scenario, Run, Year, Month)]
p <- p[,.(Precipitation=sum(Precipitation)), .(Climate_Scenario,Run, Year)]
p <- p[,.(Precipitation=mean(Precipitation)), .(Year, Climate_Scenario)]
annual_climate <- merge(annual_climate, p, by=c("Climate_Scenario", "Year"))

# Calculate PET using thornthwaite 
# since the annual heat efficiency index is based on all data included in this function, we have to calculate PET for each year + CS 
lat <- 35.0592
annual_climate$PET <- as.numeric(rep(NA, nrow(annual_climate)))
for(i in 1:nrow(annual_climate)){
  
  cs <- annual_climate$Climate_Scenario[i]
  y <- annual_climate$Year[i]
  sub <- monthly_log[Climate_Scenario == cs & Year == y, ]
  sub <-sub[,.(AirTemp=mean(AirTemp)), .(Year, Month, Climate_Scenario)]
  sub$Date <- as.Date(paste0(sub$Year, sprintf("%02d", sub$Month), "01"), format="%Y%m%d")
  sub <- sub[order(Date),]
  annual_climate$PET[i] <- as.numeric(sum(thornthwaite(sub$AirTemp, lat)))
  
}

annual_climate$PPTPET <- annual_climate$Precipitation/annual_climate$PET
annual_climate$Climate_Scenario <- factor(annual_climate$Climate_Scenario,
                                          levels= climate_scenario_list)


# calculate 5 year rolling average 
annual_climate <- mutate(annual_climate, rolling_temperature=rollmean(Temperature, k=5, fill=NA, align='right'))
annual_climate <- mutate(annual_climate, rolling_precip=rollmean(Precipitation, k=5, fill=NA, align='right'))
annual_climate <- mutate(annual_climate, rolling_pptpet=rollmean(PPTPET, k=5, fill=NA, align='right'))

temp_plot <- ggplot(annual_climate[Year >= 2005, ]) + 
  geom_line(aes(x=Year, y=rolling_temperature, color=Climate_Scenario), size=0.7, alpha=0.75) + 
  scale_color_manual(values=pal, name="Climate Scenario") +
  theme_classic() + 
  ylab(expression(italic("T")~" ("*~degree*C*")")) + 
  xlab("Year") + 
  theme(legend.position = "none")
precip_plot <- ggplot(annual_climate[Year >= 2005,]) + 
  geom_line(aes(x=Year, y=rolling_precip, color=Climate_Scenario), size=0.7, alpha=0.75) + 
  scale_color_manual(values= pal, name="Climate Scenario") +
  theme_classic() + 
  ylab(expression(italic("P")~(mm~yr^{-1}))) + 
  xlab("Year") + 
  theme(legend.position = "none")
ratio_plot <- ggplot(annual_climate[Year >= 2005,]) + 
  geom_line(aes(x=Year, y=rolling_pptpet, color=Climate_Scenario), size=0.7, alpha=0.75) + 
  scale_color_manual(values= pal, name="Climate Scenario", labels=c('No Change', 'Wet (CSIRO)', 'Dry (CNRM)', 'Driest (HadGEM2)')) +
  theme_classic() + 
  ylab(expression(paste(italic(P),":PET"))) + 
  xlab("Year") + 
  #theme(legend.position = "none")
  theme(legend.position = c(1.3, 0.4))

clim_plots <- plot_grid(precip_plot, temp_plot, ratio_plot, nrow=2, ncol=2, align='hv', axis='b', labels="AUTO") 
tiff(paste0(home, "Figures/climate_ts_pet_rolling5.tif"), width=6, height=4, units='in', res=500)
clim_plots
dev.off()


# calculate the magnitude and % change between baseline and future period 
baseline_dt <- annual_climate[Year >= 2001 & Year <= 2020, .(Temperature=mean(Temperature), VPD=mean(VPD), Precipitation=mean(Precipitation), PET=mean(PET), PPTPET=mean(PPTPET)), .(Climate_Scenario)]
future_dt <- annual_climate[Year >= 2041 & Year <= 2060, .(Temperature=mean(Temperature), VPD=mean(VPD), Precipitation=mean(Precipitation), PET=mean(PET), PPTPET=mean(PPTPET)), .(Climate_Scenario)]

mag_change <- cbind(future_dt[,1], future_dt[,2:ncol(future_dt)] - baseline_dt[,2:ncol(future_dt)])
pct_change <- cbind(future_dt[,1], (((future_dt[,2:ncol(future_dt)] - baseline_dt[,2:ncol(future_dt)])/baseline_dt[,2:ncol(future_dt)])*100))


annual_climate[Year >= 2041 & Year <= 2060, .(Temperature=mean(Temperature), VPD=mean(VPD), Precipitation=mean(Precipitation), PET=mean(PET), PPTPET=mean(PPTPET), 
                                              T_sd=sd(Temperature), P_sd=sd(Precipitation), PPTPET_sd=sd(PPTPET)), .(Climate_Scenario)]

##########################################################################################
##########################################################################################
##########################################################################################
## Figure 8. Visualize biomass according to xylem anatomy 

## Create a data table of the annual biomass and transpiration for each species for each climate scenario 
species_table <- fread(paste0(home, "SetUp/InitialCommunities/RileyFC/final_spp_table.csv"))

## Function to open the cal log, get the right columns, summarize, and then delete the cal_log 
climate_scenario_list <- c('No_Change', 'RCP85-CNRM-CM5', 'RCP85-CSIRO-Mk3-6-0', 'RCP85-HadGEM2-ES365')
summarize_cal_log <- function(filepath, cs, r){
  
  cal_log <- fread(filepath, header=TRUE,
                   select=c("Year", "Month","SpeciesName", "CohortBiomass", "Transpiration", "ActualWoodANPP", "ActualLeafANPP"))
  cal_log$Year <- cal_log$Year + start_year
  cal_log$NPP <- cal_log$ActualWoodANPP + cal_log$ActualLeafANPP
  cal_log$Transpiration <- cal_log$Transpiration * 10
  cal_log <- merge(cal_log, species_table[,c("Common_Name", "Type", "LandisCode", "SppGroup")], by.x="SpeciesName", by.y="LandisCode", all.x=T)
  cal_log$SppGroup <- factor(cal_log$SppGroup, levels=c("Maple", "Tulip poplar", "Oak", "Hickory", "Pine", "Other Diffuse", "Other Ring"))
  spp_annual <- cal_log[Year >= 2001 & Year <= 2060,.(Biomass=sum(CohortBiomass)/12, Transpiration=sum(Transpiration), NPP=sum(NPP)), .(SppGroup, Type, Year)]
  spp_annual$Climate_Scenario <- rep(cs, nrow(spp_annual))
  spp_annual$Run <- rep(r, nrow(spp_annual))
  
  rm(cal_log)
  
  return(spp_annual)
  
}

done_files <- list.files(folder_home, full.names=T, pattern="*.csv$")
done_files <- done_files[grepl("cal_summary", done_files)]
for(i in 1:length(base_folder_list)){
  
  for(j in 1:length(runs)){
    
    print(base_folder_list[i])
    print(runs[j])
    if(paste0(folder_home, "/cal_summary_", climate_scenario_list[i], "_", runs[j],".csv") %in% done_files == TRUE){
      next
    }
    dt <- summarize_cal_log(paste0(base_folder_list[i], "/", runs[j], "/NECN-calibrate-log.csv"), climate_scenario_list[i], substr(runs[j], 4, 5))
    gc()
    fwrite(dt, paste0(folder_home, "/cal_summary_", climate_scenario_list[i], "_", runs[j],".csv"))
    
  }
}


done_files <- list.files(folder_home, full.names=T, pattern="*.csv$")
done_files <- done_files[grepl("cal_summary", done_files)]
cal_dt <- do.call("rbind", lapply(done_files, fread))
fwrite(cal_dt, paste0(folder_home, "/cal_summary.csv"))

cal_dt <- fread( paste0(folder_home, "/cal_summary.csv"))
cal_dt_type <- cal_dt[,.(Biomass=sum(Biomass)), .(Year, Type, Climate_Scenario, Run)]
cal_dt_type_total <- cal_dt_type[,.(Total_biomass=sum(Biomass)), .(Year, Climate_Scenario, Run)]
cal_dt_type <- merge(cal_dt_type, cal_dt_type_total, by=c("Year", "Climate_Scenario", "Run"))
cal_dt_type$Pct_Biomass <- cal_dt_type$Biomass/cal_dt_type$Total_biomass
cal_dt_type$Biomass <- cal_dt_type$Biomass/838 # converting to g m-2
cal_dt_type_summarized <- cal_dt_type[,.(Pct_Biomass=mean(Pct_Biomass), Pct_Biomass_sd=sd(Pct_Biomass), 
                                         Biomass=mean(Biomass), Biomass_sd=sd(Biomass)), .(Year, Type)]

biomass_ts <- ggplot(cal_dt_type_summarized) + 
  geom_line(aes(x=Year, y=Biomass, color=Type), size=1) + 
  geom_ribbon(aes(x=Year, ymin=Biomass-Biomass_sd, ymax=Biomass+Biomass_sd, fill=Type), alpha=0.5) + 
  scale_color_manual(values=xa_pal, name="") + 
  scale_fill_manual(values=xa_pal, name="") + 
  theme_classic() + 
  xlab("Year") + 
  ylab(expression(Biomass~(g~m^{2}))) + 
  theme(legend.position = 'none')


fc_future <- cal_dt_type[Year == 2060, .(Pct_Biomass=mean(Pct_Biomass)), .(Type)]
fc_start <- cal_dt_type[Year == 2001, .(Pct_Biomass=mean(Pct_Biomass)), .(Type)]
fc_diff <- cbind(fc_future[,1], (fc_future[,2] - fc_start[,2]))

diffuse_ts <- ggplot(cal_dt_type_summarized[Type == "Diffuse",]) + 
  geom_line(aes(x=Year, y=Biomass, color=Type), size=1) + 
  geom_ribbon(aes(x=Year, ymin=Biomass-Biomass_sd, ymax=Biomass+Biomass_sd, fill=Type), alpha=0.5) + 
  scale_color_manual(values=xa_pal[1], name="") + 
  scale_fill_manual(values=xa_pal[1], name="") + 
  theme_classic() + 
  xlab("Year") + 
  ylab(expression(Biomass~(g~m^{-2}))) + 
  theme(legend.position = 'none')

ring_ts <- ggplot(cal_dt_type_summarized[Type == "Ring",]) + 
  geom_line(aes(x=Year, y=Biomass, color=Type), size=1) + 
  geom_ribbon(aes(x=Year, ymin=Biomass-Biomass_sd, ymax=Biomass+Biomass_sd, fill=Type), alpha=0.5) + 
  scale_color_manual(values=xa_pal[2], name="") + 
  scale_fill_manual(values=xa_pal[2], name="") + 
  theme_classic() + 
  xlab("Year") + 
  ylab(expression(Biomass~(g~m^{-2}))) + 
  theme(legend.position = 'none')

tracheid_ts <- ggplot(cal_dt_type_summarized[Type == "Tracheid",]) + 
  geom_line(aes(x=Year, y=Biomass, color=Type), size=1) + 
  geom_ribbon(aes(x=Year, ymin=Biomass-Biomass_sd, ymax=Biomass+Biomass_sd, fill=Type), alpha=0.5) + 
  scale_color_manual(values=xa_pal[3], name="") + 
  scale_fill_manual(values=xa_pal[3], name="") + 
  theme_classic() + 
  xlab("Year") + 
  ylab(expression(Biomass~(g~m^{-2}))) + 
  theme(legend.position = 'none')



## summarize species biomass by age - specifically look at regeneration 
summarize_cal_log_age <- function(filepath, cs, r){
  
  cal_log <- fread(filepath, header=TRUE,
                   select=c("Year", "Month","SpeciesName", "CohortAge", "CohortBiomass"))
  cal_log$CheckYear <- cal_log$Year - cal_log$CohortAge
  cal_log <- merge(cal_log, species_table[,c("Common_Name", "Type", "LandisCode", "SppGroup")], by.x="SpeciesName", by.y="LandisCode", all.x=T)
  cal_log$SppGroup <- factor(cal_log$SppGroup, levels=c("Maple", "Tulip poplar", "Oak", "Hickory", "Pine", "Other Diffuse", "Other Ring"))
  cal_log$Year <- cal_log$Year + start_year
  spp_annual <- cal_log[CheckYear >= 0 & Year >= 2001 & Year <= 2060 & Month==7,.(Biomass=sum(CohortBiomass)), .(SppGroup, Type, Year)]
  spp_annual$Climate_Scenario <- rep(cs, nrow(spp_annual))
  spp_annual$Run <- rep(r, nrow(spp_annual))
  
  rm(cal_log)
  
  return(spp_annual)
  
}

for(i in 1:length(base_folder_list)){
  for(j in 1:length(runs)){
    dt <- summarize_cal_log_age(paste0(base_folder_list[i], "/", runs[j], "/NECN-calibrate-log.csv"), climate_scenario_list[i], substr(runs[j], 4, 5))
    gc()
    if(i == 1 & j == 1){
      cal_dt_age <- dt
    }else{
      cal_dt_age <- rbind(cal_dt_age, dt)
    }
  }
}

fwrite(cal_dt_age, paste0(folder_home, "/cal_summary_new_growth.csv"))
cal_dt_age <- fread(paste0(folder_home, "/cal_summary_new_growth.csv"))
#cal_dt_type_age <- cal_dt_age[,.(Biomass=mean(Biomass)), .(Year, SppGroup, Type, Climate_Scenario)] # this finds the mean for each CS among all 5 runs 
cal_dt_type_age <- cal_dt_age[,.(Biomass=sum(Biomass)), .(Year, Type, Climate_Scenario, Run)] # get the total in each year + type + age group 
cal_dt_type_age <- cal_dt_type_age[Year==2060,.(Biomass=mean(Biomass)/838, Biomass_sd=sd(Biomass)/838), .(Type)]

new_cohort_biomass_barplot <- ggplot(cal_dt_type_age) + 
  geom_bar(aes(x=Type, y=Biomass, fill=Type),position="dodge", stat="identity" ) + 
  geom_errorbar(aes(x=Type, ymin=Biomass-Biomass_sd, ymax=Biomass+Biomass_sd, group=Type), color='black', width=0.5, position=position_dodge(0.9)) + 
  scale_fill_manual(values=xa_pal, name="") + 
  #scale_x_discrete(labels=c('No Change', 'CSIRO', 'CNRM', 'HadGEM2')) + 
  theme_classic() + 
  ylab(expression(New~Biomass~(g~m^{-2}))) + 
  xlab("Climate Scenario") + 
  theme(legend.position = "none")

biomass_ts <- plot_grid(diffuse_ts, ring_ts, tracheid_ts, nrow=1, ncol=3, align='vh', labels="AUTO")
new_cohort_biomass_barplot_grid <- plot_grid(new_cohort_biomass_barplot, ncol=2, nrow=1, labels="D", rel_widths = c(2, 1))

tiff(paste0(home, "Figures/xa_biomass_ts_barplot.tif"), width=8, height=5, units='in', res=300)
plot_grid(biomass_ts, new_cohort_biomass_barplot_grid, nrow=2, ncol=1, align='h', rel_heights = c(1,1))
dev.off()


cal_dt_type_age$Biomass/sum(cal_dt_type_age$Biomass)

# How did new biomass of each type compare to no change scenario 
cal_dt_age <- fread(paste0(folder_home, "/cal_summary_new_growth.csv"))
cal_dt_type_age <- cal_dt_age[,.(Biomass=sum(Biomass)), .(Year, Type, Climate_Scenario, Run)] # get the total in each year + type + age group 
cal_dt_type_age <- cal_dt_type_age[Year==2060,.(Biomass=mean(Biomass)/838, Biomass_sd=sd(Biomass)/838), .(Type, Climate_Scenario)]

cal_dt_type_age_no_change <- cal_dt_type_age[Climate_Scenario == "No_Change", "Biomass"]
cal_dt_type_age_change_cnrm <- cbind(cal_dt_type_age[Climate_Scenario == "RCP85-CNRM-CM5", c("Climate_Scenario", "Type")], (cal_dt_type_age[Climate_Scenario == "RCP85-CNRM-CM5", "Biomass"] - cal_dt_type_age_no_change)/cal_dt_type_age_no_change)
cal_dt_type_age_change_csiro <- cbind(cal_dt_type_age[Climate_Scenario == "RCP85-CSIRO-Mk3-6-0", c("Climate_Scenario", "Type")], (cal_dt_type_age[Climate_Scenario == "RCP85-CSIRO-Mk3-6-0", "Biomass"] - cal_dt_type_age_no_change)/cal_dt_type_age_no_change)
cal_dt_type_age_change_hadgem <- cbind(cal_dt_type_age[Climate_Scenario == "RCP85-HadGEM2-ES365", c("Climate_Scenario", "Type")], (cal_dt_type_age[Climate_Scenario == "RCP85-HadGEM2-ES365", "Biomass"] - cal_dt_type_age_no_change)/cal_dt_type_age_no_change)


##########################################################################################
##########################################################################################
##########################################################################################
## Figure 9. Visualize the ET, Q, NEE, and LAI from 2041 - 2060 for all scenarios 
## Check for significant differences between scenarios 

# Calculate the average annual LAI and save to a csv 
for(i in 1:length(base_folder_list)){
  
  for(j in 1:length(runs)){
    
    files <- list.files(paste0(base_folder_list[i], "/" ,runs[j], "/NECN"), full.names=T)
    files <- files[grepl("LAI", files)]
    
    if(i == 1 & j == 1){
      files_all <- files
      cs_vector <- rep(climate_scenario_list[i], length(files))
    }else{
      files_all <- c(files_all, files)
      cs_vector <- c(cs_vector, rep(climate_scenario_list[i], length(files)))
    }
  }
  
}


lai_files <- data.table(file = files_all)
lai_files$Climate_Scenario <- cs_vector
lai_files$Year <- sapply(strsplit(basename(lai_files$file), "-"), "[[", 2)
lai_files$Year <- as.numeric(substr(lai_files$Year, 1, nchar(lai_files$Year)-4))
lai_files$Year <- lai_files$Year + start_year
lai_files <- lai_files[Year>= 2001,]

get_mean <- function(x){
  r <- raster(x)
  r[r==0] <- NA
  m <- mean(values(r), na.rm=T)
  return(m)
}

lai_files$MeanLAI <- apply(lai_files[,1], MARGIN=1, FUN=get_mean)
fwrite(lai_files, paste0(folder_home, "/summary_lai.csv"))

lai_files <- fread(paste0(folder_home, "/summary_lai.csv"))
lai_files$Run <- sapply(strsplit(lai_files$file, "/"), "[[", 9)
lai_files$Run <- as.numeric(substr(lai_files$Run, 4, 5))

lai_dt <- lai_files[,.(lai=mean(MeanLAI), lai_sd=sd(MeanLAI)), .(Year)]
lai_ts <- ggplot(lai_dt[Year <= 2060,]) + 
  geom_ribbon(aes(x=Year, ymin=lai-lai_sd, ymax=lai+lai_sd), fill= met.brewer('Hokusai1', n=8)[7], alpha=0.5,) + 
  geom_line(aes(x=Year, y=lai), color='black') +
  theme_classic() + 
  ylim(0, 14) + 
  xlab("Year") + 
  ylab(expression(LAI~(m^{2}~m^{-2})))

short_log <- combine_logs(base_folder_list, climate_scenario_list, "/NECN-succession-log-short.csv")
short_log$ET <- (short_log$Evaporation + short_log$AnnualTrans)*10
short_log$Year <- short_log$Time + start_year
short_log <- short_log[Year >= 2001,]


# Create a data table of ET and NEE from the short log 
short_summary <- short_log[,.(ET=mean(ET), ET_sd=sd(ET), 
                              NEE=mean(NEEC), NEE_sd=sd(NEEC)), .(Year)]
short_summary <- short_summary[Year <= 2060,]
et_ts <- ggplot(short_summary) + 
  geom_ribbon(aes(x=Year, ymin=ET-ET_sd, ymax=ET+ET_sd),  alpha = 0.5, fill = met.brewer('Hokusai1', n=8)[7]) + 
  geom_line(aes(x=Year, y=ET)) + 
  theme_classic() + 
  ylab(expression(ET~(mm~yr^{-1}))) + 
  xlab("")

nee_ts <- ggplot(short_summary) + 
  geom_ribbon(aes(x=Year, ymin=NEE-NEE_sd, ymax=NEE+NEE_sd), alpha = 0.5, fill=met.brewer('Hokusai1', n=8)[7]) + 
  geom_line(aes(x=Year, y=NEE)) + 
  theme_classic() + 
  ylab(expression(NEE~(g~C~yr^{-1}))) + 
  xlab("")

tiff(paste0(home, "Figures/future_et_nee_lai_ts.tif"), width=5, height=6, units='in', res=500)
plot_grid(et_ts, nee_ts, lai_ts, nrow=3, ncol=1, align='v', axis='b', labels="AUTO")
dev.off()

# Merge in the LAI 
short_log <- merge(short_log, lai_files[,c("Year", "MeanLAI", "Climate_Scenario", "Run")], by=c("Climate_Scenario", "Run", "Year"), all.x=T)

# Visualize ET, NEE, and LAI for each climate scenario for the future period 
short_log_future <- short_log[Year >= 2041 & Year <= 2060, ]
short_log_future$Climate_Scenario <- factor(short_log_future$Climate_Scenario, levels=climate_scenario_list)

# But first, calculate whether there are significant differences between groups compared to the no change climate scenario
check_sig_diff <- function(VAR, Scenario){
  no_change <- short_log_future[Climate_Scenario == "No_Change", get(VAR)]
  change <- short_log_future[Climate_Scenario == Scenario, get(VAR)]
  m1 <- wilcox.test(no_change, change)
  m1pval <- m1$p.value
  symbol <- ifelse(m1pval>0.05, "NS", 
                   ifelse(m1pval <= 0.05 & m1pval>0.01, "*", 
                          ifelse(m1pval <= 0.01 & m1pval>0.001, "**", 
                                 ifelse(m1pval <= 0.001, "***"))))
  return(symbol)
  #return(m1pval)
}

et_barplot <- ggplot(short_log_future, aes(x=Climate_Scenario, y=ET)) + 
  geom_boxplot(aes(fill=Climate_Scenario)) + 
  scale_fill_manual(values= pal) + 
  scale_x_discrete(labels = function(x) str_wrap(c('No Change', 'Wet (CSIRO)', 'Dry (CNRM)', 'Driest (HadGEM2)'), width = 9)) + 
  theme_classic() + 
  ylab(expression(ET~(mm~yr^{-1}))) + 
  theme(legend.position = "none") + 
  xlab("") + 
  annotate('text', x=2, y=1500, label=check_sig_diff("ET", "RCP85-CSIRO-Mk3-6-0"), size=5) + 
  annotate('text', x=3, y=1500, label=check_sig_diff("ET", "RCP85-CNRM-CM5"), size=5) + 
  annotate('text', x=4, y=1500, label=check_sig_diff("ET", "RCP85-HadGEM2-ES365"), size=5)

nee_barplot <- ggplot(short_log_future, aes(x=Climate_Scenario, y=NEEC)) + 
  geom_boxplot(aes(fill=Climate_Scenario)) + 
  scale_fill_manual(values= pal) + 
  scale_x_discrete(labels = function(x) str_wrap(c('No Change', 'Wet (CSIRO)', 'Dry (CNRM)', 'Driest (HadGEM2)'), width = 9)) + 
  theme_classic() + 
  ylab(expression(NEE~(g~C~m^{-2}~yr^{-1}))) + 
  theme(legend.position = "none") + 
  xlab("") + 
  annotate('text', x=2, y=420, label=check_sig_diff("NEEC", "RCP85-CSIRO-Mk3-6-0"), size=5) + 
  annotate('text', x=3, y=420, label=check_sig_diff("NEEC", "RCP85-CNRM-CM5"), size=5) + 
  annotate('text', x=4, y=420, label=check_sig_diff("NEEC", "RCP85-HadGEM2-ES365"), size=5)

lai_barplot <- ggplot(short_log_future, aes(x=Climate_Scenario, y=MeanLAI)) + 
  geom_boxplot(aes(fill=Climate_Scenario)) + 
  scale_fill_manual(values= pal) + 
  scale_x_discrete(labels = function(x) str_wrap(c('No Change', 'Wet (CSIRO)', 'Dry (CNRM)', 'Driest (HadGEM2)'), width = 9)) + 
  theme_classic() + 
  ylab(expression(LAI~(m^{2}~m^{-2}))) +  
  theme(legend.position = "none") + 
  xlab("") + 
  annotate('text', x=2, y=9.3, label=check_sig_diff("MeanLAI", "RCP85-CSIRO-Mk3-6-0"), size=5) + 
  annotate('text', x=3, y=9.3, label=check_sig_diff("MeanLAI", "RCP85-CNRM-CM5"), size=5) + 
  annotate('text', x=4, y=9.3, label=check_sig_diff("MeanLAI", "RCP85-HadGEM2-ES365"), size=5)


###########################################################################################################
## Using the water balance, calculate annual streamflow for each scenario 
monthly_log <- combine_logs(base_folder_list, climate_scenario_list, "/NECN-succession-monthly-log.csv")
monthly_log$ET <- (monthly_log$AvgTranspiration + monthly_log$AvgEvaporation)*10
monthly_log$Precipitation <- monthly_log$Precipitation * 10
monthly_log$Year <- monthly_log$Time + start_year
monthly_log$VYR <- ifelse(monthly_log$Month < 6, monthly_log$Year - 1, monthly_log$Year)
monthly_log <- monthly_log[,.(ET=mean(ET), P=mean(Precipitation), NumSites=mean(NumSites)), .(VYR, Month,ClimateRegionIndex, Climate_Scenario)]
modeled_et_vyr <- monthly_log[,.(ET=sum(ET), 
                                 P=sum(P),
                                 NumSites=mean(NumSites)), .(VYR, ClimateRegionIndex, Climate_Scenario)]
modeled_et_vyr$ClimateRegionIndex <- modeled_et_vyr$ClimateRegionIndex + 1
modeled_et_vyr$Catchment <- ifelse(modeled_et_vyr$ClimateRegionIndex %in%(c(1,3,5)), 14, 18)

# weight each region x catchment
modeled_et_vyr$WeightedSites <- as.numeric(rep(NA, nrow(modeled_et_vyr)))
modeled_et_vyr[Catchment == 14,]$WeightedSites <- modeled_et_vyr[Catchment == 14,]$NumSites/699
modeled_et_vyr[Catchment == 18,]$WeightedSites <- modeled_et_vyr[Catchment == 18,]$NumSites/139
mod_et_14 <- modeled_et_vyr[Catchment == 14,.(ET=sum(ET*WeightedSites), P=sum(P*WeightedSites)), .(VYR, Catchment, Climate_Scenario)]
mod_et_18 <- modeled_et_vyr[Catchment == 18,.(ET=sum(ET*WeightedSites), P=sum(P*WeightedSites)), .(VYR, Catchment, Climate_Scenario)]

mod_et_14$Q <- mod_et_14$P - mod_et_14$ET
mod_et_18$Q <- mod_et_18$P - mod_et_18$ET

Q <- rbind(mod_et_14, mod_et_18)
Q$Climate_Scenario <- factor(Q$Climate_Scenario, levels=climate_scenario_list)
Q$RR <- Q$Q/Q$P
Q <- Q[VYR >= 2041,]

Q_agg <- Q[,.(Q=mean(Q), Q_sd=sd(Q), 
              RR=mean(RR), R_sd=sd(RR)), .(Climate_Scenario)]

# make a bar plot to compare average annual Q 
Q <- Q[,.(Q=mean(Q)), .(VYR, Climate_Scenario)]

check_sig_diff <- function(VAR, Scenario){
  no_change <- Q[Climate_Scenario == "No_Change", get(VAR)]
  change <- Q[Climate_Scenario == Scenario, get(VAR)]
  m1 <- wilcox.test(no_change, change)
  m1pval <- m1$p.value
  symbol <- ifelse(m1pval>0.05, "NS", 
                   ifelse(m1pval <= 0.05 & m1pval>0.01, "*", 
                          ifelse(m1pval <= 0.01 & m1pval>0.001, "**", 
                                 ifelse(m1pval <= 0.001, "***"))))
  return(symbol)
  #return(m1pval)
}

q_barplot <- ggplot(Q, aes(x=Climate_Scenario, y=Q)) + 
  geom_boxplot(aes(fill=Climate_Scenario)) + 
  scale_fill_manual(values= pal) + 
  scale_x_discrete(labels = function(x) str_wrap(c('No Change', 'Wet (CSIRO)', 'Dry (CNRM)', 'Driest (HadGEM2)'), width = 9)) + 
  theme_classic() + 
  ylab(expression(Q~(mm~yr^{-1}))) +  
  theme(legend.position = "none") + 
  xlab("") + 
  annotate('text', x=2, y=2500, label=check_sig_diff("Q","RCP85-CSIRO-Mk3-6-0"), size=5) + 
  annotate('text', x=3, y=2500, label=check_sig_diff("Q", "RCP85-CNRM-CM5"), size=5) + 
  annotate('text', x=4, y=2500, label=check_sig_diff("Q", "RCP85-HadGEM2-ES365"), size=5)

tiff(paste0(home, "Figures/future_et_nee_lai_Q_boxplots.tif"), width=8, height=6, units='in', res=500)
plot_grid(et_barplot, nee_barplot,q_barplot, lai_barplot, nrow=2, ncol=2, align='v', axis='b', labels="AUTO")
dev.off()



# Calculate the % difference between the no climate change scenario and the climate change scenarios and create bar plots
# calculate the average and sd of ET and NEE for each climate scenario for the historical and the future period 
short_agg <- short_log_future[,.(ET=mean(ET), ET_sd=sd(ET), NEE=mean(NEEC), NEE_sd=sd(NEEC), 
                                 LAI=mean(MeanLAI), LAI_sd=sd(MeanLAI)), .(Climate_Scenario)]
short_agg <- merge(short_agg, Q_agg, by="Climate_Scenario")
no_change <- rbind(short_agg[1,c(2,4,6, 8)], short_agg[1,c(2,4,6, 8)], short_agg[1,c(2,4,6, 8)])
pct_diff_annual_flux <- cbind(short_agg[-1, 1], ((short_agg[-1,c(2,4,6, 8)] - no_change)/no_change)*100)
pct_diff_annual_flux_long <- melt(pct_diff_annual_flux, id.vars="Climate_Scenario", 
                                  measure.vars=c("ET", "NEE", "LAI", "Q"), 
                                  variable.name="Flux")
mean(pct_diff_annual_flux$ET)
mean(pct_diff_annual_flux$Q)
mean(pct_diff_annual_flux$NEE)
mean(pct_diff_annual_flux$LAI)




##########################################################################################
##########################################################################################
##########################################################################################
## Figure 10. The fraction of annual biomass, transpiration, and npp contributed by each 
## species according to xylem anatomy during the baseline and future period

cal_dt <- fread( paste0(folder_home, "/cal_summary.csv"))
cal_dt_s <- cal_dt[,.(Biomass=mean(Biomass), Transpiration=mean(Transpiration), NPP=mean(NPP)), .(SppGroup, Type, Year, Climate_Scenario)]
cal_dt_st <- cal_dt_s[,.(Biomass=sum(Biomass), Transpiration=sum(Transpiration), NPP=sum(NPP)), .(Type, Year, Climate_Scenario)]
cal_dt_st <- merge(cal_dt_st, annual_climate, by=c("Year", "Climate_Scenario"), all.x=T)

cal_dt_total <-  cal_dt_s[,.(Total_Biomass=sum(Biomass), Total_Transpiration=sum(Transpiration), Total_NPP=sum(NPP)), .(Year, Climate_Scenario)]
cal_dt_st <- merge(cal_dt_st, cal_dt_total, by=c("Year", "Climate_Scenario"), all.x=T)
cal_dt_st$F_T <- cal_dt_st$Transpiration/cal_dt_st$Total_Transpiration
cal_dt_st$F_NPP <- cal_dt_st$NPP/cal_dt_st$Total_NPP
cal_dt_st$F_B <- cal_dt_st$Biomass/cal_dt_st$Total_Biomass

cal_dt_st$PPTPET_r <- floor(cal_dt_st$PPTPET * 2)/4
cal_dt_st$F_T <- cal_dt_st$F_T * 100
cal_dt_st$F_NPP <- cal_dt_st$F_NPP * 100
cal_dt_st$F_B <- cal_dt_st$F_B * 100
cal_dt_st$Climate_Scenario <- factor(cal_dt_st$Climate_Scenario, levels=climate_scenario_list)
cal_dt_st <- cal_dt_st[Year >= 2041 & Year <= 2060,]
type_agg_frac <- cal_dt_st[, .(F_T=mean(F_T), F_NPP=mean(F_NPP), F_B=mean(F_B),
                                                          F_T_sd=sd(F_T), F_NPP_sd=sd(F_NPP), F_B_sd=sd(F_B)), .(Climate_Scenario, Type)]
type_agg_frac$Climate_Scenario <- factor(type_agg_frac$Climate_Scenario, levels=climate_scenario_list)

check_sig_diff <- function(Scenario, SppType, Var){
  no_change <- cal_dt_st[Climate_Scenario == "No_Change" & Type == SppType, get(Var)]
  change <- cal_dt_st[Climate_Scenario == Scenario & Type == SppType, get(Var)]
  m1 <- wilcox.test(no_change, change)
  m1pval <- m1$p.value
  symbol <- ifelse(m1pval>0.05, "NS", 
                   ifelse(m1pval <= 0.05 & m1pval>0.01, "*", 
                          ifelse(m1pval <= 0.01 & m1pval>0.001, "**", 
                                 ifelse(m1pval <= 0.001, "***"))))
  return(symbol)
}

type_f_trans_barplot <- ggplot(type_agg_frac, aes(fill=Climate_Scenario, y=F_T, x=Type)) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_errorbar(aes(x=Type, ymin=F_T-F_T_sd, ymax=F_T+F_T_sd, group=Climate_Scenario), width=0.2, position=position_dodge(0.9)) + 
  scale_fill_manual(values= pal, name="", labels=c("No Change", "Wet (CSIRO)","Dry (CNRM)", "Driest (HadGEM2)")) + 
  theme_classic() + 
  xlab("") +
  ylab("Total Transpiration (%)") + 
  theme(legend.position="none")  + 
  annotate('text', x=0.9, y=0.65, label=check_sig_diff("RCP85-CSIRO-Mk3-6-0", "Diffuse", "F_T"), size=3.2) + 
  annotate('text', x=1.15, y=0.65, label=check_sig_diff("RCP85-CNRM-CM5", "Diffuse", "F_T"), size=3.2) +
  annotate('text', x=1.35, y=0.65, label= check_sig_diff("RCP85-HadGEM2-ES365", "Diffuse", "F_T"), size=3.2) + 
  annotate('text', x=1.9, y=0.5, label=check_sig_diff("RCP85-CSIRO-Mk3-6-0", "Ring", "F_T"), size=3.2) + 
  annotate('text', x=2.15, y=0.5, label=check_sig_diff("RCP85-CNRM-CM5", "Ring", "F_T"), size=3.2) + 
  annotate('text', x=2.35, y=0.5, label=check_sig_diff("RCP85-HadGEM2-ES365", "Ring", "F_T"), size=3.2) + 
  annotate('text', x=2.9, y=0.12, label=check_sig_diff("RCP85-CSIRO-Mk3-6-0", "Tracheid", "F_T"), size=3.2) + 
  annotate('text', x=3.15, y=0.12, label=check_sig_diff("RCP85-CNRM-CM5", "Tracheid", "F_T"), size=3.2) + 
  annotate('text', x=3.35, y=0.12, label=check_sig_diff("RCP85-HadGEM2-ES365", "Tracheid", "F_T"), size=3.2) 


type_f_npp_barplot <- ggplot(type_agg_frac, aes(fill=Climate_Scenario, y=F_NPP, x=Type)) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_errorbar(aes(x=Type, ymin=F_NPP-F_NPP_sd, ymax=F_NPP+F_NPP_sd, group=Climate_Scenario), width=0.2, position=position_dodge(0.9)) + 
  scale_fill_manual(values= pal, name="", labels=c("No Change", "Wet (CSIRO)","Dry (CNRM)", "Driest (HadGEM2)")) + 
  theme_classic() + 
  xlab("Xylem Anatomy") +
  ylab("Total NPP (%)") + 
  theme(legend.position=c(1.25, 0.45)) + 
  annotate('text', x=0.9, y=0.65, label=check_sig_diff("RCP85-CSIRO-Mk3-6-0", "Diffuse", "F_NPP"), size=3.2) + 
  annotate('text', x=1.15, y=0.65, label=check_sig_diff("RCP85-CNRM-CM5", "Diffuse", "F_NPP"), size=3.2) +
  annotate('text', x=1.35, y=0.65, label= check_sig_diff("RCP85-HadGEM2-ES365", "Diffuse", "F_NPP"), size=3.2) + 
  annotate('text', x=1.9, y=0.42, label=check_sig_diff("RCP85-CSIRO-Mk3-6-0", "Ring", "F_NPP"), size=3.2) + 
  annotate('text', x=2.15, y=0.42, label=check_sig_diff("RCP85-CNRM-CM5", "Ring", "F_NPP"), size=3.2) + 
  annotate('text', x=2.35, y=0.42, label=check_sig_diff("RCP85-HadGEM2-ES365", "Ring", "F_NPP"), size=3.2) + 
  annotate('text', x=2.9, y=0.12, label=check_sig_diff("RCP85-CSIRO-Mk3-6-0", "Tracheid", "F_NPP"), size=3.2) + 
  annotate('text', x=3.15, y=0.12, label=check_sig_diff("RCP85-CNRM-CM5", "Tracheid", "F_NPP"), size=3.2) + 
  annotate('text', x=3.35, y=0.12, label=check_sig_diff("RCP85-HadGEM2-ES365", "Tracheid", "F_NPP"), size=3.2) 

type_f_b_barplot <- ggplot(type_agg_frac, aes(fill=Climate_Scenario, y=F_B, x=Type)) + 
  geom_bar(position="dodge", stat="identity") + 
  geom_errorbar(aes(x=Type, ymin=F_B-F_B_sd, ymax=F_B+F_B_sd, group=Climate_Scenario), width=0.2, position=position_dodge(0.9)) + 
  scale_fill_manual(values= pal, name="", labels=c("No Change", "Wet (CSIRO)","Dry (CNRM)", "Driest (HadGEM2)")) + 
  theme_classic() + 
  xlab("") +
  ylab("Total Biomass (%)") + 
  theme(legend.position="None") + 
  annotate('text', x=0.9, y=0.38, label=check_sig_diff("RCP85-CSIRO-Mk3-6-0", "Diffuse", "F_B"), size=3.2) + 
  annotate('text', x=1.15, y=0.38, label=check_sig_diff("RCP85-CNRM-CM5", "Diffuse", "F_B"), size=3.2) +
  annotate('text', x=1.35, y=0.38, label= check_sig_diff("RCP85-HadGEM2-ES365", "Diffuse", "F_B"), size=3.2) + 
  annotate('text', x=1.9, y=0.65, label=check_sig_diff("RCP85-CSIRO-Mk3-6-0", "Ring", "F_B"), size=3.2) + 
  annotate('text', x=2.15, y=0.65, label=check_sig_diff("RCP85-CNRM-CM5", "Ring", "F_B"), size=3.2) + 
  annotate('text', x=2.35, y=0.65, label=check_sig_diff("RCP85-HadGEM2-ES365", "Ring", "F_B"), size=3.2) + 
  annotate('text', x=2.9, y=0.12, label=check_sig_diff("RCP85-CSIRO-Mk3-6-0", "Tracheid", "F_B"), size=3.2) + 
  annotate('text', x=3.15, y=0.12, label=check_sig_diff("RCP85-CNRM-CM5", "Tracheid", "F_B"), size=3.2) + 
  annotate('text', x=3.35, y=0.12, label=check_sig_diff("RCP85-HadGEM2-ES365", "Tracheid", "F_B"), size=3.2) 

type_f_barplots <- plot_grid(type_f_b_barplot, type_f_trans_barplot, type_f_npp_barplot, nrow=2, ncol=2, align='h', axis='b', labels="AUTO")

tiff(paste0(home, "/Figures/xa_pct_future_barplot.tif"), width=7.5, height=5, units='in', res=500)
type_f_barplots
dev.off()


noch_f <- rbind(type_agg_frac[Climate_Scenario=="No_Change", c(3,4,5)], type_agg_frac[Climate_Scenario=="No_Change", c(3,4,5)], type_agg_frac[Climate_Scenario=="No_Change", c(3,4,5)])

ch_f <- type_agg_frac[!Climate_Scenario=="No_Change", c(3,4,5)]

cbind(type_agg_frac[!Climate_Scenario=="No_Change", c(1,2)],(ch_f - noch_f))

mean(type_agg_frac[Type=="Diffuse",]$F_T)
mean(type_agg_frac[Type=="Diffuse",]$F_B)

mean(type_agg_frac[Type=="Ring",]$F_T)
mean(type_agg_frac[Type=="Ring",]$F_B)

mean(type_agg_frac[Type=="Tracheid",]$F_T)
mean(type_agg_frac[Type=="Tracheid",]$F_B)


##########################################################################################
##########################################################################################
##########################################################################################
## Supplementary Figure 3. NPP and Ecosystem respiration 

# calculate change in NPP and respiration into the future period 
annual_log <- combine_logs(base_folder_list, climate_scenario_list, "/NECN-succession-log.csv")
annual_log$ET <- (annual_log$Evaporation + annual_log$AnnualTrans)*10
annual_log$Year <- annual_log$Time + start_year
annual_log <- annual_log[Year >= 2001,]

# calculate the annual AG_NPP and BG_NPP for the full study area by taking the weighted average of the ecoregions
annual_log_npp <- annual_log[,.(NEE=sum(NEEC*(NumSites/838)), AG_NPPC=sum(AG_NPPC*(NumSites/838)), BG_NPPC=sum(BG_NPPC*(NumSites/838))), .(Year, Climate_Scenario, Run)]
annual_log_npp$NPP <- annual_log_npp$AG_NPPC + annual_log_npp$BG_NPPC
annual_log_npp$Re <- annual_log_npp$NPP + annual_log_npp$NEE

npp_summary <- annual_log_npp[Year >= 2041,.(NPP=mean(NPP), Re=mean(Re), NEE=mean(NEE)), .(Climate_Scenario)]

nc <- rbind(npp_summary[1,2:4], npp_summary[1,2:4], npp_summary[1,2:4])
pd <- cbind(npp_summary[-1, 1], ((npp_summary[-1, 2:4] - nc)/nc)*100)

pct_diff_annual_flux <- cbind(short_agg[-1, 1], ((short_agg[-1,c(2,4,6, 8)] - no_change)/no_change)*100)
pct_diff_annual_flux_long <- melt(pct_diff_annual_flux, id.vars="Climate_Scenario", 
                                  measure.vars=c("ET", "NEE", "LAI", "Q"), 
                                  variable.name="Flux")

npp_summary_cs <-annual_log_npp[Year >= 2041,.(NPP=mean(NPP), Re=mean(Re), NEE=mean(NEE)), .(Year, Climate_Scenario)]
ggplot(npp_summary_cs) + 
  geom_line(aes(x=Year, y=NPP, color=Climate_Scenario)) 

cs_labs <- c("No Change", "Wet (CSIRO)", "Dry (CNRM)","Driest (HadGEM2)")
names(cs_labs) <- climate_scenario_list

tiff(paste0(home, "Figures/Reco_NPP_ts_cs.tif"), width=5, height=5, units='in', res=500)
ggplot(npp_summary_cs) + 
  geom_line(aes(x=Year, y=Re, color="Re"), size=0.8) + 
  geom_line(aes(x=Year, y=NPP, color="NPP"),  size=0.8) + 
  scale_color_manual(name = "", values = c("Re" = "Black", "NPP" = "Dark Green"), label=c("Ecosystem Respiration", "NPP (above + below ground)")) + 
  facet_wrap(~Climate_Scenario, labeller=labeller(Climate_Scenario=cs_labs)) + 
  theme_bw() + 
  ylab(expression(Carbon~Flux~(g~C~m^{-2}~yr^{-1}))) + 
  xlab('Year') + 
  theme(legend.position='bottom')
dev.off()

##########################################################################################
##########################################################################################
##########################################################################################
## Supplementary Figure 4. Binned Transpiration and NPP along dryness gradient 

cal_dt_s <- cal_dt[,.(Biomass=mean(Biomass), Transpiration=mean(Transpiration), NPP=mean(NPP)), .(SppGroup, Type, Year, Climate_Scenario)]
cal_dt_st <- cal_dt_s[,.(Biomass=sum(Biomass), Transpiration=sum(Transpiration), NPP=sum(NPP)), .(Type, Year, Climate_Scenario)]
cal_dt_st <- merge(cal_dt_st, annual_climate, by=c("Year", "Climate_Scenario"), all.x=T)
cal_dt_total <-  cal_dt_s[,.(Total_Biomass=sum(Biomass), Total_Transpiration=sum(Transpiration), Total_NPP=sum(NPP)), .(Year, Climate_Scenario)]
cal_dt_st <- merge(cal_dt_st, cal_dt_total, by=c("Year", "Climate_Scenario"), all.x=T)
cal_dt_st$F_T <- cal_dt_st$Transpiration/cal_dt_st$Total_Transpiration
cal_dt_st$F_NPP <- cal_dt_st$NPP/cal_dt_st$Total_NPP
cal_dt_st$F_B <- cal_dt_st$Biomass/cal_dt_st$Total_Biomass

# round to the lowest 0.25 
cal_dt_st$PPTPET_r <- floor(cal_dt_st$PPTPET * 2)/4
cal_dt_st$F_T <- cal_dt_st$F_T * 100
cal_dt_st$F_NPP <- cal_dt_st$F_NPP * 100
cal_dt_st$F_B <- cal_dt_st$F_B * 100

# calculate the mean and sd for each rounded pptpet 
f_summary_type <- cal_dt_st[,.(F_T=mean(F_T), F_T_sd=sd(F_T), F_NPP=mean(F_NPP), F_NPP_sd=sd(F_NPP)), .(PPTPET_r, Type)]
twf_gradient <- ggplot(f_summary_type[PPTPET_r >= 0.5, ]) + 
  geom_line(aes(x=PPTPET_r, y=F_T, color=Type)) + 
  geom_point(aes(x=PPTPET_r, y=F_T, color=Type)) + 
  geom_ribbon(aes(x=PPTPET_r, ymin=F_T-F_T_sd, ymax=F_T+F_T_sd, fill=Type), alpha=0.3) + 
  scale_color_manual(values=xa_pal, name="") + 
  scale_fill_manual(values=xa_pal, name="") + 
  theme_classic() + 
  xlab(expression(paste(italic(P),":PET"))) + 
  ylab("Transpiration (%)") + 
  theme(legend.position = 'none')

tcf_gradient <- ggplot(f_summary_type[PPTPET_r >= 0.5, ]) + 
  geom_line(aes(x=PPTPET_r, y=F_NPP, color=Type)) + 
  geom_point(aes(x=PPTPET_r, y=F_NPP, color=Type)) + 
  geom_ribbon(aes(x=PPTPET_r, ymin=F_NPP-F_NPP_sd, ymax=F_NPP+F_NPP_sd, fill=Type), alpha=0.3) + 
  scale_color_manual(values=xa_pal, name="") + 
  scale_fill_manual(values=xa_pal, name="") + 
  theme_classic() + 
  xlab(expression(paste(italic(P),":PET"))) + 
  ylab("NPP (%)") + 
  theme(legend.position='none')

legend <- get_legend(ggplot(f_summary_type[PPTPET_r >= 0.5, ]) + 
                       geom_line(aes(x=PPTPET_r, y=F_NPP, color=Type)) + 
                       geom_point(aes(x=PPTPET_r, y=F_NPP, color=Type)) + 
                       geom_ribbon(aes(x=PPTPET_r, ymin=F_NPP-F_NPP_sd, ymax=F_NPP+F_NPP_sd, fill=Type), alpha=0.3) + 
                       scale_color_manual(values=xa_pal, name="") + 
                       scale_fill_manual(values=xa_pal, name="") + 
                       theme_classic() + 
                       xlab(expression(paste(italic(P),":PET"))) + 
                       ylab("NPP (%)") + 
                       theme(legend.position='bottom'))

pp <- plot_grid(twf_gradient, tcf_gradient, ncol=1, nrow=2, labels="AUTO")
p <- plot_grid(pp, legend, rel_heights=c(1, 0.1), ncol=1, nrow=2)

tiff(paste0(home, "Figures/pct_TWF_dryness_gradient.tif"), width=4, height=4.5, units='in', res=500)
p
dev.off()


