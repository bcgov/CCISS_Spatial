.libPaths(c("/sapho/kdaust/R-packages",.libPaths()))
library(climr)
library(data.table)
library(terra)
library(ranger)
library(Rcpp)
library(sf)
library(RColorBrewer)

setwd("/sapho/kdaust/FFEC/spatcciss/")
source("CreateRasters/cciss_spatial_functions.R")
final_dem <- rast("BC_DEM_200m.tif")
BGCmodel <- readRDS("BGCmodel_WNA_V2.1.rds")

## RAW BGC PREDICTION
vars_needed <- c("CMD_sm", "DDsub0_sp", "DD5_sp", "Eref_sm", "Eref_sp", "EXT", 
                 "MWMT", "NFFD_sm", "NFFD_sp", "PAS", "PAS_sp", "SHM", "Tave_sm", 
                 "Tave_sp", "Tmax_sm", "Tmax_sp", "Tmin", "Tmin_at", "Tmin_sm", 
                 "Tmin_sp", "Tmin_wt","CMI", "PPT_05","PPT_06","PPT_07","PPT_08","PPT_09","PPT_at","PPT_wt","CMD_07","CMD"
)
gcms_use <- c("ACCESS-ESM1-5","EC-Earth3","GISS-E2-1-G","MIROC6","MPI-ESM1-2-HR","MRI-ESM2-0")
runs_use <- c("r1i1p1f1","r4i1p1f1","r2i1p3f1","r2i1p1f1","r1i1p1f1","r1i1p1f1")
ssp_use <- "ssp245"
periods_use <- list_gcm_periods()

# raw_preds_future(final_dem, BGCmodel, vars_needed, gcms_use, runs_use, ssp_use, periods_use)
# raw_preds_obs(final_dem, BGCmodel, vars_needed)
# raw_preds_hist(final_dem, BGCmodel, vars_needed)
# calculate_ensembles(gcms_use)

cols <- fread("./WNAv13_v6_SubzoneCols.csv")
zone_cols <- fread("./WNAv13_ZoneCols.csv")
#print_bgc_maps(raster_template = final_dem, subzone_cols = cols, zone_cols = zone_cols)

## CCISS summaries
gcms_cciss <- c("ACCESS-ESM1-5", "CNRM-ESM2-1", "EC-Earth3", "GFDL-ESM4",
              "GISS-E2-1-G", "MIROC6", "MPI-ESM1-2-HR", "MRI-ESM2-0")

#summary_preds_gcm(final_dem, BGCmodel, gcms_cciss, list_gcm_periods(), vars_needed = vars_needed)

# bgcs <- st_read("BEC13Draft_Simplified.gpkg")
# make_bgc_raster(raster_template = final_dem, bgcs = bgcs)

bgc_rast <- rast("BC_BGC_rast.tif")
rast_ids <- fread("BC_BGC_rast_ids.csv")
edatable <- fread("Edatopic_v13_9.csv")
special <- fread("SpecialSites_v13_2.csv")
# siteseries_preds(edatopes = c("B2", "C4", "D6"),
#                 obs = F,
#                 bgc_rast = bgc_rast,
#                 rast_id = rast_ids,
#                 edatopic = edatable,
#                 special_ss = special)
siteseries_preds(edatopes = c("B2", "C4", "D6"),
                obs = TRUE,
                bgc_rast = bgc_rast,
                rast_id = rast_ids,
                edatopic = edatable,
                special_ss = special)
species <- c("Pl","Sx","Fd","Cw","Hw","Bl","At", "Ac", "Ep", "Yc", "Pw", "Ss", "Bg", "Lw", "Sb")

suit <- fread("suitability_v13_14.csv")
suit[,V1 := NULL]
setnames(suit, c("BGC","SS_NoSpace","Sppsplit","FeasOrig","Spp","Feasible","Mod","OR"))
# cciss_suitability(species = species,
#                 edatopes = c("B2","C4","D6"),
#                 feas_table = suit,
#                 obs = FALSE,
#                 periods = list_gcm_periods()[-5],
#                 tile_size = 10000)

cciss_suitability(species = species,
                edatopes = c("B2","C4","D6"),
                feas_table = suit,
                obs = TRUE,
                periods = NULL,
                tile_size = 100000)

# plot_suitability_maps(raster_template = final_dem,
#                      species = species,
#                      edatopes = c("B2","C4","D6"),
#                      periods = list_gcm_periods()[-5],
#                      bgc_rast = bgc_rast)

plot_suitability_maps(raster_template = final_dem,
                     species = species,
                     edatopes = c("B2","C4","D6"),
                     periods = list_gcm_periods()[-5],
                     bgc_rast = bgc_rast,
                     obs = TRUE)