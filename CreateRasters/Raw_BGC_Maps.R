library(climr)
library(data.table)
library(terra)
library(ranger)
library(pool)

con <- dbPool(
  drv = Postgres(),
  dbname = "climr",
  host = "146.190.244.244",
  port = 5432,
  user = "climr_client",
  password = "PowerOfBEC2023"
)

conn <- DBI::dbConnect(
  drv = RPostgres::Postgres(),
  dbname = "cciss",
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432, 
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)

# dat <- vect("~/FFEC/BGC_v13_Fixed.gpkg")
# dat <- project(dat, "epsg:4326")
# writeVector(dat, "BGC_v13.shp")

# dat <- vect("/home/kdaust/FFEC/WNA_BGC_v13_15Nov2024.gpkg")
# d2 <- simplifyGeom(dat, tolerance = 25)
# writeVector(d2, "WNA_v13_Simplified.gpkg")
# d3 <- project(d2, "epsg:4326")
# d3 <- st_as_sf(d3)
# d3 <- d3["BGC"]
# st_write(d3, "WNA_v13.shp")
# "ogr2ogr -f GeoJSON BGC_v13.geojson BGC_v13.shp"
# "tippecanoe -o BGC_v13.mbtiles -z16 --simplification=10 --force --coalesce-densest-as-needed --extend-zooms-if-still-dropping --detect-shared-borders BGC_v13.geojson"

addVars <- function(dat) {
  dat[, PPT_MJ := PPT_05 + PPT_06]
  dat[, PPT_JAS := PPT_07 + PPT_08 + PPT_09]
  dat[, PPT.dormant := PPT_at + PPT_wt]
  dat[, CMD.def := pmax(0, 500 - PPT.dormant)]
  dat[, CMDMax := CMD_07]   ## TODO: THIS IS NOT NECESSARILY CMD MAX
  dat[, CMD.total := CMD.def + CMD]
}


# library(sf)
# outline <- st_read("../Common_Files/gpr_000b11a_e/gpr_000b11a_e.shp")
# bc_ol <- outline[1,]
# plot(bc_ol)
# bc_ol <- bc_ol[,"PRUID"]
# bc_ol2 <- vect(bc_ol)
# dem_hr <- rast("../Common_Files/WNA_DEM_SRT_30m_cropped.tif")
# temp <- crop(dem_hr,bc_ol2)
# temp_dem <- aggregate(temp, fact = 5)
# bc_rast <- rasterize(bc_ol, temp_dem)
# final_dem <- mask(temp_dem, bc_rast)
# writeRaster(final_dem, "BC_DEM_100m.tif")
# 
# bc_ol <- disagg(bc_ol, fact = 10)
# t2 <- resample(temp, bc_ol)
# 
#setwd("~/FFEC/spatcciss/")
library(sf)
library(rmapshaper)
library(geojsonsf)
library(terra)
library(dplyr)

bgcs <- vect("../Common_Files/BEC13Draft_BEC13v1wLabelUpdates.shp")
bgc_simple <- simplifyGeom(bgcs, tolerance = 10)
bgc_simple <- project(bgc_simple, "epsg:4326")

writeVector(bgc_simple, "../Common_Files/BEC13Draft_Simplified.gpkg")

bgcs <- st_read("../Common_Files/BEC13Draft_Simplified.gpkg")
bgcs <- st_transform(bgcs, 4326)
bgcs <- bgcs["MAP_LABEL"]

bgcs2 <- bgcs |>
  group_by(MAP_LABEL) |>
  summarize(geom = st_union(geom), .groups = "drop")
st_write(bgcs, "../Common_Files/BEC13Draft_4326.gpkg")
bgcs <- vect("../Common_Files/BEC13Draft_4326.gpkg")

bgcs <- st_read("../Common_Files/BEC13Draft_Simplified.gpkg")
bgcs <- bgcs[c("OBJECTID", "BGC")]
#bgcs <- bgcs[,"BGC"]
names(bgcs) <- c("id","bgc","geom")
st_write(bgcs, conn, "bgcv13_mar6")

bgcs_agg <- aggregate(bgcs, by = "BGC", dissolve = T, count = F)
writeVector(bgcs_agg, filetype = "GeoJSON", filename = "../Common_Files/BGCv13_March6.geojson")
bgcs_sf <- st_as_sf(bgcs_agg)


bgcjson <- sf_geojson(bgcs)
bgcs_simple <- ms_simplify(bgcjson, keep = 0.1)

bgcs <- st_transform(bgcs, 4326)
bgcs$bgc_id <- seq_along(bgcs$BGC)
bgc_ids <- data.table(bgc = bgcs$BGC, bgc_id = bgcs$bgc_id)

bgcs <- vect(bgcs)
bc_bgc <- rasterize(bgcs, final_dem, field = "bgc_id")
writeRaster(bc_bgc,"bcg_rast.tif")
fwrite(bgc_ids, "bgc_ids.csv")

bc_bgc <- rast("BC_BGC_rast.tif.tif")

#final_dem <- aggregate(final_dem, fact = 2)
#################climr####################
points_dat <- as.data.frame(final_dem, cells=T, xy=T)
colnames(points_dat) <- c("id", "lon", "lat", "elev")
points_dat <- points_dat[,c(2,3,4,1)] #restructure for climr input

vars_needed <- c("CMD_sm", "DDsub0_sp", "DD5_sp", "Eref_sm", "Eref_sp", "EXT", 
                 "MWMT", "NFFD_sm", "NFFD_sp", "PAS", "PAS_sp", "SHM", "Tave_sm", 
                 "Tave_sp", "Tmax_sm", "Tmax_sp", "Tmin", "Tmin_at", "Tmin_sm", 
                 "Tmin_sp", "Tmin_wt","CMI", "PPT_05","PPT_06","PPT_07","PPT_08","PPT_09","PPT_at","PPT_wt","CMD_07","CMD"
)
gcms_use <- c("ACCESS-ESM1-5","EC-Earth3","GISS-E2-1-G","MIROC6","MPI-ESM1-2-HR","MRI-ESM2-0")
runs_use <- c("r1i1p1f1","r4i1p1f1","r2i1p3f1","r2i1p1f1","r1i1p1f1","r1i1p1f1")
ssp_use <- "ssp245"
periods_use <- list_gcm_periods()

splits <- c(seq(1,nrow(points_dat), by = 1000000),nrow(points_dat)+1)
load("BGC_RFresp.Rdata")
cols <- fread("./WNAv12_3_SubzoneCols.csv")

for (gcm_i in 1:length(gcms_use)){
  gcm_curr <- gcms_use[gcm_i]
  cat(gcms_use[gcm_i], "\n")
  cat(runs_use[gcm_i])
  pred_ls <- list()
  for (i in 1:(length(splits) - 1)){
    cat(i, "\n")
    clim_dat <- downscale(points_dat[splits[i]:(splits[i+1]-1),], 
                          which_refmap = "refmap_climr",
                          gcms = gcms_use[gcm_i],
                          gcm_periods = periods_use,
                          ssps = ssp_use,
                          vars = vars_needed,
                          run_nm = runs_use[gcm_i],
                          nthread = 4,
                          return_refperiod = FALSE)
    addVars(clim_dat)
    clim_dat <- na.omit(clim_dat)
    temp <- predict(BGC_RFresp, data = clim_dat, num.threads = 12)
    dat <- data.table(cellnum = clim_dat$id, gcm = clim_dat$GCM, period = clim_dat$PERIOD, bgc_pred = temp$predictions)
    if(i == 1){
      fwrite(dat, paste0("./bgc_preds_raw/BGC_Pred_",gcm_curr,".csv"))
    }else{
      fwrite(dat, paste0("./bgc_preds_raw/BGC_Pred_",gcm_curr,".csv"), append = T)
    }
    
    #pred_ls[[i]] <- dat
    rm(clim_dat,dat)
    gc()
  }
}

# ### BGC subzone and zone ensemble vote maps
fnames <- list.files("bgc_preds_raw", full.names = T)
dat <- fread("bgc_preds_raw/BGC_Pred_ACCESS-ESM1-5.csv")
flist <- lapply(fnames, FUN = function(x){fread(x)})

bgc_all <- rbindlist(flist)
bgc_all[,zone_pred := gsub("[[:lower:]]|[[:digit:]]|_.*","",bgc_pred)]

sz_votes <- bgc_all[,.(sz_vote = .N), by = .(cellnum, period, bgc_pred)]
sz_ensemble <- sz_votes[sz_votes[,.I[which.max((sz_vote))], by = .(cellnum, period)]$V1]
fwrite(sz_ensemble, "BGC_Preds_SZ_Ensemble.csv")

zone_votes <- bgc_all[,.(sz_vote = .N), by = .(cellnum, period, zone_pred)]
zone_ensemble <- zone_votes[zone_votes[,.I[which.max((sz_vote))], by = .(cellnum, period)]$V1]
setnames(zone_ensemble, old = "zone_pred", new = "bgc_pred")
fwrite(zone_ensemble, "BGC_Preds_Zone_Ensemble.csv")


gcms_use <- c("SZ_Ensemble", "ACCESS-ESM1-5","EC-Earth3","GISS-E2-1-G","MIROC6","MPI-ESM1-2-HR","MRI-ESM2-0") #"SZ_Ensemble", "Zone_Ensemble", 

for(gcm_curr in gcms_use){
  all_pred <- fread(paste0("bgc_preds_raw/BGC_Pred_",gcm_curr,".csv"))
  all_pred[,bgc_id := as.numeric(as.factor(bgc_pred))]
  for(curr_per in list_gcm_periods()){
    cat(".")
    dat <- all_pred[period == curr_per,]
    dat[,bgc_id := as.numeric(as.factor(bgc_pred))]

    values(final_dem) <- NA
    final_dem[dat$cellnum] <- dat$bgc_id
    #writeRaster(final_dem, "BGC_Pred_200m.tif")
    bgc_id <- unique(dat[,.(bgc_pred,bgc_id)])
    # fwrite(bgc_id, "BGC_ID.csv")

    bgc_id[cols, colour := i.colour, on = c(bgc_pred = "classification")]

    coltab(final_dem) <- bgc_id[,.(bgc_id,colour)]
    #plot(final_dem)
    rgbbgc <- colorize(final_dem, to = "rgb", alpha = T)
    writeRaster(rgbbgc, paste0("./bgc_rasters/bgc_",gcm_curr,"_",curr_per,".tif"), overwrite=TRUE)  
  }


}
  