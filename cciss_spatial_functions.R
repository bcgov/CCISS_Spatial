### functions for cciss spatial creation
library(climr)
library(data.table)
library(terra)
library(ranger)
library(stringr)

addVars <- function(dat) {
  dat[, PPT_MJ := PPT_05 + PPT_06]
  dat[, PPT_JAS := PPT_07 + PPT_08 + PPT_09]
  dat[, PPT.dormant := PPT_at + PPT_wt]
  dat[, CMD.def := pmax(0, 500 - PPT.dormant)]
  dat[, CMDMax := CMD_07]   ## TODO: THIS IS NOT NECESSARILY CMD MAX
  dat[, CMD.total := CMD.def + CMD]
}

raw_preds_future <- function(raster_template, 
                          BGCmodel, vars_needed, 
                          gcms_use, runs_use, spp_use, periods_use, 
                          chunk_size = 1000000, folder_name = "bgc_preds_raw"){
  points_dat <- as.data.frame(raster_template, cells=T, xy=T)
  colnames(points_dat) <- c("id", "lon", "lat", "elev")
  points_dat <- points_dat[,c(2,3,4,1)] #restructure for climr input
  
  splits <- c(seq(1,nrow(points_dat), by = chunk_size),nrow(points_dat)+1)
  
  for (gcm_i in 1:length(gcms_use)){
    gcm_curr <- gcms_use[gcm_i]
    message(gcms_use[gcm_i])
    message(runs_use[gcm_i])
    for (i in 1:(length(splits) - 1)){
      #cat(i, "\n")
      clim_dat <- downscale(points_dat[splits[i]:(splits[i+1]-1),], 
                            which_refmap = "refmap_climr",
                            gcms = gcms_use[gcm_i],
                            gcm_periods = periods_use,
                            ssps = ssp_use,
                            vars = vars_needed,
                            run_nm = runs_use[gcm_i],
                            nthread = 8,
                            return_refperiod = FALSE)
      addVars(clim_dat)
      clim_dat <- na.omit(clim_dat)
      temp <- predict(BGCmodel, data = clim_dat, num.threads = 12)
      dat <- data.table(cellnum = clim_dat$id, gcm = clim_dat$GCM, period = clim_dat$PERIOD, 
                        bgc_pred = temp$predictions)
      fwrite(dat, paste0(folder_name, "/BGC_Pred_",gcm_curr,".csv"), append = T)
      rm(clim_dat,dat)
      gc()
    }
  }
}

raw_preds_obs <- function(raster_template, 
                             BGCmodel, vars_needed, 
                             chunk_size = 1000000, folder_name = "bgc_preds_raw"){
  points_dat <- as.data.frame(raster_template, cells=T, xy=T)
  colnames(points_dat) <- c("id", "lon", "lat", "elev")
  points_dat <- points_dat[,c(2,3,4,1)] #restructure for climr input
  
  splits <- c(seq(1,nrow(points_dat), by = chunk_size),nrow(points_dat)+1)
  
  for (i in 1:(length(splits) - 1)){
    clim_dat <- downscale(points_dat[splits[i]:(splits[i+1]-1),], 
                          which_refmap = "refmap_climr",
                          obs_periods = "2001_2020",
                          vars = vars_needed,
                          nthread = 4,
                          return_refperiod = FALSE)
    addVars(clim_dat)
    clim_dat <- na.omit(clim_dat)
    temp <- predict(BGCmodel, data = clim_dat, num.threads = 12)
    dat <- data.table(cellnum = clim_dat$id, gcm = clim_dat$GCM, period = clim_dat$PERIOD, 
                      bgc_pred = temp$predictions)
    fwrite(dat, paste0(folder_name, "/BGC_Pred_Obs.csv"), append = T)
    rm(clim_dat,dat)
    gc()
  }
}

raw_preds_hist <- function(raster_template, 
                          BGCmodel, vars_needed, 
                          chunk_size = 1000000, folder_name = "bgc_preds_raw"){
  points_dat <- as.data.frame(raster_template, cells=T, xy=T)
  colnames(points_dat) <- c("id", "lon", "lat", "elev")
  points_dat <- points_dat[,c(2,3,4,1)] #restructure for climr input
  
  splits <- c(seq(1,nrow(points_dat), by = chunk_size),nrow(points_dat)+1)
  
  for (i in 1:(length(splits) - 1)){
    clim_dat <- downscale(points_dat[splits[i]:(splits[i+1]-1),], 
                          which_refmap = "refmap_climr",
                          vars = vars_needed,
                          nthread = 4,
                          return_refperiod = TRUE)
    addVars(clim_dat)
    clim_dat <- na.omit(clim_dat)
    temp <- predict(BGCmodel, data = clim_dat, num.threads = 12)
    dat <- data.table(cellnum = clim_dat$id, gcm = clim_dat$GCM, period = clim_dat$PERIOD, 
                      bgc_pred = temp$predictions)
    fwrite(dat, paste0(folder_name, "/BGC_Pred_Historic.csv"), append = T)
    rm(clim_dat,dat)
    gc()
  }
}

calculate_ensembles <- function(gcm_names, folder_name = "bgc_preds_raw"){
  fnames <- paste0(folder_name, "/BGC_Pred_",gcm_names,".csv")
  flist <- lapply(fnames, FUN = function(x){fread(x)})
  
  bgc_all <- rbindlist(flist)
  bgc_all[,zone_pred := str_extract(bgc_pred, "^[A-Z]+")]
  
  sz_votes <- bgc_all[,.(sz_vote = .N), by = .(cellnum, period, bgc_pred)]
  sz_ensemble <- sz_votes[sz_votes[,.I[which.max((sz_vote))], by = .(cellnum, period)]$V1]
  fwrite(sz_ensemble, paste0(folder_name, "/SZ_Ensemble.csv"))
  
  zone_votes <- bgc_all[,.(sz_vote = .N), by = .(cellnum, period, zone_pred)]
  zone_ensemble <- zone_votes[zone_votes[,.I[which.max((sz_vote))], by = .(cellnum, period)]$V1]
  setnames(zone_ensemble, old = "zone_pred", new = "bgc_pred")
  fwrite(zone_ensemble, paste0(folder_name, "/Zone_Ensemble.csv"))
}

cols <- fread("./WNAv13_SubzoneCols.csv")
zone_cols <- fread("./WNAv13_ZoneCols.csv")

##load processed predictions and create RGB rasters
print_bgc_maps <- function(raster_template, subzone_cols, zone_cols, 
                           input_name = "bgc_preds_raw", 
                           output_name = "bgc_rasters") {
  fnames <- list.files(input_name, full.names = TRUE)
  for(name in fnames){
    all_pred <- fread(name)
    if(grepl("Zone_Ensemble", name)){
      col_use <- zone_cols
    } else {
      col_use <- subzone_cols
    }
    
    periods <- unique(all_pred$period)
    for(curr_per in periods){
      cat(".")
      dat <- all_pred[period == curr_per,]
      dat[,bgc_id := as.numeric(as.factor(bgc_pred))]
      
      final_dem <- copy(raster_template)
      values(final_dem) <- NA
      final_dem[dat$cellnum] <- dat$bgc_id
      bgc_id <- unique(dat[,.(bgc_pred,bgc_id)])
      bgc_id[col_use, colour := i.colour, on = c(bgc_pred = "classification")]
      
      coltab(final_dem) <- bgc_id[,.(bgc_id,colour)]
      rgbbgc <- colorize(final_dem, to = "rgb", alpha = T)
      writeRaster(rgbbgc, paste0(output_name,"/bgc_",gcm_curr,"_",curr_per,".tif"), overwrite=TRUE)  
    }
  }
}