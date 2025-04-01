### functions for cciss spatial creation
library(climr)
library(data.table)
library(terra)
library(ranger)
library(stringr)
library(EnvStats)
library(plotly)
library(scales)

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
                             gcms_use, runs_use, ssp_use, periods_use, 
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



##load processed predictions and create RGB rasters
print_bgc_maps <- function(raster_template, subzone_cols, zone_cols, 
                           input_name = "bgc_preds_raw", 
                           output_name = "bgc_rasters") {
  fnames <- list.files(input_name, full.names = TRUE)
  f_short <- list.files(input_name, full.names = FALSE)
  gcm_nms <- gsub("BGC_Pred_|.csv","",f_short)
  for(i in 1:length(fnames)){
    name <- fnames[i]
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
      writeRaster(rgbbgc, paste0(output_name,"/bgc_",gcm_nms[i],"_",curr_per,".tif"), overwrite=TRUE)  
    }
  }
}

####### CCISS BGC Summaries ########################################################
summary_preds_gcm <- function(raster_template, 
                              BGCmodel, 
                              gcms_use, 
                              periods_use, 
                              ssp_use = c("ssp126", "ssp245", "ssp370"),
                              ssp_w = c(0.8,1,0.8),
                              vars_needed, 
                              out_folder = "bgc_data",
                              start_tile = 1) {
  points_dat <- as.data.frame(raster_template, cells=T, xy=T)
  colnames(points_dat) <- c("id", "lon", "lat", "elev")
  points_dat <- points_dat[,c(2,3,4,1)] #restructure for climr input
  splits <- c(seq(1, nrow(points_dat), by = 100000), nrow(points_dat) + 1)
  ssp_weights <- data.table(ssp = ssp_use, weight = ssp_w)
  message("There are ", length(splits), " tiles")
  
  for (i in start_tile:(length(splits) - 1)){
    cat(i, "\n")
    clim_dat <- downscale(points_dat[splits[i]:(splits[i+1]-1),], 
                          which_refmap = "refmap_climr",
                          gcms = gcms_use,
                          gcm_periods = periods_use,
                          ssps = ssp_use,
                          max_run = 0L,
                          vars = vars_needed,
                          nthread = 6,
                          return_refperiod = FALSE)
    addVars(clim_dat)
    clim_dat <- na.omit(clim_dat)
    temp <- predict(BGCmodel, data = clim_dat, num.threads = 8)
    dat <- data.table(cellnum = clim_dat$id, ssp = clim_dat$SSP, gcm = clim_dat$GCM, 
                      period = clim_dat$PERIOD, bgc_pred = temp$predictions)
    dat[ssp_weights, weight := i.weight, on = "ssp"]
    dat_sum <- dat[,.(bgc_prop = sum(weight)/20.8), by = .(cellnum, period, bgc_pred)]
    
    fwrite(dat_sum, paste0(out_folder, "/bgc_summary_",i, ".csv"))
    rm(clim_dat, dat, dat_sum)
    gc()
  }
  cat("Done!")
}

summary_preds_obs <- function(raster_template, 
                              BGCmodel, 
                              vars_needed, 
                              out_folder = "bgc_data") {
  points_dat <- as.data.frame(raster_template, cells=T, xy=T)
  colnames(points_dat) <- c("id", "lon", "lat", "elev")
  points_dat <- points_dat[,c(2,3,4,1)] #restructure for climr input
  splits <- c(seq(1, nrow(points_dat), by = 500000), nrow(points_dat) + 1)
  message("There are ", length(splits), " tiles")
  
  period_curr <- list_obs_periods()
  for (i in 1:(length(splits) - 1)){
    cat(i, "\n")
    clim_dat <- downscale(points_dat[splits[i]:(splits[i+1]-1),], 
                          which_refmap = "refmap_climr",
                          obs_periods = period_curr,
                          vars = vars_needed,
                          nthread = 6,
                          return_refperiod = FALSE)
    addVars(clim_dat)
    clim_dat <- na.omit(clim_dat)
    temp <- predict(BGCmodel, data = clim_dat, num.threads = 16)
    dat <- data.table(cellnum = clim_dat$id,  period = clim_dat$PERIOD, bgc_pred = temp$predictions, bgc_prop = 1)
    fwrite(dat, append = TRUE, paste0(out_folder,"/bgc_summary_obs", ".csv"))
    rm(clim_dat, dat)
    gc()
  }
  message("Done!")
}

make_bgc_raster <- function(raster_template, bgcs){
  bgcs <- st_transform(bgcs, crs(raster_template))
  bgcs$bgc_id <- as.numeric(as.factor(bgcs$BGC))
  bgc_ids <- unique(data.table(bgc = bgcs$BGC, bgc_id = bgcs$bgc_id))
  
  bgcs <- vect(bgcs)
  bc_bgc <- rasterize(bgcs, raster_template, field = "bgc_id")
  writeRaster(bc_bgc,"BC_BGC_rast.tif", overwrite = TRUE)
  fwrite(bgc_ids, "BC_BGC_rast_ids.csv")
}

siteseries_preds <- function(edatopes = c("B2", "C4", "D6"), 
                             obs = F, 
                             bgc_rast, 
                             rast_id, 
                             edatopic,
                             special_ss,
                             in_folder = "bgc_data",
                             out_folder = "ss_preds") {
  if(obs){
    periods <- list_obs_periods()
    bgc_all <- fread(file.path(in_folder, "bgc_summary_obs.csv"))
    obs_nm <- "obs_"
  } else {
    periods <- c(list_gcm_periods())
    temp_ls <- list.files(in_folder, full.names = TRUE)
    temp_ls <- temp_ls[-grep("obs",temp_ls)]
    bgc_all_ls <- lapply(temp_ls, FUN = fread)
    bgc_all <- rbindlist(bgc_all_ls)
    rm(bgc_all_ls)
    obs_nm <- ""
  }
  
  bgc_points <- as.data.frame(bgc_rast, cells=T, xy=T) |> as.data.table()
  bgc_points[rast_id, BGC := i.bgc, on = "bgc_id"]
  
  eda_ss <- special_ss[,.(SS_NoSpace,SpecialCode)]
  edatopic[eda_ss, SpecialCode := i.SpecialCode, on = "SS_NoSpace"]
  eda_all <- edatopic[is.na(SpecialCode),]
  
  for(period_curr in periods){
    bgc_sum <- bgc_all[period == period_curr,]
    bgc_sum[bgc_points, BGC := i.BGC, on = c(cellnum = "cell")]
    setcolorder(bgc_sum, c("cellnum","period","BGC","bgc_pred","bgc_prop"))
    setnames(bgc_sum, c("SiteRef","FuturePeriod","BGC","BGC.pred","BGC.prop"))
    
    for(edatope in edatopes){
      cat(period_curr, edatope, "\n")
      eda_table <- copy(eda_all)
      eda_table[,HasPos := if(any(Edatopic %in% edatope)) T else F, by = .(SS_NoSpace)]
      eda_table <- unique(eda_table[(HasPos),])
      ###########################################################
      
      sites <- unique(bgc_sum$SiteRef)
      splits <- c(seq(1, length(sites), by = 200000), length(sites) + 1)
      for (i in 1:(length(splits) - 1)){
        cat(i, "\n")
        srs <- sites[splits[i]:(splits[i+1]-1)]
        dat_sml <- bgc_sum[SiteRef %in% srs,]
        sspred <- edatopicOverlap_fast(dat_sml, E1 = eda_table)
        fwrite(sspred, append = TRUE, paste0(out_folder, "/siteseries_",obs_nm,period_curr, "_", edatope, ".csv"))
        rm(sspred)
        gc()
      }
    }
  }
  message("Done!")
}

cciss_suitability <- function(species,
                              edatopes,
                              feas_table,
                              obs = FALSE,
                              periods = list_gcm_periods(),
                              in_folder = "ss_preds",
                              out_folder = "cciss_feas",
                              tile_size = 4000) {
  stopifnot(all(c("BGC","SS_NoSpace","Sppsplit","FeasOrig","Spp","Feasible","Mod","OR") %in% names(feas_table)))
  
  if(obs) {
    periods <- list_obs_periods()
    obs_nm <- "obs_"
  } else {
    obs_nm <- ""
  }
  
  for(period in periods){
    for(edatope in edatopes){
      sspreds <- fread(paste0(in_folder,"/siteseries_",obs_nm,period,"_",edatope,".csv"))
      sitenums <- unique(sspreds$SiteRef)
      splits <- c(seq(1, length(sitenums), by = tile_size), length(sitenums) + 1)
      for(spp in species){
        message(period, edatope, spp, "\n")
        for (i in 1:(length(splits) - 1)){
          temp <- sspreds[SiteRef %in% sitenums[splits[i]:(splits[i+1]-1)],]
          cciss_res <- cciss_full(temp, feas_table, spp)
          fwrite(cciss_res, append = TRUE, paste0(out_folder,"/CCISS_",obs_nm,period,"_",edatope,".csv"))
        }
        rm(cciss_res)
        gc()
      } 
    }
    rm(sspreds)
    gc()
  }
  cat("Done!")
}

plot_suitability_maps <- function(raster_template,
                                  species,
                                  edatopes,
                                  periods,
                                  bgc_rast,
                                  obs = FALSE,
                                  in_folder = "cciss_feas",
                                  out_folder = "final_rgb",
                                  out_folder_raw = "raw_rasters") {
  final_dem <- raster_template
  breakpoints.suit <- c(1,2,3,999)
  palette.suit <-   c("#006400", "#1E90FF", "#EEC900", "#FFFFFF")
  breakpoints.change <- c(c(seq(-2.5,2.5,0.5),-10,10,20,30) + 15, 999)
  palette.change <- c(brewer.pal(11,"RdBu")[c(1,2,3,4,5,6)], brewer.pal(11,"RdBu")[c(7,8,9,10,11)],"#000000", brewer.pal(9,"YlOrRd")[1:3],"#FFFFFF") # nolint
  breakpoints.binary <- seq(-1,1,0.2)
  palette.binary <- c(brewer.pal(11,"RdBu")[c(1:4,6,6)], brewer.pal(11,"RdBu")[c(6,8:11)])
  
  ##feas colours
  suit_cols <- data.table(value = breakpoints.suit,Colour = palette.suit)
  
  ##mean change colours
  change_cols <- data.table(value = breakpoints.change, Colour = palette.change)
  
  ##addret colours
  #addret_cols <- data.table(value = breakpoints.binary*100, Colour = palette.binary)
  
  if(obs) {
    periods <- list_obs_periods()
    obs_nm <- "obs_"
  } else {
    obs_nm <- ""
  }
  
  for(period in periods){
    for(edatope in edatopes){
      dat <- fread(paste0(in_folder,"/CCISS_",obs_nm,period,"_",edatope,".csv"))
      for(spp in species){
        cat(period, edatope, spp, "\n")
        dat_spp <- dat[Spp == spp,]
        dat_spp <- dat_spp[Curr < 3.5 | Newsuit < 3.5,]
        dat_spp[,FeasChange := Curr - Newsuit]
        dat_spp[Newsuit > 3.5 & Curr <= 3, FeasChange := -10]
        dat_spp[Curr > 3.5, FeasChange := round(FeasChange) * 10]
        dat_spp[,FeasChange := round(FeasChange/0.5)*0.5]
        dat_spp[,FeasRound := round(Newsuit)]
        dat_spp[,CurrRound := round(Curr)]
        dat_spp[CurrRound > 3, CurrRound := 999]
        dat_spp[FeasRound > 3, FeasRound := 999]
        dat_spp[,AddRet := Improve]
        dat_spp[Decline > Improve, AddRet := -Decline]
        dat_spp[,AddRet := round(AddRet/20)*20]
        
        #historic feasibility
        if(period == "2001_2020" & !obs){
          values(final_dem) <- NA
          final_dem[!is.na(bgc_rast)] <- 999
          final_dem[dat_spp$SiteRef] <- dat_spp$CurrRound
          coltab(final_dem) <- suit_cols
          final_rgb <- colorize(final_dem, to = "rgb", alpha = TRUE)
          writeRaster(final_rgb, paste0(out_folder,"/HistoricFeas_",period,"_",edatope,"_",spp,".tif"), overwrite = T)
        }
        
        ##new feasibility
        values(final_dem) <- NA
        final_dem[!is.na(bgc_rast)] <- 999
        final_dem[dat_spp$SiteRef] <- dat_spp$FeasRound
        coltab(final_dem) <- suit_cols
        final_rgb <- colorize(final_dem, to = "rgb", alpha = TRUE)
        writeRaster(final_rgb, paste0(out_folder,"/NewFeas_",obs_nm,period,"_",edatope,"_",spp,".tif"), overwrite = T)
        
        ## raw rasters
        trast <- copy(final_dem)
        values(trast) <- NA
        trast[dat_spp$SiteRef] <- dat_spp$FeasRound
        trast[trast == 999] <- NA
        trast <- as.int(trast * 10)
        writeRaster(trast, paste0(out_folder_raw,"/Feasibility_",obs_nm,period,"_",edatope,"_",spp,".tif"),overwrite = T, datatype = "INT2U")
        
        ##mean change
        values(final_dem) <- NA
        final_dem[!is.na(bgc_rast)] <- 999
        final_dem[dat_spp$SiteRef] <- dat_spp$FeasChange + 15
        final_rgb <- subst(final_dem, change_cols$value, t(col2rgb(change_cols$Colour,alpha = TRUE)),names = c("red","green", "blue","alpha"))
        writeRaster(final_rgb, paste0(out_folder,"/MeanChange_",obs_nm,period,"_",edatope,"_",spp,".tif"),overwrite = T)
        
        trast <- copy(final_dem)
        values(trast) <- NA
        trast[dat_spp$SiteRef] <- dat_spp$FeasChange
        trast[trast == 999] <- NA
        trast <- as.int(trast * 10)
        writeRaster(trast, paste0("raw_rasters/MeanChange_",period,"_",edatope,"_",spp,".tif"),overwrite = T, datatype = "INT4S")
        
        gc()
      }
    }
  }
}

bgc_database <- function(dbCon, bgc_rast, bgc_ids, drop = FALSE, in_folder = "bgc_data"){
  if(drop){
    dbExecute(dbCon, "drop table bgc_preds")
    dbExecute(dbCon, "create table bgc_preds (cellid integer, fp_code smallint, bgc_pred varchar(12), bgc_prop real);")
    message("Dropped table")
  }
  data_files <- list.files(in_folder)
  data_files <- data_files[!grepl("obs",data_files)]
  for(file in data_files){
    cat(file,"\n")
    dat <- fread(file.path(in_folder,file))
    dat[,fp_code := as.integer(substr(period, 1, 4))]
    dat2 <- dat[,.(cellid = as.integer(cellnum),
                   fp_code, 
                   bgc_pred,
                   bgc_prop)]
    
    dbWriteTable(dbCon, "bgc_preds", dat2, row.names = FALSE, append = TRUE)
  }
  
  ##observed period
  dat <- fread(file.path(in_folder,"bgc_summary_obs.csv"))
  dat[,fp_code := 1981L]
  dat2 <- dat[,.(cellid = as.integer(cellnum),
                 fp_code, 
                 bgc_pred,
                 bgc_prop)]
  
  dbWriteTable(dbCon, "bgc_preds", dat2, row.names = FALSE, append = TRUE)
  
  ##historic period
  bgc_dat <- as.data.frame(bgc_rast,cells = T)
  setDT(bgc_dat)
  bgc_dat[bgc_ids, bgc_pred := i.bgc, on = "bgc_id"]
  bgc_dat[,fp_code := 1961L]
  bgc_dat[,bgc_prop := 1]
  dat2 <- bgc_dat[,.(cellid = as.integer(cell),
                     fp_code, 
                     bgc_pred,
                     bgc_prop)]
  
  dbWriteTable(dbCon, "bgc_preds", dat2, row.names = FALSE, append = TRUE)
  
  message("Creating Index...")
  #dbExecute(db, "create index bgc_idx on bgc_preds(fp_code,cellid)")
  dbExecute(dbCon, "create index on bgc_preds(cellid)")
  message("Finished bgc_preds table!")
}

suit_database <- function(dbCon, drop = FALSE, in_folder = "cciss_feas") {
  data_files <- list.files(in_folder)
  temp <- c("Pl","Sx","Fd","Cw","Hw","Bl","At", "Ac", "Ep", "Yc", "Pw", "Ss", "Bg", "Lw")
  cw_spp <- data.table(Spp = temp, spp_id = seq_along(temp))
  if(drop){
    dbExecute(dbCon, "drop table cciss_feas")
    dbExecute(dbCon, "create table cciss_feas (cellid integer, fp_code smallint, spp_id smallint, newsuit smallint, prop1 smallint, prop2 smallint, prop3 smallint, edatope smallint);")
  }
  
  for(file in data_files){
    cat(file,"\n")
    dat <- fread(file.path(in_folder,file))
    if(grepl("obs", file)){
      dat[, fp_code := 1981L]
    } else {
      dat[,fp_code := as.integer(substr(FuturePeriod, 1, 4))]
    }
    
    dat[cw_spp, spp_id := i.spp_id, on = "Spp"]
    dat[,`:=`(Newsuit = as.integer(Newsuit * 100),
              prop1 = as.integer(Prop1 * 100),
              prop2 = as.integer(Prop2 * 100),
              prop3 = as.integer(Prop3 * 100))]
    dat2 <- dat[,.(cellid = as.integer(SiteRef),
                   fp_code, 
                   spp_id,
                   newsuit = Newsuit,
                   prop1,prop2,prop3)]
    if(grepl("B2",file)){
      eda_code <- 1L
    }else if(grepl("C4",file)){
      eda_code <- 2L
    }else{
      eda_code <- 3L
    }
    dat2[,edatope := eda_code]
    dbWriteTable(dbCon, "cciss_feas", dat2, row.names = FALSE, append = TRUE)
  }
  dbExecute(dbCon, "create index on cciss_feas(cellid, edatope, spp_id)")
  message("Finished cciss_feas table...")
  
  ##cciss historic
  if(drop){
    dbExecute(dbCon, "drop table cciss_historic")
    dbExecute(dbCon, "create table cciss_historic (cellid integer, spp_id smallint, suit smallint, edatope smallint);")
  }
  data_files <- list.files(in_folder,pattern = "CCISS_obs.*")
  for(file in data_files){
    dat <- fread(file.path(in_folder,file))
    dat[cw_spp, spp_id := i.spp_id, on = "Spp"]
    dat[,`:=`(suit = as.integer(Curr * 100))]
    dat2 <- dat[,.(cellid = as.integer(SiteRef),
                   spp_id,
                   suit)]
    if(grepl("B2",file)){
      eda_code <- 1L
    }else if(grepl("C4",file)){
      eda_code <- 2L
    }else{
      eda_code <- 3L
    }
    dat2[,edatope := eda_code]
    dbWriteTable(dbCon, "cciss_historic", dat2, row.names = FALSE, append = TRUE)
  }
  dbExecute(dbCon, "create index on cciss_historic(cellid, edatope, spp_id)")
  message("Finished cciss_historic table...")
}

cciss_novelty <- function(raster_template, BGCmodel, pts, bgc_folder = "bgc_preds_raw", out_folder = "novelty_raw") {
  
  clim.pts <- downscale(xyz = pts, which_refmap = "refmap_climr",
                        vars = list_vars())
  addVars(clim.pts)
  
  # Calculate the centroid climate for the training points
  clim.pts.mean <- clim.pts[, lapply(.SD, mean), by = pts$BGC, .SDcols = -c(1,2)]
  
  # historical interannual climatic variability at the geographic centroids of the training points
  pts.mean <- pts[, lapply(.SD, mean), by = BGC]
  pts.mean$id <- 1:dim(pts.mean)[1]
  clim.icv.pts <- downscale(xyz = pts.mean,
                            obs_years = 1951:1990,
                            obs_ts_dataset = "cru.gpcc",
                            return_refperiod = FALSE,
                            vars = list_vars())
  addVars(clim.icv.pts)
  
  
  #################climr####################
  points_dat <- as.data.frame(raster_template, cells=T, xy=T)
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
  splits <- c(seq(1,nrow(points_dat), by = 500000),nrow(points_dat)+1)
  
  ## current observed period
  # message("Observed Novelty...")
  # gcm_curr <- "Obs"
  # bgc_preds <- fread(file.path(bgc_folder, "/BGC_Pred_Obs.csv"))
  # res_ls <- list()
  # for (i in 1:(length(splits) - 1)){
  #   clim_dat <- downscale(points_dat[splits[i]:(splits[i+1]-1),], #
  #                         which_refmap = "refmap_climr",
  #                         obs_periods = "2001_2020",
  #                         vars = as.vector(outer(c("Tmin", "Tmax", "PPT"), c("wt", "sp", "sm", "at"), paste, sep = "_")),
  #                         #nthread = 4,
  #                         return_refperiod = FALSE)
  #   clim_dat <- na.omit(clim_dat)
  #   res_ls[[i]] <- clim_dat
  # }
  
  # clim_all <- rbindlist(res_ls)
  # period_obs <- "2001_2020"
  # clim_curr <- clim_all[PERIOD == period_obs,]
  # bgc_curr <- bgc_preds[period == period_obs,]
  # clim_curr[bgc_curr, bgc_pred := i.bgc_pred, on = c(id = "cellnum")]
  
  # message("Starting Novelty")
  # novelty <- analog_novelty(clim.targets = clim_curr, 
  #                   clim.analogs = clim.pts, 
  #                   label.targets = clim_curr$bgc_pred, 
  #                   label.analogs = pts$BGC, 
  #                   logVars = TRUE,
  #                   vars = as.vector(outer(c("Tmin", "Tmax", "PPT"), c("wt", "sp", "sm", "at"), paste, sep = "_")),
  #                   clim.icvs <- clim.icv.pts,
  #                   label.icvs <- pts.mean$BGC[clim.icv.pts$id],
  #                   weight.icv = 0.5,
  #                   threshold = 0.95,
  #                   pcs = NULL
  # )
  # dat <- data.table(cellnum = clim_curr$id, period = period_obs, novelty = novelty)
  # fwrite(dat, file.path(out_folder, "Novelty_Obs.csv"), append = T)
  
  # message("Starting GCM novelty...")
  # #### Future periods
  # for (gcm_i in 2:length(gcms_use)){
  #   gcm_curr <- gcms_use[gcm_i]
  #   message(gcms_use[gcm_i])
  #   bgc_preds <- fread(paste0(bgc_folder,"/BGC_Pred_",gcm_curr,".csv"))
  #   res_ls <- list()
  #   for (i in 1:(length(splits) - 1)){
  #     cat(i, "\n")
  #     clim_dat <- downscale(points_dat[splits[i]:(splits[i+1]-1),], #
  #                           which_refmap = "refmap_climr",
  #                           gcms = gcms_use[gcm_i],
  #                           gcm_periods = periods_use,
  #                           ssps = ssp_use,
  #                           vars = as.vector(outer(c("Tmin", "Tmax", "PPT"), c("wt", "sp", "sm", "at"), paste, sep = "_")),
  #                           run_nm = runs_use[gcm_i],
  #                           #nthread = 4,
  #                           return_refperiod = FALSE)
  #     clim_dat <- na.omit(clim_dat)
  #     res_ls[[i]] <- clim_dat
  #   }
  
  #   clim_all <- rbindlist(res_ls)
  #   for(j in periods_use){ ##have to calculate novelty on full BC map
  #     message("novelty",j)
  #     clim_curr <- clim_all[PERIOD == j,]
  #     bgc_curr <- bgc_preds[period == j,]
  #     clim_curr[bgc_curr, bgc_pred := i.bgc_pred, on = c(id = "cellnum")]
  #     clim_curr <- na.omit(clim_curr)
  
  #     novelty <- analog_novelty(clim.targets = clim_curr, 
  #                       clim.analogs = clim.pts, 
  #                       label.targets = clim_curr$bgc_pred, 
  #                       label.analogs = pts$BGC, 
  #                       logVars = TRUE,
  #                       vars = as.vector(outer(c("Tmin", "Tmax", "PPT"), c("wt", "sp", "sm", "at"), paste, sep = "_")),
  #                       clim.icvs <- clim.icv.pts,
  #                       label.icvs <- pts.mean$BGC[clim.icv.pts$id],
  #                       weight.icv = 0.5,
  #                       threshold = 0.95,
  #                       pcs = NULL
  
  #     )
  #     dat <- data.table(cellnum = clim_curr$id, period = j, novelty = novelty)
  #     fwrite(dat, paste0(out_folder, "/Novelty_",gcm_curr,".csv"), append = T)
  #   }
  # } 
  
  
  ### subzone ensemble novelty
  # files <- list.files(bgc_folder, pattern = paste(gcms_use,collapse = "|"))
  # message("Using ", files, " for ensemble")
  # ls_all <- list()
  # for(f in files){
  #   ls_all[[f]] <- fread(file.path(bgc_folder,f))
  # }
  
  # bgc_all <- rbindlist(ls_all)
  # rm(ls_all)
  # gc()
  # message("Calculating Winning Models...")
  bgc_preds <- fread(file.path(bgc_folder,"SZ_Ensemble.csv"))
  # bgc_all[bgc_preds, bgc_win := i.bgc_pred, on = c("cellnum","period")]
  
  # win_mods <- bgc_all[bgc_pred == bgc_win, .(cellnum, period, gcm)]
  # #setorder(win_mods, cellnum, period)
  # rm(bgc_all)
  # gc()
  
  # ##calculate and save climate data based on winning models
  # message("Getting ensemble climate...")
  # splits <- c(seq(1,nrow(points_dat), by = 750000),nrow(points_dat)+1)
  # res_all <- list()
  # for (i in 1:(length(splits) - 1)){
  #   cat(".")
  #   res_ls <- list()
  #   for(gcm_i in 1:length(gcms_use)){
  #     clim_dat <- downscale(points_dat[splits[i]:(splits[i+1]-1),], #
  #                         which_refmap = "refmap_climr",
  #                         gcms = gcms_use[gcm_i],
  #                         gcm_periods = periods_use,
  #                         ssps = ssp_use,
  #                         vars = as.vector(outer(c("Tmin", "Tmax", "PPT"), c("wt", "sp", "sm", "at"), paste, sep = "_")),
  #                         run_nm = runs_use[gcm_i],
  #                         #nthread = 4,
  #                         return_refperiod = FALSE)
  #     clim_dat <- na.omit(clim_dat)
  #     res_ls[[gcm_i]] <- clim_dat
  #   }
  #   clim_all <- rbindlist(res_ls)  
  #   clim_all[win_mods, isin := i.gcm, on = c(id = "cellnum",PERIOD = "period",GCM = "gcm")]
  #   clim_all <- clim_all[!is.na(isin),]
  #   clim_sum <- clim_all[, lapply(.SD, FUN = mean), 
  #   .SDcols = c("Tmin_wt", "Tmax_wt", "PPT_wt", "Tmin_sp", "Tmax_sp", "PPT_sp", "Tmin_sm", "Tmax_sm", "PPT_sm", "Tmin_at", "Tmax_at", "PPT_at"), by = .(id, PERIOD)]
  #   setorder(clim_sum, id, PERIOD)
  #   fwrite(clim_sum, file.path(out_folder, "Ensemble_Climates.csv"), append = T)
  #   rm(clim_all,clim_sum)
  #   gc()
  # }
  
  message("Calculating novelty...")
  clim_all <- fread(file.path(out_folder, "Ensemble_Climates.csv"))
  for(j in periods_use){
    cat("novelty",j,"\n")
    clim_curr <- clim_all[PERIOD == j,]
    bgc_curr <- bgc_preds[period == j,]
    clim_curr[bgc_curr, bgc_pred := i.bgc_pred, on = c(id = "cellnum")]
    
    novelty <- analog_novelty(clim.targets = clim_curr, 
                              clim.analogs = clim.pts, 
                              label.targets = clim_curr$bgc_pred, 
                              label.analogs = pts$BGC, 
                              vars = as.vector(outer(c("Tmin", "Tmax", "PPT"), c("wt", "sp", "sm", "at"), paste, sep = "_")),
                              clim.icvs <- clim.icv.pts,
                              label.icvs <- pts.mean$BGC[clim.icv.pts$id],
                              weight.icv = 0.5,
                              threshold = 0.95,
                              pcs = NULL
                              
    )
    dat <- data.table(cellnum = clim_curr$id, period = j, novelty = novelty)
    fwrite(dat, file.path(out_folder,"Novelty_SZ_Ensemble.csv"), append = T)
  }
  message("Done!")
}

plot_novelty <- function(raster_template, in_folder = "novelty_raw",out_folder = "novelty_rgb"){
  breakseq <- c(0,4,8)
  bs2 <- breakseq * 100
  breakpoints <- seq.int(bs2[1], bs2[3], 1)
  ColScheme <- colorRampPalette(c("gray90", "gray50", "#FFF200", "#CD0000", "#000000"))(length(breakpoints))
  coltab <- data.table(
    values = as.integer(breakpoints),
    color = ColScheme               # Corresponding colors
  )
  
  
  files <- list.files(in_folder, pattern = "Novelty_.*")
  for (f in files){
    nov <- fread(file.path(in_folder, f))
    fnm <- gsub("Novelty_","",tools::file_path_sans_ext(f))
    message("Processing ",fnm)
    if(grepl("Obs",f)) periods_use <- list_obs_periods()
    else periods_use <- list_gcm_periods()
    for(j in periods_use){
      nov_curr <- nov[period == j,]
      rt <- copy(raster_template)
      values(rt) <- NA
      rt[nov_curr$cellnum] <- nov_curr$novelty
      rt <- round(rt, digits = 2)
      rt <- as.int(rt*100)
      rt[rt > 800] <- 800
      rgbnov <- subst(rt, coltab$values, t(col2rgb(coltab$color,alpha = TRUE)),names = c("red","green", "blue","alpha"))
      writeRaster(rgbnov, file.path(out_folder,paste0("Novelty_",fnm,"_",j,".tif")), overwrite = T)
    }
  }
}

novelty_database <- function(dbCon, raster_template, BGCmodel, pts, novelty_folder = "novelty_raw", bgc_folder = "bgc_preds_raw"){
  ##analog novelty database
  dbExecute(dbCon, "drop table if exists future_climate, historic_climate, historic_icv")
  pred_vars <- BGCmodel[["forest"]][["independent.variable.names"]] ##required predictors
  
  # DEM
  dem <- aggregate(raster_template, fact=15)
  # climate data for the biogeoclimatic projections
  gcms_use <- c("ACCESS-ESM1-5","EC-Earth3","GISS-E2-1-G","MIROC6","MPI-ESM1-2-HR","MRI-ESM2-0")
  runs_use <- c("r1i1p1f1","r4i1p1f1","r2i1p3f1","r2i1p1f1","r1i1p1f1","r1i1p1f1")
  ssp_use <- "ssp245"
  periods_use <- list_gcm_periods()
  
  grid <- as.data.frame(dem, cells = TRUE, xy = TRUE)
  colnames(grid) <- c("id", "lon", "lat", "elev") # rename column names to what climr expects
  
  res_ls <- list()
  for(i in 1:length(gcms_use)){
    clim <- downscale(xyz = grid,
                      gcms = gcms_use[i],
                      ssps = ssp_use,
                      gcm_periods = list_gcm_periods(),
                      run_nm = runs_use[i],
                      vars = list_vars()
    )
    res_ls[[i]] <- clim
  }
  
  clim.grid <- rbindlist(res_ls)
  addVars(clim.grid)
  clim.grid <- clim.grid[is.finite(CMD.total)] #remove NA rows to have complete cases for RF model
  
  #historical climate for training points
  #colnames(pts) <- c("id", "BGC", "lon", "lat", "elev") # rename column names to what climr expects
  clim.pts <- downscale(xyz = pts,
                        vars = list_vars())
  addVars(clim.pts)
  
  # Calculate the centroid climate for the training points
  clim.pts.mean <- clim.pts[, lapply(.SD, mean), by = pts$BGC, .SDcols = -c(1,2)]
  
  # historical interannual climatic variability at the geographic centroids of the training points
  pts.mean <- pts[, lapply(.SD, mean), by = BGC]
  pts.mean$id <- 1:dim(pts.mean)[1]
  clim.icv.pts <- downscale(xyz = pts.mean,
                            obs_years = 1951:1990,
                            obs_ts_dataset = "cru.gpcc",
                            return_refperiod = FALSE,
                            vars = list_vars())
  addVars(clim.icv.pts)
  bgc.pred <- predict(BGCmodel, data = clim.grid)[['predictions']]
  clim.grid[,bgc_pred := bgc.pred]
  vars <- c("id", "GCM", "SSP", "RUN", "PERIOD", "bgc_pred", as.vector(outer(c("Tmin", "Tmax", "PPT"), c("wt", "sp", "sm", "at"), paste, sep = "_")))
  clim_final <- clim.grid[,..vars]
  clim_final <- clim_final[!is.na(GCM),]
  
  ##write to db
  dbWriteTable(dbCon, "future_climate", clim_final, row.names = FALSE)
  
  clim.pts[,bgc := pts$BGC]
  vars <- c("id", "bgc", as.vector(outer(c("Tmin", "Tmax", "PPT"), c("wt", "sp", "sm", "at"), paste, sep = "_")))
  hist.clim <- clim.pts[,..vars]
  dbWriteTable(dbCon, "historic_climate", hist.clim, row.names = FALSE)
  dbExecute(dbCon, "create index on historic_climate(bgc)")
  
  clim_icv <- clim.icv.pts
  clim_icv[,bgc := pts.mean$BGC[clim.icv.pts$id]]
  vars <- c("id", "PERIOD", "bgc", as.vector(outer(c("Tmin", "Tmax", "PPT"), c("wt", "sp", "sm", "at"), paste, sep = "_")))
  clim_icv <- clim_icv[,..vars]
  dbWriteTable(dbCon, "historic_icv", clim_icv, row.names = FALSE)
  dbExecute(dbCon, "create index on historic_icv(bgc)")
  
  
  ##load data for SZ ensembles
  clim.grid <- fread(file.path(novelty_folder,"Ensemble_Climates.csv"))
  bgc.pred <- fread(file.path(bgc_folder, "SZ_Ensemble.csv"))
  clim.grid[bgc.pred, bgc_pred := i.bgc_pred, on = c(id = "cellnum", PERIOD = "period")]
  clim.grid[,GCM := "SZ_Ensemble"]
  clim.grid[,`:=`(SSP = NA, RUN = NA)]
  vars <- c("id", "GCM", "SSP", "RUN", "PERIOD", "bgc_pred", as.vector(outer(c("Tmin", "Tmax", "PPT"), c("wt", "sp", "sm", "at"), paste, sep = "_")))
  clim_final <- clim.grid[,..vars]
  
  clim_sml <- clim_final[,.SD[sample(.N, min(500,.N))],by = bgc_pred]
  
  
  dbWriteTable(dbCon, "future_climate", clim_sml, row.names = FALSE, append = TRUE)
  dbExecute(dbCon, "create index on future_climate(\"GCM\", \"PERIOD\", bgc_pred)")
}

##### Helper Functions #################################
cciss_full <- function(SSPred,suit,spp_select){
  suit <- suit[Spp %in% spp_select,.(BGC,SS_NoSpace,Spp,Feasible)]
  suit <- unique(suit)
  suit <- na.omit(suit)
  SSPred <- SSPred[!grepl("[0-9]a$|[0-9]b$|[0-9]c$",SS_NoSpace),] ##remove phases
  SSPred <- SSPred[!grepl("\\.1$|\\.2$|\\.3$",SS_NoSpace),]
  SSPred <- SSPred[,.(SiteRef,FuturePeriod,BGC,SS_NoSpace,SS.pred,SSprob)]
  Site_BGC <- unique(SSPred[,.(SiteRef,BGC)])
  SSPred <- na.omit(SSPred)
  setkey(SSPred,SS.pred)
  setkey(suit,SS_NoSpace)
  suitMerge <- suit[SSPred, allow.cartesian = T]
  suitMerge <- na.omit(suitMerge)
  setnames(suitMerge, old = c("SS_NoSpace", "i.SS_NoSpace"), new = c("SS.pred", "SS_NoSpace"))
  suitVotes <- data.table::dcast(suitMerge, SiteRef + Spp + FuturePeriod + SS_NoSpace ~ Feasible, 
                                 value.var = "SSprob", fun.aggregate = sum)
  # Fill with 0 if columns does not exist, encountered the error at SiteRef 3104856 
  colNms <- c("1","2","3","X")
  set(suitVotes, j = as.character(1:5)[!as.character(1:5) %in% names(suitVotes)], value = 0)
  
  suitVotes[,VoteSum := `1`+`2`+`3`+`4`+`5`]
  suitVotes[,X := 1 - VoteSum]
  suitVotes[,VoteSum := NULL]
  suitVotes[,X := X + `5` + `4`]
  suitVotes[,`:=`(`5` = NULL, `4` = NULL)]
  setkey(suitVotes, SS_NoSpace, Spp)
  setkey(suit, SS_NoSpace, Spp)
  suitVotes[suit, Curr := i.Feasible]
  suitVotes[is.na(Curr), Curr := 5]
  setorder(suitVotes,SiteRef,SS_NoSpace,Spp,FuturePeriod)
  suitVotes[Curr > 3.5, Curr := 4]
  
  suitVotes[,Improve := ModelDir(as.matrix(.SD), Curr = Curr, dir = "Improve"),.SDcols = colNms]
  suitVotes[,Decline := ModelDir(as.matrix(.SD), Curr = Curr, dir = "Decline"),.SDcols = colNms]
  datRot <- suitVotes[,lapply(.SD, mean),.SDcols = c("Improve","Decline"), by = list(SiteRef,SS_NoSpace,FuturePeriod,Spp,Curr)]
  datRot[,`:=`(Improve = round(Improve*100),Decline = round(Decline*100))]
  datRot[,Curr := NULL]
  
  suitVotes <- suitVotes[,lapply(.SD, sum),.SDcols = colNms, 
                         by = .(SiteRef,FuturePeriod, SS_NoSpace,Spp,Curr)]
  suitVotes[,Newsuit := `1`+(`2`*2)+(`3`*3)+(X*5)]
  suitVotes <- merge(suitVotes, datRot, by = c('SiteRef','FuturePeriod','SS_NoSpace','Spp'),all = T)
  suitRes <- suitVotes[,.(Curr = mean(Curr),Newsuit = mean(Newsuit), Improve = mean(Improve), Decline = mean(Decline), Prop1 = mean(`1`), Prop2 = mean(`2`), Prop3 = mean(`3`)), by = .(SiteRef,FuturePeriod,Spp)]
  return(suitRes)
}

cppFunction('NumericVector ModelDir(NumericMatrix x, NumericVector Curr, std::string dir){
  int n = x.nrow();
  NumericVector res(n);
  NumericVector temp(5);
  NumericVector temp2;
  double curr_suit;
  if(dir == "Improve"){
    for(int i = 0; i < n; i++){
      temp = x(i,_);
      temp.push_front(0);
      curr_suit = Curr[i];
      if(curr_suit == 4){
        curr_suit = 3;
      }
      res[i] = sum(temp[Range(0,curr_suit)]);
    }
  }else{
    for(int i = 0; i < n; i++){
      temp = x(i,_);
      temp.push_back(0);
      curr_suit = Curr[i];
      if(curr_suit == 4){
        curr_suit = 3;
      }
      res[i] = sum(temp[Range(curr_suit,4)]);
    }
  }
  
  return(res);
}
')

edatopicOverlap_fast <- function(BGC,E1){
  ##regular site series edatopes
  SS <- E1[,list(BGC,SS_NoSpace,Edatopic)]
  CurrBGC <- SS[BGC, on = "BGC", allow.cartesian = T]
  setkey(BGC, BGC.pred)
  setkey(SS, BGC)
  FutBGC <- SS[BGC, allow.cartesian = T]
  setnames(FutBGC, old = c("BGC","SS_NoSpace","i.BGC"), 
           new = c("BGC.pred","SS.pred","BGC"))
  
  setkey(FutBGC, SiteRef, FuturePeriod, BGC,BGC.pred, Edatopic)
  FutBGC[,BGC.prop := NULL]
  
  setkey(CurrBGC,SiteRef,FuturePeriod, BGC,BGC.pred, Edatopic)
  new <- FutBGC[CurrBGC, allow.cartesian = T]
  new <- new[!is.na(SS.pred),]
  setkey(new, SiteRef,FuturePeriod,BGC,BGC.pred,SS_NoSpace,SS.pred)
  ##new <- new[complete.cases(new),]
  
  numEda <- E1[,list(NumEdas = .N), by = list(BGC,SS_NoSpace)]
  
  ###forwards overlap
  SS.out <- new[,list(SS.prob = .N,BGC.prop = BGC.prop[1]), 
                keyby = list(SiteRef,FuturePeriod,BGC,BGC.pred,SS_NoSpace,SS.pred)]
  SS.out[numEda,SS.Curr := i.NumEdas, on = c(SS_NoSpace = "SS_NoSpace"), allow.cartesian = T]
  SS.out[,SSProb := SS.prob/SS.Curr]
  ###reverse overlap
  SS.out.rev <- new[,list(SS.prob = .N,BGC.prop = BGC.prop[1]), 
                    keyby = list(SiteRef,FuturePeriod,BGC,BGC.pred,SS.pred,SS_NoSpace)]
  SS.out.rev[numEda,SS.Curr := i.NumEdas, on = c(SS.pred = "SS_NoSpace")]
  SS.out.rev[,SSProbRev := SS.prob/SS.Curr]
  
  ##combine them
  combAll <- merge(SS.out,SS.out.rev,by = c("SiteRef","FuturePeriod","BGC","BGC.pred","SS_NoSpace","SS.pred"))
  combAll[,allOverlap := SSProb*SSProbRev]
  setnames(combAll, old = "BGC.prop.x",new = "BGC.prop")
  combAll <- combAll[,list(SiteRef, FuturePeriod, BGC, BGC.pred, SS_NoSpace, 
                           allOverlap, SS.pred, BGC.prop)]
  #combAllSave <- combAll
  combAll <- unique(combAll)
  combAll[,MainUnit := gsub("[a-c]$|\\.[1-9]$","",SS.pred)]
  combAll <- combAll[!(BGC == BGC.pred  &  SS_NoSpace != MainUnit),] ### removes overlap where past BGC = future BGC
  combAll[,MainUnit := NULL]
  
  
  ##add in BGC probability
  combAll[,SSratio := allOverlap/sum(allOverlap), by = list(SiteRef, FuturePeriod, BGC, BGC.pred,SS_NoSpace)] ##should check this?
  setorder(combAll, SiteRef, FuturePeriod, BGC, BGC.pred, SS_NoSpace)
  
  setkey(combAll, SiteRef, FuturePeriod, BGC,BGC.pred)
  temp <- unique(combAll[,list(SiteRef,FuturePeriod,BGC,BGC.pred,BGC.prop)])
  temp[,BGC.prop := BGC.prop/sum(BGC.prop), by = list(SiteRef,FuturePeriod,BGC)]
  combAll[,BGC.prop := NULL]
  combAll <- temp[combAll]
  combAll[,SSprob := SSratio*BGC.prop]
  return(combAll)
}


logVars <- function(dat,
                    elements = c("AHM", "DD", "Eref", "FFP", "NFFD", "PAS", "PPT", "SHM", "CMD"),
                    base = exp(1),
                    add.fields = FALSE,
                    zero_adjust = FALSE) {
  
  dat <- copy(dat)
  
  # Fields to operate on (generally these should be ratio (zero-limited) variable)
  logFields <- grep(paste(elements, collapse = "|"), names(dat), value = TRUE)
  dat.log <- dat[, .SD, .SDcols = logFields]
  
  # If specified by the user, give zero values a positive value that is one order of magnitude less than the minimum positive value
  if (zero_adjust) {
    dat.log <- dat.log[, lapply(.SD, function(x) {
      x[x <= 0] <- base^(log(min(x[x > 0], na.rm = TRUE), base = base) - 1)
      return(x)
    })]
  }
  
  # Perform log transformation
  dat.log <- dat.log[, lapply(.SD, function(x) log(x, base = base))]
  
  # Add 
  if(add.fields){
    setnames(dat.log, logFields, paste0(logFields, "_log"))
    dat <- cbind(dat, dat.log)
  } else {
    dat[, (logFields) := Map(x =.SD, xname = logFields, f = function(x, xname) {
      x <- dat.log[[xname]]
      return(x)
    }), .SDcols = logFields]
  }
  return(dat)
}


analog_novelty <- function(clim.targets, clim.analogs, label.targets, label.analogs, vars,
                           clim.icvs = NULL, label.icvs = NULL, weight.icv = 0.5, sigma = TRUE,
                           analog.focal = NULL, threshold = 0.95, pcs = NULL, logVars = TRUE, 
                           plotScree = FALSE, 
                           plot2d = FALSE, plot2d.pcs = cbind(c(1,2,3,4), c(2,3,4,5)), 
                           plot3d = FALSE, plot3d.pcs=c(1,2,3), biplot = TRUE){
  
  analogs <- if(is.null(analog.focal)) unique(label.targets) else analog.focal # list of analogs to loop through
  novelty <- rep(NA, length(label.targets)) # initiate a vector to store the sigma dissimilarities
  
  for(analog in analogs){ # loop through all of the analogs used to describe the target climates. 
    clim.analog <- clim.analogs[label.analogs==analog, ..vars]
    clim.target <- clim.targets[label.targets==analog, ..vars]
    if(!is.null(clim.icvs)) clim.icv <- clim.icvs[label.icvs==analog, ..vars]
    
    ## data cleaning
    clim.analog <- clim.analog[complete.cases(clim.analog)] # remove rows without data
    clim.analog <- clim.analog[, .SD, .SDcols = which(sapply(clim.analog, function(x) var(x, na.rm = TRUE) > 0))]  # Remove zero-variance columns
    clim.target <- clim.target[, .SD, .SDcols = names(clim.analog)]
    if(!is.null(clim.icvs)) clim.icv <- clim.icv[complete.cases(clim.icv)]
    if(!is.null(clim.icvs)) clim.icv <- clim.icv[, .SD, .SDcols = names(clim.analog)]
    
    ## log-transform ratio variables
    if(logVars){
      clim.analog <- logVars(clim.analog, zero_adjust = TRUE)
      clim.target <- logVars(clim.target, zero_adjust = TRUE)
      clim.icv <- logVars(clim.icv, zero_adjust = TRUE)
      
      ## remove variables with non-finite values in the target population (this is an edge case that occurs when the target population has a variable (typically CMD) with only zeroes)
      clim.target <- clim.target[, lapply(.SD, function(x) if (all(is.finite(x))) x else NULL)]
      clim.analog <- clim.analog[, .SD, .SDcols = names(clim.target)]
      clim.icv <- clim.icv[, .SD, .SDcols = names(clim.target)]
    }
    
    ## scale the data to the variance of the analog, since this is what we will ultimately be measuring the M distance in. 
    clim.mean <- clim.analog[, lapply(.SD, mean, na.rm = TRUE)]
    clim.sd <- clim.analog[, lapply(.SD, sd, na.rm = TRUE)]
    clim.analog[, (names(clim.analog)) := lapply(names(clim.analog), function(col) {
      (get(col) - unlist(clim.mean)[col]) / unlist(clim.sd)[col]
    })]
    clim.target[, (names(clim.target)) := lapply(names(clim.target), function(col) {
      (get(col) - unlist(clim.mean)[col]) / unlist(clim.sd)[col]
    })]
    if(!is.null(clim.icvs)) clim.icv[, (names(clim.icv)) := lapply(names(clim.icv), function(col) {
      (get(col) - unlist(clim.icv[, lapply(.SD, mean, na.rm = TRUE)])[col]) / unlist(clim.sd)[col] # subtract mean of ICV to centre the ICV on zero. 
    })]
    
    ## PCA on pooled target and analog
    s <- sample(1:dim(clim.target)[1], dim(clim.analog)[1], replace = TRUE) # select a random sample of the target population to match the analog points. bootstrap if target population is smaller than analog points
    clim.target.sample <- clim.target[s,]
    pca <- prcomp(rbind(clim.analog, clim.target.sample), scale=FALSE)
    pcs.analog <- data.table(predict(pca, clim.analog))
    pcs.target <- data.table(predict(pca, clim.target))
    if(!is.null(clim.icvs)) pcs.icv <- data.table(predict(pca, clim.icv))
    
    if(is.null(pcs)){
      ## select number of pcs
      cumvar <- cumsum(pca$sdev^2 / sum(pca$sdev^2)) # vector of cumulative variance explained
      pcs <- which(cumvar >= threshold)[1]
      if(pcs<3) pcs <- 3
    }
    
    ## z-standardize the pcs to the variance of the analog. this is necessary for a metric that can be translated into sigma values. 
    weight.analog <- 1 - weight.icv
    pcs.mean.analog <- pcs.analog[, lapply(.SD, mean, na.rm = TRUE)]
    pcs.sd.analog <- pcs.analog[, lapply(.SD, sd, na.rm = TRUE)]
    if(!is.null(clim.icvs)) pcs.sd.icv <- pcs.icv[, lapply(.SD, sd, na.rm = TRUE)]
    if(!is.null(clim.icvs)) pcs.sd.combined <- weight.analog * pcs.sd.analog + weight.icv * pcs.sd.icv
    pcs.sd.use <- if(!is.null(clim.icvs)) pcs.sd.combined else pcs.sd.analog
    pcs.analog[, (names(pcs.analog)) := lapply(names(pcs.analog), function(col) {
      (get(col) - unlist(pcs.mean.analog)[col]) / unlist(pcs.sd.use)[col]
    })]
    pcs.target[, (names(pcs.target)) := lapply(names(pcs.target), function(col) {
      (get(col) - unlist(pcs.mean.analog)[col]) / unlist(pcs.sd.use)[col]
    })]
    if(!is.null(clim.icvs)) pcs.icv[, (names(pcs.icv)) := lapply(names(pcs.icv), function(col) {
      (get(col) - unlist(pcs.icv[, lapply(.SD, mean, na.rm = TRUE)])[col]) / unlist(pcs.sd.use)[col] # separately centering on the ICV mean becuase sometime the ICV is not centred on the centroid, and we want it to be. 
    })]
    
    ## create a combined covariance matrix for spatial variation and ICV
    cov.analog <- var(pcs.analog[, 1:pcs])
    cov.icv <- if (!is.null(clim.icvs)) var(pcs.icv[, 1:pcs]) else NULL
    if (!is.null(cov.icv)) {
      cov.combined <- weight.analog * cov.analog + weight.icv * cov.icv
    } else {
      cov.combined <- cov.analog
    }
    
    ## Mahalanobis distance and sigma dissimilarity
    md <- (mahalanobis(pcs.target[,1:pcs], rep(0, pcs), cov.combined))^0.5
    p <- pchi(md,pcs) # percentiles of the M distances on the chi distribution with degrees of freedom equaling the dimensionality of the distance measurement (PCs)
    q <- qchi(p,1) # values of the chi percentiles on a standard half-normal distribution (chi distribution with one degree of freedom)
    q[!is.finite(q)] <- 8 # set infinite values to 8 sigma (outside the decimal precision of pchi) 
    q[is.na(p)] <- NA # reset NA values as NA
    
    ## populate the novelty vector
    novelty[label.targets==analog] <- if(sigma) q else md
    
  } # end of the for-loop
  
  ## Plots for the final iteration of the for loop
  
  # Color Scheme for sigma novelty
  breakseq <- c(0,4,8)
  breakpoints <- c(seq(breakseq[1], breakseq[3], 0.01),199); length(breakpoints)
  ColScheme <- c(colorRampPalette(c("gray90", "gray50", "#FFF200", "#CD0000", "black"))(length(breakpoints)))
  
  ## Scree plot
  if(plotScree){
    par(mfrow=c(1,1), mar=c(3,3,1,1), mgp=c(1.75,0.25,0))
    a <- apply(predict(pca, clim.analog), 2, sd)
    b <- apply(predict(pca, clim.target), 2, sd)
    if(!is.null(clim.icvs)) c <- apply(predict(pca, clim.icv), 2, sd)
    diff <- abs(apply(predict(pca, clim.target), 2, mean) - apply(predict(pca, clim.analog), 2, mean))
    plot(0, xlim=c(1,length(a)), ylim=c(0,max(c(a,b, diff))*1.02), yaxs="i", col="white", tck=-0.005,
         xlab="Principal Component (PC)", ylab = "Standard Deviation")
    rect(pcs+0.5, -99, 99, 99, col = "grey95", lty=2)
    points(a, pch=21, bg="dodgerblue", cex=1.6)
    points(b, bg="grey", pch=21, cex=1.3)
    if(!is.null(clim.icvs)) points(c, bg="black", pch=21, cex=1)
    points(diff, col="black", pch=17, cex=1.3)
    text(pcs+0.5, max(c(a,b, diff)), paste0("Truncation at ", pcs, " PCs"), pos=4)
    s <- if(!is.null(clim.icvs)) 1:4 else 1:3
    legend("topright", title=analog, 
           legend=c("Analog", "Target", "Separation of means", "ICV")[s], 
           pt.bg=c("dodgerblue", "grey", NA, NA)[s], 
           col = c("black", "black", "black", "black")[s], 
           pt.cex=c(1.6,1.3,1.3, 1)[s], 
           pch=c(21, 21, 17, 16)[s], 
           bty="n")
    box()
  }
  
  ## 2D scatterplot
  if(plot2d){
    par(mfrow=c(2,2), mar=c(3,3,1,1), mgp=c(1.75,0.25,0))
    for(i in 1:4){
      a <- predict(pca, clim.analog)[, plot2d.pcs[i,]]
      b <- predict(pca, clim.target)[, plot2d.pcs[i,]]
      b <- sweep(b, 2, apply(a, 2, mean), '-') # shift the target data so that the analog centroid is at zero. this is done at a later stage than the pca in the distance calculation.
      a <- sweep(a, 2, apply(a, 2, mean), '-') # centre the analog centroid on zero. this is done at a later stage than the pca in the distance calculation.
      plot(a, col="dodgerblue", xlim=range(c(a[,1], b[,1])), ylim=range(c(a[,2], b[,2])), asp=1, tck=0.01)
      points(b, bg=ColScheme[cut(q, breakpoints)], pch=21, cex=1.5)
      if(!is.null(clim.icvs)){
        c <- predict(pca, clim.icv)[, plot2d.pcs[i,]]
        c <- sweep(c, 2, apply(c, 2, mean), '-') # centre the ICV on the analog centroid. this is done at a later stage than the pca in the distance calculation. 
        points(c, col="black", pch=16, cex=1)
      }
      points(a, col="dodgerblue", pch=16)
      mtext(paste(analog, "\n", pcs, "PCs"), line=-2.5, adj = 0.05, )
    }
  }
  
  ## 3D scatterplot
  if(plot3d){
    
    # revert to the raw pcs (centered on the analog centroid), because standardization obscures the shape of the analog distribution
    a <- predict(pca, clim.analog)
    b <- predict(pca, clim.target)
    b <- sweep(b, 2, apply(a, 2, mean), '-') # shift the target data so that the analog centroid is at zero. this is done at a later stage than the pca in the distance calculation.
    a <- sweep(a, 2, apply(a, 2, mean), '-') # centre the analog centroid on zero. this is done at a later stage than the pca in the distance calculation.
    
    b_colors <- ColScheme[cut(q, breakpoints)] # Define colors for points in 'b'
    
    # Create the 3D scatterplot
    plot <- plot_ly() %>%
      add_trace(
        x = a[, plot3d.pcs[1]], y = a[, plot3d.pcs[2]], z = a[, plot3d.pcs[3]],
        type = "scatter3d", mode = "markers",
        marker = list(size = 5, color = "dodgerblue", opacity = 1),
        name = "Analog Points"
      ) %>%
      add_trace(
        x = b[, plot3d.pcs[1]], y = b[, plot3d.pcs[2]], z = b[, plot3d.pcs[3]],
        type = "scatter3d", mode = "markers",
        marker = list(size = 6, color = b_colors, opacity = 1),
        name = "Target Points"
      ) 
    # Add ICV points if they exist
    if(!is.null(clim.icvs)) {
      c <- predict(pca, clim.icv)
      c <- sweep(c, 2, apply(c, 2, mean), '-') # centre the ICV on the analog centroid. this is done at a later stage than the pca in the distance calculation. 
      plot <- plot %>%
        add_trace(
          x = c[, plot3d.pcs[1]], y = c[, plot3d.pcs[2]], z = c[, plot3d.pcs[3]],
          type = "scatter3d", mode = "markers",
          marker = list(size = 4, color = "black", opacity = 1),
          name = "ICV"
        )
    }
    # Add biplot lines
    if(biplot) {
      loadings <- pca$rotation[, plot3d.pcs]
      scale_factor <- max(abs(c(a, b))) * 2
      scaled_loadings <- loadings * scale_factor
      for (i in 1:nrow(scaled_loadings)) {
        plot <- plot %>%
          add_trace(
            x = c(0, scaled_loadings[i, 1]),
            y = c(0, scaled_loadings[i, 2]),
            z = c(0, scaled_loadings[i, 3]),
            type = "scatter3d",
            mode = "lines+text",
            line = list(color = "black", width = 2),
            text = rownames(scaled_loadings)[i],
            textposition = "middle center",
            showlegend = FALSE, 
            name = paste("Loading:", rownames(scaled_loadings)[i])
          )
      }
    }
    plot <- plot %>%
      layout(
        scene = list(
          xaxis = list(title = paste0("PC", plot3d.pcs[1])),
          yaxis = list(title = paste0("PC", plot3d.pcs[2])),
          zaxis = list(title = paste0("PC", plot3d.pcs[3]))
        ),
        title = list(text = paste(analog, "\nNovelty in", pcs, "PCs"), x = 0.05)
      )
    # Display the plot
    print(plot)
  }
  return(novelty)
}
