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

# cols <- fread("./WNAv13_SubzoneCols.csv")
# zone_cols <- fread("./WNAv13_ZoneCols.csv")

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

####### CCISS BGC Summaries ########################################################
summary_preds_gcm <- function(raster_template, 
                              BGCmodel, 
                              gcms_use, 
                              periods_use, 
                              vars_needed, 
                              out_folder = "bgc_data",
                              start_tile = 1) {
  points_dat <- as.data.frame(raster_template, cells=T, xy=T)
  colnames(points_dat) <- c("id", "lon", "lat", "elev")
  points_dat <- points_dat[,c(2,3,4,1)] #restructure for climr input
  splits <- c(seq(1, nrow(points_dat), by = 100000), nrow(points_dat) + 1)
  message("There are ", length(splits), " tiles")
  
  for (i in start_tile:(length(splits) - 1)){
    cat(i, "\n")
    clim_dat <- downscale(points_dat[splits[i]:(splits[i+1]-1),], 
                          which_refmap = "refmap_climr",
                          gcms = gcms_use,
                          gcm_periods = period_curr,
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
  bgc_points[rast_id, BGC := i.BGC, on = "bgc_id"]
  
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
                              in_folder = "ss_preds",
                              out_folder = "cciss_feas",
                              tile_size = 4000) {
  stopifnot(all(c("BGC","SS_NoSpace","Sppsplit","FeasOrig","Spp","Feasible","Mod","OR") %in% names(feas_table)))
  
  if(obs) {
    periods <- list_obs_periods()
    obs_nm <- "obs_"
  } else {
    periods <- list_gcm_periods()
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
                                  bgc_raster,
                                  species,
                                  edatopes,
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
  addret_cols <- data.table(value = breakpoints.binary*100, Colour = palette.binary)
  
  if(obs) {
    periods <- list_obs_periods()
    obs_nm <- "obs_"
  } else {
    periods <- list_gcm_periods()
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

##### Helper Functions #################################
cciss_full <- function(SSPred,suit,spp_select){
  suit <- suit[Spp %in% spp_select,.(BGC,SS_NoSpace,Spp,Feasible)]
  suit <- unique(suit)
  suit <- na.omit(suit)
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


