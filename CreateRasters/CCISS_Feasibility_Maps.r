library(climr)
library(data.table)
library(terra)
library(ranger)

setwd("~/FFEC/spatcciss/")
#dat <- fread("siteseries_2041_2060_C4.csv")
#dat <- fread("bgc_data/bgc_summary_all.csv")

# dat <- fread("bgc_data/bgc_summary_all.csv")

# pers <- unique(dat$period)
# for(p in pers){
#   sub <- dat[period == p,]
#   fwrite(sub, paste0("bgc_data/bgc_summary_",p,".csv"))
# }


addVars <- function(dat) {
  dat[, PPT_MJ := PPT_05 + PPT_06]
  dat[, PPT_JAS := PPT_07 + PPT_08 + PPT_09]
  dat[, PPT.dormant := PPT_at + PPT_wt]
  dat[, CMD.def := pmax(0, 500 - PPT.dormant)]
  dat[, CMDMax := CMD_07]   ## TODO: THIS IS NOT NECESSARILY CMD MAX
  dat[, CMD.total := CMD.def + CMD]
}

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

setwd("~/FFEC/spatcciss/")
final_dem <- rast("BC_DEM_200m.tif")
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
gcms_use <- c("ACCESS-ESM1-5", "CNRM-ESM2-1", "EC-Earth3", "GFDL-ESM4",
              "GISS-E2-1-G", "MIROC6", "MPI-ESM1-2-HR", "MRI-ESM2-0")
ssp_use <- c("ssp126", "ssp245", "ssp370")
periods_use <- list_gcm_periods()[-1]

#splits <- c(seq(1, nrow(points_dat), by = 100000), nrow(points_dat) + 1)
#load("BGC_RFresp.Rdata")
#cols <- fread("./WNAv12_3_SubzoneCols.csv")
ssp_weights <- data.table(ssp = c("ssp126", "ssp245", "ssp370"), weight = c(0.8,1,0.8))

##### current period ########################################################
# splits <- c(seq(1, nrow(points_dat), by = 500000), nrow(points_dat) + 1)
# period_curr <- list_obs_periods()
# for (i in 1:(length(splits) - 1)){
#     cat(i, "\n")
#     clim_dat <- downscale(points_dat[splits[i]:(splits[i+1]-1),], 
#                           which_refmap = "refmap_climr",
#                           obs_periods = period_curr,
#                           vars = vars_needed,
#                           nthread = 6,
#                           return_refperiod = FALSE)
#     addVars(clim_dat)
#     clim_dat <- na.omit(clim_dat)
#     #setnames(clim_dat, old = "DDsub0_sp", new = "DD_0_sp")
#     temp <- predict(BGC_RFresp, data = clim_dat, num.threads = 16)
#     dat <- data.table(cellnum = clim_dat$id,  period = clim_dat$PERIOD, bgc_pred = temp$predictions, bgc_prop = 1)
#     fwrite(dat, append = TRUE, paste0("bgc_data/bgc_summary_all", ".csv"))
#     rm(clim_dat, dat)
#     gc()
#   }
#   cat("Done!")

# ###future periods BGC preds
period_curr <- periods_use[1:3]
for (i in 1:(length(splits) - 1)){
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
    #setnames(clim_dat, old = "DDsub0_sp", new = "DD_0_sp")
    temp <- predict(BGC_RFresp, data = clim_dat, num.threads = 16)
    dat <- data.table(cellnum = clim_dat$id, ssp = clim_dat$SSP, gcm = clim_dat$GCM, 
    period = clim_dat$PERIOD, bgc_pred = temp$predictions)
    dat[ssp_weights, weight := i.weight, on = "ssp"]
    dat_sum <- dat[,.(bgc_prop = sum(weight)/20.8), by = .(cellnum, period, bgc_pred)]
    if (i == 1){
        fwrite(dat_sum, paste0("bgc_data/bgc_summary_all", ".csv"))
    }else{
        fwrite(dat_sum, append = TRUE, paste0("bgc_data/bgc_summary_all", ".csv"))
    }
    rm(clim_dat, dat, dat_sum)
    gc()
  }
  cat("Done!")

#periods <- c(list_obs_periods(), list_gcm_periods()[-c(1,5)])
periods <- c("2021_2040")
edatopes <- c("E6")
##current BGCs
bgc_rast <- rast("BC_BGC_rast.tif")
points_dat <- as.data.frame(bgc_rast, cells=T, xy=T)
rast_id <- fread("BC_BGC_rast_ids.csv")
bgc_points <- as.data.table(points_dat)
bgc_points[rast_id, BGC := i.BGC, on = "bgc_id"]
bgc_all <- fread(paste0("bgc_data/bgc_summary_all",".csv"))

for(period_curr in periods){
  bgc_sum <- bgc_all[period == period_curr,]
  bgc_sum[bgc_points, BGC := i.BGC, on = c(cellnum = "cell")]
  setcolorder(bgc_sum, c("cellnum","period","BGC","bgc_pred","bgc_prop"))
  setnames(bgc_sum, c("SiteRef","FuturePeriod","BGC","BGC.pred","BGC.prop"))

  for(edatope in edatopes){
    cat(period_curr, edatope, "\n")
    eda_table <- fread("Edatopic_v13_1.csv")
    eda_table[,HasPos := if(any(Edatopic %in% edatope)) T else F, by = .(SS_NoSpace)]
    eda_table <- unique(eda_table[(HasPos),])
    ###########################################################

    sites <- unique(bgc_sum$SiteRef)
    splits <- c(seq(1, length(sites), by = 600000), length(sites) + 1)
    for (i in 1:(length(splits) - 1)){
        cat(i, "\n")
        srs <- sites[splits[i]:(splits[i+1]-1)]
        dat_sml <- bgc_sum[SiteRef %in% srs,]
        sspred <- edatopicOverlap_fast(dat_sml, E1 = eda_table)
        if (i == 1){
            fwrite(sspred, paste0("siteseries_",period_curr, "_", edatope, ".csv"))
        }else{
            fwrite(sspred, append = TRUE, paste0("siteseries_",period_curr, "_", edatope, ".csv"))
        }
        rm(sspred)
        gc()
      }
  }
}
cat("Done!")
