##plot functions
# library(data.table)
# library(stinepack)
# library(RPostgres)
# library(pool)
# library(ggrepel)

# dbCon <- dbPool(
#   drv = RPostgres::Postgres(),
#   dbname = "cciss_spatial",
#   host = Sys.getenv("BCGOV_HOST"),
#   port = 5432, 
#   user = Sys.getenv("BCGOV_USR"),
#   password = Sys.getenv("BCGOV_PWD")
# )

zone_scheme <- c(PP = "#ea7200", MH = "#6f2997", SBS = "#2f7bd2", ESSF = "#ae38b8", 
                 CWH = "#488612", BWBS = "#4f54cf", CWF = "#7577e7", IGF = "#77a2eb", 
                 CMX = "#71d29e", BG = "#dd1320", IDF = "#e5d521", MS = "#e44ebc", 
                 SWB = "#a1dbde", CRF = "#af3a13", WJP = "#73330e", ICH = "#1fec26", 
                 CDF = "#edf418", JPW = "#96b3a5", CMA = "#eae1ee", SBPS = "#6edde9", 
                 IMA = "#e3f1fa", GBD = "#4d433f", OW = "#582511", BAFA = "#eee4f1", 
                 MMM = "#FF00FF", MHRF = "#2612dc", MGP = "#f0aeab", FG = "#92696c", 
                 SGP = "#cca261", GO = "#f0a325", SBAP = "#51d5a7", IWF = "#d44273", 
                 BSJP = "#424160", MSSD = "#dac370", MDCH = "#2d0cd4", CVG = "#c9edd3", 
                 SAS = "#92b1b6", CCH = "#7e22ca")

plot_bgc <- function(dbCon, studyarea, xvariable, gcm_nm, run_nm){
  clim_data <- dbGetQuery(dbCon, paste0("select * from clim_change where var = '",xvariable,
                                        "' and studyarea = '",studyarea,"' and gcm = '",gcm_nm,"' and run = '",run_nm,"'"))
  bgc_area <- dbGetQuery(dbCon, paste0("select * from predsum_zone where studyarea = '",
                                       studyarea,"' and not home and gcm = '",gcm_nm,"' and run = '",run_nm,"'"))
  
  clim_obs <- dbGetQuery(dbCon, paste0("select * from clim_change where var = '",xvariable,
                                        "' and studyarea = '",studyarea,"' and period = '1961_1990'"))
  bgc_obs <- dbGetQuery(dbCon, paste0("select * from predsum_zone where studyarea = '",studyarea,"'and not home and period = '1961_1990'"))
  
  clim_data <- rbind(clim_data, clim_obs)
  bgc_area <- rbind(bgc_area, bgc_obs)
  
  metadt <- unique(dbGetQuery(dbCon, paste0("select * from su_meta where studyarea = '",studyarea,"'")))
  setDT(bgc_area)
  setDT(clim_data)
  cellarea <- (metadt$res*111)*(metadt$res*111*cos(metadt$mn_lat * pi / 180))
  bgc_area[,freq := freq * cellarea]
  setnames(bgc_area, old = "zone_pred", new = "bgc_pred")

  bgc_temp <- bgc_area[,.(totalarea = sum(freq)), by = .(bgc_pred)]
  bgc_temp[,prop := totalarea/(metadt$tot_area * cellarea)]
  setorder(bgc_temp, -totalarea)
  bgc_keep <- bgc_temp[prop > 0.25, bgc_pred]
  dat_ens <- bgc_area[bgc_pred %in% bgc_keep,]
  
  dat_ens[clim_data, xvar := i.value, on = "period"]
  temp_wide <- dcast(dat_ens, xvar ~ bgc_pred, value.var = "freq")
  temp_wide[is.na(temp_wide)] <- 0
  dat_ens <- melt(temp_wide, id.vars = "xvar", variable.name = "bgc_pred", value.name = "area")
  
  dat_spline <- dat_ens[,.(area = stinterp(xvar,area,seq(min(xvar),max(xvar), diff(range(xvar))/200))$y,
                           xval = seq(min(xvar),max(xvar), diff(range(xvar))/200)),
                        by = .(bgc_pred)]
  dat_ends <- dat_spline[dat_spline[, .I[which.max(area)], by=bgc_pred]$V1]
  ggplot(dat_spline, aes(x = xval, y = area, col = bgc_pred)) +
    geom_line() +
    geom_text_repel(data = dat_ends, aes(x = xval, y = area, label = bgc_pred)) +
    scale_fill_manual(values = zone_scheme) +
    scale_colour_manual(values = zone_scheme) +
    theme_bw() +
    theme(legend.position = "none")
  
}

plot_species <- function(dbCon, studyarea, xvariable,  gcm_nm, run_nm, edatope){
  clim_data <- dbGetQuery(dbCon, paste0("select * from clim_change where var = '",xvariable,
                                        "' and studyarea = '",studyarea,"' and gcm = '",gcm_nm,"' and run = '",run_nm,"'"))
  spp_area <- dbGetQuery(dbCon, paste0("select * from predsum_spp where studyarea = '",
                                       studyarea,"' and not home and gcm = '",gcm_nm,"' and run = '",run_nm,"' and edatope = '",edatope,"'"))
  
  clim_obs <- dbGetQuery(dbCon, paste0("select * from clim_change where var = '",xvariable,
                                       "' and studyarea = '",studyarea,"' and gcm = 'obs'"))
  spp_obs <- dbGetQuery(dbCon, paste0("select * from predsum_spp where studyarea = '",
                                      studyarea,"'and not home and gcm = 'obs' and edatope = '",edatope,"'"))
  
  # clim_data <- dbGetQuery(dbCon, paste0("select * from clim_change where var = '",var,"' and \"GCM\" in ('ensembleMean','obs')"))
  # spp_area <- dbGetQuery(dbCon, paste0("select * from predsum_spp where studyarea = '",studyarea,
  #                                      "' and not home and edatope = 'C4' and gcm in ('ensembleMean','obs')"))
  clim_data <- rbind(clim_data, clim_obs)
  spp_area <- rbind(spp_area, spp_obs)
  setDT(spp_area)
  setDT(clim_data)
  
  metadt <- unique(dbGetQuery(dbCon, paste0("select * from su_meta where studyarea = '",studyarea,"'")))
  cellarea <- (metadt$res*111)*(metadt$res*111*cos(metadt$mn_lat * pi / 180))
  spp_area[,area := area * cellarea]
  
  spp_temp <- spp_area[,.(totalarea = sum(area)), by = .(spp)]
  spp_temp[,prop := totalarea/(metadt$tot_area * cellarea)]
  setorder(spp_temp, -totalarea)
  spp_keep <- spp_temp[prop > 0.75, spp]
  spp_area <- spp_area[spp %in% spp_keep,]
  
  spp_obs <- spp_area[gcm == "obs" & period == "2001_2020",]
  spp_area <- spp_area[!(gcm == "obs" & period == "2001_2020"),]
  clim_obs <- clim_data[gcm == "obs" & period == "2001_2020",]
  clim_data <- clim_data[!(gcm == "obs" & period == "2001_2020"),]
  
  
  spp_area[clim_data, xvar := i.value, on = "period"]
  spp_obs[clim_obs, xvar := i.value, on = "period"]
  #dat_ens[,Year := as.numeric(substr(period, 0,4))]
  temp_wide <- dcast(spp_area, xvar ~ spp, value.var = "area")
  temp_wide[is.na(temp_wide)] <- 0
  dat_ens <- melt(temp_wide, id.vars = "xvar", variable.name = "spp", value.name = "area")
  
  dat_spline <- dat_ens[,.(area = stinterp(xvar,area,seq(min(xvar),max(xvar), diff(range(xvar))/200))$y,
                           xval = seq(min(xvar),max(xvar), diff(range(xvar))/200)),
                        by = .(spp)]
  
  dat_ends <- dat_spline[dat_spline[, .I[which.max(area)], by=spp]$V1]

  ggplot(dat_spline, aes(x = xval, y = area, col = spp)) +
    geom_line() +
    geom_text_repel(data = dat_ends, aes(x = xval, y = area, label = spp)) +
    geom_point(data = spp_obs, aes(x = xvar, y = area, col = spp)) +
    theme_bw() +
    theme(legend.position = "none")
}


# bgccolours <- fread("../Common_Files/WNAv13_SubzoneCols.csv")
# zone_cols <- fread("./spatial_testing/WNAv13_ZoneCols.csv")
# zone_scheme <- zone_cols$colour
# names(zone_scheme) <- zone_cols$classification





