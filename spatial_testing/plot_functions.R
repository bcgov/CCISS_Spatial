##plot functions
# library(data.table)
# library(stinepack)
# library(RPostgres)
# library(pool)
# library(ggrepel)
# 
# dbCon <- dbPool(
#   drv = RPostgres::Postgres(),
#   dbname = "cciss_spatial",
#   host = Sys.getenv("BCGOV_HOST"),
#   port = 5432,
#   user = Sys.getenv("BCGOV_USR"),
#   password = Sys.getenv("BCGOV_PWD")
# )
# 
# bnds <- fread("./spatial_testing/district_bounds.csv")
# bnd <- bnds[ORG_UNIT == "DQU",.(ymax, ymin, xmax, xmin)]
# boundary <- t(bnd)[,1]
# rtest <- pgGetTerra(dbCon, "feasibility_raw", tile = F, bands = 1, boundary = t(bnd)[,1])

dbGetFeasible <- function(dbCon, table_name, layer_name, boundary){
  rastque <- "rast"
  nameque <- table_name
  projID <- dbGetQuery(dbCon, paste0("select ST_SRID(", rastque, ") as srid from \"", nameque, "\" where rid = 1;"))$srid[1]
  
  info <- dbGetQuery(dbCon, paste0(
    "select
            st_xmax(st_envelope(rast)) as xmx,
            st_xmin(st_envelope(rast)) as xmn,
            st_ymax(st_envelope(rast)) as ymx,
            st_ymin(st_envelope(rast)) as ymn,
            st_width(rast) as cols,
            st_height(rast) as rows
            from
            (select st_union(", rastque, ",", 1, ") rast from \"", nameque, "\" \n
            WHERE filename = '",layer_name,"' AND ST_Intersects(",
    rastque, ",ST_SetSRID(ST_GeomFromText('POLYGON((", boundary[4],
    " ", boundary[1], ",", boundary[4], " ", boundary[2],
    ",\n  ", boundary[3], " ", boundary[2], ",", boundary[3],
    " ", boundary[1], ",", boundary[4], " ", boundary[1],
    "))'),", projID, "))) as a;"
  ))
  
  
  rast_vals_temp <- dbGetQuery(dbCon, paste0(
    "SELECT UNNEST(ST_Dumpvalues(rast, 1)) as vals 
  from (SELECT ST_Union(rast) rast FROM \"", nameque, "\" 
  WHERE filename = '",layer_name,"' AND ST_Intersects(",
    rastque, ",ST_SetSRID(ST_GeomFromText('POLYGON((", boundary[4],
    " ", boundary[1], ",", boundary[4], " ", boundary[2],
    ",\n  ", boundary[3], " ", boundary[2], ",", boundary[3],
    " ", boundary[1], ",", boundary[4], " ", boundary[1],
    "))'),", projID, "))) as a;"
  ))
  
  rout <- rast(
    nrows = info$rows, ncols = info$cols, xmin = info$xmn,
    xmax = info$xmx, ymin = info$ymn, ymax = info$ymx,
    crs = paste0("EPSG:", projID), vals = rast_vals_temp$vals
  )
  
}

# 
# ##persistance/expansion
# 
# tabnm <- "predsum_zone"
# studyarea <- "DQU"
# gcm_nm <- "ensembleMean"
# run_nm <- "ensembleMean"
# bgc_area <- dbGetQuery(dbCon, paste0("select * from ",tabnm," where studyarea = '",
#                                      studyarea,"'"))
# setDT(bgc_area)
# metadt <- unique(dbGetQuery(dbCon, paste0("select * from su_meta where studyarea = '",studyarea,"'")))
# 
# cellarea <- (metadt$res*111)*(metadt$res*111*cos(metadt$mn_lat * pi / 180))
# bgc_area[,freq := freq * cellarea]
# 
# tmp <- bgc_area[(home),]
# zone.count.home <- dcast(tmp, gcm + run + period ~ zone_pred, value.var = "freq")
# zone.count.home[is.na(zone.count.home)] <- 0
# idx <- zone.count.home[,1:3]
# zone.count.home <- zone.count.home[,-c(1:3)]
# 
# tmp <- bgc_area[(!home),]
# zone.count <- dcast(tmp, gcm + run + period ~ zone_pred, value.var = "freq")
# zone.count[is.na(zone.count)] <- 0
# idx2 <- zone.count[,1:3]
# zone.count <- zone.count[,-c(1:3)]
# period_choose <- "2041_2060"
# 
# ## calculate persistence and expansion tables
# bgc.persistence <- sweep(bgc.count.home, MARGIN=2,unlist(bgc.count[1,match(names(bgc.count.home), names(bgc.count))]), '/' )
# zone.persistence <- sweep(zone.count.home, MARGIN=2,unlist(zone.count[1,match(names(zone.count.home), names(zone.count))]), '/' )
# bgc.expansion <- sweep(bgc.count[,match(names(bgc.count.home), names(bgc.count))]-bgc.count.home, MARGIN=2,unlist(bgc.count[1,match(names(bgc.count.home), names(bgc.count))]), '/' )
# zone.expansion <- sweep(zone.count[,match(names(zone.count.home), names(zone.count))]-zone.count.home, MARGIN=2,unlist(zone.count[1,match(names(zone.count.home), names(zone.count))]), '/' )
# 
# persistence <- zone.persistence
# expansion <- zone.expansion
# 
# units <- names(persistence)
# 
# par(mar=c(3,4,0.1,0.1), mgp=c(1.25, 0.25, 0), cex=1.5)
# 
# xlim <- c(0, 1.1)
# ylim <- c(-5,3)
# plot(0, xlim=xlim, ylim=ylim, col="white", xaxt="n", yaxt="n", xlab="Persistence within historical range", ylab="")
# axis(1,at=seq(xlim[1], xlim[2], 0.2), labels=paste(seq(xlim[1], xlim[2], 0.2)*100,"%", sep=""), tck=0)
# axis(2,at=seq(ylim[1], ylim[2]), labels=paste(round(2^(seq(ylim[1], ylim[2]))*100),"%", sep=""), las=2, tck=0)
# par(mgp=c(2.75, 0.25, 0))
# title(ylab="Expansion beyond historical range", cex.lab=1)
# iso <- seq(0,1.2, 0.001)
# lines(1-iso, log2(iso), lty=2, lwd=2, col="darkgray")
# #arctext(x = "Growing feasible range", center = c(-1, -28.7), radius = 4.6, start = 0.431*pi , cex = 0.8, stretch = 1.05, col="darkgray", font=2)
# # arctext(x = "Shrinking feasible range", center = c(-1, -29.3), radius = 4.6, start = 0.431*pi , cex = 0.8, stretch = 1.05, col="darkgray", font=2)
# # mtext(paste(edatope.name[which(edatopes==edatope)], " sites", " (", edatope, ")", sep=""), side=3, line=-1.25, adj= if(edatope=="C4") 0.025 else 0.075, cex=0.7, font=1)
# 
# unit=units[2]
# for(unit in units){
#   col.focal <- zone_scheme[names(zone_scheme) == unit]
#   col.focal2 <- "black"
#   x <- persistence[which(idx$period == period_choose), which(names(persistence)==unit)]
#   y <- expansion[which(idx$period == period_choose), which(names(persistence)==unit)]
#   y[y<2^(ylim[1])] <- 2^(ylim[1])
#   y <- log2(y)
#   
#   # points(x,y)
#   if(length(x)>1){
#     if(var(x)>0) if(var(y)==0) lines(range(x, na.rm=T), range(y), col=col.focal) else dataEllipse(x, y, levels=0.5, center.pch=21, add=T, col=col.focal, fill=T, lwd=0.5, plot.points=F)
#   }
#   # points(mean(x),mean(y), pch=21, bg=col.focal, cex=if(unit==unit.persistence.focal) 4.5 else 3, col=col.focal2)
#   # text(mean(x),mean(y), unit, cex=if(unit==unit.persistence.focal) 1 else 0.7, font=2, col=col.focal2)
# }
# 
# 
# 
# 
# 
# bgc_obs <- dbGetQuery(dbCon, paste0("select * from ",tabnm," where studyarea = '",studyarea,"'and not home and period = '1961_1990'"))
# bgc_area <- rbind(bgc_area, bgc_obs)
# 
# metadt <- unique(dbGetQuery(dbCon, paste0("select * from su_meta where studyarea = '",studyarea,"'")))
# setDT(bgc_area)
# 



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

sz_scheme <- subzones_colours_ref$colour
names(sz_scheme) <- subzones_colours_ref$classification

plot_bgc <- function(dbCon, studyarea, xvariable, gcm_nm, run_nm, unit = c("Zone","Subzone")){
  
  if(unit == "Zone"){
    tabnm <- "predsum_zone"
    col_scheme <- zone_scheme
    ylabel <- "BGC Zone Area ('000 sq.km)"
  } else {
    tabnm <- "predsum_bgc"
    col_scheme <- sz_scheme
    ylabel <- "BGC Subzone Area ('000 sq.km)"
  } 
  bgc_area <- dbGetQuery(dbCon, paste0("select * from ",tabnm," where studyarea = '",
                                       studyarea,"' and not home and gcm = '",gcm_nm,"' and run = '",run_nm,"'"))
  bgc_obs <- dbGetQuery(dbCon, paste0("select * from ",tabnm," where studyarea = '",studyarea,"'and not home and period = '1961_1990'"))
  bgc_area <- rbind(bgc_area, bgc_obs)
  
  metadt <- unique(dbGetQuery(dbCon, paste0("select * from su_meta where studyarea = '",studyarea,"'")))
  setDT(bgc_area)
  
  cellarea <- (metadt$res*111)*(metadt$res*111*cos(metadt$mn_lat * pi / 180))
  bgc_area[,freq := freq * cellarea]
  
  if(unit == "Zone"){
    setnames(bgc_area, old = "zone_pred", new = "bgc_pred")
  }

  bgc_temp <- bgc_area[,.(totalarea = sum(freq)), by = .(bgc_pred)]
  bgc_temp[,prop := totalarea/(metadt$tot_area * cellarea)]
  setorder(bgc_temp, -totalarea)
  bgc_keep <- bgc_temp[prop > 0.15, bgc_pred]
  dat_ens <- bgc_area[bgc_pred %in% bgc_keep,]
  
  if(xvariable != "Time"){
    clim_data <- dbGetQuery(dbCon, paste0("select * from clim_change where var = '",xvariable,
                                          "' and studyarea = '",studyarea,"' and gcm = '",gcm_nm,"' and run = '",run_nm,"'"))
    clim_obs <- dbGetQuery(dbCon, paste0("select * from clim_change where var = '",xvariable,
                                         "' and studyarea = '",studyarea,"' and period = '1961_1990'"))
    clim_data <- rbind(clim_data, clim_obs)
    setDT(clim_data)
    dat_ens[clim_data, xvar := i.value, on = "period"]
    xlabel <- paste0("Change in ", xvariable)
  }else{
    dat_ens[, xvar := as.numeric(substr(period,1,4)) + 10]
    xlabel <- "Year"
  }
  
  temp_wide <- dcast(dat_ens, xvar ~ bgc_pred, value.var = "freq")
  temp_wide[is.na(temp_wide)] <- 0
  dat_ens <- melt(temp_wide, id.vars = "xvar", variable.name = "bgc_pred", value.name = "area")
  
  dat_spline <- dat_ens[,.(area = stinterp(xvar,area,seq(min(xvar),max(xvar), diff(range(xvar))/200))$y,
                           xval = seq(min(xvar),max(xvar), diff(range(xvar))/200)),
                        by = .(bgc_pred)]
  dat_spline[, area := area/1000]
  dat_ends <- dat_spline[dat_spline[, .I[which.max(area)], by=bgc_pred]$V1]
  
  ggplot(dat_spline, aes(x = xval, y = area, col = bgc_pred)) +
    geom_line() +
    geom_text_repel(data = dat_ends, aes(x = xval, y = area, label = bgc_pred)) +
    scale_fill_manual(values = col_scheme) +
    scale_colour_manual(values = col_scheme) +
    theme_bw() +
    theme(legend.position = "none") +
    ylab(ylabel) +
    xlab(xlabel)
  
}

plot_species <- function(dbCon, studyarea, xvariable,  gcm_nm, run_nm, edatope, spp_select){
  
  spp_area <- dbGetQuery(dbCon, paste0("select * from predsum_spp where studyarea = '",
                                       studyarea,"' and not home and gcm = '",gcm_nm,"' and run = '",run_nm,"' and edatope = '",edatope,"'"))
  
  spp_obs <- dbGetQuery(dbCon, paste0("select * from predsum_spp where studyarea = '",
                                      studyarea,"'and not home and gcm = 'obs' and edatope = '",edatope,"'"))
  
  # clim_data <- dbGetQuery(dbCon, paste0("select * from clim_change where var = '",var,"' and \"GCM\" in ('ensembleMean','obs')"))
  # spp_area <- dbGetQuery(dbCon, paste0("select * from predsum_spp where studyarea = '",studyarea,
  #                                      "' and not home and edatope = 'C4' and gcm in ('ensembleMean','obs')"))
 
  spp_area <- rbind(spp_area, spp_obs)
  setDT(spp_area)
  
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
  
  if(xvariable != "Time"){
    clim_data <- dbGetQuery(dbCon, paste0("select * from clim_change where var = '",xvariable,
                                          "' and studyarea = '",studyarea,"' and gcm = '",gcm_nm,"' and run = '",run_nm,"'"))
    clim_obs <- dbGetQuery(dbCon, paste0("select * from clim_change where var = '",xvariable,
                                         "' and studyarea = '",studyarea,"' and gcm = 'obs'"))
    clim_data <- rbind(clim_data, clim_obs)
    setDT(clim_data)
    clim_obs <- clim_data[gcm == "obs" & period == "2001_2020",]
    clim_data <- clim_data[!(gcm == "obs" & period == "2001_2020"),]
    
    spp_area[clim_data, xvar := i.value, on = "period"]
    spp_obs[clim_obs, xvar := i.value, on = "period"]
    xlabel <- paste0("Change in ", xvariable)
  }else{
    spp_area[,xvar := as.numeric(substr(period,1,4)) + 10]
    spp_obs[,xvar := as.numeric(substr(period,1,4)) + 10]
    xlabel <- "Year"
  }

  temp_wide <- dcast(spp_area, xvar ~ spp, value.var = "area")
  temp_wide[is.na(temp_wide)] <- 0
  dat_ens <- melt(temp_wide, id.vars = "xvar", variable.name = "spp", value.name = "area")
  
  dat_spline <- dat_ens[,.(area = stinterp(xvar,area,seq(min(xvar),max(xvar), diff(range(xvar))/200))$y,
                           xval = seq(min(xvar),max(xvar), diff(range(xvar))/200)),
                        by = .(spp)]
  dat_spline[,area := area/1000]
  dat_ends <- dat_spline[dat_spline[, .I[which.max(area)], by=spp]$V1]
  spp_obs[,area := area/1000]
  dat_spline[, ltype := fifelse(spp == spp_select, "solid","dashed")]

  ggplot(dat_spline, aes(x = xval, y = area, col = spp)) +
    geom_line(aes(linetype = ltype)) +
    scale_linetype_identity() +
    geom_text_repel(data = dat_ends, aes(x = xval, y = area, label = spp)) +
    geom_point(data = spp_obs, aes(x = xvar, y = area, col = spp)) +
    theme_bw() +
    theme(legend.position = "none") +
    xlab(xlabel) +
    ylab("Species Area ('000 sq.km)")
}


# bgccolours <- fread("../Common_Files/WNAv13_SubzoneCols.csv")
# zone_cols <- fread("./spatial_testing/WNAv13_ZoneCols.csv")
# zone_scheme <- zone_cols$colour
# names(zone_scheme) <- zone_cols$classification





