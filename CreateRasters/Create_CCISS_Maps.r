library(data.table)
library(terra)
library(climr)
library(RColorBrewer)

setwd("~/FFEC/spatcciss/")
# final_dem <- rast("BC_DEM_100m.tif")
# final_dem <- aggregate(final_dem, fact = 2)
#writeRaster(final_dem,"BC_DEM_200m.tif",datatype = "FLT8S")
final_dem <- rast("BC_DEM_200m.tif")
breakpoints.suit <-   breakseq <- c(0.5,1.5,2.5,3.5)
palette.suit <-   c("#006400", "#1E90FF", "#EEC900")
breakpoints.change <- c(seq(-2.5,2.5,0.5),-10,10,20,30) + 15
palette.change <- c(brewer.pal(11,"RdBu")[c(1,2,3,4,5,6)], brewer.pal(11,"RdBu")[c(7,8,9,10,11)],"#000000", brewer.pal(9,"YlOrRd")[1:3]) # nolint
breakpoints.binary <- seq(-1,1,0.2)
palette.binary <- c(brewer.pal(11,"RdBu")[c(1:4,6,6)], brewer.pal(11,"RdBu")[c(6,8:11)])

##feas colours
suit_cols <- data.table(value = c(1,2,3),Colour = palette.suit)

##mean change colours
change_cols <- data.table(value = breakpoints.change, Colour = palette.change)

##addret colours
addret_cols <- data.table(value = breakpoints.binary*100, Colour = palette.binary)

periods <- list_gcm_periods()[-5]
edatopes <- c("B2","C4","E6")
species <- c("Pl","Sx","Fd","Cw","Hw","Bl","At", "Ac", "Ep", "Yc", "Pw", "Ss", "Lw", "Sb")

period <- "2021_2040"
edatope <- "C4"
spp <- "Fd"
for(period in periods){
    for(edatope in edatopes){
        dat <- fread(paste0("cciss_feas/CCISS_",period,"_",edatope,".csv"))
        for(spp in species){
            cat(period, edatope, spp, "\n")
            dat_spp <- dat[Spp == spp,]
            dat_spp <- dat_spp[Curr < 3.5 | Newsuit < 3.5,]
            dat_spp[,FeasChange := Curr - Newsuit]
            dat_spp[Newsuit > 3.5 & Curr <= 3, FeasChange := -10]
            dat_spp[Curr > 3.5, FeasChange := round(FeasChange) * 10]
            dat_spp[,FeasChange := round(FeasChange/0.5)*0.5]
            dat_spp[,FeasRound := round(Newsuit)]
            dat_spp[FeasRound > 3, FeasRound := NA]
            dat_spp[,AddRet := Improve]
            dat_spp[Decline > Improve, AddRet := -Decline]
            dat_spp[,AddRet := round(AddRet/20)*20]

            ##new feasibility
            # values(final_dem) <- NA
            # final_dem[dat_spp$SiteRef] <- dat_spp$FeasRound
            # coltab(final_dem) <- suit_cols
            # final_rgb <- colorize(final_dem, to = "rgb", alpha = TRUE)
            # writeRaster(final_rgb, paste0("final_rgb/NewFeas_",period,"_",edatope,"_",spp,".tif"), overwrite = T)

            ##mean change
            values(final_dem) <- NA
            final_dem[dat_spp$SiteRef] <- dat_spp$FeasChange + 15
            final_rgb <- subst(final_dem, change_cols$value, t(col2rgb(change_cols$Colour,alpha = TRUE)),names = c("red","green", "blue","alpha"))
            writeRaster(final_rgb, paste0("final_rgb/MeanChangeTest_",period,"_",edatope,"_",spp,".tif"),overwrite = T)
            ## Prop improve/decline
            values(final_dem) <- NA
            final_dem[dat_spp$SiteRef] <- dat_spp$AddRet
            coltab(final_dem) <- addret_cols
            final_rgb <- colorize(final_dem, to = "rgb", alpha = TRUE)
            writeRaster(final_rgb, paste0("final_rgb/AddRet_",period,"_",edatope,"_",spp,".tif"), overwrite = T)
            gc()
        }
    }
}
cat("Done!")