##analog novelty database
library(climr)
library(terra)
library(data.table)
library(bcmaps)
library(ccissr)
library(ranger)
library(scales)
library(EnvStats)
library(plotly)

#---------------------------
# Data
#---------------------------

dat <- bcmaps::nr_districts()
dat <- st_transform(dat, 4326)
d2 <- as.data.table(dat)
d3 <- d2[,.(xmin = st_bbox(geometry)[1],
            xmax = st_bbox(geometry)[2],
            ymin = st_bbox(geometry)[3],
            ymax = st_bbox(geometry)[4]), by = ORG_UNIT]
plot(dat[7,"ORG_UNIT"])

bbox_df <- dat %>%
  rowwise() %>%
  mutate(bbox = list(st_bbox(geometry))) %>%
  mutate(
    xmin = bbox[["xmin"]],
    xmax = bbox[["xmax"]],
    ymin = bbox[["ymin"]],
    ymax = bbox[["ymax"]]
  ) %>%
  select(ORG_UNIT, xmin, xmax, ymin, ymax) %>%
  ungroup()
d3 <- st_drop_geometry(bbox_df)
fwrite(d3, "district_bounds.csv")

d3 <- d2[,.(geometry = st_as_sfc(st_bbox(geometry))), by = ORG_UNIT]
d4
d3 <- st_as_sf(d3)
plot(d3[d3$ORG_UNIT == "DQU",])
st_write(d3, "district_bnds.gpkg")

#BGC model and variable list
load("../Common_Files/BGC_RFresp.Rdata") ##load RF model
pred_vars <- BGC_RFresp[["forest"]][["independent.variable.names"]] ##required predictors

# bc boundary
bc <- vect(bc_bound())
bc <- project(bc, "EPSG:4326")

# DEM
dir <- paste("//objectstore2.nrs.bcgov/ffec/Climatologies/PRISM_BC/PRISM_dem/", sep="")
dem <- rast(paste(dir, "PRISM_dem.asc", sep=""))
dem <- aggregate(dem, fact=4)
dem <- mask(dem, bc)
dem <- trim(dem)
plot(dem)
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
pts <- fread("//objectstore2.nrs.bcgov/ffec/BGC_models/WNA_v13_50-200filtpts_15Nov.csv")
colnames(pts) <- c("id", "BGC", "lon", "lat", "elev") # rename column names to what climr expects
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
bgc.focal = "CWHxm_WA" # moderate to high novelty
bgc.pred <- predict(BGC_RFresp, data = clim.grid)[['predictions']]
clim.grid[,bgc_pred := bgc.pred]
vars <- c("id", "GCM", "SSP", "RUN", "PERIOD", "bgc_pred", as.vector(outer(c("Tmin", "Tmax", "PPT"), c("wt", "sp", "sm", "at"), paste, sep = "_")))
clim_final <- clim.grid[,..vars]
clim_final <- clim_final[!is.na(GCM),]
fwrite(clim_final,"Novelty_Predicted_Climate.csv")
readRenviron("./.Renviron")

library(RPostgres)
library(pool)
dbCon <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "cciss_spatial",
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432, 
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)
dbWriteTable(dbCon, "future_climate", clim_final, row.names = FALSE)
dbExecute(dbCon, "create index on future_climate(\"GCM\", \"PERIOD\", bgc_pred)")

test <- dbGetQuery(dbCon, "select * from future_climate where \"GCM\" = 'GISS-E2-1-G' and \"PERIOD\" = '2041_2060' and bgc_pred = 'CWHxm_WA'")

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


test_fut <- dbGetQuery(dbCon, "select * from future_climate where \"GCM\" = 'GISS-E2-1-G' and \"PERIOD\" = '2041_2060' and bgc_pred = 'CWHxm_WA'") |> as.data.table()
test_hist <- dbGetQuery(dbCon, "select * from historic_climate where bgc = 'CWHxm_WA'") |> as.data.table()
test_icv <- dbGetQuery(dbCon, "select * from historic_icv where bgc = 'CWHxm_WA'") |> as.data.table()

plot_analog_novelty(clim.target = test_fut, clim.analog = test_hist, clim.icv = test_icv)

zzz <- analog_novelty(clim.targets = clim_temp,
               clim.analogs = analog_temp,
               label.targets = bgc.pred[bgc.pred == bgc.focal],
               label.analogs = pts$BGC[pts$BGC == bgc.focal],
               vars = as.vector(outer(c("Tmin", "Tmax", "PPT"), c("wt", "sp", "sm", "at"), paste, sep = "_")),
               pcs = 3,
               analog.focal = bgc.focal,
               plotScree = FALSE,
               clim.icvs <- clim.icv.pts,
               label.icvs <- pts.mean$BGC[clim.icv.pts$id],
               plot3d = TRUE,
               plot3d.pcs=c(1,2,3)
)
