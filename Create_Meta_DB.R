library(data.table)
library(terra)
library(RSQLite)

db <- dbConnect(RSQLite::SQLite(), "cciss_db.sqlite")
#dbWriteTable(db, "cciss_feas", dat2, row.names = FALSE)
setwd("~/FFEC/CCISS_ShinyApp/")

data_files <- list.files("./bgc_data/")
for(file in data_files){
  cat(file,"\n")
  dat <- fread(paste0("./bgc_data/",file))
  dat[,fp_code := as.integer(substr(period, 1, 4))]
  dat2 <- dat[,.(cellid = as.integer(cellnum),
                 fp_code, 
                 bgc_pred,
                 bgc_prop)]
  
  dbWriteTable(db, "bgc_preds", dat2, row.names = FALSE, append = TRUE)
}
dbExecute(db, "create index bgc_idx on bgc_preds(fp_code,cellid)")
dbDisconnect(db)

data_files <- list.files("./cciss_feas/")
for(file in data_files){
  cat(file,"\n")
  dat <- fread(paste0("./cciss_feas/",file))
  dat[,fp_code := as.integer(substr(FuturePeriod, 1, 4))]
  dat2 <- dat[,.(cellid = as.integer(SiteRef),
                 fp_code, 
                 species = Spp,
                 curr = as.integer(Curr),
                 newsuit = Newsuit)]
  if(grepl("B2",file)){
    eda_code <- 1L
  }else if(grepl("C4",file)){
    eda_code <- 2L
  }else{
    eda_code <- 3L
  }
  dat2[,edatope := eda_code]
  dbWriteTable(db, "cciss_feas", dat2, row.names = FALSE, append = TRUE)
}
dbExecute(db, "create index app_idx on cciss_feas(fp_code,edatope,species,cellid)")


#dat <- dbGetQuery(db, "select * from cciss_feas where fp_code = 2041 and edatope = 2 and species = 'Fd' and cellid = 20364")
