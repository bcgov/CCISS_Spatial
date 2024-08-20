library(data.table)
library(terra)
library(RSQLite)

db <- dbConnect(RSQLite::SQLite(), "cciss_db.sqlite")
dbListTables(db)
dbGetQuery(db, "select * from bgc_preds limit 5")
#dbWriteTable(db, "cciss_feas", dat2, row.names = FALSE)


data_files <- list.files("./data/")
for(file in data_files){
  cat(file,"\n")
  dat <- fread(paste0("./data/",file))
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
dbDisconnect(db)

dat <- dbGetQuery(db, "select * from cciss_feas where fp_code = 2041 and edatope = 2 and species = 'Fd' and cellid = 20364")
