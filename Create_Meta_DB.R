library(data.table)
library(terra)
library(RSQLite)
library(RPostgres)
library(pool)
library(ggplot2)

dbCon <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "cciss_spatial",
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432, 
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)

dat <- dbGetQuery(dbCon, "select fp_code, prop1, prop2, prop3 from cciss_feas where cellid = 18955989 and edatope = 2 and spp_id = 3") |> as.data.table()
dat_h <- dbGetQuery(dbCon, "select suit from cciss_historic where cellid = 18955989 and edatope = 2 and spp_id = 3")[,1]

if(length(dat_h) == 0){
  res <- "X"
}else if(dat_h > 300) {
  res <- "X"
} else {
  res <- as.character(dat_h/100)
}

temp <- data.table(Period = 1961, Suitability = c("E1","E2","E3","EX"), value = 0)
temp[grep(res, Suitability),value := 100]

setnames(dat, c("Period","E1","E2","E3"))
dat[,EX := 100L - (E1 + E2 + E3)]
dat2 <- melt(dat, id.vars = "Period", variable.name = "Suitability")
dat2 <- rbind(dat2, temp)

palette.suit <-   c("E1" = "#006400", "E2" = "#1E90FF", "E3" = "#EEC900", "EX" = "#000000")
ggplot(dat2, aes(x = Period, y = value, color = Suitability)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = palette.suit, aesthetics = c("colour","fill")) +
  ylab("Percent of Votes") +
  theme_bw()

ggplot(dat2, aes(x = Period, y = value, color = Suitability, fill = Suitability)) +
  geom_bar(stat = "identity") +
  scale_color_manual(values = palette.suit, aesthetics = c("colour","fill")) +
  ylab("Percent of Votes") +
  theme_bw() +
  scale_x_continuous(breaks=seq(1961,2070,by = 20),labels = c("1961-1990","2001-2020 (obs)","2001-2020","2021-2040","2041-2060","2061-2080"))

temp <- c("Pl","Sx","Fd","Cw","Hw","Bl","At", "Ac", "Ep", "Yc", "Pw", "Ss", "Bg", "Lw")
cw_spp <- data.table(Spp = temp, spp_id = seq_along(temp))

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
