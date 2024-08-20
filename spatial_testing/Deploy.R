library(analogsea)
Sys.setenv(DO_PAT="eae4166ed2fac0e3c41660fe26a009bb0176ab8bceeaf753faf5189f58a06520")

temp <- analogsea::droplets()
server <- temp$`shiny-server`
#analogsea::droplet_ssh(server,"rm -R /srv/shiny-server/spatial_test")
#analogsea::droplet_ssh(server, "mkdir /srv/shiny-server/spatial_test")
analogsea::droplet_upload(server, "./app.R", "/srv/shiny-server/spatial_test/app.R")
analogsea::droplet_upload(server, "./JS_Source.R", "/srv/shiny-server/spatial_test/JS_Source.R")
analogsea::droplet_upload(server, "./subzone_cols.csv", "/srv/shiny-server/spatial_test/subzone_cols.csv")
analogsea::droplet_upload(server, "./htmlwidgets", "/srv/shiny-server/spatial_test")
analogsea::droplet_upload(server, "./Raster_Templated.tif", "/srv/shiny-server/spatial_test")
analogsea::droplet_ssh(server, "chown -R shiny:shiny /srv/shiny-server")
analogsea::droplet_ssh(server, "systemctl restart shiny-server")
