README
The scripts in this folder were designed to run on a remote server to create the maps for the CCISS spatial app. As such, they may require modification for interactive use. All scripts will require a raster template (BC_DEM_200m.tif) and BGC attribution file

BGC MAPS
The `Raw_BGC_Maps.R` script contains the code necessary to predict and create maps of raw bgc predictions. 

CCISS Maps
First run `Predict_CCISS_BGC.R` to create BGC prediction summaries for each time period at each cell. Then run `Predict_SS_Feasibility.R` to perform the edatopic overlap and calculate the new feasibility values. Finally, run `Create_CCISS_Maps.R` to create the final raster maps. Note that each script saves its output as temporary csv files which are used by the subsequent script. 