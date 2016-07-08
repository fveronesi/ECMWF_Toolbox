### KMeans Clustering Toolbox
##Author: Fabio Veronesi
tool_exec <- function(in_params, out_params)
{
	if (!requireNamespace("ncdf4", quietly = TRUE))
		install.packages("ncdf4")
	require(ncdf4)
  
	if (!requireNamespace("reshape2", quietly = TRUE))
		install.packages("reshape2")
	require(reshape2)
  
	if (!requireNamespace("sp", quietly = TRUE))
		install.packages("sp")
	require(sp)
  
	if (!requireNamespace("raster", quietly = TRUE))
		install.packages("raster")
	require(raster)
  
	if (!requireNamespace("rgdal", quietly = TRUE))
		install.packages("rgdal")
	require(rgdal)
  
  
	print("Time Averages of ECMWF Datasets")
	print("Author: Fabio Veronesi")
  
	source_nc = in_params[[1]]
	time_average = in_params[[2]]
	
	out_folder = out_params[[1]]
	
	dir.create(out_folder)
	#out_shape = out_params[[1]]
   
	### Read Data
	arc.progress_label("Reading the NetCDF Dataset...")
	print("Opening NC...")
	nc <- nc_open(source_nc)
	var <- names(nc$var)
	
	print(paste("NetCDF Variable: ",var))

	
	print("Creating Average Rasters ...")
	print("Please note that this process can be time-consuming.")
	
	
	for(VAR1 in var){
	print(paste("Executing Script for Variable: ", VAR1))
	var.nc <- brick(source_nc, varname=VAR1, layer="time")

	#Divide by Month
	TIME <- as.POSIXct(substr(var.nc@data@names, start=2, stop=20), format="%Y.%m.%d.%H.%M.%S")
	df <- data.frame(INDEX = 1:length(TIME), TIME=TIME)
	
	if(time_average=="Daily Averages"){
		days <- unique(format(TIME, "%d"))
		
		#LOOP
		for(DAY in days){
			subset <- df[format(df$TIME, "%d") == DAY,]
			sub.var <- var.nc[[subset$INDEX]]
			
			print(paste("Executing Average for Day: ",DAY))
			av.var <- calc(sub.var, fun=mean, filename=paste0(out_folder,"/",VAR1,"_Day",DAY,".tif"))
			print(paste("Raster for Day ",DAY," Ready in the Output Folder"))
		}
	} else if(time_average=="Monthly Averages") {
		months <- unique(format(TIME, "%m"))
		
		#LOOP
		for(MONTH in months){
			subset <- df[format(df$TIME, "%m") == MONTH,]
			sub.var <- var.nc[[subset$INDEX]]
			
			print(paste("Executing Average for Month: ",MONTH))
			av.var <- calc(sub.var, fun=mean, filename=paste0(out_folder,"/",VAR1,"_Month",MONTH,".tif"))
			print(paste("Raster for Month ",MONTH," Ready in the Output Folder"))
		}
	} else if(time_average=="Yearly Averages") {
		years <- unique(format(TIME, "%Y"))
		
		#LOOP
		for(YEAR in years){
			subset <- df[format(df$TIME, "%Y") == YEAR,]
			sub.var <- var.nc[[subset$INDEX]]
			
			print(paste("Executing Average for Year: ",YEAR))
			av.var <- calc(sub.var, fun=mean, filename=paste0(out_folder,"/",VAR1,"_Year",YEAR,".tif"))
			print(paste("Raster for Year ",YEAR," Ready in the Output Folder"))
		}	
	} else {
		hours <- unique(format(TIME, "%H"))
		
		#LOOP
		for(HOUR in hours){
			subset <- df[format(df$TIME, "%H") == HOUR,]
			sub.var <- var.nc[[subset$INDEX]]
			
			print(paste("Executing Average for Hour: ",HOUR))
			av.var <- calc(sub.var, fun=mean, filename=paste0(out_folder,"/",VAR1,"_Hour",HOUR,".tif"))
			print(paste("Raster for Hour ",HOUR," Ready in the Output Folder"))
		}	
	}
	
	}
}
