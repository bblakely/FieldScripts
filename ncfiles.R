library(ncdf4)
file<-nc_open("MaizeCon_2020_L2.nc")
names(file$var)
nc_close(file)
