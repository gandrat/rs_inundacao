#simplify data

library(sf)
# install.packages('nngeo')
library(nngeo)
rm(list=ls()) #removing previous objects
load('output_data/data_old.Rda')


rm(dem)
rm(bairros_rg)

plot(c17_pol)

# c17_pol<-st_simplify(c17_pol,dTolerance=1000)
# c25_pol<-st_simplify(c25_pol,dTolerance=1000)
# c50_pol<-st_simplify(c50_pol,dTolerance=1000)



c17_pol<-st_remove_holes(c17_pol)
c25_pol<-st_remove_holes(c25_pol)
c50_pol<-st_remove_holes(c50_pol)

# c17_pol<-st_as_sf(c17_pol)
# c25_pol<-st_as_sf(c25_pol)
# c50_pol<-st_as_sf(c50_pol)

# set<-st_simplify(set,preserveTopology = T,dTolerance=500)
# set<-st_as_sf(set)
save(c17_pol,c25_pol,c50_pol,set,mun,file='output_data/data.Rda')


plot(set)
