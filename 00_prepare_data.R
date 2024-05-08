#Prepare data

rm(list=ls()) #removing previous objects

#Download and loading required packages-----------
packages<-c('ggplot2','dplyr','reshape2','sf','raster','terra','spatialEco')

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

load('output_data/data.Rda')

# Importing shapefiles layers------------

muni_list<-read.csv('muni_list.csv')

##Setores Censitarios-----------
set <- read_sf("/home/gandra/Documents/SIG/IBGE/Censo2022/BR_Malha_Preliminar_2022.gpkg")
set<-set%>%filter(CD_REGIAO=='4'& is.na(NM_MUN)==F)%>%mutate(CD_MUN=as.integer(CD_MUN))
set<-set%>%filter(CD_MUN %in% muni_list$CD_MUN)
set<-st_transform(set,4326)

set<-set%>%mutate(pop=as.numeric(v0001),
                  dom=as.numeric(v0002),
                  area=as.numeric(AREA_KM2),
                  dens=as.numeric(v0001)/as.numeric(AREA_KM2))

set<-set%>%select(-AREA_KM2)

plot(set)



## Municipios----------
mun<-read_sf("/home/gandra/Dropbox/SIG/inundacao/SIG/muni.shp")
mun<-st_transform(mun,"+init=epsg:4326")
plot(mun)

mun$elev_min<-extract(dem,mun)

## Localidades RG----------
bairros_rg<-read_sf("input_data/bairros_rg.shp")
bairros_rg<-bairros_rg%>%dplyr::select(id, tipo, nome_zona, zona, localidade, area_km,elev_mean)
bairros_rg<-st_transform(bairros_rg,"+init=epsg:4326")




#DEM--------
dem<-raster('/home/gandra/Dropbox/SIG/inundacao/SIG/dem_rs.tif')
dem2<-raster('/home/gandra/Dropbox/SIG/inundacao/SIG/dem_rs2.tif')

dem2<-mask(dem2,mun)
plot(dem2)
##Extract elevation for polygons-----
mun$elev_mean<-extract(dem,mun,mean)

set$elev_mean<-as.numeric(extract(dem,set,mean))

# set$elev_min<-as.numeric(extract(dem,set,min))

# dem<-mask(dem,mun)
plot(dem)




##Salvando dados de input para a PIE
b<-as.data.frame(bairros_rg)
b$geometry<-NULL
write.csv(b,'output_data/bairros_elev.csv')


# Exportando-----------
# st_write(set, 'input_data/sig_inundacao.gpkg', layer='setores_censo',append = F)
# st_write(mun, 'input_data/sig_inundacao.gpkg', layer='municipios',append = F)
# st_write(bairros_rg, 'input_data/sig_inundacao.gpkg', layer='localidades_rg_elev',append = F)

st_write(bairros_rg, 'input_data/bairros_rg.shp',overwrite=T, append = F)


save(dem,dem2,set,mun,bairros_rg, file='output_data/data.Rda')


#Cenários----------
load('output_data/data.Rda')
plot(mun)
plot(set)
dem<-raster('/home/gandra/Dropbox/SIG/inundacao/SIG/dem_rs.tif')
dem<-mask(dem,mun)
plot(dem)

c17_r<-dem<=1.7


plot(c17_r)

c17_r<-as(c17_r, "SpatRaster")
plot(c17_r)
c17_pol<-as.polygons(c17_r,values=T,aggregate=T,na.rm=T)
plot(c17_pol)
c17_pol<-st_as_sf(c17_pol)
c17_pol<-c17_pol%>%filter(layer==1)

c17_pol2= st_cast(c17_pol,"POLYGON")
c17_pol=st_as_sf(c17_pol2)
c17_pol$area<-as.numeric(st_area(c17_pol))
c17_pol<-c17_pol%>%filter(area>=8000)



plot(c17_pol)


c25_r<-dem<=2.5
c25_r<-as(c25_r, "SpatRaster")
plot(c25_r)
c25_pol<-as.polygons(c25_r,values=T,aggregate=T,na.rm=T)
c25_pol<-st_as_sf(c25_pol)
c25_pol<-c25_pol%>%filter(layer==1)

c25_pol2= st_cast(c25_pol,"POLYGON")
c25_pol=st_as_sf(c25_pol2)
c25_pol$area<-as.numeric(st_area(c25_pol))
c25_pol<-c25_pol%>%filter(area>=8000)

plot(c25_pol)

# writeRaster(c17_r,'output_data/dem_c17.tif')
# writeRaster(c25_r,'output_data/dem_c25.tif')

## Selecionando setores
# set<-set%>%mutate(c17_count=as.numeric(extract(c17_r,set,sum)),
#                   c25_count=as.numeric(extract(c25_r,set,sum)))

set$area_km<-as.numeric(st_area(set)/1000000)
set17<-st_intersection(set,c17_pol)
set17$area<-as.numeric(st_area(set17))/1000000

set17$geom<-NULL

set17<-set17%>%group_by(CD_SETOR)%>%summarise(area17=sum(area,na.rm=T))

set<-set%>%left_join(set17%>%select(CD_SETOR,area17),by='CD_SETOR')

set<-set%>%mutate(c17p=area17*100/area_km)

save(c17_pol,c25_pol,file='output_data/cenarios.Rda')

###
load('output_data/cenarios.Rda')
set25<-st_intersection(set,c25_pol)
set25$area<-as.numeric(st_area(set25))/1000000

set25$geom<-NULL

set25<-set25%>%group_by(CD_SETOR)%>%summarise(area25=sum(area,na.rm=T))

set<-set%>%left_join(set25%>%select(CD_SETOR,area25),by='CD_SETOR')

set<-set%>%mutate(c25p=area25*100/area_km)

hist(set$c25p)


write_sf(set,'/home/gandra/Dropbox/SIG/inundacao/SIG/setoresf.shp')



save(dem,set,mun,bairros_rg,c17_pol,c25_pol, file='output_data/data.Rda')



# Rio Grande------------
##Bairros-------
bairros_rg$elev_mean<-as.numeric(bairros_rg$elev_mean)
# bairros_rg$elev_max<-as.numeric(bairros_rg$elev_max)
# bairros_rg$elev_min<-as.numeric(bairros_rg$elev_min)

bairros_rg$cenario17<-as.numeric(extract(dem<=1.7,bairros_rg,sum))


bairros_rg<-bairros_rg%>%mutate(area_m=as.numeric(st_area(bairros_rg)),
                                area_c17=cenario17*30*30,
                                c17p=area_c17*100/area_m)


bairros_rg$cenario25<-as.numeric(extract(dem<=2.5,bairros_rg,sum))

bairros_rg<-bairros_rg%>%mutate(area_c25=cenario25*30*30,
                                c25p=area_c25*100/area_m)

## Setores X Bairros----------
set_rg<-read_sf('/home/gandra/Documents/SIG/inundacao/sig_inundacao.gpkg',layer='setores_fase1_rg')

bai_rg<-set_rg%>%group_by(b_localidade)%>%summarise(dom=sum(dom,na.rm=T),
                                                    pop=sum(pop,na.rm=T))

plot(bai_rg)

write_sf(bai_rg,'/home/gandra/Documents/SIG/inundacao/draft/bairros_setores.shp')

