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



##Extract elevation for polygons-----
mun$elev_mean<-extract(dem,mun,mean)
set$elev_mean<-as.numeric(extract(dem,set,mean))






# Exportando-----------
# st_write(set, 'input_data/sig_inundacao.gpkg', layer='setores_censo',append = F)
# st_write(mun, 'input_data/sig_inundacao.gpkg', layer='municipios',append = F)
# st_write(bairros_rg, 'input_data/sig_inundacao.gpkg', layer='localidades_rg_elev',append = F)

st_write(bairros_rg, 'input_data/bairros_rg.shp',overwrite=T, append = F)


##Salvando RDA-------
save(dem,set,mun,bairros_rg, file='output_data/data.Rda')


#Cenários----------
load('output_data/data.Rda')
plot(mun)
plot(set)
dem<-raster('/home/gandra/Dropbox/SIG/inundacao/SIG/dem_rs.tif')
dem<-mask(dem,mun)
plot(dem)

##Cenario 1--------
cr<-dem<=1.7

plot(cr)

cr<-as(cr, "SpatRaster")
plot(cr)
cpol<-as.polygons(cr,values=T,aggregate=T,na.rm=T)
plot(cpol)
cpol<-st_as_sf(cpol)
cpol<-cpol%>%filter(layer==1)

cpol2= st_cast(cpol,"POLYGON")
cpol=st_as_sf(cpol2)
cpol$area<-as.numeric(st_area(cpol))
cpol<-cpol%>%filter(area>=8000)

plot(cpol)


set17<-st_intersection(set,cpol)
set17$area<-as.numeric(st_area(set17))/1000000

set17$geom<-NULL

set17<-set17%>%group_by(CD_SETOR)%>%summarise(area17=sum(area,na.rm=T))

set<-set%>%left_join(set17%>%select(CD_SETOR,area17),by='CD_SETOR')

set<-set%>%mutate(c17p=area17*100/area)

##Cenario 2------
cr<-dem<=2.5

plot(cr)

cr<-as(cr, "SpatRaster")
plot(cr)
cpol<-as.polygons(cr,values=T,aggregate=T,na.rm=T)
plot(cpol)
cpol<-st_as_sf(cpol)
cpol<-cpol%>%filter(layer==1)

cpol2= st_cast(cpol,"POLYGON")
cpol=st_as_sf(cpol2)
cpol$area<-as.numeric(st_area(cpol))
cpol<-cpol%>%filter(area>=8000)

plot(cpol)

set25<-st_intersection(set,c25_pol)
set25$area<-as.numeric(st_area(set25))/1000000

set25$geom<-NULL

set25<-set25%>%group_by(CD_SETOR)%>%summarise(area25=sum(area,na.rm=T))

set<-set%>%left_join(set25%>%select(CD_SETOR,area25),by='CD_SETOR')

set<-set%>%mutate(c25p=area25*100/area)

##Cenario 3--------
cr<-dem<=5

plot(cr)

cr<-as(cr, "SpatRaster")
plot(cr)
cpol<-as.polygons(cr,values=T,aggregate=T,na.rm=T)
plot(cpol)
cpol<-st_as_sf(cpol)
cpol<-cpol%>%filter(layer==1)

cpol2= st_cast(cpol,"POLYGON")
cpol=st_as_sf(cpol2)
cpol$area<-as.numeric(st_area(cpol))
cpol<-cpol%>%filter(area>=8000)
plot(cpol)
c50_pol<-cpol
set50<-st_intersection(set,cpol)
set50$area<-as.numeric(st_area(set50))/1000000

set50$geom<-NULL

set50<-set50%>%group_by(CD_SETOR)%>%summarise(area50=sum(area,na.rm=T))

set<-set%>%left_join(set50%>%dplyr::select(CD_SETOR,area50),by='CD_SETOR')

set<-set%>%mutate(c50p=area50*100/area)

save(c17_pol,c25_pol,c50_pol,file='output_data/cenarios.Rda')

## Organizando Setores--------


set<-set%>%dplyr::select(CD_SETOR,
                  CD_MUN,
                  NM_MUN,
                  pop,
                  dom,
                  area,
                  dens,
                  area17,
                  area25,
                  area50,
                  c17p,
                  c25p,
                  c50p)%>%mutate(dens=pop/area)

hist(set$c25p)


write_sf(set,'/home/gandra/Documents/SIG/inundacao/draft/setoresf2.shp',overwrite=T)
write_sf(c17_pol,'/home/gandra/Documents/SIG/inundacao/draft/cenario17.shp',overwrite=T)

load('output_data/cenarios.Rda')

save(dem,set,mun,bairros_rg,c17_pol,c25_pol, c50_pol,file='output_data/data.Rda')



# Rio Grande------------

b<-as.data.frame(bairros_rg)
b$geometry<-NULL
write.csv(b,'output_data/bairros_elev.csv')
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

