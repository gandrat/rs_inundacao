#Prepare data


#Download and loading required packages-----------
packages<-c('ggplot2','dplyr','reshape2','RPostgreSQL','sf','lubridate','forcats')

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

# Importing shapefiles layers
area_estudo <- read_sf("input_data/pol_monitoramento_praia.shp")

plot(area_estudo)
#Setting connection parameters-----------
con = dbConnect(PostgreSQL(), 
                dbname = "nema", 
                host="200.132.11.22", 
                user='mamiferos',
                password="Otaria_2022", 
                port=1305)

#Encalhes---------
enc <- dbGetQuery(con, "select eg.idenc, eg.coord_correta, eg.data, estacoes.estacao, sp.tipo as grupo, eg.especie, e.sexo, e.g_vivo, e.comprimento, s.direcao, st_x(geom) as lon, st_y(geom) as lat, e.obs 
from encalhesg eg join estacoes on (extract('Month' from data)=mes) join praia.encalhes e using (idenc) join praia.saidas s using(idsaida) join sp using(id_sp)");
dbGetQuery(con, "select * from sp")
enc$ano<-as.integer(format(enc$data,format='%Y'))
enc$mes<-as.integer(format(enc$data,format='%m'))

# enc$estacao<-factor(enc$estacao, levels=c('Verão','Outono','Inverno','Primavera'))


sp<-enc%>%group_by(especie)%>%summarise(n_enc=n())

# enc<-enc%>%inner_join(sp, by='especie')
# enc<-enc%>%mutate(especie=fct_reorder(as.factor(especie),enc$n_enc))

#Saidas------------
saidas <- dbGetQuery(con, "select s.*, e.estacao from saidas s join estacoes e on (extract('Month' from data)=mes)");
saidas$ano<-as.integer(format(saidas$data, format="%Y"))
saidas$mes<-as.integer(format(saidas$data, format="%m"))
# saidas$estacao<-factor(saidas$estacao, levels=c('Verão','Outono','Inverno','Primavera'))

saidasano<-saidas%>%group_by(ano,estacao)%>%summarise(n=n(),km_total=sum(km_total,na.rm=T))
saidasmes<-saidas%>%group_by(mes,estacao)%>%summarise(n=n(),km_total=sum(km_total,na.rm=T))


#Pesquisadores------------
pesq <- dbGetQuery(con, "select * from pesq_km_ano")

pesqt<-pesq%>%group_by(nome)%>%summarise(km_total=sum(km))
pesqt<-pesqt%>%arrange(-km_total)%>%slice(1:20)
pesq<-pesq%>%inner_join(pesqt)

pesq$nome<-fct_reorder(pesq$nome, pesq$km_total)

# I10----------
i10saida=dbGetQuery(con, "select i.*, e.estacao, sp.tipo as grupo from praia.i10saida i join estacoes e on (extract('Month' from data)=mes) join sp using (especie)")
max(i10saida$data)  
# i10saida$estacao<-factor(i10_saida$estacao, levels=c('Verão','Outono','Inverno','Primavera'))
i10saida$ano<-as.integer(format(i10saida$data, format="%Y"))
i10saida$mes<-as.integer(format(i10saida$data, format="%m"))

##Salvando dados de input para a PIE
save(enc,saidas,saidasano,saidasmes,pesq,i10saida,area_estudo, file='input_data.Rda')








