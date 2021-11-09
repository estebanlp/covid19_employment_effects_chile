#---------------------------------------#
#     LPE Data Construction
#---------------------------------------#
download.file(url = "https://www.dropbox.com/s/7woknkjeh1xxhn6/BaseConsolidada_new.csv?dl=0",destfile = "01_Data/003_LPE/")
LPE<-fread("01_Data/003_LPE/BaseConsolidada_new.csv",encoding = "UTF-8")

#data has month midpoints that are useless, only keeping:
# 30/04/2020 (April)   31/05/2020 (May)   28/06/2020 (June)   07/08/2020(July) 31/08/2020 (August) 
dates<-c("2020-04-30","2020-05-31","2020-06-28","2020-08-07","2020-08-31","2020-09-30")
LPE[,time:=as.character(time)]
LPE<-LPE[ time %chin% dates,]
LPE[time=="2020-08-07",time:="2020-07-31"]

# creating some variables
LPE[,time:=as.Date(time)]
LPE[,day:=mday(time)]
LPE[,mes_central:=month(time)]
LPE[,ano_trimestre:=year(time)]
LPE[,codigo_region:="13"]
LPE[`Región` == "Región de Tarapacá",codigo_region:="01"]
LPE[`Región` == "Región de Antofagasta",codigo_region:="02"]
LPE[`Región` == "Región de Atacama",codigo_region:="03"]
LPE[`Región` == "Región de Coquimbo",codigo_region:="04"]
LPE[`Región` == "Región de Valparaíso",codigo_region:="05"]
LPE[`Región` == "Región del Libertador Gral. Bernardo O’Higgins",codigo_region:="06"]
LPE[`Región` == "Región del Maule",codigo_region:="07"]
LPE[`Región` == "Región del Biobío",codigo_region:="08"]
LPE[`Región` == "Región de La Araucanía",codigo_region:="09"]
LPE[`Región` == "Región de Los Lagos",codigo_region:="10"]
LPE[`Región` == "Región Aisén del Gral.Carlos Ibáñez del Campo",codigo_region:="11"]
LPE[`Región` == "Región de Magallanes y de la Antártica Chilena",codigo_region:="12"]
LPE[`Región` == "Región de Los Ríos",codigo_region:="14"]
LPE[`Región` == "Región de Arica y Parinacota",codigo_region:="15"]
LPE[`Región` == "Región de Ñuble",codigo_region:="16"]

LPE2<-LPE[,.(ads_total_stock=mean(Ads_Total_Stock,na.rm=T),
             ads_total_acumulada=mean(Ads_Total_Acumulada,na.rm=T),
             sales_level=mean(`Tramo según ventas`,na.rm=T),
             total_dependent_workers= mean(`Número de trabajadores dependientes informados`,na.rm=T),
             total_trab_region=sum(`Número de trabajadores dependientes informados`,na.rm=T),
             per_LPE_region=sum(Ads_Total_Stock,na.rm = T)/sum(`Número de trabajadores dependientes informados`,na.rm=T)), by=.(codigo_region,mes_central)]

LPE2[,ano_trimestre:=2020]

