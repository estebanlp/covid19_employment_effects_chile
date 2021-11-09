#---------------------------------------#
#     Lockdown Data Construction
#---------------------------------------#


#municipality and polupation data
ciudades<-fread("01_Data/000_OtherSupportData/ciudades_INE.csv")
ciudades[nchar(`Código de Comuna`)==4,codigo_comuna:=paste0('0',`Código de Comuna`)]
ciudades[nchar(`Código de Comuna`)>4,codigo_comuna:=paste0(`Código de Comuna`)]
ciudades<-ciudades[,.(`Total de Personas`=sum(`Total de Personas`),`Total de Viviendas`=sum(`Total de Viviendas`),Tipo_Comuna="Urbana"),by=.(codigo_comuna, `Nombre de Comuna`)]

pop_comunas<-data.table(censo_2017_comunas,stringsAsFactors = F)
pet<-levels(pop_comunas$edad)
pet<-pet[!pet%in%c("0 a 4", "5 a 9","10 a 14")]
pop_comunas[,edad2:=as.character(edad)]
PET<-pop_comunas[edad2 %in% pet,.(PET=sum(poblacion)),by=.(codigo_comuna)]
pop_comunas<-pop_comunas[,.(poblacion=sum(poblacion)),by=.(codigo_comuna)]
pop_comunas<-merge(pop_comunas,PET,by="codigo_comuna")
pop_comunas<-merge(codigos_territoriales,pop_comunas,by="codigo_comuna")

pop_comunas<-data.table(merge(pop_comunas,ciudades[,.(codigo_comuna,`Total de Personas`,`Total de Viviendas`,Tipo_Comuna)],by="codigo_comuna",all.x = T),stringsAsFactors = F)
pop_comunas[is.na(Tipo_Comuna),Tipo_Comuna:="No Urbana"]
pop_comunas[,comuna_urbana:=ifelse(Tipo_Comuna=="Urbana",1,0)]

pop_comunas[,pop_region:=sum(poblacion),by=.(codigo_region)]
pop_comunas[,weight_pop:=poblacion/pop_region]

# Lockdowns
download.file("https://www.dropbox.com/s/iy0c1y78txcxx8b/lockdowns_raw_data.dta?dl=1",destfile = "01_Data/002_Lockdowns/")
lockdowns<-data.table(foreign::read.dta("01_Data/002_Lockdowns/lockdowns_raw_data.dta"),stringsAsFactors = F)
lockdowns[,codigo_region:=substr(trimws(codigo_comuna),1,2)]

lockdowns<-merge(lockdowns,pop_comunas[,.(codigo_comuna,weight_pop,poblacion)],by="codigo_comuna",all.x = T)
lockdowns[,pop_region:=sum(poblacion),by=.(codigo_region,ano_trimestre,mes_central)]
lockdowns[,lockdown:=sum(mes_cuarentena*poblacion)/sum(poblacion),by=.(ano_trimestre,mes_central,codigo_region)]
lockdowns[,lockdown_days:=sum(diasEn_cuarentena*poblacion*mes_cuarentena)/sum(poblacion)/30,by=.(ano_trimestre,mes_central,codigo_region)]
lockdowns2<-lockdowns[,.(mes_cuarentena_max=max(mes_cuarentena),diasEn_cuarentena_max=max(diasEn_cuarentena),mes_cuarentena_mean=mean(mes_cuarentena),diasEn_cuarentena_mean=mean(diasEn_cuarentena),lockdown_per=mean(lockdown),lockdown_days_per=mean(lockdown_days)),by=.(ano_trimestre,mes_central,codigo_region)]
