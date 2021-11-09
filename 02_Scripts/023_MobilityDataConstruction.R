#---------------------------------------#
#     Mobility Data Construction
#---------------------------------------#

mov<-fread("01_Data/000_OtherSupportData/IndiceDeMovilidad.csv")
mov[,Fecha:=as.Date(Fecha,format = "%Y-%m-%d")]
mov[,mes_central:=month(Fecha)]
mov[,day:=mday(Fecha)]

mov<-mov[!is.na(`Codigo region`),]
setkeyv(mov,cols = c("Codigo region","Codigo comuna","Fecha"))

baseline<-mov[,head(.SD,5L),by=.(`Codigo region`,`Codigo comuna`)] # gets the earliest case of each
baseline<-baseline[,.(value_ini=mean(IM,na.rm=T)),by=.(`Codigo region`,`Codigo comuna`)]


mov<-merge(mov[,.(Fecha,`Codigo region`,`Codigo comuna`,IM)],baseline,by=c("Codigo region","Codigo comuna"),all.x = T,sort = F)
mov[,value_ini:=nafill(value_ini,"locf"),by=.(`Codigo region`,`Codigo comuna`)]

mov[,mov_reduc:=((IM-value_ini)/value_ini)*100,by=.(`Codigo region`,`Codigo comuna`)]

mov[nchar(`Codigo region`)==1,codigo_region:=paste0("0",`Codigo region`)]
mov[nchar(`Codigo region`)>1,codigo_region:=paste0(`Codigo region`)]

mov[,mes_central:=month(Fecha)]

mov<-mov[,.(IM=mean(IM,na.rm=T),mov_reduc=mean(mov_reduc,na.rm = T)),by=.(codigo_region,mes_central)]
mov[,ano_trimestre:=2020]
