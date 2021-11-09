library(readstata13)

download.file(url = "https://www.dropbox.com/s/9foq72p4q8xr1ah/final_data.dta?dl=1",destfile = "01_Data/")
finalD<-data.table(read.dta13("01 Data/final_data.dta"))


vars_1<-c("Ocupados","Desocupados","Inactivos","Ocupados_Mujeres","Ocupados_Hombres","Formales","Informales","Pet_reg","flujo_ocu_1","flujo_ocu_Pet_1","initial_shock","rest_pandemia","mes_cuarentena_max","diasEn_cuarentena_max","mes_cuarentena_mean_max3_1st","mes_cuarentena_mean_max3_REST")


A<-finalD[pandemia==0,.(Min=lapply(.SD,min,na.rm=T),Mean=lapply(.SD,mean,na.rm=T),Max=lapply(.SD,max,na.rm=T),`T`=.N/16,`R`=.N/(.N/16),`Obs`=.N),.SDcols=vars_1]# pre-pandemic

B<-finalD[pandemia==1 &(mes_central%in%3:4),.(Min=lapply(.SD,min,na.rm=T),Mean=lapply(.SD,mean,na.rm=T),Max=lapply(.SD,max,na.rm=T),`T`=.N/16,`R`=.N/(.N/16),`Obs`=.N),.SDcols=vars_1]# initial-shock

C<-finalD[pandemia==1 &(mes_central%in%5:9),.(Min=lapply(.SD,min,na.rm=T),Mean=lapply(.SD,mean,na.rm=T),Max=lapply(.SD,max,na.rm=T),`T`=.N/16,`R`=.N/(.N/16),`Obs`=.N),.SDcols=vars_1]# after-shock
DescStatsTable<-cbind(vars_1,A,B,C)

fwrite(DescStatsTable,"DescStatsTable.csv")


A1<-finalD[pandemia==0 & zona_centro_sur==1,.(Min=lapply(.SD,min,na.rm=T),Mean=lapply(.SD,mean,na.rm=T),Max=lapply(.SD,max,na.rm=T),`T`=.N/7,`R`=.N/(.N/7),`Obs`=.N),.SDcols=vars_1]# pre-pandemic

B1<-finalD[pandemia==1 &(mes_central%in%3:4) & zona_centro_sur==1,.(Min=lapply(.SD,min,na.rm=T),Mean=lapply(.SD,mean,na.rm=T),Max=lapply(.SD,max,na.rm=T),`T`=.N/7,`R`=.N/(.N/7),`Obs`=.N),.SDcols=vars_1]# initial-shock

C1<-finalD[pandemia==1 &(mes_central%in%5:9) & zona_centro_sur==1,.(Min=lapply(.SD,min,na.rm=T),Mean=lapply(.SD,mean,na.rm=T),Max=lapply(.SD,max,na.rm=T),`T`=.N/7,`R`=.N/(.N/7),`Obs`=.N),.SDcols=vars_1]# after-shock

DescStatsTable_Center<-cbind(vars_1,A1,B1,C1)

fwrite(DescStatsTable_Center,"DescStatsTable_Center.csv")

A2<-finalD[pandemia==0 & zona_centro_sur==0,.(Min=lapply(.SD,min,na.rm=T),Mean=lapply(.SD,mean,na.rm=T),Max=lapply(.SD,max,na.rm=T),`T`=.N/9,`R`=.N/(.N/9),`Obs`=.N),.SDcols=vars_1]# pre-pandemic

B2<-finalD[pandemia==1 &(mes_central%in%3:4) & zona_centro_sur==0,.(Min=lapply(.SD,min,na.rm=T),Mean=lapply(.SD,mean,na.rm=T),Max=lapply(.SD,max,na.rm=T),`T`=.N/9,`R`=.N/(.N/9),`Obs`=.N),.SDcols=vars_1]# initial-shock

C2<-finalD[pandemia==1 &(mes_central%in%5:9) & zona_centro_sur==0,.(Min=lapply(.SD,min,na.rm=T),Mean=lapply(.SD,mean,na.rm=T),Max=lapply(.SD,max,na.rm=T),`T`=.N/9,`R`=.N/(.N/9),`Obs`=.N),.SDcols=vars_1]# after-shock

DescStatsTable_Rest<-cbind(vars_1,A2,B2,C2)

fwrite(DescStatsTable_Rest,"DescStatsTable_Rest.csv")



# Figure 1: Regional trends of occupation flows and lockdowns
a<-c(paste0("0",1:9),10:12)
finalD[,date:=as.Date(paste0(ano_trimestre,"-",a[mes_central],"-","01"))]

finalD[,`Región`:=factor(region,levels = c(15,1,2,3,4,5,13,6,7,16,8,9,14,10,11,12),labels = c("(R15) - Arica","(R1) - Tarapacá","(R2) - Antofagasta","(R3) - Atacama","(R4) - Coquimbo","(R5) - Valparaíso","(R13) - Metropolitana","(R6) - O'Higgings","(R7) - Maule","(R16) - Ñuble","(R8) - Bío-Bío","(R9) - Araucanía","(R14) - Los Ríos","(R10) - Los Lagos","(R11) - Aysén","(R12) - Magallanes"))]
cuarentenas<-finalD[mes_cuarentena_max==1 & mes_central<10,.(date,diasEn_cuarentena_max),by=.(region, `Región`)]

F1<-ggplot(data=finalD,aes(x=date,group=region))+
  geom_vline(aes(xintercept=date,colour="Lockdowns"),alpha=0.5,lty=2,cuarentenas)+
    geom_hline(aes(yintercept=0),col='coral1')+
    geom_line(data = finalD[!(ano_trimestre==2020 & mes_central>=2)],aes(y =flujo_ocu_Pet_1))+
  geom_point(data = finalD[!(ano_trimestre==2020 & mes_central>=3)],aes(y =flujo_ocu_Pet_1,col="Pre-Pandemic"))+
  geom_line(data = finalD[pandemia==1 &(mes_central%in%2:9)],aes(y =flujo_ocu_Pet_1,group=region))+
  geom_point(data = finalD[pandemia==1 &(mes_central%in%4:9)],aes(y =flujo_ocu_Pet_1,group=region,col="Aftershock"))+
  geom_line(data = finalD[(ano_trimestre==2020 & mes_central>=1) &(mes_central%in%1:4)],aes(y =flujo_ocu_Pet_1,group=region))+
  geom_point(data = finalD[(ano_trimestre==2020 & mes_central>=2) &(mes_central%in%2:3)],aes(y =flujo_ocu_Pet_1,group=region,col="Inital Shock"))+
geom_vline(aes(xintercept=as.integer(finalD[["date"]][2])),col="grey50")+
  geom_text(aes(x=finalD[["date"]][2],y=5,label="2019"),col="grey50",nudge_x = 50,size=3)+
  scale_x_date(date_labels = "%b",date_breaks = "months")+
  geom_vline(aes(xintercept=as.Date("2020-01-01")),col="grey50")+
  geom_text(aes(x=as.Date("2020-01-01"),y=5,label="2020"),col="grey50",nudge_x = 50,size=3)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90,size = 8),legend.position = "top",legend.title = element_blank())+ labs(x=NULL,y="Month to month flow of occupied/WAP \n(Dependent variable)")+
  facet_wrap(~`Región`)
F1  
ggsave(F1,filename = "03_Figures/Figure1.pdf",width = 11.5,height = 8,units = "in",dpi = 300)

plotly::ggplotly(F1)





library(readstata13)

#---- Pandemic (cases) by region

cases<-fread("../NewCasesbyRegion.csv",encoding = 'UTF-8')

cases[,month:=month(as.Date(Date))]
cases[,year:=year(as.Date(Date))]

names(cases)[2:18]<-paste0("region_", names(cases)[2:18])
cases<-reshape(cases,varying = names(cases)[2:18],direction = "long",sep = "_")

cases2<-cases[,.(monthly_cases=sum(region)),by=.(time,year,month)]

names(cases)[1]<-"region_name"

cases<-cases[time!="Total",]

names(cases)[4]<-"Region"

cases[Region=="Tarapacá",reg_code:=1]
cases[Region=="Antofagasta",reg_code:=2]
cases[Region=="Atacama",reg_code:=3]
cases[Region=="Coquimbo",reg_code:=4]
cases[Region=="Valparaíso",reg_code:=5]
cases[Region=="O’Higgins",reg_code:=6]
cases[Region=="Maule",reg_code:=7]
cases[Region=="Biobío",reg_code:=8]
cases[Region=="Araucanía",reg_code:=9]
cases[Region=="Los Lagos",reg_code:=10]
cases[Region=="Aysén",reg_code:=11]
cases[Region=="Magallanes",reg_code:=12]
cases[Region=="Metropolitana",reg_code:=13]
cases[Region=="Los Ríos",reg_code:=14]
cases[Region=="Arica y Parinacota",reg_code:=15]
cases[Region=="Ñuble",reg_code:=16]

cases[,`Región`:=factor(reg_code,levels = c(15,1,2,3,4,5,13,6,7,16,8,9,14,10,11,12),labels = c("(R15) - Arica","(R1) - Tarapacá","(R2) - Antofagasta","(R3) - Atacama","(R4) - Coquimbo","(R5) - Valparaíso","(R13) - Metropolitana","(R6) - O'Higgings","(R7) - Maule","(R16) - Ñuble","(R8) - Bío-Bío","(R9) - Araucanía","(R14) - Los Ríos","(R10) - Los Lagos","(R11) - Aysén","(R12) - Magallanes"))]

a<-c(paste0("0",1:9),10:12)
cases[,date:=as.Date(paste0(year,"-",a[month],"-","01"))]

ggplot(cases[year==2020 &month<=10,],aes(y=`Región`,x=Date))+
  geom_raster(aes(fill=region))+
  scale_fill_gradient(low="white", high="red")+
  scale_x_date(date_labels = "%b",date_breaks = "months")+
  geom_vline(aes(xintercept=as.Date("2020-01-01")),col="grey50")+
  geom_text(aes(x=as.Date("2020-01-01"),y=1,label="2020"),col="grey50",nudge_x = 50,size=3)+
  theme_minimal()
  



#---- Pandemic (incidence:= cases/pop) by region and date

incidence<-fread("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto18/TasaDeIncidencia_std.csv",encoding = "UTF-8")

head(incidence)
incidence[,date:=as.Date(Fecha)]
incidence[,year:=year(date)]
incidence[,month:=month(date)]

incidence<-incidence[Comuna!="Total"]

incidence[,cases:=round((`Tasa de incidencia`*Poblacion)/100000,0)]

ggplot(incidence[year==2020 &month<=10,],aes(y=Comuna,x=date))+
  geom_raster(aes(fill=`Tasa de incidencia`))+
  scale_fill_gradient(low="white", high="red")+
  theme_minimal()

pop<-incidence[Fecha=="2020-03-30",.(Pop=sum(Poblacion)),by=.(Region)]
pop[Region=="Tarapaca",reg_code:=1]
pop[Region=="Antofagasta",reg_code:=2]
pop[Region=="Atacama",reg_code:=3]
pop[Region=="Coquimbo",reg_code:=4]
pop[Region=="Valparaiso",reg_code:=5]
pop[Region=="Del Libertador General Bernardo O’Higgins",reg_code:=6]
pop[Region=="Maule",reg_code:=7]
pop[Region=="Biobio",reg_code:=8]
pop[Region=="La Araucania",reg_code:=9]
pop[Region=="Los Lagos",reg_code:=10]
pop[Region=="Aysen",reg_code:=11]
pop[Region=="Magallanes y la Antartica",reg_code:=12]
pop[Region=="Metropolitana",reg_code:=13]
pop[Region=="Los Rios",reg_code:=14]
pop[Region=="Arica y Parinacota",reg_code:=15]
pop[Region=="Nuble",reg_code:=16]


cases<-merge(cases,pop[,.(reg_code,Pop)],by="reg_code")


cases[,`Incidence Rate`:=((region/Pop)*100000)]


F2<-ggplot(cases[year==2020 &month<=10,],aes(y=`Región`,x=Date))+
  geom_raster(aes(fill=`Incidence Rate`))+
  scale_fill_gradient(low="white", high="black")+
  scale_x_date(date_labels = "%b",date_breaks = "months")+
  geom_vline(aes(xintercept=as.Date("2020-01-01")),col="grey50")+
  geom_text(aes(x=as.Date("2020-01-01"),y=1,label="2020"),col="grey50",nudge_x = 50,size=3)+
  theme_minimal()+theme(legend.position = 'top',legend.justification = "left")+labs(caption = expression("Tasa de Incidencia"[rt] ==("Total New Cases"[rt]/"Population"[r])*"x100,000"))



finalD[,.(region,Región,date,lockdown_days_per_1st,lockdown_days_per_REST)]

f2_final<-merge(cases,finalD[,.(reg_code=region,year=ano_trimestre,month=mes_central,mes_cuarentena_max,`Lockdown \nIntensity`=lockdown_days_per)],by = c('reg_code','year','month'))


F2<-ggplot(f2_final[year==2020 &month<=10,],aes(y=`Región`,x=Date))+
  theme_classic()+
  geom_tile(aes(colour=`Lockdown \nIntensity`,fill=`Incidence Rate`),lty=1)+
  scale_fill_viridis_c(direction = -1)+
  #scale_fill_gradient(low = 'lightgrey',high = 'navyblue')+
  scale_color_gradient(low = NA,high = 'black')+
  scale_x_date(date_labels = "%b",date_breaks = "months")+
  geom_text(aes(x=as.Date("2020-01-01"),y=1,label="2020"),col="grey50",nudge_x = 50,size=3)+
  theme(legend.position = 'top',legend.justification = "left")+labs(caption = expression("Incidence Rate"[rt] ==("Total New Cases"[rt]/"Population"[r])*"x100,000"))

ggsave(F2,filename = "03_Figures/Figure2b.pdf",width = 11.5,height = 8,units = "in",dpi = 300)



#---- Firm size by region and employment


firm_size<-finalD[,.(reg_code=region,ano_trimestre,mes_central,Small=Small/Labor_Force,Medium=Medium/Labor_Force,Large=Large/Labor_Force)]
firm_size[,`N.A.`:=1-(Small+Medium+Large)]

firm_size<-melt(firm_size,id.vars = c('reg_code','ano_trimestre','mes_central'))

firm_size[,Region:=factor(reg_code,levels = c(12,11,10,14,9,8,16,7,6,13,5,4,3,2,1,15),labels = c("(R12) - Magallanes","(R11) - Aysén","(R10) - Los Lagos","(R14) - Los Ríos","(R9) - Araucanía","(R8) - Bío-Bío","(R16) - Ñuble","(R7) - Maule","(R6) - O'Higgings","(R13) - Metropolitana","(R5) - Valparaíso","(R4) - Coquimbo","(R3) - Atacama","(R2) - Antofagasta","(R1) - Tarapacá","(R15) - Arica"))]
firm_size[,`Firm size`:=as.factor(variable)]

ggplot(firm_size[ano_trimestre==2019,.(`Share Employed`=mean(value)),by=.(Region,`Firm size`)],aes(x=Region,fill=`Firm size`,y=`Share Employed`))+
  geom_bar(position='fill',stat = 'identity')+
  coord_flip()+
 theme_classic()+
  theme(legend.position = "top",legend.justification = 'left') + labs(caption = "Share is an average of the pre-pandemic monthly values (2019)")
  

# LPE intensity by region

LPE<-finalD[(ano_trimestre==2020 & mes_central>=4),.(reg_code=region,date,`LPE Intensity`=per_LPE_region)]
LPE[,Region:=factor(reg_code,levels = c(12,11,10,14,9,8,16,7,6,13,5,4,3,2,1,15),labels = c("(R12) - Magallanes","(R11) - Aysén","(R10) - Los Lagos","(R14) - Los Ríos","(R9) - Araucanía","(R8) - Bío-Bío","(R16) - Ñuble","(R7) - Maule","(R6) - O'Higgings","(R13) - Metropolitana","(R5) - Valparaíso","(R4) - Coquimbo","(R3) - Atacama","(R2) - Antofagasta","(R1) - Tarapacá","(R15) - Arica"))]

F3b<-ggplot(LPE,aes(x=date,y=Region))+
  geom_tile(aes(fill=`LPE Intensity`))+
  scale_fill_gradient(low=NA, high="black")+
  theme_classic()+
  theme(legend.position = "top",legend.justification = 'left') + labs(caption = "LPE Intenstity is the share of the total labor force subscribed in the LPE")

ggsave(F3b,filename = "03_Figures/Figure3b.pdf",width = 8,height = 11.5,units = "in",dpi = 300)



#---- Other graphs: legacy

library(data.table)
library(ggplot2)
emp_data<-fread(file = "01_Data/paper_data.csv")
str(emp_data)


emp_data[,Region:=factor(region,levels = c(12,11,10,14,9,8,16,7,6,13,5,4,3,2,1,15),labels = c("(R12) - Magallanes","(R11) - Aysén","(R10) - Los Lagos","(R14) - Los Ríos","(R9) - Araucanía","(R8) - Bío-Bío","(R16) - Ñuble","(R7) - Maule","(R6) - O'Higgings","(R13) - Metropolitana","(R5) - Valparaíso","(R4) - Coquimbo","(R3) - Atacama","(R2) - Antofagasta","(R1) - Tarapacá","(R15) - Arica"))]

ggplot(emp_data,aes())



emp_data[,per_occup_traditional:=(Occupied_Regular + Occupied_Irregular)/(WorkingAgePop)]
emp_data[,per_occup_absent:=(Occupied_Absent)/(WorkingAgePop)]
emp_data[,per_unoccup:=(Unoccupied/WorkingAgePop)]
emp_data[,per_inactive:=(Inactive/WorkingAgePop)]

DD<-melt.data.table(emp_data, id.vars = c("region","ano_trimestre","mes_central"),measure.vars = c("per_occup_traditional","per_occup_absent","per_unoccup","per_inactive"))

ggplot(DD[ano_trimestre==2020,], aes(x=mes_central,y=value,fill=variable))+geom_area() +facet_wrap(~region)


emp_data[,Occupied_Tradicional:=Occupied_Regular + Occupied_Irregular]

emp_data[,var_occup_traditional:=Occupied_Tradicional-shift(Occupied_Tradicional,n = 1,type = 'lag')]
emp_data[,varP_occup_traditional:=(Occupied_Tradicional/shift(Occupied_Tradicional,n = 1,type = 'lag'))-1]

emp_data[,var_occup_absent:=Occupied_Absent-shift(Occupied_Absent,n = 1,type = 'lag')]
emp_data[,varP_occup_absent:=(Occupied_Absent/shift(Occupied_Absent,n = 1,type = 'lag'))-1]

emp_data[,var_inactive:=Inactive-shift(Inactive,n = 1,type = 'lag')]
emp_data[,varP_inactive:=(Inactive/shift(Inactive,n = 1,type = 'lag'))-1]

ggplot(emp_data[ano_trimestre==2020 ],aes(x=varP_occup_absent,y=varP_inactive,group=mes_central, color=mes_central))+geom_point()+facet_wrap(~region)

ggplot(emp_data[ano_trimestre==2020, ])+
  geom_hline(aes(yintercept=0,col='black'))+
  geom_line(aes(x=mes_central,y=emp_data[ano_trimestre==2020, ]$varP_inactive,color='Inactivos'))+
  geom_line(aes(x=mes_central,y=emp_data[ano_trimestre==2020, ]$varP_occup_absent,color='Ausentes'))+
  facet_wrap(~region,scales = 'free_y')

ggplot(emp_data[ano_trimestre==2020, ])+
  geom_hline(aes(yintercept=0,col='black'))+
  #geom_line(aes(x=mes_central,y=emp_data[ano_trimestre==2020, ]$var_inactive,color='Inactivos'))+
  geom_line(aes(x=mes_central,y=emp_data[ano_trimestre==2020, ]$var_occup_absent,color='Ausentes'))+
  facet_wrap(~region,scales = 'free_y')


