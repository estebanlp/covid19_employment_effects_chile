#---------------------------------------#
#     Employment Data Construction
#---------------------------------------#

#### Loading municipality and regional names and codes:
codigos <- data.table(read_xlsx("01_Data/000_OtherSupportData/codigo comunas a 2018-08-01.xlsx"))

#### Loading raw employment data:
download.file("https://www.dropbox.com/s/06gx3ikztlgwcs0/EmploymentRawData.zip?dl=1",destfile = "01_Data/001_Employment/")
unzip(zipfile = "01_Data/001_Employment/EmploymentRawData.zip",exdir = "01_Data/001_Employment/")

path<-getwd()
month1 <- c(paste0("0",1:9),10:12)
trim <- c("def","efm","fma","mam","amj","mjj","jja","jas","aso","son","ond","nde")
TABLA_r <- NULL

for(i in 2017:2020){
  for(j in 1:12){
    if(!file.exists(paste0(path,"/01_Data/001_Employment/ene-",i,"-",month1[j],".dta"))){
      next  
    }
    print(paste0("We are in: ",trim[j],i))
    dat<-data.table(read.dta13(paste0(path,"/01_Data/001_Employment/ene-",i,"-",month1[j],".dta"), convert.factors = F,encoding = 'latin1'))
    dat<-dat[edad >= 15,]
    dat[b15_1 %in% c(1,2), tamano:="Small"]
    dat[b15_1 %in% c(3), tamano:="Medium"]
    dat[b15_1 %in% c(4,5), tamano:="Large"]
    #dat[is.na(b15_1), tamano:="NA"]
    T0<-dat[,.N,by=.(ano_trimestre,mes_central,region)]
    T1<-dat[,.(Total=sum(fact_cal,na.rm = T)),by=.(ano_trimestre,mes_central,region,activ,ocup_form,sexo,b14_rev4cl_caenes,tamano,cae_general,cae_especifico)]
    T1[b14_rev4cl_caenes %in% c(1,2), sectorO10:="Naturales_Resources"]
    T1[b14_rev4cl_caenes %in% c(3:6), sectorO10:="Manufacturing"]
    T1[b14_rev4cl_caenes %in% c(7,9), sectorO10:="Commerce"]
    T1[b14_rev4cl_caenes %in% c(8,10), sectorO10:="Transport_Comunication"]
    T1[b14_rev4cl_caenes %in% c(11:21), sectorO10:="Services"]
    T1_sit<-dcast.data.table(T1[!is.na(activ),],ano_trimestre+mes_central+region~activ,fun.aggregate = sum,na.rm=T,value.var = "Total")
    names(T1_sit)[4:6]<-c('Occupied',"Unoccupied","Inactive")
    T1_sex<-dcast.data.table(T1[!is.na(activ),],ano_trimestre+mes_central+region~sexo+activ,fun.aggregate = sum, na.rm=T,value.var = "Total")
    names(T1_sex)[4:9]<-c("Occupied_Male","Unoccupied_Male","Inactive_Male","Occupied_Female","Unoccupied_Female","Inactive_Female")
    T1_inf<-dcast.data.table(T1[activ==1],ano_trimestre+mes_central+region~ocup_form,fun.aggregate = sum, na.rm=T,value.var = "Total")
    names(T1_inf)[4:5]<-c("Formal","Informal")
    T1_sector<-dcast.data.table(T1[activ==1 & !is.na(sectorO10),],ano_trimestre+mes_central+region~sectorO10,fun.aggregate = sum, na.rm=T,value.var = "Total")
    T1_tam<-dcast.data.table(T1[activ==1 & !is.na(sectorO10) ,],ano_trimestre+mes_central+region~tamano,fun.aggregate = sum, na.rm=T,value.var = "Total")
    T1_aus<-dcast.data.table(T1[activ==1,],ano_trimestre+mes_central+region~cae_general,fun.aggregate = sum, na.rm=T,value.var = "Total")
    names(T1_aus)[4:6]<-c('Occupied_Regular','Occupied_Irregular','Occupied_Absent')
    T1_aus_sex<-dcast.data.table(T1[cae_general==3,],ano_trimestre+mes_central+region~sexo,fun.aggregate = sum, na.rm=T,value.var = "Total")
    names(T1_aus_sex)[4:5]<-c('Occupied_Absent_Male','Occupied_Absent_Female')
    T1_FT<-T1[cae_especifico%in%c(1:9),.(Labor_Force=sum(Total)),by=.(ano_trimestre,mes_central,region)]
    T1_PET<-T1[,.(WorkingAgePop=sum(Total)),by=.(ano_trimestre,mes_central,region)]
    T1_final<-merge(T1_sit,T1_sex,by=c('ano_trimestre',"mes_central","region"),all=T)
    T1_final<-merge(T1_final,T1_inf,by=c('ano_trimestre',"mes_central","region"),all=T)
    T1_final<-merge(T1_final,T1_sector,by=c('ano_trimestre',"mes_central","region"),all=T)
    T1_final<-merge(T1_final,T1_tam,by=c('ano_trimestre',"mes_central","region"),all=T)
    T1_final<-merge(T1_final,T1_aus,by=c('ano_trimestre',"mes_central","region"),all=T)
    T1_final<-merge(T1_final,T1_aus_sex,by=c('ano_trimestre',"mes_central","region"),all=T)
    T1_final<-merge(T1_final,T1_FT,by=c('ano_trimestre',"mes_central","region"),all=T)
    T1_final<-merge(T1_final,T1_PET,by=c('ano_trimestre',"mes_central","region"),all=T)
    T1_final<-merge(T1_final,T0,by=c('ano_trimestre',"mes_central","region"),all=T)
    T1_final[,Participation_Rate:=Labor_Force/WorkingAgePop]
    TABLA_r<-rbind(TABLA_r,T1_final)
  }
}

TABLA_r[nchar(region)==1,codigo_region:=paste0("0",region)]
TABLA_r[nchar(region)>1,codigo_region:=paste0(region)]
TABLA_r[,`NA`:=NULL]

#Creating a table with one-year percentage change values
TABLA_c<-NULL
ind<-c(9:12,1:8) # one year index
a<-2018
for(i in 9:12){
  for(j in 1:16){
    print(paste("I am doing year",a,"month",i, "region",j))
    aa<-TABLA_r[ano_trimestre==a & mes_central==i & region==j,c(4:28)]/TABLA_r[ano_trimestre==(a-1) & mes_central==i & region==j,c(4:28)]
    names(aa)<-paste0("PC_",names(aa))
    aa[,':='(ano_trimestre=a, mes_central=i, region=j)]
    TABLA_c<-rbind(TABLA_c,aa)  
  }
}

a<-2019
for(i in 1:12){
  for(j in 1:16){
    print(paste("I am doing year",a,"month",i, "region",j))
    aa<-TABLA_r[ano_trimestre==a & mes_central==i & region==j,c(4:28)]/TABLA_r[ano_trimestre==(a-1) & mes_central==i & region==j,c(4:28)]
    names(aa)<-paste0("PC_",names(aa))
    aa[,':='(ano_trimestre=a, mes_central=i, region=j)]
    TABLA_c<-rbind(TABLA_c,aa)  
  }
}

a<-2020
for(i in 1:9){
  for(j in 1:16){
    print(paste("I am doing year",a,"month",i, "region",j))
    aa<-TABLA_r[ano_trimestre==a & mes_central==i & region==j,c(4:28)]/TABLA_r[ano_trimestre==(a-1) & mes_central==i & region==j,c(4:28)]
    names(aa)<-paste0("PC_",names(aa))
    aa[,':='(ano_trimestre=a, mes_central=i, region=j)]
    TABLA_c<-rbind(TABLA_c,aa)  
  }
}

emp_data<-merge(TABLA_r,TABLA_c,by=c("region","ano_trimestre","mes_central"),all.y = T,sort = F)

foreign::write.dta(dataframe = emp_data,file = "01_Data/001_Employment/employment_panel_data.dta")

#deleting unzipped files once code runed (to avoid using Large Files Storage (LFS) in github)
d_list<-dir("01_Data/001_Employment/")[grepl(pattern = "ene",x = dir("01_Data/001_Employment/"))]
file.remove(paste0("01_Data/001_Employment/",d_list))
file.remove("01_Data/001_Employment/__MACOSX")
