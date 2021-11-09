#---------------------------------------#
#     Modeling & Estimation
#---------------------------------------#
library(plm)
library(data.table)
library(estimatr)
library(stargazer)

# Further cleaning ----

emp_data<-fread(file = "01_Data/paper_data.csv")
emp_data<-emp_data[!(ano_trimestre==2018) & !is.na(region),]
emp_data[,table(mes_central,ano_trimestre)]


##### Genereting measures of flow into different states
setkeyv(emp_data,c("region", "ano_trimestre", "mes_central"))

## Inactivos
emp_data[, flujo_inact_1 := shift(Inactive - shift(Inactive,n = 1),type = "lead",n = 1),by=.(region)]

#Gender
emp_data[, flujo_inact_h_1  := shift(Inactive_Male-shift(Inactive_Male,n=1),type = "lead",n = 1),by=.(region)]
emp_data[, flujo_inact_m_1  := shift(Inactive_Female-shift(Inactive_Female,n=1),type = "lead",n = 1),by=.(region)]


## Ocupados
emp_data[, flujo_ocu_1 := shift(Occupied - shift(Occupied,n = 1),type = "lead",n = 1),by=.(region)]
emp_data[, flujo_ocu_h_1  := shift(Occupied_Male-shift(Occupied_Male,n=1),type = "lead",n = 1),by=.(region)]
emp_data[, flujo_ocu_m_1  := shift(Occupied_Female-shift(Occupied_Female,n=1),type = "lead",n = 1),by=.(region)]

## Firm Size
emp_data[, flujo_ocu_small_1 := shift(Small - shift(Small,n = 1),type = "lead",n = 1),by=.(region)]
emp_data[, flujo_ocu_med_1 := shift(Medium - shift(Medium,n = 1),type = "lead",n = 1),by=.(region)]
emp_data[, flujo_ocu_large_1 := shift(Medium - shift(Medium,n = 1),type = "lead",n = 1),by=.(region)]

## Sectors
emp_data[, flujo_ocu_comm_1 := shift(Commerce - shift(Commerce,n = 1),type = "lead",n = 1),by=.(region)]
emp_data[, flujo_ocu_man_1 := shift(Manufacturing - shift(Manufacturing,n = 1),type = "lead",n = 1),by=.(region)]
emp_data[, flujo_ocu_nat_1 := shift(Naturales_Resources - shift(Naturales_Resources,n = 1),type = "lead",n = 1),by=.(region)]
emp_data[, flujo_ocu_ser_1 := shift(Services - shift(Services,n = 1),type = "lead",n = 1),by=.(region)]
emp_data[, flujo_ocu_tran_1 := shift(Transport_Comunication - shift(Transport_Comunication,n = 1),type = "lead",n = 1),by=.(region)]

## Formality

emp_data[, flujo_ocu_formal_1 := shift(Formal - shift(Formal,n = 1),type = "lead",n = 1),by=.(region)]
emp_data[, flujo_ocu_infor_1 := shift(Informal - shift(Informal,n = 1),type = "lead",n = 1),by=.(region)]

## Unoccupied

emp_data[, flujo_desoc_1 := shift(Unoccupied - shift(Unoccupied,n = 1),type = "lead",n = 1),by=.(region)]
emp_data[, flujo_desoc_h_1 := shift(Unoccupied_Male - shift(Unoccupied_Male,n = 1),type = "lead",n = 1),by=.(region)]
emp_data[, flujo_desoc_m_1 := shift(Unoccupied_Female - shift(Unoccupied_Female,n = 1),type = "lead",n = 1),by=.(region)]


### final adjustments to  local conditions related to lockdowns ----


emp_data[, initial_shock := as.numeric(ano_trimestre == 2020 &
                                         mes_central %in% 2:4)]
emp_data[, rest_pandemic := as.numeric(ano_trimestre == 2020 &
                                         mes_central >= 5)]

emp_data[is.na(per_LPE_region), per_LPE_region := 0] ## LPE
emp_data[is.na(mov_reduc), mov_reduc := 0]
emp_data[, mov_reduc := mov_reduc / 100]
emp_data[is.na(mes_cuarentena_mean), mes_cuarentena_mean := 0]
emp_data[is.na(diasEn_cuarentena_mean), diasEn_cuarentena_mean := 0]
emp_data[is.na(lockdown_per), lockdown_per := 0]
emp_data[is.na(lockdown_days_per), lockdown_days_per := 0]


emp_data[mes_cuarentena_max==1,lockdown_days_per_1st:=as.numeric(mes_central==min(mes_cuarentena_max*mes_central,na.rm = T)),by=.(region,ano_trimestre)]
emp_data[is.na(lockdown_days_per_1st),lockdown_days_per_1st:=0]
emp_data[,lockdown_days_per_1st:=100*lockdown_days_per*lockdown_days_per_1st]

emp_data[,lockdown_days_per_REST:=as.numeric(lockdown_days_per_1st==0 & mes_cuarentena_max==1),by=.(region,ano_trimestre)]
emp_data[is.na(lockdown_days_per_REST),lockdown_days_per_REST:=0]
emp_data[,lockdown_days_per_REST:=100*lockdown_days_per*lockdown_days_per_REST]

# Leveling variable defs

emp_data[, Pet_reg  := (Inactive + Occupied + Unoccupied)]
emp_data[, Pet_reg_h  := (Inactive_Male + Occupied_Male + Unoccupied_Male)]
emp_data[, Pet_reg_m  := (Inactive_Female + Occupied_Female + Unoccupied_Female)]

emp_data[, flujo_inact_Pet_1  := (flujo_inact_1 / Pet_reg) * 100]
emp_data[, flujo_inact_h_Pet_1  := (flujo_inact_h_1 / Pet_reg_h) * 100]
emp_data[, flujo_inact_m_Pet_1  := (flujo_inact_m_1 / Pet_reg_m) * 100]

emp_data[, flujo_ocu_Pet_1  := (flujo_ocu_1 / Pet_reg) * 100]
emp_data[, flujo_ocu_h_Pet_1  := (flujo_ocu_h_1 / Pet_reg_h) * 100]
emp_data[, flujo_ocu_m_Pet_1  := (flujo_ocu_m_1 / Pet_reg_m) * 100]

emp_data[, flujo_desoc_Pet_1  := (flujo_desoc_1 / Pet_reg) * 100]
emp_data[, flujo_desoc_h_Pet_1  := (flujo_desoc_h_1 / Pet_reg_h) * 100]
emp_data[, flujo_desoc_m_Pet_1  := (flujo_desoc_m_1 / Pet_reg_m) * 100]

emp_data[, flujo_ocu_formal_Pet_1  := (flujo_ocu_formal_1 / Pet_reg) * 100]
emp_data[, flujo_ocu_infor_Pet_1  := (flujo_ocu_infor_1 / Pet_reg) * 100]

emp_data[, flujo_ocu_small_Pet_1  := (flujo_ocu_small_1 / Pet_reg) * 100]
emp_data[, flujo_ocu_med_Pet_1  := (flujo_ocu_med_1 / Pet_reg) * 100]
emp_data[, flujo_ocu_large_Pet_1  := (flujo_ocu_large_1 / Pet_reg) * 100]

emp_data[, flujo_ocu_comm_Pet_1  := (flujo_ocu_comm_1 / Pet_reg) * 100]
emp_data[, flujo_ocu_man_Pet_1  := (flujo_ocu_man_1 / Pet_reg) * 100]
emp_data[, flujo_ocu_nat_Pet_1  := (flujo_ocu_nat_1 / Pet_reg) * 100]
emp_data[, flujo_ocu_ser_Pet_1  := (flujo_ocu_ser_1 / Pet_reg) * 100]
emp_data[, flujo_ocu_tran_Pet_1  := (flujo_ocu_tran_1 / Pet_reg) * 100]


# final variables

emp_data[, zona_centro_sur  := as.numeric(region %in% c(5, 8, 9, 10, 12, 13, 16))]
emp_data[, region := as.factor(region)]
emp_data[, mes_central := as.factor(mes_central)]


setnames(emp_data,c("Occupied","Formal","Informal","Inactive","Unoccupied"),
                  c("Occupied_stock","Formal_stock","Informal_stock","Innactive_stock","Unnocupied_Stock"))


setnames(emp_data,c("flujo_ocu_Pet_1","flujo_ocu_formal_Pet_1","flujo_ocu_infor_Pet_1","flujo_inact_Pet_1","flujo_desoc_Pet_1"),
                  c("Occupied","Formal","Informal","Inactive","Unoccupied"))

setnames(emp_data,c("initial_shock","rest_pandemic","per_LPE_region","mov_reduc","lockdown_days_per_1st","lockdown_days_per_REST"),
                  c("Initial shock","Rest of pandemic","LPE","Mobility","% Pop in Lockdown (1st month)","% Pop in Lockdown (following months)"))

Coef<-c("`Initial shock`","`Rest of pandemic`","`LPE`","`Mobility`","`% Pop in Lockdown (1st month)`","`% Pop in Lockdown (following months)`","(Intercept)")



# Modeling ----

#library(readstata13)
#data2<-data.table(read.dta13("../00 Data/final_data.dta",convert.factors = F),stringsAsFactors = F)
#data2[, region := as.factor(region)]
#data2[, mes_central := as.factor(mes_central)]

#### Baseline ----

# specifications
table1_ocu<- formula(`Occupied` ~	`Initial shock` + `Rest of pandemic` + `% Pop in Lockdown (1st month)` + `% Pop in Lockdown (following months)`)
table1_for<- formula(`Formal` ~	`Initial shock` + `Rest of pandemic` + `% Pop in Lockdown (1st month)` + `% Pop in Lockdown (following months)`)
table1_infor<- formula(`Informal` ~	`Initial shock` + `Rest of pandemic` + `% Pop in Lockdown (1st month)` + `% Pop in Lockdown (following months)`)
table1_ina<- formula(`Inactive` ~	`Initial shock` + `Rest of pandemic` + `% Pop in Lockdown (1st month)` + `% Pop in Lockdown (following months)`)
table1_des<- formula(`Unoccupied` ~	`Initial shock` + `Rest of pandemic` + `% Pop in Lockdown (1st month)` + `% Pop in Lockdown (following months)`)

models_t1<-c("table1_ocu","table1_for","table1_infor","table1_ina","table1_des")


for(i in 1:length(models_t1)){ 
  # Base OLS model
  eval(parse(text = paste0("m0_t1_",models_t1[i],"<-lm(update(",
                           models_t1[i],", ~ . + region + mes_central),data = emp_data)")))
  # Robust model (clustered coefficients at the region level)
  eval(parse(text = paste0("m_t1_",models_t1[i],"<-estimatr::lm_robust(",
                           models_t1[i],",clusters = region,fixed_effects = ~region+mes_central,data = emp_data)")))
  
  # Base OLS model - Regions with EARLY lockdowns
  eval(parse(text = paste0("m0_t2_",models_t1[i],"<-lm(update(",
                           models_t1[i],", ~ . + region + mes_central),data = emp_data[zona_centro_sur==1,])")))
  # Robust model (clustered coefficients at the region level) - Regions with EARLY lockdowns
  eval(parse(text = paste0("m_t2_",models_t1[i],"<-estimatr::lm_robust(",
                           models_t1[i],",clusters = region,fixed_effects = ~region+mes_central,data = emp_data[zona_centro_sur==1,])")))
  
  # Base OLS model - Regions with LATER lockdowns
  eval(parse(text = paste0("m0_t3_",models_t1[i],"<-lm(update(",
                           models_t1[i],", ~ . + region + mes_central),data = emp_data[zona_centro_sur==0,])")))
  # Robust model (clustered coefficients at the region level) - Regions with LATER lockdowns
  eval(parse(text = paste0("m_t3_",models_t1[i],"<-estimatr::lm_robust(",
                           models_t1[i],",clusters = region,fixed_effects = ~region+mes_central,data = emp_data[zona_centro_sur==0,])")))
}


#results_table1_v1.html - country level
stargazer(list(m0_t1_table1_ocu,m0_t1_table1_for,m0_t1_table1_infor,m0_t1_table1_ina,m0_t1_table1_des),
          se = starprep(list(m_t1_table1_ocu,m_t1_table1_for,m_t1_table1_infor,m_t1_table1_ina,m_t1_table1_des)),
          p= starprep(list(m_t1_table1_ocu,m_t1_table1_for,m_t1_table1_infor,m_t1_table1_ina,m_t1_table1_des),stat = "p.value"),
          type = "html", out = "04_Results/results_table1_v1.html",#style = 'aer',
          digits = 5,omit = c(paste0("region",2:16),paste0("mes_central",2:12)),
          dep.var.labels = c("Occupied","Formal","Informal","Inactive","Unemployed"),
          title = "All Regions")

#results_table1_v2.html - EARLY quarenteened regions
stargazer(list(m0_t2_table1_ocu,m0_t2_table1_for,m0_t2_table1_infor,m0_t2_table1_ina,m0_t2_table1_des),
          se = starprep(list(m_t2_table1_ocu,m_t2_table1_for,m_t2_table1_infor,m_t2_table1_ina,m_t2_table1_des)),
          p= starprep(list(m_t2_table1_ocu,m_t2_table1_for,m_t2_table1_infor,m_t2_table1_ina,m_t2_table1_des),stat = "p.value"),
          type = "html", out = "04_Results/results_table1_v2.html",#style = 'aer',
          digits = 5,omit = c(paste0("region",2:16),paste0("mes_central",2:12)),
          dep.var.labels = c("Occupied","Formal","Informal","Inactive","Unemployed"),
          title = "Regions with EARLY lockdowns", 
          notes = "Regions 5, 8, 9, 10, 12, 13, 16 had EARLY lockdowns (before LPE was implemented)", notes.append = T )


#results_table1_v3.html - LATER quarenteened regions
stargazer(list(m0_t3_table1_ocu,m0_t3_table1_for,m0_t3_table1_infor,m0_t3_table1_ina,m0_t3_table1_des),
          se = starprep(list(m_t3_table1_ocu,m_t3_table1_for,m_t3_table1_infor,m_t3_table1_ina,m_t3_table1_des)),
          p= starprep(list(m_t3_table1_ocu,m_t3_table1_for,m_t3_table1_infor,m_t3_table1_ina,m_t3_table1_des),stat = "p.value"),
          type = "html", out = "04_Results/results_table1_v3.html",#style = 'aer',
          digits = 5,omit = c(paste0("region",2:16),paste0("mes_central",2:12)),
          dep.var.labels = c("Occupied","Formal","Informal","Inactive","Unemployed"), 
          title = "Regions with LATER lockdowns",
          notes = "Regions 1:4, 6, 7, 11, 14, 15 had LATER lockdowns (after LPE was implemented)", notes.append = T )


#Panel estimation
emp_data[,date:=as.Date(paste(ano_trimestre,mes_central,"01",sep = "-"))]

setnames(emp_data, c("Initial shock","Rest of pandemic","LPE","Mobility","% Pop in Lockdown (1st month)","% Pop in Lockdown (following months)"),
         c("initial_shock","rest_pandemic","per_LPE_region","mov_reduc","lockdown_days_per_1st","lockdown_days_per_REST"))


plm1<-plm(Occupied ~	initial_shock + rest_pandemic + lockdown_days_per_1st + lockdown_days_per_REST + mes_central, index=c("region","date"),data=emp_data,model = "within",effect = "individual")
summary(plm1)

stargazer(plm1,
          type = 'html',out = "05_Robustness/Panel.html",digits = 5,omit = paste0("mes_central",2:12))


#### Robustness ----

####### Testing for spatial autocorrelation in the error term ----

#install.packages("chilemapas")
library(splm)
library(chilemapas)

regs<-chilemapas::generar_regiones()
regs<-as_Spatial(regs) # sf -> sp

nbs<-poly2nb(regs,queen = T) # Contiguity
w<-nb2listw(nbs,style = "W")

#plot(regs)
#plot(nbs,coordinates(regs),add=T,col='blue',pch=".")

# testing OLS models with region and time dummy variables

spdep::lm.LMtests(m0_t1_table1_ocu,listw = w,test = 'all')


# FE spatial error model --> No spatial effects

sink("05_Robustness/LM_residual_test_spatial_dependence.txt")
print(slmtest(plm1,data=emp_data,listw = w,test="rlme",model="within"))
print(slmtest(plm1,data=emp_data,listw = w,test="rlml",model="within"))
sink()

splm1<-spml(Occupied ~	initial_shock + rest_pandemic + lockdown_days_per_1st + lockdown_days_per_REST,
     data=emp_data[complete.cases(Occupied)==T,],
     index=c("region","date"),
     listw=w,
     spatial.error='b',
     effect="individual",
     model='within')

sink("05_Robustness/SpatialPanel_ML_Error_Model.txt")
print(summary(splm1))
sink()

splm1<-spgm(Occupied ~	initial_shock + rest_pandemic + lockdown_days_per_1st + lockdown_days_per_REST + mes_central,
     data=emp_data[complete.cases(Occupied)==T,],
     index=c("region","date"),
     listw=w,
     lag=FALSE,
     spatial.error=TRUE,
     moments = "initial",
     method = "w2sls")

sink("05_Robustness/SpatialPanel_GM_Error_Model.txt")
print(summary(splm1))
sink()


#FE spatial lag model --> No spatial effects in the lag

splm2<-spml(Occupied ~	initial_shock + rest_pandemic + lockdown_days_per_1st + lockdown_days_per_REST,
            data=emp_data,
            index=c("region","date"),
            listw=w,
            model="within",
            spatial.error='none',
            lag=TRUE)
summary(splm2)

sink("05_Robustness/SpatialPanel_ML_Lag_Model.txt")
print(summary(splm2))
sink()

splm2<-spgm(Occupied ~	initial_shock + rest_pandemic + lockdown_days_per_1st + lockdown_days_per_REST + mes_central,
            data=emp_data[complete.cases(Occupied)==T,],
            index=c("region","date"),
            listw=w,
            lag=TRUE,
            spatial.error=FALSE,
            moments = "fullweights",
            method = "w2sls")

sink("05_Robustness/SpatialPanel_GM_Lag_Model.txt")
print(summary(splm2))
sink()


#RE spatial lag & error model --> No spatial effects in the lag & error terms

splm3<-spml(Occupied ~	initial_shock + rest_pandemic + lockdown_days_per_1st + lockdown_days_per_REST,
            data=emp_data,
            index=c("region","date"),
            listw=w,
            model="within",
            spatial.error='b',
            lag=TRUE,effect = "individual")
summary(splm3)

# splm3<-spgm(Occupied ~	initial_shock + rest_pandemic + lockdown_days_per_1st + lockdown_days_per_REST + mes_central,
#             data=emp_data[complete.cases(Occupied)==T,],
#             index=c("region","date"),
#             listw=w,
#             lag=TRUE,
#             spatial.error=TRUE,
#             moments = "fullweights",
#             method = "w2sls")

sink("05_Robustness/SpatialPanel_ML_LagError_Model.txt")
print(summary(splm3))
sink()

