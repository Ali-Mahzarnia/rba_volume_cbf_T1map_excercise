options(java.parameters = "-Xmx8000m")
library(readxl)
library(dplyr)
library(magrittr)
library(xlsx)
path_vol="/Users/ali/Desktop/Aug23/primary_rba_excercise/individual_label_statistics/"
file_list=list.files(path_vol)


temp=read.delim( paste0(path_vol,file_list[1]) )
len=length(temp$T1map_mean)
volumes=matrix(NA,length(file_list),333)
noreadcsf=c(148,152,161,314,318,327)



Atlas_path = '/Users/ali/Desktop/Aug23/primary_rba_excercise/CHASSSYMM3AtlasLegends.xlsx'
Atlas = read.xlsx(Atlas_path, sheetIndex = 1)


for (i in 1:length(file_list)) {
  #print(file)
  temp=read.delim( paste0(path_vol,file_list[i]) , header = T)
  len=length(temp$T1map_mean)
  if (len > 0) {
  # temp$T1map_mean
  #print(sum(temp$T1map_mean))
  # print( sum(temp$T1map_mean[2:len]))
  #if (sum(temp$T1map_mean[2:len])==0) {volumes[i,2:len] = 0}
  #else 
  temp = temp[2:len,, drop =T]
  #rownames(temp) = as.numeric(rownames(temp))-1
  ind= which(temp$ROI>166)
  #temp$ROI[ind]
  temp_index = temp$ROI[ind] 
  temp$ROI[ind] = temp_index-1000+166
  missings= setdiff(seq(1,332),temp$ROI)
  for (j in missings) {
    temp=tibble::add_row(temp, ROI = j )
  }
  
  temp = temp[order(temp$ROI,decreasing = FALSE), ]
  temp[missings,]$T1map_mean =0 
  temp[missings,]$structure = Atlas$Structure[Atlas$index%in%missings] 
  
  
  
    volumes[i,2:333]=temp$T1map_mean[1:332]/ sum(temp$T1map_mean[1:332])
  # print(sum(as.numeric(volumes[i,2:len])))
  # whole_volume[i,2]=sum(temp$T1map_mean[-noreadcsf])
 
  
  } 
  
  else 
  {
    volumes[i,2:333] = rep(NA,332)
    
  }
  volumes[i,1]=substr( file_list[i] , 1, 9)
}

volumes = na.omit(volumes)
 
 volumes=as.data.frame(volumes);
 # volumes$V1=as.numeric(substr(volumes$V1,2,9)) # make dwi numeric
 # volumes[,2:len]=as.numeric(volumes[,2:len])
 xlsx::write.xlsx2(volumes, "T1_map_mean_list.xlsx" )
#  
# 

path_master='/Users/ali/Desktop/Aug23/primary_rba_excercise/MasterSheet_Experiments2021.xlsx'
data=read.xlsx(path_master, sheetName = '18ABB11_readable02.22.22_BJ_Cor' )
datatemp=data%>%dplyr::select(ARunno,Genotype,Weight, Sex, Age_Months, Treatment)#subselect

# 
# CIVMID_temp = datatemp$CIVMID
# 
# for (k in 1:length(CIVMID_temp)) {
#   j = CIVMID_temp[k]
#   if (!is.na(j)) {
#     if ( grepl( "-",j  ) ){  j= gsub("-","_",j)}
#     if ( grepl( "_",j  ) ){   after_dash = sub(".*_", "", j)  }
#        if (as.numeric(after_dash)<9) { CIVMID_temp[k]= paste0( sub("\\_.*", "", j) ,paste0("0",after_dash)) }
#       else { CIVMID_temp[k]= paste0( sub("\\_.*", "", j) ,paste0(after_dash)) }
#   }
#   
# }
# datatemp$CIVMID = CIVMID_temp

# 
# datatemp$CIVMID = gsub('_','',datatemp$CIVMID)
# datatemp$CIVMID = gsub('-','',datatemp$CIVMID)

#nchar(datatemp[111,1])
# datatemp=na.omit(datatemp)
# datatemp[nchar(datatemp$DWI)==1,]=matrix(NA,1,dim(datatemp)[2])
# datatemp=na.omit(datatemp)


datatemp=na.omit(datatemp) ## ommit all na and zero character dwi and died durring
# datatemp$ARunno=as.numeric(substr(datatemp$ARunno,1,9)) # make dwi numeric
#datatemp=datatemp[datatemp$Genotype=="APOE22",]

len = sum(datatemp$ARunno%in%volumes$V1)
temp_bind = matrix(NA, len, (dim(datatemp)[2]+ dim(volumes)[2]) )

# 
# excercise_path= '/Users/ali/Desktop/Aug23/primary_rba_excercise/Mice_Inventory_Proteomic.xlsx'
# excercise=read.xlsx(excercise_path, sheetName = 'ExerciseVsControl' )
# excercise_temp = excercise%>%dplyr::select(AnimalID,Exercise)#subselect
# excercise_temp = na.omit(excercise_temp)
# excercise_ID_temp = excercise_temp$AnimalID
# 
# for (k in 1:length(excercise_ID_temp)) {
#   j = excercise_ID_temp[k]
#   if (!is.na(j)) {
#     if ( grepl( "-",j  ) ){  j= gsub("-","_",j)}
#     if ( grepl( "_",j  ) ){   after_dash = sub(".*_", "", j)  }
#     if (as.numeric(after_dash)<9) { excercise_ID_temp[k]= paste0( sub("\\_.*", "", j) ,paste0("0",after_dash)) }
#     else { excercise_ID_temp[k]= paste0( sub("\\_.*", "", j) ,paste0(after_dash)) }
#   }
#   
# }
# excercise_temp$AnimalID = as.numeric(excercise_ID_temp )


for (j in 1:dim(temp_bind)[1]) {
  
  
  index_master = which(volumes[j,1] == datatemp$ARunno)
  # index_exercise =  which(volumes[j,1] == excercise_temp$AnimalID)
  
  if (length(index_master) >0)
  {
    temp_row = cbind(volumes[j,], datatemp[index_master,] )
    temp_bind [j,] =  unlist(temp_row)

    
    
  }
  
  
}



# 
# 
# temp_bind=cbind(volumes[indeces_of_whole,],datatemp[indeces_of_master,])
temp_bind=as.data.frame(temp_bind)
temp_bind=na.omit(temp_bind)
len_temp_bind = dim(temp_bind)[2]
colnames(temp_bind)[(len_temp_bind-6+1):len_temp_bind] = c("ID", "Genotype", "Weight", "Sex",  "Age_Months","Treatment")
temp_bind$Age_Months = as.numeric(temp_bind$Age_Months)
# temp_bind=cbind(volumes[indeces_of_whole,],datatemp)

temp_bind=temp_bind%>%mutate( age_cat=case_when(  Age_Months<median(Age_Months)~1 ,
                                                 Age_Months>=median(Age_Months)~2            ), .after = "ID" )




pathnames='/Users/ali/Desktop/Aug23/primary_rba_excercise/mouse_anatomy.csv'
ROI=read.csv(pathnames, header = TRUE, sep = ",", quote = "")
ROI=paste0(ROI$Bigpart, " ",ROI$ROI)
# 


library(emmeans)
library(effectsize)

len = dim(volumes)[2]
result=matrix(NA,14,(len-1))
for (i in 2:len) {
  var2=as.numeric(temp_bind[,i])
  lm <- lm( var2 ~ Age_Months*as.factor(Sex)*as.factor(Genotype)* as.factor(Treatment) ,data=temp_bind )
  # lm <- lm( var2 ~ as.numeric(Age_Months)*as.factor(Sex),data=temp_bind )
  
  an=anova(lm)
  pvals=an$`Pr(>F)`
  eff=eta_squared(lm, partial = TRUE)
  
  
  result[1:(dim(an)[1]-1),(i-1)]=pvals[1:(dim(an)[1]-1)]
  # result[4:6,(i-1)]=eff$Eta2_partial
  
  
  #emmeans(lm, ~ Age_Months, contr="tukey") 
  # sd(temp_bind$V2[temp_bind$age_cat==1]) 
  # 
  # mean(temp_bind$V2[  temp_bind$age_cat==1  ])
  # median(temp_bind$V2[  temp_bind$age_cat==1  ])
  # mean(temp_bind$V2[  temp_bind$age_cat==2  ])
  # median(temp_bind$V2[  temp_bind$age_cat==2 ])
  # sd(temp_bind$Age_Months[temp_bind$age_cat==1])
  # sd(temp_bind$Age_Months[temp_bind$age_cat==2])
}

rownames(result)= rownames(an)[1:(dim(an)[1]-1)]

adjustresult=result
for (j in 1:3) {
  adjustresult[j,]=p.adjust(adjustresult[j,], method = "fdr")
  
}
adjustresult [is.na(adjustresult)] =1
sum(is.na(adjustresult))



# # 
#  sum(adjustresult[1,]<0.05) #age
#  sum(adjustresult[2,]<0.05) #sex
#  sum(adjustresult[3,]<0.05) #age*sex


pathnames='/Users/ali/Desktop/Aug23/primary_rba_excercise/mouse_anatomy.csv'
ROI=read.csv(pathnames, header = TRUE, sep = ",", quote = "")
ROI=paste0(ROI$Bigpart, " ",ROI$ROI)
  
  

colnames(adjustresult) = ROI
  
write.xlsx2(adjustresult, "adjusted_pvalue_T1_map_mean.xlsx", sheetName = "All_Pvalues" )


# write.xlsx2(0, "volume.xlsx", sheetName = "0" )


for (j in 1:dim(adjustresult)[1]) {
  




var_index_sig=which(adjustresult[j,] <0.05)
var_index_sig=setdiff(var_index_sig, noreadcsf)
sig_result=adjustresult[j,var_index_sig]
# colnames(sig_result)=var_index_sig
# sig_result=sig_result[,order(sig_result[4,], decreasing = T)]

# 
# 
# table=matrix(NA, length(var_index_sig) ,12 )
# colnames(table_age) = c("Number" , "Index", "Name of the region" , "FDR corrected Pvalue", "Effect Size Eta^2", 
#                         "CI lower bound", "CI upper bound", "Mean group 1", "Mean group 2", 
#                         "SD group 1", "SD group 2", "F-value")
# for (i in 1:length(var_index_sig)) {
#   table[i,1]=i
#   index=as.numeric(colnames(sig_result)[i])
#   table_age[i,2] = index
#   table_age[i,3] = ROI[index]
#   
#   var2=as.numeric(temp_bind[,index+1])
#   lm <- lm( as.numeric(var2) ~Age_Months*as.factor(Sex)*as.factor(Genotype)* as.factor(Treatment),data=temp_bind )
#   
#   # pvals=an$`Pr(>F)`
#   temp=sig_result[,i]
#   eff=eta_squared(lm, partial = TRUE)
#   print=cbind(temp[1],t(unlist(eff[1,2:5]))  ) 
#   table[i,4:7] = print[-c(3)]
#   means=by(var2,as.factor(temp_bind$age_cat), mean )
#   table_age[i,8:9]=c(means[1], means[2])
#   sds=by(var2,as.factor(temp_bind$age_cat), sd )
#   table_age[i,10:11]=c(sds[1], sds[2])
#   an=anova(lm)
#   table_age[i,12]=an$`F value`[1]
# }

# print(paste0(rownames(adjustresult)[j]))
name_of_region = gsub("as.factor","",(gsub(":","-",rownames(adjustresult)[j])))


sig_result = sig_result[order(sig_result,decreasing = FALSE)]
sig_result = as.data.frame(sig_result)
write.xlsx2(sig_result , "adjusted_pvalue_T1_map_mean.xlsx", sheetName =  name_of_region , append=TRUE )

}


