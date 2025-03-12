library(data.table)
library(data.table)
#regions 

#Set root directory
# in mac 
root <- "/Users/edmun/Library/CloudStorage/OneDrive-Personal/Edmundo-ITESM/3.Proyectos/51. WB Decarbonization Project/SriLanka_CaseStudy/"
#load emissions targets 
#te_all<-read.csv(paste0(root,"/code/emission_targets.csv"))
te_all<-read.csv(paste0(root,"/code/emission_targets_art6.csv"))
target_country <- "LKA"
te_all<-te_all[,c("Subsector","Gas","Vars","Edgar_Class",target_country)]
te_all[,"tvalue"] <- te_all[,target_country]
te_all[,target_country] <- NULL
target_vars <- unlist(strsplit(te_all$Vars,":"))

#ouputfile
output.folder <- "/simulations raw/2025_03_10/"
output.file<-"sisepuede_results_sisepuede_run_2025-03-03T16;33;34.945350_WIDE_INPUTS_OUTPUTS.csv"


data_all<-read.csv(paste0(root,output.folder,output.file))
rall <- unique(data_all$region)

#set params of rescaling function
dir.output <- paste0(root,output.folder)
initial_conditions_id <- "_0"
time_period_ref <- 7

dim(data_all)
data_all <- subset(data_all,time_period>=time_period_ref)
dim(data_all)

#revise which sector-gas ids are zero at baseline 
te_all$simulation <- 0
for (i in 1:nrow(te_all))
 {
   # i<- 12
    vars <- unlist(strsplit(te_all$Vars[i],":"))
    if (length(vars)>1) {
    te_all$simulation[i] <- as.numeric(rowSums(data_all[data_all$primary_id==gsub("_","",initial_conditions_id) &  data_all$time_period==time_period_ref,vars]))
    } else {
     te_all$simulation[i] <- as.numeric(data_all[data_all$primary_id==gsub("_","",initial_conditions_id) &  data_all$time_period==time_period_ref,vars])   
    }
  }
te_all$simulation <- ifelse(te_all$simulation==0 & te_all$tvalue>0,0,1)
correct<- aggregate(list(factor_correction=te_all$simulation),list(Edgar_Class=te_all$Edgar_Class),mean)
te_all <- merge(te_all,correct,by="Edgar_Class")
te_all$tvalue <- te_all$tvalue/te_all$factor_correction
te_all$simulation<-NULL 
te_all$factor_correction<-NULL
te_all$Edgar_Class<-NULL

#now run

source(paste0(root,"/code/","rescale_function_baseline_mapping_timeref.r"))
z<-1
rescale(z,rall,data_all,te_all,initial_conditions_id,dir.output,time_period_ref)    


