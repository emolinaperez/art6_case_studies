#in mac  
root<- "/Users/edmun/Library/CloudStorage/OneDrive-Personal/Edmundo-ITESM/3.Proyectos/51. WB Decarbonization Project/TanzaniaArt6_CaseStudy/"
file.name <-"united_republic_of_tanzania.csv"
iso_code3 <- "TZA"
Country <- "united_republic_of_tanzania"
output.folder <- "/simulations raw/2025_03_10/"
dir.data <- paste0(root,output.folder)

#load data
data <- read.csv(paste0(dir.data,file.name)) 
data <- subset(data,region==Country)
id_vars <-c('region','time_period',"primary_id")
vars <- subset(colnames(data),!(colnames(data)%in%id_vars))
library(data.table)
data<-data.table::data.table(data)
DT.m1 = melt(data, id.vars = id_vars,
                   measure.vars = vars,
             )
DT.m1 <- data.frame(DT.m1)
DT.m1$variable <- as.character(DT.m1$variable)
sapply(DT.m1,class)

#now read drivers taxonomy. 
drivers <- read.csv(paste0(root,"simulations raw/driver_variables_taxonomy_20240117.csv"))

#change column name to taxonomy 
drivers$variable <- drivers$field
drivers$field <- NULL 

#merge
 dim(DT.m1)
 DT.m1 <- subset(DT.m1,variable%in%unique(drivers$variable))
 dim(DT.m1)
 
#
#merge  
 dim(DT.m1)
 test2 <- merge(DT.m1,data.table(drivers),by="variable")
 dim(test2)

#
#
test2$Year <- test2$time_period + 2015 
test2$time_period <- NULL 
test2 <- subset (test2,Year>=2023)

#read attribute primary
att <- read.csv(paste0(root,output.folder,"ATTRIBUTE_PRIMARY.csv"))
head(att)

#merge 
dim(test2)
test2 <- merge(test2,att,by="primary_id")
dim(test2)


#merge stratgy atts 
atts <- read.csv(paste0(root,output.folder,"ATTRIBUTE_STRATEGY.csv"))
head(atts)
#merge 
dim(test2)
test2 <- merge(test2,atts[c("strategy_id","strategy")],by="strategy_id")
dim(test2)

test2$Units <- "NA"
test2$Data_Type <- "sisepuede simulation"
test2$iso_code3<-iso_code3
test2$Country <- Country
test2$region <- NULL
test2$subsector_total_field <- NULL
test2$gas <- NA  

test2$model_variable_information <- NULL
test2$output_type<- "drivers"

#create an additional sector variable for energy  
energy_vars <- data.frame(variable=subset(unique(test2$variable),grepl("energy",unique(test2$variable))==TRUE ))
energy_vars$energy_subsector <-"TBD"
energy_vars$energy_subsector <- ifelse(grepl("ccsq",energy_vars$variable)==TRUE,"Carbon Capture and Sequestration",energy_vars$energy_subsector )
energy_vars$energy_subsector <- ifelse(grepl("inen",energy_vars$variable)==TRUE,"Industrial Energy",energy_vars$energy_subsector )
energy_vars$energy_subsector <- ifelse(grepl("entc",energy_vars$variable)==TRUE,"Power(electricity/heat)",energy_vars$energy_subsector )
energy_vars$energy_subsector <- ifelse(grepl("trns",energy_vars$variable)==TRUE,"Transportation",energy_vars$energy_subsector )
energy_vars$energy_subsector <- ifelse(grepl("scoe",energy_vars$variable)==TRUE,"Buildings",energy_vars$energy_subsector )

#merge energy vars with test2 
dim(test2)
test2 <- merge(test2,energy_vars,by="variable", all.x=TRUE)
dim(test2)

test2 <- test2[order(test2$strategy_id,test2$model_variable,test2$subsector,test2$category_value,test2$Year),]

write.csv(test2,paste0(root,"Tableau/drivers.csv"), row.names=FALSE)

