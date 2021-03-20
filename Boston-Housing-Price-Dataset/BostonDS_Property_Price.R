#************************************************************************************************************#
#                                                                                                            #
#      Environment Set up                                                                                    #
#                                                                                                            #  
#************************************************************************************************************#

setwd("H:/Imarticus/RProgramming/ProjectsDatasets/linearR/Dataset")
dataT=read.csv("Property_Price_Train.csv",stringsAsFactors=F)

library(dummies)
library(car)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
#install.packages("plotly")
library(plotly)
library(moments)
library(ggpubr)
#install.packages("nth-root")
library(pracma)
library(corrgram)
library(car)

dataTest=read.csv("Property_Price_Test.csv",stringsAsFactors = F)

#*****************************************************************************************************

str(dataT)

#Check for NAs

NAval=as.data.frame(colSums(is.na(dataT)))
NAval$Colnames<-colnames(dataT)
NAval.rownames<-NULL
colnames(NAval)<-c("NAcount","ColumnName")
NAval%>%filter(NAval$NAcount>0)-> NAval
NAval<-NAval[order(NAval$NAcount),]
NAval



#*************************************Electrical System***************************************************

dataT[is.na(dataT$Electrical_System),c("Electrical_System","Building_Class","Construction_Year","Remodel_Year")]

dataT$Building_Class<-as.factor(dataT$Building_Class)

dataT%>%filter(Building_Class==80)%>%ggplot()+
  geom_boxplot(aes(x=Electrical_System,y=Building_Class))
#The electrical system is FuseP and SBrkr for building class=80

ggplotly(ggplot()+geom_boxplot(aes(x=dataT$Electrical_System,y=dataT$Construction_Year)))
#Construction year for SBrkr=1890 to 2010,
#for FuseP contruction year range is 1910-1955
#and construction year for electrical_system=NA
#data point is 2006 which falls in the range of SBrkr

ggplotly(ggplot()+geom_boxplot(aes(x=dataT$Electrical_System,y=dataT$Remodel_Year)))
#remodel year for SBrkr=1950-2010 and that for FuseP=1950-2004
#remodel year for NA valued data point of electrical system is 2007
#which falls in the SBrkr range

#Therefore for all three analysis variables Contruction year, remodel year
#and Building class suggests the Electrical system should be SBrkr.

#NA electrical system column is: Sbrkr
dataT[is.na(dataT$Electrical_System),"Electrical_System"]<-"SBrkr"

#second nul value column
NAval[2,]

#**************************************Brick_Veneer_Type*****************************************************

dataT%>%filter(is.na(Brick_Veneer_Type))

#Variables under consideration are :
#Construction_Year,Remodel_Year

BrickType<-as.factor(dataT$Brick_Veneer_Type)

#contruction year
ggplotly(ggplot()+geom_boxplot(aes(x=BrickType,y=dataT$Construction_Year)))
#Stone brick type: 1940 to 2010,  brick type na for 1957 point is closer to the median of
#"none" and "Brkcmm" brick type so we can consider that to be used for na with construction year 1957
#Rest points 2002-2007 are closer to the median of the stone category. So we could use
#stone to replace the na with construction year from 2002-2007

ggplotly(ggplot()+geom_boxplot(aes(x=BrickType,y=dataT$Remodel_Year)))
#Stone: 1950 to 2008
#brick type with na lie in 1957 to 2008 as  point 1975 is more closer to the median of 
#BrckCmm we can consider that for 1975 point
#2002-2008 bricktype na is closert o median of stone which is 2006. so we can consider
#that to be used for those point lying in that range

dataT[,c("Construction_Year","Remodel_Year","Brick_Veneer_Type")]%>%filter(Construction_Year==1957 & is.na(Brick_Veneer_Type))

# so for the 1957 construction year and 1975 remodel year for Brickveneer type we have two choices
#acc to contruction year it should be "None" or "BrkCmm" and acc to remodel year it should be "BrckCmm"
class(dataT$Brick_Veneer_Type)
dataTemp<-dataT

index=dataTemp$Construction_Year==1957 & dataTemp$Remodel_Year==1975 & is.na(dataTemp$Brick_Veneer_Type)

dataTemp$Brick_Veneer_Type=ifelse(index,"BrkCmn",dataTemp$Brick_Veneer_Type)
dataTemp[dataTemp$Construction_Year==1957 & dataTemp$Remodel_Year==1975,c("Brick_Veneer_Type")]
dataTemp$Brick_Veneer_Type=ifelse(is.na(dataTemp$Brick_Veneer_Type), "Stone",dataTemp$Brick_Veneer_Type)
dataTemp[is.na(dataTemp$Brick_Veneer_Type),"Brick_Veneer_Type"]

#checking plots
ggplotly(ggplot()+geom_boxplot(aes(x=dataTemp$Brick_Veneer_Type,y=dataTemp$Construction_Year)))
ggplotly(ggplot()+geom_boxplot((aes(x=dataTemp$Brick_Veneer_Type,y=dataTemp$Remodel_Year))))

dataT$Brick_Veneer_Type=dataTemp$Brick_Veneer_Type
#removing temporary variables
rm(BrickType,index,NAval.rownames)

NAval[3,]

#*********************************Brick_Veneer_Area : 8 NAs*********************************************
#variables under consideration : Construction year, Remodel year, Brick_Veneer_Type

dataT[is.na(dataT$Brick_Veneer_Area),c("Brick_Veneer_Type","Brick_Veneer_Area","Construction_Year","Remodel_Year")]
mean(dataT[dataT$Brick_Veneer_Type=="Stone","Brick_Veneer_Area"],na.rm=TRUE)
median(dataT[dataT$Brick_Veneer_Type=="Stone","Brick_Veneer_Area"],na.rm=TRUE)
#for stone category of brick veneer type mean = 239.3047 and median = 206
ggplot()+geom_histogram(aes(dataT$Brick_Veneer_Area[dataT$Brick_Veneer_Type=="Stone"]))
#as data is skewed taking median value : 206

mean(dataT[dataT$Construction_Year==2002,"Brick_Veneer_Area"],na.rm=TRUE)
median(dataT[dataT$Construction_Year==2002,"Brick_Veneer_Area"],na.rm=TRUE)
#for construction year 2002 = we have mean = 136.619, and median = 110. Therefore data is little skewed
#hence considering 110 median value.

mean(dataT[dataT$Remodel_Year==2002,"Brick_Veneer_Area"],na.rm=TRUE)
median(dataT[dataT$Remodel_Year==2002,"Brick_Veneer_Area"],na.rm=TRUE)
#mean for remodel year = 2002 is 113.5435 and median = 0
#as data is skewed and for 2002 construction year and brick veneer type both suggest 
#value=206 and 110 and remodel year suggests value=0.
(206+110+0)/3
#therefore taking mean of it we get 105.3333.
#Therefore we substitute 105 as the brick veneer area for brick type Stone and construction and remodel
#year = 2002

index=dataT$Brick_Veneer_Type=="Stone" & dataT$Construction_Year ==2002 & dataT$Remodel_Year==2002 & is.na(dataT$Brick_Veneer_Area)
dataTemp=dataT
dataTemp$Brick_Veneer_Area=ifelse(index,105,dataTemp$Brick_Veneer_Area)
dataTemp[dataT$Brick_Veneer_Type=="Stone" & dataT$Construction_Year ==2002 & dataT$Remodel_Year==2002 & dataTemp$Brick_Veneer_Area==105,"Brick_Veneer_Area"]


dataTemp[is.na(dataTemp$Brick_Veneer_Area),c("Brick_Veneer_Type","Brick_Veneer_Area","Construction_Year","Remodel_Year")]

mean(dataT[dataT$Brick_Veneer_Type=="BrkCmn","Brick_Veneer_Area"],na.rm=TRUE)
median(dataT[dataT$Brick_Veneer_Type=="BrkCmn","Brick_Veneer_Area"],na.rm=TRUE)
#mean = 247.6667 and median=192. therefore data is skewed and taking median = 192 for brick type= brkcmn.

mean(dataT[dataT$Construction_Year==1957,"Brick_Veneer_Area"],na.rm=TRUE)
median(dataT[dataT$Construction_Year==1957,"Brick_Veneer_Area"],na.rm=TRUE)
#mean=55.110526, median=0 therefore data is skewed and taking median=0

mean(dataT[dataT$Remodel_Year==1975,"Brick_Veneer_Area"],na.rm=TRUE)
median(dataT[dataT$Remodel_Year==1975,"Brick_Veneer_Area"],na.rm=TRUE)
#mean=44.3333 and median=0 therefore data is skewed and taking median=0

#as for brick type = BrkCmn value = 192 and for construction year 1957 and remodel year 1975 value=0 we take mean of three values
(192+0+0)/3
#therefore substituting the value 64 for the brick veneer area = BrkCmn and contruction and remodel year = 1957 and 1975 respectively
index=is.na(dataTemp$Brick_Veneer_Area)& dataTemp$Construction_Year==1957 & 
  dataTemp$Remodel_Year==1975 & dataTemp$Brick_Veneer_Type=="BrkCmn"
dataTemp$Brick_Veneer_Area=ifelse(index,64,dataTemp$Brick_Veneer_Area)
dataTemp[dataTemp$Construction_Year==1957 & 
           dataTemp$Remodel_Year==1975 & dataTemp$Brick_Veneer_Type=="BrkCmn","Brick_Veneer_Area"]

dataTemp[is.na(dataTemp$Brick_Veneer_Area),c("Construction_Year","Remodel_Year","Brick_Veneer_Type")]

#for construction year 2007 and remodel year 2007

mean(dataT[dataT$Brick_Veneer_Type=="Stone","Brick_Veneer_Area"],na.rm=TRUE)
median(dataT[dataT$Brick_Veneer_Type=="Stone","Brick_Veneer_Area"],na.rm=TRUE)
#mean = 239 and median=206 therefore data is skewed and taking median = 206 for brick type=Stone.

mean(dataT[dataT$Construction_Year==2007,"Brick_Veneer_Area"],na.rm=TRUE)
median(dataT[dataT$Construction_Year==2007,"Brick_Veneer_Area"],na.rm=TRUE)
#mean=181, median=130 therefore data is skewed and taking median=130

mean(dataT[dataT$Remodel_Year==2007,"Brick_Veneer_Area"],na.rm=TRUE)
median(dataT[dataT$Remodel_Year==2007,"Brick_Veneer_Area"],na.rm=TRUE)
#mean = 134 and median=77. As data is skewed taking median = 77
#Taking mean of all estimated value of the NA from all three variables:
(206+130+77)/3
#therefore imputing 137 value in the NA for bricktype = Stone , construction year = 2007 and 
#remodel year = 2007
index=dataTemp$Brick_Veneer_Type=="Stone" & 
  dataTemp$Construction_Year==2007 &
  dataTemp$Remodel_Year==2007 & is.na(dataTemp$Brick_Veneer_Area)
dataTemp$Brick_Veneer_Area=ifelse(index,137,dataTemp$Brick_Veneer_Area)

dataTemp[is.na(dataTemp$Brick_Veneer_Area),c("Construction_Year","Remodel_Year","Brick_Veneer_Type","Brick_Veneer_Area")]


#brick type = 2003 and remodel and construction year = 2003
mean(dataTemp$Brick_Veneer_Area[dataTemp$Construction_Year==2003],na.rm = TRUE)
median(dataTemp$Brick_Veneer_Area[dataTemp$Construction_Year==2003],na.rm=TRUE)
#AS data is skewed mean= 196 and median=141.
#therefore using median as the value for imputation consideration

mean(dataTemp$Brick_Veneer_Area[dataTemp$Remodel_Year==2003],na.rm = TRUE)
median(dataTemp$Brick_Veneer_Area[dataTemp$Remodel_Year==2003],na.rm = TRUE)
#Mean=106 and median = 0.Data is highly skewed so taking the median value=0

#For brickType="Stone" : mean = 239.3047 and median = 206. As data looks skewed taking the median =206
#for imputation consideration
#taking average of all values to be taken into consideration for  imputation
(141+0+206)/3
#Imputation value: 115

index=dataTemp$Brick_Veneer_Type=="Stone" &
  dataTemp$Construction_Year==2003 &
  dataTemp$Remodel_Year==2003 &
  is.na(dataTemp$Brick_Veneer_Area)
dataTemp$Brick_Veneer_Area=ifelse(index,115,dataTemp$Brick_Veneer_Area)
index=is.na(dataTemp$Brick_Veneer_Area)
dataTemp[index,c("Construction_Year","Remodel_Year","Brick_Veneer_Type","Brick_Veneer_Area")]


#Construction = 2007 and remodel=2008 and brick type = "Stone"

mean(dataTemp$Brick_Veneer_Area[dataTemp$Construction_Year==2007],na.rm = TRUE)
median(dataTemp$Brick_Veneer_Area[dataTemp$Construction_Year==2007],na.rm=TRUE)
#mean=180 and median=130
#as data looks slightly skewed taking median value for imputation consideration

mean(dataTemp$Brick_Veneer_Area[dataTemp$Remodel_Year==2008],na.rm=TRUE)
median(dataTemp$Brick_Veneer_Area[dataTemp$Remodel_Year==2008],na.rm=TRUE)
#mean=237 and medain=186
#as data looks slightly skewed taking median value for imputation consideration

#
#For brickType="Stone" : mean = 239.3047 and median = 206. As data looks skewed taking the median =206
#for imputation consideration
#taking average of all values to be taken into consideration for  imputation
(130+186+206)/3
#therefore value to be imputed is 174

index=dataTemp$Brick_Veneer_Type=="Stone" &
  dataTemp$Construction_Year==2007 &
  dataTemp$Remodel_Year==2008 &
  is.na(dataTemp$Brick_Veneer_Area)
dataTemp$Brick_Veneer_Area=ifelse(index,174,dataTemp$Brick_Veneer_Area)

dataTemp[is.na(dataTemp$Brick_Veneer_Area),c("Construction_Year","Remodel_Year","Brick_Veneer_Type","Brick_Veneer_Area")]

#Construction year=2006 and remodel year = 2007 and bricktype = "Stone"
mean(dataTemp$Brick_Veneer_Area[dataTemp$Construction_Year==2006],na.rm = TRUE)
median(dataTemp$Brick_Veneer_Area[dataTemp$Construction_Year==2006],na.rm=TRUE)
#Data looks highly skewed as mean= 169 and median = 72. 
#therefore considering median =72 for imputation consideration

mean(dataTemp$Brick_Veneer_Area[dataTemp$Remodel_Year==2007],na.rm=TRUE)
median(dataTemp$Brick_Veneer_Area[dataTemp$Remodel_Year==2007],na.rm=TRUE)
#data looks highly skewed as mean=134 and median = 80.
#therefore taking median =80 for imputation consideration

#For brickType="Stone" : mean = 239.3047 and median = 206. As data looks skewed taking the median =206
#for imputation consideration
#taking average of all values to be taken into consideration for  imputation
(72+80+206)/3
#Therefore the value to be imputed is 119.
index=dataTemp$Construction_Year==2006 &
  dataTemp$Remodel_Year==2007 &
  dataTemp$Brick_Veneer_Type=="Stone" &
  is.na(dataTemp$Brick_Veneer_Area)
dataTemp$Brick_Veneer_Area=ifelse(index,119,dataTemp$Brick_Veneer_Area)

dataTemp[is.na(dataTemp$Brick_Veneer_Area),c("Construction_Year","Remodel_Year","Brick_Veneer_Type","Brick_Veneer_Area")]

#Construction year = 2006 and remodel year =2006 for brick type = "Stone"
#For contruction year = 2006 we know found earlier that 
#Data looks highly skewed as mean= 169 and median = 72. 
#therefore considering median =72 for imputation consideration
mean(dataTemp$Brick_Veneer_Area[dataTemp$Remodel_Year==2006],na.rm=TRUE)
median(dataTemp$Brick_Veneer_Area[dataTemp$Remodel_Year==2006],na.rm=TRUE)
##mean=138 and median=25
#therefore data is highly skewed and so taking median=25 for imputation consideration

#For brickType="Stone" : mean = 239.3047 and median = 206. As data looks skewed taking the median =206
#for imputation consideration
#taking average of all values to be taken into consideration for  imputation
(72+25+206)/3
#Therefore imputing 101 value in the brick_veneer_area for construction = 2006
#remodel year = 2006 and brick type = "Stone"
index=dataTemp$Construction_Year==2006 &
  dataTemp$Remodel_Year==2006 &
  dataTemp$Brick_Veneer_Type=="Stone" &
  is.na(dataTemp$Brick_Veneer_Area)
dataTemp$Brick_Veneer_Area=ifelse(index,101,dataTemp$Brick_Veneer_Area)

dataTemp[is.na(dataTemp$Brick_Veneer_Area),c("Construction_Year","Remodel_Year","Brick_Veneer_Type")]
dataT$Brick_Veneer_Area=dataTemp$Brick_Veneer_Area

NAval[4,]

#****************************Basement_Height*********************************************************
#Variables under consideration
#Property shape and house design and basement presence or absence

dataTemp=dataT
dataTemp[is.na(dataTemp$Basement_Height),c("Basement_Condition","Basement_Height")]
#Therefore All NA's present for basement condition which means no basement similarly those corresponding
#columns have NA's for basement Height
#if we put 0 means basement has 0 height which is equivalent to saying basement is not present
#therefore imputing zero value for basement height

dataTemp$Basement_Height=ifelse(is.na(dataTemp$Basement_Height),0,dataTemp$Basement_Height)
dataTemp[is.na(dataTemp$Basement_Height),]

dataT$Basement_Height=dataTemp$Basement_Height

NAval[5,]
#********************************Basement_Condition**************************************************
#as per the document NA in basement condition indicates no basement
#therefore substituting a no basement category for basement condition
class(dataTemp$Basement_Condition)
dataTemp$Basement_Condition=ifelse(is.na(dataTemp$Basement_Condition),"no basement",dataTemp$Basement_Condition)
dataTemp$Basement_Condition[is.na(dataTemp$Basement_Condition)]
dataT=dataTemp

NAval[6,]
#**********************************BsmtFinType1*****************************************************
dataTemp[is.na(dataTemp$BsmtFinType1),c("Basement_Condition","BsmtFinType1")]
#Therefore all NA values in BsmtFinType1 are corresponding to the no basement category of BsmtFinType1
#therefore imputing a new category none in bsmtfintype1
class(dataTemp$BsmtFinType1)
dataTemp$BsmtFinType1=ifelse(is.na(dataTemp$BsmtFinType1),"none",dataTemp$BsmtFinType1)
dataTemp$BsmtFinType1[is.na(dataTemp$BsmtFinType1)]

dataT$BsmtFinType1=dataTemp$BsmtFinType1

NAval[7,]
#******************************Exposure level - 38 NAs ***********************************************
#According to the document NA refers to no basement category
#therefore imputing a new category No basement in the variable
class(dataT$Exposure_Level)
dataTemp=dataT
dataTemp$Exposure_Level=ifelse(is.na(dataTemp$Exposure_Level),"no basement",dataTemp$Exposure_Level)
dataTemp$Exposure_Level[is.na(dataTemp$Exposure_Level)]
dataT$Exposure_Level=dataTemp$Exposure_Level


NAval[8,]
#******************************BsmtFinType2 - 38 NAs ***********************************************
#According to the document NA refers to no basement category
#therefore imputing a new category No basement in the variable
class(dataT$BsmtFinType2)
dataTemp=dataT
dataTemp$BsmtFinType2=ifelse(is.na(dataTemp$BsmtFinType2),"no basement", dataTemp$BsmtFinType2)
dataTemp$BsmtFinType2[is.na(dataTemp$BsmtFinType2)]
dataT$BsmtFinType2=dataTemp$BsmtFinType2


NAval[9,]
#*************************************Garage**********************************************************
class(dataT$Garage)
dataTemp=dataT
#according to the document a NA value in the garage column signifies no garage.
#therefore immputing the value of no garage in place of NA values
dataTemp$Garage=ifelse(is.na(dataTemp$Garage),"no garage",dataTemp$Garage)
dataTemp$Garage[is.na(dataTemp$Garage)]
dataT$Garage=dataTemp$Garage

NAval[10,]
#***********************************Garage built year**************************************************
#garage built year doesnt make sense if there is no garage we put -1 value for garage built year with 
#NA values
dataTemp$Garage_Built_Year=ifelse(is.na(dataTemp$Garage_Built_Year),-1,dataTemp$Garage_Built_Year)
dataTemp[is.na(dataTemp$Garage_Built_Year),]
dataT$Garage_Built_Year=dataTemp$Garage_Built_Year

NAval[11,]
#****************************Garage_Finish_Year****************************************************

#garage Quality doesnt make sense if there is no garage we put -1 value for garage finish year with 
#NA values
dataTemp$Garage_Finish_Year=ifelse(is.na(dataTemp$Garage_Finish_Year),-1,dataTemp$Garage_Finish_Year)
dataTemp[is.na(dataTemp$Garage_Finish_Year),]
dataT$Garage_Finish_Year=dataTemp$Garage_Finish_Year

NAval[12,]
#****************************Garage_Quality****************************************************

#garage Quality doesnt make sense if there is no garage we put no garage value for garage quality
#with NA values
dataTemp<-dataT
dataTemp$Garage_Quality=ifelse(is.na(dataTemp$Garage_Quality),"no garage",dataTemp$Garage_Quality)
dataTemp[is.na(dataTemp$Garage_Quality),]
dataT$Garage_Quality=dataTemp$Garage_Quality

NAval[13,]
#****************************Garage_Condition****************************************************

#garage condition doesnt make sense if there is no garage we put no garage value for garage condition
#with NA values
dataTemp<-dataT
dataTemp$Garage_Condition=ifelse(is.na(dataTemp$Garage_Condition),"no garage",dataTemp$Garage_Condition)
dataTemp[is.na(dataTemp$Garage_Condition),]
dataT$Garage_Condition=dataTemp$Garage_Condition

NAval[14,]
#************************************Lot_Extent***************************************************
#Variables under consideration : Property shape,  lane type, road type and zoning class


dataT[is.na(dataT$Lot_Extent),c("Property_Shape","Lane_Type","Road_Type","Zoning_Class")]

#groupby each unique combination of values in property shape , lane type, road type and zoning class
group_by_all(dataT[is.na(dataT$Lot_Extent),c("Property_Shape","Lane_Type","Road_Type","Zoning_Class")])
#Since all lanetypes are NA meaning there is no alley access as per the document.
#therefore checking total no of NA's in Lane type 
count=0
i=0
for( i in is.na(dataT$Lane_Type)){
  if( i == TRUE){
    count=count+1
  }
}
count
#removing temporary variables
rm(i,count)
#total no of NA's in Lane type = 1368.
#therefore this variable isn't useful for analysis. Discarding this variable from analysis of NA's for 
#lot_Extent
dataTemp=dataT
dataTemp$Property_Shape=as.factor(dataTemp$Property_Shape)
dataTemp$Zoning_Class=as.factor(dataTemp$Zoning_Class)
dataTemp$Road_Type=as.factor(dataTemp$Road_Type)

dataTemp[is.na(dataT$Lot_Extent),c("Property_Shape","Zoning_Class","Road_Type")]%>%
  group_by_all%>%filter(row_number()==1)

#Property shape: IR1, Zoning_Class=RLD, Road_Type=Paved

mean(dataTemp[dataTemp$Property_Shape=="IR1","Lot_Extent"],na.rm=TRUE)
median(dataTemp[dataTemp$Property_Shape=="IR1","Lot_Extent"],na.rm=TRUE)  
#Property shape :IR1 - mean= 76 and median=74.
#Therefore taking median=74 as data is slighlty skewed for imputation consideration


mean(dataTemp[dataTemp$Zoning_Class=="RLD","Lot_Extent"],na.rm=TRUE)
median(dataTemp[dataTemp$Zoning_Class=="RLD","Lot_Extent"],na.rm=TRUE) 
#Zoning Class = RLD, mean=74 and median =72.
#therefore as data is slighlty skewed taking median =72 for imputation consideration


mean(dataTemp[dataTemp$Road_Type=="Paved","Lot_Extent"],na.rm=TRUE)
median(dataTemp[dataTemp$Road_Type=="Paved","Lot_Extent"],na.rm=TRUE) 
#Road type= paved , mean=70, median=69.
#theerefore data is slightly skewed and taking median =69 for imputation consideration
#As the values from each variable under consideration is different taking mean of all the values
(74+72+69)/3
#72 therefore substituting this value for lot extent where
#Property shape: IR1, Zoning_Class=RLD, Road_Type=Paved
index=dataTemp$Property_Shape=="IR1" &
  dataTemp$Zoning_Class=="RLD" &
  dataTemp$Road_Type=="Paved" &
  is.na(dataTemp$Lot_Extent)
dataTemp$Lot_Extent=ifelse(index,71,dataTemp$Lot_Extent)
length(which(is.na(dataTemp$Lot_Extent)))
259-96
# therefore out of 259 NA's in Lot_Extent 163 are replaced with 72, 96 NA's are still remaining in Lot_Extent

dataTemp[is.na(dataTemp$Lot_Extent),c("Property_Shape","Zoning_Class","Road_Type")]%>%
  group_by_all()%>%filter(row_number()==1)

#Property shape = IR2, Zoning_Class="RLD" and Road_type="Paved"
#we have already calculated :Zoning class=RLD and road type=Paved
#Zoning Class = RLD, mean=74 and median =72.
#therefore as data is slighlty skewed taking median =72 for imputation consideration
#Road type= paved , mean=70, median=69.
#theerefore data is slightly skewed and taking median =69 for imputation consideration
mean(dataTemp$Lot_Extent[dataTemp$Property_Shape=="IR2"],na.rm=TRUE)
median(dataTemp$Lot_Extent[dataTemp$Property_Shape=="IR2"],na.rm=TRUE)
#Property shape : IR2, mean= 76 and median = 57
#therefore as data is skewed taking the median value = 57 for imputation consideration
#As the values under consideration for imputation are all different taking the mean of all three values
(72+69+57)/3
#Imputing value 66 for property shape = IR2 , Zoning Class = RLD, and Road type = Paved
index=dataTemp$Property_Shape=="IR2" & dataTemp$Zoning_Class=="RLD" & dataTemp$Road_Type=="Paved" &
  is.na(dataTemp$Lot_Extent)
dataTemp$Lot_Extent=ifelse(index,66,dataTemp$Lot_Extent)
length(which(is.na(dataTemp$Lot_Extent)))
#therefore 10 records are replaced with 66 value and 83 records are remaining for NA imputation

dataTemp[is.na(dataTemp$Lot_Extent),c("Property_Shape","Road_Type","Zoning_Class")]%>%
  group_by_all()%>%filter(row_number()==1)

#Property shape : Reg, Zoning Class= RLD, Road_Type=Paved


mean(dataTemp[dataTemp$Property_Shape=="Reg","Lot_Extent"],na.rm=TRUE)
median(dataTemp[dataTemp$Property_Shape=="Reg","Lot_Extent"],na.rm=TRUE)  
#Property shape :Reg - mean= 67 and median=66
#Therefore taking median=66 as data is slighlty skewed for imputation consideration


#we have already calculated :Zoning class=RLD and road type=Paved
#Zoning Class = RLD, mean=74 and median =72.
#therefore as data is slighlty skewed taking median =72 for imputation consideration
#Road type= paved , mean=70, median=69.
#theerefore data is slightly skewed and taking median =69 for imputation consideration
#As all values under consideration for imputation are different we take mean of the values
(72+69+66)/3
#Therefore imputing 66 for Property shape:Reg, ZoningClass=RLD, and Road_type=Paved
index=dataTemp$Property_Shape=="Reg" & dataTemp$Zoning_Class=="RLD" & dataTemp$Road_Type=="Paved" &
  is.na(dataTemp$Lot_Extent)
dataTemp$Lot_Extent=ifelse(index,66,dataTemp$Lot_Extent)
length(which(is.na(dataTemp$Lot_Extent)))
83-34
#49 values imputated by 66 and 34 NA's are still remaining in Lot_Extent

dataTemp[is.na(dataTemp$Lot_Extent),c("Property_Shape","Zoning_Class","Road_Type")]%>%
  group_by_all()%>%filter(row_number()==1)
#Property shape=Reg , Zoning_class=RMD and Road_type=Paved

#we have already calculated :Property Shape=Reg and road type=Paved
#Road type= paved , mean=70, median=69.
#theerefore data is slightly skewed and taking median =69 for imputation consideration
#Property shape :Reg - mean= 67 and median=66
#Therefore taking median=66 as data is slighlty skewed for imputation consideration

mean(dataTemp$Lot_Extent[dataTemp$Zoning_Class=="RMD"],na.rm=TRUE)
median(dataTemp$Lot_Extent[dataTemp$Zoning_Class=="RMD"],na.rm=TRUE)
#For Zoning class= RMD we have mean=52 and median = 51.
#as data is only slightly skewed we can take either mean or median. Taking median=51 for 
#imputation consideration
#As the values for imputation under consideration are all different taking mean of all the values
(69+66+51)/3
#Therefore imputing 62 in the rows with NA for Lot extent having
#property shape="Reg", ZoningClass="RMD" and road_type="Paved"
index=dataTemp$Zoning_Class=="RMD" & dataTemp$Road_Type=="Paved" & dataTemp$Property_Shape=="Reg" &
  is.na(dataTemp$Lot_Extent)
dataTemp$Lot_Extent=ifelse(index,62,dataTemp$Lot_Extent)    
length(which(is.na(dataTemp$Lot_Extent)))
34-17
#therefore 17 values are imputated with 62 value for property shape =Reg,
#zoning class="RMD" and road_type="Paved" and remaning are 17 NA's

dataTemp[is.na(dataTemp$Lot_Extent),c("Property_Shape","Road_Type","Zoning_Class")]%>%
  group_by_all()%>%filter(row_number()==1)

#Property shape : IR1, Zoning Class= RMD, Road_Type=Paved

#Since we have already calculated values for property shape IR1, zoning class=RMD
#and road_type=Paved as below

#For Zoning class= RMD we have mean=52 and median = 51.
#as data is only slightly skewed we can take either mean or median. Taking median=51 for 
#imputation consideration
#Property shape :IR1 - mean= 76 and median=74.
#Therefore taking median=74 as data is slighlty skewed for imputation consideration
#Road type= paved , mean=70, median=69.
#theerefore data is slightly skewed and taking median =69 for imputation consideration
#As all the values under imputation consideration are different using mean of those values
(51+74+69)/3

#therefore imputing 64 value for NA's in Lot_Extent and property shape=IR1, Zoning_Class="RMD"
#and Road_Type="Paved"
index=dataTemp$Zoning_Class=="RMD" & dataTemp$Road_Type=="Paved" & dataTemp$Property_Shape=="IR1" &
  is.na(dataTemp$Lot_Extent)
dataTemp$Lot_Extent=ifelse(index,64,dataTemp$Lot_Extent)    
length(which(is.na(dataTemp$Lot_Extent)))
#therefore 2 values imputated with 64 for property shape=IR1, Zoning class=RMD and 
#road_Type=Paved. Remaining 15 NA's in Lot_Extent

dataTemp[is.na(dataTemp$Lot_Extent),c("Property_Shape","Road_Type","Zoning_Class")]%>%
  group_by_all()%>%filter(row_number()==1)

#Property shape=Reg, road type=Paved, Zoning_class=FVR

mean(dataTemp$Lot_Extent[dataTemp$Zoning_Class=="FVR"],na.rm = TRUE)
median(dataTemp$Lot_Extent[dataTemp$Zoning_Class=="FVR"],na.rm=TRUE)
#Zoning class= "FVR", mean= 59, median = 65
#therefore data is slightly skewed and so considering median=65 for imputation consideration

#Since we have already calculated mean and median for Property shape= Reg and Road type=Paved
#the values are as below
#Property shape :Reg - mean= 67 and median=66
#Therefore taking median=66 as data is slighlty skewed for imputation consideration
#Road type= paved , mean=70, median=69.
#theerefore data is slightly skewed and taking median =69 for imputation consideration
#As all the values under imputation consideration are different using mean of those values
(65+66+69)/3
#therefore imputing 66 for the NA's for property shape=Reg, ZoningClass=FVR, and Road_Type=Paved
index=dataTemp$Zoning_Class=="FVR" & dataTemp$Property_Shape=="Reg" & dataTemp$Road_Type=="Paved" &
  is.na(dataTemp$Lot_Extent)
dataTemp$Lot_Extent=ifelse(index,66,dataTemp$Lot_Extent)
length(which(is.na(dataTemp$Lot_Extent)))

15-9
#therefore 6 values imputed with 66 for property shape= Reg, Zoning_class=FVR and road type=Paved
#and remaining 9 NA's in Lot_Extent

dataTemp[is.na(dataTemp$Lot_Extent),c("Property_Shape","Road_Type","Zoning_Class")]%>%
  group_by_all()%>%filter(row_number()==1)

#Property shape=IR1, Road_type=Gravel, Zoning Class= RLD

mean(dataTemp$Lot_Extent[dataTemp$Road_Type=="Gravel"],na.rm=TRUE)
median(dataTemp$Lot_Extent[dataTemp$Road_Type=="Gravel"],na.rm=TRUE)
#for Road_type=Gravel we have mean = 85, and median = 81.
#therefore data is slightly skewed. so taking median = 81 for imputation consideration
#As we have already calculated the mean median for Property shape=IR1 and zoning_class= RLD
#Using same values as below
#Property shape :IR1 - mean= 76 and median=74.
#Therefore taking median=74 as data is slighlty skewed for imputation consideration
#Zoning Class = RLD, mean=74 and median =72.
#therefore as data is slighlty skewed taking median =72 for imputation consideration
#As the values for consideration for imputation are all different taking the mean of those values
(81+74+72)/3
#therefore takinhg 75 as the imputation value for Property shape=IR1, Zoning Class= RLD,
#and road_type=Gravel

index=dataTemp$Zoning_Class=="RLD" & dataTemp$Property_Shape=="IR1" & dataTemp$Road_Type=="Gravel" &
  is.na(dataTemp$Lot_Extent)
dataTemp$Lot_Extent=ifelse(index,75,dataTemp$Lot_Extent)
length(which(is.na(dataTemp$Lot_Extent)))

#therefore 1 value1 imputed with 75 for property shape= IR1, Zoning_class=RLD and road type=Gravel
#and remaining 8 NA's in Lot_Extent

dataTemp[is.na(dataTemp$Lot_Extent),c("Property_Shape","Road_Type","Zoning_Class")]%>%
  group_by_all()%>%filter(row_number()==1)

#Property shape=IR2, RoadType=Paved and Zoning class= FVR
#As we have already computated mean and median for property shape=IR2 ,
#Roadtype=Paved and zoning class= FVR stating below for reference
#Road type= paved , mean=70, median=69.
#theerefore data is slightly skewed and taking median =69 for imputation consideration
#Property shape : IR2, mean= 76 and median = 57
#therefore as data is skewed taking the median value = 57 for imputation consideration
#Zoning class= "FVR", mean= 59, median = 65
#therefore data is slightly skewed and so considering median=65 for imputation consideration
#As the values under consideration for imputation are all different taking the mean of all three values
(69+57+65)/3
#therefore imputing 63 value for NA for property shape=IR2,roadType=Paved and zoning class=FVR


index=dataTemp$Zoning_Class=="FVR" & dataTemp$Property_Shape=="IR2" & dataTemp$Road_Type=="Paved" &
  is.na(dataTemp$Lot_Extent)
dataTemp$Lot_Extent=ifelse(index,63,dataTemp$Lot_Extent)
length(which(is.na(dataTemp$Lot_Extent)))

#therefore 2 values imputed with 63 for property shape= IR2, Zoning_class=FVR and road type=Paved
#and remaining 6 NA's in Lot_Extent

dataTemp[is.na(dataTemp$Lot_Extent),c("Property_Shape","Road_Type","Zoning_Class")]%>%
  group_by_all()%>%filter(row_number()==1)

#Property shape=IR1, Zoning class=RHD, roadType=Paved

mean(dataTemp$Lot_Extent[dataTemp$Zoning_Class=="RHD"],na.rm=TRUE)
median(dataTemp$Lot_Extent[dataTemp$Zoning_Class=="RHD"],na.rm=TRUE)
#For zoning class RHD we have mean= 59 and median = 60
#therefore data is only very slightly skewed
#therefore taking median 60 for imputation consideration

#As we have already calculated the values for property shape = IR1 and roadtype = Paved
#for easy reference below are the details
#Property shape :IR1 - mean= 76 and median=74.
#Therefore taking median=74 as data is slighlty skewed for imputation consideration
#Road type= paved , mean=70, median=69.
#theerefore data is slightly skewed and taking median =69 for imputation consideration
#As the values from each variable under consideration is different taking mean of all the values
(60+74+69)/3
#Therefore imputing 67 for NA in LotExtent for property shape=IR1, zoning class=RHD and road type=Paved

index=dataTemp$Zoning_Class=="RHD" & dataTemp$Property_Shape=="IR1" & dataTemp$Road_Type=="Paved" &
  is.na(dataTemp$Lot_Extent)
dataTemp$Lot_Extent=ifelse(index,67,dataTemp$Lot_Extent)
length(which(is.na(dataTemp$Lot_Extent)))
#therefore 1 value imputated by 67 and remaining 5 NA's in Lot_Extent


dataTemp[is.na(dataTemp$Lot_Extent),c("Property_Shape","Road_Type","Zoning_Class")]%>%
  group_by_all()%>%filter(row_number()==1)

#Property shape=IR3, Zoning class=RLD, roadType=Paved

mean(dataTemp$Lot_Extent[dataTemp$Property_Shape=="IR3"],na.rm=TRUE)
median(dataTemp$Lot_Extent[dataTemp$Property_Shape=="IR3"],na.rm=TRUE)
#for property shape = IR3 we have mean = 138 and median = 150
#therefore as data looks skewed for that we take median - 150 for imputation consideration
#We have alreday calculated the mean and median for Zoning class and roadtype 
#for easy reference they as below
#Zoning Class = RLD, mean=74 and median =72.
#therefore as data is slighlty skewed taking median =72 for imputation consideration
#Road type= paved , mean=70, median=69.
#theerefore data is slightly skewed and taking median =69 for imputation consideration
#As the values from each variable under consideration is different taking mean of all the values
(150+72+69)/3
#therefore imputing 97 value in the Lot_Extent for property shape=IR3, roadtype=Paved, and
#zoning class=RLD


index=dataTemp$Zoning_Class=="RLD" & dataTemp$Property_Shape=="IR3" & dataTemp$Road_Type=="Paved" &
  is.na(dataTemp$Lot_Extent)
dataTemp$Lot_Extent=ifelse(index,97,dataTemp$Lot_Extent)
length(which(is.na(dataTemp$Lot_Extent)))
#therefore 3 value imputated by 97 and remaining 2 NA's in Lot_Extent


dataTemp[is.na(dataTemp$Lot_Extent),c("Property_Shape","Road_Type","Zoning_Class")]%>%
  group_by_all()%>%filter(row_number()==1)
#PropertyShape=Reg, zoning class = RHD and road_Type=Paved

#As we have already calculated the values for them for easy reference they are as below

#Property shape :Reg - mean= 67 and median=66
#Therefore taking median=66 as data is slighlty skewed for imputation consideration
#For zoning class RHD we have mean= 59 and median = 60
#therefore data is only very slightly skewed
#therefore taking median 60 for imputation consideration
#Road type= paved , mean=70, median=69.
#theerefore data is slightly skewed and taking median =69 for imputation consideration
#As the values from each variable under consideration is different taking mean of all the values
(66+60+69)/3
#Therefore using 65 value for imputation of Lot_Extent for property shape=REg, zoning_Class=RHD, 
#and road_type=Paved
index=dataTemp$Zoning_Class=="RHD" & dataTemp$Property_Shape=="Reg" & dataTemp$Road_Type=="Paved" &
  is.na(dataTemp$Lot_Extent)
dataTemp$Lot_Extent=ifelse(index,65,dataTemp$Lot_Extent)
length(which(is.na(dataTemp$Lot_Extent)))
#therefore 2 value imputated by 65 and remaining 0 NA's in Lot_Extent
dataT$Lot_Extent<-dataTemp$Lot_Extent

NAval[15,]
#***********************************Fireplace_Quality**************************************************
dataTemp=dataT

dataTemp[is.na(dataTemp$Fireplace_Quality),c("Fireplaces","Fireplace_Quality")]%>%
  group_by(Fireplaces)%>%
  filter(row_number()==1)
#According to the Fireplaces columns refers to the no of fireplaces.
#and all NA values in Fireplace quality correspond to the 0 no of fireplaces.
#therefore imputing one more category named no fireplaces in the fireplace quality
class(dataTemp$Fireplace_Quality)
dataTemp$Fireplace_Quality<-ifelse(is.na(dataTemp$Fireplace_Quality),"no fireplaces",
                                   dataTemp$Fireplace_Quality)
length(which(is.na(dataTemp$Fireplace_Quality)))
dataT$Fireplace_Quality<-dataTemp$Fireplace_Quality

NAval[16,]
#************************************Fence Quality*****************************************************
dataTemp<-dataT
#According to the document NA value in Fence quality refers to the no fence categoery.
#therefore substituting no fence in place of NA values in Fence quality
class(dataTemp$Fence_Quality)
dataTemp$Fence_Quality<-ifelse(is.na(dataTemp$Fence_Quality),"no fence",dataTemp$Fence_Quality)
length(which(is.na(dataTemp$Fence_Quality)))
dataT$Fence_Quality<-dataTemp$Fence_Quality


NAval[17,]
#************************************Lane type*******************************************************
dataTemp<-dataT
#according to the document NA in a lanetype signifies there is no alley access.
#therefore substitutinng no alley acces in place of NA values in Lane Type
class(dataTemp$Lane_Type)
dataTemp$Lane_Type<-ifelse(is.na(dataTemp$Lane_Type),"no alley access",dataTemp$Lane_Type)
length(which(is.na(dataTemp$Lane_Type)))

dataT$Lane_Type<-dataTemp$Lane_Type

NAval[18,]
#********************************Miscellaneous feature*********************************************
dataTemp<-dataT
#According to the document NA in miscellaneous feature indicates no miscellaneous features(none)
#therefore inserting none value for NA in Miscellaneous feature
class(dataTemp$Miscellaneous_Feature)
dataTemp$Miscellaneous_Feature<-ifelse(is.na(dataTemp$Miscellaneous_Feature),"none",dataTemp$Miscellaneous_Feature)
length(which(is.na(dataTemp$Miscellaneous_Feature)))

dataT$Miscellaneous_Feature<-dataTemp$Miscellaneous_Feature

NAval[19,]
#********************************Pool Quality***************************************************
dataTemp<-dataT
#According to the document NA in pool quality indicates no pool.
#therefore puting no pool inplace of NA in pool_quality feature
class(dataTemp$Pool_Quality)
dataTemp$Pool_Quality<-ifelse(is.na(dataTemp$Pool_Quality),"no pool",dataTemp$Pool_Quality)
length(which(is.na(dataTemp$Pool_Quality)))
dataT$Pool_Quality<-dataTemp$Pool_Quality

NAval[20,]

#*****************************************************************************************************

#*******************               End of NA treatment section                        ****************

#*****************************************************************************************************

#transforming data to factor wherever applicable


#removal of unique valued columns
summary(dataT[1:5])
dataTemp<-dataT[,-1]
str(dataTemp)
dataTrans<-dataTemp

#If columns have one value for most of the data then there is little or no scope for an algorithm to 
#learn from it.
#therefore removing biased columns from the data
dataTemp<-dataTrans
summary(dataTrans[0:5])
dataTemp$Zoning_Class<-as.factor(dataTemp$Zoning_Class)
dataTemp$Road_Type<-as.factor(dataTemp$Road_Type)
dataTemp$Building_Class<-as.factor(dataTemp$Building_Class)

summary(dataTemp[6:10])
dataTemp$Lane_Type<-as.factor(dataTemp$Lane_Type)
dataTemp$Property_Shape<-as.factor(dataTemp$Property_Shape)
dataTemp$Land_Outline<-as.factor(dataTemp$Land_Outline)
dataTemp$Utility_Type<- as.factor(dataTemp$Utility_Type)
dataTemp$Lot_Configuration<-as.factor(dataTemp$Lot_Configuration)

summary(dataTemp[11:15])
dataTemp$Property_Slope<-as.factor(dataTemp$Property_Slope)
dataTemp$Neighborhood<-as.factor(dataTemp$Neighborhood)
dataTemp$Condition1<-as.factor(dataTemp$Condition1)
dataTemp$Condition2<-as.factor(dataTemp$Condition2)
dataTemp$House_Type<-as.factor(dataTemp$House_Type)

summary(dataTemp[16:20])
dataTemp$House_Design<-as.factor(dataTemp$House_Design)
dataTemp$Overall_Material<-as.factor(dataTemp$Overall_Material)
dataTemp$House_Condition<-as.factor(dataTemp$House_Condition)

summary(dataTemp[21:25])
dataTemp$Roof_Design<-as.factor(dataTemp$Roof_Design)
dataTemp$Roof_Quality<-as.factor(dataTemp$Roof_Quality)
dataTemp$Exterior1st<-as.factor(dataTemp$Exterior1st)
dataTemp$Exterior2nd<-as.factor(dataTemp$Exterior2nd)
dataTemp$Brick_Veneer_Type<-as.factor(dataTemp$Brick_Veneer_Type)

summary(dataTemp[26:30])
dataTemp$Exterior_Material<-as.factor(dataTemp$Exterior_Material)
dataTemp$Exterior_Condition<-as.factor(dataTemp$Exterior_Condition)
dataTemp$Foundation_Type<-as.factor(dataTemp$Foundation_Type)
dataTemp$Basement_Height<-as.factor(dataTemp$Basement_Height)

summary(dataTemp[31:35])
dataTemp$Basement_Condition<-as.factor(dataTemp$Basement_Condition)
dataTemp$Exposure_Level<-as.factor(dataTemp$Exposure_Level)
dataTemp$BsmtFinType1<-as.factor(dataTemp$BsmtFinType1)
dataTemp$BsmtFinType2<-as.factor(dataTemp$BsmtFinType2)

summary(dataTemp[36:40])
dataTemp$Heating_Type<-as.factor(dataTemp$Heating_Type)
dataTemp$Heating_Quality<-as.factor(dataTemp$Heating_Quality)

summary(dataTemp[41:45])
dataTemp$Air_Conditioning<-as.factor(dataTemp$Air_Conditioning)
dataTemp$Electrical_System<-as.factor(dataTemp$Electrical_System)

summary(dataTemp[46:50])
dataTemp$Underground_Full_Bathroom<-as.factor(dataTemp$Underground_Full_Bathroom)
dataTemp$Underground_Half_Bathroom<-as.factor(dataTemp$Underground_Half_Bathroom)
dataTemp$Full_Bathroom_Above_Grade<-as.factor(dataTemp$Full_Bathroom_Above_Grade)
dataTemp$Half_Bathroom_Above_Grade<-as.factor(dataTemp$Half_Bathroom_Above_Grade)

summary(dataTemp[51:55])
dataTemp$Bedroom_Above_Grade<-as.factor(dataTemp$Bedroom_Above_Grade)
dataTemp$Kitchen_Above_Grade<-as.factor(dataTemp$Kitchen_Above_Grade)
dataTemp$Kitchen_Quality<-as.factor(dataTemp$Kitchen_Quality)
dataTemp$Rooms_Above_Grade<-as.factor(dataTemp$Rooms_Above_Grade)
dataTemp$Functional_Rate<-as.factor(dataTemp$Functional_Rate)

summary(dataTemp[56:60])
dataTemp$Fireplaces<-as.factor(dataTemp$Fireplaces)
dataTemp$Fireplace_Quality<-as.factor(dataTemp$Fireplace_Quality)
dataTemp$Garage<-as.factor(dataTemp$Garage)

summary(dataTemp[61:65])
dataTemp$Garage_Size<-as.factor(dataTemp$Garage_Size)
dataTemp$Garage_Quality<-as.factor(dataTemp$Garage_Quality)
dataTemp$Garage_Condition<-as.factor(dataTemp$Garage_Condition)
dataTemp$Pavedd_Drive<-as.factor(dataTemp$Pavedd_Drive)

summary(dataTemp[66:70])

summary(dataTemp[71:75])
dataTemp$Pool_Quality<-as.factor(dataTemp$Pool_Quality)
dataTemp$Fence_Quality<-as.factor(dataTemp$Fence_Quality)
dataTemp$Miscellaneous_Feature<-as.factor(dataTemp$Miscellaneous_Feature)

summary(dataTemp[76:80])
dataTemp$Month_Sold<-as.factor(dataTemp$Month_Sold)
dataTemp$Sale_Type<-as.factor(dataTemp$Sale_Type)
dataTemp$Sale_Condition<-as.factor(dataTemp$Sale_Condition)
dataTrans<-dataTemp

#*****************************************************************************************************

# ************************          End of datatype conversions         *******************************

#*****************************************************************************************************


dataTemp<-dataTrans

#identifying biased columns
summary(dataTemp[0:5])
1150/1459
#zoning class has 78.82 % same records for RLD.
1453/1459
#Road type has 99.58 % data only for Paved and only 6 records for gravel.
#this includes bias in the data. Therefore removing this column from analysis
dataTemp[,c("Road_Type")]<-NULL

dim(dataTemp)

summary(dataTemp[6:10])
#as all the records belong to the AllPub category and only 1 record to the NoSewa.
#this variable is of little use in training the model.
#Therefore removing the utility type from the dataset
dataTemp[,c("Utility_Type")]<-NULL
1381/1459
#for Property slope also 94.65%data is in GS category.
#therefore this variable is also of little use to us.
#therefore removing property slope from the analysis
dataTemp[,c("Property_Slope")]<-NULL

summary(dataTemp[11:16])
#Condition1 has 1444 records in Norm. Therefore this variable also is of little use to us.
#therefore removing it from analysis
dataTemp[,c("Condition2")]<-NULL
1219/1459

summary(dataTemp[16:20])
#roofQuality has 1433 records in one category SS. This variable is also of no use to us.
#therefore removing it from analysis
dataTemp[,c("Roof_Quality")]<-NULL

summary(dataTemp[21:25])
summary(dataTemp[26:30])
1310/1459
summary(dataTemp[31:35])
#Heating type has 1427 records in GasA type. so its biased.
#therefore removing this variable from analysis
dataTemp[,c("Heating_Type")]<-NULL

summary(dataTemp[36:40])
summary(dataTemp[41:45])
summary(dataTemp[46:50])
1391/1459
#Therefore as Ktichen above grade has biased data 1391 records in 1 category. 
#removing it from analysis
dataTemp[,c("Kitchen_Above_Grade")]<-NULL
1359/1459
#Same with functional rate 1359 records in TF category.
#therefore removing functional rate form analysis
dataTemp[,c("Functional_Rate")]<-NULL

summary(dataTemp[51:55])
1310/1459

summary(dataTemp[56:60])
1325/1459
#removing garage condition as 1325 records belong to the TA category (90%).
dataTemp[,c("Garage_Condition")]<-NULL
1339/1459
#removing pavedd drive as 91 % i.e. 1339 records belong to Y category.
dataTemp[,c("Pavedd_Drive")]<-NULL

summary(dataTemp[61:65])
#removing pool quality as 1452 records belong to the No pool category
dataTemp[,c("Pool_Quality")]<-NULL

summary(dataTemp[66:69])
dataTrans<-dataTemp

#*****************************************************************************************************

#*******************            End of bias column removal          ***********************************

#*****************************************************************************************************

#*******************************************************************************************

#                               Exploratory Data Analysis

#*******************************************************************************************



#Transformin certain variables in one variables to reduce complexity
#Construction year and remodel year could be used to check newness value
dataTrans$NewnessAge<-dataTrans$Year_Sold-dataTrans$Remodel_Year

dataTrans$OldAge<-dataTrans$Year_Sold-dataTrans$Construction_Year

ggplotly(ggplot(data=dataTrans,aes(x=NewnessAge,y=..density..))+
           geom_histogram(color="lightblue",fill="lightblue1")+geom_density(color="darkcyan"))
ggplotly(ggplot(data=dataTrans,aes(x=OldAge,y=..density..))+
           geom_histogram(color="lightblue",fill="lightblue1")+
           geom_density(color="darkcyan"))
class(dataTrans$Garage_Built_Year)
class(dataTrans$Garage_Finish_Year)
dataTrans[,"Garage_Finish_Year"]

#Target variable analysis for skewness and outlier checks

skewness(dataTrans$Sale_Price)
ggplot(data=dataTrans,aes(x=dataTrans$Sale_Price,y=..density..))+
  geom_histogram(color="lightsalmon3",fill="lightsalmon1")+
  geom_density(color="brown")


#As data is skewed checking for outliers
#for any other dataset apart from normally distributed data the data is 95% within 4.5SD
#according to chebyshevs rule
#therefore checking for outliers with 4.5SD
index=dataTrans$Sale_Price>mean(dataTrans$Sale_Price)-4.5*sd(dataTrans$Sale_Price)&
      dataTrans$Sale_Price<mean(dataTrans$Sale_Price)+4.5*sd(dataTrans$Sale_Price)
count=0
for(i in index){
  if(i==FALSE){
    count=count+1
  }
}
count
#therefore there are seven outliers.
dataTrans$Sale_Price[index==FALSE]
#removing the outliers by imputing median value in it
dataTrans$Sale_Price=ifelse(index,dataTrans$Sale_Price,median(dataTrans$Sale_Price))
skewness(dataTrans$Sale_Price)
#The data is still skewed.
#therefore transforming the data
saleprice=nthroot(dataTrans$Sale_Price,2)
skewness(saleprice)
dataTrans$Sale_Price=saleprice
#now the data is normally distributed
ggplot(data=dataTrans,aes(x=saleprice,y=..density..))+
  geom_histogram(color="lightsalmon3",fill="lightsalmon1")+
  geom_density(color="brown")

rm(saleprice,index,count,i)
rm(NAval)


#********************************************************************************************

#Analysing for outliers for other variables

#********************************************************************************************
data.numeric=dataTrans[,sapply(dataTrans,is.numeric)]
colnames(data.numeric)


#Numeric variables analysis

dim(data.numeric)
#26 numeric variables

str(data.numeric)
#removing non required variables:Construction year,remodel year, and garage built year
data.numeric[,c("Construction_Year","Remodel_Year","Garage_Built_Year")]<-NULL
#removing year_sold variable
data.numeric[,c("Year_Sold")]<-NULL
dim(data.numeric)
#22 variables are remaining

#checking for each variable the correlation with target variable

corrgram(data.numeric)
#from the plot important variables are : 
#Lot extent, lot_size is slightly significant,brickveneer area,bsmtfinsf1,bsmtunfsf,
#total_basement area, firstfloor area, second floor area,grade living area
#newness age and oldage of house are in negative strong correlation with Sale_Price


corval<-cor(data.numeric)[rownames(cor(data.numeric))=="Sale_Price"  ]
cormat<-cbind(colnames(cor(data.numeric)),corval)
cormat[corval>0.05 | corval<(-0.05),]

#according to the table we get below variables as significant
# [1] "Lot_Extent"              "0.312620110208075" 
# [2,] "Lot_Size"                "0.259121099871568" 
# [3,] "Brick_Veneer_Area"       "0.417755454889764" 
# [4,] "BsmtFinSF1"              "0.357617201464225" 
# [5,] "BsmtUnfSF"               "0.218632056188969" 
# [6,] "Total_Basement_Area"     "0.589420182460964" 
# [7,] "First_Floor_Area"        "0.5802944977393"   
# [8,] "Second_Floor_Area"       "0.296796532608"    
# [9,] "Grade_Living_Area"       "0.670520605629484" 
# [10,] "Three_Season_Lobby_Area" "0.055925515760007" 
# [11,] "Screen_Lobby_Area"       "0.110669583602817" 
# [12,] "Sale_Price"              "1"                 
# [13,] "NewnessAge"              "-0.556157277827411"
# [14,] "OldAge"                  "-0.570614686446211"

#clubbing the lobby areas and floor areas
Floorarea<-data.numeric$First_Floor_Area+data.numeric$Second_Floor_Area
cor(Floorarea,data.numeric$Sale_Price)
#combined correlation is 72%

#combining lobby areas
Lobbyarea<-data.numeric$Screen_Lobby_Area+data.numeric$Three_Season_Lobby_Area
cor(Lobbyarea,data.numeric$Sale_Price)
#combined correlation with target variable is 13%

data.numeric$TotalFloorArea<-Floorarea
data.numeric$LobbyArea<-Lobbyarea

#removing redundant variables after adding lobby and total floor area: 
#Screen_Lobby_Area,Three_Season_Lobby_Area,First_Floor_Area,&Second_Floor_Area

data.numeric[,c("Screen_Lobby_Area","Three_Season_Lobby_Area",
                "First_Floor_Area","Second_Floor_Area")]<-NULL
dim(data.numeric)
str(data.numeric)[1]
#removing redundant variables which are not strongly correlated with the target variable
data.numeric_temp<-data.numeric[,c("Lot_Extent",            
                                   "Lot_Size" ,           
                                   "Brick_Veneer_Area",      
                                   "BsmtFinSF1",     
                                   "BsmtUnfSF"  ,    
                                   "Total_Basement_Area" ,   
                                   "TotalFloorArea" ,  
                                   "Grade_Living_Area",
                                   "LobbyArea",
                                   "Pool_Area" ,
                                   "Sale_Price" ,
                                   "NewnessAge" ,
                                   "OldAge" )]
dim(data.numeric_temp)
data.numeric<-data.numeric_temp
#removing temporary variables
rm(data.numeric_temp)
rm(Floorarea,Lobbyarea,corval)

#checking for outliers of each variables 
colnames(data.numeric)[1]



ggplotly(ggplot(data.numeric,aes(x=Lot_Extent,y=..density..))+
           geom_histogram(color="lightblue1",fill="lightcyan")+
           geom_density(color="lightblue")+
           ggtitle("Lot_Extent distribution")+
           theme_light())

#the distribution looks normally distributed with some extreme points on the right

ggplotly(ggplot()+geom_point(aes(x=data.numeric$Lot_Extent,y=data.numeric$Sale_Price),
                             color="lightblue")+
           ggtitle("Lot_Extent Vs Sale_Price")+
           xlab("Lot_Extent")+
           ylab("Sale_Price")+
           theme_light()
)


#from the graph is it appeared that there are only three outliers at
#lot_extent=153, 313,313
#imputing mean or median for these two points to remove outliers.
#since we already saw the data is skewed therefore imputing with median
median(data.numeric$Lot_Extent)
#70 is the value of median to be imputed in place of outliers
index<-data.numeric$Lot_Extent %in% c(153,313)
data.numeric$Lot_Extent[index]


data.numeric$Lot_Extent<-ifelse(index,70,data.numeric$Lot_Extent)

#checking th edistributions after removal of outliers
ggplotly(ggplot(data.numeric,aes(x=Lot_Extent,y=..density..))+
           geom_histogram(color="lightblue1",fill="lightcyan")+
           geom_density(color="lightblue")+
           ggtitle("Lot_Extent distribution")+
           theme_light())

ggplotly(ggplot()+geom_point(aes(x=data.numeric$Lot_Extent,y=data.numeric$Sale_Price),
                             color="lightblue")+
           ggtitle("Lot_Extent Vs Sale_Price")+
           xlab("Lot_Extent")+
           ylab("Sale_Price")+
           theme_light()
)


colnames(data.numeric)[2]
#**************************************Lot_Size*****************************************************

#checking the data distributions 
ggplotly(ggplot(data.numeric,aes(x=Lot_Size,y=..density..))+
           geom_histogram(color="lightblue1",fill="lightcyan")+
           geom_density(color="lightblue")+
           ggtitle("Lot_size distribution")+
           theme_light())

#the data looks highly skewed towards the right
skewness(data.numeric$Lot_Size)
#12.1908

#checking the spread of dat to detect outliers in the data
ggplotly(ggplot()+geom_point(aes(x=data.numeric$Lot_Size,y=data.numeric$Sale_Price),
                             color="lightblue")+
           ggtitle("Lot_Size Vs Sale_Price")+
           xlab("Lot_Size")+
           ylab("Sale_Price")+
           theme_light()
)

#there are four outliers:
#lotSize:115149,159000,164660,215245

#since the data is skewed taking median for imputation of outliers to remove outliers
median(data.numeric$Lot_Size)
#9477
index<-data.numeric$Lot_Size %in% c(115149,159000,164660,215245)
count=0
for(i in index){
  if(i==TRUE){
    count=count+1
  }
}
count
data.numeric$Lot_Size<-ifelse(index,9477,data.numeric$Lot_Size)

#checking the distributions and spread after outlier removal


#checking the data distributions 
ggplotly(ggplot(data.numeric,aes(x=Lot_Size,y=..density..))+
           geom_histogram(color="lightblue1",fill="lightcyan")+
           geom_density(color="lightblue")+
           ggtitle("Lot_size distribution")+
           theme_light())

#the data looks highly skewed towards the right
skewness(data.numeric$Lot_Size)
#4.1357

#checking the spread of dat to detect outliers in the data
ggplotly(ggplot()+geom_point(aes(x=data.numeric$Lot_Size,y=data.numeric$Sale_Price),
                             color="lightblue")+
           geom_vline(xintercept = 40001)+
           ggtitle("Lot_Size Vs Sale_Price")+
           xlab("Lot_Size")+
           ylab("Sale_Price")+
           theme_light()
)
#even now point abobe 40,000 seem to be outliers
#imputing them with 9477
index<-data.numeric$Lot_Size>40000
count=0
for(i in index){
  if(i == TRUE){
    count=count+1
  }
}
count
#imputing 10 outliers
data.numeric$Lot_Size<-if_else(index,9477,data.numeric$Lot_Size)

#checking distribution after outlier removal
ggplotly(ggplot(data.numeric,aes(x=Lot_Size,y=..density..))+
           geom_histogram(color="lightblue1",fill="lightcyan")+
           geom_density(color="lightblue")+
           ggtitle("Lot_size distribution")+
           theme_light())

#the data looks highly skewed towards the right
skewness(data.numeric$Lot_Size)
#1.716153

#checking the spread of dat to detect outliers in the data
ggplotly(ggplot()+geom_point(aes(x=data.numeric$Lot_Size,y=data.numeric$Sale_Price),
                             color="lightblue")+
           geom_vline(xintercept = 30000)+
           ggtitle("Lot_Size Vs Sale_Price")+
           xlab("Lot_Size")+
           ylab("Sale_Price")+
           theme_light()
)
#still there are point above 30000 which seem like outliers
index<-data.numeric$Lot_Size>30000
count=0
for(i in index){
  if(i==TRUE){
    count=count+1
  }
}
count
#8 outliers are present.
#imputing those with median
data.numeric$Lot_Size<-ifelse(index,9477,data.numeric$Lot_Size)

#checling distribution again after removing extreme points from the graph

#checking distribution after outlier removal
ggplotly(ggplot(data.numeric,aes(x=Lot_Size,y=..density..))+
           geom_histogram(color="lightblue1",fill="lightcyan")+
           geom_density(color="lightblue")+
           ggtitle("Lot_size distribution")+
           theme_light())

#the data looks highly skewed towards the right
skewness(data.numeric$Lot_Size)
#0.9363487

#checking the spread of dat to detect outliers in the data
ggplotly(ggplot()+geom_point(aes(x=data.numeric$Lot_Size,y=data.numeric$Sale_Price),
                             color="lightblue")+
           ggtitle("Lot_Size Vs Sale_Price")+
           xlab("Lot_Size")+
           ylab("Sale_Price")+
           theme_light()
)


colnames(data.numeric)[3]

#***********************************Brick_Veneer_Area*************************************************

ggplotly(ggplot(data.numeric,aes(x=Brick_Veneer_Area,y=..density..))+
           geom_histogram(color="lightblue1",fill="lightcyan")+
           geom_density(color="lightblue")+
           ggtitle("Brick Veneer Area Data distribution")+
           xlab("Brick Veneer Area")+
           theme_light())

skewness(data.numeric$Brick_Veneer_Area)
#2.670936

ggplotly(ggplot(data.numeric,aes(x=Brick_Veneer_Area,y=Sale_Price))+
           geom_point(color="lightblue1")+
           xlab("Brick Veneer Area")+
           ylab("Sale_Price")+
           ggtitle("Brick Veneer Area Vs Sale Price")+
           theme_light()+
           geom_vline(xintercept = 1250))

#from the graph we can see that there are two outliers
#brick veneer area=1378,1600
#as data is skewed we take the median of the data
median(data.numeric$Brick_Veneer_Area)
#since median of data is 0 we take mean
mean(data.numeric$Brick_Veneer_Area)
#103.8184

index<-data.numeric$Brick_Veneer_Area %in% c(1378,1600)
count=0
for(i in index){
  if(i == TRUE){
    count=count+1
  }
}
count
#2
data.numeric$Brick_Veneer_Area<-ifelse(index,103.8184,data.numeric$Brick_Veneer_Area)

#checking distribution of data after outlier imputation
ggplotly(ggplot(data.numeric,aes(x=Brick_Veneer_Area,y=..density..))+
           geom_histogram(color="lightblue1",fill="lightcyan")+
           geom_density(color="lightblue")+
           ggtitle("Brick Veneer Area Data distribution")+
           xlab("Brick Veneer Area")+
           theme_light())

skewness(data.numeric$Brick_Veneer_Area)
#2.349778

ggplotly(ggplot(data.numeric,aes(x=Brick_Veneer_Area,y=Sale_Price))+
           geom_point(color="lightblue1")+
           xlab("Brick Veneer Area")+
           ylab("Sale_Price")+
           ggtitle("Brick Veneer Area Vs Sale Price")+
           theme_light()
)


#The data looks okay without any outliers now

colnames(data.numeric)[4]

#************************************** BsmtFinSF1***************************************************

ggplotly(ggplot(data.numeric,aes(x=BsmtFinSF1,y=..density..))+
           geom_histogram(color="lightblue1",fill="lightcyan")+
           geom_density(color="lightblue")+
           ggtitle("BsmtFinSF1 data distribution")+
           xlab("BsmtFinSF1")+
           theme_light())
skewness(data.numeric$BsmtFinSF1)
#1.685763

ggplotly(ggplot(data.numeric,aes(x=BsmtFinSF1,y=Sale_Price))+
           geom_point(color="lightblue")+
           ggtitle("Sale_Price Vs BsmtFinSF1")+
           xlab("Type 1 finished square feet")+
           ylab("Sale Price")+
           theme_light())
#there is one extreme outlier 5644
mean(data.numeric$BsmtFinSF1)
#443.3749
index<-data.numeric$BsmtFinSF1==5644
count=0
for(i in index){
  if (i==TRUE){
    count=count+1
  }
}
count
skewness(data.numeric$BsmtFinSF1)
#1.685763
#therefore using mediab to impute the values
median(data.numeric$BsmtFinSF1)
#383
data.numeric$BsmtFinSF1<-ifelse(index,383,data.numeric$BsmtFinSF1)

#checking plots an distributions after removal of extreme outliers
ggplotly(ggplot(data.numeric,aes(x=BsmtFinSF1,y=..density..))+
           geom_histogram(color="lightblue1",fill="lightcyan")+
           geom_density(color="lightblue")+
           ggtitle("BsmtFinSF1 data distribution")+
           xlab("BsmtFinSF1")+
           theme_light())
skewness(data.numeric$BsmtFinSF1)
#0.7944117

ggplotly(ggplot(data.numeric,aes(x=BsmtFinSF1,y=Sale_Price))+
           geom_point(color="lightblue")+
           ggtitle("Sale_Price Vs BsmtFinSF1")+
           xlab("Type 1 finished square feet")+
           ylab("Sale Price")+
           theme_light())

colnames(data.numeric)[5]

#****************************************BsmtUnfSF***************************************************

ggplotly(ggplot(data.numeric,aes(x=BsmtUnfSF,y=..density..))+
           geom_histogram(color="lightblue1",fill="lightcyan")+
           geom_density(color="lightblue")+
           ggtitle("data distribution of Unfinished square feet of basement area")+
           xlab("Unfinished square feet of basement area")+
           theme_light())

skewness(data.numeric$BsmtUnfSF)
#0.9185394

ggplotly(ggplot(data.numeric,aes(x=BsmtUnfSF,y=Sale_Price))+
           geom_point(color="lightblue")+
           ggtitle("Sale Price vs  Unfinished square \n feet of basement area")+
           xlab("Unfinished square feet of basement area")+
           ylab("Sale_Price")+
           theme_light())

#there seem to be no outliers in BsmtUnfSF

colnames(data.numeric)[6]
#**********************************Total_Basement_Area************************************************

ggplotly(ggplot(data.numeric,aes(x=data.numeric$Total_Basement_Area,y=..density..))+
           geom_histogram(color="lightblue1",fill="lightcyan")+
           geom_density(color="lightblue")+
           ggtitle("data distribution of total basement area")+
           xlab("total basement area")+
           theme_light())

skewness(data.numeric$Total_Basement_Area)
#1.523355

ggplotly(ggplot(data.numeric,aes(x=Total_Basement_Area,y=Sale_Price))+
           geom_point(color="lightblue")+
           ggtitle("Sale Price vs  Total basement area")+
           xlab("Total basement area")+
           ylab("Sale_Price")+
           theme_light())
#there is one extreme outlier at 6110.
#imputing the outlier with median since data is skewed
index<-data.numeric$Total_Basement_Area==6110
count=0
for(i in index){
  if(i == TRUE){
    count=count+1
  }
}
count
median(data.numeric$Total_Basement_Area)
#991
data.numeric$Total_Basement_Area<-ifelse(index,991,data.numeric$Total_Basement_Area)
str(data.numeric$Total_Basement_Area)
#1459,13

#checking the distribution after removing the outlier
ggplotly(ggplot(data.numeric,aes(x=Total_Basement_Area,y=..density..))+
           geom_histogram(color="lightblue1",fill="lightcyan")+
           geom_density(color="lightblue")+
           ggtitle("data distribution of total basement area")+
           xlab("total basement area")+
           theme_light())

skewness(data.numeric$Total_Basement_Area)
#0.5743945

ggplotly(ggplot(data.numeric,aes(x=Total_Basement_Area,y=Sale_Price))+
           geom_point(color="lightblue")+
           ggtitle("Sale Price vs  Total basement area")+
           xlab("Total basement area")+
           ylab("Sale_Price")+
           geom_vline(xintercept = 2800)+
           theme_light())

#there are 4 more outliers above 3000
#3094,3200,3206,3138
index<-data.numeric$Total_Basement_Area>2800
count=0
for(i in index){
  if(i==TRUE){
    count=count+1
  }
}
count
data.numeric$Total_Basement_Area[index]
#imputing by median 991
data.numeric$Total_Basement_Area<-ifelse(index,991,data.numeric$Total_Basement_Area)

#checking the plot after removal of moderate outliers
ggplotly(ggplot(data.numeric,aes(x=Total_Basement_Area,y=..density..))+
           geom_histogram(color="lightblue1",fill="lightcyan")+
           geom_density(color="lightblue")+
           ggtitle("data distribution of total basement area")+
           xlab("total basement area")+
           theme_light())

skewness(data.numeric$Total_Basement_Area)
#0.293845

ggplotly(ggplot(data.numeric,aes(x=Total_Basement_Area,y=Sale_Price))+
           geom_point(color="lightblue")+
           ggtitle("Sale Price vs  Total basement area")+
           xlab("Total basement area")+
           ylab("Sale_Price")+
           theme_light())


colnames(data.numeric)[7]

#*****************************************TotalFloorArea********************************************

ggplotly(ggplot(data.numeric,aes(x=TotalFloorArea,y=..density..))+
           geom_histogram(color="lightblue1",fill="lightcyan")+
           geom_density(color="lightblue")+
           ggtitle("data distribution of total floor area")+
           xlab("Total floor area")+
           theme_light())
skewness(data.numeric$TotalFloorArea)
#1.328049

ggplotly(ggplot(data.numeric,aes(x=TotalFloorArea,y=Sale_Price))+
           geom_point(color="lightblue")+
           ggtitle("Sale Price Vs Total Floor Area")+
           xlab("Total Floor Area")+ylab("Sale Price")+
           theme_light())
#there looks like two extreme outliers : 4676,5642

index<-data.numeric$TotalFloorArea %in% c(4676,5642)
count=0
for(i in index){
  if(i==TRUE){
    count=count+1
  }
}
count
#since data is skewed we will impute the outlier with the median
median(data.numeric$TotalFloorArea)
#1458
data.numeric$TotalFloorArea<-ifelse(index,1458,data.numeric$TotalFloorArea)

#ploting the graphs again to check for moderate outliers
ggplotly(ggplot(data.numeric,aes(x=TotalFloorArea,y=..density..))+
           geom_histogram(color="lightblue1",fill="lightcyan")+
           geom_density(color="lightblue")+
           ggtitle("data distribution of total floor area")+
           xlab("Total floor area")+
           theme_light())
skewness(data.numeric$TotalFloorArea)
#0.9563372

ggplotly(ggplot(data.numeric,aes(x=TotalFloorArea,y=Sale_Price))+
           geom_point(color="lightblue")+
           ggtitle("Sale Price Vs Total Floor Area")+
           xlab("Total Floor Area")+ylab("Sale Price")+
           theme_light())

#there are two points 4316 and 4476 but they are in the direction of the relationship with saleprice
#therefore retaining those moderate outliers

index=data.numeric$TotalFloorArea>4000
count=0
for(i in index){
  if(i==TRUE){
    count=count+1
  }
}
count
data.numeric$TotalFloorArea<-ifelse(index,1458,data.numeric$TotalFloorArea)


#ploting the graphs again to check for moderate outliers
ggplotly(ggplot(data.numeric,aes(x=TotalFloorArea,y=..density..))+
           geom_histogram(color="lightblue1",fill="lightcyan")+
           geom_density(color="lightblue")+
           ggtitle("data distribution of total floor area")+
           xlab("Total floor area")+
           theme_light())
skewness(data.numeric$TotalFloorArea)
#0.7700989

ggplotly(ggplot(data.numeric,aes(x=TotalFloorArea,y=Sale_Price))+
           geom_point(color="lightblue")+
           ggtitle("Sale Price Vs Total Floor Area")+
           xlab("Total Floor Area")+ylab("Sale Price")+
           theme_light())

colnames(data.numeric)[8]
#*******************************************Grade Living area*****************************************

ggplotly(ggplot(data.numeric,aes(x=Grade_Living_Area,y=..density..))+
           geom_histogram(color="lightblue1",fill="lightcyan")+
           geom_density(color="lightblue")+
           ggtitle("data distribution of Grade living area")+
           xlab("Grade Living area")+
           theme_light())
skewness(data.numeric$Grade_Living_Area)
#1.364098

ggplotly(ggplot(data.numeric,aes(x=Grade_Living_Area,y=Sale_Price))+
           geom_point(color="lightblue")+
           ggtitle("Sale Price Vs Grade Living Area")+
           xlab("Grade Living Area")+ylab("Sale Price")+
           theme_light())
#There are two extreme outliers : 4676,5642
#Since grade Living area is equal to first floor area + second floor area
#we remove from analysis calculated totalfloorarea
index<-data.numeric$Grade_Living_Area >4000
count=0
for(i in index){
  if(i==TRUE){
    count=count+1
  }
}
count
#since data is skewed we will impute the outlier with the median
median(data.numeric$Grade_Living_Area)
#1464
data.numeric$Grade_Living_Area<-ifelse(index,1464,data.numeric$Grade_Living_Area)

#ploting the graphs again to check for moderate outliers

ggplotly(ggplot(data.numeric,aes(x=Grade_Living_Area,y=..density..))+
           geom_histogram(color="lightblue1",fill="lightcyan")+
           geom_density(color="lightblue")+
           ggtitle("data distribution of Grade living area")+
           xlab("Grade Living area")+
           theme_light())
skewness(data.numeric$Grade_Living_Area)
#0.8351375

ggplotly(ggplot(data.numeric,aes(x=Grade_Living_Area,y=Sale_Price))+
           geom_point(color="lightblue")+
           ggtitle("Sale Price Vs Grade Living Area")+
           xlab("Grade Living Area")+ylab("Sale Price")+
           theme_light())
#there are two points 4316 and 4476 but they are in the direction of the relationship with saleprice
#therefore retaining those moderate outliers
#removing total floor area from analysis
data.numeric[,c("TotalFloorArea")]<-NULL
colnames(data.numeric)[8]

#***************************************LobbyArea*************************************************

ggplotly(ggplot(data.numeric,aes(x=LobbyArea,y=..density..))+
           geom_histogram(color="lightblue1",fill="lightcyan")+
           geom_density(color="lightblue")+
           ggtitle("data distribution of Lobby area")+
           xlab("Lobby area")+
           theme_light())
skewness(data.numeric$LobbyArea)
#3.765599

ggplotly(ggplot(data.numeric,aes(x=LobbyArea,y=Sale_Price))+
           geom_point(color="lightblue")+
           ggtitle("Sale Price Vs Lobby Area")+
           xlab("Lobby Area")+ylab("Sale Price")+
           theme_light())
#there seem to be no outliers

colnames(data.numeric)[9]
#********************************* Pool Area ********************************************************

ggplotly(ggplot(data.numeric,aes(x=Pool_Area,y=..density..))+
           geom_histogram(color="lightblue1",fill="lightcyan")+
           geom_density(color="lightblue")+
           ggtitle("data distribution of Pool area")+
           xlab("Pool area")+
           theme_light())
skewness(data.numeric$Pool_Area)
#14.80799

ggplotly(ggplot(data.numeric,aes(x=Pool_Area,y=Sale_Price))+
           geom_point(color="lightblue")+
           ggtitle("Sale Price Vs Pool Area")+
           xlab("Pool Area")+ylab("Sale Price")+
           theme_light())
cor(data.numeric$Sale_Price,data.numeric$Pool_Area)
#as most of the points are at zero and only 7 points are above 400.
#removing the column from analysis as it is biased.
data.numeric[,c("Pool_Area")]<-NULL

colnames(data.numeric)[10]

#************************************* NewnessAge *************************************************
ggplotly(ggplot(data.numeric,aes(x=NewnessAge,y=..density..))+
           geom_histogram(color="lightblue1",fill="lightcyan")+
           geom_density(color="lightblue")+
           ggtitle("data distribution of how new is the house")+
           xlab("Newness Age")+
           theme_light())
skewness(data.numeric$NewnessAge)
#0.5036574

ggplotly(ggplot(data.numeric,aes(x=NewnessAge,y=Sale_Price))+
           geom_point(color="lightblue")+
           ggtitle("Sale Price Vs how new is the house")+
           xlab("how new is the house")+ylab("Sale Price")+
           theme_light())
#there are few : 11,12,44 values of moderate outliers
#imputing those with mean as data is equally distributed on the axis

mean(data.numeric$NewnessAge)
#22.93626

index<-data.numeric$NewnessAge==44 & data.numeric$Sale_Price>612.3724

count=0
for(i in index){
  if(i == TRUE){
    count=count+1
  }
}
count
data.numeric$Sale_Price[index]

data.numeric$NewnessAge<-ifelse(index,22.93626,data.numeric$NewnessAge)
#plotting the distribution after removing moderate outliers
ggplotly(ggplot(data.numeric,aes(x=NewnessAge,y=..density..))+
           geom_histogram(color="lightblue1",fill="lightcyan")+
           geom_density(color="lightblue")+
           ggtitle("data distribution of how new is the house")+
           xlab("Newness Age")+
           theme_light())
skewness(data.numeric$NewnessAge)
#0.5038414

ggplotly(ggplot(data.numeric,aes(x=NewnessAge,y=Sale_Price))+
           geom_point(color="lightblue")+
           ggtitle("Sale Price Vs how new is the house")+
           xlab("how new is the house")+ylab("Sale Price")+
           theme_light())
#there is one outlier at newnessage=22.93626 and sale_price=90.65368

#now the dats looks good with no extreme values in the graph

colnames(data.numeric)[11]

#**************************************OldAge********************************************************

#plotting the distribution 
ggplotly(ggplot(data.numeric,aes(x=OldAge,y=..density..))+
           geom_histogram(color="lightblue1",fill="lightcyan")+
           geom_density(color="lightblue")+
           ggtitle("data distribution of how old is the house")+
           xlab("Old Age")+
           theme_light())
skewness(data.numeric$OldAge)
#0.6086563

ggplotly(ggplot(data.numeric,aes(x=OldAge,y=Sale_Price))+
           geom_point(color="lightblue")+
           ggtitle("Sale Price Vs how old is the house")+
           xlab("how old is the house")+ylab("Sale Price")+
           theme_light())
#there are four outliers:114,115,128,129
#withsale price:78.02454,68.75344,66.56930,64.31058

index=data.numeric$OldAge==114 & data.numeric$Sale_Price>689.2024 |
  data.numeric$OldAge==115 & data.numeric$Sale_Price >570.0877|
  data.numeric$OldAge==128 & data.numeric$Sale_Price>543.1390|
  data.numeric$OldAge==129 & data.numeric$Sale_Price>515.7315
data.numeric$OldAge[index]

mean(data.numeric$OldAge)
#36.54352
median(data.numeric$OldAge)
#35
#as data is only slightly skewed imputing with mean value
data.numeric$OldAge<-ifelse(index,36.54352,data.numeric$OldAge)

#plotting the distributions after removal of outliers
ggplotly(ggplot(data.numeric,aes(x=OldAge,y=..density..))+
           geom_histogram(color="lightblue1",fill="lightcyan")+
           geom_density(color="lightblue")+
           ggtitle("data distribution of how old is the house")+
           xlab("Old Age")+
           theme_light())
skewness(data.numeric$OldAge)
#0.5884392

ggplotly(ggplot(data.numeric,aes(x=OldAge,y=Sale_Price))+
           geom_point(color="lightblue")+
           ggtitle("Sale Price Vs how old is the house")+
           xlab("how old is the house")+ylab("Sale Price")+
           theme_light())

#the data looks good with no outliers now.
colnames(data.numeric)


#it is an observation that all the columns had a positive linear relationship with the 
#target variable price and NewnessAge and Oldage column has negative linear relationship with the 
#target variable
#as seen from the graph all the variables are correlated to Sale_price with a cofficient greater
#than 5%
#therefore we will use linear regression for consideration for model building

#***************************************************************************************************

#*****************************  End of Numeric variable analysis for EDA ****************************

#***************************************************************************************************

#removing temporary variables
rm(count,i,index)
rm(cormat)

#***************************************************************************************************

#                           Categorical variable data analysis

#***************************************************************************************************

data.factor=dataTrans[,sapply(dataTrans,is.factor)]
dim(data.factor)
#44columns


colnames(data.factor)[1]

ggplot(data=data.factor,aes(x=Building_Class,color=Building_Class,fill=Building_Class))+
  geom_bar()+
  theme_light()+
  xlab("Building_Class")+
  ylab("Count")+
  ggtitle("Distribution of data across Building_Class")
#we can see that most of the records belong to the 20 category>60>50 then rest.
table(data.factor$Building_Class)

colnames(data.factor)[2]
ggplot(data=data.factor,aes(x=Zoning_Class,color=Zoning_Class,fill=Zoning_Class))+
  geom_bar()+
  theme_light()+
  xlab("Zoning_Class")+
  ylab("Count")+
  ggtitle("Distribution of data across Zoning_Class")
#we can see that a great proportion  of the records 
#belong to the RLD category>>RMD>> then rest.
table(data.factor$Zoning_Class)

colnames(data.factor)[3]
ggplot(data=data.factor,aes(x=Lane_Type,color=Lane_Type,fill=Lane_Type))+
  geom_bar()+
  theme_light()+
  xlab("Lane_Type")+
  ylab("Count")+
  ggtitle("Distribution of data across Lane_Type")
#we can see that a great proportion  of the records 
#belong to the no alley access category>> then rest.
table(data.factor$Lane_Type)
1368/1459
#93.76%data belongs to one category.
#Therefore removing the variable from analysis.
data.factor[,c("Lane_Type")]<-NULL

colnames(data.factor)[3]
ggplot(data=data.factor,aes(x=Property_Shape,color=Property_Shape,fill=Property_Shape))+
  geom_bar()+
  theme_light()+
  xlab("Property_Shape")+
  ylab("Count")+
  ggtitle("Distribution of data across Property_Shape")
#we can see that a great proportion  of the records 
#belong to the reg then more than half to the IR1 category and very few to the rest categories.
table(data.factor$Property_Shape)

colnames(data.factor)[4]
ggplot(data=data.factor,aes(x=Land_Outline,color=Land_Outline,fill=Land_Outline))+
  geom_bar()+
  theme_light()+
  xlab("Land_Outline")+
  ylab("Count")+
  ggtitle("Distribution of data across Land_Outline")
#we can see that a great proportion  of the records 
#Lvl category and significantly few records belong to the rest categories.
#therefore we need to check the variable for 90% bias
table(data.factor$Land_Outline)
1310/1459
#89.7875<90%
#as the bais is less than 90% retaining the variable.

colnames(data.factor)[5]
ggplot(data=data.factor,aes(x=Lot_Configuration,
                            color=Lot_Configuration,
                            fill=Lot_Configuration))+
  geom_bar()+
  theme_light()+
  xlab("Lot_Configuration")+
  ylab("Count")+
  ggtitle("Distribution of data across Lot_Configuration")
#we can see that a great proportion  of the records 
#belong to the I category and rest records are distributed in a decreasing order
#to the rest categories.
#therefore we need to check the variable for 90% bias
table(data.factor$Lot_Configuration)
1051/1459
#72.03%

#we retain the variable as its bais is less than 90% threshold


colnames(data.factor)[6]
ggplotly(ggplot(data=data.factor,aes(x=Neighborhood,
                            color=Neighborhood,
                            fill=Neighborhood))+
  geom_bar()+
  theme_light()+
  xlab("Neighborhood")+
  ylab("Count")+
  ggtitle("Distribution of data across Neighborhood"))
#the data is distributed amongst all categories but few categories have in the range
#225 and the lowest is 2 records in Blueste category.
table(data.factor$Neighborhood)


colnames(data.factor)[7]
ggplotly(ggplot(data=data.factor,aes(x=Condition1,
                                     color=Condition1,
                                     fill=Condition1))+
           geom_bar()+
           theme_light()+
           xlab("Condition1")+
           ylab("Count")+
           ggtitle("Distribution of data across Condition1"))
#a great proportion of values(1210) are in the Norm category and very few in other categroies where 
#the remaining count is fairly distributed amongst the rest categories
table(data.factor$Condition1)
1259/1459
#86.29<90% Therefore retaining the column.


colnames(data.factor)[8]
ggplotly(ggplot(data=data.factor,aes(x=House_Type,
                                     color=House_Type,
                                     fill=House_Type))+
           geom_bar()+
           theme_light()+
           xlab("House_Type")+
           ylab("Count")+
           ggtitle("Distribution of data across House_Type"))
#a great proportion of values(1219) are in the 1Fam category and very few in other categroies where 
#the remaining count is fairly distributed amongst the rest categories
table(data.factor$House_Type)

colnames(data.factor)[9]
ggplotly(ggplot(data=data.factor,aes(x=House_Design,
                                     color=House_Design,
                                     fill=House_Design))+
           geom_bar()+
           theme_light()+
           xlab("House_Design")+
           ylab("Count")+
           ggtitle("Distribution of data across House_Design"))
#a great proportion of values(725) almost half 
#are in the 1Story category and more than half of that in 2Story actegory
#and very few in other categroies where 
#the remaining count is fairly distributed amongst the rest categories
table(data.factor$House_Design)


colnames(data.factor)[10]
ggplotly(ggplot(data=data.factor,aes(x=Overall_Material,
                                     color=Overall_Material,
                                     fill=Overall_Material))+
           geom_bar()+
           theme_light()+
           xlab("Overall_Material")+
           ylab("Count")+
           ggtitle("Distribution of data across Overall_Material"))
#the data is distributed amongst the categories in a decreasing count fashion with
#5-being highest at 395 and 1 being lowest at 2.
table(data.factor$Overall_Material)
#this shows that most houses are rated at 5 and very few houses are rates at 1/2/3 or 9/10
#extreme ends of the ordinal data.


colnames(data.factor)[11]
ggplotly(ggplot(data=data.factor,aes(x=House_Condition,
                                     color=House_Condition,
                                     fill=House_Condition))+
           geom_bar()+
           theme_light()+
           xlab("House_Condition")+
           ylab("Count")+
           ggtitle("Distribution of data across House_Condition"))
#the data is distributed amongst the categories in a decreasing count fashion with
#5-being highest at 821 and lowest at the two extreme ratings on the either ends.
table(data.factor$House_Condition)
#this shows that most houses are rated at 5 and very few houses are rates at 1/2 or 9
#extreme ends of the ordinal data.


colnames(data.factor)[12]
ggplotly(ggplot(data=data.factor,aes(x=Roof_Design,
                                     color=Roof_Design,
                                     fill=Roof_Design))+
           geom_bar()+
           theme_light()+
           xlab("Roof_Design")+
           ylab("Count")+
           ggtitle("Distribution of data across Roof_Design"))
#the data is largely distributed in the category Gable and 1/3 that size is i Hip while rest
#categories have data as low as close to 0.
table(data.factor$Roof_Design)
1140/1459
#78.13<90% threshold. Therefore retaining this column.

colnames(data.factor)[13]
ggplotly(ggplot(data=data.factor,aes(x=Exterior1st,
                                     color=Exterior1st,
                                     fill=Exterior1st))+
           geom_bar()+
           theme_light()+
           xlab("Exterior1st")+
           ylab("Count")+
           ggtitle("Distribution of data across Exterior1st"))
#almost half of the data is distributed in the category VinyISd 
#and rest categories have less than half of the max category value  and some are almost close 
#zero
table(data.factor$Exterior1st)

colnames(data.factor)[14]
ggplotly(ggplot(data=data.factor,aes(x=Exterior2nd,
                                     color=Exterior2nd,
                                     fill=Exterior2nd))+
           geom_bar()+
           theme_light()+
           xlab("Exterior2nd")+
           ylab("Count")+
           ggtitle("Distribution of data across Exterior2nd"))
#almost half of the data is distributed in the category VinyISd 
#and rest categories have less than half of the max category value  and some are almost close 
#zero
table(data.factor$Exterior2nd)

colnames(data.factor)[15]
ggplotly(ggplot(data=data.factor,aes(x=Brick_Veneer_Type,
                                     color=Brick_Veneer_Type,
                                     fill=Brick_Veneer_Type))+
           geom_bar()+
           theme_light()+
           xlab("Brick_Veneer_Type")+
           ylab("Count")+
           ggtitle("Distribution of data across Brick_Veneer_Type"))
#more than half of the data is distributed in the category None 
#and rest categories have less than half of the max category value  and some are almost close 
#zero
table(data.factor$Brick_Veneer_Type)

colnames(data.factor)[16]
ggplotly(ggplot(data=data.factor,aes(x=Exterior_Material,
                                     color=Exterior_Material,
                                     fill=Exterior_Material))+
           geom_bar()+
           theme_light()+
           xlab("Exterior_Material")+
           ylab("Count")+
           ggtitle("Distribution of data across Exterior_Material"))
#more than half of the data is distributed in the category TA 
#and rest categories have less than half of the max category value  and some are almost close 
#zero
table(data.factor$Exterior_Material)


colnames(data.factor)[17]
ggplotly(ggplot(data=data.factor,aes(x=Exterior_Condition,
                                     color=Exterior_Condition,
                                     fill=Exterior_Condition))+
           geom_bar()+
           theme_light()+
           xlab("Exterior_Condition")+
           ylab("Count")+
           ggtitle("Distribution of data across Exterior_Condition"))
#a great portion  of the data is distributed in the category TA 
#and rest categories have very less records in them.
table(data.factor$Exterior_Condition)

1281/1459
#87.799<90%. Therefore retaining the column.

colnames(data.factor)[18]
ggplotly(ggplot(data=data.factor,aes(x=Foundation_Type,
                                     color=Foundation_Type,
                                     fill=Foundation_Type))+
           geom_bar()+
           theme_light()+
           xlab("Foundation_Type")+
           ylab("Count")+
           ggtitle("Distribution of data across Foundation_Type"))
#almost more than half of the data is distributed in the category PC and CB 
#and rest categories have very less records in them.
table(data.factor$Foundation_Type)

colnames(data.factor)[19]
ggplotly(ggplot(data=data.factor,aes(x=Basement_Height,
                                     color=Basement_Height,
                                     fill=Basement_Height))+
           geom_bar()+
           theme_light()+
           xlab("Basement_Height")+
           ylab("Count")+
           ggtitle("Distribution of data across Basement_Height"))
#almost more than half of the data is distributed in the category Gd and TA 
#and rest categories have very less records in them.
table(data.factor$Basement_Height)

colnames(data.factor)[20]
ggplotly(ggplot(data=data.factor,aes(x=Basement_Condition,
                                     color=Basement_Condition,
                                     fill=Basement_Condition))+
           geom_bar()+
           theme_light()+
           xlab("Basement_Condition")+
           ylab("Count")+
           ggtitle("Distribution of data across Basement_Condition"))
#almost all of the data is distributed in the category TA 
#and rest categories have very less records in them.
table(data.factor$Basement_Condition)
1310/1459
#89.78<90% threshold therefore retaining the column.

colnames(data.factor)[21]
ggplotly(ggplot(data=data.factor,aes(x=Exposure_Level,
                                     color=Exposure_Level,
                                     fill=Exposure_Level))+
           geom_bar()+
           theme_light()+
           xlab("Exposure_Level")+
           ylab("Count")+
           ggtitle("Distribution of data across Exposure_Level"))
#more than 3/4 of the data is distributed in the category No.
#and rest categories have comparatively less records in them.
table(data.factor$Exposure_Level)

colnames(data.factor)[22]
ggplotly(ggplot(data=data.factor,aes(x=BsmtFinType1,
                                     color=BsmtFinType1,
                                     fill=BsmtFinType1))+
           geom_bar()+
           theme_light()+
           xlab("BsmtFinType1")+
           ylab("Count")+
           ggtitle("Distribution of data across BsmtFinType1"))
#almost 1000 records are equally distributed in the categories GLQ and Unf.
#and rest categories well distributed in each category with lowest being at none=37 records.
table(data.factor$BsmtFinType1)

colnames(data.factor)[23]
ggplotly(ggplot(data=data.factor,aes(x=BsmtFinType2,
                                     color=BsmtFinType2,
                                     fill=BsmtFinType2))+
           geom_bar()+
           theme_light()+
           xlab("BsmtFinType2")+
           ylab("Count")+
           ggtitle("Distribution of data across BsmtFinType2"))
#almost 1200 records are in the category Unf.
#and rest categories well distributed in each category with lowest being at ALQ=19 records.
table(data.factor$BsmtFinType2)

1256/1459
#86.08<90%.
#therefore retaining the column.

colnames(data.factor)[24]
ggplotly(ggplot(data=data.factor,aes(x=Heating_Quality,
                                     color=Heating_Quality,
                                     fill=Heating_Quality))+
           geom_bar()+
           theme_light()+
           xlab("Heating_Quality")+
           ylab("Count")+
           ggtitle("Distribution of data across Heating_Quality"))
#almost more than half the records are in the category Ex.
#and rest categories have the records distributed in decreasing order with lowest being at Po
#at almost close to zero records

table(data.factor$Heating_Quality)


colnames(data.factor)[25]
ggplotly(ggplot(data=data.factor,aes(x=Air_Conditioning,
                                     color=Air_Conditioning,
                                     fill=Air_Conditioning))+
           geom_bar()+
           theme_light()+
           xlab("Air_Conditioning")+
           ylab("Count")+
           ggtitle("Distribution of data across Air_Conditioning"))
#almost more tham 1300 the records are in the category Y.
#and only 95 records fall in the category N.

table(data.factor$Air_Conditioning)
1364/1459
#93.488>90% Threshold of bias check. Therefore removing the column from analysis
data.factor[,c("Air_Conditioning")]<-NULL

colnames(data.factor)[25]
ggplotly(ggplot(data=data.factor,aes(x=Electrical_System,
                                     color=Electrical_System,
                                     fill=Electrical_System))+
           geom_bar()+
           theme_light()+
           xlab("Electrical_System")+
           ylab("Count")+
           ggtitle("Distribution of data across Electrical_System"))
#almost more tham 1300 the records are in the category Y.
#and remaining records are in them distributed almost equally amongst the remaining groups.

table(data.factor$Electrical_System)
1334/1459
#91.432>90% bias threshold , therefore removing this column from analysis.
data.factor[,c("Electrical_System")]<-NULL

colnames(data.factor)[25]
ggplotly(ggplot(data=data.factor,aes(x=Underground_Full_Bathroom,
                                     color=Underground_Full_Bathroom,
                                     fill=Underground_Full_Bathroom))+
           geom_bar()+
           theme_light()+
           xlab("Underground_Full_Bathroom")+
           ylab("Count")+
           ggtitle("Distribution of data across Underground_Full_Bathroom"))
#the records are distributed in a decreasing order from ) to 3.

table(data.factor$Underground_Full_Bathroom)

colnames(data.factor)[26]
ggplotly(ggplot(data=data.factor,aes(x=Underground_Half_Bathroom,
                                     color=Underground_Half_Bathroom,
                                     fill=Underground_Half_Bathroom))+
           geom_bar()+
           theme_light()+
           xlab("Underground_Half_Bathroom")+
           ylab("Count")+
           ggtitle("Distribution of data across Underground_Half_Bathroom"))
#the records are distributed in a decreasing order from ) to 2.

table(data.factor$Underground_Half_Bathroom)
1377/1459
#94.37 therefore the column is biased. Therefore removing the column from analysis.
data.factor[,c("Underground_Half_Bathroom")]<-NULL

colnames(data.factor)[26]
ggplotly(ggplot(data=data.factor,aes(x=Full_Bathroom_Above_Grade,
                                     color=Full_Bathroom_Above_Grade,
                                     fill=Full_Bathroom_Above_Grade))+
           geom_bar()+
           theme_light()+
           xlab("Full_Bathroom_Above_Grade")+
           ylab("Count")+
           ggtitle("Distribution of data across Full_Bathroom_Above_Grade"))
#the records are distributed mostly is the 1 and 2 with very few on the 0/3 catgory.

table(data.factor$Full_Bathroom_Above_Grade)

colnames(data.factor)[27]
ggplotly(ggplot(data=data.factor,aes(x=Half_Bathroom_Above_Grade,
                                     color=Half_Bathroom_Above_Grade,
                                     fill=Half_Bathroom_Above_Grade))+
           geom_bar()+
           theme_light()+
           xlab("Half_Bathroom_Above_Grade")+
           ylab("Count")+
           ggtitle("Distribution of data across Half_Bathroom_Above_Grade"))
#the records are distributed in decreasing order from 0 to 2 with very few on the 2 catgory.

table(data.factor$Half_Bathroom_Above_Grade)

colnames(data.factor)[28]
ggplotly(ggplot(data=data.factor,aes(x=Bedroom_Above_Grade,
                                     color=Bedroom_Above_Grade,
                                     fill=Bedroom_Above_Grade))+
           geom_bar()+
           theme_light()+
           xlab("Bedroom_Above_Grade")+
           ylab("Count")+
           ggtitle("Distribution of data across Bedroom_Above_Grade"))
#the records are mostly  distributed in a normal fashion centering around 3 and reducing towards the
#extreme ends of the category

table(data.factor$Bedroom_Above_Grade)

colnames(data.factor)[29]
ggplotly(ggplot(data=data.factor,aes(x=Kitchen_Quality,
                                     color=Kitchen_Quality,
                                     fill=Kitchen_Quality))+
           geom_bar()+
           theme_light()+
           xlab("Kitchen_Quality")+
           ylab("Count")+
           ggtitle("Distribution of data across Kitchen_Quality"))
#the records are mostly  distributed in Gd, TA category
#with comparatively less records in the Ex and Fa category

table(data.factor$Kitchen_Quality)



colnames(data.factor)[30]
ggplotly(ggplot(data=data.factor,aes(x=Rooms_Above_Grade,
                                     color=Rooms_Above_Grade,
                                     fill=Rooms_Above_Grade))+
           geom_bar()+
           theme_light()+
           xlab("Rooms_Above_Grade")+
           ylab("Count")+
           ggtitle("Distribution of data across Rooms_Above_Grade"))
#the records are normally centering around 6-7 and decreasing towards the extreme ends
# with very few records being at 2 and 14

table(data.factor$Rooms_Above_Grade)

colnames(data.factor)[31]
ggplotly(ggplot(data=data.factor,aes(x=Fireplaces,
                                     color=Fireplaces,
                                     fill=Fireplaces))+
           geom_bar()+
           theme_light()+
           xlab("Fireplaces")+
           ylab("Count")+
           ggtitle("Distribution of data across Fireplaces"))
#almost the data is equally distribtued in 0 and 1 category then drastically reducing towards
#2 and 3 category

table(data.factor$Fireplaces)

colnames(data.factor)[32]
ggplotly(ggplot(data=data.factor,aes(x=Fireplace_Quality,
                                     color=Fireplace_Quality,
                                     fill=Fireplace_Quality))+
           geom_bar()+
           theme_light()+
           xlab("Fireplace_Quality")+
           ylab("Count")+
           ggtitle("Distribution of data across Fireplace_Quality"))
#almost half of the records are in the no fireplaces category and TA and Gd hold almost
#equal no of records will very less records are present in the remaining categories

table(data.factor$Fireplace_Quality)

colnames(data.factor)[33]
ggplotly(ggplot(data=data.factor,aes(x=Garage,
                                     color=Garage,
                                     fill=Garage))+
           geom_bar()+
           theme_light()+
           xlab("Garage")+
           ylab("Count")+
           ggtitle("Distribution of data across Garage"))
#almost half of the records are in the attched category and half of that are there in detchd
#category while will very less records are present in the remaining categories

table(data.factor$Garage)

colnames(data.factor)[34]
ggplotly(ggplot(data=data.factor,aes(x=Garage_Size,
                                     color=Garage_Size,
                                     fill=Garage_Size))+
           geom_bar()+
           theme_light()+
           xlab("Garage_Size")+
           ylab("Count")+
           ggtitle("Distribution of data across Garage_Size"))
#the data is normally distributed from 0 to 4 category order with max being at 2 category.

table(data.factor$Garage_Size)

colnames(data.factor)[35]
ggplotly(ggplot(data=data.factor,aes(x=Garage_Quality,
                                     color=Garage_Quality,
                                     fill=Garage_Quality))+
           geom_bar()+
           theme_light()+
           xlab("Garage_Quality")+
           ylab("Count")+
           ggtitle("Distribution of data across Garage_Quality"))
#more than 1300 records are in the TA category with very less records almost equally distributed
##in the remaining categories with lowed being at EX and Po


table(data.factor$Garage_Quality)

colnames(data.factor)[36]
ggplotly(ggplot(data=data.factor,aes(x=Fence_Quality,
                                     color=Fence_Quality,
                                     fill=Fence_Quality))+
           geom_bar()+
           theme_light()+
           xlab("Fence_Quality")+
           ylab("Count")+
           ggtitle("Distribution of data across Fence_Quality"))
#more than 1100 records are in the no fence category 
#with very less records almost equally distributed
##in the remaining categories with lowed being at MnWw


table(data.factor$Fence_Quality)

colnames(data.factor)[37]
ggplotly(ggplot(data=data.factor,aes(x=Miscellaneous_Feature,
                                     color=Miscellaneous_Feature,
                                     fill=Miscellaneous_Feature))+
           geom_bar()+
           theme_light()+
           xlab("Miscellaneous_Feature")+
           ylab("Count")+
           ggtitle("Distribution of data across Miscellaneous_Feature"))
#more than 1400 records are in the no fence category 
#with very less records almost equally distributed
##in the remaining categories with lowed being at MnWw


table(data.factor$Miscellaneous_Feature)
1405/1459
#96.29>90% bias threshold. Therefore removing this columnn from analysis
data.factor[,c("Miscellaneous_Feature")]<-NULL

colnames(data.factor)[37]
ggplotly(ggplot(data=data.factor,aes(x=Month_Sold,
                                     color=Month_Sold,
                                     fill=Month_Sold))+
           geom_bar()+
           theme_light()+
           xlab("Month_Sold")+
           ylab("Count")+
           ggtitle("Distribution of data across Month_Sold"))
#the data is normally distributed with from 1-12 months with being at 6.


table(data.factor$Month_Sold)

colnames(data.factor)[38]
ggplotly(ggplot(data=data.factor,aes(x=Sale_Type,
                                     color=Sale_Type,
                                     fill=Sale_Type))+
           geom_bar()+
           theme_light()+
           xlab("Sale_Type")+
           ylab("Count")+
           ggtitle("Distribution of data across Sale_Type"))
#most of the records are present in the WD category with very few records in the
#rest categories

table(data.factor$Sale_Type)

colnames(data.factor)[39]
ggplotly(ggplot(data=data.factor,aes(x=Sale_Condition,
                                     color=Sale_Condition,
                                     fill=Sale_Condition))+
           geom_bar()+
           theme_light()+
           xlab("Sale_Condition")+
           ylab("Count")+
           ggtitle("Distribution of data across Sale_Condition"))
#most of the records are present in the Normal category with very few records in the
#rest categories

table(data.factor$Sale_Condition)

#******************************************************************************************
#             End of factor data visualisation and biased column identification
#******************************************************************************************

#********************************************************************************************
#                   Significance check for factor variables
#********************************************************************************************
colnames(data.factor)[1]
#************************************Building_Class******************************************

#checking homogeneity of variances
ggplotly(ggplot(data.factor,aes(x=Building_Class,y=data.numeric$Sale_Price,color=Building_Class))+
           geom_boxplot()+
           xlab("Building class")+ylab("Sale Price")+
           theme_light()
)

#Since the sample sizes are small in many groups we use the levene's test to check
#homogeneity of variances
leveneTest(data.numeric$Sale_Price~data.factor$Building_Class)
#therefore the variances are not equal in the group.
#therefore the anova test can't be applied to it.
kruskal.test(data.numeric$Sale_Price~data.factor$Building_Class)
#the pval is less than 0.05. and chi square critical=26.685 and the chi square for the 
#kruskal test is therefore more than chi square critical.
#therefore the two variables are related

colnames(data.factor)[2]

ggplotly(ggplot(data.factor,aes(x=Zoning_Class,y=data.numeric$Sale_Price,color=Zoning_Class))+
           geom_boxplot()+
           xlab("Zoning_Class")+ylab("Sale Price")+
           theme_light()
)

#Since the sample sizes are small in many groups we use the levene's test to check
#homogeneity of variances
leveneTest(data.numeric$Sale_Price~data.factor$Zoning_Class)
#as pval is below 0.05 therefore the variances are not equal in the group.
#therefore the anova test can't be applied to it.
kruskal.test(data.numeric$Sale_Price~data.factor$Zoning_Class)
#the pval is less than 0.05. and chi square critical=9.488 for 4 degrees of freedom 
#and the chi square for the 
#kruskal test(270.31) is therefore more than chi square critical.
#therefore the two variables are related


colnames(data.factor)[3]
ggplotly(ggplot(data.factor,
                aes(x=Property_Shape,y=data.numeric$Sale_Price,color=Property_Shape))+
           geom_boxplot()+
           xlab("Property_Shape")+ylab("Sale Price")+
           theme_light()
)

#Since the sample sizes are almost equal in all groups we use the levene's test to check
#homogeneity of variances
leveneTest(data.numeric$Sale_Price~data.factor$Property_Shape)
#as pval is more than 0.05 therefore the variances are equal in the group.
#therefore the anova test can be applied to it.
#checking for second assumption of normality.
ggplot(data=data.numeric,aes(sample=Sale_Price))+geom_qq(geom="point",position="identity")+
  facet_wrap(~data.factor$Property_Shape)
#as the data is normal in each group also.
#therefore using Anova test.

summary(aov(data.numeric$Sale_Price~data.factor$Property_Shape))
#as the pval is <0.05 therefore the variable Sale price is dependent on property shape.

colnames(data.factor)[4]
ggplotly(ggplot(data.factor,
                aes(x=Land_Outline,y=data.numeric$Sale_Price,color=Land_Outline))+
           geom_boxplot()+
           xlab("Land_Outline")+ylab("Sale Price")+
           theme_light()
)

#Since the sample sizes are small in two groups we use the levene's test to check
#homogeneity of variances
leveneTest(data.numeric$Sale_Price~data.factor$Land_Outline)
#since the variances are different across groups as pval <0.05 we us the kruskal walis test.
kruskal.test(data.numeric$Sale_Price~data.factor$Land_Outline)
#since the pval is <0.05 and chi square critical for 3 degrees of freedom is 7.815 which less 
#than chi squared value 35.94. We conclude that the variables are not independent.

colnames(data.factor)[5]
ggplotly(ggplot(data.factor,
                aes(x=Lot_Configuration,y=data.numeric$Sale_Price,color=Lot_Configuration))+
           geom_boxplot()+
           xlab("Lot_Configuration")+ylab("Sale Price")+
           theme_light()
)
#since the variances in two of the groups look smaller than the rest using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Lot_Configuration)
#since the pval(0.7208) is greater than 0.05 therefore we conclude that the variance are
#homogenous.
#checking the second assumption of anova : normal distribution
ggplot(data=data.numeric,aes(sample=Sale_Price))+
  geom_qq()+facet_wrap(~data.factor$Lot_Configuration)
#since the data is normally distribution in each group we can apply
#anova test to check the variable independence.
summary(aov(data.numeric$Sale_Price~data.factor$Lot_Configuration))
#the pval is <0.05. Therefore we reject the null hypothesis that the variables are 
#independent

colnames(data.factor)[6]
ggplotly(ggplot(data.factor,
                aes(x=Neighborhood,y=data.numeric$Sale_Price,color=Neighborhood))+
           geom_boxplot()+
           xlab("Neighborhood")+ylab("Sale Price")+
           theme_light()
)
#since the variances in many of the groups look smaller than the rest using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Neighborhood)
#since the pval is less than 0.05 therefore we conclude that the variances are not
#homogenous.
#therefore using kruskal wallis non parametric test for testing independence of variables
kruskal.test(data.numeric$Sale_Price~data.factor$Neighborhood)
#since the pval is less than 0.05 and chi square critical for 24 degrees of freedom is
#36.415 < chi squared evaluated fromt the kruskal test(848.91) 
#we conclude that the variables are not independent. We fail to accept the null hypothesis.

colnames(data.factor)[7]
ggplotly(ggplot(data.factor,
                aes(x=Condition1,y=data.numeric$Sale_Price,color=Condition1))+
           geom_boxplot()+
           xlab("Condition1")+ylab("Sale Price")+
           theme_light()
)
#since the variances in many of the groups look smaller than the rest using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Condition1)
#since the pval is less than 0.05 therefore we conclude that the variances are not
#homogenous.
#therefore using kruskal wallis non parametric test for testing independence of variables
kruskal.test(data.numeric$Sale_Price~data.factor$Condition1)
#since the pval is less than 0.05 and chi square critical for 8 degrees of freedom is
#15.507 < chi squared evaluated fromt the kruskal test(74.4) 
#we conclude that the variables are not independent. We fail to accept the null hypothesis.


colnames(data.factor)[8]
ggplotly(ggplot(data.factor,
                aes(x=House_Type,y=data.numeric$Sale_Price,color=House_Type))+
           geom_boxplot()+
           xlab("House_Type")+ylab("Sale Price")+
           theme_light()
)
#since the variances look different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$House_Type)
#since the pval is less than 0.05 therefore we conclude that the variances are not
#homogenous.
#therefore using kruskal wallis non parametric test for testing independence of variables
kruskal.test(data.numeric$Sale_Price~data.factor$House_Type)
#since the pval is less than 0.05 and chi square critical for 4 degrees of freedom is
#9.488 < chi squared evaluated fromt the kruskal test(69.408) 
#we conclude that the variables are not independent. We fail to accept the null hypothesis.

colnames(data.factor)[9]

ggplotly(ggplot(data.factor,
                aes(x=House_Design,y=data.numeric$Sale_Price,color=House_Design))+
           geom_boxplot()+
           xlab("House_Design")+ylab("Sale Price")+
           theme_light()
)
#since the variances look different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$House_Design)
#since the pval is less than 0.05 therefore we conclude that the variances are not
#homogenous.
#therefore using kruskal wallis non parametric test for testing independence of variables
kruskal.test(data.numeric$Sale_Price~data.factor$House_Design)
#since the pval is less than 0.05 and chi square critical for 7 degrees of freedom is
#14.067 < chi squared evaluated fromt the kruskal test(181.26) 
#we conclude that the variables are not independent. We fail to accept the null hypothesis.

colnames(data.factor)[10]
ggplotly(ggplot(data.factor,
                aes(x=Overall_Material,y=data.numeric$Sale_Price,color=Overall_Material))+
           geom_boxplot()+
           xlab("Overall_Material")+ylab("Sale Price")+
           theme_light()
)
#since the variances look different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Overall_Material)
#since the pval is less than 0.05 therefore we conclude that the variances are not
#homogenous.
#therefore using kruskal wallis non parametric test for testing independence of variables
kruskal.test(data.numeric$Sale_Price~data.factor$Overall_Material)
#since the pval is less than 0.05 and chi square critical for 9 degrees of freedom is
#16.919 < chi squared evaluated fromt the kruskal test(945.33) 
#we conclude that the variables are not independent. We fail to accept the null hypothesis.

colnames(data.factor)[11]
ggplotly(ggplot(data.factor,
                aes(x=House_Condition,y=data.numeric$Sale_Price,color=House_Condition))+
           geom_boxplot()+
           xlab("House_Condition")+ylab("Sale Price")+
           theme_light()
)
#since the variances look different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$House_Condition)
#since the pval is less than 0.05 therefore we conclude that the variances are not
#homogenous.
#therefore using kruskal wallis non parametric test for testing independence of variables
kruskal.test(data.numeric$Sale_Price~data.factor$House_Condition)
#since the pval is less than 0.05 and chi square critical for 8 degrees of freedom is
#15.507 < chi squared evaluated fromt the kruskal test(236.83) 
#we conclude that the variables are not independent. We fail to accept the null hypothesis.

colnames(data.factor)[12]
ggplotly(ggplot(data.factor,
                aes(x=Roof_Design,y=data.numeric$Sale_Price,color=Roof_Design))+
           geom_boxplot()+
           xlab("Roof_Design")+ylab("Sale Price")+
           theme_light()
)
#since the variances look different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Roof_Design)
#since the pval is less than 0.05 therefore we conclude that the variances are not
#homogenous.
#therefore using kruskal wallis non parametric test for testing independence of variables
kruskal.test(data.numeric$Sale_Price~data.factor$Roof_Design)
#since the pval is less than 0.05 and chi square critical for 5 degrees of freedom is
#11.07 < chi squared evaluated fromt the kruskal test(34.241) 
#we conclude that the variables are not independent. We fail to accept the null hypothesis.


colnames(data.factor)[13]
ggplotly(ggplot(data.factor,
                aes(x=Exterior1st,y=data.numeric$Sale_Price,color=Exterior1st))+
           geom_boxplot()+
           xlab("Exterior1st")+ylab("Sale Price")+
           theme_light()
)
#since the variances look different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Exterior1st)
#since the pval is less than 0.05 therefore we conclude that the variances are not
#homogenous.
#therefore using kruskal wallis non parametric test for testing independence of variables
kruskal.test(data.numeric$Sale_Price~data.factor$Exterior1st)
#since the pval is less than 0.05 and chi square critical for 14 degrees of freedom is
#23.685 < chi squared evaluated fromt the kruskal test(305.07) 
#we conclude that the variables are not independent. We fail to accept the null hypothesis.

colnames(data.factor)[14]
ggplotly(ggplot(data.factor,
                aes(x=Exterior2nd,y=data.numeric$Sale_Price,color=Exterior2nd))+
           geom_boxplot()+
           xlab("Exterior2nd")+ylab("Sale Price")+
           theme_light()
)
#since the variances look different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Exterior2nd)
#since the pval is less than 0.05 therefore we conclude that the variances are not
#homogenous.
#therefore using kruskal wallis non parametric test for testing independence of variables
kruskal.test(data.numeric$Sale_Price~data.factor$Exterior2nd)
#since the pval is less than 0.05 and chi square critical for 15 degrees of freedom is
#24.996 < chi squared evaluated fromt the kruskal test(282.04) 
#we conclude that the variables are not independent. We fail to accept the null hypothesis.



colnames(data.factor)[15]
ggplotly(ggplot(data.factor,
                aes(x=Brick_Veneer_Type,y=data.numeric$Sale_Price,color=Brick_Veneer_Type))+
           geom_boxplot()+
           xlab("Brick_Veneer_Type")+ylab("Sale Price")+
           theme_light()
)
#since the variances looks slightly different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Brick_Veneer_Type)
#since the pval is less than 0.05 therefore we conclude that the variances are not
#homogenous.
#therefore using kruskal wallis non parametric test for testing independence of variables
kruskal.test(data.numeric$Sale_Price~data.factor$Brick_Veneer_Type)
#since the pval is less than 0.05 and chi square critical for 3 degrees of freedom is
#7.815 < chi squared evaluated fromt the kruskal test(269.58) 
#we conclude that the variables are not independent. We fail to accept the null hypothesis.



colnames(data.factor)[16]
ggplotly(ggplot(data.factor,
                aes(x=Exterior_Material,y=data.numeric$Sale_Price,color=Exterior_Material))+
           geom_boxplot()+
           xlab("Exterior_Material")+ylab("Sale Price")+
           theme_light()
)
#since the variances looks slightly different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Exterior_Material)
#since the pval is less than 0.05 therefore we conclude that the variances are not
#homogenous.
#therefore using kruskal wallis non parametric test for testing independence of variables
kruskal.test(data.numeric$Sale_Price~data.factor$Exterior_Material)
#since the pval is less than 0.05 and chi square critical for 3 degrees of freedom is
#7.815 < chi squared evaluated fromt the kruskal test(666.59) 
#we conclude that the variables are not independent. We fail to accept the null hypothesis.



colnames(data.factor)[17]
ggplotly(ggplot(data.factor,
                aes(x=Exterior_Condition,y=data.numeric$Sale_Price,color=Exterior_Condition))+
           geom_boxplot()+
           xlab("Exterior_Condition")+ylab("Sale Price")+
           theme_light()
)
#since the variances looks different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Exterior_Condition)
#since the pval is greater than 0.05 therefore we conclude that the variances are homogenous
#as the fval(2.1977) < f critical fro df(4,1454)=2.3719

#therefore we test the second assumption of normality
ggplot(data=data.numeric,aes(sample=Sale_Price))+
  geom_qq()+
  facet_wrap(data.factor$Exterior_Condition)
#as the data is normally distributed in the groups we apply the anova test

summary(aov(data.numeric$Sale_Price~data.factor$Exterior_Condition))
#as the pval is less than 0.05 we conclude that the variables are not independent
#and fail to accept the null hypothesis.




colnames(data.factor)[18]
ggplotly(ggplot(data.factor,
                aes(x=Foundation_Type,y=data.numeric$Sale_Price,color=Foundation_Type))+
           geom_boxplot()+
           xlab("Foundation_Type")+ylab("Sale Price")+
           theme_light()
)
#since the variances looks different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Foundation_Type)
#since the pval is less than 0.05 therefore we conclude that the variances are not homogenous
#as the fval(10.129) > f critical fro df(4,1453)=2.2141

#therefore we apply the kruskal wallis non parametric test.
kruskal.test(data.numeric$Sale_Price~data.factor$Foundation_Type)
#since the pval is less than 0.05 and chi square critical for 5 degrees of freedom is
#11.07 < chi squared evaluated fromt the kruskal test(480.65) 
#we conclude that the variables are not independent. We fail to accept the null hypothesis.

colnames(data.factor)[19]
ggplotly(ggplot(data.factor,
                aes(x=Basement_Height,y=data.numeric$Sale_Price,color=Basement_Height))+
           geom_boxplot()+
           xlab("Basement_Height")+ylab("Sale Price")+
           theme_light()
)
#since the variances looks slightly different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Basement_Height)
#since the pval is less than 0.05 therefore we conclude that the variances are not homogenous
#as the fval(14.599) > f critical fro df(4,1454)=2.3719

#therefore we apply the kruskal wallis non parametric test.
kruskal.test(data.numeric$Sale_Price~data.factor$Basement_Height)
#since the pval is less than 0.05 and chi square critical for 4 degrees of freedom is
#9.488 < chi squared evaluated fromt the kruskal test(653.11) 
#we conclude that the variables are not independent. We fail to accept the null hypothesis.


colnames(data.factor)[20]
ggplotly(ggplot(data.factor,
                aes(x=Basement_Condition,y=data.numeric$Sale_Price,color=Basement_Condition))+
           geom_boxplot()+
           xlab("Basement_Condition")+ylab("Sale Price")+
           theme_light()
)
#since the variances looks different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Basement_Condition)
#since the pval is less than 0.05 therefore we conclude that the variances are not homogenous
#as the fval(4.1837) > f critical fro df(4,1454)=2.3719

#therefore we apply the kruskal wallis non parametric test.
kruskal.test(data.numeric$Sale_Price~data.factor$Basement_Condition)
#since the pval is less than 0.05 and chi square critical for 4 degrees of freedom is
#9.488 < chi squared evaluated fromt the kruskal test(121.28) 
#we conclude that the variables are not independent. We fail to accept the null hypothesis.



colnames(data.factor)[21]
ggplotly(ggplot(data.factor,
                aes(x=Exposure_Level,y=data.numeric$Sale_Price,color=Exposure_Level))+
           geom_boxplot()+
           xlab("Exposure_Level")+ylab("Sale Price")+
           theme_light()
)
#since the variances looks different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Exposure_Level)
#since the pval is less than 0.05 therefore we conclude that the variances are not homogenous
#as the fval(11.636) > f critical for df(4,1454)=2.3719

#therefore we apply the kruskal wallis non parametric test.
kruskal.test(data.numeric$Sale_Price~data.factor$Exposure_Level)
#since the pval is less than 0.05 and chi square critical for 4 degrees of freedom is
#9.488 < chi squared evaluated fromt the kruskal test(178.04) 
#we conclude that the variables are not independent. We fail to accept the null hypothesis.




colnames(data.factor)[22]
ggplotly(ggplot(data.factor,
                aes(x=BsmtFinType1,y=data.numeric$Sale_Price,color=BsmtFinType1))+
           geom_boxplot()+
           xlab("BsmtFinType1")+ylab("Sale Price")+
           theme_light()
)
#since the variances looks slightly different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$BsmtFinType1)
#since the pval is less than 0.05 therefore we conclude that the variances are not homogenous
#as the fval(15.136) > f critical for df(6,1452)=2.0986

#therefore we apply the kruskal wallis non parametric test.
kruskal.test(data.numeric$Sale_Price~data.factor$BsmtFinType1)
#since the pval is less than 0.05 and chi square critical for 6 degrees of freedom is
#12.592 < chi squared evaluated fromt the kruskal test(339.45) 
#we conclude that the variables are not independent. We fail to accept the null hypothesis.



colnames(data.factor)[23]
ggplotly(ggplot(data.factor,
                aes(x=BsmtFinType2,y=data.numeric$Sale_Price,color=BsmtFinType2))+
           geom_boxplot()+
           xlab("BsmtFinType2")+ylab("Sale Price")+
           theme_light()
)
#since the variances looks slightly different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$BsmtFinType2)
#since the pval is less than 0.05 therefore we conclude that the variances are not homogenous
#as the fval(4.9196) > f critical for df(6,1452)=2.0986

#therefore we apply the kruskal wallis non parametric test.
kruskal.test(data.numeric$Sale_Price~data.factor$BsmtFinType2)
#since the pval is less than 0.05 and chi square critical for 6 degrees of freedom is
#12.592 < chi squared evaluated fromt the kruskal test(63.398) 
#we conclude that the variables are not independent. We fail to accept the null hypothesis.




colnames(data.factor)[24]
ggplotly(ggplot(data.factor,
                aes(x=Heating_Quality,y=data.numeric$Sale_Price,color=Heating_Quality))+
           geom_boxplot()+
           xlab("Heating_Quality")+ylab("Sale Price")+
           theme_light()
)
#since the variances looks  different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Heating_Quality)
#since the pval is less than 0.05 therefore we conclude that the variances are not homogenous
#as the fval(15.25) > f critical for df(4,1454)=2.3719
#therefore we apply the kruskal wallis non parametric test.
kruskal.test(data.numeric$Sale_Price~data.factor$Heating_Quality)
#since the pval is less than 0.05 and chi square critical for 4 degrees of freedom is
#9.488 < chi squared evaluated fromt the kruskal test(350.15) 
#we conclude that the variables are not independent. We fail to accept the null hypothesis.



colnames(data.factor)[25]
ggplotly(ggplot(data.factor,
                aes(x=Underground_Full_Bathroom,
                    y=data.numeric$Sale_Price,
                    color=Underground_Full_Bathroom))+
           geom_boxplot()+
           xlab("Underground_Full_Bathroom")+ylab("Sale Price")+
           theme_light()
)
#since the variances looks  different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Underground_Full_Bathroom)
#since the pval is less than 0.05 therefore we  that the variances are not homogenous
#as the fval(5.9724) > f critical for df(3,1455)=2.6049
#therefore we apply the kruskal wallis non parametric test.
kruskal.test(data.numeric$Sale_Price~data.factor$Underground_Full_Bathroom)
#since the pval is less than 0.05 and chi square critical for 3 degrees of freedom is
#7.815 < chi squared evaluated fromt the kruskal test(73.231) 
#we conclude that the variables are not independent. We fail to accept the null hypothesis.



colnames(data.factor)[26]
ggplotly(ggplot(data.factor,
                aes(x=Full_Bathroom_Above_Grade,
                    y=data.numeric$Sale_Price,
                    color=Full_Bathroom_Above_Grade))+
           geom_boxplot()+
           xlab("Full_Bathroom_Above_Grade")+ylab("Sale Price")+
           theme_light()
)
#since the variances looks slightly
#different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Full_Bathroom_Above_Grade)
#since the pval is less than 0.05 therefore we  that the variances are not homogenous
#as the fval(31.613) > f critical for df(3,1455)=2.6049
#therefore we apply the kruskal wallis non parametric test.
kruskal.test(data.numeric$Sale_Price~data.factor$Full_Bathroom_Above_Grade)
#since the pval is less than 0.05 and chi square critical for 3 degrees of freedom is
#7.815 < chi squared evaluated fromt the kruskal test(585.57) 
#we conclude that the variables are  independent. We fail to accept the null hypothesis.



colnames(data.factor)[27]
ggplotly(ggplot(data.factor,
                aes(x=Half_Bathroom_Above_Grade,
                    y=data.numeric$Sale_Price,
                    color=Half_Bathroom_Above_Grade))+
           geom_boxplot()+
           xlab("Half_Bathroom_Above_Grade")+ylab("Sale Price")+
           theme_light()
)
#since the variances looks slightly
#different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Half_Bathroom_Above_Grade)
#since the pval is greater than 0.05 therefore we  that the variances are  homogenous
#as the fval(1.5003) < f critical for df(2,1456)=2.9957
#therefore we check for the second assumption of normality of anova
ggplot(data=data.numeric,aes(sample=Sale_Price))+
  geom_qq()+facet_wrap(~data.factor$Half_Bathroom_Above_Grade)
#as the data is normally distributed across each group we apply the anova analysis.

summary(aov(data.numeric$Sale_Price~data.factor$Half_Bathroom_Above_Grade))
#as the pval is less than 0.05 we conclude that the variables are not independent
#and therefore fail to accept the null hypothesis.

colnames(data.factor)[28]
ggplotly(ggplot(data.factor,
                aes(x=Bedroom_Above_Grade,
                    y=data.numeric$Sale_Price,
                    color=Bedroom_Above_Grade))+
           geom_boxplot()+
           xlab("Bedroom_Above_Grade")+ylab("Sale Price")+
           theme_light()
)
#since the variances looks 
#different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Bedroom_Above_Grade)
#since the pval is less than 0.05 therefore we  that the variances are not homogenous
#as the fval(6.5059) > f critical for df(7,1451)=2.0096 
#therefore we conclude that the variances are nto homogenous.
#therefore we apply the kruskal wallis non parametric test.
kruskal.test(data.numeric$Sale_Price~data.factor$Bedroom_Above_Grade)
#since the pval is less than 0.05 and chi square critical for 7 degrees of freedom is
#14.067 < chi squared evaluated fromt the kruskal test(98.548) 
#we conclude that the variables are  independent. We fail to accept the null hypothesis.

colnames(data.factor)[29]
ggplotly(ggplot(data.factor,
                aes(x=Kitchen_Quality,
                    y=data.numeric$Sale_Price,
                    color=Kitchen_Quality))+
           geom_boxplot()+
           xlab("Kitchen_Quality")+ylab("Sale Price")+
           theme_light()
) 
#since the variances looks slightly
#different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Kitchen_Quality)
#since the pval is less than 0.05 therefore we  that the variances arenot  homogenous
#as the fval(28.768) > f critical for df(3,1455)=2.6049 
#therefore we conclude that the variances are nto homogenous.
#therefore we apply the kruskal wallis non parametric test.
kruskal.test(data.numeric$Sale_Price~data.factor$Kitchen_Quality)
#since the pval is less than 0.05 and chi square critical for 3 degrees of freedom is
#7.815 < chi squared evaluated fromt the kruskal test(643.02) 
#we conclude that the variables are  independent. We fail to accept the null hypothesis.

colnames(data.factor)[30]
ggplotly(ggplot(data.factor,
                aes(x=Rooms_Above_Grade,
                    y=data.numeric$Sale_Price,
                    color=Rooms_Above_Grade))+
           geom_boxplot()+
           xlab("Rooms_Above_Grade")+ylab("Sale Price")+
           theme_light()
) 
#since the variances looks 
#different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Rooms_Above_Grade)
#since the pval is less than 0.05 therefore we  that the variances are not homogenous
#as the fval(9.1212) > f critical for df(11,1447)=1.7522 
#therefore we conclude that the variances are nto homogenous.
#therefore we apply the kruskal wallis non parametric test.
kruskal.test(data.numeric$Sale_Price~data.factor$Rooms_Above_Grade)
#since the pval is less than 0.05 and chi square critical for 11 degrees of freedom is
#19.675 < chi squared evaluated fromt the kruskal test(405.73) 
#we conclude that the variables are  independent. We fail to accept the null hypothesis.


colnames(data.factor)[31]
ggplotly(ggplot(data.factor,
                aes(x=Fireplaces,
                    y=data.numeric$Sale_Price,
                    color=Fireplaces))+
           geom_boxplot()+
           xlab("Fireplaces")+ylab("Sale Price")+
           theme_light()
) 
#since the variances looks 
#different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Fireplaces)
#since the pval is less than 0.05 therefore we  that the variances are not homogenous
#as the fval(20.129) > f critical for df(3,1447)=2.6049 
#therefore we conclude that the variances are not homogenous.
#therefore we apply the kruskal wallis non parametric test.
kruskal.test(data.numeric$Sale_Price~data.factor$Fireplaces)
#since the pval is less than 0.05 and 
#chi squared(20.129)>chi squared critical for df(3)=7.815
#therefore we fail to accept the null hypothesis and conclude
#that the vairbales are not independent


colnames(data.factor)[32]
ggplotly(ggplot(data.factor,
                aes(x=Fireplace_Quality,
                    y=data.numeric$Sale_Price,
                    color=Fireplace_Quality))+
           geom_boxplot()+
           xlab("Fireplace_Quality")+ylab("Sale Price")+
           theme_light()
) 
#since the variances looks 
#different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Fireplace_Quality)
#since the pval is less than 0.05 therefore we  that the variances are not  homogenous
#as the fval(21.846) > f critical for df(5,1453)=2.2141 
#therefore we conclude that the variances are not homogenous.
#therefore we apply the kruskal wallis non parametric test.

kruskal.test(data.numeric$Sale_Price~data.factor$Fireplace_Quality)
#since the pval is less than 0.05 and 
#chi squared(452.48)>chi squared critical for df(5)=11.07
#therefore we fail to accept the null hypothesis and conclude
#that the vairbales are not independent



colnames(data.factor)[33]
ggplotly(ggplot(data.factor,
                aes(x=Garage,
                    y=data.numeric$Sale_Price,
                    color=Garage))+
           geom_boxplot()+
           xlab("Garage")+ylab("Sale Price")+
           theme_light()
) 
#since the variances looks 
#different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Garage)
#since the pval is less than 0.05 therefore we  that the variances are not homogenous
#as the fval(11.661) > f critical for df(7,1451)=2.0096 
#therefore we conclude that the variances are not homogenous.
#therefore we apply the kruskal wallis non parametric test.

kruskal.test(data.numeric$Sale_Price~data.factor$Garage)
#since the pval is less than 0.05 and 
#chi squared(530.25)>chi squared critical for df(7)=14.067
#therefore we fail to accept the null hypothesis and conclude
#that the vairbales are not independent


colnames(data.factor)[34]
ggplotly(ggplot(data.factor,
                aes(x=Garage_Size,
                    y=data.numeric$Sale_Price,
                    color=Garage_Size))+
           geom_boxplot()+
           xlab("Garage_Size")+ylab("Sale Price")+
           theme_light()
) 
#since the variances looks slighlty
#different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Garage_Size)
#since the pval is less than 0.05 therefore we  that the variances are not homogenous
#as the fval(26.294) > f critical for df(4,1454)=2.3719 
#therefore we conclude that the variances are not homogenous.
#therefore we apply the kruskal wallis non parametric test.

kruskal.test(data.numeric$Sale_Price~data.factor$Garage_Size)
#since the pval is less than 0.05 and 
#chi squared(682.53)>chi squared critical for df(4)=9.488
#therefore we fail to accept the null hypothesis and conclude
#that the vairbales are not independent



colnames(data.factor)[35]
ggplotly(ggplot(data.factor,
                aes(x=Garage_Quality,
                    y=data.numeric$Sale_Price,
                    color=Garage_Quality))+
           geom_boxplot()+
           xlab("Garage_Quality")+ylab("Sale Price")+
           theme_light()
) 
#since the variances looks 
#different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Garage_Quality)
#since the pval is less than 0.05 therefore we  that the variances are not homogenous
#as the fval(4.6058) > f critical for df(5,1453)=2.2141 
#therefore we conclude that the variances are not homogenous.
#therefore we apply the kruskal wallis non parametric test.

kruskal.test(data.numeric$Sale_Price~data.factor$Garage_Quality)
#since the pval is less than 0.05 and 
#chi squared(191.75)>chi squared critical for df(5)=11.07
#therefore we fail to accept the null hypothesis and conclude
#that the variables are not independent



colnames(data.factor)[36]
ggplotly(ggplot(data.factor,
                aes(x=Fence_Quality,
                    y=data.numeric$Sale_Price,
                    color=Fence_Quality))+
           geom_boxplot()+
           xlab("Fence_Quality")+ylab("Sale Price")+
           theme_light()
) 
#since the variances looks 
#different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Fence_Quality)
#since the pval is less than 0.05 therefore we  that the variances are not homogenous
#as the fval(14.46) > f critical for df(4,1454)=2.3719 
#therefore we conclude that the variances are not homogenous.
#therefore we apply the kruskal wallis non parametric test.

kruskal.test(data.numeric$Sale_Price~data.factor$Fence_Quality)
#since the pval is less than 0.05 and 
#chi squared(79.259)>chi squared critical for df(4)=9.488
#therefore we fail to accept the null hypothesis and conclude
#that the variables are not independent

colnames(data.factor)[37]
ggplotly(ggplot(data.factor,
                aes(x=Month_Sold,
                    y=data.numeric$Sale_Price,
                    color=Month_Sold))+
           geom_boxplot()+
           xlab("Month_Sold")+ylab("Sale Price")+
           theme_light()
) 
#since the variances looks 
#different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Month_Sold)
#since the pval is greater than 0.05 therefore we  that the variances are  homogenous
#as the fval(1.4143) < f critical for df(11,1447)=1.7522 
#therefore we conclude that the variances are  homogenous.
#therefore we check for the normality of data.

ggplot(data=data.numeric,aes(sample=Sale_Price))+
  geom_qq()+facet_wrap(~data.factor$Month_Sold)
#as the data is normally distributed in each group we apply aov to the data.
summary(aov(data.numeric$Sale_Price~data.factor$Month_Sold))
#as the pval is greater than 0.05 we conclude that the variables are independent.
#therefore we fail to reject the null hypothesis.

data.factor[,c("Month_Sold")]<-NULL

colnames(data.factor)[37]
ggplotly(ggplot(data.factor,
                aes(x=Sale_Type,
                    y=data.numeric$Sale_Price,
                    color=Sale_Type))+
           geom_boxplot()+
           xlab("Sale_Type")+ylab("Sale Price")+
           theme_light()
) 
#since the variances looks 
#different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Sale_Type)
#since the pval is greater than 0.05 therefore we  that the variances are  homogenous
#as the fval(2.1665) < f critical for df(8,1450)=1.9348 
#therefore we conclude that the variances are  homogenous.
#therefore we check for the normality of data.
#therefore we check for the normality of data.

ggplot(data=data.numeric,aes(sample=Sale_Price))+
  geom_qq()+facet_wrap(~data.factor$Sale_Type)
#as the data is normally distributed in each group we apply aov to the data.
summary(aov(data.numeric$Sale_Price~data.factor$Sale_Type))
#as the pval is less than 0.05 we conclude that the variables are not independent.
#therefore we fail to accept the null hypothesis.
#also fval(26.64) > for df(8,1450)=fcritical=1.9348.
#therefore we conclude that the variables are not independent


colnames(data.factor)[38]
ggplotly(ggplot(data.factor,
                aes(x=Sale_Condition,
                    y=data.numeric$Sale_Price,
                    color=Sale_Condition))+
           geom_boxplot()+
           xlab("Sale_Condition")+ylab("Sale Price")+
           theme_light()
) 
#since the variances looks 
#different in all the groups therefore  using levenetest to confirm
#the finding
leveneTest(data.numeric$Sale_Price~data.factor$Sale_Condition)
#since the pval is less than 0.05 therefore we  that the variances are  homogenous
#as the fval(3.0435) > f critical for df(5,1453)=2.2141 
#therefore we conclude that the variances are not homogenous.
#therefore we check for kruskal wallis test
kruskal.test(data.numeric$Sale_Price~data.factor$Sale_Condition)
#the pval is less than 0.05.
#also the chi square (162.77) for df(5)>chi squared critical=11.07
#therefore we conclude that the variables are not independent.
#and we fail to accept the null hypothesis.
dim(data.factor)
dim(data.numeric)


#********************************************************************************************
#********************************************************************************************

#                 End of Exploratory data analysis

#********************************************************************************************
#********************************************************************************************



#********************************************************************************************
#               Preparation of data for Model building
#********************************************************************************************

dummies<-dummy.data.frame(data.factor)
dim(dummies)

#Adding missing categories columns to the dataset.
colnames(data.factor)[1]
levels(data.factor$Building_Class)
#150 level is missing.
str(dummies)
dummies$Building_Class150<-0

colnames(data.factor)[2]
levels(data.factor$Zoning_Class)
str(dummies)
dummies$Zoning_ClassOther<-0

dataCmb<-cbind(dummies,data.numeric)
write.csv(dataCmb,"data.csv")

#********************************************************************************************
#               Sampling of data
#********************************************************************************************
index<-sample(1:nrow(dataCmb),0.70*nrow(dataCmb))
data.train<-dataCmb[index,]
data.test<-dataCmb[-index,]

dim(data.train)
dim(data.test)

#********************************************************************************************

#               Model Building

#********************************************************************************************

model.full<-lm(Sale_Price~.,data=data.train)
summary(model.full)
traindata=data.train[-c(1170,899,804,814),]
dim(traindata)
dim(data.train)
data.train[c(1170,899,804,814),"Sale_Price"]
data.train[c(1170,804,899,814),"Sale_Price"]=median(data.train$Sale_Price)
model.null<-lm(Sale_Price~1,data=traindata)
model.null
summary(model.null)

#Stepwise modeling

fit <- step(model.null, scope=list(lower= model.null, upper=model.full), direction="both")


fit

#build the model

model<-lm(formula = Sale_Price ~ Grade_Living_Area + OldAge + Total_Basement_Area + 
            NewnessAge + House_Type1Fam + Overall_Material8 + Overall_Material9 + 
            NeighborhoodCrawfor + Overall_Material7 + House_Condition3 + 
            House_Condition5 + Zoning_ClassCommer + NeighborhoodClearCr + 
            Kitchen_QualityEx + Overall_Material6 + Exterior1stBrkFace + 
            Garage_Size3 + House_Condition4 + Full_Bathroom_Above_Grade3 + 
            Exterior_MaterialEx + Lot_Size + NeighborhoodSomerst + Fireplaces0 + 
            Sale_ConditionAbnorml + Kitchen_QualityGd + Condition1Norm + 
            Property_ShapeIR2 + NeighborhoodNridgHt + House_TypeTwnhs + 
            Exposure_LevelGd + `Garageno garage` + NeighborhoodStoneBr + 
            Garage_QualityEx + Garage2Types + Underground_Full_Bathroom0 + 
            Underground_Full_Bathroom1 + House_Condition1 + Sale_ConditionFamily + 
            Rooms_Above_Grade12 + Zoning_ClassRMD + House_Condition6 + 
            Garage_Size2 + Fireplace_QualityEx + Exterior_ConditionPo + 
            NeighborhoodBrkSide + Sale_ConditionNormal + Foundation_TypeW + 
            Rooms_Above_Grade11 + Brick_Veneer_Area + Exterior_ConditionTA + 
            House_Condition9 + Brick_Veneer_TypeStone + Roof_DesignHip + 
            Brick_Veneer_TypeBrkCmn + Fireplaces2 + Condition1RRAe + 
            Land_OutlineHLS + BsmtFinType1LwQ + BsmtUnfSF + BsmtFinType2Unf + 
            NeighborhoodNPkVill + House_Design2Story + Exterior2ndPlywood + 
            Exterior2ndHdBoard + NeighborhoodBrDale + Building_Class30 + 
            NeighborhoodNoRidge + LobbyArea + NeighborhoodNAmes + NeighborhoodEdwards + 
            Underground_Full_Bathroom2 + Garage2TFes + Rooms_Above_Grade10 + 
            Sale_TypeCWD + House_Condition7 + Bedroom_Above_Grade2 + 
            NeighborhoodMitchel + Condition1PosN + BsmtFinType2ALQ + 
            Half_Bathroom_Above_Grade2, data = traindata)


summary(model)

#Check for multicollinearity
vif(model)
#removing Underground_Full_Bathroom0: column from model for VIF more than 284
model<-lm(formula = Sale_Price ~ Grade_Living_Area + OldAge + Total_Basement_Area + 
            NewnessAge + House_Type1Fam + Overall_Material8 + Overall_Material9 + 
            NeighborhoodCrawfor + Overall_Material7 + House_Condition3 + 
            House_Condition5 + Zoning_ClassCommer + NeighborhoodClearCr + 
            Kitchen_QualityEx + Overall_Material6 + Exterior1stBrkFace + 
            Garage_Size3 + House_Condition4 + Full_Bathroom_Above_Grade3 + 
            Exterior_MaterialEx + Lot_Size + NeighborhoodSomerst + Fireplaces0 + 
            Sale_ConditionAbnorml + Kitchen_QualityGd + Condition1Norm + 
            Property_ShapeIR2 + NeighborhoodNridgHt + House_TypeTwnhs + 
            Exposure_LevelGd + `Garageno garage` + NeighborhoodStoneBr + 
            Garage_QualityEx + Garage2Types +  
            Underground_Full_Bathroom1 + House_Condition1 + Sale_ConditionFamily + 
            Rooms_Above_Grade12 + Zoning_ClassRMD + House_Condition6 + 
            Garage_Size2 + Fireplace_QualityEx + Exterior_ConditionPo + 
            NeighborhoodBrkSide + Sale_ConditionNormal + Foundation_TypeW + 
            Rooms_Above_Grade11 + Brick_Veneer_Area + Exterior_ConditionTA + 
            House_Condition9 + Brick_Veneer_TypeStone + Roof_DesignHip + 
            Brick_Veneer_TypeBrkCmn + Fireplaces2 + Condition1RRAe + 
            Land_OutlineHLS + BsmtFinType1LwQ + BsmtUnfSF + BsmtFinType2Unf + 
            NeighborhoodNPkVill + House_Design2Story + Exterior2ndPlywood + 
            Exterior2ndHdBoard + NeighborhoodBrDale + Building_Class30 + 
            NeighborhoodNoRidge + LobbyArea + NeighborhoodNAmes + NeighborhoodEdwards + 
            Underground_Full_Bathroom2 + Garage2TFes + Rooms_Above_Grade10 + 
            Sale_TypeCWD + House_Condition7 + Bedroom_Above_Grade2 + 
            NeighborhoodMitchel + Condition1PosN + BsmtFinType2ALQ + 
            Half_Bathroom_Above_Grade2, data = traindata)


vif(model)
#as all v=VIF are <5 we dont remove any variables any further.

#*****************************************************************************************

#               End of Model Building

#*****************************************************************************************

#*****************************************************************************************
# Predicting and check the performance on test set
#*****************************************************************************************
pred=predict(model,data.test)

str(pred)

results<-as.data.frame(cbind(pred,data.test$Sale_Price))
head(results)
colnames(results)<-c("Predicted","Actual")
#calculating MSE
mse <- mean((results$Actual-results$Predicted)^2)
print(mse)

#Or the root mean squared error:
mse^0.5

#Or just the R-Squared Value for our model (just for the predictions)
SSE = sum((results$Actual-results$Predicted)^2) 
SST = sum( (results$Actual - mean(results$Actual))^2)
SSR = sum((results$Predicted-mean(results$Actual))^2)

R2= SSR/SST 
R2
#0.922575
#Comparing it train data summary
summary(model)
#R2 of model on training set: 0.9151
#therefore the model is showing greater accuracy on the test set than the training set.

rm(index,mse,pred,R2,SSE,SSR,SST)
rm(fit,model.full,model.null,results)
#*****************************************************************************************
#*****************************************************************************************

#               End of Model Building and Evaluation

#*****************************************************************************************
#*****************************************************************************************
plot(model)

traindata[804,"Sale_Price"]

lmtest::bptest(model)


#*****************************************************************************************

#           Predicting the output for the test data set

#*****************************************************************************************

str(dataTest)
