#This code is part of the Healthcare Data Analyst interview exercise. 
#Code developed by Pinky Malempati Hari.

#Load Required Datasets
visit<-read.csv('visit.csv')
department<-read.csv('department.csv')
visit_diagnosis<-read.csv('visit_diagnosis.csv')
diagnosis<-read.csv('diagnosis.csv')
medication_order<-read.csv('medication_order.csv')

#Set Working Directory- To be changed by user
#setwd('~/<path to directory here>/')

#Load required packages
library('dplyr')
library ('stringr')
library('lubridate')

#merge datasets by keys
#merge by visit 
cohort<-merge(x=visit_diagnosis,y=visit,by='VISIT_KEY',all=TRUE) 
#merge by diagnosis
cohort<-merge(x=cohort,y=diagnosis,by='DX_KEY',all=TRUE) 
#merge by department
cohort<-merge(x=cohort,y=department,by='DEPT_KEY',all=TRUE) 

#Part 1. Create Cohort for analysis
#filter by hospital encounter
cohort<-cohort %>% filter (DICT_ENC_TYPE_KEY==83) 
#filter by ED diagnosis
cohort<-cohort %>% 
  filter (DICT_DX_STS_KEY %in% (313:314))
#format date
cohort$HOSP_ADMIT_DT<-
  as.POSIXct(cohort$HOSP_ADMIT_DT,format="%d/%m/%y %H:%M") 
#filter by date after Aug 1,2014
cohort<-cohort%>% 
  filter (HOSP_ADMIT_DT > as.POSIXct("2014-08-01 00:00", tz=Sys.timezone())) 
#filter by age
cohort<-cohort %>% filter (AGE >=1, AGE <=18) 

#array of ICD9 codes
ICDcodes<-c(995.0,995.3,995.6,995.61,995.62,99.63,
            99.64,995.65,995.66,995.67,995.68,995.69,
            995.7,999.4,999.41,999.42,999.49) 

#filter cohort by desired ICD9 codes
cohort<-cohort %>% filter (ICD9_CD %in% ICDcodes) 
#filter cohort to remove urgent care visits
cohort<-cohort %>% filter(SPECIALTY != "URGENT CARE") 

#Part 2. Create indicators based on cohort

#Anaphylactic Indicator
cohort$ANAPH_DX_IND<-
  ifelse(grepl("anaphylaxis",cohort$DX_NM,ignore.case=T),"1",
         ifelse(grepl("anaphylactic",cohort$DX_NM,ignore.case=T),"1","0"))

#Epinephrine Indicator
medication_order_epi<-medication_order
medication_order_epi$EPI_ORDER_IND<-
  ifelse(grepl("epinephrine",medication_order_epi$MED_ORD_NM,ignore.case=T),"1","0")
medication_order_epi<-medication_order_epi %>% filter (EPI_ORDER_IND == 1)
cohort$EPI_ORDER_IND<-
  ifelse(cohort$VISIT_KEY %in% medication_order_epi$VISIT_KEY,"1","0")

#Follow-up Indicator
cohort$PAT_KEY<-cohort$PAT_KEY.x
visit_OP<-visit %>% filter (DICT_ENC_TYPE_KEY==108)
mergecohortvisit<-merge(cohort,visit_OP, by="PAT_KEY",all.x=TRUE)
mergecohortvisit$HOSP_DISCHRG_DT.x<-
  as.POSIXct(mergecohortvisit$HOSP_DISCHRG_DT.x,format="%m/%d/%y %H:%M")
mergecohortvisit$APPT_DT.y<-
  as.POSIXct(mergecohortvisit$APPT_DT.y,format="%m/%d/%y %H:%M")
mergecohortvisit$datediff<-
  difftime(mergecohortvisit$APPT_DT.y,mergecohortvisit$HOSP_DISCHRG_DT.x,unit=c("days"))
mergecohortvisit<-mergecohortvisit %>% filter (datediff>0, datediff<8)
cohort$FOLLOW_UP_IND<-
  ifelse(cohort$PAT_KEY.x %in% mergecohortvisit$PAT_KEY,"1","0")

#Date of Follow-up
alldates=as.data.frame(list(PAT_KEY=mergecohortvisit$PAT_KEY,FOLLOW_UP_DATE=mergecohortvisit$APPT_DT.y))
xx=tapply(alldates$FOLLOW_UP_DATE,list(alldates$PAT_KEY),min)
firstdates=as.data.frame(list(FOLLOW_UP_DATE=as.POSIXct(xx,origin="1970-01-01"),PAT_KEY=names(xx)))
cohort<-merge(cohort,firstdates,by='PAT_KEY',all=TRUE)

#Days to Follow-up
cohort$FOLLOW_UP_DATE<-as.POSIXct(cohort$FOLLOW_UP_DATE,format="%m/%d/%y %H:%M")
cohort$HOSP_DISCHRG_DT<-as.POSIXct(cohort$HOSP_DISCHRG_DT,format="%m/%d/%y %H:%M")
cohort$DAYS_TO_FOLLOW_UP<-difftime(cohort$FOLLOW_UP_DATE,cohort$HOSP_DISCHRG_DT,unit=("days"))
cohort$DAYS_TO_FOLLOW_UP<-floor(cohort$DAYS_TO_FOLLOW_UP)
