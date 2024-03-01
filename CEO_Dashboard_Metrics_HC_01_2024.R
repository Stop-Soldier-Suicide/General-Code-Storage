###########################################
### USAA 2024 Reporting
###########################################
# For KPIs see https://docs.google.com/document/d/11wK3bq3vlMSIE2lk-Snuv0zLV_FbwbACqCIQ7US-QpU/edit
##  Among New clients:
# Screening: Number of contacts screened using CSSRS
# Number of new clients receiving full initial assessment to include demographics, social needs, and risk factors
# Stratification of clients receiving full initial assessment by demographics, social needs, and risk factors
    # Race, ethnicity, Gender Identity, Sex, Age Category,
## Among New AND Existing Clients AND not referred out:
# Management: MRC/HRCs who receive Crisis Response Plan/Contingency plan
# Treatment: MRC/HRCs who receive CAMS or BCBT-SP
# Stratification of clients receiving treatment by demographics to include categories of diversity, social needs, and risk factors
# Lives saved or suicide attempts averted

library(salesforcer)
library(lubridate)
library(summarytools)
library(dplyr)
library(ggplot2)

setwd("~/Programs/CEO Dashboard Metrics")

sf_auth(login_url = "https://sss33.my.salesforce.com/")

#Set time frame for all analyses below
gv_startdate="2023-12-01T05:00:00Z" #This day is included in the range  ## Since it pulls based on the time in England, I allow 5 extra hours to align with midnight EST
gv_enddate="2024-03-01T05:00:00Z" #The range goes up to but NOT including this date
gv_startyear="2023-01-01T00:00:00Z"


############################################
# 1. Screening & Intakes & Characteristics
############################################

# SF query parameters
gv_requestsource1="Case"
gv_requestfields1="Id, AccountId, CaseNumber, CreatedDate, Origin, Status, Status_Change_Reason__c, I_am__c, What_can_ROGER_help_you_with__c, utm_term__c, utm_campaign__c, utm_content__c, Any_thoughts_of_killing_yourself__c, Thoughts_of_killing_yourself_right_now__c, Wished_you_were_dead_Past_Month__c, Thoughts_of_killing_yourself_past_month__c, Thinking_about_how_you_might_do_this__c, Thoughts_Intention_of_acting_on_them__c, Details_of_How_Intend_to_Carry_Out__c, Done_Started_Prepared_to_end_your_life__c, Done_Started_Prepared_last_3_months__c, INT_CKL_Chronic_Pain__c, INT_CKL_Physical_Heath_Issues__c, INT_CKL_Physical_Health_Treatment__c, INT_CKL_Mental_Health_Issues__c, INT_CKL_Mental_Health_Treatment__c, INT_CKL_Substance_Abuse__c, INT_CKL_Peer_Support__c, INT_CKL_Receiving_Care_at_VHA__c"
gv_requestsource2="Account"
gv_requestfields2="Id, Name, CreatedDate, Test_Account__c, HealthCloudGA__Age__pc, HealthCloudGA__Gender__pc, Assigned_Sex_at_Birth__pc, Race__pc, Ethnicity__pc, Service_Status__pc, Homeless__pc, INT_CKL_Domestic_Violence__pc, INT_CKL_Employment_Status__pc, INT_CKL_Health_Insurance__pc"
gv_requestsource3="Case_Note__c"
gv_requestfields3="Id, Case__c, CreatedDate, Case_Note_Type__c, Interaction_Date_Time__c, Call_Center_Type__c, Connection_Outcome__c, Session_Type__c, Interaction_With_Whom__c, Overall_Risk_Level__c"

# Read in and merge data
case_query=sprintf(paste("SELECT ",gv_requestfields1," FROM ",gv_requestsource1," WHERE CreatedDate>=",gv_startdate," and CreatedDate<",gv_enddate))
requestcases=sf_query(case_query)

requestcases=requestcases%>%
  rename(Case__c=Id)%>%
  filter(is.na(AccountId)==FALSE, I_am__c=="I am a veteran/service member seeking help",(is.na(Status_Change_Reason__c)|(Status_Change_Reason__c!="Third Party - Did Not Convert Client" & Status_Change_Reason__c!="Third Party - Converted Client"))) #Remove cases without an account, and 3P requests
requestcases=requestcases[order(requestcases$AccountId),]

#account_query=sprintf(paste("SELECT ",gv_requestfields2," FROM ",gv_requestsource2," WHERE Id IN (",paste("'",unique(requestcases$AccountId),"'",sep="",collapse=","),")")) #Only read in the Accounts that have a case in the requestcases file
requestaccounts=sf_query(paste("SELECT ",gv_requestfields2," FROM ",gv_requestsource2," WHERE Id IN (",paste("'",unique(requestcases$AccountId),"'",sep="",collapse=","),")"))

requestaccounts=requestaccounts%>%
  rename(AccountId=Id,AccountCreatedDate=CreatedDate)

requests=merge(requestcases,requestaccounts,by="AccountId") #inner join, only keep cases that have an account associated with it.

#remove test cases, duplicate cases, and ineligible cases
requests=requests%>%
  mutate(testname=grepl("TEST",toupper(Name),fixed = TRUE)*1)%>%
  mutate(rrbcname=grepl("RRBC",toupper(Name),fixed = TRUE)*1)%>%
  filter(testname==0,rrbcname==0,Test_Account__c==FALSE,(is.na(Status_Change_Reason__c)|(Status_Change_Reason__c!="Duplicate Case for Active Client" & Status_Change_Reason__c!="Test Case" & Status_Change_Reason__c!="Ineligible - Not a Service Member/Veteran" & Status_Change_Reason__c!="Ineligible - Underage"))) #Important exclusion criteria!
requests$Origin[requests$Origin=="Phone"]="Call Center"

#bring in Case Note data for the new cases within this time
requests=requests[order(requests$Case__c),]
#casenote_query=sprintf(paste("SELECT ",gv_requestfields3," FROM ",gv_requestsource3," WHERE (Call_Center_Type__c=null or Call_Center_Type__c != 'Did not call / duplicate help request') and Case__c IN (",paste("'",unique(requests$Case__c),"'",sep="",collapse=","),")")) #Only read in the Case Notes that have a case in the requests file
requestcasenotes=sf_query(paste("SELECT ",gv_requestfields3," FROM ",gv_requestsource3," WHERE (Call_Center_Type__c=null or Call_Center_Type__c != 'Did not call / duplicate help request') and Interaction_Date_Time__c>=",gv_startdate," and Interaction_Date_Time__c<",gv_enddate," and Case__c IN (",paste("'",unique(requests$Case__c),"'",sep="",collapse=","),")"))
freq(requestcasenotes$Call_Center_Type__c)
freq(requestcasenotes$Connection_Outcome__c)
freq(requestcasenotes$Case_Note_Type__c)
freq(requestcasenotes$Session_Type__c)

requestcasenotes=requestcasenotes%>%
  rename(CaseNoteId=Id,CaseNoteCreatedDate=CreatedDate)%>%
  mutate(anyrisklevel=case_when(Overall_Risk_Level__c=="HRC" | Overall_Risk_Level__c=="MRC" | Overall_Risk_Level__c=="LRC"~1,TRUE~0))
requestcasenotes=requestcasenotes[order(requestcasenotes$Case__c,-requestcasenotes$anyrisklevel,unclass(requestcasenotes$CaseNoteCreatedDate)),] #we want the first one with any risk level
requestcasescasenotes=requestcasenotes%>%
  group_by(Case__c)%>%
  mutate(ncasenotes=max(row_number()),
         sssconnectedtoclient=max(case_when(Case_Note_Type__c!="Call Center" & Case_Note_Type__c!="Supervisor" & Connection_Outcome__c=="Connected by Phone, Video, SMS Conversation, or Email Conversation"~1,
                                            TRUE~0)),
         nsssconnections=sum(case_when(Case_Note_Type__c!="Call Center" & Case_Note_Type__c!="Supervisor" & Connection_Outcome__c=="Connected by Phone, Video, SMS Conversation, or Email Conversation"~1,
                                       TRUE~0)),
         v4wconnection=max(case_when(Case_Note_Type__c=="Call Center" & Connection_Outcome__c=="Connected by Phone, Video, SMS Conversation, or Email Conversation"~1,
                                     TRUE~0)),
         schedulerconnection=max(case_when(Case_Note_Type__c=="Scheduler" & Connection_Outcome__c=="Connected by Phone, Video, SMS Conversation, or Email Conversation"~1,
                                           TRUE~0)),
         intakesession=max(case_when(Session_Type__c=="Intake" & Connection_Outcome__c=="Connected by Phone, Video, SMS Conversation, or Email Conversation"~1,
                                     TRUE~0)),
         riskassigned=max(case_when(is.na(Overall_Risk_Level__c)==FALSE~1,
                                    TRUE~0)),
         intakeandriskassigned=case_when(intakesession==1 & riskassigned==1~1,
                                         sssconnectedtoclient==1 & riskassigned==1~1,
                                         intakesession==1 & nsssconnections>3~1,
                                         TRUE~0))%>% ##It is weird that some have a risk assigned but no intake during this time and many case notes. We should explore this more!
  select(-CaseNoteId,-Connection_Outcome__c, -Session_Type__c,-anyrisklevel)%>%
  filter(row_number()==1) %>% #only keep the first case note record information
  rename(initialrisklevel=Overall_Risk_Level__c) %>%
  mutate(initialrisklevel=case_when(initialrisklevel=="HRC"~"1. HRC",
                                    initialrisklevel=="MRC"~"2. MRC",
                                    initialrisklevel=="LRC"~"3. LRC"))


freq(requestcasescasenotes$sssconnectedtoclient)

requests=merge(requests,requestcasescasenotes,by="Case__c",all.x=TRUE) #keep all the original cases even if they do not have a Case Note

#fill in missing values after the merge
requests$sssconnectedtoclient[is.na(requests$sssconnectedtoclient)==TRUE]<-0
requests$v4wconnection[is.na(requests$v4wconnection)==TRUE]<-0
requests$schedulerconnection[is.na(requests$schedulerconnection)==TRUE]<-0

freq(requests$Status_Change_Reason__c)
freq(requests$Origin)
freq(requests$ncasenotes)

# test=requests%>%
#   filter(is.na(Origin)==TRUE) ## Need to talk with JZ about why these people have a missing origin and the schedulers spoke with them. are from 3Ps? 
requests=requests%>%
  mutate(Origin=case_when(is.na(Origin)==TRUE & Call_Center_Type__c=="Inbound call"~"Call Center",
                          TRUE~Origin))
freq(requests$Origin)

# test=requests%>%
#   filter(Status_Change_Reason__c=="Unable to Contact - Receiving Inpatient Care")
# test=requests%>%
#   filter(Status_Change_Reason__c=="Third Party - Did Not Convert Client")

freq(requests$sssconnectedtoclient)
freq(requests$v4wconnection)
freq(requests$schedulerconnection[requests$v4wconnection==1])

# test=requests%>%
#   filter(is.na(ncasenotes)==TRUE)


## Create cleaned variables for analyses
requests=requests%>%
  mutate(cssrsscreen=(is.na(Wished_you_were_dead_Past_Month__c)==FALSE & is.na(Thoughts_of_killing_yourself_past_month__c)==FALSE & is.na(Done_Started_Prepared_to_end_your_life__c)==FALSE)*1,
         cssrsrisk=case_when(Wished_you_were_dead_Past_Month__c=="No" & Thoughts_of_killing_yourself_past_month__c=="No" & Done_Started_Prepared_to_end_your_life__c=="No"~0,
                             Done_Started_Prepared_last_3_months__c=="Yes" | Details_of_How_Intend_to_Carry_Out__c =="Yes" | Thoughts_Intention_of_acting_on_them__c=="Yes"~3,
                             Done_Started_Prepared_to_end_your_life__c=="Yes" | Thinking_about_how_you_might_do_this__c=="Yes"~2,
                             Wished_you_were_dead_Past_Month__c=="Yes" | Thoughts_of_killing_yourself_past_month__c=="Yes"~1,
                             TRUE~NA),
         cssrsminrisk=(cssrsrisk==0)*1,
         cssrslowrisk=(cssrsrisk==1)*1,
         cssrsmodrisk=(cssrsrisk==2)*1,
         cssrshighrisk=(cssrsrisk==3)*1,
         ethnicity=case_when(Ethnicity__pc=="Prefer Not to Say" | Ethnicity__pc=="Unknown" | is.na(Ethnicity__pc)==TRUE~"Unknown / Prefer Not to Say",
                             TRUE~Ethnicity__pc),
         ethnicityHL=(ethnicity=="Hispanic or Latino")*1,
         ethnicitynotHL=(ethnicity=="Not Hispanic or Latino")*1,
         ethnicitymissing=(ethnicity=="Unknown / Prefer Not to Say")*1,
         race=case_when(Race__pc=="Black or African American" | Race__pc== "Black or African American;Nonwhite"~"Black or African American", 
                        Race__pc=="White"~"White or Caucasian", # Did not include: Race__pc=="White;Other"
                        is.na(Race__pc)==FALSE & Race__pc !="Prefer not to say"~"Other or Mixed Race", #everything else as other or mixed race
                        TRUE~"Unknown / Prefer Not to Say"),
         raceblack=(race=="Black or African American")*1,
         racewhite=(race=="White or Caucasian")*1,
         raceothermix=(race=="Other or Mixed Race")*1,
         racemissing=(race=="Unknown / Prefer Not to Say")*1,
         agecat=case_when(as.numeric(substr(HealthCloudGA__Age__pc,1,2))>=18 & as.numeric(substr(HealthCloudGA__Age__pc,1,2))<=34~"18 to 34 Years",
                          as.numeric(substr(HealthCloudGA__Age__pc,1,2))>=35 & as.numeric(substr(HealthCloudGA__Age__pc,1,2))<=54~"35 to 54 Years",
                          as.numeric(substr(HealthCloudGA__Age__pc,1,2))>=55 & as.numeric(substr(HealthCloudGA__Age__pc,1,2))<=74~"55 to 74 Years",
                          as.numeric(substr(HealthCloudGA__Age__pc,1,2))>=75~"75+ Years",
                          TRUE~"Unknown / Prefer Not to Say"),
         agecat18to34=(agecat=="18 to 34 Years")*1,
         agecat35to54=(agecat=="35 to 54 Years")*1,
         agecat55to74=(agecat=="55 to 74 Years")*1,
         agecat75plus=(agecat=="75+ Years")*1,
         agecatmissing=(agecat=="Unknown / Prefer Not to Say")*1,
         gender=case_when(HealthCloudGA__Gender__pc=="Man"~"Man",
                          HealthCloudGA__Gender__pc=="Woman"~"Woman",
                          HealthCloudGA__Gender__pc=="Other"~"Other",
                          TRUE~"Unknown / Prefer Not to Say"),
         genderman=(gender=="Man")*1,
         genderwoman=(gender=="Woman")*1,
         genderother=(gender=="Other")*1,
         gendermissing=(gender=="Unknown / Prefer Not to Say")*1,
         physicalhealthissuesyes=case_when(is.na(INT_CKL_Physical_Heath_Issues__c)~0,
                                           INT_CKL_Physical_Heath_Issues__c==""|grepl("NONE",toupper(INT_CKL_Physical_Heath_Issues__c),fixed=TRUE)|toupper(INT_CKL_Physical_Heath_Issues__c)=="NO"|toupper(INT_CKL_Physical_Heath_Issues__c)=="NO MENTIONED"~0,
                                           TRUE~1),
         physicalhealthissuesno=case_when(is.na(INT_CKL_Physical_Heath_Issues__c)~0,
                                          INT_CKL_Physical_Heath_Issues__c==""|grepl("NONE",toupper(INT_CKL_Physical_Heath_Issues__c),fixed=TRUE)|toupper(INT_CKL_Physical_Heath_Issues__c)=="NO"|toupper(INT_CKL_Physical_Heath_Issues__c)=="NO MENTIONED"~1,
                                          TRUE~0),
         physicalhealthissuesmissing=case_when(is.na(INT_CKL_Physical_Heath_Issues__c)~1, TRUE~0),
         mentalhealthissuesyes=case_when(is.na(INT_CKL_Mental_Health_Issues__c)~0,
                                         INT_CKL_Mental_Health_Issues__c==""|grepl("NONE",toupper(INT_CKL_Mental_Health_Issues__c),fixed=TRUE)|toupper(INT_CKL_Mental_Health_Issues__c)=="NO"|toupper(INT_CKL_Mental_Health_Issues__c)=="NO MENTIONED"~0,
                                         TRUE~1),
         mentalhealthissuesno=case_when(is.na(INT_CKL_Mental_Health_Issues__c)~0,
                                        INT_CKL_Mental_Health_Issues__c==""|grepl("NONE",toupper(INT_CKL_Mental_Health_Issues__c),fixed=TRUE)|toupper(INT_CKL_Mental_Health_Issues__c)=="NO"|toupper(INT_CKL_Mental_Health_Issues__c)=="NO MENTIONED"~1,
                                        TRUE~0),
         servicestatusactive=case_when(Service_Status__pc=="Active Duty"~1, TRUE~0),
         servicestatusreserveguard=case_when(grepl("Reserve",Service_Status__pc,fixed = TRUE) | grepl("National Guard",Service_Status__pc,fixed = TRUE)~1, TRUE~0),
         servicestatusretired=case_when(grepl("Retired",Service_Status__pc,fixed = TRUE)~1, TRUE~0),
         servicestatusdischarged=case_when(Service_Status__pc=="Discharged (any status)" | Service_Status__pc=="Active Duty;Discharged (any status)"~1, TRUE~0), #I am not including those that say they were discharged and in the reserve or guard 
         servicestatusmissing=case_when(is.na(Service_Status__pc)==TRUE~1, TRUE~0),
         employmentdisabled=case_when(grepl("Disabled",INT_CKL_Employment_Status__pc,fixed=TRUE)~1,TRUE~0),
         employmentlooking=case_when(grepl("Looking for work",INT_CKL_Employment_Status__pc,fixed=TRUE)~1,TRUE~0),
         employmentnotempl=case_when(grepl("Not currently employed",INT_CKL_Employment_Status__pc,fixed=TRUE)~1,TRUE~0),
         employmentretired=case_when(grepl("Retired",INT_CKL_Employment_Status__pc,fixed=TRUE)~1,TRUE~0),
         employmentfulltime=case_when(grepl("Working full time for pay",INT_CKL_Employment_Status__pc,fixed=TRUE)~1,TRUE~0),
         employmentpartime=case_when(grepl("Working part time for pay",INT_CKL_Employment_Status__pc,fixed=TRUE)~1,TRUE~0),
         employmentother=case_when(grepl("Other",INT_CKL_Employment_Status__pc,fixed=TRUE)~1,TRUE~0),
         employmentmissing=case_when(is.na(INT_CKL_Employment_Status__pc)==TRUE~1,TRUE~0),
         homelessyes=case_when(Homeless__pc=="Yes"~1,TRUE~0),
         homelessno=case_when(Homeless__pc=="No"~1,TRUE~0),
         homelessmissing=case_when(is.na(Homeless__pc)==TRUE~1,TRUE~0),
         dvyes=case_when(INT_CKL_Domestic_Violence__pc=="Yes"~1,TRUE~0),
         dvno=case_when(INT_CKL_Domestic_Violence__pc=="No"~1,TRUE~0),
         dvmissing=case_when(is.na(INT_CKL_Domestic_Violence__pc)==TRUE~1,TRUE~0),
         healthinsuranceyes=case_when(INT_CKL_Health_Insurance__pc=="Yes"~1,TRUE~0),
         healthinsuranceno=case_when(INT_CKL_Health_Insurance__pc=="No"~1,TRUE~0),
         healthinsurancemissing=case_when(is.na(INT_CKL_Health_Insurance__pc)==TRUE~1,TRUE~0),
         chronicpainyes=case_when(INT_CKL_Chronic_Pain__c=="Yes"~1,TRUE~0),
         chronicpainno=case_when(INT_CKL_Chronic_Pain__c=="No"~1,TRUE~0),
         chronicpainmissing=case_when(is.na(INT_CKL_Chronic_Pain__c)==TRUE~1,TRUE~0),
         physicaltxyes=case_when(INT_CKL_Physical_Health_Treatment__c=="Yes"~1,TRUE~0),
         physicaltxno=case_when(INT_CKL_Physical_Health_Treatment__c=="No"~1,TRUE~0),
         physicaltxmissing=case_when(is.na(INT_CKL_Physical_Health_Treatment__c)==TRUE~1,TRUE~0),
         mentaltxyes=case_when(INT_CKL_Mental_Health_Treatment__c=="Yes"~1,TRUE~0),
         mentaltxno=case_when(INT_CKL_Mental_Health_Treatment__c=="No"~1,TRUE~0),
         mentaltxmissing=case_when(is.na(INT_CKL_Mental_Health_Treatment__c)==TRUE~1,TRUE~0),
         substanceabuseyes=case_when(INT_CKL_Substance_Abuse__c=="Yes"~1,TRUE~0),
         substanceabuseno=case_when(INT_CKL_Substance_Abuse__c=="No"~1,TRUE~0),
         substanceabusemissing=case_when(is.na(INT_CKL_Substance_Abuse__c)==TRUE~1,TRUE~0),
         peersupportyes=case_when(INT_CKL_Peer_Support__c=="Yes"~1,TRUE~0),
         peersupportno=case_when(INT_CKL_Peer_Support__c=="No"~1,TRUE~0),
         peersupportmissing=case_when(is.na(INT_CKL_Peer_Support__c)==TRUE~1,TRUE~0),
         vhacareyes=case_when(INT_CKL_Receiving_Care_at_VHA__c=="Yes"~1,TRUE~0),
         vhacareno=case_when(INT_CKL_Receiving_Care_at_VHA__c=="No"~1,TRUE~0),
         vhacaremissing=case_when(is.na(INT_CKL_Receiving_Care_at_VHA__c)==TRUE~1,TRUE~0),
         rogerhelpsleep=case_when(grepl("Sleep",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelptrauma=case_when(grepl("Trauma",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelpabuse=case_when(grepl("Abuse",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelptbi=case_when(grepl("TBI",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelpaddictions=case_when(grepl("Addictions",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelprelationships=case_when(grepl("Relationships",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelpfinances=case_when(grepl("Finances",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelphousing=case_when(grepl("Housing",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelpchronicpain=case_when(grepl("Chronic Pain",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelphopelessness=case_when(grepl("Hopelessness",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelptrapped=case_when(grepl("Feeling Trapped",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelpanxiety=case_when(grepl("Anxiety",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelpisolation=case_when(grepl("Isolation",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelpguilt=case_when(grepl("Guilt",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelpselfhate=case_when(grepl("Self-hate",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelpanguish=case_when(grepl("Anguish/Misery",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelpgrief=case_when(grepl("Grief/Loss",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelpanger=case_when(grepl("Anger",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelptrust=case_when(grepl("Trust",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelppurpose=case_when(grepl("Life Purpose",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelpother=case_when(grepl("Other",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelpnumber=rogerhelpsleep+rogerhelptrauma+rogerhelpabuse+rogerhelptbi+rogerhelpaddictions+rogerhelprelationships+rogerhelpfinances+rogerhelphousing+rogerhelpchronicpain+rogerhelphopelessness+rogerhelptrapped+rogerhelpanxiety+rogerhelpisolation+rogerhelpguilt+rogerhelpselfhate+rogerhelpanguish+rogerhelpgrief+rogerhelpanger+rogerhelptrust+rogerhelppurpose+rogerhelpother,
         rogerhelpmissing=case_when(is.na(What_can_ROGER_help_you_with__c)==TRUE~1,TRUE~0),
         anysthoughtsyes=case_when(Any_thoughts_of_killing_yourself__c=="Yes"~1,TRUE~0),
         anysthoughtsno=case_when(Any_thoughts_of_killing_yourself__c=="No"~1,TRUE~0),
         anysthoughtsmissing=case_when(is.na(Any_thoughts_of_killing_yourself__c)==TRUE~1,TRUE~0),
         currentsthoughtsyes=case_when(Thoughts_of_killing_yourself_right_now__c=="Yes"~1,TRUE~0),
         currentsthoughtsno=case_when(Thoughts_of_killing_yourself_right_now__c=="No"|Any_thoughts_of_killing_yourself__c=="No"~1,TRUE~0),
         currentsthoughtsmissing=case_when(is.na(Thoughts_of_killing_yourself_right_now__c)==TRUE & is.na(Any_thoughts_of_killing_yourself__c)==TRUE~1,TRUE~0),
         monthcasecreated=month(CreatedDate)) 
  
#should we make the character variables factors?
freq(requests$cssrsscreen)
freq(requests$cssrsrisk)
freq(requests$Race__pc)
freq(requests$race)
freq(requests$Ethnicity__pc)
freq(requests$ethnicity)
freq(requests$HealthCloudGA__Age__pc)
freq(requests$agecat)
freq(requests$HealthCloudGA__Gender__pc)
freq(requests$gender)

#Other characteristics
freq(requests$Assigned_Sex_at_Birth__pc)
freq(requests$Service_Status__pc)
freq(requests$Homeless__pc)
freq(requests$INT_CKL_Domestic_Violence__pc)
freq(requests$INT_CKL_Employment_Status__pc)
freq(requests$INT_CKL_Health_Insurance__pc)
freq(requests$INT_CKL_Chronic_Pain__c)
table(requests$Physical_Health_Issues)
freq(requests$INT_CKL_Physical_Health_Treatment__c)
table(requests$Mental_Health_Issues)
freq(requests$INT_CKL_Mental_Health_Treatment__c)
freq(requests$INT_CKL_Substance_Abuse__c)
freq(requests$INT_CKL_Peer_Support__c)
freq(requests$INT_CKL_Receiving_Care_at_VHA__c)
#freq(requests$What_can_ROGER_help_you_with__c)
freq(requests$utm_campaign__c)
freq(requests$utm_content__c)
freq(requests$utm_term__c)

#Race, ethnicity, Gender Identity, Sex, Age Category
#HealthCloudGA__Age__pc, HealthCloudGA__Gender__pc, Assigned_Sex_at_Birth__pc, Race__pc, Ethnicity__pc, 
#Service_Status__pc, Homeless__pc, INT_CKL_Domestic_Violence__pc, INT_CKL_Employment_Status__pc, INT_CKL_Health_Insurance__pc
#INT_CKL_Chronic_Pain__c, INT_CKL_Physical_Heath_Issues__c, INT_CKL_Physical_Health_Treatment__c, 
#INT_CKL_Mental_Health_Issues__c, INT_CKL_Mental_Health_Treatment__c, INT_CKL_Substance_Abuse__c, 
#INT_CKL_Peer_Support__c, INT_CKL_Receiving_Care_at_VHA__c

### limit to the earliest case where the intake and risk was assigned (1 instead of 0) per account
requests=requests[order(requests$AccountId,-requests$intakeandriskassigned,unclass(requests$CreatedDate)),]  
requests=requests%>%
  group_by(AccountId)%>%
  filter(row_number()==1)

dfSummary(as.data.frame(requests))
#dfSummary(requests[,127:148])

# Connected with SSS
freq(requests$monthcasecreated)
freq(requests$sssconnectedtoclient)
freq(requests$sssconnectedtoclient[requests$monthcasecreated==10])
freq(requests$sssconnectedtoclient[requests$monthcasecreated==11])
freq(requests$sssconnectedtoclient[requests$monthcasecreated==12|requests$monthcasecreated==1])

# Screening: Number of contacts screened using CSSRS
freq(requests$cssrsscreen) #250 #freq(requests$Status_Change_Reason__c)

rogerhelpall=as.data.frame(colMeans(requests[,127:148]))
names(rogerhelpall)="Proportion of clients that requested help"

# Number of new clients receiving full initial assessment to include demographics, social needs, and risk factors
estclient=requests%>%
  filter(intakeandriskassigned==1,(is.na(Status_Change_Reason__c)==TRUE | (Status_Change_Reason__c!="Declined Services At Intake" & Status_Change_Reason__c!="Declined Services At Scheduling" & Status_Change_Reason__c!="Unable to Contact - Intake Not Completed, Client Nonconnection" & Status_Change_Reason__c!="Caring Contacts - Ongoing")))
  #69

#write.csv(estclient[,c("Case__c","AccountId")],"~/Programs/CEO Dashboard Metrics/estclient100123to020824.csv",row.names = FALSE)

freq(estclient$Status_Change_Reason__c)

# Stratification of clients receiving full initial assessment by demographics, social needs, and risk factors
freq(estclient$gender)
freq(estclient$agecat)
freq(estclient$race)
freq(estclient$ethnicity)

rogerhelp=as.data.frame(colMeans(estclient[,127:148]))
names(rogerhelp)="Proportion of clients that requested help"
#Cluster analysis
#distance=dist(estclient[,126:146])
#rogerhelpclusters=hclust(distance)
#plot(rogerhelpclusters)


freq(estclient$cssrsrisk)
freq(estclient$anysthoughtsyes)
freq(estclient$currentsthoughtsyes)
freq(estclient$initialrisklevel)
ctable(estclient$initialrisklevel,as.factor(estclient$cssrsrisk))

freq(estclient$servicestatusactive)
freq(estclient$servicestatusreserveguard)
freq(estclient$servicestatusretired)
freq(estclient$servicestatusdischarged)
freq(estclient$servicestatusmissing)
freq(estclient$healthinsuranceno)
freq(estclient$employmentdisabled)
freq(estclient$employmentlooking)
freq(estclient$homelessyes)
freq(estclient$dvyes)
freq(estclient$chronicpainyes)
freq(estclient$physicalhealthissuesyes)
freq(estclient$physicaltxyes)
freq(estclient$mentalhealthissuesyes)
freq(estclient$mentaltxyes)
freq(estclient$substanceabuseyes)
ctable(as.factor(estclient$mentalhealthissuesyes),as.factor(estclient$substanceabuseyes))
ctable(as.factor(estclient$mentaltxyes),as.factor(estclient$substanceabuseyes))
freq(estclient$vhacareyes)
freq(estclient$peersupportyes)

##########################################################################
# CAMS/BCBT/CRP TX
# This is with the cohort of all clients served
##########################################################################

# SF query parameters
gv_txsource1="Case_Note__c"
gv_txfields1="Id, Case__c, CreatedDate, Case_Note_Type__c, Interaction_Date_Time__c, Call_Center_Type__c, Connection_Outcome__c, Session_Type__c, Interaction_With_Whom__c, Overall_Risk_Level__c, Acute_Risk_Level__c, Chronic_Risk_Level__c, Risk_Level_Change_Reason__c, History_of_Chronic_Ideation__c, History_of_Self_Directed_Violence__c, History_of_Attempts__c, Chronic_Major_Mental_Diagnosis__c, Chronic_Major_Personality_Diagnosis__c, History_of_Substance_Misuse_Abuse__c, Chronic_Major_Medical_Diagnosis__c, Chronic_Pain__c, Limited_Coping_Skills_Chronic__c, Unstable_Psychosocial_Status__c, Limited_Reasons_for_Living__c, History_of_Risky_Actions__c, History_of_Impulsivity__c, Current_Suicidal_Ideation_even_passive__c, Inability_to_Stay_Safe__c, Intent_or_Wish_to_Die__c, Set_Method_of_Choice_or_Plan__c, Recent_Attempt__c, More_than_One_Attempt__c, Ongoing_Preparatory_Behavior__c, Will_Not_Use_CRP__c, Limited_Coping_Skills_Acute__c, Access_to_Means__c, Acute_Major_Illness__c, Intensified_Personality_Disorder__c, Acute_Psychosocial_Stressors__c, Lookup_Primary_Phone_Call__c, Lookup_Secondary_Phone_Call__c, Lookup_Tertiary_Phone_Call__c"
gv_txsource2="Case"
gv_txfields2="Id, AccountId, CaseNumber, CreatedDate, OwnerId, Origin, Status, Status_Change_Reason__c, I_am__c, Most_Recent_Overall_Risk_Level__c, Most_Recent_Chronic_Risk_Level__c, Most_Recent_Acute_Risk_Level__c, 	Recent_CRP_Warning_Signs__c, Recent_CRP_Self_Help_Actions__c, Recent_CRP_Reasons_for_Living__c, Recent_CRP_Social_Support__c, Recent_CRP_Professional_Support__c, Recent_CRP_Plan_Storage__c, Recent_CRP_Means_Reduction__c, What_can_ROGER_help_you_with__c, utm_term__c, utm_campaign__c, utm_content__c, Any_thoughts_of_killing_yourself__c, Thoughts_of_killing_yourself_right_now__c, Wished_you_were_dead_Past_Month__c, Thoughts_of_killing_yourself_past_month__c, Thinking_about_how_you_might_do_this__c, Thoughts_Intention_of_acting_on_them__c, Details_of_How_Intend_to_Carry_Out__c, Done_Started_Prepared_to_end_your_life__c, Done_Started_Prepared_last_3_months__c, INT_CKL_Chronic_Pain__c, INT_CKL_Physical_Heath_Issues__c, INT_CKL_Physical_Health_Treatment__c, INT_CKL_Mental_Health_Issues__c, INT_CKL_Mental_Health_Treatment__c, INT_CKL_Substance_Abuse__c, INT_CKL_Peer_Support__c, INT_CKL_Receiving_Care_at_VHA__c"
gv_txsource3="Account"
gv_txfields3="Id, Name, CreatedDate, Test_Account__c, HealthCloudGA_Last_Active_Case_IR__pc, HealthCloudGA_LstActCaseCurrentRiskLvll__pc, HealthCloudGA__Age__pc, HealthCloudGA__Gender__pc, Assigned_Sex_at_Birth__pc, Race__pc, Ethnicity__pc, Service_Status__pc, Homeless__pc, INT_CKL_Domestic_Violence__pc, INT_CKL_Employment_Status__pc, INT_CKL_Health_Insurance__pc"
gv_txsource4="Form_Data__c"
gv_txfields4="Id, Care_Plan__c, Name, CreatedDate, CP_Plan_Enacted_Date__c, CP_ROGER_Contact_Level__c, 	CP_Contact_Monitoring_Level__c, CP_Safety_Plan__c, CP_Lethal_Means_Reduction__c, CP_Contingency_End_Date__c, Future_Optimism__c, Feeling_Useful__c, Relaxation__c, Problem_Handling__c, Clear_Thinking__c, Social_Closeness__c, Independent_Decisions__c, COQ_Self_Rated_Risk__c, COQ_Manage_Thoughts_And_Feelings__c, COQ_Fleeting_Thoughts__c, COQ_Current_Thoughts__c, Suicidality_Subscale_Group_1__c, Suicidality_Subscale_Group_2__c, Suicidality_Subscale_Group_3__c, Suicidality_Subscale_Group_4__c, BSCS_Self_Worth__c, BSCS_Problem_Help__c, BSCS_Coping_Struggle__c, BSCS_Enduring_Pain__c, BSCS_Self_Redeeming__c, BSCS_Suicide_Thoughts__c, CAMS_RATE_Psychological_Pain__c, CAMS_RATE_STRESS__c, CAMS_RATE_AGITATION__c, CAMS_RATE_HOPELESSNESS__c, CAMS_RATE_SELF_HATE__c, CAMS_RATE_OVERALL_SUICIDE_RISK__c, FSS_NPS_Question__c, COQ_F_ROGER_Prevent_Attempt__c, COQ_F_ROGER_Prevent_Attempt_Details__c, COQ_F_Consent_For_Attempt_Details__c, FSS_Share_With_Others_Txt__c, FSS_Consent_to_Share__c, FSS_Learned_from_ROGER_Txt__c, FSS_ROGER_Improvement_Txt__c, COQ_B_Talked_About_Suicide__c, COQ_B_Talked_About_Suicide_Who__c, COQ_B_Talked_About_Suicide_Other__c"
gv_txsource5="amazonconnect__AC_ContactChannelAnalytics__c"
gv_txfields5="Id, amazonconnect__ContactLensTranscriptsFullText__c" #, amazonconnect__Sentiment__c, amazonconnect__Keywords__c, amazonconnect__ContactLensTalkTimeCustomer__c, amazonconnect__ContactLensTalkSpeedCustomer__c,  amazonconnect__ContactLensCustomerSentiment__c, amazonconnect__ContactLensTalkTimeAgent__c, amazonconnect__ContactLensTalkSpeedAgent__c, amazonconnect__ContactLensAgentSentiment__c"
gv_txsource6="User"
gv_txfields6="Id, Name, Username"

txcasenotes=sf_query(paste("SELECT ",gv_txfields1," FROM ",gv_txsource1," WHERE (Case_Note_Type__c='Session' or Case_Note_Type__c='General' or Case_Note_Type__c='RRBC') and Interaction_Date_Time__c>=",gv_startdate," and Interaction_Date_Time__c<",gv_enddate))  #OLD: WHERE (Call_Center_Type__c=null or Call_Center_Type__c != 'Did not call / duplicate help request') and ...
txcasenotes=txcasenotes%>%
  mutate(anyrisklevel=case_when(Overall_Risk_Level__c=="HRC" | Overall_Risk_Level__c=="MRC" | Overall_Risk_Level__c=="LRC"~1,TRUE~0))

txcasenotes=txcasenotes[order(txcasenotes$Case__c,-txcasenotes$anyrisklevel,unclass(txcasenotes$CreatedDate)),]#we want the first one with any risk level
txcasenotes=txcasenotes%>%
  group_by(Case__c)%>%
  mutate(cams_bcbt=max(case_when(Connection_Outcome__c=="Connected by Phone, Video, SMS Conversation, or Email Conversation"&(Session_Type__c=="CAMS"|Session_Type__c=="BCBT-SP")~1,
                                 TRUE~0)),
         cams=max(case_when(Connection_Outcome__c=="Connected by Phone, Video, SMS Conversation, or Email Conversation" & Session_Type__c=="CAMS"~1,
                            TRUE~0)),
         bcbt=max(case_when(Connection_Outcome__c=="Connected by Phone, Video, SMS Conversation, or Email Conversation" & Session_Type__c=="BCBT-SP"~1,
                            TRUE~0)),
         thirdpartyclient=case_when(Interaction_With_Whom__c=="3rd Party Client"~1,TRUE~0))%>%
  rename(initialrisklevel=Overall_Risk_Level__c) %>%
  mutate(initialrisklevel=case_when(initialrisklevel=="HRC"~"1. HRC",
                                    initialrisklevel=="MRC"~"2. MRC",
                                    initialrisklevel=="LRC"~"3. LRC")) #Initial risk level may not be available for everyone we transferred from the old system

txcaseid=txcasenotes%>%
  group_by(Case__c)%>%
  filter(Connection_Outcome__c=="Connected by Phone, Video, SMS Conversation, or Email Conversation" & is.na(Case__c)==FALSE & thirdpartyclient==0)%>% #remove the 3rd party only calls and only keep case notes where we connected with the person
  select(Case__c,cams_bcbt,cams,bcbt,initialrisklevel)%>%
  filter(row_number()==1)

txcases=sf_query(paste("SELECT ",gv_txfields2," FROM ",gv_txsource2," WHERE Id IN (",paste("'",unique(txcaseid$Case__c),"'",sep="",collapse=","),")"))
txcases=txcases%>%
  rename(Case__c=Id,CaseCreatedDate=CreatedDate)
txcases=merge(txcaseid,txcases,by="Case__c")

txaccounts=sf_query(paste("SELECT ",gv_txfields3," FROM ",gv_txsource3," WHERE Id IN (",paste("'",unique(txcases$AccountId),"'",sep="",collapse=","),")"))
txaccounts=txaccounts%>%
  rename(AccountId=Id,AccountCreatedDate=CreatedDate)
txcases=merge(txaccounts,txcases,by="AccountId")

#This time we may not want to limit by the I Am variable because it will also exclude many of the legacy clients we brought over from NPSP
txcases=txcases%>%
  filter(is.na(AccountId)==FALSE, (is.na(I_am__c)|I_am__c!="I am requesting help for a veteran/service member"),((is.na(Status_Change_Reason__c)==TRUE & Status=="Active_In_Care")|(Status_Change_Reason__c!="Third Party - Did Not Convert Client" & Status_Change_Reason__c!="Third Party - Converted Client" & Status_Change_Reason__c!="Duplicate Case for Active Client" & Status_Change_Reason__c!="Test Case" & Status_Change_Reason__c!="Ineligible - Not a Service Member/Veteran" & Status_Change_Reason__c!="Ineligible - Underage" & Status_Change_Reason__c!="Declined Services At Intake" & Status_Change_Reason__c!="Declined Services At Scheduling" & Status_Change_Reason__c!="Referred to External Provider" & Status_Change_Reason__c!="Unable to Contact - Intake Not Completed, Client Nonconnection" & Status_Change_Reason__c!="Caring Contacts - Ongoing" & Status_Change_Reason__c!="Caring Contacts - Completed" & Status_Change_Reason__c!="Caring Contacts - Client Returned to Care" & Status_Change_Reason__c!="Caring Contacts - Client Opted Out")))%>%
  mutate(testname=grepl("TEST",toupper(Name),fixed = TRUE)*1)%>%
  mutate(rrbcname=grepl("RRBC",toupper(Name),fixed = TRUE)*1)%>%
  filter(testname==0,rrbcname==0,Test_Account__c==FALSE)%>%
  mutate(risklevel=case_when(Most_Recent_Overall_Risk_Level__c=="HRC"~"1. HRC", #create an initial risk level from the Case Note
                             Most_Recent_Overall_Risk_Level__c=="MRC"~"2. MRC",
                             Most_Recent_Overall_Risk_Level__c=="LRC"~"3. LRC"),
         ncrpvars=(is.na(Recent_CRP_Warning_Signs__c)==FALSE)*1+(is.na(Recent_CRP_Self_Help_Actions__c)==FALSE)*1+(is.na(Recent_CRP_Reasons_for_Living__c)==FALSE)*1+(is.na(Recent_CRP_Social_Support__c)==FALSE)*1+(is.na(Recent_CRP_Professional_Support__c)==FALSE)*1+(is.na(Recent_CRP_Plan_Storage__c)==FALSE)*1+(is.na(Recent_CRP_Means_Reduction__c)==FALSE)*1,
         crp=(ncrpvars>=4)*1)

freq(txcases$risklevel) 
freq(txcases$initialrisklevel)
txcases$initialrisklevel[is.na(txcases$initialrisklevel)]=txcases$risklevel[is.na(txcases$initialrisklevel)]
freq(txcases$initialrisklevel)
ctable(txcases$initialrisklevel,txcases$risklevel)

freq(txcases$ncrpvars)
freq(txcases$crp)

#look at those with NA risk level
# checkrisklevel=txcases%>%
#   filter(is.na(risklevel))
# freq(checkrisklevel$Status_Change_Reason__c)

freq(txcases$Status_Change_Reason__c)
ctable(txcases$Status_Change_Reason__c,txcases$risklevel)
ctable(txcases$risklevel,as.factor(txcases$cams_bcbt))
ctable(txcases$risklevel,as.factor(txcases$cams))
ctable(txcases$risklevel,as.factor(txcases$bcbt))
ctable(txcases$risklevel,as.factor(txcases$crp))

#bring in form data for each case
txformdata=sf_query(paste("SELECT ",gv_txfields4," FROM ",gv_txsource4," WHERE CreatedDate>=",gv_startdate," and CreatedDate<",gv_enddate," and Care_Plan__c IN (",paste("'",unique(txcases$Case__c),"'",sep="",collapse=","),")"))
freq(txformdata$Name)
#6 have a Crisis Form #2 Contingency Plan

txformdata=txformdata[order(txformdata$Care_Plan__c,-unclass(txformdata$CreatedDate)),] #I want the last crisis form per client
cpformdata=txformdata%>%
  filter(Name=="Contingency Plan",is.na(CP_Plan_Enacted_Date__c)==FALSE)%>% #select(Care_Plan__c, Name, CreatedDate, CP_Plan_Enacted_Date__c, CP_ROGER_Contact_Level__c, 	CP_Contact_Monitoring_Level__c, CP_Safety_Plan__c, CP_Lethal_Means_Reduction__c, CP_Contingency_End_Date__c)
  group_by(Care_Plan__c)%>%
  mutate(contingencyplan=case_when(is.na(CP_Plan_Enacted_Date__c)==FALSE~1),
         ncontingencyplan=max(row_number()))%>%
  rename(Case__c=Care_Plan__c)%>%
  select(Case__c,contingencyplan,ncontingencyplan,CP_Plan_Enacted_Date__c)%>%
  filter(row_number()==1)

txcases=merge(txcases,cpformdata,by="Case__c",all.x=TRUE)
txcases$contingencyplan[is.na(txcases$contingencyplan)]=0
txcases$ncontingencyplan[is.na(txcases$ncontingencyplan)]=0

freq(txcases$contingencyplan)

txcases=txcases%>%
  mutate(crp_cp=case_when(contingencyplan==1 | crp==1~1,TRUE~0),
         cssrsscreen=(is.na(Wished_you_were_dead_Past_Month__c)==FALSE & is.na(Thoughts_of_killing_yourself_past_month__c)==FALSE & is.na(Done_Started_Prepared_to_end_your_life__c)==FALSE)*1,
         cssrsrisk=case_when(Wished_you_were_dead_Past_Month__c=="No" & Thoughts_of_killing_yourself_past_month__c=="No" & Done_Started_Prepared_to_end_your_life__c=="No"~0,
                             Done_Started_Prepared_last_3_months__c=="Yes" | Details_of_How_Intend_to_Carry_Out__c =="Yes" | Thoughts_Intention_of_acting_on_them__c=="Yes"~3,
                             Done_Started_Prepared_to_end_your_life__c=="Yes" | Thinking_about_how_you_might_do_this__c=="Yes"~2,
                             Wished_you_were_dead_Past_Month__c=="Yes" | Thoughts_of_killing_yourself_past_month__c=="Yes"~1,
                             TRUE~NA),
         cssrsminrisk=(cssrsrisk==0)*1,
         cssrslowrisk=(cssrsrisk==1)*1,
         cssrsmodrisk=(cssrsrisk==2)*1,
         cssrshighrisk=(cssrsrisk==3)*1,
         ethnicity=case_when(Ethnicity__pc=="Prefer Not to Say" | Ethnicity__pc=="Unknown" | is.na(Ethnicity__pc)==TRUE~"Unknown / Prefer Not to Say",
                             TRUE~Ethnicity__pc),
         ethnicityHL=(ethnicity=="Hispanic or Latino")*1,
         ethnicitynotHL=(ethnicity=="Not Hispanic or Latino")*1,
         ethnicitymissing=(ethnicity=="Unknown / Prefer Not to Say")*1,
         race=case_when(Race__pc=="Black or African American" | Race__pc== "Black or African American;Nonwhite"~"Black or African American", 
                        Race__pc=="White"~"White or Caucasian", # Did not include: Race__pc=="White;Other"
                        is.na(Race__pc)==FALSE & Race__pc !="Prefer not to say"~"Other or Mixed Race", #everything else as other or mixed race
                        TRUE~"Unknown / Prefer Not to Say"),
         raceblack=(race=="Black or African American")*1,
         racewhite=(race=="White or Caucasian")*1,
         raceothermix=(race=="Other or Mixed Race")*1,
         racemissing=(race=="Unknown / Prefer Not to Say")*1,
         agecat=case_when(as.numeric(substr(HealthCloudGA__Age__pc,1,2))>=18 & as.numeric(substr(HealthCloudGA__Age__pc,1,2))<=34~"18 to 34 Years",
                          as.numeric(substr(HealthCloudGA__Age__pc,1,2))>=35 & as.numeric(substr(HealthCloudGA__Age__pc,1,2))<=54~"35 to 54 Years",
                          as.numeric(substr(HealthCloudGA__Age__pc,1,2))>=55 & as.numeric(substr(HealthCloudGA__Age__pc,1,2))<=74~"55 to 74 Years",
                          as.numeric(substr(HealthCloudGA__Age__pc,1,2))>=75~"75+ Years",
                          TRUE~"Unknown / Prefer Not to Say"),
         agecat18to34=(agecat=="18 to 34 Years")*1,
         agecat35to54=(agecat=="35 to 54 Years")*1,
         agecat55to74=(agecat=="55 to 74 Years")*1,
         agecat75plus=(agecat=="75+ Years")*1,
         agecatmissing=(agecat=="Unknown / Prefer Not to Say")*1,
         gender=case_when(HealthCloudGA__Gender__pc=="Man"~"Man",
                          HealthCloudGA__Gender__pc=="Woman"~"Woman",
                          HealthCloudGA__Gender__pc=="Other"~"Other",
                          TRUE~"Unknown / Prefer Not to Say"),
         genderman=(gender=="Man")*1,
         genderwoman=(gender=="Woman")*1,
         genderother=(gender=="Other")*1,
         gendermissing=(gender=="Unknown / Prefer Not to Say")*1,
         physicalhealthissuesyes=case_when(is.na(INT_CKL_Physical_Heath_Issues__c)~0,
                                        INT_CKL_Physical_Heath_Issues__c==""|grepl("NONE",toupper(INT_CKL_Physical_Heath_Issues__c),fixed=TRUE)|toupper(INT_CKL_Physical_Heath_Issues__c)=="NO"|toupper(INT_CKL_Physical_Heath_Issues__c)=="NO MENTIONED"~0,
                                        TRUE~1),
         physicalhealthissuesno=case_when(is.na(INT_CKL_Physical_Heath_Issues__c)~0,
                                        INT_CKL_Physical_Heath_Issues__c==""|grepl("NONE",toupper(INT_CKL_Physical_Heath_Issues__c),fixed=TRUE)|toupper(INT_CKL_Physical_Heath_Issues__c)=="NO"|toupper(INT_CKL_Physical_Heath_Issues__c)=="NO MENTIONED"~1,
                                        TRUE~0),
         physicalhealthissuesmissing=case_when(is.na(INT_CKL_Physical_Heath_Issues__c)~1, TRUE~0),
         mentalhealthissuesyes=case_when(is.na(INT_CKL_Mental_Health_Issues__c)~0,
                                      INT_CKL_Mental_Health_Issues__c==""|grepl("NONE",toupper(INT_CKL_Mental_Health_Issues__c),fixed=TRUE)|toupper(INT_CKL_Mental_Health_Issues__c)=="NO"|toupper(INT_CKL_Mental_Health_Issues__c)=="NO MENTIONED"~0,
                                      TRUE~1),
         mentalhealthissuesno=case_when(is.na(INT_CKL_Mental_Health_Issues__c)~0,
                                      INT_CKL_Mental_Health_Issues__c==""|grepl("NONE",toupper(INT_CKL_Mental_Health_Issues__c),fixed=TRUE)|toupper(INT_CKL_Mental_Health_Issues__c)=="NO"|toupper(INT_CKL_Mental_Health_Issues__c)=="NO MENTIONED"~1,
                                      TRUE~0),
         mentalhealthissuesmissing=case_when(is.na(INT_CKL_Mental_Health_Issues__c)~1, TRUE~0),
         servicestatusactive=case_when(Service_Status__pc=="Active Duty"~1, TRUE~0),
         servicestatusreserveguard=case_when(grepl("Reserve",Service_Status__pc,fixed = TRUE) | grepl("National Guard",Service_Status__pc,fixed = TRUE)~1, TRUE~0),
         servicestatusretired=case_when(grepl("Retired",Service_Status__pc,fixed = TRUE)~1, TRUE~0),
         servicestatusdischarged=case_when(Service_Status__pc=="Discharged (any status)" | Service_Status__pc=="Active Duty;Discharged (any status)"~1, TRUE~0), #I am not including those that say they were discharged and in the reserve or guard 
         servicestatusmissing=case_when(is.na(Service_Status__pc)==TRUE~1, TRUE~0),
         employmentdisabled=case_when(grepl("Disabled",INT_CKL_Employment_Status__pc,fixed=TRUE)~1,TRUE~0),
         employmentlooking=case_when(grepl("Looking for work",INT_CKL_Employment_Status__pc,fixed=TRUE)~1,TRUE~0),
         employmentnotempl=case_when(grepl("Not currently employed",INT_CKL_Employment_Status__pc,fixed=TRUE)~1,TRUE~0),
         employmentretired=case_when(grepl("Retired",INT_CKL_Employment_Status__pc,fixed=TRUE)~1,TRUE~0),
         employmentfulltime=case_when(grepl("Working full time for pay",INT_CKL_Employment_Status__pc,fixed=TRUE)~1,TRUE~0),
         employmentpartime=case_when(grepl("Working part time for pay",INT_CKL_Employment_Status__pc,fixed=TRUE)~1,TRUE~0),
         employmentother=case_when(grepl("Other",INT_CKL_Employment_Status__pc,fixed=TRUE)~1,TRUE~0),
         employmentmissing=case_when(is.na(INT_CKL_Employment_Status__pc)==TRUE~1,TRUE~0),
         homelessyes=case_when(Homeless__pc=="Yes"~1,TRUE~0),
         homelessno=case_when(Homeless__pc=="No"~1,TRUE~0),
         homelessmissing=case_when(is.na(Homeless__pc)==TRUE~1,TRUE~0),
         dvyes=case_when(INT_CKL_Domestic_Violence__pc=="Yes"~1,TRUE~0),
         dvno=case_when(INT_CKL_Domestic_Violence__pc=="No"~1,TRUE~0),
         dvmissing=case_when(is.na(INT_CKL_Domestic_Violence__pc)==TRUE~1,TRUE~0),
         healthinsuranceyes=case_when(INT_CKL_Health_Insurance__pc=="Yes"~1,TRUE~0),
         healthinsuranceno=case_when(INT_CKL_Health_Insurance__pc=="No"~1,TRUE~0),
         healthinsurancemissing=case_when(is.na(INT_CKL_Health_Insurance__pc)==TRUE~1,TRUE~0),
         chronicpainyes=case_when(INT_CKL_Chronic_Pain__c=="Yes"~1,TRUE~0),
         chronicpainno=case_when(INT_CKL_Chronic_Pain__c=="No"~1,TRUE~0),
         chronicpainmissing=case_when(is.na(INT_CKL_Chronic_Pain__c)==TRUE~1,TRUE~0),
         physicaltxyes=case_when(INT_CKL_Physical_Health_Treatment__c=="Yes"~1,TRUE~0),
         physicaltxno=case_when(INT_CKL_Physical_Health_Treatment__c=="No"~1,TRUE~0),
         physicaltxmissing=case_when(is.na(INT_CKL_Physical_Health_Treatment__c)==TRUE~1,TRUE~0),
         mentaltxyes=case_when(INT_CKL_Mental_Health_Treatment__c=="Yes"~1,TRUE~0),
         mentaltxno=case_when(INT_CKL_Mental_Health_Treatment__c=="No"~1,TRUE~0),
         mentaltxmissing=case_when(is.na(INT_CKL_Mental_Health_Treatment__c)==TRUE~1,TRUE~0),
         substanceabuseyes=case_when(INT_CKL_Substance_Abuse__c=="Yes"~1,TRUE~0),
         substanceabuseno=case_when(INT_CKL_Substance_Abuse__c=="No"~1,TRUE~0),
         substanceabusemissing=case_when(is.na(INT_CKL_Substance_Abuse__c)==TRUE~1,TRUE~0),
         peersupportyes=case_when(INT_CKL_Peer_Support__c=="Yes"~1,TRUE~0),
         peersupportno=case_when(INT_CKL_Peer_Support__c=="No"~1,TRUE~0),
         peersupportmissing=case_when(is.na(INT_CKL_Peer_Support__c)==TRUE~1,TRUE~0),
         vhacareyes=case_when(INT_CKL_Receiving_Care_at_VHA__c=="Yes"~1,TRUE~0),
         vhacareno=case_when(INT_CKL_Receiving_Care_at_VHA__c=="No"~1,TRUE~0),
         vhacaremissing=case_when(is.na(INT_CKL_Receiving_Care_at_VHA__c)==TRUE~1,TRUE~0),
         rogerhelpsleep=case_when(grepl("Sleep",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelptrauma=case_when(grepl("Trauma",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelpabuse=case_when(grepl("Abuse",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelptbi=case_when(grepl("TBI",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelpaddictions=case_when(grepl("Addictions",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelprelationships=case_when(grepl("Relationships",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelpfinances=case_when(grepl("Finances",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelphousing=case_when(grepl("Housing",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelpchronicpain=case_when(grepl("Chronic Pain",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelphopelessness=case_when(grepl("Hopelessness",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelptrapped=case_when(grepl("Feeling Trapped",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelpanxiety=case_when(grepl("Anxiety",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelpisolation=case_when(grepl("Isolation",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelpguilt=case_when(grepl("Guilt",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelpselfhate=case_when(grepl("Self-hate",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelpanguish=case_when(grepl("Anguish/Misery",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelpgrief=case_when(grepl("Grief/Loss",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelpanger=case_when(grepl("Anger",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelptrust=case_when(grepl("Trust",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelppurpose=case_when(grepl("Life Purpose",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelpother=case_when(grepl("Other",What_can_ROGER_help_you_with__c,fixed=TRUE)~1,TRUE~0),
         rogerhelpnumber=rogerhelpsleep+rogerhelptrauma+rogerhelpabuse+rogerhelptbi+rogerhelpaddictions+rogerhelprelationships+rogerhelpfinances+rogerhelphousing+rogerhelpchronicpain+rogerhelphopelessness+rogerhelptrapped+rogerhelpanxiety+rogerhelpisolation+rogerhelpguilt+rogerhelpselfhate+rogerhelpanguish+rogerhelpgrief+rogerhelpanger+rogerhelptrust+rogerhelppurpose+rogerhelpother,
         rogerhelpmissing=case_when(is.na(What_can_ROGER_help_you_with__c)==TRUE~1,TRUE~0),
         anysthoughtsyes=case_when(Any_thoughts_of_killing_yourself__c=="Yes"~1,TRUE~0),
         anysthoughtsno=case_when(Any_thoughts_of_killing_yourself__c=="No"~1,TRUE~0),
         anysthoughtsmissing=case_when(is.na(Any_thoughts_of_killing_yourself__c)==TRUE~1,TRUE~0),
         currentsthoughtsyes=case_when(Thoughts_of_killing_yourself_right_now__c=="Yes"~1,TRUE~0),
         currentsthoughtsno=case_when(Thoughts_of_killing_yourself_right_now__c=="No"|Any_thoughts_of_killing_yourself__c=="No"~1,TRUE~0),
         currentsthoughtsmissing=case_when(is.na(Thoughts_of_killing_yourself_right_now__c)==TRUE & is.na(Any_thoughts_of_killing_yourself__c)==TRUE~1,TRUE~0))

#create risk field with legacy risk data
freq(txcases$HealthCloudGA_Last_Active_Case_IR__pc)
freq(txcases$HealthCloudGA_LstActCaseCurrentRiskLvll__pc)
txcases=txcases%>%
  mutate(legacyinitialrisk=case_when(grepl("HRC",HealthCloudGA_Last_Active_Case_IR__pc,fixed = TRUE)~"1. HRC",
                                     grepl("MRC",HealthCloudGA_Last_Active_Case_IR__pc,fixed = TRUE)~"2. MRC",
                                     grepl("LRC",HealthCloudGA_Last_Active_Case_IR__pc,fixed = TRUE)~"3. LRC",
                                     TRUE~NA),
         legacycurrentrisk=case_when(grepl("HRC",HealthCloudGA_LstActCaseCurrentRiskLvll__pc,fixed = TRUE)~"1. HRC",
                                     grepl("MRC",HealthCloudGA_LstActCaseCurrentRiskLvll__pc,fixed = TRUE)~"2. MRC",
                                     grepl("LRC",HealthCloudGA_LstActCaseCurrentRiskLvll__pc,fixed = TRUE)~"3. LRC",
                                     TRUE~NA),
         legacyrisk=case_when(is.na(legacyinitialrisk)==FALSE~legacyinitialrisk,
                              TRUE~legacycurrentrisk),
         initialrisklevel2=case_when(is.na(initialrisklevel)==FALSE~initialrisklevel,
                                     TRUE~legacyrisk))

freq(txcases$legacyinitialrisk)
freq(txcases$legacycurrentrisk)
freq(txcases$initialrisklevel)
freq(txcases$initialrisklevel2)
ctable(txcases$initialrisklevel,txcases$initialrisklevel2)
##############################################
## HRC Case Closure and Treatment Graduation
#############################################
freq(txcases$Status_Change_Reason__c[txcases$initialrisklevel=="1. HRC" & txcases$Status=="Closed"])
freq(txcases$Status_Change_Reason__c[txcases$initialrisklevel=="2. MRC" & txcases$Status=="Closed"])
freq(txcases$Status_Change_Reason__c[txcases$initialrisklevel=="3. LRC" & txcases$Status=="Closed"])

freq(txcases$Status_Change_Reason__c[txcases$initialrisklevel=="1. HRC" & txcases$Status=="Closed" & txcases$cams_bcbt==1])
freq(txcases$Status_Change_Reason__c[txcases$initialrisklevel=="2. MRC" & txcases$Status=="Closed" & txcases$cams_bcbt==1])
freq(txcases$Status_Change_Reason__c[txcases$initialrisklevel=="3. LRC" & txcases$Status=="Closed" & txcases$cams_bcbt==1])
#####################################################
#Treatment process measures
#only count one case per person giving priority to the case where cams_bcbt was provided, 
#is there only 1 case per account?
txcases=txcases[order(txcases$AccountId,-unclass(txcases$CaseCreatedDate)),] #instead of most recent case, we can order to give preference to cams/bcbt and highest risk level
txaccounts=txcases%>%
  group_by(AccountId)%>%
  filter(row_number()==1)

freq(txaccounts$initialrisklevel)
#freq(txaccounts$initialrisklevel2)

freq(txaccounts$crp_cp)
freq(txaccounts$crp)
freq(txaccounts$contingencyplan)
ctable(txaccounts$initialrisklevel,as.factor(txaccounts$crp_cp))
ctable(txaccounts$initialrisklevel,as.factor(txaccounts$crp))
ctable(txaccounts$initialrisklevel,as.factor(txaccounts$contingencyplan))

freq(txaccounts$cams_bcbt)
freq(txaccounts$cams)
freq(txaccounts$bcbt)
#ctable(txaccounts$risklevel,as.factor(txaccounts$cams_bcbt))
ctable(txaccounts$initialrisklevel,as.factor(txaccounts$cams_bcbt)) ## use initial risk level
ctable(txaccounts$initialrisklevel,as.factor(txaccounts$cams))
ctable(txaccounts$initialrisklevel,as.factor(txaccounts$bcbt))

#ctable(as.factor(txaccounts$crp_cp),as.factor(txaccounts$crp))
ctable(as.factor(txaccounts$crp),as.factor(txaccounts$cams_bcbt))
ctable(as.factor(txaccounts$crp),as.factor(txaccounts$cams))
ctable(as.factor(txaccounts$crp),as.factor(txaccounts$bcbt))
ctable(txaccounts$initialrisklevel[txaccounts$cams_bcbt==0],as.factor(txaccounts$crp[txaccounts$cams_bcbt==0])) #those who receive a CRP but no treatment
  #Divide thes by the total population size for LRC, MRC, HRC


ctable(as.factor(txaccounts$gender),as.factor(txaccounts$cams_bcbt))
chisq.test(as.factor(txaccounts$gender),as.factor(txaccounts$cams_bcbt))
fisher.test(as.factor(txaccounts$gender),as.factor(txaccounts$cams_bcbt))
#No significant differences by gender. Below we exclude missing values
fisher.test(as.factor(txaccounts$gender[txaccounts$gender!="Unknown / Prefer Not to Say"]),as.factor(txaccounts$cams_bcbt[txaccounts$gender!="Unknown / Prefer Not to Say"]))
#ctable(as.factor(txaccounts$gender[txaccounts$gender!="Unknown / Prefer Not to Say"]),as.factor(txaccounts$cams_bcbt[txaccounts$gender!="Unknown / Prefer Not to Say"]))


ctable(as.factor(txaccounts$agecat),as.factor(txaccounts$cams_bcbt))
chisq.test(as.factor(txaccounts$agecat),as.factor(txaccounts$cams_bcbt))
fisher.test(as.factor(txaccounts$agecat),as.factor(txaccounts$cams_bcbt))
#Significant differences by age with the older groups being less likely to receive CAMS/BCBT
fisher.test(as.factor(txaccounts$agecat[txaccounts$agecat!="Unknown / Prefer Not to Say"]),as.factor(txaccounts$cams_bcbt[txaccounts$agecat!="Unknown / Prefer Not to Say"]))

ctable(as.factor(txaccounts$race),as.factor(txaccounts$cams_bcbt))
chisq.test(as.factor(txaccounts$race),as.factor(txaccounts$cams_bcbt))
fisher.test(as.factor(txaccounts$race),as.factor(txaccounts$cams_bcbt))
#No significant differences by race. Below is if we exclude missing values:
fisher.test(as.factor(txaccounts$race[txaccounts$race!="Unknown / Prefer Not to Say"]),as.factor(txaccounts$cams_bcbt[txaccounts$race!="Unknown / Prefer Not to Say"]))

ctable(as.factor(txaccounts$ethnicity),as.factor(txaccounts$cams_bcbt))
chisq.test(as.factor(txaccounts$ethnicity),as.factor(txaccounts$cams_bcbt))
fisher.test(as.factor(txaccounts$ethnicity),as.factor(txaccounts$cams_bcbt))
#Significant differences by ethnicity but particularly for unknown ethnicity. Below we exclude the missing responses:
fisher.test(as.factor(txaccounts$ethnicity[txaccounts$ethnicity!="Unknown / Prefer Not to Say"]),as.factor(txaccounts$cams_bcbt[txaccounts$ethnicity!="Unknown / Prefer Not to Say"]))
#The differences are not significant for those that provide their ethnicity.

# Characteristics
freq(txaccounts$gender)
freq(txaccounts$agecat)
freq(txaccounts$race)
freq(txaccounts$ethnicity)

freq(txaccounts$gender[txaccounts$cams_bcbt==1])
freq(txaccounts$gender[txaccounts$cams_bcbt==0])
freq(txaccounts$agecat[txaccounts$cams_bcbt==1])
freq(txaccounts$agecat[txaccounts$cams_bcbt==0])
freq(txaccounts$race[txaccounts$cams_bcbt==1])
freq(txaccounts$race[txaccounts$cams_bcbt==0])
freq(txaccounts$ethnicity[txaccounts$cams_bcbt==1])
freq(txaccounts$ethnicity[txaccounts$cams_bcbt==0])

freq(txaccounts$servicestatusactive)
freq(txaccounts$servicestatusreserveguard)
freq(txaccounts$servicestatusretired)
freq(txaccounts$servicestatusdischarged)
freq(txaccounts$servicestatusmissing)
freq(txaccounts$healthinsuranceno)
freq(txaccounts$employmentdisabled)
freq(txaccounts$employmentlooking)
freq(txaccounts$homelessyes)
freq(txaccounts$dvyes)
freq(txaccounts$chronicpainyes)
freq(txaccounts$physicalhealthissuesyes) 
freq(txaccounts$physicaltxyes)
freq(txaccounts$mentalhealthissuesyes)
freq(txaccounts$mentaltxyes)
freq(txaccounts$substanceabuseyes)
freq(txaccounts$vhacareyes)
freq(txaccounts$peersupportyes)

######################
## No risk level file
######################
user_query=sprintf(paste("SELECT ",gv_txfields6," FROM ",gv_txsource6," WHERE Id IN (",paste("'",unique(txaccounts$OwnerId),"'",sep="",collapse=","),")"))
users=sf_query(user_query)

users=users%>%
  rename(OwnerId=Id, OwnerName=Name, OwnerUsername=Username)%>%
  filter(OwnerName != "Meagan Henry")
freq(users$OwnerName)

txaccounts=merge(txaccounts,users,by="OwnerId")

write.csv(txaccounts[is.na(txaccounts$initialrisklevel)==TRUE,c("CaseNumber","OwnerName")],"~/Programs/CEO Dashboard Metrics/casesnoRL.csv",row.names = FALSE)

#################
## CAMS Outcomes
#################
#it might be possible to have a CAMS interim created before a CAMS initial

txformdata=txformdata[order(txformdata$Care_Plan__c,unclass(txformdata$CreatedDate)),]  #We want the CAMS sessions ordered in ascending order
camsformdata=txformdata%>%
  filter(Name=="CAMS Initial Session"|Name=="CAMS Interim Session"|Name=="CAMS Final Session")%>% 
  group_by(Care_Plan__c)%>%
  mutate(anycamsinitial=max(case_when(Name=="CAMS Initial Session"~1,TRUE~0)))%>%
  rename(Case__c=Care_Plan__c,CAMSsessiondate=CreatedDate,CAMSsession=Name)%>%
  filter(anycamsinitial==1)%>%
  mutate(daysbetweenCAMS=case_when(row_number()==1~0,TRUE~unclass(date(CAMSsessiondate)-lag(date(CAMSsessiondate)))),
         daysfromCAMSinitial=0,
         daysfromCAMSinitial=cumsum(daysbetweenCAMS), #case_when(row_number()==1~0,TRUE~cumsum(daysbetweenCAMS))
         daygroupsfromCAMSinitial=case_when(daysfromCAMSinitial==0~"0. Baseline",
                                            daysfromCAMSinitial>0 & daysfromCAMSinitial<=30~"1-30 Days",
                                            daysfromCAMSinitial>30 & daysfromCAMSinitial<=60~"31-60 Days",
                                            daysfromCAMSinitial>60 & daysfromCAMSinitial<=90~"61-90 Days",
                                            daysfromCAMSinitial>90 & daysfromCAMSinitial<=120~"91-120 Days",
                                            daysfromCAMSinitial>120 & daysfromCAMSinitial<=150~"121-150 Days",
                                            daysfromCAMSinitial>150 & daysfromCAMSinitial<=180~"151-180 Days",
                                            daysfromCAMSinitial>180~"181+ Days"),
         psychpainbase=max(case_when(row_number()==1~CAMS_RATE_Psychological_Pain__c,TRUE~0)),
         psychpainbdiff=CAMS_RATE_Psychological_Pain__c-psychpainbase,
         stressbase=max(case_when(row_number()==1~CAMS_RATE_STRESS__c,TRUE~0)),
         stressbdiff=CAMS_RATE_STRESS__c-stressbase,
         agitationbase=max(case_when(row_number()==1~CAMS_RATE_AGITATION__c,TRUE~0)),
         agitationbdiff=CAMS_RATE_AGITATION__c-agitationbase,
         hopelessbase=max(case_when(row_number()==1~CAMS_RATE_HOPELESSNESS__c,TRUE~0)),
         hopelessbdiff=CAMS_RATE_HOPELESSNESS__c-hopelessbase,
         selfhatebase=max(case_when(row_number()==1~CAMS_RATE_SELF_HATE__c,TRUE~0)),
         selfhatebdiff=CAMS_RATE_SELF_HATE__c-selfhatebase,
         suicideriskbase=max(case_when(row_number()==1~CAMS_RATE_OVERALL_SUICIDE_RISK__c,TRUE~0)),
         suicideriskbdiff=CAMS_RATE_OVERALL_SUICIDE_RISK__c-suicideriskbase,
         suicideriskpctdiff=suicideriskbdiff/suicideriskbase,
         weeksfromCAMSinitial=ceiling(daysfromCAMSinitial/7),
         nCAMSsessions=row_number())%>%
  group_by(weeksfromCAMSinitial)%>%
  mutate(nperweek=max(row_number()))%>%
  group_by(daysfromCAMSinitial)%>%
  mutate(nperday=max(row_number()))%>%
  select(Case__c,CAMSsessiondate,CAMSsession,daysbetweenCAMS,daysfromCAMSinitial,daygroupsfromCAMSinitial,nperday,weeksfromCAMSinitial,nperweek,nCAMSsessions,CAMS_RATE_Psychological_Pain__c, CAMS_RATE_STRESS__c, CAMS_RATE_AGITATION__c, CAMS_RATE_HOPELESSNESS__c, CAMS_RATE_SELF_HATE__c, CAMS_RATE_OVERALL_SUICIDE_RISK__c,psychpainbdiff,stressbdiff,agitationbdiff,hopelessbdiff,selfhatebdiff,suicideriskbdiff,suicideriskpctdiff)

freq(camsformdata$CAMSsession)
descr(camsformdata$nCAMSsessions)
descr(camsformdata$daysbetweenCAMS)

#Scores each week
p_pain <- ggplot(data = camsformdata, aes(x = weeksfromCAMSinitial, y = CAMS_RATE_Psychological_Pain__c, group = Case__c)) + stat_smooth(aes(group = 1))  + stat_summary(aes(group = 1,size = nperweek),geom = "point", fun.y = mean, shape = 16, colour="red" ) + xlab("Weeks from CAMS initial") + ylab("Rating of Psychological Pain") #+ ggtitle("Plot title")
p_pain
d_pain=ggplot_build(p_pain)$data[[1]]
painweek0=d_pain$y[1]
painweek4=d_pain$y[which.min(abs(d_pain$x-4))] #inside the brackets gives me the row with x value closest to 4.
painweek8=d_pain$y[which.min(abs(d_pain$x-8))]
painweek0
painweek4
painweek8
(painweek4-painweek0)/painweek0*100
(painweek8-painweek0)/painweek0*100

p_stress <- ggplot(data = camsformdata, aes(x = weeksfromCAMSinitial, y = CAMS_RATE_STRESS__c, group = Case__c)) + stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1,size = nperweek),geom = "point", fun.y = mean, shape = 16, colour="red" ) + xlab("Weeks from CAMS initial") + ylab("Rating of Stress")
p_stress
d_stress=ggplot_build(p_stress)$data[[1]]
stressweek0=d_stress$y[1]
stressweek4=d_stress$y[which.min(abs(d_stress$x-4))] 
stressweek8=d_stress$y[which.min(abs(d_stress$x-8))]
stressweek0
stressweek4
stressweek8
(stressweek4-stressweek0)/stressweek0*100
(stressweek8-stressweek0)/stressweek0*100

p_agitation <- ggplot(data = camsformdata, aes(x = weeksfromCAMSinitial, y = CAMS_RATE_AGITATION__c, group = Case__c)) + stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1,size = nperweek),geom = "point", fun.y = mean, shape = 16, colour="red" ) + xlab("Weeks from CAMS initial") + ylab("Rating of Agitation")
p_agitation
d_agitation=ggplot_build(p_agitation)$data[[1]]
agitationweek0=d_agitation$y[1]
agitationweek4=d_agitation$y[which.min(abs(d_agitation$x-4))] 
agitationweek8=d_agitation$y[which.min(abs(d_agitation$x-8))]
agitationweek0
agitationweek4
agitationweek8
(agitationweek4-agitationweek0)/agitationweek0*100
(agitationweek8-agitationweek0)/agitationweek0*100

p_hopeless <- ggplot(data = camsformdata, aes(x = weeksfromCAMSinitial, y = CAMS_RATE_HOPELESSNESS__c, group = Case__c)) + stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1,size = nperweek),geom = "point", fun.y = mean, shape = 16, colour="red" ) + xlab("Weeks from CAMS initial") + ylab("Rating of Hopelessness")
p_hopeless
d_hopeless=ggplot_build(p_hopeless)$data[[1]]
hopelessweek0=d_hopeless$y[1]
hopelessweek4=d_hopeless$y[which.min(abs(d_hopeless$x-4))] 
hopelessweek8=d_hopeless$y[which.min(abs(d_hopeless$x-8))]
hopelessweek0
hopelessweek4
hopelessweek8
(hopelessweek4-hopelessweek0)/hopelessweek0*100
(hopelessweek8-hopelessweek0)/hopelessweek0*100

p_hate <- ggplot(data = camsformdata, aes(x = weeksfromCAMSinitial, y = CAMS_RATE_SELF_HATE__c, group = Case__c)) + stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1,size = nperweek),geom = "point", fun.y = mean, shape = 16, colour="red" ) + xlab("Weeks from CAMS initial") + ylab("Rating of Self Hate")
p_hate 
d_hate=ggplot_build(p_hate)$data[[1]]
hateweek0=d_hate$y[1]
hateweek4=d_hate$y[which.min(abs(d_hate$x-4))] 
hateweek8=d_hate$y[which.min(abs(d_hate$x-8))]
hateweek0
hateweek4
hateweek8
(hateweek4-hateweek0)/hateweek0*100
(hateweek8-hateweek0)/hateweek0*100

p_suiciderisk <- ggplot(data = camsformdata, aes(x = weeksfromCAMSinitial, y = CAMS_RATE_OVERALL_SUICIDE_RISK__c, group = Case__c)) + stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1,size = nperweek),geom = "point", fun.y = mean, shape = 16, colour="red" ) + xlab("Weeks from CAMS initial") + ylab("Rating of Overall Suicide Risk") 
p_suiciderisk
d_suiciderisk=ggplot_build(p_suiciderisk)$data[[1]]
suicideriskweek0=d_suiciderisk$y[1]
suicideriskweek4=d_suiciderisk$y[which.min(abs(d_suiciderisk$x-4))] 
suicideriskweek8=d_suiciderisk$y[which.min(abs(d_suiciderisk$x-8))]
suicideriskweek0
suicideriskweek4
suicideriskweek8
(suicideriskweek4-suicideriskweek0)/suicideriskweek0*100
(suicideriskweek8-suicideriskweek0)/suicideriskweek0*100

#Difference from baseline each week
p_pain2 <- ggplot(data = camsformdata, aes(x = weeksfromCAMSinitial, y = psychpainbdiff, group = Case__c)) + stat_smooth(aes(group = 1))  + stat_summary(aes(group = 1,size = nperweek),geom = "point", fun.y = mean, shape = 16, colour="red" ) + xlab("Weeks from CAMS initial") + ylab("Diff From Baseline Rating of Psychological Pain") #+ ggtitle("Plot title")
p_pain2
d_pain2=ggplot_build(p_pain2)$data[[1]]
pain2week0=d_pain2$y[1]
pain2week4=d_pain2$y[which.min(abs(d_pain2$x-4))] #inside the brackets gives me the row with x value closest to 4.
pain2week8=d_pain2$y[which.min(abs(d_pain2$x-8))]
pain2week0
pain2week4
pain2week8
pain2week4/5*100
pain2week8/5*100

p_stress2 <- ggplot(data = camsformdata, aes(x = weeksfromCAMSinitial, y = stressbdiff, group = Case__c)) + stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1,size = nperweek),geom = "point", fun.y = mean, shape = 16, colour="red" ) + xlab("Weeks from CAMS initial") + ylab("Diff From Baseline Rating of Stress")
p_stress2
d_stress2=ggplot_build(p_stress2)$data[[1]]
stress2week0=d_stress2$y[1]
stress2week4=d_stress2$y[which.min(abs(d_stress2$x-4))] 
stress2week8=d_stress2$y[which.min(abs(d_stress2$x-8))]
stress2week0
stress2week4
stress2week8
stress2week4/5*100
stress2week8/5*100

p_agitation2 <- ggplot(data = camsformdata, aes(x = weeksfromCAMSinitial, y = agitationbdiff, group = Case__c)) + stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1,size = nperweek),geom = "point", fun.y = mean, shape = 16, colour="red" ) + xlab("Weeks from CAMS initial") + ylab("Diff From Baseline Rating of Agitation")
p_agitation2
d_agitation2=ggplot_build(p_agitation2)$data[[1]]
agitation2week0=d_agitation2$y[1]
agitation2week4=d_agitation2$y[which.min(abs(d_agitation2$x-4))] 
agitation2week8=d_agitation2$y[which.min(abs(d_agitation2$x-8))]
agitation2week0
agitation2week4
agitation2week8
agitation2week4/5*100
agitation2week8/5*100

p_hopeless2 <- ggplot(data = camsformdata, aes(x = weeksfromCAMSinitial, y = hopelessbdiff, group = Case__c)) + stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1,size = nperweek),geom = "point", fun.y = mean, shape = 16, colour="red" ) + xlab("Weeks from CAMS initial") + ylab("Diff From Baseline Rating of Hopelessness")
p_hopeless2
d_hopeless2=ggplot_build(p_hopeless2)$data[[1]]
hopeless2week0=d_hopeless2$y[1]
hopeless2week4=d_hopeless2$y[which.min(abs(d_hopeless2$x-4))] 
hopeless2week8=d_hopeless2$y[which.min(abs(d_hopeless2$x-8))]
hopeless2week0
hopeless2week4
hopeless2week8
hopeless2week4/5*100
hopeless2week8/5*100

p_hate2 <- ggplot(data = camsformdata, aes(x = weeksfromCAMSinitial, y = selfhatebdiff, group = Case__c)) + stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1,size = nperweek),geom = "point", fun.y = mean, shape = 16, colour="red" ) + xlab("Weeks from CAMS initial") + ylab("Diff From Baseline Rating of Self Hate")
p_hate2 
d_hate2=ggplot_build(p_hate2)$data[[1]]
hate2week0=d_hate2$y[1]
hate2week4=d_hate2$y[which.min(abs(d_hate2$x-4))] 
hate2week8=d_hate2$y[which.min(abs(d_hate2$x-8))]
hate2week0
hate2week4
hate2week8
hate2week4/5*100
hate2week8/5*100

p_suiciderisk2 <- ggplot(data = camsformdata, aes(x = weeksfromCAMSinitial, y = suicideriskbdiff, group = Case__c)) + stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1,size = nperweek),geom = "point", fun.y = mean, shape = 16, colour="red" ) + xlab("Weeks from CAMS initial") + ylab("Diff From Baseline Rating of Overall Suicide Risk") 
p_suiciderisk2
d_suiciderisk2=ggplot_build(p_suiciderisk2)$data[[1]]
suiciderisk2week0=d_suiciderisk2$y[1]
suiciderisk2week4=d_suiciderisk2$y[which.min(abs(d_suiciderisk2$x-4))] 
suiciderisk2week8=d_suiciderisk2$y[which.min(abs(d_suiciderisk2$x-8))]
suiciderisk2week0
suiciderisk2week4
suiciderisk2week8
suiciderisk2week4/5*100
suiciderisk2week8/5*100


p_suiciderisk3 <- ggplot(data = camsformdata, aes(x = weeksfromCAMSinitial, y = suicideriskpctdiff, group = Case__c)) + stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1,size = nperweek),geom = "point", fun.y = mean, shape = 16, colour="red" ) + xlab("Weeks from CAMS initial") + ylab("Diff From Baseline Rating of Overall Suicide Risk") 
p_suiciderisk3
d_suiciderisk3=ggplot_build(p_suiciderisk3)$data[[1]]
suiciderisk3week0=d_suiciderisk3$y[1]
suiciderisk3week4=d_suiciderisk3$y[which.min(abs(d_suiciderisk3$x-4))] 
suiciderisk3week8=d_suiciderisk3$y[which.min(abs(d_suiciderisk3$x-8))]
suiciderisk3week0
suiciderisk3week4
suiciderisk3week8



# p_pain <- ggplot(data = camsformdata, aes(x = daysfromCAMSinitial, y = CAMS_RATE_Psychological_Pain__c, group = Case__c))
# p_stress <- ggplot(data = camsformdata, aes(x = daysfromCAMSinitial, y = CAMS_RATE_STRESS__c, group = Case__c))
# p_agitation <- ggplot(data = camsformdata, aes(x = daysfromCAMSinitial, y = CAMS_RATE_AGITATION__c, group = Case__c))
# p_hopeless <- ggplot(data = camsformdata, aes(x = daysfromCAMSinitial, y = CAMS_RATE_HOPELESSNESS__c, group = Case__c))
# p_hate <- ggplot(data = camsformdata, aes(x = daysfromCAMSinitial, y = CAMS_RATE_SELF_HATE__c, group = Case__c))
# p_suiciderisk <- ggplot(data = camsformdata, aes(x = daysfromCAMSinitial, y = CAMS_RATE_OVERALL_SUICIDE_RISK__c, group = Case__c))
# 
# p_pain + stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1),geom = "point", fun.y = mean, shape = 20, size = 3) #circle shapes: 1, 16, 20
# p_stress + stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1),geom = "point", fun.y = mean, shape = 20, size = 3)
# p_agitation + stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1),geom = "point", fun.y = mean, shape = 20, size = 3)
# p_hopeless + stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1),geom = "point", fun.y = mean, shape = 20, size = 3)
# p_hate + stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1),geom = "point", fun.y = mean, shape = 20, size = 3)
# p_suiciderisk + stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1),geom = "point", fun.y = mean, shape = 20, size = 3)

#how to get a prediction from the stat_smooth algorithm in ggplot
#does stat_smooth weight each dot the same? or is it by sample size?


# p_pain2 <- ggplot(data = camsformdata, aes(x = daygroupsfromCAMSinitial, y = CAMS_RATE_Psychological_Pain__c, group = Case__c))
# p_pain2 + stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1),geom = "point", fun.y = mean, shape = 20, size = 3) #circle shapes: 1, 16, 20
# #geom_jitter() n()
# #geom_point()
# #geom_point(aes(size=nperweek))
# p_stress2 <- ggplot(data = camsformdata, aes(x = daygroupsfromCAMSinitial, y = CAMS_RATE_STRESS__c, group = Case__c))
# p_stress2 + stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1),geom = "point", fun.y = mean, shape = 20, size = 3)
# 
# p_agitation2 <- ggplot(data = camsformdata, aes(x = daygroupsfromCAMSinitial, y = CAMS_RATE_AGITATION__c, group = Case__c))
# p_agitation2 + stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1),geom = "point", fun.y = mean, shape = 20, size = 3)
# 
# p_hopeless2 <- ggplot(data = camsformdata, aes(x = daygroupsfromCAMSinitial, y = CAMS_RATE_HOPELESSNESS__c, group = Case__c))
# p_hopeless2 + stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1),geom = "point", fun.y = mean, shape = 20, size = 3)
# 
# p_hate2 <- ggplot(data = camsformdata, aes(x = daygroupsfromCAMSinitial, y = CAMS_RATE_SELF_HATE__c, group = Case__c))
# p_hate2 + stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1),geom = "point", fun.y = mean, shape = 20, size = 3)
# 
# p_suiciderisk2 <- ggplot(data = camsformdata, aes(x = daygroupsfromCAMSinitial, y = CAMS_RATE_OVERALL_SUICIDE_RISK__c, group = Case__c))
# p_suiciderisk2 + stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1),geom = "point", fun.y = mean, shape = 20, size = 3)


# ggplot(camsformdata, aes(daysfromCAMSinitial, CAMS_RATE_Psychological_Pain__c)) +
#   geom_violin() +
#   geom_boxplot(width = 0.1, outlier.colour = "blue") +
#   theme_classic()

#make plots that show the absolute change from their baseline, and the percent change from their baseline


#################
## MWB Outcomes
#################
txformdata=txformdata[order(txformdata$Care_Plan__c,unclass(txformdata$CreatedDate)),]  #We want the CAMS sessions ordered in ascending order
mwbformdata=txformdata%>%
  filter(Name=="Baseline Core Outcomes Questionnaire"|Name=="Follow-up Core Outcomes Questionnaire")%>% 
  group_by(Care_Plan__c)%>%
  mutate(anybcoq=max(case_when(Name=="Baseline Core Outcomes Questionnaire"~1,TRUE~0)),
         MWBnqresponses=(is.na(Future_Optimism__c)==FALSE)*1 + (is.na(Feeling_Useful__c)==FALSE)*1 + (is.na(Relaxation__c)==FALSE)*1 + (is.na(Problem_Handling__c)==FALSE)*1 + (is.na(Clear_Thinking__c)==FALSE)*1 + (is.na(Social_Closeness__c)==FALSE)*1 + (is.na(Independent_Decisions__c)==FALSE)*1,
         MWBscore=case_when(MWBnqresponses==7 ~ Future_Optimism__c+Feeling_Useful__c+Relaxation__c+Problem_Handling__c+Clear_Thinking__c+Social_Closeness__c+Independent_Decisions__c))%>%
  rename(Case__c=Care_Plan__c,MWBdate=CreatedDate,FormName=Name)%>%
  filter(anybcoq==1,MWBnqresponses==7)%>% #Only include complete MWB scores that have responses to all 7 questions
  mutate(MWBscorechange=MWBscore-lag(MWBscore),
         daysbetweenMWB=case_when(row_number()==1~0,TRUE~unclass(date(MWBdate)-lag(date(MWBdate)))),
         daysfromMWBinitial=cumsum(daysbetweenMWB),
         daygroupsfromMWBinitial=case_when(daysfromMWBinitial==0~"0. Baseline",
                                            daysfromMWBinitial>0 & daysfromMWBinitial<=30~"1-30 Days",
                                            daysfromMWBinitial>30 & daysfromMWBinitial<=60~"31-60 Days",
                                            daysfromMWBinitial>60 & daysfromMWBinitial<=90~"61-90 Days",
                                            daysfromMWBinitial>90 & daysfromMWBinitial<=120~"91-120 Days",
                                            daysfromMWBinitial>120 & daysfromMWBinitial<=150~"121-150 Days",
                                            daysfromMWBinitial>150 & daysfromMWBinitial<=180~"151-180 Days",
                                            daysfromMWBinitial>180~"181+ Days"),
         MWBbase=max(case_when(row_number()==1~MWBscore,TRUE~0)),
         MWBbdiff=MWBscore-MWBbase,
         weeksfromMWBinitial=ceiling(daysfromMWBinitial/7),
         nMWBassessments=row_number())%>% 
  group_by(weeksfromMWBinitial)%>%
  mutate(nperweek=max(row_number()))%>%
  group_by(daysfromMWBinitial)%>%
  mutate(nperday=max(row_number()))%>%
  select(Case__c,MWBdate,FormName,daysbetweenMWB,daysfromMWBinitial,daygroupsfromMWBinitial,nperday,weeksfromMWBinitial,nperweek,nMWBassessments,MWBscore,MWBnqresponses,MWBscorechange,MWBbdiff )

freq(mwbformdata$FormName)
descr(mwbformdata$nMWBassessments) #There are no cases that have a follow-up 
descr(mwbformdata$daysbetweenMWB) 


#Scores each week
p_MWB <- ggplot(data = mwbformdata, aes(x = weeksfromMWBinitial, y = MWBscore, group = Case__c)) + stat_smooth(aes(group = 1))  + stat_summary(aes(group = 1,size = nperweek),geom = "point", fun.y = mean, shape = 16, colour="red" ) + xlab("Weeks from First Questionnaire") + ylab("Mental Well-being Score") 
p_MWB
descr(mwbformdata$MWBscore)
# d_MWB=ggplot_build(p_MWB)$data[[1]]
# MWBweek0=d_MWB$y[1]
# MWBweek4=d_MWB$y[which.min(abs(d_MWB$x-4))] #inside the brackets gives me the row with x value closest to 4.
# MWBweek8=d_MWB$y[which.min(abs(d_MWB$x-8))]
# MWBweek0
# MWBweek4
# MWBweek8
# (MWBweek4-painweek0)/MWBweek0*100
# (MWBweek8-painweek0)/MWBweek0*100


################################################
## Never Spoke about Suicide before Outcomes
################################################
txformdata=txformdata[order(txformdata$Care_Plan__c,unclass(txformdata$CreatedDate)),]  #We want the CAMS sessions ordered in ascending order
talksuicideformdata=txformdata%>%
  filter(Name=="Baseline Core Outcomes Questionnaire")%>% 
  #group_by(Care_Plan__c)%>%
  rename(Case__c=Care_Plan__c,FormDate=CreatedDate,FormName=Name)%>%
  mutate(talkedsuicide_friends=grepl("Friend/s",COQ_B_Talked_About_Suicide_Who__c,fixed=TRUE)*1,
         talkedsuicide_family=grepl("Family member/s",COQ_B_Talked_About_Suicide_Who__c,fixed=TRUE)*1,
         talkedsuicide_va=grepl("Veterans Health Administration or the VA",COQ_B_Talked_About_Suicide_Who__c,fixed=TRUE)*1,
         talkedsuicide_vamhprovider=grepl("VA mental health provider",COQ_B_Talked_About_Suicide_Who__c,fixed=TRUE)*1,
         talkedsuicide_dodmhprovider=grepl("DoD mental health provider",COQ_B_Talked_About_Suicide_Who__c,fixed=TRUE)*1,
         talkedsuicide_othermhprovider=grepl("Mental health provider outside of the VA or DoD",COQ_B_Talked_About_Suicide_Who__c,fixed=TRUE)*1,
         talkedsuicide_chainofcommand=grepl("Current or prior chain of command in the military",COQ_B_Talked_About_Suicide_Who__c,fixed=TRUE)*1,
         talkedsuicide_smsinunit=grepl("Other service members in your unit",COQ_B_Talked_About_Suicide_Who__c,fixed=TRUE)*1,
         talkedsuicide_othervso=grepl("Other veteran serving organization/s (not including ROGER)",COQ_B_Talked_About_Suicide_Who__c,fixed=TRUE)*1,
         talkedsuicide_ehp=grepl("Employee health program",COQ_B_Talked_About_Suicide_Who__c,fixed=TRUE)*1,
         talkedsuicide_militaryonesource=grepl("Military OneSource",COQ_B_Talked_About_Suicide_Who__c,fixed=TRUE)*1,
         talkedsuicide_vcl=grepl("Veterans Crisis Line",COQ_B_Talked_About_Suicide_Who__c,fixed=TRUE)*1,
         talkedsuicide_othercrisisline=grepl("Other crisis line",COQ_B_Talked_About_Suicide_Who__c,fixed=TRUE)*1,
         talkedsuicide_religiousleader=grepl("Religious leader or affiliate",COQ_B_Talked_About_Suicide_Who__c,fixed=TRUE)*1,
         talkedsuicide_other=(grepl("Other;",COQ_B_Talked_About_Suicide_Who__c,fixed=TRUE)|grepl("Other(?!\\s)",COQ_B_Talked_About_Suicide_Who__c,fixed=TRUE))*1)%>% 
  #filter(FormDate==min(FormDate))%>%
  select(Case__c,FormDate,FormName,COQ_B_Talked_About_Suicide__c, COQ_B_Talked_About_Suicide_Who__c, talkedsuicide_friends, talkedsuicide_family, talkedsuicide_va, talkedsuicide_vamhprovider, talkedsuicide_dodmhprovider, talkedsuicide_othermhprovider, talkedsuicide_chainofcommand, talkedsuicide_smsinunit, talkedsuicide_othervso, talkedsuicide_ehp, talkedsuicide_militaryonesource, talkedsuicide_vcl, talkedsuicide_othercrisisline, talkedsuicide_religiousleader, talkedsuicide_other, COQ_B_Talked_About_Suicide_Other__c,COQ_Fleeting_Thoughts__c, COQ_Current_Thoughts__c)

freq(talksuicideformdata$FormName)
dfSummary(talksuicideformdata)
ctable(talksuicideformdata$COQ_B_Talked_About_Suicide__c,talksuicideformdata$COQ_Fleeting_Thoughts__c)

newtalksuicide=talksuicideformdata%>%
  filter(COQ_B_Talked_About_Suicide__c=="No")%>%
  select(Case__c, FormDate)

newtalksuicide=merge(newtalksuicide,txaccounts,by="Case__c",all.x=TRUE)

freq(newtalksuicide$initialrisklevel2)
freq(newtalksuicide$Status_Change_Reason__c)

##################
## BSCS Outcomes
##################
txformdata=txformdata[order(txformdata$Care_Plan__c,unclass(txformdata$CreatedDate)),]  #We want the CAMS sessions ordered in ascending order
bscsformdata=txformdata%>%
  filter(Name=="Baseline Core Outcomes Questionnaire"|Name=="Follow-up Core Outcomes Questionnaire"|Name=="Brief Suicide Cognitions Scale (B-SCS)"|Name=="Checklist: Intake Lite")%>% 
  group_by(Care_Plan__c)%>%
  mutate(anybcoq=max(case_when(Name=="Baseline Core Outcomes Questionnaire"|Name=="Checklist: Intake Lite"~1,TRUE~0)), #it is possible that they received a BSCS initially during an Intake Lite
         BSCSnqresponses=(is.na(BSCS_Self_Worth__c)==FALSE)*1 + (is.na(BSCS_Problem_Help__c)==FALSE)*1 + (is.na(BSCS_Coping_Struggle__c)==FALSE)*1 + (is.na(BSCS_Enduring_Pain__c)==FALSE)*1 + (is.na(BSCS_Self_Redeeming__c)==FALSE)*1 + (is.na(BSCS_Suicide_Thoughts__c)==FALSE)*1 ,
         BSCSscore=case_when(BSCSnqresponses==6 ~ BSCS_Self_Worth__c+BSCS_Problem_Help__c+BSCS_Coping_Struggle__c+BSCS_Enduring_Pain__c+BSCS_Self_Redeeming__c+BSCS_Suicide_Thoughts__c))%>%
  rename(Case__c=Care_Plan__c,BSCSdate=CreatedDate,FormName=Name)%>%
  filter(anybcoq==1,BSCSnqresponses==6)%>% #Only include complete BSCS scores that have responses to all 6 questions
  mutate(BSCSscorechange=BSCSscore-lag(BSCSscore),
         daysbetweenBSCS=case_when(row_number()==1~0,TRUE~unclass(date(BSCSdate)-lag(date(BSCSdate)))),
         daysfromBSCSinitial=cumsum(daysbetweenBSCS),
         daygroupsfromBSCSinitial=case_when(daysfromBSCSinitial==0~"0. Baseline",
                                           daysfromBSCSinitial>0 & daysfromBSCSinitial<=30~"1-30 Days",
                                           daysfromBSCSinitial>30 & daysfromBSCSinitial<=60~"31-60 Days",
                                           daysfromBSCSinitial>60 & daysfromBSCSinitial<=90~"61-90 Days",
                                           daysfromBSCSinitial>90 & daysfromBSCSinitial<=120~"91-120 Days",
                                           daysfromBSCSinitial>120 & daysfromBSCSinitial<=150~"121-150 Days",
                                           daysfromBSCSinitial>150 & daysfromBSCSinitial<=180~"151-180 Days",
                                           daysfromBSCSinitial>180~"181+ Days"),
         BSCSbase=max(case_when(row_number()==1~BSCSscore,TRUE~0)),
         BSCSbdiff=BSCSscore-BSCSbase,
         weeksfromBSCSinitial=ceiling(daysfromBSCSinitial/7),
         nBSCSassessments=row_number())%>% 
  group_by(weeksfromBSCSinitial)%>%
  mutate(nperweek=max(row_number()))%>%
  group_by(daysfromBSCSinitial)%>%
  mutate(nperday=max(row_number()))%>%
  select(Case__c,BSCSdate,FormName,daysbetweenBSCS,daysfromBSCSinitial,daygroupsfromBSCSinitial,nperday,weeksfromBSCSinitial,nperweek,nBSCSassessments,BSCSscore,BSCSnqresponses,BSCSscorechange,BSCSbdiff )

freq(bscsformdata$FormName)
descr(bscsformdata$nBSCSassessments)
descr(bscsformdata$daysbetweenBSCS) 

#Scores each week
p_BSCS <- ggplot(data = bscsformdata, aes(x = weeksfromBSCSinitial, y = BSCSscore, group = Case__c)) + stat_smooth(aes(group = 1))  + stat_summary(aes(group = 1,size = nperweek),geom = "point", fun.y = mean, shape = 16, colour="red" ) + xlab("Weeks from First Questionnaire") + ylab("Brief Suicide Cognitions Scale Score") 
p_BSCS #not enough points right now for the stat_smooth function
# p_BSCS <- ggplot(data = bscsformdata, aes(x = daysfromBSCSinitial, y = BSCSscore, group = Case__c)) + stat_smooth(aes(group = 1))  + stat_summary(aes(group = 1),geom = "point", fun.y = mean, shape = 16, colour="red" ) + xlab("Weeks from First Questionnaire") + ylab("Brief Suicide Cognitions Scale Score") 
# p_BSCS
descr(bscsformdata$BSCSscore)
# d_BSCS=ggplot_build(p_BSCS)$data[[1]]
# BSCSweek0=d_BSCS$y[1]
# BSCSweek4=d_BSCS$y[which.min(abs(d_BSCS$x-4))] #inside the brackets gives me the row with x value closest to 4.
# BSCSweek8=d_BSCS$y[which.min(abs(d_BSCS$x-8))]
# BSCSweek0
# BSCSweek4
# BSCSweek8
# (BSCSweek4-painweek0)/BSCSweek0*100
# (BSCSweek8-painweek0)/BSCSweek0*100

# mutate(bscsscore=BSCS_Self_Worth__c+BSCS_Problem_Help__c+BSCS_Coping_Struggle__c+BSCS_Enduring_Pain__c+BSCS_Self_Redeeming__c+BSCS_Suicide_Thoughts__c,
#        bscsscorechange=bscsscore-lag(bscsscore),
#        daysbetweenbscs=case_when(row_number()==1~0,TRUE~unclass(date(BSCSdate)-lag(date(BSCSdate))))
# )%>%

###############################
#NPS and Feedback Analysis
###############################
feedbackformdata=txformdata%>%
  filter(Name=="Feedback Survey")%>%
  select(Id, Care_Plan__c, Name, CreatedDate, FSS_NPS_Question__c)
  
freq(feedbackformdata$FSS_NPS_Question__c)

feedbackformdata=feedbackformdata[order(feedbackformdata$Care_Plan__c,-unclass(feedbackformdata$CreatedDate)),] #sort in descending order within each care plan so that when I filter I only keep the most recent
feedbackformdata=feedbackformdata%>%
  group_by(Care_Plan__c)%>%
  rename(Case__c=Care_Plan__c)%>%
  filter(row_number()==1)%>%
  mutate(Promoter=case_when(FSS_NPS_Question__c==9 | FSS_NPS_Question__c==10~1,is.na(FSS_NPS_Question__c)==FALSE~0),
         Detractor=case_when(FSS_NPS_Question__c %in% c(1,2,3,4,5,6)~1,is.na(FSS_NPS_Question__c)==FALSE~0))

freq(feedbackformdata$Promoter)
freq(feedbackformdata$Detractor)


# storiesformdata=txformdata%>%
#   filter(Name=="Feedback Survey"|Name=="Follow-up Core Outcomes Questionnaire")%>%
#   select(Id, Care_Plan__c, Name, CreatedDate, COQ_F_ROGER_Prevent_Attempt__c, COQ_F_ROGER_Prevent_Attempt_Details__c, COQ_F_Consent_For_Attempt_Details__c, FSS_Share_With_Others_Txt__c, FSS_Consent_to_Share__c, FSS_Learned_from_ROGER_Txt__c, FSS_ROGER_Improvement_Txt__c)

###################
## Stories
###################

stories=txformdata%>%
  filter(Name=="Feedback Survey" | Name=="Follow-up Core Outcomes Questionnaire")%>%
  mutate(consent2share=case_when(COQ_F_Consent_For_Attempt_Details__c=="Yes"|FSS_Consent_to_Share__c=="Yes"~"Yes",
                                  TRUE~"No"),
         Promoter=case_when(FSS_NPS_Question__c==9 | FSS_NPS_Question__c==10~1,is.na(FSS_NPS_Question__c)==FALSE~0),
         Detractor=case_when(FSS_NPS_Question__c %in% c(1,2,3,4,5,6)~1,is.na(FSS_NPS_Question__c)==FALSE~0))%>%
  select(consent2share,FSS_NPS_Question__c,Promoter,Detractor,COQ_F_ROGER_Prevent_Attempt__c, COQ_F_ROGER_Prevent_Attempt_Details__c,  FSS_Share_With_Others_Txt__c, FSS_Learned_from_ROGER_Txt__c, FSS_ROGER_Improvement_Txt__c)%>%
  rename('Consent to share stories'=consent2share,
         'NPS rating'=FSS_NPS_Question__c,
         'Did ROGER prevent an attempt?'=COQ_F_ROGER_Prevent_Attempt__c,
         'Prevented attempt - details'=COQ_F_ROGER_Prevent_Attempt_Details__c,  
         'What would you share with others?'=FSS_Share_With_Others_Txt__c, 
         'What did you learn from ROGER that you plan to use day-to-day?'=FSS_Learned_from_ROGER_Txt__c, 
         'What could ROGER have done differently?'=FSS_ROGER_Improvement_Txt__c)
  
write.csv(stories,"~/Programs/CEO Dashboard Metrics/stories.csv",row.names = FALSE)


########################
## Risk Assessment Data
########################

casenotes=txcasenotes%>%
  rename(CaseNoteId=Id,CaseNoteCreatedDate=CreatedDate,CNrisklevel=initialrisklevel)%>%
  select(-cams_bcbt,-cams,-bcbt)

casenotes=merge(txcases,casenotes,by="Case__c", all.x=TRUE) #restrict case notes to the ones in our txcases cohort
riskcasenotes=casenotes%>%
  filter(is.na(CNrisklevel)==FALSE)%>%
  rename(callid1=Lookup_Primary_Phone_Call__c,callid2=Lookup_Secondary_Phone_Call__c,callid3=Lookup_Tertiary_Phone_Call__c)%>%
  select(CaseNumber, Case__c, AccountId, callid1, callid2, callid3, CNrisklevel, Acute_Risk_Level__c, Chronic_Risk_Level__c, Risk_Level_Change_Reason__c, History_of_Chronic_Ideation__c, History_of_Self_Directed_Violence__c, History_of_Attempts__c, Chronic_Major_Mental_Diagnosis__c, Chronic_Major_Personality_Diagnosis__c, History_of_Substance_Misuse_Abuse__c, Chronic_Major_Medical_Diagnosis__c, Chronic_Pain__c, Limited_Coping_Skills_Chronic__c, Unstable_Psychosocial_Status__c, Limited_Reasons_for_Living__c, History_of_Risky_Actions__c, History_of_Impulsivity__c, Current_Suicidal_Ideation_even_passive__c, Inability_to_Stay_Safe__c, Intent_or_Wish_to_Die__c, Set_Method_of_Choice_or_Plan__c, Recent_Attempt__c, More_than_One_Attempt__c, Ongoing_Preparatory_Behavior__c, Will_Not_Use_CRP__c, Limited_Coping_Skills_Acute__c, Access_to_Means__c, Acute_Major_Illness__c, Intensified_Personality_Disorder__c, Acute_Psychosocial_Stressors__c)

dfSummary(riskcasenotes)

#look for more recent case notes and completion by people

#examine the frequency of risk factor scales

#the transcripts are not being read into R from the CCA file, maybe because the field is too large? 
txcalls=sf_query(paste("SELECT ",gv_txfields5," FROM ",gv_txsource5," WHERE Id IN (",paste("'",unique(riskcasenotes$callid1[is.na(riskcasenotes$callid1)==FALSE]),"'",sep="",collapse=","),")"))
txcalls=txcalls%>%
  rename(Case__c=Id,CaseCreatedDate=CreatedDate)

riskcasenotes=merge(riskcasenotes,txcalls,by="Case__c")



