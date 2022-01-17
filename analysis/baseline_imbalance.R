library(kableExtra)
library(stargazer)
library(sandwich)
library(haven)
library(tidyverse)
library(miceadds)
library(MatchIt)


df_main<-read_dta('https://www.dropbox.com/s/yxgigmtcrut9fii/yop_analysis.dta?dl=1') 

# list of vars
b<-df_main %>%
  mutate(nonag_hrs=lowskill7da_zero+lowbus7da_zero+skilledtrade7da_zero+highskill7da_zero+acto7da_zero) %>%
  mutate(weeklyemp=aghours7da_zero+nonag_hrs) %>%
  mutate(skilledtrade=ifelse(skilledtrade7da_zero>0,1,0)) %>%
  dplyr::select(admin_cost_us,group_size,grantsize_pp_US_est3,group_existed,
                group_age,ingroup_hetero,ingroup_dynamic,avgdisteduc,
                ind_found_b,age,female,urban,risk_aversion,grp_leader,
                grp_chair,weeklyemp,nonag_hrs,lowskill7da_zero,lowbus7da_zero,
                skilledtrade7da_zero,highskill7da_zero,acto7da_zero,aghours7da_zero,
                chores7da_zero,zero_hours,nonag_dummy,skilledtrade,
                inschool,education,literate,voc_training,numeracy_numcorrect_m,
                adl,wealthindex,savings_6mo_p99,cash4w_p99,loan_100k,loan_1mil,
                S_H,S_K,S_P_m,aggression_n)

vars<-colnames(b)

# create df 

clustdiff<-data.frame(vars,mean=1:length(vars),pvalue=1:length(vars))

df_diff<-df_main %>% filter(e1==1) %>%
  mutate(nonag_hrs=lowskill7da_zero+lowbus7da_zero+skilledtrade7da_zero+highskill7da_zero+acto7da_zero) %>%
  mutate(weeklyemp=aghours7da_zero+nonag_hrs) %>%
  mutate(skilledtrade=ifelse(skilledtrade7da_zero>0,1,0)) %>%
  dplyr::select(admin_cost_us,group_size,grantsize_pp_US_est3,group_existed,
                group_age,ingroup_hetero,ingroup_dynamic,avgdisteduc,
                ind_found_b,age,female,urban,risk_aversion,grp_leader,
                grp_chair,weeklyemp,nonag_hrs,lowskill7da_zero,lowbus7da_zero,
                skilledtrade7da_zero,highskill7da_zero,acto7da_zero,aghours7da_zero,
                chores7da_zero,zero_hours,nonag_dummy,skilledtrade,
                inschool,education,literate,voc_training,numeracy_numcorrect_m,
                adl,wealthindex,savings_6mo_p99,cash4w_p99,loan_100k,loan_1mil,
                S_H,S_K,S_P_m,aggression_n,
                D_1,D_2,D_3,D_4,D_5,D_6,D_7,D_8,D_9,D_10,D_11,D_12,D_13,
                assigned, groupid,) %>%
  .[which(complete.cases(.)),]

form<-c('assigned','D_1','D_2','D_3','D_4','D_5','D_6','D_7','D_8',
        'D_9','D_10','D_11','D_12','D_13')

for (i in 1:length(vars)){
  mod<-lm.cluster(reformulate(form,vars[i]),
                data=df_diff,cluster='groupid')
  clustdiff[i,2:3]<-round(summary(mod)[2,c(1,4)],3)
}

clustdiff%>%arrange(desc(pvalue))

df_treat<-df_diff%>%filter(assigned==1)%>%.[,1:42]
df_ctrl<-df_diff%>%filter(assigned==0)%>%.[,1:42] 
#control 
cmean<-round(apply(df_ctrl,2,mean),3)
csd<-round(apply(df_ctrl,2,sd),3)

#treat
tmean<-round(apply(df_treat,2,mean),3)
tsd<-round(apply(df_treat,2,sd),3)

sumtable<-cbind(cmean,csd,tmean,tsd,clustdiff[,2:3])

rownames(sumtable)<-c('Grant amount applied for, USD',
                      'Group size','Grant amount per member, USD',
                      'Group existed before application',
                      'Group age, in years','Within-group heterogeneity (z-score)',
                      'Quality of group dynamic (z-score)',
                      'Distance to educational facilities (km)',
                      'Individual unfound at baseline',
                      'Age at baseline',
                      'Female','Large town/urban area',
                      'Risk aversion index (z-score)',
                      'Any leadership position in group',
                      'Group chair or vice-chair','Weekly employment, hours',
                      'All nonagricultural work','Casual labor, low skill',
                      'Petty business, low skill','Skilled trades','High-skill wage labor',
                      'Other nonagricultural work','All agricultural work',
                      'Weekly household chores, hours','Zero employment hours in past month',
                      'Main occupation is nonagricultural','Engaged in a skilled trade',
                      'Currently in school','Highest grade reached at school',
                      'Able to read and write minimally','Received prior vocational training',
                      'Digit recall test score','Index of physical disability','Durable assets (z-score)',
                      'Savings in past 6 mos. (000s 2008 UGX)','Monthly gross cash earnings (000s 2008 UGX)',
                      'Can obtain 100,000 UGX ($58) loan','Can obtain 1,000,000 UGX ($580) loan',
                      'Human capital (z-score)','Working capital (z-score)',
                      'Patience index (z-score)','Aggression index (z-score)')

colnames(sumtable)<-c('Mean','Std. dev.',
                      'Mean','Std. dev.',
                      'Mean','p-value')


kbl(sumtable,format='latex',booktabs = T,caption = 'PRE-INTERVENTION
DESCRIPTIVE STATISTICS AND TEST OF BALANCE',
    longtable = T) %>%
  add_header_above(c('','Control'=2,'Treatment'=2,'Regression difference'=2)) %>%
  kable_styling(latex_options = c("repeat_header")) %>%
  landscape() %>%
  footnote(symbol = 'Measures of human capital, working capital, patience, risk-aversion, and aggression were aggregated from questionnaire responses, weighted, then standardised. We detail this in the Appendix.',
           threeparttable = T)

# tests of balance after discarding 

x<-df_main %>% filter(e1==1) %>%  #filter results only from endline survey1
  mutate(nonag_hrs=lowskill7da_zero+lowbus7da_zero+skilledtrade7da_zero+highskill7da_zero+acto7da_zero) %>%
  mutate(weeklyemp=aghours7da_zero+nonag_hrs) %>%
  mutate(skilledtrade=ifelse(skilledtrade7da_zero>0,1,0)) %>%
  dplyr::select(admin_cost_us,group_size,grantsize_pp_US_est3,group_existed,
                group_age,ingroup_hetero,ingroup_dynamic,avgdisteduc,ind_found_b,age,female,
                urban,risk_aversion,grp_leader,grp_chair,weeklyemp,nonag_hrs,
                lowskill7da_zero,lowbus7da_zero,skilledtrade7da_zero,highskill7da_zero,acto7da_zero,
                aghours7da_zero,chores7da_zero,zero_hours,nonag_dummy,skilledtrade,
                inschool,education,literate,
                voc_training,numeracy_numcorrect_m,adl,wealthindex,savings_6mo_p99,cash4w_p99,
                loan_100k,loan_1mil,
                S_K,S_H,S_P_m,aggression_n,assigned,
                D_1,D_2,D_3,D_4,D_5,D_6,D_7,D_8,D_9,D_10,D_11,D_12,D_13,
                groupid,partid)

#eliminate NA values 
df<-x[which(complete.cases(x)),]

out<-matchit(assigned~.-groupid-partid,data=df,method='optimal',distance = 'glm',
             link='probit')

df2<-match.data(out)
vars<-colnames(df2)[1:42]

clustdiff<-data.frame(vars,mean=1:length(vars),pvalue=1:length(vars))

form<-c('assigned','D_1','D_2','D_3','D_4','D_5','D_6','D_7','D_8',
        'D_9','D_10','D_11','D_12','D_13')

for (i in 1:length(vars)){
  mod<-lm.cluster(reformulate(form,vars[i]),
                  data=df2,cluster='groupid')
  clustdiff[i,2:3]<-round(summary(mod)[2,c(1,4)],3)
}

clustdiff %>% arrange(desc(pvalue))

df_treat<-df2%>%filter(assigned==1)%>%.[,1:42]
df_ctrl<-df2%>%filter(assigned==0)%>%.[,1:42] 
#control 
cmean<-round(apply(df_ctrl,2,mean),3)
csd<-round(apply(df_ctrl,2,sd),3)

#treat
tmean<-round(apply(df_treat,2,mean),3)
tsd<-round(apply(df_treat,2,sd),3)

sumtable<-cbind(cmean,csd,tmean,tsd,clustdiff[,2:3])

rownames(sumtable)<-c('Grant amount applied for, USD',
                      'Group size','Grant amount per member, USD',
                      'Group existed before application',
                      'Group age, in years','Within-group heterogeneity (z-score)',
                      'Quality of group dynamic (z-score)',
                      'Distance to educational facilities (km)',
                      'Individual unfound at baseline',
                      'Age at baseline',
                      'Female','Large town/urban area',
                      'Risk aversion index (z-score)',
                      'Any leadership position in group',
                      'Group chair or vice-chair','Weekly employment, hours',
                      'All nonagricultural work','Casual labor, low skill',
                      'Petty business, low skill','Skilled trades','High-skill wage labor',
                      'Other nonagricultural work','All agricultural work',
                      'Weekly household chores, hours','Zero employment hours in past month',
                      'Main occupation is nonagricultural','Engaged in a skilled trade',
                      'Currently in school','Highest grade reached at school',
                      'Able to read and write minimally','Received prior vocational training',
                      'Digit recall test score','Index of physical disability','Durable assets (z-score)',
                      'Savings in past 6 mos. (000s 2008 UGX)','Monthly gross cash earnings (000s 2008 UGX)',
                      'Can obtain 100,000 UGX ($58) loan','Can obtain 1,000,000 UGX ($580) loan',
                      'Human capital (z-score)','Working capital (z-score)',
                      'Patience index (z-score)','Aggression index (z-score)')

colnames(sumtable)<-c('Mean','Std. dev.',
                      'Mean','Std. dev.',
                      'Mean','p-value')


kbl(sumtable,format='latex',booktabs = T,caption = 'PRE-INTERVENTION
DESCRIPTIVE STATISTICS',
    longtable = T) %>%
  add_header_above(c('','Control'=2,'Treatment'=2,'Regression difference'=2)) %>%
  kable_styling(latex_options = c("repeat_header")) %>%
  landscape()

# comparison of ML methods

ml<-data.frame(ridge=
             c('',9.810,20.960,'',17326.310,2140.553,'',0.004,0.001,
               '',0.002,'',155.674,'',5957.837),
           rf=c('',20.642,128.080,'',92487.950,2584.131,'',0.002,0.004,
                '',0.003,'',102.464,'',8804.406))

colnames(ml)<-c('Ridge regression','Random forest')
rownames(ml)<-c('Profits','E1','E2','Capital stock','E3','E4',
                'Durable consumption','E5','E6','Nondurable consumption',
                'E7','Savings','E8','Training hours','E9')

kbl(ml,format='latex',booktabs = T,caption='Comparison of ML methods')

# CLAN

cln<-matrix(0.123,33,2) %>%as.data.frame()
colnames(cln)<-c('Capital stock (E1)','Training hours (E1)')

rownames(cln)<-c('Human capital','1','2','Working capital','3','4','Unemployed at baseline','5','6',
                 'Risk aversion','7','8','Average distance to education facility','9','10',
                 'Urban','11','12','Age','13','14','Patience','15','16','Aggression','17','18','Female','19','20','In school','21','22')

kbl(cln,format='latex',booktabs = T,caption='Classification Analysis (CLAN)') %>%
  footnote(symbol = 'Columns (2) and (3) report control and treatment means with standard errors in parentheses. Column (4) reports the regression difference with district fixed effects and group-clustered standard errors (not shown). P-values are indicated in brackets. We show only covariates included in our heterogeneity analysis.',
           threeparttable = T)


# balance unfound

# corr test
b<-cor(df%>%select(S_H,S_K,zero_hours,risk_aversion,avgdisteduc,urban,age,S_P_m,aggression_n,female,inschool))
rownames(b)<-c('Human capital','Working capital','Unemployed at baseline',
               'Risk aversion','Average distance to education facility',
               'Urban','Age','Patience','Aggression','Female','In school')

colnames(b)<-c('Human capital','Working capital','Unemployed at baseline',
               'Risk aversion','Average distance to education facility',
               'Urban','Age','Patience','Aggression','Female','In school')

kbl(b,format='latex',booktabs = T,caption='Pairwise correlation') 





