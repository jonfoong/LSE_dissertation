library(MatchIt)
library(sandwich)
library(miceadds)




x<-df_main %>% filter(e1==1) %>%  #filter results only from endline survey1
  mutate(nonag_hrs=lowskill7da_zero+lowbus7da_zero+skilledtrade7da_zero+highskill7da_zero+acto7da_zero) %>%
  mutate(weeklyemp=aghours7da_zero+nonag_hrs) %>%
  dplyr::select(admin_cost_us,group_size,grantsize_pp_US_est3,group_existed,
                group_age,ingroup_hetero,ingroup_dynamic,avgdisteduc,ind_found_b,age,female,
                urban,risk_aversion,grp_leader,grp_chair,weeklyemp,nonag_hrs,
                lowskill7da_zero,lowbus7da_zero,skilledtrade7da_zero,highskill7da_zero,acto7da_zero,
                aghours7da_zero,chores7da_zero,zero_hours,nonag_dummy,inschool,education,literate,
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

vars<-colnames(df2)[1:41]

clustdiff<-data.frame(vars,mean=1:length(vars),pvalue=1:length(vars))

form<-c('assigned','D_1','D_2','D_3','D_4','D_5','D_6','D_7','D_8',
        'D_9','D_10','D_11','D_12','D_13')

for (i in 1:length(vars)){
  mod<-lm.cluster(reformulate(form,vars[i]),
                  data=df2,cluster='groupid')
  clustdiff[i,2:3]<-round(summary(mod)[2,c(1,4)],3)
}

clustdiff %>% arrange(desc(pvalue))
