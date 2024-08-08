survey = read.csv("VOTER_Survey_December16_Release1.csv")
getwd()
setwd('/Users/laraschuman/Desktop/PSCI')
survey_2018 = read.csv("")
attach(survey)
would_vote2012 = survey$cmatch_romn_baseline
did_vote2016 = survey$presvote16post_2016
fam_inc = survey$faminc_2016
state = survey$inputstate_2016
did_vote2012 = survey$post_presvote12_2012
c2 = cbind(did_vote2012,did_vote2016)
warnings()
survey2 <- survey2[survey2$tookpost==1,]
plot(survey$post_presvote12_2012)
plot(survey$presvote16post_2016)


data <- c(1,2,3,4,5,6,1,1,1,1)
data2 <- c(1,2,1,4,2,3,1,1,1,1)
table(data,data2)
dim(survey)
library(rpart)

vars
#survey2 <- subset(survey, select="presvote16post_2016")
survey2 <- survey[(survey$presvote16post_2016 == 1 | survey$presvote16post_2016 == 2) & 
                     (survey$post_presvote12_2012 == 1 | survey$post_presvote12_2012 == 2),]
#survey2$presvote16post_2016 <- factor(survey2$presvote16post_2016)
#survey2$post_presvote12_2012 <- factor(survey2$post_presvote12_2012)

new_survey <- survey2[!is.na(survey2$post_presvote12_2012), ]
new_survey <- new_survey[!is.na(new_survey$presvote16post_2016),]
new_survey

# Split into test and train
sample <- sample.int(n = nrow(new_survey), size = floor(.7*nrow(new_survey)), replace = F)
new_survey_train <- new_survey[sample, ] # 70% of data
new_survey_test  <- new_survey[-sample, ] # 30% of data

survey2
plot(survey2)
test = table(survey$presvote16post_2016)
plot(test)
test1 = table(survey$post_presvote12_2012)
barplot(test,test1)
library(VennDiagram)
x = c(newdata$post_inputstate_2012,newdata$cmatch_romn_baseline,newdata$post_pid7_2012)
a <- venn.diagram(newdata$post_inputstate_2012,newdata$cmatch_romn_baseline,filename = "test venn diagram.png",output = TRUE ,
                  imagetype="png" ,
                  height = 480 , 
                  width = 480)
?vennCounts
install.packages("VennDiagram")
#########################################
vars = c('PARTY_AGENDAS_D1_2016','PARTY_AGENDAS_D2_2016'
         ,'PARTY_AGENDAS_D3_2016','PARTY_AGENDAS_D4_2016','PARTY_AGENDAS_D5_2016'
         ,'PARTY_AGENDAS_D6_2016','PARTY_AGENDAS_D7_2016','PARTY_AGENDAS_D8_2016'
         ,'PARTY_AGENDAS_D9_2016','PARTY_AGENDAS_D10_2016','PARTY_AGENDAS_D11_2016'
         ,'PARTY_AGENDAS_D12_2016','PARTY_AGENDAS_R1_2016','PARTY_AGENDAS_R2_2016'
         ,'PARTY_AGENDAS_R3_2016','PARTY_AGENDAS_R4_2016','PARTY_AGENDAS_R5_2016'
         ,'PARTY_AGENDAS_R6_2016','PARTY_AGENDAS_R7_2016','PARTY_AGENDAS_R8_2016'
         ,'PARTY_AGENDAS_R9_2016','PARTY_AGENDAS_R10_2016','PARTY_AGENDAS_R11_2016'
         ,'PARTY_AGENDAS_R12_2016')
survey2 <- subset(survey, select=c('presvote16post_2016','post_presvote12_2012'))
survey3 <- subset(survey2, filter(presvote16post_2016 == 1 || presvote16post_2016 ==2))
if(survey$presvote16post_2016 == 2 && survey$post_presvote12_2012 == 1){}
newdata <- na.omit(survey2$presvote16post_2016)
newdata <- na.omit(survey2$post_presvote12_2012)
print(newdata)
print(presvote16post_2016)
install.packages('dphylr2')
library()
install.packages('caTools')
library(caTools)
set.seed(100)
split<-sample.split(survey$post_pid3_2012)
survey3 <- subset(survey,split == TRUE)
survey4 <- subset(survey,split == FALSE)
vars =c('presvote16post_2016')
###########################
model <- glm(presvote16post_2016~PARTY_AGENDAS_D1_2016+PARTY_AGENDAS_D2_2016
             +PARTY_AGENDAS_D3_2016+PARTY_AGENDAS_D4_2016+PARTY_AGENDAS_D5_2016
             +PARTY_AGENDAS_D6_2016+PARTY_AGENDAS_D7_2016+PARTY_AGENDAS_D8_2016
             +PARTY_AGENDAS_D9_2016+PARTY_AGENDAS_D10_2016
             +PARTY_AGENDAS_D11_2016+PARTY_AGENDAS_D12_2016
             ,data = survey, weights = weight)

model2 <- glm(presvote16post_2016~PARTY_AGENDAS_R1_2016+PARTY_AGENDAS_R2_2016
             +PARTY_AGENDAS_R3_2016+PARTY_AGENDAS_R4_2016+PARTY_AGENDAS_R5_2016
             +PARTY_AGENDAS_R6_2016+PARTY_AGENDAS_R7_2016+PARTY_AGENDAS_R8_2016
             +PARTY_AGENDAS_R9_2016+PARTY_AGENDAS_R10_2016
             +PARTY_AGENDAS_R11_2016+PARTY_AGENDAS_R12_2016
             ,data = survey, weights = weight)

View(new_survey)
model_2016_predictor <- glm(presvote16post_2016~obamaapp_2016+track_2016
                            +persfinretro_2016+econtrend_2016+trustgovt_2016
                            +imiss_a_2016+imiss_b_2016+imiss_c_2016+imiss_f_2016
                            +imiss_g_2016+imiss_d_2016, data = new_survey)

# use this -- need to use training data
set.seed(100)
lm_model_2016 <- lm(presvote16post_2016~as.factor(obamaapp_2016)+as.factor(track_2016)
               +as.factor(persfinretro_2016)+as.factor(econtrend_2016)+as.factor(trustgovt_2016)
               +as.factor(imiss_a_2016)+as.factor(imiss_b_2016)+as.factor(imiss_c_2016)+as.factor(imiss_f_2016)
               +as.factor(imiss_g_2016)+as.factor(imiss_d_2016)+as.factor(imiss_h_2016)
               +as.factor(imiss_j_2016)+as.factor(imiss_m_2016)+as.factor(imiss_p_2016)
               +as.factor(imiss_q_2016)+as.factor(imiss_r_2016)+as.factor(imiss_s_2016)
               +as.factor(imiss_t_2016), data=new_survey_train)
summary(lm_model_2016)
plot(lm_model)

lm_model_2012 <- lm(post_presvote12_2012~as.factor(obamaapp_baseline)+as.factor(track_baseline)
                    +as.factor(persfinretro_baseline)+as.factor(econtrend_baseline)+as.factor(trustgovt_baseline)
                    +as.factor(imiss_a_baseline)+as.factor(imiss_c_baseline)+as.factor(imiss_c_baseline)+as.factor(imiss_f_baseline)
                    +as.factor(imiss_g_baseline)+as.factor(imiss_d_baseline)+as.factor(imiss_h_baseline)
                    +as.factor(imiss_j_baseline)+as.factor(imiss_m_baseline)+as.factor(imiss_p_baseline)
                    +as.factor(imiss_q_baseline)+as.factor(imiss_r_baseline)+as.factor(imiss_s_baseline)
                    +as.factor(imiss_t_baseline), data = new_survey_train)
summary(lm_model_2012)
plot(lm_model_2012)

# Predict on new data (test) (2012)
preds_2012 <- round(predict(lm_model_2012, newdata=new_survey_test))
rem_preds_2012 <- new_survey_test$post_presvote12_2012 == preds_2012
rem_preds_2012 <- rem_preds_2012[!is.na(rem_preds_2012)]
sum(rem_preds_2012)/length(rem_preds_2012) # this is the % accuracy of your predictor
table(preds_2012, new_survey_test$post_presvote12_2012) #confusion matrix 


# Predict on new data (test) (2016)
preds <- round(predict(lm_model_2016, newdata=new_survey_test))
rem_preds <- new_survey_test$presvote16post_2016 == preds
rem_preds <- rem_preds[!is.na(rem_preds)]
sum(rem_preds)/length(rem_preds) # this is the % accuracy of your predictor
table(preds, new_survey_test$presvote16post_2016) #confusion matrix 


# decision tree
#train
new_survey_train_cat <- new_survey_train
new_survey_train_cat$obamaapp_2016 <- as.factor(new_survey_train_cat$obamaapp_2016)
new_survey_train_cat$track_2016 <- as.factor(new_survey_train_cat$track_2016)
new_survey_train_cat$persfinretro_2016 <- as.factor(new_survey_train_cat$persfinretro_2016)
new_survey_train_cat$econtrend_2016 <- as.factor(new_survey_train_cat$econtrend_2016)
new_survey_train_cat$trustgovt_2016 <- as.factor(new_survey_train_cat$trustgovt_2016)
new_survey_train_cat$imiss_a_2016 <- as.factor(new_survey_train_cat$imiss_a_2016)
new_survey_train_cat$imiss_b_2016 <- as.factor(new_survey_train_cat$imiss_b_2016)
new_survey_train_cat$imiss_c_2016 <- as.factor(new_survey_train_cat$imiss_c_2016)
new_survey_train_cat$imiss_d_2016 <- as.factor(new_survey_train_cat$imiss_d_2016)
new_survey_train_cat$imiss_f_2016 <- as.factor(new_survey_train_cat$imiss_f_2016)
new_survey_train_cat$imiss_g_2016 <- as.factor(new_survey_train_cat$imiss_g_2016)
new_survey_train_cat$imiss_h_2016 <- as.factor(new_survey_train_cat$imiss_h_2016)
new_survey_train_cat$imiss_j_2016 <- as.factor(new_survey_train_cat$imiss_j_2016)
new_survey_train_cat$imiss_m_2016 <- as.factor(new_survey_train_cat$imiss_m_2016)
new_survey_train_cat$imiss_p_2016 <- as.factor(new_survey_train_cat$imiss_p_2016)
new_survey_train_cat$imiss_q_2016 <- as.factor(new_survey_train_cat$imiss_q_2016)
new_survey_train_cat$imiss_r_2016 <- as.factor(new_survey_train_cat$imiss_r_2016)
new_survey_train_cat$imiss_s_2016 <- as.factor(new_survey_train_cat$imiss_s_2016)
new_survey_train_cat$imiss_t_2016 <- as.factor(new_survey_train_cat$imiss_t_2016)

#test
new_survey_test_cat <- new_survey_test
new_survey_test_cat$obamaapp_2016 <- as.factor(new_survey_test_cat$obamaapp_2016)
new_survey_test_cat$track_2016 <- as.factor(new_survey_test_cat$track_2016)
new_survey_test_cat$persfinretro_2016 <- as.factor(new_survey_test_cat$persfinretro_2016)
new_survey_test_cat$econtrend_2016 <- as.factor(new_survey_test_cat$econtrend_2016)
new_survey_test_cat$trustgovt_2016 <- as.factor(new_survey_test_cat$trustgovt_2016)
new_survey_test_cat$imiss_a_2016 <- as.factor(new_survey_test_cat$imiss_a_2016)
new_survey_test_cat$imiss_b_2016 <- as.factor(new_survey_test_cat$imiss_b_2016)
new_survey_test_cat$imiss_c_2016 <- as.factor(new_survey_test_cat$imiss_c_2016)
new_survey_test_cat$imiss_d_2016 <- as.factor(new_survey_test_cat$imiss_d_2016)
new_survey_test_cat$imiss_f_2016 <- as.factor(new_survey_test_cat$imiss_f_2016)
new_survey_test_cat$imiss_g_2016 <- as.factor(new_survey_test_cat$imiss_g_2016)
new_survey_test_cat$imiss_h_2016 <- as.factor(new_survey_test_cat$imiss_h_2016)
new_survey_test_cat$imiss_j_2016 <- as.factor(new_survey_test_cat$imiss_j_2016)
new_survey_test_cat$imiss_m_2016 <- as.factor(new_survey_test_cat$imiss_m_2016)
new_survey_test_cat$imiss_p_2016 <- as.factor(new_survey_test_cat$imiss_p_2016)
new_survey_test_cat$imiss_q_2016 <- as.factor(new_survey_test_cat$imiss_q_2016)
new_survey_test_cat$imiss_r_2016 <- as.factor(new_survey_test_cat$imiss_r_2016)
new_survey_test_cat$imiss_s_2016 <- as.factor(new_survey_test_cat$imiss_s_2016)
new_survey_test_cat$imiss_t_2016 <- as.factor(new_survey_test_cat$imiss_t_2016)
library(rpart)
tree <- rpart(presvote16post_2016~obamaapp_2016+track_2016
              +persfinretro_2016+econtrend_2016+trustgovt_2016
              +imiss_a_2016+imiss_b_2016+imiss_c_2016+imiss_f_2016
              +imiss_g_2016+imiss_d_2016+imiss_f_2016+imiss_g_2016
              +imiss_h_2016+imiss_j_2016+imiss_m_2016+imiss_p_2016
              +imiss_q_2016+imiss_r_2016+imiss_s_2016+imiss_t_2016,
             method="class", data=new_survey_train_cat)
summary(tree)
plot(tree)

tree_preds <- predict(tree, newdata=new_survey_test_cat, type='class')
tree_rem_preds <- new_survey_test_cat$presvote16post_2016 == tree_preds
tree_rem_preds <- tree_rem_preds[!is.na(tree_rem_preds)]
sum(tree_rem_preds)/length(tree_rem_preds) # this is the % accuracy of your predictor
table(tree_preds, new_survey_test_cat$presvote16post_2016) #confusion matrix 

table(preds, new_survey_test$presvote16post_2016) #confusion matrix 

# decision tree #2 2012
#train
new_survey_train_cat_2012 <- new_survey_train
new_survey_train_cat_2012$obamaapp_baseline <- as.factor(new_survey_train_cat_2012$obamaapp_baseline)
new_survey_train_cat_2012$track_baseline <- as.factor(new_survey_train_cat_2012$track_baseline)
new_survey_train_cat_2012$persfinretro_baseline <- as.factor(new_survey_train_cat_2012$persfinretro_baseline)
new_survey_train_cat_2012$econtrend_baseline <- as.factor(new_survey_train_cat_2012$econtrend_baseline)
new_survey_train_cat_2012$trustgovt_baseline <- as.factor(new_survey_train_cat_2012$trustgovt_baseline)
new_survey_train_cat_2012$imiss_a_baseline <- as.factor(new_survey_train_cat_2012$imiss_a_baseline)
new_survey_train_cat_2012$imiss_b_baseline <- as.factor(new_survey_train_cat_2012$imiss_b_baseline)
new_survey_train_cat_2012$imiss_c_baseline <- as.factor(new_survey_train_cat_2012$imiss_c_baseline)
new_survey_train_cat_2012$imiss_d_baseline <- as.factor(new_survey_train_cat_2012$imiss_d_baseline)
new_survey_train_cat_2012$imiss_f_baseline <- as.factor(new_survey_train_cat_2012$imiss_f_baseline)
new_survey_train_cat_2012$imiss_g_baseline <- as.factor(new_survey_train_cat_2012$imiss_g_baseline)
new_survey_train_cat_2012$imiss_h_baseline <- as.factor(new_survey_train_cat_2012$imiss_h_baseline)
new_survey_train_cat_2012$imiss_j_baseline <- as.factor(new_survey_train_cat_2012$imiss_j_baseline)
new_survey_train_cat_2012$imiss_m_baseline <- as.factor(new_survey_train_cat_2012$imiss_m_baseline)
new_survey_train_cat_2012$imiss_p_baseline <- as.factor(new_survey_train_cat_2012$imiss_p_baseline)
new_survey_train_cat_2012$imiss_q_baseline <- as.factor(new_survey_train_cat_2012$imiss_q_baseline)
new_survey_train_cat_2012$imiss_r_baseline <- as.factor(new_survey_train_cat_2012$imiss_r_baseline)
new_survey_train_cat_2012$imiss_s_baseline <- as.factor(new_survey_train_cat_2012$imiss_s_baseline)
new_survey_train_cat_2012$imiss_t_baseline <- as.factor(new_survey_train_cat_2012$imiss_t_baseline)

#test
new_survey_test_cat_2012 <- new_survey_test
new_survey_test_cat_2012$obamaapp_baseline <- as.factor(new_survey_test_cat_2012$obamaapp_baseline)
new_survey_test_cat_2012$track_baseline <- as.factor(new_survey_test_cat_2012$track_baseline)
new_survey_test_cat_2012$persfinretro_baseline <- as.factor(new_survey_test_cat_2012$persfinretro_baseline)
new_survey_test_cat_2012$econtrend_baseline <- as.factor(new_survey_test_cat_2012$econtrend_baseline)
new_survey_test_cat_2012$trustgovt_baseline <- as.factor(new_survey_test_cat_2012$trustgovt_baseline)
new_survey_test_cat_2012$imiss_a_baseline <- as.factor(new_survey_test_cat_2012$imiss_a_baseline)
new_survey_test_cat_2012$imiss_b_baseline <- as.factor(new_survey_test_cat_2012$imiss_b_baseline)
new_survey_test_cat_2012$imiss_c_baseline <- as.factor(new_survey_test_cat_2012$imiss_c_baseline)
new_survey_test_cat_2012$imiss_d_baseline <- as.factor(new_survey_test_cat_2012$imiss_d_baseline)
new_survey_test_cat_2012$imiss_f_baseline <- as.factor(new_survey_test_cat_2012$imiss_f_baseline)
new_survey_test_cat_2012$imiss_g_baseline <- as.factor(new_survey_test_cat_2012$imiss_g_baseline)
new_survey_test_cat_2012$imiss_h_baseline <- as.factor(new_survey_test_cat_2012$imiss_h_baseline)
new_survey_test_cat_2012$imiss_j_baseline <- as.factor(new_survey_test_cat_2012$imiss_j_baseline)
new_survey_test_cat_2012$imiss_m_baseline <- as.factor(new_survey_test_cat_2012$imiss_m_baseline)
new_survey_test_cat_2012$imiss_p_baseline <- as.factor(new_survey_test_cat_2012$imiss_p_baseline)
new_survey_test_cat_2012$imiss_q_baseline <- as.factor(new_survey_test_cat_2012$imiss_q_baseline)
new_survey_test_cat_2012$imiss_r_baseline <- as.factor(new_survey_test_cat_2012$imiss_r_baseline)
new_survey_test_cat_2012$imiss_s_baseline <- as.factor(new_survey_test_cat_2012$imiss_s_baseline)
new_survey_test_cat_2012$imiss_t_baseline <- as.factor(new_survey_test_cat_2012$imiss_t_baseline)

tree_2012 <- rpart(post_presvote12_2012~obamaapp_baseline+track_baseline
              +persfinretro_baseline+econtrend_baseline+trustgovt_baseline
              +imiss_a_baseline+imiss_b_baseline+imiss_c_baseline+imiss_f_baseline
              +imiss_g_baseline+imiss_d_baseline+imiss_f_baseline+imiss_g_baseline
              +imiss_h_baseline+imiss_j_baseline+imiss_m_baseline+imiss_p_baseline
              +imiss_q_baseline+imiss_r_baseline+imiss_s_baseline+imiss_t_baseline,
              method="class", data=new_survey_train_cat_2012)
summary(tree_2012)

tree_preds_2012 <- predict(tree_2012, newdata=new_survey_test_cat_2012, type='class')
tree_rem_preds_2012 <- new_survey_test_cat_2012$post_presvote12_2012 == tree_preds_2012
tree_rem_preds_2012 <- tree_rem_preds_2012[!is.na(tree_rem_preds_2012)]
sum(tree_rem_preds_2012)/length(tree_rem_preds_2012) # this is the % accuracy of your predictor
table(tree_preds_2012, new_survey_test_cat_2012$post_presvote12_2012) #confusion matrix 

table(preds, new_survey_test$post_presvote12_2012)






model_2012_predictor <- glm(post_presvote12_2012~obamaapp_baseline+track_baseline
                            +persfinretro_baseline+econtrend_baseline+trustgovt_baseline
                            +imiss_a_baseline+imiss_c_baseline+imiss_c_baseline+imiss_f_baseline
                            +imiss_g_baseline+imiss_d_baseline, data = new_survey)



switcher_predictor_dems <- glm()
while (newdata$post_presvote12_2012[100] == 1&&newdata$presvote16post_2016[100]==1) || 
  (newdata$post_presvote12_2012[100] == 2) &&(newdata$presvote16post_2016[100]==2)) {
    if ((newdata$post_presvote12_2012[100] == newdata$presvote16post_2016[100])){
      print('same')
    }else{
      print('different')
      print(newdata$post_presvote12_2012[100])
      print(newdata$presvote16post_2016[100])
    }
}




predict <- predict(model_2012_predictor,response="relevance")
accurary(predict)
plot(model_2012_predictor)
x <- c('1','2')
col(survey)
data <- subset(survey2,select=c(1,2))
summary(model)

predict <- predict(model, type = 'response')
plot(predict)
##########################
library(ROCR)
install.packages('ggplot2')
library(ggplot2)
install.packages('ROCR')
survey$
col <- factor(survey$col)
vars
mean(vars)
faminc_2016+inputstate_2016+post_presvote12_2012
+

+PARTY_AGENDAS_R6_2016+PARTY_AGENDAS_R7_2016+PARTY_AGENDAS_R8_2016
+PARTY_AGENDAS_R9_2016+PARTY_AGENDAS_R10_2016+PARTY_AGENDAS_R11_2016
+PARTY_AGENDAS_R12_2016



sapply(survey,function(x) sum(is.na(x)))
sapply(survey, function(x) length(unique(x)))
install.packages('Amelia')
library(Amelia)
missmap(survey, main = "Missing values vs observed")
survey2$post_inputstate_2012[is.na(survey2$post_inputstate_2012)] <- mean(survey2$post_inputstate_2012,na.rm=T)
is.factor(survey2$post_inputstate_2012)
model <- glm(formula,family = binomial,subset = newdata)

summary(survey2)

newdata <- na.omit(survey2)
plot(newdata)
colSums(is.na(survey))
sum(is.na(survey$presvote16post_2016))
