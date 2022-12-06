library(dplyr)
library(nnet)
library(stargazer)
library(margins)

data <- read.csv('reg_data_R_mnlogit.csv')
data2 <- data %>% mutate(cl014_348 = recode(cl014_348, '0' = 'a0', '-1' = 'cnegative', '1' = 'bpositive'))

## Robustness checks removing LA county
#data <- data %>% filter(cnty_FIPS != 06037)
#data2 <- data2 %>% filter(cnty_FIPS != 06037)

## Children
reg_child18 = multinom(formula = cl014_348 ~ imm + 
                      educ_c_8 + educ_c_12 + educ_c_16 + educ_c_18 + educ_c_20 + 
                      hhi_c_0 + hhi_c_20 + hhi_c_60 + hhi_c_80 + 
                      case3 + case2 + case4 + case5 + 
                      age1 + age2 + age4
                    ,
                    data=data2%>%filter(haschild==1))
reg_child24 = multinom(formula = cl014_348 ~ imm + 
                         educ_c_8 + educ_c_12 + educ_c_16 + educ_c_18 + educ_c_20 + 
                         hhi_c_0 + hhi_c_20 + hhi_c_60 + hhi_c_80 + 
                         case3 + case2 + case4 + case5 + 
                         age1 + age2 + age4
                       ,
                       data=data2%>%filter(haschild18==1))
reg_childno = multinom(formula = cl014_348 ~ imm + 
                         educ_c_8 + educ_c_12 + educ_c_16 + educ_c_18 + educ_c_20 + 
                         hhi_c_0 + hhi_c_20 + hhi_c_60 + hhi_c_80 + 
                         case3 + case2 + case4 + case5 + 
                         age1 + age2 + age4
                       ,
                       data=data2%>%filter(haschild18==0, haschild==0))
stargazer(reg_child18, reg_child24, reg_childno, type='text', apply.coef=exp, p.auto=FALSE, out='regout.txt')

## Simple imm reg
reg_simple = multinom(formula = cl014_348 ~ imm, data=data2)
stargazer(reg_simple, type='text', apply.coef=exp, p.auto=FALSE)
reg_gen = multinom(formula = cl014_348 ~ immigrant_gen_1.First.generation.immigrant +
                     immigrant_gen_2.Second.generation.immigrant +
                     immigrant_gen_3.Third.generation.immigrant, 
                   data=data2)
reg_gen2 = multinom(formula = cl014_348 ~ immigrant_gen_1.First.generation.immigrant +
                     immigrant_gen_2.Second.generation.immigrant +
                     immigrant_gen_3.Third.generation.immigrant +
                     educ_c_8 + educ_c_12 + educ_c_16 + educ_c_18 + educ_c_20 + 
                     hhi_c_0 + hhi_c_20 + hhi_c_60 + hhi_c_80 + 
                     case1 + case2 + case4 + case5 + 
                      age1 + age2 + age4
                    ,
                   data=data2)
stargazer(reg_gen, reg_gen2, type='text', apply.coef=exp, p.auto=FALSE, out='regout.txt')

## Race and high race prop reg
data2$white_prop_low = ifelse(data2$White.alone < 0.2768, 1, 0)
data2$high_diverse = ifelse(data2$White.alone < 0.4 & data2$Hispanic.or.Latino < 0.6 & 
                              data2$Black.or.African.American.alone < 0.6 & data2$Asian.alone < 0.6 & 
                              data2$Native.Hawaiian.and.Other.Pacific.Islander.alone < 0.6 & 
                              data2$American.Indian.and.Alaska.Native.alone < 0.6,
                            1,
                            0)
## Check if diversity and high same race matters: no
reg_race_ols_div = lm(formula = data$cl014_348 ~ imm 
                      + high_diverse + imm:high_diverse + 
                        #                    + educ_c_8 + educ_c_12 + educ_c_16 + educ_c_18 + educ_c_20 + 
                        #                    hhi_c_0 + hhi_c_20 + hhi_c_60 + hhi_c_80 + 
                        #                    case1 + case2 + case4 + case5 + 
                        #                   age1 + age2 + age4 
                        + hisp + black + anative + asian + race_6.Mixed
                      ,data=data2)
reg_race_ols_high = lm(formula = data$cl014_348 ~ imm 
                       + same_race_high + imm:same_race_high + 
                         #                        + educ_c_8 + educ_c_12 + educ_c_16 + educ_c_18 + educ_c_20 + 
                         #                       hhi_c_0 + hhi_c_20 + hhi_c_60 + hhi_c_80 + 
                         #                       case1 + case2 + case4 + case5 + 
                         #                       age1 + age2 + age4 
                         + hisp + black + anative + asian + race_6.Mixed
                       ,data=data2)
stargazer(reg_race_ols_div, reg_race_ols_high, type='text')

## OLS and Multinom basline
data2$imm2 = as.integer(data2$immigrant_gen_1.First.generation.immigrant==1 | data2$immigrant_gen_2.Second.generation.immigrant==1)
reg_race = multinom(formula = cl014_348 ~ 
                      imm + hisp + black + anative + asian + race_6.Mixed +
                      black*imm + 
                      race_6.Mixed*imm
                    ,data=data2)

reg_race_ols = lm(formula = data$cl014_348 ~ 
                    imm + hisp + black + anative + asian + race_6.Mixed +
                    black*imm + 
                    race_6.Mixed*imm
                  ,data=data2)

stargazer(reg_race, type='text', apply.coef=exp, p.auto=FALSE)
stargazer(reg_race_ols, type='text')

reg_race2 = multinom(formula = cl014_348 ~ 
                       imm + hisp + black + anative + asian + race_6.Mixed +
                       black*imm + 
                       race_6.Mixed*imm
                     + educ_c_8 + educ_c_12 + educ_c_16 + educ_c_18 + educ_c_20 + 
                       hhi_c_0 + hhi_c_20 + hhi_c_60 + hhi_c_80 + 
                       case1 + case2 + case4 + case5 + 
                       age1 + age2 + age4 
                    ,data=data2)

reg_race_ols2 = lm(formula = data$cl014_348 ~ 
                     imm + hisp + black + anative + asian + race_6.Mixed +
                     black*imm + 
                     race_6.Mixed*imm
                     + educ_c_8 + educ_c_12 + educ_c_16 + educ_c_18 + educ_c_20 + 
                     hhi_c_0 + hhi_c_20 + hhi_c_60 + hhi_c_80 + 
                     case1 + case2 + case4 + case5 + 
                     age1 + age2 + age4 
                  ,data=data2)

stargazer(reg_race, reg_race2, type='text', apply.coef=exp, p.auto=FALSE)
stargazer(reg_race_ols2, type='text')

## Education groups
data_hs <- data2[data2$educ_c_8==1 | data2$educ_c_12==1, ]
reg_hs = multinom(formula = cl014_348 ~ imm +
                    hhi_c_0 + hhi_c_20 + hhi_c_60 + hhi_c_80 + 
                    case1 + case2 + case4 + case5 + age1 + age2 + age4 
#                   + hisp + black + anative + asian + pislander
                  ,data=data_hs)

data_ba <- data2[data2$educ_c_14==1 | data2$educ_c_16==1, ]
reg_ba = multinom(formula = cl014_348 ~ imm  +
                    hhi_c_0 + hhi_c_20 + hhi_c_60 + hhi_c_80 + 
                    case1 + case2 + case4 + case5 + age1 + age2 + age4 
#                   + hisp + black + anative + asian + pislander
                  ,data=data_ba)

data_grad <- data2[data2$educ_c_18==1 | data2$educ_c_20==1, ]
reg_grad = multinom(formula = cl014_348 ~ imm +
                    hhi_c_0 + hhi_c_20 + hhi_c_60 + hhi_c_80 + 
                    case1 + case2 + case4 + case5 + age1 + age2 + age4
#                    +  hisp + black + anative + asian + pislander
                  ,data=data_grad)

stargazer(reg_hs, reg_ba, reg_grad, type='text', apply.coef=exp, p.auto=FALSE)

##Unemployment 
data_ue <- read.csv('reg_data_R_ue.csv')
data_ue2 <- data_ue %>% mutate(cl014_348 = recode(cl014_348, '0' = 'a0', '-1' = 'cnegative', '1' = 'bpositive'))

reg_simple_ue = multinom(formula = cl014_348 ~ imm + ue_ever + imm*ue_ever, data=data_ue2)

reg_chetty = multinom(formula = cl014_348 ~ imm + ue_ever + 
                        imm*ue_ever +
#                        ue_chetty_c_10 
                        + ue_chetty_c_100 + 
                        imm*ue_chetty_c_100 +
                        educ_c_12 + educ_c_16 + educ_c_18 + educ_c_20 + 
                        hhi_c_0 + hhi_c_20 + hhi_c_60 + hhi_c_80 + 
                       case1 + case2 + case4 + case5 
                       + age1 + age2 + age4
#                      +  race_0.hisplatino + race_2.Black.Only + race_3.American.Indian.or.Alaska.Native.Only + race_4.Asian.Only + race_5.Hawaiian.Pacific.Islander.Only
                        , data=data_ue2)

stargazer(reg_simple_ue, reg_chetty, type='text', apply.coef=exp, p.auto=FALSE)

reg_bls = multinom(formula = cl014_348 ~ imm + ue_ever 
                   + imm*ue_ever 
 #                 +ue_bls_c_10  
  + ue_bls_c_100 + 
                        imm*ue_bls_c_100 +
                         educ_c_12 + educ_c_16 + educ_c_18 + educ_c_20 + 
                        hhi_c_0 + hhi_c_20 + hhi_c_60 + hhi_c_80 + 
                        case1 + case2 + case4 + case5 + age1 + age2 + age4 
#  +  race_0.hisplatino + race_2.Black.Only + race_3.American.Indian.or.Alaska.Native.Only + race_4.Asian.Only + race_5.Hawaiian.Pacific.Islander.Only
                      , data=data_ue2)

stargazer(reg_bls,  type='text', apply.coef=exp, p.auto=FALSE)


