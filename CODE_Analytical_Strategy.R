## Change working directory
setwd("C:/Users/K56CA/Dropbox/PhD/Experiments Incentives/Follow_Up_Incentives")

##Relevant libraries
library("dplyr")
library("tidyr")
library("ggplot2")

## read raw_data
data_clean_imp<-read.csv("clean_data.csv")
# correcting ID veriable
data_clean<- mutate(data_clean_imp, ID= X+1) %>% 
        select(ID, everything(), -X)


#Define factor variables

data_clean$r_amount <- factor(data_clean$r_amount, levels = c(1,2),
                      labels = c("�5", "�2")) 

data_clean$r_type <- factor(data_clean$r_type, levels = c(1,2),
                    labels = c("Cash", "Product")) 

data_clean$gender <- factor(data_clean$gender, levels = c(1,2),
                    labels = c("Male", "Female"))

data_clean$income <- factor(data_clean$income, levels = c(1:5),
                    labels = c("< 15k", "15k-19k",
                               "20k-29k", "> 30k", "No.Ans")) 

data_clean$employ <- factor(data_clean$employ, levels = c(1:6),
                    labels = c("FT-Emp", "PT-Emp","Unempl.", 
                               "Stud.", "Retired","Other")) 


# Addint totals for Measures: Neg.Evaluations and Expectations 

expect_1<-c("exp_x1_K","exp_x1_T", "exp_x1_S.R","exp_x1_G")
expect_2<-c("exp_x2_K","exp_x2_T", "exp_x2_S.R","exp_x2_G")
neg_eval<-c("neg.ev_1","neg.ev_2.R","neg.ev_3","neg.ev_4.R",
            "neg.ev_5","neg.ev_6","neg.ev_7.R","neg.ev_8",
            "neg.ev_9","neg.ev_10.R", "neg.ev_11","neg.ev_12")

final_df<-data_clean %>% mutate(
        tot_exp1 = rowMeans(.[expect_1]),
        tot_exp2 = rowMeans(.[expect_2]),
        tot_neg_eval = rowMeans(.[neg_eval])
        )  

# Code to audit cases ###################
# final_df %>% filter(ID==90) %>% gather()
#########################################


### FUNCTION calculating t.tests between AMOUNT size

t<-lapply(final_df[,c(2:12,31:33)], 
          function(x) t.test(x ~ final_df[,29], var.equal = TRUE))

ps<-data.frame(p.value = sapply(t, getElement, name = "p.value"))
format(ps, scientific = FALSE) 


### FUNCTION calculating t.tests between REWARD TYPE size

t<-lapply(data_clean[,2:12], 
          function(x) t.test(x ~ data_clean[,30], var.equal = TRUE))

ps<-data.frame(p.value = sapply(t, getElement, name = "p.value"))
format(ps, scientific = FALSE) 


g<- data_clean %>% group_by(r_type)  %>% summarise_each(funs(mean))

