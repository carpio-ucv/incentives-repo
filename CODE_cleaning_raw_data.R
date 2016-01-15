## Change working directory
setwd("C:/Users/K56CA/Dropbox/PhD/Experiments Incentives/Follow_Up_Incentives")

## read raw_data
data<-read.csv("raw_data.csv", na.strings=c(""), stringsAsFactors=FALSE)

# store total number of participants
n<-length(data[,1])

## There are 4 groups defined as follow:
#    Reward_Type  Amount 
# g1    cash       £5  
# g2    cash       £2 
# g3    product    £5  
# g4    product    £2 

# defining variable names
# (k=Kind, T=Thoughfulh, S=Selfish, G=Generous)

variables<-c("vd_0f", "vd_2f", "vd_1f", "exp_x1_K", "exp_x1_T", 
             "exp_x1_S.R", "exp_x1_G", "exp_x2_K", "exp_x2_T", "exp_x2_S.R", 
             "exp_x2_G", "neg.ev_1", "neg.ev_2.R", "neg.ev_3", "neg.ev_4.R",
             "neg.ev_5", "neg.ev_6", "neg.ev_7.R", "neg.ev_8", "neg.ev_9",
             "neg.ev_10.R", "neg.ev_11", "neg.ev_12", "gender", "income",
             "age", "employ")

#Filtering Groups
g1<-data[c(2:n),c(34:36, 39:42, 44:47, 87:98, 100:103)]
tidy_g1<-g1[complete.cases(g1),]
colnames(tidy_g1)<-variables

g2<-data[c(2:n),c(48:50, 52:55, 57:60, 87:98, 100:103)]
tidy_g2<-g2[complete.cases(g2),]
colnames(tidy_g2)<-variables

g3<-data[c(2:n),c(61:63, 65:68, 70:73, 87:98, 100:103)]
tidy_g3<-g3[complete.cases(g3),]
colnames(tidy_g3)<-variables

g4<-data[c(2:n),c(74:76, 78:81, 83:86, 87:98, 100:103)]
tidy_g4<-g4[complete.cases(g4),]
colnames(tidy_g4)<-variables

#Building final data set
groups_data<- rbind(tidy_g1,tidy_g2,tidy_g3,tidy_g4)

# Defining Length of each group
l1<- nrow(tidy_g1)
l2<- nrow(tidy_g2)
l3<- nrow(tidy_g3)
l4<- nrow(tidy_g4)

# Create vector defining the "Reward Amount"
r_amount_length<-c(l1,l2,l3,l4)
r_amount<- rep.int(c(1,2,1,2), times= r_amount_length)

# Create vector defining the "Type of Reward" (Cach vs Product)
r_type_length<-c(l1+l2, l3+l4)
r_type<- rep.int(1:2, times = r_type_length)

# Joining data with groups information
df<-cbind(groups_data, r_amount, r_type)

##function to transform all values to "numeric"data

df[1:ncol(df)] <- lapply(df[1:ncol(df)], function(x) as.numeric(x))

### FUNCTION changing data values

new_values<-function(x){
        data=NULL
        
        pos<-c(4:5,7:9,11,12,14,16,17,19,20,22,23)
        
        x[,pos]<- x[,pos]/10
        
        rev.ex<-c(6,10)
        
        x[,rev.ex][x[,rev.ex]==90] <- (1)
        x[,rev.ex][x[,rev.ex]==80] <- (2)
        x[,rev.ex][x[,rev.ex]==70] <- (3)
        x[,rev.ex][x[,rev.ex]==60] <- (4)
        x[,rev.ex][x[,rev.ex]==50] <- (5)
        x[,rev.ex][x[,rev.ex]==40] <- (6)
        x[,rev.ex][x[,rev.ex]==30] <- (7)
        x[,rev.ex][x[,rev.ex]==20] <- (8)
        x[,rev.ex][x[,rev.ex]==10] <- (9)
        
        rev.neg<-c(13,15,18,21)
        
        x[,rev.neg][x[,rev.neg]==50] <- (1)
        x[,rev.neg][x[,rev.neg]==40] <- (2)
        x[,rev.neg][x[,rev.neg]==30] <- (3)
        x[,rev.neg][x[,rev.neg]==20] <- (4)
        x[,rev.neg][x[,rev.neg]==10] <- (5)
        
        data=as.data.frame(x)       
        tidy<<-data
}

new_values(df) #running function

# Create CSV File with clean data
write.csv(tidy, file="clean_data.csv")
