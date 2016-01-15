##library
library("car")

## Change local directory
#setwd("C:/Users/wbs.WBS004287/Documents")
setwd("C:/Users/K56CA/Dropbox/PhD/Experiments Incentives/Follow_Up_Incentives")
data<-read.csv("pilot.csv", na.strings=c(""), stringsAsFactors=FALSE)

variables<-c("name", "Unkind(*)", "Friendly", "Rude(*)", 
             "Generous", "Giving", "Inconsid.(*)", "Nice", "group")
#Filtering Groups
g1<-data[c(2:21),c(11, 14:20)]
tidy<-cbind(g1[complete.cases(g1),], rep(1, each=10))
colnames(tidy)<-variables

g2<-data[c(2:21),c(11, 23:29)]
tidy2<-cbind(g2[complete.cases(g2),], rep(2, each=10))
colnames(tidy2)<-variables

# Merging clean data set
clean<-rbind(tidy, tidy2) 
clean$group <- factor(clean$group, levels = c(1,2),
                    labels = c("prod.", "cash")) 

### FUNCTION changing data values

values<-function(x){
        data=NULL
        for (i in 2:8) 
                x[i]<-as.numeric(x[,i])
        
        pos<-c(3,5,6,8)
        
       x[,pos][x[,pos]=="18"] <- (7)
       x[,pos][x[,pos]=="16"] <- (6)
       x[,pos][x[,pos]=="14"] <- (5)
       x[,pos][x[,pos]=="13"] <- (4)
       x[,pos][x[,pos]=="12"] <- (3)
       x[,pos][x[,pos]=="8"] <- (2)
        
        neg<-c(2,4,7)
        
        x[,neg][x[,neg]=="1"] <- (7)
        x[,neg][x[,neg]=="18"] <- (1)
        x[,neg][x[,neg]=="16"] <- (2)
        x[,neg][x[,neg]=="14"] <- (3)
        x[,neg][x[,neg]=="13"] <- (4)
        x[,neg][x[,neg]=="12"] <- (5)
        x[,neg][x[,neg]=="8"] <- (6)
        
        data=as.data.frame(x)       
        tidy<<-data
}

#running function
values(clean)
head(tidy)

####
g<- tidy[2:9] %>% group_by(group)  %>% summarise_each(funs(mean))
wilcox.test(y~A)

## graph per participant
df <- melt(tidy)
#by condition
ggplot(df, aes(variable, value, group=factor(name))) + 
        geom_line(aes(color=factor(group)))
#by person
ggplot(df, aes(variable, value, group=factor(name))) + 
        geom_line(aes(color=factor(name)))
#by person
ggplot(df, aes(variable, value, group=factor(group))) + 
        geom_bar(aes(color=factor(group)))+
        stat_bin()+
        geom_bar(position = "dodge")