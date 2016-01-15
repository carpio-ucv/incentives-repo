##library
library("car")

## Change local directory
#setwd("C:/Users/wbs.WBS004287/Documents")
setwd("C:/Users/K56CA/Dropbox/PhD/Experiments Incentives/Follow_Up_Incentives")
data2<-read.csv("pilot2.csv", na.strings=c(""), stringsAsFactors=FALSE)

variables<-c("name", "Unkind(*)", "Friendly", "Rude(*)", 
             "Generous", "Giving", "Inconsid.(*)", "Nice", "group")
#Filtering Groups
g1_2<-data2[c(2:29),c(11, 14:20)]
tidy_2<-cbind(g1_2[complete.cases(g1_2),], rep(1, each=14))
colnames(tidy_2)<-variables

g2_2<-data2[c(2:29),c(11, 23:29)]
tidy2_2<-cbind(g2_2[complete.cases(g2_2),], rep(2, each=13))
colnames(tidy2_2)<-variables

# Merging clean data set
clean_2<-rbind(tidy_2, tidy2_2) 
clean_2$group <- factor(clean_2$group, levels = c(1,2),
                      labels = c("prod.", "cash")) 

### FUNCTION changing data values

values_2<-function(x){
        data=NULL
        for (i in 2:8) 
                x[i]<-as.numeric(x[,i])
        
        pos<-c(3,5,6,8)
        
        x[,pos]<- x[,pos]/10
        
        neg<-c(2,4,7)
        
        x[,neg][x[,neg]=="70"] <- (1)
        x[,neg][x[,neg]=="60"] <- (2)
        x[,neg][x[,neg]=="50"] <- (3)
        x[,neg][x[,neg]=="40"] <- (4)
        x[,neg][x[,neg]=="30"] <- (5)
        x[,neg][x[,neg]=="20"] <- (6)
        x[,neg][x[,neg]=="10"] <- (7)
        
        data=as.data.frame(x)       
        new_tidy<<-data
}

#running function
values_2(clean_2)
head(new_tidy)

####
g_2<- new_tidy[2:9] %>% group_by(group)  %>% summarise_each(funs(mean))

wilcox.test(y~A)

## graph per participant
df_2 <- melt(new_tidy)
#by condition
ggplot(df_2, aes(variable, value, group=factor(name))) + 
        geom_line(aes(color=factor(group)))
#by person
ggplot(df, aes(variable, value, group=factor(name))) + 
        geom_line(aes(color=factor(name)))

#TOTAL 2 PILOTS
def_t<-rbind(tidy,new_tidy)
pilot<-rbind(cbind((rep(1,20))), cbind((rep(2,27))))
def_tidy<-cbind(def_t, pilot)


g_def<- def_tidy[2:10] %>% group_by(group)  %>% summarise_each(funs(mean))
g_pilots<- def_tidy[2:10] %>% group_by(pilot)  %>% summarise_each(funs(mean))

##graphs 2 pilots
df_pilot <- melt(def_tidy)
ggplot(df_pilot, aes(variable, value, group=factor(name))) + 
        stat_boxplot()

### FUNCTION calculating t.tests between conditions IN TOTAL

t<-lapply(def_tidy[,2:8], 
       function(x) t.test(x ~ def_tidy[,9], var.equal = TRUE))

data.frame(p.value = sapply(t, getElement, name = "p.value"))


### FUNCTION calculating t.tests between conditions IN PILOT 2

t<-lapply(new_tidy[,2:8], 
          function(x) t.test(x ~ new_tidy[,9], var.equal = TRUE))

data.frame(p.value = sapply(t, getElement, name = "p.value"))


###Compare means between pilots
g_def<- def_tidy[2:9] %>% group_by(pilot)  %>% summarise_each(funs(mean))


### FUNCTION calculating MANN WITNEY between conditions IN PILOT 2

W<-lapply(new_tidy[,2:8], 
          function(x) wilcox.test(x ~ new_tidy[,9], var.equal = TRUE))

data.frame(p.value = sapply(W, getElement, name = "p.value"))


#ITEMS ANALYSIS
library("psychometric")
item.exam(def_tidy[,2:8], y=NULL, discrim=TRUE)

#variability analysis
var<- tidy[2:9] %>% group_by(group)  %>% summarise_each(funs(sd))
var_2<- new_tidy[2:9] %>% group_by(group)  %>% summarise_each(funs(sd))
var_def<- def_tidy[2:10] %>% group_by(group)  %>% summarise_each(funs(sd))


tvar<- tidy[2:9]  %>% summarise_each(funs(sd))
tvar_2<- new_tidy[2:9]  %>% summarise_each(funs(sd))
tvar_def<- def_tidy[2:10]  %>% summarise_each(funs(sd))



rbind(var, var_2, var_def)
#####
wilcox.test(y~A)
