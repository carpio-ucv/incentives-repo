####################
# EXPERIMENT 3 #####
####################

##libraries
library("prefmod", lib.loc="~/R/win-library/3.2")

##DATA SETTING
setwd("C:/Users/K56CA/Dropbox/Big Data/Robust Methods R/R Experiment 3")
exp <- read.csv("btlexp3.csv")
exp3 <- llbt.design(exp, nitems = 6, cat.scovs = "cond")
gr <- factor(exp3$cond)
names(exp3)[5:10] <- c("GIFT50x1", "GIFT25x2", "GIFT30x1", "GIFT15x2",
                      "GIFT10x1", "GIFT5x2")
head(exp3)

###GENERAL MODEL GNM
res <- gnm(formula = y ~ GIFT10x1 + GIFT5x2+ GIFT30x1 + GIFT15x2 +
                   + GIFT25x2 + GIFT50x1, eliminate = mu:cond, 
                family = poisson, data = exp3)
summary(res)
1-pchisq(res$deviance, df= res$df)

#MODEL INCLUDING GROUPS INTERACTION
resgr <- gnm(formula = y ~ GIFT10x1 + GIFT5x2+ GIFT30x1 + GIFT15x2 +
                + GIFT25x2 + GIFT50x1+(GIFT10x1 + GIFT5x2+ GIFT30x1 
                + GIFT15x2 +GIFT25x2 + GIFT50x1):cond, 
             eliminate = mu:cond, family = poisson, data = exp3)
summary(resgr)
1-pchisq(resgr$deviance, df= resgr$df)

##ANOVA
anova(res,resgr, test = "Chisq")


##gesgr coefficients

coefg2 <- resgr$coefficients [1:6] + resgr$coefficients [c(7,9,11,13,15,17)]
coefg2

coefg3 <- resgr$coefficients [1:6] + resgr$coefficients [c(8,10,12,14,16,18)]
coefg3


##WORTH 1 (CONTROL NO AWARENESS)
resgr$coefficients[c(6)] <- 0
worth1 <- round(exp(2 * resgr$coefficients[1:6])/(sum(exp(2 *
                        resgr$coefficients[1:6]))), digits = 5)
print(worth1)

##WORTH 2 (NO RECIPROCITY)
coefg2[c(6)] <- 0
worth2 <- round(exp(2 * coefg2[1:6])/(sum(exp(2 * coefg2[1:6]))), digits = 5)
print(worth2)

##WORTH 3 (RECIPROCITY)
coefg3[c(6)] <- 0
worth3 <- round(exp(2 * coefg3[1:6])/(sum(exp(2 *coefg3[1:6]))), digits = 5)
print(worth3)

#PLOT WORTH- pi PARAMETER

matrixplot <- matrix(cbind(worth1, worth2, worth3), nrow=6, ncol=3) 
stores<- c("GIFT10x1", "GIFT5x2","GIFT30x1", "GIFT15x2", "GIFT50x1", 
           "GIFT25x2")
dimnames(matrixplot)= list(stores, c("NO-Awareness", "NO-Reciprocity", "Reciprocity"))
matrixplot
plot.wmat(matrixplot, ylab = "Worth Parameters", main = NULL, 
          palette="Set1",
          psymb = c(17, 15, 16, 18, 19, 20), ylim= c(0, 0.35))


################## FINO HASTA ACA

#MODEL INCLUDING OBJECT COVARIANT AND GROUPS INTERACTION

COV <- data.frame(split = c(0, 1, 0, 1, 0, 1), 
                   share = c(50, 50, 20, 20, 10, 10))

exp3b <- llbt.design(exp, nitems = 6,  objcov= COV, cat.scovs = "cond")
names(exp3b)[5:10] <- c("GIFT50x1", "GIFT25x2", "GIFT30x1", "GIFT15x2",
                        "GIFT10x1", "GIFT5x2")
##0
resobj <- gnm(formula = y ~ split+share+cond:(split+share),
              eliminate = mu, family = poisson, data = exp3b)
summary(resobj)
1-pchisq(resobj$deviance, df= resobj$df)

##1
resobj1 <- gnm(formula = y ~ split+ share,
              eliminate = mu, family = poisson, data = exp3b)
summary(resobj1)
1-pchisq(resobj1$deviance, df= resobj1$df)

##2
resobj2 <- gnm(formula = y ~  split + share + split:cond + share:cond,
              eliminate = mu:cond, family = poisson, data = exp3b)
summary(resobj2)
1-pchisq(resobj2$deviance, df= resobj2$df)

##ANOVA
anova(resgr,resobj2, test = "Chisq")


#####

OBJ <- as.matrix(exp3[,5:10])
splitdf <- data.frame(c(0, 1, 0, 1, 0, 1))
split <- (c(0, 1, 0, 1, 0, 1))
share <- c(50, 50, 30, 30, 10, 10)
COV <- data.frame(OBJ * split)

head(COV)

give<-c(0, 0, 1, 1, 2, 2)
givef<-factor(c(0, 0, 1, 1, 2, 2))
nfriend<-c(1, 0, 1, 0, 1, 0)
nfriendf<-factor(c(1, 0, 1, 0, 1, 0))
COV2 <- data.frame(OBJ * split)

OBJ <- data.frame(give, nfriend)

exp3b <- llbt.design(exp, nitems = 6, cat.scovs = "cond", objcov= splitdf)
names(exp3b)[5:10] <- c("GIFT50x1", "GIFT25x2", "GIFT30x1", "GIFT15x2",
                       "GIFT10x1", "GIFT5x2")








