
##libraries
library("prefmod", lib.loc="~/R/win-library/3.2")

##DATA SETTING
setwd("C:/Users/K56CA/Dropbox/Big Data/Robust Methods R")#cambiar
exp <- read.csv("bltexp2.csv") # cambiar... 
exp2 <- llbt.design(exp, nitems = 5, cat.scovs = "cond")
gr <- factor(exp2$cond)
names(exp2)[5:9] <- c("GIFT50", "GIFT75", "GIFT25", "GIFT0", "GIFT100")


###GENERAL MODEL GNM
res <- gnm(formula = y ~ GIFT100 + GIFT75 + GIFT50 + GIFT25 + GIFT0
           , eliminate = mu:cond, family = poisson, data = exp2)
summary(res)

1-pchisq(res$deviance, df= res$df)

#MODEL INCLUDING GROUPS INTERACTION
resgr <- gnm(formula = y ~ GIFT100 + GIFT75 + GIFT50 + GIFT25 + GIFT0 +
               (GIFT100 + GIFT75 + GIFT50 + GIFT25 + GIFT0):cond, 
             eliminate = mu:cond, family = poisson, data = exp2)
summary(resgr)

1-pchisq(resgr$deviance, df= resgr$df)

##ANOVA
anova(res,resgr2, test = "Chisq")

##COEFFICIENTS res-MODEL 1
res$coefficients[5] <- 0
worth <- round(exp(2 * res$coefficients[1:5])/(sum(exp(2 *
                        res$coefficients[1:5]))), digits = 5)
print(worth)

##gesgr coefficients

coefg2 <- resgr$coefficients [1:5] + resgr$coefficients [c(6,8,10,12,14)]
coefg2

coefg3 <- resgr$coefficients [1:5] + resgr$coefficients [c(7,9,11,13,15)]
coefg3


##WORTH 1 (CONTROL NO AWARENESS)
resgr$coefficients[c(5)] <- 0
worth1 <- round(exp(2 * resgr$coefficients[1:5])/(sum(exp(2 *
                                                        resgr$coefficients[1:5]))), digits = 5)
print(worth1)

##WORTH 2 (NO RECIPROCITY)
coefg2[c(5)] <- 0
worth2 <- round(exp(2 * coefg2[1:5])/(sum(exp(2 * coefg2[1:5]))), digits = 5)
print(worth2)


##WORTH 3 (RECIPROCITY)
coefg3[c(5)] <- 0
worth3 <- round(exp(2 * coefg3[1:5])/(sum(exp(2 *coefg3[1:5]))), digits = 5)
print(worth3)


#PLOT WORTH- pi PARAMETER- FIGURE 7

matrixplot <- matrix(cbind(worth1, worth2, worth3), nrow=5, ncol=3) 
stores<- c("Give-100%", "Give-75%", "Give-50%","Give-25%", "Give-0%")
dimnames(matrixplot)= list(stores, c("No Awareness", "NO-Reciprocity", "Reciprocity"))
matrixplot
plot.wmat(matrixplot, ylab = "Worth Parameters", main = NULL, 
          psymb = c(16, 15, 17, 18, 19), ylim= c(0, 0.45))


##OTHERS
###MODEL 1-LLBTPC
exp2ch <- exp2[,c("y", "mu", "g0", "g1", "GIFT100", "GIFT75",
                  "GIFT50", "GIFT25", "GIFT0", "cond")] 
items <- c("Give-100", "Give-75", "Give-50","Give-25", "Give-0")
respc <- llbtPC.fit(exp2ch, nitems = 5, formel = ~gr, elim = ~1,
                    undec = FALSE)
summary(respc)
pis<-llbt.worth(respc)
pis

################
# STAGE 2  #####
################

##DATA SETTING
setwd("C:/Users/K56CA/Dropbox/Robust Methods R")
exp <- read.csv("bltexp2B.csv")
exp2b <- llbt.design(exp, nitems = 3, cov.sel = "cond")
gr <- factor(exp2b$cond)
names(exp2b)[5:7] <- c("Certainty_Both", "Uncertainty_Partner", 
                       "Uncertainty_Both")



### GENERAL MODEL GNM
res2 <- gnm(formula = y ~ Uncertainty_Partner + Uncertainty_Both + 
                    Certainty_Both , family = poisson, eliminate = mu:gr
                        , data = exp2b)
summary(res2)
1-pchisq(res2$deviance, df= res2$df)

#MODEL INCLUDING GROUPS INTERACTION
resgr2 <- gnm(formula = y ~ Uncertainty_Partner + Uncertainty_Both + 
                      Certainty_Both+(Uncertainty_Partner + 
                        Uncertainty_Both + Certainty_Both):gr,
             eliminate = mu:gr, family = poisson, data = exp2b)
summary(resgr2)
1-pchisq(resgr2$deviance, df= resgr2$df)

##ANOVA
anova(res2,resgr2, test = "Chisq")

##gesgr coefficients

coef2g2 <- resgr2$coefficients [1:3] + resgr2$coefficients [c(4,6,8)]
coef2g2

coef2g3 <- resgr2$coefficients [1:3] + resgr2$coefficients [c(5,7,9)]
coef2g3


##WORTH parameters model 1
res2$coefficients[c(3)] <- 0
worth1 <- round(exp(2 * res2$coefficients[1:3])/(sum(exp(2 *
                        res2$coefficients[1:3]))), digits = 5)
print(worth1)




#PLOT WORTH- pi PARAMETER - MODEL 1-DEF

matrixplot <- matrix(worth1, nrow=3, ncol=1) 
stores<- c("Uncertainty_Partner", 
           "Uncertainty_Both", "Certainty_Both")
dimnames(matrixplot)= list(stores, c("Loyalty Programmes"))
plotworth(matrixplot, ylab = "Worth Parameters", main = NULL, 
          pcol = c("black", "gray", "black"), 
          psymb = c(16, 17, 15), ylim= c(0, 0.6))

########## por sia ####
##WORTH 2 (NO RECIPROCITY)
coef2g2[c(3)] <- 0
worth2 <- round(exp(2 * coef2g2[1:3])/(sum(exp(2 * coef2g2[1:3]))), digits = 5)
print(worth2)


##WORTH 3 (RECIPROCITY)
coef2g3[c(3)] <- 0
worth3 <- round(exp(2 * coef2g3[1:3])/(sum(exp(2 *coef2g3[1:3]))), digits = 5)
print(worth3)

#PLOT WORTH- pi PARAMETER

matrixplot <- matrix(cbind(worth1, worth2, worth3), nrow=3, ncol=3) 
stores<- c("GIFT10", "GIFT7525", "GIFT50")
dimnames(matrixplot)= list(stores, c("No Awareness", "NO-Reciprocity", "Reciprocity"))
matrixplot
plotworth(matrixplot, ylab = "Worth Parameters", main = NULL, 
          pcol = c("black", "gray", "black"), 
          psymb = c(16, 15, 17), ylim= c(0, 0.6))
