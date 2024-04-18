#######################R codes for PSM####################################################
# Install packages, do not remove the packages
packages <- c("tidyverse", "dplyr", "haven", "MatchIt", "cobalt", "marginaleffects");for(pack in packages){
  if(!require(pack, character.only = TRUE)){
    install.packages(pack, dependencies = TRUE)
    require(pack, character.only = TRUE)
  }
  # Clean up the workspace
  rm(pack)
}

data("lalonde")


##The data consists of a number of demographic variables
#(age, race, academic background, and previous real earnings), 
#as well as a treatment indicator, and the real earnings in the year 1978 (the response)
#Robert Lalonde, "Evaluating the Econometric Evaluations of Training Programs",
#American Economic Review, Vol. 76, pp. 604-620


##Let's inspect the dataset##
data <- as.data.frame(lalonde)
race_factor <- as.numeric(as.factor(data$race))
data <- cbind(data,race_factor)

summary(data$treat) ##30.1% of the sample received treatment##
observables <- c("age","educ","race","married","nodegree","re74","re75")
summary(data[observables]) ##here are some descriptives for our observables X##


##We look at the covariate balance pre-matching##
m.out0 <- matchit(treat ~ age + educ + race + married + 
                    nodegree + re74 + re75, data = lalonde,
                  method = NULL) ##we're not yet matching here: note that method=NULL here as we are looking for pre-matching balance##

summary(m.out0) ##Here's the initial balance based on SMD: a SMD > 0.1 to 0.2 suggests imbalance (as expected)##

t.test(data$age ~ data$treat)
t.test(data$educ ~ data$treat)
t.test(data$re74 ~ data$treat)
t.test(data$re75 ~ data$treat)
###you can do the same for the other variables, but the point is that it is NOT balanced pre-matching##


#####################We now proceed to matching###########################
# We use the simplest method NN-1 PS matching w/o replacement #
m.out1 <- matchit(treat ~ age + educ + race + married + 
                    nodegree + re74 + re75, data = lalonde,
                  method = "nearest", ratio=1, distance = "glm", link="logit") #method is NN-1 and we use a logit model#
m.out1 ##this summarizes the matching algorithm##
##We have 370 matched observations, being 185 treatment units and 185 matched control units##

summary(m.out1, un = FALSE) ##we set un=FALSE to suppress the data before matching since we've already seen it##

match.treat <- data[row.names(m.out1$match.matrix),] ##note that this has 185 observations##
matx <- m.out1$match.matrix
dim(matx) <- c(dim(matx)[1]*dim(matx)[2],1) #flatten matrix#
match.control <- data[matx,] ##this also has 185 observations##

age.treat <- match.treat$age
educ.treat <- match.treat$educ
married.treat <- match.treat$married
nodegree.treat <- match.treat$nodegree
re74.treat <- match.treat$re74
re75.treat <- match.treat$re75

age.control <- match.control$age
educ.control <- match.control$educ
married.control <- match.control$married
nodegree.control <- match.control$nodegree
re74.control <- match.control$re74
re75.control <- match.control$re75

#mean difference in ages is not statistically significant#
t.test(age.treat,age.control,paired=T) 
t.test(educ.treat,educ.control,paired=T)
t.test(married.treat,married.control,paired=T)
t.test(nodegree.treat,nodegree.control,paired=T)
t.test(re74.treat,re74.control,paired=T)
t.test(re75.treat,re75.control,paired=T)

#####################we visualize the balance##############

plot(m.out1, type = "density", interactive = FALSE,
     which.xs = ~ age + educ + married + nodegree + re74 + re75 )
plot(m.out1, type = "jitter", interactive = FALSE)
plot(m.out1, type = "hist", interactive = FALSE,
     ylim=c(0,100))
love.plot(bal.tab(m.out1,m.threshold=0.1),stat="mean.diffs",grid=T,stars="raw",abs=F) #standardized mean differences mostly fall inside +/-0.1 post-matching#


##################Now we try a different matching algo###################
m.out2 <- matchit(treat ~ age + educ + race + married + 
                    nodegree + re74 + re75, data = lalonde,
                  method = "full", distance = "glm", link = "probit") ##here we use full matching, and use a probit model for propensity scores##

###Letting method = "full" performs optimal full matching, which is a form of subclassification wherein all units, both treatment and control##
##(i.e., the "full" sample), are assigned to a subclass and receive at least one match.##
##The matching is optimal in the sense that that sum of the absolute distances between the treated and control units in each subclass##
##is as small as possible##

summary(m.out2, un=T) ##Look at the SMD, they are all <0.1 post-matching##

plot(m.out2, type = "density", interactive = FALSE,
     which.xs = ~age + educ + married + nodegree + re74 + re75 )
plot(m.out2, type = "jitter", interactive = FALSE) ##note the effect of choosing full matching##
plot(m.out2, type = "hist", interactive = FALSE) ##look at the matched propensity scores##
love.plot(bal.tab(m.out2,m.threshold=0.1),stat="mean.diffs",grid=T,stars="raw",abs=F) #standardized mean differences fall outside +/-0.1 post-matching#

###########################Now we estimate the treatment effect####################################
m.data2 <- match.data(m.out2)
head(m.data2) ##distance is the propensity score##

fit <- lm(re78 ~ treat, data = m.data2, weights = weights) ##this is a regular specification##
summary(fit) ##in a regular specification, the estimated TOT is 1832##
avg_comparisons(fit,
                variables = "treat",
                vcov = ~subclass,
                newdata = subset(m.data, treat == 1),
                wts = "weights") ##we use clustered standard errors in this case, hence vcov = ~subclass##



fit_robust <- lm(re78 ~ treat * (age + educ + race + married + nodegree + 
                            re74 + re75), data = m.data2, weights = weights) ##this is a robust specification##
summary(fit_robust)
avg_comparisons(fit_robust,
                variables = "treat",
                vcov = ~subclass,
                newdata = subset(m.data2, treat == 1),
                wts = "weights") ##we use clustered standard errors in this case, hence vcov = ~subclass##

##The estimated effect was $1912 (SE = 764.7, p = 0.012), indicating that the average effect of the treatment for those who received it is 
##to increase earnings##


##########################We also try to estimate the treatment effect for our first model (Logit PSM with NN-1 matching)##############
m.data1 <- match.data(m.out1)
head(m.data1) ##distance is the propensity score##

fit_NN1 <- lm(re78 ~ treat, data = m.data1, weights = weights) ##this is a regular specification##
avg_comparisons(fit_NN1,
                variables = "treat",
                vcov = ~subclass,
                newdata = subset(m.data, treat == 1),
                wts = "weights") ##we use clustered standard errors in this case, hence vcov = ~subclass##
##in a regular specification, the estimated TOT is 894##

fit_robust_NN1 <- lm(re78 ~ treat * (age + educ + race + married + nodegree + 
                                   re74 + re75), data = m.data1, weights = weights) ##this is a robust specification##
avg_comparisons(fit_robust_NN1,
                variables = "treat",
                vcov = ~subclass,
                newdata = subset(m.data1, treat == 1),
                wts = "weights") ##we use clustered standard errors in this case, hence vcov = ~subclass##

##The estimated effect was $1121 (SE = 764.7, p = 0.012), indicating that the average effect of the treatment for those who received it is 
##to increase earnings##


