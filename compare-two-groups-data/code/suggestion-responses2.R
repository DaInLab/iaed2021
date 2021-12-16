#For information, the random-effect model given by @Henrik:

if(!("lme4") %in% installed.packages()) install.packages("lme4")
library(lme4)
f <- function(x) sqrt(x)
( fit1 <- lmer(f(Value) ~ Group + (1|Subject), data=dat) )
#Linear mixed model fit by REML ['lmerMod']
#Formula: f(Value) ~ Group + (1 | Subject) 
#  Data: dat 
#REML criterion at convergence: 296.3579 
#Random effects:
#  Groups   Name        Std.Dev.
#  Subject  (Intercept) 0.5336  
#  Residual             0.8673  
#Number of obs: 108, groups: Subject, 18
#Fixed Effects:
#(Intercept)       Group2       Group3  
#    3.03718     -0.07541      1.11886  

#is equivalent to a generalized least-squares model with an exchangeable correlation structure for subjects:
library(nlme)
fit2 <-  gls(f(Value) ~ Group, data=dat, 
             na.action=na.omit, 
             correlation=corCompSymm(form= ~  1 | Subject)) 
#The fitted variance matrix is then:
getVarCov(fit2)
#Marginal variance covariance matrix
#         [,1]    [,2]    [,3]    [,4]    [,5]    [,6]
# [1,] 1.03690 0.28471 0.28471 0.28471 0.28471 0.28471
# [2,] 0.28471 1.03690 0.28471 0.28471 0.28471 0.28471
# [3,] 0.28471 0.28471 1.03690 0.28471 0.28471 0.28471
# [4,] 0.28471 0.28471 0.28471 1.03690 0.28471 0.28471
# [5,] 0.28471 0.28471 0.28471 0.28471 1.03690 0.28471
# [6,] 0.28471 0.28471 0.28471 0.28471 0.28471 1.03690
#   Standard Deviations: 1.0183 1.0183 1.0183 1.0183 1.0183 1.0183

#As you can see, the diagonal entry corresponds to the total variance in the first model:
VarCorr(fit1)
#Groups   Name        Std.Dev.
#Subject  (Intercept) 0.53358 
#Residual             0.86731 
0.53358^2+0.86731^2
#[1] 1.036934
#and the covariance corresponds to the between-subject variance:
0.53358^2
#[1] 0.2847076


#Actually the gls model is more general because it allows a negative covariance. 
#The advantage of nlme is that you can more generally use other repeated correlation 
#structures and also you can specify different variances per group with the weights argument.
#I think that residuals are different because they are constructed with the random-effects in the first model. 
#In order to get multiple comparisons you can use the lsmeans and the multcomp packages, 
#but the 𝑝-values of the hypotheses tests are anticonservative with defaults (too high) degrees of freedom. 
#Unfortunately, the pbkrtest package does not apply to gls/lme models.
