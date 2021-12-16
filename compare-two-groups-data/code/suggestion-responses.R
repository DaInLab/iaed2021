#To control for the zero floor effect (i.e., positive skew), I fit two alternative versions transforming
#the dependent variable either with sqrt for mild skew and log for stronger skew.

if(!("afex") %in% installed.packages()) {
  install.packages("afex")
}
library(afex)
# read the dput() in as dat <- ...    
dat <- as.data.frame(dat)
dat$Group <- factor(dat$Group)
dat$Subject <- factor(dat$Subject)

(model <- mixed(Value ~ Group + (1|Subject), dat))
##        Effect    stat ndf ddf F.scaling p.value
## 1 (Intercept) 237.730   1  15         1  0.0000
## 2       Group   7.749   2  15         1  0.0049

(model.s <- mixed(sqrt(Value) ~ Group + (1|Subject), dat))
##        Effect    stat ndf ddf F.scaling p.value
## 1 (Intercept) 418.293   1  15         1  0.0000
## 2       Group   4.121   2  15         1  0.0375

(model.l <- mixed(log1p(Value) ~ Group + (1|Subject), dat))
##        Effect    stat ndf ddf F.scaling p.value
## 1 (Intercept) 458.650   1  15         1  0.0000
## 2       Group   2.721   2  15         1  0.0981

#The effect is significant for the untransformed and sqrt dv. But are these model sensible? 
#Let's plot the residuals.

png("./graphics/qq.png", 800, 300, units = "px", pointsize = 12)
par(mfrow = c(1, 3))
par(cex = 1.1)
par(mar = c(2, 2, 2, 1)+0.1)
qqnorm(resid(model[[2]]), main = "original")
qqline(resid(model[[2]]))
qqnorm(resid(model.s[[2]]), main = "sqrt")
qqline(resid(model.s[[2]]))
qqnorm(resid(model.l[[2]]), main = "log")
qqline(resid(model.l[[2]]))
dev.off()

#It seems that the model with sqrt trasnformation provides a reasonable fit 
# (there still seems to be one outlier, but I will ignore it). 
#So, let's further inspect this model using multcomp to get the comparisons among groups:

if(!("multcomp") %in% installed.packages()){
  install.packages("multcomp")
}
library(multcomp)
# using bonferroni-holm correction of multiple comparison
summary(glht(model.s[[2]], linfct = mcp(Group = "Tukey")), test = adjusted("holm"))
##          Simultaneous Tests for General Linear Hypotheses
## 
## Multiple Comparisons of Means: Tukey Contrasts
## 
## 
## Fit: lmer(formula = sqrt(Value) ~ Group + (1 | Subject), data = data)
## 
## Linear Hypotheses:
##            Estimate Std. Error z value Pr(>|z|)  
## 2 - 1 == 0  -0.0754     0.3314   -0.23    0.820  
## 3 - 1 == 0   1.1189     0.4419    2.53    0.023 *
## 3 - 2 == 0   1.1943     0.4335    2.75    0.018 *
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## (Adjusted p values reported -- holm method)

# using default multiple comparison correction (which I don't understand)
summary(glht(model.s[[2]], linfct = mcp(Group = "Tukey")))
##          Simultaneous Tests for General Linear Hypotheses
## 
## Multiple Comparisons of Means: Tukey Contrasts
## 
## 
## Fit: lmer(formula = sqrt(Value) ~ Group + (1 | Subject), data = data)
## 
## Linear Hypotheses:
##            Estimate Std. Error z value Pr(>|z|)  
## 2 - 1 == 0  -0.0754     0.3314   -0.23    0.972  
## 3 - 1 == 0   1.1189     0.4419    2.53    0.030 *
## 3 - 2 == 0   1.1943     0.4335    2.75    0.016 *
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## (Adjusted p values reported -- single-step method)

##Punchline: group 3 differs from the other two groups which do not differ among each other.