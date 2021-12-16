#Now, try to you write down the model: 𝑦𝑖𝑗𝑘=... where 𝑦𝑖𝑗𝑘 is the 𝑘-th value for 
#individual 𝑗 of group 𝑖. 

#Then look at what happens for the means 𝑦¯𝑖𝑗∙: you get a classical Gaussian linear model, 
#with variance homogeneity because there are 6 repeated measures for each subject:
xtabs(~Group+Subject, data=dat)
#     Subject
#Group 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18
#    1 6 6 6 6 6 6 6 0 0  0  0  0  0  0  0  0  0  0
#    2 0 0 0 0 0 0 0 6 6  6  6  6  6  6  6  0  0  0
#    3 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  6  6  6

#Thus, since you are interested in mean comparisons only, you don't need to resort 
# to a random-effect or generalised least-squares model - just use a classical (fixed effects)
#model using the means 𝑦¯𝑖𝑗∙ as the observations:

tdat <- transform(dat, tvalue=f(Value))
dd <- aggregate(tvalue~Group+Subject, data=tdat, FUN=mean)
fit3 <- lm(tvalue~Group, data=dd)

#I think this approach always correctly work when we average the data over the levels of a random
#effect (I show on my blog how this fails for an example with a fixed effect).
#The ANOVA provides the same answer as @Henrik's approach (and that shows that Kenward-Rogers 
#approximation is correct):
anova(fit3)
#Analysis of Variance Table
#Response: tvalue
#          Df Sum Sq Mean Sq F value  Pr(>F)  
#Group      2 3.3799 1.68994   4.121 0.03747 *
#  Residuals 15 6.1512 0.41008                  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Then you can use TukeyHSD() or the lsmeans package for multiple comparisons:
TukeyHSD(aov(fit3), "Group")
#  Tukey multiple comparisons of means
#   95% family-wise confidence level
#
#Fit: aov(formula = fit3)
#
#$Group
#diff         lwr       upr     p adj
#2-1 -0.07541248 -0.93627828 0.7854533 0.9719148
#3-1  1.11885667 -0.02896441 2.2666777 0.0565628
#3-2  1.19426915  0.06817536 2.3203629 0.0370434

if(!("lsmeans") %in% installed.packages()) {
  install.packages("lsmeans")
}
library(lsmeans)

lsmeans(fit3, pairwise~Group)
