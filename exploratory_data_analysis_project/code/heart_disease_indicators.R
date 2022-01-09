# Heart disease indicators
#Simple analysis which should help to find three most promising attributes for predicting 
# possible diameter narrowing
# Data from UCI Machine Learning Repository

#Downloading data
if (!file.exists("./data/processed.cleveland.data")) {
  download.file(url = "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data", 
                destfile = "./data/processed.cleveland.data")
}
require(tools)
md5sum("./data/processed.cleveland.data")
#   ./data/processed.cleveland.data 
#"2d91a8ff69cfd9616aa47b59d6f843db" # Ok!

# Loading data into data frame
# Processed data file for Cleveland should have 14 attributes. 
heart.data <- read.csv("./data/processed.cleveland.data", header = FALSE)
#Lets check if we have proper data
nrow(heart.data)
#[1] 303
ncol(heart.data)
#[1] 14
head(heart.data)
#   V1 V2 V3  V4  V5 V6 V7  V8 V9 V10 V11 V12 V13 V14
# 1 63  1  1 145 233  1  2 150  0 2.3   3 0.0 6.0   0
# 2 67  1  4 160 286  0  2 108  1 1.5   2 3.0 3.0   2

# Lets adjust names accordingly:
names(heart.data) <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num")

# Data preparation
#ca and thal have missing values indicated by “?” lets treat them properly
heart.data$ca[heart.data$ca == "?"] <- NA
heart.data$thal[heart.data$thal == "?"] <- NA

# And also lets fix variable types:
heart.data$sex <- factor(heart.data$sex)
levels(heart.data$sex) <- c("female", "male")
heart.data$cp <- factor(heart.data$cp)
levels(heart.data$cp) <- c("typical","atypical","non-anginal","asymptomatic")
heart.data$fbs <- factor(heart.data$fbs)
levels(heart.data$fbs) <- c("false", "true")
heart.data$restecg <- factor(heart.data$restecg)
levels(heart.data$restecg) <- c("normal","stt","hypertrophy")
heart.data$exang <- factor(heart.data$exang)
levels(heart.data$exang) <- c("no","yes")
heart.data$slope <- factor(heart.data$slope)
levels(heart.data$slope) <- c("upsloping","flat","downsloping")
heart.data$ca <- factor(heart.data$ca) # not doing level conversion because its not necessary
heart.data$thal <- factor(heart.data$thal)
levels(heart.data$thal) <- c("normal","fixed","reversable")
heart.data$num <- factor(heart.data$num) # not doing level conversion because its not necessary

#Summary of prepared data:
summary(heart.data)
#      age            sex                 cp         trestbps          chol          fbs             restecg   
# Min.   :29.00   female: 97   typical     : 23   Min.   : 94.0   Min.   :126.0   false:258   normal     :151  
# 1st Qu.:48.00   male  :206   atypical    : 50   1st Qu.:120.0   1st Qu.:211.0   true : 45   stt        :  4  
# Median :56.00                non-anginal : 86   Median :130.0   Median :241.0               hypertrophy:148  
# Mean   :54.44                asymptomatic:144   Mean   :131.7   Mean   :246.7                                
# 3rd Qu.:61.00                                   3rd Qu.:140.0   3rd Qu.:275.0                                
# Max.   :77.00                                   Max.   :200.0   Max.   :564.0                                
#    thalach      exang        oldpeak             slope        ca              thal     num    
# Min.   : 71.0   no :204   Min.   :0.00   upsloping  :142   0.0 :176   normal    :166   0:164  
# 1st Qu.:133.5   yes: 99   1st Qu.:0.00   flat       :140   1.0 : 65   fixed     : 18   1: 55  
# Median :153.0             Median :0.80   downsloping: 21   2.0 : 38   reversable:117   2: 36  
# Mean   :149.6             Mean   :1.04                     3.0 : 20   NA's      :  2   3: 35  
# 3rd Qu.:166.0             3rd Qu.:1.60                     NA's:  4                    4: 13  
# Max.   :202.0             Max.   :6.20

# Selecting data
# Results which “0” and “1” are related to possibility of diameter narrowing. 
# Lets select only data ralated to those two results.
heart.data <- heart.data[heart.data$num == "0" | heart.data$num == "1", ]

#Checking again if we have proper data
nrow(heart.data)
#[1] 219
ncol(heart.data)
#[1] 14
head(heart.data)
#   age    sex           cp trestbps chol   fbs     restecg thalach exang oldpeak       slope  ca       thal num
# 1  63   male      typical      145  233  true hypertrophy     150    no     2.3 downsloping 0.0      fixed   0
# 3  67   male asymptomatic      120  229 false hypertrophy     129   yes     2.6        flat 2.0 reversable   1
# 4  37   male  non-anginal      130  250 false      normal     187    no     3.5 downsloping 0.0     normal   0

# Classification tree
#will use rpart package for classification tree.
library(rpart)

#Growing the tree
heart.tree <- rpart(num ~ age + sex + cp + trestbps + chol + fbs + restecg + 
                      thalach + exang + oldpeak + slope + ca + thal,
                    method = "class", 
                    data = heart.data)

#Plotting the tree
library(rpart.plot)
prp(heart.tree, extra = 100)

#Results
# Picking three best attributes that can predict possible diameter narrowing one would select: thal, cp and age.

sessionInfo()
