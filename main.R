library(ggplot2)
library(readr)
        
#read in data

data <- read_csv("path_to_file.csv")

#select males only

data <- subset(data, SEX == 1)

#convert categorical variables (physical activity and smoking status) to factors

data$RUPE <- as.factor(data$RUPE)
data$CIGSTAT <- as.factor(data$CIGSTAT)

#remove any individuals with relevant missing data for complete case analysis

data <- data[!(is.na(data$MDSPYR_1) | data$MDSPYR_1==""), ]
data <- data[!(is.na(data$ALT_HEIGHT_1HC) | data$ALT_HEIGHT_1HC==""), ]
data <- data[!(is.na(data$ALT_WEIGHT_1HC) | data$ALT_WEIGHT_1HC==""), ]
data <- data[!(is.na(data$RUPE) | data$RUPE==""), ]
data <- data[!(is.na(data$TOWNINDX) | data$TOWNINDX==""), ]
data <- data[!(is.na(data$FFQ1_NUT064) | data$FFQ1_NUT064==""), ]
data <- data[!(is.na(data$CIGSTAT) | data$CIGSTAT==""), ]
data <- data[!(is.na(data$UNITSNOW) | data$UNITSNOW==""), ]

#main incidence logistic regression model, calculate betas and confidence intervals

m1 <- glm(INC_PROSTATE ~ MDSPYR_1 + DOC_1HC + AGE + FFQ1_NUT064 + TOWNINDX + RUPE + ALT_HEIGHT_1HC + ALT_WEIGHT_1HC + CIGSTAT + UNITSNOW, family = binomial(link="logit"), data = data)

summary(m1)
exp(coef(m1))  
exp(confint(m1))

#main mortality logistic regression model

m2 <- glm(PROSTATE_UMORT ~ MDSPYR_1 + DOC_1HC + AGE + FFQ1_NUT064 + TOWNINDX + ALT_HEIGHT_1HC + ALT_WEIGHT_1HC + CIGSTAT + UNITSNOW, family = binomial(link="logit"), data = data)

summary(m2)
exp(coef(m2))  
exp(confint(m2))

#categorise Gleason scores, if Gleason = 4+3 = aggressive, if 3+4 = localised, label aggressive 4+3 or >=7 as 1 and 3+4 or <6 as 0

data$severity1 <- ifelse(data$GLEASON1 == 4 & dataSGLEASON2 == 3, 1, 0)
data$severity2 <- ifelse(data$GLEASONS > 7, 1, 0)
data$severity_aggressive <- data$severity1 + data$severity2

data$severity1 <- ifelse(data$GLEASON1== 3 & data§GLEASON2 == 4, 1, 0)
data$severity2 <- ifelse(data$GLEASONS < 7, 1, 0)
data$severity_localised <- data$severity1 + data$severity2

#aggressive cases model

controls <- subset(data, INC_PROSTATE == 0)
cases <- subset(data, severity_aggressive == 1)
data_aggressive <- rbind(controls, cases)

m3 <- glm(INC_PROSTATE ~ MDSPYR_1 + DOC_1HC + AGE + FFQ1_NUT064 + TOWNINDX + ALT_HEIGHT_1HC + ALT_WEIGHT_1HC + CIGSTAT + UNITSNOW, family = binomial(link="logit"), data = data_aggressive)

summary(m3)
exp(coef(m3))  
exp(confint(m3))

#localised cases model

cases <- subset(data, severity_localised == 1)
data_localised <- rbind(controls, cases)

m4 <- glm(INC_PROSTATE ~ MDSPYR_1 + DOC_1HC + AGE + FFQ1_NUT064 + TOWNINDX + ALT_HEIGHT_1HC + ALT_WEIGHT_1HC + CIGSTAT + UNITSNOW, family = binomial(link="logit"), data = data_localised)

summary(m4)
exp(coef(m4))  
exp(confint(m4))

#figure 1, categorise participants by age group

data["Age_Group"] = cut(data$AGE, c(35, 44, 54, 64, 74, Inf), c("35-44", "45-54", "55-64", "65-74", "75+"), include.lowest=TRUE)

ggplot(data, aes(x = Age_Group, y = MDSPYR_1, colour = Age_Group)) +
geom_boxplot() +
theme bw() +
geom_jitter () +
geom_point (alpha = 0.1) +
ylab ("PyrMDS score") + 
xlab("Age group") +
labs(color = "Age group") +
annotate ("text",
x = 1:length(table(data$Age_Group)),
y = aggregate (MDSPYR_1 ~ Age_Group, data, median)[, 2],
label = table(data$Age_group))

#summary of missing data

table(is.na(data))

#filter out cases without Gleason score data, after excluding missing exposure and covariates, find those who had a Gleason score but their INC_PROSTATE was recorded as 0

data$INC_PROSTATE[!is.na(data$GLEASONS)] <- 1
