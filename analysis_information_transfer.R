##R code for analysing data

#You first need to manually set working directory to folder with data.
setwd("/data")

#Open .csv files
entropy_colour_symbol <- read.csv(file="H_C_S.csv", header=TRUE, sep=",")
entropy_symbol_colour <- read.csv(file="H_S_C.csv", header=TRUE, sep=",")
hamming <- read.csv(file="hamming.csv", header=TRUE, sep=",")
accuracy <- read.csv(file="success.csv", header=TRUE, sep=",")

#Provide reference levels for mixed effect models
entropy_colour_symbol$condition <- relevel(entropy_colour_symbol$condition, ref="Recall")
entropy_symbol_colour$condition <- relevel(entropy_symbol_colour$condition, ref="Recall")
hamming$condition <- relevel(hamming$condition, ref="Recall")
accuracy$condition <- relevel(accuracy$condition, ref="Correspondence")

#Installs lmerTest package if it is not already present.
if (!require("lmerTest","dplyr")) {
  install.packages("lmerTest")
  install.packages("dplyr")
  library(lmerTest)
  library(dplyr)
}

#Filtering and providing reference levels
c_s_synchronous <- filter(entropy_colour_symbol, condition=="Dialogue" | condition=="Recall")
c_s_synchronous$condition <- relevel(c_s_synchronous$condition, ref="Recall")
c_s_asynchronous <- filter(entropy_colour_symbol, condition=="Correspondence" | condition=="Mnemonic")
c_s_asynchronous$condition <- relevel(c_s_asynchronous$condition, ref="Mnemonic")

s_c_synchronous <- filter(entropy_symbol_colour, condition=="Dialogue" | condition=="Recall")
s_c_asynchronous <- filter(entropy_symbol_colour, condition=="Correspondence" | condition=="Mnemonic")
s_c_synchronous$condition <- relevel(s_c_synchronous$condition, ref="Recall")
s_c_asynchronous$condition <- relevel(s_c_asynchronous$condition, ref="Mnemonic")

hamm_synchronous <- filter(hamming, condition=="Dialogue" | condition=="Recall")
hamm_asynchronous <- filter(hamming, condition=="Correspondence" | condition=="Mnemonic")
hamm_synchronous$condition <- relevel(hamm_synchronous$condition, ref="Recall")
hamm_asynchronous$condition <- relevel(hamm_asynchronous$condition, ref="Mnemonic")

acc_synchronous <- filter(accuracy, condition=="Dialogue" | condition=="Recall")
acc_asynchronous <- filter(accuracy, condition=="Correspondence" | condition=="Mnemonic")
acc_synchronous$condition <- relevel(acc_synchronous$condition, ref="Dialogue")
acc_asynchronous$condition <- relevel(acc_asynchronous$condition, ref="Correspondence")

#Mixed Effect models
model1_1 <- lmer(c.ent.norm~condition*block+(1*block|participant)+(1*condition*block|symbol), REML=T, data=c_s_synchronous)
model1_2 <- lmer(c.ent.norm~condition*block+(1*block|participant)+(1*condition*block|symbol), REML=T, data=c_s_asynchronous)

model2_1 <- lmer(ent_norm~condition*block+(1*block|participant)+(1*condition*block|category), REML=T, data=s_c_synchronous)
model2_2 <- lmer(ent_norm~condition*block+(1*block|participant)+(1*condition*block|category), REML=T, data=s_c_asynchronous)

model3_1 <- lmer(error~condition*block+(1*block|participant)+(1*condition*block|category/colour), REML=T, data=hamm_synchronous)
model3_2 <- lmer(error~condition*block+(1*block|participant)+(1*condition*block|category/colour), REML=T, data=hamm_asynchronous)

model4_1 <- glmer(response.new~condition*trial+(1*trial|participant)+(1*condition*trial|symbol)+(1*condition*trial|context), family=binomial(link="logit"), control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e4)), data=acc_synchronous)
model4_2 <- glmer(response.new~condition*trial+(1*trial|participant)+(1*condition*trial|symbol)+(1*condition*trial|context), family=binomial(link="logit"), control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e4)), data=acc_asynchronous)

#Summaries of each model
summary(model1_1)
summary(model1_2)
summary(model2_1)
summary(model2_2)
summary(model3_1)
summary(model3_2)
summary(model4_1)
summary(model4_2)