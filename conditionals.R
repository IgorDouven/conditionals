library(haven)
library(lme4)
library(optimx)
library(sjstats)
library(rcompanion)
library(pROC)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ordinal)
library(psych)

##################
## EXPERIMENT 1 ##
##################

data <- read_sav("HIT.sav")

# time spent on survey, demographics, ...
describe(c(data$Duration__in_seconds_))
table(data$gender)
describe(as.numeric(c(data$age)))

# select columns with relevant responses
truth <- data[,c(13:24)]
follows <- data[,c(32:43)]

# histogram of responses to truth questions
qplot(c(as.matrix(truth)),
      geom="histogram",
      binwidth = .5,
      main = "Truth", 
      xlab = "Response option",
      ylab = "Count",
      fill=I("thistle4"), 
      col=I("thistle4"), 
      alpha=I(.15)) + 
  scale_x_continuous(breaks=c(0,.5,1), labels=c("False", "Neither/nor", "True")) +
  theme_light() +
  theme(axis.text = element_text(size=11, family="Avenir Book"), 
        axis.title = element_text(size=14, family="Avenir Book"),
        plot.title = element_text(size=16, family="Avenir Book"))

# histogram of responses to inferential strenght questions
qplot(c(as.matrix(follows)),
      geom="histogram",
      binwidth = 1,
      main = "Inferential strength", 
      xlab = "Response option",
      ylab = "Count",
      fill=I("thistle4"), 
      col=I("thistle4"), 
      alpha=I(.2)) +
  scale_x_continuous(breaks=c(1:7), labels=c(1:7)) +
  theme_light() +
  theme(axis.text = element_text(size=11, family="Avenir Book"), 
        axis.title = element_text(size=14, family="Avenir Book"),
        plot.title = element_text(size=16, family="Avenir Book"))

df0 <- data.frame(truth = c(as.matrix(truth)), follows = c(as.matrix(follows)))
df0 <- df0[complete.cases(df0), ]

df0 %>% filter(truth == 1.0) -> dfT
df0 %>% filter(truth == 0.5) -> dfNN
df0 %>% filter(truth == 0.0) -> dfF

qplot(dfT$follows,
      geom="histogram",
      binwidth = 1,
      main = "Inferential strength: True", 
      xlab = "Response option",
      ylab = "Count",
      fill=I("thistle4"), 
      col=I("thistle4"), 
      alpha=I(.2)) +
  scale_x_continuous(breaks=c(1:7), labels=c(1:7)) +
  theme_light() +
  theme(axis.text = element_text(size=11, family="Avenir Book"), 
        axis.title = element_text(size=14, family="Avenir Book"),
        plot.title = element_text(size=16, family="Avenir Book"))

qplot(dfNN$follows,
      geom="histogram",
      binwidth = 1,
      main = "Inferential strength: Neither/nor", 
      xlab = "Response option",
      ylab = "Count",
      fill=I("thistle4"), 
      col=I("thistle4"), 
      alpha=I(.2)) +
  scale_x_continuous(breaks=c(1:7), labels=c(1:7)) +
  theme_light() +
  theme(axis.text = element_text(size=11, family="Avenir Book"), 
        axis.title = element_text(size=14, family="Avenir Book"),
        plot.title = element_text(size=16, family="Avenir Book"))

qplot(dfF$follows,
      geom="histogram",
      binwidth = 1,
      main = "Inferential strength: False", 
      xlab = "Response option",
      ylab = "Count",
      fill=I("thistle4"), 
      col=I("thistle4"), 
      alpha=I(.2)) +
  scale_x_continuous(breaks=c(1:7), labels=c(1:7)) +
  theme_light() +
  theme(axis.text = element_text(size=11, family="Avenir Book"), 
        axis.title = element_text(size=14, family="Avenir Book"),
        plot.title = element_text(size=16, family="Avenir Book"))

# true versus not true (so both false and neither/nor)
trF <- floor(c(t(as.matrix(truth))))
flw <- c(t(as.matrix(follows)))
item <- as.factor(rep(1:12, nrow(data)))
id <- as.factor(rep(1:nrow(data), each = 12))

# add CRT score as covariate (turns out to be useless)
crt0 <- data$BatNballCorrect + data$lilypadCorrect + data$widgetsCorrect
crt <- rep(crt0, each = 12)

dfF <- data.frame(truth = trF, inference_strength = flw, crt = crt, item = item, id = id)
dfF <- dfF[complete.cases(dfF),]

mm1 <- glmer(truth ~ inference_strength + (1 + inference_strength | item) + (1 + inference_strength | id), 
             data = dfF, family = binomial, 
             control = glmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

summary(mm1)

# Tjur's coefficient of determination
cod(mm1) # .87, which counts as very high

m.null1 <- glm(truth ~ 1, data = dfF, family = binomial)

nagelkerke(mm1, m.null1) # .86, which counts as very high

t_respF <- as.numeric(predict(mm1, type = "response") > .5)
mean(t_respF == dfF$truth) # 96 percent classified correctly 
auc(roc(dfF$truth, t_respF)) # AUROC = .96, which is very high

# the model with crt score added
mm1crt <- glmer(truth ~ inference_strength + crt + (1 + inference_strength | item) + (1 + inference_strength | id), 
             data = dfF, family = binomial, 
             control = glmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

summary(mm1crt)

# not-false (so both true and neither/nor) versus false
trC <- ceiling(c(t(as.matrix(truth))))

dfC <- data.frame(truth = trC, inference_strength = flw, item = item, id = id)
dfC <- dfC[complete.cases(dfC),]

mm2 <- glmer(truth ~ inference_strength + (1 + inference_strength | item) + (1 + inference_strength | id), 
             data = dfC, family = binomial, 
             control = glmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

summary(mm2)

# Tjur's coefficient of determination
cod(mm2) # .67, which counts as high

m.null2 <- glm(truth ~ 1, data = dfC, family = binomial)

nagelkerke(mm2, m.null2) # .7, which counts as high

t_respC <- as.numeric(predict(mm2, type = "response") > .5)
mean(t_respC == dfC$truth) # 88 percent classified correctly 
auc(roc(dfC$truth, t_respC)) # AUROC = .88, which is high

# if inferentialism holds, truth evaluation should also be a good predictor of inferential strength evaluation in that 'true' judgments
# predict judgments of greater inferential strength, 'false' judgments predict lesser inferential strength, and 'neither/nor' judgments
# predict something in between. the histograms already give reason to believe that this is so, but they're at the aggregate level. we
# can use the 'ordinal' package to fit ordered regression models.

tr1 <- c(t(as.matrix(truth)))
df1 <- data.frame(truth = as.factor(tr1), inference_strength = as.factor(flw))
df1 <- df1[complete.cases(df1),]

m_ord0 <- clm(inference_strength ~ 1, data = df1)
m_ord <- clm(inference_strength ~ truth, data = df1)
summary(m_ord)
anova(m_ord0, m_ord)
confint(m_ord, type = "Wald")

# McFadden's pseudo-R^2
as.vector(1 - (logLik(m_ord)/logLik(m_ord0))) # .3, which counts as excellent model fit

# we can also treat 'true / neither/nor / false' as an ordinal variable, interpreting 'neither/nor' as holding the middle
# between 'true' and 'false'. then we can use ordinal regression to predict truth judgment on the basis of
# inferential strength.

m_ordT0 <- clm(truth ~ 1, data = df1)
m_ordT <- clm(truth ~ inference_strength, data = df1)
summary(m_ordT)

anova(m_ordT0, m_ordT)
confint(m_ordT, type = "Wald")

# McFadden's pseudo-R^2
as.vector(1 - (logLik(m_ordT)/logLik(m_ordT0))) # .55, which counts as *excellent* model fit

## graphics

# true vs not-true
cfs <- unname(fixef(mm1))

ind.fnc <- function(i) sapply(seq(0.5, 7.5, by=.05), function(x) 1/(1 + exp(-((cfs[1] + lme4::ranef(mm1)$id[i,1]) + (cfs[2] + lme4::ranef(mm1)$id[i,2])*x))))
ind.m <- matrix(nrow = nrow(data), ncol = 141, NA)
for (i in 1:nrow(data)) ind.m[i,] <- ind.fnc(i)

mdf <- t(rbind(seq(0.5, 7.5, by=.05), ind.m))
colnames(mdf) <- c("Xval", seq(1, nrow(data)))
rownames(mdf) <- seq(0.5, 7.5, by=.05)
mdf <- data.frame(mdf)

mdf %>% gather(key, Yval, 2:(nrow(data) + 1)) %>% ggplot(aes(x=Xval, y=Yval, group=key)) + 
  geom_line(alpha=.25, color="thistle4") + ggtitle("True vs. not true") +
  xlab("Inferential strength") + ylab("Probability of truth judgment") +
  theme_minimal() + stat_function(fun=function(x) 1/(1 + exp(-(cfs[1] + cfs[2]*x))), color="grey20", size=.9, n=1000) +
  theme(axis.text = element_text(size=11, family="Avenir Book"), 
        axis.title = element_text(size=14, family="Avenir Book"),
        plot.title = element_text(size=16, family="Avenir Book"))

# not-false vs false
cfs2 <- unname(fixef(mm2))

ind.fnc2 <- function(i) sapply(seq(-0.5, 6.5, by=.05), function(x) 1/(1 + exp(-((cfs2[1] + lme4::ranef(mm2)$id[i,1]) + (cfs2[2] + lme4::ranef(mm2)$id[i,2])*x))))
ind.m2 <- matrix(nrow = nrow(data), ncol = 141, NA)
for (i in 1:nrow(data)) ind.m2[i,] <- ind.fnc2(i)

mdf2 <- t(rbind(seq(-0.5, 6.5, by=.05), ind.m2))
colnames(mdf2) <- c("Xval", seq(1, nrow(data)))
rownames(mdf2) <- seq(-0.5, 6.5, by=.05)
mdf2 <- data.frame(mdf2)

mdf2 %>% gather(key, Yval, 2:(nrow(data) + 1)) %>% ggplot(aes(x=Xval, y=Yval, group=key)) + 
  geom_line(alpha=.25, color="thistle4") + ggtitle("Not-false vs. false") +
  xlab("Inferential strength") + ylab("Probability of truth judgment") +
  theme_minimal() + stat_function(fun=function(x) 1/(1 + exp(-(cfs2[1] + cfs2[2]*x))), color="grey20", size=.9, n=1000) +
  theme(axis.text = element_text(size=12, family="Avenir Book"), 
        axis.title = element_text(size=14, family="Avenir Book"),
        plot.title = element_text(size=16, family="Avenir Book"))

##################
## EXPERIMENT 2 ##
##################

data2 <- read_sav("HIT2.sav")

# time spent on survey, demographics, ...
describe(c(data2$Duration__in_seconds_))
table(data2$gender)
table(data2$edu)
describe(as.numeric(c(data2$age)))

# select columns with relevant responses
truth <- data2[,c(12:23)]
ant <- data2[,c(31:42)]
cons <- data2[,c(43:54)]

# histogram of responses to truth questions
qplot(c(as.matrix(truth)),
      geom="histogram",
      binwidth = .5,
      main = "Truth", 
      xlab = "Response option",
      ylab = "Count",
      fill=I("thistle4"), 
      col=I("thistle4"), 
      alpha=I(.15)) + 
  scale_x_continuous(breaks=c(0,.5,1), labels=c("False", "Neither/nor", "True")) +
  theme_light() +
  theme(axis.text = element_text(size=11, family="Avenir Book"), 
        axis.title = element_text(size=14, family="Avenir Book"),
        plot.title = element_text(size=16, family="Avenir Book"))

# histogram of responses to antecedent believability questions
qplot(c(as.matrix(ant)),
      geom="histogram",
      binwidth = 1,
      main = "Believability of antecedent", 
      xlab = "Response option",
      ylab = "Count",
      fill=I("thistle4"), 
      col=I("thistle4"), 
      alpha=I(.2)) +
  scale_x_continuous(breaks=c(1:7), labels=c(1:7)) +
  theme_light() +
  theme(axis.text = element_text(size=11, family="Avenir Book"), 
        axis.title = element_text(size=14, family="Avenir Book"),
        plot.title = element_text(size=16, family="Avenir Book"))

# histogram of responses to consequent believability questions
qplot(c(as.matrix(cons)),
      geom="histogram",
      binwidth = 1,
      main = "Believability of consequent", 
      xlab = "Response option",
      ylab = "Count",
      fill=I("thistle4"), 
      col=I("thistle4"), 
      alpha=I(.2)) +
  scale_x_continuous(breaks=c(1:7), labels=c(1:7)) +
  theme_light() +
  theme(axis.text = element_text(size=11, family="Avenir Book"), 
        axis.title = element_text(size=14, family="Avenir Book"),
        plot.title = element_text(size=16, family="Avenir Book"))

df0 <- data.frame(truth = c(as.matrix(truth)), antecedent = c(as.matrix(ant)), consequent = c(as.matrix(cons)))
df0 <- df0[complete.cases(df0), ]

df0 %>% filter(truth == 1.0) -> dfT
df0 %>% filter(truth == 0.5) -> dfNN
df0 %>% filter(truth == 0.0) -> dfF

qplot(dfT$antecedent,
      geom="histogram",
      binwidth = 1,
      main = "Believability of antecedent: True", 
      xlab = "Response option",
      ylab = "Count",
      fill=I("thistle4"), 
      col=I("thistle4"), 
      alpha=I(.2)) +
  scale_x_continuous(breaks=c(1:7), labels=c(1:7)) +
  theme_light() +
  theme(axis.text = element_text(size=11, family="Avenir Book"), 
        axis.title = element_text(size=14, family="Avenir Book"),
        plot.title = element_text(size=16, family="Avenir Book"))

qplot(dfNN$antecedent,
      geom="histogram",
      binwidth = 1,
      main = "Believability of antecedent: Neither/nor", 
      xlab = "Response option",
      ylab = "Count",
      fill=I("thistle4"), 
      col=I("thistle4"), 
      alpha=I(.2)) +
  scale_x_continuous(breaks=c(1:7), labels=c(1:7)) +
  theme_light() +
  theme(axis.text = element_text(size=11, family="Avenir Book"), 
        axis.title = element_text(size=14, family="Avenir Book"),
        plot.title = element_text(size=16, family="Avenir Book"))

qplot(dfF$antecedent,
      geom="histogram",
      binwidth = 1,
      main = "Believability of antecedent: False", 
      xlab = "Response option",
      ylab = "Count",
      fill=I("thistle4"), 
      col=I("thistle4"), 
      alpha=I(.2)) +
  scale_x_continuous(breaks=c(1:7), labels=c(1:7)) +
  theme_light() +
  theme(axis.text = element_text(size=11, family="Avenir Book"), 
        axis.title = element_text(size=14, family="Avenir Book"),
        plot.title = element_text(size=16, family="Avenir Book"))

qplot(dfT$consequent,
      geom="histogram",
      binwidth = 1,
      main = "Believability of consequent: True", 
      xlab = "Response option",
      ylab = "Count",
      fill=I("thistle4"), 
      col=I("thistle4"), 
      alpha=I(.2)) +
  scale_x_continuous(breaks=c(1:7), labels=c(1:7)) +
  theme_light() +
  theme(axis.text = element_text(size=11, family="Avenir Book"), 
        axis.title = element_text(size=14, family="Avenir Book"),
        plot.title = element_text(size=16, family="Avenir Book"))

qplot(dfNN$consequent,
      geom="histogram",
      binwidth = 1,
      main = "Believability of consequent: Neither/nor", 
      xlab = "Response option",
      ylab = "Count",
      fill=I("thistle4"), 
      col=I("thistle4"), 
      alpha=I(.2)) +
  scale_x_continuous(breaks=c(1:7), labels=c(1:7)) +
  theme_light() +
  theme(axis.text = element_text(size=11, family="Avenir Book"), 
        axis.title = element_text(size=14, family="Avenir Book"),
        plot.title = element_text(size=16, family="Avenir Book"))

qplot(dfF$consequent,
      geom="histogram",
      binwidth = 1,
      main = "Believability of consequent: False", 
      xlab = "Response option",
      ylab = "Count",
      fill=I("thistle4"), 
      col=I("thistle4"), 
      alpha=I(.2)) +
  scale_x_continuous(breaks=c(1:7), labels=c(1:7)) +
  theme_light() +
  theme(axis.text = element_text(size=11, family="Avenir Book"), 
        axis.title = element_text(size=14, family="Avenir Book"),
        plot.title = element_text(size=16, family="Avenir Book"))

# true versus not true (so both false and neither/nor)
trF <- floor(c(t(as.matrix(truth))))
antcd <- c(t(as.matrix(ant)))
cnsqt <- c(t(as.matrix(cons)))
item <- as.factor(rep(1:12, nrow(data2)))
id <- as.factor(rep(1:nrow(data2), each = 12))

dfF <- data.frame(truth = trF, antecedent = antcd, consequent = cnsqt, item = item, id = id)
dfF <- dfF[complete.cases(dfF),]

mm1 <- glmer(truth ~ antecedent + consequent + (1 + antecedent + consequent | item) + (1 + antecedent + consequent | id), 
             data = dfF, family = binomial, 
             control = glmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

summary(mm1)

# Tjur's coefficient of determination
cod(mm1) # .50

m.null1 <- glm(truth ~ 1, data = dfF, family = binomial)

nagelkerke(mm1, m.null1) # .54

t_respF <- as.numeric(predict(mm1, type = "response") > .5)
mean(t_respF == dfF$truth) # 86 percent classified correctly 
auc(roc(dfF$truth, t_respF)) # AUROC = .84

# graphics
mm01 <- glmer(truth ~ consequent + (1 + consequent | item) + (1 + consequent | id), 
             data = dfF, family = binomial, 
             control = glmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

cfs <- unname(fixef(mm01))

ind.fnc <- function(i) sapply(seq(0, 7.5, by=.05), function(x) 1/(1 + exp(-((cfs[1] + lme4::ranef(mm01)$id[i,1]) + (cfs[2] + lme4::ranef(mm01)$id[i,2])*x))))
ind.m <- matrix(nrow = nrow(data2), ncol = 151, NA)
for (i in 1:nrow(data2)) ind.m[i,] <- ind.fnc(i)

mdf <- t(rbind(seq(0, 7.5, by=.05), ind.m))
colnames(mdf) <- c("Xval", seq(1, nrow(data2)))
rownames(mdf) <- seq(0, 7.5, by=.05)
mdf <- data.frame(mdf)

mdf %>% gather(key, Yval, 2:(nrow(data2) + 1)) %>% ggplot(aes(x=Xval, y=Yval, group=key)) + 
  geom_line(alpha=.25, color="thistle4") + ggtitle("True vs. not true") +
  xlab("Believability of consequent") + ylab("Probability of truth judgment") +
  theme_minimal() + stat_function(fun=function(x) 1/(1 + exp(-(cfs[1] + cfs[2]*x))), color="grey20", size=.9, n=1000) +
  theme(axis.text = element_text(size=11, family="Avenir Book"), 
        axis.title = element_text(size=14, family="Avenir Book"),
        plot.title = element_text(size=16, family="Avenir Book"))
