# Libraries
# Data manipulation and visualization
library(tidyverse)
library(haven)
library(lattice)
library(car)
library(sjPlot)
# Missing data
library(VIM)
library(missForest)
# Decision tree
library(rpart)
library(rpart.plot)
# Classification task
library(UBL)
library(randomForest)
library(pROC)
library(pdp)
# Multilevel models
library(lme4)
library(lmerTest)
library(performance)


# Select German samples from PISA 2015 and 2018
# PISA 2015, Student questionnaire SPSS http://www.oecd.org/pisa/data/2015database/
alldata15 <- read_sav("CY6_MS_CMB_STU_QQQ.sav")
DEUdata15 <- alldata15[alldata15$CNT == "DEU", ]
rm(alldata15)
# PISA 2018, Student questionnaire SPSS http://www.oecd.org/pisa/data/2018database/
alldata18 <- read_sav("CY07_MSU_STU_QQQ.sav")
DEUdata18 <- alldata18[alldata18$CNT == "DEU", ]
rm(alldata18)

# Select variables: demographics, ICT, maths, science, and weights
# dataA is the initial dataset with our variables of interest selected from German data
dataA15 <- dplyr::select(
  DEUdata15,
  c(
    "CNTSCHID",
    "ST004D01T",
    "ESCS",
    "INTICT",
    "COMPICT",
    "AUTICT",
    "SOIAICT",
    intersect(starts_with("PV"), ends_with("MATH")),
    intersect(starts_with("PV"), ends_with("SCIE")),
    starts_with("W_FS")
  )
)
# the same for 2018
dataA18 <- dplyr::select(
  DEUdata18,
  c(
    "CNTSCHID",
    "ST004D01T",
    "ESCS",
    "INTICT",
    "COMPICT",
    "AUTICT",
    "SOIAICT",
    intersect(starts_with("PV"), ends_with("MATH")),
    intersect(starts_with("PV"), ends_with("SCIE")),
    starts_with("W_FS")
  )
)
# Change column names
dataA15 <-
  rename(
    dataA15,
    "SCHL" = "CNTSCHID",
    "GEND" = "ST004D01T",
    "INTE" = "INTICT",
    "COMP" = "COMPICT",
    "AUTO" = "AUTICT",
    "SOCI" = "SOIAICT"
  )
dataA18 <-
  rename(
    dataA18,
    "SCHL" = "CNTSCHID",
    "GEND" = "ST004D01T",
    "INTE" = "INTICT",
    "COMP" = "COMPICT",
    "AUTO" = "AUTICT",
    "SOCI" = "SOIAICT"
  )
# Remove haven labels
dataA15 <- as.data.frame(haven::zap_labels(dataA15))
dataA18 <- as.data.frame(haven::zap_labels(dataA18))

## Missing Data
# Find and remove rows with 100% ICT attitudes missing
NArowsICT15 <-
  rowSums(is.na(subset(dataA15, select = c(
    INTE, COMP, AUTO, SOCI
  )))) / ncol(subset(dataA15, select = c(INTE, COMP, AUTO, SOCI)))
sum(NArowsICT15 == 1) / nrow(dataA15) # percentage of rows we want to remove
# the same for 2018
NArowsICT18 <-
  rowSums(is.na(subset(dataA18, select = c(
    INTE, COMP, AUTO, SOCI
  )))) / ncol(subset(dataA18, select = c(INTE, COMP, AUTO, SOCI)))
sum(NArowsICT18 == 1) / nrow(dataA18) # percentage of rows we want to remove

# dataB is the dataset for analysis, with 100% missingness in ICT removed
dataB15 <- dataA15[NArowsICT15 < 1,] # 2015
dataB18 <- dataA18[NArowsICT18 < 1,] # 2018

# Compare removed and left rows
dataRem15 <- dataA15[NArowsICT15 == 1,] # for 2015
nrow(dataRem15[dataRem15$GEND == 1, ])# female
nrow(dataRem15[dataRem15$GEND == 2, ])# male
hist(
  dataRem15$ESCS,
  ylim = c(0, 1000),
  col = rgb(0.5, 0.2, 0, 0.5),
  main = NULL,
  xlab = "ESCS"
) #ESCS missing
hist(
  dataB15$ESCS,
  ylim = c(0, 1000),
  col = rgb(0.6, 0.2, 0, 0.25),
  add = T
) # ESCS remaining
NstudMiss <-
  apply(as.data.frame(unique(dataRem15$SCHL)), 1, function(i) {
    sum(nrow(dataRem15[dataRem15$SCHL == i,]))
  }) # per school
min(NstudMiss)
max(NstudMiss)
# the same for 2018
dataRem18 <- dataA18[NArowsICT18 == 1,]
nrow(dataRem18[dataRem18$GEND == 1, ])# female
nrow(dataRem18[dataRem18$GEND == 2, ])# male
hist(
  dataRem18$ESCS,
  ylim = c(0, 1000),
  col = rgb(0.5, 0.2, 0, 0.5),
  main = NULL,
  xlab = "ESCS"
) # ESCS missing
hist(
  dataB18$ESCS,
  ylim = c(0, 1000),
  col = rgb(0.6, 0.2, 0, 0.25),
  add = T
) # ESCS remaining
NstudMiss <-
  apply(as.data.frame(unique(dataRem18$SCHL)), 1, function(i) {
    sum(nrow(dataRem18[dataRem18$SCHL == i,]))
  }) # per school
min(NstudMiss)
max(NstudMiss)

# Missingness per variable for 2015
NAvar15 <- sapply(dataB15, function(y)
  sum(length(which(is.na(
    y
  )))) / nrow(dataB15))
# the same for 2018
NAvar18 <- sapply(dataB18, function(y)
  sum(length(which(is.na(
    y
  )))) / nrow(dataB18))

# Descriptives for 2015
length(unique(dataB15$SCHL)) # N schools
nrow(dataB15[dataB15$GEND == 1, ]) / nrow(dataB15) * 100 # female students
nrow(dataB15[dataB15$GEND == 2, ]) / nrow(dataB15) * 100 # male students
# Descriptives for 2018
length(unique(dataB18$SCHL)) # N schools
nrow(dataB18[dataB18$GEND == 1, ]) / nrow(dataB18) * 100 # female students
nrow(dataB18[dataB18$GEND == 2, ]) / nrow(dataB18) * 100 # male students

# dataC is a susbset with missingness > 0 (ESCS & ICT), for imputation
dataC15 <- dataB15[, NAvar15 > 0]
mean(is.na(dataC15)) # percent of missing in the dataset
dataC18 <- dataB18[, NAvar18 > 0]
mean(is.na(dataC18)) # percent of missing in the dataset

# Aggregation plots
VIM::aggr(dataC15, cex.axis = 0.8)
title("2015")
VIM::aggr(dataC18, cex.axis = 0.8)
title("2018")
# Missingness matrices
VIM::scattmatrixMiss(dataC15)
VIM::scattmatrixMiss(dataC18)

# Impute with missForest
set.seed(100)
dataRF15 <- missForest(dataC15)
dataI15 <- dataRF15$ximp
# the same for 2018
set.seed(100)
dataRF18 <- missForest(dataC18)
dataI18 <- dataRF18$ximp

# Compare  histograms
# For 2015
C <- na.omit(dataC15)
I <- dataI15
par(mfrow = c(1, 5))
histfun <-
  function(i) {
    hist(
      C[, i],
      ylim = c(0, 1500),
      col = rgb(0, 0.2, 0, 0.5),
      main = NULL,
      xlab = print(paste(names(C[i])))
    )
    hist(
      I[, i],
      ylim = c(0, 1500),
      col = rgb(0.6, 1, 0, 0.25),
      add = T
    )
  }
lapply(c(1:5), histfun)

# for 2018
C <- na.omit(dataC18)
I <- dataI18
histfun <-
  function(i) {
    hist(
      C[, i],
      ylim = c(0, 1500),
      col = rgb(0, 0.2, 0, 0.5),
      main = NULL,
      xlab = print(paste(names(C[i])))
    )
    hist(
      I[, i],
      ylim = c(0, 1500),
      col = rgb(0.6, 1, 0, 0.25),
      add = T
    )
  }
lapply(c(1:5), histfun)

# Prepare the data for machine learning models
dataPV15 <-
  dplyr::select(dataB15, c("GEND", starts_with("PV")))
dataD15 <- cbind(dataI15, dataPV15)
# gender as factor recoded as 0 and 1
dataD15$GEND <- as.factor(car::recode(dataD15$GEND, "'2' = 1; '1' = 0"))
# levels of proficiency
dataD15$meansM <-
  rowMeans(subset(dataD15, select = c(PV1MATH:PV10MATH)))
dataD15$meansS <-
  rowMeans(subset(dataD15, select = c(PV1SCIE:PV10SCIE)))
dataD15$Sc <-
  cut(dataD15$meansS,
      c(0, 409.54, 633.33, 1000),
      labels = c("1", "2", "3"))
dataSc15 <-
  dplyr::select(dataD15, c("INTE", "COMP", "AUTO", "SOCI", "ESCS", "GEND", "Sc"))
dataD15$M <-
  cut(dataD15$meansM,
      c(0, 420.07, 606.99, 1000),
      labels = c("1", "2", "3"))
dataM15 <-
  dplyr::select(dataD15, c("INTE", "COMP", "AUTO", "SOCI", "ESCS", "GEND", "M"))
# the same for 2018
dataPV18 <-
  dplyr::select(dataB18, c("GEND", starts_with("PV")))
dataD18 <- cbind(dataI18, dataPV18)
# gender as factor recoded as 0 and 1
dataD18$GEND <- as.factor(car::recode(dataD18$GEND, "'2' = 1; '1' = 0"))
# levels of proficiency
dataD18$meansM <-
  rowMeans(subset(dataD18, select = c(PV1MATH:PV10MATH)))
dataD18$meansS <-
  rowMeans(subset(dataD18, select = c(PV1SCIE:PV10SCIE)))
dataD18$Sc <-
  cut(dataD18$meansS,
      c(0, 409.54, 633.33, 1000),
      labels = c("1", "2", "3"))
dataSc18 <-
  dplyr::select(dataD18, c("INTE", "COMP", "AUTO", "SOCI", "ESCS", "GEND", "Sc"))
dataD18$M <-
  cut(dataD18$meansM,
      c(0, 420.07, 606.99, 1000),
      labels = c("1", "2", "3"))
dataM18 <-
  dplyr::select(dataD18, c("INTE", "COMP", "AUTO", "SOCI", "ESCS", "GEND", "M"))

# Look at imbalanced classes
for (i in 1:3) {
  print(paste(nrow(dataM15[dataM15$M == i, ])))
}
for (i in 1:3) {
  print(paste(nrow(dataSc15[dataSc15$Sc == i, ])))
}
for (i in 1:3) {
  print(paste(nrow(dataM18[dataM18$M == i, ])))
}
for (i in 1:3) {
  print(paste(nrow(dataSc18[dataSc18$Sc == i, ])))
}

# Split 2015 into training set and test set
set.seed(100)
indSc15 <-
  sample(2, nrow(dataSc15), replace = TRUE, prob = c(.8, .2))
trainScU15 <- dataSc15[indSc15 == 1, ]
testSc15 <- dataSc15[indSc15 == 2, ]
set.seed(100)
indM15 <- sample(2, nrow(dataM15), replace = TRUE, prob = c(.8, .2))
trainMU15 <- dataM15[indM15 == 1, ]
testM15 <- dataM15[indM15 == 2, ]

# Oversample traning set (but not test set!)
trainSc15 <- RandOverClassif(form = Sc ~ ., dat = trainScU15)
trainM15 <- RandOverClassif(form = M ~ ., dat = trainMU15)

# Picture a tree
dev.off()
model <- rpart(M ~ ., data = trainM15)
rpart.plot(model, type = 0)

# Model RF
# Mathematics
set.seed(100)
rfM <- randomForest(M ~ ., data = trainM15, importance = T)
# Science
set.seed(100)
rfSc <- randomForest(Sc ~ ., data = trainSc15, importance = T)

# Importance plots
par(mfrow = c(1, 2))
varImpPlot(rfM,
           type = 1,
           scale = F,
           main = "Mathematics")
varImpPlot(rfSc,
           type = 1,
           scale = F,
           main = "Science")

# Evaluate the models
# Mathematics 2015, test set
fittedM15 <- predict(rfM, newdata = testM15)
# Mathematics 2018, the whole dataset
fittedM18 <- predict(rfM, newdata = dataM18)
# Science 2015, test set
fittedSc15 <- predict(rfSc, newdata = testSc15)
# Science 2018, the whole dataset
fittedSc18 <- predict(rfSc, newdata = dataSc18)

# Multiclass AUCs
aucM15 <-
  multiclass.roc(as.numeric(fittedM15), as.numeric(testM15$M), percent = T)
aucM18 <-
  multiclass.roc(as.numeric(fittedM18), as.numeric(dataM18$M), percent = T)
aucSc15 <-
  multiclass.roc(as.numeric(fittedSc15), as.numeric(testSc15$Sc), percent = T)
aucSc18 <-
  multiclass.roc(as.numeric(fittedSc18), as.numeric(dataSc18$Sc), percent = T)


# AUC plots, four in one
par(pty = "s")
layout(
  mat = matrix(c(1, 2, 3, 4, 5, 5, 5, 5), nrow = 2, byrow = TRUE),
  heights = c(0.4, 0.2)
)
# Mathematics 2015
plot(
  smooth(roc(
    as.numeric(fittedM15),
    as.numeric(testM15$M),
    levels = c("1", "3")
  )),
  xlim = c(1, 0),
  legacy.axes = T,
  xaxs = "i",
  yaxs = "i",
  lty = 1,
  xlab = "False Positive Rate",
  ylab = "True Positive Rate"
)
plot(
  smooth(roc(
    as.numeric(fittedM15),
    as.numeric(testM15$M),
    levels = c("2", "3")
  )),
  xlim = c(1, 0),
  legacy.axes = T,
  xaxs = "i",
  yaxs = "i",
  lty = 2,
  add = T
)
plot(
  smooth(roc(
    as.numeric(fittedM15),
    as.numeric(testM15$M),
    levels = c("1", "2")
  )),
  xlim = c(1, 0),
  legacy.axes = T,
  xaxs = "i",
  yaxs = "i",
  lty = 3,
  add = T
)
title(xlab = "Mathematics 2015", adj = 0.7, line = -6)
# Science 2015
plot(
  smooth(roc(
    as.numeric(fittedSc15),
    as.numeric(testSc15$Sc),
    levels = c("1", "3")
  )),
  xlim = c(1, 0),
  legacy.axes = T,
  xaxs = "i",
  yaxs = "i",
  lty = 1,
  xlab = "False Positive Rate",
  ylab = "True Positive Rate"
)
plot(
  smooth(roc(
    as.numeric(fittedSc15),
    as.numeric(testSc15$Sc),
    levels = c("2", "3")
  )),
  xlim = c(1, 0),
  legacy.axes = T,
  xaxs = "i",
  yaxs = "i",
  lty = 2,
  add = T
)
plot(
  smooth(roc(
    as.numeric(fittedSc15),
    as.numeric(testSc15$Sc),
    levels = c("1", "2")
  )),
  xlim = c(1, 0),
  legacy.axes = T,
  xaxs = "i",
  yaxs = "i",
  lty = 3,
  add = T
)
title(xlab = "Science 2015", adj = 0.7, line = -6)
# Mathematics 2018
plot(
  smooth(roc(
    as.numeric(fittedM18),
    as.numeric(dataM18$M),
    levels = c("1", "3")
  )),
  xlim = c(1, 0),
  legacy.axes = T,
  xaxs = "i",
  yaxs = "i",
  lty = 1,
  xlab = "False Positive Rate",
  ylab = "True Positive Rate"
)
plot(
  smooth(roc(
    as.numeric(fittedM18),
    as.numeric(dataM18$M),
    levels = c("2", "3")
  )),
  xlim = c(1, 0),
  legacy.axes = T,
  xaxs = "i",
  yaxs = "i",
  lty = 2,
  add = T
)
plot(
  smooth(roc(
    as.numeric(fittedM18),
    as.numeric(dataM18$M),
    levels = c("1", "2")
  )),
  xlim = c(1, 0),
  legacy.axes = T,
  xaxs = "i",
  yaxs = "i",
  lty = 3,
  add = T
)
title(xlab = "Mathematics 2018", adj = 0.7, line = -6)
# Science 2018
plot(
  smooth(roc(
    as.numeric(fittedSc18),
    as.numeric(dataSc18$Sc),
    levels = c("1", "3")
  )),
  xlim = c(1, 0),
  legacy.axes = T,
  xaxs = "i",
  yaxs = "i",
  lty = 1,
  xlab = "False Positive Rate",
  ylab = "True Positive Rate"
)
plot(
  smooth(roc(
    as.numeric(fittedSc18),
    as.numeric(dataSc18$Sc),
    levels = c("2", "3")
  )),
  xlim = c(1, 0),
  legacy.axes = T,
  xaxs = "i",
  yaxs = "i",
  lty = 2,
  add = T
)
plot(
  smooth(roc(
    as.numeric(fittedSc18),
    as.numeric(dataSc18$Sc),
    levels = c("1", "2")
  )),
  xlim = c(1, 0),
  legacy.axes = T,
  xaxs = "i",
  yaxs = "i",
  lty = 3,
  add = T
)
title(xlab = "Science 2018", adj = 0.7, line = -6)
# Legend
par(mar = c(0, 0, 0, 0))
plot(
  1,
  type = "n",
  axes = FALSE,
  xlab = "",
  ylab = ""
)
legend(
  x = "top",
  legend = c("1-3", "2-3", "1-2"),
  lty = 1:3,
  title = "Class Comparisons",
  xpd = NA,
  horiz = T, # was T
  inset = 0,
  pch = NA,
  seg.len = 3
)

# Plot one predictor pdps
# Mathematics model
dev.off()
par(mfrow = c(2, 5))
# Class 1
for (j in 1:5) {
  plot(
    partial(
      rfM,
      pred.var = names(trainM15[j]),
      train = trainM15,
      type = "classification",
      prob = T,
      chull = TRUE,
      which.class = "1"
    ),
    type = "l",
    ylim = c(0, 0.5),
    yaxs = "i",
    xlab = print(paste(names(trainM15[j]))),
    ylab = "MTH-1"
  )
}
# Class 3
for (j in 1:5) {
  plot(
    partial(
      rfM,
      pred.var = names(trainM15[j]),
      train = trainM15,
      type = "classification",
      prob = T,
      chull = TRUE,
      which.class = "3"
    ),
    type = "l",
    ylim = c(0, 0.5),
    yaxs = "i",
    xlab = print(paste(names(trainM15[j]))),
    ylab = "MTH-3"
  )
}
# Science model
# Class 1
for (j in 1:5) {
  plot(
    partial(
      rfSc,
      pred.var = names(trainSc15[j]),
      train = trainSc15,
      type = "classification",
      prob = T,
      chull = TRUE,
      which.class = "1"
    ),
    type = "l",
    ylim = c(0, 0.5),
    yaxs = "i",
    xlab = print(paste(names(trainSc15[j]))),
    ylab = "SCI-1"
  )
}
# Class 3
for (j in 1:5) {
  plot(
    partial(
      rfSc,
      pred.var = names(trainSc15[j]),
      train = trainSc15,
      type = "classification",
      prob = T,
      chull = TRUE,
      which.class = "3"
    ),
    type = "l",
    ylim = c(0, 0.5),
    yaxs = "i",
    xlab = print(paste(names(trainSc15[j]))),
    ylab = "SCI-3"
  )
}

# 3D pdps, example for ESCS and autonomy in science model
pdAE1 <- partial(
  rfSc,
  train = trainSc15,
  pred.var = c("AUTO", "ESCS"),
  grid.resolution = 40,
  which.class = "1"
)

plotPartial(
  pdAE1,
  levelplot = FALSE,
  zlab      = "SCI-1",
  drape     = TRUE,
  colorkey  = FALSE,
  screen    = list(z = -40, x = -40)
)
pdAE2 <- partial(
  rfSc,
  train = trainSc15,
  pred.var = c("AUTO", "ESCS"),
  grid.resolution = 40,
  which.class = "2"
)

plotPartial(
  pdAE2,
  levelplot = FALSE,
  zlab      = "SCI-2",
  drape     = TRUE,
  colorkey  = FALSE,
  screen    = list(z = -40, x = -40)
)
pdAE3 <- partial(
  rfSc,
  train = trainSc15,
  pred.var = c("AUTO", "ESCS"),
  grid.resolution = 40,
  which.class = "3"
)

plotPartial(
  pdAE3,
  levelplot = FALSE,
  zlab      = "SCI-3",
  drape     = TRUE,
  colorkey  = FALSE,
  screen    = list(z = -40, x = -40)
)

# Statistical part, HLM
# Run the script to the end first for the 2015 data
dataI <- dataI15
dataB <- dataB15
# and then for 2018 data, with the two lines below
#dataI <- dataI18
#dataB <- dataB18

# Prepare the final dataset
# ICT variables and ESCS: center and standardise by two sds
DataForStd <-
  as.data.frame(scale(dataI, center = TRUE, scale = FALSE))
twosd <- function(i) {
  0.5 * i / sd(i)
}
StanData <- as.data.frame(apply(DataForStd, 2, twosd))
#Gender: as factor & recode to 0 and 1
GEND <- as.data.frame(as.factor(car::recode(dataB$GEND, "'2' = 1; '1' = 0")))
names(GEND) <- "GEND"
# PVs and school ID: leave as they were
LeftData <-
  dplyr::select(dataB, c("SCHL", starts_with("PV")))
# Weights: correct for the sample size
WeightDataOld <- dplyr::select(dataB, starts_with("W_FS"))
correctweight <- function(i) {
  nrow(WeightDataOld) * i / sum(i)
}
WeightDataNew <-
  as.data.frame(apply(WeightDataOld, 2, correctweight))
# Merge
mydata <- cbind(StanData, GEND, LeftData, WeightDataNew)

# Students per group
Nstud <-
  apply(as.data.frame(unique(mydata$SCHL)), 1, function(i) {
    sum(nrow(mydata[mydata$SCHL == i,]))
  })
sum(Nstud < 10) # number of groups with less than 10 students

# Multilevel models
# Select math and science PVs and weights
PVsMath     <- dplyr::select(mydata, contains("MATH"))
PVsScie     <- dplyr::select(mydata, contains("SCIE"))
WeightsData <- dplyr::select(mydata, contains("W_FS"))

# Null (unconditional) models
# Variance decomposition and ICC
nullmodvar <- function(i) {
  summary(lmer(i ~ 1 + (1 | SCHL), mydata, weights = W_FSTUWT))$varcor
}
NullMathVar <-
  dplyr::select(as.data.frame(apply(PVsMath, 2, nullmodvar)), contains("vcov"))
NullMathInCptVar <- rowMeans(NullMathVar[1, ])
NullMathResidVar <- rowMeans(NullMathVar[2, ])
ICCmath <- NullMathInCptVar / (NullMathInCptVar + NullMathResidVar)

NullScieVar <-
  dplyr::select(as.data.frame(apply(PVsScie, 2, nullmodvar)), contains("vcov"))
NullScieInCptVar <- rowMeans(NullScieVar[1, ])
NullScieResidVar <- rowMeans(NullScieVar[2, ])
ICCscie          <-
  NullScieInCptVar / (NullScieInCptVar + NullScieResidVar)

# Fixed effects in null models (intercept estimate)
nullmodFixed <- function(i) {
  lme4::fixef(lmer(i ~ 1 + (1 | SCHL), mydata, weights = W_FSTUWT))
}
NullMathFixed <- as.data.frame(apply(PVsMath, 2, nullmodFixed))
NullScieFixed <- as.data.frame(apply(PVsScie, 2, nullmodFixed))
NullMathInCpt <- colMeans(NullMathFixed) # intercept maths
NullScieInCpt <- colMeans(NullScieFixed) # intercept science

# Significance of the estimate
nullmodsign <- function(i) {
  summary(lmer(i ~ 1 + (1 | SCHL), mydata, weights = W_FSTUWT))$coef
}
NullMathSign <- as.data.frame(apply(PVsMath, 2, nullmodsign))[5, ]
NullScieSign <- as.data.frame(apply(PVsScie, 2, nullmodsign))[5, ]

# Standard errors for null models
# Imputation variance
nullimpvarM <- function(i) {
  (i - NullMathInCpt) ^ 2
}
NullImpVarM <- 1 / 9 * sum(apply(NullMathFixed, 1, nullimpvarM))

nullimpvarS <- function(i) {
  (i - NullScieInCpt) ^ 2
}
NullImpVarS <- 1 / 9 * sum(apply(NullScieFixed, 1, nullimpvarS))

# Sampling variance
nullsamvar <- function(i) {
  apply(WeightsData, 2, function(j) {
    lme4::fixef(lmer(i ~ 1 + (1 | SCHL), mydata, weights = j))
  })
}

NullSamM <- as.data.frame(apply(PVsMath, 2, nullsamvar))
NullSamS <- as.data.frame(apply(PVsScie, 2, nullsamvar))
NullSamVarM <-
  1 / 10 * sum(1 / 20 * colSums((NullSamM - as.list(NullSamM[1, ])) ^ 2))
NullSamVarS <-
  1 / 10 * sum(1 / 20 * colSums((NullSamS - as.list(NullSamS[1, ])) ^ 2))

# Standard errors
nullSEmath <- sqrt(NullSamVarM + 1.1 * NullImpVarM) # SE maths
nullSEscie <- sqrt(NullSamVarS + 1.1 * NullImpVarS) # SE science

# Full models
# Random slopes AND inercepts failed to converge
#finalmodSlInt <- function(i) {
#  lme4::fixef(lmer(
#    i ~ GEND + ESCS + COMP + INTE + SOCI + AUTO
#    + (GEND + ESCS + COMP + INTE + SOCI + AUTO | SCHL),
#    mydata,
#    weights = W_FSTUWT
#  ))
#}
#FinalMathSlInt <- as.data.frame(apply(PVsMath, 2, finalmodSlInt))
#FinalScieSlInt <- as.data.frame(apply(PVsScie, 2, finalmodSlInt))

# Random intercept models
finalmodFixed <- function(i) {
  lme4::fixef(lmer(i ~ GEND + ESCS + COMP + INTE + SOCI + AUTO
             + (1 | SCHL),
             mydata,
             weights = W_FSTUWT))
}
# Fixed effects estimates
FinalMathFixed <- as.data.frame(apply(PVsMath, 2, finalmodFixed))
FinalScieFixed <- as.data.frame(apply(PVsScie, 2, finalmodFixed))
for (i in 1:7) {
  print(paste(mean(as.numeric(FinalMathFixed[i, ]))))
}
for (i in 1:7) {
  print(paste(mean(as.numeric(FinalScieFixed[i, ]))))
}

# Significance of estimates
finalmodsign <- function(i) {
  car::Anova(lmer(i ~ GEND + ESCS + COMP + INTE + SOCI + AUTO
                  + (1 | SCHL),
                  mydata,
                  weights = W_FSTUWT),
             type = "III")$`Pr(>Chisq)`
}
FinalMathSign <- as.data.frame(apply(PVsMath, 2, finalmodsign))
FinalScieSign <- as.data.frame(apply(PVsScie, 2, finalmodsign))

# Variance decomposition for full models
finalmodVar <- function(i) {
  summary(lmer(i ~ GEND + ESCS + COMP + INTE + SOCI + AUTO
               + (1 | SCHL),
               mydata,
               weights = W_FSTUWT))$varcor
}
FinMathVar <-
  dplyr::select(as.data.frame(apply(PVsMath, 2, finalmodVar)), contains("vcov"))
FinIntVarM <- rowMeans(FinMathVar[1, ])
FinResVarM <- rowMeans(FinMathVar[2, ])
FinScieVar <-
  dplyr::select(as.data.frame(apply(PVsScie, 2, finalmodVar)), contains("vcov"))
FinIntVarS <- rowMeans(FinScieVar[1, ])
FinResVarS <- rowMeans(FinScieVar[2, ])
# Nakagawa R2
nakagawa <- function(i) {
  r2_nakagawa(lmer(i ~ GEND + ESCS + COMP + INTE + SOCI + AUTO
                   + (1 | SCHL),
                   mydata,
                   weights = W_FSTUWT))
}
nakmathAll  <- unname(unlist(apply(PVsMath, 2, nakagawa)))
nakscieAll  <- unname(unlist(apply(PVsScie, 2, nakagawa)))
mean(nakmathAll[seq(1, 19, 2)])#conditional
mean(nakmathAll[seq(2, 20, 2)])#marginal
mean(nakscieAll[seq(1, 19, 2)])#conditional
mean(nakscieAll[seq(2, 20, 2)])#marginal

# Standard errors for full models
# Imputation variance
ImpVarM <- matrix(, nrow = 7, ncol = 1)
for (i in 1:7) {
  ImpVarM[i, 1] <-
    1 / 9 * sum((FinalMathFixed[i, ] - rowMeans(FinalMathFixed[i, ])) ^ 2)
}
ImpVarS <- matrix(, nrow = 7, ncol = 1)
for (i in 1:7) {
  ImpVarS[i, 1] <-
    1 / 9 * sum((FinalScieFixed[i, ] - rowMeans(FinalScieFixed[i, ])) ^ 2)
}

# Sampling variance
finsamvar <- function(i) {
  apply(WeightsData, 2, function(j) {
    lme4::fixef(lmer(
      i ~ GEND + ESCS + COMP + INTE + SOCI + AUTO
      + (1 | SCHL),
      mydata,
      weights = j
    ))
  })
}
FinSamM <- as.data.frame(apply(PVsMath, 2, finsamvar))
SamVarM <- matrix(, nrow = 7, ncol = 1)
for (i in 1:7) {
  SamVarM[i, 1] <-
    1 / 10 *
    sum(1 / 20 *
          colSums((FinSamM[seq(i, nrow(FinSamM), 7),]
                   - as.list(FinSamM[seq(i, nrow(FinSamM), 7),][1,])) ^ 2))
}
FinSamS <- as.data.frame(apply(PVsScie, 2, finsamvar))
SamVarS <- matrix(, nrow = 7, ncol = 1)
for (i in 1:7) {
  SamVarS[i, 1] <-
    1 / 10 *
    sum(1 / 20 *
          colSums((FinSamS[seq(i, nrow(FinSamS), 7), ]
                   - as.list(FinSamS[seq(i, nrow(FinSamS), 7), ][1, ])) ^ 2))
}
# Standard errors
SEmathFin <- sqrt(SamVarM + 1.1 * ImpVarM)
SEscieFin <- sqrt(SamVarS + 1.1 * ImpVarS)

# Check assumptions for all full models
finalmod <- function(i) {
  lmer(i ~ GEND + ESCS + COMP + INTE + SOCI + AUTO
       + (1 | SCHL),
       mydata,
       weights = W_FSTUWT)
}
fmM <- apply(PVsMath, 2, finalmod)
fmS <- apply(PVsScie, 2, finalmod)

# Diagnostic plots with sjPlot: residuals
diagplotM <- function(i) {
  sjPlot::plot_model(fmM[[i]], type = "diag")
}
lapply(c(1:10), diagplotM)
diagplotS <- function(i) {
  sjPlot::plot_model(fmS[[i]], type = "diag")
}
lapply(c(1:10), diagplotS)
# Check for no multicollinearity with VIF
for (i in 1:10) {
  print(paste(car::vif(fmM[[i]])))
}
for (i in 1:10) {
  print(paste(car::vif(fmS[[i]])))
}
# Visualize models
# Plot estimates, all PVs
dev.off()
sjPlot::plot_models(fmM, show.legend = F, title = "Mathematics 2015") # change the title for 2018
sjPlot::plot_models(fmS, show.legend = F, title = "Science 2015") # change the title for 2018
# The end
