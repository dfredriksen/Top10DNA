library(reshape2)
library(ggplot2)
library(plyr)
library(gridExtra)
library(lubridate)
library(data.table)
Random <- read.csv("Random.csv")
Top10 <- read.csv("Top10.csv")
PrimaryEmotions <- table(Random$primary, Random$valence)
barplot(PrimaryEmotions, main="Primary Emotion Frequency By Valence", xlab="Valence", ylab="Frequency", col=c("gray", "green", "blue","yellow","red","black","purple","white"), names.arg=c("Negative","Neutral","Positive"), legend = c("Neutral","Happy","Sad","Fear","Anger","Disgust","Surprise","multi"), ylim=c(0,450), beside=TRUE)
dev.copy2pdf(file='PrimaryEmotions.pdf')

emotionColumns <- c("neutral","happy","sad","fear","anger","disgust","surprise")
emotionColumnsValence <- c("neutral","happy","sad","fear","anger","disgust","surprise","valence")
Emotions <- Random[, match(emotionColumnsValence, names(Random))]
Emotions$valence <- as.character(Emotions$valence)
Melted <- melt(Emotions, id.vars = "valence", measure.vars = emotionColumns)
Melted <- Melted[Melted$value > 0,]

Melted2 <- count(Melted, c('variable','value','valence'))

Negative <- Melted2[Melted2$valence == -1,]
Positive <- Melted2[Melted2$valence == 1,]
Neutral <- Melted2[Melted2$valence == 0,]

plotNegative <- qplot(
  Negative$variable,Negative$value,
  aes(x=Negative$variable,y=Negative$value),
  xlab = "Emotion",
  ylab = "Weight",
  alpha=I(0.7)
) + geom_point(aes(size=Negative$freq), col="red")

plotNeutral <- qplot(
  Neutral$variable,Neutral$value,
  aes(x=Neutral$variable,y=Neutral$value),
  vxlab = "Emotion",
  ylab = "Weight",
  alpha=I(0.7)
) + geom_point(aes(size=Neutral$freq), col="gray")

plotPositive <- qplot(
  Positive$variable,Positive$value,
  aes(x=Positive$variable,y=Positive$value),
  xlab = "Emotion",
  ylab = "Weight",
  alpha=I(0.7)
) + geom_point(aes(size=Positive$freq), col="green")

chartList <- list(plotNegative, plotNeutral, plotPositive)
do.call(grid.arrange, c(chartList, list(ncol=2)))
dev.copy2pdf(file='EmotionsVsWeight.pdf', paper="a4r")

RandomFormatted <- Random
RandomFormatted$date <- parse_date_time(RandomFormatted$date, orders="ymd")
RandomFormatted$month <- format(RandomFormatted$date, "%m")
RandomSummary <- sapply(split(RandomFormatted[,c(9:15)], list(RandomFormatted$month, RandomFormatted$valence)), FUN=colMeans)
RandomSummary <- t(RandomSummary)

RandomSummaryNeut <- as.data.frame(RandomFormatted[,c("neutral","month","valence")])
RandomSummaryHappy <- RandomFormatted[,c("happy","month","valence")]
RandomSummarySad <- RandomFormatted[,c("sad","month","valence")]
RandomSummaryFear <- RandomFormatted[,c("fear","month","valence")]
RandomSummaryAnger <- RandomFormatted[,c("anger","month","valence")]
RandomSummaryDisgust <- RandomFormatted[,c("disgust","month","valence")]
RandomSummarySurprise <- RandomFormatted[,c("surprise","month","valence")]

RandomSummaryNeutSd <- sapply(split(RandomSummaryNeut[,1], list(RandomSummaryNeut$month, RandomSummaryNeut$valence)), FUN=sd)
RandomSummaryHappySd <- sapply(split(RandomSummaryHappy[,1], list(RandomSummaryHappy$month, RandomSummaryHappy$valence)), FUN=sd)
RandomSummarySadSd <- sapply(split(RandomSummarySad[,1], list(RandomSummarySad$month, RandomSummarySad$valence)), FUN=sd)
RandomSummaryFearSd <- sapply(split(RandomSummaryFear[,1], list(RandomSummaryFear$month, RandomSummaryFear$valence)), FUN=sd)
RandomSummaryAngerSd <- sapply(split(RandomSummaryAnger[,1], list(RandomSummaryAnger$month, RandomSummaryAnger$valence)), FUN=sd)
RandomSummaryDisgustSd <- sapply(split(RandomSummaryDisgust[,1], list(RandomSummaryDisgust$month, RandomSummaryDisgust$valence)), FUN=sd)
RandomSummarySurpriseSd <- sapply(split(RandomSummarySurprise[,1], list(RandomSummarySurprise$month, RandomSummarySurprise$valence)), FUN=sd)

#RandomSummaryNeutSd <- t(RandomSummaryNeutSd)
#RandomSummaryHappySd <- t(RandomSummaryHappySd)
#RandomSummarySadSd <- t(RandomSummarySadSd)
#RandomSummaryFearSd <- t(RandomSummaryFearSd)
#RandomSummaryAngerSd <- t(RandomSummaryAngerSd)
#RandomSummaryDisgustSd <- t(RandomSummaryDisgustSd)
#RandomSummarySurpriseSd <- t(RandomSummarySurpriseSd)

RandomSummaryNeutSd <- data.frame(RandomSummaryNeutSd)
RandomSummaryHappySd <- data.frame(RandomSummaryHappySd)
RandomSummarySadSd <- data.frame(RandomSummarySadSd)
RandomSummaryFearSd <- data.frame(RandomSummaryFearSd)
RandomSummaryAngerSd <- data.frame(RandomSummaryAngerSd)
RandomSummaryDisgustSd <- data.frame(RandomSummaryDisgustSd)
RandomSummarySurpriseSd <- data.frame(RandomSummarySurpriseSd)

RandomSummaryNeutSd <- setDT(RandomSummaryNeutSd, keep.rownames = TRUE)[]
RandomSummaryHappySd <- setDT(RandomSummaryHappySd, keep.rownames = TRUE)[]
RandomSummarySadSd <- setDT(RandomSummarySadSd, keep.rownames = TRUE)[]
RandomSummaryFearSd <- setDT(RandomSummaryFearSd, keep.rownames = TRUE)[]
RandomSummaryAngerSd <- setDT(RandomSummaryAngerSd, keep.rownames = TRUE)[]
RandomSummaryDisgustSd <- setDT(RandomSummaryDisgustSd, keep.rownames = TRUE)[]
RandomSummarySurpriseSd <- setDT(RandomSummarySurpriseSd, keep.rownames = TRUE)[]

Split <- t(as.data.frame(strsplit(RandomSummaryNeutSd$rn, split="\\.")))
RandomSummaryNeutSd$month <- Split[,1]
RandomSummaryNeutSd$valence <- Split[,2]
RandomSummaryNeutSd$rn <- NULL
RandomSummaryNeutSd$variance <- RandomSummaryNeutSd$RandomSummaryNeutSd
RandomSummaryNeutSd$variance <- RandomSummaryNeutSd$variance^2

Split <- t(as.data.frame(strsplit(RandomSummaryHappySd$rn, split="\\.")))
RandomSummaryHappySd$month <- Split[,1]
RandomSummaryHappySd$valence <- Split[,2]
RandomSummaryHappySd$rn <- NULL
RandomSummaryHappySd$variance <- RandomSummaryHappySd$RandomSummaryHappySd
RandomSummaryHappySd$variance <- RandomSummaryHappySd$variance^2

Split <- t(as.data.frame(strsplit(RandomSummarySadSd$rn, split="\\.")))
RandomSummarySadSd$month <- Split[,1]
RandomSummarySadSd$valence <- Split[,2]
RandomSummarySadSd$rn <- NULL
RandomSummarySadSd$variance <- RandomSummarySadSd$RandomSummarySadSd
RandomSummarySadSd$variance <- RandomSummarySadSd$variance^2

Split <- t(as.data.frame(strsplit(RandomSummaryFearSd$rn, split="\\.")))
RandomSummaryFearSd$month <- Split[,1]
RandomSummaryFearSd$valence <- Split[,2]
RandomSummaryFearSd$rn <- NULL
RandomSummaryFearSd$variance <- RandomSummaryFearSd$RandomSummaryFearSd
RandomSummaryFearSd$variance <- RandomSummaryFearSd$variance^2

Split <- t(as.data.frame(strsplit(RandomSummaryAngerSd$rn, split="\\.")))
RandomSummaryAngerSd$month <- Split[,1]
RandomSummaryAngerSd$valence <- Split[,2]
RandomSummaryAngerSd$rn <- NULL
RandomSummaryAngerSd$variance <- RandomSummaryAngerSd$RandomSummaryAngerSd
RandomSummaryAngerSd$variance <- RandomSummaryAngerSd$variance^2

Split <- t(as.data.frame(strsplit(RandomSummaryDisgustSd$rn, split="\\.")))
RandomSummaryDisgustSd$month <- Split[,1]
RandomSummaryDisgustSd$valence <- Split[,2]
RandomSummaryDisgustSd$rn <- NULL
RandomSummaryDisgustSd$variance <- RandomSummaryDisgustSd$RandomSummaryDisgustSd
RandomSummaryDisgustSd$variance <- RandomSummaryDisgustSd$variance^2

Split <- t(as.data.frame(strsplit(RandomSummarySurpriseSd$rn, split="\\.")))
RandomSummarySurpriseSd$month <- Split[,1]
RandomSummarySurpriseSd$valence <- Split[,2]
RandomSummarySurpriseSd$rn <- NULL
RandomSummarySurpriseSd$variance <- RandomSummarySurpriseSd$RandomSummarySurpriseSd
RandomSummarySurpriseSd$variance <- RandomSummarySurpriseSd$variance^2


RandomFinal <- data.frame(RandomSummary)
RandomFinal <- setDT(RandomFinal, keep.rownames = TRUE)[]
Split <- t(as.data.frame(strsplit(RandomFinal$rn, split="\\.")))
RandomFinal$month <- Split[,1]
RandomFinal$valence <- Split[,2]
RandomFinal$rn <- NULL
SampleCount <- count(RandomFormatted, c("month","valence"))
SampleCountNeg <- SampleCount[SampleCount$valence == "-1",c("month","freq")]
SampleCountNeu <- SampleCount[SampleCount$valence == "0",c("month","freq")]
SampleCountPos <- SampleCount[SampleCount$valence == "1",c("month","freq")]
RandomFinalNeg <- RandomFinal[RandomFinal$valence == "-1",]
RandomFinalNeg$valence <- NULL
RandomFinalNeu <- RandomFinal[RandomFinal$valence == "0",]
RandomFinalNeu$valence <- NULL
RandomFinalPos <- RandomFinal[RandomFinal$valence == "1",]
RandomFinalPos$valence <- NULL
RandomFinalNeg$count <- SampleCountNeg$freq
RandomFinalNeu$count <- SampleCountNeu$freq
RandomFinalNeu <- RandomFinalNeu[1:9,]
RandomFinalPos$count <- SampleCountPos$freq

RandomSummaryNeutSd <- data.frame(RandomSummaryNeutSd)
RandomSummaryNeutSdNeg <- RandomSummaryNeutSd[RandomSummaryNeutSd$valence == "-1",]
RandomSummaryNeutSdNeg$valence <- NULL
RandomSummaryNeutSdNeu <- RandomSummaryNeutSd[RandomSummaryNeutSd$valence == "0",]
RandomSummaryNeutSdNeu$valence <- NULL
RandomSummaryNeutSdPos <- RandomSummaryNeutSd[RandomSummaryNeutSd$valence == "1",]
RandomSummaryNeutSdPos$valence <- NULL

RandomSummaryHappySd <- data.frame(RandomSummaryHappySd)
RandomSummaryHappySdNeg <- RandomSummaryHappySd[RandomSummaryHappySd$valence == "-1",]
RandomSummaryHappySdNeg$valence <- NULL
RandomSummaryHappySdNeu <- RandomSummaryHappySd[RandomSummaryHappySd$valence == "0",]
RandomSummaryHappySdNeu$valence <- NULL
RandomSummaryHappySdPos <- RandomSummaryHappySd[RandomSummaryHappySd$valence == "1",]
RandomSummaryHappySdPos$valence <- NULL

RandomSummarySadSd <- data.frame(RandomSummarySadSd)
RandomSummarySadSdNeg <- RandomSummarySadSd[RandomSummarySadSd$valence == "-1",]
RandomSummarySadSdNeg$valence <- NULL
RandomSummarySadSdNeu <- RandomSummarySadSd[RandomSummarySadSd$valence == "0",]
RandomSummarySadSdNeu$valence <- NULL
RandomSummarySadSdPos <- RandomSummarySadSd[RandomSummarySadSd$valence == "1",]
RandomSummarySadSdPos$valence <- NULL

RandomSummaryFearSd <- data.frame(RandomSummaryFearSd)
RandomSummaryFearSdNeg <- RandomSummaryFearSd[RandomSummaryFearSd$valence == "-1",]
RandomSummaryFearSdNeg$valence <- NULL
RandomSummaryFearSdNeu <- RandomSummaryFearSd[RandomSummaryFearSd$valence == "0",]
RandomSummaryFearSdNeu$valence <- NULL
RandomSummaryFearSdPos <- RandomSummaryFearSd[RandomSummaryFearSd$valence == "1",]
RandomSummaryFearSdPos$valence <- NULL

RandomSummaryAngerSd <- data.frame(RandomSummaryAngerSd)
RandomSummaryAngerSdNeg <- RandomSummaryAngerSd[RandomSummaryAngerSd$valence == "-1",]
RandomSummaryAngerSdNeg$valence <- NULL
RandomSummaryAngerSdNeu <- RandomSummaryAngerSd[RandomSummaryAngerSd$valence == "0",]
RandomSummaryAngerSdNeu$valence <- NULL
RandomSummaryAngerSdPos <- RandomSummaryAngerSd[RandomSummaryAngerSd$valence == "1",]
RandomSummaryAngerSdPos$valence <- NULL

RandomSummaryDisgustSd <- data.frame(RandomSummaryDisgustSd)
RandomSummaryDisgustSdNeg <- RandomSummaryDisgustSd[RandomSummaryDisgustSd$valence == "-1",]
RandomSummaryDisgustSdNeg$valence <- NULL
RandomSummaryDisgustSdNeu <- RandomSummaryDisgustSd[RandomSummaryDisgustSd$valence == "0",]
RandomSummaryDisgustSdNeu$valence <- NULL
RandomSummaryDisgustSdPos <- RandomSummaryDisgustSd[RandomSummaryDisgustSd$valence == "1",]
RandomSummaryDisgustSdPos$valence <- NULL

RandomSummarySurpriseSd <- data.frame(RandomSummarySurpriseSd)
RandomSummarySurpriseSdNeg <- RandomSummarySurpriseSd[RandomSummarySurpriseSd$valence == "-1",]
RandomSummarySurpriseSdNeg$valence <- NULL
RandomSummarySurpriseSdNeu <- RandomSummarySurpriseSd[RandomSummarySurpriseSd$valence == "0",]
RandomSummarySurpriseSdNeu$valence <- NULL
RandomSummarySurpriseSdPos <- RandomSummarySurpriseSd[RandomSummarySurpriseSd$valence == "1",]
RandomSummarySurpriseSdPos$valence <- NULL

RandomFinalNeu <- rbind(RandomFinalNeu, list(0,0,0,0,0,0,0,10,0))

RandomSummarySdNeg <- RandomSummaryNeutSdNeg
RandomSummarySdNeg$neutral <- RandomSummaryNeutSdNeg$variance
RandomSummarySdNeg$variance <- NULL
RandomSummarySdNeg$RandomSummaryNeutSd <- NULL
RandomSummarySdNeg$neutral <- RandomSummaryNeutSdNeg$variance
RandomSummarySdNeg$happy <- RandomSummaryHappySdNeg$variance
RandomSummarySdNeg$sad <- RandomSummarySadSdNeg$variance
RandomSummarySdNeg$fear <- RandomSummaryFearSdNeg$variance
RandomSummarySdNeg$anger <- RandomSummaryAngerSdNeg$variance
RandomSummarySdNeg$disgust <- RandomSummaryDisgustSdNeg$variance
RandomSummarySdNeg$surprise <- RandomSummarySurpriseSdNeg$variance

RandomSummarySdNeu <- RandomSummaryNeutSdNeu
RandomSummarySdNeu$neutral <- RandomSummaryNeutSdNeu$variance
RandomSummarySdNeu$variance <- NULL
RandomSummarySdNeu$RandomSummaryNeutSd <- NULL
RandomSummarySdNeu$neutral <- RandomSummaryNeutSdNeu$variance
RandomSummarySdNeu$happy <- RandomSummaryHappySdNeu$variance
RandomSummarySdNeu$sad <- RandomSummarySadSdNeu$variance
RandomSummarySdNeu$fear <- RandomSummaryFearSdNeu$variance
RandomSummarySdNeu$anger <- RandomSummaryAngerSdNeu$variance
RandomSummarySdNeu$disgust <- RandomSummaryDisgustSdNeu$variance
RandomSummarySdNeu$surprise <- RandomSummarySurpriseSdNeu$variance

RandomSummarySdPos <- RandomSummaryNeutSdPos
RandomSummarySdPos$neutral <- RandomSummaryNeutSdPos$variance
RandomSummarySdPos$variance <- NULL
RandomSummarySdPos$RandomSummaryNeutSd <- NULL
RandomSummarySdPos$neutral <- RandomSummaryNeutSdPos$variance
RandomSummarySdPos$happy <- RandomSummaryHappySdPos$variance
RandomSummarySdPos$sad <- RandomSummarySadSdPos$variance
RandomSummarySdPos$fear <- RandomSummaryFearSdPos$variance
RandomSummarySdPos$anger <- RandomSummaryAngerSdPos$variance
RandomSummarySdPos$disgust <- RandomSummaryDisgustSdPos$variance
RandomSummarySdPos$surprise <- RandomSummarySurpriseSdPos$variance

RandomSummarySdNeg$count <- RandomFinalNeg$count
RandomSummarySdNeu$count <- RandomFinalNeu$count
RandomSummarySdPos$count <- RandomFinalPos$count

VectorTotal <- c(0,0,0,0,0,0,0)
SDTotal <- c(0,0,0,0,0,0,0)
maxRange <- nrow(RandomFinalNeg)
CountSum <- sum(RandomFinalNeg$count)
for(i in 1:maxRange)
{
  Count <- RandomFinalNeg$count
  Vector <- as.numeric(RandomFinalNeg[i,])
  Vector <- Vector[1:7]
  VectorWeighted <- Vector *  (Count[i] / CountSum)
  VectorTotal <- VectorTotal + VectorWeighted
  SD <- as.numeric(RandomSummarySdNeg[i,])
  SD <- SD[2:8]
  SDWeighted <- SD * (Count[i] / CountSum)
  SDTotal <- SDTotal + SDWeighted
}

NegTotal <- t(as.data.frame(VectorTotal))
NegTotal <- cbind(NegTotal,"-1")
NegSDTotal <- t(as.data.frame(SDTotal))
NegSDTotal <- sqrt(NegSDTotal)
NegSDTotal <- cbind(NegSDTotal, "-1")
colnames(NegTotal) <- emotionColumnsValence
colnames(NegSDTotal) <- emotionColumnsValence

VectorTotal <- c(0,0,0,0,0,0,0)
SDTotal <- c(0,0,0,0,0,0,0)
maxRange <- nrow(RandomFinalNeu)
CountSum <- sum(RandomFinalNeu$count)
for(i in 1:maxRange)
{
  Count <- RandomFinalNeu$count
  Vector <- as.numeric(RandomFinalNeu[i,])
  Vector <- Vector[1:7]
  VectorWeighted <- Vector *  (Count[i] / CountSum)
  VectorTotal <- VectorTotal + VectorWeighted
  SD <- as.numeric(RandomSummarySdNeu[i,])
  SD <- SD[2:8]
  SDWeighted <- SD * (Count[i] / CountSum)
  SDTotal <- SDTotal + SDWeighted
}

NeuTotal <- t(as.data.frame(VectorTotal))
NeuTotal = cbind(NeuTotal,"0")
NeuSDTotal <- t(as.data.frame(SDTotal))
NeuSDTotal <- sqrt(NeuSDTotal)
NeuSDTotal <- cbind(NeuSDTotal, "0")
colnames(NeuTotal) <- emotionColumnsValence
colnames(NeuSDTotal) <- emotionColumnsValence

VectorTotal <- c(0,0,0,0,0,0,0)
SDTotal <- c(0,0,0,0,0,0,0)
maxRange <- nrow(RandomFinalPos)
CountSum <- sum(RandomFinalPos$count)
for(i in 1:maxRange)
{
  Count <- RandomFinalPos$count
  Vector <- as.numeric(RandomFinalPos[i,])
  Vector <- Vector[1:7]
  VectorWeighted <- Vector *  (Count[i] / CountSum)
  VectorTotal <- VectorTotal + VectorWeighted
  SD <- as.numeric(RandomSummarySdNeu[i,])
  SD <- SD[2:8]
  SDWeighted <- SD * (Count[i] / CountSum)
  SDTotal <- SDTotal + SDWeighted
}

PosTotal <- t(as.data.frame(VectorTotal))
PosTotal <- cbind(PosTotal, "1")
PosSDTotal <- t(as.data.frame(SDTotal))
PosSDTotal <- sqrt(PosSDTotal)
PosSDTotal <- cbind(PosSDTotal, "1")
colnames(PosTotal) <- emotionColumnsValence
colnames(PosSDTotal) <- emotionColumnsValence

EmotionFactors <- as.factor(emotionColumns)

plotNegative <- qplot(
  EmotionFactors,NegTotal[1:7],
  xlab = "Emotion",
  ylab = "Weight",
  alpha=I(0.7)
) + 
  geom_line(col="red", aes(group=1)) +
  ggtitle("Average Weight by Emotion\n(Negative)")

plotNeutral <- qplot(
  EmotionFactors,NeuTotal[1:7],
  xlab = "Emotion",
  ylab = "Weight",
  alpha=I(0.7)
) + 
  geom_line(col="gray", aes(group=1)) +
  ggtitle("Average Weight by Emotion\n(Neutral)")

plotPositive <- qplot(
  EmotionFactors,PosTotal[1:7],
  xlab = "Emotion",
  ylab = "Weight",
  alpha=I(0.7)
) + 
  geom_line(col="green", aes(group=1)) +
  ggtitle("Average Weight by Emotion\n(Positive)")

chartList <- list(plotNegative, plotNeutral, plotPositive)
do.call(grid.arrange, c(chartList, list(ncol=2)))
dev.copy2pdf(file='AverageRandomWeights.pdf', width=10)

barplot(PrimaryEmotions, main="Primary Emotion Frequency By Valence", xlab="Valence", ylab="Frequency", col=c("gray", "green", "blue","yellow","red","black","purple","white"), names.arg=c("Negative","Neutral","Positive"), legend = c("Neutral","Happy","Sad","Fear","Anger","Disgust","Surprise","multi"), ylim=c(0,450), beside=TRUE)
dev.copy2pdf(file='PrimaryEmotions.pdf')

Random$row_id <- NULL
Random$path <- NULL
Random$views <- NULL
Random$pathTail <- NULL
Random$story_id <- NULL
Random$headline <- NULL
Random$teaser <- NULL
Random$primary_emotion <- NULL

Top10$row_id <- NULL
Top10$path <- NULL
Top10$views <- NULL
Top10$pathTail <- NULL
Top10$story_id <- NULL
Top10$headline <- NULL
Top10$teaser <- NULL
Top10$primary_emotion <- NULL

RandomNeg <- Random[Random$valence == "-1",2:8]
RandomNeu <- Random[Random$valence == "0",2:8]
RandomPos <- Random[Random$valence == "1",2:8]
Top10Neg <- Top10[Top10$valence == "-1",2:8]
Top10Neu <- Top10[Top10$valence == "0",2:8]
Top10Pos <- Top10[Top10$valence == "1",2:8]

t.test(RandomNeg$neutral, Top10Neg$neutral, var.equal=TRUE, paired=FALSE)
t.test(RandomNeg$happy, Top10Neg$happy, var.equal=TRUE, paired=FALSE)
t.test(RandomNeg$sad, Top10Neg$sad, var.equal=TRUE, paired=FALSE)
t.test(RandomNeg$fear, Top10Neg$fear, var.equal=TRUE, paired=FALSE)
t.test(RandomNeg$anger, Top10Neg$anger, var.equal=TRUE, paired=FALSE)
t.test(RandomNeg$disgust, Top10Neg$disgust, var.equal=TRUE, paired=FALSE)
t.test(RandomNeg$surprise, Top10Neg$surprise, var.equal=TRUE, paired=FALSE)

t.test(RandomNeu$neutral, Top10Neu$neutral, var.equal=TRUE, paired=FALSE)
t.test(RandomNeu$happy, Top10Neu$happy, var.equal=TRUE, paired=FALSE)
t.test(RandomNeu$sad, Top10Neu$sad, var.equal=TRUE, paired=FALSE)
t.test(RandomNeu$fear, Top10Neu$fear, var.equal=TRUE, paired=FALSE)
t.test(RandomNeu$anger, Top10Neu$anger, var.equal=TRUE, paired=FALSE)
t.test(RandomNeu$disgust, Top10Neu$disgust, var.equal=TRUE, paired=FALSE)
t.test(RandomNeu$surprise, Top10Neu$surprise, var.equal=TRUE, paired=FALSE)

t.test(RandomPos$neutral, Top10Neu$neutral, var.equal=TRUE, paired=FALSE)
t.test(RandomPos$happy, Top10Neu$happy, var.equal=TRUE, paired=FALSE)
t.test(RandomPos$sad, Top10Neu$sad, var.equal=TRUE, paired=FALSE)
t.test(RandomPos$fear, Top10Neu$fear, var.equal=TRUE, paired=FALSE)
t.test(RandomPos$anger, Top10Neu$anger, var.equal=TRUE, paired=FALSE)
t.test(RandomPos$disgust, Top10Neu$disgust, var.equal=TRUE, paired=FALSE)
t.test(RandomPos$surprise, Top10Neu$surprise, var.equal=TRUE, paired=FALSE)

a <- c(11,11,12,12,13,11,11,10,10,11,11,12,13,13,14,15,14,11,13,12,12,11,11,11,10,10,9,10)
b <- c(21,21,22,22,23,21,21,20,20,21,21,22,23,23,24,25,24,21,23,22,22,21,21,21,20,20,19,20)
t.test(a,b, var.equal=FALSE, paired=FALSE)
