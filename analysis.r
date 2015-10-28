library(reshape2)
library(ggplot2)
library(plyr)
library(gridExtra)
library(lubridate)
library(data.table)
Random <- read.csv("Random.csv")
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

colSds <- function(x) {x}
RandomFormatted <- Random
RandomFormatted$date <- parse_date_time(RandomFormatted$date, orders="ymd")
RandomFormatted$month <- format(RandomFormatted$date, "%m")
RandomSummary <- sapply(split(RandomFormatted[,c(9:15)], list(RandomFormatted$month, RandomFormatted$valence)), FUN=colMeans)
RandomSummary <- t(RandomSummary)

#RandomSummaryNeut <- as.data.frame(RandomFormatted[,c("neutral","month","valence")])
#RandomSummaryHappy <- RandomFormatted[,c("happy","month","valence")]
#RandomSummarySad <- RandomFormatted[,c("sad","month","valence")]
#RandomSummaryFear <- RandomFormatted[,c("fear","month","valence")]
#RandomSummaryAnger <- RandomFormatted[,c("anger","month","valence")]
#RandomSummaryDisgust <- RandomFormatted[,c("disgust","month","valence")]
#RandomSummarySurprise <- RandomFormatted[,c("surprise","month","valence")]

#RandomSummaryNeutSd <- sapply(split(RandomSummaryNeut[,1], list(RandomSummaryNeut$month, RandomSummaryNeut$valence)), FUN=sd)
#RandomSummaryHappySd <- t(sapply(split(RandomSummaryHappy[,1], list(RandomSummaryHappy$month, RandomSummaryHappy$valence)), FUN=sd))
#RandomSummarySadSd <- t(sapply(split(RandomSummarySad[,1], list(RandomSummarySad$month, RandomSummarySad$valence)), FUN=sd))
#RandomSummaryFearSd <- t(sapply(split(RandomSummaryFear[,1], list(RandomSummaryFear$month, RandomSummaryFear$valence)), FUN=sd))
#RandomSummaryAngerSd <- t(sapply(split(RandomSummaryAnger[,1], list(RandomSummaryAnger$month, RandomSummaryAnger$valence)), FUN=sd))
#RandomSummaryDisgustSd <- t(sapply(split(RandomSummaryDisgust[,1], list(RandomSummaryDisgust$month, RandomSummaryDisgust$valence)), FUN=sd))
#RandomSummarySurpriseSd <- t(sapply(split(RandomSummarySurprise[,1], list(RandomSummarySurprise$month, RandomSummarySurprise$valence)), FUN=sd))

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

VectorTotal <- c(0,0,0,0,0,0,0)
maxRange <- nrow(RandomFinalNeg)
CountSum <- sum(RandomFinalNeg$count)
for(i in 1:maxRange)
{
  Count <- RandomFinalNeg$count
  Vector <- as.numeric(RandomFinalNeg[i,])
  Vector <- Vector[1:7]
  VectorWeighted <- Vector *  (Count[i] / CountSum)
  VectorTotal <- VectorTotal + VectorWeighted
}

NegTotal <- t(as.data.frame(VectorTotal))
NegTotal <- cbind(NegTotal,"-1")
colnames(NegTotal) <- emotionColumnsValence

VectorTotal <- c(0,0,0,0,0,0,0)
maxRange <- nrow(RandomFinalNeu)
CountSum <- sum(RandomFinalNeu$count)
for(i in 1:maxRange)
{
  Count <- RandomFinalNeu$count
  Vector <- as.numeric(RandomFinalNeu[i,])
  Vector <- Vector[1:7]
  VectorWeighted <- Vector *  (Count[i] / CountSum)
  VectorTotal <- VectorTotal + VectorWeighted
}

NeuTotal <- t(as.data.frame(VectorTotal))
NeuTotal = cbind(NeuTotal,"0")
colnames(NeuTotal) <- emotionColumnsValence

VectorTotal <- c(0,0,0,0,0,0,0)
maxRange <- nrow(RandomFinalPos)
CountSum <- sum(RandomFinalPos$count)
for(i in 1:maxRange)
{
  Count <- RandomFinalPos$count
  Vector <- as.numeric(RandomFinalPos[i,])
  Vector <- Vector[1:7]
  VectorWeighted <- Vector *  (Count[i] / CountSum)
  VectorTotal <- VectorTotal + VectorWeighted
}

PosTotal <- t(as.data.frame(VectorTotal))
PosTotal <- cbind(PosTotal, "1")
colnames(PosTotal) <- emotionColumnsValence

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

#barplot(PrimaryEmotions, main="Primary Emotion Frequency By Valence", xlab="Valence", ylab="Frequency", col=c("gray", "green", "blue","yellow","red","black","purple","white"), names.arg=c("Negative","Neutral","Positive"), legend = c("Neutral","Happy","Sad","Fear","Anger","Disgust","Surprise","multi"), ylim=c(0,450), beside=TRUE)
#dev.copy2pdf(file='PrimaryEmotions.pdf')

