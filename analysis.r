library(reshape2)
library(ggplot2)
library(plyr)
library(gridExtra)
library(lubridate)
library(data.table)
Top10 <- read.csv("Top10.csv")
PrimaryEmotions <- table(Top10$primary, Top10$valence)
barplot(PrimaryEmotions, main="Primary Emotion Frequency By Valence", xlab="Valence", ylab="Frequency", col=c("gray", "green", "blue","yellow","red","black","purple","white"), names.arg=c("Negative","Neutral","Positive"), legend = c("Neutral","Happy","Sad","Fear","Anger","Disgust","Surprise","multi"), ylim=c(0,450), beside=TRUE)
dev.copy2pdf(file='PrimaryEmotions.pdf')

emotionColumns <- c("neutral","happy","sad","fear","anger","disgust","surprise")
emotionColumnsValence <- c("neutral","happy","sad","fear","anger","disgust","surprise","valence")
Emotions <- Top10[, match(emotionColumnsValence, names(Top10))]
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
Top10Formatted <- Top10
Top10Formatted$date <- parse_date_time(Top10Formatted$date, orders="ymd")
Top10Formatted$month <- format(Top10Formatted$date, "%m")
Top10Summary <- sapply(split(Top10Formatted[,c(9:15)], list(Top10Formatted$month, Top10Formatted$valence)), FUN=colMeans)
Top10Summary <- t(Top10Summary)

#Top10SummaryNeut <- as.data.frame(Top10Formatted[,c("neutral","month","valence")])
#Top10SummaryHappy <- Top10Formatted[,c("happy","month","valence")]
#Top10SummarySad <- Top10Formatted[,c("sad","month","valence")]
#Top10SummaryFear <- Top10Formatted[,c("fear","month","valence")]
#Top10SummaryAnger <- Top10Formatted[,c("anger","month","valence")]
#Top10SummaryDisgust <- Top10Formatted[,c("disgust","month","valence")]
#Top10SummarySurprise <- Top10Formatted[,c("surprise","month","valence")]

#Top10SummaryNeutSd <- sapply(split(Top10SummaryNeut[,1], list(Top10SummaryNeut$month, Top10SummaryNeut$valence)), FUN=sd)
#Top10SummaryHappySd <- t(sapply(split(Top10SummaryHappy[,1], list(Top10SummaryHappy$month, Top10SummaryHappy$valence)), FUN=sd))
#Top10SummarySadSd <- t(sapply(split(Top10SummarySad[,1], list(Top10SummarySad$month, Top10SummarySad$valence)), FUN=sd))
#Top10SummaryFearSd <- t(sapply(split(Top10SummaryFear[,1], list(Top10SummaryFear$month, Top10SummaryFear$valence)), FUN=sd))
#Top10SummaryAngerSd <- t(sapply(split(Top10SummaryAnger[,1], list(Top10SummaryAnger$month, Top10SummaryAnger$valence)), FUN=sd))
#Top10SummaryDisgustSd <- t(sapply(split(Top10SummaryDisgust[,1], list(Top10SummaryDisgust$month, Top10SummaryDisgust$valence)), FUN=sd))
#Top10SummarySurpriseSd <- t(sapply(split(Top10SummarySurprise[,1], list(Top10SummarySurprise$month, Top10SummarySurprise$valence)), FUN=sd))

Top10Final <- data.frame(Top10Summary)
Top10Final <- setDT(Top10Final, keep.rownames = TRUE)[]
Split <- t(as.data.frame(strsplit(Top10Final$rn, split="\\.")))
Top10Final$month <- Split[,1]
Top10Final$valence <- Split[,2]
Top10Final$rn <- NULL
SampleCount <- count(Top10Formatted, c("month","valence"))
SampleCountNeg <- SampleCount[SampleCount$valence == "-1",c("month","freq")]
SampleCountNeu <- SampleCount[SampleCount$valence == "0",c("month","freq")]
SampleCountPos <- SampleCount[SampleCount$valence == "1",c("month","freq")]
Top10FinalNeg <- Top10Final[Top10Final$valence == "-1",]
Top10FinalNeg$valence <- NULL
Top10FinalNeu <- Top10Final[Top10Final$valence == "0",]
Top10FinalNeu$valence <- NULL
Top10FinalPos <- Top10Final[Top10Final$valence == "1",]
Top10FinalPos$valence <- NULL
Top10FinalNeg$count <- SampleCountNeg$freq
Top10FinalNeu$count <- SampleCountNeu$freq
Top10FinalNeu <- Top10FinalNeu[1:9,]
Top10FinalPos$count <- SampleCountPos$freq

VectorTotal <- c(0,0,0,0,0,0,0)
maxRange <- nrow(Top10FinalNeg)
CountSum <- sum(Top10FinalNeg$count)
for(i in 1:maxRange)
{
  Count <- Top10FinalNeg$count
  Vector <- as.numeric(Top10FinalNeg[i,])
  Vector <- Vector[1:7]
  VectorWeighted <- Vector *  (Count[i] / CountSum)
  VectorTotal <- VectorTotal + VectorWeighted
}

NegTotal <- t(as.data.frame(VectorTotal))
NegTotal <- cbind(NegTotal,"-1")
colnames(NegTotal) <- emotionColumnsValence

VectorTotal <- c(0,0,0,0,0,0,0)
maxRange <- nrow(Top10FinalNeu)
CountSum <- sum(Top10FinalNeu$count)
for(i in 1:maxRange)
{
  Count <- Top10FinalNeu$count
  Vector <- as.numeric(Top10FinalNeu[i,])
  Vector <- Vector[1:7]
  VectorWeighted <- Vector *  (Count[i] / CountSum)
  VectorTotal <- VectorTotal + VectorWeighted
}

NeuTotal <- t(as.data.frame(VectorTotal))
NeuTotal = cbind(NeuTotal,"0")
colnames(NeuTotal) <- emotionColumnsValence

VectorTotal <- c(0,0,0,0,0,0,0)
maxRange <- nrow(Top10FinalPos)
CountSum <- sum(Top10FinalPos$count)
for(i in 1:maxRange)
{
  Count <- Top10FinalPos$count
  Vector <- as.numeric(Top10FinalPos[i,])
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
dev.copy2pdf(file='AverageTop10Weights.pdf', width=10)

#barplot(PrimaryEmotions, main="Primary Emotion Frequency By Valence", xlab="Valence", ylab="Frequency", col=c("gray", "green", "blue","yellow","red","black","purple","white"), names.arg=c("Negative","Neutral","Positive"), legend = c("Neutral","Happy","Sad","Fear","Anger","Disgust","Surprise","multi"), ylim=c(0,450), beside=TRUE)
#dev.copy2pdf(file='PrimaryEmotions.pdf')

