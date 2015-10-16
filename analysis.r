library(reshape2)
library(ggplot2)
library(plyr)
library(gridExtra)
Top10 <- read.csv("Top10.csv")
PrimaryEmotions <- table(Top10$primary, Top10$valence)
barplot(PrimaryEmotions, main="Primary Emotion Frequency By Valence", xlab="Valence", ylab="Frequency", col=c("gray", "green", "blue","yellow","red","black","purple"), names.arg=c("Negative","Neutral","Positive"), legend = c("Neutral","Happy","Sad","Fear","Anger","Disgust","Surprise"), ylim=c(0,450), beside=TRUE)
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
  xlab = "Emotion",
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