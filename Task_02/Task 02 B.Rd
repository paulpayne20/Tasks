setwd(&#39;~/Desktop/Evolution/tasks/Task_02&#39;)
Data &lt;- read.csv (&#39;http://jonsmitchell.com/data/beren.csv&#39;, stringsAsFactors=F)
write.csv(Data, &#39;rawdata.csv&#39;, quote=F)
length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
head(data)
Data[1,]
Data[2,]
Data[1:3,]
Data[1:3, 4]
Data[1:5, 1:3]
Feeds &lt;- which(Data[,9] == &#39;bottle&#39;)
berenMilk &lt;- Data[Feeds ,]
head(berenMilk)
Feeds &lt;- which(Data[,&#39;event&#39;] == &#39;bottle&#39;)
head(Feeds)
Feeds &lt;- which(Data$event == &#39;bottle&#39;)
head(Feeds)
dayID &lt;- apply(Data, 1, function(x) paste(x[1:3],collapse=&#39;-&#39;))
head(dayID)
dateID &lt;- sapply(dayID, as.Date, format = &quot;%Y-%m-%d&quot;, origin = &quot;2019-04-18&quot;)
head(dateID)
Data$age &lt;- dateID - dateID [which(Data$event == &#39;birth&#39;)]
head(data)
beren2&lt;-Data
beren3&lt;- beren2[order(beren2$age) ,]
write.csv(beren3, &#39;beren_new.csv&#39;, quote=F, row.names=FALSE)
Feeds &lt;- which(beren3$event == &quot;bottle&quot;)
avgMilk &lt;- mean(beren3$value[Feeds])
avgFeed &lt;- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
varFeed &lt;- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed &lt;- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds &lt;- tapply(beren3$value[Feeds] , beren3$age[Feeds], length)
head(numFeeds)
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor &lt;- cor.test(beren3$value[Feeds], beren3$age[Feeds])
head(berenCor)
berenANOVA &lt;- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
head(berenANOVA)
boxplot( beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab= &quot;who gave the bottle&quot; ,
ylab = &quot;amount of milk consumed (oz)&quot; )
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)

plot(as.numeric(names(totalFeed)), totalFeed, type=&quot;b&quot;, pch=16, xlab=&quot;age in days&quot;,
ylab=&quot;ounces of milk&quot;)
abline(h=mean(totalFeed), lty=2, col=&#39;red&#39;)
pdf(&quot;r02b-totalMilkByDay.pdf&quot;, height = 4, width = 4)
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type=&quot;b&quot;, pch=16, xlab=&quot;age in days&quot;,
ylab=&quot;ounces of milk&quot;)
abline(h=mean(totalFeed), lty=2, col=&#39;red&#39;)
dev.off()