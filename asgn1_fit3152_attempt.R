specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))


library(zoo)
library(plyr)
library(dplyr)
library(lubridate)
webforum = read.csv("webforum.csv", header = TRUE)  #read the data

wc0Webforum = webforum[!(webforum$WC==0),] #eliminate the 0 word count

wc0Webforum$Date = as.Date(wc0Webforum$Date) #make the date column as a date type
countPerMon = aggregate(wc0Webforum$PostID ~ as.yearmon(wc0Webforum$Date), wc0Webforum, FUN = function(x) length(x))  #find out how many posts per month
#mean(countPerMon$`wc0Webforum$PostID`) #which is equal to 184.463


countPerThread = aggregate(cbind(count = wc0Webforum$ThreadID) ~ wc0Webforum$ThreadID, data = wc0Webforum, FUN = function(x){NROW(x)})  #find out how many posts per thread

#plot(countPerThread$`wc0Webforum$ThreadID`, countPerThread$count)
postsMoreThanMedian = countPerThread[countPerThread$count > median(countPerThread$count),]  #get the threads where the number of posts is more than the median. It is because we believed that there are several outliers that affect the result of the average, so we use median instead.

wc0MrThMednWebforum = as.data.frame(wc0Webforum[(wc0Webforum$ThreadID %in% postsMoreThanMedian$'wc0Webforum$ThreadID'), ])  #get the full data of each threadID according to PostsMoreThanMedian


nOfAuthPerThread = as.data.frame(as.table(by(wc0MrThMednWebforum, wc0MrThMednWebforum$ThreadID, function(df) length(unique(df$AuthorID))))) #get the number of authors per thread, so find out how many number of unique authors there are in each thread

nOfAuthPerThreadMoreThanMedian = nOfAuthPerThread[nOfAuthPerThread$Freq > median(nOfAuthPerThread$Freq),] #again here we use median since we've noticed there are a couple of outliers although the result of the median and the mean is only differs by 7


wc0_MrThMednP_and_A_Webforum = as.data.frame(wc0MrThMednWebforum[(wc0MrThMednWebforum$ThreadID %in% nOfAuthPerThreadMoreThanMedian$wc0MrThMednWebforum.ThreadID), ])  #get the full data of each authorID according to nOfAuthPerThreadMoreThanMedian

numOfPostsPerThread = as.data.frame(as.table(by(wc0_MrThMednP_and_A_Webforum, wc0_MrThMednP_and_A_Webforum$ThreadID, function(df) length(df$PostID))))  #get the number of posts per thread

#GRAPH
PostCountGraph = ggplot(numOfPostsPerThread, aes(reorder(numOfPostsPerThread$wc0_MrThMednP_and_A_Webforum.ThreadID, numOfPostsPerThread$Freq), numOfPostsPerThread$Freq)) + geom_bar(width = 1, stat= "identity") + labs(x="Number of Post Per Thread",y="Thread ID") + theme_light() + coord_flip() #get the graph based on number of posts per thread
PostCountGraph

top6ThreadwMostPosts = numOfPostsPerThread[order(numOfPostsPerThread$Freq,decreasing=T)[1:6],]  #get the top 6 threads w most posts because the fifth and the sixth most posts have the same value
top6ThreadwMostPostsFullData = as.data.frame(wc0_MrThMednP_and_A_Webforum[(wc0_MrThMednP_and_A_Webforum$ThreadID %in% top6ThreadwMostPosts$wc0_MrThMednP_and_A_Webforum.ThreadID),]) #get the top 6 threads' full data
#
#
#
#
#
#___________________________________________
#
#
#
#
#

medianOfAnalyticForEachTop6Thread = as.data.frame(as.table(by(top6ThreadwMostPostsFullData, top6ThreadwMostPostsFullData$ThreadID, function(df) median(df$Analytic)))) #get the thread which is the most analytical based on median since a couple of threads has outliers such as thread 127115

library(ggplot2)
library(scales)
#GRAPH
bpFormedianOfAnalyticForEachTop6Thread = ggplot(medianOfAnalyticForEachTop6Thread, aes(x="", y=medianOfAnalyticForEachTop6Thread$Freq, fill=medianOfAnalyticForEachTop6Thread$top6ThreadwMostPostsFullData.ThreadID)) + geom_bar(width = 1, stat= "identity")
pieFormedianOfAnalyticForEachTop6Thread = bpFormedianOfAnalyticForEachTop6Thread + coord_polar("y", start=0) + geom_text(aes(y = medianOfAnalyticForEachTop6Thread$Freq/6 + c(0, cumsum(medianOfAnalyticForEachTop6Thread$Freq)[-length(medianOfAnalyticForEachTop6Thread$Freq)]), label = medianOfAnalyticForEachTop6Thread$Freq), size=5)


#pick thread 127115 since it is the most analytic and it is active from 2005-2011 while thread 252620 has the most posts but it is only active in 2005-2006

as.data.frame(as.table(by(thread127115, thread127115$AuthorID, function(df) NROW(df$PostID))))  #get the author with the most posts in thread 127115

author47875 = top6ThreadwMostPostsFullData[top6ThreadwMostPostsFullData$AuthorID == 47875,] #get the data for the author with most posts in thread 127115

meanOfAnalyticOfAuthr47875 = as.data.frame(as.table(by(author47875, author47875$ThreadID, function(df) as.numeric(specify_decimal(mean(df$Analytic), 3)))))  #the mean can be seen that author 47875 in thread 127115 is the most analytical and this supports the thread 127115 that is the most analytical thread.


library(ggplot2)
library(scales)

bpFormeanOfAnalyticOfAuthr47875 = ggplot(meanOfAnalyticOfAuthr47875, aes(x="", y=meanOfAnalyticOfAuthr47875$Freq, fill=meanOfAnalyticOfAuthr47875$author47875.ThreadID)) + geom_bar(width = 1, stat= "identity") #make a bar plot to later transform it to pie chart

pieFormeanOfAnalyticOfAuthr47875 = bpFormeanOfAnalyticOfAuthr47875 + coord_polar("y", start=0) + geom_text(aes(y = meanOfAnalyticOfAuthr47875$Freq/4 + c(0, cumsum(meanOfAnalyticOfAuthr47875$Freq)[-length(meanOfAnalyticOfAuthr47875$Freq)]), label = meanOfAnalyticOfAuthr47875$Freq), size=5)  #make a pie chart based on the mean of analytics of author 47875 per thread that he/she has posts based on df top6ThreadswMostPostsFullData

#need to subset further based on months?
thread127115in2009 = thread127115[thread127115$Date >= as.Date("2009-01-01") & thread127115$Date <= as.Date("2009-12-31"),] #pick year 2009 to be analyzed since it has the most posts
#GRAPH
ggplot(thread127115in2009, aes(x=month(thread127115in2009$Date), y=thread127115in2009$Analytic)) + geom_bar(stat="identity")  #the graph shows that the thread 127115 in 2009 which has the most posts, the members communicate directly in an analytical way.


thread145223in2004 = thread145223[thread145223$Date >= as.Date("2004-01-01") & thread145223$Date <= as.Date("2004-12-31"),]
#GRAPH
ggplot(thread145223in2004, aes(x=thread145223in2004$Date, y=thread145223in2004$Analytic)) + geom_point() + geom_line() + geom_ribbon(aes(ymin = 0, ymax = thread145223in2004$Analytic), fill = 'blue')


#GRAPH
ggplot(thread252620InDec2005, aes(x=thread252620InDec2005$Date, y=thread252620InDec2005$Analytic)) + geom_point() + geom_line() + geom_ribbon(aes(ymin = 0, ymax = thread252620InDec2005$Analytic), fill = 'blue')


thread283958In2006 = thread283958highestanon[thread283958highestanon$Date >= as.Date("2006-01-01") & thread283958highestanon$Date <= as.Date("2006-12-31"),]
#GRAPH
ggplot(thread283958In2006, aes(x=thread283958In2006$Date, y=thread283958In2006$Analytic)) + geom_point() + geom_line() + geom_ribbon(aes(ymin = 0, ymax = thread283958In2006$Analytic), fill = 'blue')

thread472752In2009 = thread472752[thread472752$Date >= as.Date("2009-01-01") & thread472752$Date <= as.Date("2009-12-31"),]
#GRAPH
ggplot(thread472752In2009, aes(x=thread472752In2009$Date, y=thread472752In2009$Analytic)) + geom_point() + geom_line() + geom_ribbon(aes(ymin = 0, ymax = thread472752In2009$Analytic), fill = 'blue')

thread532649In2009 = thread532649[thread532649$Date >= as.Date("2009-01-01") & thread532649$Date <= as.Date("2009-12-31"),]
#GRAPH
ggplot(thread532649In2009, aes(x=thread532649In2009$Date, y=thread532649In2009$Analytic)) + geom_point()
qplot(thread532649In2009$Date, thread532649In2009$negemo, data=thread532649In2009, shape = as.factor(round_any(thread532649In2009$swear,1)), size = thread532649In2009$anx , color = as.factor(round_any(thread532649In2009$Tone, 1))) #color=as.factor(thread532649In2009$Analytic), size=5)

#
#
#
qplot(thread127115$Date, thread127115$negemo, data=thread127115,  size = as.factor(round_any(thread127115$negemo,1)))
#
#
#
cor(thread127115$posemo, thread127115$Clout)


thread127115in2009$DateTime = as.POSIXct(paste(thread127115in2009$Date, thread127115in2009$Time), format = "%Y-%m-%d %H:%M") #combine date and time columns

thread127115inOCT2009 = thread127115in2009[thread127115in2009$Date >= as.Date("2009-10-01") & thread127115in2009$Date <= as.Date("2009-10-31"),] 



#For negemo, anger and anx, we use thread 252620 based on highest median of negemo, anx, anger
summary(lm(thread252620$negemo ~ thread252620$anx+thread252620$anger))  #we choose negemo, anx, anger to be grouped together because they are both related with 3 significant stars
p1 = ggplot(thread252620[20:22], aes(x=thread252620$Date, y=thread252620$anx)) + geom_line(aes(x=thread252620$Date, y=thread252620$negemo), colour = 'red', size=3) + geom_line(aes(x=thread252620$Date, y=thread252620$anger), colour = alpha('blue', 0.6), size=3) + geom_line(aes(x=thread252620$Date, y=thread252620$anx), colour = 'green', size=3) + scale_colour_discrete(labels=c("negemo", "angry", "anx"))
#graph
p2 = qplot(thread127115in2009$DateTime, thread127115in2009$anger, data=thread127115in2009, size=4, color=as.factor(round_any(thread127115in2009$negemo, 1))) #working for now


thread252620InDec2005 = thread252620[thread252620$Date >= as.Date("2005-01-01") & thread252620$Date <= as.Date("2005-12-31"),]  #pick 2005 since in 2005, it has the most posts
thread252620InDec2005$DateTime = as.POSIXct(paste(thread252620InDec2005$Date, thread252620InDec2005$Time), format = "%Y-%m-%d %H:%M") #combine date and time columns

thread252620In18Dec2005 = thread252620InDec2005[thread252620InDec2005$Date >= as.Date("2005-12-08") & thread252620InDec2005$Date <= as.Date("2005-12-18"),]
thread252620In18Dec2005$DateTime = as.POSIXct(paste(thread252620In18Dec2005$Date, thread252620In18Dec2005$Time), format = "%Y-%m-%d %H:%M") #combine date and time columns



##################################################################################################################################################3


top5ThreadWout252620 = top6ThreadwMostPostsFullData[!(top6ThreadwMostPostsFullData$ThreadID==252620),]  #we decided to remove thread 252620 because the thread is only active between 3 months only which is not suitable in choosing the most posts in a year
plot(thread252620$Date,thread252620$PostID, xlab = "Date", ylab = "PostID")
noOfPostsPerYearTop5 = as.data.frame(as.table(by(top5ThreadWout252620, year(top5ThreadWout252620$Date), function(df) length(df$PostID)))) #get the number of posts per year and the maximum is 2005 where it has the most number of posts

library(ggplot2)
library(scales)
bpFornoOfPostsPerYearTop5 = ggplot(noOfPostsPerYearTop5, aes(x="", y=noOfPostsPerYearTop5$Freq, fill=noOfPostsPerYearTop5$year.top5ThreadWout252620.Date.)) + geom_bar(width = 1, stat= "identity") +  ggtitle("Number of Posts in Top 6 Thread Without Thread 252620") + scale_fill_discrete(name = "Year") + labs(x="", y="")
pieFornoOfPostsPerYearTop5 = bpFornoOfPostsPerYearTop5 + coord_polar("y", start=0) + geom_text(aes(y = noOfPostsPerYearTop5$Freq/2 + c(0, cumsum(noOfPostsPerYearTop5$Freq)[-length(noOfPostsPerYearTop5$Freq)]), label = noOfPostsPerYearTop5$Freq), size=5)
pieFornoOfPostsPerYearTop5

postsIn2006To2009 = top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2006-01-01") & top5ThreadWout252620$Date <= as.Date("2009-12-31"),]



#medianForeachDataPerMonthIn2006To2009QTR = aggregate(postsIn2006To2009[18:32], list(as.yearqtr(postsIn2006To2009$Date)), median)
#medianForeachDataPerMonthIn2006To2009QTR$negemo = ts(medianForeachDataPerMonthIn2006To2009QTR$negemo, frequency = 4, start = c(2006, 1))
#plot(ts(medianForeachDataPerMonthIn2006To2009QTR$negemo, frequency = 4, start = c(2006, 1)))
#dev.new()
#plot(ts(medianForeachDataPerMonthIn2006To2009QTR$posemo, frequency = 4, start = c(2006, 1)))
#plot(ts(medianForeachDataPerMonthIn2006To2009QTR$anger, frequency = 4, start = c(2006, 1)))

#CORRECT.....................................
#>............>>>>>>>>>>...........>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#For the top 5 threads without 252620
mean(top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2006-01-05") & top5ThreadWout252620$Date <= as.Date("2006-05-31"),]$leisure)

#[1] 1.438732

mean(top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2006-06-01") & top5ThreadWout252620$Date <= as.Date("2006-09-30"),]$leisure)

#[1] 2.563697

mean(top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2006-11-01") & top5ThreadWout252620$Date <= as.Date("2007-01-05"),]$leisure)
#[1] 2.0575


#leisureJanuaryMay2006 = top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2006-01-05") & top5ThreadWout252620$Date <= as.Date("2006-05-31"),]$leisure
#leisureJuneSeptember2006 = top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2006-06-01") & top5ThreadWout252620$Date <= as.Date("2006-09-30"),]$leisure
#t.test(leisureJuneSeptember2006,leisureJanuaryMay2006, "greater" , conf.level = 0.95)

#leisureJuneSeptember2009 = top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2009-06-01") & top5ThreadWout252620$Date <= as.Date("2009-09-30"),]$leisure
#leisureJanuaryMay2009 = top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2009-01-05") & top5ThreadWout252620$Date <= as.Date("2009-05-31"),]$leisure
#t.test(leisureJuneSeptember2009,leisureJanuaryMay2009, conf.level = 0.95)

#leisureJuneSeptember2010 = top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2007-06-01") & top5ThreadWout252620$Date <= as.Date("2007-09-30"),]$leisure
#leisureJanuaryMay2010 = top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("20-01-05") & top5ThreadWout252620$Date <= as.Date("2007-05-31"),]$leisure
#t.test(leisureJuneSeptember2010,leisureJanuaryMay2010, conf.level = 0.95)

median(top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2006-01-05") & top5ThreadWout252620$Date <= as.Date("2006-05-31"),]$leisure)
#[1] 0
median(top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2006-06-01") & top5ThreadWout252620$Date <= as.Date("2006-09-30"),]$leisure)
#[1] 2.22
median(top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2006-12-01") & top5ThreadWout252620$Date <= as.Date("2007-01-05"),]$leisure)
#[1] 1.63


mean(top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2009-06-01") & top5ThreadWout252620$Date <= as.Date("2009-09-30"),]$leisure)

#[1] 2.022609
mean(top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2009-01-05") & top5ThreadWout252620$Date <= as.Date("2009-05-31"),]$leisure)

#[1] 0.9746207
mean(top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2009-11-01") & top5ThreadWout252620$Date <= as.Date("2010-01-05"),]$leisure)
#[1] 1.162333

median(top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2009-06-01") & top5ThreadWout252620$Date <= as.Date("2009-09-30"),]$leisure)
#[1] 1.545
median(top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2009-01-05") & top5ThreadWout252620$Date <= as.Date("2009-05-31"),]$leisure)
#[1] 0
median(top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2009-12-01") & top5ThreadWout252620$Date <= as.Date("2010-01-05"),]$leisure)
#[1] 0


mean(top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2010-06-01") & top5ThreadWout252620$Date <= as.Date("2010-09-30"),]$leisure)

#[1] 1.201268
mean(top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2010-01-05") & top5ThreadWout252620$Date <= as.Date("2010-05-31"),]$leisure)

#[1] 0.7748571
mean(top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2010-11-01") & top5ThreadWout252620$Date <= as.Date("2011-01-05"),]$leisure)
#[1] 1.835769

median(top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2010-06-01") & top5ThreadWout252620$Date <= as.Date("2010-09-30"),]$leisure)
#[1] 1.545
median(top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2010-01-05") & top5ThreadWout252620$Date <= as.Date("2010-05-31"),]$leisure)
#[1] 0
median(top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2010-12-01") & top5ThreadWout252620$Date <= as.Date("2011-01-05"),]$leisure)
#[1] 0

#CORRECT.....................................
#>............>>>>>>>>>>...........>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


#For a single thread 127115
mean(thread127115[thread127115$Date >= as.Date("2006-01-05") & thread127115$Date <= as.Date("2006-05-31"),]$leisure)
#[1] 1.948571
mean(thread127115[thread127115$Date >= as.Date("2009-06-01") & thread127115$Date <= as.Date("2009-09-30"),]$leisure)
#[1] 2.899615
mean(thread127115[thread127115$Date >= as.Date("2009-01-05") & thread127115$Date <= as.Date("2009-05-31"),]$leisure)
#[1] 2.143023
mean(thread127115[thread127115$Date >= as.Date("2009-12-01") & thread127115$Date <= as.Date("2010-01-05"),]$leisure)
#[1] 2.463
median(thread127115[thread127115$Date >= as.Date("2009-06-01") & thread127115$Date <= as.Date("2009-09-30"),]$leisure)
#[1] 2.895
median(thread127115[thread127115$Date >= as.Date("2009-01-05") & thread127115$Date <= as.Date("2009-05-31"),]$leisure)
#[1] 1.69
median(thread127115[thread127115$Date >= as.Date("2009-12-01") & thread127115$Date <= as.Date("2010-01-05"),]$leisure)
#[1] 2.695
##################################################################################################################################################




#getting more than median number of authors that posts in top 6 threads
countPerAuthor = aggregate(cbind(count = wc0Webforum$AuthorID) ~ wc0Webforum$AuthorID, data = wc0Webforum, FUN = function(x){NROW(x)})
countPerAuthorInTop6 = aggregate(cbind(count = top5ThreadWout252620$AuthorID) ~ top5ThreadWout252620$AuthorID, data = top5ThreadWout252620, FUN = function(x){NROW(x)})

postsMoreThanMedianAuthorInTop6 = countPerAuthorInTop6[countPerAuthorInTop6$count > median(countPerAuthorInTop6$count),]

dataWhereMoreNoOfAuthorIsMany = as.data.frame(top5ThreadWout252620[(top5ThreadWout252620$AuthorID %in% postsMoreThanMedianAuthorInTop6$`top5ThreadWout252620$AuthorID`), ])








#found out the author 143126 that has the most home attribute in thread 472752 and compare it with the rest of the threads he/she is in top6threads then compare it with wc0_mrthmednP...
#found out that he/she is only posts once so it is not enough to make a conclusion and the mean for home in thread 472752 is only 0.27 compared with thread 532649 with 0.4644574





meanForAuthorsinTop6 = aggregate(top6ThreadwMostPostsFullData[18:32], list(top6ThreadwMostPostsFullData$AuthorID), mean)  #getting the number of authors and their mean

tmp = aggregate(top6ThreadwMostPostsFullData[3], list(top6ThreadwMostPostsFullData$AuthorID), length) #getting the frequency of posts per authorID to see how often they post
meanForAuthorsinTop6$AuthorsFreqPost = tmp$AuthorID





#by(thread252620, thread252620$AuthorID, function(df) mean(df$clout))


by(top6ThreadwMostPostsFullData, top6ThreadwMostPostsFullData$ThreadID, function(df) mean(df$QMark))  #gets the thread that has the most question asked and returns that thread 145223 has the most question asked.
#then see the mean for authors in Top6 threads, then pick








list = data.frame(Date=as.Date(character())
                 ) 
listForEndDate = data.frame(Date=as.Date(character())
                 ) 

webforum$Date = as.Date(webforum$Date)


popular = as.data.frame(as.table(by(webforum, webforum$ThreadID, function(df) NROW(df$PostID))))

popular$webforum.ThreadID = as.numeric(levels(popular$webforum.ThreadID))[popular$webforum.ThreadID]


for(i in 1:nrow(popular)){
  thread = popular[i,"webforum.ThreadID"]
  threadData = webforum[webforum$ThreadID==thread,]
  threadData$Date = as.Date(threadData$Date)
  #threadRow = NROW(webforum[webforum$ThreadID==thread,])
  abc = threadData[order(threadData$Date)[1],4]
  list[i,] = abc
}

list$ThreadID = popular$webforum.ThreadID
list$PostCount = popular$Freq

for(i in 1:nrow(popular)){
  thread = popular[i,"webforum.ThreadID"]
  threadData = webforum[webforum$ThreadID==thread,]
  threadData$Date = as.Date(threadData$Date)
  #threadRow = NROW(webforum[webforum$ThreadID==thread,])
  abc = threadData[order(threadData$Date, decreasing = T)[1],4]
  listForEndDate[i,] = abc
}
list$`End-Post` = listForEndDate$Date


popularityByYear = as.data.frame(as.table(by(list, year(list$`End-Post`), function(df) sum(df$PostCount))))
list$diff_time = difftime(list$`End-Post`, list$Date, units = c("days"))
list$diff_time = as.numeric(list$diff_time)

list = transform(list, PostPerDay = PostCount / diff_time)


#ggplot(list, aes(x=list$diff_time, y=list$PostCount, fill = as.factor(list$ThreadID))) + geom_bar(stat = "identity")



#-----------------------------------------------------------------------------------------------------------

listAuthors = data.frame(Date=as.Date(character())
        )
webforum$Date = as.Date(webforum$Date)

authorsWithoutAnon = webforum[webforum$AuthorID!=-1,]

authorsPostCountWOutAnon = as.data.frame(as.table(by(authorsWithoutAnon, authorsWithoutAnon$AuthorID, function(df) NROW(df$PostID))))

authorsPostCountWOutAnon$authorsWithoutAnon.AuthorID = as.numeric(levels(authorsPostCountWOutAnon$authorsWithoutAnon.AuthorID))[authorsPostCountWOutAnon$authorsWithoutAnon.AuthorID]

for(i in 1:nrow(authorsPostCountWOutAnon)){
  author = authorsPostCountWOutAnon[i,"authorsWithoutAnon.AuthorID"]
  authorData = webforum[webforum$AuthorID==author,]
  authorData$Date = as.Date(authorData$Date)
  def = authorData[order(authorData$Date)[1],4]
  listAuthors[i,] = def
}
listAuthors$AuthorID = authorsPostCountWOutAnon$authorsWithoutAnon.AuthorID
listAuthors$PostCount = authorsPostCountWOutAnon$Freq


authorsPostFreqPerYear = as.data.frame(as.table(by(listAuthors, year(listAuthors$Date), function(df) sum(df$PostCount))))


top6ThreadwMostPostsFullDataAnon = top6ThreadwMostPostsFullData[top6ThreadwMostPostsFullData$AuthorID==-1,]
top6ThreadwMostPostsFullDataWOutAnon = top6ThreadwMostPostsFullData[top6ThreadwMostPostsFullData$AuthorID!=-1,]

#---------------------------------------------------------------------------------------------------------------------------------
#make night and day for top5 threads
top6ThreadwMostPostsFullData$DateTime = as.POSIXct(paste(top6ThreadwMostPostsFullData$Date, top6ThreadwMostPostsFullData$Time), format = "%Y-%m-%d %H:%M")
top6ThreadwMostPostsFullData$DateTime <- as.POSIXct(top6ThreadwMostPostsFullData$DateTime)
top6ThreadwMostPostsFullData$Date <- as.Date(top6ThreadwMostPostsFullData$DateTime)
top6ThreadwMostPostsFullData$Time <- format(as.POSIXct(top6ThreadwMostPostsFullData$DateTime) ,format = "%H:%M") 

top6ThreadwMostPostsFullData$dayNight <- ifelse(top6ThreadwMostPostsFullData$Time >= as.character("06.00") & top6ThreadwMostPostsFullData$Time < as.character("18.00"), 'day', 'night')

aggregate(top6ThreadwMostPostsFullData[18:32], list(top6ThreadwMostPostsFullData$dayNight), mean)

#-----------------------------------------------------------------------------------------------------------------------------
top6ThreadwMostPostsFullData$Day = weekdays(top6ThreadwMostPostsFullData$Date)  #make a new column for time of day

top6ThreadwMostPostsFullData$WeekendVsWeekday = ifelse(top6ThreadwMostPostsFullData$Day == as.character("Saturday") | top6ThreadwMostPostsFullData$Day == as.character("Sunday"), 'Weekend', 'Weekday')

by(top6ThreadwMostPostsFullData, top6ThreadwMostPostsFullData$WeekendVsWeekday, function(df) NROW(df$PostID))
aggregate(top6ThreadwMostPostsFullData[18:32], list(top6ThreadwMostPostsFullData$WeekendVsWeekday), mean)

#-----------------------------------------------------------------------------------------------------------------------------
attach(top6ThreadwMostPostsFullData)
classifyTop6OnThreadAndDay = top6ThreadwMostPostsFullData %>%
                                   group_by(top6ThreadwMostPostsFullData$'ThreadID', top6ThreadwMostPostsFullData$'Day') %>%
                                   summarise(a_pos=mean(posemo),
                                             a_neg=(mean(negemo)),
                                             a_leisure=(mean(leisure)),
                                             a_analytic=(mean(Analytic)),
                                             a_anx=(mean(anx)),
                                             a_anger=(mean(anger)),
                                             a_work=(mean(work)),
                                             a_social=(mean(social)),
                                             a_friend=(mean(friend)),
                                             a_home=(mean(home)),
                                             a_clout=(mean(Clout)),
                                             a_authentic=(mean(Authentic)),
                                             a_tone=(mean(Tone)),
                                             a_ppron=(mean(ppron)),
                                             a_i=(mean(i)),
                                             a_we=(mean(we)),
                                             a_you=(mean(you)),
                                             a_shehe=(mean(shehe)),
                                             a_they=(mean(they)),
                                             a_number=(mean(number)),
                                             a_affect=(mean(affect)),
                                             a_family=(mean(family)),
                                             a_money=(mean(money)),
                                             a_relig=(mean(relig)),
                                             a_swear=(mean(swear)),
                                             a_qmark=(mean(QMark)),
                                             a_postcount=(NROW(PostID))
                                             )

by(classifyTop6OnThreadAndDay, classifyTop6OnThreadAndDay$`top6ThreadwMostPostsFullData$Day`, function(df) sum(df$a_postcount)) #check the number of posts in total for each day and found out that friday, saturday and sunday has the most posts









