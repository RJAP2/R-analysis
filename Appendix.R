install.packages("zoo")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("plyr")
install.packages("dplyr")
install.packages("scales")


library(zoo)
library(lubridate)
library(ggplot2)
library(plyr)
library(dplyr)
library(scales)

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

                                                                    #Subset data

#read the webforum data and put it to data frame
webforum = read.csv("webforum.csv", header = TRUE)


#eliminate the 0 word count
wc0Webforum = webforum[!(webforum$WC==0),]


#make the date column as a date type
wc0Webforum$Date = as.Date(wc0Webforum$Date)


#find out how many posts per thread
countPerThread = aggregate(cbind(count = wc0Webforum$ThreadID) ~ wc0Webforum$ThreadID, data = wc0Webforum, FUN = function(x){NROW(x)})


#get the threads where the number of posts is more than the median
postsMoreThanMedian = countPerThread[countPerThread$count > median(countPerThread$count),] 


#get the full data of each threadID according to PostsMoreThanMedian
wc0MrThMednWebforum = as.data.frame(wc0Webforum[(wc0Webforum$ThreadID %in% postsMoreThanMedian$'wc0Webforum$ThreadID'), ])


#get the number of authors per thread, so find out how many number of unique authors there are in each thread
nOfAuthPerThread = as.data.frame(as.table(by(wc0MrThMednWebforum, wc0MrThMednWebforum$ThreadID, function(df) length(unique(df$AuthorID))))) 


#get the number of threads where there is more authors than the median number of authors in a thread
nOfAuthPerThreadMoreThanMedian = nOfAuthPerThread[nOfAuthPerThread$Freq > median(nOfAuthPerThread$Freq),] 


#get the full data of each authorID according to nOfAuthPerThreadMoreThanMedian
wc0_MrThMednP_and_A_Webforum = as.data.frame(wc0MrThMednWebforum[(wc0MrThMednWebforum$ThreadID %in% nOfAuthPerThreadMoreThanMedian$wc0MrThMednWebforum.ThreadID), ])


#get the number of posts per thread
numOfPostsPerThread = as.data.frame(as.table(by(wc0_MrThMednP_and_A_Webforum, wc0_MrThMednP_and_A_Webforum$ThreadID, function(df) length(df$PostID))))


#get the graph based on number of posts per thread
PostCountGraph = ggplot(numOfPostsPerThread, aes(reorder(numOfPostsPerThread$wc0_MrThMednP_and_A_Webforum.ThreadID, numOfPostsPerThread$Freq), numOfPostsPerThread$Freq)) + geom_bar(width = 1, stat= "identity") + labs(x="Number of Post Per Thread",y="Thread ID") + theme_light() + coord_flip() 

#GRAPH
dev.new() 
PostCountGraph

#get the top 6 threads w most posts because the fifth and the sixth most posts have the same value
top6ThreadwMostPosts = numOfPostsPerThread[order(numOfPostsPerThread$Freq,decreasing=T)[1:6],]


#get the top 6 threads' full data
top6ThreadwMostPostsFullData = as.data.frame(wc0_MrThMednP_and_A_Webforum[(wc0_MrThMednP_and_A_Webforum$ThreadID %in% top6ThreadwMostPosts$wc0_MrThMednP_and_A_Webforum.ThreadID),])


                                                                    #Analysing Analytics

#get the thread which is the most analytical based on median since a couple of threads has outliers such as thread 127115
meanOfAnalyticForEachTop6Thread = as.data.frame(as.table(by(top6ThreadwMostPostsFullData, top6ThreadwMostPostsFullData$ThreadID, function(df) as.numeric(specify_decimal(mean(df$Analytic), 3))))) 


#get the mean of analytics of the top 6 threads
#GRAPH
#bar graph for analytics for top 6 threads
bpFormeanOfAnalyticForEachTop6Thread = ggplot(meanOfAnalyticForEachTop6Thread, aes(x="", y=meanOfAnalyticForEachTop6Thread$Freq, fill=meanOfAnalyticForEachTop6Thread$top6ThreadwMostPostsFullData.ThreadID)) + geom_bar(width = 1, stat= "identity")

#make a piechart
pieFormeanOfAnalyticForEachTop6Thread = bpFormeanOfAnalyticForEachTop6Thread + coord_polar("y", start=0) + geom_text(aes(y = meanOfAnalyticForEachTop6Thread$Freq/2 + c(0, cumsum(meanOfAnalyticForEachTop6Thread$Freq)[-length(meanOfAnalyticForEachTop6Thread$Freq)]), label = meanOfAnalyticForEachTop6Thread$Freq), size=5) + labs(x="", y="") + scale_fill_discrete("Threads")

dev.new() 
pieFormeanOfAnalyticForEachTop6Thread



#get the thread where threadID is 127115
thread127115 = top6ThreadwMostPostsFullData[top6ThreadwMostPostsFullData$ThreadID == 127115,]


#statistics of number of posts per year in thread 127115 since it is the most analytical thread
thread127115findTopPostPerYear = as.data.frame(as.table(by(thread127115, year(thread127115$Date), function(df) NROW(df$PostID))))



dev.new()
#bar graph based on thread127115findTopPostPerYear
ggplot(thread127115findTopPostPerYear, aes(x=thread127115findTopPostPerYear$year.thread127115.Date., y=thread127115findTopPostPerYear$Freq)) + geom_bar(stat = "identity") + labs(x="Year", y="Number of Posts")


#pick year 2009 to be analyzed since it has the most posts
thread127115in2009 = thread127115[thread127115$Date >= as.Date("2009-01-01") & thread127115$Date <= as.Date("2009-12-31"),]


#combine date and time columns
thread127115in2009$DateTime = as.POSIXct(paste(thread127115in2009$Date, thread127115in2009$Time), format = "%Y-%m-%d %H:%M")

#GRAPH
dev.new()

#graph to show which authors have posted the most
qplot(thread127115in2009$DateTime, thread127115in2009$Analytic, data=thread127115in2009, size=4, color=as.factor(thread127115in2009$AuthorID)) + labs(x="Year", y="Number of Posts")  + scale_colour_discrete(name = "Authors")


#get the data for the author with most posts in thread 127115
author47875 = top6ThreadwMostPostsFullData[top6ThreadwMostPostsFullData$AuthorID == 47875,]


#the mean can be seen that author 47875 in thread 127115 is the most analytical and this supports the thread 127115 that is the most analytical thread.
meanOfAnalyticOfAuthr47875 = as.data.frame(as.table(by(author47875, author47875$ThreadID, function(df) as.numeric(specify_decimal(mean(df$Analytic), 3)))))  


#to see which threads that the author 47875 has posted
author47875PostCountPerThread = as.data.frame(as.table(by(author47875, author47875$ThreadID, function(df) NROW(df$PostID)))) 


#get the mean of all the attributes in the top 6 threads
summaryOfTop6Threads = aggregate(top6ThreadwMostPostsFullData[18:32], list(top6ThreadwMostPostsFullData$ThreadID), mean)  


#get the posts in thread 472752
thread472752 = top6ThreadwMostPostsFullData[top6ThreadwMostPostsFullData$ThreadID==472752,]


#get the posts in thread 145223
thread145223 = top6ThreadwMostPostsFullData[top6ThreadwMostPostsFullData$ThreadID==145223,]


#t-test to compare the posemo in thread 472752 to the rest of the thread
t.test(thread472752$posemo, top6ThreadwMostPostsFullData[top6ThreadwMostPostsFullData$ThreadID!=472752,]$posemo, "greater", conf.level = 0.95)
#t-test to compare the posemo in thread 472752 based on the posts that the author 47875 made to the rest of the posts that he/she made
t.test(author47875[author47875$ThreadID==472752,]$posemo, author47875[author47875$ThreadID!=472752,]$posemo, "greater", conf.level = 0.95) 

#t-test to compare the anxiety in thread 145223 to the rest of the thread
t.test(thread145223$anx, top6ThreadwMostPostsFullData[top6ThreadwMostPostsFullData$ThreadID!=145223,]$anx, "greater", conf.level = 0.95)
#t-test to compare the anxiety in thread 145223 based on the posts that the author 47875 made to the rest of the posts that he/she made
t.test(author47875[author47875$ThreadID==145223,]$anx, author47875[author47875$ThreadID!=145223,]$anx, "greater", conf.level = 0.95) 


#get the author 39170 which has the most posts in thread 472752
author39170 = top6ThreadwMostPostsFullData[top6ThreadwMostPostsFullData$AuthorID==39170,]


#t-test to compare the analytics in thread 127115 based on the posts that the author 47875 made to the rest of the posts that he/she made
t.test(author39170[author39170$ThreadID==127115,]$Analytic, author39170[author39170$ThreadID!=127115,]$Analytic, "greater", conf.level = 0.95) 



                                                                    #Analysing Leisure


#we decided to remove thread 252620 because the thread is only active between 3 months only which is not suitable in choosing the most posts in a year
top5ThreadWout252620 = top6ThreadwMostPostsFullData[!(top6ThreadwMostPostsFullData$ThreadID==252620),]


#get the posts with thread 252620
thread252620 = top6ThreadwMostPostsFullData[top6ThreadwMostPostsFullData$ThreadID == 252620,]


#get the post count of thread 252620 per month
thread252620PostCount = as.data.frame(as.table(by(thread252620, as.yearmon(thread252620$Date), function(df) NROW(df$PostID))))



#GRAPH
dev.new()

#plot the post count for th thread 252620 to show that the thread is focused on certain time frame
ggplot(thread252620PostCount, aes(x=thread252620PostCount$as.yearmon.thread252620.Date., y=thread252620PostCount$Freq)) + geom_bar(stat = "identity") + labs(x="Months",y="Number of Posts") + ggtitle("Thread 252620 Post Count")  


#define summer break period
summerBreakPeriod = top5ThreadWout252620[month(top5ThreadWout252620$Date)==6 | month(top5ThreadWout252620$Date)==7 | month(top5ThreadWout252620$Date)==8 | month(top5ThreadWout252620$Date)==9,]
#define non summer break period
notSummerBreakPeriod = top5ThreadWout252620[month(top5ThreadWout252620$Date)!=6 | month(top5ThreadWout252620$Date)!=7 | month(top5ThreadWout252620$Date)!=8 | month(top5ThreadWout252620$Date)!=9,]


#perform a t-test on the summer break period and no summer break period
t.test(summerBreakPeriod$leisure,notSummerBreakPeriod$leisure, "greater",conf.level = 0.95) 


#get the number of posts per year and the maximum is 2005 where it has the most number of posts
noOfPostsPerYearTop5 = as.data.frame(as.table(by(top5ThreadWout252620, year(top5ThreadWout252620$Date), function(df) length(df$PostID)))) 


#number of posts in top 6 thread without thread 252620
bpFornoOfPostsPerYearTop5 = ggplot(noOfPostsPerYearTop5, aes(x="", y=noOfPostsPerYearTop5$Freq, fill=noOfPostsPerYearTop5$year.top5ThreadWout252620.Date.)) + geom_bar(width = 1, stat= "identity") +  ggtitle("Number of Posts in Top 6 Thread Without Thread 252620") + scale_fill_discrete(name = "Year") + labs(x="", y="")
#make bpFornoOfPostsPerYearTop5 in pie chart
pieFornoOfPostsPerYearTop5 = bpFornoOfPostsPerYearTop5 + coord_polar("y", start=0) + geom_text(aes(y = noOfPostsPerYearTop5$Freq/3 + c(0, cumsum(noOfPostsPerYearTop5$Freq)[-length(noOfPostsPerYearTop5$Freq)]), label = noOfPostsPerYearTop5$Freq), size=5) 

#GRAPH
dev.new()
pieFornoOfPostsPerYearTop5


#take the summer break on 2006
summerBreakPeriod2006 = top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2006-06-01") & top5ThreadWout252620$Date <= as.Date("2006-09-30"),] 
#take the period where it is not summer break on 2006
notsummerBreakPeriod2006  = top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2006-01-05") & top5ThreadWout252620$Date <= as.Date("2006-05-31"),] 

#perform a t-test in 2006 on summer break vs not summer break period
t.test(summerBreakPeriod2006$leisure,notsummerBreakPeriod2006$leisure, "greater",conf.level = 0.95) 

#take the summer break on 2009
summerBreakPeriod2009 = top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2009-06-01") & top5ThreadWout252620$Date <= as.Date("2009-09-30"),]
#take the period where it is not summer break on 2009
notsummerBreakPeriod2009  = top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2009-01-05") & top5ThreadWout252620$Date <= as.Date("2009-05-31"),]

#perform a t-test in 2009 on summer break vs not summer break period
t.test(summerBreakPeriod2009$leisure,notsummerBreakPeriod2009$leisure, "greater",conf.level = 0.95) 



                                                                    #Analysing anonymous

#GRAPH
dev.new()

#Plot the anger and date and the color to be negemo level
qplot(top6ThreadwMostPostsFullData$Date, top6ThreadwMostPostsFullData$anger, size = as.factor(round_any(top6ThreadwMostPostsFullData$negemo, 1)), color = as.factor(round_any(top6ThreadwMostPostsFullData$negemo, 1))) +labs(x="Year", y="Anger")


#get the posts where it is made by anon
top6ThreadwMostPostsFullDataAnon = top6ThreadwMostPostsFullData[top6ThreadwMostPostsFullData$AuthorID==-1,]


#get the posts without anonymous and also without thread 252620
top5ThreadwMostPostsFullDataWOutAnonWOut252620 = top5ThreadWout252620[top5ThreadWout252620$AuthorID!=-1,]


#get the data of anonymous in top 6 threads without thread 252620
anonFreqWOut252620 = top6ThreadwMostPostsFullDataAnon[top6ThreadwMostPostsFullDataAnon$ThreadID!=252620,] 


#frequency of anonymous posts in the top 6 threads without thread 252620
freqAnonTop6ThreadsWout252620 = as.data.frame(as.table(by(anonFreqWOut252620, year(anonFreqWOut252620$Date), function(df) NROW(df$PostID) )))

#GRAPH
dev.new()
ggplot(freqAnonTop6ThreadsWout252620, aes(x=freqAnonTop6ThreadsWout252620$year.anonFreqWOut252620.Date., y=freqAnonTop6ThreadsWout252620$Freq, group = 1)) + geom_point() + geom_line() + labs(x="Year", y="Post Count")


#get the data of the top 6 threads between 2005 to end of 2006 including anonymous
anonIn2002To2006 = top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2002-01-01") & top5ThreadWout252620$Date <= as.Date("2006-12-31"),]
#get the data of the top 6 threads between 2005 to end of 2006 including anonymous
NonAnonIn2002To2006 = top5ThreadwMostPostsFullDataWOutAnonWOut252620[top5ThreadwMostPostsFullDataWOutAnonWOut252620$Date >= as.Date("2002-01-01") & top5ThreadwMostPostsFullDataWOutAnonWOut252620$Date <= as.Date("2006-12-31"),]  


#get the data of the top 6 threads between 2007 to end of 2011 including anonymous
anonIn2007To2011 = top5ThreadWout252620[top5ThreadWout252620$Date >= as.Date("2007-01-01") & top5ThreadWout252620$Date <= as.Date("2011-12-31"),] 
#get the data of the top 6 threads between 2007 to end of 2011 including anonymous
NonanonIn2007To2011 = top5ThreadwMostPostsFullDataWOutAnonWOut252620[top5ThreadwMostPostsFullDataWOutAnonWOut252620$Date >= as.Date("2007-01-01") & top5ThreadwMostPostsFullDataWOutAnonWOut252620$Date <= as.Date("2011-12-31"),]  



#Performing t.tests.
t.test(anonIn2002To2006$negemo, NonAnonIn2002To2006$negemo, "less", conf.level = 0.95)
t.test(anonIn2002To2006$anger, NonAnonIn2002To2006$anger, "less", conf.level = 0.95)

t.test(anonIn2007To2011$anger, NonanonIn2007To2011$anger, "greater", conf.level = 0.95)
t.test(anonIn2007To2011$negemo, NonanonIn2007To2011$negemo, "greater", conf.level = 0.95)

