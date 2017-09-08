groupbyThread = function(){
  c = aggregate(cbind(count = d$ThreadID) ~ d$ThreadID, 
                       data = d, 
                       FUN = function(x){NROW(x)})
  return(as.data.frame(c))
}

#groupbyThread()






#library(zoo)
#d = webforum[!(WC==0),]#eliminate word count = 0
#d$Date = as.Date(d$Date)#change the column to date values
#dMonthly = aggregate(d$posemo ~ as.yearmon(d$Date), d, mean)#group the data about positive emotions and average it per month 
#fit = lm(dMonthly$`d$posemo` ~ dMonthly$`as.yearmon(d$Date)`)#make a linear model
#dmonthPlot = qplot(dMonthly$`as.yearmon(d$Date)`,dMonthly$`d$posemo`)#plot the graph
#abline(fit)#plot the line of best fit




#count number of posts per month
#countPerMon = aggregate(d$PostID ~ as.yearmon(d$Date), d, FUN = function(x) length(x))
#mean(countPerMon$freq) = 184.5

#FinalFreq where Freq > mean
#freqFinal = countPerMon[countPerMon$freq > mean(countPerMon$freq),]

#getting the posemo of freqfinal
#FinalPosemo = as.data.frame(dMonthly[(dMonthly$`as.yearmon(d$Date)` %in% freqFinal$as.yearmon.d.Date.), ])

#fitPosemo = lm(FinalPosemo$`d$posemo` ~ FinalPosemo$`as.yearmon(d$Date)`)
#PosemoPlot= plot(FinalPosemo$`as.yearmon(d$Date)`,FinalPosemo$`d$posemo`)




#countPerThread = aggregate(cbind(count = d$ThreadID) ~ d$ThreadID, 
               #data = d, 
               #FUN = function(x){NROW(x)})
#postCountFinal = countPerThread[countPerThread$count > mean(countPerThread$count),]
#dQuestion1 = as.data.frame(d[(d$ThreadID %in% postCountFinal$'d$ThreadID'), ]) #DATA WHERE POST PER THREAD IS More THAN MEAN POST PER THREAD







#group data and find the dates which are unique to a particular thread
#by(dQuestion1, dQuestion1$ThreadID, function(df) unique(df$Date))

#group data and find the author which are unique to a particular thread
#by(dQuestion1, dQuestion1$ThreadID, function(df) unique(df$AuthorID))


#number of unique authors per thread
#nOfAuthPerThread = as.data.frame(as.table(by(dQuestion1, dQuestion1$ThreadID, function(df) length(unique(df$AuthorID)))))


#nOfAuthPerThreadMoreThanMean = nOfAuthPerThread[nOfAuthPerThread$Freq > mean(nOfAuthPerThread$Freq),]


#c = as.data.frame(dQuestion1[(dQuestion1$ThreadID %in% nOfAuthPerThreadMoreThanMean$dQuestion1.ThreadID), ]) #data in dQuestion1 where threadID is equal to nOfAuthPerThread more than mean





#auth118 = c[c$AuthorID == 118,]
#auth118anger = as.data.frame(as.table(by(auth118, auth118$ThreadID, function(df) mean(df$anger)))) #getting the 
#auth118anger[auth118anger$Freq == max(auth118anger$Freq),]






#by(c, c$ThreadID, function(df) length(unique(df$Date))) = 287 max




dev.new()
plot(thread127115$Date, thread127115$anger)
dev.new()
plot(thread145223$Date, thread145223$anger)
dev.new()
plot(thread252620$Date, thread252620$anger)
dev.new()
plot(thread472752$Date, thread472752$anger)
dev.new()
plot(thread532649$Date, thread532649$anger)
dev.new()
plot(thread249001_highestAnger$Date, thread249001_highestAnger$anger)








#----------------------------------------------------------------------------------------------------------------------------------------------------






#numOfPostsPerThread = as.data.frame(as.table(by(c, c$ThreadID, function(df) length(df$PostID))))

#top5ThreadwMostPosts = numOfPostsPerThread[order(numOfPostsPerThread$Freq,decreasing=T)[1:5],]


#top5ThreadwMostPostsFullData = as.data.frame(c[(c$ThreadID %in% top5ThreadwMostPosts$c.ThreadID),])



#checking for NOV-JAN and JUNE-AUGUST if leisure posts increase in all threads

#library(lubridate)
#by(top5ThreadwMostPostsFullData, year(top5ThreadwMostPostsFullData$Date), function(df) length(df$PostID)) which equals to 557 for 2005 as maximum no of posts and pick 2006 for extra data since it is the second most no of posts
#postsin20052006 = c[c$Date >= as.Date("2005-01-01") & c$Date <= as.Date("2006-12-31"),]
#postsInNovToJan2005 = postsin20052006[postsin20052006$Date>= as.Date("2005-11-01") & postsin20052006$Date <= as.Date("2006-01-05"),]

#plot using ggplot2 for 2005-2006 (for now no need)
#ggplot(postsin20052006, aes(x = postsin20052006$Date, y = postsin20052006$leisure)) + 
                      #geom_line() +
                      #theme_bw() +
                      #labs(x = "Month", y = "Leisure Stats") +
                      #scale_x_date(date_breaks = "2 month", date_labels =  "%b %Y")
#get the value of posts between jan to oct in 2005 to exclude posts between nov 2005 and jan 2006
#postsInJanToOct2005 = postsin20052006[postsin20052006$Date>= as.Date("2005-01-01") & postsin20052006$Date <= as.Date("2005-10-31"),]

#MEAN FOR LEISURE POSTS IN 5 JAN-MAY 2005 IS 0.7262736
#MEAN FOR LEISURE POSTS IN JUN-SEP IS 0.8161957
#MEAN FOR LEISURE POSTS IN NOV 2005-5 JAN 2006 IS 1.283363




#checking for DEC-JAN and MAY-SEP if leisure posts increase in thread 127115 (we took this thread because it is one of the threads w/ most posts and distributed evenly in terms of date)

#by(thread127115, year(thread127115$Date), function(df) length(df$PostID))  #which tells us in what year there is the most posts to analyze

#postsin2009thread127115 = thread127115[thread127115$Date >= as.Date("2009-01-01") & thread127115$Date <= as.Date("2010-02-01"),] #then we pick 2009 to early 2010


#postsin2009thread127115JanToApr = postsin2009thread127115[postsin2009thread127115$Date>= as.Date("2009-01-01") & postsin2009thread127115$Date <= as.Date("2009-04-30"),]
#mean(postsin2009thread127115JanToApr$leisure) = 1.939062

#postsin2009thread127115DecToJan2010 = postsin2009thread127115[postsin2009thread127115$Date>= as.Date("2009-12-01") & postsin2009thread127115$Date <= as.Date("2010-01-05"),]
#mean(postsin2009thread127115DecToJan2010$leisure) = 2.463

#postsin2009thread127115MayToSep = postsin2009thread127115[postsin2009thread127115$Date>= as.Date("2009-05-01") & postsin2009thread127115$Date <= as.Date("2010-09-20"),]
#mean(postsin2009thread127115MayToSep$leisure) = 2.232821



#see the relationship of negemo and anxious in thread 179689 in year 2006
#thread179689 = c[c$ThreadID == 179689,]
#postsin2006thread179689 = thread179689[thread179689$Date >= as.Date("2006-01-01") & thread179689$Date <= as.Date("2006-12-31"),]
#t179689plot = ggplot(postsin2006thread179689, aes(x = postsin2006thread179689$Date, y = postsin2006thread179689$negemo, size = postsin2006thread179689$anx)) + geom_point() + scale_size(range = c(3, 11))



#regression for best predictor for negemo. Either: anx, anger or swear
#fit = lm(formula = postsin2006thread179689$negemo ~  postsin2006thread179689$anx + postsin2006thread179689$anger + postsin2006thread179689$swear)
#summary(fit)



#getting the most posts by anon in a single thread and see if no of anonymous in a thread is decreasing
anon = d[d$AuthorID==-1,]
anonFreqPost = as.data.frame(as.table(by(anon, anon$ThreadID, function(df) length(df$PostID))))
anonFreqPost[anonFreqPost$Freq == max(anonFreqPost$Freq),]  #= thread = 283958  freq = 184
thread283958highestanon = d[d$ThreadID==283958,]




anonOnlyInThread283958 = thread283958highestanon[thread283958highestanon$AuthorID==-1,]

test = anonOnlyInThread283958[anonOnlyInThread283958$Date <= as.Date("2009-12-31"),]
anonPostsCountInThread283958 = as.data.frame(as.table(by(test, as.yearmon(test$Date), function(df) length(df$PostID))))
View(anonPostsCountInThread283958)
anonPostsCountInThread283958$as.yearmon.test.Date. = as.Date(as.yearmon(anonPostsCountInThread283958$as.yearmon.test.Date.))
str(anonPostsCountInThread283958)

fitAnon = lm(anonPostsCountInThread283958$Freq ~ anonPostsCountInThread283958$as.yearmon.test.Date.)
plot(anonPostsCountInThread283958$as.yearmon.test.Date., anonPostsCountInThread283958$Freq)
abline(fitAnon)
summary(fitAnon)


#same as above except apply it to the whole data
anonFreq = as.data.frame(as.table(by(anon, as.yearmon(anon$Date), function(df) NROW(df$PostID))))
anonFreq$as.yearmon.anon.Date. = as.Date(as.yearmon(anonFreq$as.yearmon.anon.Date.))
plot(anonFreq$as.yearmon.anon.Date., anonFreq$Freq)
#TODO: CHECK WHETHER A DECREASE IN ANON POSTS, DECREASE IN NEGEMO, ANGER, POSEMO ETC


anonFreq2006To2009 = anonFreq[anonFreq$as.yearmon.anon.Date. >= as.Date("2006-07-01") & anonFreq$as.yearmon.anon.Date. <= as.Date("2009-12-31"),]
plot(anonFreq2006To2009$as.yearmon.anon.Date., anonFreq2006To2009$Freq)
anonFit = lm(anonFreq2006To2009$Freq ~ anonFreq2006To2009$as.yearmon.anon.Date.)
abline(anonFit)
summary(anonFit)

anonIn2006To2009 = anon[anon$Date >= as.Date("2006-07-01") & anon$Date <= as.Date("2009-12-31"),]








#check to see if there is a decline in sth after no of anonymous posts dropped in 2006-2009
anonIn2006To2009 = anon[anon$Date >= as.Date("2006-07-01") & anon$Date <= as.Date("2009-12-31"),]
postsIn2006To2009 = d[d$Date >= as.Date("2006-07-01") & d$Date <= as.Date("2009-12-31"),]
meanBetween2006To2009 = as.data.frame(as.table(aggregate(postsIn2006To2009[18:32], postsIn2006To2009[2], mean)))#check the mean for period between 2006-2009 where no of anon posts started to decline
meanFullData = as.data.frame(aggregate(d[18:32], d[2], mean))#we want to check the mean for the full data

mean(meanBetween2006To2009$anger)
#[1] 0.9519681
mean(meanFullData$anger)
#[1] 1.014701
mean(meanBetween2006To2009$negemo)
#[1] 2.121815
mean(meanFullData$negemo)
#[1] 2.257006
mean(meanBetween2006To2009$anx)
#[1] 0.2373792
mean(meanFullData$anx)
#[1] 0.2602676
mean(meanBetween2006To2009$posemo)
#[1] 3.442346
mean(meanFullData$posemo)
#[1] 3.367229                             

#we can conclude from above that anon users bring positivity


#now we check against a refined data frame where there are diverse authors
meanFullDataForDataFrameC = as.data.frame(aggregate(c[18:32], c[2], mean))

mean(meanBetween2006To2009$anger)
#[1] 0.9519681
mean(meanFullDataForDataFrameC$anger)
#[1] 0.8225691
mean(meanBetween2006To2009$negemo)
#[1] 2.121815
mean(meanFullDataForDataFrameC$negemo)
#[1] 1.751093
mean(meanBetween2006To2009$anx)
#[1] 0.2373792
mean(meanFullDataForDataFrameC$anx)
#[1] 0.1774302
mean(meanBetween2006To2009$posemo)
#[1] 3.442346
mean(meanFullDataForDataFrameC$posemo)
#[1] 3.721632

#----------------------------------------------------------------------------------------------------------------------------------------------------



#plot for 2005-2011
#ggplot(top5ThreadwMostPostsFullData, aes(x = top5ThreadwMostPostsFullData$Date, y = top5ThreadwMostPostsFullData$leisure)) + 
  #geom_line() +
  #theme_bw() +
  #labs(x = "Month", y = "Leisure Stats") +
  #scale_x_date(date_breaks = "5 month", date_labels =  "%b %Y")




#posts in 2008-2011
#postsin20082011 = top5ThreadwMostPostsFullData[top5ThreadwMostPostsFullData$Date >= as.Date("2008-01-01") & top5ThreadwMostPostsFullData$Date <= as.Date("2011-12-31"),]
#ggplot(postsin20082011, aes(x = postsin20082011$Date, y = postsin20082011$leisure)) + 
  #geom_line() +
  #theme_bw() +
  #labs(x = "Month", y = "Leisure Stats") +
  #scale_x_date(date_breaks = "2 months", date_labels =  "%b %Y")


bp = ggplot(top5ThreadwMostPostsFullData, aes(x = top5ThreadwMostPostsFullData$Date, y = top5ThreadwMostPostsFullData$anger, color = as.factor(top5ThreadwMostPostsFullData$AuthorID) ,size = top5ThreadwMostPostsFullData$swear)) + geom_point()
t179689plot = ggplot(postsin2006thread179689, aes(x = postsin2006thread179689$Date, y = postsin2006thread179689$negemo, size = postsin2006thread179689$anx)) + geom_point()









#get the number of posts of each thread of each month in a list (not used maybe used later)
#test = by(d, d$ThreadID, function(df) aggregate(df$PostID ~ as.yearmon(df$Date), d, FUN = function(x) length(x)))
