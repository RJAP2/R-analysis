qplot(thread127115$DateTime, thread127115$Analytic, data=thread127115, size=4)

thread127115findTopPostPerYear = as.data.frame(as.table(by(thread127115, year(thread127115$Date), function(df) NROW(df$PostID)))) #statistics of number of posts per year

ggplot(thread127115findTopPostPerYear, aes(x=thread127115findTopPostPerYear$year.thread127115.Date., y=thread127115findTopPostPerYear$Freq)) + geom_bar(stat = "identity")  #pick 2009 as the most post


qplot(thread127115in2009$DateTime, thread127115in2009$Analytic, data=thread127115in2009, size=4, color=as.factor(thread127115in2009$AuthorID))


#meanOfAnalyticInThread127115 = as.data.frame(as.table(by(thread127115, thread127115$AuthorID, function(df) median(df$Analytic))))

#qplot(meanOfAnalyticInThread127115$thread127115.AuthorID, meanOfAnalyticInThread127115$Freq, data=meanOfAnalyticInThread127115, size=4, color=as.factor(meanOfAnalyticInThread127115$thread127115.AuthorID))

author47875PostCountPerThread = as.data.frame(as.table(by(author47875, author47875$ThreadID, function(df) NROW(df$PostID))))

bpForauthor47875PostCountPerThread = ggplot(author47875PostCountPerThread, aes(x="", y=author47875PostCountPerThread$Freq, fill=author47875PostCountPerThread$author47875.ThreadID)) + geom_bar(width = 1, stat= "identity")
pieForauthor47875PostCountPerThread = bpForauthor47875PostCountPerThread + coord_polar("y", start=0) + geom_text(aes(y = author47875PostCountPerThread$Freq/4 + c(0, cumsum(author47875PostCountPerThread$Freq)[-length(author47875PostCountPerThread$Freq)]), label = author47875PostCountPerThread$Freq), size=5)



author47875Thread472752 = author47875[author47875$ThreadID==472752,]


#get the mean for top6 threads and author 47875
summaryOfTop6Threads = aggregate(top6ThreadwMostPostsFullData[18:32], list(top6ThreadwMostPostsFullData$ThreadID), mean)
summaryOfAuthor47875 = aggregate(author47875[18:32], list(author47875$ThreadID), mean)



#thread 532649 is the highest in social
ggplot(summaryOfTop6Threads, aes(x=as.factor(summaryOfTop6Threads$Group.1), y=summaryOfTop6Threads$social)) + geom_bar(stat = "identity")
ggplot(summaryOfAuthor47875, aes(x=as.factor(summaryOfAuthor47875$Group.1), y=summaryOfAuthor47875$social)) + geom_bar(stat = "identity")




#thread 532649 is the highest in anx
ggplot(summaryOfTop6Threads, aes(x=as.factor(summaryOfTop6Threads$Group.1), y=summaryOfTop6Threads$anx)) + geom_bar(stat = "identity")
ggplot(summaryOfAuthor47875, aes(x=as.factor(summaryOfAuthor47875$Group.1), y=summaryOfAuthor47875$anx)) + geom_bar(stat = "identity")



#thread 532649 is the highest in family
ggplot(summaryOfTop6Threads, aes(x=as.factor(summaryOfTop6Threads$Group.1), y=summaryOfTop6Threads$family)) + geom_bar(stat = "identity")
ggplot(summaryOfAuthor47875, aes(x=as.factor(summaryOfAuthor47875$Group.1), y=summaryOfAuthor47875$family)) + geom_bar(stat = "identity")
