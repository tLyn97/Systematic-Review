#all we are going to do is to remove duplicates

#upload the cv
data=read.csv('Search1SystematicReview2.csv',header=TRUE)
colnames(data)

data.important=data[,c(2:6,9)]
rm(data)
head(data.important)

#now we need a tool that looks removes duplicates

data.important$Publication.Title=tolower(data.important$Publication.Title)
data.important$Publication.Title

#first duplicated doi
isolation=data.important[which(data.important$DOI==""),]
data.process1=data.important[!duplicated(data.important$DOI),]
data.process1=rbind(isolation,data.process1)

#now remove title duplications
data.process1$Title=tolower(data.process1$Title)
data.process2=data.process1[!duplicated(data.process1$Title),]

#now check if there are author duplicates
a=data.process2$Author[which(duplicated(data.process2$Author))]

data.process2[which(data.process2$Author %in% a),]

data.process3=data.process2[!duplicated(data.process2$Author),]

#save information here for our search
#save all the author names and see who is popping up the most

a=unlist(strsplit(data.process3$Author,"; "))
b=as.data.frame(table(a))
author_list=b[order(b$Freq,decreasing=TRUE),]

#save all the journal names and see who is popping up the most
c=as.data.frame(table(data.process3$Publication.Title))
journal_list=c[order(c$Freq,decreasing=TRUE),]

write.csv(author_list,file="author_list.csv")
write.csv(journal_list,file="journal_list.csv")
write.csv(data.process3,"unduplicated_search1_results.csv")

#yay