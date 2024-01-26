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

#now let us do the second set of citations
data=read.csv('Search2SystematicReview.csv',header=TRUE)

data.important=data[,c(2:6,9)]
rm(data)
head(data.important)

#uncapitilize the letters
data.important$Publication.Title=tolower(data.important$Publication.Title)
data.important$Publication.Title

#let's deal with DOIs first
isolation=data.important[which(data.important$DOI==""),]
data.process1=data.important[!duplicated(data.important$DOI),]
data.process1=rbind(isolation,data.process1)

#lets now look at the article titles
data.process1$Title=tolower(data.process1$Title)
data.process2=data.process1[!duplicated(data.process1$Title),]

#now check the author duplicates

a=data.process2$Author[which(duplicated(data.process2$Author))]

p=data.process2[which(data.process2$Author %in% a),]

k=p$Author[c(3,4,7)] #4,3,7

which(data.process2$Author %in% k)

data.process2$Author[which(data.process2$Author %in% k)] #43,61,90

data.process2$Author[c(43,61,90)]

data.process3=data.process2[c(-43,-61,-90),]

#save information here for our search
#save all the author names and see who is popping up the most

al_old=read.csv('author_list.csv',header=TRUE)
al_old=al_old[,-1]

a=unlist(strsplit(data.process3$Author,"; "))



b=as.data.frame(table(a))
author_list_search2=b[order(b$Freq,decreasing=TRUE),]

#now add to unduplicated results to previous short results

und_results_1=read.csv('unduplicated_search1_results.csv',header=TRUE)

und_results_1=und_results_1[,-1]

#now let us add the previous results to new results
data.process4=rbind(und_results_1,data.process3)

#now we need to clear these duplicates
isolation=data.process4[which(data.process4$DOI==""),]
data.process5=data.process4[!duplicated(data.process4$DOI),]
data.process5=rbind(isolation,data.process5)

#now we need to deal with article titles

