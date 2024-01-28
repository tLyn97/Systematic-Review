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
#we should probably uncapatalize the title of the articles
data.process5$Title=tolower(data.process5$Title)
data.process6=data.process5[!duplicated(data.process5$Title),]

#now we need to check to see if author duplication is indeed true duplication
a=data.process6$Title[which(duplicated(data.process6$Author))]
#they look to be all the same

#so now we need to know what articles were gain from our search
#so remove the original search articles
data.process7=data.process6[!(data.process6$Title %in% und_results_1$Title),]

#now we can add to our author list and journal list
a2=unlist(strsplit(data.process7$Author,'; '))

al_new=as.data.frame(table(a2))

al_new_order=al_new[order(al_new$Freq,decreasing=TRUE),]

together=al_new_order[which(al_new_order[,1] %in% al_old[,1]),]

total_author_list=matrix(NA,nrow(together),2)
total_author_list[,1]=as.character(together[,1])
for(i in 1:nrow(together)){
  total_author_list[i,2]=al_new_order[which(al_new_order[,1]==together[i,1]),2]+al_old[which(al_old[,1]==together[i,1]),2]
}

k=al_new_order[-1*which(al_new_order[,1] %in% al_old[,1]),]
k2=al_old[-1*which(al_old[,1] %in% al_new_order[,1]),]

total_author_list2=rbind(total_author_list,k,k2)

colnames(total_author_list)=c('Author','Frequency')
colnames(k)=c('Author','Frequency')
colnames(k2)=c('Author','Frequency')

total_author_list2=total_author_list2[order(as.numeric(total_author_list2$Frequency),decreasing=TRUE),]

#now do journals
b2=unlist(strsplit(data.process7$Publication.Title,'; '))
jl_new=as.data.frame(table(b2))

b1=read.csv('journal_list.csv',head=TRUE)
b1=b1[,-1]

head(jl_new)

together_jl=jl_new[which(jl_new[,1] %in% b1[,1]),]

total_journal_list=matrix(NA,nrow(together_jl),2)

total_journal_list[,1]=as.character(together_jl[,1])

for (i in 1:nrow(together_jl)){
  total_journal_list[i,2]=jl_new[which(jl_new[,1]==together_jl[i,1]),2]+b1[which(b1[,1]==together_jl[i,1]),2]
}

#now combine them all
k=jl_new[-1*which(jl_new[,1] %in% b1[,1]),]
k2=b1[-1*which(b1[,1] %in% jl_new[,1]),]

total_journal_list2=rbind(total_journal_list,k,k2)

colnames(total_journal_list)=c('Journal','Frequency')
colnames(k)=c('Journal','Frequency')
colnames(k2)=c('Journal','Frequency')

total_journal_list2=total_journal_list2[order(as.numeric(total_journal_list2$Frequency),decreasing=TRUE),]
#we need to keep track of unique citations from expanded search
#removed citations
#added citations
remove_citations_search2=und_results_1[-1*which(und_results_1$Title %in% data.process6$Title),]
#0
add_citations_search2=data.process7

write.csv(file='added_citations_search2.csv',add_citations_search2)
write.csv(file='total_journal_list.csv',total_journal_list2)
write.csv(file='total_author_list.csv',total_author_list2)
