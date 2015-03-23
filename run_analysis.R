initial.dir<-getwd()
library('plyr')

setwd(paste(initial.dir,"/UCI HAR Dataset",sep=""))
Headers<-read.table('features.txt')
Headers<-cbind(Headers[2])


gg<-sapply(Headers,is.factor)
Headers[gg]<-lapply(Headers[gg],as.character)


setwd(paste(initial.dir,"/UCI HAR Dataset/test",sep=""))
x.test<-read.table('X_test.txt')
rnames.test<-read.table('subject_test.txt')
colnames(x.test)<-Headers[,1]
x.test<-cbind(rnames.test[,1],x.test)
colnames(x.test)[1]<-c('Test Subject')

setwd(paste(initial.dir,"/UCI HAR Dataset/train",sep=""))
x.train<-read.table('X_train.txt')
rnames.train<-read.table('subject_train.txt')
colnames(x.train)<-Headers[,1]
x.train<-cbind(rnames.train[,1],x.train)
colnames(x.train)[1]<-c('Test Subject')


data<-rbind(x.test,x.train)



vars<-as.list(Headers[,1])

mean.i <- grep("mean", vars)
std.i <- grep("std", vars)

indys<-c(mean.i,std.i)
good.cols<-Headers[indys,1]

tidy.data<-data[good.cols]



setwd(paste(initial.dir,"/UCI HAR Dataset/train",sep=""))
y.train<-read.table('y_train.txt')
setwd(paste(initial.dir,"/UCI HAR Dataset/test",sep=""))
y.test<-read.table('y_test.txt')

activity<-rbind(y.test,y.train)
nums<-c('1','2','3','4','5','6')
acts<-c('walking','walking up','walking down','sitting','standing','laying')
activity<-mapvalues(activity[,1],from = nums,to = acts)


tidy.data<-cbind(data$'Test Subject',activity,tidy.data)
colnames(tidy.data)[1]<-c('Test Subject')
colnames(tidy.data)[2]<-c('activity')




tidy.data<-data.frame(tidy.data)


s<-split(tidy.data,tidy.data$activity)
q<-30

subject<-list()
activities<-list()

for (i in 1:6){
        acti<-s[[i]]
        subj<-split(acti,acti$Test.Subject)

        for(j in 1:q){
                subj.mean<-apply(subj[[j]][3:81],2,mean)
                subject<-rbind(subject,subj.mean)
        }
        s.check <- subject
        
#         row.names(subject)<-seq(along=subject)
#         subject<-list(subject)
        if(i==1){
                activities$walking<- data.frame(subject)
        }else if(i==2){
                activities$walking.up<- data.frame(subject)
        }else if(i==3){
                activities$walking.down<-data.frame(subject)
        }else if(i==4){
                activities$sitting<-data.frame(subject)
        }else if(i==5){
                activities$standing<-data.frame(subject)
        }else if(i==6){
                activities$laying<-data.frame(subject)
        }else{}
            
        subject<-list()
}


activities<-data.frame(activities)
y.df <- data.frame(lapply(activities, as.character), stringsAsFactors=FALSE)
write.table(y.df,file = 'tidy_data.txt',row.name = F)



setwd("../") #go up one directory folder

setwd(initial.dir)
getwd()


x.train[1:4,1:4]
