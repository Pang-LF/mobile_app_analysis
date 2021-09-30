#import data
getwd()
setwd('/Users/Lotus/Desktop/stevens/job/mobile app store')
description <- read.csv('appleStore_description.csv',na.strings = c(''))
AppleStore <- read.csv('AppleStore.csv',na.strings = c(''))

#check the na data
description[!complete.cases(description),]
AppleStore[!complete.cases(AppleStore),]

head(AppleStore,10)
str(description)
str(AppleStore)

#chr convert to factor
AppleStore$currency <- factor(AppleStore$currency)
AppleStore$prime_genre <- factor(AppleStore$prime_genre)
AppleStore$cont_rating <- factor(AppleStore$cont_rating)
AppleStore$vpp_lic <- factor(AppleStore$vpp_lic)
summary(AppleStore)

#check outliers
Poutliers <- AppleStore[AppleStore$price > 50,]
Poutliers

library(ggplot2)
#visualize price distribution of paid apps
paidapp <- AppleStore[!AppleStore$price==0,]
paidapp <- paidapp[!paidapp$price>50,]
p <- ggplot(data = paidapp,aes(x=price,fill=prime_genre))
p+geom_histogram(binwidth = 1,colour='black')

#p+geom_density(aes(fill=cont_rating),position = 'stack')

#devices and languages
q<-ggplot(data = AppleStore, aes(x=sup_devices.num,y=lang.num,colour=prime_genre,size=price))
q+geom_point()+xlab('Support devices number')+ylab('languages number')+
  ggtitle('devices and languages')+theme(legend.position = c(1,1),legend.justification = c(1,1))

#price distribution get affected by category
v <- ggplot(data=paidapp,aes(x=price))
v+geom_histogram(binwidth = 1,aes(fill=prime_genre),colour='black') +
  facet_grid(prime_genre~.,scale='free')

#affected by content rating
v+geom_histogram(binwidth = 1,aes(fill=cont_rating),colour='black') +
  facet_grid(cont_rating~.,scale='free')

#affected by content rating and genre
v+geom_histogram(binwidth = 1,aes(fill=cont_rating),colour='black') +
  facet_grid(cont_rating~prime_genre,scale='free')

#ratings
v+geom_histogram(binwidth = 1,aes(x=user_rating,fill=prime_genre),colour='black') +
  facet_grid(prime_genre~cont_rating,scale='free')

#adding paid or not column
AppleStore[AppleStore$price>0,'PaidOrNot'] <- 'paid'
AppleStore[AppleStore$price==0,'PaidOrNot'] <- 'unpaid'
AppleStore$PaidOrNot = factor(AppleStore$PaidOrNot)
head(AppleStore)

#ratings affected by paid and free
a <- ggplot(data=AppleStore)
a+geom_histogram(binwidth = 1,aes(x=user_rating,fill=prime_genre),colour='black') +
  facet_grid(PaidOrNot~.,scale='free')

#ratings affected by paid and genre
a+geom_histogram(binwidth = 1,aes(x=user_rating,fill=prime_genre),colour='black') +
  facet_grid(prime_genre~PaidOrNot,scale='free')

#price and ratings
pr <- ggplot(data=paidapp,aes(x=price,y=user_rating,colour=prime_genre))
pr+geom_point()+geom_hline(yintercept = 4.0,colour='Grey',size = 1,linetype=3)
pr+geom_point()+facet_grid(.~prime_genre,scale='free')+ # facet genre
  geom_hline(yintercept = 4.0,colour='Grey',size = 1,linetype=3)

#size and price
sp <- ggplot(data=AppleStore,aes(x=size_bytes,y=price,colour=prime_genre))
sp + geom_point()
#group by genre
sp + geom_point(data=paidapp)+geom_smooth()+facet_grid(.~prime_genre,scale='free')

