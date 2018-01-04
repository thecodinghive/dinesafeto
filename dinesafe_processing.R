#DineSafe is Toronto Public Health's food safety program that inspects all establishments serving and preparing food. Each inspection results in a pass, a conditional pass or a closed notice.
#Objective: Can information about restaurants inspection be used to predict how long a restaurant stays in business?
#Developer: Fouad Yousif, fouad.yousif@thecodinghive.com
#Date: January 2018

##############Load Required Packages###########
library(plyr)
library(reshape2)
library(ggplot2)
library(ggmap)
library(dummies)
library(mlr)
library(RColorBrewer)
library(wordcloud)


#############  Part I: Data Cleaning / Feature Engineering ##################

#read the dataset
dinesafe<-read.delim("/users/fouadyousif/Downloads/dinesafe_data_export-with-postal-code-2017-10-27-0009 2.csv",sep=",",header=T)

#subset the dataset
dinesafe_time <- dinesafe[c('establishment_id','inspection_date','deleted_at','inspection_status')]
#convert inspection date feature to date
dinesafe_time$inspection_date <- as.Date(dinesafe_time$inspection_date)
#assign NA to empty deleted_at values
dinesafe_time[dinesafe_time$deleted_at=="",c('deleted_at')]<-NA
#convert deleted_at variable feature to date
dinesafe_time$deleted_at<-as.Date(dinesafe_time$deleted_at)

#collapse dates
minimum_inspection_date <-aggregate(dinesafe_time$inspection_date ~ dinesafe_time$establishment_id,FUN=min)
maximum_inspection_date <-aggregate(dinesafe_time$inspection_date ~ dinesafe_time$establishment_id,FUN=max)
maximum_deleted_at_date <-aggregate(dinesafe_time$deleted_at ~ dinesafe_time$establishment_id,FUN=max,na.action=NULL)

#collapse inspection status
collapsed_inspection_status <- data.frame(maximum_deleted_at_date)
collapsed_inspection_status$count <- NA
collapsed_inspection_status$score <- NA
names(collapsed_inspection_status)<-c('establishment_id','inspection_status','inspection_count','inspection_score')
collapsed_inspection_status$inspection_status <- as.character(collapsed_inspection_status$inspection_status)

#assign a value of 0, 50, or 100 for each inspection outcome and take the mean
for (i in unique(collapsed_inspection_status$establishment_id)) {
  tmp <- (na.omit(dinesafe_time[dinesafe_time$establishment_id==i,c('inspection_status')]))
  tmp <- gsub("Closed",0,tmp)
  tmp <- gsub("Conditional Pass",50,tmp)
  tmp <- gsub("Pass",100,tmp)
  collapsed_inspection_status[collapsed_inspection_status$establishment_id==i,c('inspection_count')]<-length(tmp)
  if(length(tmp)>=5){
    collapsed_inspection_status[collapsed_inspection_status$establishment_id==i,c('inspection_score')]<-mean(as.numeric(tmp))
  }else{
    collapsed_inspection_status[collapsed_inspection_status$establishment_id==i,c('inspection_score')]<-NA
  }
  if (any(na.omit(dinesafe_time[dinesafe_time$establishment_id==i,c('inspection_status')])=="Closed") || any(na.omit(dinesafe_time[dinesafe_time$establishment_id==i,c('inspection_status')])=="Conditional Pass")){
    collapsed_inspection_status[collapsed_inspection_status$establishment_id==i,c('inspection_status')]<-c('FAIL')
  }else{
    collapsed_inspection_status[collapsed_inspection_status$establishment_id==i,c('inspection_status')]<-c('PASS')
  }
}

#combine columns
dinesafe_time <- cbind(minimum_inspection_date,maximum_inspection_date[2],maximum_deleted_at_date[2],collapsed_inspection_status[c(2,3,4)])
names(dinesafe_time)<-c("establishment_id","min_inspection_date","max_inspection_date","deleted_at","inspection_status","inspection_count","inspection_score")

#create a location dataframe
dinesafe_location<-dinesafe[c('establishment_id','address','lat','lng','latest_name','latest_type','postal_code')]

#merge time and location
dinesafe_c <- unique(merge(dinesafe_time,dinesafe_location,by=c('establishment_id')))

#only keep restaurants with inspection date > 2011. Dinesafe database was populated in 2010
dinesafe_c <- dinesafe_c[as.Date(dinesafe_c$min_inspection_date) > '2011-01-01',]

#remove restaurants that have the same address, same name and different estabilishment ids
count<-(ddply(dinesafe_c,.(dinesafe_c$address,dinesafe_c$latest_name),nrow))
names(count)<-c('address','latest_name','number')
remove <- data.frame(count[count$number>1,c('latest_name','address')])
remove$id <- paste(remove$latest_name,remove$address,sep='_')
dinesafe_c$id <- paste(dinesafe_c$latest_name,dinesafe_c$address,sep='_')
dinesafe_c <- dinesafe_c[which(!dinesafe_c$id %in% remove$id),]

#only keep the following types
#keep <- c('Bake Shop','Bakery','Food Take Out','Ice Cream / Yogurt Vendors','Restaurant')
keep <-c('Restaurant','Food Take Out')
dinesafe_c <- dinesafe_c[which(dinesafe_c$latest_type %in% keep),]

#split postal code and create fsa feature
dinesafe_c$fsa <- colsplit(dinesafe_c$postal_code, " ",c('fsa','last'))[1]
dinesafe_c[which(dinesafe_c$fsa=="NULL"),c('fsa')]<-NA
dinesafe_c[which(dinesafe_c$fsa==""),c('fsa')]<-NA
dinesafe_c[which(dinesafe_c$fsa=="M5A0A4"),c('fsa')]<-c('M5A')
dinesafe_c$fsa <- as.factor(as.matrix(dinesafe_c$fsa))
  
#define variables to be used in survival analysis
dinesafe_c$closure <- NA
dinesafe_c$time_to_closure <- NA

#add event column
for (i in 1:nrow(dinesafe_c)){
  if(dinesafe_c[i,c('deleted_at')] > '2017-01-01' | is.na(dinesafe_c[i,c('deleted_at')])){
    dinesafe_c[i,c('closure')]<-'open'
  }else{
    dinesafe_c[i,c('closure')]<-'closed'
  }
}

#add time to event column (in days)
for (i in 1:nrow(dinesafe_c)){
  if(dinesafe_c[i,c('deleted_at')] > '2017-01-01' | is.na(dinesafe_c[i,c('deleted_at')])){
    dinesafe_c[i,c('time_to_closure')] <- as.Date('2017-11-01')-as.Date(dinesafe_c[i,c('min_inspection_date')])
  }else{
    dinesafe_c[i,c('time_to_closure')]<- as.Date(dinesafe_c[i,c('deleted_at')])-as.Date(dinesafe_c[i,c('min_inspection_date')])
  }
}

#get rid of restaurants that are in business less than 6 months
dinesafe_c<-dinesafe_c[dinesafe_c$time_to_closure>180,]

#change status of closure to T and F
dinesafe_c$closure<-dinesafe_c$closure=="closed"

#perform enrichment analysis right here
dinesafe_fail_fraction <-data.frame(table(as.matrix(dinesafe_c$fsa),as.character(dinesafe_c$inspection_status))[,1],table(as.matrix(dinesafe_c$fsa),as.character(dinesafe_c$inspection_status))[,2],table(as.matrix(dinesafe_c$fsa),as.character(dinesafe_c$inspection_status))[,1]/(table(as.matrix(dinesafe_c$fsa),as.character(dinesafe_c$inspection_status))[,1]+table(as.matrix(dinesafe_c$fsa),as.character(dinesafe_c$inspection_status))[,2]))
names(dinesafe_fail_fraction)<-c('n_failed','n_total','ratio_failed')

#remove fsa that has <50 restautants
dinesafe_fail_fraction <- dinesafe_fail_fraction[dinesafe_fail_fraction$n_total>50,]
dinesafe_fail_fraction$pvalue <- NULL

#perform a binomial test
for (i in 1:nrow(dinesafe_fail_fraction)){
  #dinesafe_fail_fraction[i,c('pvalue')] <- binom.test(dinesafe_fail_fraction[i,c('n_failed')], dinesafe_fail_fraction[i,c('n_total')], p = mean(dinesafe_fail_fraction[,c('ratio_failed')]),alternative = c("two.sided"),conf.level = 0.95)$p.value
  dinesafe_fail_fraction[i,c('pvalue')] <- prop.test(dinesafe_fail_fraction[i,c('n_failed')], dinesafe_fail_fraction[i,c('n_total')], p= mean(dinesafe_fail_fraction[,c('ratio_failed')]), correct=FALSE,alternative =c("two.sided"))$p.value
}

#adjust p-values
dinesafe_fail_fraction$qvalue <- p.adjust(dinesafe_fail_fraction$pvalue, method = "fdr", n = length(dinesafe_fail_fraction$pvalue))

#add directionality
dinesafe_fail_fraction$direction <- dinesafe_fail_fraction$ratio_failed - mean(dinesafe_fail_fraction[,c('ratio_failed')])

#pull significant records
sig <- dinesafe_fail_fraction[dinesafe_fail_fraction$qvalue<0.05,]

#generate a word cloud with significant enrichments
pal2 <- brewer.pal(8,"Dark2")
wordcloud(row.names(dinesafe_fail_fraction),-log10(dinesafe_fail_fraction$qvalue), scale=c(8,0.6),min.freq=0,max.words=Inf, random.order=FALSE, rot.per=.15,col=pal2)

#plotting inspection score on a map
lat <- as.numeric(as.character(dinesafe_c$lat))
lon <- as.numeric(as.character(dinesafe_c$lng))
inspection_score <- dinesafe_c$inspection_score

#split the inspection score to low, medium and high
to_map <- na.omit(data.frame(inspection_score, lat, lon))
to_map$color <- NA
to_map[to_map$inspection_score==100,c('color')] <- 'high'
to_map[to_map$inspection_score<100 & to_map$inspection_score>=65,c('color')] <- 'medium'
to_map[to_map$inspection_score<65,c('color')] <- 'low'
to_map$color <- as.factor(to_map$color)

colnames(to_map) <- c('inspection_score', 'lat', 'lon','color')
sbbox <- make_bbox(lon = to_map$lon, lat = to_map$lat, f=0.01)
#my_map <- get_map(location= sbbox, maptype = "roadmap", scale = 2, color="bw", zoom = 10)
#use this for downtown: 43.655906, -79.380657 and this for the city c(-79.399871,43.716273)
my_map <- get_googlemap(center=c(-79.380657,43.655906),scale=2,zoom=14,extend="normal",color="bw")
ggmap(my_map) +
  geom_point(data=to_map, aes(x = to_map$lon, y = to_map$lat, colour = to_map$color), 
             size = 3, alpha = 0.5) +
  xlab('Longitude') +
  ylab('Latitude') +
  scale_color_manual(values = c("high" = "green","medium"="yellow","low" = "red"), guide=guide_legend(title = "Inspection Score")) +
  #scale_colour_gradient(low="blue", high="red") +
  #scale_fill_manual(values=wes_palette(n=4, name="GrandBudapest")) +
  #scale_color_gradient(colours = rainbow(5)) +
  #scale_fill_gradient(low="blue", high="red") +
  ggtitle('      Toronto Restaurant Inspection Score') 
  #guides(colour = guide_colorbar(), size = guide_legend(),shape = guide_legend())
  #guides(color=FALSE)

#explore zip codes
fsa <- colsplit(dinesafe_c$postal_code, " ",c('first','last'))[1]
establishment_fsa <- cbind(dinesafe_c$establishment_id,fsa)
names(establishment_fsa) <- c('establishment_id','fsa')
per_fsa_count <- aggregate(establishment_fsa$establishment_id ~ establishment_fsa$fsa, FUN="length")
names(per_fsa_count) <- c('fsa','count')
#plot the distribution of restaurants density per zip/fsa
plot(density(as.numeric(per_fsa_count$count)),xlab="Number of Restaurants Per FSA",main="Restaurants Geographical Density")


################ Part II: Machine Learning ######################

#convert the fsa into dummy variables
dinesafe_c_model <- cbind(dinesafe_c$closure,dinesafe_c$time_to_closure,dinesafe_c$latest_type,dinesafe_c$inspection_score,dummy(dinesafe_c$fsa))
colnames(dinesafe_c_model) <- gsub("closure", "", colnames(dinesafe_c_model))
dinesafe_c_model<-data.frame(dinesafe_c_model)
names(dinesafe_c_model)[1:4] <- c('closure','time_to_closure','latest_type','inspection_score')

#split to 70% train and 30% test
## set the seed to make your partition reproducible
set.seed(123)
smp_size <- floor(0.70 * nrow(dinesafe_c_model))
train_ind <- sample(seq_len(nrow(dinesafe_c_model)), size = smp_size)

train <- na.omit(dinesafe_c_model[train_ind, ])
test <- na.omit(dinesafe_c_model[-train_ind, ])

#create a task
traintask <- makeSurvTask(data = train,target = c("time_to_closure","closure")) 
testtask <- makeSurvTask(data = test, target=c("time_to_closure","closure"))

#set 5 fold cross validation
rdesc <- makeResampleDesc("CV",iters=5L)

#make randomForest learner
rf.lrn <- makeLearner("surv.randomForestSRC",predict.type = "response")
rf.lrn$par.vals <- list(ntree = 100L, importance=TRUE)

#train model
rf_model <- train(rf.lrn,traintask)

#test model
pred = predict(rf_model, task = testtask)
performance(pred)

#get importance score
imp<-getFeatureImportance(rf_model)

#plot importance score
barplot(as.matrix(sort(imp$res)[100:104]),las=2,ylab="Importance Score")

#set parameter space
params <- makeParamSet(makeIntegerParam("mtry",lower = 2,upper = 10),makeIntegerParam("nodesize",lower = 10,upper = 50))

#set validation strategy
rdesc <- makeResampleDesc("CV",iters=5L)

#set optimization technique
ctrl <- makeTuneControlRandom(maxit = 5L)

#start tuning
tune <- tuneParams(learner = rf.lrn, task = traintask, resampling = rdesc, measures = list(acc), par.set = params, control = ctrl, show.info = T)

#plot KM plots for validation set
predictions.df<-data.frame(pred[[2]])
predictions.df[5]<-NA
predictions.df[predictions.df$response>70,5]<-0
predictions.df[predictions.df$response<=70,5]<-1
names(predictions.df)[5]<-c("response.binary")
fit <- survfit(Surv(predictions.df$truth.time,predictions.df$truth.event)~predictions.df$response.binary, data=predictions.df)
plot(fit,col=c('blue','red'), xlab="Days", ylab = "Proportion of Restaurants Open")
legend('left', c("High Risk","Low Risk"), col=c('blue','red'), lty = 1)

#cindex = 0.65
