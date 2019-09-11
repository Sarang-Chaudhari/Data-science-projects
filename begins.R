setwd("C:\\Users\\DELL\\Desktop\\R projects\\Manufacturing")
getwd()

wob_train=read.csv("product_train.csv",header = T,nrows = 50000)
wob_test=read.csv("product_test.csv",header = T)

head(wob_train)

wob_test$went_on_backorder= NA

wob_train$data="train"
wob_test$data="test"

wob_all=rbind(wob_train,wob_test)

apply(wob_all,2,function(x) length(unique(x)))

library(dplyr)
library(magrittr)
?magrittr

wob_all=wob_all%>% select(-sku)

glimpse(wob_all)

CreateDummies=function(data,var,freq_cutoff = 100){
t=table(data[,var])
t=t[t>freq_cutoff]
t=sort(t)
categories=names(t) [-1]

for( cat in categories){
  name=paste(var,cat,sep="_")
  name=gsub(" ","",name)
  name=gsub("-","_",name)
  name=gsub("\\?","Q",name)
  name=gsub("<","LT_",name)
  name=gsub("\\+","",name)
  name=gsub(">","GT_",name)
  name=gsub("=","EQ_",name)
  name=gsub(",","",name)
  name=gsub("/","_",name)
  data[,name]=as.numeric(data[,var]==cat)
}
data[,var]=NULL
return(data)
}

glimpse(wob_all)

wob_all= wob_all[!((is.na(wob_all$went_on_backorder)) & wob_all$data=="train"),]

for(col in names(wob_all)){
  if(sum(is.na(wob_all[,col]))>0 & !(col %in% c("data","went_on_backorder"))){
    wob_all[is.na(wob_all[,col]),col]=mean(wob_all[wob_all$data=='train',col],na.rm=T)
  }
}

wob_train = wob_all %>% filter(data == 'train') %>% select(-data) 
wob_test= wob_all %>% filter(data == 'test') %>% select(-data,-went_on_backorder) 

any(is.na(wob_train))
any(is.na(wob_test))

library(randomForest)
fit = randomForest(as.factor(went_on_backorder)~., data = wob_train)

test.predictions =predict(fit,newdata = wob_test)
write.csv(test.predictions,file = "Sarang_Chaudhari_P3_part2.csv", row.names = F)

library(magrittr)
wob_train %>%
  filter(went_on_backorder=="Yes") %>%
  ok = median(wob_train$perf_6_month_avg)-median(wob_train$perf_12_month_avg)
ok

wob_train$pieces_past_due %>%
count(0)
    

forecast_9_month = wob_train$forecast_9_month
sales_9_month = wob_train$sales_9_month
x=cor(forecast_9_month,sales_9_month)
round(x,2)

mean(wob_train)

table(wob_all$)














