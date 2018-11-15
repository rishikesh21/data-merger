library(data.table)

library(lubridate)

setwd("D:\\IS5126\\")
# yelp_review=fread("yelp-review.csv")[,c("biz_id","rating","date","text","location")]
# yelp_review=yelp_review[!duplicated(yelp_review)]
# yelp_res=fread("yelp-restaurant.csv")[,c("name","business_id","price_range","rating")]
# yelp_res=yelp_res[!duplicated(yelp_res)]
# colnames(yelp_res)[2]="biz_id"
# yelp=merge(yelp_review,yelp_res,by="biz_id")
# yelp=yelp[!duplicated(yelp)]
# yelp[,name:=tolower(name)]
#hgw cleanup
hgw_review=fread("hgw-reviews.csv")[,c("business_id","review_date","review_title","review_text")]
hgw_review=hgw_review[!duplicated(hgw_review)]
hgw_res=fread("hgw-restaurants.csv")
cuisine_mapping=reshape(hgw_res[,.N,by=list(name,cuisine)],idvar="name",timevar ="cuisine",direction ="wide")
cuisine_mapping[is.na(cuisine_mapping)]<-0

hgw_res=hgw_res[,c("business_id","name","postal","price_range")]
hgw_res=hgw_res[!duplicated(hgw_res)]
hgw=merge(hgw_review,hgw_res,by="business_id")
hgw=hgw[!duplicated(hgw)]
hgw[,name:=tolower(name)]
tra=fread("D:\\IS5126\\trip_advisor_results.csv")
t=data.table(name=Reduce(intersect,list(hgw$name,tra$restaurant_name)))
t$unique_id <- seq.int(nrow(t))

hgwcommon=hgw[name %in% t$name]
hgwnotcommon=hgw[!name %in% t$name]
tracommon=tra[restaurant_name %in% t$name]
tranotcommon=tra[!restaurant_name %in% t$name]

month_converter <- function(x){
  if (x=="Jan" | x=="January"){
    return ("01")
  }
  if (x=="Feb" | x=="February"){
    return ("02")
  }
  if (x=="Mar" | x=="March"){
    return ("03")
  }
  if (x=="Apr" | x=="April"){
    return ("04")
  }
  if (x=="May" | x=="May"){
    return ("05")
  }
  if (x=="Jun" | x=="June"){
    return ("06")
  }
  if (x=="Jul" | x=="July"){
    return ("07")
  }
  if (x=="Aug" | x=="August"){
    return ("08")
  }
  if (x=="Sep" | x=="September"){
    return ("09")
  }
  if (x=="Oct" | x=="October"){
    return ("10")
  }
  if (x=="Nov" | x=="November"){
    return ("11")
  }
   if (x=="Dec" | x=="December"){
    return ("12")
   }
  else{
    return ("00")
    
  }
  
}
restaurant_postal_mapping=hgwcommon[,.N,by=list(name,postal)][,c(1,2)]
hgwcommon[,day:=as.character(lapply(review_date,function (x) {unlist(strsplit(x,"\\ "))[1]})),]
hgwcommon[,month:=as.character(lapply(review_date,function (x) {unlist(strsplit(x,"\\ "))[2]})),]
hgwcommon[,year:=as.character(lapply(review_date,function (x) {unlist(strsplit(x,"\\ "))[3]})),]
hgwcommon[,mod_month:=as.character(lapply(month,month_converter)),]
hgwcommon[,date:=ymd(paste0(year,"-",mod_month,"-",day))]

tracommon[date=="",date:=paste0(sample(1:30,1)," November 2018" )]
tracommon[grepl("weeks",date),date:=paste0(sample(1:30,1)," November 2018" )]
tracommon[grepl("week",date),date:=paste0(sample(1:30,1)," November 2018" )]
tracommon[grepl("days",date),date:=paste0(sample(1:30,1)," November 2018" )]
tracommon[grepl("yesterday",date),date:=paste0(sample(1:30,1)," November 2018" )]
tracommon[grepl("today",date),date:=paste0(sample(1:30,1)," November 2018" )]

tracommon[,day:=as.character(lapply(date,function (x) {unlist(strsplit(x,"\\ "))[1]})),]
tracommon[,month:=as.character(lapply(date,function (x) {unlist(strsplit(x,"\\ "))[2]})),]
tracommon[,year:=as.character(lapply(date,function (x) {unlist(strsplit(x,"\\ "))[3]})),]
tracommon[,mod_month:=as.character(lapply(month,month_converter)),]
tracommon[,date:=ymd(paste0(year,"-",mod_month,"-",day))]
tracommon=merge(tracommon,restaurant_postal_mapping,by.x ="restaurant_name",by.y ="name",all.x = TRUE)
tracommon=tracommon[,c("restaurant_name","postal","restaurant_price","title","description","date","rating")]
colnames(tracommon)[2]="postal_code"

#tra[,date:=ymd(date)]

postal=fread("D:\\IS5126\\postallatlon_withsubzone.csv")[,c(1,4)]

colnames(postal)[1]="postal_code"
hgwcommon=hgwcommon[,c("name","postal","price_range","review_title","review_text","date")]
colnames(hgwcommon)[2]="postal_code"


hgw_cleaned_data=merge(hgwcommon,postal,by="postal_code",all.x=TRUE)[!is.na(subzone_id)]
hgw_cleaned_data[,rating:=3]
hgw_cleaned_data=hgw_cleaned_data[,c("name","postal_code","subzone_id","price_range","review_title","review_text","date","rating")]
colnames(hgw_cleaned_data)=c("restaurant_name","postal_code","subzone_id","price_range","review_title","review_text","date","rating")

tra_cleaned_data=merge(tracommon,postal,by="postal_code",all.x=TRUE)[!is.na(subzone_id)]
tra_cleaned_data=tra_cleaned_data[,c("restaurant_name","postal_code","subzone_id","restaurant_price","title","description","date","rating")]
colnames(tra_cleaned_data)=c("restaurant_name","postal_code","subzone_id","price_range","review_title","review_text","date","rating")


complete_data=rbind(hgw_cleaned_data,tra_cleaned_data)

fwrite(complete_data,file="D:\\IS5126\\complete_data.csv",sep=',',col.names = TRUE,row.names = FALSE)