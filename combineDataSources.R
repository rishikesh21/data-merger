library(data.table)
library(lubridate)
library(syuzhet)
setwd("D:\\IS5126\\")

cuisine_mapper <- function(z){
  if(z=="Steak and Grills" |z=="Steak" | z=="BBQ"){
    return ("Barbecue")
  }
  if(z=="wine" | z=="Beer" | z=="Cocktails" | z=="Bar Food"){
    return ("Wine & Breweries")
  }
  if(z=="North Indian" | z=="South Indian" | z=="Pakistani" | z=="Nepalese" | z=="Bangladeshi"){
    return ("Indian")
  }
  if(z=="Italian" | z=="Spanish" | z=="Greek" | z=="Mediterranean" | z=="Belgian" | z=="Turkish" | z=="Swiss" | z=="Modern European" | z=="Portuguese" |z=="Irish" | z=="Moroccan" | z=="German" | z=="Dutch" | z=="Catalan"){
    return ("European")
  }
  if(z=="Local"){
    return ("Singaporean")
  }
  if(z=="American" | z=="Tapas" | z=="Australian" | z=="Burger" | z=="Finger Food" | z=="Cafe" | z=="Bakery" | z=="Sandwich" | z=="Fast Food" | z=="English" | z=="Latin American" | z=="Mexican" | z=="Brazilian" | z=="Cajun/Creol" | z=="Cuban" | z=="New Zealand" |z=="Eurasian" | z=="Canadian"){
    return ("Western")
  }
  if(z=="Vietnamese" | z=="Hong Kong" | z=="Mongolian" | z=="Filipino" |z=="Burmese"| z=="Myanmar"){
    return ("Asian")
  }
  if(z=="Malaysian/Indonesian" | z=="Nonya / Peranakan" |z=="Malay" |z=="Malay / Indonesian"){
    return ("Malaysian")
  }
  if(z=="Drinks" | z=="Juices" | z=="Tea" |z=="Beverages" ){
    return ("Fruits & Drinks")
  }
  if(z=="Dim Sum" | z=="Juices" | z=="Hokkien" | z=="Zi Char" | z=="Peranakan" | z=="Hakka Chinese" | z=="Hakka" | z=="Shanghainese" | z=="Sichuan" | z=="Beijing" | z=="Xinjiang" | z=="Fujian" | z=="Foochow" | z=="Shandong" | z=="Yunnan" | z=="Dongbei" | z=="Jilin" | z=="Shaanxi"){
    return ("Chinese")
  }
  if(z=="Pork Free" ){
    return ("Halal")
  }
  if(z=="Arab" | z=="Lebanese" | z=="Iranian" |z=="Arabian" ){
    return ("Middle Eastern")
  }
  if(z=="African" | z=="Russian" | z=="Czech" | z=="Caribbean" | z=="Danish" | z=="Cajun/Creole" | z=="Argentinean" | z=="Continental" ){
    return ("International")
  }
  if(z=="Sushi" | z=="Teppanyaki"){
    return("Japanese")
  }
  if(z=="Healthy Food" |z=="Salad"){
    return("Healthy Food")
  }
  else{
    return(z)
  }
}
#hgw cleanup
hgw_review=fread("hgw-reviews.csv")[,c("business_id","review_date","review_title","review_text")]
hgw_review=hgw_review[!duplicated(hgw_review)]
#hgw_res=fread("hgw-restaurants.csv")
hgw_res=fread("hgw-yelp-restaurants.csv")[source=="hungrygowhere"]
hgw_res[,cuisine:=as.character(lapply(cuisine,cuisine_mapper)),]
hgw_res=hgw_res[!duplicated(hgw_res)]
hgw_res=hgw_res[,c("business_id","name","postal","price_range","cuisine","type")]
hgw_res=hgw_res[!duplicated(hgw_res)]
hgw_res[,business_id:=as.integer(business_id)]
hgw=merge(hgw_review,hgw_res,by="business_id",allow.cartesian = TRUE)
hgw=hgw[!duplicated(hgw)]
hgw[,name:=tolower(name)]
tra=fread("D:\\IS5126\\trip_advisor_results.csv")
t=data.table(name=Reduce(intersect,list(hgw$name,tra$restaurant_name)))
t$unique_id <- seq.int(nrow(t))
hgwcommon=hgw[name %in% t$name]
cuisine_mapping=hgwcommon[,.N,by=list(name,cuisine,type)][,c(1,2,3)]
#hgwcommon=hgw
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
hgwnotcommon[,day:=as.character(lapply(review_date,function (x) {unlist(strsplit(x,"\\ "))[1]})),]
hgwnotcommon[,month:=as.character(lapply(review_date,function (x) {unlist(strsplit(x,"\\ "))[2]})),]
hgwnotcommon[,year:=as.character(lapply(review_date,function (x) {unlist(strsplit(x,"\\ "))[3]})),]
hgwnotcommon[,mod_month:=as.character(lapply(month,month_converter)),]
hgwnotcommon[,date:=ymd(paste0(year,"-",mod_month,"-",day))]
tracommon[date=="",date:=paste0(sample(1:30,1)," November 2018" )]
tracommon[grepl("weeks",date),date:=paste0(sample(1:30,1)," November 2018" )]
tracommon[grepl("week",date),date:=paste0(sample(1:30,1)," November 2018" )]
tracommon[grepl("days",date),date:=paste0(sample(1:30,1)," November 2018" )]
tracommon[grepl("yesterday",date),date:=paste0(sample(1:30,1)," November 2018" )]
tracommon[grepl("today",date),date:=paste0(sample(1:30,1)," November 2018" )]
tracommon[,day:=as.character(lapply(date,function (x) {unlist(strsplit(x,"\\ "))[1]})),]
tracommon[,month:=as.character(lapply(date,function (x) {unlist(strsplit(x,"\\ "))[2]})),]
tracommon[,year:=as.character(lapply(date,function (x) {unlist(strsplit(x,"\\ "))[3]})),]
tracommon=tracommon[month %in% c("January","February","March","April","May" ,"June","July","August","September","October" ,"November" ,"December")]
tracommon[,mod_month:=as.character(lapply(month,month_converter)),]
tracommon[,date:=ymd(paste0(year,"-",mod_month,"-",day))]
tracommon=merge(tracommon,restaurant_postal_mapping,by.x ="restaurant_name",by.y ="name",all.x = TRUE)
tracommon=tracommon[,c("restaurant_name","postal","restaurant_price","title","description","date","rating")]
colnames(tracommon)[2]="postal_code"
tranotcommon[date=="",date:=paste0(sample(1:30,1)," November 2018" )]
tranotcommon[grepl("weeks",date),date:=paste0(sample(1:30,1)," November 2018" )]
tranotcommon[grepl("week",date),date:=paste0(sample(1:30,1)," November 2018" )]
tranotcommon[grepl("days",date),date:=paste0(sample(1:30,1)," November 2018" )]
tranotcommon[grepl("yesterday",date),date:=paste0(sample(1:30,1)," November 2018" )]
tranotcommon[grepl("today",date),date:=paste0(sample(1:30,1)," November 2018" )]
tranotcommon[,day:=as.character(lapply(date,function (x) {unlist(strsplit(x,"\\ "))[1]})),]
tranotcommon[,month:=as.character(lapply(date,function (x) {unlist(strsplit(x,"\\ "))[2]})),]
tranotcommon[,year:=as.character(lapply(date,function (x) {unlist(strsplit(x,"\\ "))[3]})),]
tranotcommon=tranotcommon[month %in% c("January","February","March","April","May" ,"June","July","August","September","October" ,"November" ,"December")]
tranotcommon[,mod_month:=as.character(lapply(month,month_converter)),]
tranotcommon[,date:=ymd(paste0(year,"-",mod_month,"-",day))]
tranotcommon=merge(tranotcommon,restaurant_postal_mapping,by.x ="restaurant_name",by.y ="name",all.x = TRUE)
tranotcommon=tranotcommon[,c("restaurant_name","postal","restaurant_price","title","description","date","rating")]
colnames(tranotcommon)[2]="postal_code"
#tra[,date:=ymd(date)]
postal=fread("D:\\IS5126\\postallatlon_withsubzone.csv")[,c(1,4)]
colnames(postal)[1]="postal_code"
hgwcommon=hgwcommon[,c("name","postal","price_range","review_title","review_text","date","cuisine","type")]
hgwcommon[,postal:=as.integer(postal)]
colnames(hgwcommon)[2]="postal_code"
hgw_cleaned_data=merge(hgwcommon,postal,by="postal_code",all.x=TRUE)[!is.na(subzone_id)]
#hgw_cleaned_data[,rating:=sample(1:5,1)]
hgw_cleaned_data[,rating:=round(get_sentiment(review_text))]
hgw_cleaned_data[rating>5,rating:=5]
hgw_cleaned_data[rating<=0,rating:=1]

hgw_cleaned_data=hgw_cleaned_data[,c("name","postal_code","subzone_id","price_range","review_title","review_text","date","rating","cuisine","type")]
colnames(hgw_cleaned_data)=c("restaurant_name","postal_code","subzone_id","price_range","review_title","review_text","date","rating","cuisine","type")
hgwnotcommon=hgwnotcommon[,c("name","postal","price_range","review_title","review_text","date","cuisine","type")]
hgwnotcommon[,postal:=as.integer(postal)]
colnames(hgwnotcommon)[2]="postal_code"
hgw_cleaned_data_3=merge(hgwnotcommon,postal,by="postal_code",all.x=TRUE)[!is.na(subzone_id)]
hgw_cleaned_data_3[,rating:=sample(1:5,1)]
hgw_cleaned_data_3=hgw_cleaned_data_3[,c("name","postal_code","subzone_id","price_range","review_title","review_text","date","rating","cuisine","type")]
colnames(hgw_cleaned_data_3)=c("restaurant_name","postal_code","subzone_id","price_range","review_title","review_text","date","rating","cuisine","type")
tracommon[,postal_code:=as.integer(postal_code)]
tra_cleaned_data=merge(tracommon,postal,by="postal_code",all.x=TRUE)[!is.na(subzone_id)]
tra_cleaned_data=merge(tra_cleaned_data,cuisine_mapping,by.x ="restaurant_name",by.y="name",allow.cartesian = TRUE )
tra_cleaned_data=tra_cleaned_data[,c("restaurant_name","postal_code","subzone_id","restaurant_price","title","description","date","rating","cuisine","type")]
colnames(tra_cleaned_data)=c("restaurant_name","postal_code","subzone_id","price_range","review_title","review_text","date","rating","cuisine","type")
complete_data=rbind(hgw_cleaned_data,tra_cleaned_data,hgw_cleaned_data_3)[subzone_id !=-1]
planning_area_mapping=fread("D:\\IS5126\\postalPlanningArea.csv")
planning_area_mapping=planning_area_mapping[,c("postalcode","planning_area_id")]
colnames(planning_area_mapping)[1]="postal_code"
complete_data_pla=merge(complete_data,planning_area_mapping,by="postal_code")
region_area_mapping=fread("D:\\IS5126\\postalRegionArea.csv")
region_area_mapping=region_area_mapping[,c("postalcode","planning_area_id")]
colnames(region_area_mapping)=c("postal_code","region_area_id")
cleaned_merged_data=merge(complete_data_pla,region_area_mapping,by="postal_code")
cleaned_merged_data=cleaned_merged_data[,c("restaurant_name","price_range","review_title","review_text","date","rating","cuisine","type","subzone_id","planning_area_id","region_area_id" ,"postal_code")]
zomato=fread("D:\\IS5126\\zomatocrawlerbot_results_new.csv")
zomato[,cuisine:=as.character(lapply(cuisine,cuisine_mapper)),]
zomato[,price_range:="$$$"]
zomato[,review_title:=""]
zomato[,review_text:=""]
zomato[,date:=paste0("2018-",sample(1:12,1),"-",sample(1:30,1))]
zomato[,date:=ymd(date)]
zomato[,type:=""]
zomato[,postal_code:=as.integer(postal_code)]
zomato_subzone=merge(zomato,postal,by="postal_code",all.x=TRUE)[!is.na(subzone_id)]
zomato_planning_area=merge(zomato_subzone,planning_area_mapping,by="postal_code")
zomato_region_area=merge(zomato_planning_area,region_area_mapping,by="postal_code")
zomato_region_area=zomato_region_area[,c("restaurant_name","price_range","review_title","review_text","date","rating","cuisine","type","subzone_id","planning_area_id","region_area_id" ,"postal_code")]
#dist=cleaned_merged_restaurants[,.N,by=list(restaurant_name,subzone_id)][,list(restaurant_count=.N),list(subzone_id)][order(subzone_id)]

yelp_review=fread("yelp-review.csv")[,c("biz_id","rating","date","text","location")]
yelp_review=yelp_review[!duplicated(yelp_review)]
yelp_res=fread("hgw-yelp-restaurants.csv")[source=="yelp",c("name","business_id","price_range","rating","cuisine","postal","type")]
colnames(yelp_res)[1]="restaurant_name"
colnames(yelp_res)[2]="biz_id"
colnames(yelp_res)[4]="restaurant_rating"
colnames(yelp_res)[6]="postal_code"


yelp=merge(yelp_review,yelp_res,by="biz_id",allow.cartesian=TRUE)
yelp=yelp[date!=""]
yelp[,cuisine:=as.character(lapply(cuisine,cuisine_mapper)),]
yelp[,day:=as.character(lapply(date,function (x) {unlist(strsplit(x,"\\/"))[1]})),]
yelp[,month:=as.character(lapply(date,function (x) {unlist(strsplit(x,"\\/"))[2]})),]
yelp[,year:=as.character(lapply(date,function (x) {unlist(strsplit(x,"\\/"))[3]})),]
yelp[,date:=ymd(paste0(year,"-",month,"-",day))]
yelp[,postal_code:=as.integer(postal_code)]
yelp_subzone=merge(yelp,postal,by="postal_code",all.x=TRUE)[!is.na(subzone_id)]
yelp_planning_area=merge(yelp_subzone,planning_area_mapping,by="postal_code")
yelp_region_area=merge(yelp_planning_area,region_area_mapping,by="postal_code")
colnames(yelp_region_area)[5]="review_text"
yelp_region_area[,review_title:=""]
yelp_region_area=yelp_region_area[,c("restaurant_name","price_range","review_title","review_text","date","rating","cuisine","type","subzone_id","planning_area_id","region_area_id" ,"postal_code")]
cleaned_merged_restaurants=rbind(cleaned_merged_data,zomato_region_area,yelp_region_area)
cleaned_merged_restaurants[,rating:=as.numeric(rating)]
cleaned_merged_restaurants[,rating:=round(rating)]
cleaned_merged_restaurants[,get_sentiment_rating:=round(get_sentiment(review_text))]
cleaned_merged_restaurants[get_sentiment_rating>5,get_sentiment_rating:=5]
cleaned_merged_restaurants[get_sentiment_rating<=0,get_sentiment_rating:=1]
fwrite(cleaned_merged_restaurants,file="D:\\IS5126\\merged_final_data_with_zomato_yelp_sentiment_rating.csv",sep=',',col.names = TRUE,row.names = FALSE)
