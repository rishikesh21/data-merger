library(data.table)
library(lubridate)
library(ggpubr)

data=fread("D:\\IS5126\\merged_final_data_with_zomato_yelp_sentiment_rating.csv")
income=fread("D:\\IS5126\\income.csv")[year==2015]
aggregated_income_details=income[,list(income_below_2000=sum(below_sgd_1000,sgd_1000_to_1499,sgd_1500_to_1999),income_2000_5000=sum(sgd_2000_to_2499,sgd_2500_to_2999,sgd_3000_to_3999,sgd_4000_to_4999),income_more_5000=sum(sgd_5000_to_5999,sgd_6000_to_6999,sgd_7000_to_7999)),by=list(planning_area)]
aggregated_income_details[,planning_area:=toupper(planning_area),]
planning_area_id=fread("D:\\IS5126\\PlanningAreaMapping.csv")[,c(1,2)]
colnames(planning_area_id)[2]="planning_area"
aggregated_income_planningarea=merge(aggregated_income_details,planning_area_id,by="planning_area")
planning_area_rating=data[,list(count_rating=.N,avg_res_rating=mean(rating)),by=list(planning_area_id)]
result=merge(planning_area_rating,aggregated_income_planningarea,by.x ="planning_area_id",by.y ="id")
planning_area_cuisine_rating=data[cuisine!="",list(avg_res_rating=mean(rating)),by=list(planning_area_id,cuisine)]
planning_area_cuisine_name_rating=data[cuisine!="",list(count_rating=.N,avg_res_rating=mean(rating)),by=list(planning_area_id,cuisine,restaurant_name)]
review_distribution=data[,list(review_count=.N),by=list(restaurant_name)][,list(no_of_restaurants=.N),by=review_count]
top_planning_areas=planning_area_cuisine_rating[,max(avg_res_rating),by=list(cuisine)]

cor.test(~avg_res_rating+income_below_2000,data=result,method="pearson",conf.level=0.95)
cor.test(~avg_res_rating+income_2000_5000,data=result,method="pearson",conf.level=0.95)
cor.test(~avg_res_rating+income_more_5000,data=result,method="pearson",conf.level=0.95)
ethnic=fread("D://IS5126//ethnic.csv")[year==2015]
ethinic_merged=merge(ethnic,planning_area_id,by="planning_area")
result_ethinic=merge(planning_area_cuisine_rating,ethinic_merged,by.x ="planning_area_id",by.y ="id")

x=ggscatter(result,x="avg_res_rating",y="income_below_2000",add="reg.line",conf.int=TRUE,cor.coef=TRUE,cor.method="pearson",xlab="avg_res_rating",ylab="income_below_2000")
y=ggscatter(result,x="avg_res_rating",y="income_2000_5000",add="reg.line",conf.int=TRUE,cor.coef=TRUE,cor.method="pearson",xlab="avg_res_rating",ylab="income_2000_5000")
z=ggscatter(result,x="avg_res_rating",y="income_more_5000",add="reg.line",conf.int=TRUE,cor.coef=TRUE,cor.method="pearson",xlab="avg_res_rating",ylab="income_more_5000")

colnames(result_ethinic)=c("planning_area_id","cuisine","avg_res_rating","planning_area","chinese","indian","malays","others" ,"total","chinese_rate","indian_rate","malays_rate","others rate","year")      
x=ggscatter(result_ethinic[cuisine=="Chinese" | cuisine=="Cantonese" | cuisine=="Singaporean" | cuisine=="Indochinese" | cuisine=="Asian" | cuisine=="Hainanese" | cuisine=="Taiwanese"| cuisine=="Steamboat"| cuisine=="Teochew" | cuisine=="Hawker Centre"],x="avg_res_rating",y="chinese_rate",add="reg.line",conf.int=TRUE,cor.coef=TRUE,cor.method="pearson",xlab="avg_res_rating",ylab="chinese_rate")
y=ggscatter(result_ethinic[cuisine=="Indian"],x="avg_res_rating",y="indian_rate",add="reg.line",conf.int=TRUE,cor.coef=TRUE,cor.method="pearson",xlab="avg_res_rating",ylab="chinese_rate")
z=ggscatter(result_ethinic[cuisine=="Malaysian"],x="avg_res_rating",y="malays_rate",add="reg.line",conf.int=TRUE,cor.coef=TRUE,cor.method="pearson",xlab="avg_res_rating",ylab="chinese_rate")


cor.test(~avg_res_rating+chinese_rate,data=result_ethinic[cuisine=="Chinese"],method="pearson",conf.level=0.95)
