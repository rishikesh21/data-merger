library(data.table)
library(lubridate)
library(ggpubr)
data=fread("D:\\IS5126\\merged_final_data_with_zomato_yelp_sentiment_rating.csv")

#income related analysis

income=fread("D:\\IS5126\\income.csv")[year==2015]
aggregated_income_details=income[,list(income_below_2000=sum(below_sgd_1000,sgd_1000_to_1499,sgd_1500_to_1999),income_2000_5000=sum(sgd_2000_to_2499,sgd_2500_to_2999,sgd_3000_to_3999,sgd_4000_to_4999),income_more_5000=sum(sgd_5000_to_5999,sgd_6000_to_6999,sgd_7000_to_7999)),by=list(planning_area)]
aggregated_income_details[,planning_area:=toupper(planning_area),]
planning_area_id=fread("D:\\IS5126\\PlanningAreaMapping.csv")[,c(1,2)]
colnames(planning_area_id)[2]="planning_area"
aggregated_income_planningarea=merge(aggregated_income_details,planning_area_id,by="planning_area")
planning_area_rating=data[,list(count_rating=.N,avg_res_rating=mean(rating)),by=list(planning_area_id)]
result_income=merge(planning_area_rating,aggregated_income_planningarea,by.x ="planning_area_id",by.y ="id")

planning_area_cuisine_rating=data[cuisine!="",list(avg_res_rating=mean(rating)),by=list(planning_area_id,cuisine)]
planning_area_cuisine_name_rating=data[cuisine!="",list(count_rating=.N,avg_res_rating=mean(rating)),by=list(planning_area_id,cuisine,restaurant_name)]
review_distribution=data[,list(review_count=.N),by=list(restaurant_name)][,list(no_of_restaurants=.N),by=review_count]
top_planning_areas=planning_area_cuisine_rating[,max(avg_res_rating),by=list(cuisine)]

#age population analysis
age=fread("age_gender.csv")
age[age_group=="Early Working Age" | age_group=="Prime Working Age" | age_group=="Mature Working Age",age_group:="All working Age"]
age_planning_area=age[year==2018,list(population=sum(population)),by=list(planning_areas,age_group)]
colnames(age_planning_area)[1]="planning_area"
age_planning_area=merge(age_planning_area,planning_area_id,by="planning_area")
result_age=merge(planning_area_rating,age_planning_area,by.x ="planning_area_id",by.y ="id")

planning_area_cuisine_rating_top=planning_area_cuisine_rating[planning_area_cuisine_rating[,.I[which.max(avg_res_rating)],by=list(planning_area_id)]$V1]
planning_area_cuisine_rating_top[cuisine=="Steamboat" |cuisine=="Indochinese" | cuisine=="Hainanese" | cuisine=="Cantonese" | cuisine=="Teochew" ,cuisine:="Chinese"]
planning_area_cuisine_rating_top[cuisine=="Middle Eastern" ,cuisine:="Halal"]

cor.test(~avg_res_rating+income_below_2000,data=result_income,method="pearson",conf.level=0.95)
cor.test(~avg_res_rating+income_2000_5000,data=result_income,method="pearson",conf.level=0.95)
cor.test(~avg_res_rating+income_more_5000,data=result_income,method="pearson",conf.level=0.95)


#ethinic related analysis
ethnic=fread("D://IS5126//ethnic.csv")[year==2015]
ethinic_merged=merge(ethnic,planning_area_id,by="planning_area")
result_ethinic=merge(planning_area_cuisine_rating,ethinic_merged,by.x ="planning_area_id",by.y ="id")

#x=ggscatter(result,x="avg_res_rating",y="income_below_2000",add="reg.line",conf.int=TRUE,cor.coef=TRUE,cor.method="pearson",xlab="avg_res_rating",ylab="income_below_2000")
#y=ggscatter(result,x="avg_res_rating",y="income_2000_5000",add="reg.line",conf.int=TRUE,cor.coef=TRUE,cor.method="pearson",xlab="avg_res_rating",ylab="income_2000_5000")
#z=ggscatter(result,x="avg_res_rating",y="income_more_5000",add="reg.line",conf.int=TRUE,cor.coef=TRUE,cor.method="pearson",xlab="avg_res_rating",ylab="income_more_5000")

colnames(result_ethinic)=c("planning_area_id","cuisine","avg_res_rating","planning_area","chinese","indian","malays","others" ,"total","chinese_rate","indian_rate","malays_rate","others rate","year")      
cor.test(~avg_res_rating+chinese_rate,data=result_ethinic[cuisine=="Chinese" | cuisine=="Cantonese" | cuisine=="Singaporean" | cuisine=="Indochinese" | cuisine=="Hainanese" | cuisine=="Taiwanese"| cuisine=="Steamboat"| cuisine=="Teochew" | cuisine=="Hawker Centre"],method="pearson",conf.level=0.95)
cor.test(~avg_res_rating+population,data=result_age[age_group=="Elderly"],method="pearson",conf.level=0.95)
cor.test(~avg_res_rating+population,data=result_age[age_group=="Children"],method="pearson",conf.level=0.95)


x_ethnic=ggscatter(result_ethinic[cuisine=="Chinese" | cuisine=="Cantonese" | cuisine=="Singaporean" | cuisine=="Indochinese" | cuisine=="Hainanese" | cuisine=="Taiwanese"| cuisine=="Steamboat"| cuisine=="Teochew" | cuisine=="Hawker Centre"],x="avg_res_rating",y="chinese_rate",add="reg.line",conf.int=TRUE,cor.coef=TRUE,cor.method="pearson",xlab="avg_res_rating",ylab="chinese_rate")
y_ethnic=ggscatter(result_ethinic[cuisine=="Indian"],x="avg_res_rating",y="indian_rate",add="reg.line",conf.int=TRUE,cor.coef=TRUE,cor.method="pearson",xlab="avg_res_rating",ylab="chinese_rate")
z_ethnic=ggscatter(result_ethinic[cuisine=="Malaysian"],x="avg_res_rating",y="malays_rate",add="reg.line",conf.int=TRUE,cor.coef=TRUE,cor.method="pearson",xlab="avg_res_rating",ylab="chinese_rate")
cor.test(~avg_res_rating+population,data=result_age[age_group=="All working Age"],method="pearson",conf.level=0.95)


cor.test(~avg_res_rating+population,data=result_age[age_group=="All working Age"],method="pearson",conf.level=0.95)
cor.test(~avg_res_rating+population,data=result_age[age_group=="Elderly"],method="pearson",conf.level=0.95)
cor.test(~avg_res_rating+population,data=result_age[age_group=="Children"],method="pearson",conf.level=0.95)

x_age=ggscatter(result_age[age_group=="All working Age"],x="avg_res_rating",y="population",add="reg.line",conf.int=TRUE,cor.coef=TRUE,cor.method="pearson",xlab="avg_res_rating",ylab="chinese_rate")
y_age=ggscatter(result_age[age_group=="Elderly"],x="avg_res_rating",y="population",add="reg.line",conf.int=TRUE,cor.coef=TRUE,cor.method="pearson",xlab="avg_res_rating",ylab="chinese_rate")
z_age=ggscatter(result_age[age_group=="Children"],x="avg_res_rating",y="population",add="reg.line",conf.int=TRUE,cor.coef=TRUE,cor.method="pearson",xlab="avg_res_rating",ylab="Children Population")



x_income=ggscatter(result_income,x="avg_res_rating",y="income_below_2000",add="reg.line",conf.int=TRUE,cor.coef=TRUE,cor.method="pearson",xlab="avg_res_rating",ylab="income_below_2000")
y_income=ggscatter(result_income,x="avg_res_rating",y="income_2000_5000",add="reg.line",conf.int=TRUE,cor.coef=TRUE,cor.method="pearson",xlab="avg_res_rating",ylab="income_2000_5000")
z_income=ggscatter(result_income,x="avg_res_rating",y="income_more_5000",add="reg.line",conf.int=TRUE,cor.coef=TRUE,cor.method="pearson",xlab="avg_res_rating",ylab="income_more_5000")

result_ethinic_age=merge(result_ethinic,result_age,by=c("planning_area_id"),allow.cartesian = TRUE)
#cor.test(~avg_res_rating+chinese_rate+population,data=result_ethinic_age[cuisine=="Chinese"],method="pearson",conf.level=0.95)


# graph_data=data[,list(review_count=.N),by=list(cuisine,rating)][cuisine %in% c("Chinese","Asian","Western","Singaporean","Japanese","European","Desserts","Halal","Snacks","Indian") & rating!=0][order(rating)]
# ggplot(graph_data, aes(x = cuisine, y = review_count, fill = rating, label = rating)) +
#   geom_bar(stat = "identity") +
#   geom_text(size = 3, position = position_stack(vjust = 0.5))


planning_area_cuisine_rating_top[cuisine %in% c("Chinese","Asian","Western","Singaporean","Japanese","European","Desserts","Halal","Snacks","Indian")]