library(data.table)

setwd("D:\\IS5126\\")

name=fread("unique_restaurants(hgw+yelp).csv")
name[,name:=tolower(name)]
name=data.table(name=unique(name$name))


zomato_name=fread("zomato_restaurant_list.csv",sep=',')
zomato_name[,name:=tolower(name)]

list=merge(name,zomato_name,by="name")
crawled=fread("D:\\IS5126\\trip_advisor_results.csv")[,c(1)]
crawled_list=data.table(restaurant_name=tolower(unique(crawled$restaurant_name)))
to_be_crwaled=data.table(name=setdiff(list$name,crawled_list$restaurant_name))




itr= round(nrow(to_be_crwaled)/100)
for(i in 1:itr){
  start=((i-1)*100)+1
  end=(i*100)

  fwrite(to_be_crwaled[start:end],file=paste0("L:\\TripAdvisorCrawler\\source\\trip_advisor_",i,".csv"),sep=',',col.names = FALSE,row.names = FALSE)

}


list$unique_id <- seq.int(nrow(list))





fwrite(list,file="L:\\restaurant_id_common_1.csv",sep=',',col.names = TRUE,row.names = FALSE)



