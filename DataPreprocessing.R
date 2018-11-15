library(data.table)
library(dplyr)
setwd("D:\\IS5126\\RestaurantReviewTripAdvisor\\")
files = list.files(pattern="*.csv")

DT =tryCatch({
  
 rbindlist(lapply(files, fread))
},
error=function(e){
  print(e)
}
)

DT=DT[!duplicated(DT)]


fwrite(DT,file="D:\\IS5126\\trip_advisor_results.csv",sep=',',col.names = TRUE,row.names = FALSE)
