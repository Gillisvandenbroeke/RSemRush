SemRushOrganicResults <- function(keyword,                   
                           key,
                           market = "uk",
                           historic = FALSE,
                           dateStart_YMD = NULL,
                           dateEnd_YMD = NULL,
                           fileName = "data"
                           ){
  
  library(data.table)
  library(RCurl)
  library(lubridate)
  
  fileName = paste0(fileName,".csv")  
  
  # column length consistentcy check
  if(length(market) > 1 && length(keyword) > 1 && length(keyword) != length(market))
    stop("keywords and markets are different in length")
  
  arguments = data.table(keyword = keyword, market = market)
  
  if(historic){
    start <- ymd(dateStart_YMD)
    day(start) <- 1
    end <- ymd(dateEnd_YMD)
    day(end) <-1
    
    #generate dates to query
    interval <- interval(start,end)
    n.months <- round(as.period(interval)/months(1))
    
    day(start) <- 15
    dates <- start + months(0:n.months)
      
    tmp = data.table()  
    for(i in 1:length(dates)) 
      tmp <- rbind(tmp,arguments[,date:=dates[i]])    
    
    arguments <- tmp
    
  }  
  
  data = data.table()
  
  for(i in seq(1:nrow(arguments))){
    
    keyword <- arguments[i,keyword]
    market <- arguments[i,market]
    if(historic){
      date <- arguments[i,date]
      YYYYMM15 <- paste0(format(date, "%Y%m"),"15")
    }
    
    
    
    # For some reason getForm() builds a '400 - bad request' for the SEMrush api, thus parameters are built manually.
    parameters = paste0("?action=", "report",
                        "&type=", "phrase_organic",
                        "&key=", key,
                        "&display_limit=","20",
                        "&export=", "api",
                        "&export_columns=","Dn,Ur",
                        "&phrase=", keyword)
    
    parameters = URLencode(parameters)
    
    url = paste(market, "api.semrush.com", sep = ".")
    
    if(historic) url = paste(YYYYMM15, url, sep = ".")
    
    url = paste0("http://",url)
    
    request = paste(url, parameters, sep = "/")                      
    
    response = try(getURL(request, verbose = FALSE))
    
    # Error handling:    
    if(class(response)=="try-error"){
      responseData <- data.table()
      print("http error")
      
    }else if(grepl(pattern="^<html>",response,ignore.case=TRUE)){
      responseData <- data.table()
      print("no data at this url")
      
    }else if(grepl(pattern="^ERROR",response)){
        responseData <- data.table()
        print(response)
        
    }else {  #respone parsing    
      responseData <- data.table(read.table(textConnection(response),sep=";",header=TRUE))
      responseData[ , Keyword:= keyword]
      responseData[ , Market:= market]
      if(historic)
        responseData[ , Month:= paste(month(date, label = TRUE),year(date))]
      responseData[ , Position:= seq(1:nrow(responseData))]
    }
    
    if(historic) setkey(responseData, Keyword, Market, Month)
    else setkey(responseData, Keyword, Market)
        
    data <- rbind(data,responseData)
    
    Sys.sleep(0.1) #SEMrush allows no more than 10 requests per second
    
    print(paste0(i, " out of ", nrow(arguments), " requests completed"))
    
    #print latest datas to csv so progress is not lost in case script crash midway
    write <- try(write.csv(data,fileName,row.names=FALSE),silent=TRUE)
    if(class(write)=="try-error")
      print("could not write to the file")    
  }
  
  print(paste0("data collection completed and saved as ", fileName))
    
  return(data)
  
}

