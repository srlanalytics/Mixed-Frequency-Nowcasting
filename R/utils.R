
library(bdfm) #estimation routines
library(tsbox) #used to format time series
library(jsonlite) #for calling the FRED API
library(data.table) #used to format data 

your_key = "9c39f8a4fbcc38c22475bc7f26988367"

# A handy little utility to call data from the FRED API. 
get_fred_lite <- function(series_id, series_name = NULL, observation_start = "1776-07-04", observation_end = "9999-12-31",frequency = NULL, vintage_dates = NULL){
  if(!is.null(vintage_dates)){
    observation_end = max(as.Date(vintage_dates))
    output_type = 2
  }else{
    output_type = 1
  }
  fred_call <- paste0("https://api.stlouisfed.org/fred/series/observations?series_id=",
                      series_id,
                      "&observation_start=",
                      observation_start,
                      "&observation_end=",
                      observation_end,
                      "&output_type=",
                      output_type,
                      "&api_key=",
                      your_key,
                      "&file_type=json")
  
  if(!is.null(frequency)){
    fred_call <- paste0(fred_call,"&frequency=",frequency)
  }
  if(!is.null(vintage_dates)){
    fred_call <- paste0(fred_call, "&vintage_dates=", vintage_dates)
  }
  X <- fromJSON(fred_call)
  if("value"%in%colnames(X$observations)){
    X$observations <- X$observations[, c("date", "value")]
  }else if(length(grep(series_id, colnames(X$observations)))!=0 ){
    X$observations = data.frame(date = X$observations[,"date"], 
                                value = X$observations[,grep(series_id, colnames(X$observations))])
  }
  X$observations[X$observations[,"value"] == ".","value"] <- NA 
  if(is.null(series_name)){
    series_name = series_id
  }
  X$observations <- data.frame(date = as.Date(X$observations[,"date"]), 
                               series_name = rep(series_name,nrow(X$observations)),
                               value = as.numeric(X$observations[,"value"]) )
  return(X)
}

# Run the actual API call and 
Get_Data <- function(series_name, vintage_dates = NULL){
  if(is.null(vintage_dates)){
    if(series_name%in%c('T10Y3M', 'ICSA', 'TWEXB')){
      frq  <- 'm' #Aggregate these up to monthly
      Data <- get_fred_lite(series_id = series_name, observation_start = "1980-01-01", frequency = frq)
    }else{
      Data <- get_fred_lite(series_id = series_name, observation_start = "1980-01-01")
    }
  }else{
    if(series_name%in%c('T10Y3M', 'ICSA', 'TWEXB')){
      frq  <- 'm' #Aggregate these up to monthly
      Data <- get_fred_lite(series_id = series_name, observation_start = "1980-01-01", frequency = frq, vintage_dates = vintage_dates)
    }else{
      Data <- get_fred_lite(series_id = series_name, observation_start = "1980-01-01", vintage_dates = vintage_dates)
    }
  }
  return(Data$observations)
} 

# Go from long to mixed frequency wide format. We can also use as_of for backtesting. 
# This function requires data.table
call_data <- function(series_names, dt = Data, as_of = NULL, date = "date", series_id = "series_id", value = "value", pub_date = NULL){
  
  dt <- data.table(dt)
  
  colnames(dt)[colnames(dt)==date]        <- "date"
  colnames(dt)[colnames(dt)==series_id]   <- "series_id"
  colnames(dt)[colnames(dt)==value]       <- "value"
  if(is.null(pub_date)){
    dt[,pub_date := as.Date(NA)]
  }else{
    colnames(dt)[colnames(dt)==pub_date]    <- "pub_date"
  }
  
  unique_names <- unique(dt$series_id)
  missing_series <- series_names[!series_names%in%unique_names]
  
  if(length(missing_series)>0){
    warning(paste("The following series are missing:", paste(missing_series, collapse = ", ")))
  }
  
  if(is.null(as_of)) as_of <- Sys.time() #Model from current time (default) or set as_of for backtesting
  
  Out <- dcast(dt[series_id%in%series_names & (pub_date <= as.Date(as_of) | (is.na(pub_date) & as.Date(date) <= as.Date(as_of)) ) ], date ~ series_id,    value.var = "value")
  
  return(Out)
}