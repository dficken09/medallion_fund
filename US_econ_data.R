#### Grab Past U.S. GDP Data

library(Quandl)
library(blsAPI)

#### Tools for grabbing data from Bureau of Labor Statistics ####

### grabbed from https://github.com/keberwein/blscrapeR/blob/master/R/bls_api.R

bls_api <- function (seriesid, startyear = NULL, endyear = NULL, registrationKey = NULL, 
                     catalog = FALSE, calculations = FALSE, annualaverage = FALSE, ...){
  # Set some dummy variables. This keeps CRAN check happy.
  year=period=':='=seriesID=NULL
  # Begin constructing payload.
  payload <- list(seriesid = seriesid)
  # Check for start and end years
  if (!is.null(startyear) & is.null(endyear)){
    endyear <- format(Sys.Date(), "%Y")
    message("The API requires both a start and end year." 
            ,"\nThe endyear argument has automatically been set to ", format(Sys.Date(), "%Y"),".")
  }
  # Payload won't take NULL values, have to check every field.
  # Probably a more elegant way do do this.
  if (exists("registrationKey") & !is.null(registrationKey)){
    if (registrationKey=="BLS_KEY"){
      payload["registrationKey"] <- as.character(Sys.getenv("BLS_KEY"))
    }
    else{
      payload["registrationKey"] <- as.character(registrationKey)
    }
    # Base URL for V2 for folks who have a key.
    base_url <- "https://api.bls.gov/publicAPI/v2/timeseries/data/"
    if (exists("catalog") & !is.null(catalog)){
      if (!is.logical(catalog)){
        message("Please select TRUE or FALSE for catalog argument.")
      }
      payload["catalog"] <- tolower(as.character(catalog))
    }
    if (exists("calculations") & !is.null(calculations)){
      if (!is.logical(calculations)){
        message("Please select TRUE or FALSE for calculations argument.")
      }
      payload["calculations"] <- tolower(as.character(calculations))
    }
    if (exists("annualaverage") & !is.null(annualaverage)){
      if (!is.logical(annualaverage)){
        message("Please select TRUE or FALSE for calculations argument.")
      }
      payload["annualaverage"] <- tolower(as.character(annualaverage))
    }
  } else {
    # Base URL for everyone else.
    base_url <- "https://api.bls.gov/publicAPI/v1/timeseries/data/"
  }
  # Both sets of users can select these args.
  if (exists("startyear") & !is.null(startyear)){
    payload["startyear"] <- as.character(startyear)
  }
  if (exists("endyear") & !is.null(endyear)){
    payload["endyear"] <- as.character(endyear)
  }
  # Manually construct payload since the BLS formatting is wakey.
  payload <- jsonlite::toJSON(payload)
  loadparse <- regmatches(payload, regexpr("],", payload), invert = TRUE)
  parse1 <- loadparse[[1]][1]
  parse2 <- gsub("\\[|\\]", "", loadparse[[1]][2])
  payload <- paste(parse1, parse2, sep = "],")
  
  # Here's the actual API call.
  jsondat <- httr::content(httr::POST(base_url, body = payload, httr::content_type_json()))
  
  if(jsondat$status == "REQUEST_SUCCEEDED") {
    df <- purrr::map_dfr(jsondat$Results$series, function(s) {
      out <- purrr::map_dfr(s$data, function(d) {
        d[["footnotes"]] <- paste(unlist(d[["footnotes"]]), collapse = " ")
        d[["seriesID"]] <- paste(unlist(s[["seriesID"]]), collapse = " ")
        d <- purrr::map(purrr::map(d, unlist), paste, collapse=" ")
      })
    })
    
    df$value <- as.numeric(as.character(df$value))
    
    if ("year" %in% colnames(df)){
      df$year <- as.numeric(as.character(df$year))
    }
    
    message(jsondat$status)
  } else {
    # If request fails, return an empty data frame plus the json status and message.
    df <- data.frame()
    message(jsondat$status)
    message(jsondat$message)
  }
  return(df)
}

#### grabbed from https://github.com/keberwein/blscrapeR/blob/master/R/quick_functions.R



########################################################################################

# 1. GDP - GDP represents the market value of all final goods and services produced within a country during a given period. 
# The figure is usually given in nominal and real formats, with real GDP adjusting for changes in monetary value. 
# Given its vast breadth, this indicator is among the most-watched by the financial markets.

# The expansion of a country's GDP is indicative of a growing economy, while a contraction in GDP indicates a slowdown 
# in a country's economy. Meanwhile, a country's projected GDP growth rate can be used to determine an appropriate level 
# of sovereign debt or determine if companies operating within the country are likely to experience growth.

gdp_data = Quandl("FRED/GDP",type ="xts")

# 2. Employment indicators - The productivity and wealth of a country's citizens is arguably the ultimate determiner 
# of economic success. Employment indicators, such as labor force, payroll, and unemployment data estimate how many 
# citizens are employed and whether they are making more or less money than before.

labor_force_data = NULL

payroll_data = NULL

unemployment_data = 

# 3. Consumer Price Index - CPI measures changes in the prices of consumer goods and services that are purchased by 
# households. The index is a statistical estimate created by using prices from a sample of representative items 
# collected periodically. Often times, this measure is used as a gauge of inflation, which can positively or 
# negatively affect a country's currency.

cpi_data = NULL


# 4. PMI Manufacturing & Services - The Purchasing Manager's Index (PMI) is an economic indicator developed by 
# Markit Group and the Institute for Supply Management. By polling businesses on a monthly basis, the index 
# reflects the acquisition of goods and services by purchasing managers. The two most important surveys are 
# the PMI Manufacturing and PMI Services indices.

# The financial markets watch the PMI Manufacturing and PMI Services indices as key leading economic indicators 
# because companies stop purchasing raw materials when demand dries up. This can indicate problems in an economy
# much before other reports like retail sales or consumer spending.

pmi_data = NULL
