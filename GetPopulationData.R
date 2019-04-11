### Libraries ###
library(jsonlite)
library(httr)

### Functions ###
# Get data from the census API
getCensusData <- function(url) {
  authToken <- "KEYGOESHERE"
  urlPrefix <- "https://api.census.gov/data/"
  urlPostfix <- paste("&key=", authToken, sep="")
  url <- paste(urlPrefix, url, urlPostfix, sep="")
  request <- GET(url)
  if (status_code(request) == 200) {
    json <- content(request, as="text")
    return(fromJSON(json))
  } else {
    return(NA)
  }  
}

### Main Code ###
# Get population data from the census API
pop1997to1999 <- getCensusData("1990/pep/int_natcivpop?get=TOT_POP&YEAR=1997,1998,1999")
pop2000to2009 <- getCensusData("2000/pep/int_population?get=POP&for=us:1&DATE=2,3,4,5,6,7,8,9,10,11")
pop2010to2017 <- getCensusData("2017/pep/population?get=POP&for=us:*&DATE=3,4,5,6,7,8,9,10")

# Clean population data and make dataframe
colNames <- pop1997to1999[1,]
pop1997to1999 <- data.frame(pop1997to1999[2:4,])
names(pop1997to1999) <- colNames
pop1997to1999 <- pop1997to1999 %>% mutate(Year = as.numeric(as.character(YEAR)),
                                          POP = as.numeric(as.character(TOT_POP))) %>%
  select(-TOT_POP, -YEAR)
colNames <- pop2000to2009[1,]
pop2000to2009 <- data.frame(pop2000to2009[2:11,])
names(pop2000to2009) <- colNames
pop2000to2009 <- pop2000to2009 %>% mutate(Year = as.numeric(as.character(DATE)) + 1998,
                                          POP = as.numeric(as.character(POP))) %>%
  select(-DATE, -us)
colNames <- pop2010to2017[1,]
pop2010to2017 <- data.frame(pop2010to2017[2:9,])
names(pop2010to2017) <- colNames
pop2010to2017 <- pop2010to2017 %>% mutate(Year = as.numeric(as.character(DATE)) + 2007,
                                          POP = as.numeric(as.character(POP))) %>%
  select(-DATE, -us)
popData <- bind_rows(pop1997to1999, pop2000to2009, pop2010to2017)

write.csv(popData, "./Data/population.csv", row.names = FALSE)