
suppressPackageStartupMessages(library(dplyr))
library(knitr)
options(geonamesUsername = "santina")   
library(geonames)    
library(ggplot2)
library(rplos)
library(parsedate)
library(countrycode)

articles <- searchplos(q= "gut microbiome", limit = 999,     #query gut microbiome, max 1000 articles
                       fl=c("id","author_affiliate,publication_date"), # return these values
                       fq=list('article_type:"Research Article"', "doc_type:full"),
                       start=0)

articles_meta <- articles$meta
articles_meta
articles_data <- articles$data      #pull out only the data and not the metadata for this search
author_affiliate <- articles_data$author_affiliate   # extract the author affiliation text
author_pub_year <- articles_data$publication_date %>% 
  parse_date() %>% 
  format(.,'%Y') # extract the publication date

year_affiliate <- data.frame(year = author_pub_year, 
                             affiliation = author_affiliate) # combine year and author affiliation into 1 df
knitr::kable(year_affiliate %>% head(3))



isocodes$name %>% head()    # isocodes$name contains all the country names used by rplos
countries <- lapply(year_affiliate$affiliation, #lapply loops through the affiliation column
function(x){ out <- sapply(isocodes$name, function(z) grepl(z, x)) #sapply loops thru the isocodes$names vector
isocodes$name[out]   #grepl returns TRUE on matches, which is used on the [] subset
})
countries %>% head() 



countries_sort <- sapply(countries, function (x) paste(x,collapse=',')) #collapse multiple countries into a single string
year_affiliate$affiliation <- countries_sort #replace the long affiliation text with only the country names
colnames(year_affiliate)[2] <- "country_name"
knitr::kable(year_affiliate %>% head(5))



is_more_country <- function (x) {        # determines if a countryName has > 1 value
  if (grepl(",",x)) {
    x <- "Joint_research"                # if it does, rename it to Joint_research 
  } else x <- as.character(x)
}
convert_countrynames <- function (x) {   # convert country names into iso3 format
  if (x == "Joint_research") {           # for "Joint_research" country names, 
    "VAT"                                # fill in the country code with VAT
  } else {
    countrycode(x,"country.name","iso3c") #search for a country code for the country provided
  }
}

year_affiliate[["country_name"]] <- year_affiliate[["country_name"]] %>% 
  sapply(., is_more_country) 
year_affiliatyear_affiliate %>% mutate(country_code = sapply(country_name,convert_countrynames))
knitr::kable(year_affiliate %>% head())

countryInfo <- GNcountryInfo() # pull in the general country info
country_area <- countryInfo %>% 
  select(countryName,areaInSqKm,isoAlpha3); #jeep only the name, area and country code
country_area %>% head()        

year_country_area <- merge(year_affiliate,    # merge rplos data with geonames data
                           country_area,
                           by.x = "country_code",
                           by.y = "isoAlpha3")

