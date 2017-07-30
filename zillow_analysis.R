library(RCurl)
library(XML)
library(tidyverse)
library(lubridate)
library(ggmap)

zillow.sample.size <- 1000

zid <- 'X1-ZWz1ewo9h1s07f_89sjr'
fields <- c('//zpid', '//street', '//zipcode', '//city', '//state', '//longitude',
            '//latitude', '//FIPScounty', '//useCode', '//taxAssessmentYear',
            '//taxAssessment', '//yearBuilt', '//lotSizeSqFt', '//finishedSqFt',
            '//bathrooms', '//bedrooms', '//totalRooms', '//lastSoldDate', '//lastSoldPrice',
            '//amount', '//last-updated', '//valueChange', '//low', '//high')

## Grabs the search results for one property using adress and city/state or zip
getDSR <- function(adrs, csz){
  return(getForm('http://www.zillow.com/webservice/GetDeepSearchResults.htm',
                 'zws-id' = zid,
                 'address' = adrs,
                 'citystatezip' = csz))
}

#test <- as.vector(read.csv('/Users/stevensandiford/Home data analysis/jpzone.csv'))
test.group <- read_csv('/Users/stevensandiford/Home data analysis/jpzone.csv')
test.sample <- sample(1:nrow(test.group), zillow.sample.size, replace=FALSE)
test.group <- test.group[test.sample,]

n <- length(fields)
nHomes <- nrow(test.group)

### Run through sample list and grab Zillow detailed info ###
list1 <- list()
for (x in 1:nHomes) {
  r <- getDSR(test.group[x,2],'02130')
  doc <- xmlTreeParse(r, asText=TRUE, useInternal=TRUE)
  list2 <- list()
  if (xmlValue(doc[['//code']]) != 0) {
    for (i in 1:n) {
      newRow <- 'ERROR'
      list2[[i]] <- newRow 
    }
    names(list2) <- fields
    eRow <- do.call(cbind, list2)
    list1[[x]] <- eRow
    next
  }
  else {
    for (i in 1:n) {
      newRow <- tryCatch({ xmlValue(doc[[fields[i]]]) },
                          warning = function(w) {'warning'},
                          error = function(e) {'error for field'})
      #newRow <- xmlValue(doc[[fields[i]]])
      list2[[i]] <- newRow 
    }
    names(list2) <- fields
    sRow <- do.call(cbind, list2)
    list1[[x]] <- sRow
  }
}
matrix <- do.call(rbind, list1)
colnames(matrix) <- gsub("//", "", colnames(matrix))

homesDf <- as.data.frame(matrix)
homesDf <- homesDf %>% filter(zpid != 'ERROR' & zpid != 'warning')
homesDf <- homesDf %>% mutate(last.updated = mdy(`last-updated`), 
                              last.sold.date = mdy(lastSoldDate),
                              zillow.value = as.numeric(as.character(amount)),
                              finished.sqft = as.numeric(as.character(finishedSqFt)))
homesDf <- homesDf %>% mutate(avg.value.per.sqft = zillow.value / finished.sqft)

### Quick boxplot ###
plot1 <- ggplot(homesDf %>% filter(!is.na(avg.value.per.sqft) & 
                                       avg.value.per.sqft <=1500 &
                                       useCode %in% c('Condominium', 'Duplex', 'MultiFamily2To4','SingleFamily','Triplex'))
                , aes(x=useCode, y=avg.value.per.sqft)) +
                geom_boxplot()
plot1

### Quick summary views of zillow data ###
homesDf %>% 
    group_by(useCode) %>% 
    summarize(avg.size = mean(finished.sqft),
              avg.value.per.sf = sum(zillow.value, na.rm=T) / sum(finished.sqft, na.rm=T),
              record.count = n())

### Get Lat/Long for key locations in jamaica plain ###
sb <- geocode("Stony brook t station, 02130", source = "google")
gs <- geocode("Green Street t station, 02130", source = "google")
js <- geocode("Jackson Square, Boston, 02130", source = "google")
wf <- geocode("413 Centre St Boston, MA 02130", source = "google")
sas <- geocode("301 Centre St Boston, MA 02130", source = "google")
csc <- geocode("669A Centre St Boston, MA 02130", source = "google")
jpbh <- geocode("507 Jamaicaway, Jamaica Plain, MA 02130", source = "google")

keyLocs <- rbind(sb,gs,js,wf,sas,csc,jpbh)
nl_1 <- list()

for (x in 1:5) { #returned 142 rows of data
  nl_2 <- list()
  details <- list()
  for (i in 1:nrow(keyLocs)) {
    q <- mapdist(as.numeric(as.list(c(as.character(homesDf[x,6]),as.character(homesDf[x,7])))), as.numeric(as.list(keyLocs[i,])), mode = "walking")
    q1 <- q[,7]
    nl_2[[i]] <- q1
    details[[i]] <- paste(homesDf[x,1],homesDf[x,2])
  }
  coldf <- do.call(cbind, nl_2)
  nl_1[[x]] <- coldf
}
m <- do.call(rbind, nl_1)
colnames(m) <- c('stonybrook', 'green_street', 'wrong_jackson_sq', 'wholefoods',
              'stop_and_shop', 'centre_st_cafe', 'jp_boat_house')
nrow(homesDf)-142
newdf <- cbind(homesDf[143:nrow(homesDf),], m)
