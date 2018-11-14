# This assignment is completed by Wei Yu (UNI: wy2314) 
csv_str = readLines("/Users/helenyu/Desktop/Columbia University/Courses/5200 Framework & Methods
                    /Group Assignment/Deliverable 2/Airbnb_Texas_Rentals.csv", warn = FALSE)
csv_str = iconv(csv_str, from = "WINDOWS-1252", to = "UTF-8", sub = " ")
Encoding(csv_str) <- "UTF-8"
csv = read.csv(text = csv_str, encoding = "UTF-8")

airbnb_data = csv
# identify key data issues
# 1. Unreadable data
# 2. Unformatted data: when measuring number of bedrooms, 
# studio is a misleading term -> change to 0
# 3. missing data (rate, no. of bedroom, latitute and longitude); 
head(airbnb_data, 10)
summary(airbnb_data)

# Examine the dataset
unique(airbnb_data$city)
# By using the above function, we have found that some output are not proper English
which(airbnb_data$city == "诺斯莱克")
which(airbnb_data$city == "阿纳瓦克")
# Identify the entries of "studio"
which(airbnb_data$bedrooms_count == "Studio")
# Identify the incompatible symbol "-"
which(airbnb_data$city == "Bryan–College Station")

# Code for identifying all the missing values within the dataset
airbnb_data[!complete.cases(airbnb_data), ]
# Identify missing values in each main column seperately
which(airbnb_data$average_rate_per_night == "")
which(airbnb_data$bedrooms_count == "")
which(airbnb_data$city == "")
which(airbnb_data$date_of_listing == "")
which(airbnb_data$description == "")
which(airbnb_data$latitude == "NA")
which(airbnb_data$longitude == "NA")

# Now start processing the data
# Before rewrite, we need to convert them into character type
airbnb_data$city <- as.character(airbnb_data$city)
# After locating the unreadable entries, we need to rewrite them in proper English. 
airbnb_data$city[which(airbnb_data$city == "诺斯莱克")] = "Northlake"
airbnb_data$city[which(airbnb_data$city == "阿纳瓦克")] = "Anawak"

# In order to find out the studios, first need to change all the counts to "character" type, 
# because "Studio" is in "character"
airbnb_data$bedrooms_count <- as.character(airbnb_data$bedrooms_count)
# Studios are counted as 0.7 bedroom
airbnb_data$bedrooms_count[which(airbnb_data$bedrooms_count == "Studio")] = "0.7"
# Now change the data type back to numeric
airbnb_data$bedrooms_count <- as.numeric(airbnb_data$bedrooms_count)

# Convert all the entries from dollars to numeric format
airbnb_data$average_rate_per_night <- as.numeric(sub('$','',as.character(
  airbnb_data$average_rate_per_night),fixed=TRUE))
is.numeric(airbnb_data$average_rate_per_night)

# Get rid of the symbol "-" which is incompatible
airbnb_data$city[which(airbnb_data$city == "Bryan–College Station")] = "Bryan College Station"

# Now deal with missing values in the column of average_rate_per_night and bedrooms_count
# Use the data found from the website
airbnb_data$average_rate_per_night[104] = 85
airbnb_data$average_rate_per_night[105] = 30
airbnb_data$average_rate_per_night[168] = 210
airbnb_data$average_rate_per_night[173] = 49
airbnb_data$average_rate_per_night[181] = 32
airbnb_data$average_rate_per_night[182] = 49
airbnb_data$average_rate_per_night[343] = 79
airbnb_data$average_rate_per_night[344] = 109
airbnb_data$average_rate_per_night[345] = 99
airbnb_data$average_rate_per_night[948] = 250
airbnb_data$average_rate_per_night[1123] = 160
airbnb_data$average_rate_per_night[1215] = 120
airbnb_data$average_rate_per_night[1217] = 115
airbnb_data$average_rate_per_night[1219] = 129

airbnb_data$bedrooms_count[14238] = 0.7 # as it is a studio
airbnb_data$bedrooms_count[16812] = 1 # as it is a 1-bedroom property

# Calculate the mean and determine the temporary value 
# for missing values in column average_rate_per_night
sumRate <- sum(airbnb_data$average_rate_per_night, na.rm = TRUE)
sumRoom <- sum(airbnb_data$bedrooms_count, na.rm = TRUE)
meanvalue <- sumRate / sumRoom
print(meanvalue)
airbnb_data$average_rate_per_night[which(is.na(airbnb_data$average_rate_per_night) )] = 
  meanvalue * airbnb_data$bedrooms_count[which(is.na(airbnb_data$average_rate_per_night) )]
# Round up the numbers to integers, i.e. no decimal places
airbnb_data$average_rate_per_night <- round(airbnb_data$average_rate_per_night, digits = 0)

# Fill the missing entry in "bedroom_count" column
airbnb_data$bedrooms_count[6876] = 1

# Finallly check if there is any missing value left in the columns of average_rate_per_night and bedrooms_count
which(airbnb_data$average_rate_per_night == "")
which(airbnb_data$bedrooms_count == "")

library(ggmap)
#Create Texas map
latavg = mean(na.omit(airbnb_data$latitude))
longavg = mean(na.omit(airbnb_data$longitude))
tx_map = get_map(location = c(lon=longavg, lat=latavg), zoom = 6, scale = 2, maptype = "hybrid")

# Create the heat map
airbnb_data_no_na = na.omit(airbnb_data)
ggmap(tx_map, extent = "panel") + 
  geom_density2d(data = airbnb_data_no_na, aes(x = longitude, y =latitude), 
                 size = 0.3) +
  stat_density2d(data = airbnb_data_no_na, aes(x = longitude, y = latitude, 
                                         fill = ..level.., alpha = ..level.. ), size = 0.001, 
                 bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "blue", high = "red",name = "Density") + 
  scale_alpha(range = c(0,0.3), guide = FALSE) +
  ggtitle("Density Distribution of Airbnb Properties in Texas")

library(ggplot2)
library(lubridate)
library(plyr)
# number of airbnb growing with month/year
count_airbnb <- count(airbnb_data$date_of_listing)
#fre_airbnb <- frequency(count_airbnb)
#theme_set(theme_bw())

count_airbnb$x <- as.Date(count_airbnb$x)
plot1 <- ggplot(data=count_airbnb, aes(x=count_airbnb$x, y=count_airbnb$freq, group=1)) +
  geom_line(linetype = "dashed", color="red") + scale_x_date(date_breaks = "1 year", date_labels = "%b %Y") +
  geom_point()
plot1  + labs(title = "Number of Airbnb Property Trend in Texas", x = "Month/Year", y = "Property Counts") 

library(qdap)
# 10 most frequent words used
plot(freq_terms(airbnb_data$description,top = 10,at.least = 3,stopwords = tm::stopwords('english')))

# text mining
library(RColorBrewer)
library(wordcloud)
library(tm)
library(SnowballC)

corpus<- Corpus(VectorSource(airbnb_data$description))

#inspect(corpus)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
corpus <- tm_map(corpus, toSpace, "/")
corpus <- tm_map(corpus, toSpace, "@")
corpus <- tm_map(corpus, toSpace, "\\|")
# Text cleaning
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
# Remove punctuations
corpus <- tm_map(corpus, removePunctuation)
# Eliminate extra white spaces
corpus <- tm_map(corpus, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)
dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(600)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=120, random.order=FALSE, random.color = FALSE, rot.per=0.5,
          colors=brewer.pal(12, "Paired"))

