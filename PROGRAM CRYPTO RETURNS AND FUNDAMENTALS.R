
# PROGRAM CRYPTO RETURNS AND FUNDAMENTALS


install.packages("httr")
install.packages("jsonlite")
# Run to install the packages (not active)

library(httr)
library(jsonlite)
# Run to activate the packages


# 1. Create the database
# a. Data extraction

myList <- NULL                              # Assigning a value to the variable 'mylist'
x <- c("eth", "ada", "xrp", "xtz", "trx")   # Assigning a vector with the chosen crypto to a variable 'x'

for(i in x) {     # The loop imports data from an API and assign values for each parameters (price in USD, market cap in USD, Network size) of each crypto. 
  
  parameters <- list(assets = i, 
                 metrics = "PriceUSD, AdrActCnt, CapMrktCurUSD",
                 frequency =  "1d",
                 start_time = "20000101",
                 page_size = 4000)
  
  name <- paste(i)
  myList[[name]]  <- GET("https://community-api.coinmetrics.io/v4/timeseries/asset-metrics", query = parameters)    # Groups the data extracted for each crypto in variable 'myList'
} 


# b. Error check in the data extraction

statusCode <- list()    # Assigning a list to the variable 'statusCode' that will store the request response for each crypto (success or not)

for (i in x) {          # The loop extract each status code for each assets and assigned them to the variable 'statusCode'
  
   statusCode[[i]] <- myList[[i]]$status_code
 
   if (statusCode[[i]] != 200) {
   print("There is an error in a crypto acronym line 18")
   }
}


# c. Convert the data and create dataframes

data <- list()   # Assigning a list to variable 'data'

for (i in x) {
   
   data[i] <- fromJSON(rawToChar(myList[[i]][["content"]]), flatten = TRUE)   # The 'fromJson' function converts data in JSON form to R objects, while 'rawToChar' function converts raw bytes to a single character string or to a character vector of single bytes
   data[[i]] <- na.omit(data[[i]])                                            # Removes 'NA' values in the data 
}

for (i in x) {   # The loop converts 'AdrActCnt' and 'CapMrktCurUSD' values as numeric instead of characters
   
   data[[i]]$AdrActCnt <- as.numeric(data[[i]]$AdrActCnt) 
   data[[i]]$CapMrktCurUSD <- as.numeric(data[[i]]$CapMrktCurUSD) 
   
   assign(i,data.frame(data[i]))   # Creates an individual dataframe for each crypto 
}



# 2. Mathematical application
# a. Regression calculation

regression <- list()   # Assigning a list to variable 'regression'

for (i in x) {   # The loop compute the parameters of a simple linear regression model for each crypto and store the results into the variable 'regression'
  
  regression[[i]] <- lm(log(data[[i]]$CapMrktCurUSD) ~ log(data[[i]]$AdrActCnt), data = data[[i]])
}

summary <- list()

for (i in x) {
  summary[[i]] <-summary(regression[[i]])
}

# b. Create a dataframe of the results

alpha <- list()
beta <- list()     
rSquared <- list()

for (i in x) {    # The loop extract alpha, beta and R^2 from the regressions results for each crypto
  
  alpha[i] <- regression[[i]]$coefficients[1]
  beta[i] <- regression[[i]]$coefficients[2]
  rSquared[i]<-summary[[i]]$r.squared
}

results <- cbind(alpha, beta, rSquared)   # Groups each coefficients in a dataframe called 'results'


# c. Compute a weighted alpha and beta

x1 <- c()
x2 <- c()
weight <- list()

for (i in x) {   # The loop assign alphas values to 'x1', beta values to 'x2' and a mean of the network size to 'weight' of each crypto
  
  x1[i] <- alpha[i]
  x2[i] <- beta[i]
  weight[i] <- mean(data[[i]]$AdrActCnt)
}

weight <- cbind.data.frame(weight)
x1 <- cbind.data.frame(x1)
x2 <- cbind.data.frame(x2)

alphaWeightedMean <- weighted.mean(x1, weight)   # Compute the alpha weighted mean 
betaWeightedMean <- weighted.mean(x2, weight)    # Compute the beta weighted mean 

# Update the 'results' dataframe with alpha and beta for each crypto and the weighted mean of alpha and beta
results <- rbind(results, c(alphaWeightedMean, betaWeightedMean))
row.names(results) = c(x, "Weighted Mean")



# 3. Print the regression's graphs
## Using 'plot' function to plot the data for each crypto and create the regression line with abline

par(mfrow = c(3, 2)) # Groups all the graphs together

for (i in x) {
  
  plot(log(data[[i]]$AdrActCnt),
       log(data[[i]]$CapMrktCurUSD),
       main = i,
       xlab = "Log Active Adresses",
       ylab = "Log MarketCap",
       ylim = c(10, 40),
       col = "blue")
  abline(regression[[i]], col = "red")
}
results # Printings the results

"We hereby certify that
– We have written the program ourselves except for clearly marked pieces of code
– We have tested the program and it ran without crashing (if applicable)

Baranzini Guillaume 22-992-549
Renard Emeric" 


