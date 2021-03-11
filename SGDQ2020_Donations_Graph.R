''' Install these packages if you have not already
install.packages("tidyverse")
install.packages("rvest")
install.packages("lubridate")
'''
library(magrittr) # Tool used to pipe "%>%"
library(stringr) # Tool used to clean "\n" strings
library(rvest) # Tool used to pull html and css data
library(xml2) # Tool used to read html data
library(ggplot2) # Tool used to graph data
library(lubridate) # Tool used to do date time stuff
library(scales) # Tool used to enhance the graph


# Get SGDQ 2020 data

# Clear the rawdata vector
rawdata <- character()

# There are 675 pages of donations to parse through
# Loop through each of these pages, appending the raw data to the rawdata variable
for (i in 1:675)
{
  # For each iteration of the for loop, change the page number. We use the paste0() function to concatenate the website with the page number, which is derived by the loop
  website <- read_html(paste0("https://gamesdonequick.com/tracker/donations/sgdq2020?page=", i))
  
  # Print the page number so you know how long this process is going to take
  print('Scraping data from page', i)
    
  # Append the rawdata from the website by combining the current rawdata vector with the new website data into the rawdata vector
  rawdata <- c(rawdata, website %>% html_nodes("td") %>% html_text())
}


# From the rawdata, it appears that there are 4 bits of information per row: 1st is the name of the donor, 2nd is the time of the donation, 3rd is the donation amount, and 4th is a Yes/No indicator of if there was a comment. We're only interested in the 2nd and 3rd bits of info.

# Get every 2nd (the time) element 
# This code means starting from the 2nd item in the vector, all the way up to the last element in the vector, iterate by 4
donation_time <- rawdata[seq(2, length(rawdata), 4)]

# Get the 3rd (the donation amount) element
donation_amt <- rawdata[seq(3, length(rawdata), 4)]

# Clean the donation times and convert to PDT
donation_time <- with_tz(ymd_hms(str_sub(donation_time, 2, -2)), tzone = "US/Pacific")

# Clean and convert the donation amounts into numeric
# The gsub function is used to remove commas, which R has trouble translating into numeric (i.e. the number "2,000" needs to become "2000")
donation_amt <- as.numeric(str_extract(gsub(",", "", donation_amt), "\\d*\\.\\d{2}"))


# Bind the donation times to the amounts as a dataframe
data <- data.frame("time" = donation_time, "amount" = donation_amt)

# Order the data frame by the time of the donations
data <- data[order(data$time),]

# Create a running sum (or cumulative sum) of the donation amounts
data$cumsum <- cumsum(data$amount)

# Filter the data to only be the data within 8 days of the start of the event
summer_data <-  subset(data, time < data[1, 'time'] + days(8))

# Create an elapsed time field for the summer_data
summer_data$elapsed_time <- as.numeric(difftime(summer_data$time, data[1, 'time'], units = c('hours')))

# Get the AGDQ 2021 data
# Clear the rawdata vector
rawdata <- character()

# There are 843 pages of donations to parse through
# Loop through each of these pages, appending the raw data to the rawdata variable
for (i in 1:843)
{
  # For each iteration of the for loop, change the page number. We use the paste0() function to concatenate the website with the page number, which is derived by the loop
  website <- read_html(paste0("https://gamesdonequick.com/tracker/donations/AGDQ2021?page=", i))
  
  # Print the page number so you know how long this process is going to take
  print(paste0('Scraping data from page ', i))
  
  # Append the rawdata from the website by combining the current rawdata vector with the new website data into the rawdata vector
  rawdata <- c(rawdata, website %>% html_nodes("td") %>% html_text())
}

# Get every 2nd (the time) element 
# This code means starting from the 2nd item in the vector, all the way up to the last element in the vector, iterate by 4
donation_time <- rawdata[seq(2, length(rawdata), 4)]

# Get the 3rd (the donation amount) element
donation_amt <- rawdata[seq(3, length(rawdata), 4)]

# Clean the donation times and convert to PDT
donation_time <- with_tz(ymd_hms(str_sub(donation_time, 2, -2)), tzone = "US/Pacific")

# Clean and convert the donation amounts into numeric
# The gsub function is used to remove commas, which R has trouble translating into numeric (i.e. the number "2,000" needs to become "2000")
donation_amt <- as.numeric(str_extract(gsub(",", "", donation_amt), "\\d*\\.\\d{2}"))

# Bind the donation times to the amounts as a dataframe
data <- data.frame("time" = donation_time, "amount" = donation_amt)

# Order the data frame by the time of the donations
data <- data[order(data$time),]

# Create a running sum (or cumulative sum) of the donation amounts
data$cumsum <- cumsum(data$amount)

# Filter the data to only be the data within 8 days of the start of the event
winter_data <-  subset(data, time < data[1, 'time'] + days(8))

# Create an elapsed time field for the summer_data
winter_data$elapsed_time <- as.numeric(difftime(winter_data$time, data[1, 'time'], units = c('hours')))

# Plot the running total against time
ggplot() + 
  geom_ribbon(data = summer_data, 
              aes(x = elapsed_time, y = cumsum, ymin = 0),
              fill = "goldenrod1", 
              alpha = 0.5, 
              ymax = summer_data$cumsum) + 
  geom_line(data = summer_data, 
            aes(x= elapsed_time, y = cumsum, color = "SGDQ 2020"),
            size = 1) +
  geom_ribbon(data = winter_data, 
              aes(x = elapsed_time, y = cumsum, ymin = 0), 
              fill = "steelblue", 
              alpha = 0.5,
              ymax = winter_data$cumsum) +
  geom_line(data = winter_data, 
            aes(x = elapsed_time, y = cumsum, color = "AGDQ 2021"), 
            size = 1) +
  ggtitle("Games Done Quick Donations") + 
  ylab("Running Total Donations ($)") + 
  xlab("Hours after GDQ Event Started") + 
  scale_x_continuous(breaks = seq(summer_data$elapsed_time[1], summer_data$elapsed_time[length(summer_data$elapsed_time)], by = 24)) + 
  scale_y_continuous(labels = comma) + 
  scale_color_manual(name = "Event", values = c('SGDQ 2020' = 'gold',
                                'AGDQ 2021' = 'darkblue')) +
  theme_light() + 
  theme(legend.position = c(.15,.75), 
        legend.background = element_blank(), 
        legend.box.background = element_rect())
                                             
