# import library
library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(shiny)
library(stats)
library(forecast)

library(leaflet)
library(ggmap)
library(gplots)

# Specify the folder path of data
folder_path <- "Data"

# Initialize an empty list to store the data frames
data_list <- list()

# get data set from folder
# Loop over the years from 2017->2022
for (year in 2017:2022) {
  # Construct the subfolder path for the current year
  subfolder_path <- paste0(folder_path, "/data-rf-", year, "/data-rf-", year)
  
  file_names <- list.files(subfolder_path, full.names = TRUE)
  
  # Loop over the file names and read each file
  for (file_name in file_names) {
    # Read the data file
    data <- read.table(file_name, sep = "\t", header = TRUE)
    
    # If the data has only one column, try reading with semicolon separator
    if (ncol(data) == 1) {
      data <- read.table(file_name, sep = ";", header = TRUE)
    }
    
    # Store the data frame in the list using the file path as the key
    print(file_name)
    data_list[[file_name]] <- data
  }
  
}

# # combine periods data for each year dataset in each year
data_years = list()

for (year in 2017:2022) {
  path_file_1 = paste0(folder_path, "/data-rf-", year, "/data-rf-", year, "/", year, "_S1_NB_FER.txt")
  path_file_2 = paste0(folder_path, "/data-rf-", year, "/data-rf-", year, "/", year, "_S2_NB_FER.txt")

  data_file_1 = data_list[[path_file_1]]
  data_file_2 = data_list[[path_file_2]]

  if (year == 2022) {
    names(data_file_2)[names(data_file_2) == 'lda'] = 'ID_REFA_LDA'
  }

  combined_data = rbind(data_file_1, data_file_2)
  key =  paste0(folder_path, "/data-rf-", year, "/data-rf-", year)
  data_years[[key]] = combined_data

}

# Aggregate the data from 2017->2022 into one dataframe
combined_df = data.frame()
for(year in 2017:2022) {
  # Get the data frame for the current year
  key =  paste0(folder_path, "/data-rf-", year, "/data-rf-", year)
  df =  data_years[[key]]

  # Append the aggregated data for the current year to the final data frame
  combined_df = rbind(combined_df, df)
}

# # Read the CSV data file for first period of 2023
data_first_se_2023 = read_csv2("Data/validations-reseau-ferre-nombre-validations-par-jour-1er-semestre.csv",
                                 col_types = cols(
                                 JOUR = col_date(),
                                 CODE_STIF_TRNS = col_integer(),
                                 CODE_STIF_RES = col_integer(),
                                 CODE_STIF_ARRET = col_character(),
                                 LIBELLE_ARRET = col_character(),
                                 lda = col_integer(),
                                 CATEGORIE_TITRE = col_character(),
                                 NB_VALD = col_integer()
                               )
)


# # Change the format of the date column to "day-month-year"
data_first_se_2023$JOUR = format(data_first_se_2023$JOUR, "%d/%m/%Y")

# rename the column of 2023 data to match 2017-2022 data
names(data_first_se_2023)[names(data_first_se_2023) == 'lda'] <- 'ID_REFA_LDA'

# sort the 2023 data as increasing order of the date time
data_first_se_2023 = data_first_se_2023 %>% arrange(JOUR)

#combine data from 2017->2022 with 2023 data
combined_df = rbind(combined_df, data_first_se_2023)

# check the schema of each columns
str(combined_df)

# change the schema of columns
combined_df = combined_df %>%
  mutate_at(vars(CODE_STIF_TRNS, CODE_STIF_RES, CODE_STIF_ARRET, ID_REFA_LDA, NB_VALD), as.integer) %>%
  mutate_at(vars(LIBELLE_ARRET, CATEGORIE_TITRE), as.character)

# check the schema of columns again
str(combined_df)

# check the missing values of each column
missing_values = colSums(is.na(combined_df))
print(missing_values)

# drop rows with missing values
combined_df = combined_df %>%
  filter(!is.na(CODE_STIF_RES)) %>%
  filter(!is.na(ID_REFA_LDA)) %>%
  filter(!is.na(NB_VALD))

# check consistency of column
unique_values = unique(combined_df$CATEGORIE_TITRE)
print(unique_values)

# after checking consistency, we find there is anomaly data as "?" value
# we need to drop anomaly data
combined_df = combined_df %>%
  filter(CATEGORIE_TITRE != '?')

#################
# read the geodata
geo_data = read_csv2("Data/zones-d-arrets.csv")

# check schema of geodata
str(geo_data)

# # change the name of ZdCId column from geodata to the same name of Id_stopping_points column from with 2017-2023 data
# # this will be useful for merge operator
names(geo_data)[names(geo_data) == 'ZdCId'] <- 'ID_REFA_LDA'

# # change the schema of column ID_REFA_LDA of geodata to match with col ID_REFA_LDA of 2017-2023 data
geo_data$ID_REFA_LDA = as.integer(geo_data$ID_REFA_LDA)
str(geo_data)

## sort the geo data according to the most recent update of location
# then we remove duplicated station name
geo_data = geo_data[order(geo_data$ZdAChanged), ]
geo_data = subset(geo_data, !duplicated(ZdAName))

# Merge the two data frames
joined_df = left_join(combined_df, geo_data, by = "ID_REFA_LDA")
# only keep important columns, including longitude and latitude attributes of stopping points
joined_df = joined_df %>% select(1:8, 14, 15)

# ################## Exploratory data analysis
#
# # Converting a column to Date format fortime-series visualization
joined_df$JOUR = as.Date(joined_df$JOUR, format = "%d/%m/%Y")

# Take the mean of the values for duplicated days
joined_df = joined_df %>%
  group_by(JOUR) %>%
  mutate(NB_VALD_MONTH = mean(NB_VALD, na.rm = TRUE)) %>%
  select(c('JOUR', 'LIBELLE_ARRET', 'NB_VALD', 'ZdAXEpsg2154', 'ZdAYEpsg2154', 'NB_VALD_MONTH'))

# Extract month and year as separate columns
joined_df$MONTH = format(joined_df$JOUR, "%m")
joined_df$YEAR = format(joined_df$JOUR, "%Y")
joined_df$DAY = format(joined_df$JOUR, "%d")


# plot seasonality trend to get overview of data
# ggplot(joined_df, aes(x = JOUR, y = NB_VALD, alpha=0.5)) +
#   geom_line() +
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2017-01-01", "2023-05-31"))) +
#   scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

# # Plot the monthly trend
#we can also see outliers at the figure
# ggplot(joined_df, aes(x = MONTH, y = NB_VALD_MONTH)) +
#   geom_boxplot() +
#   facet_wrap(~YEAR, scales = "free_x") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))

# create a map for each station coordinate#####3
# df_unique <- distinct(joined_df, ZdAXEpsg2154, ZdAYEpsg2154, NB_VALD)
# ggplot(df_unique, aes(x = ZdAXEpsg2154, y = ZdAYEpsg2154)) +
#   geom_point() +
#   xlab('ox') +
#   ylab('oy') +
#   ggtitle('Unique Coordinates of each station in Paris region')


################## Define the holiday weeks
# read the calendar data
data_calendar = read.csv("Data/calendar.csv")

### we define holidays = official public holiday such as Chrismast, school breaks, New Year Eve

# Convert the 'date_column' from char to date type
data_calendar$date <- as.Date(data_calendar$date, format='%Y-%m-%d')

# get dates in 2016 (the most updated year in the calendar data)
date_holiday = data_calendar[as.numeric(format(data_calendar$date , "%Y")) == 2016, ] %>%
               select(c('date', 'vacances'))

# Create a pivot table for heatmap
heatmap_data <- date_holiday %>%
  mutate(month = month(date), day = day(date)) %>%
  select(month, day, vacances) %>%
  pivot_wider(names_from = day, values_from = vacances, values_fill=0)

# Convert the pivot table to a matrix
heatmap_matrix <- as.matrix(heatmap_data[, -1])

# # # Create the heatmap
heatmap(heatmap_matrix,
        Rowv = NA,
        Colv = NA,
        col = colorRampPalette(c("white", "gold"))(100),
        scale = "none",
        main = "Heatmap of Vacances",
        xlab = "Day",
        ylab = "Month")


#### add another day and month cols to date_holiday data
date_holiday$MONTH = format(date_holiday$date, "%m")
date_holiday$DAY = format(date_holiday$date, "%d")

# perform left join
joined_df_new = left_join(joined_df, date_holiday, by = c("DAY", "MONTH"))

# change the format of a col from char to int for calculation
joined_df_new$YEAR <- as.integer(joined_df_new$YEAR)

# take the the mean of NB_VALD for each day
df_avg <- joined_df_new %>%
  group_by(JOUR) %>%
  summarize(AVG_NB_VALD = mean(NB_VALD), YEAR = mean(YEAR), VACANCES = mean(vacances))

# Create a line plot with facets for each year and color by vacances
ggplot(df_avg, aes(JOUR, AVG_NB_VALD, color=factor(VACANCES))) +
  geom_line() +
  facet_wrap(~ YEAR, ncol = 1) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(title = "Average Value by Month") +
  theme_bw()

##################### Statistical test

# check the distribution
# the distribution of validation variable is skewed and follow normal distribution
ggplot(joined_df, aes(x = NB_VALD)) +
  geom_histogram() +
  labs(x = "Validations", y = "Frequency", title = "Distribution of Validations") +
  scale_x_continuous(labels = scales::comma)  +
  scale_y_continuous(labels = scales::comma) 

# we transform the validation variable to normally distributed data
# before applying any hypothesis testing
joined_df$NB_VALD = log(joined_df$NB_VALD)

# Hypothesis Testing
# Subset the data before and after 2020
before_2020 <- subset(joined_df, YEAR < 2020)$NB_VALD
after_2020 <- subset(joined_df, YEAR >= 2020)$NB_VALD

# Perform t-test
ttest <- t.test(before_2020, after_2020)
print(ttest)

#### very small t-value suggest that the difference between 2020
### and 2020 is happened systematically not randomly
#### the change maybe due to covide 19 period factor

# seasonal Analysis
august_data <- subset(joined_df, MONTH == '08')$NB_VALD

# Subset the data for other months
other_months_data <- subset(joined_df, MONTH != '8')$NB_VALD

# Perform t-test
ttest_seasonal <- t.test(august_data, other_months_data)
print(ttest_seasonal)

# checking the impact of holiday period
holiday_data = subset(joined_df_new, vacances == 1)$NB_VALD
non_holiday_data = subset(joined_df_new, vacances == !1)$NB_VALD

t_test_impact_holiday = t.test(holiday_data, non_holiday_data)
print(t_test_impact_holiday)


# 3 Time Series Forecasting 
# Get the start and end dates
start_date <- min(df_avg$JOUR)
end_date <- max(df_avg$JOUR)
end_date

# Create a time series object using the AVG_NB_VALD column
ts_data <- ts(df_avg$AVG_NB_VALD, start = c(year(start_date), month(start_date)), end = c(year(end_date), month(end_date)), frequency = 365)
ts_data


plot(ts_data)

ts_data_predict = HoltWinters(ts_data, beta=FALSE, gamma=FALSE)
ts_data_predict
fitted_values = ts_data_predict$fitted


# Plot the fitted values in red and the actual values in black
plot(ts_data_predict, main = "Holt-Winters Forecast", xlab = "Time", ylab = "Values")
points(fitted_values, col = "red", pch = 16)
points(ts_data, col = "black", pch = 16, cex = 0.7, lty = "dashed")
legend("topleft", legend = c("Estimated", "Actual"), col = c("red", "black", "green"), pch = 16, lty = c(0, 1), cex = 0.8)



##########
forecast_data = forecast(ts_data_predict, h=365)
forecast_data

plot(forecast_data, col='blue', pch = 16, lty = "dashed", xlab = "Time", ylab = "Mean validations per day")
points(ts_data, col='orange', pch = 16, cex = 0.4, lty = "dashed")
legend("topleft", legend = c("Estimated", "Actual"), col = c("blue", "orange"), pch = 16, lty = c(0, 1), cex = 0.8)





################

############################# create shiny app - Oussou tasks
#############################
## show 5 types of plot

# first plot as yearly trend
# ggplot(joined_df, aes(x = JOUR, y = NB_VALD, alpha=0.5)) +
#   geom_line() +
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2017-01-01", "2023-05-31"))) +
#   scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))


# second  plot
# # Plot the monthly trend
# we can also see outliers at the figure
# ggplot(joined_df, aes(x = MONTH, y = NB_VALD_MONTH)) +
#   geom_boxplot() +
#   facet_wrap(~YEAR, scales = "free_x") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))

# third plot


############ develop shiny App
# Define the user interface
ui <- fluidPage(
  titlePanel("Île-de-France Railway Stations Ridership Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      # Inputs
      dateRangeInput(
        inputId = "dateRange",
        label = "Choose Date Range",
        start = "2017-01-01", # Use a reasonable start date
        end = Sys.Date() # Defaults to the current date
      ),
      selectInput(
        inputId = "stationSelect",
        label = "Choose a Station",
        choices = c("Station A", "Station B"), # Placeholder, replace with actual station names
        selected = "Station A"
      ),
      actionButton("update", "Update View")
    ),
    
    mainPanel(
      # Outputs
      tabsetPanel(
        tabPanel("Map View", leafletOutput(outputId = "map")),
        tabPanel("Time Series", plotOutput(outputId = "timeSeries"))
      )
    )
  )
  
  
)


