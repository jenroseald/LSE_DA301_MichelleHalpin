## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 4. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 5. Include your insights and observations.



############ Week 4 #######################################


#### 4.1 Import libraries and explore data ####

# Import libraries
library(tidyverse)
library(ggplot2)
library(reshape2)

# Set working directory

# Import turtle_sales csv data
sales <- read.csv('turtle_sales.csv', header=TRUE)

# Sense check data
str(sales)
dim(sales)
class(sales)

# View the data frame
View(sales)

# Print the data frame
as_tibble(sales)
glimpse(sales)



#### 4.2 Prepare data for analysis ####

# Create a new data frame from a subset of the sales data frame
# Remove unnecessary columns using dplyr select(): 
sales2 <- select(sales, -Ranking, -Year, -Genre, -Publisher)

# View data frame
as_tibble(sales2)
dim(sales2)

# View descriptive statistics
summary(sales2)

# Convert ‘Product’ to factor (categorical variable).
sales_factor <- mutate(sales2,
                       Product=as.factor(Product))

# Check data types
glimpse(sales_factor)



#### 4.3 Review plot types to determine insights ####

# 4.3.1 Scatter plots

# Global Sales and Product
# x is Product, y is Global Sales
qplot(Product, Global_Sales, colour=Platform, data=sales_factor)

# EU Sales and Product
# x is Product, y is EU Sales
qplot(Product, EU_Sales, colour=Platform, data=sales_factor)

# NA Sales and Product
# x is Product, y is NA Sales
qplot(Product, NA_Sales, colour=Platform, data=sales_factor)


# 4.3.2 Histograms

# Product is x variable
qplot(Product, data=sales_factor)

# Platform is x variable
# Determine how many unique Platform types
length(unique(sales_factor$Platform))
# Adjust histogram bins to 22 to match number of platform types
qplot(Platform, bins=22, data=sales_factor)

# Global Sales is x variable
qplot(Global_Sales, data=sales_factor)

# EU Sales is x variable
qplot(EU_Sales, data=sales_factor)

# NA Sales is x variable
qplot(NA_Sales, data=sales_factor)


# 4.3.3. Box plots

# Global_Sales
# Set x to Global_Sales
qplot(Global_Sales, data=sales_factor, colour=I('blue'), geom='boxplot')

# EU_Sales
# Set x to EU_Sales
qplot(EU_Sales, data=sales_factor, colour=I('blue'), geom='boxplot')

# NA_Sales
# Set x to NA_Sales
qplot(NA_Sales, data=sales_factor, colour=I('blue'), geom='boxplot')


#### 4.3.4 Exploratory visualisation observations ####

# Scatter plots:

# High to low sales as Product ID number increases observed in all regions
# Even distribution of platform purchases across all sales and regions
# Global sales: outlier identified at 60+ sales
# EU Sales: outlier identified at 20+ sales
# NA Sales: outlier identified at 30+ sales

# Histograms:

# Non-aggregated Product variable not effective for histogram visualisation
# Top 5 selling plaftorms are: X360, PSC, PC, Wii, DS
# Non-normal sales distribution observed in all regions
# Sales distribution is leptokutic (heavy tailed)

# Boxplots:

# All region distributions support outlier findings observed in scatter plots
# Most sale values are less than £5



#### 4.4 Determine impact on sales per product ####

# 4.4.1 Group by Product (factor) and sum sales
sales_by_product = sales_factor %>% group_by(Product)  %>%
  summarise(Europe = sum(EU_Sales),
            NAmerica = sum(NA_Sales),
            Global = sum(Global_Sales),
            .groups = 'drop')

# Explore data frame
as_tibble(sales_by_product)
dim(sales_by_product)

# 4.4.2 Identify top selling products by region

# Sort top products by Europe region
arrange(sales_by_product, desc(Europe))
# Top 10 selling products in Europe are:
# 107, 515, 195, 3967, 2371, 876, 3645, 979, 231, 399

# Sort top products by North America region
arrange(sales_by_product, desc(NAmerica))
# Top 10 selling products in Europe are:
# 107, 123, 326, 254, 515, 948, 535, 195, 231, 876

# Sort top products by Global region
arrange(sales_by_product, desc(Global))
# Top 10 selling products in Europe are:
# 107, 515, 123, 254, 195, 231, 249, 948, 876, 263


#### 4.4.3 Product sales popularity findings: ####

# (popularity is defined as being within top 10 selling products)

# Top selling products popular in all regions: 
# 107, 515, 195, 876, 231 and 515 

# Top selling product popular in only one region:
# Products only popular in Europe: 3967, 2371, 3645, 979, 399
# Products only popular in North America: 326, 535
# Products only popular in Global area: 249, 263

# Products 123, 254, 948 are not popular in Europe


# 4.4.3 Descriptive statistics for sales by product (product as factor)
summary(sales_by_product)

#### 4.4.4 Regional average, Min, Max sales by product ####

# Average sales by region:
# Europe: £3.36, NAmerica: £5.06, Global: £10.73

# Minimum sales by region:
# Europe: £0, NAmerica: £0.06, Global: £4.20

# Maximum sales by region:
# Europe: £23.80, NAmerica: £34.02, Global: £67.85


# 4.4.5 Create additional data frame where Product data type is integer

# Group by Product (as integer) and sum sales
sales_by_product_int = sales2 %>% group_by(Product)  %>%
  summarise(Europe = sum(EU_Sales),
            NAmerica = sum(NA_Sales),
            Global = sum(Global_Sales),
            .groups = 'drop')

# Explore data frame
as_tibble(sales_by_product_int)
dim(sales_by_product_int)

# Descriptive statistics for sales by product (product as integer)
summary(sales_by_product_int)


# 4.4.6 Create additional data frame that groups sales by Platform

# Group by Platform and sum of sales
sales_grp_plats = sales_factor %>% group_by(Platform)  %>%
  summarise(total_EUsales = sum(EU_Sales),
            total_NAsales = sum(NA_Sales),
            total_ALLsales = sum(Global_Sales),
            .groups = 'drop')

# View the data frame
as_tibble(sales_grp_plats)
dim(sales_grp_plats)


#### 4.5 Determine plot type effectiveness ####

# 4.5.1 Scatter plots

# Global product sales
# x is Product, y is Global Sales
qplot(Product, Global, data=sales_by_product,
      geom=c('point', 'jitter'))

# Europe product sales
# x is Product, y is Global Sales
qplot(Product, Europe, data=sales_by_product,
      geom=c('point', 'jitter'))

# NAmerica product sales
# x is Product, y is Global Sales
qplot(Product, NAmerica, data=sales_by_product,
      geom=c('point', 'jitter'))


# 4.5.2 Histograms

# Global product sales: 
# x is Global
# 20 bins
qplot(Global, bins=20, data=sales_by_product)

# Europe product sales: 
# x is Europe
# 20 bins
qplot(Europe, bins=20, data=sales_by_product)

# NAmerica product sales: 
# x is NAmerica
# 20 bins
qplot(NAmerica, bins=20, data=sales_by_product)


# 4.5.3 Box plots

# Global product sales 
# x is Global sales
qplot(Global, data=sales_by_product, 
      colour=I('tomato2'), geom='boxplot')

# Europe product sales 
# x is Europe sales
qplot(Europe, data=sales_by_product, 
      colour=I('tomato2'), geom='boxplot')

# NAmerica product sales 
# x is NAmerica sales
qplot(NAmerica, data=sales_by_product, 
      colour=I('tomato2'), geom='boxplot')



#### 4.5.4 Plot type observations and insights ####

# Scatter plots work well to depict relationship trends: 
# value of sales decreases as product ID increases from 1 to 9000

# Histograms show sales in all regions are not normally distributed
# They also show most common sales value is low at under £5

# Box plots findings support outlier identification shown in scatter plots
# They also support most common lower sales values

# Recommendation is to not remove outliers but instead advise
# further investigation into these higher sales by product values
# Higher values could result from higher priced product types



############ Week 5 #######################################

# Week 5 assignment: Cleaning and manipulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 3. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 4. Include your insights and observations.


#### 5.1 Import libraries and explore data ####

# Import libraries
library(skimr)
library(DataExplorer)
library(ggplot2)
library(moments)
library(BSDA)

# Set working directory
# Load / explore the data set from week 4: sales_by_product
# This data frame groups sales by product - product is factor data type

# View data frame created in Week 4
as_tibble(sales_by_product)
glimpse(sales_by_product)

# Check for missing values
sum(is.na(sales_by_product))



#### 5.2 Determine descriptive statistics: sales by product ####

# Alternate way to calculate min and max sales
# apply(sales_new, 2, min)
# apply(sales_new, 2, max)

# 5.2.1 Create individual counts for mean, min, max and sum

# Average mean sales: North America, Europe, Global
sales_by_product_means <- summarise(sales_by_product,
                                    Europe = mean(Europe, 2),
                                    NAmerica = mean(NAmerica, 2),
                                    Global = mean(Global, 2))


# Minimum sales: North America, Europe, Global
sales_by_product_mins <- summarise(sales_by_product, 
                                   Europe = min(Europe, 2),
                                   NAmerica = min(NAmerica, 2),
                                   Global = min(Global, 2))


# Maximum sales: North America, Europe, Global
sales_by_product_maxs <- summarise(sales_by_product, 
                                   Europe = max(Europe, 2),
                                   NAmerica = max(NAmerica, 2),
                                   Global = max(Global, 2))


# Sum of sales: North America, Europe, Global
sales_by_product_sums <- summarise(sales_by_product, 
                                   Europe = sum(Europe, 2),
                                   NAmerica = sum(NAmerica, 2),
                                   Global = sum(Global, 2))


# 5.2.2 Combine individual stats into overview data frame
sales_overview <- rbind(sales_by_product_means, 
                        sales_by_product_mins, 
                        sales_by_product_maxs, 
                        sales_by_product_sums) 

#round values to 2 decimal places
round(sales_overview, digits = 2)

# Name rows to indicate mean, min, max and sum
rownames(sales_overview) = c("Mean", "Min", "Max", "Sum")

# View Overview summary data frame
View(sales_overview)  

# Export data frame as CSV file
write.csv(data, file='sales_overview.csv')

# 5.2.3 Create and visualise summary statistics with non-aggreagted sales data
DataExplorer::create_report(sales_factor)


#### 5.2.4 Observations of regional sales mean, min, max and sum totals ####

# Average, maximum and minimum sales are highest in the global region
# Average, maximum and minimum sales are lowest in the Europe region



#### 5.3 Determine normality of data set ####

# 5.3.1 Sales Quantile Quantile Plots (sales_by_product data frame)

# Global Sales QQ Plot
qqnorm(sales_by_product$Global, 
       main='Q-Q Plot for Global Sales by Product Normality',
       col='steelblue',
       xlab="z Value",
       ylab='Global Sales by Product')

# Add a reference line to Global Sales qqplot.
qqline(sales_by_product$Global,
       col='tomato2',
       lwd=2)

# EU Sales QQ Plot
qqnorm(sales_by_product$Europe,
       main='Q-Q Plot for EU Sales by Product Normality',
       col='steelblue',
       xlab="z Value",
       ylab='EU Sales by product')

# Add a reference line to EU Sales qqplot.
qqline(sales_by_product$Europe,
       col='tomato2',
       lwd=2)

## NA Sales QQ Plot
qqnorm(sales_by_product$NAmerica,
       main='Q-Q Plot for NA Sales Normality',
       col='steelblue',
       xlab="z Value",
       ylab='NA Sales by product')

# Add a reference line to the qqplot.
qqline(sales_by_product$NAmerica,
       col='tomato2',
       lwd=2)


#### 5.3.3 QQ plot observations ####

# All regions of QQ plots indicate a non-normal distribution:
# The 'S' shaped curve of data points indicates data is right skewed
# The 'fat' tails of the data points also indicate non-normal distribution



#### 5.4 Perform Shapiro-Wilk test ####

# Ensure moments package is installed and imported

#### 5.4.1 Global Sales S-W test and findings ####

# Shapiro-Wilk test
shapiro.test(sales_by_product$Global)
# Skewness and kurtosis
skewness(sales_by_product$Global)
kurtosis(sales_by_product$Global)
# Standard deviation.
sd(sales_by_product$Global) 

# P-value: 2.2e-16
# Less than 0.05, therefore normal distribution is not assumed
# Skewness is 3.066769
# Data is highly skewed to the right
# Kurtosis: 17.79072
# As this value is much greater than 3, data is leptokurtic 
# and very heavy tailed
# Standard Deviation: 8.129224
# Highest SD is observed within the Global sales region


#### 5.4.2 Europe Sales S-W test and findings ####

# Shapiro-Wilk test
shapiro.test(sales_by_product$Europe)
# Skewness and kurtosis
skewness(sales_by_product$Europe)
kurtosis(sales_by_product$Europe)
# Standard deviation.
sd(sales_by_product$Europe)

# P-value: 2.987e-16
# Less than 0.05, therefore normal distribution is not assumed
# Skewness is 2.886029
# Data is highly skewed to the right
# Kurtosis: 16.22554
# As this value is much greater than 3, data is leptokurtic 
# and very heavy tailed
# Standard Deviation: 3.083948
# Lowest SD is observed within the Europe region


#### 5.4.3 NAmerica Sales S-W test and findings ####

# Shapiro-Wilk test
shapiro.test(sales_by_product$NAmerica)
# Skewness and kurtosis
skewness(sales_by_product$NAmerica)
kurtosis(sales_by_product$NAmerica)
# Standard deviation.
sd(sales_by_product$NAmerica) 

# P-value: 2.92e-16
# Less than 0.05, therefore normal distribution is not assumed
# Skewness is 3.048198
# Data is highly skewed to the right
# Kurtosis: 15.6026
# As this value is much greater than 3, data is leptokurtic 
# and very heavy tailed
# Standard Deviation: 4.556351


#### 5.4.4 Determine correlations between sales regions ####

# Correlation between Europe and NAmerica sales
cor(sales_by_product$Europe, sales_by_product$NAmerica)
# R = 0.6209317
# Pearsons correlation coefficient is greater than zero
# Therefore positive association between variables

# Correlation between Europe and Global sales
cor(sales_by_product$Europe, sales_by_product$Global)
# R = 0.8486148
# Pearsons correlation coefficient is near to 1
# Therefore strong positive association between variables

# Correlation between NAmerica and Global sales
cor(sales_by_product$NAmerica, sales_by_product$Global)
# R = 0.9162292
# Pearsons correlation coefficient is almost 1
# Therefore very strong positive association between variables

# Strength of correlation between sales variables can be ranked as:
# 1. North America and Global - strongest correlation
# 2. Europe and Global - second strongest correlation
# 3. Europe and North America - third, moderate correlation



#### 5.5 Plot data to gain insights ####

# 5.5.1 Prepare descriptive summary data for plots

# Melt sales_means data frame
meansmelt <- reshape2::melt(sales_by_product_means)
 # View head of melted data frame
head(meansmelt)
# Rename meansmelt columns to Region and Mean
colnames(meansmelt) = c("Region", "Mean")

# Melt sales_mins data frame
minsmelt <- reshape2::melt(sales_by_product_mins)
# View head of melted data frame
head(minsmelt)
# Rename minsmelt columns to Region and Min
colnames(minsmelt) = c("Region", "Min")

# Melt sales_maxs data frame
maxsmelt <- reshape2::melt(sales_by_product_maxs)
# View head of melted data frame
head(maxsmelt)
# Rename maxsmelt columns to Region and Max
colnames(maxsmelt) = c("Region", "Max")

# Use dylpr to left join means melt and minsmelt
stat <- left_join(meansmelt, minsmelt, by='Region')

# Use dylpr to left join statsmelt and maxsmelt
all_stats <- left_join(stat, maxsmelt, by='Region')

# View all_stats data frame
View(all_stats)

# Export data frame as CSV file
write.csv(data, file='all_stats.csv')


#### 5.5.2 Plot mean, min, max with bar and scatter plots ####

# Create Bar Plots to visualise Mean, Min and Max Sales by region

# Bar plot: mean sales by Region
ggplot(all_stats, aes(x=Region, y=Mean)) + 
  geom_bar(stat="identity", width=.5, fill="steelblue") + 
  labs(title="Mean Sales by Region", 
       subtitle="Turtle Games", 
       caption="Games Sales Analysis") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

# Bar plot: min sales by Region
ggplot(all_stats, aes(x=Region, y=Min)) + 
  geom_bar(stat="identity", width=.5, fill="steelblue") + 
  labs(title="Minimum Sales by Region", 
       subtitle="Turtle Games", 
       caption="Games Sales Analysis") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

# Bar plot: max sales by Region
ggplot(all_stats, aes(x=Region, y=Max)) + 
  geom_bar(stat="identity", width=.5, fill="steelblue") + 
  labs(title="Maximum Sales by Region", 
       subtitle="Turtle Games", 
       caption="Games Sales Analysis") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


#### Plot mean, min, max as scatter plot ####

# Prepare data to combine visualised Mean, Min and Max by region
# Melt all_stats data frame
regionmelt <- melt(all_stats)
# View melted data frame
View(regionmelt)
# Rename regionmelt columns to Region and Max
colnames(regionmelt) = c("Region", "Measure", "SaleValue")

#Create scatter plot for Mean, Min, Max regional sales by product
ggplot(data=regionmelt,
       mapping=aes(x=Region, y=SaleValue, color=Measure)) +
  geom_point(alpha=.9, size=4) +
  scale_color_manual(values=c('red', 'blue', 'orange')) +
  labs(title="Relationship between Products and Sales",
       subtitle="Turtle Games",
       caption="Turtle Sales Analysis",
       x="Product IDs 1 to 9000",
       y="Value of Sales",
       color="Sales Region") +
  theme_minimal() 



#### 5.5.3 Plot sales data: visualise with overlays ####

# Prepare data for KDP
# Melt sales_by_product data frame (Product variable is factor)
salesmelt <- reshape2::melt(sales_by_product)
# View melted data frame
View(salesmelt)
# Rename sales columns to Product, Region and Sale
colnames(salesmelt) = c("Product", "Region", "Sale")

# Melted sales data: KDP to overlay sale density by regions
ggplot(salesmelt, aes(x=Sale, fill=Region)) +
  geom_density(alpha=0.25) +
  labs(title="Gaming Sales Density by Region",
       x="Gaming Sales",
       y="Sales Density",
       fill="Sales Region",
       subtitle="Turtle Games",
       caption="Turtle Sales Analysis") +
  theme_minimal()

# Melted sales data: histogram to overlay sale density by region
ggplot(salesmelt, aes(x=Sale, fill=Region)) +
  geom_histogram(alpha=0.4) + 
  scale_fill_manual(values=c('red', 'blue', 'orange')) +
  labs(x="Gaming Sales",
       title="Gaming Sales Distribution by Region",
       subtitle="Turtle Games",
       caption="Turtle Sales Analysis") +
  theme_minimal()

# Melted sales data: box plot for Sales by Region
ggplot(salesmelt, aes(x=Region, y=Sale)) +
  geom_boxplot(fill='steelblue', notch=TRUE, outlier.color='red') +
  labs(title="Distribution of Sales by Region",
       subtitle="Turtle Games",
       caption="Turtle Sales Analysis",
       x="Sales Regions",
       y="Value of Sales") +
  theme_minimal()


#### 5.5.4 Product/sales/region scatter plot with trend line ####

# Use sales_by_product_int data frame to create scatter plot with trend line
# Product variable data type is integer
# Integer type needed to add a trend line

# Reshape sales_by_product_int wide to long
sales_by_intprod <- reshape(data=sales_by_product_int, idvar="Product",
                varying=c("Europe","NAmerica", "Global"),
                v.name=c("value"),
                times=c("Europe","NAmerica", "Global"),
                new.row.names = 1:1000,
                direction="long")

# Rename sales columns to Product, Region and Sale
colnames(sales_by_intprod) = c("Product", "Region", "Sale")
# View data frame
View(sales_by_intprod)

# Reshaped data: scatter plot for Product and Sales relationship (by region)
ggplot(data=sales_by_intprod,
       mapping=aes(x=Product, y=Sale, color=Region)) +
  geom_point(alpha=.6, size=1.5) +
  geom_smooth(method='lm', se=FALSE, size=1.1) +
  scale_color_manual(values=c('tomato2', 'steelblue', 'orange')) +
  labs(title="Relationship between Products and Sales",
       subtitle="Turtle Games",
       caption="Turtle Sales Analysis",
       x="Product IDs 1 to 9000",
       y="Value of Sales",
       color="Sales Region") +
  theme_minimal()


#### 5.5.5 Plot observations and insights ####

# All plot options display the following findings:
# Sales values are highest in global regions
# North America regions shows second highest sales values
# Europe region has lowest sales values



############ Week 6 #######################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.


#### 6.1 Install, import libraries and load data ####

# Install libraries
# Import the necessary libraries
library(ggplot2)
library(forecast)
library(tseries)

# Load and explore the data created in Week 5
View(sales_by_product_int)
summary(sales_by_product_int)


#### 6.2 Explore regional sales correlations ####

# Correlation between Europe and North America sales
cor(sales_by_product_int$Europe, sales_by_product_int$NAmerica)
# R = 0.86209317
# Pearsons correlation coefficient is near to 1
# Therefore strong positive association between variables

# Correlation between Europe and Global sales
cor(sales_by_product$Europe, sales_by_product$Global)
# R = 0.8486148
# Pearsons correlation coefficient is near to 1
# Therefore strong positive association between variables

# Correlation between NAmerica and Global sales
cor(sales_by_product$NAmerica, sales_by_product$Global)
# R = 0.9162292
# Pearsons correlation coefficient is almost 1
# Therefore very strong positive association between variables


#### 6.2.1 Regional sales correlation findings ####

# Strength of correlation between sales variables can be ranked as:
# 1. North America and Global - strongest correlation
# 2. Europe and Global - second strongest correlation
# 3. Europe and North America - third, moderate correlation


#### 6.3 Simple linear regression ####

# 6.3.1 Linear regression: Europe sales by product
model_Europe <- lm(Product~Europe, data=sales_by_product_int)
# View the model
model_Europe
# View full regression table
summary(model_Europe)
# European Sales: R-squared explains 20.47% of variability
# P-value is 3.25e-10: less than 0.05 so Europe sales are significant
# Pr(>|t|) is 3.25e-10: less than 0.05 so model fit is good



# 6.3.2 Linear regression: North America sales by product
model_NAmerica <- lm(Product~NAmerica, data=sales_by_product_int)
# View the model
model_NAmerica
# View full regression table
summary(model_NAmerica)
# NAmerica Sales: R-squared explains 29.54% of variability
# P-value is 7.688e-15: less than 0.05 so Europe sales are significant
# Pr(>|t|) is 2e-26: less than 0.05 so model fit is good


# 6.3.3 Linear regression: Global sales by product
model_Global <- lm(Product~Global, data=sales_by_product_int)
# View the model
model_Global
# View full regression table
summary(model_Global)
# Global Sales: R-squared explains 36.74% of variability
# P-value is 2.2e-16: less than 0.05 so Europe sales are significant
# Pr(>|t|) is 2e-16: less than 0.05 so model fit is good


#### 6.4 Plot simple linear regressions ####

# 6.4.1 View European Sales residuals on a plot
plot(model_Europe$residuals)
# Plot the relationship with base R graphics
plot(sales_by_product_int$Product, sales_by_product_int$Europe)


# 6.4.2 View North American Sales residuals on a plot
plot(model_NAmerica$residuals)
# Plot the relationship with base R graphics
plot(sales_by_product_int$Product, sales_by_product_int$NAmerica)


# 6.4.3 View Global Sales residuals on a plot
plot(model_Global$residuals)
# Plot the relationship with base R graphics
plot(sales_by_product_int$Product, sales_by_product_int$Global)


#### 6.4.4 linear regression plot observations ####

# All three regions indicate the presence of outliers
# All three regions show a upwards trend for residuals
# Significant 'noise' / residual dispersal exists in all regions
# Strongest trend with least dispersal is in global residuals



#### 6.5 Log transformations ####

# 6.5.1 Complete log transformation with dplyr's mutate() function
saleslog <- mutate(sales_by_product_int, 
                   logEU=log(Europe),
                   logNA=log(NAmerica),
                   logGlobal=log(Global))

# View new object with new variable
head(saleslog)

# logEU contains negative values
# Replace negative values with NA so that models can be built

# Duplicate data
saleslog_new <- saleslog
# Replace negative values with NA
saleslog_new[saleslog_new < 0] <- NA

# 6.5.2 EU Sales model: model_EU using new saleslog_new dataframe
model_logEU <- lm(logEU~Product, data=saleslog_new)
# View full regression table
summary(model_logEU)
# Plot the relationship between year and logIndex
plot(saleslog_new$Product, saleslog_new$logEU)
# Add a line-of-best fit to existing plot
abline(coefficients(model_logEU))

# Europe changes after log transformation:
# R-squared increases from 20.47% to 23.81%
# P-value lowers from 3.25e-10 to 1.585e-10
# Pr(>|t|) lowers from  3.25e-10 to 1.58e-10


# 6.5.3 North American Sales model: model_NA using logNA
model_logNA <- lm(logNA~Product, data=saleslog_new)
# View full regression table
summary(model_logNA)
# Plot the relationship between year and logIndex
plot(saleslog_new$Product, saleslog_new$logNA)
# Add a line-of-best fit to existing plot
abline(coefficients(model_logNA))

# NAmerica changes after log transformation:
# R-squared increases from 29.54% to 39.1%
# P-value lowers from  7.688e-15 to 2.2e-16
# Pr(>|t|) lowers from 2e-26 to 2e-16


# 6.5.4 Create Global Sales model: model_logGlobal using logGlobal
model_logGlobal <- lm(logGlobal~Product, data=saleslog_new)
# View full regression table
summary(model_logGlobal)
# Plot the relationship between year and logIndex
plot(saleslog_new$Product, saleslog_new$logGlobal)
# Add a line-of-best fit to existing plot
abline(coefficients(model_logGlobal))

# Global changes after log transformation:
# R-squared increases from 36.74% to 54.85%
# P-value remains unchanged at 2.2e-16
# Pr(>|t|) remains unchanged at 2e-16



#### 6.6 Multiple linear regression ####

#### 6.6.1 Select only numeric data ####

# Create a subset of the sales data frame with only numeric columns
num_sales <- sales_by_product_int %>% keep(is.numeric)
# Check for missing values
sum(is.na(num_sales))


#### 6.6.2 Calculate correlations to discover variable significance ####
cor(num_sales)

# Correlation results:
#           Product     Europe   NAmerica     Global
# Product   1.0000000 -0.4524737 -0.5435505 -0.6061376
# Europe   -0.4524737  1.0000000  0.6209317  0.8486148
# NAmerica -0.5435505  0.6209317  1.0000000  0.9162292
# Global   -0.6061376  0.8486148  0.9162292  1.0000000

# Strongest correlations are between regions
# Correlations in order from high to low (near to + 1 or -1):
# Global / North America: 0.9162292
# Global / Europe: 0.8486148
# North America / Europe: 0.6209317

# Weaker correlations are between product and regions
# Correlations in order from high to low (near to + 1 or -1):
# Product / Global: -0.6061376
# Product / North America: -0.5435505
# Product / Europe: -0.4524737


#### 6.6.3 MLR test variables on global sales ####

# Investigate how how global sales are affected by all variables
modelA = lm(Global~Product+Europe+NAmerica,
            data=num_sales)
# Print the summary statistics.
summary(modelA)
# Adj R-squared: 97.09% of global sales explained by all variables
# P-value is 2.2e-16: all variables are significant
# Pr(>|t|) is 8.24e-130: less than 0.05 so model fit is good


# Investigate how global sales are affected by Product and Europe sales
modelB = lm(Global~Product+Europe,
            data=num_sales)
# Print the summary statistics.
summary(modelB)
# Adj R-squared: 77.97% of global sales explained by Product and Europe sales
# P-value is 2.2e-16: variables are significant
# Pr(>|t|) is 2e-16: less than 0.05 so model fit is good


# Investigate how how global sales are affected by Product and NAmerica
modelC = lm(Global~Product+NAmerica,
            data=num_sales)
# Print the summary statistics.
summary(modelC)
# Adj R-squared: 85.44% of global sales explained by Product and NAmeric sales
# P-value is 2.2e-16: variables are significant
# Pr(>|t|) is 2e-16: less than 0.05 so model fit is good


#### 6.6.4 MLR observations and model choice ####

# North American sales appear greater significance than European sales
# ModelA adjusted R-squared closest to 1 (100%) therefore strongest model



#### 6.7 Make predictions based on given values ####

# 6.7.1 Find rows where given value conditions are met

num_sales[num_sales$Europe == 23.80 & num_sales$NAmerica == 34.02,]
# Actual are: Product: 107, Europe: 23.8, NAmerica: 34.0, Global: 67.8

# Closest to assignment specified values EU:1.56 and NA:3.93
num_sales[num_sales$Europe == 1.54 & num_sales$NAmerica == 3.66,]
# Actual are: Product: 2296, Europe: 1.54, NAmerica: 3.66, Global: 7.4

# Closest to assignment specified values EU:0.65 and NA:2.73
num_sales[num_sales$Europe == 0.65 & num_sales$NAmerica == 2.73,]
# Actual are: Product: 6815, Europe: 0.65, NAmerica: 2.73, Global: 4.32

# Closest to assignment specified values EU:0.97 and NA:2.26
num_sales[num_sales$Europe == 0.97 & num_sales$NAmerica == 4.42,]
# Actual are: Product: 3158, Europe: 0.97, NAmerica: 4.42, Global: 6.12

num_sales[num_sales$Europe == 0.52 & num_sales$NAmerica == 22.08,]
# Actual are: Product: 326, Europe: 0.52, NAmerica: 22.1, Global: 23.2


# 6.7.2 Test modelA against given values to predict global sales values

# Given values: Europe: 1.54, NAmerica: 3.66
Product <- 107
Europe <- c(23.80)
NAmerica <- c(34.02)
predict_dataA <- data.frame(Product, Europe, NAmerica)

# Create a new object and specify the predict function.
predictA = predict(modelA, newdata=predict_dataA,
                   interval='confidence')
# Print predictA
predictA
# Actual global value is 67.80
# Predicted values:
# fit: 66.3587      
# lwr: 64.71258      
# upr: 68.00482


  
# Given values: Europe: 1.54, NAmerica: 3.66
Product <- 2296
Europe <- c(1.54)
NAmerica <- c(3.66)
predict_dataB <- data.frame(Product, Europe, NAmerica)

# Create a new object and specify the predict function.
predictB = predict(modelA, newdata=predict_dataB,
                   interval='confidence')
# Print predictB
predictB
# Actual global value is 7.4
# Predicted values:
# fit: 7.514566      
# lwr: 7.212602      
# upr: 7.81653



# Given values: Europe: 0.65, NAmerica: 2.73
Product <- 6815
Europe <- c(0.65)
NAmerica <- c(2.73)
predict_dataC <- data.frame(Product, Europe, NAmerica)

# Create a new object and specify the predict function.
predictC = predict(modelA, newdata=predict_dataC,
                   interval='confidence')
# Print predictC
predictC
# Actual global value is 4.32
# Predicted values:
# fit: 4.245235     
# lwr: 3.873852      
# upr: 4.616618



# Given values: Europe: 0.97, NAmerica: 4.42
Product <- 3158
Europe <- c(0.97)
NAmerica <- c(4.42)
predict_dataD <- data.frame(Product, Europe, NAmerica)

# Create a new object and specify the predict function.
predictD = predict(modelA, newdata=predict_dataD,
                   interval='confidence')
# Print predictD
predictD
# Actual global value is 6.12
# Predicted values:
# fit: 7.428072    
# lwr: 7.140741     
# upr: 7.715403



# Given values: Europe: 0.52, NAmerica: 22.08
Product <- 326
Europe <- c(0.52)
NAmerica <- c(22.08)
predict_dataE <- data.frame(Product, Europe, NAmerica)

# Create a new object and specify the predict function.
predictE = predict(modelA, newdata=predict_dataE,
                   interval='confidence')
# Print predictE
predictE
# Actual global value is 23.2
# Predicted values:
# fit: 26.54879   
# lwr: 25.37628    
# upr: 27.7213



#### 6.7.3 Prediction observations and insights ####

# 3 our of 5 predicted sales values appear close to actual values
# 60% model success
# Actions to progress could be to investigate outliers
# If appropriate, remove outliers
# Investigate product IDs for 2 out 3 predictions where model is weak







