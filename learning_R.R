# Function to check and install packages
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Checks and installs packages
packages <- c("ggplot2")
check.packages(packages)

# Load data (actual file path)
df <- read.csv("C:/Users/44797/Desktop/Repo/GEOG-70581/data/flow_data.csv")

# Commands for data frame inspection
head(df)
colnames(df)
str(df)

# What is the average daily flow?
mean(df$flow)

# What is the maximum daily flow?
max(df$flow)

# What is the minimum daily flow?
min(df$flow)

# Extracts rows corresponding to 2019 using indexing
df <- df[13242:13514,]

# Converts dates (in character format) to date format
df$date <- as.Date(df$date, format =  "%d/%m/%Y")

# Assigning to a variable
g <- ggplot(data = df, mapping=aes(x=date, y=flow)) +
  geom_line(colour = "#56ACFF") +
  geom_point(colour = "#767676") +
  theme_classic() +
  theme(aspect.ratio = 1)

# Saves to a png using ggsave
ggsave(plot = g, "C:/Users/44797/Desktop/Repo/GEOG-70581/images/flow_data_2019.png", dpi = 150)

# Creating a histogram
h <- ggplot(data = df, mapping=aes(x=flow)) +
  geom_histogram(binwidth = 1, colour = "#767676", fill = "#56ACFF") +
  theme_classic() +
  theme(aspect.ratio = 1)

# Saves to a png using ggsave
ggsave(plot = h, "C:/Users/44797/Desktop/Repo/GEOG-70581/images/histogram_2019.png", dpi = 150)


# Solution to the Chapter 3 formative task

# Load data from csv
df <- read.csv("C:/Users/44797/Desktop/Repo/GEOG-70581/data/flow_data.csv")

# Convert dates (in character format) to date format
df$date <- as.Date(df$date, format =  "%d/%m/%Y")

# Extracts post-2000 river flow
df_post2000 <- subset(df, date >= as.Date("2000-01-01"))

# Create a new column, corresponding to the month of measurement, using the months() function
df_post2000$month <- months(df_post2000$date)

# Summarise by month, using the aggregate() function and 'mean'
summary <- aggregate(flow ~ month, df_post2000, mean)

# Converts dates (in character format) to date format
summary$month <- factor(summary$month, levels = month.name)

# Plotting using ggplot2
g <- ggplot(data = summary, mapping=aes(x=month, y=flow)) +
  # Stat = "identity" is used when the values of x and y are known
  geom_bar(fill = "#3EBBFB", stat="identity") +
  # Sets the theme
  theme_classic() +
  # Add x and y-axis labels
  labs(x = "Month (2000 - 2019)", y = bquote('Average daily flow'~(m^3~'per second)'))) +
  # Tight fitting y-axis
  scale_y_continuous(expand = c(0,0)) +
  # Adjusts angle of x-axis labels
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
g

print(paste0("The month with the highest average daily flow is ", summary$month[summary$flow == max(summary$flow)]))
print(paste0("The month with the lowest average daily flow is ", summary$month[summary$flow == min(summary$flow)]))

# Saves to a png using ggsave
ggsave(plot = g, "C:/Users/44797/Desktop/Repo/GEOG-70581/images/flow_data_2019.png", dpi = 150)


