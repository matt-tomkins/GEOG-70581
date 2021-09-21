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
