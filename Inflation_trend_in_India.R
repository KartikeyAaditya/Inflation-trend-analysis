library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(zoo)
library(ggtext)

inflation_data <- read_csv("https://github.com/SigmaGyaat706/Inflation-trend-analysis/blob/main/API_NY.GDP.DEFL.KD.ZG_DS2_en_csv_v2_145.csv", skip = 4)


inflation_data <- inflation_data%>%
  select(-`Indicator Name`, -`Indicator Code`)


inflation_data_long <- inflation_data%>%
  pivot_longer(cols = -c(`Country Name`, `Country Code`),
               names_to = "Year",
               values_to = "inflation")
  
inflation_data_long$Year <- as.numeric(inflation_data_long$Year)

india_data <- inflation_data_long%>%
  filter(`Country Name` == "India")%>%
  filter(!is.na(inflation))

events <- data.frame(
  Year = c(1991, 2008,2020),
  Label = c("1991 economic crisis", "Global Recession", "Covid 19 OutBreak")
)

india_data <- india_data%>%
  arrange(Year)%>%
  mutate(MA_5yr = rollmean(inflation, k = 5, fill = NA, align = "right"))

max_inflation = max(india_data$inflation, na.rm = TRUE)

ggplot(india_data, aes(x = Year, y = inflation))+
  geom_col(aes(fill = inflation), show.legend = FALSE)+
  scale_fill_gradient2(low= "navy", mid = "yellow", high = "green", midpoint = 5)+
  geom_text(data = events, aes(x = Year, y = max_inflation * 0.8, label = Label), size = 4, hjust = 0.5,vjust = 0, color = "red", fontface = "italic")+
  geom_vline(data = events, aes(xintercept = Year), color = "black", linetype = "dotted", size = 0.8)+
  geom_line(aes(y = MA_5yr),color = "orange", size = 1)+
  labs(title = "Inflation trend in India", x = "Year", y = "Inflation(%)", subtitle = "Data Cleaning,Moving Average, annotated Economic Events and Visualization", caption = "Source: World bank")+
  theme_minimal(base_size = 10)+
  theme(plot.title = element_markdown(size = 18, face = "bold"),
        plot.subtitle = element_markdown(size = 12))
  



