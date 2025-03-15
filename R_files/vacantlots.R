setwd("C:/Users/DELL/Documents/Course/Winter Quarter/Policy Lab/Vacant Lots/data")
options(scipen = 999)
rm(list = ls())
v1 <- read.csv("assessment_and_city_owned_new_data.csv")
colnames(v1) <- trimws(colnames(v1)) 
colnames(v1)
v2 <- subset(v1, Community.Area.Name %in% c("ENGLEWOOD", "WEST ENGLEWOOD", "NEW CITY", "NORTH LAWNDALE", "EAST GARFIELD PARK", "WEST GARFIELD PARK"), )

v3 <- subset(v2, Property.Status %in% c("Sold", "Owned by City"))


library(ggplot2)
library(dplyr)

#Group by community and status
v3_1 <- v3 %>%
  group_by(Community.Area.Name, Property.Status) %>%
  summarise(Count = n(), .groups = "drop") 

community_order <- toupper(c("Englewood", "West Englewood", "New City", 
                             "East Garfield Park", "West Garfield Park", "North Lawndale"))

v3_1$Community.Area.Name <- factor(v3_1$Community.Area.Name, levels = community_order)


v3_1 <- v3_1 %>%
  group_by(Community.Area.Name) %>%
  mutate(Percent = Count / sum(Count) * 100)

ggplot(v3_1, aes(x = Community.Area.Name, y = Count, fill = Property.Status)) +
  geom_bar(stat = "identity", position = "stack") +  
  geom_text(aes(label = paste0(round(Percent, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white") +
  labs(title = "Property Status Distribution by Community Area",
       x = "Community Area Name",
       y = "Number of Properties",
       fill = "Property Status") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),     
    axis.text.y = element_text(size = 8),                            
    legend.title = element_text(size = 8, face = "bold"),           
    legend.text = element_text(size = 8),                            
  )


#Updated one

v2_small <- v2 %>%
  filter(Property.Status %in% c("Leased", "In Acquisition")) %>%
  group_by(Community.Area.Name, Property.Status) %>%
  summarise(Count = n(), .groups = "drop")

community_order_small <- v2_small %>%
  group_by(Community.Area.Name) %>%
  summarise(Total = sum(Count)) %>%
  arrange(desc(Total)) %>%
  pull(Community.Area.Name)

v2_small$Community.Area.Name <- factor(v2_small$Community.Area.Name, levels = community_order_small)

colors <- c("Leased" = "#fc8d62", "In Acquisition" = "#049688")  

ggplot(v2_small, aes(x = Community.Area.Name, y = Count, fill = Property.Status)) +
  geom_bar(stat = "identity", position = "dodge") +  
  labs(title = "Zoomed-in: Leased & In Acquisition Properties",
       x = "Community Area Name",
       y = "Number of Properties",
       fill = "Property Status") +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  




#Time and Duration
v2_sold <- subset(v2, Property.Status == "Sold", )
v2_obc <- subset(v2, Property.Status == "Owned by City", )

#How many missing data
non_blank_ratio_sold <- v2_sold %>%
  summarise(NonBlank = sum(nchar(Date.of.Acquisition) > 0, na.rm = TRUE),
            Total = n()) %>%
  mutate(Ratio = NonBlank / Total)

non_blank_ratio_sold$Ratio

non_blank_ratio_obc <- v2_obc %>%
  summarise(NonBlank = sum(nchar(Date.of.Acquisition) > 0, na.rm = TRUE),
            Total = n()) %>%
  mutate(Ratio = NonBlank / Total)

non_blank_ratio_obc$Ratio

check_mismatch <- v2_sold %>%
  filter(nchar(Date.of.Acquisition) > 0 & (is.na(Date.of.Disposition) | nchar(Date.of.Disposition) == 0))

if(nrow(check_mismatch) == 0) {
  print("True")
} else {
  print(paste("Have", nrow(check_mismatch), "False observations"))
}


#v2_sold中有效数据的平均销售时间
library(lubridate)
# 删除空值或异常数据
v2_sold_clean <- v2_sold %>%
  filter(nchar(Date.of.Acquisition) > 0 & nchar(Date.of.Disposition) > 0) %>%
  mutate(
    Date.of.Acquisition = mdy(Date.of.Acquisition),
    Date.of.Disposition = mdy(Date.of.Disposition)
  )


# avgsold
v2_sold_clean <- v2_sold %>%
  filter(nchar(Date.of.Acquisition) > 0 & nchar(Date.of.Disposition) > 0) %>%
  mutate(
    Date.of.Acquisition = mdy(Date.of.Acquisition),
    Date.of.Disposition = mdy(Date.of.Disposition)
  )
v2_sold_clean <- v2_sold_clean %>%
  mutate(
    avgsold = as.numeric(difftime(Date.of.Disposition, Date.of.Acquisition, units = "days"))
  )

# 输出 avgsold 的平均值
avg_avgsold <- v2_sold_clean %>%
  filter(avgsold >= 0) %>%
  summarise(mean_avgsold = mean(avgsold, na.rm = TRUE)) 
avg_avgsold/365

per_as <- quantile(v2_sold_clean$avgsold/365)
per_as

# 画图，每年acquire的平均多少天卖出
v2_sold_clean <- v2_sold_clean %>%
  mutate(Year = year(Date.of.Acquisition))

yearly_avg <- v2_sold_clean %>%
  group_by(Year) %>%
  summarise(AvgSold = mean(avgsold, na.rm = TRUE)) %>%
  arrange(Year)

# 筛选占有时长大于 0 的数据
filtered_avg <- yearly_avg %>%
  filter(AvgSold / 365 > 0)

# 绘制图形，并限制横轴年份范围
ggplot(filtered_avg, aes(x = Year, y = AvgSold/365)) +
  geom_col(fill = "skyblue") +
  labs(title = "Average Ownership Duration for Sold Properties", x = "Year of Acquisition", y = "Years owned by city") +
  scale_x_continuous(
    limits = c(1955, 2025),   
    breaks = seq(1955, 2025, by = 5)  
  ) +
  theme_minimal()

#有标注版
ggplot(filtered_avg, aes(x = Year, y = AvgSold / 365)) +
  geom_col(fill = "skyblue") +
  geom_text(aes(label = round(AvgSold / 365, 1)),   
            vjust = -0.5,                        
            size = 2,                            
            family = "Times New Roman") +        
  labs(
    title = "Average Ownership Duration for Sold Properties", 
    x = "Year of Acquisition", 
    y = "Years owned by city"
  ) +
  scale_x_continuous(
    limits = c(1955, 2025),   
    breaks = seq(1955, 2025, by = 5)  
  ) +
  scale_y_continuous(
    limits = c(0, 70),   
  ) +
  theme_minimal()

#横轴平均时间，纵轴数量
ggplot(v2_sold_clean, aes(x = avgsold / 365)) +
  geom_histogram(aes(y = ..count..), bins = 30, fill = "skyblue", color = "white") +  
  geom_text(stat = "bin", aes(y = ..count.., label = ..count..), 
            bins = 30, vjust = -0.5, size = 3) +
  labs(
    title = "Distribution of Ownership Duration for Sold Properties",
    x = "Average Years Owned by City",
    y = "Number of Properties"
  ) +
  scale_x_continuous(
    limits = c(0, 60),  
    breaks = seq(0, 60, by = 5)  
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  
  theme_minimal()

#分community版
ggplot(v2_sold_clean, aes(x = avgsold / 365)) +
  geom_histogram(aes(y = ..count..), bins = 30, fill = "skyblue", color = "white") +
  geom_text(stat = "bin", aes(y = ..count.., label = ..count..), 
            bins = 30, vjust = -0.5, size = 3) +
  labs(
    title = "Distribution of Ownership Duration for Sold Properties by Community",
    x = "Average Years Owned by City",
    y = "Number of Properties"
  ) +
  scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 5)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  facet_wrap(~ Community.Area.Name) +  
  theme_minimal()



# 筛选出 Date.of.Acquisition 在1990年1月1日之后的数据，并计算 avgsold 的均值
cutoff_date_1990 <- as.Date("1990-01-01")

avgsold_1990_after <- v2_sold_clean %>%
  filter(Date.of.Acquisition > cutoff_date_1990, nchar(Date.of.Acquisition) > 0) %>%
  summarise(mean_avgsold = mean(avgsold, na.rm = TRUE))

avgsold_1990_after$mean_avgsold/365


avgsold_1990_after <- v2_sold_clean %>%
  filter(Date.of.Acquisition > cutoff_date_1990, nchar(Date.of.Acquisition) > 0) %>%
  summarise(median_avgsold = median(avgsold, na.rm = TRUE))

avgsold_1990_after$median_avgsold/365


#现在是Owned by city
library(dplyr)
library(lubridate)

# 定义2025年3月1日
cutoff_date_2025 <- as.Date("2025-03-01")

# 剔除 Date.of.Acquisition 为空白的行，并将 Date.of.Acquisition 转换为日期格式
v2_obc_clean <- v2_obc %>%
  filter(trimws(Date.of.Acquisition) != "" & !is.na(Date.of.Acquisition)) %>%  
  mutate(Date.of.Acquisition = mdy(Date.of.Acquisition)) 

v2_obc_clean <- v2_obc_clean %>%
  mutate(avgobc = as.numeric(difftime(cutoff_date_2025, Date.of.Acquisition, units = "days")))

mean_avgobc <- mean(v2_obc_clean$avgobc, na.rm = TRUE)
mean_avgobc/365

median_avgobc <- median(v2_obc_clean$avgobc, na.rm = TRUE)
median_avgobc/365

per_ao <- quantile(v2_obc_clean$avgobc/365)
per_ao

v2_obc_clean <- v2_obc_clean %>%
  mutate(Year = year(Date.of.Acquisition))

yearly_avg2 <- v2_obc_clean %>%
  group_by(Year) %>%
  summarise(AvgObc = mean(avgobc, na.rm = TRUE)) %>%
  arrange(Year)

#横轴年份，纵轴占有时间
ggplot(yearly_avg2, aes(x = Year, y = AvgObc/365)) +
  geom_col(fill = "yellow") +
  labs(title = "Average occupying days for unsold properties acquired in 1955-2025", x = "Year", y = "Average days") +
  scale_x_continuous(
    limits = c(1955, 2025),   # 设置横轴范围
    breaks = seq(1955, 2025, by = 5)  # 每隔10年标一个
  ) +
  theme_minimal()

#横轴平均时间，纵轴数量

ggplot(v2_obc_clean, aes(x = avgobc / 365)) +
  geom_histogram(aes(y = ..count..), bins = 30, fill = "skyblue", color = "white") +  
  geom_text(stat = "bin", aes(y = ..count.., label = ..count..), 
            bins = 30, vjust = -0.5, size = 3) +
  labs(
    title = "Distribution of Ownership Duration for City-owned Properties",
    x = "Average Years Owned by City",
    y = "Number of Properties"
  ) +
  scale_x_continuous(
    limits = c(0, 60),  
    breaks = seq(0, 60, by = 5)  
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  
  theme_minimal()

#分community版

ggplot(v2_obc_clean, aes(x = avgobc / 365)) +
  geom_histogram(aes(y = ..count..), bins = 30, fill = "skyblue", color = "white") +  
  geom_text(stat = "bin", aes(y = ..count.., label = ..count..), 
            bins = 30, vjust = -0.5, size = 3) +
  labs(
    title = "Distribution of Ownership Duration for City-owned Properties by Community",
    x = "Average Years Owned by City",
    y = "Number of Properties"
  ) +
  scale_x_continuous(
    limits = c(0, 60),  
    breaks = seq(0, 60, by = 5)  
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  
  facet_wrap(~ Community.Area.Name) +  
  theme_minimal()



# 绘制obc分区柱状图

# 计算每个社区的平均 avgobc，并过滤掉 avgobc <= 0 的数据
v2_obc_avg <- v2_obc_clean %>%
  filter(avgobc > 0) %>%
  group_by(Community.Area.Name) %>%
  summarise(mean_avgobc = mean(avgobc, na.rm = TRUE))  # 计算平均值，忽略缺失值

# 绘制柱状图

ggplot(v2_obc_avg, aes(x = reorder(Community.Area.Name, mean_avgobc), y = mean_avgobc/365, fill = Community.Area.Name)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Age by Community Area", x = "Community Area", y = "Average Age") +
  theme_minimal() +
  coord_flip()  


#obc面积密度图
ggplot(v2_obc, aes(x = Square.Footage...City.Estimate)) +
  geom_density(fill = "skyblue", color = "black", alpha = 0.7) +  # 设置颜色和透明度
  labs(title = "Density Distribution of Sqft", x = "Sqft", y = "Density") +
  scale_x_continuous(limits = c(0, 10000), breaks = seq(0, 10000, by = 1000)) + 
  theme_minimal()

