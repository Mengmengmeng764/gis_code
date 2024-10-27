
install.packages("tidyverse")  # 数据处理
install.packages("sf")          # 空间数据处理
install.packages("countrycode") # 国家代码转换

library(tidyverse)  # 加载数据处理和可视化工具
library(sf)        # 加载空间数据处理工具
library(countrycode) # 加载国家代码转换工具


gender_data <- read.csv("E:/CASA/05/my-repo/gis_code/HDR23-24_Composite_indices_complete_time_series.csv")
head(gender_data)
str(gender_data)

library(dplyr)
cleaned_data <- gender_data %>%
  select(country, hdi_2010, hdi_2019) #select useful
head(cleaned_data)

cleaned_data <- cleaned_data %>%
  +     mutate(inequality_difference = hdi_2019 - hdi_2010)
head(cleaned_data)

world_data <- st_read("E:/CASA/05/my-repo/gis_code/World_Countries_(Generalized)_9029012925078512962.geojson")
head(world_data)

cleaned_data <- cleaned_data %>%
  +     mutate(iso = countrycode(country, "country.name", "iso2c"))  #warning
#solutions by gpt
# 查看 cleaned_data 中的国家名称
> unique(cleaned_data$country) %>% head(20)
# 手动过滤掉非国家名称
# 这里根据警告信息中的内容，排除一些区域性名称
non_country_names <- c(
    +     "Arab States", 
    +     "East Asia and the Pacific", 
    +     "Europe and Central Asia", 
    +     "High human development", 
    +     "Latin America and the Caribbean", 
    +     "Low human development", 
    +     "Medium human development", 
    +     "South Asia", 
    +     "Sub-Saharan Africa", 
    +     "T<fc>rkiye", 
    +     "Very high human development", 
    +     "World" )
# 过滤掉这些名称
cleaned_data <- cleaned_data %>%
  +     filter(!country %in% non_country_names)
# 再次尝试添加 ISO 代码
cleaned_data <- cleaned_data %>%
  mutate(iso = countrycode(country, "country.name", "iso2c"))
# 检查是否有 NA 值
missing_iso <- cleaned_data %>% filter(is.na(iso))
print(missing_iso) # 77NA


#Resolve NA - Record NA Data
merged_data <- world_data %>%
  +     left_join(cleaned_data, by = c("COUNTRY" = "country", "ISO" = "iso"))
head(merged_data)
#create a new list to record NA
merged_data <- merged_data %>%
  +     mutate(missing_data_flag = ifelse(is.na(inequality_difference), "Missing", "Present"))

#visualization
library(ggplot2)
ggplot(data = merged_data) +
  +     geom_sf(aes(fill = inequality_difference, color = missing_data_flag)) +
  +     scale_fill_gradient(low = "blue", high = "red", na.value = "grey50") +
  +     labs(title = "Global Gender Inequality Index (2010-2019)",
             +          fill = "Inequality Index",
             +          color = "Data Status") +
  +     theme_minimal()


