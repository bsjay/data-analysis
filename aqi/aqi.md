aqi
================

# 1\. 问题

对该数据集，有以下问题需要解答：

1.  该数据集一共收集了多少个城市的空气数据？

2.  哪些城市的空气质量最好，哪个最差？

3.  北上广深这些一线城市的空气质量如何？

4.  城市AQI、PM2.5、PM10和地区AQI的数据分布是怎样的？

5.  污染等级中哪个级别最高？

6.  PM2.5和PM10之间存在相关性吗？

-----

# 2\. 导入包

``` r
library(tidyverse) # 数据分析包
library(readxl) # 读取excel文件
library(psych) # 查看描述统计量
library(Hmisc)
library(pastecs)
library(knitr)
library(magrittr)
```

-----

# 3\. 导入数据

``` r
aqi <- read_xlsx("空气质量指数.xlsx")
head(aqi)
```

    ## # A tibble: 6 x 8
    ##   城市       地区    城市AQI PM2.5浓度 PM10浓度 首要污染物 污染等级 地区AQI
    ##   <chr>      <chr>   <chr>   <chr>     <chr>    <chr>      <chr>      <dbl>
    ## 1 鞍山实时空气质量指… 明达新区… 177     125μg/m³  228μg/m³ PM2.5      轻度污染     165
    ## 2 鞍山实时空气质量指… 千山    177     117μg/m³  145μg/m³ PM2.5      轻度污染     153
    ## 3 鞍山实时空气质量指… 深沟寺  177     138μg/m³  244μg/m³ PM2.5      轻度污染     183
    ## 4 鞍山实时空气质量指… 太平    177     126μg/m³  239μg/m³ PM2.5      轻度污染     166
    ## 5 鞍山实时空气质量指… 太阳城  177     142μg/m³  242μg/m³ PM2.5      轻度污染     189
    ## 6 鞍山实时空气质量指… 铁西工业园区… 177     156μg/m³  324μg/m³ PM2.5      中度污染     206

-----

# 4\. 数据清洗

  - 将变量中的冗余字符去掉

<!-- end list -->

``` r
aqi$城市 <- str_replace_all(aqi$城市, "[实时空气质量指数]", "") # 只保留城市名
aqi$PM2.5浓度 <- str_replace_all(aqi$PM2.5浓度, "[μg/m³|—μg/m³]", "") # 将单位去掉，只保留数值
aqi$PM10浓度 <- str_replace_all(aqi$PM10浓度, "[μg/m³|—μg/m³]", "")
aqi$首要污染物 <- str_replace_all(aqi$首要污染物, "[—]", "NA") 
```

  - 转换变量的数据类型，以便进行更好地进行计算

<!-- end list -->

``` r
aqi$城市AQI <- parse_double(aqi$城市AQI, na = "NA") # 转换为浮点类型
aqi$PM2.5浓度 <- parse_number(aqi$PM2.5浓度, na = "NA") # 转换为数值类型
aqi$PM10浓度 <- parse_number(aqi$PM10浓度, na = "NA")
aqi$首要污染物 <- parse_factor(aqi$首要污染物, na = "NA")

level <- c("优", "良", "轻度污染", "中度污染", "严重污染")
aqi$污染等级 <- parse_factor(aqi$污染等级, levels = level, na = "NA") # 转换为因子
```

  - 简化变量名

<!-- end list -->

``` r
aqi <- rename(aqi, PM2.5 = PM2.5浓度, PM10 = PM10浓度)
```

``` r
# 输出头6行数据
head(aqi)
```

    ## # A tibble: 6 x 8
    ##   城市  地区         城市AQI PM2.5  PM10 首要污染物 污染等级 地区AQI
    ##   <chr> <chr>          <dbl> <dbl> <dbl> <fct>      <fct>      <dbl>
    ## 1 鞍山  明达新区         177   125   228 PM2.5      轻度污染     165
    ## 2 鞍山  千山             177   117   145 PM2.5      轻度污染     153
    ## 3 鞍山  深沟寺           177   138   244 PM2.5      轻度污染     183
    ## 4 鞍山  太平             177   126   239 PM2.5      轻度污染     166
    ## 5 鞍山  太阳城           177   142   242 PM2.5      轻度污染     189
    ## 6 鞍山  铁西工业园区     177   156   324 PM2.5      中度污染     206

-----

# 5\. 探索性数据分析

## 5.1 查看描述统计量

``` r
stat.desc(aqi)
```

    ##          城市 地区      城市AQI        PM2.5         PM10 首要污染物
    ## nbr.val    NA   NA 1.453000e+03 1.453000e+03 1.389000e+03         NA
    ## nbr.null   NA   NA 0.000000e+00 0.000000e+00 0.000000e+00         NA
    ## nbr.na     NA   NA 0.000000e+00 0.000000e+00 6.400000e+01         NA
    ## min        NA   NA 2.600000e+01 1.000000e+00 1.000000e+00         NA
    ## max        NA   NA 5.000000e+02 4.760000e+02 1.135000e+03         NA
    ## range      NA   NA 4.740000e+02 4.750000e+02 1.134000e+03         NA
    ## sum        NA   NA 1.242730e+05 8.403500e+04 1.360510e+05         NA
    ## median     NA   NA 7.600000e+01 5.000000e+01 8.700000e+01         NA
    ## mean       NA   NA 8.552856e+01 5.783551e+01 9.794888e+01         NA
    ## SE.mean    NA   NA 1.119277e+00 9.671441e-01 1.829588e+00         NA
    ## CI.mean    NA   NA 2.195573e+00 1.897149e+00 3.589056e+00         NA
    ## var        NA   NA 1.820291e+03 1.359089e+03 4.649527e+03         NA
    ## std.dev    NA   NA 4.266486e+01 3.686583e+01 6.818744e+01         NA
    ## coef.var   NA   NA 4.988376e-01 6.374255e-01 6.961533e-01         NA
    ##          污染等级      地区AQI
    ## nbr.val        NA 1.453000e+03
    ## nbr.null       NA 0.000000e+00
    ## nbr.na         NA 0.000000e+00
    ## min            NA 1.100000e+01
    ## max            NA 5.000000e+02
    ## range          NA 4.890000e+02
    ## sum            NA 1.248190e+05
    ## median         NA 7.500000e+01
    ## mean           NA 8.590434e+01
    ## SE.mean        NA 1.186049e+00
    ## CI.mean        NA 2.326552e+00
    ## var            NA 2.043952e+03
    ## std.dev        NA 4.521008e+01
    ## coef.var       NA 5.262841e-01

> *整个数据集有1453个观测（行），8个变量（列），这些变量分别是城市、地区、城市AQI、PM2.5、PM10、首要污染物、污染等级和地区AQI。*

## 5.2 单因子探索性数据分析

### 1\. 分析城市变量

  - 统计城市数量

<!-- end list -->

``` r
aqi %>% 
  group_by(城市) %>% 
  count() %>% 
  summary()
```

    ##      城市                 n         
    ##  Length:365         Min.   : 1.000  
    ##  Class :character   1st Qu.: 2.000  
    ##  Mode  :character   Median : 4.000  
    ##                     Mean   : 3.981  
    ##                     3rd Qu.: 5.000  
    ##                     Max.   :17.000

> *经聚合后，数据集有365个城市。*

### 2\. 分析地区变量

  - 统计地区数量

<!-- end list -->

``` r
aqi %>% 
  group_by(地区) %>% 
  count() %>% 
  summary()
```

    ##      地区                 n        
    ##  Length:1264        Min.   : 1.00  
    ##  Class :character   1st Qu.: 1.00  
    ##  Mode  :character   Median : 1.00  
    ##                     Mean   : 1.15  
    ##                     3rd Qu.: 1.00  
    ##                     Max.   :27.00

> *经聚合后，收集空气数据的地区有1264个。整个数据集有1453个观测，有些城市的数据收集地区有重复，重复数量有189个*

### 3\. 分析城市AQI变量

  - 查看城市AQI的描述统计量

<!-- end list -->

``` r
describe(aqi$城市AQI)
```

    ## aqi$城市AQI 
    ##        n  missing distinct     Info     Mean      Gmd      .05      .10 
    ##     1453        0      129        1    85.53    42.64     39.0     44.0 
    ##      .25      .50      .75      .90      .95 
    ##     56.0     76.0    107.0    138.0    159.4 
    ## 
    ## lowest :  26  27  28  29  30, highest: 178 189 196 227 500

> *城市AQI变量中有1453个值，没有缺失值，其中平均值是85.53，中位数是76，最小值是26，最大值是500*

  - 按城市分组，计算各城市的AQI平均值

<!-- end list -->

``` r
avg_city_aqi <- aqi %>% 
  group_by(城市) %>% 
  summarise(城市AQI平均值 = mean(城市AQI))
avg_city_aqi
```

    ## # A tibble: 365 x 2
    ##    城市       城市AQI平均值
    ##    <chr>              <dbl>
    ##  1 阿坝州                29
    ##  2 阿克苏地区           500
    ##  3 阿拉善盟              42
    ##  4 阿勒泰地区            49
    ##  5 阿里地区              28
    ##  6 安康                  88
    ##  7 安庆                 150
    ##  8 安顺                  41
    ##  9 安阳                 100
    ## 10 鞍山                 177
    ## # … with 355 more rows

  - 查看城市AQI平均值的描述统计量

<!-- end list -->

``` r
summary(avg_city_aqi)
```

    ##      城市           城市AQI平均值   
    ##  Length:365         Min.   : 26.00  
    ##  Class :character   1st Qu.: 55.00  
    ##  Mode  :character   Median : 74.00  
    ##                     Mean   : 83.19  
    ##                     3rd Qu.:102.00  
    ##                     Max.   :500.00

> *365个城市的AQI指数平均值为83.19，中位数是74，最小值是26，最大值是500。*

  - 城市AQI平均值的数值分布

<!-- end list -->

``` r
avg_city_aqi %>% 
  count(AQI指数 = cut_width(城市AQI平均值, 30))
```

    ## # A tibble: 9 x 2
    ##   AQI指数       n
    ##   <fct>     <int>
    ## 1 [15,45]      49
    ## 2 (45,75]     144
    ## 3 (75,105]     88
    ## 4 (105,135]    48
    ## 5 (135,165]    22
    ## 6 (165,195]    10
    ## 7 (195,225]     1
    ## 8 (225,255]     1
    ## 9 (495,525]     2

  - 绘制城市AQI平均值的直方图

<!-- end list -->

``` r
plot_theme = theme(plot.title = element_text(hjust = 0.5),
              text = element_text(family = "MicrosoftYaHei"))

ggplot(avg_city_aqi, aes(城市AQI平均值)) +
  geom_histogram(color="black", fill="red", binwidth = 30) +
  labs(title="城市AQI平均值分布图", x="AQI指数", y="频率") +
  plot_theme
```

![](aqi_files/figure-gfm/avg_city_aqi%20histogram-1.png)<!-- -->

> *有个49城市的AQI指数在15-45之间，等级为优，占比为13.4%；*

> *有232个城市的AQI指数在45-105之间，等级为良，占比为63.6%；*

> *有80个城市的AQI指数在105-195之间，等级为轻度污染，占比为21.9%；*

> *有2个城市的AQI指数在195-255之间，等级为中度污染，占比为0.5%；*

> *有2个城市的AQI指数在495-525之间，等级为重度污染，占比为0.5%。*

  - 城市AQI平均值最低的10个城市

<!-- end list -->

``` r
avg_city_aqi %>% 
  arrange(城市AQI平均值) %>%  # 按从小到大排列
  head(10) %>%  # 输出AQI平均值最低的10个城市
  # 绘制条形图
  ggplot(aes(reorder(城市, 城市AQI平均值), 城市AQI平均值)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "城市AQI平均值最低的10个城市", x="城市", y="城市AQI平均值") + # 标题
  geom_text(aes(label=城市AQI平均值), hjust=-0.2) + # 数据标签
  coord_flip() + # 图形转置
  plot_theme
```

![](aqi_files/figure-gfm/avg_city_aqi%2010%20highest-1.png)<!-- -->

  - 城市AQI平均值最高的10个城市

<!-- end list -->

``` r
avg_city_aqi %>% 
  arrange(desc(城市AQI平均值)) %>%  # 按从大到小降序排列
  head(10) %>%  # 输出AQI平均值最高的10个城市
  # 绘制条形图
  ggplot(aes(reorder(城市, 城市AQI平均值), 城市AQI平均值)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "城市AQI平均值最高的10个城市", x="城市", y="城市AQI平均值") +
  geom_text(aes(label=城市AQI平均值), hjust=-0.1) +
  coord_flip() +
  plot_theme
```

![](aqi_files/figure-gfm/avg_city_aqi%2010%20lowest-1.png)<!-- -->

### 4\. 分析PM2.5变量

  - 查看PM2.5的描述统计量

<!-- end list -->

``` r
describe(aqi$PM2.5)
```

    ## aqi$PM2.5 
    ##        n  missing distinct     Info     Mean      Gmd      .05      .10 
    ##     1453        0      161        1    57.84    38.87       13       20 
    ##      .25      .50      .75      .90      .95 
    ##       32       50       79      107      125 
    ## 
    ## lowest :   1   2   3   4   5, highest: 193 212 272 283 476

> *PM2.5变量有1453个值，没有缺失值，其中平均值是57.84，中位数是50，最小值是1，最大值是476*

  - 按城市分组，计算各城市的PM2.5平均值

<!-- end list -->

``` r
avg_city_pm2.5 <- aqi %>% 
  group_by(城市) %>% 
  summarise(城市PM2.5平均值 = mean(PM2.5))
avg_city_pm2.5
```

    ## # A tibble: 365 x 2
    ##    城市       城市PM2.5平均值
    ##    <chr>                <dbl>
    ##  1 阿坝州                2.67
    ##  2 阿克苏地区          248.  
    ##  3 阿拉善盟             23   
    ##  4 阿勒泰地区           16.5 
    ##  5 阿里地区              4   
    ##  6 安康                 64   
    ##  7 安庆                114.  
    ##  8 安顺                 27   
    ##  9 安阳                 75.2 
    ## 10 鞍山                134   
    ## # … with 355 more rows

  - 查看城市PM2.5平均值的描述统计量

<!-- end list -->

``` r
summary(avg_city_pm2.5)
```

    ##      城市           城市PM2.5平均值  
    ##  Length:365         Min.   :  2.667  
    ##  Class :character   1st Qu.: 30.500  
    ##  Mode  :character   Median : 47.125  
    ##                     Mean   : 54.943  
    ##                     3rd Qu.: 72.250  
    ##                     Max.   :374.000

> *365个城市的PM2.5指数的平均值是54.94，中位数是47.13，最小值是2.67，最大值是374*

  - 城市PM2.5平均值的数值分布

<!-- end list -->

``` r
avg_city_pm2.5 %>% 
  count(PM2.5指数 = cut_width(城市PM2.5平均值, 30))
```

    ## # A tibble: 9 x 2
    ##   PM2.5指数     n
    ##   <fct>     <int>
    ## 1 [-15,15]     22
    ## 2 (15,45]     152
    ## 3 (45,75]     106
    ## 4 (75,105]     57
    ## 5 (105,135]    23
    ## 6 (135,165]     2
    ## 7 (165,195]     1
    ## 8 (225,255]     1
    ## 9 (345,375]     1

  - 绘制城市PM2.5平均值的直方图

<!-- end list -->

``` r
ggplot(avg_city_pm2.5, aes(城市PM2.5平均值)) +
  geom_histogram(color="black", fill="red", binwidth = 30) +
  labs(title="城市PM2.5平均值分布图", x="PM2.5指数", y="频率") +
  plot_theme
```

![](aqi_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

# 参考资料

1.  [城市空气质量等级](https://baike.baidu.com/item/%E5%9F%8E%E5%B8%82%E7%A9%BA%E6%B0%94%E8%B4%A8%E9%87%8F%E7%AD%89%E7%BA%A7/8429673?fr=aladdin)
