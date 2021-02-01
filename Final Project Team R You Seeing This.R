install.packages("plotly")
install.packages("knitr")
install.packages("waffle")
install.packages("packcircles")
library(packcircles)
library(waffle)
library(knitr)
library(plotly)
library(ggplot2)
library(reshape2)
library(viridis)
library(hrbrthemes)
library(dplyr)
library(ggpubr)
library(tidyverse)
library(readr)
library(readxl)
library(ggcorrplot)
library(trelliscopejs)

#Import Data
wine <- read_excel("C:\\Users\\saman\\Documents\\Data Visualization\\Final Project Submission\\WineDataFinal.xlsx")
#colnames(wine)


#Set as DataFrame
df_wine <- as.data.frame(wine)


## Outliers for Year 
df_wine%>%
  plot_ly()%>%
  add_boxplot(x = ~year)%>%
  layout(title="Year Outliers")


## Outliers for Price 
df_wine%>%
  plot_ly()%>%
  add_boxplot(x = ~price)%>%
  layout(title="Price Outliers")


#Outliers for Price and Year
plot_ly(wine, x = ~price, y = ~year, colors = "black")%>%
  layout(title="Price Vs.Year")


#Split into top 10 countries based on count
top_wine<-df_wine %>%
  add_count(country) %>%
  filter(dense_rank(-n) < 11)


#Separating outliers
outliers<-top_wine[top_wine$year>1990,]
outlier<-outliers[outliers$price<100,]


#top 10 countries graph 
deftop<- top_wine%>%
  plot_ly(x = ~country, marker = list(color = 'rgb(170,201,91)')) %>%
  layout(title = "Top 10 Countries Based on Count", xaxis = list(title = "Country", autorange = "reversed"), yaxis = list(title = "Count"))

deftop


#Distribution of Price Variable
df_wine_var <- df_wine %>% filter(price <200 & year>1990)

df_wine_var %>%
  plot_ly(x = ~price) %>%
  layout( title = "Distribution of Price",
          xaxis = list(title = "Price"),
          yaxis = list(title = "Frequency")
  )

#Distribution of Points Variable
df_wine_var %>%
  plot_ly(x = ~points) %>%
  layout(title = "Distribution of Points",
         xaxis = list(title = "Points"),
         yaxis = list(title = "Frequency")
  )

#Distruibution of Year Variable
df_wine %>%
  plot_ly(x = ~year)%>%
  layout( title = "Distribution of Vintage Year",
          xaxis = list(title = "Vintage Year"),
          yaxis = list(title = "Frequency")
  )


## Year and Points Correlation 
year_v_points <- ggplot(outlier, aes(points, year, fill = Quality)) +
  geom_point(aes(size = 3, frame = country)) +
  ggtitle("Year vs Point Correlation") 

ggplotly(year_v_points)


## Price and Points Correlation
price_v_point <- ggplot(outlier, aes(points, price, fill = Quality)) +
  geom_point(aes(size = 3, frame = country)) +
  scale_x_log10()+
  geom_smooth(method = "lm",
              formula = y ~ x)+
  ggtitle("Price vs Point Correlation")

ggplotly(price_v_point)


#Create DataFrame for Trelliscope
trell<- top_wine %>%
  select(Index, country, points, price, year)

trell

#Trelliscope of Top 10 Countries Price vs. Points Correlations
qplot(points, price, data = trell) +
  xlim(80, 100) + ylim(4, 500) + theme_bw() +
  facet_trelliscope(~country,
                    nrow = 2, ncol = 5, width = 300, 
                    as_plotly = TRUE, 
                    plotly_args = list(dynamicTicks = T),
                    plotly_cfg = list(displayModeBar = F)
  )



#Get Top 10 Unique Varieties Based on Points for France
expensive<- top_wine %>%
  select(Index, country, points, price, title, variety, winery, year, region_1) %>%
  filter(country == "France")


expensive.group <- expensive %>% group_by(expensive$variety) %>%
  arrange(expensive$variety) %>%
  summarise_if(is.numeric,
               funs(mean = mean(., na.rm = TRUE)))

expensive.new <- data.frame(expensive.group)



#Get All Varieties With Average Points
all_var_fr <- expensive.new %>%
  arrange(desc(points_mean))


#mean(all_var_fr$points_mean)
#median points = 88.17452
#mean points: 88.25643


#Diverging Bar Chart for All Varieties in France Based on Average Points
all_var_fr$points_mean_z_score <- round((all_var_fr$points_mean - mean(all_var_fr$points_mean))/sd(all_var_fr$points_mean), digits=3)
all_var_fr$points_mean_type <- ifelse(all_var_fr$points_mean_z_score < 0, "below", "above")

all_var_fr <- all_var_fr[order(all_var_fr$points_mean_z_score), ] #Ascending sort on Z Score
all_var_fr$expensive.variety <- factor(all_var_fr$expensive.variety, levels = all_var_fr$expensive.variety)

ggplot(all_var_fr, aes(x=expensive.variety, y=points_mean_z_score, label=points_mean_z_score)) +
  geom_bar(stat='identity', aes(fill=points_mean_type), width=.5) +
  scale_fill_manual(name="Points (deviation)",
                    labels = c("Above Average", "Below Average"),
                    values = c("above"="#AAC95B", "below"="#82171C")) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  labs(subtitle="Average score for each unique variety",
       title= "Wine Varieties in France")+
  xlab(label = "Variety") + 
  ylab(label = "Standard Deviation From Mean Score") +
  theme(plot.title = element_text(size = 19, face = "bold", vjust = 2), plot.subtitle = element_text(vjust = 3)) +
  theme(axis.title.x = element_text(size = 13.5, vjust = -2), axis.title.y = element_text(size = 14)) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(axis.line = element_line(color = "black")) +
  annotate(
    geom = "curve", x = "Sylvaner", y = 0, xend = "Duras", yend = 0.5, 
    curvature = .1, arrow = arrow(length = unit(2, "mm")))+
  annotate(geom = "text", x = "Duras", y = 0.6, label = "Mean: 88.25643", hjust = "left")+
coord_flip()


#Get Top 10 Varieties for France Based on Average Points
top_vars_fr <- expensive.new %>%
  arrange(desc(points_mean)) %>%
  head(10)

top_vars_fr

# Create Circle Plot to Introduce Varieties
data <- top_vars_fr
packing <- circleProgressiveLayout(data$points_mean, sizetype='area')
data <- cbind(data, packing)

dat.gg<-circleLayoutVertices(packing, npoints=50)

ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
  geom_text(data = data, aes(x, y, label = paste(expensive.variety, "\n", (round(points_mean, 2))))) +
  scale_size_continuous(range = c(1,4)) +
  ggtitle("Top 10 Wine Varieties in France", subtitle = "Based on Average Points") +
  theme_void() + 
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(legend.position="none") +
  coord_equal()



#Define Top 10 Titles in France
expensivefr<- expensive %>%
  select(points, price, variety, title)%>%
  filter(points > 99)%>%
  arrange(desc(points)) %>%
  subset(variety != "Champagne Blend") %>%
  #filter(variety == "Pinot Meunier") %>%
  head(10)

#expensivefr


#Create Text Plot for Top 10 Titles in France
titles_fr <- ggplot(expensivefr, aes(points, price, label = title, size = points, scale_size(range = c(1,4)), color = variety)) + 
  geom_text() +
  ggtitle("Best Wine Titles in France", subtitle = "Based on points") +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  xlab(label = "") +
  ylab(label = "Price of Wine") +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.title.x = element_text(size = 13.5, vjust = -2), axis.title.y = element_text(size = 14)) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(axis.line = element_line(color = "black")) + 
  theme(legend.position = "top") 

titles_fr


#Define Top 10 Regions in France
fr_region<- expensive %>% group_by(expensive$region_1) %>%
  arrange(desc(expensive$region_1)) %>%
  summarise_if(is.numeric, 
               funs(mean = mean(., na.rm = TRUE)))

fr_region<-data.frame(fr_region) %>%
  arrange(desc(price_mean)) %>%
  head(10)

fr_region


#Create Circle Plot for Best Regions in France
France <- fr_region %>%
  ggplot(aes(x=expensive.region_1, y=points_mean, size = price_mean, fill = expensive.region_1 )) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_size(range = c(1, 20)) +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  ylab("Points") +
  xlab("Region") +
  theme(legend.position = "none") +
  ggtitle("Best Regions in France")


ggplotly(France)
  


#Get Top 10 Varieties for Argentina Based on Average Price
cheap<- top_wine %>%
  select(Index, country, points, price, title, variety, winery, year, region_1) %>%
  filter(country == "Argentina")
#cheap

cheap.group <- cheap %>% group_by(cheap$variety) %>%
  arrange(cheap$variety) %>%
  summarise_if(is.numeric,
               funs(mean = mean(., na.rm = TRUE)))

cheap.new <- data.frame(cheap.group)

top_vars_ar <- cheap.new %>%
  arrange(desc(price_mean)) %>%
  head(10)

top_vars_ar


#Get All Varieties With Average Price in Argentina
all_var_ar <- expensive.new %>%
  arrange(desc(price_mean))

all_var_ar

#mean(all_var_ar$price_mean)
#mean price: 25.28063


#Diverging Bar Chart for All Varieties in Argentina Based on Average Price
all_var_ar$price_mean_z_score <- round((all_var_ar$price_mean - mean(all_var_ar$price_mean))/sd(all_var_ar$price_mean), digits=3)
all_var_ar$price_mean_type <- ifelse(all_var_ar$price_mean_z_score < 0, "below", "above")

all_var_ar <- all_var_ar[order(all_var_ar$price_mean_z_score), ] #Ascending sort on Z Score
all_var_ar$expensive.variety <- factor(all_var_ar$expensive.variety, levels = all_var_ar$expensive.variety)

ggplot(all_var_ar, aes(x=expensive.variety, y=price_mean_z_score, label=price_mean_z_score)) +
  geom_bar(stat='identity', aes(fill=price_mean_type), width=.5) +
  scale_fill_manual(name="Price (deviation)",
                    labels = c("Above Average", "Below Average"),
                    values = c("above"="#AAC95B", "below"="#82171C")) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  labs(subtitle="Average price for each unique variety",
       title= "Wine Varieties in Argentina")+
  xlab(label = "Variety") + 
  ylab(label = "Standard Deviation From Mean Price") +
  theme(plot.title = element_text(size = 19, face = "bold", vjust = 2), plot.subtitle = element_text(vjust = 3)) +
  theme(axis.title.x = element_text(size = 13.5, vjust = -2), axis.title.y = element_text(size = 14)) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(axis.line = element_line(color = "black")) +
  annotate(
    geom = "curve", x = "Poulsard", y = 0.2, xend = "Petite Verdot", yend = 1.5, 
    curvature = .1, arrow = arrow(length = unit(2, "mm")))+
  annotate(geom = "text", x = "Poulsard", y = 1.8, label = "Mean Price: $25.28063", hjust = "left")+
  coord_flip()


#Get Top 10 Varieties for Argentina Closest To Mean Price
top_vars_ar2 <- cheap.new %>%
  arrange(desc(price_mean)) %>%
  head(21)

top_vars_ar3 <- top_vars_ar2 %>%
  arrange(price_mean) %>%
  head(10)

#top_vars_ar3

# Create Circle Plot to Introduce Top 10 Varieties for Argentina
data_ar <- top_vars_ar3
packing_ar <- circleProgressiveLayout(data_ar$price_mean, sizetype='area')
data_ar <- cbind(data_ar, packing_ar)

dat.gg_ar<-circleLayoutVertices(packing_ar, npoints=50)

ggplot() + 
  geom_polygon(data = dat.gg_ar, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
  geom_text(data = data_ar, aes(x, y, label = paste(cheap.variety, "\n", "$", (round(price_mean, 2))))) +
  scale_size_continuous(range = c(1,4)) +
  ggtitle("Top 10 Wine Varieties in Argentina", subtitle = "Closest to Mean Price") +
  theme_void() + 
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(legend.position="none") +
  coord_equal()


#Define Top 10 Titles in Argentina
cheapar<- cheap %>%
  select(points, price, variety, title, region_1)%>%
  filter(points >= 92)%>%
  arrange(desc(points)) %>%
  #subset(variety != "Champagne Blend") %>%
  #filter(variety == "Pinot Meunier") %>%
  head(10)

cheapar


#Create Text Plot for Top 10 Titles in Argentina
titles_ar <- ggplot(cheapar, aes(points, price, label = title, size = points, scale_size(range = c(6,8)), color = variety)) + 
  geom_text() +
  ggtitle("Best Wine Titles in Argentina", subtitle = "Based on points") +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  xlab(label = "") +
  ylab(label = "Price of Wine") +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.title.x = element_text(size = 13.5, vjust = -2), axis.title.y = element_text(size = 14)) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(axis.line = element_line(color = "black")) + 
  theme(legend.position = "top") 

titles_ar
  

#Define Top 10 Regions in Argentina
ar_region<- cheap %>% group_by(cheap$region_1) %>%
  arrange(desc(cheap$region_1)) %>%
  summarise_if(is.numeric, 
            funs(mean = mean(., na.rm = TRUE)))
  
ar_region<-data.frame(ar_region) %>%
  arrange(desc(price_mean)) %>%
  head(10)

ar_region


#Create Circle Plot for Best Regions in Argentina
Argentina <- ar_region %>%
  ggplot(aes(x=cheap.region_1, y=points_mean, size = price_mean, fill = cheap.region_1 )) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_size(range = c(.1, 20)) +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  ylab("Points") +
  xlab("") +
  theme(legend.position = "none") +
  ggtitle("Best Regions in Argentina")

ggplotly(Argentina)



#### Variety comparation per country
price_v_points <- ggplot(outlier, aes(points, price,color=variety)) +
  geom_point(aes(size = price, frame = country)) +
  scale_x_log10()

ggplotly(price_v_points)