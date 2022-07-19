library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(readxl)
data <- read_excel("shaurya r project/data.xlsx")
View(data)
data <- read_excel("C:/Users/shaurya khanna/Desktop/shaurya r project/data.xlsx")
View(data)
str(data)

data$Date<- as.numeric(data$Date)

#summary of Columns of HUL 
summary(data$`Open Price`)
summary(data$`High Price`)
summary(data$`Low Price`)
summary(data$`Close Price`)
summary(data$`Total Traded Quantity`)



#Correlation Between Date and other Variables
cor.test(data$'Date',data$'Low Price', method = "pearson")
cor.test(data$'Date',data$'High Price',method = "pearson")

cor(data$Date,data$'Open Price')
cor(data$Date,data$'High Price')
cor(data$Date,data$'Low Price')
cor(data$Date,data$`Close Price`)
cor(data$Date,data$`Total Traded Quantity`)

#Correlation With different Variables
cor(data$'Open Price', data$`Close Price`)
cor(data$`Open Price` ,data$`High Price`)
cor(data$`Open Price`,data$`Low Price`)
cor(data$`Close Price`, data$`Low Price`)
cor(data$`Close Price`, data$`High Price`)
cor(data$`High Price`, data$`Low Price`)
cor(data$`Open Price`, data$`Total Traded Quantity`)
cor(data$`Close Price`, data$`Total Traded Quantity`)
cor(data$`High Price`, data$`Total Traded Quantity`)
cor(data$`Low Price`, data$`Total Traded Quantity`)

#Correlation Test Between Date and other Variables

cor.test(data$Date, data$`Open Price`)
cor.test(data$Date, data$`Close Price`)
cor.test(data$Date, data$`High Price`)
cor.test(data$Date, data$`Low Price`)
cor.test(data$Date, data$`Total Traded Quantity`)


#Correlation Test With different Variables

cor.test(data$`Open Price`, data$`Close Price`)
cor.test(data$`Open Price` ,data$`High Price`)
cor.test(data$`Open Price`,data$`Low Price`)
cor.test(data$`Close Price`,data$`Low Price`)
cor.test(data$`Close Price`, data$`High Price`)
cor.test(data$`High Price`, data$`Low Price`)
cor.test(data$`Open Price`, data$`Total Traded Quantity`)
cor.test(data$`Close Price`, data$`Total Traded Quantity`)
cor.test(data$`High Price`, data$`Total Traded Quantity`)
cor.test(data$`Low Price`, data$`Total Traded Quantity`)


model=lm(data$`Open Price`~data$`Low Price`)
model
summary(model)

model1=lm(data$`Open Price`~data$`High Price`)
model1
summary(model1)

model2=lm(data$`Close Price`~data$`Low Price`)
model2
summary(model2)

model3=lm(data$`Open Price`~data$`High Price`)
model3
summary(model3)

model4=lm(data$`High Price`~data$`Total Traded Quantity`)
model4
summary(model4)

model5=lm(data$`High Price`~data$`Low Price`)
model5
summary(model5)

model6=lm(data$`Open Price`~data$`Total Traded Quantity`)
model6
summary(model6)

model7=lm(data$`Close Price`~data$`Total Traded Quantity`)
model7
summary(model7)

model8=lm(data$`Low Price`~data$`Total Traded Quantity`)
model8
summary(model8)

model9=lm(data$'Date'~data$`Open Price`)
model9
summary(model9)

################Graphical Representation#####################
library(readxl)
data2 <- read_excel("data2.xlsx")
View(data2)

#linechart Between date and open price

library(ggplot2)

ggplot(data2) +
  aes(x = Date, y = `Open Price`) +
  geom_line(size = 0.5, colour = "#112446") +
  labs(x = "Date(months)", y = " Open Price(in Rs.)", title = "Line Chart of Date and  Open Price of HUL") +
  theme_dark()

#linechart Between date and  Close Price

library(ggplot2)

ggplot(data2) +
  aes(x = Date, y = `Close Price`) +
  geom_line(size = 0.5, colour = "#112446") +
  labs(x = "Date", y = " Close Price (in Rs)", title = "Line Chart for Date and  Close Price of HUL") +
  theme_gray()

#linechart Between Date and  High Price

library(ggplot2)

ggplot(data2) +
  aes(x = Date, y = `High Price`) +
  geom_line(size = 0.5, colour = "#112446") +
  labs(x = "Date", y = "High Price(in Rs)", title = "Date and High Price of HUL") +
  theme_gray()


#linechart Between Date and Low Price


library(ggplot2)

ggplot(data2) +
  aes(x =Date, y = `Low Price`) +
  geom_line(size = 0.5, colour = "#112446") +
  labs(x = "Date", 
       y = "Low Price (in Rs.)", title = "Date and Low Price of HUL") +
  theme_gray()

#linechart Between Date and Total Traded Quantity

ggplot(data2) +
  aes(x = Date, y = `Total Traded Quantity`) +
  geom_line(size = 0.5, colour = "#112446") +
  labs(
    x = "Date",
    y = "Vol. Traded",
    title = "Date and Total Traded Quantity of HUL"
  ) +
  theme_gray()

library(rvest)
library(dplyr)

url="https://www.moneycontrol.com/stocks/company_info/stock_news.php?sc_id=HL&scat=&pageno=3&next=0&durationType=Y&Year=2021&duration=1&news_type="
page1=read_html(url)
page1
date1=page1%>%html_nodes(".a_10dgry")%>%html_text()
date1
news1=page1%>%html_nodes(".g_14bl strong")%>%html_text()
news1
desc1=page1%>%html_nodes(".company-news-listing-txt p+ p")%>%html_text()
newsmodel2=data.frame(date1,news1)
View(newsmodel2)
write.csv(newsmodel2,"newsmodel2.csv")
library(readxl)
newsmodel2 <- read_excel("C:/Users/shaurya khanna/Desktop/newsmodel2.xlsx")
View(newsmodel2)
Country <- c("Positive", "Negative", "Positive", "Negative", "Positive", "Negative",    
             "Positive", "Negative", "Positive", "Negative","Positive", "Negative","Positive", "Negative","Positive", "Negative","Positive", "Negative","Positive", "Negative")
newsmodel2['Description'] <- Country
newsmodel2$Description
newsmodel2$News_new_Category=ifelse(newsmodel2$Description=="Positive", "+1", ifelse(newsmodel2$Description=="Negative","-1",ifelse(newsmodel2$Description=="Nuetral", "0"," ")))
View(newsmodel2)

library(dplyr)
newsmodel2$Date<-as_date(newsmodel2$Date)
Merge_HUL_news<- inner_join(data2,newsmodel2,by="Date")
View(Merge_HUL_news)
names(Merge_HUL_news)


Merge_HUL_news$Average_Price<-((Merge_HUL_news$`Open Price`+Merge_HUL_news$`Close Price`)/2)
model=lm(Merge_HUL_news$News_new_Category~Merge_HUL_news$Average_Price)
summary(model)
names(Merge_HUL_news)

ggplot(data = Merge_HUL_news,aes(y=Merge_HUL_news$Average_Price,x=Merge_HUL_news$News_new_Category)) + geom_point() + labs(y='Average_price',x="News_new_Category")

mode3=lm(data$`Open Price`~britannia_1_$`Open Price`)
mode3
summary(mode3)

mode4=lm(data$`High Price`~britannia_1_$`High Price`)
mode4
summary(mode4)

mode5=lm(data$`Low Price`~britannia_1_$`Low Price`)
mode5
summary(mode5)

mode6=lm(data$`Close Price`~britannia_1_$`Close Price`)
mode6
summary(mode6)

