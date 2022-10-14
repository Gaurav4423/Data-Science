#Q1

install.packages("gapminder")
library(gapminder)
install.packages("dplyr")
library(dplyr)
gm <- gapminder

#Q1. 1
gm %>% 
  group_by(continent) %>% 
  summarize(n=n_distinct(country))

#Q1. 2
gm %>%
  filter(continent == "Europe" & year == 1997) %>%
  arrange(gdpPercap) %>%
  head(1)

#Q1. 3
gm %>%
  filter(year == 1982 | year == 1987) %>%
  group_by(continent) %>%
  summarize(mean_lifeExp = mean(lifeExp))

#Q1. 4
gm %>%
  mutate(gdp = gdpPercap*pop) %>%
  group_by(country) %>%
  summarise(Total.GDP = sum(gdp)) %>%
  arrange(desc(Total.GDP)) %>%
  head(5)

#Q1. 5
gm %>%
  filter(lifeExp >= 80) %>%
  select(country, lifeExp, year)

#Q1. 6
gm %>%
  group_by(country) %>%
  summarise(cor = cor(lifeExp, gdpPercap)) %>%
  arrange(desc(abs(cor))) %>%
  head(10)

#Q1. 7
gm %>%
  filter(continent != "Asia") %>%
  group_by(continent, year) %>%
  summarise(mean.pop = mean(pop)) %>%
  arrange(desc(mean.pop))

#Q1. 8
gm %>%
  group_by(country) %>%
  summarize(sd.pop = sd(pop)) %>%
  arrange(sd.pop) %>%
  head(3)

#Q1. 9
gm %>% 
  arrange(country, year) %>% 
  group_by(country) %>% 
  filter(pop < lag(pop) & lifeExp > lag(lifeExp))

#Q2

#Q2. 1
MedID<-c(1,2,3,4,5,6,7,8,9,10)
Med_Name<-c("Abacavir","Acyclovir","Alemtuzumab","Alendronate","Allopurinol","Ampicillin","Amoxicillin","Atenolol","Azithromycin","Carboplatin")
Company<-c("Acadia","Abbott","Alnylam","Amgen","Assertio","Alexion","Advanz","Avella","Advaxis","Arbutus")
Manf_year<-c(2000,2001,1997,2010,2008,2021,2019,2020,2001,2008)
Exp_date<-c(20/06/2023, 30/01/2001, 08/09/2002, 07/07/2011, 19/05/2020, 17/06/2024, 13/04/2020, 01/01/2022, 27/06/2020, 09/08/2010)
Quantity_in_stock<-c(2,4,6,3,8,9,4,8,10,7)
Sales<-c(3004.5, 8090.7, 2537.6, 4358.9, 4396.14, 2597.39, 7737.93, 2333.12, 4177.35, 4099.68)

df<- data.frame(MedID, Med_Name, Company, Manf_year, Exp_date, Quantity_in_stock, Sales)

#Q2. 2
head(df,4)

#Q2. 3
tail(df, 4)

#Q2. 4
cor(df$)

























