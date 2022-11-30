library("tidyverse")
library("janitor")
library("esquisse")
library("ggforce")
library("performance")
library("correlation")
library("see")
library("caret")
library("rattle")
library("RColorBrewer")
library("hrbrthemes")
library("forcats")
library("scales")

df <- read.csv("./data/flight_delays_data.csv", stringsAsFactors=T)
df <- drop_na(df)


df <- slice_sample(df, prop = 1)


df <- df %>% 
  mutate(
    is_claim = ifelse(is_claim == 800, TRUE, FALSE),
    std_hour = as.double(df$std_hour),
    is_cancelled = ifelse(delay_time == "Cancelled", TRUE, FALSE)
  ) 







df %>% 
  tabyl(Airline, is_claim) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(digits = 1)  %>% 
  adorn_ns() %>% 
  arrange(desc("FALSE"))

df %>% 
  tabyl(Airline, is_claim) %>% 
  chisq.test()


# Top performing flights
top_flights <- df %>% 
  filter(Airline %in% c("CX", "OZ", "NZ", "MH", "JL", "GK", "GA", "BX")) %>% 
  group_by(Airline, is_claim) %>% 
  count() %>% 
  group_by(Airline) %>% 
  mutate(percent = n/sum(n)) 
  


top_flights %>% 
  filter(is_claim == TRUE) %>% 
  ggplot() + 
  geom_bar(aes(x =Airline, y = percent), stat = "identity") + 
  theme_ipsum() + 
  scale_fill_grey() +
  labs(title = "Top Performing Carriers", 
       subtitle = "By Claims Ratio", 
       y = "Claims Ratio (%)", 
       x = "Airline Carrier") + 
  scale_y_continuous(labels = label_percent())



worst_flights <- df %>% 
  filter(Airline %in% c("SV", "BO", "HB", "P7", "O3")) %>% 
  group_by(Airline, is_claim) %>% 
  count() %>% 
  group_by(Airline) %>% 
  mutate(percent = n/sum(n)) 


worst_flights %>% 
  filter(is_claim == TRUE) %>% 
  ggplot() + 
  geom_bar(aes(x = Airline, y = percent, fill = is_claim), stat = "identity", position = "dodge")




# Un Filteres
df %>% 
  select(Week, is_claim) %>% 
  group_by(Week, is_claim) %>% 
  count() %>% 
  ggplot(aes(x = Week, y = n)) + geom_smooth(aes(color = is_claim)) + 
  geom_point() +
  labs(title = "Number of Total Claims Occur", 
       subtitle = "By Weeks", 
       y = "Number of Flights", 
       x = "Weeks in a Year") + 
  theme_ipsum()


# Ratios to Claims
df %>% 
  select(Week, is_claim) %>% 
  group_by(Week, is_claim) %>% 
  count() %>% 
  group_by(Week) %>% 
  mutate(percent = n/sum(n)) %>% 
  ggplot(aes(x = Week, y = percent)) + geom_smooth(aes(color = is_claim)) + 
  geom_point() 



df %>% 
  select(Week, is_claim) %>% 
  t.test()


# train the model on training set
model <- train(as.factor(is_claim) ~ std_hour + Airline,
               data = df,
               method = "rpart")



fancyRpartPlot(model$finalModel)






# Identify Factors that attribute to low risk or high risk flights,
# Delay Time is Greater than 3 hours
# Flight was cancelled

glimpse(df)
# Discovery
#




# All Departures are from HK
df %>% 
  tabyl(Departure)


# 137 Destinations from HK
unique(df$Arrival)
df %>% 
  tabyl(Arrival) %>% 
  arrange(desc(n))









final

# two way proportional Table

# Statistical Tests

# Logistic Regression