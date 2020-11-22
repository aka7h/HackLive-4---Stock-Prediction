library(tidyverse)
library(tsibble)
library(feasts)
library(forecast)
library(fable)
library(lubridate)
library(timetk)
library(quantmod)
library(prophet)
library(fable.prophet)


train <- read_csv("Train_awoL0xl.csv")
test <- read_csv("Test_QQKW4dv.csv")

train <- train %>% 
  unite("stock_pred",c("stock","unpredictability_score"),sep = "_",remove = F)

#d <- train %>% group_by(stock) %>% summarise(cnt=n())



#check the how unpredictable each stock option is. 

stock_predictability <- train %>% group_by(stock,unpredictability_score) %>% 
  select(stock,unpredictability_score) %>% distinct() %>% ungroup()

stock_pred_1 <- stock_predictability %>% 
  group_by(unpredictability_score) %>% 
  filter(row_number()==1) %>% 
  unite("stock_pred",c("stock","unpredictability_score"),sep = "_",remove = F)


#now lets use straight forward statistical time series model 

tr_1 <- train %>% select(stock_pred,Date,Close) %>% as_tsibble(.,key="stock_pred")


tr_1 %>% filter(stock_pred %in% stock_pred_1$stock_pred) %>% autoplot(Close)

#filter(stock_pred %in% stock_pred_1$stock_pred) %>%
prophet_fit <- tr_1 %>% 
  model(
    prophet_m = prophet(Close~growth("linear")),
  )
components(prophet_fit) %>% autoplot()

final_pred <- prophet_fit %>% forecast(h=61)


test <- test %>% 
  unite("stock_pred",c("stock","unpredictability_score"),sep = "_",remove = F)

final_sub <- test %>% select(ID,stock_pred,Date) %>%
  left_join(select(final_pred,stock_pred,Date,.mean),by=c("stock_pred","Date"))

final_sub <- final_sub %>% select(ID,.mean) %>%
  rename("Close"=".mean")

filename <- paste('ak_prophet',format(Sys.time(),"%Y%m%d%H%M%s"),sep = '_')
write.csv(final_sub,paste0(filename,'.csv',collapse = ''),row.names = FALSE)
