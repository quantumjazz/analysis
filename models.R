library(tidyverse)
library(tsibble)
library(fable)
library(feasts)
library(splines)
library(lubridate)
library(sugrrants)


#Round dates so conversion to tsibble is done properly
bg_full$datetime <- round_date(bg_full$datetime)

#Analysis of monthly data
#Create tibble with aggregated monthly data
monthly_ts <- bg_full %>%
mutate(month = yearmonth(datetime)) %>%
group_by(month) %>%
summarise(m_demand = sum(demand),
m_temp = mean(temp1))

#Create tsibble with aggregated monthly data
monthly_ts <- monthly_ts %>% as_tsibble(index = month)


#Create tsibble with hourly data structure, train and test sets
timeseries <- bg_full %>%
mutate(datetime = as_datetime(datetime)) %>%
mutate(dateyear = as_date(dateyear)) %>% 
as_tsibble(index = datetime)

train <- monthly_ts %>%
filter(month < yearmonth("2020 Jan"))

#Fit models
#first, forecast with decomposition
model_def = decomposition_model(STL(m_demand ~ trend(window =23) + season(window = Inf)),
                                ARIMA(season_adjust ~ PDQ(0,0,0)),
                                SNAIVE(season_year))
fit <- train %>% model(model_def)
report(fit)
forecast(fit, h=12) %>% autoplot(size = 0.2) + autolayer(monthly_ts, size = 0.2) + 
  theme(axis.line = element_blank()) + 
  theme(panel.border = element_rect(size = 0.2, colour = "black")) + 
  xlab("Година") + ylab("GWh")
ggsave("four_seven.png", width = 18, height = 9, units = "cm", dpi = 600)

#Then..
mdls <- monthly_ts %>% model(
Mdl1 = ARIMA(m_demand ~ ns(m_temp, df=4) + pdq(0,1,1) + PDQ(1,1,0)),
Mdl2 = ARIMA(m_demand ~ season() + pdq(0,1,2) + PDQ(0,0,0)),
Mdl3 = ETS(m_demand)
)

augment(mdls) %>% features(.innov, ljung_box, dof=15, lag = 20)
fc1 <- forecast(mdls, new_data = test)

accuracy(fc1, data = monthly_ts)


#Hourly analysis



