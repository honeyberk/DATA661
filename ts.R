
# http://blog.fastforwardlabs.com/2017/03/22/prophet.html

library(prophet)
library(dplyr)

df <- read.csv('example_wp_peyton_manning.csv') %>%
  mutate(y = log(y))

m <- prophet(df)
future <- make_future_dataframe(m, periods = 365)
tail(future)

forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m, forecast)

prophet_plot_components(m, forecast)

