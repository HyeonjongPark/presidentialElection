


library(tidyr)
library(highcharter)
library(forecast)
library(dplyr)

vote_vis = function(voteRate) {
  vote = data.frame(blue = voteRate[seq(1,length(voteRate),2)],
                    red = voteRate[seq(2,length(voteRate),2)],
                    realpred = "real")
  
  
  h_value = 100 - nrow(vote)
  
  red_fitted = auto.arima(vote$red)
  red_forecast = forecast(red_fitted,h=h_value)
  red_forecast = as.double(red_forecast$mean)
  
  blue_fitted = auto.arima(vote$blue)
  blue_forecast = forecast(blue_fitted,h=h_value)
  blue_forecast = as.double(blue_forecast$mean)
  
  vote = rbind(vote, data.frame(red = red_forecast,
                                blue = blue_forecast,
                                realpred = "pred"))
  
  vote$openRate = 1:nrow(vote)
  
  vote$openRate = as.factor(vote$openRate)
  
  vote_diff = vote %>% mutate(diff = red - blue)
  vote_diff = gather(vote_diff, key = 'flag', value = "values", `red`,`blue`,`diff`)
  
  
  custom_colours <- c("black", "red", "blue")
  
  vote_diff2 = vote_diff %>% mutate(flag = ifelse(flag == "blue", "이재명",
                                                  ifelse(flag == "red" , "윤석열", "%포인트 차이")))
  vote_diff2 %>% 
    hchart(., 
           type = "line", 
           hcaes(x = openRate, 
                 y = values, 
                 group = flag)) %>% 
    hc_yAxis(opposite = TRUE,
             labels = list(format = "{value}%")) %>% 
    hc_colors(custom_colours) %>% 
    hc_xAxis(title = list(text = "득표율 & 격차"),
             plotLines = list(list(
               value = (length(voteRate)/2 - 0.5),
               color = 'green',
               width = 3,
               zIndex = 4,
               label = list(text = "현재 개표율",
                            style = list( color = '개표율', fontWeight = 'bold'   )
               )))) %>% 
    hc_yAxis(title = list(text = "득표율 & 격차"),
             plotLines = list(list(
               value = 0,
               color = 'yellow',
               width = 3,
               zIndex = 4,
               label = list(text = "Cross",
                            style = list( color = 'red', fontWeight = 'bold'   )
               ))))
  
}

voteRate = c(53.8, 43.2,
             53.3, 43.7,
             52.7, 44.2,
             51.8, 45.1,
             51.3, 45.6,
             51.1, 45.8,
             50.6, 46.3,
             50.2, 46.6,
             50.0, 46.8,
             50.0, 46.8,
             50.2, 46.6,
             50.2, 46.6,
             50.1, 46.7,
             50.1, 46.7,
             50.2, 46.6,
             50.2, 46.7,
             50.0, 46.8,
             49.9, 46.9,
             49.9, 46.9,
             49.9, 46.9,
             49.8, 46.9, # 21
             49.8, 46.9, # 22
             49.9, 46.9, # 23
             49.9, 46.9, # 24
             49.8, 46.9, # 25
             49.8, 46.9, # 26
             49.8, 47.0, # 27
             49.8, 47.0, # 28
             49.6, 47.1, # 29
             49.6, 47.1, # 30
             49.5, 47.2, # 31
             49.4, 47.3, # 32
             49.2, 47.5, # 33
             49.2, 47.5, # 34
             49.0, 47.7, # 35
             49.0, 47.7, # 36
             48.9, 47.8, # 37
             48.8, 47.8, # 38
             48.7, 47.9, # 39
             48.6, 48.0, # 40
             48.6, 48.1, # 41
             48.6, 48.1, # 42
             48.6, 48.1, # 43
             48.5, 48.1, # 44
             48.5, 48.1, # 45
             48.5, 48.1, # 46
             48.5, 48.1, # 47
             48.4, 48.2, # 48
             48.4, 48.2, # 49
             48.4, 48.2, # 50
             48.3, 48.3, # 51
             48.2, 48.4, # 52
             48.2, 48.4, # 53
             48.2, 48.4, # 54
             48.1, 48.4, # 55
             48.1, 48.5) # 56



vote_vis(voteRate)

