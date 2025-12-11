# 2.1. Оценка эффекта методом разности разностей

# Загрузка набора данных
#install.packages("dplyr")       
#install.packages("stargazer")
#install.packages("haven") 
#install.packages("devtools")

library(dplyr)
library(stargazer)
library(haven)

#dataset <- read_dta("data_mac_sr.dta")

# Фильтрация данных по условию sample_mac_sr == 1
filtered_data <- dataset %>% filter(sample_mac_sr == 1)

# Создание дамми-переменных для дальнейшего анализа
filtered_data <- filtered_data %>%
  mutate(
    is_corsica = ifelse(region == 1, 1, 0),
    Corsica_bdays = is_corsica * bdays,
    Sardinia_bdays = (1 - is_corsica) * bdays
  )

# Модель 1: "Обычная" разность разностей без контрольных переменных
model_1 <- lm(lprice100 ~ Treated*Post, data = filtered_data)
summary(model_1)

# Модель 2: Добавление контрольных переменных
#Это нужно для того, чтобы убрать смещенность оценки
#Скорее всего оценка не состоятельна и смещена
#все что нескоррелировано с переменной интереса можно не включать - смещения не будет
model_2 <- lm(lprice100 ~ Treated*Post + Corsica_bdays + Sardinia_bdays + google_src + town_avail,
              data = filtered_data)
summary(model_2)

# Модель 3: Включение тройного взаимодействия с типом отелей (сетевые/несетевые)
model_3 <- lm(lprice100 ~ Treated*Post*dchain + Corsica_bdays + Sardinia_bdays + google_src + town_avail, 
              data = filtered_data)
summary(model_3)

# Модель 4: Включение тройного взаимодействия со звёздностью отелей
model_4 <- lm(lprice100 ~ Treated*Post*stars + Corsica_bdays + Sardinia_bdays + google_src + town_avail, 
              data = filtered_data)
summary(model_4)

# Модель 5: Включение тройного взаимодействия с размером отелей
model_5 <- lm(lprice100 ~ Treated*Post*hot_size + Corsica_bdays + Sardinia_bdays + google_src + town_avail, 
              data = filtered_data)
summary(model_5)

# Формирование таблицы с результатами всех моделей

stargazer(model_1, model_2, model_3, model_4, model_5, 
          type = "text", 
          title = "Результаты регрессионного анализа методом разности разностей")



# Задание 2.2. Оценка эффекта синтетическим контролем


library("devtools")
devtools::install_github("edunford/tidysynth")
library("tidysynth")

# Фильтрация данных по условию synth_mac == 1
data <- read_dta("data_mac_synth.dta")
data <- data %>% filter(synth_mac == 1)

out1 <- data %>%
  synthetic_control(outcome= lprice100,
                    time = week_src,
                    unit = classification,
                    i_unit = 1,
                    i_time = 32,
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 25:32,
                     bdays = mean(bdays, na.rm = TRUE),
                     stars = mean(stars, na.rm = TRUE),
                     hot_size = mean(hot_size, na.rm = TRUE),
                     capacity = mean(capacity, na.rm = TRUE),
                     punteggio = mean(punteggio, na.rm = TRUE),
                     date_start_booking = mean(date_start_booking, na.rm = TRUE),
                     google_src = mean(google_src, na.rm = TRUE)
  ) %>%
  
  generate_predictor(time_window = 25,
                     lprice100_25 = lprice100) %>%
  generate_predictor(time_window = 28,
                     lprice100_28 = lprice100) %>%
  generate_predictor(time_window = 32,
                     lprice100_32 = lprice100) %>%
  
  generate_weights(optimization_window = 25:32,
                   margin_ipop = 0.02,
                   sigf_ipop = 7,
                   bound_ipop = 6) %>%
  generate_control()

# количественный показатель качества подгонки в SC и в плацебо-тестах
out1 %>% plot_mspe_ratio()

filter_class <- c(1, 22, 6, 9, 16, 10, 20, 17, 14, 19, 13, 15, 18, 7, 12)

data <- data[data$classification %in% filter_class, ]

out1 <- data %>%
  synthetic_control(outcome= lprice100,
                    time = week_src,
                    unit = classification,
                    i_unit = 1,
                    i_time = 32,
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 25:32,
                     bdays = mean(bdays, na.rm = TRUE),
                     stars = mean(stars, na.rm = TRUE),
                     hot_size = mean(hot_size, na.rm = TRUE),
                     capacity = mean(capacity, na.rm = TRUE),
                     punteggio = mean(punteggio, na.rm = TRUE),
                     date_start_booking = mean(date_start_booking, na.rm = TRUE),
                     google_src = mean(google_src, na.rm = TRUE)
  ) %>%
  
  generate_predictor(time_window = 25,
                     lprice100_25 = lprice100) %>%
  generate_predictor(time_window = 28,
                     lprice100_28 = lprice100) %>%
  generate_predictor(time_window = 32,
                     lprice100_32 = lprice100) %>%
  
  generate_weights(optimization_window = 25:32,
                   margin_ipop = 0.02,
                   sigf_ipop = 7,
                   bound_ipop = 6) %>%
  generate_control()

#проверка весов, стало лучше
out1 %>% plot_mspe_ratio()

# графики
# тренды в настоящей и в синтетической Корсике
out1 %>% plot_trends()


# баланс контрольных переменных - имеется
out1 %>% grab_balance_table() #настоящая - синтетика - донор

# gap
out1 %>% plot_differences()

# веса
out1 %>% grab_unit_weights()
out1 %>% grab_predictor_weights()
out1 %>% plot_weights()

#2.2.3

# плацебо-тест во времени

out_placebo <- data %>%
  synthetic_control(outcome= lprice100,
                    time = week_src,
                    unit = classification,
                    i_unit = 1,
                    i_time = 28,
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = 25:28,
                     bdays = mean(bdays, na.rm = TRUE),
                     stars = mean(stars, na.rm = TRUE),
                     hot_size = mean(hot_size, na.rm = TRUE),
                     capacity = mean(capacity, na.rm = TRUE),
                     punteggio = mean(punteggio, na.rm = TRUE),
                     date_start_booking = mean(date_start_booking, na.rm = TRUE),
                     google_src = mean(google_src, na.rm = TRUE)
  ) %>%
  
  generate_predictor(time_window = 25,
                     lprice100_25 = lprice100) %>%
  generate_predictor(time_window = 28,
                     lprice100_28 = lprice100) %>%
  
  generate_weights(optimization_window = 25:28,
                   margin_ipop = 0.02,
                   sigf_ipop = 7,
                   bound_ipop = 6) %>%
  generate_control()


# результаты плацебо-тестов
#sc_placebo<-out_placebo %>% grab_synthetic_control()
out_placebo %>% plot_trends()
out_placebo %>% plot_differences()

#2.2.4
# плацебо в пространстве
out1 %>% plot_placebos() #не знаю, нужно ли

out1 %>% plot_placebos(prune = FALSE)

out1 %>%
  plot_mspe_ratio()
