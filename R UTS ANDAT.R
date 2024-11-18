getwd()
setwd("C:/andat")

utsandat = read.csv("Employee Attrition.csv", sep = ",", header = TRUE)
utsandat


# c. Check the packaging 
nrow(utsandat)
ncol(utsandat)
str(utsandat)


# d. Look at the Top and Bottom of your Data 
head(utsandat)
tail(utsandat)


# e. Check your “n”s 
head(table(utsandat$last_evaluation))

library(dplyr)
filter(utsandat, last_evaluation == "0.8") %>%
  select(satisfaction_level,last_evaluation)
select(utsandat, satisfaction_level) %>% unique %>% nrow
unique(utsandat$satisfaction_level)
dim(utsandat)

# f. Validate with at least one external data source
summary(utsandat$satisfaction_level)
quantile(utsandat$satisfaction_level, seq(0, 1, 0.1), na.rm = TRUE)

summary(utsandat$last_evaluation)
quantile(utsandat$last_evaluation, seq(0, 1, 0.1), na.rm = TRUE)

# g. Make a plot 
# Memuat library yang diperlukan
library(ggplot2)

# Membuat boxplot untuk satisfaction_level berdasarkan last_evaluation
ggplot(utsandat, aes(x = as.factor(last_evaluation), y = satisfaction_level)) +
  geom_boxplot(fill = "darkred", color = "black") +
  labs(title = "Boxplot Tingkat Kepuasan Karyawan Berdasarkan Evaluasi Terakhir",
       x = "Evaluasi Terakhir (Last Evaluation)",
       y = "Tingkat Kepuasan Karyawan (Satisfaction Level)") +
  theme_minimal()

# h.	Try the easy solution first
summary_table <- utsandat %>%
  group_by(last_evaluation) %>%
  summarize(
    mean_satisfaction = mean(satisfaction_level, na.rm = TRUE),
    median_satisfaction = median(satisfaction_level, na.rm = TRUE),
    count = n()
  )

# Menampilkan ringkasan
print(summary_table)

# 3a. Model as expectations
model = lm(satisfaction_level ~ last_evaluation, data = utsandat)
model 
summary(model)


# 3b. Comparing Model Expectations to Reality: buat histogram variabel dependen 
# dan bandingkan dengan histogram data yang berdistribusi normal! 
# Memuat library yang diperlukan
library(ggplot2)

# Mengambil data satisfaction_level untuk last_evaluation = 0.8
data_filtered <- filter(utsandat, last_evaluation == 0.8)

# Membuat histogram dari satisfaction_level
ggplot(data_filtered, aes(x = satisfaction_level)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "yellow", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(data_filtered$satisfaction_level, na.rm = TRUE), 
                                         sd = sd(data_filtered$satisfaction_level, na.rm = TRUE)), 
                color = "darkred", size = 1) +
  labs(title = "Histogram Tingkat Kepuasan Karyawan dengan Kurva Normal",
       x = "Tingkat Kepuasan Karyawan",
       y = "Kepadatan") +
  theme_minimal()
  
