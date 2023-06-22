# Membaca data CSV
covid <- read.csv("")

library(ggplot2)
library(dplyr)
library(stats)

# Memilih kolom-kolom yang relevan
data <- select(covid, Total.Cases, Location, Total.Deaths)

# Mencari data untuk DKI Jakarta
data <- filter(data, Location == "DKI Jakarta")

print(data)

# Melakukan regresi linier sederhana
fit <- lm(Total.Deaths ~ Total.Cases, data = data)

print(cor(as.numeric(data$Total.Cases), data$Total.Deaths))

# Menampilkan ringkasan regresi linier
print(summary(fit))

# Membuat scatter plot dengan garis regresi linier
ggplot(data, aes(x = Total.Cases, y = Total.Deaths)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red")

# Menentukan nilai threshold untuk klasifikasi
threshold1 <- 500000
threshold2 <- 1000000

# Menghasilkan prediksi berdasarkan nilai threshold
pred1 <- ifelse(data$Total.Cases <= threshold1, "Low", "High")
pred2 <- ifelse(data$Total.Cases <= threshold2, "Low", "High")

# Membuat tabel kontingensi
contingency_table <- matrix(c(
  sum(pred1 == "Low" & pred2 == "Low"),   sum(pred1 == "Low" & pred2 == "High"),
  sum(pred1 == "High" & pred2 == "Low"),  sum(pred1 == "High" & pred2 == "High")
), nrow = 2, ncol = 2, dimnames = list(Actual = c("Low", "High"),
                                                Predicted = c("Low", "High")))

print(contingency_table)

# Melakukan uji McNemar
mcnemar_result <- mcnemar.test(contingency_table)

# Menampilkan hasil uji McNemar
print(mcnemar_result)