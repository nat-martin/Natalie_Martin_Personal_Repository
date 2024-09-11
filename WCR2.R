#WCR2
library(readr)
data <- data.frame(
  "Name" = c("natalie martin", "teddy grace", "marge kuffner"),
  "Age" = c(20, 21, 22),
  "Occupation" = c("Biologist", "Billionare ", "Plumber")
)

write.csv(data,"csv_file.csv")





