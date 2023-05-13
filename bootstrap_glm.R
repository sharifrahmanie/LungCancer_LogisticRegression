require(readr)
require(boot)
# By biomedical_informatics Edris Sharif Rahmani May 13, 2023
data <- data.frame(read_csv("lung_cancer.csv"))
names <- colnames(data)
for(i in names) {
  if(i == "age"){
    next
  } else {
    data[,i] <- factor(data[,i])
  }
}
calculate_p_values <- function(data, indices) {
  bootstrap_data <- data[indices, ]
  model <- glm(lung_cancer ~ gender + age + smoking +  
                 alcohol.consuming + yellow_fingers + anxiety +
                 peer_pressure + allergy + swallowing.difficulty +
                 chest.pain, data = bootstrap_data, family = binomial)
  coef_table <- summary(model)$coef
  p_values <- coef_table[, "Pr(>|z|)"]
  return(p_values)
}
boot_results <- boot(data, calculate_p_values, R = 1000)
bootstrap_p_values <- data.frame(boot_results$t0)
