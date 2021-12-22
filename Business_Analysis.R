library(gam)
library(mgcv)

input_data = read.csv("business.csv",header = TRUE)

head(input_data)
gam_input = gam(count ~ s(date,spar = 0.38),data = input_data)
plot(gam_input, col = "red", se = TRUE,residuals = T,cex = 1)
fitted_values = fitted(gam_input)

std_obj = predict(gam_input, se.fit = TRUE)
std = std_obj$se.fit
upper_bound = fitted_values + 2 * std
lower_bound = fitted_values - 2 * std
print(std)

output_data = data.frame(input_data$count, fitted_values, lower_bound, upper_bound)
output_boo = (input_data$count > lower_bound)&(input_data$count < upper_bound)
output_show = data.frame(output_data, output_boo)
print(output_show)
write.table(output_show,file = "count.csv",row.names = FALSE,sep = ",")
