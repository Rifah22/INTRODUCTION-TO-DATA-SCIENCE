library(dplyr)



data <- read.csv("C:/Final Task 1/Dataset/user_behavior_dataset.csv", header = TRUE, sep = ',')
print(data)



pearson_app_usage <- cor.test(data$App.Usage.Time..min.day., data$User.Behavior.Class, method = "pearson")
cat("Pearson correlation for App Usage Time (r): ", pearson_app_usage$estimate, "\n")
cat("p-value for App Usage Time: ", pearson_app_usage$p.value, "\n")

pearson_age <- cor.test(data$Age, data$User.Behavior.Class, method = "pearson")
cat("Pearson correlation for Age (r): ", pearson_age$estimate, "\n")
cat("p-value for Age: ", pearson_age$p.value, "\n")

pearson_data_usage <- cor.test(data$Data.Usage..MB.day., data$User.Behavior.Class, method = "pearson")
cat("Pearson correlation for Data Usage (r): ", pearson_data_usage$estimate, "\n")
cat("p-value for Data Usage: ", pearson_data_usage$p.value, "\n")

pearson_apps_installed <- cor.test(data$Number.of.Apps.Installed, data$User.Behavior.Class, method = "pearson")
cat("Pearson correlation for Number of Apps Installed (r): ", pearson_apps_installed$estimate, "\n")
cat("p-value for Number of Apps Installed: ", pearson_apps_installed$p.value, "\n")

pearson_battery_drain <- cor.test(data$Battery.Drain..mAh.day., data$User.Behavior.Class, method = "pearson")
cat("Pearson correlation for Battery Drain (r): ", pearson_battery_drain$estimate, "\n")
cat("p-value for Battery Drain: ", pearson_battery_drain$p.value, "\n")

pearson_screen_on <- cor.test(data$Screen.On.Time..hours., data$User.Behavior.Class, method = "pearson")
cat("Pearson correlation for Screen On Time (r): ", pearson_screen_on$estimate, "\n")
cat("p-value for Screen On Time: ", pearson_screen_on$p.value, "\n")



chisq_device <- chisq.test(table(data$Device.Model, data$User.Behavior.Class))
cat("Chi-Squared p-value for Device Model:", chisq_device$p.value, "\n")

chisq_os <- chisq.test(table(data$Operating.System, data$User.Behavior.Class))
cat("Chi-Squared p-value for Operating System:", chisq_os$p.value, "\n")

chisq_gender <- chisq.test(table(data$Gender, data$User.Behavior.Class))
cat("Chi-Squared p-value for Gender:", chisq_gender$p.value, "\n")



kendall_age <- cor.test(data$Age, data$User.Behavior.Class, method = "kendall")
cat("Kendall correlation for Age (r): ", kendall_age$estimate, "\n")
cat("p-value for Age: ", kendall_age$p.value, "\n")

kendall_data_usage <- cor.test(data$Data.Usage..MB.day., data$User.Behavior.Class, method = "kendall")
cat("Kendall correlation for Data Usage (r): ", kendall_data_usage$estimate, "\n")
cat("p-value for Data Usage: ", kendall_data_usage$p.value, "\n")

kendall_apps_installed <- cor.test(data$Number.of.Apps.Installed, data$User.Behavior.Class, method = "kendall")
cat("Kendall correlation for Number of Apps Installed (r): ", kendall_apps_installed$estimate, "\n")
cat("p-value for Number of Apps Installed: ", kendall_apps_installed$p.value, "\n")

kendall_battery_drain <- cor.test(data$Battery.Drain..mAh.day., data$User.Behavior.Class, method = "kendall")
cat("Kendall correlation for Battery Drain (r): ", kendall_battery_drain$estimate, "\n")
cat("p-value for Battery Drain: ", kendall_battery_drain$p.value, "\n")

kendall_screen_on <- cor.test(data$Screen.On.Time..hours., data$User.Behavior.Class, method = "kendall")
cat("Kendall correlation for Screen On Time (r): ", kendall_screen_on$estimate, "\n")
cat("p-value for Screen On Time: ", kendall_screen_on$p.value, "\n")

kendall_app_usage <- cor.test(data$App.Usage.Time..min.day., data$User.Behavior.Class, method = "kendall")
cat("Kendall correlation for App Usage Time (r): ", kendall_app_usage$estimate, "\n")
cat("p-value for App Usage Time: ", kendall_app_usage$p.value, "\n")



anova_gender <- aov(User.Behavior.Class ~ Gender, data = data)
p_value_gender <- summary(anova_gender)[[1]]$`Pr(>F)`[1]
cat("Anova p-value for Gender:", round(p_value_gender, 4), "\n")

anova_device <- aov(User.Behavior.Class ~ Device.Model, data = data)
p_value_device <- summary(anova_device)[[1]]$`Pr(>F)`[1]
cat("Anova p-value for Device Model:", round(p_value_device, 4), "\n")

anova_os <- aov(User.Behavior.Class ~ Operating.System, data = data)
p_value_os <- summary(anova_os)[[1]]$`Pr(>F)`[1]
cat("Anova p-value for Operating System :", round(p_value_os, 4), "\n")



kendall_age <- cor.test(data$Age, data$Number.of.Apps.Installed, method = "kendall")
cat("Kendall correlation for Age (r): ", kendall_age$estimate, "\n")
cat("p-value for Age: ", kendall_age$p.value, "\n")

anova_os <- aov(Age ~ Operating.System, data = data)
p_value_os <- summary(anova_os)[[1]]$`Pr(>F)`[1]
cat("Anova p-value for Operating System :", round(p_value_os, 4), "\n")

