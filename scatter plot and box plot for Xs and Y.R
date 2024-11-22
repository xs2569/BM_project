data <- read.csv("Project_2_data.csv")


### scatter plot for Y and numeric variable
library(ggplot2)
library(gridExtra)
numerical_vars <- c("Age","Tumor.Size","Regional.Node.Examined","Reginol.Node.Positive")
plot_list1 <- list()
for (var in numerical_vars) {
  p<- ggplot(data, aes_string(x = var, y = "Survival.Months")) + 
    geom_point(color = "blue", alpha = 0.6) +
    geom_smooth(method = "lm", color = "red", se = TRUE) +
    labs(title = paste (var, "vs Survival Months"), x = var, y = "Survival Months")+    theme_minimal() 
  plot_list1[[var]] <- p
}
grid.arrange(grobs = plot_list1, ncol = 2)

### box plot for Y and categorical variable
categorical_vars <- c("Race","Marital.Status","T.Stage","N.Stage","X6th.Stage","differentiate","Grade","A.Stage","Estrogen.Status", "Progesterone.Status")
plot_list2 <- list()
for (var in categorical_vars) {
  m<- ggplot(data, aes_string(x = var, y = "Survival.Months")) +
    geom_boxplot(fill = "lightblue", color = "darkblue") +
    labs(title = paste (var, "vs Survival Months"), x = var, y = "Survival Months") + 
    theme_minimal()
  plot_list2[[var]] <- m
}
grid.arrange(grobs= plot_list2, ncol = 5)


