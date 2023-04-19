library(tidyverse)
library(DataExplorer)

data <- read.csv("C:/Users/noord/Desktop/Parameter_optimization_SVM/letter-recognition.data")
colnames(data)<-c("letter","x_box","y_box","width","high","onpix","x_bar","y_bar","x2bar","y2bar","xybar","x2ybr","xy2br","x_ege","xegvy","y_ege","yegvx")

#Analysing Dataset
summary(data)
DataExplorer::create_report(data)


# Scatter plot of x-box vs. y-box
ggplot(data, aes(x = x_box, y = y_box)) +
  geom_point(color = "black", stroke = 1, size = 2) +
  labs(title = "Scatter Plot of x-box vs. y-box",
       x = "x-box",
       y = "y-box") +
  theme(plot.background = element_rect(fill = "black"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"))

# Box Plot of x_box by letters
ggplot(data, aes(x = letter, y = x_box)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Box Plot of x_box by letters",
       x = "letters",
       y = "x_box") +
  theme(plot.background = element_rect(fill = "lightgray"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"))


