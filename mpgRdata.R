# load data
 library(readr)
 auto_mpg1 <- read_csv("Fuel Efficiency/auto-mpg1.csv")
View(auto_mpg1)
# view column names of the dataset
   names(auto_mpg1)
# view top 5 rows
 head(auto_mpg1)
 view last 5 rows
 tail(auto_mpg1)
library(tidyverse)
 library(caret)
 library(ggplot2)
 library(dplyr)
 # replace NA values by their mean grouped by mpg
   mpgdata_clean <- auto_mpg1 %>%  
       group_by(mpg) %>%
       mutate_at(vars(-c("mpg")),~ifelse(is.na(.), mean(., na.rm = TRUE), .))
# view cleaned data
View(mpgdata_clean)
 # Structure of mpgdata
   str(mpgdata_clean)
str(mpgdata_clean$horsepower)
# Convert Horsepower Column from Character to Integer Vector
horsepower_chr_values <- strsplit(mpgdata_clean$horsepower, split = " ", fixed = TRUE)
 horsepower_count <- length(horsepower_chr_values)
 new_horsepower_values <- vector()
 
   for (i in 1:horsepower_count) {
         new_horsepower_values[i] <- strtoi(horsepower_chr_values[[i]][1])
     }

   mpgdata_clean$horsepower <- new_horsepower_values
str(mpgdata_clean$horsepower)
# Visualising mpgdata
 # Histogram
   hist(mpgdata_clean$weight, main = "Weight vs MPG", xlab = "weight", ylab = "mpg")
# Pie Chart
 cylinders_count = auto_mpg1 %>% count(cylinders)
 pie(cylinders_count$cylinders, labels = cylinders_count$cylinders, radius = 1, col = c("orange", "blue", "yellow", "dark green"), main = "Number of Cylinders")
 # Bar Plot
   barplot(table(mpgdata_clean$acceleration), main = "Acceleration vs MPG")
# Scatter Plot
   graph_data_1 <- mpgdata_clean %>%
       select(weight, mpg) %>%
       filter(weight < 5000, mpg < 400)

   ggplot(graph_data_1, aes(x = weight, y = mpg)) + geom_point() + geom_smooth() + labs(title = "Weight vs MPG", x = "weight", y = "mpg")
graph_data_2 <- mpgdata_clean %>%
           select(acceleration, mpg) %>%
           filter(acceleration < 500, mpg < 100)

        ggplot(graph_data_2, aes(x = acceleration, y = mpg)) + geom_point() + geom_smooth() + labs(title = "Acceleration vs MPG", x = "acceleration", y = "mpg")
graph_data_3 <- mpgdata_clean %>%
            select(horsepower, mpg) %>%
            filter(horsepower < 500, mpg < 100)
 
        ggplot(graph_data_3, aes(x = horsepower, y = mpg)) + geom_point() + geom_smooth() + labs(title = "Horsepower vs MPG", x = "horsepower", y = "mpg")
summary(mpgdata_clean)


# Perform the linear regression analysis
# Simple regression: mpg and weight
   simple.mpgdata1.lm <- lm(mpg ~ weight, data = mpgdata_clean)
View(simple.mpgdata1.lm)
 summary(simple.mpgdata1.lm)

# Simple regression: mpg and acceleration
 simple.mpgdata2.lm <- lm(mpg ~ acceleration, data = mpgdata_clean)
 View(simple.mpgdata2.lm)
 summary(simple.mpgdata2.lm)

# Simple regression: mpg and horsepower
 simple.mpgdata3.lm <- lm(mpg ~ horsepower, data = mpgdata_clean)
 View(simple.mpgdata3.lm)
 summary(simple.mpgdata3.lm)


# Check for homoscedasticity
 par(mfrow=c(2,2))
 plot(simple.mpgdata1.lm)
 par(mfrow=c(1,1))

par(mfrow=c(2,2))
  plot(simple.mpgdata2.lm)
  par(mfrow=c(1,1))

par(mfrow=c(2,2))
 plot(simple.mpgdata3.lm)
  par(mfrow=c(1,1))

# Multiple regression: mpg, horsepower, weight, and acceleration
 multiple.mpgdata1.lm<-lm(mpg ~ weight + acceleration, data = mpgdata_clean)

summary(multiple.mpgdata1.lm)

multiple.mpgdata2.lm<-lm(mpg ~ horsepower + acceleration, data = mpgdata_clean)
 
       summary(multiple.mpgdata2.lm)

multiple.mpgdata3.lm<-lm(horsepower ~ acceleration + mpg, data = mpgdata_clean)
 
        summary(multiple.mpgdata3.lm)


# Check for homoscedasticity
# Multiple regression
   par(mfrow=c(2,2))
plot(multiple.mpgdata1.lm)
 par(mfrow=c(1,1))

par(mfrow=c(2,2))
 plot(multiple.mpgdata2.lm)
 par(mfrow=c(1,1))

par(mfrow=c(2,2))
 plot(multiple.mpgdata3.lm)
 par(mfrow=c(1,1))

# Visualize the results with a graph
 # simple regression
   
   model1.graph<-ggplot(mpgdata_clean, aes(x=income, y=happiness))+
  +     geom_point()
model1.graph

View(model1.graph)
 model.graph<-ggplot(mpgdata_clean, aes(x=acceleration, y=mpg))+
            geom_point()
 View(model.graph)
 View(model.graph)
 plot(model.graph)

# Add the linear regression line to the plotted data
 
   model.graph <- model.graph + geom_smooth(method="lm", col="black")
plot(model.graph)


# Make the graph ready for publication
 model.graph +
       theme_bw() +
       labs(title = "Reported acceleration as a function of mpg",
                      x = "acceleration (x$100)",
                       y = "mpg (0 to 20)")

# Conclusions
 # In simple regression we can say that among all models we can say that there is a significant  positive relationship between mpg and acceleration (p value < 0.001), with a 1.191-unit  increase in mpg for every unit increase in acceleration. ///Comparing both the estimated, standard error, test stastic and p value for horsepower,acceleration and weight, we can conclude that acceleration has a # much better correlation with mpg.
   # So, if we have to use only one input to our model, we should definitely use acceleration.
  
  
  # In Multiple regression
   #
   # The estimated effect of weight on mpg is -0.007, while the estimated effect of acceleration is 0.250.
   
   #This means that for every 1% increase in weight to work, there is a correlated 0.007% decrease in the incidence of mpg. Meanwhile, for every 1% increase in acceleration, there is a 0.250% increase in the rate of mpg.