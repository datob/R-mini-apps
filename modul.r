library(ggplot2)
library(dplyr)


#1 Задание 
beaver <- rbind(data.frame(beaver1, id = "beaver1"), 
                data.frame(beaver2, id = "beaver2"))       
#а)Температура тела от времени для 2 груп(активный и не активный)
ggplot(beaver,  aes(x = time, y = temp,  colour = id)) + geom_point() + facet_grid(activ ~ .)
#б)Гистограмма температуры
qplot(temp, data = beaver,geom = "histogram", color=activ , facets = activ ~ .)
#с)Среднее температура
avg <- (mean(beaver$temp))
qplot(avg, data = beaver,geom = "bar", color=activ,facets=activ~.) 
#d)Яшики с усамы 
beaver$activ<-ifelse(beaver$activ==1,"Active","Dormant")
qplot(data=beaver,x=activ,y=temp,geom="boxplot", colour=activ)+
   labs(title = "Boxplot of Temperatures",x = "Beaver State",y="Temperature",colour="State")




#2 Задание 
#a) mpg ~ disp
ggplot(mtcars) + geom_point(aes(mpg,disp), color = 'red') + theme_classic(base_size = 16)
#b) table(mtcars$cyl)
cil <- table(mtcars$cyl)
pie(cil, cex = 0.6, radius = 0.9, init.angle = -10,
    main = "Cylinders",
    col = c(2:8))
#c) automatic manual transsmision
cyl.am <- ggplot(mtcars, aes(x = factor(cyl), fill = factor(am)))
cyl.am + geom_bar(position = "stack")
cyl.am + geom_bar(position = "fill")
cyl.am + geom_bar(position = "dodge")
val = c("red", "green")
lab = c("Manual", "Automatic")
cyl.am + geom_bar(position = "dodge") + scale_x_discrete("Cylinders") + scale_y_continuous("Number") + scale_fill_manual("Transmission", values = val, labels = lab)
#d) Correl Matrix
d <- data.frame(x1=rnorm(10),
                 x2=rnorm(10),
                 x3=rnorm(10))
M <- cor(d) # get correlations
library('corrplot') #package corrplot
corrplot(M, method = "circle") #plot matrix


#3 Задание 
#price/carat
ggplot(diamonds, aes(price,carat,color=cut)) +
  geom_point() +
  geom_smooth()




