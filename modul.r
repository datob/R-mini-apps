library(ggplot2)
library(dplyr)


#1 Задание 
beaver <- rbind(data.frame(beaver1, id = "beaver1"), 
                data.frame(beaver2, id = "beaver2"))       
#а)Температура тела от времени для 2 груп(активный и не активный)
ggplot(beaver1,  aes(x = time, y = temp, group=activ, colour =activ)) + geom_line() 
#б)Гистограмма температуры
qplot(temp, data = beaver1,geom = "histogram",group=activ, color=activ)
#с)Среднее температура
beaver1 %>% group_by(activ) %>% summarise(avg=mean(temp)) %>% ggplot(aes(x=activ,y=avg)) + geom_point()mtcars
#d)Яшики с усамы 
beaver1$activ<-ifelse(beaver1$activ==1,"Active","Dormant")
qplot(data=beaver1,x=activ,y=temp,geom="boxplot", colour=activ)+
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
mtcars %>% group_by(am) %>% summarise(avg=mean(mpg)) %>% ggplot(aes(x=am,y=avg)) + geom_bar(stat="identity", fill=c("green","red"))
#d) Correl Matrix
M <- cor(mtcars) # get correlations
library('corrplot') #package corrplot
corrplot(M, method = "circle") #plot matrix


#3 Задание 
#price/carat
ggplot(diamonds, aes(carat,price,color=cut)) +
  geom_point() +
  geom_smooth()




