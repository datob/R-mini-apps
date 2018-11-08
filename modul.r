library(ggplot2)
library(dplyr)

beaver <- rbind(data.frame(beaver1, id = "beaver1"), 
                data.frame(beaver2, id = "beaver2"))       


#ggplot(beaver,  aes(x = time, y = temp,  colour = id)) + geom_point() + facet_grid(activ ~ .)

#qplot(temp, data = beaver,geom = "histogram", color=id) + facet_grid(activ ~ .)
#qplot(mean(beaver$temp), data = beaver,geom = "histogram", color=id) + facet_grid(activ ~ .)

#summary(beaver)