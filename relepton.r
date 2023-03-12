library(tidyverse)

data_import <- read.csv("./data.csv") 
data <- within(data_import, Date <- as.Date(Date))
names(data) <- c("Monitoring_Site", "Date", "Temperature_C")
criteria <- 21 # C°

# Make LOEs table
loes_total <- as.data.frame(table(data$Monitoring_Site))
names(loes_total) <-  c("Monitoring_Site", "Total")
loes_exceedances <- aggregate(Temperature_C ~ Monitoring_Site, 
                              data = data, 
                              FUN = \(x) sum(x >= criteria))
names(loes_exceedances) <- c("Monitoring_Site", "Exceedances")
loes <- merge(loes_exceedances, loes_total)
# write.csv(loes, "loes.csv")

# Make trends analysis graph
graph <- 
    ggplot(data, aes(x = Date, 
                     y = Temperature_C, 
                     color = Monitoring_Site)) +
    geom_point() +
    geom_smooth(method = "glm", se = FALSE) +
    ggtitle("Temperature of Lower Salinas River", 
            subtitle = "2000–2021") +
    theme(text = element_text(size = 16))
# png("graph.png", width = 640)
# graph
# dev.off()
