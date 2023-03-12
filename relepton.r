data_import <- read.csv("./data.csv") 
data <- within(data_import, date <- as.Date(date))
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
