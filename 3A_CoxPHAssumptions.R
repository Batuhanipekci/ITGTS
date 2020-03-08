library(survival)
library(mlr)
library(rms)
library(survminer)
data = read.csv("ultimate_data/surv_data_season.csv")

data = data[,-c(1,31)]
set.seed(123)

survival_target <- c("weeks_on_market","status")
features_surv <- names(data)[!(names(data) %in% c(survival_target,"object_type_ordinal") )]

surv_form = as.formula(paste("Surv(weeks_on_market, status) ~", paste(features_surv,collapse ="+")))
cox_model <- coxph(surv_form, data = data, x = TRUE)
summary(cox_model)


# Linearity Assumption
dev.off()
p1[[1]]
p2[[1]]
p3[[1]]
p4[[1]]
p5[[1]]
p6[[1]]
p7[[1]]
p8[[1]]
p9[[1]]
p10[[1]]
p11[[1]]
p12[[1]]



require(gridExtra)

grid.arrange( p1[[1]],  p2[[1]],
              p3[[1]],  p4[[1]],
              p5[[1]],  p6[[1]],
              p7[[1]],  p8[[1]],
              p9[[1]],  p10[[1]],
              p11[[1]],  p12[[1]],
              nrow = 3)


plot_list = list()
plot_list[[1]]=ggcoxfunctional(Surv(weeks_on_market, status) ~ DOP, data = data)
plot_list[[2]]=ggcoxfunctional(Surv(weeks_on_market, status) ~ expected_price_sqm, data = data, xlab = "Expected Price per square meter")
plot_list[[3]]=ggcoxfunctional(Surv(weeks_on_market, status) ~ address_lat, data = data, xlab = "Latitude")
plot_list[[4]]=ggcoxfunctional(Surv(weeks_on_market, status) ~ address_lng, data = data, xlab = "Longitude")
plot_list[[5]]=ggcoxfunctional(Surv(weeks_on_market, status) ~ structure_rooms_living, data = data, xlab = "Number of Rooms")
plot_list[[6]]=ggcoxfunctional(Surv(weeks_on_market, status) ~ structure_area_living, data = data, xlab = "Living Area")
plot_list[[7]]=ggcoxfunctional(Surv(weeks_on_market, status) ~ construction_yearfinished, data = data,xlab = "Construction Year")
plot_list[[8]]=ggcoxfunctional(Surv(weeks_on_market, status) ~ zip_market_size, data = data,xlab = "Market Size (Zip Code)")
plot_list[[9]]=ggcoxfunctional(Surv(weeks_on_market, status) ~ city_distance, data = data,xlab = "Distance to the City Centroid")
plot_list[[10]]=ggcoxfunctional(Surv(weeks_on_market, status) ~ time_online, data = data,xlab = "Time of First Advertisement")
plot_list[[11]]=ggcoxfunctional(Surv(weeks_on_market, status) ~ zip_population_density, data = data,xlab= "Population Density (Zip Code)")
datalist = list()

export_list = list()
for(i in 1:length(plot_list)){
  print(i)
  dat =plot_list[[i]][[1]]$data
  names(dat) = paste(names(plot_list[[i]]),names(dat),sep = "_")
  export_list[[i]] = dat
  
}
export_df = do.call(cbind, export_list)


library(feather)
path <- "cox_linearity_new.feather"
write_feather(export_df,path)

# Proportional Hazards

test.ph <- cox.zph(cox_model)
ph <- as.data.frame(test.ph$table)
ph$stars = ifelse(ph$p<=0.001, "***",
                  ifelse(ph$p>0.001 & ph$p<=0.01, "**",
                         ifelse(ph$p>0.01 & ph$p<=0.05, "*"," ")))

#ggcoxzph(test.ph)






