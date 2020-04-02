library(mgcv)
library(ggplot2)
library(dplyr)

# Fake Data
distance_along <- seq(0,1020, by = 30)
elev1 <- c(c(85,85, 85, seq(85,60, by = -5))*rnorm(9, 1, 0.02),55)
elev2 <- c(55,seq(60, 80, by = 2.5)*rnorm(9, 1, 0.02))
miss <- rep(NA,15) 
mydf <- data.frame(distance_along, elev = c(elev1, miss, elev2))

# Fit model and predict depths in missing space
elev_predict_df <- filter(mydf, is.na(elev))
mygam <- mgcv::gam(elev ~ s(distance_along, bs = "cs"), data = mydf)
predictions <- predict(mygam, newdata = elev_predict_df)
predicted_df <- mutate(elev_predict_df, predictions)

# Plot it
ggplot(mydf, aes(distance_along, elev)) +
  geom_point() +
  scale_y_continuous(limits = c(0,100)) +
  geom_smooth(method = "gam") +
  geom_point(data = predicted_df, aes(x = distance_along, y = predictions), 
             color = "red")
