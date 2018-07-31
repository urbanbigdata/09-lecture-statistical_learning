
################################
# VSP Big Data Group Session 8
# Author: Andy Hong
# Date: July 30, 2018
################################

# Install packages
#install.packages("leaflet")
#install.packages("dplyr")
#install.packages("magrittr")
#install.packages("ggplot2")
#install.packages("plotly")

# Load packages
library(leaflet)
library(dplyr)
library(magrittr)
library(ggplot2)
library(plotly)

# Paths
# gapminder = read.csv(file.choose())
# geo = read.csv(file.choose())

# Paths
gapminder = read.csv("/Users/andyhong/Documents/vsp_bigdata/group_session/08-lecture/gapminder_data_2016.csv")
geo = read.csv("/Users/andyhong/Documents/vsp_bigdata/group_session/08-lecture/gapminder_geo.csv")

# Join gapminder and geographic coordinates
gapminder = gapminder %>% inner_join(geo, by="name")


# 1. Data exploration ###################
head(gapminder)
head(geo)

#View(gapminder)
#View(geo)

str(gapminder)
str(geo)

summary(gapminder)
summary(geo)

# Turn off scientific notations
options(scipen=99)




# 2. Data joining ###################


head(gapminder)
str(gapminder)

ggplot(gapminder, aes(x=income, y=lifeExp)) + geom_point()


model1 = lm(data=gapminder, lifeExp ~ income)

summary(model1)


gapminder$predicted <- predict(model1)   # Save the predicted values
gapminder$residuals <- residuals(model1) # Save the residual values

ggplot(gapminder, aes(x = income, y = lifeExp)) +
  geom_smooth(method = "lm", se = FALSE, color = "white", alpha=1) +
  # geom_segment(aes(xend = income, yend = predicted), alpha = .2) +
  
  # > Color AND size adjustments made here...
  geom_point(aes(color = abs(residuals)), size = 3, alpha=0.6) + # size also mapped
  # scale_color_continuous(low = "black", high = "red") +
  guides(color = FALSE, size = FALSE) +  # Size legend also removed
  # geom_point(aes(y = predicted), shape = 1) +
  theme_bw()

ggplot(gapminder, aes(x = income, y = lifeExp)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_segment(aes(xend = income, yend = predicted), alpha = .2, color="red") +
  
  # > Color AND size adjustments made here...
  geom_point(aes(color = abs(residuals)), size = 3, alpha=0.6) + # size also mapped
  # scale_color_continuous(low = "black", high = "red") +
  guides(color = FALSE, size = FALSE) +  # Size legend also removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()


library(plotly)
library(reshape2)
data(iris)

#load data

head(gapminder)

gapminder$inc = gapminder$income
gapminder$pop = gapminder$population

my_df <- gapminder
fit <- lm(lifeExp ~ 0 + inc + pop, data = my_df)

#Graph Resolution (more important for more complex shapes)
# graph_reso <- 0.05
graph_reso <- 10

#Setup Axis
axis_x <- seq(min(my_df$inc), max(my_df$inc), by = 10000)
axis_y <- seq(min(my_df$pop), max(my_df$pop), by = 100000)

#Sample points
fit_surface <- expand.grid(inc = axis_x, pop = axis_y, KEEP.OUT.ATTRS = F)
fit_surface$lifeExp <- predict.lm(fit, newdata = fit_surface)
fit_surface <- acast(fit_surface, pop ~ inc, value.var = "lifeExp") #y ~ x

hcolors=c("red","blue","green","purple")[my_df$region]


head(my_df)

gapminder_plot <- plot_ly(my_df, 
                          x = ~inc, 
                          y = ~pop, 
                          z = ~lifeExp,
                          text = ~region,
                          type = "scatter3d", 
                          mode = "markers",
                          marker = list(color = hcolors))

gapminder_plot <- add_trace(p = gapminder_plot,
                            z = fit_surface,
                            x = axis_x,
                            y = axis_y,
                            type = "surface")

gapminder_plot



plot_ly(data = gapminder, z = ~lifeExp, x = ~income, y = ~population, color = ~region, colors = hcolors ,opacity = 0.5) %>%
  add_markers( marker = list(size = 2)) 



# fit model
m <- lm(lifeExp ~ income + population, data = gapminder)

# predict over sensible grid of values
income <- unique(gapminder$income)
population <- unique(gapminder$population)
grid <- with(gapminder, expand.grid(income, population))
d <- setNames(data.frame(grid), c("income", "population"))
vals <- predict(m, newdata = d)

# form matrix and give to plotly
m <- matrix(vals, nrow = length(unique(d$income)), ncol = length(unique(d$population)))

library(plotly)
plot_ly() %>% add_surface(x = ~population, y = ~income, z = ~m)


plot_ly() %>% add_surface(x = ~disps, y = ~wts, z = ~m, colors = c("#d1d1d1", "#000000"))


oneplane <- expand.grid(x = 1:6, y = 1:6)
oneplane$z <- 1:6
oneplane.m <- as.matrix(spread(oneplane, key = x, value = z)[, -1])
plot_ly() %>% add_trace(x = 1:6, 
                        y = 1:6, 
                        z = oneplane.m, 
                        type = "surface", 
                        opacity = .5,
                        cauto = FALSE,
                        cmax = 1,
                        cmin = 0,
                        colorscale = list(c(0,'#d1d1d1'),c(1,'#000000')))