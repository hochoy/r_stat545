#Homework 4: Building your own function

# I obtained a lot of my lm(), linear regression, code here http://www.statmethods.net/stats/regression.html

suppressPackageStartupMessages(library(dplyr))
library(plyr)
library(gapminder)
library(ggplot2)
library(broom)

gap_df <- tbl_df(gapminder)
gap_df %>% glimpse()

# I would like to create a linear function that allows me to view the gdpPercap vs year regression line for each country in a continent
# In other words, to define for each country,the formula: GdpPercap = B*year + intercept + error

# A world overview of gdpPercap versus year shows that while gdpPercap tends to increase in all continents, 
# the trend for each continent is not the same. This suggests that I cannot use 1 formula to fit all continents.

world_gdp_year_plot_1 <- gap_df %>% ggplot(aes(x=lifeExp, y= gdpPercap)) +
  geom_point() +
  geom_smooth(method = loess) +
  facet_grid(continent ~ .)
world_gdp_year_plot_1;

# Instead, I will calculate a linear function (line with slope and intercept) for each continent
# Let's start with a single continent, Africa before making a function that can handle any country in all continents.
# In this formula, I have transformed the gdpPercap values using log() to better visualize the data

fit_line_reg <- function(data) {
  fit <- lm(log(gdpPercap) ~ year, data)
  setNames(data.frame(t(coef(fit))), c("intercept", "slope"))
}
gap_df_Africa <- gap_df %>% filter(continent == "Africa")   # I select only the Africa data
Africa_line_coeff <- fit_line_reg(gap_df_Africa)

# Using the slope and intercept, I can now plot the data
Africa_gdp_year_plot1 <- ggplot(data = gap_df_Africa,aes(x=year, y=log(gdpPercap), color = country)) +
  geom_point() +
  geom_abline(aes(intercept=Africa_line_coeff$intercept, slope =Africa_line_coeff$slope))
Africa_gdp_year_plot1;

# Hmm, based on this plot, there appears to be a lot of outliers that don't fit the line of best fit.
# These outliers also appear to belong to certain countries, in which the majority show greater and more sudden increases in gdpPercap
# To determine which countries have trends that differ the most from the average trend, we can run linear regression
# on each country in a particular continent and compare their coefficients to the average trend. To do that,
# we can use anova to compare the fit of one country with the average fit. Let us use Zimbabwe as an example.
# Similarly, I have log-transformed the gdpPercap of Zimbabwe for visualization

gap_df_Zimbabwe <- gap_df_Africa %>% filter(country == "Zimbabwe")
Zimbabwe_line_coeff <- fit_line_reg(gap_df_Zimbabwe)

# As with the continent, I will also plot the country's gdpPercap vs year
Zimbabwe_gdp_year_plot1 <- ggplot(data = gap_df_Zimbabwe,aes(x=year, y=log(gdpPercap), color = country)) +
  geom_point() +
  geom_abline(aes(intercept=Zimbabwe_line_coeff$intercept, slope =Zimbabwe_line_coeff$slope))
Zimbabwe_gdp_year_plot1;

# As we can see, Zimbabwe's gdpPercap vs year clearly does not follow a simple linear function. Instead, 
# it appears to follow a polynomial function (reverse-U-shaped). In a later part, we will try using polynomial 
# regression to fit Zimbabwe's data. But for now, I will first create a function to look through the linear regression
# of all countries. Moving on, to compare the fit in Africa vs Zimbabwe, we can visually compare the trendline of Zimbabwe
# vs the average trend of Africa. There are more statistical ways to compare trendlines (i.e. ANOVA), however, I hadn't had the time
# to read up about them. In this homework, I will only compare the slopes as I assume each country will begin at a unique intercept

Zimbabwe_vs_Africa_trend <- Africa_gdp_year_plot1 +
  geom_abline(aes(intercept=Zimbabwe_line_coeff$intercept, slope =Zimbabwe_line_coeff$slope),color = "red")
Zimbabwe_vs_Africa_trend;


GDPpercap_vs_year_of_continent <- function(cont = "Africa", 
                                           data = gapminder,
                                           show_neg = FALSE,
                                           envir = environment()) {
  gap_df <- tbl_df(data)
  
  # continent linear regression
  gap_continent <- gap_df %>% filter(continent == cont)     # filter for a particular continent
  gdp_year_coeff_continent <- gap_continent %>%         #fit a linear model with gdpPercap as y-axis
    lm(log(gdpPercap) ~ year,data=.) %>%                # and year as x-axis
    coefficients()                                      # extract the coefficients
  gap_continent2 <- gap_continent %>% mutate(intercept = gdp_year_coeff_continent[[1]], 
                                             slope = gdp_year_coeff_continent[[2]])
  
  # country linear regression
  fit_line_reg <- function(data) {                # custom function to calculate linear model for
    fit <- lm(log(gdpPercap) ~ year, data)       # a particular set of gdpPercap and year observation
    setNames(data.frame(t(coef(fit))), c("intercept", "slope"))    # rename the coefficients 
  }
  
  
  country_coef <- gapminder %>% tbl_df() %>%     # using group_by(), we group the data by country
    filter(continent == cont) %>% 
    group_by(country) %>%                       # then, we apply our custom function to each country
    do(fit_line_reg(.))                     # using do()
  
  if(show_neg == TRUE) {
    country_coef <- country_coef %>% filter(slope < 0)
  }
  # plot overlay of country linear model(red) over continent linear model (black) and dotplot
  gdp_year_overlay_plot <- ggplot(data = gap_continent2,aes(x=year, y=log(gdpPercap), color = country)) +
    geom_point() +                        # draw dotplot of gdpPercap for all countries for all years 
    geom_abline(aes(intercept=intercept,  # draw line of the continent's linear model
                    slope = slope),
                color = "black") +
    geom_abline(aes(intercept= country_coef$intercept, # draw line of each country's linear model
                    slope = country_coef$slope),
                color = "red")
  gdp_year_overlay_plot$plot_env <- envir     # allows any calls to variables within ggplot to include variables in the GDPpercap_vs_year_ofcontinent()'s environment
  
  list(gdp_year_overlay_plot,country_coef);   # draw both the plot and the table of coefficients
}





###
GDPpercap_vs_year_of_continent()
GDPpercap_vs_year_of_continent("Africa")
GDPpercap_vs_year_of_continent("Africa",show_neg = TRUE)
GDPpercap_vs_year_of_continent("Oceania")
GDPpercap_vs_year_of_continent("Asia")
GDPpercap_vs_year_of_continent("Americas")
GDPpercap_vs_year_of_continent("Europe")

