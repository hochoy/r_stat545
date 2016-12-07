# Complete code

suppressPackageStartupMessages(library(dplyr))
library(plyr)
library(gapminder)
library(ggplot2)
library(broom)

cont = "Asia"
show_neg = TRUE
show_country = "Algeria"
envir = environment()

  gap_df <- tbl_df(gapminder)
  
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
    country_coef2 <- country_coef %>% filter(slope < 0)
  }
  if (country_coef3 == ""){
    country_coef3 <- country_coef2
  }
  
  
  # plot overlay of country linear model(red) over continent linear model (black) and dotplot
  gdp_year_overlay_plot <- ggplot(data = gap_continent2,aes(x=year, y=log(gdpPercap), color = country)) +
    geom_point() +                        # draw dotplot of gdpPercap for all countries for all years 
    geom_abline(aes(intercept=intercept,  # draw line of the continent's linear model
                    slope = slope),
                color = "black") +
    geom_abline(aes(intercept= intercept, # draw line of each country's linear model
                    slope = slope),
                data = country_coef2,
                color = "red")
  gdp_year_overlay_plot$plot_env <- envir     # allows any calls to variables within ggplot to include variables in the GDPpercap_vs_year_ofcontinent()'s environment
  gdp_year_overlay_plot
  
  list(gdp_year_overlay_plot,country_coef);   # draw both the plot and the table of coefficients

  gap_df
  gap_df  %>% filter(country == c("Iraq","Afghanistan"))
  c4 <- country_coef2 %>% ungroup()
  c4
  c4 %>% filter(country %in% c("Iraq","Afghanistan"))




