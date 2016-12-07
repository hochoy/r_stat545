# Load in libraries
library(gapminder)
library(dplyr)
library(ggplot2)

# Have a look at gapminder dataset
head(gapminder)
str(gapminder)

# Re-form the dataframe as a table_dataframe(an upgrade of a dataframe that does what df does and more)
gtbl <- tbl_df(gapminder) 
gtbl      # as we can see, the column headers include the class of the variable for each column
glimpse(gtbl)  # the dplyr version of str()

# Task 1 of 3: Get maximum and mininum of GDP per capita for all continents
maxminGDP <- gtbl %>%    # table
  group_by(continent,year) %>% 
  summarize(max = max(gdpPercap),
            min = min(gdpPercap),
            mean = mean(gdpPercap))
maxminGDP;

maxminGDP %>% 
  ggplot(aes(x = year)) + 
  geom_line(aes(y=max, color = continent)) +
  geom_line(aes(y=min, color = continent)) + 
  facet_grid(continent ~ .)
  
  
# Task 2 of 3: Look at the spread of GDP per capita within continents
gdpPercap_within_continent_2007 <- gtbl %>% 
  filter(year == "2007") %>% 
  select(continent, country,gdpPercap) %>% 
  arrange(continent, gdpPercap)
   
gdpPercap_within_continent_2007;  summary(gdpPercap_within_continent_2007)
  
# spread by continent
gdpPercap_within_continent_2007 %>% 
  ggplot(aes(x=log10(gdpPercap), color = continent)) +
  geom_histogram(fill="white", binwidth = 0.1,alpha = 0.5, position = "dodge") +
  facet_grid(continent ~ .) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=0)) +
  scale_x_discrete(breaks = c(1:5))

# Task 3 of 3: Compute a weighted mean for life expectancy (mean(lifeEx/pop)
weighted_lifeExp_dat <- gtbl %>% 
  group_by(continent) %>% 
  summarize(weighted_lifeExp = mean(lifeExp/pop))
weighted_lifeExp_dat;

weight_lifeExp_plot <- weighted_lifeExp_dat %>% 
  ggplot(aes(x=continent)) +
  geom_bar(aes(y=weighted_lifeExp), stat = "identity")
weight_lifeExp_plot;

# Task 4 of 3: How is life expectancy changing over time on different continents
meanlifeExp_peryear_dat <- gtbl %>% 
  group_by(continent,year) %>% 
  summarize(meanlifeExp_peryear = mean(lifeExp))
meanlifeExp_peryear_dat;

meanlifeExp_peryear_plot <- meanlifeExp_peryear_dat %>% 
  ggplot(aes(x = year, y=meanlifeExp_peryear, color=continent)) +
  geom_line()
meanlifeExp_peryear_plot;

# Task 5 of 3: Report the absolute and relative abundance of countries with low life expectancy over time by continent
# low life expectancy = less than mean
world_lifeExpmean <- gtbl %>%  
  select(year, lifeExp) %>% 
  group_by(year) %>% 
  summarize(world_lifeExp = mean(lifeExp))
world_lifeExpmean;

gtbl_showlifeExp1 <- mutate(gtbl,world_lifeExp = rep(world_lifeExpmean$world_lifeExp,142))  #rep(x,times) is used to repeat 
      # the vector world_lifeExp 142 times to match the number of rows in the gapminder dataset (1704 rows). 12 * 142 = 1704
gtbl_showlifeExp1

gtbl_showlifeexp2 <- gtbl_showlifeExp1 %>% 
  mutate(islowlife = (lifeExp < world_lifeExp))
gtbl_showlifeexp2;

ratio_life_exp <- gtbl_showlifeexp2 %>% 
  group_by(continent,year) %>% 
  summarize(n_countries_lowlife = sum(islowlife),
            n_countries_total = n(),
            ratio_lowlife_total = 100* n_countries_lowlife/n_countries_total)
ratio_life_exp;

ratio_life_exp %>% 
  ggplot(aes(x = year, color = continent, y=ratio_lowlife_total)) +
  geom_line() +
  labs(title = "Relative abundance of countries with 
       low life expectancy over time by continent", 
       x = "Year", 
       y = "Percent of countries within 
       continent with low life expectancy (%) ")

#Task 6 of 3: Compare the average GDPpercap of high life vs low life expectancy countries 
#GDP_perCap_vs_lifeExp 
1.
GDP_percap_highlow<- gtbl %>%  
  mutate(world_lifeExpmean = rep(world_lifeExpmean$world_lifeExp,142),
         islowlife = lifeExp < world_lifeExpmean) %>% 
  group_by(continent,year,islowlife) %>% 
  summarize(total_GDP_percap = sum(gdpPercap),
            total_countries = n())
GDP_percap_highlow;

GDP_percap_highlow %>% 
  ggplot(aes(x=year, color = islowlife, y = total_GDP_percap)) +
  geom_line() +
  scale_colour_manual(values = c("green","red"),
                      name= "Life Expectancy",
                      labels = c("High", "Low")) +
  facet_grid(continent ~.)
  

