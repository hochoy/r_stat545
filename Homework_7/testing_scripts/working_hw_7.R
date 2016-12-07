# First, we load the libraries

library(readr)
suppressPackageStartupMessages(library(dplyr))
library(tidyr)
library(stringr)
library(ggplot2)
library(dplyr)
library(gridExtra)

# Then, we read in the candy data
work_filepath <- "git_world/wai-ho_choy/Homework_7/lecture_data/CANDY-HIERARCHY-2015 SURVEY-Responses.csv"
home_filepath <- "git_world/wai-ho_choy/Homework_7/lecture_data/CANDY-HIERARCHY-2015 SURVEY-Responses.csv"

if (file.exists(work_filepath)) {
  candy_path <- work_filepath;
} else candy_path <- home_filepath;
raw <- read_csv(candy_path,col_types = cols(Timestamp = col_datetime("%m/%d/%Y %H:%M:%S")))

# We don't yet have a way to idnetify each entry so let's create an ID variable. 

raw_with_id <- raw %>%                                # sprintf() returns a character vector of strings when given a format
  mutate(id = sprintf("ID%04d", row_number()))        # from %04 means include up to 4 zeroes as padding if the digit is not 4 digits
                                                      # e.g. ID4 -> ID0004
raw_with_id2 <- raw_with_id %>%                       # We want to sort the columns a little so that we have the Timestamp, id, age,
  select(Timestamp,                                   # trick_or_treat variables in front. They seem more useful although I might not use
         id,                                          # all of them
         age = starts_with("How"),                    # We also rename age, trick_or_treat because they are too long and unwieldy
         trick_treat = starts_with("Are you going"),
         everything())
raw_with_id2 %>% glimpse()

# As we can see in raw_with_id2, there is hella lot to be cleaned. To save time, I know I will be working with age and candy variables so
# I will only clean those variables.

# For age, the column names are okay but we want to double-check to see if age is reasonable (5-100) and in the correct format(a number)
# To remove all non-number entries, I use as.integer() to coerce the character strings into integers. If the entries are numbers in character
# form such as "1", "15", 124.12", they will be co-erced into proper integers. However, if the entries are words or gibberish such as "one",
# "potato", "4$Y)()", then it should return NAs which I will filter out
as.integer(c("1","15","124.12")) 
as.integer(c("one","potato","4$Y)("))

raw_clean_age <- raw_with_id2 %>% mutate(age = as.integer(age))
raw_clean_age <- raw_clean_age %>% filter(!is.na(age)) %>% filter(age < 100) %>% filter(age > 5)
raw_clean_age %>% ggplot(aes(x=age)) + geom_histogram(stat="bin")

# For candy names, I used Jenny's regex code to extract only the candy names which had a surrounding [ ] bracket

candy_oldnew_names <-
  data_frame(orig_name = names(raw_clean_age)) %>%
  mutate(is_candy = str_detect(orig_name,"^\\["),
         new_name = str_replace_all(orig_name, "(^\\[)(.*)(\\]$)", "\\2"),
         new_name = str_replace_all(new_name, '["’]', "'"),
         is_changed = orig_name != new_name)
candy_oldnew_names
sum(candy_oldnew_names$is_candy)      # It looks like there are 95 candies in the list of column names
names(raw_clean_age) <- candy_oldnew_names$new_name     # replace the current column names with new column names for candies
raw_clean_age

## Now for the analysis. I only want to use id, age, trick_or_treat and the candies. So I will select for those.
# Selecting id,age,trick_or_treat is a breeze. However, selecting only the candies in the next 100+ columns is difficult. Luckily, we already have a list of candy names to use from before

candy_names <- candy_oldnew_names %>% filter(is_candy) %>% .[["new_name"]];
candy_ready <- raw_clean_age %>% select(one_of(c("id",
                                                 "age",
                                                 "trick_treat",
                                                 candy_names)))
candy_ready

# Instead of having the candy variables separate, gathering them into 1 column would allow us to use more dplyr. I also want to select 
# the top ten most popular candies. 

candy_gathered <- gather(candy_ready,"candynames","review",4:length(candy_ready))
candy_gathered
candy_counts <- candy_gathered %>% group_by(candynames,review) %>% summarize(count = n())  

#Lets calculate the popularity of the candies by dividing the number of Joy responses by the total response
candy_counts
joy_df <- candy_counts %>% ungroup() %>% filter(review == "JOY");names(joy_df)[3] <- "joy_count"
despair_df <- candy_counts %>% filter(review == "DESPAIR");names(despair_df)[3] <- "despair_count"
na_df <- candy_counts %>% filter(is.na(review)); names(na_df)[3] <- "na_count"

candy_counts_split <- cbind(subset(joy_df,select = -c(review)),
                            despair_count = despair_df$despair_count,
                            na_count = na_df$na_count)
candy_counts_popular <- candy_counts_split %>% mutate(popularity = 100 * joy_count/(joy_count+despair_count+na_count))
candy_counts_popular

# Lets add additional info (candy type & manufacturer) to our candies. I did this manually using google search and excel as it was only 95 variables. Not practical for larger datasets
candy_manufacture_type <- read_csv("git_world/wai-ho_choy/Homework_7/candy-manu-type.csv")
candy_counts_complete <- merge(candy_manufacture_type,candy_counts_popular,by.x="candynames")
candy_counts_complete %>% head()
candy_counts_top_20 <- candy_counts_complete %>% arrange(-popularity) %>% head(20)
plot_candy <- function(df,
                       filler = "type",
                       facet_split = "type",
                       envir=environment()) {
  plot1 <- df %>% 
    mutate(candynames = reorder(candynames,-popularity)) %>%  
    ggplot(aes_string(x="candynames", y = "popularity", fill = filler)) + 
    geom_bar(stat="identity") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=0.5,size = 10, color = "black")) +
    facet_grid(as.formula(paste(facet_split," ~ .")))
  plot1$plot_env <- envir
  plot1
}
plot_candy(candy_counts_top_20)
plot_candy(candy_counts_top_20,filler = "manufacturer")
manufacture_popularity_top_20 <- candy_counts_top_20 %>% group_by(manufacturer) %>% summarize(manufacturer_popularity = n()) %>% arrange(-manufacturer_popularity)
manufacture_popularity_top_20
# From this plot, we can see that candies in the chocolate category are much more popular than candies. We also see that 'cash' is 
# included in this dataset to mess up the assumption that this list is all candies. Using filter(type == danger), I can remove these
# nonsensical candy types. By setting the fill to manufacturer, we can also see that there are 8 manufacturers on this list, of which Mars
# is the most popular manufacturer in this list. Is this true however for the full list?
manufacture_popularity_all_top_10 <- candy_counts_complete %>% 
  group_by(manufacturer) %>% 
  summarize(mean_popularity = mean(popularity)) %>% 
  arrange(-mean_popularity) %>% 
  mutate(manufacturer = reorder(manufacturer,-mean_popularity)) %>% head(10)
manufacture_popularity_all_top_10
manufacturer_plot_overall <- manufacture_popularity_all_top_10 %>% 
  ggplot(aes(x=manufacturer,y=mean_popularity, fill = manufacturer)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=0.5,size = 10, color = "black"))
manufacturer_plot_overall  
grid.arrange(tableGrob(candy_gathered %>% head(10)),
             tableGrob(candy_counts %>% head(10)))
