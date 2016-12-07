#' ---
#' title: "Homework2"
#' author: "Wai-ho choy"
#' date: "September 23rd, 2015"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---

getwd()
setwd(dir = "git_world/wai-ho_choy/Homework_2/")

library(knitr)
library(ggplot2)
library(scales)


# IMPORTING DATA
      #1. read.table() throws an error that at least one of the lines does not have all 6 elements
      gapminder1 <- read.delim(file = "gapminderDataFiveYear.txt" )   # read.delim() works completely fine
      #gapminder2 <- read.table(file ="gapminderDataFiveYear.txt")   # read.table() throws an error
      gapminder2 <- read.table(file = "gapminderDataFiveYear.txt",fill = TRUE)   #fill adds blank values where elements are missing in a particular row
      
      #2. although adding fill allowed us to import the data, the read.table() dataset is broken. 
      identical(gapminder1,gapminder2)    #identical() confirms that the two datasets are not the same
      str(gapminder1); str(gapminder2)    #str() allows me to look at the summary of the data                     
      head(gapminder1); tail(gapminder1)  # head and tail view of both datasets are similar
      head(gapminder2); tail(gapminder2)  # perhaps the damage is in the middle of the data
      #View(gapminder2);                   # using View(), we can manually look through the data as a spreadsheet
      
      #3. Looking at the summary using str() shows that:
      #   A. there are more variables in the read.table() dataset
      #   B. the read.table()dataset lacks column headers and proper classes 
      #   C. from experience, it is likely that the wrong args were used, creating additional rows
      #4. Looking at the View() shows that:
      #   A. the column headers are being included as data
      #   B. spaces(" ") are being used as separators instead of tab
      #   C. single quotes(' ') are being used as quotes
      #   D. To fix, I can copy over the default args of read.delim() in ?read.delim() to read.table()
      
      #5. Creating our modified read.table() and see if the results are the same as read.delim()
      gapminder2fixed <- read.table(file = "gapminderDataFiveYear.txt", header = TRUE, sep = "\t", quote ="\"", 
                                    dec = ".", fill = TRUE, comment.char = "" )
      identical(gapminder1,gapminder2fixed)     #Yay! Identical!


#SMELL TEST THE DATA

      #1. The fastest way to review the data is using str()
      class(gapminder1)       # the data class is a data.frame
      mode (gapminder1)       # its mode is a list
      str(gapminder1)         # it has 1704 observations/rows and 6 variables
                              # first column, country is a factor with 142 levels
                              # second column, year are integers
                              # third column, population are numerics
                              # fourth column, continent are factors with 5 levels
                              # fifth column, lifeExp are numerics
                              # sixth column, gdpPercap are numerics                        
      #2. alternatively a more tedious way to do it is to use the following functions or for loops:
      ncol(gapminder1)        # number of columns, use with df/tables/matrices
      nrow(gapminder1)        # number of rows, use with df/tables/matrices
      length(gapminder1)      # it is a list with 6 variables (columns), generic-use
      length(gapminder1[,1])  # column 1 of the list has 1704 observations, generic-use
      z <- NULL;       for (i in 1:length(gapminder1)) {z <- c(z, class(gapminder1[,i]))};     z;   #for loop that checks class of each column

# EXPLORE INDIVIDUAL VARIABLES 
      
      # 1. Exploring quantitative variables (Life Expectancy)
      # Before we begin, note that subset() pulls out data.frames from data.frames while $ pulls out vectors 
      # from data.frames.
      # I learned most of the ggplot code from http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
      
            #1. Data exploration can be done with automatic statistics using summary()
            alllifeExp <- subset(gapminder1, subset = !is.na(lifeExp),select = 'lifeExp') #subsets the non-NA lifeExp column
            head(alllifeExp)                 # check to see if we successfully pulled out lifeExp values
            summary(alllifeExp)              # summary statistics of the lifeExp values as a table
            summary(gapminder1$lifeExp)       # summary also works on the vector version of lifeExp values 
            alllifeExp_mean <- summary(gapminder1$lifeExp)[4]; alllifeExp_mean    # pulls out the mean value as a numeric
            
            #3. Here's a sample of subsetting the lifeExp values using subset() and applying a filter to sort high
            # lifeExp values (more than the mean) and low lifeExp values (less than the mean)
            highlifeExp <- subset(gapminder1,subset = (lifeExp > alllifeExp_mean),select = 'lifeExp'); 
            lowlifeExp <- subset(gapminder1,subset = (lifeExp < alllifeExp_mean),select = 'lifeExp'); 
            head(highlifeExp)              # these are the life expectancy values higher than the mean value
            head(lowlifeExp)               # these are the life expectancy values lower than the mean value
            
            #4. Making tables 
            summary(highlifeExp$lifeExp); 
            summary(lowlifeExp$lifeExp);
            
            #5. Here is the recombination of the two subsets with the addition of a new column "rank" which describes if the observation has a high or low life expectancy
            highlifeExp$rank <- c("high"); head(highlifeExp)
            lowlifeExp$rank <- c("low"); head(lowlifeExp)
            alllifeExp_ranked <- rbind(highlifeExp,lowlifeExp);   head(alllifeExp) ; tail(alllifeExp)
            # additional calculation for drawing mean lines in geom_vline
                # data = your_data
                # .(...) = sort using the groups in this column
                # summarize , creates a condensed dataframe. alternatively, mutate & transform allow you to change the data without condensing it
                # lifeExpmean = mean(lifeExp), here I can add a new column(s) and specify the function used to transform the data (that was previously sorted/subsetted by .(...))
            library(plyr)
            alllifeExp_ranked_means <- ddply(alllifeExp_ranked, .(rank), summarize,  lifeExpmean=mean(lifeExp)); alllifeExp_ranked_means
            
            #6. ggplot-ting
            #   a.basic histogram   
                    # aes = what to draw
                    # geom_histogram = plot type
                    # binwidth = histogram unit)
                highlifeExp_plot1 <- ggplot(highlifeExp, aes(x=lifeExp)) + geom_histogram(binwidth = 2); highlifeExp_plot1  
            #   b.two sets of data in one histogram   
                # geom_histogram (draws a histogram)
                # color = choose a column to color the data differently
                # alpha = transparency level
                # position = where to place the bars 
                    # "identity" means place exactly where the value is
                    # "dodge" means interleave the bars if they overlap))
                # geom_vline (draws a Vertical(v) line)
                alllifeExpPlot <- ggplot(alllifeExp_ranked, aes(x=lifeExp, color=rank)) +
                     geom_histogram(fill="white", binwidth = 2,alpha = 0.5, position = "identity") +
                     geom_vline(data = alllifeExp_ranked_means, aes(xintercept=lifeExpmean, color=rank),
                           linetype="dashed") +
                     geom_density(aes(group=rank, colour=rank, fill=rank), alpha=0.3) +
                     labs (title = "High and Low Life Expectancies in All Countries", 
                           x = "Life Expectancy (years)", y = "Number of People (Million)")
                alllifeExpPlot;
            
          
      #2. Exploring categorical variables (Country) 
                # data summary
                
            
                allEuropecountry1952 <- subset(gapminder1, (year == 1952) & (continent == 'Europe') , select = c(country,pop)); head(allEuropecountry1952)
                str(allEuropecountry1952)
                head(allEuropecountry1952)  
                summary(allEuropecountry1952)
                # the basic plot
                allEuropecountry1952plot <- ggplot(allEuropecountry1952, aes(x = country, y = pop, color = country)) +
                       geom_point() 
                allEuropecountry1952plot;
                
                # additional beauty stuff
                num_of_breaks <- 5;
                max_pop <- (summary(allEuropecountry1952$pop))[6]; max_pop;
                pop_breaks = seq (from = 0, to = max_pop, by = (max_pop/num_of_breaks)); pop_breaks;
              
                # the better plot
                allEuropecountry1952plot_reordered <- ggplot(allEuropecountry1952, aes(x = reorder(country,pop), y = pop, color = country)) +
                       geom_point() +
                       theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=0.5)) +
                       theme(legend.position = "none") +
                       labs(title = "Year 1952: The Population of People in European Countries",
                            x = "Country", y = "Population") +
                       scale_y_continuous(breaks=pop_breaks,labels = comma) +
                       coord_flip();
                allEuropecountry1952plot_reordered;
                
            