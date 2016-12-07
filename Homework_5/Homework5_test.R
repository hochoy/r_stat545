# Load the libraries

main_folder <- getwd()
setwd("git_world/wai-ho_choy/Homework_5/")
library(gapminder)
library(ggplot2)
library(gridExtra)
library(grid)
library(gtable)
library(gplots) # to convert color from character to hex using col2hex()
#library(plyr) # causes problems with dplyr's group_by and summarize()
suppressPackageStartupMessages(library(dplyr))

# Filter the data to exclude all Oceania data
gap_all <- tbl_df(gapminder);gap_all
gap_no_drop <- gap_all %>% filter(continent != "Oceania"); gap_no_drop
gap_no_ocean <- gap_no_drop %>% droplevels(); gap_no_ocean

# Find out the levels of the continents and number for rows before and after removing Oceania
lvl_bef <- gap_all$continent %>% nlevels()
nrow_bef <- gap_all %>% nrow()
lvl_aft <- gap_no_ocean$continent %>% nlevels()
nrow_aft <- gap_no_ocean %>% nrow()

bef_aft_df <- data.frame(levels = c(lvl_bef,lvl_aft), nrows = c(nrow_bef,nrow_aft)); 
dimnames(bef_aft_df)[[1]] <- c("Before dropping Oceania","After dropping Oceania"); bef_aft_df

# Changes affected by filtering data and by changing factor levels
plot_all_gdp <- gap_all %>%  ggplot(aes(x = year, y = gdpPercap, color = continent)) +
  geom_point() + facet_grid(. ~ continent)
plot_no_ocean_gdp <- gap_no_ocean %>%  ggplot(aes(x = year, y = gdpPercap, color = continent)) +
  geom_point() + facet_grid(. ~ continent)
grid.arrange(plot_all_gdp,plot_no_ocean_gdp)

# Reorder the levels of continent 
gap_all_diffs <- gap_no_ocean %>% 
  group_by(continent) %>% 
  summarize(pop_diff = max(pop) - min(pop),
            gdp_diff = max(gdpPercap) - min(gdpPercap))
gap_all_diffs$continent %>% levels()
gap_all_diffs <- gap_all_diffs %>% 
  mutate(coolor = c("2","3","4","1"))

# using arrange()
gap_all_diffs_arrange <- gap_all_diffs %>% 
  ungroup() %>% 
  arrange(pop_diff)

# using reorder()
gap_all_diffs_reorder <- gap_all_diffs %>% 
  ungroup() %>% 
  mutate (continent = reorder(continent,pop_diff))

# using reorder() and arrange()
gap_all_diffs_dual <- gap_all_diffs %>% 
  ungroup() %>% 
  mutate (continent = reorder(continent,pop_diff)) %>% 
  arrange(pop_diff)


# Comparison

# Extra code to add title and footnotes to tables in grid.arrange(), found here http://stackoverflow.com/questions/11774703/adding-text-to-a-grid-table-plot
describe_table <- function(df, title_var = "Table1",foot_var ="footnote") {
  table <- tableGrob(df);
  title <- textGrob(title_var,gp=gpar(fontsize=20));
  footnote <- textGrob(foot_var, x=0, hjust=0,gp=gpar( fontface="italic"));
  padding <- unit(0.5,"line");
  
  table2 <- gtable_add_rows(table, heights = grobHeight(title) + padding,pos = 0)
  table3 <- gtable_add_rows(table2, heights = grobHeight(footnote)+ padding)
  table4 <- gtable_add_grob(table3, list(title, footnote),t=c(1, nrow(table3)), l=c(1,2), r=ncol(table3))
  table4
}

# As we can see, reorder() alone does not appear to change the table view of the dataframe while arrange() does
grid.arrange(describe_table(gap_all_diffs,title_var = "Original"),                      # un-ordered
             describe_table(gap_all_diffs_reorder,title_var = "Reordered"),  # re-ordered shows no changes in order in table-form
             describe_table(gap_all_diffs_arrange,title_var = "Arranged"),  # arranged shows changes in order in table-form
             describe_table(gap_all_diffs_dual,title_var = "Both"))  # both ordered and arranged

# However, when we plot the graphs for the dataframes, we can see that reorder() has re-ordered the continents 
# by their respective factor levels as determined by the variable pop_diff which I defined previously. In 
# contrast, arrange() has had no effect on the graph plot. This is because although the order of each 
# observations has changed in the dataframe, the factor levels (which control the ggplot display) has not 
# changed.
plot_diff_ori <- gap_all_diffs %>% 
  ggplot(aes(x=continent,y=pop_diff,fill = continent)) +
  geom_bar(stat="identity") +
  ggtitle("Original") 
plot_diff_reorder <- gap_all_diffs_reorder %>% 
  ggplot(aes(x=continent,y=pop_diff,fill = continent)) +
  geom_bar(stat="identity") +
  ggtitle("Reordered") 
plot_diff_arrange <- gap_all_diffs_arrange %>% 
  ggplot(aes(x=continent,y=pop_diff,fill = continent)) +
  geom_bar(stat="identity") +
  ggtitle("Arranged")
plot_diff_dual <- gap_all_diffs_dual %>% 
  ggplot(aes(x=continent,y=pop_diff,fill = continent)) +
  geom_bar(stat="identity") +
  ggtitle("Reordered and arranged") 

grid.arrange(plot_diff_ori,
             plot_diff_reorder,
             plot_diff_arrange,
             plot_diff_dual)

#By comparing the table-view and plot-view of the re-ordered and arranged dataframe, I can see that:
# 1. re-ordering the factor-levels changes the way plots are organized
# 2. re-ordering does not change the order of the rows of observation
# 3. conversely, arrange() does not change the ways plots are organized as factor levels are preserved
# 4. arrange does, however, change the rows of observation as can be seen in the table-view of the dataframe

# Visualization design
# Both the use and choice of color is very important to visualization. As we can see in my "before" plots, 
# the bars are in different colors between plots and do not give additional information about the data. This
# is due to reorder() changing the factor levels. In contrast, 
# my "after" plots use colors in increasing intensity (Here, I use the colors resembling fire). 
# Furthermore, having the colors match the continents allow better comparisons between plots

#To do this, I had previously created a new column called coolor (color) to be used by fill = in ggplot instead of continent
# Because the re-ordering is done on continent only, coolor preserves its factor levels and stays consistent betweent plots
flame <- col2hex(c("yellow","orange","tomato","maroon"))
plot_diff_ori_coolor <- plot_diff_ori + aes(fill = coolor) + scale_fill_manual(values = flame)
plot_diff_reorder_coolor <- plot_diff_reorder + aes(fill = coolor) + scale_fill_manual(values = flame)
plot_diff_arrange_coolor <- plot_diff_arrange + aes(fill = coolor) + scale_fill_manual(values = flame)
plot_diff_dual_coolor <- plot_diff_dual + aes(fill = coolor) + scale_fill_manual(values = flame)

plot_before <- arrangeGrob(plot_diff_ori,
                           plot_diff_reorder,
                           plot_diff_arrange,
                           plot_diff_dual)
plot_after <- arrangeGrob(plot_diff_ori_coolor,
                          plot_diff_reorder_coolor,
                          plot_diff_arrange_coolor,
                          plot_diff_dual_coolor)
grid.arrange(plot_before)
grid.arrange(plot_after)

#saving using ggsave
ggsave(plot = plot_diff_dual, filename = "Plot_dual_before_color.pdf",scale = 1.2) 
# saving using png
png("png_dual_plot.png",width=500,height=250)
grid.arrange(plot_after)
dev.off()
#saving using svg
svg("svg_dual_plot.svg",width=15,height=10)
grid.arrange(plot_after)
dev.off()

# Rmd link: ![Alt text](/path/to/img.png)

