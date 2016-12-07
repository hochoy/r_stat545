# README for homework 11

Hi, my homework 11 is [here](https://wai-ho-choy.shinyapps.io/new_app). 

The ui.R and server.R files are stored within my new_app folder while the combined app.R file is out here. For this homework, I have used the code I learned in the lecture and added a few of the recommended additions.

Additions added:

1.  Add an option using checkboxInput() to choose to filter results by country using conditionalPanel()
2.  Added the BC liquor Store image to the UI by placing the image in a www folder and then calling the image using img(src = "filename.png")
3.  Used the DT package to create an interactive tbale of results where we can sort rows by clicking on column names
4.  Added color to the ggplot2 aesthetics
5.  Added a if.null(filtered()) to return() if there are no search results so that ggplot doesn't try to render a NULL dataframe
6.  Added a textOutput in between the plot and table to print the number of search results
7.  Used an if (input$selectCountry) statement to determine whether to filter by country or show all countries

Sorry for the late entry and thanks!