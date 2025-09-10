library(tidyverse)
library(janitor)
library(knitr)
library(kableExtra)
library(readxl)

# The following is a function that takes in a dataframe and a set of variables and returns a table where each cell has the format of either n, x%, or n (x%) where n is the n value for that cell and x% is its percent of the total of its subgroup. Optionally, you can include row and/or column totals.

# dataframe should be the dataframe you want to use
# group should be the list of variables you want to group by
# sub_list should be the list of variables you want to subset by (the same as "group" most likely -- hence why that is the default)
# spread_list should be the variables that you want to put into the "spread" command before "nprop"
# to_include: Specify whether you want to get a table with count data, percentage data, or count and percentage data
  # Options:
  # "nprop": This is the default and will return a table of format n (x%)
  # "n": This will return a table of just the count data
  # "prop_perc": This will return a table of just the percentages with a percent sign behind them
  # "prop_num": This will return a table of the percentages with no percent sign behind them
# col should be TRUE if you want column totals and false if you don't
# row should be TRUE if you want row totals and false if you don't 
# title: If you want to add a caption to the table, add it here
# weighted indicates whether you want to use weighted values, rather than raw count
# weight_var: If you want to use weighted values, then please indicate (in quotes) the variable used to weight. 
# return_table indicates that the output returned will be a table. If you make it false, then this function won't run the code to turn the dataset into a table and format it and instead will just return a dataframe. This is included as it is occasionally useful to just get the dataframe back, but the default is to return a table. 

make_table <- function(dataframe, group, sub_list = group, spread_list = c(group[1]), to_include = "nprop", col = TRUE, row = TRUE, title = "", weighted = FALSE, weight_var = NULL, return_table = TRUE){
  
  ######################################
  #### Set Up Initial Table
  ######################################
  # Get a value for the number of groups
  num_group <- length(group)
  
  # Re-order the dataframe in the way that I specified through the group order
  # (This makes sure that the percentages are calculated in the way that I want)
  dataframe <- dataframe %>%
    select(group, everything())
  
  if (to_include == "prop_perc"|to_include == "prop_num"){
    col = FALSE
    row = FALSE
  }
  
  if (to_include == "nprop"){
    cell_content <- "nprop"
  }else if (to_include == "prop_perc") {
    cell_content <- "prop_perc"
  }else if (to_include == "n") {
    cell_content <- "n"
  }else if (to_include == "prop_num"){
    cell_content <- "prop_num"
  }else if (to_include == "weight_sum"){
    cell_content <- "weight_sum"
  }
  
  if (weighted == FALSE){
    
    if (num_group == 1){
      table <- dataframe %>%
        group_by(dataframe[, names(dataframe) %in% group]) %>% 
        dplyr::summarize(n=n())%>%
        mutate(prop=n/sum(n))%>%
        mutate(nprop = paste0(n, " (", round(prop,4)*100, "%)"))%>%
        mutate(prop_num = round(prop,4)*100)%>%
        mutate(prop_perc = paste0(round(prop,4)*100, "%"))
      
      colnames(table)[1] <- group
      
      table <- table %>%  
        subset(select=c(sub_list, cell_content))%>%
        spread(spread_list, cell_content)
    }else{
      table <- dataframe %>%
        group_by(dataframe[, names(dataframe) %in% group])%>%
        dplyr::summarize(n=n())%>%
        mutate(prop=(n/sum(n)))%>%
        mutate(nprop = paste0(n, " (", round(prop,4)*100, "%)"))%>%
        mutate(prop_num = round(prop,4)*100)%>%
        mutate(prop_perc = paste0(round(prop,4)*100, "%"))%>%
        subset(select=c(sub_list, cell_content))%>%
        spread(spread_list, cell_content)
    }
  
  }
  
  if (weighted == TRUE){
    
    dataframe <- dataframe %>% dplyr::rename(variable = weight_var)
    
    if (num_group == 1){
      table <- dataframe %>%
        group_by(dataframe[, names(dataframe) %in% group]) %>% 
        dplyr::summarize(weight_sum = sum(variable))%>%
        mutate(prop=weight_sum/sum(weight_sum))%>%
        mutate(nprop = paste0(weight_sum, " (", round(prop,4)*100, "%)"))%>%
        mutate(prop_num = round(prop,4)*100)%>%
        mutate(prop_perc = paste0(round(prop,4)*100, "%"))
      
      colnames(table)[1] <- group
      
      table <- table %>%  
        subset(select=c(sub_list, cell_content))%>%
        spread(spread_list, cell_content)
    }else{
      table <- dataframe %>%
        group_by(dataframe[, names(dataframe) %in% group])%>%
        dplyr::summarize(weight_sum = sum(variable))%>%
        mutate(prop=weight_sum/sum(weight_sum))%>%
        mutate(nprop = paste0(weight_sum, " (", round(prop,4)*100, "%)"))%>%  
        mutate(prop_num = round(prop,4)*100)%>%
        mutate(prop_perc = paste0(round(prop,4)*100, "%"))%>%
        subset(select=c(sub_list, cell_content))%>%
        spread(spread_list, cell_content)
    }
  }
  
  x <- table
  
  # If row totals aren't being calculated, then make this the final table
  if (row == FALSE & col == FALSE){
    table3 <- table
  }
  
  ######################################
  #### Get Column Totals
  ######################################
  
  if (col == TRUE){
    
    # Call y the provided table
    y <- x
    
    # Get rid of the percentages in parentheses
    y <- as.data.frame(lapply(y, function(x) gsub("\\s*\\([^\\)]+\\)","",as.character(x))))
    
    # Get rid of extra spaces
    y <- as.data.frame(lapply(y, function(x) gsub(" ","",as.character(x))))
    
    # Convert string to numbers
    y <- as.data.frame(lapply(y, function(x) as.numeric(as.character(x))))
    
    # Return original colnames
    colnames(y) <- colnames(x)
    
    # Give an empty variable to be filled
    y$hold <- ""
    
    # Put the empty column up front
    y <- y %>% select(hold, everything())
    
    # Get column totals
    y  <- y %>% adorn_totals("row")
    
    # Get the number of rows
    ynr <- nrow(y)
    
    if (num_group == 1){
      y <- y %>% dplyr::rename(" " = "hold")
      x$` ` <- ""
      x <- x %>% select(` `, everything())
    }else{
      y$hold <- NULL
      y[ynr,1:(num_group-1)] <- "" # Get rid of the totals formed for the category columns 
      y[ynr,(num_group-1)] <- "Total" # Add the word "total" for the bottom row
    }
    
    # Add the total row below
    table2 <- rbind(data.frame(x), data.frame(y[ynr,]))
    
    # Return original colnames
    colnames(table2) <- colnames(x)
    
    # If row totals aren't being calculated, then make this the final table
    if (row == FALSE){
      table3 <- table2
    }
    
  }
  
  ######################################
  #### Get Row Totals
  ######################################
  
  if (row == TRUE){
    
    # If column totals weren't calculated, make table2 the input table
    if (col == FALSE){
      table2 <- x
    }
    
    y <- table2
    
    # Get rid of the percentages
    y <- as.data.frame(lapply(y, function(x) gsub("\\s*\\([^\\)]+\\)","",as.character(x))))
    
    # Get rid of extra spaces
    y <- as.data.frame(lapply(y, function(x) gsub(" ","",as.character(x))))
    
    # Turn into numbers 
    y <- as.data.frame(lapply(y, function(x) as.numeric(as.character(x))))
    
    # Give original column names
    colnames(y) <- colnames(table2)
    
    # Get row totals
    if (num_group>2){
      y_first_row <- data.frame(y[,1]) # Separate the first row of y
      colnames(y_first_row) <- colnames(y)[1] # Give it the correct column name 
      y_rest  <- y[,c((num_group-1):ncol(y))] %>% adorn_totals("col") # Calculate the sum of the cols of interest. Note that adorn_totals naturally cuts off the first column of whatever dataframe you feed it, since it assume that column to be categorical
      y <- cbind(y_first_row, y_rest) # Re-bind the dataframe together
    }
    
    if (num_group == 2){
      y  <- y %>% adorn_totals("col") # If you're only working with two grouping variables, then adorn_totals naturally works as intended
    }
    
    if (num_group == 1){
      if (col == TRUE){
        y  <- y %>% adorn_totals("col")
      }
      if (col == FALSE){
        y  <- y %>% adorn_totals("col")
        y$Total <- y$Total + y[,1] # Recall that adorn_totals will skip the first column categorically, so you have to add it back on in this case
      }
    }
    
    
    # Get number of columns 
    ync <- ncol(y)
    
    # Bind the data frame and totals
    table3 <- cbind(data.frame(table2), data.frame(y[,ync]))
    
    # Make total column
    table3$Total <- table3[,ync]
    
    # Get rid of extra column
    table3$`y...ync.` <- NULL
    
    # Give original column names back
    table2$Total <- ""
    colnames(table3) <- colnames(table2)
    
  }
  
  ######################################
  #### Table Design
  ######################################
  
  # As long as I really want a table and not a dataframe, then I run the table formatting commands
  # Otherwise, I return the dataframe
  if (return_table == TRUE){
    output <- knitr::kable(table3, caption = title, format = "html")%>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)
  }else{
    output <- table3
  }
  
  output
}






