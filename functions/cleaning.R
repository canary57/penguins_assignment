
### R script containing the cleaning functions necessary to organise the penguins dataset.


### Cleaning functions provided by Lydia France at 
### https://github.com/LydiaFrance/PenguinProject/blob/main/functions/cleaning.r


#Clean column names function
clean_column_names <- function(penguins_data) {
  penguins_data %>%
    select(-starts_with("Delta")) %>% #Removes columns that start with 'delta'
    select(-Comments) %>% #Removes columns that have 'comments' in them
    clean_names() #Cleaning function from janitor package
}

#Function to shorten the species names
shorten_species <- function(penguins_data) {
  penguins_data %>%
    mutate(species = case_when(
      species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie", #Shortens each of the species names
      species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
      species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo"
    ))
}

# Function to remove any empty columns or rows
remove_empty_columns_rows <- function(penguins_data) {
  penguins_data %>%
    remove_empty(c("rows", "cols")) #Removes empty rows and columns
}


### Overall cleaning function, which combines the cleaning functions above.

#Overall cleaning function
clean_data <- function(penguins_data) {
  penguins_data %>%
    clean_column_names() %>%
    shorten_species() %>%
    remove_empty_columns_rows()
}


### Filter the data function provided by Lydia France. 

#Filter the data by column
subset_columns <- function(penguins_data, column_names) {
       penguins_data %>%
           select(all_of(column_names)) #Allows you to filter by certain column names
}

#Filter by species:
filter_by_species <- function(penguins_data, selected_species) {
      penguins_data %>%
           filter(species == selected_species) #Can filter the data by certain species
 }


