
### Plotting and save plot functions 


# Plot'bad figure'
plot_bad_figure <- function(penguins_clean){
  penguins_clean %>%
    ggplot(aes(x =body_mass_g, #Plot body mass by flipper length
                         y = flipper_length_mm)) +
  geom_point(colour = 'grey', size = 3.5, shape = 8) + #Using a bad colour, size and shape. 
  theme_dark() #Using a bad colour theme
}



## Plot and save exploratory figure

#Plot exploratory figure
plot_exploratory_figure <- function(flipper_mass_data){
  flipper_mass_data %>%
    ggplot(aes(x = body_mass_g, y = flipper_length_mm)) + #Plot body mass by flipper length
    geom_point(aes(colour = species), alpha = 0.5) + #Colour the points by species, and change the transparency
    labs(title="Correlation Between Body Mass and Flipper Length", y = "Flipper Length (mm)", 
         x = "Body Mass (g)", colour = 'Species') + #Change the axis and title names
    theme_bw() + #Change the theme of the plot
    scale_colour_manual(values = c('dodgerblue3', 'sienna3', 'olivedrab')) + #Manually change the colours of the species
    theme(plot.title = element_text(hjust = 0.5)) #Move the position of the title of the plot to the middle
}

#Save exploratory figure
save_exploratory_svg <- function(flipper_mass_data, 
                                 filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width   = size_inches, 
          height  = size_inches, 
          scaling = scaling)
  exploratory_figure <- plot_exploratory_figure(flipper_mass_data) #Use exploratory figure
  print(exploratory_figure)
  dev.off()
}



### Plot and save overall relationship

# Plot overall relationship figure
plot_overall_figure <- function(flipper_mass_data){
  flipper_mass_data %>%
    ggplot(aes(x = body_mass_g, y = flipper_length_mm)) + #Plot body mass by flipper length
    geom_point(aes(colour = species), alpha = 0.5) + #Colour the points by species, and change the transparency
    labs(title="Correlation Between Body Mass and Flipper Length", y = "Flipper Length (mm)", 
         x = "Body Mass (g)", colour = 'Species') + #Change the axis and title names
    theme_bw() + #Change the theme of the plot
    scale_colour_manual(values = c('dodgerblue3', 'sienna3', 'olivedrab')) + #Manually change the colours of the species
    theme(plot.title = element_text(hjust = 0.5)) + #Move the position of the title of the plot to the middle
    geom_smooth(method = lm, colour = 'black', linewidth = 0.7, show.legend= FALSE) + #Adds the regression line to the graph
    stat_cor(label.y = 230, method = 'spearman') #Adds the R and p values
}

#Save overall relationship figure
save_overall_svg <- function(flipper_mass_data, 
                                 filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width   = size_inches, 
          height  = size_inches, 
          scaling = scaling)
  overall_figure <- plot_overall_figure(flipper_mass_data) #Use overall figure
  print(overall_figure)
  dev.off()
}




### Plot individual species results figure

#Plot individual species results figure
plot_species_figure <- function(flipper_mass_data){
  flipper_mass_data %>%
    ggplot(aes(x = body_mass_g, y = flipper_length_mm)) + #Plot body mass by flipper length
    geom_point(aes(colour = species), alpha = 0.5) + #Colour the points by species, and change the transparency
    labs(title="Correlation Between Body Mass and Flipper Length", y = "Flipper Length (mm)", 
         x = "Body Mass (g)", colour = 'Species') + #Change the axis and title names
    theme_bw() + #Change the theme of the plot
    scale_colour_manual(values = c('dodgerblue3', 'sienna3', 'olivedrab')) + #Manually change the colours of the species
    theme(plot.title = element_text(hjust = 0.5)) + #Move the position of the title of the plot to the middle
    geom_smooth(method = lm, colour = 'black', linewidth = 0.7, show.legend= FALSE) + #Adds the regression line to the graph
    facet_wrap(vars(species)) + #Separates the graph to show individual species
    stat_cor(label.y = 235, method = "spearman") #Adds the R and p values
}

#Save individual results figure
save_species_svg <- function(flipper_mass_data, 
                             filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width   = size_inches, 
          height  = size_inches, 
          scaling = scaling)
  species_figure <- plot_species_figure(flipper_mass_data) #Use species figure
  print(species_figure)
  dev.off()
}








