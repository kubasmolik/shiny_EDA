#### helper function to plot histogram ####
plot_histogram <- function(df, var_name, bins_num, mean_x, sd_x, title, ...) {
    
    ## select var_name and rename it
    df %>%
        select(rlang::UQ(as.name(var_name))) %>%
        # rename(zmienna = rlang::UQ(as.name(var_name))) %>% 
        
        ## define histogram
        # ggplot(aes(x = zmienna)) +
        ggplot(aes_string(x = var_name)) +
        geom_histogram(fill = RColorBrewer::brewer.pal(n = 3, name = "Set1")[2], 
                       aes(y = ..density..), 
                       bins = bins_num) +
        
        ## density
        geom_density(alpha = 0.2, size = 1, aes(colour = "Density")) + 
        
        ## normal distribution
        stat_function(fun = dnorm, args = 
                          list(mean = mean_x, 
                               sd = sd_x), 
                      geom = "line", 
                      size = 1, 
                      aes(colour = "Normal dist.")) +
        
        ## layout
        # labs(x = paste(var_name),
        labs(x = var_name,
             y = "Density",
             title = title) + 
        scale_y_continuous(labels = scales::comma) + 
        scale_color_manual(values = c("Density" = "#000000", 
                                      "Normal dist." = rgb(1,0,0,1))
        ) + 
        theme_bw() +
        theme(legend.position = "bottom", legend.title = element_blank())
    
}