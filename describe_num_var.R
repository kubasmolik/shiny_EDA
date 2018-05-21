#------------------------------------------------------------------------------#
####                  FUNCTION DESCRIBING NUMERIC VARIABLES                 ####
#------------------------------------------------------------------------------#

#### parameters
## df               - data frame
## var_name         - numeric variable you want to describe in quotes ("")
## target           - target variable in quotes ("")
## markdown         - generate markdown html report - TRUE/FALSE
## path_to_rmd_file - path to associated .Rmd file (report template)
## output_path      - where to output generated html report

describe_num_var <- function(df, var_name, target, markdown = FALSE, 
                             path_to_rmd_file = NULL, output_path = NULL) {
    
    ## check for packages
    if(!("DescTools" %in% installed.packages())) install.packages("DescTools")
    if(!("RColorBrewer" %in% installed.packages())) install.packages("RColorBrewer")
    if(!("dplyr" %in% installed.packages())) install.packages("dplyr")
    if(!("ggplot2" %in% installed.packages())) install.packages("ggplot2")
    if(!("rlang" %in% installed.packages())) install.packages("rlang")
    if(!("nortest" %in% installed.packages())) install.packages("nortest")
    if(!("scales" %in% installed.packages())) install.packages("scales")
    
    if(markdown == T) {
        if(!("plotly" %in% installed.packages())) install.packages("plotly")
        if(!("markdown" %in% installed.packages())) install.packages("markdown")
        if(!("rmarkdown" %in% installed.packages())) install.packages("rmarkdown")
        if(!("knitr" %in% installed.packages())) install.packages("knitr")
    }
    
    ## load packages if not loaded
    library("dplyr")
    library("ggplot2")
    library("DescTools")
    
    ## check if type of var_name is valid
    df %>%
        select(UQ(as.name(var_name))) %>%
        .[[1]] %>%
        typeof() -> var_type
    
    if(!(var_type %in% c("integer", "numeric", "double"))) {
        stop("Variable you want to describe is not numeric")
    }
    
    ## check type of target variable
    df %>%
        distinct(UQ(as.name(target))) %>%
        count() %>%
        .[[1]] -> target_n_levels
    
    target_type <- ifelse(target_n_levels == 2, "binary", "numeric")
    
    #### VARIABLE DESCRIPTION ####
    
    ## summary statistics
    df %>%
        select(rlang::UQ(as.name(var_name))) %>%
        DescTools::Desc(plotit = F) -> var_description
    
    ## normality test
    df %>%
        select(rlang::UQ(as.name(var_name))) %>%
        .[[1]] %>%
        nortest::lillie.test() -> normality_test
    
    ## histogram
    df %>%
        summarize(x = mean(rlang::UQ(as.name(var_name))),
                  y = sd(rlang::UQ(as.name(var_name)))) %>% 
        .[1, ] %>% 
        as.numeric() -> var_name_stats
    
    plot_histogram(df = df, var_name = var_name, mean_x = var_name_stats[1],
                   sd_x = var_name_stats[2], 
                   title = paste("Distribution of ", var_name, sep ="")
                   ) -> dist_plot
    
    ### VARIABLE DESCRIPTION  WITHOUT OUTLIERS ####
    
    ## filtering
    f <- . %>%
        mutate(x = as.numeric(scale(rlang::UQ(as.name(var_name))))) %>%
        filter(abs(x) <= 3) 
    
    ## summary statistics
    df %>%
        f %>%
        select(rlang::UQ(as.name(var_name))) %>%
        DescTools::Desc(plotit = F) -> var_description_out
    
    ## normality test
    df %>%
        f %>%
        select(rlang::UQ(as.name(var_name))) %>%
        .[[1]] %>%
        nortest::lillie.test() -> normality_test_out
    
    ## histogram
    df %>%
        f %>%
        summarize(x = mean(rlang::UQ(as.name(var_name))),
                  y = sd(rlang::UQ(as.name(var_name)))) %>% 
        .[1, ] %>% 
        as.numeric() -> var_name_stats_out
    
    plot_histogram(df = df %>% f, 
                   var_name = var_name, 
                   mean_x = var_name_stats_out[1],
                   sd_x = var_name_stats_out[2],
                   title = paste0("Distribution of ", var_name, " without outliers")
                   ) -> dist_plot_out
    
    #### RELATIONSHIP WITH TARGET ####
    
    #### numeric target ####
    if(target_type == "numeric"){
        
        ## var_name ~ target plot
        ## data preparation
        df %>%
            f %>%
            mutate(x2 = as.numeric(scale(rlang::UQ(as.name(target))))) %>%
            filter(abs(x2) <= 3) %>%
            rename(zmienna = rlang::UQ(as.name(var_name)), 
                   y = rlang::UQ(as.name(target))) %>%
            select(zmienna, y) %>%
            
            ## define plot
            ggplot(aes(x = zmienna, y = y)) +
            geom_point(fill = "black", alpha = 0.5) + 
            geom_smooth(color = "red", fill = "darkblue") +
            
            ## layout
            labs(x = paste(var_name),
                 y = paste(target),
                 title = paste0(target, " vs. ", var_name)) +
            theme_bw() -> y_plot
        
        ## correlation
        df %>%
            select(rlang::UQ(as.name(var_name)), 
                   rlang::UQ(as.name(target))) %>%
            #as.data.frame() %>%
            cor(use = "complete.obs") %>% 
            .[1,2] -> correlation

    }
    
    #### binary target ####
    if(target_type == "binary") {
        
        ## densities comparison
        ## data preparation
        df %>%
            f %>%
            rename(zmienna = rlang::UQ(as.name(var_name)), 
                   y = rlang::UQ(as.name(target))) %>%
            mutate(y = as.factor(as.character(y))) %>%
            select(zmienna, y) %>%
            
            ## define plot
            ggplot(aes(x = zmienna, fill = y)) +
            geom_density(alpha = 0.5, size = 1) +
            
            ## layout
            scale_fill_manual(
                name = paste0("Levels of ", target),
                values = RColorBrewer::brewer.pal(n = 3, name = "Set1")[2:1]
                ) +
            labs(x = paste(var_name),
                 y = "Density",
                 title = "Conditional densities comparison"
            ) + 
            scale_y_continuous(labels = scales::comma) + 
            theme_bw() +
            theme(legend.position = "bottom") -> y_plot
        
        ## boxplot
        ## data preparation
        df %>%
            f %>%
            rename(zmienna = rlang::UQ(as.name(var_name)), 
                   y = rlang::UQ(as.name(target))) %>%
            mutate(y = as.factor(as.character(y))) %>%
            select(zmienna, y) %>%
            
            ## define plot
            ggplot(aes(y = zmienna, x = y, fill = y)) +
            geom_boxplot() +
            
            ## layout
            scale_fill_manual(
                name = paste0("Levels of ", target),
                values = RColorBrewer::brewer.pal(n = 3, name = "Set1")[2:1]
            ) +
            labs(x = paste(target),
                 y = paste(var_name),
                 title = paste0(target, " vs. ", var_name)
            ) + 
            scale_y_continuous(labels = scales::comma) + 
            theme_bw() +
            theme(legend.position = "bottom") -> y_plot_2
            
        ## T-test for means
        t.test(
            as.formula(paste(var_name, " ~ ", target)), 
            data = df) -> means_ttest
        
        ## Wilcoxon test for means
        wilcox.test(
            as.formula(paste(var_name, " ~ ", target)), 
            data = df) -> means_wilcoxon
    }
    
    #### RETURNS WITHOUT MARKDOWN ####
    if(markdown == FALSE) {
    
        #### prints
        ## with outliers
        print(paste(rep("=", 80), collapse = ""))
        print(paste0("DESCRIPTION FOR VARIABLE ", var_name))
        cat("\n")
        print(var_description)
        cat("\n")
        print(normality_test)
        print(paste(rep("=", 80), collapse = ""))
        
        ## without outliers
        print("STATISTICS WITHOUT OUTLIERS:")
        cat("\n")
        print(var_description)
        cat("\n")
        print(normality_test)
        print(paste(rep("=", 80), collapse = ""))
        
        ## tests
        print("STATISTICAL TESTS:")
        cat("\n")
        if(target_type == "numeric") {
            print(paste0("correlation between ", 
                         var_name, 
                         " and target variable: ", 
                         correlation))
        } else {
            print(means_ttest)
            cat("\n")
            print(means_wilcoxon)
        }
        
        #### plots
        plot(dist_plot)
        plot(dist_plot_out)
        plot(y_plot)
        if(target_type == "binary") { plot(y_plot_2) }
    }
    
    #### GENERATING MARKDOWN REPORT ####
    if(markdown == TRUE) {
        
        ## save output so the .Rmd file could see it
        to_save <- c("var_name", "target", "var_description", "normality_test", 
                     "dist_plot", "var_description_out", "normality_test_out", 
                     "dist_plot_out", "y_plot", "target_type")
        if(target_type == "numeric") {
            to_save <- append(to_save, c("correlation"))
        } else {
            to_save <- append(to_save, c("y_plot_2", "means_ttest", 
                                         "means_wilcoxon"))
        }
        save(list = to_save, 
             file = paste(path_to_rmd_file, "df_markdown.RData", sep = "/"))
        
        ## render RMD file
        rmarkdown::render(paste0(path_to_rmd_file, "/describe_num_var.Rmd"),
                          output_file = paste0(var_name, ".html"),
                          output_dir = paste0(output_path),
                          params = list(dynamictitle = 
                                            paste0("Report for variable ", 
                                                  var_name),
                                        dynamicdate = Sys.Date())
                          )
    }
}

#### helper function to plot histogram ####
plot_histogram <- function(df, var_name, mean_x, sd_x, title, ...) {
    
    ## select var_name and rename it
    df %>%
        select(rlang::UQ(as.name(var_name))) %>%
        rename(zmienna = rlang::UQ(as.name(var_name))) %>% 
        
        ## define histogram
        ggplot(aes(x = zmienna)) +
        geom_histogram(fill = RColorBrewer::brewer.pal(n = 3, name = "Set1")[2], 
                       aes(y = ..density..), 
                       bins = 30) +
        
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
        labs(x = paste(var_name),
             y = "Density",
             title = title) + 
        scale_y_continuous(labels = scales::comma) + 
        scale_color_manual(values = c("Density" = "#000000", 
                                      "Normal dist." = rgb(1,0,0,1))
                           ) + 
        theme_bw() +
        theme(legend.position = "bottom", legend.title = element_blank())
        
}
