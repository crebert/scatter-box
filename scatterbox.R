set.seed(145)
# ggplot2 from tidyverse
library(tidyverse)
# plot grid
library(cowplot) 

# set standard theme
theme_set(theme_classic(base_family = "sans"))


# colors
my_pal <- c('#1F8A70', '#FD7400')


# combination of boxplot, points and lines connecting paired observations
geom_jitterbox <- function(){
  geom_custom <- list(
    geom_boxplot(aes(col = corpus, fill = corpus), show.legend = FALSE, 
                 outlier.alpha = 0, alpha = 0.1),
    geom_point(aes(col=corpus), show.legend = FALSE,
               alpha = 0.4,
               position = position_jitter(width = 0.2, seed = 578)),
    scale_color_manual(values = my_pal),
    scale_fill_manual(values = my_pal))
  
  # add lines
  geom_custom <- append(geom_custom,
                        geom_line(aes(group = treebank), 
                                  alpha = 0.4, col = 'gainsboro', 
                                  position = position_jitter(width = 0.2, seed = 578)))
  return(geom_custom)
}

annotate_log_odds <- function(high_anno = '', 
                              low_anno = '',
                              y_low = NULL,
                              y_high = NULL){
  
  # add interpretation hints for log odds
  list(
    geom_hline(yintercept = 0, color = 'lightgrey', linetype = 'dashed'),
    annotate(geom = 'text', x = 1.5, y = y_high, 
             label = high_anno, vjust = 0,
             color = 'darkgrey', size = 3),
    annotate(geom = 'text', x = 1.5, y = y_low,
             label = low_anno, vjust = 0,
             color = 'darkgrey', size = 3)
  )
}

# dummy data with n items
n <- 20
dt <- data.frame(corpus = c(rep("corpus1", n), rep("corpus2", n)),
                 treebank = rep(paste("treebank", seq(1:n)), 2),
                 value = rnorm(n*2))

# arrange by treebank to make jittered dots and lines match
dt <- arrange(dt, treebank)

# creating the plot
p <- dt %>% 
  ggplot(aes(corpus, value)) +
  geom_jitterbox() +
  labs(title = "Dummy feature") +
  annotate_log_odds(low_anno = "low value", 
                    high_anno = "high value",
                    y_low = -2,
                    y_high = 2.2)


# to generate the grid, create a list of plots
plots <- list(p, p, p, p) # add plots here
plot_grid(plotlist = plots, ncol = 2)

