vr_forest_plot <- function(
    broom_object, 
    title = "", 
    abbreviations = "", 
    variable_definitions = "", 
    N, 
    N_unit = "patients"
) {
    # TODO Mark to implement this
    # See vr_attrition for an example of documentation strings
}



# TODO: 
# 1. add estimate type 
# 2. add raw data i.e. n, y, etc. 
# 3. think about glue information to y to make more informative labels
# 4. think about how to plot forest plot in different ways
# 5. meta data arounf endpoint type, title, subtitles, data source, time executred, etc. 
# 6. function write to ARD. 
#     - could the meta data be a tibble with the estimates a tibble to keep table strucutre. 
#     - the overarching tibble would contain meta data and unique analysis id. 

library(tidyverse)

vr_tidy_rbest(map_crohn)


map_crohn %>%
    vr_tidy_rbest() %>%
    ggplot(aes(x = row_id, y = estimate)) +
    geom_pointrange(aes(y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_point() +
    coord_flip()




### this is the endpoint label - 
## TODO: BUILD THIS IN TO META DATA
xlab_str <- switch(x$family$family,
                   gaussian="Response",
                   binomial="Response Rate",
                   poisson="Counting Rate")



map_crohn %>%
    vr_tidy_rbest() %>%
    filter(model == "meta") %>%
    ggplot(aes(
        x = study,
        y = estimate,
        ymin = conf.low,
        ymax = conf.high
    )) +
    geom_pointrange(show.legend = FALSE,
                    width = 1) +
    coord_flip()

+
    #  facet_wrap( ~ model) +
    theme_light()

position_dodge(width = 1)




library(RBesT)
library(tidyverse)
library(ggplot2)
example(crohn)
print(map_crohn)
str(map_crohn)
forest_plot(map_crohn)


ddf <- map_crohn %>%
    vr_tidy_rbest()

map_crohn %>%
    vr_tidy_rbest() %>%
    ggplot(aes(y = row_id, x = estimate, label = study)) +
    geom_point() +
    geom_text()



###
# adjust the base font size
theme_set(theme_default(base_size=16))
forest_plot(map_crohn, model="both", est="MAP", size=1) + legend_move("right") +
    labs(title="Forest plot", subtitle="Results of Meta-Analytic-Predictive (MAP) analysis", 
         caption="Plot shows point estimates (posterior medians) with 95% intervals")

