library(tidyverse)

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Statistical Analysis
#----------------------------------------------------------------------------#
trends <- read.csv('/Volumes/sfkunal/INFO201/assignments/a4-sfkunal/data/incarceration_trends.csv')

#this function gets the total number of observations
get_observations <- function() {
  return(nrow(trends))
}

#this function returns the average percent of black male prisoners in all male prisoners
get_black_pct <- function() {
  pct <- mean(trends$black_male_prison_pop, na.rm=TRUE) / mean(trends$male_prison_pop, na.rm=TRUE) * 100
  return(round(pct, 2))
}

#this function returns the average percent of white male prisoners in all male prisoners
get_white_pct <- function() {
  pct <- mean(trends$white_male_prison_pop, na.rm=TRUE) / mean(trends$male_prison_pop, na.rm=TRUE) * 100
  return(round(pct, 2))
}

#this function returns the average percent of latin male prisoners in all male prisoners
get_latin_pct <- function() {
  pct <- mean(trends$latinx_male_prison_pop, na.rm=TRUE) / mean(trends$male_prison_pop, na.rm=TRUE) * 100
  return(round(pct, 2))
}

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#
# This function returns a df consisting of year and summed jail population by year
get_year_jail_pop <- function() {
  df <- trends %>%
    select(year, total_jail_pop) %>%
    group_by(year) %>%
    summarise(total = sum(total_jail_pop, na.rm=TRUE))
  return(df)   
}

# This function plots a bar plot of the U.S prison population by year
plot_jail_pop_for_us <- function()  {
  df <- get_year_jail_pop()
  p <- ggplot(data=df, aes(x=year, y=total)) +
    geom_bar(stat="sum") + 
    theme(legend.position = "none") +
    labs(
      title = "Growth of the U.S Prison Population",
      x = "Year",
      y = "Total Jail Population",
      caption = "Figure 1: U.S. Prison Population Growth"
    )
  library(scales)
  p <- p + scale_y_continuous(labels = label_comma())
  return(p)   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
#----------------------------------------------------------------------------#
#this function filters data given a vector of states, sorts by year, and sums jail populations
get_pop_by_states <- function(states) {
  df <- trends %>%
    filter(state %in% states) %>%
    group_by(year, state) %>%
    summarise(state_pop = sum(total_jail_pop, na.rm=TRUE))
  return(df)
}

#this function returns a line plot of each states prison growth, given a vector of states
plot_pop_by_states <- function(states) {
  df <- get_pop_by_states(states)
  p <- ggplot(df) + 
    geom_smooth(aes(x=year, y=state_pop, color=state), se=F) +
    labs(
      title = "Growth of Prison Population by States",
      x = "Year",
      y = "Total Jail Population",
      caption = "Figure 2: U.S. Prison Population Growth by Specific States"
    )
  return(p)
}

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
#----------------------------------------------------------------------------#
#this function filters and returns a df of  the maximum white and black prisoners in each year
get_black_white_pop <- function() {
  df <- trends %>%
    group_by(year) %>%
    summarise(
      white_pop_sum = max(white_jail_pop, na.rm=TRUE),
      black_pop_sum = max(black_jail_pop, na.rm=TRUE)
    ) %>%
    select(year, white_pop_sum, black_pop_sum)
  return(df)
}

#this function returns a plot of two lines, black and white race prison pop growth. 
plot_black_white_pop <- function() {
  df <- get_black_white_pop()
  p <- ggplot(df) +
    geom_smooth(aes(x=year, y=white_pop_sum, color="white"), se=F) + 
    geom_smooth(aes(x=year, y=black_pop_sum, color="black"), se=F) + 
    labs(
      title = "White Vs. Black Prison Population Growth",
      x = "Year",
      y = "Highest Jail Population",
      caption = "Figure 3: U.S. Prison Population Growth by White and Black Ethnicity",
      color = "Ethnicity"
    )
  return(p)
}

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

#this function returns a filtered df, subsetting the data to only 1995
get_1995_us_data <- function() {
  df <- trends %>%
    filter(year == 1995)
  return(df)
}

#this function returns a heatmap plot of the U.S, outlining black prison populations in each county
plot_1995_us <- function() {
  library(maps)
  df <- get_1995_us_data()
  state_shape <- map_data("county") %>% 
    unite(polyname, region, subregion, sep = ",") %>%
    left_join(county.fips, by = "polyname")
  county_shape <- state_shape %>%
    left_join(df, by = "fips")
  
  p <- ggplot(county_shape) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = black_prison_pop),
      color = "white",
      size = .1        
    ) + 
    labs(
      title = "Map of U.S Black Prison Populations in 1995",
      x = "",
      y = "",
      caption = "Figure 4: Black Prison Populations Across The Country",
      fill = "Black Prison Population"
    )
  return(p)
}