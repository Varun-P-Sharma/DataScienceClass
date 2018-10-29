# GGplot basics
rm(list=ls())
library(ggplot2)

# what is location-scale invariant??

## Example 1: specify everything
ggplot()+
  layer(
    data = diamonds, mapping = aes(x = carat, y =price),
    geom="point", stat = "identity", position = "identity"
  ) +
scale_y_continuous()+
scale_x_continuous()+
coord_cartesian()

## Example 2: using defaults
ggplot()+
  layer(
    data = diamonds, mapping = aes(x=carat, y = price),
    geom="point"
)

## example 3: shorthand
ggplot(diamonds, aes(carat, price))+geom_point()
# easthetics specified in the layer will override the defaults
# datasets specified in the layer will override plot default

# Example 4: add another layer and override iwth log transformations
ggplot(diamonds, aes(carat, price)) +
  geom_point() + # the layer
  stat_smooth(method = lm) +
  scale_x_log10() +
  scale_y_log10()

# Previous example without the shorthand
ggplot() +
  layer(
    data = diamonds, mapping = aes(x = carat, y = price),
    geom = "point", stat = "identity", position = "identity"
  ) +
  layer(
    data = diamonds, mapping = aes(x = carat, y = price),
    geom = "smooth", position = "identity",
    stat = "smooth", method = lm
  ) +
  scale_y_log10() +
  scale_x_log10() +
  coord_cartesian()

## qplot function
# assumes that all layers will use the same data and aesthetic mappings and defaults to scatterplot
qplot(carat, price, data = diamonds)

qplot(carat, price, data = diamonds,
      geom=c("point", "smooth"),
      method = "lm", log = "xy"
)

# making some crazy ass plot about Napolean
plot_troops <- ggplot(troops, aes(long, lat)) +
  geom_path(aes(size = survivors, color = direction,
                group = group))
plot_both <- troops_plot +
  geom_text(aes(label = city), size = 4, data = cities)
plot_polished <- both +
  scale_size(to = c(1, 10),
             breaks = c(1, 2, 3) * 10^5,
             labels = comma(c(1, 2, 3) * 10^5)) +
  scale_color_manual(values = c("grey50","red")) +
  xlab(NULL) +
  ylab(NULL)


# histograms
ggplot(data = diamonds, mapping = aes(price)) +
  layer(geom = "bar", stat = "bin",
        mapping = aes(y = ..count..))
# doesn't work?
ggplot(diamonds, aes(x = price)) + geom_histogram()
qplot(price, data = diamonds, geom = "histogram")


# data visualization tutorial
# tidyverse
library(tidyverse)
ggplot()+
  layer(
    data = mpg, mapping = aes(x = displ, y =hwy),
    geom="point", stat = "identity", position = "identity"
  ) 


ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y = hwy)) # adds a layer of points

# each geom function takes a mapping argument. Mapping argument lways paired to aes(). 

# 3.2.4 exercises-----
ggplot(data = mpg) # bank plot
dim(mpg) #234 rows, 11 columns
ggplot(data = mpg) +
  geom_point(mapping = aes(x=hwy, y = cyl))
ggplot(data = mpg) +
  geom_point(mapping = aes(x=class, y = drv)) # categorical var

# add a third variable by mapping it onto an aesthetic
# add class of car to a color
ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y = hwy, color = class))
# assignment of color to each value is known as scaling

# set colors manually (has to be outside of the aes -- it's part of the geom but not the mapping)
ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y = hwy), color = "blue")


ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y = hwy, color = year))
ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y = hwy, size = year))
ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y = hwy, shape = year)) # can't do with continuous vars


ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y = hwy, color = year, size = year)) # nothing?

ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y = hwy), stroke = 12) # size of the border

ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y = hwy, color = displ < 5)) # creates a condition to color it by...neat

# facets

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

# on a continuous var
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ cty, nrow = 2)

# other geoms
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# right
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))


# multiple geoms
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))
# if they share geometry:
ggplot(data = mpg, mapping = aes(x=displ, y = hwy))+
  geom_point()+
  geom_smooth()


ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()


ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(se = FALSE)


ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class),show.legend = FALSE) + 
  geom_smooth()
  
# plotting where the data have to be created
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))
# ggplot actually calculates the counts (summarizes the data)
# this is called a stat
?geom_bar

# what if we already have the counts?
demo <- tribble(
  ~cut,         ~freq,
  "Fair",       1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551
)

ggplot(data = demo) +
  geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")
# proportion instead of count
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))


# plotting summary stats
ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )
### ???
ggplot(data = diamonds, aes(x=cut, y = depth)) + 
  geom_pointrange(stat = "summary", fun.ymin = min, fun.ymax = max, fun.y = median)


ggplot(data = demo) + 
  geom_col(aes(x=cut, y =freq))


ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop..))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = color, y = ..prop.., group = 1))


ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))


ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity)) # automatically stacks!!
# tracking performed automatically by the position adjustment in the position agrument
# if you don't want stacked: use "identity", "dodge" or "fill"

# position ideintiy will place each object where it falls in the graph (causing overlaps)
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity")
ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(position = "identity")


# fill makes them all the same height (for comparing proportions)
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")

# dodge puts overlapping bars next to one another
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

# for overlapping points
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")

### Data transformation
library(nycflights13)
flights
# filtering
jan1 <- filter(flights, month==1, day == 1)
filter(flights, month == 11 | month == 12)
nov_dec <- filter(flights, month %in% c(11, 12))


# dealing with NAs
df <- tibble(x = c(1, NA, 3))
filter(df, x> 1) # filters out na!
filter(df, is.na(x) | x >1)

## Exercises 5.2.4
filter(flights, arr_delay >=120)
filter(flights, dest == "IAH" | dest == "HOU")
filter(flights, arr_delay > 120 & dep_delay <=0)

filter(flights, between(day, 1,10))

# arranging the order of rows
arrange(flights, year, month, day)
arrange(flights, desc(dep_delay)) # descending order

arrange(flights, desc(is.na(arr_delay))) # descending order
# arrange(flights, desc(is.na(year:arr_delay))) # descending order

# selecting columns
select(flights, year, month, day)
select(flights, year: day)

rename(flights, tail_num = tailnum)


select(flights, time_hour, air_time, everything())
# moves those columns to the front but saves other columns


vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, one_of(vars))
select(flights, day, day, year) # ignores extra
select(flights, contains("TIME")) # any columns with time in it


# add new variables
flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
)
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60
)

# summarizing data

summarise(flights, delay = mean(dep_delay, na.rm = TRUE))
#> # A tibble: 1 x 1
#>   delay
#>   <dbl>
#> 1  12.6

by_day <- group_by(flights, year, month, day)
by_day
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))


# THE PIPE
# group flights by destination and take the mean distance, delay, number of flights per destination
# filter to remove noisy points and honolulu
delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first_dep = first(dep_time), 
    last_dep = last(dep_time)
  )
