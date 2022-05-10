# ggtext practice

library(glue)

ggplot(fam, aes(x=qdate, y=cha_visit)) + 
  geom_vline(aes(xintercept=qdate[5]), size=1.2, color="darkgoldenrod2", alpha=.6) +
  geom_line(color=medium_blue, size=1, alpha=.6) + 
  geom_point(color=medium_blue, size=2.5) + 
  #  stat_smooth(color=medium_blue) +
  scale_x_date(#limits=c(as.Date("2018-01-01"), as.Date("2021-12-31")),
    date_breaks="1 year",
    date_labels="%Y") +
  scale_y_continuous(labels=comma) +
  labs(x="",
       y="",
       title="CHA worker visits declined in Jan-Mar 2022,\ncontinuing a </span style='color:red;'>>trend</span> beginning in Q3 2020") +
  annotate("text", x=as.Date("2018-12-01"), y=45000, label="SUNTA\nlaunch", color="grey60", size=4) +
  theme(plot.title.position="plot",
        plot.title=element_markdown())


data(iris)

iris_cor <- iris %>% 
  group_by(Species) %>%
  summarise(r_square = cor(Sepal.Length, Sepal.Width)^2) %>%
  mutate(
    # location of each text label in data coordinates
    Sepal.Length = 8, Sepal.Width = 4.5,
    # text label containing r^2 value 
    label = glue("r^2 = {round(r_square, 2)}")
  )

iris_cor

iris_facets <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x) +
  facet_wrap(~Species) +
  theme_bw()

iris_facets

iris_facets + 
  geom_text(
    data = iris_cor,
    aes(label = label),
    hjust = 1, vjust = 1
  )


iris_cor_md <- iris_cor %>% 
  mutate(
    # markdown version of text label
    label = glue("*r*<sup>2</sup> = {round(r_square, 2)}")
  )

iris_cor_md
#> # A tibble: 3 x 5
#>   Species    r_square Sepal.Length Sepal.Width label                 
#>   <fct>         <dbl>        <dbl>       <dbl> <glue>                
#> 1 setosa        0.551            8         4.5 *r*<sup>2</sup> = 0.55
#> 2 versicolor    0.277            8         4.5 *r*<sup>2</sup> = 0.28
#> 3 virginica     0.209            8         4.5 *r*<sup>2</sup> = 0.21

iris_facets + 
  geom_richtext(
    data = iris_cor_md,
    aes(label = label),
    hjust = 1, vjust = 1
  )



data(mtcars)

mtcars <- mtcars %>%
  mutate(
    transmission = ifelse(am == 1, "automatic", "manual"))

ggplot(mtcars, aes(hp, mpg, color = transmission)) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c(automatic = "#0072B2", manual = "#D55E00"),
    guide = "none"
  ) +
  labs(
    x = "Horse power",
    y = "Miles per gallon (MPG)",
    title = "<span style = 'font-size:14pt; font-family:Helvetica;'>Transmission type impacts fuel efficiency</span><br>
MPG is higher for <span style = 'color:#0072B2;'>automatic</span>
than for <span style = 'color:#D55E00;'>manual</span> transmissions"
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "Times"),
    plot.title.position = "plot",
    plot.title = element_markdown(size = 11, lineheight = 1.2)
  )


ggplot(mtcars, aes(hp, mpg, color=transmission)) + 
  geom_point(size=2) +
  scale_color_manual(values=c(web_blue, usaid_red),
                     guide="none") +
  labs(title="This is a test for <span style='color:#205493;'>blue</span> and a test for <span style='color:#BA0C2F;'>red</span>") +
  theme(plot.title=element_markdown())





























