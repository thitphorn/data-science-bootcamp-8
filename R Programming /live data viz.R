library(tidyverse)

tibble(mtcars)

ggplot(data = mtcars,
       mapping = aes(x=mpg )) +
  geom_histogram()


ggplot(data = mtcars,
       mapping = aes(x=mpg )) +
  geom_boxplot()


base <- ggplot(data = mtcars,
       mapping = aes(x=mpg ))

base + geom_histogram(bins = 8 , fill = "skyblue")
base + geom_freqpoly(bins = 8)

colors()

base2 <- ggplot(mtcars,aes(x=hp , y = mpg))

base2 +
  geom_point(size = 5 , col = "midnightblue" , alpha = 0.4)

base2 +
  geom_smooth(method = "lm" ,
              col = "black",
              fill = "gold")+
  geom_point(col = "blue")+
  theme_minimal()
  
###############################################################

tibble(diamonds)
glimpse(diamonds)
diamonds$cut %>% head
diamonds$color %>% head

ggplot(diamonds ,
       aes(price))+
  geom_boxplot() 

ggplot(diamonds ,
       aes(price))+
  geom_boxplot() +
  coord_flip()


ggplot(diamonds ,
       aes(color,carat))+
  geom_boxplot() 

ggplot(diamonds ,
       aes(cut,price))+
  geom_boxplot() 


ggplot(diamonds ,
       aes(cut,price))+
  geom_violin() 

##summarize data
diamonds %>% 
  group_by(cut) %>%
  summarise(
    med_price = median(price)
  ) %>%
  ggplot(aes(cut,med_price))+
  geom_col() + 
  theme_minimal()

##qplot
qplot(x=price,
      data=diamonds,
      geom = "histogram",
      bins = 1000)


qplot(x=carat,
      y=price,
      data = diamonds,
      geom = "point")

##############################################################
library(patchwork)
 p1 <- qplot(x=price,
             data=diamonds,
             geom = "histogram",
             bins = 10)
 p2 <- qplot(x=carat,
             y=price,
             data = diamonds,
             geom = "point")
 p3 <- qplot(x=cut,
             data = diamonds,
             geom = "bar")
p1 + p2 + p3
(p1+p2)/p3
p1/p2/p3
p1/(p2+p3)

##overplotting
ggplot(diamonds , aes(carat ,  price )) +
  geom_point(alpha = 0.1)

ggplot(diamonds , aes(carat ,  price )) +
  geom_point(shape = ".")

set.seed(42)
ggplot(sample_n(diamonds,500),
       aes(carat ,  price )) +
  geom_point(alpha = 0.5)

set.seed(42)
ggplot(sample_frac(diamonds,0.08),
       aes(carat ,  price )) +
  geom_point(alpha = 0.5)


mini_diamonds <- sample_frac(diamonds,0.02)
ggplot(mini_diamonds,
       aes(carat , price , col = cut ))+
  geom_point(alpha = 0.8)+
  theme_minimal()+
  scale_color_manual(
    values = c("pink","lightblue","gold","purple","lightgreen")
  )


ggplot(mini_diamonds,
       aes(carat , price , col = cut ))+
  geom_point(alpha = 0.8)+
  theme_minimal()+
  scale_color_brewer(type = "seq",
                     palette = 1)


ggplot(mini_diamonds,
       aes(carat , price , col = cut ))+
  geom_point(alpha = 0.8)+
  theme_minimal()+
  scale_color_brewer(type = "div",
                     palette = 4)

ggplot(mini_diamonds,
       aes(carat , price , col = cut ))+
  geom_point(alpha = 0.8)+
  theme_minimal()+
  scale_color_brewer(type = "qual",
                     palette = 8)



ggplot(mini_diamonds,
       aes(carat , price , col = price ))+
  geom_point(alpha = 0.8)+
  theme_minimal()+
  scale_color_gradient(low = "gold",
                       high = "blue")

##facet
diamonds %>%
  count(cut)

ggplot(mini_diamonds,
       aes(carat , price , col = price ))+
  geom_point(alpha = 0.8)+
  theme_minimal()+
  scale_color_gradient(low = "gold",
                       high = "blue")+
  facet_wrap(~cut , ncol = 2)


mini_diamonds40 <- sample_frac(diamonds,0.40)

ggplot(mini_diamonds40,
       aes(carat , price , col = price ))+
  geom_point(alpha = 0.8)+
  theme_minimal()+
  scale_color_gradient(low = "gold",
                       high = "blue")+
  facet_grid(cut ~ clarity)


ggplot(mini_diamonds40,
       aes(carat , price , col = price ))+
  geom_point(alpha = 0.8)+
  geom_smooth()+
  theme_minimal()+
  scale_color_gradient(low = "gold",
                       high = "blue")+
  facet_grid(cut ~ clarity)

##labels
ggplot(mtcars,aes(hp,mpg))+
  geom_point()+
  theme_minimal()+
  labs(
    title = "My first scatter plot",
    x = "Horse Power",
    y = "Miles per Gallon",
    caption = "Source : R Studio"
  )

#simple bar chart
ggplot(diamonds,aes(cut,fill = color))+
  geom_bar()+
  theme_minimal()

ggplot(diamonds,aes(cut,fill = color))+
  geom_bar(position = "stack")+
  theme_minimal()

ggplot(diamonds,aes(cut,fill = color))+
  geom_bar(position = "fill")+ 
  theme_minimal()


ggplot(diamonds,aes(cut,fill = color))+
  geom_bar(position = "dodge")+
  theme_minimal()

##install more themes 
library(ggthemes)
ggplot(diamonds,aes(cut,fill = color))+
  geom_bar(position = "stack")+
  theme_wsj()


###############################################################

mini <- sample_n(diamonds,1000)
ggplot(mini,
       aes(carat,price,col = price)) +
  geom_point(alpha = 0.5 , size = 2)+
  theme_minimal()+
  scale_color_gradient(low = "pink",
                       high = "purple")+
  labs(
    title = "scatter plot carat with price  by price" ,
    caption = "Question 1 : Which carat has the greatest distribution?"
  )
  
diamonds %>%
  count(cut)


ggplot(diamonds,
       mapping = aes(price))+
  geom_freqpoly(bins = 100 , col = "midnightblue")+
  theme_minimal()+
  labs(title = "freqpoly chart price" ,
       caption = "Question 2 : Which price range tends to be the highest?")



ggplot(diamonds,aes(clarity , fill = cut ))+
  geom_bar(position = "fill")+
  theme_minimal() +
  labs(title = "bar chart clarity by cut" ,
       caption = "Question 3 : What class of clarity is the most fair?")

ggplot(diamonds,
       mapping = aes(x=carat))+
  geom_boxplot()+
  coord_flip() +
  theme_clean()+
  labs(title = "boxplot chart carat" )

set.seed(17)
mini2 <- ggplot(sample_frac(diamonds,0.02),
                 aes(x=carat,y=price))


mini2+
  geom_smooth(method = "lm",
              col = "black",
              fill = "lightblue")+
  geom_point(col = "pink")+
  theme_clean()+
  labs(title = "sampling 2% carat with price" ,
       caption = "Question 5 : What is the trend at a 2 percent frequency?")
