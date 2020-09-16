
# install.packages("ggplot2")
library(ggplot2)
str(mtcars)
View(mtcars)
head(mtcars)

#point
ggplot(mtcars, aes(x=factor(cyl), y=mpg))+
  geom_point()


ggplot(mtcars, aes(wt, mpg, color=disp))+
  geom_point()


ggplot(mtcars, aes(wt, mpg, size=disp))+
  geom_point()

ggplot(mtcars, aes(wt, mpg, shape=factor(cyl)))+
  geom_point()


ggplot(mtcars, aes(x=wt, y=mpg, shape=factor(cyl)))+
  geom_point()


ggplot(mtcars, aes(wt, mpg, color=factor(cyl)))+
  geom_point(shape=24, color='yellow')

?diamonds

View(diamonds)

ggplot(diamonds, aes(carat, price))+
  geom_point()+
  geom_smooth()


ggplot(diamonds, aes(carat, price))+
  geom_point()+
  geom_smooth(se=FALSE)


plt_price_vc_carat <-ggplot(diamonds, aes(carat, price))

plt_price_vc_carat+geom_point()
plt_price_vc_carat+geom_point()+geom_smooth()

ggplot(diamonds, aes(carat, price, color=clarity))+
  geom_point()+
  geom_smooth()

ggplot(diamonds, aes(carat, price, color=clarity))+
  geom_point(alpha=0.5, shape=16)+
  geom_smooth()

ggplot(mtcars, aes(factor(cyl), mpg, color=factor(am)))+
  geom_point(alpha=0.5)+
  geom_jitter(with = 0.3)

#histogram

ggplot(mtcars, aes(mpg))+
  geom_histogram(binwidth = 1)

ggplot(mtcars, aes(mpg, ..density..))+
  geom_histogram(binwidth = 1)


ggplot(mtcars, aes(mpg, fill=factor(am)))+
  geom_histogram(binwidth = 1, position="dodge")



