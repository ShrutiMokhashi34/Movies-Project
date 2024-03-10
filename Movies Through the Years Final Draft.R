movies <- read.csv(file.choose())
head(movies)
colnames(movies) <- c("Film", "Genre", "CriticRating", 
                      "AudienceRating", "BudgetinMillions", 
                      "YearofRelease")

head(movies)
str(movies)
tail(movies)
summary(movies)

movies$Genre <- factor(movies$Genre, 
                       levels = c("Action", "Adventure",
                                  "Comedy","Drama", "Horror",
                                  "Romance", "Thriller") )
movies$YearofRelease <- factor(movies$YearofRelease, 
                                 levels = c("2007", "2008",
                                        "2009","2010", "2011"))
library(ggplot2)

#Chart 1
w <- ggplot(data=movies, aes(x=CriticRating,
                             y=AudienceRating, 
                             colour=Genre))
w + geom_point(aes(size=BudgetinMillions)) +
  geom_smooth() +
  facet_grid(Genre~YearofRelease) +
  coord_cartesian(ylim = c(0,100))

#Chart 2
q <- ggplot(data=movies, aes(x=CriticRating, y=AudienceRating, 
                             colour=Genre, size=BudgetinMillions))
q + geom_point(aes(x=BudgetinMillions)) +
  xlab("Budget in Millions")

#Chart 3
s <- ggplot(data=movies, aes(x=BudgetinMillions))
s + geom_histogram(binwidth = 10, aes(fill=Genre)
                   ,colour="black")

#Chart 4 and #Chart 5
t <- ggplot(data=movies)
t + geom_histogram(binwidth = 10, 
                   aes(x=AudienceRating), 
                   fill="white", colour="blue") #4
t + geom_histogram(binwidth = 10, 
                   aes(x=CriticRating), 
                   fill="white", colour="blue") #5

#Chart 6
u <- ggplot(data=movies, aes(x=Genre, y=AudienceRating
                             , colour=Genre))
u + geom_jitter() + geom_boxplot(size=1.2, 
                                 alpha=0.5)
#-------------------------------------------------------------------------------------------------
moviedata <-read.csv(file.choose())
colnames(moviedata) <- c("DayofWeek", "Director", "Genre"
                         , "MovieTitle", "ReleaseDate",
                         "Studio", "AdjustedGrossinMill",
                         "BudgetinMill", "GrossinMill",
                         "IMDbRating", "MovieLensRating",
                         "Overseas$Mill", "OverseasPercent",
                         "ProfitinMill", "ProfitPercent", 
                         "RuntimeinMin", "USinMill",
                         "GrossPercentUS")

filterGenre <- moviedata$Genre=="action" |moviedata$Genre=="adventure" |moviedata$Genre=="animation" |moviedata$Genre=="comedy" |moviedata$Genre=="drama"
filterStudio <- moviedata$Studio=="Buena Vista Studios" |moviedata$Studio=="Fox" |moviedata$Studio=="Paramount Pictures" |moviedata$Studio=="Sony" |moviedata$Studio=="Universal" |moviedata$Studio=="WB"


moviedata2 <- moviedata[filterGenre & filterStudio,]

head(moviedata2)

plot <- ggplot(data=moviedata2, aes(x=Genre, y=GrossPercentUS))

q <- plot + geom_jitter(aes(size=BudgetinMill, colour=Studio)) +
  geom_boxplot(alpha=0.7, outlier.colour = NA)

q <- q + xlab("Genre") +
  ylab("Gross % US")+
  ggtitle("Domestic Gross % by Genre")

#Theme
q <- q +
  theme(
    text = element_text(family="Arial"),
    plot.title = element_text(colour="black", size=20, hjust=0.5),
    axis.title.x = element_text(colour="darkblue", size=15),
    axis.title.y = element_text(colour="darkblue", size=15),
    axis.text.x = element_text(size=15),
    axis.text.y = element_text(size=15),
    legend.title = element_text(size=15),
    legend.text = element_text(size=10)
  )

q$labels$size = "Budget $M"
warnings()
