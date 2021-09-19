library(ggplot2)
library(DBI)
library(dplyr)
library(dbplyr)
library(ggrepel)

#Chess Games Analysis

Chess_games <- read.csv("Chess Games.csv")

#Looking at what data is stored in the set
head(Chess_games)
length(Chess_games)
colnames(Chess_games)
#16 columns 
#Lets say I want to determine, from this data, what opening will allow me to win the most
#I will have to seperate out wins for both black and white
Opening <-  Chess_games %>%
  select(winner, opening_name)

White_wins <- subset(Opening, winner == "white")
Black_wins <- subset(Opening, winner == "black")
Draws <- subset(Opening, winner == "draw")
#This just made a subset of each of the wins into who won or if was a draw
#Note: I could have done this from the initial dataset Chess_games but (out of personal preference) I wanted a set with only what I wanted

total_games <- n_distinct(Opening)
White_wins_total <- n_distinct(White_wins)
Black_wins_total <- n_distinct(Black_wins)
Draw_total <- n_distinct(Draws)
#Now I have the total amount of games in each of the categories

Percentage_White <- (White_wins_total/ total_games) *100
Percentage_Black <- (Black_wins_total/ total_games) *100
Percentage_Draw <- (Draw_total/ total_games) *100
#Now we have some data! 43.12% of the games are won by white, 41.80% won by black, and 15.09% are draws
#I am now interested in the openings of each of the wins

sapply(White_wins,function(x) length(unique(x)))
sapply(Black_wins,function(x) length(unique(x)))
sapply(Draws,function(x) length(unique(x)))

#Using sapply, we can see that for white there are 1181 unique openings, black 1145 unique openings, and draws 413 unique openings
#Lets make some graphs and tables so we can see this
White_unique<- White_wins %>%
  count(opening_name)
Black_unique<- Black_wins %>%
  count(opening_name)
Draw_unique<- Draws %>%
  count(opening_name)

ggplot(data = White_unique, aes(x = opening_name, y= n )) + geom_point()
ggplot(data = Black_unique, aes(x = opening_name, y= n )) + geom_point()
ggplot(data = Draw_unique, aes(x = opening_name, y= n )) + geom_point()

#Not very great plots, but we are able to determine that there are a few more openings that get used (or are more common)
#Now we need to sort the wins to those who have the top 10 highest wins

White_top <- head(White_unique[order(-White_unique$n),],10)
Black_top <- head(Black_unique[order(-Black_unique$n),],10)
Draw_top <- head(Draw_unique[order(-Draw_unique$n),],10)

#Now lets try graphing it (we could also just look at the data at this point)
ggplot(data = White_top, aes(x = opening_name, y= n )) + geom_point()
ggplot(data = Black_top, aes(x = opening_name, y= n )) + geom_point()
ggplot(data = Draw_top, aes(x = opening_name, y= n )) + geom_point()

#As you can see, that is a lot of different data points on this graph. For right now, lets try to only focus on 10 of them
#With this we have the openings that showed up the most for white wins, black wins, and draws
#Now, I am interested in what each of the openings has for a win_percentage for its total plays
#I want to take these top tens (ignoring draws here on out) and find the amount that they lose and calculate the win percentage when played
#This will allow us to find a opening that allows for a better winning chance (note: I wanted to do the top 10 due to the sheer amount of openings that had less than 10 wins, so they might be lacking data on them)

White_loss <- subset(Opening, winner == "black" | winner == "draw")
Black_loss <- subset(Opening, winner == "white" | winner == "draw")

White_top_names <-White_top %>%
  select(opening_name)
Black_top_names <-Black_top %>%
  select(opening_name)

White_loss_games <- subset(White_loss, opening_name %in% White_top_names$opening_name)
Black_loss_games <- subset(Black_loss, opening_name %in% Black_top_names$opening_name)

White_unique_loss<- White_loss_games %>%
  count(opening_name)
Black_unique_loss<- Black_loss_games %>%
  count(opening_name)

White_top_loss <- head(White_unique_loss[order(White_unique_loss$opening_name),],10)
Black_top_loss <- head(Black_unique_loss[order(Black_unique_loss$opening_name),],10)

White_top_re <- head(White_top[order(White_top$opening_name),],10)
Black_top_re <- head(Black_top[order(Black_top$opening_name),],10)

White_Win_loss_Ratio <- White_top_re$n / White_top_loss$n
Black_Win_loss_Ratio <- Black_top_re$n / Black_top_loss$n

#Now we have the different win loss ratios for each of the white and black most winning ratios
#Now we want to combine the different vectors into one dataset so we are able to compare them and analze them
White_names <- print(White_top_re$opening_name)
droplevels(White_names)
Black_names <- print(Black_top_re$opening_name)
droplevels(Black_names)

White_Ratios <- do.call(rbind, Map(data.frame, A = White_names, B = White_Win_loss_Ratio))
Black_Ratios <- do.call(rbind, Map(data.frame, A = Black_names, B = Black_Win_loss_Ratio))

#Now we have two different data frames with the top 10 most winning openings and their win loss ratios
#Lets make some graphs
ggplot(data = White_Ratios, aes(x = A, y= B )) + geom_point() + geom_label_repel(aes(label = A), box.padding = 0.35, point.padding = 1, segment.color = 'grey50') + theme_classic()+ theme(axis.text.x = element_blank())
ggplot(data = Black_Ratios, aes(x = A, y= B )) + geom_point() + geom_label_repel(aes(label = A), box.padding = 0.35, point.padding = 1, segment.color = 'grey50') + theme_classic()+ theme(axis.text.x = element_blank())

#From here we are able to make some pretty clear assumptions about our winning potential based on the openings played
#Important to note that while these are the openings that won the most, they might not be the best due to having a fairly low win loss ratio (ex: Scotch Game for Black)
#Openings that are benfitical to play are: Philidor Defense #3 (White), Scandinavian Defense: Mieses-Kotroc Variation (White), Van't Kruijs Opening (Black), and (Scandinavian Defense)

#Now we need the openings with the highest win loss ratios
#Take the unique values of win (or loss) for the opening names and check to see if it has matching values in its componet. 
White_A <- head(White_loss[order(White_loss$opening_name),],10057)
Black_A <- head(Black_loss[order(Black_loss$opening_name),],10951)
White_B <- head(White_wins[order(White_wins$opening_name),],10001)
Black_B <- head(Black_wins[order(Black_wins$opening_name),],9107)
White_c <- White_A %>%
  count(opening_name)
Black_C <- Black_A %>%
  count(opening_name)
White_D <- White_B %>%
  count(opening_name)
Black_D <- Black_B %>%
  count(opening_name)
#Now we have to deal with the openings that only have one game (meaning it only appears once)
White_E <- merge(White_c,White_D, by = "opening_name")
Black_E <- merge(Black_C, Black_D, by = "opening_name")

White_Ratio_Total <- White_E$n.y/White_E$n.x
Black_Ratio_Total <- Black_E$n.y/Black_E$n.x

White_Final <- do.call(rbind, Map(data.frame, "Opening" = White_E$opening_name, "Win/Loss Ratio" = White_Ratio_Total))
Black_Final <- do.call(rbind, Map(data.frame, "Opening" = Black_E$opening_name, "Win/Loss Ratio" = Black_Ratio_Total))

#Now we have  a list of all the ratios of chess openings that have both a win and a loss
#All That is left is to order the dataframe and plot the top openings!

