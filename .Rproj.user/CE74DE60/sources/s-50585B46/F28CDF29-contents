library(plotly) # Use this for pretty graphs
library(tidyverse) # This includes the dplyr verbs and the magical pipe (%>%)
library(Lahman) # Use the built in data sets for our plots

# Read in the batting data from the Lahman library
battingData <- Batting

# Create a count of unqiue playerID's for each of the seven leagues
playersByLeague <- Batting %>%
  group_by(lgID) %>%
  summarise(
    TotalPlayers = n_distinct(playerID)
  ) 

# Create a bar plot of the counts and color it by league
# Note that whenever we are referencing a column name we need to use the '~'
plot_ly(
  data = playersByLeague,
  x = ~lgID,
  y = ~TotalPlayers,
  color = ~lgID,
  type = 'bar'
) 

# Lets recreate the previous graph but spruce it up a bit
plot_ly(
  data = playersByLeague,
  x = ~lgID,
  y = ~TotalPlayers,
  color = ~lgID,
  hoverinfo = "x+y",
  type = 'bar'
) %>%
  layout(title = "Total Unique Players by League", showlegend=F,
         xaxis = list(title = "League ID"), yaxis=list(title = "Total Players"))

# That was cool but what about over time?
# To do this we will make a line chart to display this information
# But first we must create our data set
playersByLeagueByYear <- battingData %>%
  group_by(lgID, yearID) %>%
  summarise(
    TotalPlayers = n_distinct(playerID)
  ) 


# First attempt
plot_ly(
  data=playersByLeagueByYear,
  x=~yearID,
  y=~TotalPlayers,
  color=~lgID,
  type='scatter',
  mode ='lines+markers'
)

# Clean it up some
plot_ly(
  data=playersByLeagueByYear,
  x=~yearID,
  y=~TotalPlayers,
  color=~lgID,
  hoverinfo = "text",
  text = ~paste("Year: ", yearID,
                "<br> League: ", lgID,
                "<br> Total Players: ", TotalPlayers),
  type='scatter',
  mode ='lines+markers'
) %>%
  layout(title = "Total Players by Year and League",
         xaxis = list(title="Year"),
         yaxis = list(title="Total Players"))

playersByLeagueByYearByTeam <- battingData %>%
  group_by(lgID, teamID, yearID) %>%
  summarise(
    TotalPlayers = n_distinct(playerID)
  )

plot_ly(
  data = playersByLeagueByYearByTeam,
  x = ~yearID) %>%
  add_markers(
  y = ~TotalPlayers,
  color = ~lgID,
  hoverinfo = 'text',
  text = ~paste("TeamID: ", teamID,
                "<br> Total Players: ", TotalPlayers,
                "<br> Year: ", yearID))  %>%
  add_lines(y = ~fitted(lm(TotalPlayers ~ yearID)),
            line = list(color = 'black'),
            name = "Linear Trend") %>%
  layout(title = "Total Players by Team, League, and Year", showlegend=F)

# How does the number of players that recieve an At-Bat affect the
# the number of At-Bats that players get
playerMeanAtBatsYearLeague <- battingData %>%
  group_by(lgID, yearID, teamID) %>%
  summarise(
    TotalPlayers = n_distinct(playerID),
    MeanAtBats = mean(AB)
  )

plot_ly(
  data = playerMeanAtBatsYearLeague,
  x = ~TotalPlayers,
  y = ~jitter(MeanAtBats),
  hoverinfo = 'text',
  text = ~teamID
) %>%
  add_markers(frame = ~yearID) %>%
  layout(title = "Total Players vs. Mean of At-Bats",
         xaxis = list(title = "Total Players"),
         yaxis = list(title = "Mean of At-Bats")) %>%
  animation_opts(300, easing = 'elastic', redraw = F) %>%
  animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  ) %>%
  animation_slider(
    currentvalue = list(prefix = "Year ", font = list(color="red"))
  )



p1 <- plot_ly(
  data = playerMeanAtBatsYearLeague,
  x= ~yearID,
  y= ~MeanAtBats,
  type ='scatter',
  mode='lines',
  name='Mean of At-Bats'
)

p2 <- plot_ly(
  data = playerMeanAtBatsYearLeague,
  x= ~yearID,
  y= ~TotalPlayers,
  type ='scatter',
  mode='lines',
  name = 'Total Players'
)


subplot(p1,p2, nrows = 2, shareX = T)

 ggplotToTranslate <- ggplot(playerMeanAtBatsYearLeague, aes(x=yearID, y=TotalPlayers, color = lgID)) + 
   geom_point() +
   geom_smooth(method='lm')
 

 ggplotToTranslate %>%
   ggplotly()
 
 
 playerData <- Master
 
 usPlayers <- playerData %>%
   filter(birthCountry == 'USA') %>%
   group_by(birthState) %>%
   summarise(
     Total = n_distinct(playerID)
   )
 
 plot_geo(usPlayers) %>%
   add_trace(
     z = ~Total, 
     locations = ~birthState,
     locationmode = 'USA-states',
     hoverinfo = "text",
     text = ~paste("State: ", birthState,
                   "<br> Total: ", Total)
   ) %>%
   layout(geo = list(
     scope = 'usa',
     projection = list(type = 'albers usa'),
     lakecolor = toRGB('white')
   ))
 