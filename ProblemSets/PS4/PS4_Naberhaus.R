#I was unable to get this to work

library(jsonlite)
system("linux shell command")
wget -o stats.json "http://api.fantasy.
nfl.com/v1/players/stats?statType=seasonStats&season=2010&week=1&format=json"
#I was unable to download using wget, but I downloaded the JSON from the URL
cat stats.json
mydf <- fromJSON('stats.json')
class(mydf$players)
head(mydf$players)
