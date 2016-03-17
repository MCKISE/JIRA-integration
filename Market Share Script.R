#Market Shares
#I want to create a table that links all of the site IDs to a zip code that is then
# linked to a state.
State=c("New York","CT","MA","NJ","CA","MA","NJ","CA","MA","NJ",
            "CA","MA","NJ","CA","MA","NJ","CA")
Snapshot <- read.delim("C:/Users/MCKISE/Desktop/Practice/Snapshot.txt")
Snapshot=cbind(Snapshot,State)
attach(Snapshot)
Snapshot %>%
  filter(grepl("2012",Month))%>%   #regular expression
  group_by(State,Site_ID)%>%
  summarise(Totalsales=sum(Quantity)) %>% #Going deeper dplyr at 18:30
  ggplot(mapping=aes(Site_ID,Totalsales)) +
  #geom_bar(stat="identity")
  geom_boxplot()
  
Snapshot %>%
    filter(grepl("2012",Month))%>%   #regular expression
    group_by(State)%>%
    #summarise(Totalsales=sum(Quantity)) %>%
  #ggplot(mapping=aes(State,Totalsales)) +
   # geom_bar(stat="identity")
  ggplot(mapping = aes(State,Quantity))
  geom_boxplot(varwidth = T)

  
  #If there were null values in the State field
State=c("New York",NA,"MA","NJ",NA,"MA","NJ","CA",NA,"NJ",
        "CA","MA","NJ","CA",NA,"NJ","CA")
Snapshot <- read.delim("C:/Users/MCKISE/Desktop/Practice/Snapshot.txt")
Snapshot=cbind(Snapshot,State)
attach(Snapshot)
na.omit(Snapshot)
as.numeric(Month)
Snapshot %>%
  filter(grepl("2012",Month))%>%   #regular expression
  group_by(State)%>%
  #summarise(Totalsales=sum(Quantity)) %>% #Going deeper dplyr at 18:30
  #ggplot(mapping=aes(State,Totalsales)) +
  ggplot(mapping=aes(State,Quantity))
  geom_boxplot()
  qplot(State,Quantity,facet(.~State))
