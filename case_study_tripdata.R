
# hot months data
march_df <- read_csv('202203-divvy-tripdata.csv')
april_df <- read_csv('202204-divvy-tripdata.csv')
may_df <- read_csv('202205-divvy-tripdata.csv')
june_df <- read_csv('202206-divvy-tripdata.csv')
july_df <- read_csv('202207-divvy-tripdata.csv')

#merging datasets of hot months into a single data frame
data_list <- list(march_df,april_df,may_df,june_df,july_df)
merge_df <- function(df1,df2){merge(df1,df2)}
data_03_07_22 <- Reduce(merge_df,data_list)


data_03_07_22 <- data_03_07_22%>%
  filter(!is.na(start_station_name))
#removed the empty rows

# adding a column ride_length to show the difference between both
data_03_07_22 <- data_03_07_22 %>% 
  mutate(ride_length = started_at-ended_at)

# casual_member average rides
casual_member_avg_rides <- data_03_07_22 %>% 
  group_by(member_casual) %>% 
  summarize(avg_ride_length= mean(-ride_length)) %>% 
  mutate(avg_ride_length= minute(seconds_to_period(avg_ride_length)))

# plot for member_casual average rides
ggplot(data = casual_member_avg_rides,aes(x =member_casual,y=avg_ride_length))+
  geom_bar(stat="identity",aes(fill=member_casual))+
  geom_text(aes(label=paste0(avg_ride_length,' ','minutes'),vjust=-0.25))


#counting casual and member riders and percentage
rider_type <- data_03_07_22 %>% 
  count(member_casual) %>% 
  mutate(per=n/sum(n),percentage=paste0(round(per*100),'%'))

# bar plot for riders type against percentage
ggplot(data = rider_type,aes(x =member_casual,y=percentage))+
  geom_bar(stat="identity",aes(fill=member_casual))+
  geom_text(aes(label=percentage,vjust=-0.25))

#stations  where most rides were started
 station_most_casual_riders <-data_03_07_22 %>% 
   filter(member_casual=='casual') %>% 
   count(start_station_name) %>% 
   mutate(stations_rides=n) %>% 
   arrange(-stations_rides) %>% 
   head()
 
 #plot for rides on most starting stations
 ggplot(data = station_most_casual_riders,aes(x =start_station_name,y=stations_rides))+
   geom_bar(stat="identity",aes(fill=start_station_name))+
   geom_text(aes(label=stations_rides,vjust=-0.25))+
   theme(axis.text.x = element_text(angle = 30))+
   labs(title = " mostly casual rides on stations",
   caption = "data is taken from divvy_tripdata.s3amazonaws.com")+
   theme(plot.title=element_text(hjust=0.5),plot.caption=element_text(hjust=0.5))
 
   
   
   
 
 #finding rides per day for casuals
 
 per_day <- data_03_07_22 %>% 
   filter(member_casual=='casual') %>% 
   count(weekday) %>% 
   mutate(rides=n)
 
 # plot for casual rides per day
 ggplot(data = per_day,aes(x =weekday,y=rides))+
   geom_bar(stat="identity",aes(fill=weekday))+
   geom_text(aes(label=rides,vjust=-0.25))+
   labs(title = " Casual Rides each day",
        caption = "data is taken from divvy_tripdata.s3amazonaws.com")+
   theme(plot.title=element_text(hjust=0.5),plot.caption=element_text(hjust=0.5))
 
   # cold months data 
    df_oct <- read_csv('202210-divvy-tripdata.csv')
    df_nov <- read_csv('202211-divvy-tripdata.csv')
    df_dec <- read_csv('202212-divvy-tripdata.csv')
    df_jan <- read_csv('202201-divvy-tripdata.csv')
    df_feb <- read_csv('202202-divvy-tripdata.csv')
    
    # merging months dataset into a single dataframe
    list_cold_months <- list(df_oct,df_nov,df_dec,df_jan,df_feb)
    merge_function_cold_months <- function(df1,df2){merge(df1,df2,all=TRUE)} 
    data_10_02_22 <- Reduce(merge_function_cold_months,list_cold_months)
    
    # removing empty column's observations which may effect our analysis
    data_10_02_22 <- data_10_02_22 %>% 
      filter(!is.na(start_station_name))
    
    #line trend between hot and cold months
    
    #hot months
    data_hot_months <- data_03_07_22 %>% 
      filter(member_casual=='casual') %>% 
      mutate(month= month(started_at, label=TRUE, abbr= TRUE)) %>% 
      count(month) %>%
      mutate(riders= n) %>% 
      arrange(month)
    
    
    #cold months
    data_cold_months <- data_10_02_22 %>% 
      filter(member_casual=='casual') %>% 
      mutate(month= month(started_at, label=TRUE, abbr= TRUE)) %>% 
      count(month) %>%
      mutate(riders= n) %>% 
      arrange(month)
    
    #merging both hot&cold months data
    both_hot_cold <- merge(data_hot_months,data_cold_months,all=TRUE)
    
    
    # line plot for showing trend between hot and cold months
    
    ggplot(data=both_hot_cold,aes(x=month,y=riders,color=riders))+
      geom_point()+
      labs(title = " Casual Riders according to each month",
           caption = "data is taken from divvy_tripdata.s3amazonaws.com")+
      theme(plot.title=element_text(hjust=0.5))
      
      
    
      
    
    
              