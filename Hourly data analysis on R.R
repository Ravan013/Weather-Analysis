#Sami Bala Shrestha
#NP000419

#creating objects
data_file<- read.csv("E:/4. Hourly weather data.csv") #data_file as an string reads location of data
#printing object data
print(data_file)

#including libraries
library("ggplot2")
library("dplyr")
library("tidyverse")
data_file=read.csv("E:/4. Hourly weather data.cs4v")
data_file = read.csv("E:/4. Hourly weather data.csv")


  #First pre-processing
    #Filling missing data in wind_gust and pressure by Month
    windgustmean = data_file %>% group_by(month)%>% summarise(Mean = mean(wind_gust, na.rm = TRUE))
    pressuremean= data_file %>% group_by(month)%>% summarise(Mean = mean(pressure, na.rm = TRUE))
    windg_test = merge(data_file, windgustmean, by = "month")
    pressurem_test = merge(data_file, pressuremean, by = "month")
    
    # add the mean value column into original"data_file" data set 
    data_file$wind_gust[is.na(data_file$wind_gust)] = windg_test$Mean[is.na(data_file$wind_gust)]
    data_file$pressure[is.na(data_file$pressure)] = pressurem_test$Mean[is.na(data_file$pressure)]
    
    # use to check whether it still having remaining null value 
    sapply(data_file, function(data_file) sum(is.na(data_file)))
    
    # remove the duplicate data set that has taken the mean and replace into "data_file" NA value
    remove(windg_test)
    remove(pressurem_test)

    #-add season column based on day and month
    data_file=mutate(data_file, season = 
                       if_else((month==3&day>=20)|month==4|month==5|(month==6&day<=19),1,
                               if_else((month==6&day>=20)|month==7|month==8|(month==9&day<=21),2,
                                       if_else((month==9&day>=22)|month==10|month==11|(month==12&day<=20),3,4))))
    
   

  # To remove remaining NA value by inserting mean values (wind_dir &wind_speed)
  for (i in which(sapply(data_file, is.numeric))) { 
    data_file[is.na(data_file[, i]), i] = mean(data_file[, i],  na.rm = TRUE) 
  }
  # use to check whether it still having remaining null value in that column
  sapply(data_file, function(data_file) sum(is.na(data_file)))
  
  # Check outlier
  cleantemperature= data_file%>%filter(month==5, day==8,origin =="JFK")
  # To delete the row that are outlier
  df = cleantemperature[-23,]
  #To calculate the mean of temperature in df
  mean(df$temp)
  #check which row number that are outlier in main datasets
  which(data_file$temp==13.1)
  #replace mean
  data_file[3065,6]=mean(df$temp)
  remove(cleantemperature)
  remove(df)




#Analysis
# Analysis no.1: Best period for holiday on the basis of temperature
#-a. Data Manipulation
data_file %>% group_by(season) %>% summarise(avg_temp=mean(temp),
                                             max_temp=max(temp),
                                             min_temp=min(temp), .groups='drop') #.group='drop' to remove attributes from summarize()
#-b. Data Visualization
ggplot(data_file, aes(season, temp, fill=season)) + geom_boxplot(outlier.color='green',aes(group=season))




#Analysis no. 2: Average humidity at JFK and LGA on individual months.
#-a. Data Manipulation
jfk_humidity=data_file %>% filter(origin=="JFK") %>% group_by(month) %>% #JFK session
  summarise(humid=mean(humid),origin, .groups='drop')

lga_humid=data_file %>% filter(origin=="LGA") %>% group_by(month) %>% #LGA session
  summarise(humid=mean(humid),origin, .groups='drop')

#-b. Data Visualization
ggplot() + 
  geom_line(color='green',data = jfk_humidity, aes(x = month, y = humid)) +
  geom_line(color='blue',data = lga_humid, aes(x = month, y = humid)) + 
  scale_x_continuous(breaks=1:12,labels=c("jan","feb","mar","apr","may","jun", #Months in characters
                                          "jul","aug","sep","oct","nov","dec")) + 
  labs(title="LGA and JFK Humidity in a year", y="Humidity (%)",x="Month") 


#Analysis no. 3: Rainy days 
#-a. Data Manipulation
#--Let us assume rain is at humidity of 100
jfk_rain=data_file %>% filter(humid==100, origin=="JFK") %>% group_by(month) %>% #JFK session
  summarise(rain=n_distinct(day),origin,.groups='drop') #All rain grouped as one then dropped
lga_rain=data_file %>% filter(humid==100, origin=="LGA") %>% group_by(month) %>% #LGA session
  summarise(rain=n_distinct(day),origin,.groups='drop') #All rain grouped as one then dropped

#-b. Data Visualization
ggplot() + 
  geom_point(data=jfk_rain, aes(x = month, y = rain, color=origin),size=3) + #JFK session
  geom_point(data=lga_rain, aes(x = month, y = rain, color=origin),size=3) + #LGA session
  annotate("rect", xmin=3.6, xmax=6.6, ymin=1 , ymax=6, alpha=0.1, fill="green")+ #Summer time
  scale_x_continuous(breaks=1:12,labels=c("jan","feb","mar","apr","may","jun",
                                          "jul","aug","sep","oct","nov","dec"))+ 
  scale_y_continuous(breaks=1:7) + labs(title="Number of Rainy Days in A Year", 
                                        x="Month", y="Rainy Days") #1:12 as in months and 1:7 as in days


#Analysis no.4: Temperature in Summer at Queens
#-a. Data Manipulation
summer_temp=data_file %>% filter(season==2) %>% group_by(day) %>% 
  summarise(temp=max(temp),.groups='drop')

#-b. Data Visualization
ggplot(summer_temp, aes(day, temp))+geom_line(aes(color=temp))+geom_point(aes(color=temp))+
  scale_x_continuous(breaks=1:31)+geom_hline(yintercept=90,color="orange")+ #show 31 days and make a warning line
  labs(title="Temperature in Summer at Queens 2013",subtitle="Maximum temperature per day averaged",
       y="Temperature(F)",x="Day",color="Temperature") 


#5: Temperature and Wind Speed in Winter
#-a. Data Manipulation
winter_wind=data_file%>%filter(season==4)%>%select(temp,wind_speed) 
summary(winter_wind) #show summary for temperature and wind speed in winter

#-b. Data Visualization
ggplot(winter_wind, aes(temp,wind_speed)) + geom_point(na.rm=TRUE)+ #remove any NA value
  geom_vline(xintercept=10,color="blue",alpha=0.3)+ #frostbite potential temperature
  geom_hline(yintercept=55,color="blue",alpha=0.3)+ #frostbite potential wind speed
  annotate("pointrange", x=10, y=55, ymin=55, ymax=55,colour ="blue", size=0.77, alpha=0.6)+
  labs(title="Relationship between Temperature and Wind Speed",subtitle="Winter 2013",
       x="Temperature(F)",y="Wind Speed(mph)")+
  geom_text(label="Frostbite potential", x=16,y=56.5,color="blue")


#Analysis no.6: Temperature per seasons at Queens
#-a. Data Manipulation
data_file%>%select(season,temp)%>%group_by(season)%>%summarise(avg_temp=mean(temp),.groups='drop')
#-b. Data Visualization
ggplot(data_file, aes(x = temp, y=frequency(temp), fill = factor(season))) + 
  geom_bar(width=0.18,stat = "identity")+ theme_classic() + labs(fill="Season", 
                                                            x="Temperature(F)",
                                                            y="Count",
                                                            title="Temperature per season",
                                                            subtitle = "Queens  2013")


#Analysis no.7: Average temperature compared for both airports
  #-a. Data Manipulation
  jfk_temp=data_file %>% filter(origin=="JFK") %>% group_by(month) %>% 
    summarise(temp=mean(temp),origin, .groups='drop') #temp for JFK averaged
  lga_temp=data_file %>% filter(origin=="LGA") %>% group_by(month) %>% 
    summarise(temp=mean(temp),origin, .groups='drop') #temp for LGA averaged
  
  #-b. Data Visualization
  ggplot() + 
    geom_line(data = jfk_temp, aes(x = month, y = temp, color=origin)) +
    geom_line(data = lga_temp, aes(x = month, y = temp, color=origin)) + 
    scale_x_continuous(breaks=1:12,labels=c("jan","feb","mar","apr","may","jun",
                                            "jul","aug","sep","oct","nov","dec")) + 
    labs(title="Average Temperature in a year compared between LGA and JFK", y="Temperature(F)",x="Month") 


#Analysis no.8: Finding Best Indoor Temperature for each month 
  #-a. Data Manipulation
  #--show temperature summary for each month
  data_file%>%select(month,temp)%>%group_by(month)%>%summarise(avg_temp=mean(temp),
                                                               max_temp=max(temp),
                                                               min_temp=min(temp),.groups='drop')
  
  #-b. Data Visualization
  monlab=c("1"="jan","2"="feb","3"="mar","4"="apr","5"="may","6"="jun","7"="jul",
           "8"="aug","9"="sep","10"="oct","11"="nov","12"="dec") #monolab is for labeling
  ggplot(data_file, aes(x=temp))+geom_histogram(bins=30,aes(fill = factor(season)))+
    facet_wrap(~month,labeller = labeller(month = monlab)) +
    geom_vline(xintercept=64,color="black",alpha=0.5) + #safe and well-balanced indoor temperature line
    labs(title="Average LGA and JFK Temperature per month", y="Count",x="Temperature(F)",fill="Season",
         subtitle="2013") #Temperature around 64 F is good for indoor temperature, thus Sep and Oct is the best


#Analysis no.9: Wind Speed and Wind Direction affecting Flights at JFK
  #-a. Data Manipulation
  jfk_hwind=data_file%>%filter(origin=="JFK")%>%
    
    #Calculating headwind
    mutate(knot_speed=wind_speed/1.151,headwind=knot_speed*cos(wind_dir-45))%>% 
    select(month,knot_speed,wind_dir,headwind)%>%filter(headwind>=0) #convert mph into knotspeed
  
    jfk_hwind%>%group_by(month)%>%summarise(hwind_avg=mean(headwind),
                                            hwind_max=max(headwind),.groups='drop')#--Average and maximum headwind per month
  #-b. Data Visualization
  ggplot(lga_hwind, aes(month,headwind))+geom_boxplot(aes(group=month), outlier.colour = 'red')+
    scale_x_continuous(breaks=1:12,labels=c("jan","feb","mar","apr","may","jun",
                                            "jul","aug","sep","oct","nov","dec"))+
    labs(title="JFK Airport Headwind 2013", y="Headwind",x="Month",subtitle="Runway 4") 


#Analysis no.10: Finding possibility of heat stroke in Summer
  #-a. Data Manipulation
    #Potential heat stroke starting from 86F
    summer_dewp=data_file%>%filter(season==2, temp>=86)%>%select(time_hour,dewp,temp)
    #--Heat index indicating heat stroke potential (>104)
    heatindex=data.frame(temp=c(90:100),dewp=c(78,77,76,75,76,73,72,71,70,68,68))
  
  #-b. Data Visualization
    ggplot() +
      geom_point(data=summer_dewp,aes(x=dewp,y=temp))+
      geom_line(data=heatindex, aes(x=dewp,y=temp),color="red",size=2,alpha=0.2)+ #heatstroke potential line
      scale_x_continuous(breaks=53:80)+scale_y_continuous(breaks=86:100)+
      labs(title="Heat Stroke Potential in Summer 2013", y="Temperature(F)",x="Dew Point(F)")


#Analysis no.11: Temperature and dew point relationship 
  #-a. Data Manipulation
  cor(data_file$temp,data_file$dewp) #correlation between temperature and dew point
  #-b. Data Visualization
  ggplot(data_file,aes(dewp,temp))+geom_jitter()+geom_smooth(method="lm")+
    labs(title="Correlation Between Temperature and Dew Point", y="Temperature(F)",x="Dew Point(F)")


#Analysis no. 12: Precipitation and temperature during Winter
  #-a. Data Manipulation
  winter=data_file%>%filter(season==4,precip>0)
  #--to show the days
  data_file%>%filter(season==4,precip>0,temp<=32)
  #-b. Data Visualization
  ggplot(winter, aes(temp,precip))+geom_count(alpha=0.2,color="blue") + facet_wrap(~hour)+
    
  annotate("rect", xmin=15, xmax=32, ymin=0.0 , ymax=0.6, alpha=0.1, fill="blue")+
  labs(title="Hourly Temperature and Precipitation", 
         y="Precipitation(in)",x="Temperature(F)",subtitle = "Winter, 2013") #here goes snowfall


#Analysis no.13: Pressure summary in LGA and JFK
  #-a. Data Manipulation
  #Summarizing Pressure for both airports
  data_file%>%filter(origin=="JFK")%>%select(jfk_pressure=pressure)%>%summary() #Summarizing Pressure for JFK
  data_file%>%filter(origin=="LGA")%>%select(lga_pressure=pressure)%>%summary() #Summarizing Pressure for LGA
  #-b. Data Visualization
  ggplot(data_file,aes(pressure))+geom_histogram(bins=20,aes(group=origin,fill=origin),na.rm=T)+
    labs(title="Pressure histogram", 
         y="count",x="Pressure",subtitle = "2013")

#14: Crosswind summary
  #-a. Data Manipulation
    #For LGA
    lga_cwind=data_file%>%filter(origin=="LGA")%>%
      #convert mph into Knot & calculate crosswind for Runway 4
      mutate(knot_speed=wind_speed/1.151,crosswind=knot_speed*sin(wind_dir-45))%>% 
      filter(crosswind>0)%>%select(origin,month,knot_speed,wind_dir,crosswind)
    #--show average and maximum crosswind at LGA
    lga_cwind%>%summarise(cwind_avg=mean(crosswind,na.rm=T),cwind_max=max(crosswind,na.rm=T),.groups='drop')
    
    #For JFK
    jfk_cwind=data_file%>%filter(origin=="JFK")%>%
      #convert mph into Knot & calculate crosswind for Runway 13R
      mutate(knot_speed=wind_speed/1.151,crosswind=knot_speed*sin(wind_dir-133))%>% 
      filter(crosswind>0)%>%select(origin,month,knot_speed,wind_dir,crosswind)
    #--show average and maximum crosswind at JFK
    jfk_cwind%>%summarise(cwind_avg=mean(crosswind,na.rm=T),cwind_max=max(crosswind,na.rm=T),.groups='drop')
  
  #-b. Data Visualization
  ggplot()+
    geom_jitter(data = jfk_cwind, aes(y=crosswind,x=month,color=origin)) +
    geom_jitter(data = lga_cwind, aes(y=crosswind,x=month,color=origin)) +
    scale_x_continuous(breaks=1:12,labels=c("jan","feb","mar","apr","may","jun",
                                            "jul","aug","sep","oct","nov","dec"))+
    scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35)) +
    geom_hline(yintercept=30,color="darkred",size=2,alpha=0.2)+
    labs(title = "Jitterplot for Crosswind in 2013", subtitle="JFK: Runway 13R | LGA: Runway 4")


# Additional feature 1: Temperature and humidity in average for each Month

humidtempmonth <- obj %>% group_by(month) %>% summarise(temp_mean = mean(temp, na.rm = TRUE),
                                                        humid_mean = mean(humid, na.rm = TRUE))
ggplot(humidtempmonth) + 
  geom_col(aes(x = month, y = temp_mean), size = 1, color = "black", fill = "skyblue") +
  geom_line(aes(x = month, y = humid_mean), size = 1, color="purple") + 
  geom_point(aes(x = month, y = humid_mean), size = 1, color="red")+
  scale_x_continuous(breaks=(1:12))+
  scale_y_continuous(name="Average temperature",sec.axis = sec_axis(~./1, name = "Average humidity"))+
  ggtitle("Average temperature and humidity in each Month")+
  theme_classic()


# Additional feature 2: Temperature distribution in each month
#violin plot 
ggplot(obj, aes(y=temp, x=month)) + 
  geom_violin(aes(group=cut_width(month,1)),scale="width",alpha=2,fill="orange") +
  ylab("Temperature") +xlab("month")+scale_x_continuous(breaks=(1:12))+
  ggtitle("Distribution of temperature on each Month")


