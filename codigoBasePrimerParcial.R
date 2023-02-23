library(ggplot2)
library(dplyr)
library(ggthemes)
#More fancy ggplot themes
library(hrbrthemes)
setwd("/Users/joaquinalvarez/Downloads/InferenciaCausalPrimerParcial")
getwd()
data=read.csv('tmpdfeo3qy2.csv', header = T, stringsAsFactors = T)
str(data)
View(data)
names(data)
data[,9]
#ok year is an irrelevant variable!
#we already know that we are analyzing data from 2022
data[,4]
data=data[,-9] #we remove the column associated to year
data
str(data)
data$OFFENSE_CODE_GROUP
#this is also a garbage variable

data=subset(data, select=-OFFENSE_CODE_GROUP)
str(data)
data[data$SHOOTING==1,]
data=subset(data, select=-UCR_PART) # a garbage variable for the purposes of the analysis
head(data)
names(data)
#we eliminate all the variables that we are definitely not going to use.
data$DISTRICT
data$REPORTING_AREA
head(data)
summary(data)
data=subset(data, select=-REPORTING_AREA) # a garbage variable
#for our purposes.
summary(data)
names(data)

#Graffiti!
data[data$OFFENSE_CODE==1415,]

levels(data$DISTRICT) 
#the first one and the last one can be removed
length(levels(data$DISTRICT))
levels(data$DISTRICT)[2:13]
districts=levels(data$DISTRICT)[2:13]
length(districts)
number_of_crimes=c()
#number of crimes per district in 2022
for(i in 1:length(districts)){
  number_of_crimes[i]=dim(data[data$DISTRICT==districts[i],])[1]
}
number_of_crimes

#we can plot the number of crimes per district
#and we can create a heat map

crimesPerDistrict=data.frame(district=districts,number_of_crimes=number_of_crimes)
crimesPerDistrict
p <- ggplot(crimesPerDistrict, aes(x=district, y=number_of_crimes, fill=district)) + 
  geom_bar(stat="identity", position=position_dodge()) 
p
p + scale_fill_brewer(palette="Greens") + theme_minimal()
p+theme_minimal()
p + scale_fill_brewer(palette="Paired") + theme_minimal()
p+theme_ipsum()
ggplot(crimesPerDistrict, aes(x=district, y=number_of_crimes)) + 
  geom_bar(stat = "identity")

ggplot(crimesPerDistrict, aes(x=district, y=number_of_crimes)) + 
  geom_bar(stat = "identity")
ggplot(crimesPerDistrict, aes(x=reorder(district, -number_of_crimes), y=number_of_crimes)) + 
  geom_bar(stat = "identity", fill='tomato')+
  labs(x='District', y='Number of crimes',
       title='Number of crimes per district')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size = 20))
  


#ordering the values

ggplot(crimesPerDistrict, aes(x = reorder(district, -number_of_crimes), y =number_of_crimes)) + 
  geom_bar(stat = "identity")+theme_hc()+scale_colour_hc()

#More on bar plots
ggplot(crimesPerDistrict, aes(x=district, y=number_of_crimes)) + 
  geom_bar(stat = "identity")

#ordering the values
ggplot(crimesPerDistrict, aes(x = reorder(district, -number_of_crimes), y =number_of_crimes)) + 
  geom_bar(stat = "identity")+theme_wsj()

ggplot(crimesPerDistrict, aes(x = reorder(district, -number_of_crimes), y =number_of_crimes)) + 
  geom_bar(stat = "identity")

#Crimes by hour


data$OCCURRED_ON_DATE

#now lets create a time series counting the number of crimes registered per day.

date=seq(as.Date("2022-01-01"), as.Date("2022-12-31"), by="day")
date
length(date)

data$OCCURRED_ON_DATE
data$OCCURRED_ON_DATE[1]

substr(data$OCCURRED_ON_DATE[1], 1,5)
substr(data$OCCURRED_ON_DATE[1], 1,10)==date[1]
#great!!!! we'll use this in order to count the number of crimes per day 
#and generate a time series.
substr(data$OCCURRED_ON_DATE, 1,10)
data[substr(data$OCCURRED_ON_DATE, 1,10)==date[1],]
data[substr(data$OCCURRED_ON_DATE, 1,10)==date[2],]
counts=c()
#number of crimes per day
for(k in 1:365){
  print(k)
  counts[k]=dim(data[substr(data$OCCURRED_ON_DATE, 1,10)==date[k],])[1]
}
counts
crimes_per_day=data.frame(day=date, num_crimes=counts)
crimes_per_day
ggplot(crimes_per_day, aes(x=day, y=num_crimes))+
  geom_line(col='blue', size=1.5)+
  labs(x='Day of the year', y='Number of crimes',
       title='Number of crimes for each day of 2022')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size = 20))




crimes_per_day[crimes_per_day$day=='2022-01-02',]
which(crimes_per_day$day=='2022-01-02')
which(crimes_per_day$day=='2022-02-03')
which(crimes_per_day$day=='2022-04-01')
crimes_per_day[crimes_per_day$day=='2022-04-18',]
crimes_per_day[crimes_per_day$day=='2022-04-17',]
crimes_per_day[crimes_per_day$day=='2022-04-19',]
crimes_per_day[crimes_per_day$day=='2022-04-20',]
crimes_per_day[crimes_per_day$day=='2022-04-22',]
mean(crimes_per_day$num_crimes)
sd(crimes_per_day$num_crimes)

plot(density(crimes_per_day$num_crimes))


ggplot(crimes_per_day, aes(x=num_crimes))+
  geom_density(color="darkblue", fill="lightblue")+
  labs(x='number of reports (crimes)', y='density',
       title='Distribution of daily crimes')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size = 20))

ggplot(crimes_per_day[90:120,], aes(x=day, y=num_crimes))+
  geom_line(col='orange', size=1.5)+
  annotate("text", x=as.Date("2022-04-18"), y=170, label= "marathon")+
  labs(x='Day', y='Number of crimes',
       title='Number of crimes during April 2022')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size = 20))+
  geom_hline(yintercept=mean(crimes_per_day$num_crimes), linetype="dashed", color = "red")+
  geom_point(aes(x=as.Date("2022-04-18"),y=167),colour="blue", cex=2)
#it is interesting to observe that the number 
#of crimes is very low during the weekend  of the marathon.
#The Boston Marathon of 2022 was in April 18, 2022.
#great!!!!!

#It's important to keep in mind that the Boston Marathon is a big event that involves a lot of tourism and activities throughout the whole city. Moreover, in the 2013 Boston Marathon there was a terrorist attack that consisted of a bombing close to the finish line of the race. Thus, it should be no surprise that the authorities take security during this event very seriously ever since......LINKS etc.


#links curiosos de maraton de Boston con info:
#https://www.bu.edu/articles/2019/boston-marathon-security/
#https://www.domesticpreparedness.com/preparedness/security-lessons-learned-part-1-boston-marathon-bombings/
#https://www.boston25news.com/news/local/boston-marathon-security-plans-pay-off/4L5ALBZ3BFBUZJS3CLCUN5FONY/
#https://www.boston.gov/news/2022-boston-marathon-public-safety-preparations-announced




#Next we explore number of crimes per day of the week
summary(data)
#already shows number of crimes per day of the week but
#we can implement similar ideas to construct bar plots

day=c('Monday', 'Tuesday', 'Wednesday','Thursday', 'Friday',
      'Saturday', 'Sunday')
number_of_crimes=c()
k=1
data[data$DAY_OF_WEEK=='Monday',]
for(i in day){
  number_of_crimes[k]=dim(data[data$DAY_OF_WEEK==i,])[1]
  k=k+1
}
number_of_crimes
crimes_dayOfWeek=data.frame(day=day, number_of_crimes=number_of_crimes)

ggplot(crimes_dayOfWeek, aes(x=day,y=number_of_crimes))+
  geom_bar(stat = "identity")

?geom_bar
crimes_dayOfWeek[order(number_of_crimes, decreasing = T),]
ggplot(crimes_dayOfWeek[order(number_of_crimes, decreasing = T),], aes(x=day,y=number_of_crimes))+
  geom_bar(stat = "identity")

ggplot(crimes_dayOfWeek, aes(x=reorder(day, -number_of_crimes),y=number_of_crimes))+
  geom_bar(stat = "identity", fill='salmon')+
  labs(x='Day of the week', y='Total number of crimes',
       title='Number of crimes by day of the week')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size = 20))
  




#faltaria ordenar por dia de la semana
#si no se ve muy caotico


## Most dangerous streets in Boston

data$STREET
typeof(data$STREET)
as.factor(data$STREET)
unique(as.factor(data$STREET))
x=unique(as.factor(data$STREET)) #get all streets in the data set
x=as.character(x)
x
#withput repetitions
head(x)
length(x)
#8378 streets!
head(data)

data$STREET[1]==x[1]
#let's identify the top 10 streets with most crimes in Boston
length(x)
counting=c()
k=1
for(j in x){
  print(k)
  counting[k]=dim(data[data$STREET==j,])[1] #extract the number of crimes in the j-th street
  k=k+1
}
counting
counting[1]
print(counting[1])
streets=data.frame(street=x, crimes=counting)
head(streets)
tail(streets)
#streets=streets[order(as.integer(number_of_crimes), decreasing = T),]
#streets
#no me pelo!!!
str(streets)
summary(streets)
streets=streets[order(streets$crimes, decreasing = T),]
streets
head(streets)
tail(streets)
str(streets)

top10=streets[1:10,]
top10


ggplot(top10, aes(x=street, y=crimes))+
  geom_bar(stat='identity')+coord_flip()


#it would be nice to plot the locations and create heat maps for the respective coordinates !!!!!
data$DISTRICT
unique(data$DISTRICT)
#we remove the external observation
length(unique(data$DISTRICT))
unique(data$DISTRICT)[1:12]
levels(data$DISTRICT)
districts=levels(data$DISTRICT)[2:13]
length(districts)
districts
counts=matrix(nrow=365, ncol=length(districts))
counts=as.data.frame(counts)
names(counts)
names(counts)=districts
fancyPlot=cbind(date,counts)
fancyPlot
names(fancyPlot)
#now we count the number of observations per district
data[substr(data$OCCURRED_ON_DATE, 1,10)==date[3],]
data[substr(data$OCCURRED_ON_DATE, 1,10)==date[3]&(data$DISTRICT==districts[2]),]
t1 <- Sys.time()
for(k in 1:365){ #for each day, count each of the crimes per district
  print(k)
  for(j in 1:length(districts)){
    fancyPlot[k,(j+1)]=dim(data[substr(data$OCCURRED_ON_DATE, 1,10)==date[k]&(data$DISTRICT==districts[j]),])[1]
  }
}
t2 <- Sys.time()
t2-t1
#Time difference of 15.45458 mins
head(fancyPlot)

#Useful link to undertand Box Plots!
#https://r-graph-gallery.com/boxplot.html
p=ggplot(data=fancyPlot, aes(x=date))+
  geom_line(aes(y=A1, group =1L, color = "A1"))+ 
  geom_line(aes(y=A15, group =1L, color = 'A15'))+ 
  geom_line(aes(y=A7, group =1L, color = "A7"))+ 
  geom_line(aes(y=B2, group =1L, color = "B2"))+ 
  geom_line(aes(y=B3, group =1L, color = "B3"))+ 
  geom_line(aes(y=C11, group =1L, color = "C11"))+ 
  geom_line(aes(y=C6, group =1L, color = "C6"))+ 
  geom_line(aes(y=D14, group =1L, color = "D14"))+ 
  geom_line(aes(y=D4, group =1L, color = "D4"))+ 
  geom_line(aes(y=E13, group =1L, color = "E13"))+
  geom_line(aes(y=E18, group =1L, color = "E18"))+
  geom_line(aes(y=E5, group =1L, color = "E5"))+
  #scale_color_manual(values=c('black',"blue","red","orange",'green4'),                   #breaks = category)+
  labs(x='Day of the year', y='Number of registered crimes',
       title='Number of crimes per day for each district')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size = 20))  
p
#chance conviene una grafica de las de colorsitos con barras
#o de plano la otra cosa que no es muy precisa 
#pero tiene la ventaja de que no se enciman las lineas y asi no se satura
p <- ggplot(fancyPlot, aes(y=B3)) + 
  geom_boxplot()
p
# Rotate the box plot
p + coord_flip()

#y asi con cada distrito.
#chance quedan mejor asi las visualizaciones
ggplot(fancyPlot, aes(x=B3))+
  geom_density()
ggplot(fancyPlot, aes(x=A7))+
  geom_density()

estructurado=matrix(nrow=365*length(districts), ncol=2)
estructurado=as.data.frame(estructurado)
names(estructurado)=c('num_crimes','district')
#we fill the structured data

for(j in 1:length(districts)){
  estructurado[((j-1)*365+1):(j*365),2]=rep(districts[j], 365)
  estructurado[((j-1)*365+1):(j*365),1]=fancyPlot[,j+1]
}
head(estructurado)
tail(estructurado)
ggplot(estructurado,
       aes(x = num_crimes,
           fill = district)) +
  geom_boxplot()+
  labs(x='Number of crimes',
       title='Boxplot of the number of crimes per day for each district')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size = 20))   
#Niceeeeee!!!! :)

#This allows us th visualize multiple properties and make multiple comparisons simultaneously about the crimes in each district in Boston. For example, we can appreciate a lot of variance for
#D4, B2 districts, because the arms of the box plots are very long! The same for E5, whereas A15 has very low variance, and the smallest mean. 
#Actually the box plots allows us to see a very interesting hindsight: those neighborhoods with big meadian in the number of crimes per day also tend to have  a bigger variance!!!!!
  
#  Which suggest a positive correlation between mean and variance!
  
#  Let's verify that!!!!!!!!
mean(fancyPlot$A1)
sd(fancyPlot$A1)
#a different way to obtain it
estructurado$num_crimes[estructurado$district=='A1']
mean(estructurado$num_crimes[estructurado$district=='A1'])
#:)
summary(fancyPlot)
hindsight=matrix(nrow=length(districts), ncol=3)

hindsight=as.data.frame(hindsight)
names(hindsight)

#we rename de variables of the data frame
names(hindsight)=c('district', 'mean', 'variance')

for(i in 1:length(districts)){
  hindsight[i,1]=districts[i]
  hindsight[i,2]=mean(estructurado$num_crimes[estructurado$district==districts[i]])
  hindsight[i,3]=var(estructurado$num_crimes[estructurado$district==districts[i]])
}
cor(hindsight$mean, hindsight$variance)
#WOW!!!!!! super high!!!!!!!
#we observe a great linear relationship!!!!! Which we discovered 
#when visualizing the grouped boxplots :))
plot(hindsight$mean, hindsight$variance)

ggplot(hindsight, aes(x=mean, y=variance))+
  geom_point(col='green4', size=4)+
  labs(x='Average number of crimes per day', y='Variance',
       title='Relationship between mean and variance for the number of crimes per district')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size = 20)) 



#correlation between crimes per district.
#they can find it there's a strong relationship between certain disttricts
#https://statisticsglobe.com/r-pairs-plot-example/
pairs(fancyPlot[,-1]) #no obvious 
#or 
library("GGally") 
ggpairs(fancyPlot[,-1])
#as we can see, the number of crimes per day per district aren't hightly correlated

#otros bonitos


fancyPlot
fancyPlot$D4
pairs(cbind(fancyPlot[,2], fancyPlot$D4))
cor(fancyPlot[,2], fancyPlot$D4)
pairs(cbind(fancyPlot$A1, fancyPlot$D4))
cor(fancyPlot$A1, fancyPlot$D4)
#we could also study spatial correlation: idea: nieghbor districts have correalted number of crimes
#makes sense!!! A1 and D4 are next to each other!!!!
M=as.matrix(fancyPlot[,-1])
M=cor(M)
M
library(corrplot)
# as colour
corrplot(M, method="color", number.cex = 1)
corrplot(M,method = 'number', number.digits = 3)


library(GGally)

# Nice visualization of correlations
ggcorr(fancyPlot[,-1], method = c("everything", "pearson")) 

# From the help page:

ggpairs(fancyPlot[,-1], columns = 2:4, ggplot2::aes(colour='tomato')) 
ggpairs(fancyPlot[,-1], title="correlogram with ggpairs()") 
ggpairs(fancyPlot[,-1],
  upper = list(continuous = "density", combo = "box_no_facet"),
  lower = list(continuous = "points", combo = "dot_no_facet")
)

?pairs
pairs(cbind(A1=fancyPlot$A1,D4=fancyPlot$D4), col='black', cex=1)

#https://rspatial.org/analysis/3-spauto.html
#https://mgimond.github.io/Spatial/spatial-autocorrelation.html
pairs(cbind(A7=fancyPlot$A7,C6=fancyPlot$C6), col='forestgreen')



#https://r-coder.com/correlation-plot-r/
install.packages("psych")
library(psych)
corPlot(fancyPlot[,-1], cex = 1.2)
library(corrplot)
corrplot(fancyPlot[,-1], method = 'square', order = 'AOE', addCoef.col = 'black', tl.pos = 'd',
         cl.pos = 'n', col = COL2('BrBG'))

fancyPlot[,-1]


#fanciest plots, with cool ideas:
#https://r-graph-gallery.com/



#nothing interesting :(

summary(data)
summary(data$OFFENSE_DESCRIPTION)



#we can study specific types of crime

#eg 
# we can focus on VANDALISM, LARCENY SHOPLIFTING, HARASSMENT/ CRIMINAL HARASSMENT , 
#AUTO THEFT




#we can study the location of suicide attempts
# and find if they occur in some specicic part of the city
#SUICIDE / SUICIDE ATTEMPT

#


#And graffiti, for example


#Suicide locations
library(mapview)
data[data$OFFENSE_DESCRIPTION=='SUICIDE / SUICIDE ATTEMPT',]
data[data$OFFENSE_DESCRIPTION=='LARCENY SHOPLIFTING'&(is.na(data$Lat)+is.na(data$Long)==0),]

?mapview
mapview(data[data$OFFENSE_DESCRIPTION=='SUICIDE / SUICIDE ATTEMPT',], xcol = "Long", ycol = "Lat", crs = 4269, grid = FALSE, color='orange')
data[is.na(data$Lat)+is.na(data$Long)==0,] #remove data with missing values on  the coordinate

mapview(data[is.na(data$Lat)+is.na(data$Long)==0,],
        xcol = "Long", ycol = "Lat", crs = 4269, grid = FALSE, color='orange', 
        layer.name = c("Points of Interest"), col.regions = "orange")



mapview(data[data$OFFENSE_DESCRIPTION=='SUICIDE / SUICIDE ATTEMPT',], xcol = "Long", ycol = "Lat", crs = 4269, grid = FALSE, 
        col.regions = "red", layer.name = c("SUICIDE"))


data[data$OFFENSE_DESCRIPTION=='GRAFFITI',]
mapview(data[data$OFFENSE_DESCRIPTION=='GRAFFITI'&(is.na(data$Lat)+is.na(data$Long)==0) ,], xcol = "Long", ycol = "Lat", crs = 4269, grid = FALSE,
        layer.name = c("GRAFFITI"), col.regions='blue')+
mapview(data[data$OFFENSE_DESCRIPTION=='LARCENY SHOPLIFTING'&(is.na(data$Lat)+is.na(data$Long)==0),], xcol = "Long", ycol = "Lat", crs = 4269, grid = FALSE, color='blue',
        layer.name = c("LARCENY SHOPLIFTING"), col.regions = "orange")
# Amazing :)
data[data$OFFENSE_DESCRIPTION=='SUICIDE / SUICIDE ATTEMPT'&(is.na(data$Lat)+is.na(data$Long)==0),]

data[data$OFFENSE_DESCRIPTION=='HARASSMENT/ CRIMINAL HARASSMENT'&(is.na(data$Lat)+is.na(data$Long)==0),]

mapview(data[data$OFFENSE_DESCRIPTION=='GRAFFITI'&(is.na(data$Lat)+is.na(data$Long)==0) ,], xcol = "Long", ycol = "Lat", crs = 4269, grid = FALSE,
        layer.name = c("GRAFFITI"), col.regions='blue')+
  mapview(data[data$OFFENSE_DESCRIPTION=='LARCENY SHOPLIFTING'&(is.na(data$Lat)+is.na(data$Long)==0),], xcol = "Long", ycol = "Lat", crs = 4269, grid = FALSE, color='orange',
          layer.name = c("LARCENY SHOPLIFTING"), col.regions = "orange")+
  mapview(data[data$OFFENSE_DESCRIPTION=='AUTO THEFT'&(is.na(data$Lat)+is.na(data$Long)==0),], xcol = "Long", ycol = "Lat", crs = 4269, grid = FALSE, color='green',
          layer.name = c('AUTO THEFT'), col.regions = "green")+
  mapview(data[data$OFFENSE_DESCRIPTION=='HARASSMENT/ CRIMINAL HARASSMENT'&(is.na(data$Lat)+is.na(data$Long)==0) ,], xcol = "Long", ycol = "Lat", crs = 4269, grid = FALSE,
          layer.name = c("HARASSMENT"), col.regions='red')
  
summary(data$OFFENSE_DESCRIPTION)
#Something interesting: there are way more LARCENY SHOPLIFTING  observations than there are
#Auto thefts. However 


# esta increible esto
mapview(data[data$OFFENSE_DESCRIPTION=='GRAFFITI'&(is.na(data$Lat)+is.na(data$Long)==0) ,], xcol = "Long", ycol = "Lat", crs = 4269, grid = FALSE,
        layer.name = c("GRAFFITI"), col.regions='blue')+
  mapview(data[data$OFFENSE_DESCRIPTION=='VANDALISM'&(is.na(data$Lat)+is.na(data$Long)==0),], xcol = "Long", ycol = "Lat", crs = 4269, grid = FALSE, color='orange',
          layer.name = c("VANDALISM"), col.regions = "orange")

# esta increible esto

mapview(data[data$OFFENSE_DESCRIPTION=='GRAFFITI'&(is.na(data$Lat)+is.na(data$Long)==0) ,], xcol = "Long", ycol = "Lat", crs = 4269, grid = FALSE,
        layer.name = c("GRAFFITI"), col.regions='blue')

mapview(data[data$OFFENSE_DESCRIPTION=='HARASSMENT/ CRIMINAL HARASSMENT'&(is.na(data$Lat)+is.na(data$Long)==0) ,], xcol = "Long", ycol = "Lat", crs = 4269, grid = FALSE,
        layer.name = c("HARASSMENT"), col.regions='blue')


mapview(data[(is.na(data$Lat)+is.na(data$Long)==0),], xcol = "Long", ycol = "Lat", crs = 4269, grid = FALSE,
        layer.name = c("Crime"), col.regions='gold')
#general view of crimes without stratifying.

#now by district!
levels(data$DISTRICT)
districts
length(districts)
colors=c('forestgreen', 'darkorchid1', 'gold', 'navy','lightgreen','orange', 'slateblue',
                      'magenta', 'red', 'brown', 'tan', 'olivedrab')
length(colors)

p=mapview(data[data$DISTRICT==districts[1]&(is.na(data$Lat)+is.na(data$Long)==0) ,], xcol = "Long", ycol = "Lat", crs = 4269, grid = FALSE,
          layer.name = c("A1"), col.regions=colors[1])
p
for(i in 2:12){
  p=p+mapview(data[data$DISTRICT==districts[i]&(is.na(data$Lat)+is.na(data$Long)==0) ,], xcol = "Long", ycol = "Lat", crs = 4269, grid = FALSE,
            layer.name = c(districts[i]), col.regions=colors[i])
}
p





