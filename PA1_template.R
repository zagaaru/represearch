install.packages("lattice")
install.packages("knitr")
install.packages("dplyr")
install.packages("rmarkdown")
library(dplyr)
library(lattice)
setwd("C:/Users/uzagaar/Google Drive/Google DriveUZ/Google Drive/Personal/School/DataScienceTookkitclass/represearch")
#reading the activity data  
activity <- read.csv('activity.csv', header = TRUE, sep = ",")
activity$date <- as.Date(activity$date)

###For this part of the assignment, calculate the total number of steps taken per day
##What is the mean total number of steps per day?
activityfile2 <- group_by(activity,date)
activityfile2 <- na.omit(activityfile2)
dailysteps <- summarise(activityfile2,total_steps = sum(steps))

head(dailysteps)

#png("plot1.png",width = 480, height = 480)
hist(dailysteps$total_steps,main = "Total steps taken per day",
     xlab = "Total number of steps",col = "blue",
     breaks= length(dailysteps$total_steps)-1,plot = TRUE)


###Calculate and report the mean and median of the total number of steps taken per day

MeanStepsPerDay <- mean(dailysteps$total_steps)
MedianStepsPerDay <-median(dailysteps$total_steps)

MeanStepsPerDay

MedianStepsPerDay


##What is the average daily activity pattern?
##Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the avg number of steps taken, avged across all days (y-axis)

##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

##Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

activityfile3 <- group_by(activity, interval)
activityfile3 <- na.omit(activityfile3)
avgsteps_interval <- summarise(activityfile3, avg_steps = mean(steps)) 
#png("plot2.png",width = 480, height = 480)
plot(avgsteps_interval,type = 'l', col = 'brown',lwd=2,
     main = "Average number of steps taken across all days in 5min intervals",
     xlab = "5min intervals",ylab = "average number of steps taken")

##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

maxsteps <- filter(avgsteps_interval, avg_steps == max(avg_steps))
cat(sprintf("At %sth 5min interval, the maximum number of %f steps are found on average on all days in the data set. \n", maxsteps$interval, maxsteps$avg_steps))

##Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

missing_steps <- sum(is.na(activity$steps))
missing_date <- sum(is.na(activity$date))
missing_interval <- sum(is.na(activity$interval))

cat(sprintf("Total number of missing values in steps field =  %s\n", missing_steps))

cat(sprintf("Total number of missing values in date field =  %s\n", missing_date))

cat(sprintf("Total number of missing values in interval field =  %s\n",missing_interval))

##Develop a strategy for filling in all of the missing values in the dataset.  
##For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
##Create a new dataset that is equal to the original dataset but with the missing data filled in.

#create new dataset with removed NA
activityfile4<- activity
avgsteps <- summarise(activityfile3, mean_steps = mean(steps), median_steps = median(steps))

activityfile4$steps[is.na(activityfile4$steps)] <- tapply(activityfile4$steps, activityfile4$interval, mean, na.rm = TRUE)

#check for any missing values 
sum(is.na(activity4))

##Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
##Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

activityfile5 <- group_by(activityfile4, date)
dailysteps_afms <- summarise(activityfile5,total_steps = sum(steps))

#png("plot3.png",width = 480, height = 480)
hist(dailysteps_afms$total_steps,breaks=length(dailysteps_afms$total_steps)-1, 
     col = 'red', main = "Histogram of total number of steps taken each day",
     xlab = "Total number of Steps",plot = T)

new_mean <-mean(dailysteps_afms$total_steps)
new_median <-median(dailysteps_afms$total_steps)

cat(sprintf("The new mean is=  %s\n", new_mean))
## The new mean is=  10766.1886792453
cat(sprintf("The new median is =  %s\n", new_median))

##Are there differences in activity patterns between weekdays and weekends?

activityfile6<- activityfile5
activityfile6$wdays <- weekdays(as.Date(activityfile6$date))

activityfile6 <-mutate(activityfile6, wk.factor = factor((wdays == "Sunday" | wdays == "Saturday"),levels = c(FALSE,TRUE),
                                                 labels = c("weekdays","weekends")))

MeanStep_byIntervalByWkday <- aggregate(steps ~ interval + wk.factor, data=activityfile6, FUN="mean")
#png("plot4.png",width = 480, height = 480)
xyplot(steps ~ interval | wk.factor, data=MeanStep_byIntervalByWkday, type="l", grid=T, layout=c(1,2),
       main="Plot for comparision of number of steps on weekdays and weekends",
       xlab = "5min. intervals", ylab = "Number of steps",lwd = 2,col = "blue")