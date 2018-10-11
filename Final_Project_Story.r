For this project, I will be using a data set from Kaggle regarding college types and salaries.
https://www.kaggle.com/wsj/college-salaries#salaries-by-college-type.csv

I will also be using a similar data set with salaries by region.

I hope to discover some trends regarding schools by type and perfrom analysis on degrees starting pay and mid-career pay. I will combine the two tables, find the best type of school, and from those types, the best schools by region.The audience may be upper classmen in high school who are trying to make a decsion regarding which type of college they should attend. From my story, they will be able to utilize the many factors that go into my analysis and make a cost effective decision based on what they are hoping to gain from the college experience; whether that be that they want to have fun throughout college and attend a party school, or if they want to crack down and attend a pristegious Ivy League university.

salaries-by-college-type.csv
salaries-by-region.csv

#load in both files that I will be combining
type_salary <- read.csv('salaries-by-college-type.csv')
sal_region <- read.csv('salaries-by-region.csv')

head(type_salary)
head(sal_region)


#Load potentially necessary packages
library(dplyr)
library(caret)
library(ggplot2)
library(e1071)
library(plotly)
library(gridExtra)
library(RColorBrewer)

#select school type columns with dollar amounts
salary_cols_type <- type_salary[3:8] 
#remove $ and commas, convert to numerics
salary_cols_type <- apply(salary_cols_type, 2, function(x) as.numeric(gsub('\\$|,', '', x))) 

#select region columns with dollar amounts
salary_cols_reg <- sal_region[3:8]
#remove $ and commas, convert to numerics
salary_cols_reg <- apply(salary_cols_reg, 2, function(x) as.numeric(gsub('\\$|,', '', x)))

head(salary_cols_type)
head(salary_cols_reg)
    
#select non-money columns from school types
non_sal_cols_type <- type_salary[1:2]
head(non_sal_cols_type)
    
#select non-money columns from region
non_sal_cols_reg <- sal_region[1:2]
head(non_sal_cols_reg)

#combine non-salary and salary columns to make new data frame with numeric salary amounts
df_type <- cbind(non_sal_cols_type,salary_cols_type) 
head(df_type)
str(df_type)

df_reg <- cbind(non_sal_cols_reg,salary_cols_reg)
head(df_reg)
str(df_reg)

#combine tables to include region
df <- merge(df_type,df_reg,by=c("School.Name","Starting.Median.Salary","Mid.Career.Median.Salary","Mid.Career.10th.Percentile.Salary",
                                "Mid.Career.25th.Percentile.Salary","Mid.Career.75th.Percentile.Salary",
                                "Mid.Career.90th.Percentile.Salary"))
df_sal_col <- df[2:7]
df

my_theme <- function() {
      
    # Generate the colors for the chart procedurally with RColorBrewer
    palette <- brewer.pal("Blues", n=9) # create a palette of shades of grey 
    color.background = palette[1] # background plot color
    color.grid.major = palette[3] 
    color.grid.minor = palette[2]
    color.axis.text = palette[6] # text color 
    color.axis.title = palette[7]
    color.title = palette[9]

    # Begin construction of chart
     theme_bw(base_size=9) +

    # Set the entire chart region to a light blue color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +

    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +

    # Format the legend, but hide by default
    theme(legend.position="bottom") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +

    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
    theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +
    
    # Set facet attributes
    theme(strip.background = element_rect(fill=color.background, colour = color.background)) +
    theme(strip.text = element_text(size = 15, colour = color.axis.text)) +

    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

my_palette <- palette(c("#849483", "#4E937A", "#B4656F", "#948392"))


options(scipen=999)
#See distribtion of school types
ggplot(df, aes(School.Type)) +
    geom_bar() +
    labs(x="School Type",y="Count",title="Distribution of School Types") +
    my_theme()

#create table to see highest starting median salary by school type
sal_by_type <- df %>%
    group_by(School.Type) %>%
    summarize(avg_sal = mean(Mid.Career.Median.Salary, rm.na = TRUE)) %>%
    arrange(desc(avg_sal))

#Group by region then by school type to see highest mid career median salary
reg_type <- df %>%
    group_by(Region,School.Type) %>%
    summarize(avg_sal=mean(Mid.Career.Median.Salary, rm.na = TRUE)) %>%
    arrange(Region,desc(avg_sal))

grid.arrange(
#create bar plot to visualize
ggplot(sal_by_type, aes(x=School.Type, y=avg_sal)) +
    geom_bar(stat="identity") +
    labs(x="School Type",y="Mid Career Median Starting Salary",title="Mid Career Median Salary by School Type") +
    my_theme(), 

ggplot(reg_type, aes(x=Region, y=avg_sal)) +
    geom_bar(stat="identity") +
    labs(x="Region",y="Mid Career Median Salary",title="Mid Career Median Salary by Region") +
    my_theme(),
ncol=2)

#Create boxplot to see highest starting pay by school type
ggplot(df, aes(School.Type,Mid.Career.Median.Salary)) +
    geom_boxplot() +
    labs(x="School Type",y="Mid Career Median Salary") +
    my_theme()

#Do the same for region
ggplot(df, aes(Region,Mid.Career.Median.Salary)) +
    geom_boxplot() +
    labs(x="Region",y="Mid Career Median Salary") +
    my_theme()



#Look to see which school type/region has highest salary growth potential
sal_diff <- df %>%
    mutate(start_mid_diff = Mid.Career.Median.Salary - Starting.Median.Salary) %>%
    select(School.Name,Region, School.Type, start_mid_diff) %>%
    arrange(desc(start_mid_diff))

ggplot(sal_diff, aes(x=School.Type, y=start_mid_diff, fill=Region)) +
    geom_bar(stat="identity",position='dodge') +
    labs(x="School Type",y="Difference Between Starting and \n Mid Career Salary",
         title="Difference Between Starting and Mid Career Salary \n by School Type & Region") +
    my_theme()

#find top 10 schools by region
top30 <- df %>%
    select(School.Name,Region,School.Type,Mid.Career.Median.Salary) %>%
    arrange(desc(Mid.Career.Median.Salary)) %>%
    head(30) 

#Visual for top schools by type
ggplot(top30, aes(reorder(School.Name,Mid.Career.Median.Salary), Mid.Career.Median.Salary)) +
    geom_col(aes(fill=School.Type)) +
    labs(x="School Name",y="Mid Career Median Salary") +
    coord_flip() +
    my_theme()


#Visual for top schools by region
ggplot(top30, aes(reorder(School.Name,Mid.Career.Median.Salary), Mid.Career.Median.Salary)) +
    geom_col(aes(fill=Region)) +
    labs(x="School Name",y="Mid Career Median Salary") +
    coord_flip() +
    my_theme()

#Find top 5 schools of each school type and create bar charts to visualize
top_ivy <- df %>%
    select(School.Name, School.Type,Region, Mid.Career.Median.Salary) %>%
    filter(School.Type == 'Ivy League') %>%
    head(5)

top_eng <- df %>%
    select(School.Name, School.Type,Region, Mid.Career.Median.Salary) %>%
    filter(School.Type == 'Engineering') %>%
    head(5)

top_lib <- df %>%
    select(School.Name, School.Type,Region, Mid.Career.Median.Salary) %>%
    filter(School.Type == 'Liberal Arts') %>%
    head(5)

top_state <- df %>%
    select(School.Name, School.Type,Region, Mid.Career.Median.Salary) %>%
    filter(School.Type == 'State') %>%
    head(5)

top_party <- df %>%
    select(School.Name, School.Type,Region, Mid.Career.Median.Salary) %>%
    filter(School.Type == 'Party') %>%
    head(5)

grid.arrange(
ggplot(top_ivy, aes(reorder(School.Name,Mid.Career.Median.Salary), Mid.Career.Median.Salary)) +
    geom_col(aes(fill=Region)) +
    labs(x="School Name",y="Mid Career Median Salary",title="Ivy League") +
    coord_flip() +
    my_theme(),

ggplot(top_eng, aes(reorder(School.Name,Mid.Career.Median.Salary), Mid.Career.Median.Salary)) +
    geom_col(aes(fill=Region)) +
    labs(x="School Name",y="Mid Career Median Salary",title="Engineering") +
    coord_flip() +
    my_theme(),

ggplot(top_lib, aes(reorder(School.Name,Mid.Career.Median.Salary), Mid.Career.Median.Salary)) +
    geom_col(aes(fill=Region)) +
    labs(x="School Name",y="Mid Career Median Salary",title="Liberal Arts") +
    coord_flip() +
    my_theme(),

ggplot(top_state, aes(reorder(School.Name,Mid.Career.Median.Salary), Mid.Career.Median.Salary)) +
    geom_col(aes(fill=Region)) +
    labs(x="School Name",y="Mid Career Median Salary",title="State") +
    coord_flip() +
    my_theme(),

ggplot(top_party, aes(reorder(School.Name,Mid.Career.Median.Salary), Mid.Career.Median.Salary)) +
    geom_col(aes(fill=Region)) +
    labs(x="School Name",y="Mid Career Median Salary",title="Party") +
    coord_flip() +
    my_theme(),
ncol=2)

#top_all_type <- rbind(top_ivy,top_eng,top_lib,top_state,top_party) %>%
#    group_by(School.Type)
#top_all_type

#ggplot(top_all_type, aes(reorder(School.Name,Mid.Career.Median.Salary), Mid.Career.Median.Salary)) +
#    geom_col(aes(fill=School.Type),position='dodge') +
#    facet_wrap(~Region) +
#    labs(x="School Name",y="Mid Career Median Salary",title="Five Highest Mid Career Median Salary \n of Each School Type by Region") +
#   coord_flip() +   
#    theme(panel.spacing = unit(100, "lines")) +
#    my_theme()

#Find top 5 schools of each region and create bar charts to visualize
top_mid <- df %>%
    select(School.Name, School.Type,Region, Mid.Career.Median.Salary) %>%
    filter(Region == 'Midwestern') %>%
    head(5)

top_south <- df %>%
    select(School.Name, School.Type,Region, Mid.Career.Median.Salary) %>%
    filter(Region == 'Southern') %>%
    head(5)

top_cal <- df %>%
    select(School.Name, School.Type,Region, Mid.Career.Median.Salary) %>%
    filter(Region == 'California') %>%
    head(5)

top_north <- df %>%
    select(School.Name, School.Type,Region, Mid.Career.Median.Salary) %>%
    filter(Region == 'Northeastern') %>%
    head(5)

grid.arrange(
ggplot(top_mid, aes(reorder(School.Name,Mid.Career.Median.Salary), Mid.Career.Median.Salary)) +
    geom_col(aes(fill=School.Type)) +
    labs(x="School Name",y="Mid Career Median Salary",title="Midwestern") +
    coord_flip() +
    my_theme(),

ggplot(top_south, aes(reorder(School.Name,Mid.Career.Median.Salary), Mid.Career.Median.Salary)) +
    geom_col(aes(fill=School.Type)) +
    labs(x="School Name",y="Mid Career Median Salary",title="Southern") +
    coord_flip() +
    my_theme(),
    
ggplot(top_north, aes(reorder(School.Name,Mid.Career.Median.Salary), Mid.Career.Median.Salary)) +
    geom_col(aes(fill=School.Type)) +
    labs(x="School Name",y="Mid Career Median Salary",title="Northeastern") +
    coord_flip() +
    my_theme(),
nrow=2)

ggplot(top_cal, aes(reorder(School.Name,Mid.Career.Median.Salary), Mid.Career.Median.Salary)) +
    geom_col(aes(fill=School.Type)) +
    labs(x="School Name",y="Mid Career Median Salary",title="California") +
    coord_flip() +
    my_theme()

#top_all_reg <- rbind(top_mid,top_cal,top_south,top_north) %>%
#    group_by(Region)
#top_all_reg

#ggplot(top_all_reg, aes(reorder(School.Name,Mid.Career.Median.Salary), Mid.Career.Median.Salary)) +
#    geom_col(aes(fill=Region),position='dodge') +
#    facet_wrap(~School.Type) +
#    labs(x="School Name",y="Mid Career Median Salary",title="Five Highest Mid Career Median Salary \n of Each Region by School Type") +
#    coord_flip() +   
#    my_theme()
