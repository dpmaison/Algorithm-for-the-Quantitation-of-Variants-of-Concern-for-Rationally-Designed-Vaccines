#SARS-CoV-2 Variant Algorithm Analysis
#David Maison
#02.27.2022

install.packages("ggstatsplot")
install.packages("ggExtra")
library("ggExtra")
library("ggplot2")
library("ggstatsplot")


my_data <- read.csv(file.choose())
print(my_data)

##########
Daily <- c("Nov 1,'21", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", 
           "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
           "Dec 1,'21", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", 
           "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31")
Daily2 <- c("Nov 1,'21", "", "", "", "", "", "7", "", "", "", "", "", "", "14", "", 
           "", "", "", "", "", "21", "", "", "", "", "", "", "", "", "",
           "Dec 1,'21", "", "", "", "", "", "7", "", "", "", "", "", "", "14", "", 
           "", "", "", "", "", "21", "", "", "", "", "", "", "", "", "", "")
Weekly <- c("Oct 01-07,'21", "Oct 08-14,'21",
            "Oct 15-21,'21", "Oct 22-30,'21",
            "Nov 01-07,'21", "Nov 08-14,'21",
            "Nov 15-22,'21", "Nov 22-30,'21",
            "Dec 01-07,'21", "Dec 08-14,'21",
            "Dec 15-21,'21", "Dec 22-31,'21",
            "Jan 01-07,'22")
BiWeekly <- c("Jul 01-14,'21", "Jul 15-31,'21",
            "Aug 01-14,'21", "Aug 15-31,'21",
            "Sep 01-14,'21", "Sep 15-30,'21",
            "Oct 01-14,'21", "Oct 15-30,'21",
            "Nov 01-14,'21", "Nov 15-30,'21",
            "Dec 01-14,'21", "Dec 15-31,'21",
            "Jan 01-14,'22")
Monthly <- c("Dec'20", "Jan'21",
            "Feb'21", "Mar'21",
            "Apr'21", "May'21",
            "Jun'21", "Jul'21",
            "Aug'21", "Sep'21",
            "Oct'21", "Nov'21",
            "Dec'21", "Jan'22")




##########
#Daily
#1____________________________________________________________
#B.1.1.529+BA. Daily
my_data <- read.csv(file.choose())

attach(my_data)

cor.test(MM,LL,method = "pearson")

B11529BAD <- ggplot(my_data, aes(x=MM, y=log_OmicronAll)) + 
  geom_point(color = "black", size = 5)+ 
  annotate("text", x = 14, y = 0, label = "B.1.1.529+BA.", col = "blue", size = 8) + 
  annotate("text", x = 14, y = -0.4, label = "r = 0.9533349 ", col = "blue", size = 7) + 
  annotate("text", x = 14, y = -0.8, label = "P = 2.2e-16 ****", col = "blue", size = 7) +
  annotate("text", x = 14, y = -1.2, label = "Dec 22-31,'21 = 72%", col = "blue", size = 7) +
  geom_smooth(method=lm, se=FALSE, color = "blue") + 
  geom_vline(xintercept=32, linetype="dashed", 
             color = "orange", size=1) +
  geom_vline(xintercept=43, linetype="dashed", 
             color = "red", size=1) +
  scale_x_continuous(name = "Days", labels = Daily2, limits = c(1,61), breaks = seq(1, 61, 1)) + 
  scale_y_continuous(name = "Prevalence (Logarithmic Transformed Ratio)", limits = c(-5, 0), breaks = seq(-5, 0, 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank(), axis.line = element_line(color="black"), axis.line.x = element_line(color="black"))  

B11529BAD+theme(axis.text=element_text(size=12),
                axis.text.x=element_text(size=12, angle = 45, vjust = 1, hjust=1),
                axis.title=element_text(size=14,face="bold"))

#2____________________________________________________________
#BA.1 Daily

cor.test(M,Log,method = "pearson")

BA1D <- ggplot(my_data, aes(x=MM, y=log_BA1)) + 
  geom_point(color = "black", size = 5)+ 
  annotate("text", x = 14, y = 0, label = "BA.1", col = "blue", size = 8) + 
  annotate("text", x = 14, y = -0.4, label = "r = 0.9537968", col = "blue", size = 7) + 
  annotate("text", x = 14, y = -0.8, label = "P = 2.2e-16 ****", col = "blue", size = 7) +
  annotate("text", x = 14, y = -1.2, label = "Dec 22-31,'21 = 57.4%", col = "blue", size = 7) +
  geom_smooth(method=lm, se=FALSE, color = "blue") + 
  geom_vline(xintercept=33, linetype="dashed", 
             color = "orange", size=1) +
  geom_vline(xintercept=44, linetype="dashed", 
             color = "red", size=1) +
  scale_x_continuous(name = "Days", labels = Daily2, limits = c(1,61), breaks = seq(1, 61, 1)) + 
  scale_y_continuous(name = "Prevalence (Logarithmic Transformed Ratio)", limits = c(-5, 0), breaks = seq(-5, 0, 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank(), axis.line = element_line(color="black"), axis.line.x = element_line(color="black"))  

BA1D+theme(axis.text=element_text(size=12),
           axis.text.x=element_text(size=12, angle = 45, vjust = 1, hjust=1),
           axis.title=element_text(size=14,face="bold"))

#3____________________________________________________________
#BA.1.1 Daily
my_data <- read.csv(file.choose())

attach(my_data)

cor.test(MM,log_BA11,method = "pearson")

BA11D <- ggplot(my_data, aes(x=MM, y=log_BA11)) + 
  geom_point(color = "black", size = 5)+ 
  annotate("text", x = 14, y = 0, label = "BA.1.1", col = "blue", size = 8) + 
  annotate("text", x = 14, y = -0.4, label = "r = 0.9706282", col = "blue", size = 7) + 
  annotate("text", x = 14, y = -0.8, label = "P = 2.2e-16 ****", col = "blue", size = 7) +
  annotate("text", x = 14, y = -1.2, label = "Dec 22-31,'21 = 14%", col = "blue", size = 7) +
  geom_smooth(method=lm, se=FALSE, color = "blue") + 
  geom_vline(xintercept=40, linetype="dashed", 
             color = "orange", size=1) +
  scale_x_continuous(name = "Days", labels = Daily2, limits = c(1,61), breaks = seq(1, 61, 1)) + 
  scale_y_continuous(name = "Prevalence (Logarithmic Transformed Ratio)", limits = c(-5, 0), breaks = seq(-5, 0, 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank(), axis.line = element_line(color="black"), axis.line.x = element_line(color="black"))  

BA11D+theme(axis.text=element_text(size=12),
            axis.text.x=element_text(size=12, angle = 45, vjust = 1, hjust=1),
            axis.title=element_text(size=14,face="bold"))

#4____________________________________________________________
#BA.2 Daily
my_data <- read.csv(file.choose())

attach(my_data)

cor.test(MM,log_BA2,method = "pearson")

BA2D <- ggplot(my_data, aes(x=MM, y=log_BA2)) + 
  geom_point(color = "black", size = 5)+ 
  annotate("text", x = 14, y = 0, label = "BA.2", col = "blue", size = 8) + 
  annotate("text", x = 14, y = -0.4, label = "r = 0.9168598", col = "blue", size = 7) + 
  annotate("text", x = 14, y = -0.8, label = "P = 7.183e-12 ****", col = "blue", size = 7) +
  annotate("text", x = 14, y = -1.2, label = "Dec 22-31,'21 = 0.35%", col = "blue", size = 7) +
  geom_smooth(method=lm, se=FALSE, color = "blue") + 
  scale_x_continuous(name = "Days", labels = Daily2, limits = c(1,61), breaks = seq(1, 61, 1)) + 
  scale_y_continuous(name = "Prevalence (Logarithmic Transformed Ratio)", limits = c(-5, 0), breaks = seq(-5, 0, 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank(), axis.line = element_line(color="black"), axis.line.x = element_line(color="black"))  

BA2D+theme(axis.text=element_text(size=12),
           axis.text.x=element_text(size=12, angle = 45, vjust = 1, hjust=1),
           axis.title=element_text(size=14,face="bold"))

#5____________________________________________________________
#BA.3 Daily
my_data <- read.csv(file.choose())

attach(my_data)

cor.test(MM,log_BA3,method = "pearson")

BA3D <- ggplot(my_data, aes(x=MM, y=log_BA3)) + 
  geom_point(color = "black", size = 5)+ 
  annotate("text", x = 14, y = 0, label = "BA.3", col = "blue", size = 8) + 
  annotate("text", x = 14, y = -0.4, label = "r = 0.295625", col = "blue", size = 7) + 
  annotate("text", x = 14, y = -0.8, label = "P = 0.2493", col = "blue", size = 7) +
  annotate("text", x = 14, y = -1.2, label = "Dec 22-31,'21 = 0%", col = "blue", size = 7) +
  geom_smooth(method=lm, se=FALSE, color = "blue") + 
  scale_x_continuous(name = "Days", labels = Daily2, limits = c(1,61), breaks = seq(1, 61, 1)) + 
  scale_y_continuous(name = "Prevalence (Logarithmic Transformed Ratio)", limits = c(-5, 0), breaks = seq(-5, 0, 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank(), axis.line = element_line(color="black"), axis.line.x = element_line(color="black"))  

BA3D+theme(axis.text=element_text(size=12),
           axis.text.x=element_text(size=12, angle = 45, vjust = 1, hjust=1),
           axis.title=element_text(size=14,face="bold"))




##########
#WEEKLY
#1____________________________________________________________
#B.1.1.529+BA. Weekly
my_dataW <- read.csv(file.choose())

attach(my_dataW)

cor.test(MM,log_OmicronAll,method = "pearson")

B11529BAW <- ggplot(my_dataW, aes(x=MM, y=log_OmicronAll)) + 
  geom_point(color = "black", size = 5)+ 
  annotate("text", x = 5, y = 0, label = "B.1.1.529+BA.", col = "blue", size = 8) + 
  annotate("text", x = 5, y = -0.4, label = "r = 0.9692661", col = "blue", size = 7) + 
  annotate("text", x = 5, y = -0.8, label = "P = 1.626e-05 ****", col = "blue", size = 7) +
  annotate("text", x = 5, y = -1.2, label = "Dec 22-31,'21 = 81.4%", col = "blue", size = 7) +
  geom_smooth(method=lm, se=FALSE, color = "blue") + 
  geom_vline(xintercept=9, linetype="dashed", 
             color = "orange", size=1) +
  geom_vline(xintercept=11, linetype="dashed", 
             color = "red", size=1) +
  scale_x_continuous(name = "Weeks", labels = Weekly, limits = c(1,13), breaks = seq(1, 13, 1)) + 
  scale_y_continuous(name = "Prevalence (Logarithmic Transformed Ratio)", limits = c(-5, 0), breaks = seq(-5, 0, 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank(), axis.line = element_line(color="black"), axis.line.x = element_line(color="black"))  

B11529BAW+theme(axis.text=element_text(size=12),
            axis.text.x=element_text(size=12, angle = 45, vjust = 1, hjust=1),
            axis.title=element_text(size=14,face="bold"))

#2____________________________________________________________
#BA.1 Weekly
my_data <- read.csv(file.choose())

attach(my_data)

cor.test(MM,log_BA1,method = "pearson")

BA1W <- ggplot(my_dataW, aes(x=MM, y=log_BA1)) + 
  geom_point(color = "black", size = 5)+ 
  annotate("text", x = 5, y = 0, label = "BA.1", col = "blue", size = 8) + 
  annotate("text", x = 5, y = -0.4, label = "r = 0.9658675", col = "blue", size = 7) + 
  annotate("text", x = 5, y = -0.8, label = "P = 2.34e-05 ****", col = "blue", size = 7) +
  annotate("text", x = 5, y = -1.2, label = "Dec 22-31,'21 = 61.8%", col = "blue", size = 7) +
  geom_smooth(method=lm, se=FALSE, color = "blue") + 
  geom_vline(xintercept=9, linetype="dashed", 
             color = "orange", size=1) +
  geom_vline(xintercept=12, linetype="dashed", 
             color = "red", size=1) +
  scale_x_continuous(name = "Weeks", labels = Weekly, limits = c(1,13), breaks = seq(1, 13, 1)) + 
  scale_y_continuous(name = "Prevalence (Logarithmic Transformed Ratio)", limits = c(-5, 0), breaks = seq(-5, 0, 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank(), axis.line = element_line(color="black"), axis.line.x = element_line(color="black"))  

BA1W+theme(axis.text=element_text(size=12),
               axis.text.x=element_text(size=12, angle = 45, vjust = 1, hjust=1),
               axis.title=element_text(size=14,face="bold"))

#3____________________________________________________________
#BA.1.1 Weekly
my_data <- read.csv(file.choose())

attach(my_data)

cor.test(MM,log_BA11,method = "pearson")

BA11W <- ggplot(my_dataW, aes(x=MM, y=log_BA11)) + 
  geom_point(color = "black", size = 5)+ 
  annotate("text", x = 5, y = 0, label = "BA.1.1", col = "blue", size = 8) + 
  annotate("text", x = 5, y = -0.4, label = "r = 0.97179", col = "blue", size = 7) + 
  annotate("text", x = 5, y = -0.8, label = "P = 5.494e-05 ***", col = "blue", size = 7) +
  annotate("text", x = 5, y = -1.2, label = "Dec 22-31,'21 = 18.7%", col = "blue", size = 7) +
  geom_smooth(method=lm, se=FALSE, color = "blue") + 
  geom_vline(xintercept=10, linetype="dashed", 
             color = "orange", size=1) +
  scale_x_continuous(name = "Weeks", labels = Weekly, limits = c(1,13), breaks = seq(1, 13, 1)) + 
  scale_y_continuous(name = "Prevalence (Logarithmic Transformed Ratio)", limits = c(-5, 0), breaks = seq(-5, 0, 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank(), axis.line = element_line(color="black"), axis.line.x = element_line(color="black"))  

BA11W+theme(axis.text=element_text(size=12),
           axis.text.x=element_text(size=12, angle = 45, vjust = 1, hjust=1),
           axis.title=element_text(size=14,face="bold"))

#4____________________________________________________________
#BA.2 Weekly
my_data <- read.csv(file.choose())

attach(my_data)

cor.test(MM,log_BA2,method = "pearson")

BA2W <- ggplot(my_dataW, aes(x=MM, y=log_BA2)) + 
  geom_point(color = "black", size = 5)+ 
  annotate("text", x = 5, y = 0, label = "BA.2", col = "blue", size = 8) + 
  annotate("text", x = 5, y = -0.4, label = "r = 0.9959816", col = "blue", size = 7) + 
  annotate("text", x = 5, y = -0.8, label = "P = 1.962e-06 ****", col = "blue", size = 7) +
  annotate("text", x = 5, y = -1.2, label = "Dec 22-31,'21 = 0.9%", col = "blue", size = 7) +
  geom_smooth(method=lm, se=FALSE, color = "blue") + 
  geom_vline(xintercept=13, linetype="dashed", 
             color = "orange", size=1) +
  scale_x_continuous(name = "Weeks", labels = Weekly, limits = c(1,13), breaks = seq(1, 13, 1)) + 
  scale_y_continuous(name = "Prevalence (Logarithmic Transformed Ratio)", limits = c(-5, 0), breaks = seq(-5, 0, 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank(), axis.line = element_line(color="black"), axis.line.x = element_line(color="black"))  

BA2W+theme(axis.text=element_text(size=12),
            axis.text.x=element_text(size=12, angle = 45, vjust = 1, hjust=1),
            axis.title=element_text(size=14,face="bold"))

#5____________________________________________________________
#BA.3 Weekly
my_data <- read.csv(file.choose())

attach(my_data)

cor.test(MM,log_BA3,method = "pearson")

BA3W <- ggplot(my_dataW, aes(x=MM, y=log_BA3)) + 
  geom_point(color = "black", size = 5)+ 
  annotate("text", x = 5, y = 0, label = "BA.3", col = "blue", size = 8) + 
  annotate("text", x = 5, y = -0.4, label = "r = 0.5005875", col = "blue", size = 7) + 
  annotate("text", x = 5, y = -0.8, label = "P = 0.2525", col = "blue", size = 7) +
  annotate("text", x = 5, y = -1.2, label = "Dec 22-31,'21 = 0.00096%", col = "blue", size = 7) +
  geom_smooth(method=lm, se=FALSE, color = "blue") + 
  geom_vline(xintercept=33, linetype="dashed", 
             color = "orange", size=1) +
  geom_vline(xintercept=45, linetype="dashed", 
             color = "red", size=1) +
  scale_x_continuous(name = "Weeks", labels = Weekly, limits = c(1,14), breaks = seq(1, 13, 1)) + 
  scale_y_continuous(name = "Prevalence (Logarithmic Transformed Ratio)", limits = c(-5, 0), breaks = seq(-5, 0, 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank(), axis.line = element_line(color="black"), axis.line.x = element_line(color="black"))  

BA3W+theme(axis.text=element_text(size=12),
           axis.text.x=element_text(size=12, angle = 45, vjust = 1, hjust=1),
           axis.title=element_text(size=14,face="bold"))


##########
#BIWEEKLY
#6____________________________________________________________
#B.1.1.529+BA. BiWeekly
my_databw <- read.csv(file.choose())

attach(my_databw)

cor.test(M,log_OmicronAll,method = "pearson")

B11529BABW <- ggplot(my_databw, aes(x=M, y=log_OmicronAll)) + 
  geom_point(color = "black", size = 5)+ 
  annotate("text", x = 5, y = 0, label = "B.1.1.529+BA.", col = "blue", size = 8) + 
  annotate("text", x = 5, y = -0.4, label = "r = 0.953851", col = "blue", size = 7) + 
  annotate("text", x = 5, y = -0.8, label = "P = 0.01182 *", col = "blue", size = 7) +
  annotate("text", x = 5, y = -1.2, label = "Dec 15-31,'21 = 66.6%", col = "blue", size = 7) +
  geom_smooth(method=lm, se=FALSE, color = "blue") + 
  geom_vline(xintercept=11, linetype="dashed", 
             color = "orange", size=1) +
  geom_vline(xintercept=12, linetype="dashed", 
             color = "red", size=1) +
  scale_x_continuous(name = "Bi-Weeks", labels = BiWeekly, limits = c(1,13), breaks = seq(1, 13, 1)) + 
  scale_y_continuous(name = "Prevalence (Logarithmic Transformed Ratio)", limits = c(-5, 0), breaks = seq(-5, 0, 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank(), axis.line = element_line(color="black"), axis.line.x = element_line(color="black"))  

B11529BABW+theme(axis.text=element_text(size=12),
                axis.text.x=element_text(size=12, angle = 45, vjust = 1, hjust=1),
                axis.title=element_text(size=14,face="bold"))

#2____________________________________________________________
#BA.1 BiWeekly
my_data <- read.csv(file.choose())

attach(my_data)

cor.test(M,log_BA1,method = "pearson")

BA1BW <- ggplot(my_databw, aes(x=M, y=log_BA1)) + 
  geom_point(color = "black", size = 5)+ 
  annotate("text", x = 5, y = 0, label = "BA.1", col = "blue", size = 8) + 
  annotate("text", x = 5, y = -0.4, label = "r = 0.9509154", col = "blue", size = 7) + 
  annotate("text", x = 5, y = -0.8, label = "P = 0.01296 *", col = "blue", size = 7) +
  annotate("text", x = 5, y = -1.2, label = "Dec 15-31,'21 = 48.7%", col = "blue", size = 7) +
  geom_smooth(method=lm, se=FALSE, color = "blue") + 
  geom_vline(xintercept=11, linetype="dashed", 
             color = "orange", size=1) +
  geom_vline(xintercept=13, linetype="dashed", 
             color = "red", size=1) +
  scale_x_continuous(name = "Bi-Weeks", labels = BiWeekly, limits = c(1,13), breaks = seq(1, 13, 1)) + 
  scale_y_continuous(name = "Prevalence (Logarithmic Transformed Ratio)", limits = c(-5, 0), breaks = seq(-5, 0, 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank(), axis.line = element_line(color="black"), axis.line.x = element_line(color="black"))  

BA1BW+theme(axis.text=element_text(size=12),
           axis.text.x=element_text(size=12, angle = 45, vjust = 1, hjust=1),
           axis.title=element_text(size=14,face="bold"))

#3____________________________________________________________
#BA.1.1 BiWeekly
my_data <- read.csv(file.choose())

attach(my_data)

cor.test(MM,log_BA11,method = "pearson")

BA11BW <- ggplot(my_databw, aes(x=MM, y=log_BA11)) + 
  geom_point(color = "black", size = 5)+ 
  annotate("text", x = 5, y = 0, label = "BA.1.1", col = "blue", size = 8) + 
  annotate("text", x = 5, y = -0.4, label = "r = 0.9547178", col = "blue", size = 7) + 
  annotate("text", x = 5, y = -0.8, label = "P = 0.01149 *", col = "blue", size = 7) +
  annotate("text", x = 5, y = -1.2, label = "Dec 15-31,'21 = 17.5%", col = "blue", size = 7) +
  geom_smooth(method=lm, se=FALSE, color = "blue") + 
  geom_vline(xintercept=12, linetype="dashed", 
             color = "orange", size=1) +
  scale_x_continuous(name = "Bi-Weeks", labels = BiWeekly, limits = c(1,13), breaks = seq(1, 13, 1)) + 
  scale_y_continuous(name = "Prevalence (Logarithmic Transformed Ratio)", limits = c(-5, 0), breaks = seq(-5, 0, 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank(), axis.line = element_line(color="black"), axis.line.x = element_line(color="black"))  

BA11BW+theme(axis.text=element_text(size=12),
            axis.text.x=element_text(size=12, angle = 45, vjust = 1, hjust=1),
            axis.title=element_text(size=14,face="bold"))

#4____________________________________________________________
#BA.2 BiWeekly
my_data <- read.csv(file.choose())

attach(my_data)

cor.test(MM,log_BA2,method = "pearson")

BA2BW <- ggplot(my_databw, aes(x=MM, y=log_BA2)) + 
  geom_point(color = "black", size = 5)+ 
  annotate("text", x = 5, y = 0, label = "BA.2", col = "blue", size = 8) + 
  annotate("text", x = 5, y = -0.4, label = "r = 0.9944188", col = "blue", size = 7) + 
  annotate("text", x = 5, y = -0.8, label = "P = 0.005581 *", col = "blue", size = 7) +
  annotate("text", x = 5, y = -1.2, label = "Dec 15-31,'21 = 0.46%", col = "blue", size = 7) +
  geom_smooth(method=lm, se=FALSE, color = "blue") + 
  geom_vline(xintercept=13, linetype="dashed", 
             color = "orange", size=1) +
  scale_x_continuous(name = "Bi-Weeks", labels = BiWeekly, limits = c(1,13), breaks = seq(1, 13, 1)) + 
  scale_y_continuous(name = "Prevalence (Logarithmic Transformed Ratio)", limits = c(-5, 0), breaks = seq(-5, 0, 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank(), axis.line = element_line(color="black"), axis.line.x = element_line(color="black"))  

BA2BW+theme(axis.text=element_text(size=12),
           axis.text.x=element_text(size=12, angle = 45, vjust = 1, hjust=1),
           axis.title=element_text(size=14,face="bold"))

#5____________________________________________________________
#BA.3 BiWeekly
my_data <- read.csv(file.choose())

attach(my_data)

cor.test(MM,log_BA3,method = "pearson")

BA3BW <- ggplot(my_databw, aes(x=MM, y=log_BA3)) + 
  geom_point(color = "black", size = 5)+ 
  annotate("text", x = 5, y = 0, label = "BA.3", col = "blue", size = 8) + 
  annotate("text", x = 5, y = -0.4, label = "r = 0.330722", col = "blue", size = 7) + 
  annotate("text", x = 5, y = -0.8, label = "P = 0.6693", col = "blue", size = 7) +
  annotate("text", x = 5, y = -1.2, label = "Dec 15-31,'21 = 0.006%", col = "blue", size = 7) +
  geom_smooth(method=lm, se=FALSE, color = "blue") + 
  geom_vline(xintercept=33, linetype="dashed", 
             color = "orange", size=1) +
  geom_vline(xintercept=45, linetype="dashed", 
             color = "red", size=1) +
  scale_x_continuous(name = "Bi-Weeks", labels = BiWeekly, limits = c(1,13), breaks = seq(1, 13, 1)) + 
  scale_y_continuous(name = "Prevalence (Logarithmic Transformed Ratio)", limits = c(-5, 0), breaks = seq(-5, 0, 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank(), axis.line = element_line(color="black"), axis.line.x = element_line(color="black"))  

BA3BW+theme(axis.text=element_text(size=12),
           axis.text.x=element_text(size=12, angle = 45, vjust = 1, hjust=1),
           axis.title=element_text(size=14,face="bold"))



##########
#MONTHLY
#1____________________________________________________________
#B.1.1.529+BA. Monthly
my_data <- read.csv(file.choose())

attach(my_data)

cor.test(M,log_OmicronAll,method = "pearson")

B11529BAM <- ggplot(my_data, aes(x=M, y=log_OmicronAll)) + 
  geom_point(color = "black", size = 5)+ 
  annotate("text", x = 5, y = 0, label = "B.1.1.529+BA.", col = "blue", size = 8) + 
  annotate("text", x = 5, y = -0.4, label = "r = 0.918474", col = "blue", size = 7) + 
  annotate("text", x = 5, y = -0.8, label = "P = 0.2588", col = "blue", size = 7) +
  annotate("text", x = 5, y = -1.2, label = "Dec'21 = 46.9%", col = "blue", size = 7) +
  geom_smooth(method=lm, se=FALSE, color = "blue") + 
  geom_vline(xintercept=12.96, linetype="dashed", 
             color = "orange", size=1) +
  geom_vline(xintercept=13.04, linetype="dashed", 
             color = "red", size=1) +
  scale_x_continuous(name = "Months", labels = Monthly, limits = c(1,14), breaks = seq(1, 14, 1)) + 
  scale_y_continuous(name = "Prevalence (Logarithmic Transformed Ratio)", limits = c(-5, 0), breaks = seq(-5, 0, 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank(), axis.line = element_line(color="black"), axis.line.x = element_line(color="black"))  

B11529BAM+theme(axis.text=element_text(size=12),
                axis.text.x=element_text(size=12, angle = 45, vjust = 1, hjust=1),
                axis.title=element_text(size=14,face="bold"))
rlang::last_error()

#2____________________________________________________________
#BA.1 Monthly
my_data <- read.csv(file.choose())

attach(my_data)

cor.test(M,log_BA1,method = "pearson")

BA1M <- ggplot(my_data, aes(x=M, y=log_BA1)) + 
  geom_point(color = "black", size = 5)+ 
  annotate("text", x = 5, y = 0, label = "BA.1", col = "blue", size = 8) + 
  annotate("text", x = 5, y = -0.4, label = "r = 0.9146366", col = "blue", size = 7) + 
  annotate("text", x = 5, y = -0.8, label = "P = 0.265", col = "blue", size = 7) +
  annotate("text", x = 5, y = -1.2, label = "Dec'21 = 31%", col = "blue", size = 7) +
  geom_smooth(method=lm, se=FALSE, color = "blue") + 
  geom_vline(xintercept=12.96, linetype="dashed", 
             color = "orange", size=1) +
  geom_vline(xintercept=13.04, linetype="dashed", 
             color = "red", size=1) +
  scale_x_continuous(name = "Months", labels = Monthly, limits = c(1,14), breaks = seq(1, 14, 1)) + 
  scale_y_continuous(name = "Prevalence (Logarithmic Transformed Ratio)", limits = c(-5, 0), breaks = seq(-5, 0, 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank(), axis.line = element_line(color="black"), axis.line.x = element_line(color="black"))  

BA1M+theme(axis.text=element_text(size=12),
           axis.text.x=element_text(size=12, angle = 45, vjust = 1, hjust=1),
           axis.title=element_text(size=14,face="bold"))

#3____________________________________________________________
#BA.1.1 Monthly
my_data <- read.csv(file.choose())

attach(my_data)

cor.test(M,log_BA11,method = "pearson")

BA11M <- ggplot(my_data, aes(x=M, y=log_BA11)) + 
  geom_point(color = "black", size = 5)+ 
  annotate("text", x = 5, y = 0, label = "BA.1.1", col = "blue", size = 8) + 
  annotate("text", x = 5, y = -0.4, label = "r = 0.9276997", col = "blue", size = 7) + 
  annotate("text", x = 5, y = -0.8, label = "P = 0.2436", col = "blue", size = 7) +
  annotate("text", x = 5, y = -1.2, label = "Dec'21 = 11.5%", col = "blue", size = 7) +
  geom_smooth(method=lm, se=FALSE, color = "blue") + 
  geom_vline(xintercept=13, linetype="dashed", 
             color = "orange", size=1) +
  geom_vline(xintercept=14, linetype="dashed", 
             color = "red", size=1) +
  scale_x_continuous(name = "Months", labels = Monthly, limits = c(1,14), breaks = seq(1, 14, 1)) + 
  scale_y_continuous(name = "Prevalence (Logarithmic Transformed Ratio)", limits = c(-5, 0), breaks = seq(-5, 0, 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank(), axis.line = element_line(color="black"), axis.line.x = element_line(color="black"))  

BA11M+theme(axis.text=element_text(size=12),
            axis.text.x=element_text(size=12, angle = 45, vjust = 1, hjust=1),
            axis.title=element_text(size=14,face="bold"))

#4____________________________________________________________
#BA.2 Monthly
my_data <- read.csv(file.choose())

attach(my_data)

cor.test(M,log_BA2,method = "pearson")

BA2M <- ggplot(my_data, aes(x=M, y=log_BA2)) + 
  geom_point(color = "black", size = 5)+ 
  annotate("text", x = 5, y = 0, label = "BA.2", col = "blue", size = 8) + 
  annotate("text", x = 5, y = -0.4, label = "r = 0.9888815 ", col = "blue", size = 7) + 
  annotate("text", x = 5, y = -0.8, label = "P = 0.09502", col = "blue", size = 7) +
  annotate("text", x = 5, y = -1.2, label = "Dec'21 = 0.29%", col = "blue", size = 7) +
  geom_smooth(method=lm, se=FALSE, color = "blue") + 
  geom_vline(xintercept=14, linetype="dashed", 
             color = "orange", size=1) +
  scale_x_continuous(name = "Months", labels = Monthly, limits = c(1,14), breaks = seq(1, 14, 1)) + 
  scale_y_continuous(name = "Prevalence (Logarithmic Transformed Ratio)", limits = c(-5, 0), breaks = seq(-5, 0, 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank(), axis.line = element_line(color="black"), axis.line.x = element_line(color="black"))  

BA2M+theme(axis.text=element_text(size=12),
           axis.text.x=element_text(size=12, angle = 45, vjust = 1, hjust=1),
           axis.title=element_text(size=14,face="bold"))

#5____________________________________________________________
#BA.3 Monthly
my_data <- read.csv(file.choose())

attach(my_data)

cor.test(M,log_BA3,method = "pearson")

BA3M <- ggplot(my_data, aes(x=M, y=log_BA3)) + 
  geom_point(color = "black", size = 5)+ 
  annotate("text", x = 5, y = 0, label = "BA.3", col = "blue", size = 8) + 
  annotate("text", x = 5, y = -0.4, label = "r = 0.9982936", col = "blue", size = 7) + 
  annotate("text", x = 5, y = -0.8, label = "P = 0.0372 *", col = "blue", size = 7) +
  annotate("text", x = 5, y = -1.2, label = "Dec'21 = 0.03%", col = "blue", size = 7) +
  geom_smooth(method=lm, se=FALSE, color = "blue") + 
  geom_vline(xintercept=33, linetype="dashed", 
             color = "orange", size=1) +
  geom_vline(xintercept=45, linetype="dashed", 
             color = "red", size=1) +
  scale_x_continuous(name = "Months", labels = Monthly, limits = c(1,14), breaks = seq(1, 14, 1)) + 
  scale_y_continuous(name = "Prevalence (Logarithmic Transformed Ratio)", limits = c(-5, 0), breaks = seq(-5, 0, 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank(), axis.line = element_line(color="black"), axis.line.x = element_line(color="black"))  

BA3M+theme(axis.text=element_text(size=12),
           axis.text.x=element_text(size=12, angle = 45, vjust = 1, hjust=1),
           axis.title=element_text(size=14,face="bold"))



