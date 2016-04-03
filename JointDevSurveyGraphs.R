# SURVEY DATA VISUALIZATIONS FOR JOINT DEVELOPMENT PROJECT
# MARCH 18 2016

################################################################################
#     INCLUDE PACKAGES USED
################################################################################
library(xlsx)     # to read excel files
library(data.table)     # for data management
library(ggplot2)  # to produce graphics
library(dplyr)    # for data management
library(plyr)     # for data management
library(tidyr)    # for data management

################################################################################
#     OPEN FILES AND LOAD DATA
################################################################################

# save original working directory
originalwd <- getwd()

# OPEN FILES:
# Reads excel sheets by name with read.xlsx2() [xlsx package]
# and creates data tables [data.table package] with the data read.
# Check file path names and spreadsheet names if this isn't working
setwd("K:/2015-09 Joint Development/Surveys")

JSF <- data.table(read.xlsx2("./JSF F35/Responses/Response Matrix.xlsx", 
                  sheetName = "Sheet2"))
M777 <- data.table(read.xlsx2("./LW155/RESPONSES/Response Matrix.xlsx", 
                  sheetName = "Sheet2"))
AGS <- data.table(read.xlsx2("./NATO AGS/RESPONSES/Response Matrix.xlsx", 
                  sheetName = "Sheet2"))

# fixes numeric data that got imported as factors 
# ignore the NA warning, NAs are being correctly recognized now, not introduced
JSF$Score <- as.numeric(as.character(JSF$Score))
JSF$Weight <- as.numeric(as.character(JSF$Weight))
M777$Score <- as.numeric(as.character(M777$Score))
M777$Weight <- as.numeric(as.character(M777$Weight))
AGS$Score <- as.numeric(as.character(AGS$Score))
AGS$Weight <- as.numeric(as.character(AGS$Weight))

# MERGE DATA:
# tags each dataset with the program it comes from, then merges datasets 
JSF <- mutate(JSF, Program = "JSF")
M777 <- mutate(M777, Program = "M777")
AGS <- mutate(AGS, Program = "AGS")

SurveyData <- rbind(JSF, M777, AGS, fill = TRUE)



################################################################################
#     PREPARE TO CREATE GRAPHS
################################################################################

questions <- levels(SurveyData$Characteristic)

# CHANGE QUESTION TITLES HERE
# The '\n' is a line break character

questionTitle <- c("Integration between\ngovernment and industry",
                   "Integration between\nparticipating governments",
                   "Integration between\nparticipating industries",
                   "Impact of the number\nof participating countries",
                   "Impact of operational\nneeds on decision making",
                   "Impact of political needs\non decision making",
                   "Strength of explicit\nmeasures to prevent defection",
                   "Flexibility to change\nrequirements",
                   "Compatibility of requirements\nbetween countries",
                   "Motivated by demand for\nleading-edge technology",
                   "Motivated by demand for\nlow-cost economies of scale",
                   "Workshare distributed by\ncomparative advantage",
                   "Workshare distributed by\npolitical or industrial goals")


# Find the maximum total weight for any one combination of score, program, and
# question;

# This will be used to set the scale of the Y axis on graphs, ensuring that
# A) Graphs have a consistent scale, and 
# B) All data fits on the graphs

weightsum <- with(SurveyData, aggregate(Weight ~ Program + Score +
                                              Characteristic, FUN = sum))
maxheight <- max(weightsum$Weight)

################################################################################
#     BUILD AND SAVE GRAPHS
################################################################################

for(i in seq_along(questions)){

      oneQdata <- filter(SurveyData, Characteristic == questions[i])
      ggplot(oneQdata, aes(x=Score, y = Weight, fill = Program)) +
            geom_bar(stat = "identity") +
            labs(y= "Frequency", x= "Score") +
            facet_grid(Program ~ .) +
            scale_x_continuous(limits = c(0.5,6.5), breaks =c(1,2,3,4,5,6)) +
            scale_y_continuous(limits = c(0,maxheight), 
                               breaks = seq(0, floor(maxheight), 1)) +
            scale_fill_brewer(palette = "Set1") +
            ggtitle(questionTitle[i]) +
            theme(plot.title=element_text(size = rel(1.5), face = "bold")) +
            theme(strip.text = element_blank(), 
                  strip.background = element_blank())

# CHANGE SAVE LOCATION HERE
      filepath <- paste("K:/Development/JointDevelopment/FinalGraphs/",
                        "q",questions[i],".png",sep = "")

# CHANGE GRAPH SIZE AND RESOLUTION HERE            
      ggsave(filepath, width = 6, height = 6, unit="in", dpi = 300)      
}


# cleanup: return to original working directory
setwd(originalwd)





################################################################################
# END OF WORKING FILE:
# 
# AFTER THIS POINT EVERYTHING IS SAVED CODE FROM STUFF WE DIDN'T END UP USING
# 
# AND HAS ALL BEEN COMMENTED OUT
################################################################################



























# ################################################################################
# #     AVERAGE ACROSS SCORES BY WEIGHT
# ################################################################################
# 
# avgJSF <- ddply(JSF, .variables = c("Characteristic","Stakeholder"),
#                 .fun = summarize, Score = mean(Score))
# avgAGS <- ddply(AGS, .variables = c("Characteristic","Stakeholder"),
#                 .fun = summarize, Score = mean(Score))
# avgLW155 <- ddply(LW155, .variables = c("Characteristic","Stakeholder"),
#                   .fun = summarize, Score = mean(Score))
# 
# 
# ################################################################################
# #     CREATE DOT PLOTS FOR JSF
# ################################################################################
# 
# ggplot(avgJSF, aes(x = Score, fill = Stakeholder)) + 
#       geom_dotplot(stackdir = "centerwhole", stackgroups = TRUE, 
#                    method = "histodot", binwidth = 0.14) +
#       facet_grid(Characteristic ~ .) +
#       scale_y_continuous(breaks =NULL) +
#       theme(axis.title.y=element_blank()) +
#       theme(axis.title.x=element_blank()) +
#       scale_x_continuous(breaks =c(1,2,3,4,5,6)) +
#       ##       scale_fill_brewer(palette = "RdYlBu") +
#       ggtitle("JSF Survey responses by question")
# 
# 
# ################################################################################
# #     STACKED BARS BY QUESTION
# ################################################################################
#  
# # create two charts to have a more managable amount of data displayed in each
# setkey(SurveyData, Characteristic)
# firsthalf <- SurveyData[c("1a","1b","1c","2","3a","3b","4")]
# secondhalf <- SurveyData[c("5","6","7a","7b","8a","8b")]
# 
# # first half
# ggplot(firsthalf, aes(x=Score, y = Weight, fill = Program)) +
#       geom_bar(stat = "identity") +
#       facet_grid(Program ~ Characteristic) +
#       theme(axis.title.y=element_blank()) +
#       theme(axis.title.x=element_blank()) +
#       scale_x_continuous(breaks =c(1,2,3,4,5,6)) +
#       ggtitle("Ratings by question and program") +
#       guides(fill = FALSE)
# 
# # second half
# ggplot(secondhalf, aes(x=Score, y = Weight, fill = Program)) +
#       geom_bar(stat = "identity") +
#       facet_grid(Program ~ Characteristic) +
#       theme(axis.title.y=element_blank()) +
#       theme(axis.title.x=element_blank()) +
#       scale_x_continuous(breaks =c(1,2,3,4,5,6)) +
#       ggtitle("Ratings by question and program") +
#       guides(fill = FALSE)
# 
# ################################################################################
# #     OFFSET BARS BY PROGRAM
# ################################################################################
# 
# 
# ## NEEDS : CHECK Y SCALING
# 
# 
# 
# # Expand firsthalf dataframe to include all combinations of:
# # [Characteristic x Program x Score]
# # This is a kludge to get ggplot to display zero-height bars when no
# # respondants gave a certain score to a certain program.
# # Without the kludge, ggplot just surrenders that space to the programs
# # that do have data in that range, which makes their bars too fat.
# firsthalfall <- expand(firsthalf, Characteristic, Program,
#                         Score, Stakeholder)
# firsthalfplus <- left_join(firsthalfall, firsthalf)
# firsthalfplus$Weight[is.na(firsthalfplus$Weight)] <- 0
# 
# # first half
# ggplot(firsthalfplus, aes(x = Score, y = Weight, fill = Program)) +
#       geom_bar(stat = "identity", position = "dodge",) +
#       facet_grid(Characteristic ~ ., scale = "free_x", space="free") +
#       scale_y_continuous(breaks =NULL) +
#       theme(axis.title.y=element_blank()) +
#       theme(axis.title.x=element_blank()) +
#       scale_x_continuous(breaks =c(1,2,3,4,5,6)) +
#       scale_fill_brewer(palette = "Set2") +
#       ggtitle("Ratings by question and program")
# 
# # second half
# secondhalfall <- expand(secondhalf, Characteristic, Program,
#                        Score, Stakeholder)
# secondhalfplus <- left_join(secondhalfall, secondhalf)
# secondhalfplus$Weight[is.na(secondhalfplus$Weight)] <- 0
# 
# ggplot(secondhalfplus, aes(x = Score, y = Weight, fill = Program)) +
#       geom_bar(stat = "identity", position = "dodge",) +
#       facet_grid(Characteristic ~ ., scale = "free_x", space="free") +
#       scale_y_continuous(breaks =NULL) +
#       theme(axis.title.y=element_blank()) +
#       theme(axis.title.x=element_blank()) +
#       scale_x_continuous(breaks =c(1,2,3,4,5,6)) +
#       scale_fill_brewer(palette = "Set2") +
#       ggtitle("Ratings by question and program")
