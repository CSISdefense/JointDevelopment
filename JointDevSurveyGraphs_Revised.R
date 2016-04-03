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
library(scales)   # for percent format in y-axis
################################################################################
#     OPEN FILES AND LOAD DATA
################################################################################

# save original working directory
originalwd <- getwd()

# OPEN FILES:
# Reads excel sheets by name with read.xlsx2() [xlsx package]
# and creates data tables [data.table package] with the data read.
# Check file path names and spreadsheet names if this isn't working
setwd("K:/Development/JointDevelopment")

JSF <- data.table(read.xlsx2("./Surveys/Response MatrixJSF.xlsx", 
                  sheetName = "Sheet2"))
M777 <- data.table(read.xlsx2("./Surveys/Response MatrixM777.xlsx", 
                  sheetName = "Sheet2"))
AGS <- data.table(read.xlsx2("./Surveys/Response MatrixAGS.xlsx", 
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

# CHANGE QUESTION TITLES AND LEVELS IN THESE DOCUMENTS
# The '\n' is a line break character
questions <- data.table(read.csv("./Surveys/Questions.csv"))
SurveyLevels <- data.table(read.csv("./Surveys/SurveyLevels.csv"))


################################################################################
#     PREPARE TO CREATE GRAPHS
################################################################################

SurveySummary<-ddply(SurveyData, 
      .(Characteristic,Score,Program),
      .fun=summarise,
      Weight=sum(Weight)
)

ZeroSurveySummary<-expand.grid(Characteristic=unique(SurveySummary$Characteristic),
                               Program=unique(SurveySummary$Program),
                               Score=1:6)
ZeroSurveySummary$Weight<-0 

SurveySummary<-ddply(rbind(SurveySummary,ZeroSurveySummary), 
                     .(Characteristic,Score,Program),
                     .fun=summarise,
                     Weight=sum(Weight)
)

SurveySummary<-ddply(SurveySummary, 
                     .(Characteristic,Program),
                     .fun=mutate,
                     Percent=Weight/sum(Weight)
)

SurveySummary$CharacteristicNumber<-substring(SurveySummary$Characteristic,1,1)
SurveySummary$CharacteristicLetter<-substring(SurveySummary$Characteristic,2,2)
SurveySummary$CharacteristicLetter[SurveySummary$CharacteristicLetter==""]<-NA

SurveySummary<-join(SurveySummary,questions,by="Characteristic")

SurveySummary[order(SurveySummary$Program,SurveySummary$Characteristic,SurveySummary$Score),]

# questions <- dist(SurveySummary$Characteristic)
questionsNumber <- unique(SurveySummary$CharacteristicNumber)




# CHANGE QUESTION TITLES HERE
# The '\n' is a line break character
# SurveySummary$SubTitle<-NA
# SurveySummary$SubTitle[SurveySummary$Characteristic=="1a"]<-"Between Government\nand Industry"
# SurveySummary$SubTitle[SurveySummary$Characteristic=="1b"]<-"Between Participating\nGovernments"
# SurveySummary$SubTitle[SurveySummary$Characteristic=="1c"]<- "Between Participating\nIndustries"
# SurveySummary$SubTitle[SurveySummary$Characteristic=="3a"]<-  "Impact of\nOperational Needs"
# SurveySummary$SubTitle[SurveySummary$Characteristic=="3b"]<-  "Impact of\nPolitical Needs"
# SurveySummary$SubTitle[SurveySummary$Characteristic=="7a"]<-  "Technology Motivation"
# SurveySummary$SubTitle[SurveySummary$Characteristic=="7b"]<-  "Cost Motivation"
# SurveySummary$SubTitle[SurveySummary$Characteristic=="8a"]<-  "By Comparative\nAdvantage"
# SurveySummary$SubTitle[SurveySummary$Characteristic=="8b"]<-  "By Political or\nIndustrial Goals"


questionTitle2 <- c("Figure 1.  Integration",
                   "Figure 2. Number of\nParticipating Countries",
                   "Figure 3. Decision Making",
                   "Figure 4. Commitment",
                   "Figure 5. Flexibility",
                   "Figure 6. Alignment of\nOperational Needs",
                   "Figure 7. Tradeoff Between\nLeading-Edge Technology and Cost",
                   "Figure 8. Workshare Distribution"
                   
                   )


# Find the maximum total weight for any one combination of score, program, and
# question;

# This will be used to set the scale of the Y axis on graphs, ensuring that
# A) Graphs have a consistent scale, and 
# B) All data fits on the graphs

weightsum <- with(SurveySummary, aggregate(Weight ~ Program + Score +
                                              Characteristic, FUN = sum))
maxheight <- max(weightsum$Weight)

################################################################################
#     BUILD AND SAVE GRAPHS
################################################################################
# 
# for(i in seq_along(questions)){
# 
#       oneQdata <- filter(SurveySummary, Characteristic == questions[i])
#       ggplot(oneQdata, aes(x=Score, y = Weight, fill = Program)) +
#             geom_bar(stat = "identity") +
#             labs(y= "Frequency", x= "Score") +
#             facet_grid(Program ~ .) +
#             scale_x_continuous(limits = c(0.5,6.5), breaks =c(1,2,3,4,5,6)) +
#             scale_y_continuous(limits = c(0,maxheight), 
#                                breaks = seq(0, floor(maxheight), 1)) +
#             scale_fill_brewer(palette = "Spectral") +
#             ggtitle(bquote(atop(.(questionTitle[i]),
#                                 atop(italic(.(questionSubTitle[i])), "")))) +
#             theme(plot.title=element_text(size = rel(1), face = "bold")) +
#             theme(strip.text = element_blank(), 
#                   strip.background = element_blank(),
#                   legend.position="right")
# 
# # CHANGE SAVE LOCATION HERE
#       filepath <- paste("K:/Development/JointDevelopment/FinalGraphs/",
#                         "q",questions[i],".png",sep = "")
# 
# # CHANGE GRAPH SIZE AND RESOLUTION HERE            
#       ggsave(filepath, width = 3, height = 3, unit="in", dpi = 300)      
# }



for(i in seq_along(questionsNumber)){
    
    oneQdata <- filter(SurveySummary, CharacteristicNumber == questionsNumber[i])
    
    if(any(is.na(oneQdata$CharacteristicLetter))){
        ggplot(oneQdata, aes(x=Score, y= Percent,  fill=Program, color=Program)) + # y = Weight,
            geom_area(alpha=0.5, position = "identity") +#stat = "identity"
            labs(y= "Percent of Responses", x= "Score") +
            #facet_grid(Program ~ .) +
            scale_x_continuous(limits = c(0.5,6.5), breaks =c(1,2,3,4,5,6)) +
            scale_y_continuous( labels = percent_format())+
#             scale_y_continuous(limits = c(0,maxheight), 
#                                breaks = seq(0, floor(maxheight), 1)) +
            scale_fill_brewer(palette = "Accent") +
            scale_color_brewer(palette = "Accent") +
            ggtitle(questionTitle2[i]) +
            theme(plot.title=element_text(size = rel(1), face = "bold")) +
            theme(strip.text = element_blank(), 
                  strip.background = element_blank(),
                  legend.position="right")
    }
    else{
        ggplot(oneQdata, aes(x=Score, y = Percent, fill = Program, color=Program)) +
            geom_area(alpha=0.5, position = "identity") +
            labs(y= "Percent of Responses", x= "Score") +
            facet_grid(. ~ SubTitle) +
            scale_x_continuous(limits = c(0.5,6.5), breaks =c(1,2,3,4,5,6)) +
            scale_y_continuous( labels = percent_format())+
        # scale_y_continuous(limits = c(0,maxheight), 
                               # breaks = seq(0, floor(maxheight), 1)) +
            scale_fill_brewer(palette = "Accent") +
            scale_color_brewer(palette = "Accent") +
            ggtitle(questionTitle2[i]) +
            theme(plot.title=element_text(size = rel(1), face = "bold")) +
            theme(#strip.text = element_blank(), 
                  #strip.background = element_blank(),
                  legend.position="right")
    }
    # CHANGE SAVE LOCATION HERE
    filepath <- paste("K:/Development/JointDevelopment/FinalGraphs/",
                      "q",questionsNumber[i],".png",sep = "")
    
    graphwidth<-2+1.333*length(unique(oneQdata$CharacteristicLetter))
    # CHANGE GRAPH SIZE AND RESOLUTION HERE            
    ggsave(filepath, width = graphwidth, height = 3, unit="in", dpi = 300)      
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
# setkey(SurveySummary, Characteristic)
# firsthalf <- SurveySummary[c("1a","1b","1c","2","3a","3b","4")]
# secondhalf <- SurveySummary[c("5","6","7a","7b","8a","8b")]
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
