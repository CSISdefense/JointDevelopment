# SURVEY DATA VISUALIZATIONS FOR JOINT DEVELOPMENT PROJECT
# MARCH 18 2016

################################################################################
#     INCLUDE PACKAGES USED
################################################################################
library(xlsx)     # to read excel files
library(data.table)     # for data management
library(ggplot2)  # to produce graphics
library(grid)   # to produce graphics
library(dplyr)    # for data management
library(plyr)     # for data management
library(tidyr)    # for data management
library(scales)   # for percent format in y-axis

options(error=recover)
options(warn=1)

################################################################################
#     OPEN FILES AND LOAD DATA
################################################################################

# save original working directory
originalwd <- getwd()

# OPEN FILES:
# Reads excel sheets by name with read.xlsx2() [xlsx package]
# and creates data tables [data.table package] with the data read.
# Check file path names and spreadsheet names if this isn't working
# setwd("K:/Development/JointDevelopment")
setwd("D:/Users/Greg Sanders/Documents/Development/JointDevelopment")

# JSF <- data.table(read.xlsx2("./Surveys/Response MatrixJSF.xlsx", 
#                              sheetName = "Sheet3"))
# M777 <- data.table(read.xlsx2("./Surveys/Response MatrixM777.xlsx", 
#                               sheetName = "Sheet3"))
# AGS <- data.table(read.xlsx2("./Surveys/Response MatrixAGS.xlsx", 
#                              sheetName = "Sheet3"))

JSF <- data.table(read.csv("./Surveys/Survey_JSF_Anonymous.csv"))
M777 <- data.table(read.csv("./Surveys/Survey_M777_Anonymous.csv"))
AGS <- data.table(read.csv("./Surveys/Survey_AGS_Anonymous.csv"))
MEADS <- data.table(read.csv("./Surveys/Survey_MEADS_Anonymous.csv"))
SM3 <- data.table(read.csv("./Surveys/Survey_SM3_Anonymous.csv"))
A400m <- data.table(read.csv("./Surveys/Survey_A400m_Anonymous.csv"))

SurveyData <- rbind(JSF, M777, AGS, MEADS, SM3, A400m, fill = TRUE)
SurveyData$x <- as.numeric(as.character(SurveyData$x))
SurveyData$y <- as.numeric(as.character(SurveyData$y))
SurveyData$Weight <- as.numeric(as.character(SurveyData$Weight))


# CHANGE QUESTION TITLES AND LEVELS IN THESE DOCUMENTS
# The '\n' is a line break character
questions <- data.table(read.csv("./Surveys/Questions.csv"))
SurveyLevels <- data.table(read.csv("./Surveys/SurveyLevels.csv"))

SurveyLevels$Level <- gsub("\\\\n","\n",SurveyLevels$Level)
SurveyLevels$LevelEnds <- gsub("\\\\n","\n",SurveyLevels$LevelEnds)
questions$SubTitle <- gsub("\\\\n","\n",questions$SubTitle)

################################################################################
#     PREPARE TO CREATE GRAPHS
################################################################################
# 
# SurveySummary<-melt.data.table(SurveyData
#                 ,id.vars=c("Program","CharacteristicNumber","Stakeholder","StakeholderPart","Weight")
#                 ,variable.name = "CharacteristicLetter"
#                 ,value.name="Score")
# 
# SurveySummary$CharacteristicLetter[SurveySummary$CharacteristicNumber %in% c(2,4,5,6)]<-NA
# 
# SurveySummary$Characteristic<-paste(SurveySummary$CharacteristicNumber,ifelse(is.na(SurveySummary$CharacteristicLetter)
#                                                                               ,""
#                                                                               ,as.character(SurveySummary$CharacteristicLetter)),sep="")


SurveySummary<-subset(SurveyData,!Characteristic %in% c("3","7","8"),select=-c(y))
SurveySummary<-dplyr::rename(SurveySummary,Score=x)


SurveySummary<-ddply(SurveySummary, 
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
                     Weight=sum(Weight,na.rm=TRUE) #JSF has some missing responses
)

SurveySummary<-ddply(SurveySummary, 
                     .(Characteristic,Program),
                     .fun=mutate,
                     Percent=Weight/sum(Weight,na.rm=TRUE) #JSF has some missing responses
)




SurveySummary$CharacteristicNumber<-substring(SurveySummary$Characteristic,1,1)
SurveySummary$CharacteristicLetter<-substring(SurveySummary$Characteristic,2,2)
SurveySummary$CharacteristicLetter[SurveySummary$CharacteristicLetter==""]<-NA



SurveyLevels$CharacteristicNumber<-substring(SurveyLevels$Characteristic,1,1)
SurveyLevels$CharacteristicLetter<-substring(SurveyLevels$Characteristic,2,2)
SurveyLevels$CharacteristicLetter[SurveyLevels$CharacteristicLetter==""]<-NA


# questions
SurveySummary<-join(SurveySummary,questions,by="Characteristic")

SurveyAverage<-ddply(SurveySummary,
                     .(Characteristic,Program),
                     .fun=summarise,
                     Average=sum(Weight*Score,na.rm=TRUE)/sum(Weight,na.rm=TRUE),
                     sum(Weight,na.rm=TRUE)
)
#Manually jittering average matches to enable them to be displayed.
SurveyAverage$Round<-round(SurveyAverage$Average,1)
SurveyDuplicates<-plyr::count(SurveyAverage,c("Characteristic","Round"))
SurveyDuplicates$Adjustment<-(SurveyDuplicates$freq-1)/-2/10
SurveyAverage<-plyr::join(SurveyAverage,SurveyDuplicates)
SurveyAverage<-SurveyAverage[order(SurveyAverage$Average),]
SurveyAverage$AvgAdjusted<-SurveyAverage$Average
for(i in 1:nrow(SurveyAverage)){
  if(SurveyAverage$freq[i]>1){
    SurveyAverage$AvgAdjusted[i]<-SurveyAverage$Round[i]+
      SurveyDuplicates$Adjustment[SurveyDuplicates$Characteristic==SurveyAverage$Characteristic[i]&
                                    SurveyDuplicates$Round==SurveyAverage$Round[i]]
    SurveyDuplicates$Adjustment[SurveyDuplicates$Characteristic==SurveyAverage$Characteristic[i]&
                                  SurveyDuplicates$Round==SurveyAverage$Round[i]]<-
      SurveyDuplicates$Adjustment[SurveyDuplicates$Characteristic==SurveyAverage$Characteristic[i]&
                                    SurveyDuplicates$Round==SurveyAverage$Round[i]]+0.1
  }
}
                        

SurveyAverage<-plyr::join(SurveySummary,
                    SurveyAverage)
                     

SurveySummary[order(SurveySummary$Program,SurveySummary$Characteristic,SurveySummary$Score),]
SurveyLevels[order(SurveyLevels$Characteristic,SurveyLevels$Score),]

# questions <- dist(SurveySummary$Characteristic)




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


questionTitle <- c("Figure 1. Extent of Integration",
                   "Figure 2. Number of Participating Countries",
                   "Figure 3. Decision Making",
                   "Figure 4. Commitment",
                   "Figure 5. Flexibility",
                   "Figure 6. Alignment of Operational Needs",
                   "Figure 7. Tradeoff Between Leading-Edge Technology and Cost",
                   "Figure 8. Basis of Workshare Distribution"
                   
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
#       filepath <- paste("./FinalGraphs/",
#                         "q",questions[i],".png",sep = "")
# 
# # CHANGE GRAPH SIZE AND RESOLUTION HERE            
#       ggsave(filepath, width = 3, height = 3, unit="in", dpi = 300)      
# }



for(i in c(1,2,4,5,6)){
    
    oneQdata <- filter(SurveySummary, CharacteristicNumber == i)
    oneQlabels <- filter(SurveyLevels, CharacteristicNumber == i)
    oneQaverage <- filter(SurveyAverage, CharacteristicNumber == i)
    
    if(any(is.na(oneQdata$CharacteristicLetter))){
        ggplot(oneQdata, aes(x=Score, y= Percent,  fill=Program, color=Program)) + # y = Weight,
            geom_area(alpha=0.5, position = "identity") +#stat = "identity"
            geom_vline(data=oneQaverage, 
                       aes(xintercept=AvgAdjusted, color = Program),
                       size=1,
                       linetype="dashed")+#
            labs(y= "Percent of Responses",
                 
                 x=gsub("\n"," ",filter(questions, substring(questions$Characteristic,1,1)== i)$SubTitle)) + #, x= "Score") + #, x= "Score"
            #facet_grid(Program ~ .) +
            scale_x_continuous( breaks=c(1:nrow(oneQlabels)),
                                limits = c(0.5,6.5),
                                labels=paste(oneQlabels$Score,
                                             ifelse(oneQlabels$LevelEnds=="","",
                                                 paste("\n",oneQlabels$LevelEnds,sep="")),sep=""))+
            scale_y_continuous( labels = percent_format())+
            #             scale_y_continuous(limits = c(0,maxheight), 
            #                                breaks = seq(0, floor(maxheight), 1)) +
            scale_fill_brewer(palette = "Accent") +
            scale_color_brewer(palette = "Accent") +
            ggtitle(questionTitle[i]) +
            theme(plot.title=element_text(size = rel(1), face = "bold",hjust=0.4)) +
            theme(axis.text.x=element_text(angle=0,
                                           size=7,
                                           vjust = 1, 
                                           hjust=0.5),
                  axis.text.y=element_text(size=7),
                  axis.title.x=element_text(size=8,hjust=0.4),
                  axis.title.y=element_text(size=8))+ #size=axis.text.
            theme(strip.text = element_blank(), 
                  strip.background = element_blank(),
                  legend.text=element_text(size=8),
                  plot.margin=unit(c(0,0.25,0,0),"cm"),
                  legend.margin=unit(0,"cm"),
                  legend.key.size=unit(0.25,"cm"),
                  legend.position="right")
    }
    else{
        #This is a trick to allow each of the multiple part questions to have a different score
        #1-6 for a, 7-12 for b, and 13-18 for c
        oneQdata$Score[oneQdata$CharacteristicLetter=="b"]<-oneQdata$Score[oneQdata$CharacteristicLetter=="b"]+6
        oneQdata$Score[oneQdata$CharacteristicLetter=="c"]<-oneQdata$Score[oneQdata$CharacteristicLetter=="c"]+12
        # browser()
        oneQaverage$AvgAdjusted[oneQaverage$CharacteristicLetter=="b"]<-oneQaverage$AvgAdjusted[oneQaverage$CharacteristicLetter=="b"]+6
        oneQaverage$AvgAdjusted[oneQaverage$CharacteristicLetter=="c"]<-oneQaverage$AvgAdjusted[oneQaverage$CharacteristicLetter=="c"]+12
        ggplot(oneQdata, aes(x=Score, y = Percent, fill = Program, color=Program)) +
            geom_area(alpha=0.5, position = "identity") +
            geom_vline(
              data=oneQaverage, aes(xintercept=AvgAdjusted, color = Program), 
              size=1,
              linetype="dashed")+#linetype="dashed" position="dodge",
            labs(y= "Percent of Responses") + #, x= "Score"
            scale_x_continuous(breaks=c(1:nrow(oneQlabels)),
                               labels=paste(oneQlabels$Score,
                                            ifelse(oneQlabels$LevelEnds=="","",
                                                   paste("\n",oneQlabels$LevelEnds,sep="")),sep=""))+
            facet_grid(. ~ SubTitle, scales="free_x") +
            scale_y_continuous( labels = percent_format())+
            # scale_y_continuous(limits = c(0,maxheight), 
            # breaks = seq(0, floor(maxheight), 1)) +
            scale_fill_brewer(palette = "Accent") +
            scale_color_brewer(palette = "Accent") +
            ggtitle(questionTitle[i]) +
            theme(plot.title=element_text(size = rel(1), face = "bold")) +
            theme(axis.text.x=element_text(angle=0,
                                           size=7,
                                           vjust = 1, 
                                           hjust=0.5),
                  axis.text.y=element_text(size=7),
                  axis.title.x=element_text(size=8),
                  axis.title.y=element_text(size=8))+ #size=axis.text.
            
            theme(strip.text = element_text(size=7), 
                  panel.margin=unit(0.35,"in"),#strip.background = element_blank(),
                  axis.title.x=element_blank(),
                  legend.text=element_text(size=8),
                  plot.margin=unit(c(0,0.75,-0.1,0),"cm"),
                  # panel.margin=unit(0.5,"cm"),
                  legend.margin=unit(0,"cm"),
                  legend.key.size=unit(0.25,"cm"),
                  legend.position="bottom")
    }
    # CHANGE SAVE LOCATION HERE
    filepath <- paste("./FinalGraphs/",
                      "q",i,".png",sep = "")
    
    graphwidth<-2+1.5*length(unique(oneQdata$CharacteristicLetter))
    # CHANGE GRAPH SIZE AND RESOLUTION HERE            
    ggsave(filepath, width = graphwidth, height = 2.25, unit="in", dpi = 450)      
}





# SurveyData<-ddply(SurveyData, 
#                      .(Characteristic,Program),
#                      .fun=mutate,
#                      Percent=Weight/sum(Weight,na.rm=TRUE) #JSF has some missing responses
# )

SurveyData$CharacteristicNumber<-substring(SurveyData$Characteristic,1,1)
SurveySumcheck<-ddply(SurveyData, 
                      .(Characteristic,Program,Stakeholder),
                      .fun=summarise,
                      Weight=sum(Weight,na.rm = TRUE)
)
subset(SurveySumcheck,is.na(Weight)|abs(Weight-1)>0.02)



for(i in c(3,7,8)){
    
    oneQdata <- filter(SurveyData, CharacteristicNumber == i )
    oneQdata<-dplyr::rename(oneQdata, ScoreX=x)
    oneQdata<-dplyr::rename(oneQdata, ScoreY=y)
    oneQdata$StakeholderPart[is.na(oneQdata$StakeholderPart)]<-''
    oneQdata<-oneQdata[complete.cases(oneQdata),]
    
    oneQdata<-ddply(oneQdata, 
                    .(CharacteristicNumber,ScoreX,ScoreY,Program),
                    .fun=summarise,
                    Weight=sum(Weight)
    )
    
    
    oneQdata<-ddply(oneQdata, 
                    .(CharacteristicNumber,Program),
                    .fun=mutate,
                    Percent=Weight/sum(Weight,na.rm=TRUE) #JSF has some missing responses
    )
    
    oneQlabelsA <- filter(SurveyLevels, CharacteristicNumber == i &CharacteristicLetter=='a')
    oneQlabelsB <- filter(SurveyLevels, CharacteristicNumber == i &CharacteristicLetter=='b')
    
    
    
    ggplot(oneQdata, aes(x=ScoreX, y = ScoreY, size = Percent, color=Program)) +
        geom_point() +
        scale_size_area(labels = percent_format())+
        labs(x= gsub("\n"," ",filter(questions, substring(questions$Characteristic,1,1) == i  &
                                         substring(questions$Characteristic,2,2)=='a')$SubTitle),
             y= gsub("\n"," ",filter(questions, substring(questions$Characteristic,1,1)== i  &
                                         substring(questions$Characteristic,2,2)=='b')$SubTitle)) +
        scale_x_continuous(breaks=c(1:nrow(oneQlabelsA)),
                           limits = c(0.5,6.5),
                         
                           labels=  paste(oneQlabelsA$Score,
                                          ifelse(oneQlabelsA$LevelEnds=="","",
                                                 paste("\n",oneQlabelsA$LevelEnds,sep="")),sep=""))+#limits = c(0.5,6.5), breaks =c(1,2,3,4,5,6)) +
        facet_wrap( ~ Program) +
        scale_y_continuous(breaks=c(1:nrow(oneQlabelsB)),
                           limits = c(0.5,6.5),
                           labels=paste(oneQlabelsB$Score,
                                        ifelse(oneQlabelsB$LevelEnds=="","",
                                               paste("-",oneQlabelsB$LevelEnds,sep="")),sep=""))+#limits = c(0.5,6.5), breaks =c(1,2,3,4,5,6)) +
                               # paste(oneQlabelsB$Score,oneQlabelsB$LevelEnds,sep=""))+#limits = c(0.5,6.5), breaks =c(1,2,3,4,5,6)) +
        # scale_y_continuous(limits = c(0,maxheight), 
        # breaks = seq(0, floor(maxheight), 1)) +
        scale_fill_brewer(palette = "Accent") +
        scale_color_brewer(palette = "Accent") +
        ggtitle(questionTitle[i]) +
        theme(plot.title=element_text(size = rel(1), face = "bold")) +
        theme(axis.text.x=element_text(angle=0,
                                       size=7,
                                       vjust = 1, 
                                       hjust=0.5))+ #size=axis.text.
        theme(axis.text.y=element_text(angle=0,
                                       size=7,
                                       vjust = 0.5, 
                                       hjust=0.5))+ #size=axis.text.
        guides(color=FALSE)+
        geom_abline(slope=1,lty=2,alpha=0.5)+
        annotate("text",
                 x = 6, 
                 y = 0.5, 
                 label = filter(questions, substring(questions$Characteristic,1,1) == i  &
                                    substring(questions$Characteristic,2,2)=='a')$ScatterAnnotation,
                 alpha=0.5,
                 size=2,
                 hjust=1)+
        annotate("text",
                 x = 0.5,
                 y = 6, 
                 angle = 90,
                 label = filter(questions, substring(questions$Characteristic,1,1) == i  &
                                    substring(questions$Characteristic,2,2)=='b')$ScatterAnnotation,
                 alpha=0.5,
                 size=2,
                 hjust=1)+
        annotate("text",
                 x = 0.75,
                 y = 0.75, 
                 angle = 45,
                 label = "Equal Influence",
                 size=2,
                 alpha=0.5,
                 hjust=0,
                 vjust=1)+
        geom_segment(aes(x = 4,
                         y = 1,
                         xend = 6, 
                         yend = 1), 
                     colour='#000000',
                     size=0.5,
                     alpha=0.5,
                     arrow = arrow(length = unit(0.2, "cm"))
                     )+
        geom_segment(aes(x = 1,
                         y = 4,
                         xend = 1, 
                         yend = 6), 
                     colour='#000000',
                     size=0.5,
                     alpha=0.5,
                     arrow = arrow(length = unit(0.2, "cm"))
        )+
        # geom_abline()
        theme(axis.title.x=element_text(size=8),
              axis.title.y=element_text(size=7,hjust=0.65)
              # plot.title=element_text(hjust=0)
        )+
        theme(aspect.ratio = 1)+
        theme(#strip.text = element_blank(), 
            #strip.background = element_blank(),
            # axis.title.x=element_blank(),
            # axis.title.y=element_blank(),
            strip.text = element_text(size=7), 
            legend.text=element_text(size=8),
            legend.margin=unit(0,"cm"),
            plot.margin=unit(c(0,0,0,0),"cm"),
            panel.margin=unit(0.5,"cm"),
            legend.key.size=unit(0.1,"cm"),
            legend.position="right")
    
    # CHANGE SAVE LOCATION HERE
    filepath <- paste("./FinalGraphs/",
                      "q",i,"scatter.png",sep = "")
    
    graphwidth<-6.5
    # CHANGE GRAPH SIZE AND RESOLUTION HERE            
    ggsave(filepath, width = graphwidth, height = 3.5, unit="in", dpi = 450)      
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



# write.xlsx2(JSF,"./Surveys/Response MatrixJSF.xlsx", 
#             sheetName = "Sheet2a",append=TRUE)
# write.xlsx2(M777,"./Surveys/Response MatrixM777.xlsx", 
#             sheetName = "Sheet2a",append=TRUE)
# write.xlsx2(AGS,"./Surveys/Response MatrixAGS.xlsx", 
#             sheetName = "Sheet2a",append=TRUE)

# fixes numeric data that got imported as factors 
# ignore the NA warning, NAs are being correctly recognized now, not introduced
# JSF$Score <- as.numeric(as.character(JSF$Score))
# JSF$a <- as.numeric(as.character(JSF$a))
# JSF$b <- as.numeric(as.character(JSF$b))
# JSF$c <- as.numeric(as.character(JSF$c))
# JSF$Weight <- as.numeric(as.character(JSF$Weight))
# M777$a <- as.numeric(as.character(M777$a))
# M777$b <- as.numeric(as.character(M777$b))
# M777$c <- as.numeric(as.character(M777$c))
# # M777$Score <- as.numeric(as.character(M777$Score))
# M777$Weight <- as.numeric(as.character(M777$Weight))
# SurveyData$Score <- as.numeric(as.character(SurveyData$Score))


# MERGE DATA:
# tags each dataset with the program it comes from, then merges datasets 
# JSF <- mutate(JSF, Program = "JSF")
# M777 <- mutate(M777, Program = "M777")
# AGS <- mutate(AGS, Program = "AGS")
# SurveyData$CharacteristicNumber <- as.numeric(as.character(SurveyData$CharacteristicNumber))

# Unchanged<-subset(SurveyData,CharacteristicNumber>1)
# 
# 
# Rotate<-subset(SurveyData,CharacteristicNumber==1)
# Rotate<-melt(Rotate,id=c("Program","CharacteristicNumber","Stakeholder","StakeholderPart","Weight"),
#              value.name="x",
#              variable.name = "CharactersticLetter")
# Rotate$Characteristic<-paste(Rotate$CharacteristicNumber,Rotate$CharactersticLetter,sep="")
# Rotate<-subset(Rotate,select=-c(CharacteristicNumber,CharactersticLetter))
# Rotate<-ddply(Rotate,.(Characteristic,Program,Stakeholder,x),summarise,Weight=sum(Weight))
# Rotate$StakeholderPart<-NA
# Rotate$y<-NA

# Unchanged<-dplyr::rename(Unchanged,x=a)
# Unchanged<-dplyr::rename(Unchanged,y=b)
# Unchanged<-dplyr::rename(Unchanged,Characteristic=CharacteristicNumber)
# Unchanged<-subset(Unchanged,select=-c(c))
# 
# Recombined<-rbind(Rotate,Unchanged)
# SurveyRedone<-SurveyData
# SurveyRedone<-melt.data.table(SurveyRedone, Program + 
#                         CharacteristicNumber +
#                         Stakeholder + 
#                         StakeholderPart +
#                         Weight  ~
#                         CharacteristicLetter, 
#                     value.var="Score")
# Recombined<-subset(Recombined,select=-c(Stakeholder))
# Recombined$ProgramStakeholder<-paste(Recombined$Program,Recombined$Stakeholder,sep="-")
# Recombined$ProgramStakeholder<-factor(Recombined$ProgramStakeholder, levels=Stakeholders$name,labels=Stakeholders$number)
# Recombined<-dplyr::rename(Recombined,Stakeholder=ProgramStakeholder)
# 
# Stakeholders<-data.frame(name=levels(Recombined$ProgramStakeholder),
#                          number=sample(0:9999,length(levels(Recombined$ProgramStakeholder))))
# Stakeholders$Number<-
# SurveyRedone<-SurveyData
# SurveyRedone$CharacteristicLetter[SurveyRedone$CharacteristicLetter==""]<-"a"
# SurveyRedone<-dcast(SurveyRedone, Program + CharacteristicNumber + Stakeholder + StakeholderPart + Weight  ~ CharacteristicLetter, value.var="Score")
# Survey
# Recombined<-Recombined[order(Recombined$Program,Recombined$Characteristic,Recombined$Stakeholder),
#                        c("Program","Characteristic","Stakeholder","StakeholderPart","Weight","x","y")]
# 
# colnames(Recombined)
#  write.csv(subset(Recombined,Program=="JSF"),"./Surveys/Survey_JSF_Anonymous.csv")
#  write.csv(subset(Recombined,Program=="M777"),"./Surveys/Survey_M777_Anonymous.csv")
# write.csv(subset(Recombined,Program=="AGS"),"./Surveys/Survey_AGS_Anonymous.csv")
