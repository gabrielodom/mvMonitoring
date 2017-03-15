
###  Time Series Graphics  ###
# Create a grid with four rows by three columns. The first row will be 6 - 9
# hours of the NOC data, with the columns as the three features. The specific
# hours will be the first sets of hours after fault introduction The other three
# rows will be for the fault states: the individual graphs may be different if
# Fault B does not affect that feature.

library(tidyverse)
library(reshape2)
library(gridExtra)
library(xts)

# Start with a random draw. The faults were introduced at s = 8500
adpcaObs_ls <- mspProcessData(faults = "All",
                              faultStartIndex = 8500,
                              startTime = "2016-11-27 00:00:00 CST",
                              multiState = TRUE,
                              adpcaTest = TRUE)
adpcaObs_ls %>% str
8500 / 60 # This is in the middle of a state
141 * 60 # This is the beginning
faultStart <- index(adpcaObs_ls$NOC[8500,])

######  Create Graphing Function  ######
mspGraphsGrid <- function(data_xts){
  obs_df <- data_xts[7500:10080, 1:4] %>% as.data.frame() # 8461
  obs_df$dateTime <- index(data_xts[7500:10080,]) # 8461
  obs_df <- obs_df %>% mutate(state = as.factor(state))

  p1 <- ggplot(data = obs_df,
               aes(x = dateTime, y = x, color = state, group = 1)) +
    # group = 1 in the aesthetic means that you want different colours, but not
    # different groups. See
    # https://kohske.wordpress.com/2010/12/27/faq-geom_line-doesnt-draw-lines/
    scale_colour_manual(values = c(`1` = "red", `2` = "green", `3` = "blue"),
                        breaks = c("1", "2", "3"),
                        guide = FALSE) +
    geom_line() +
    scale_x_datetime(breaks = c(as.POSIXct("2016-12-02 12:00:00 CST"),
                                as.POSIXct("2016-12-03 12:00:00 CST")),
                     date_labels = "%m-%d %H:%M") +
    theme_bw() +
    theme(axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle("Feature X") +
    geom_vline(xintercept = as.numeric(faultStart), size = 1.2)

  p2 <- ggplot(data = obs_df,
               aes(x = dateTime, y = y, color = state, group = 1)) +
    scale_colour_manual(values = c(`1` = "red", `2` = "green", `3` = "blue"),
                        breaks = c("1", "2", "3"),
                        guide = FALSE) +
    geom_line() +
    scale_x_datetime(breaks = c(as.POSIXct("2016-12-02 12:00:00 CST"),
                                as.POSIXct("2016-12-03 12:00:00 CST")),
                     date_labels = "%m-%d %H:%M") +
    theme_bw() +
    theme(axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle("Feature Y") +
    geom_vline(xintercept = as.numeric(faultStart), size = 1.2)

  p3 <- ggplot(data = obs_df,
               aes(x = dateTime, y = z, color = state, group = 1)) +
    scale_colour_manual(values = c(`1` = "red", `2` = "green", `3` = "blue"),
                        breaks = c("1", "2", "3")) +
    geom_line() +
    scale_x_datetime(breaks = c(as.POSIXct("2016-12-02 12:00:00 CST"),
                                as.POSIXct("2016-12-03 12:00:00 CST")),
                     date_labels = "%m-%d %H:%M") +
    theme_bw() +
    theme(axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5),
          legend.justification=c(1,0), legend.position=c(1,0)) +
    ggtitle("Feature Z") +
    geom_vline(xintercept = as.numeric(faultStart), size = 1.2)

  grid.arrange(p1, p2, p3, ncol = 3)
}


######  Use Graphing Function  ######
adpcaObs_ls$NOC %>% mspGraphsGrid()
adpcaObs_ls$A1 %>% mspGraphsGrid()
adpcaObs_ls$B1 %>% mspGraphsGrid()
adpcaObs_ls$A2 %>% mspGraphsGrid()
adpcaObs_ls$B2 %>% mspGraphsGrid()
adpcaObs_ls$A3 %>% mspGraphsGrid()
adpcaObs_ls$B3 %>% mspGraphsGrid()
