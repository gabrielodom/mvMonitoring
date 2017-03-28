
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
  # The fault is in the state from 8460:8520 (8500 / 60 = 141.67). Dr. Hering
  # wants to see four full cycles before this state, and six full cycles after.
  # Thus, we start at 141 * 60 - 4 * 3 * 60 = 7740 and end at
  # 142 * 60 + 6 * 3 * 60 = 9600.
  obs_df <- data_xts[7741:9600, 1:4] %>% as.data.frame()
  obs_df$dateTime <- index(data_xts[7741:9600,])
  obs_df <- obs_df %>% mutate(State = as.factor(state))

  # Friendly colours for the Colour-Blind
  # http://bconnelly.net/2013/10/creating-colorblind-friendly-figures/
  cbFriend = c(`1` = "#E69F00", `2` = "#56B4E9", `3` = "#009E73")

  p1 <- ggplot(data = obs_df,
               aes(x = dateTime, y = x, color = State, group = 1)) +
    # group = 1 in the aesthetic means that you want different colours, but not
    # different groups. See
    # https://kohske.wordpress.com/2010/12/27/faq-geom_line-doesnt-draw-lines/
    scale_colour_manual(values = cbFriend,
                        breaks = c("1", "2", "3")) +
    geom_line() +
    scale_x_datetime(breaks = c(as.POSIXct("2016-12-02 12:00:00 CST"),
                                as.POSIXct("2016-12-03 12:00:00 CST")),
                     date_labels = "%m-%d %H:%M",
                     name = "") +
    scale_y_continuous(limits = c(-0.5, 3.5)) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5),
          legend.position = c(0.12, 0.85),
          legend.background = element_blank()) + # legend.justification = c(1,0),
    guides(color = guide_legend(override.aes = list(size = 2))) +
    ggtitle("Feature X") +
    geom_vline(xintercept = as.numeric(faultStart), size = 1)

  p2 <- ggplot(data = obs_df,
               aes(x = dateTime, y = y, color = State, group = 1)) +
    scale_colour_manual(values = cbFriend,
                        breaks = c("1", "2", "3"),
                        guide = FALSE) +
    geom_line() +
    scale_x_datetime(breaks = c(as.POSIXct("2016-12-02 12:00:00 CST"),
                                as.POSIXct("2016-12-03 12:00:00 CST")),
                     date_labels = "%m-%d %H:%M",
                     name = "Time") +
    scale_y_continuous(limits = c(-2.5, 2)) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle("Feature Y") +
    geom_vline(xintercept = as.numeric(faultStart), size = 1)

  p3 <- ggplot(data = obs_df,
               aes(x = dateTime, y = z, color = State, group = 1)) +
    scale_colour_manual(values = cbFriend,
                        breaks = c("1", "2", "3"),
                        guide = FALSE) +
    geom_line() +
    scale_x_datetime(breaks = c(as.POSIXct("2016-12-02 12:00:00 CST"),
                                as.POSIXct("2016-12-03 12:00:00 CST")),
                     date_labels = "%m-%d %H:%M",
                     name = "") +
    scale_y_continuous(limits = c(-6, 3)) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle("Feature Z") +
    geom_vline(xintercept = as.numeric(faultStart), size = 1)

  grid.arrange(p1, p2, p3, ncol = 3)
}


######  Use Graphing Function  ######
adpcaObs_ls$NOC %>% mspGraphsGrid()
adpcaObs_ls$A1 %>% mspGraphsGrid()
adpcaObs_ls$B1 %>% mspGraphsGrid()
adpcaObs_ls$C1 %>% mspGraphsGrid()
adpcaObs_ls$A2 %>% mspGraphsGrid()
adpcaObs_ls$B2 %>% mspGraphsGrid()
adpcaObs_ls$C2 %>% mspGraphsGrid()
adpcaObs_ls$A3 %>% mspGraphsGrid()
adpcaObs_ls$B3 %>% mspGraphsGrid()
adpcaObs_ls$C3 %>% mspGraphsGrid()
