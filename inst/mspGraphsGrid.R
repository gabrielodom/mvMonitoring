
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
library(mvMonitoring)

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
mspGraphsGrid <- function(data_xts,
                          rows = 7741:9600,
                          faultStart = "2016-12-02 21:39:00 EST",
                          Xlims = c(-0.5, 3.5),
                          Ylims = c(-2.5, 2),
                          Zlims = c(-6, 3)){
  # The fault is in the state from 8460:8520 (8500 / 60 = 141.67). Dr. Hering
  # wants to see four full cycles before this state, and six full cycles after.
  # Thus, we start at 141 * 60 - 4 * 3 * 60 = 7740 and end at
  # 142 * 60 + 6 * 3 * 60 = 9600.
  obs_df <- data_xts[rows, 1:4] %>% as.data.frame()
  obs_df$dateTime <- index(data_xts[rows,])
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
    scale_y_continuous(limits = Xlims) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5),
          legend.position = c(0.12, 0.825),
          legend.background = element_blank()) + # legend.justification = c(1,0),
    guides(color = guide_legend(override.aes = list(size = 2))) +
    ggtitle("Feature X") +
    geom_vline(xintercept = as.numeric(as.POSIXct(faultStart)), size = 1)

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
    scale_y_continuous(limits = Ylims) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle("Feature Y") +
    geom_vline(xintercept = as.numeric(as.POSIXct(faultStart)), size = 1)

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
    scale_y_continuous(limits = Zlims) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5),
          # So that the right side of Figure Z doesn't get eaten by the PDF
          plot.margin = unit(c(0.2,0.5,0.2,0.2), "cm")) +
    ggtitle("Feature Z") +
    geom_vline(xintercept = as.numeric(as.POSIXct(faultStart)), size = 1)

  grid.arrange(p1, p2, p3, ncol = 3)
}


######  Use Graphing Function  ######
setwd("C:/Users/gabriel_odom/Box Sync/Consulting/Dr. Hering/MV_Process_Control/MVSPC (including Graphics)/Paper Graphs")

# Getting the PDFs to look exactly the way we need them to in the LaTeX document
# can be a giant pain in the ass.

pdf("NOC_20170420.pdf", width = 8.624, height = 3.744)
adpcaObs_ls$NOC %>% mspGraphsGrid()
dev.off()

pdf("Fault1A_20170420.pdf", width = 8.624, height = 3.744)
adpcaObs_ls$A1 %>% mspGraphsGrid()
dev.off()

pdf("Fault1B_20170420.pdf", width = 8.624, height = 3.744)
adpcaObs_ls$B1 %>% mspGraphsGrid()
dev.off()

pdf("Fault1C_20170420.pdf", width = 8.624, height = 3.744)
adpcaObs_ls$C1 %>% mspGraphsGrid()
dev.off()

pdf("Fault2A_20170420.pdf", width = 8.624, height = 3.744)
adpcaObs_ls$A2 %>% mspGraphsGrid()
dev.off()

pdf("Fault2B_20170420.pdf", width = 8.624, height = 3.744)
adpcaObs_ls$B2 %>% mspGraphsGrid()
dev.off()

pdf("Fault2C_20170420.pdf", width = 8.624, height = 3.744)
adpcaObs_ls$C2 %>% mspGraphsGrid()
dev.off()

pdf("Fault3A_20170420.pdf", width = 8.624, height = 3.744)
adpcaObs_ls$A3 %>% mspGraphsGrid()
dev.off()

pdf("Fault3B_20170420.pdf", width = 8.624, height = 3.744)
adpcaObs_ls$B3 %>% mspGraphsGrid()
dev.off()

pdf("Fault3C_20170420.pdf", width = 8.624, height = 3.744)
adpcaObs_ls$C3 %>% mspGraphsGrid()
dev.off()


######  Graphs for the Reviewer  ######
# After saving, move files to:
# ~/Box Sync/Consulting/Dr. Hering/MV_Process_Control/MVSPC (including Graphics)/Paper Graphs

pdf("NOC_20180125.pdf", width = 8.624, height = 3.744)
adpcaObs_ls$NOC %>% mspGraphsGrid(rows = 1:10080,
                                 Xlims = c(-0.5, 4.5),
                                 Zlims = c(-6, 5))
dev.off()

pdf("Fault3C_20180125.pdf", width = 8.624, height = 3.744)
adpcaObs_ls$C3 %>% mspGraphsGrid(rows = 1:10080,
                                 Xlims = c(-0.5, 4.5),
                                 Zlims = c(-6, 5))
dev.off()
