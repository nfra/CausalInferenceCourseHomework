###################
# Author: Brice Green
# Last Edited: 2/23/2020
# Summary: Reproduce Chris Hansen's 2013
# AER paper examining drunk driving
###################

pkgs <- c("data.table", # best data package of ALL TIME
          "rdrobust", # robust kernel regression for rdd
          "rdd", # regression discontinuity design
          "magrittr", # pipes for functional programming
          "haven", # read .dta files in R
          "ggplot2", # plots
          "ggthemes", # plot themes
          "stargazer") # tables

# attach the packages I use
invisible(sapply(pkgs, function(x) {
  if(!require(x, character.only = T, quietly = T)) {
    install.packages(x)
  } else {
    require(x, character.only = T, quietly = T)
    message(paste0(x, " already installed, attaching library now."))
  }
  })
)

hansen_data <- as.data.table(read_dta("data/hansen_dwi.dta"))

# construct cutoff variable
hansen_data[bac1 < 0.08, aboveBACThreshold := 0]
hansen_data[bac1 >= 0.08, aboveBACThreshold := 1]

# create variable centered at the threshold
hansen_data[,bac1MinusThresh :=  bac1 - 0.08]
hansen_data[,bac1LevelOverThreshold := bac1MinusThresh*aboveBACThreshold]


# generate histogram used as "Figure 1" in Hansen's paper
bac_hist <- ggplot(hansen_data, aes(x = bac1)) +
  geom_histogram(binwidth = 0.001) +
  geom_vline(xintercept = 0.08) +
  theme_minimal() +
  ggtitle("Frequency of Measurements of Blood Alcohol Levels",
          subtitle = "Bin width of 0.001, corresponding to instrument's measurement process") +
  xlab("BAC") +
  ylab("Frequency")


# save in Figures/ directory
ggsave(plot = bac_hist,
       file = "Figures/hansen-figure1-bac-hist.png",
       dpi = 300,
       width = 9,
       height = 6)

# run all of the regressions!
fits <-
  list("male",
       "white",
       "aged",
       "acc") %>%
  lapply(function(x, DT) {
    lm(as.formula(paste0(x, " ~ aboveBACThreshold*bac1MinusThresh")), data = DT)
  }, DT = hansen_data)

# function for using asymptotic (robust) standard errors
get_robust_ses <- function(fit) {
  sqrt(
    diag(
      sandwich::vcovHC(fit, type = "HC1")
    )
  )
}

# print out a nice table
tbl <- capture.output(stargazer(fits, header = F, style = "aer",
                 title = "Covariate Balance Tests",
                 column.labels = c("Male", "White","Age", "Accidents"),
                 covariate.labels = c("DUI"),
                 omit = c("Constant", "bac1MinusThresh",
                                 "aboveBACThreshold:bac1MinusThresh"),
                 se = lapply(fits, get_robust_ses),
                 dep.var.caption  = "",
                 dep.var.labels.include = F,
                 out = "Tables/covariate-balance-tests.tex"))

# generate binned data for the plot
# using the binwidth 0.001
plot_data <-
  hansen_data[,.(bac1, bin = findInterval(bac1, seq(0, 1,
                                                    by = 0.001),
                                          all.inside = T),
               aged, male, white, acc)] %>%
  merge(data.table(bin = 1:1001, level = seq(0, 1, by = 0.001)),
        by = "bin") %>%
  melt(c("bin","bac1","level"),
       variable.name = "Covariate",
       value.name = "Value") %>%
  .[,overThreshold := fifelse(level >= 0.08, 1, 0)]

# make panel names pretty
label_panel <- function(cov) {
  cov <- as.character(cov)
  if(cov == "aged") {
    "Age"
  } else if (cov == "white") {
    "White"
  } else if (cov == "acc") {
    "Accident at Scene"
  } else if (cov == "male") {
    "Male"
  } else {
    stringr::str_to_title(cov)
  }
}

label_panels <- function(cov) {
  # vectorize it
  # but apparently ggplot2 takes in a data.frame of all labels
  # this applies a vectorized function across all label variables
  lapply(cov, function(x) sapply(x, label_panel))
}

# replicate figure 2
lin_cov_balance_plots <- ggplot(
  plot_data[level < 0.2],
  aes(x = level, y = Value, group = overThreshold)
  ) +
  stat_summary(fun.y = "mean", geom = "point") +
  geom_smooth(method = 'lm') +
  facet_wrap(~Covariate, scales = 'free_y',
             labeller = label_panels) +
  theme_fivethirtyeight() +
  geom_vline(xintercept = 0.08) +
  ggtitle("Measuring Covariate Balance at the Threshold",
          subtitle = "Linear model with y ~ x")

# save in Figures/ directory
ggsave(plot = lin_cov_balance_plots,
       file = "Figures/lin_cov_balance_plots.png",
       dpi = 300,
       width = 9,
       height = 9)

# replicate figure 2, quadratic formula
quad_cov_balance_plots <- ggplot(plot_data[level < 0.2],
                                 aes(x = level,
                                     y = Value,
                                     group = overThreshold)) +
  stat_summary(fun.y = "mean", geom = "point") +
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2)) +
  facet_wrap(~Covariate, scales = 'free_y',
             labeller = label_panels) +
  theme_fivethirtyeight() +
  geom_vline(xintercept = 0.08) +
  ggtitle("Measuring Covariate Balance at the Threshold",
          subtitle = bquote("Linear model with y ~ x + " ~x^2))

# save in Figures/ directory
ggsave(plot = quad_cov_balance_plots,
       file = "Figures/quad_cov_balance_plots.png",
       dpi = 300,
       width = 9,
       height = 9)

## First bandwidth, bac \in (0.03 0.13)
# control for bac1 linearly
lin_control <- lm(recidivism ~ 1 + white +
                    aged + male +
                    bac1 +
                    aboveBACThreshold,
   data = hansen_data[bac1 >= 0.03 & bac1 <= 0.13])

# add interaction with threshold
lin_plus_interact <- lm(recidivism ~ 1 + white + aged +
                          male  +
                          bac1*aboveBACThreshold,
   data = hansen_data[bac1 >= 0.03 & bac1 <= 0.13])

# add quadratic controls
quad_plus_interact <- lm(recidivism ~ 1 + white +
                           aged +
                           male + aboveBACThreshold +
                           bac1 + bac1:aboveBACThreshold +
                           I(bac1^2) +
                          I(bac1^2):aboveBACThreshold,
                        data = hansen_data[bac1 >= 0.03 &
                                             bac1 <= 0.13])

rd_panel_a <-
  list(
    lin_control,
    lin_plus_interact,
    quad_plus_interact
  )


tbl <- capture.output(stargazer(rd_panel_a, header = F, style = "aer",
                                title = "LATE Estimates under different specifications for subsample between 0.03 and 0.13 BAC",
                                column.labels = c(
                                  "Linear Control",
                                  "With Interaction",
                                  "Quadratic Controls"
                                ),covariate.labels = "DUI",
                                omit = c(
                                  setdiff(names(
                                  quad_plus_interact$coefficients),
                                  "aboveBACThreshold"
                                  ),

                                  "Constant"),
                                se = lapply(rd_panel_a, get_robust_ses),
                                out = "Tables/rd-panel-a.tex"))


## second bandwidth  bac \in (0.55, 0.105)
# control for bac1 linearly
lin_control_panelb <- lm(recidivism ~ 1 + white +
                    aged + male +
                    bac1 +
                    aboveBACThreshold,
                  data = hansen_data[bac1 >= 0.055 &
                                       bac1 <= 0.105])

# add interaction with threshold
lin_plus_interact_panelb <- lm(recidivism ~ 1 + white + aged +
                          male +
                          bac1*aboveBACThreshold,
                        data = hansen_data[bac1 >= 0.055 &
                                             bac1 <= 0.105])

# add quadratic controls
quad_plus_interact_panelb <- lm(recidivism ~ 1 + white +
                           aged +
                           male + aboveBACThreshold +
                           bac1 + bac1:aboveBACThreshold +
                           I(bac1^2) +
                           I(bac1^2):aboveBACThreshold,
                         data = hansen_data[bac1 >= 0.055 &
                                              bac1 <= 0.105])


rd_panel_b <-
  list(
    lin_control_panelb,
    lin_plus_interact_panelb,
    quad_plus_interact_panelb
  )


tbl <- capture.output(stargazer(rd_panel_b, header = F, style = "aer",
                                title = "LATE Estimates under different specifications for subsample between 0.055 and 0.105 BAC",
                                column.labels = c(
                                  "Linear Control",
                                  "With Interaction",
                                  "Quadratic Controls"
                                ),covariate.labels = "DUI",
                                omit = c(
                                  setdiff(names(
                                    quad_plus_interact$coefficients),
                                    "aboveBACThreshold"
                                  ),

                                  "Constant"),
                                se = lapply(rd_panel_b, get_robust_ses),
                                out = "Tables/rd-panel-b.tex"))


# generate figure 3, RD plots
rd_plot_data <- hansen_data[,.(bac1, recidivism,
                               bin = findInterval(bac1, seq(0, 1,
                                                            by = 0.001),
                                                        all.inside = T))] %>%
  merge(data.table(bin = 1:1001, level = seq(0, 1, by = 0.001)),
        by = "bin") %>%
  .[,overThreshold := fifelse(level >= 0.08, 1, 0)]

# replicate figure 2
linear_rd_plot <- ggplot(rd_plot_data[level < 0.15], aes(x = level,
                                                         y = recidivism,
                                                         group = overThreshold)) +
  stat_summary(fun.y = "mean", geom = "point") +
  geom_smooth(method = 'lm') +
  theme_fivethirtyeight() +
  geom_vline(xintercept = 0.08) +
  ggtitle("Regression Discontinuity: All Offenders",
          subtitle = "Linear model with y ~ x")

# save in Figures/ directory
ggsave(plot = linear_rd_plot,
       file = "Figures/linear_rd_plot.png",
       dpi = 300,
       width = 9,
       height = 9)

quad_rd_plot <- ggplot(rd_plot_data[level < 0.15], aes(x = level,
                                                    y = recidivism,
                                                    group = overThreshold)) +
  stat_summary(fun.y = "mean", geom = "point") +
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2)) +
  theme_fivethirtyeight() +
  geom_vline(xintercept = 0.08) +
  ggtitle("Regression Discontinuity: All Offenders",
          subtitle = "Linear model with y ~ x + x^2")


# save in Figures/ directory
ggsave(plot = quad_rd_plot,
       file = "Figures/quad_rd_plot.png",
       dpi = 300,
       width = 9,
       height = 9)
