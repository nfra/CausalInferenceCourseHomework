library(tidyverse)
library(haven)
library(data.table)
library(AER)
library(stargazer)
library(knitr)
library(lfe)
library(gridExtra)
library(bacondecomp)


# read data
divorce_all <- as.data.table(read_dta("./data/sw_nofault_divorce.dta"))


nfd_var_investigation <- divorce_all %>%
  group_by(statename) %>%
  summarize(nfd = min(nfd), nfd2 = min(nfd2))
nfd_var_investigation


# drop data not needed for the analysis
divorce <- divorce_all[,1:40]
divorce$region <- divorce_all$region

# table for states and timings

state_timing <- divorce %>%
  group_by(statename) %>%
  summarize(nfd = min(nfd))
state_timing 

kable(state_timing)

# replication

# create post var
divorce$post = 0
divorce[year >= `_nfd`,]$post = 1


# run regressions without trend
homicide_fe_reg = felm(asmrh ~ post | statename + year | 0 | statename, data = divorce)
suicide_fe_reg = felm(asmrs ~ post | statename + year | 0 | statename, data = divorce)

# create trend var
divorce$trend = divorce$year - 1963

# run regressions with trend
homicide_fe_trend_reg = felm(asmrh ~ post | as.factor(statename):trend + statename + year | 0 | statename, data = divorce)
suicide_fe_trend_reg = felm(asmrs ~ post | as.factor(statename):trend + statename + year | 0 | statename, data = divorce)

stargazer(homicide_fe_reg, 
          homicide_fe_trend_reg, 
          suicide_fe_reg, 
          suicide_fe_trend_reg,
          type = "text",
          header = FALSE,
          title = "Estimates of Effect of Divorce Laws's on Female ASMR Due to Homicide and Suicide",
          dep.var.labels = c("Homicide", "Suicide"),
          covariate.labels = c("Post-Treatment"),
          add.lines = list(c('State and Year FEs', 'X', 'X', 'X', 'X'),
                           c('State-Specific Trends', '', 'X', '', 'X')),
          keep.stat = c('n'),
          notes = "Standard errors are clustered by state."
          )

# event study

homicide_fe_leadlag_reg = felm(asmrh ~ as.factor(I(year - `_nfd`)) | statename + year | 0 | statename, data = divorce)
suicide_fe_leadlag_reg = felm(asmrs ~ as.factor(I(year - `_nfd`)) | statename + year | 0 | statename, data = divorce)

summary(homicide_fe_leadlag_reg)

leadlag_data <- as.data.frame(tibble(label = -20:27,
                                         mean_homicide = coef(homicide_fe_leadlag_reg)[1:48],
                                         se_homicide = homicide_fe_leadlag_reg$cse[1:48],
                                         mean_suicide = coef(suicide_fe_leadlag_reg)[1:48],
                                         se_suicide = suicide_fe_leadlag_reg$cse[1:48]))


homicide_plot <- ggplot(data = leadlag_data, aes(x = label)) +
  geom_point(aes(y = mean_homicide)) +
  geom_errorbar(aes(ymin = mean_homicide - 1.96*se_homicide, 
                    ymax = mean_homicide +1.96*se_homicide)) +
  xlab("Years before and after no-fault divorce law passage") +
  ylab("Mean ASMR, homicide")

suicide_plot <- ggplot(data = leadlag_data, aes(x = label)) +
  geom_point(aes(y = mean_suicide)) +
  geom_errorbar(aes(ymin = mean_suicide - 1.96*se_suicide, 
                    ymax = mean_suicide +1.96*se_suicide)) +
  xlab("Years before and after no-fault divorce law passage") +
  ylab("Mean ASMR, suicide")

grid.arrange(homicide_plot, suicide_plot, ncol=1)

unique(divorce$nfd)

homicide_bacon_decomp <- bacon(asmrh ~ post,
                              data = divorce, id_var = "statename",
                              time_var = "year")

suicide_bacon_decomp <- bacon(asmrs ~ post,
                  data = divorce, id_var = "statename",
                  time_var = "year")

homicide_bacon_decomp
kable(suicide_bacon_decomp)

suicide_bacon_decomp_plot_data = as.data.frame(suicide_bacon_decomp)
homicide_bacon_decomp_plot_data = as.data.frame(homicide_bacon_decomp)

ggplot(data = suicide_bacon_decomp_plot_data) + 
  geom_point(aes(x = weight, y = estimate, color = type))

homicide_fe_leadlag_post_reg = felm(asmrh ~ post + as.factor(I(year - `_nfd`)) | statename + year | 0 | statename, data = divorce)
suicide_fe_leadlag_post_reg = felm(asmrs ~ post + as.factor(I(year - `_nfd`)) | statename + year | 0 | statename, data = divorce)

stargazer(homicide_fe_leadlag_post_reg, 
          suicide_fe_leadlag_post_reg,
          header = FALSE,
          title = "Estimates of Mean Female ASMR Due to Homicide and Suicide in Lead and Lag Years",
          notes = "Standard errors are clustered by state.",
          type = 'text'
)


summary(suicide_fe_leadlag_post_reg)

