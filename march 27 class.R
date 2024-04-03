library(tidyverse)
library(haven)
library(janitor)
library(knitr)

df = read_dta('AnalysisOverview/backlash.dta') |>
  mutate(
    pid2016 = case_when(
      PartyID2016 %in% 1:3 ~ '1. DEM',
      PartyID2016 == 4 ~ '2. IND',
      PartyID2016 %in% 5:7 ~ '3. GOP'
    ),
    pid2020 = case_when(
      PartyID2020 %in% 1:3 ~ '1. DEM',
      PartyID2020 == 4 ~ '2. IND',
      PartyID2020 %in% 5:7 ~ '3. GOP'
    )
  ) |>
  mutate(across(where(is.labelled), ~ as_factor(.))) 
    #for changing haven labels to factor vars

## One-Way Tables -----

# Using tidyverse: Frequency Table

tidytable = df |>
  count(pid2016) |>
  na.omit() |>
  mutate(percent = 100 * n / sum(n)) |>
  kable(digits = 1L)
tidytable

# Using janitor: Frequency Table
  # janitor is more useful for two-way tables
  # Specifically, "tabyl"
  # By putting only one var, it makes a frequency table
  # adorn_totals adds a total. Default is add row to bottom. 

janitortable = df |>
  tabyl(pid2016, show_na = F) |>
  adorn_totals() |>
  adorn_pct_formatting() |>
  kable(digits = 1L)
janitortable

# Significance Testing 

sigtable = df |>
  count(pid2016) |>
  na.omit() |>
  mutate(per = 100 * n / sum(n)) |>
  mutate(
    nExp = 0.5 * sum(n),
    perExp = 100 * nExp / sum(nExp)
  ) |>
  kable(digits = 1L)
sigtable

# Chi-Squared (Goodness of Fit)
  # If answers were chosen at random, you would expect 1/3 in each category
  # We see a significant variation across categories from the 33% baseline

chitable = df |>
  tabyl(pid2016, show_na = F) |>
  pull(n) |>
  stats::chisq.test()
chitable 
# For janitor, you have to do the stats::chisq.test() to do a chi^2 for a 1-way tab

## More Extensive Tables ------

# Two-Way/Contingency Tables
# Joint/Marginal Distribution

## Raw cross-tab
crosstable = df |>
  tabyl(
    pid2016, Female, # DV, IV !!!!!!!!!!
    show_na = F,
    show_missing_levels = F
  ) 
crosstable
  #When interpreting, READ DOWN THE COLUMNS.

# Pretty table
crosstable |>
  adorn_totals(where = c("row", "col")) |>
  adorn_percentages('col') |>
  adorn_pct_formatting(digits = 1) |>
  adorn_title()
crosstable

# Chi-Squared 
chisq.test(crosstable)

# Note: p-value indicates the prob of seeing your sample data given
# the assumption that the two variables are independent

changepartisan = df |>
  tabyl(
    pid2020, pid2016,
    show_na = F,
    show_missing_levels = F
  ) 
changepartisan |>
  adorn_totals(where = c("row", "col")) |>
  adorn_percentages('col') |>
  adorn_pct_formatting(digits = 1) |>
  adorn_title()

chisq.test(changepartisan)

## Summary statistics -----
# Descriptive statistics
  # He wants to see min, max, mean, sd, 
  # describe shape (skew, normality, bimodal, etc.), and a graph
  # shape described with words not numbers

# Single Continuous Variable
summary(df$ImmIndex)
sd(df$ImmIndex, na.rm = T)
  # Summary does not find sd; has to be done separately

hist(df$ImmIndex, main = NULL)

# Support for Immigration

immigration |>
  ggplot(aes(x = ImmIndex)) +
  stat_density(trim = F, fill = 'cornflowerblue') +
  geom_histogram(aes(y = ..density..), color = 'black', fill = NA, bins = 15) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(y = 'Frequency', x = 'Immigration index score') +
  theme_classic(base_size = 14) +
  theme(
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )
immigration

## Comparing Groups
# Group means
groupmean = df |>
  group_by(Female) |>
  summarise(
    Avg = mean(ImmIndex, na.rm = T),
    SD = sd(ImmIndex, na.rm = T)
  ) |>
  kable(digits = 2L)
groupmean

# Group distribution
groupdist = df |>
  ggplot(aes(x = ImmIndex, fill = Female)) +
  stat_density(
    position = 'dodge', 
    alpha = 0.3, 
    color = 'black'
  )
groupdist

## Alternatives (Mean-Focused)
# Mean by Group
altmean = df |>
  group_by(pid2016) |>
  summarise(
    Avg = mean(ImmIndex, na.rm =T)
  ) |>
  na.omit()
altmean

# Plotting alt mean
altmean |>
  ggplot(aes(x = pid2016, 
             y = Avg, 
             color = pid2016)) +
  geom_hline(yintercept = 0) +
  geom_point(size = 3) +
  geom_segment(aes(
    x = pid2016, 
    xend = pid2016, 
    y = 0, 
    yend = Avg
  ))

## T-Test ------
t.test(ImmIndex ~ Female, df)
  # Support for immigration does not differ systematically 
  # for females vs non-females (t = -0.11, ~ p = 0.91)

## ANOVA ------
# ANOVA is a t-test but for more than 2 groups
# ANOVA is "analysis of variance"
# ANOVA is a type of f-test
summary(aov(ImmIndex ~ pid2016, df))
  # Attitudes about immigration differ significantly by party id. 
  # It is unlikely we observe these differences by chance alone 
  # (F=364, ~p < 0.001)

## Skew ------
  # Skewness: the pull of the mean from the median
  # Created by one-sided extreme values
  # Renders mean a poor measure of location/middle
  # Transform the data to mitigate skew
  # For positive skew: use ln(X) or log_{10}(X)

GDP = gapminder::gapminder |>
  filter(year == 1997) |> 
  mutate(
    country = country, 
    gdp = gdpPercap,
    metric = 'Original',
    .keep = 'none'
  )
G2 = gapminder::gapminder |>
  filter(year == 1997) |> 
  mutate(
    country = country, 
    gdp = log(gdpPercap),
    metric = 'Logged',
    .keep = 'none'
  )

bind_rows(GDP, G2) |>
  ggplot(aes(x = gdp)) +
  facet_wrap(~  fct_rev(metric), nrow = 2, 
             scales = 'free_x', strip.position = 'right') +
  geom_histogram(bins = 15, color = 'white') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.4))) +
  labs(
    y = NULL,
    x = 'Measure of GDP'
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


