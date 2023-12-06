library(dplyr)
library(tidyverse)
library(data.table)
library(tidyr)

load("ESS_2018_CH.Rda")
ess <- ESS_2018_CH
de <- ESS_2018_CH %>%
  filter(cntry == "DE")

plt <- ess$trstplt
prt <- ess$trstprt
prl <- ess$trstprl

trst <- data.frame(TrstPrl = prl, TrstPlt = plt, TrstPrt = prt)

# Reshape the data to long format using tidyr
trst_long <- gather(trst, key = "Variable", value = "Response", TrstPrl:TrstPrt)

# Create a multivariable bar chart
ggplot(trst_long, aes(x = Response, fill = Variable)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Distribution of responses to questions of trust in government",
       x = "Response",
       y = "Count")

# Create a stacked multivariable bar chart
ggplot(trst_long, aes(x = Response, fill = Variable)) +
  geom_bar(aes(fill = Variable)) +
  labs(title = "Distribution of responses to questions of trust in government",
       x = "Response",
       y = "Count")