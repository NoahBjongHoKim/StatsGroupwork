# load tidyverse packages (run install.packages("tidyverse") the first time)
library(tidyverse)

#load ESS data
load('ESS9e03_2.Rda')

#create filtered dataset from Germany
Data_DE <- ESS9 %>% filter(cntry == "DE")

#Analyse trust in politics against political interest
#group by polintr and summarize by mean score of trust in politicians, and other trust variables
by_polintr <- Data_DE %>% group_by(polintr) %>% 
  summarize(meanTrstplt = mean(trstplt, na.rm = TRUE), 
            meanTrstprl = mean(trstprl, na.rm = TRUE),
            meanTrstprt = mean(trstprt, na.rm = TRUE),
            meanTrstlgl = mean(trstlgl, na.rm = TRUE),
            meanTrstplc = mean(trstplc, na.rm = TRUE))


# plot graphs: trust indicator against political interest
# 1. trust in politicians
ggplot(by_polintr, aes(x=polintr, y=meanTrstplt)) + geom_col() +
  labs(title="Trust in politicians")
# 2. trust in parliament
ggplot(by_polintr, aes(x=polintr, y=meanTrstprl)) + geom_col() +
  labs(title="Trust in parliament")
# 3. trust in political parties
ggplot(by_polintr, aes(x=polintr, y=meanTrstprt)) + geom_col() +
  labs(title="Trust in political parties")
# 4. trust in legal system
ggplot(by_polintr, aes(x=polintr, y=meanTrstlgl)) + geom_col() +
  labs(title="Trust in legal system")
# 5. trust in police
ggplot(by_polintr, aes(x=polintr, y=meanTrstplc)) + geom_col() +
  labs(title="Trust in police")