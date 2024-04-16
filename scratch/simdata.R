library(tidyverse)

# Sample size
# Queer-identifying students
n_q <- 40
# Non-queer-identifying students
n_nq <- 10

# Likert probabilities for queer/non-queer pre/post  
p_likert <- array(
  c(0.20, 0.45, 0.20, 0.10, 0.05,
    0.05, 0.10, 0.20, 0.45, 0.20,
    0.15, 0.20, 0.30, 0.20, 0.15,
    0.10, 0.15, 0.35, 0.25, 0.15),
  dim = c(5, 2, 2),
  dimnames = list(likert = c("strong disagree", "disagree", "neutral", "agree", "strong agree"),
                  prepost = c("pre", "post"),
                  orientation = c("queer", "non-queer"))
)

# Simulated data
pits_responses <- tibble(
  prepost = rep(c("pre", "post"), n_q + n_nq),
  orientation = c(rep("queer", n_q * 2), rep("non-queer", n_nq * 2)),
  response = mapply(function(t, o) {
    sample(c("strong disagree", "disagree", "neutral", "agree", "strong agree"),
           size = 1,
           prob = p_likert[, t, o])
  }, prepost, orientation) %>% factor(levels = c("strong disagree", "disagree", "neutral", "agree", "strong agree"))
)

# visualize
pits_responses %>% 
  count(prepost, orientation, response) %>% 
  group_by(prepost, orientation) %>% 
  mutate(frac = n / sum(n)) %>% 
  ungroup() %>% 
  mutate(response = fct_rev(response)) %>% 
  ggplot(aes(interaction(prepost, orientation), frac, fill = response)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "PRGn", direction = -1) +
  labs(y = "Proportion", x = "pre/post . queer/non-queer") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "bottom")
