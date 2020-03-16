allScores %>%
  mutate(nps_rating = categorizeNPSRating(nps_company_score)) %>%
  group_by(customer_no) %>%
  mutate(rating_no = cumsum(!duplicated(prod_season_no))) %>%
  group_by(customer_no) %>%
  filter(any(rating_no > 1)) %>%
  arrange(customer_no) %>%
  mutate(had_nps_rating = ifelse(rating_no > 1, lag(nps_rating, 1L), nps_rating)) -> test2
  group_by(nps_rating, rating_no, had_nps_rating) %>%
  summarize(patrons = n()) %>%
  # filter(rating_no > 1) %>%
  mutate(has_label = paste(nps_rating, rating_no),
         had_label = paste(had_nps_rating, rating_no - 1)) %>%
  ungroup() %>%
  mutate(has_label = factor(has_label, levels=levels, ordered = TRUE),
         had_label = factor(had_label, levels=levels, ordered = TRUE))-> test

levels <- c("Detractor 1", "Detractor 2", "Detractor 3", "Detractor 4", "Passive 1", "Passive 2" , "Passive 3", "Passive 4",  "Promoter 1", "Promoter 2", "Promoter 3",  "Promoter 4" )

  ggplot(aes(rating_no, had_nps_rating, size=patrons, col=rating_no)) +
  geom_line()

 library(plotly)

plot_ly(
  type="sankey",
  orientation="h",
  
  node = list(
    label = levels(test$has_label),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  link = list(
    source=as.numeric(test$had_label) - 1,
    target=as.numeric(test$has_label) - 1,
    value=test$patrons
  )
) %>%
  layout(
    title = "Patron Movement Between NPS Categories From Rating to Rating"
  )
