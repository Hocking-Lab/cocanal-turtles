str(EDF)
EDF_CPIC

EM_CPIC <- EDF_CPIC %>%
  group_by(site_num, ind, trap_id_edited, day) %>%
  select(site_num, ind, trap_id_edited, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  #spread(trap_id_edited, count, fill = 0) %>%
  ungroup() %>%
  mutate(id = as.integer(as.factor(ind)))

EM_CPIC$site_trap <- ave(EM_CPIC$site_num, EM_CPIC$trap_id_edited, FUN = function(x) as.numeric(factor(x)))

#site_id_combos <- expand.grid(site_num = 1:12, id = 1:500) %>%
#arrange(site_num, id)
site_trap_combos <- expand.grid(site_num = 1:12, trap_id_edited = 1:168) %>%
  arrange(site_num, trap_id_edited) # ... need to add in extra traps per site, need to go through and label trap ids with 14 trap "gap" per site

#foo <- site_id_combos %>%
# left_join(EM_CPIC)

foo <- site_trap_combos  %>%
  left_join(EM_CPIC)

left_join(select(max_trap_csv, -site)) %>%
  # mutate(count = ifelse(site_trap > max_trap, NA_integer_, count)) %>%
  mutate(count = ifelse(site_trap <= .$max_trap & is.na(count), 0, count))

#all individuals are going into each trap at the moment.... geesh louise
#add in augments

foo$EM_CPIC <- ave(foo$EM_CPIC, foo$EM_CPIC, FUN = function(x) as.numeric(factor(x)))

#foo_spread <- as.array(foo, dim = c(foo$site_num, foo$id, foo$site_trap, foo$count))
