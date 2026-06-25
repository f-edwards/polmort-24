### quality checks on MPV

mpv <- read_csv("./data/mpv_5_28_26.csv")

# create row number for id
mpv <- mpv %>%
  mutate(id = 1:n())


# duplicates --------------------------------------------------------------
### check for duplicates
dupes <- mpv %>%
  filter(name != "Name Withheld") %>%
  mutate(dupe = duplicated(name)) %>%
  filter(dupe == T)

dupes <- mpv %>%
  filter(name %in% dupes$name) %>%
  arrange(name) %>%
  select(id, name, age, gender, race, date, city, state, agency_responsible) |>
  group_by(name)
### manual review. flag removals by id
# Ray Wyatt ID 9962
# drop duplicates
mpv <- mpv %>%
  filter(!id %in% c(9962))

# off duty ----------------------------------------------------------------
# exclude them for conservatism
# argument for in domain, but difficult to be precise
mpv <- mpv %>%
  filter(is.na(off_duty_killing))


# # cause of death ----------------------------------------------------------
# causes <- sort(unique(unlist(str_split(mpv$cause_of_death, pattern = ","))))
# # print(causes)
# force <- c(
#   "Asphyxiated",
#   "baton",
#   "Bean bag",
#   "Beaten",
#   "blunt force trauma",
#   "carotid restraint",
#   "choke hold",
#   "Gunshot",
#   "hobble restraint",
#   "K-9 dog",
#   "launched projectiles",
#   "Pepper Spray",
#   "Physical Restraint",
#   "prone restraint",
#   "prone restraint cardiac arrest",
#   "spit hood",
#   "strangulation",
#   "Taser"
# )
#
#
# ### Manually check all small cell count cats
# ### check circumstances on low n cats
# # removing OD deaths with no alleged use of force
# # removing one death in custody from jail force
# checks <- mpv %>%
#   filter(
#     !cause1 %in%
#       c("Beaten", "Gunshot", "Physical Restraint", "Taser", "Vehicle")
#   ) %>%
#   select(id, circumstances)
# pander::pander(checks)
# # FLAGS to check news URLS
# # flag anything that may not be UoF
# # 219 (retain), 370 (drop), 479 (drop),
# # 1083 (retain), 1207 (drop), 1567 (retain),
# # 1636 (retain), 2182 (drop), 2727 (retain), 5718 (retain),
# # 7713 (retain), 11914 (drop)
# # drop excluded
# mpv <- mpv %>%
#   filter(!id %in% c(370, 479, 1207, 2182, 11914))
# ### Review vehicle deaths.
# # criteria: collision with officer vehicle or use of force involved
# # all look ok - retaining.
# # several 'stop stick' cases, where crash resulting from use of device was fatal
# # note these
# t <- mpv %>% filter(cause1 == "Vehicle") %>% select(id, circumstances)
#
# # stop stick cases
# t %>% filter(str_detect(circumstances, "stop stick"))

### output the final data
write_csv(mpv, "./data/mpv_cleaned.csv")
