## prep_figure_data.R
## Builds the small CSVs that talk.qmd plots from (data/*.csv).
## Run from this folder:  Rscript prep_figure_data.R
## Code is copied from tswift_showgirl/saudade_analysis_final.R and
## pop_cand/1_analysis.R so the slides match the papers; nothing is written
## back to either project.

suppressMessages(library(tidyverse))
dir.create("data", showWarnings = FALSE)

################ Swift: song-level PCA (with Showgirl) ################
swift_dir <- "../tswift_showgirl"

Albums_all <- c("Taylor Swift", "Fearless", "Fearless (TV)", "Speak Now", "Speak Now (TV)",
                "Red", "Red (TV)", "1989", "1989 (DE)", "1989 (TV)",
                "Reputation", "Lover", "folklore", "folklore (DE)", "evermore",
                "evermore (DE)", "Midnights", "Midnights (3E)", "Midnights (TD)",
                "TTPD", "TTPD (Anthology)", "The Life of a Showgirl","The Life of a Showgirl: The Encore")
keepers <- c("Taylor Swift", "Fearless (TV)", "Speak Now (TV)", "Red (TV)", "1989 (TV)",
             "Reputation", "Lover", "folklore (DE)", "evermore (DE)", "Midnights (TD)",
             "TTPD (Anthology)", "The Life of a Showgirl: The Encore")

# Encore rows were pasted from the LIWC export, which lacks the 8 hand-coded columns
# (quill ... saudade), so every LIWC value sits 8 columns to the left. Shift them back.
fix_encore <- function(df) {
  df    <- df %>% mutate(Title = str_trim(Title))
  enc   <- df$Album == "The Life of a Showgirl: The Encore"
  liwc  <- names(df)[which(names(df) == "Segment"):(which(names(df) == "NRC_anger") - 1)]
  src   <- names(df)[which(names(df) == "quill") + seq_along(liwc) - 1]
  df[enc, liwc] <- lapply(df[enc, src], as.numeric)
  df[enc, c("quill", "glitter", "solo", "dessner", "antonoff", "num_writers", "saudade")] <- NA
  df$pen[enc] <- NA
  std <- df %>% filter(Album == "The Life of a Showgirl") %>%
    select(key = Title, quill, pen, glitter, solo, dessner, antonoff, num_writers) %>%
    mutate(Album = "The Life of a Showgirl: The Encore")
  df %>% mutate(key = str_remove(Title, " \\(Ft\\..*\\)$")) %>%
    rows_update(std, by = c("Album", "key"), unmatched = "ignore") %>%
    mutate(across(c(solo, dessner, antonoff),
                  ~ if_else(Album == "The Life of a Showgirl: The Encore" & is.na(.x), 0, .x))) %>%
    select(-key)
}

all_sent3 <- read_csv(file.path(swift_dir, "ts_sentiment.csv"), show_col_types = FALSE) %>% fix_encore()

saudade_songs <- all_sent3 %>%
  select(Title, Album, starts_with("NRC")) %>%
  pivot_longer(starts_with("NRC"), names_to = "sent", values_to = "value") %>%
  group_by(Title, Album) %>%
  mutate(rank = rank(-value, ties.method = "min")) %>% filter(rank < 4) %>%
  filter(sent %in% c("NRC_anticipation", "NRC_joy", "NRC_sadness")) %>%
  filter((any(grepl("joy", sent)) & any(grepl("sadness", sent))) |
           (any(grepl("anticipation", sent)) & any(grepl("sadness", sent)))) %>%
  summarise(saudade = 1, .groups = "drop")

swift <- saudade_songs %>%
  right_join(all_sent3 %>% select(-saudade), by = c("Title", "Album")) %>%
  mutate(saudade = replace_na(saudade, 0)) %>%
  select(Title, Album, song, NRC_joy, NRC_sadness, NRC_anger, Authentic, NRC_trust, track_num, NRC_disgust, NRC_surprise,
         NRC_anticipation, NRC_fear, focuspresent, emo_sad, swear, Tone,
         focusfuture, focuspast, Linguistic, Affect, emo_anger, emo_anx,
         emotion, ppron, solo, antonoff, dessner, saudade) %>%
  na.omit() %>%
  filter(Album %in% keepers)

swift_pca <- swift %>% select(where(is.numeric)) %>% select(!track_num) %>%
  scale() %>% as.data.frame() %>% princomp(cor = TRUE)

swift %>%
  transmute(Album = factor(Album, levels = Albums_all), Title, song = str_trim(song), track_num, solo,
            pc1 = swift_pca$scores[, 1], pc2 = swift_pca$scores[, 2]) %>%
  mutate(across(c(pc1, pc2), ~ round(.x, 4))) %>%
  write_csv("data/swift_pca_songs.csv")

################ Campaigns: genre residuals + PCA centroids ################
cand_dir <- "../pop_cand"
set.seed(1389)

cand_songs <- read_csv(file.path(cand_dir, "data/cand_songs_bycampaign.csv"), show_col_types = FALSE) |>
  mutate(cand_year = fct_reorder(cand_year, year)) |>
  mutate(across(starts_with("NRC_"), ~ 100 * .x / WC),
         i_minus_we = i - we, fem_male = female - male)

## genre: adjusted standardized residuals (1_analysis.R, "genre")
gp <- cand_songs |>
  filter(party %in% c("D", "R"), !is.na(genre)) |>
  mutate(genre = fct_lump_min(factor(genre), min = 10))
cs <- chisq.test(table(gp$genre, gp$party))
as.data.frame.table(cs$stdres, responseName = "stdres") |>
  rename(genre = Var1, party = Var2) |>
  filter(party == "D") |>
  transmute(genre = as.character(genre), stdres = round(stdres, 3),
            p_val = signif(2 * pnorm(-abs(stdres)), 3),
            chisq = round(unname(cs$statistic), 2), df = unname(cs$parameter)) |>
  write_csv("data/genre_residuals.csv")

## PCA with varimax rotation (1_analysis.R, "METHODS: PCA")
liwc_nrc_vars <- c("relig", "politic", "focuspast", "focuspresent",
                   "focusfuture", "prosocial", "affiliation", "Authentic", "Clout", "male", "female",
                   "conflict", "NRC_trust", "NRC_joy", "avg_dominance", "i_minus_we", "fem_male")
keep_vars <- setdiff(c(liwc_nrc_vars, "id"), c("fem_male"))
num_data <- cand_songs |>
  select(where(is.numeric)) |> select(-song_ct) |>
  select(all_of(keep_vars)) |> filter(if_all(all_of(keep_vars), is.finite))
pca_res <- prcomp(num_data |> select(-id), center = TRUE, scale. = TRUE)
L2 <- pca_res$rotation[, 1:2] %*% diag(pca_res$sdev[1:2])
vm <- varimax(L2)
rot_load   <- unclass(vm$loadings)
rot_scores <- pca_res$x[, 1:2] %*% diag(1 / pca_res$sdev[1:2]) %*% vm$rotmat
colnames(rot_load) <- colnames(rot_scores) <- c("PC1", "PC2")
if (abs(rot_load["NRC_joy", 1]) > abs(rot_load["NRC_joy", 2])) {
  rot_load <- rot_load[, 2:1]; rot_scores <- rot_scores[, 2:1]
  colnames(rot_load) <- colnames(rot_scores) <- c("PC1", "PC2")
}
if (rot_load["affiliation", 1] > 0) { rot_load[, 1] <- -rot_load[, 1]; rot_scores[, 1] <- -rot_scores[, 1] }
if (rot_load["NRC_joy", 2] < 0)     { rot_load[, 2] <- -rot_load[, 2]; rot_scores[, 2] <- -rot_scores[, 2] }

pca_out <- cand_songs |>
  select(id, campaign, cand_full, party) |>
  inner_join(tibble(id = num_data$id, PC1 = rot_scores[, 1], PC2 = rot_scores[, 2]), by = "id") |>
  mutate(cand_year = paste(word(cand_full, -1), str_extract(campaign, "\\d{4}")))

## bootstrap campaign centroids (songs within campaign)
B <- 2000
set.seed(1389)
cent <- pca_out |> group_by(cand_year, party) |>
  summarise(n_songs = n(), c1 = mean(PC1), c2 = mean(PC2), .groups = "drop")
boot <- map_dfr(seq_len(B), function(b) {
  pca_out |> group_by(cand_year) |> slice_sample(prop = 1, replace = TRUE) |>
    summarise(m1 = mean(PC1), m2 = mean(PC2), .groups = "drop")
})
stab <- boot |> group_by(cand_year) |>
  summarise(lo1 = quantile(m1, .025), hi1 = quantile(m1, .975),
            lo2 = quantile(m2, .025), hi2 = quantile(m2, .975), .groups = "drop") |>
  left_join(cent, by = "cand_year")

## nested bootstrap party centroids (campaigns within party, songs within campaign)
camps   <- pca_out |> distinct(cand_year, party)
by_camp <- split(pca_out, pca_out$cand_year)
party_cent <- pca_out |> group_by(party, cand_year) |>
  summarise(m1 = mean(PC1), m2 = mean(PC2), .groups = "drop_last") |>
  summarise(n_camp = n(), c1 = mean(m1), c2 = mean(m2), .groups = "drop")
set.seed(8987)
party_boot <- map_dfr(seq_len(B), function(b) {
  map_dfr(unique(camps$party), function(pp) {
    cs <- sample(camps$cand_year[camps$party == pp], replace = TRUE)
    mm <- vapply(cs, function(cc) {
      s <- by_camp[[cc]]; i <- sample.int(nrow(s), nrow(s), replace = TRUE)
      c(mean(s$PC1[i]), mean(s$PC2[i]))
    }, numeric(2))
    tibble(party = pp, p1 = mean(mm[1, ]), p2 = mean(mm[2, ]))
  })
})
party_stab <- party_boot |> group_by(party) |>
  summarise(lo1 = quantile(p1, .025), hi1 = quantile(p1, .975),
            lo2 = quantile(p2, .025), hi2 = quantile(p2, .975), .groups = "drop") |>
  left_join(party_cent, by = "party")

bind_rows(stab |> mutate(level = "campaign", label = cand_year),
          party_stab |> mutate(level = "party", label = paste0(party, " (", n_camp, " campaigns)"))) |>
  select(level, label, party, c1, c2, lo1, hi1, lo2, hi2) |>
  mutate(across(where(is.numeric), ~ round(.x, 4))) |>
  write_csv("data/campaign_centroids.csv")

cat("wrote:", list.files("data"), "\n")
