library(tidyverse)
#### BFS data ####
data_plz4_to_gdnr <-
  readxl::read_excel(path = "data-raw/BFS/do-t-09.02-gwr-37.xlsx", sheet = 2L) %>%
  group_by(PLZ4) %>%
  slice(which.max(`%_IN_GDE`)) %>%
  ungroup() %>%
  distinct(PLZ4, GDNR = GDENR) %>%
  mutate_all(as.integer) %>%
  mutate(true_match = TRUE)

# for all plz from 1000 to 9999, return closest
plz_fill_table <- tibble(PLZ4 = seq.int(from = 1000L, to = 9999L, by = 1L)) %>%
  left_join(data_plz4_to_gdnr) %>%
  replace_na(replace = list(true_match = FALSE))

plz_candidates <- drop_na(plz_fill_table)

closest_candidate <- integer(length = 1L)
for (i in which(is.na(plz_fill_table$GDNR))) {
  closest_candidate <- which.min(abs(plz_candidates$PLZ4 - plz_fill_table$PLZ4[i]))
  plz_fill_table$GDNR[i] <- plz_candidates$GDNR[closest_candidate]
}

bfs_regio_info <- readxl::read_excel(path = "data-raw/BFS/be-d-00.04-rgs-01.xlsx",
                                     sheet = 1L, skip = 3,
                                     col_names = c("GDNR", "GDNM", "KTKZ", "BZNR", "BZNM", "GRNR", "AGLO2012",
                                                   "AGLOKAT2012", "remove1", "RS2012", "STATS2012", "GD3T2012", "GD9T2012", "GD25T2012", "DEGURBA",
                                                   "SPRACH", "METRO", "AGLO2000", "SL2000",
                                                   "GD9T2000", "GD22T2000", "MSNR", "AMREG", "remove2", "MSTYP", "remove3", "EUBERG")
) %>% select(-matches("^remove\\d{1,}")) %>% mutate_if(is.numeric, as.integer) %>%
  left_join(
    tribble(
      ~MSNR,                      ~MSNM,
      1L,                    "Zürich",
      2L,           "Glattal/Furttal",
      3L,                 "Limmattal",
      4L,               "Knonaueramt",
      5L,                "Zimmerberg",
      6L,              "Pfannenstiel",
      7L,          "Zürcher Oberland",
      8L,                "Winterthur",
      9L,                  "Weinland",
      10L,         "Zürcher Unterland",
      11L,                      "Bern",
      12L,            "Erlach/Seeland",
      13L,              "Biel/Seeland",
      14L,              "Jura bernois",
      15L,                "Oberaargau",
      16L,                  "Burgdorf",
      17L,           "Oberes Emmental",
      18L,                   "Aaretal",
      19L,             "Schwarzwasser",
      20L,           "Thun/Innertport",
      21L,   "Saanen/Oberes Simmental",
      22L,                 "Kandertal",
      23L,              "Oberland-Ost",
      24L,                  "Grenchen",
      25L,                 "Laufental",
      26L,                    "Luzern",
      27L,             "Sursee/Seetal",
      28L,                  "Willisau",
      29L,                 "Entlebuch",
      30L,                       "Uri",
      31L,               "Innerschwyz",
      32L,                "Einsiedeln",
      33L,                     "March",
      34L,               "Sarneraatal",
      35L,       "Nidwalden/Engelberg",
      36L, "Glarner Mittel-/Unterland",
      37L,        "Glarner Hinterland",
      38L,                       "Zug",
      39L,                 "La Sarine",
      40L,                "La Gruyère",
      41L,                     "Sense",
      42L,                    "Murten",
      43L,             "Glâne/Veveyse",
      44L,          "Olten/Gösgen/Gäu",
      45L,                      "Thal",
      46L,                 "Solothurn",
      47L,               "Basel-Stadt",
      48L,         "Unteres Baselbiet",
      49L,          "Oberes Baselbiet",
      50L,              "Schaffhausen",
      51L,    "Appenzell Ausserrhoden",
      52L,     "Appenzell Innerrhoden",
      53L,      "St. Gallen/Rorschach",
      54L,               "Rheintal SG",
      55L,                "Werdenberg",
      56L,             "Sarganserland",
      57L,               "Linthgebiet",
      58L,                "Toggenburg",
      59L,                       "Wil",
      60L,          "Bündner Rheintal",
      61L,                 "Prättigau",
      62L,                     "Davos",
      63L,                 "Schanfigg",
      64L,              "Mittelbünden",
      65L,     "Domleschg/Hinterrhein",
      66L,                  "Surselva",
      67L,           "Engiadina bassa",
      68L,               "Oberengadin",
      69L,                 "Mesolcina",
      70L,                     "Aarau",
      71L,             "Brugg/Zurzach",
      72L,                     "Baden",
      73L,      "Rohrdorf/Mutschellen",
      74L,                   "Freiamt",
      75L,                  "Fricktal",
      76L,                   "Thurtal",
      77L,            "Untersee/Rhein",
      78L,               "Oberthurgau",
      79L,                 "Tre Valli",
      80L,                   "Locarno",
      81L,                "Bellinzona",
      82L,                    "Lugano",
      83L,                 "Mendrisio",
      84L,                  "Lausanne",
      85L,              "Morges/Rolle",
      86L,                      "Nyon",
      87L,              "Vevey/Lavaux",
      88L,                     "Aigle",
      89L,             "Pays d‘Enhaut",
      90L,              "Gros-de-Vaud",
      91L,                   "Yverdon",
      92L,                 "La Vallée",
      93L,                  "La Broye",
      94L,                      "Goms",
      95L,      "Brig-Östliches Raron",
      96L,     "Visp-Westliches Raron",
      97L,                      "Leuk",
      98L,                    "Sierre",
      99L,                      "Sion",
      100L,                  "Martigny",
      101L,        "Monthey/St-Maurice",
      102L,                 "Neuchâtel",
      103L,         "La Chaux-de-Fonds",
      104L,            "Val-de-Travers",
      105L,                    "Genève",
      106L,                      "Jura"
    )
  )


# Shapefile:
ch_shape_munip <- rgdal::readOGR("data-raw/BFS/gd-b-00.03-875-gg18/ggg_2018-LV03/shp/g1g18.shp", layer = "g1g18", stringsAsFactors = FALSE)
ch_shape_lake <- rgdal::readOGR("data-raw/BFS/gd-b-00.03-875-gg18/ggg_2018-LV03/shp/g1s18.shp", layer = "g1s18", stringsAsFactors = FALSE)

usethis::use_data(
  plz_fill_table,
  bfs_regio_info,
  ch_shape_munip,
  ch_shape_lake,
  overwrite = TRUE
)
