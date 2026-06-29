# ============================================================
# ONLINE EXTRACTION SCRIPT: METHODOLOGY DOCUMENTATION
# ============================================================
# 
# TITLE: Systematic Extraction of Demographic Reporting in BCI Research
# 
# OBJECTIVE:
# This script systematically extracts, processes, and analyzes demographic 
# reporting practices (sex, age, race/ethnicity) from a curated set of 
# brain-computer interface (BCI) research articles.
#
# METHODOLOGICAL APPROACH:
# 1. Article Identification: Query academic databases using article titles
# 2. Metadata Retrieval: Extract publication metadata (journal, year, DOI)
# 3. Full-Text Acquisition: Retrieve full text when available via Europe PMC
# 4. Text Mining: Apply pattern-matching algorithms to detect demographic reporting
# 5. Statistical Analysis: Calculate reporting rates at article and journal levels
# 6. Data Visualization: Generate publication-ready figures
#
# DATA SOURCES:
# - Europe PMC (primary): https://www.ebi.ac.uk/europepmc/
# - Crossref (fallback): https://api.crossref.org/
#
# EXTRACTION STRATEGY:
# - Primary: Exact title matching in Europe PMC
# - Secondary: Broad title search if exact match fails
# - Tertiary: Crossref API for metadata if Europe PMC fails
# - Full Text: XML parsing from Europe PMC when available
#
# REPORTING DETECTION:
# - Rule-based pattern matching using keyword lists
# - Context-aware snippet extraction for validation
# - Multiple patterns to capture variant terminology
#
# OUTPUTS:
# - Metadata: online_metadata_results.csv
# - Article-level data: online_extracted_demographic_reporting.csv
# - Journal-level aggregates: online_journal_level_reporting_practices.csv
# - Overall statistics: online_overall_reporting_practices.csv
# - Figures: PNG and PDF formats
#
# DEPENDENCIES:
# - R packages: tidyverse, httr2, jsonlite, xml2, stringdist, glue, RColorBrewer
# - System: Internet connection, R version 4.0+
#
# ETHICAL CONSIDERATIONS:
# - All data retrieved from publicly available academic databases
# - API rate limiting implemented to avoid server overload
# - User-agent string identifies the script for transparency
#
# DATE CREATED: 2026-06-25
# SCRIPT VERSION: 2.0
# ============================================================

# ============================================================
# SECTION 1: ENVIRONMENT SETUP
# ============================================================

packages <- c(
  "tidyverse",
  "httr2",
  "jsonlite",
  "xml2",
  "stringdist",
  "glue",
  "RColorBrewer"
)

new_packages <- packages[!(packages %in% rownames(installed.packages()))]

if (length(new_packages) > 0) {
  install.packages(new_packages)
}

library(tidyverse)
library(httr2)
library(jsonlite)
library(xml2)
library(stringdist)
library(glue)
library(RColorBrewer)

# ============================================================
# SECTION 2: VISUALIZATION THEME CONFIGURATION
# ============================================================

if (Sys.info()["sysname"] == "Windows") {
  windowsFonts(Helvetica = windowsFont("Helvetica"))
} else if (Sys.info()["sysname"] == "Darwin") {
  quartzFonts(Helvetica = quartzFont(rep("Helvetica", 4)))
} else {
  # Linux - assumes Helvetica is available
}

theme_journal_reporting <- function(base_size = 9) {
  theme_minimal(base_size = base_size, base_family = "Helvetica") +
    theme(
      plot.title = element_text(
        face = "bold",
        family = "Helvetica",
        size = base_size * 1.1
      ),
      plot.subtitle = element_text(
        family = "Helvetica",
        size = base_size * 0.8
      ),
      axis.title = element_text(
        family = "Helvetica",
        size = base_size * 0.9
      ),
      axis.text = element_text(
        family = "Helvetica",
        size = base_size * 0.8
      ),
      legend.text = element_text(
        family = "Helvetica",
        size = base_size * 0.8
      ),
      legend.title = element_text(
        family = "Helvetica",
        size = base_size * 0.9
      ),
      strip.text = element_text(
        family = "Helvetica",
        size = base_size * 0.9
      ),
      legend.position = "bottom"
    )
}

blue_palette <- c(
  "#045275",
  "#0F7BA6",
  "#34A1C7",
  "#6CC5E0",
  "#A8E0F0",
  "#D4F0F8"
)

reporting_blue_palette <- c(
  "Sex reported" = blue_palette[1],
  "Age reported" = blue_palette[3],
  "Race/ethnicity reported" = blue_palette[5]
)

# ============================================================
# SECTION 3: ARTICLE TITLES
# ============================================================

included_titles <- c(
  "Long-Term Training with a Brain-Machine Interface-Based Gait Protocol Induces Partial Neurological Recovery in Paraplegic Patients",
  "Training with brain-machine interfaces, visuo-tactile feedback and assisted locomotion improves sensorimotor, visceral, and psychological signs in chronic paraplegic patients",
  "Surface EEG-Transcranial Direct Current Stimulation (tDCS) Closed-Loop System",
  "Enhancing classification of a large lower-limb motor imagery EEG dataset for BCI in knee pain patients",
  "The Promoter, a brain-computer interface-assisted intervention to promote upper limb functional motor recovery after stroke",
  "Brain-computer interface-based robotic end effector system for wrist and hand rehabilitation: Results of a three-armed randomized controlled trial for chronic stroke",
  "One-Dimensional Local Binary Pattern and Common Spatial Pattern Feature Fusion Brain Network for Central Neuropathic Pain",
  "Evaluation of Neurofeedback Therapy for Treatment of Central Neuropathic Pain in Paraplegic Patients Using Deep Learning",
  "Effects of Brain-Computer Interface-controlled Functional Electrical Stimulation Training on Shoulder Subluxation for Patients with Stroke: A Randomized Controlled Trial",
  "New Approaches Based on Non-Invasive Brain Stimulation and Mental Representation Techniques Targeting Pain in Parkinson's Disease Patients",
  "Neurofeedback Training without Explicit Phantom Hand Movements and Hand-Like Visual Feedback to Modulate Pain: A Randomized Crossover Feasibility Trial",
  "BCI training to move a virtual hand reduces phantom limb pain: A randomized crossover trial",
  "Does feedback based on FES-evoked nociceptive withdrawal reflex condition event-related desynchronization? An exploratory study with brain-computer interfaces",
  "EEG-based brain-computer interface with immersive virtual reality for phantom limb pain: a single-center pilot neurofeedback trial",
  "Single-session communication with a locked-in patient by functional near-infrared spectroscopy",
  "Activation of a Rhythmic Lower Limb Movement Pattern during the Use of a Multimodal Brain-Computer Interface: A Case Study of a Clinically Complete Spinal Cord Injury",
  "DiSCIoser: unlocking recovery potential of arm sensorimotor functions after spinal cord injury by promoting activity-dependent brain plasticity by means of brain-computer interface technology",
  "The influence of central neuropathic pain in paraplegic patients on performance of a motor imagery based Brain Computer Interface",
  "Motor imagery in spinal cord injured people is modulated by somatotopic coding, perspective taking, and post-lesional chronic pain",
  "Decoding of Pain Perception using EEG Signals for a Real-Time Reflex System in Prostheses: A Case Study",
  "Movement-related cortical potentials in paraplegic patients: Abnormal patterns and considerations for BCI-rehabilitation",
  "QCBO-WSVM: Quantum Chaos Butterfly Optimization-Based Weighted Support Vector Machine for Neuropathic Pain Detection from EEG Signal"
)

included_titles <- unique(included_titles)

# ============================================================
# SECTION 4: TEXT PROCESSING HELPER FUNCTIONS
# ============================================================

clean_text <- function(x) {
  if (is.null(x)) return("")
  x <- as.character(x)
  x[is.na(x)] <- ""
  x %>%
    str_replace_all("[\r\n\t]", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_squish()
}

normalize_title <- function(x) {
  clean_text(x) %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish()
}

# ============================================================
# SECTION 5: API COMMUNICATION FUNCTIONS
# ============================================================

safe_request_json <- function(url, pause = 0.25) {
  Sys.sleep(pause)
  
  tryCatch(
    {
      response <- request(url) %>%
        req_user_agent("BCI demographic reporting extraction; contact: student research project") %>%
        req_perform()
      
      if (resp_status(response) >= 300) {
        return(NULL)
      }
      
      resp_body_json(response, simplifyVector = TRUE)
    },
    error = function(e) {
      return(NULL)
    }
  )
}

safe_request_text <- function(url, pause = 0.25) {
  Sys.sleep(pause)
  
  tryCatch(
    {
      response <- request(url) %>%
        req_user_agent("BCI demographic reporting extraction; contact: student research project") %>%
        req_perform()
      
      if (resp_status(response) >= 300) {
        return(NA_character_)
      }
      
      resp_body_string(response)
    },
    error = function(e) {
      return(NA_character_)
    }
  )
}

# ============================================================
# SECTION 6: TEXT SEGMENTATION FUNCTIONS
# ============================================================

sentence_split <- function(text) {
  text <- clean_text(text)
  
  if (text == "") {
    return(character(0))
  }
  
  str_split(text, "(?<=[.!?])\\s+", simplify = FALSE)[[1]] %>%
    clean_text()
}

extract_keyword_snippets <- function(text, keywords, n_max = 8) {
  sents <- sentence_split(text)
  
  if (length(sents) == 0) {
    return(NA_character_)
  }
  
  pattern <- paste(keywords, collapse = "|")
  
  hits <- sents[str_detect(str_to_lower(sents), str_to_lower(pattern))]
  
  if (length(hits) == 0) {
    return(NA_character_)
  }
  
  hits <- hits[nchar(hits) > 20]
  hits <- unique(hits)
  hits <- hits[1:min(length(hits), n_max)]
  
  paste(hits, collapse = " || ")
}

# ============================================================
# SECTION 7: EUROPE PMC SEARCH FUNCTION
# ============================================================

search_europe_pmc <- function(title) {
  
  query_exact <- URLencode(paste0('TITLE:"', title, '"'), reserved = TRUE)
  
  url_exact <- paste0(
    "https://www.ebi.ac.uk/europepmc/webservices/rest/search?",
    "query=", query_exact,
    "&format=json&pageSize=10&resultType=core"
  )
  
  res <- safe_request_json(url_exact)
  
  results <- NULL
  
  if (!is.null(res$resultList$result) && length(res$resultList$result) > 0) {
    results <- as_tibble(res$resultList$result)
  }
  
  if (is.null(results) || nrow(results) == 0) {
    
    query_broad <- URLencode(title, reserved = TRUE)
    
    url_broad <- paste0(
      "https://www.ebi.ac.uk/europepmc/webservices/rest/search?",
      "query=", query_broad,
      "&format=json&pageSize=10&resultType=core"
    )
    
    res <- safe_request_json(url_broad)
    
    if (!is.null(res$resultList$result) && length(res$resultList$result) > 0) {
      results <- as_tibble(res$resultList$result)
    }
  }
  
  if (is.null(results) || nrow(results) == 0) {
    return(tibble(
      input_title = title,
      found = FALSE,
      matched_title = NA_character_,
      title_similarity = NA_real_,
      journal = NA_character_,
      year = NA_character_,
      doi = NA_character_,
      pmid = NA_character_,
      pmcid = NA_character_,
      source = NA_character_,
      europepmc_id = NA_character_,
      abstract_text = NA_character_
    ))
  }
  
  results <- results %>%
    mutate(
      input_title = title,
      matched_title = if_else(is.na(title), "", title),
      title_similarity = stringsim(
        normalize_title(input_title),
        normalize_title(matched_title),
        method = "jw"
      )
    ) %>%
    arrange(desc(title_similarity))
  
  best <- results[1, ]
  
  tibble(
    input_title = title,
    found = TRUE,
    matched_title = clean_text(best$title),
    title_similarity = best$title_similarity,
    journal = clean_text(best$journalTitle),
    year = clean_text(best$pubYear),
    doi = clean_text(best$doi),
    pmid = clean_text(best$pmid),
    pmcid = clean_text(best$pmcid),
    source = clean_text(best$source),
    europepmc_id = clean_text(best$id),
    abstract_text = clean_text(best$abstractText)
  )
}

# ============================================================
# SECTION 8: CROSSREF FALLBACK SEARCH
# ============================================================

search_crossref <- function(title) {
  
  query <- URLencode(title, reserved = TRUE)
  
  url <- paste0(
    "https://api.crossref.org/works?",
    "query.title=", query,
    "&rows=5"
  )
  
  res <- safe_request_json(url)
  
  items <- res$message$items
  
  if (is.null(items) || length(items) == 0) {
    return(tibble(
      input_title = title,
      crossref_found = FALSE,
      crossref_title = NA_character_,
      crossref_similarity = NA_real_,
      crossref_journal = NA_character_,
      crossref_year = NA_character_,
      crossref_doi = NA_character_
    ))
  }
  
  items_tbl <- tibble(
    crossref_title = map_chr(items$title, ~ clean_text(.x[1])),
    crossref_journal = map_chr(items$`container-title`, ~ clean_text(.x[1])),
    crossref_doi = map_chr(items$DOI, ~ clean_text(.x[1])),
    crossref_year = map_chr(items$issued$`date-parts`, ~ {
      if (length(.x) == 0) {
        NA_character_
      } else {
        as.character(.x[[1]][1])
      }
    })
  ) %>%
    mutate(
      input_title = title,
      crossref_found = TRUE,
      crossref_similarity = stringsim(
        normalize_title(input_title),
        normalize_title(crossref_title),
        method = "jw"
      )
    ) %>%
    arrange(desc(crossref_similarity))
  
  items_tbl[1, ]
}

# ============================================================
# SECTION 9: FULL TEXT RETRIEVAL
# ============================================================

get_europepmc_fulltext <- function(source, europepmc_id, pmcid) {
  
  urls_to_try <- c()
  
  if (!is.na(pmcid) && pmcid != "") {
    urls_to_try <- c(
      urls_to_try,
      paste0(
        "https://www.ebi.ac.uk/europepmc/webservices/rest/PMC/",
        pmcid,
        "/fullTextXML"
      )
    )
  }
  
  if (!is.na(source) && source != "" && !is.na(europepmc_id) && europepmc_id != "") {
    urls_to_try <- c(
      urls_to_try,
      paste0(
        "https://www.ebi.ac.uk/europepmc/webservices/rest/",
        source,
        "/",
        europepmc_id,
        "/fullTextXML"
      )
    )
  }
  
  urls_to_try <- unique(urls_to_try)
  
  if (length(urls_to_try) == 0) {
    return(NA_character_)
  }
  
  for (u in urls_to_try) {
    xml_txt <- safe_request_text(u)
    
    if (!is.na(xml_txt) && nchar(xml_txt) > 500 && str_detect(xml_txt, "<")) {
      
      parsed <- tryCatch(
        read_xml(xml_txt),
        error = function(e) NULL
      )
      
      if (!is.null(parsed)) {
        
        all_text <- xml_text(parsed) %>%
          clean_text()
        
        if (nchar(all_text) > 500) {
          return(all_text)
        }
      }
    }
  }
  
  NA_character_
}

# ============================================================
# SECTION 10: DEMOGRAPHIC REPORTING DETECTION
# ============================================================

detect_reporting <- function(text) {
  
  text <- clean_text(text)
  text_low <- str_to_lower(text)
  
  if (text == "") {
    return(tibble(
      reported_age = FALSE,
      reported_sex = FALSE,
      reported_race_ethnicity = FALSE,
      age_snippets = NA_character_,
      sex_snippets = NA_character_,
      race_ethnicity_snippets = NA_character_
    ))
  }
  
  age_keywords <- c(
    "age", "aged", "years old", "year-old", "yr-old",
    "mean age", "median age", "age range", "range of age"
  )
  
  sex_keywords <- c(
    "sex", "gender", "male", "female", "men", "women", "boys", "girls"
  )
  
  race_keywords <- c(
    "race", "racial", "ethnicity", "ethnic",
    "white", "black", "african american", "asian",
    "hispanic", "latino", "latina", "caucasian",
    "native american", "pacific islander"
  )
  
  age_snips <- extract_keyword_snippets(text, age_keywords)
  sex_snips <- extract_keyword_snippets(text, sex_keywords)
  race_snips <- extract_keyword_snippets(text, race_keywords)
  
  age_pattern <- paste0(
    "(",
    "mean age|median age|age range|aged|years old|year-old|yr-old|",
    "\\b\\d{1,2}(\\.\\d+)?\\s*(±|\\+/-|years|yrs|y/o|yo)\\b",
    ")"
  )
  
  sex_pattern <- paste0(
    "(",
    "\\b\\d+\\s*(male|males|female|females|men|women)\\b|",
    "\\b(male|males|female|females|men|women)\\s*[:=]?\\s*\\d+\\b|",
    "\\b\\d+(\\.\\d+)?%\\s*(male|female|men|women)\\b|",
    "\\bsex\\b|\\bgender\\b",
    ")"
  )
  
  race_pattern <- paste0(
    "(",
    "\\brace\\b|\\bracial\\b|\\bethnicity\\b|\\bethnic\\b|",
    "\\bwhite\\b|\\bblack\\b|african american|\\basian\\b|",
    "\\bhispanic\\b|\\blatino\\b|\\blatina\\b|\\bcaucasian\\b|",
    "native american|pacific islander",
    ")"
  )
  
  reported_age <- str_detect(text_low, age_pattern)
  
  reported_sex <- str_detect(text_low, sex_pattern) &&
    str_detect(text_low, "\\bmale\\b|\\bfemale\\b|\\bmen\\b|\\bwomen\\b|\\bsex\\b|\\bgender\\b")
  
  reported_race_ethnicity <- str_detect(text_low, race_pattern)
  
  tibble(
    reported_age = reported_age,
    reported_sex = reported_sex,
    reported_race_ethnicity = reported_race_ethnicity,
    age_snippets = age_snips,
    sex_snippets = sex_snips,
    race_ethnicity_snippets = race_snips
  )
}

# ============================================================
# SECTION 11: MAIN EXECUTION - DATA EXTRACTION
# ============================================================

cat("=== STARTING ONLINE EXTRACTION ===\n")
cat("Searching Europe PMC for article metadata...\n")

epmc_results <- map_dfr(included_titles, search_europe_pmc)

cat("Searching Crossref for fallback metadata...\n")

crossref_results <- map_dfr(included_titles, search_crossref)

cat("Combining metadata from all sources...\n")

metadata <- epmc_results %>%
  left_join(crossref_results, by = "input_title") %>%
  mutate(
    final_title = case_when(
      !is.na(matched_title) & matched_title != "" ~ matched_title,
      !is.na(crossref_title) & crossref_title != "" ~ crossref_title,
      TRUE ~ input_title
    ),
    final_journal = case_when(
      !is.na(journal) & journal != "" ~ journal,
      !is.na(crossref_journal) & crossref_journal != "" ~ crossref_journal,
      TRUE ~ NA_character_
    ),
    final_year = case_when(
      !is.na(year) & year != "" ~ year,
      !is.na(crossref_year) & crossref_year != "" ~ crossref_year,
      TRUE ~ NA_character_
    ),
    final_doi = case_when(
      !is.na(doi) & doi != "" ~ doi,
      !is.na(crossref_doi) & crossref_doi != "" ~ crossref_doi,
      TRUE ~ NA_character_
    )
  )

write_csv(metadata, "online_metadata_results.csv")
cat("Metadata saved to online_metadata_results.csv\n")

cat("Retrieving full text where available...\n")
cat("This may take several minutes...\n")

fulltext_results <- metadata %>%
  mutate(
    full_text = pmap_chr(
      list(source, europepmc_id, pmcid),
      function(source, europepmc_id, pmcid) {
        get_europepmc_fulltext(source, europepmc_id, pmcid)
      }
    ),
    text_for_extraction = case_when(
      !is.na(full_text) & nchar(full_text) > 500 ~ full_text,
      !is.na(abstract_text) & abstract_text != "" ~ abstract_text,
      TRUE ~ ""
    ),
    extraction_source = case_when(
      !is.na(full_text) & nchar(full_text) > 500 ~ "Europe PMC full text",
      !is.na(abstract_text) & abstract_text != "" ~ "Abstract only",
      TRUE ~ "No text found"
    )
  )

cat("Detecting demographic reporting in text...\n")

demographic_flags <- map_dfr(fulltext_results$text_for_extraction, detect_reporting)

online_extracted_data <- bind_cols(
  fulltext_results %>%
    select(
      input_title,
      final_title,
      final_journal,
      final_year,
      final_doi,
      pmid,
      pmcid,
      found,
      title_similarity,
      extraction_source
    ),
  demographic_flags
)

write_csv(online_extracted_data, "online_extracted_demographic_reporting.csv")
cat("Article-level data saved to online_extracted_demographic_reporting.csv\n")

# ============================================================
# SECTION 12: DATA AGGREGATION AND STATISTICAL ANALYSIS
# ============================================================

cat("\n=== SAMPLE OF EXTRACTED ARTICLE DATA ===\n")

print(
  online_extracted_data %>%
    select(
      final_title,
      final_journal,
      extraction_source,
      reported_sex,
      reported_age,
      reported_race_ethnicity
    ),
  n = 100
)

# ============================================================
# SECTION 13: JOURNAL-LEVEL ANALYSIS
# ============================================================

cat("\n=== CALCULATING JOURNAL-LEVEL REPORTING RATES ===\n")

journal_reporting <- online_extracted_data %>%
  mutate(
    journal = if_else(
      is.na(final_journal) | final_journal == "",
      "Journal not found online",
      final_journal
    )
  ) %>%
  group_by(journal) %>%
  summarise(
    n_studies = n(),
    percent_reporting_sex = mean(reported_sex, na.rm = TRUE) * 100,
    percent_reporting_age = mean(reported_age, na.rm = TRUE) * 100,
    percent_reporting_race_ethnicity = mean(reported_race_ethnicity, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(n_studies), journal)

write_csv(journal_reporting, "online_journal_level_reporting_practices.csv")
cat("Journal-level data saved to online_journal_level_reporting_practices.csv\n")

cat("\n=== JOURNAL-LEVEL REPORTING RATES ===\n")
print(journal_reporting, n = 100)

# ============================================================
# SECTION 14: OVERALL STATISTICS
# ============================================================

cat("\n=== CALCULATING OVERALL REPORTING STATISTICS ===\n")

overall_reporting <- online_extracted_data %>%
  summarise(
    n_articles = n(),
    n_full_text_available = sum(extraction_source == "Europe PMC full text", na.rm = TRUE),
    n_abstract_only = sum(extraction_source == "Abstract only", na.rm = TRUE),
    n_no_text_found = sum(extraction_source == "No text found", na.rm = TRUE),
    percent_reporting_sex = mean(reported_sex, na.rm = TRUE) * 100,
    percent_reporting_age = mean(reported_age, na.rm = TRUE) * 100,
    percent_reporting_race_ethnicity = mean(reported_race_ethnicity, na.rm = TRUE) * 100
  )

write_csv(overall_reporting, "online_overall_reporting_practices.csv")
cat("Overall statistics saved to online_overall_reporting_practices.csv\n")

cat("\n=== OVERALL REPORTING STATISTICS ===\n")
print(overall_reporting)

# ============================================================
# SECTION 15: DATA VISUALIZATION
# ============================================================

cat("\n=== CREATING VISUALIZATIONS ===\n")

journal_reporting_long <- journal_reporting %>%
  pivot_longer(
    cols = starts_with("percent_reporting"),
    names_to = "reporting_category",
    values_to = "percent"
  ) %>%
  mutate(
    reporting_category = recode(
      reporting_category,
      percent_reporting_sex = "Sex reported",
      percent_reporting_age = "Age reported",
      percent_reporting_race_ethnicity = "Race/ethnicity reported"
    ),
    journal_label = paste0(journal, " (n=", n_studies, ")"),
    journal_label = fct_reorder(journal_label, n_studies)
  )

journal_reporting_plot <- ggplot(
  journal_reporting_long,
  aes(
    x = journal_label,
    y = percent,
    fill = reporting_category
  )
) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.7
  ) +
  geom_text(
    aes(label = paste0(round(percent), "%")),
    position = position_dodge(width = 0.75),
    hjust = -0.1,
    size = 2.0,
    family = "Helvetica"
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, 110),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%")
  ) +
  scale_fill_manual(
    values = reporting_blue_palette,
    name = "Reporting variable"
  ) +
  labs(
    title = "Journal-Level Reporting Practices",
    subtitle = "Online extraction of age, sex, and race/ethnicity reporting among included studies",
    x = "Journal",
    y = "% of included studies",
    fill = "Reporting variable"
  ) +
  theme_journal_reporting(base_size = 9)

print(journal_reporting_plot)

ggsave(
  filename = "online_journal_level_reporting_practices.png",
  plot = journal_reporting_plot,
  width = 11,
  height = 7,
  dpi = 300,
  device = "png"
)

ggsave(
  filename = "online_journal_level_reporting_practices.pdf",
  plot = journal_reporting_plot,
  width = 11,
  height = 7,
  device = "pdf"
)

# ============================================================
# SECTION 16: CLEANER FIGURE EXCLUDING JOURNALS NOT FOUND
# ============================================================

journal_reporting_clean <- journal_reporting %>%
  filter(journal != "Journal not found online")

if (nrow(journal_reporting_clean) > 0) {
  
  journal_reporting_clean_long <- journal_reporting_clean %>%
    pivot_longer(
      cols = starts_with("percent_reporting"),
      names_to = "reporting_category",
      values_to = "percent"
    ) %>%
    mutate(
      reporting_category = recode(
        reporting_category,
        percent_reporting_sex = "Sex reported",
        percent_reporting_age = "Age reported",
        percent_reporting_race_ethnicity = "Race/ethnicity reported"
      ),
      journal_label = paste0(journal, " (n=", n_studies, ")"),
      journal_label = fct_reorder(journal_label, n_studies)
    )
  
  journal_reporting_clean_plot <- ggplot(
    journal_reporting_clean_long,
    aes(
      x = journal_label,
      y = percent,
      fill = reporting_category
    )
  ) +
    geom_col(
      position = position_dodge(width = 0.75),
      width = 0.7
    ) +
    geom_text(
      aes(label = paste0(round(percent), "%")),
      position = position_dodge(width = 0.75),
      hjust = -0.1,
      size = 2.0,
      family = "Helvetica"
    ) +
    coord_flip() +
    scale_y_continuous(
      limits = c(0, 110),
      breaks = seq(0, 100, 20),
      labels = function(x) paste0(x, "%")
    ) +
    scale_fill_manual(
      values = reporting_blue_palette,
      name = "Reporting variable"
    ) +
    labs(
      title = "Journal-Level Reporting Practices",
      subtitle = "Excludes articles where journal metadata was not found online",
      x = "Journal",
      y = "% of included studies",
      fill = "Reporting variable"
    ) +
    theme_journal_reporting(base_size = 9)
  
  print(journal_reporting_clean_plot)
  
  ggsave(
    filename = "online_journal_level_reporting_practices_clean.png",
    plot = journal_reporting_clean_plot,
    width = 11,
    height = 7,
    dpi = 300,
    device = "png"
  )
  
  ggsave(
    filename = "online_journal_level_reporting_practices_clean.pdf",
    plot = journal_reporting_clean_plot,
    width = 11,
    height = 7,
    device = "pdf"
  )
}

# ============================================================
# SECTION 17: SUMMARY TABLE
# ============================================================

if ("knitr" %in% installed.packages() && "kableExtra" %in% installed.packages()) {
  library(knitr)
  library(kableExtra)
  
  journal_reporting_table <- journal_reporting %>%
    mutate(
      across(starts_with("percent"), ~ round(.x, 1)),
      across(starts_with("percent"), ~ paste0(.x, "%"))
    ) %>%
    rename(
      Journal = journal,
      "N studies" = n_studies,
      "Sex reported" = percent_reporting_sex,
      "Age reported" = percent_reporting_age,
      "Race/ethnicity reported" = percent_reporting_race_ethnicity
    )
  
  kable(
    journal_reporting_table,
    format = "html",
    caption = "Journal-Level Reporting Practices",
    align = c("l", "r", "r", "r", "r")
  ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover"),
      font_size = 12,
      full_width = FALSE
    ) %>%
    save_kable("journal_reporting_table.html")
}

# ============================================================
# SECTION 18: FINAL MESSAGE
# ============================================================

cat("\n=== EXTRACTION COMPLETE ===\n")
cat("Files created:\n")
cat("- online_metadata_results.csv\n")
cat("- online_extracted_demographic_reporting.csv\n")
cat("- online_journal_level_reporting_practices.csv\n")
cat("- online_overall_reporting_practices.csv\n")
cat("- online_journal_level_reporting_practices.png\n")
cat("- online_journal_level_reporting_practices.pdf\n")
cat("- online_journal_level_reporting_practices_clean.png (if applicable)\n")
cat("- online_journal_level_reporting_practices_clean.pdf (if applicable)\n")