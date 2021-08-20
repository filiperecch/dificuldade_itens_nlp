##############################################################################-
## Project: NLP itens ENEM
## Script purpose: ler prova ENEM, quebrar por questao e fazer BD
## Date: 2021-08-18
## Author: Filipe Recch
##############################################################################-

##  Overview ----
##############################################################################-

## Packages, Parameters, & Input Data ----
##############################################################################-
suppressPackageStartupMessages(library(conflicted))
suppressPackageStartupMessages(library(janitor))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(tidyverse))

conflict_prefer("filter", "dplyr")

# Paths -------------------------------------------------------------------

source("file_paths.R")

path_provas <- fs::path(path_data_raw, "Provas", "txt")


##  TBD ----
##############################################################################-

for (y in seq(2009, 2019)) {
  txt_prova_temp <- readtext::readtext(fs::path(path_provas, glue::glue("{y}.txt")), encoding = "UTF-8")
  
  data_prova_temp <- txt_prova_temp$text %>% 
    str_split("(?=Questão enem\\d{10}\\w?)") %>% 
    unlist() %>% 
    tibble("questao" = .) %>% 
    slice(-1) %>% 
    mutate(codigo_questao = str_extract(questao, "(?!Questão enem)\\d{10}\\w?"),
           texto_split = str_split(questao, "\\n\\s?\\n?\\s*?(?=A\\)\\s*)")) %>% 
    unnest_wider(texto_split) %>% 
    rename(enunciado_codigo = ...1,
           alternativas = ...2) %>% 
    mutate(enunciado = str_remove(enunciado_codigo, "Questão enem\\d{10}\\w?")) %>% 
    select(codigo_questao, questao, enunciado, alternativas)
  
  assign(glue::glue("data_prova_{y}"),
         data_prova_temp)
  
  write_excel_csv(data_prova_temp,
                  fs::path(path_data_root, glue::glue("data_prova_{y}.csv")))
}

for (y in seq(2009, 2019)) {

  glue::glue("data_prova_{y}") %>% 
    get() %>% 
    pull(alternativas) %>% 
    is.na() %>% 
    mean() %>% 
    print()
}


