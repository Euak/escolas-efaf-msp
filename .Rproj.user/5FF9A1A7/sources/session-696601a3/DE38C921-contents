library(tidyverse)

censo <- tibble()

for (x in c(7:19)) {
    y <- if_else(x < 10, paste0("0", as.character(x)), as.character(x))
    censo.tmp <- read_csv2(paste0("Bases/microdados_ed_basica_20", y, ".csv"), 
                           locale = locale(encoding = "ISO-8859-1")) %>% 
        filter(CO_MUNICIPIO == 3550308 & IN_FUND_AF == 1) %>% 
        select(NU_ANO_CENSO, CO_MUNICIPIO, CO_DISTRITO, CO_ENTIDADE, 
               NO_ENTIDADE, TP_DEPENDENCIA, TP_CATEGORIA_ESCOLA_PRIVADA,
               CO_CEP)
    
    censo <- bind_rows(censo, censo.tmp)
}


TBL1 <- censo %>% 
    group_by(NU_ANO_CENSO) %>% 
    summarise(n = n())

TBL1 %>% 
    ggplot(aes(x = NU_ANO_CENSO, y = n)) +
    geom_col()

TBL2 <- censo %>% 
    group_by(NU_ANO_CENSO, TP_DEPENDENCIA) %>% 
    summarise(n = n())

TBL2 %>% 
    ggplot(aes(x = NU_ANO_CENSO, y = n)) +
    geom_col() +
    facet_wrap(~TP_DEPENDENCIA)


sum(!is.na(censo$CO_CEP))
    