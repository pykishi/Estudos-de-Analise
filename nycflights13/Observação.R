#-----Vôos sem aeronaves
sem_aviao = flights %>% anti_join(planes, "tailnum")

#-- Vôos sem aeroportos no destino
sem_aeroporto <- flights %>% anti_join(airports %>% rename(dest = faa))

#-- Vôos sem cia
sem_cia <- flights %>% anti_join(airlines, "carrier")

#-- Vôos sem partida NÃO CONSEGUI VER
sem_partida <- flights %>% filter(year == 0) %>% count()

#-- vôos sem hora de chegada e sem tempo de vôo
#-- NA em arr_time, arr_delay, air_time
colSums(is.na(flights))

#-- Vôos sem tempo
sem_tempo <- flights %>% 


view(dfSummary(flights))
view(dfSummary(airports))

View(flights)

voos <- flights %>% group_by(origin) %>% count();
class(voos)
class(flights$dep_time)

## Quantitativo ontime e deleyed
##???
flights <- flights %>% mutate(ontime = dep_delay <= 5)

flights <- flights %>% mutate(tipo = ifelse(dep_delay <= 5, "on time", "delayed"))

tbl <- prop.table(table(flights$tipo, flights$origin))
barplot(tbl,
        main = 'Analise Bivariada - Ontime',
        cex.names = 1.0,
        xlab = 'Aeroporto',
        ylab = 'Frequência (%)', cex.axis = 1.0,
        ylim = c(0,0.5), col = c('tomato','blue'), border = 'blue2')

legend("bottomleft", legend = row.names(tbl), fill = c('tomato','blue'))


nycflights %>% group_by(origin) %>% summarise(ontime_prop = sum(ontime == TRUE) / n()) %>% 
  arrange(desc(ontime_prop))



