#'getONC
#'
#'Consultar los datos de incidencia delictiva del SESNSP limpiados por el Observatorio Nacional Ciudadano
#'
#'@param nivel
#'Valor string puede ser "estatal" o "municipal" para consultar los datos estatales o municipales
#'
#'@param valores
#'Valor string para definir los valores consultados de la base, opciones: "ci_tasa" para tasa de carpetas de investigacion, "ci_abs" para numero absoluto de carpetas, "vic_tasa" para tasa de victimas y "vic_abs" para numero absoluto de victimas.
#'Tambien se puede definir como un vector que contenga mas de uno de los valores
#'
#'@param nom_periodo
#'Valor string para definir el periodo temporal de los datos, opciones: "mensual", "bimestral", "trimestral", "cuatrimestral", "semestral" o "anual"
#'
#'@param modalidades
#'Argumento para incluir datos sobre modalidad del delito y caracteristicas de las victimas
#'
#'@return
#'@export
#'
#'@examples
#'getONC("estatal", c("ci_tasa", "vic_abs"), "mensual", modalidades = FALSE)
#'
#'getONC("estatal", c("ci_tasa", "vic_abs"), "semestral", modalidades = TRUE)
#'
#'getONC("municipal", "ci_abs", "anual", modalidades = FALSE)
getONC <- function(nivel, valores, nom_periodo, modalidades = FALSE) {

  aux_per <- data.frame(periodo =  c("mensual", "bimestral", "trimestral",
                                     "cuatrimestral", "semestral", "anual"),
                        num_periodo = c(1, 2, 3, 4, 6, 12))
  num_per <-  aux_per %>% filter(periodo == nom_periodo) %>% pull(num_periodo)



  if(modalidades == FALSE) {
    if(num_per %in% c(1,2,3,4,6,12) & nivel == 'estatal') {

      niveles <- c('estatal' = 'estatal_virk_ordenado.csv?dl=1',
                   'municipal' = 'tidy_mpal_full.csv')

      valores1 <- c('ci_abs' = 'researchFolders', 'ci_tasa' = 'researchFoldersRate',
                    'vic_abs' = 'victimCase', 'vic_tasa' = 'victimRate')

      base <- read_csv("https://www.dropbox.com/s/qqkadfzjq1gktpt/estatal_virk_ordenado.csv?dl=1",
                       col_types = c('victimCase' = 'd',
                                     'victimRate' = 'd')) %>%
        mutate(date = ceiling_date(date, paste(num_per, 'months'))-1) %>%
        group_by(inegiEntidad, inegiEntidadName, typeCrimeName, typeCrime, date) %>%
        summarise(researchFolders = sum(researchFolders),
                  researchFoldersRate = sum(researchFoldersRate),
                  victimCase = sum(victimCase, na.rm = TRUE),
                  victimRate = sum(victimRate, na.rm = TRUE)) %>% ungroup() %>%
        select(inegiEntidad:date, valores1[valores])

      return(base)

    } else if (num_per %in% c(1,2,3,4,6,12) & nivel == 'municipal') {

      nivel <- "municipal"
      nom_periodo <- "anual"
      valores <- "ci_abs"

      niveles <- c('estatal' = 'estatal_virk_ordenado.csv',
                   'municipal' = 'tidy_mpal_full.csv')

      valores1 <- c('ci_abs' = 'researchFolders', 'ci_tasa' = 'researchFoldersRate')

      base <- read_csv("https://www.dropbox.com/s/fl0rpte4lxplq1b/tidy_mpal_full.csv?dl=1") %>%
        mutate(date = ceiling_date(date, paste(num_per, 'months'))-1) %>%
        group_by(inegiEntidad, inegiMunicipality, inegiMunicipalityName,
                 inegiEntidadName, typeCrimeName, typeCrime, date) %>%
        summarise(researchFolders = sum(researchFolders),
                  researchFoldersRate = sum(researchFoldersRate)) %>% ungroup() %>%
        select(inegiEntidad:date, valores1[valores])

      return(base)


    } else {
      if(num_per %in% -c(1,2,3,4,6,12)){
        stop('plis pon periodo correcto, opciones:

                 "mensual", "bimestral", "trimestral",
                 "cuatrimestral", "semestral", "anual"')
      } else {
        stop('plis pon nivel correcto, opciones:

                 "estatal", "municipal"
                 ')

      }

    }

  } else {
    warning("Para la base de modalidades y víctimas sólo existe información
          disponible a nivel estatal y de valores absolutos (no tasas)")

    if(num_per %in% c(1,2,3,4,6,12) & nivel == 'estatal') {
      base <- read_csv("https://www.dropbox.com/s/8gvc9yf1ujpow2z/tidy_mod.csv?dl=1") %>%
        mutate(date = ceiling_date(fecha, paste(num_per, 'months'))-1) %>%
        group_by(inegiEntidad, inegiEntidadName, typeCrimeName, date, modalidad, sexo, rango_edad) %>%
        summarise(vic_abs = sum(vic_abs_ind, na.rm = T)) %>% ungroup()

      return(base)
    } else {
      stop('plis pon periodo correcto, opciones:

                 "mensual", "bimestral", "trimestral",
                 "cuatrimestral", "semestral", "anual"')
    }
  }
}
