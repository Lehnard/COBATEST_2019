library(DiagrammeR)

## DESCRIPCIÓ:
#
#   Funció per a dibuixar el flowchart de la neteja de dades específica per a la ETL COBATEST 2021. 
#
#
## VERSIÓ:
#
#   13-09-2022


make_flowchart_diagramm <- function(vih_flowchart) {
  grViz("digraph flowchart {

      graph [ranksep= 0.2]
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle]        
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      tab5 [label = '@@5']
      tab6 [label = '@@6']
      tab7 [label = '@@7']
      tab8 [label = '@@8']
      tab9 [label = '@@9']
      tab10 [label = '@@10']
      tab11 [label = '@@11']
      tab12 [label = '@@12']
      tab13 [label = '@@13']
      tab14 [label = '@@14']
      tab15 [label = '@@15']
      tab16 [label = '@@16']
      
      node [shape=none, width=0, height=0, label='']
      break1 -> tab6
      break2 -> tab8
      break3 -> tab10
      break4 -> tab12
      break5 -> tab14
      break6 -> tab16
      edge [minlen= 3]
      {rank=same; break1 -> tab5}
      {rank=same; break2 -> tab7}
      {rank=same; break3 -> tab9}
      {rank=same; break4 -> tab11}
      {rank=same; break5 -> tab13}
      {rank=same; break6 -> tab15}
      
      # edge definitions with the node IDs
      tab1 -> tab4
      tab2 -> tab4 
      tab3 -> tab4
      
      
      edge [dir=none]
      tab4 -> break1
      tab6 -> break2
      tab8 -> break3
      tab10 -> break4
      tab12 -> break5
      tab14 -> break6
      }

      
      [1]: paste0('Disaggregated data (Tool)', '\\n\\nN= ', vih_flowchart['Disaggregated data (Tool)']) 
      [2]: paste0('Disaggregated data (Non tool)', '\\n\\nN= ', vih_flowchart['Disaggregated data (Non tool)'])
      [3]: paste0('Aggregated data', '\\n\\nN= ', vih_flowchart['Aggregated data'])
      [4]: paste0('Total data', '\\n\\nN= ', vih_flowchart['Total data'])
      [5]: paste0('Non-HIV tests performed', '\\n\\nN= ', vih_flowchart['Non-HIV tests performed'])
      [6]: paste0('HIV Tests performed', '\\n\\nN= ', vih_flowchart['HIV Tests performed'])
      [7]: paste0('HIV Aged < 16', '\\n\\nN= ', vih_flowchart['HIV Aged < 16'])
      [8]: paste0('HIV Tests of people Aged >= 16', '\\n\\nN= ', vih_flowchart['HIV Tests of people Aged >= 16'])
      [9]: paste0('HIV Tests of people repeated same day', '\\n\\nN= ', vih_flowchart['HIV Tests of people repeated same day'])
      [10]: paste0('HIV Tests of people one per day', '\\n\\nN= ', vih_flowchart['HIV Tests of people one per day'])
      [11]: paste0('HIV Test prior to most recent\\nfor those tested more than once', '\\n\\nN= ', vih_flowchart['HIV Test prior to most recent for those tested more than once'])
      [12]: paste0('People tested for HIV', '\\n\\nN= ', vih_flowchart['People tested for HIV'])
      [13]: paste0('People with No HIV test result available', '\\n\\nN= ', vih_flowchart['People with No HIV test result available'])
      [14]: paste0('People tested for HIV screening test result available', '\\n\\nN= ', vih_flowchart['People tested for HIV screening test result available'])
      [15]: paste0('Previously diagnosed with HIV', '\\n\\nN= ', vih_flowchart['Previously diagnosed with HIV'])
      [16]: paste0('People tested with HIV screening test result available\\nwho are not previously diagnosed with HIV', '\\n\\nN= ', vih_flowchart['People tested with HIV screening test result available who are not previously diagnosed with HIV'])
      ")
}


## EXAMPLE:  ####
# 
# vih_flowchart <- c("Disaggregated data (Tool)"= 12741, 
#                    "Disaggregated data (Non tool)"= 21259, 
#                    "Aggregated data"= 104610, 
#                    "Total data"= 138610, 
#                    "Non-HIV tests performed"= 6676, 
#                    "HIV Tests performed"= 131934, 
#                    "HIV Aged < 16"= 425, 
#                    "HIV Tests of people Aged >= 16"= 131509, 
#                    "HIV Tests of people repeated same day"= 22, 
#                    "HIV Tests of people one per day"= 131487, 
#                    "HIV Test prior to most recent for those tested more than once"= 1878, 
#                    "People tested for HIV"= 129631, 
#                    "People with No HIV test result available"= 1014, 
#                    "People tested for HIV screening test result available"= 128617, 
#                    "Previously diagnosed with HIV"= 51, 
#                    "People tested with HIV screening test result available who are not previously diagnosed with HIV"= 128566)
# 
# make_flowchart_diagramm(vih_flowchart)
