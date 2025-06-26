library(quarto)
setwd("/home/vruiz/URJC/CURSOS/curso_verano")

quarto_render (
   "quijote.qmd" ,
   output_format = c ("pdf","html","docx")
   )
