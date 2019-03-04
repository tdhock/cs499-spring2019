quiz.tex.vec <- Sys.glob("*/quiz.tex")
quiz.vec <- sapply(quiz.tex.vec, function(quiz.tex){
  quiz.lines <- readLines(quiz.tex)
  paste(quiz.lines, collapse="\n")
})
content.vec <- namedCapture::str_match_variable(
  quiz.vec,
  "Name:[^\n]+\n",
  "\n",
  "(?:[^\n]+\n)+",
  "\n",
  content="(?:[^\n]*\n)*",
  "\\\\end{document}")
nchar(content.vec)
out.vec <- paste0(
  "\\section{", sub("/.*", "", rownames(content.vec)), "}\n",
  content.vec,
  "\\newpage")
writeLines(out.vec, "exam-1-review.tex")
