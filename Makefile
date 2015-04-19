IRkernel.pdf: man/*.Rd
	RD2PDF_INPUTENC=inputenx R_RD4PDF=ae,hyper R CMD Rd2pdf --force --batch --no-preview --encoding=UTF-8 --output=$@ .

docs:
	Rscript -e "library(devtools); document('.'); check_doc()"

check:
	Rscript -e "library(devtools); check()"

test: 
	Rscript  -e "library(testthat); test()"
