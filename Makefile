docs:
	Rscript -e "library(devtools); document('.'); check_doc()"

check:
	Rscript -e "library(devtools); check()"

test: 
	Rscript  -e "library(testthat); test()"	