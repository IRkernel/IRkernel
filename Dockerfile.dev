# Installs Jupyter Notebook and IRkernel dependencies into a r-notebook-dev image

FROM jupyter/r-notebook

RUN Rscript -e "install.packages(c(\"devtools\", \"testthat\", \"roxygen2\"), repos = c(\"http://irkernel.github.io/\", \"http://cran.rstudio.com\"))"

RUN Rscript -e "library(\"devtools\")" -e "install_github(\"IRkernel/repr\")" -e "install_github(\"IRkernel/IRdisplay\")"

RUN pip install jupyter_kernel_test ndjson-testrunner

RUN unlink /opt/conda/lib/libstdc++.so.6
