document:
	R script -e "devtools::document()"

build:
	R script -e "devtools::build()"

check:
	R script -e "devtools::check()"

install:
	R script -e "devtools::install(build_vignettes = TRUE, upgrade_dependencies = TRUE)"

winbuild:
	R script -e "devtools::build_pkg(version = 'R-devel', quiet = TRUE)"

release:
	R script -e "devtools::release()"

pkgdown:
	Rscript -e "library(pkgdown); clean_site(); init_site(); build_home(); build_reference(run_dont_run = TRUE); build_news()"
