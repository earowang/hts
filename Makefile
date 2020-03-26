document:
	Rscript -e "devtools::document()"

readme:
	Rscript -e "rmarkdown::render('README.Rmd')"

build:
	Rscript -e "devtools::build()"

check:
	Rscript -e "devtools::check()"

install:
	Rscript -e "devtools::install(build_vignettes = TRUE)"

revdep-check:
	Rscript -e "devtools::revdep_check(); devtools::revdep_check_save_summary(); devtools::revdep_check_print_problems()"

winbuild:
	Rscript -e "devtools::build_win(version = 'R-devel', quiet = TRUE)"

pkgdown:
	Rscript -e "pkgdown::build_site(run_dont_run = TRUE)"
