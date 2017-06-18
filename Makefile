oneclick:
	Rscript -e "library(pkgdown); clean_site(); init_site(); build_home(); build_reference(run_dont_run = TRUE); build_news()"
