install:
	R CMD INSTALL .

test:
	Rscript -e 'testthat::test_local()'

check:
	Rscript -e 'rcmdcheck::rcmdcheck(args = "--no-manual")'

doc:
	Rscript -e 'roxygen2::roxygenise()'

