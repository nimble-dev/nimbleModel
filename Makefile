.PHONY: FORCE

VERSION=$(shell grep Version: nimbleModel/DESCRIPTION | grep -o '[^ ]*$$')

version: FORCE
	@echo $(VERSION)

configure: nimbleModel/configure.ac
	(cd nimbleModel; autoconf)

# this rebuilds Rd files in man/ and rebuilds NAMESPACE
man: FORCE
	./prep_pkg.R

nimbleModel_$(VERSION).tar.gz: FORCE
	@echo "Building nimbleModel package in this directory"
	(R CMD build nimbleModel)

build: nimbleModel_$(VERSION).tar.gz

install: nimbleModel_$(VERSION).tar.gz
	R CMD INSTALL nimbleModel_$(VERSION).tar.gz

check: build FORCE
	R CMD check --as-cran nimbleModel_$(VERSION).tar.gz

rhub: build FORCE
	Rscript -e "library(rhub); check(platform = c('ubuntu-gcc-devel','windows-x86_64-devel','linux-x86_64-rocker-gcc-san','linux-x86_64-centos6-epel','fedora-clang-devel'))"

test: FORCE
	cd .. ; ./install_requirements.R
	cd .. ; ./run_tests.R --parallel

benchmark: FORCE
	mkdir -p profile
	NIMBLEMODEL_BENCHMARK_DIR=$(PWD)/profile \
	  TF_CPP_MIN_LOG_LEVEL=3 \
	  R --quiet --slave --vanilla --file=benchmark.R

# This requires libgoogle-perftools-dev, pprof, and requires
# building nimbleModel with the following additions to ~/.R/Makvars:
# LDFLAGS += -g
# PKG_LIBS += -lprofiler
profile-google: FORCE
	CPUPROFILE=$(PWD)/profile/profile.log \
	  NIMBLEMODEL_BENCHMARK_DIR=$(PWD)/profile \
	  NIMBLEMODEL_BENCHMARK_SEC=0.5 \
	  R --quiet --vanilla --slave --file=benchmark.R
	PPROF_BINARY_PATH=$(PATH):$(shell R RHOME)/bin:$(PWD)/profile \
	  pprof -web $(shell R RHOME)/bin/R $(PWD)/profile/profile.log

# This requires valgrind, prefers kcachegrind, and requires
# building nimbleModel with the following additions to ~/.R/Makvars:
# LDFLAGS += -g
profile-callgrind: FORCE
	mkdir -p profile
	NIMBLEMODEL_BENCHMARK_DIR=$(PWD)/profile \
	NIMBLEMODEL_BENCHMARK_SEC=10.0 \
	  R --quiet --slave --vanilla --file=benchmark.R \
	   --debugger=valgrind \
	   --debugger-args="--tool=callgrind --callgrind-out-file=profile/callgrind.out"
	kcachegrind profile/callgrind.out || callgrind_annotate profile/callgrind.out

# We are progressively delegating style to clang-format.
# Some older C++ files are still styled manually.
clang-format: FORCE
	clang-format -style=file \
	  nimbleModel/inst/include/nimbleModel/NimArrBase.h \
	  nimbleModel/inst/include/nimbleModel/NimArr.h \
	  nimbleModel/inst/include/nimbleModel/predefinedNimbleModelLists.h \
	  nimbleModel/inst/include/nimbleModel/tensorflow.h \
	  nimbleModel/inst/CppCode/predefinedNimbleModelLists.cpp \
	  nimbleModel/inst/CppCode/tensorflow.cpp \
	  -i

license: FORCE
	Rscript update_license.R

# Keeps .Rproj.user/ and .Rhistory.
clean: FORCE
	git clean -dfx -e .R*

mrproper: FORCE
	git clean -dfx

FORCE:

