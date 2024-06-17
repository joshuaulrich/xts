### Milestone
- [ ] Create and associate all release issues/PRs with the milestone
- [ ] Ensure all open issues are closed and roll open issues to the next milestone
- [ ] Update NEWS file
- [ ] Add any new contributors

### Package Testing
- [ ] Ensure all [*Additional* CRAN checks](https://cran.r-project.org/web/checks/check_results_xts.html) are addressed
- [ ] Run R-devel on Win-builder
- [ ] Use [Winston's R-debug containers](https://github.com/wch/r-debug) for valgrind, UBSAN, and ASAN
    * Start container: `docker run --rm -tiv [pwd]:/opt/xts/ wch1/r-debug:latest`
    * Get dependencies: `RDscript -e 'install.packages("xts", dependencies = TRUE)'`
    * Run check: `RD CMD check xts_X.Y.Z.tar.gz`
    * Install and run check with the remaining builds: `Rsan`, `Rcsan`, `Rvalgrind`
- [ ] Run rchk
    * `docker run --rm -v [pwd]/packages:/rchk/packages kalibera/rchk /rchk/packages/xts_X.Y.Z.tar.gz`

### Reverse Dependency Checks
- [ ] Note failures that are not related to package, for example:
    - Required vignette builder is not installed
    - Unit tests do not conditionally use other packages
- [ ] Notify downstream maintainers of API changes affecting their package
    - Provide the necessary patch and give them at least 2-weeks notice before release

    ##### Fix Failures
    - [ ]

### CRAN
- [ ] Tag release after package is on its way to CRAN
- [ ] Create release on GitHub
- [ ] Announce on social media (blog, tweet, LinkedIn, r-sig-finance, r-announce, etc.)
