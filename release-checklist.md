### Milestone
- [ ] Create and associate all release issues/PRs with the milestone
- [ ] Ensure all open issues are closed and roll open issues to the next milestone
- [ ] Update NEWS file
- [ ] Add any new contributors

### Package Testing
- [ ] Ensure all [*Additional* CRAN checks](https://cran.r-project.org/web/checks/check_results_xts.html) are addressed
- [ ] Run via R-Hub for linux, Windows, Solaris
    ```r
    platforms <-
      c("linux-x86_64-rocker-gcc-san",
        "solaris-x86-patched",
        "windows-x86_64-devel",
        "windows-x86_64-release",
        "ubuntu-rchk")
    rhub::check("xts_X.Y.Z.tar.gz", platforms)
    ```
- [ ] Run R-devel on Win-builder
- [ ] Use [Winston's R-debug containers](https://github.com/wch/r-debug) for valgrind, UBSAN, and ASAN
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
