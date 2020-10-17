<!--Before submitting an issue please be sure you can check these boxes (`[ ]` → `[x]`):-->

- [ ] I followed the [official installation instructions](https://irkernel.github.io/installation/)
  <!--If you have an installation problem, a segfault, `caught illegal operation`, `cannot open shared object file`,  …
      and installed IRkernel using Anaconda, please report your issue to them instead.-->
- [ ] I reproduced the error with the newest versions of `IRkernel`, `IRdisplay`, and `repr`
  <!--If the following code installs packages, you didn’t yet:
      old_pkgs <- intersect(old.packages()[, 'Package'], c('IRkernel', 'IRdisplay', 'repr'))
      if (length(old_pkgs) > 0) install.packages(old_pkgs)
  -->
- [ ] I included a [minimal reproducible example](https://stackoverflow.com/a/5963610/247482)
- [ ] This the right repository: I think the issue is with IRkernel, not
      [IRkernel/repr](https://github.com/IRkernel/repr/issues/new) or a third party repository
  <!--If the way a value is displayed is ugly or you got `ERROR while rich displaying an object`, it’s a `repr` problem.
      If an error stack trace or the install log shows a problem in another package, go to its issue tracker.-->
