Before submitting an issue please be sure to answer these questions with **yes**:

- [ ] Did you follow the [official installation instructions](https://irkernel.github.io/installation/)?

    If you have an installation problem and used e.g. Anaconda, please report your issue to them instead.

- [ ] Did you reproduce the error with the newest versions? If the following code installs packages, you didn’t yet!

    ```r
    old_pkgs <- intersect(old.packages()['Package', ], c('IRkernel', 'IRdisplay', 'repr'))
    if (length(old_pkgs) > 0) install.packages(old_pkgs)
    ```

- [ ] Did you include a [minimal reproducible example](https://stackoverflow.com/a/5963610/247482)?
- [ ] Is this the right repository?

    If the way a value is displayed is ugly or you got `ERROR while rich displaying an object`,
    go to [IRkernel/repr/issues/new](https://github.com/IRkernel/repr/issues/new) instead.
