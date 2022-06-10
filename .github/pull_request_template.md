**What changes are proposed in this pull request?**

**Which files are involved in this pull request and what changes were made?**

**Did you include unit tests for the proposed change/bug fix (https://testthat.r-lib.org/)?**

**If there is an GitHub issue associated with this pull request, please provide link.**

--------------------------------------------------------------------------------

Checklist for PR reviewer

- [ ] PR branch has pulled the most recent updates from main branch. Ensure the pull request branch and your local version match and both have the latest updates from the main branch.
- [ ] If a new function was added, function should be included in `_pkgdown.yml`
- [ ] If a bug was fixed, a unit test was added for the bug check
- [ ] Run `pkgdown::build_site()`. Check the R console for errors, and review the rendered website.
- [ ] Code coverage is suitable for any new functions/features. Review coverage with `withr::with_envvar(new = c("NOT_CRAN" = "true"), covr::report())`. Before you run, begin a fresh R session without any packages loaded. 
- [ ] R CMD Check runs without errors, warnings, and notes
- [ ] `usethis::use_spell_check()` runs with no spelling errors in documentation
- [ ] Has `NEWS.md` been updated with the changes from this pull request under the heading indicating the latest version. If there is an issue associated with the pull request, reference it in parentheses at the end update (see `NEWS.md` for examples).
- [ ] Has the version number been incremented using `usethis::use_version(which = "dev")` 
- [ ] Has the README been knitted before merging to ensure each PR has the most up-to-date readme?
- [ ] Approve Pull Request
- [ ] Merge the PR. Please use "Squash and merge".
