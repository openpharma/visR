## Resubmission
This is a resubmission. In this version I have:

* added notes that we are deprecating `visr.survfit()` and `visr.tidycuminc()`, recommending the use of {ggsurvfit} instead.  
* Minor edit to documentation in `vis.R` to remove a note due to an itemized list. 


## Test environments
* Ubuntu 18.04 LTS (on github actions), devel, release, oldrel-1, oldrel-2, oldrel-3, oldrel-4
* Windows Server 2019 (on github actions), release
* macOS (on github actions), release
* win-builder devel

## R CMD check results

There were no ERRORs or WARNINGs. 

There are 2 NOTEs I am aware of. 

1. One is only found on Windows (Server 2022, R-devel 64-bit): 

```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```

As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this may be due to a bug/crash in MiKTeX and can likely be ignored.


2. The second is 

```
* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''
```

As noted in [R-hub issue #560](https://github.com/r-hub/rhub/issues/560), this seems to be an Rhub issue and hopefully be ignored. 
