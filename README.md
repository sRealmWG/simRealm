# simRealm
Simulations, results and figures for the sRealmTools project.

## sRealmTools and mobsim packages
This project depends heavily on the package sRealmTools ("sRealmWG/sRealmTools") that
has functions built on top of mobsim to handle mobsim simulations.  
simRealm also depends heavily on a fork of mobsim ("AlbanSagouis/mobsim") where 
useful new features were added to core functions.

## Containerisation
The sRealmTools package only works with Alban's version of mobsim, not the official one.
Since you probably have code that runs with the official `mobsim`, you should use
`renv` to isolate your sRealmTools R project folder from your other project folders.  
After setting your sRealmTools folder as the R working directory or created a R project
inside this folder, installing `sRealmTools` should also install Alban's version of `mobsim`.

```
renv::activate()
renv::install("sRealmWG/sRealmTools")
renv::restore() # to install packages used by collaborators
renv::snapshot() # to save/update the list of packages you use
```
