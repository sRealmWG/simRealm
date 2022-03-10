# simRealm
Simulations, results and figures for the sRealm project.

## sRealm and mobsim packages
This project depends heavily on the package sRealm ("AlbanSagouis/sRealm") that
has functions built on top of mobsim to handle mobsim simulations.  
simRealm also depends heavily on a fork of mobsim ("AlbanSagouis/mobsim") where 
useful new features were added to core functions.

## Containerisation
The sRealm package only works with Alban's version of mobsim, not the official one.
Since you probably have code that runs with the official `mobsim`, you should use
`renv` to isolate your sRealm R project folder from your other project folders.  
After setting your sRealm folder as the R working directory or created a R project
inside this folder, installing `sRealm` should also install Alban's version of `mobsim`.

```
renv::activate()
renv::install("AlbanSagouis/sRealm")
```
