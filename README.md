# project-template
All we need to start a collaborative repo :rocket:

# getting started

1. This template requires that the path to your Onedrive is set up in your `.Renviron`
  - Open the `.Renviron` located in your home folder. This can be done from the Terminal or using `usethis::edit_r_environ()`
  - Add the corresponding line to it:
```
EPI_ONEDRIVE_PATH = "ADD YOUR ONEDRIVE PATH HERE"

```
  - Restart your R session so that the updated .Renviron is loaded
  - the `set_paths()` function allows you to quicly access your onedrive path as well as all of the sync folder paths. 
  
2. If you need to store data locally for your project. Create a `/data` directory. This directory is gitignored and will never be tracked.
3. If you need to store outputs locally for your project. Create a `/local` directory. This directory is gitignored and will never be tracked.
5. If you need to use temporary/sensitive files for your project. Create a `/temp` directory. This directory is gitignored and will never be tracked.
6. If you are using [renv](https://cran.r-project.org/web/packages/renv/index.html), all `renv` files and folders will gitignored and will never be tracked. This is because the `renv` lockfile usually generates a lot of conflicts. You can always use it locally for your own needs.
