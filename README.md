# Central Park Squirrel Dashboard

An R Shiny application visualising squirrel sightings in Central Park, NYC.

## Installing Packages

To restore the project environment, run the following in your R console from the project root:

```r
install.packages('renv')
renv::restore()
```

## Running the App Locally

**RStudio:** Open `src/app.R` in RStudio and click the **Run App** button in the top-right corner of the editor.

**R console:** Run the following from the project root:

```r
shiny::runApp('src')
```