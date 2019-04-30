## MODEL REQUIREMENTS

 - all models must have an `$initialize()`, `$fit()` and a `$forecast()` method
 - all models must have a logical `fit_once` as an active binding. This should be set to FALSE if the model requires itself to be refit every time there is new data. It should be set to true if the model is designed to be fit once at the beginning of a season.
 - the main model definition must reside in a file with the same name as the model. E.g. the `sarimaTDModel` is stored in the `models/sarimaTDModel.R` file.