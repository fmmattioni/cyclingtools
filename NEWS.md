# cyclingtools 0.0.0.9000

* Added function to estimate critical speed - `critical_speed()` (thanks to [@mrpetebonner](https://twitter.com/mrpetebonner)).

* Adjusted critical power functions to make sure it works even when column names have spaces.

* Added `critical_power()`.

* Added a `NEWS.md` file to track changes to the package.

## Breaking changes

* `power_in_y_axis` argument in `critical_power()` is now `reverse_y_axis`.

* All the previously `method_*` functions for individual critical power methods have been deleted and critical power should be estimated through the generic `critical_power()` only.
