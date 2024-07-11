Improving code for analysis of the Scottish Abdominal Aortic Aneurysm (AAA) Screening Programme
==============

This package includes functions used in the Public Health Scotland's analysis of the Scottish AAA Screening Programme data.

The idea behind these functions is to build more robust quality assurance into the code, reducing the chance of errors when running scripts.

Currently, there are 4 functions exported:
- add_new_rows()
- build_history()
- eval_seasonal_diff()
- query_write_rds()

***`add_new_rows()`***

This function is similar to base `rbind()` or dplyr's `bind_rows()`, in that it combines two dataframes row-wise. The additional feature is that you can prevent uncontrolled replication of this concatenation by specifcying which columns to check for duplication between the dataframes.


***`build_history()`***

This function is specific to the AAA KPIs project and updates the dataframe containing historical data with new data in the Autumn run. 


***`eval_seasonal_diff()`***

This function allows season-dependent evaluation of expressions. It will run different expressions based on whether `season` is "spring" or "autumn".


***`query_write_rds()`***

This function prevents unknowing overwriting of .rds files by requiring user input before evaluating the `write_rds()` expression. It can be used as a very useful QA step where `write_rds()` functions may be embedded in other functions or just not very obviously placed in the script.


