# Testing the residual-based shading scheme

Ran the first test in `quick-test.R`


## Error: Use of `%>%` rather than native pipe `|>`

```
Warning message:
  Computation failed in `stat_mosaic()`.
Caused by error in `data %>% dplyr::select(dplyr::all_of(c(vars, ".n"))) %>% dplyr::distinct()`:
  ! could not find function "%>%"
```
* I corrected this.

## `stat_mosaic()` error

```
Warning message:
Computation failed in `stat_mosaic()`.
Caused by error in `UseMethod()`:
! no applicable method for 'distinct' applied to an object of class "NULL" 
```
