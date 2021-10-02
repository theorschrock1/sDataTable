# sdatatable_error_messages

    Code
      self$new_variable(name = "price", J = "SUM(price)", I = NULL, by = NULL,
        include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T)
    Output
      variable name `price` self referenced

---

    Code
      class(self$new_variable(name = "price", J = "SUM(price)", I = NULL, by = NULL,
        include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T))
    Output
      [1] "error_message" "glue"          "character"    

---

    Code
      self$new_variable(name = "price", J = "sum(price", I = NULL, by = NULL,
        include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T)
    Output
      Missing closed parenthises `)`

---

    Code
      class(self$new_variable(name = "price", J = "sum(price", I = NULL, by = NULL,
        include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T))
    Output
      [1] "error_message" "glue"          "character"    

---

    Code
      self$new_variable(name = "price", J = "sum('price)", I = NULL, by = NULL,
        include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T)
    Output
      Formula has errors: unexpected INCOMPLETE_STRING

---

    Code
      class(self$new_variable(name = "price", J = "sum('price)", I = NULL, by = NULL,
        include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T))
    Output
      [1] "error_message" "glue"          "character"    

---

    Code
      self$new_variable(name = NULL, J = "sum(price)", I = NULL, by = NULL,
        include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T)
    Output
      Missing variable name

---

    Code
      class(self$new_variable(name = NULL, J = "sum(price)", I = NULL, by = NULL,
        include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T))
    Output
      [1] "error_message" "glue"          "character"    

---

    Code
      self$new_variable(name = "sd", J = NULL, I = NULL, by = NULL, include_LOD = NULL,
        exclude_LOD = NULL, from_source = F, replace_old = T)
    Output
      Formula can't be empty

---

    Code
      class(self$new_variable(name = "sd", J = NULL, I = NULL, by = NULL,
        include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T))
    Output
      [1] "error_message" "glue"          "character"    

---

    Code
      self$new_variable(name = "sds", J = "sum(sd)", I = NULL, by = NULL,
        include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T)
    Output
      Unknown function call:`sum`.  Variable/s `sd` not found.

---

    Code
      self$new_variable(name = "Today's Date", J = "SUM(price)", I = NULL, by = NULL,
        include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = F)
    Output
      Variable name `Today's Date` already exists

---

    Code
      class(self$new_variable(name = "Today's Date", J = "SUM(price)", I = NULL, by = NULL,
        include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = F))
    Output
      [1] "error_message" "glue"          "character"    

---

    Code
      self$new_variable(name = "tmp", J = "`Today's Date`+`Today's Date`", I = NULL,
        by = NULL, include_LOD = NULL, exclude_LOD = NULL, from_source = F,
        replace_old = T)
    Output
      Error : Incompatible operation:[ `Today's Date` + `Today's Date` ]
      'date'+'date' not supported.
      

---

    Code
      class(self$new_variable(name = "tmp", J = "`Today's Date`+`Today's Date`", I = NULL,
        by = NULL, include_LOD = NULL, exclude_LOD = NULL, from_source = F,
        replace_old = T))
    Output
      [1] "error_message" "glue"          "character"    

---

    Code
      self$new_variable(name = "tmp", J = "carat+cut", I = NULL, by = NULL,
        include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T)
    Output
      Error : Incompatible operation:[ carat + cut ]
      'float'+'ordered' not supported.
      

---

    Code
      class(self$new_variable(name = "tmp", J = "carat+cut", I = NULL, by = NULL,
        include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T))
    Output
      [1] "error_message" "glue"          "character"    

---

    Code
      self$new_variable(name = "tmp", J = "cut+cut", I = NULL, by = NULL,
        include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T)
    Output
      Error : Incompatible operation:[ cut + cut ]
      'ordered'+'ordered' not supported.
      

---

    Code
      class(self$new_variable(name = "tmp", J = "cut+cut", I = NULL, by = NULL,
        include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T))
    Output
      [1] "error_message" "glue"          "character"    

---

    Code
      class(self$new_variable(name = "tmp", J = "cut+cut", I = NULL, by = NULL,
        include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T))
    Output
      [1] "error_message" "glue"          "character"    

---

    Code
      class(self$new_variable(name = "duration", J = "`Today's Date`-`Yesterday's Date`",
        I = NULL, by = NULL, include_LOD = NULL, exclude_LOD = NULL, from_source = F,
        replace_old = T))
    Output
      [1] "variable_id" "character"  

---

    Code
      self$new_variable(name = "tmp", J = "duration+carat", I = NULL, by = NULL,
        include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T)
    Output
      Error : Incompatible operation:[ duration + carat ]
      'd'+'float' not supported.
      

---

    Code
      class(self$new_variable(name = "tmp", J = "duration+carat", I = NULL, by = NULL,
        include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T))
    Output
      [1] "error_message" "glue"          "character"    

---

    Code
      self$new_variable(name = "tmp", J = "{fd=cut+cut}", I = NULL, by = NULL,
        include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T)
    Output
      Error : Incompatible operation:[ cut + cut ]
      'ordered'+'ordered' not supported.
      

---

    Code
      class(self$new_variable(name = "tmp", J = "{fd=cut+cut}", I = NULL, by = NULL,
        include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T))
    Output
      [1] "error_message" "glue"          "character"    

---

    Code
      self$new_variable(name = "tmp", J = "{fd=cut+cut\nfd+gg}", I = NULL, by = NULL,
        include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T)
    Output
      Variable/s `gg` not found.

---

    Code
      class(self$new_variable(name = "tmp", J = "{fd=cut+cut\nfd+gg}", I = NULL, by = NULL,
        include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T))
    Output
      [1] "error_message" "glue"          "character"    

---

    Code
      self$new_variable(name = "tmp", J = "{caret2=carat+table\nc\n}", I = NULL, by = NULL,
        include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T)
    Output
      Variable/s `c` not found.

---

    Code
      class(self$new_variable(name = "tmp", J = "{caret2=carat+table\nc\n}", I = NULL,
        by = NULL, include_LOD = NULL, exclude_LOD = NULL, from_source = F,
        replace_old = T))
    Output
      [1] "error_message" "glue"          "character"    

---

    Code
      self$new_variable(name = "tmp", J = "SUM()", I = NULL, by = NULL, include_LOD = NULL,
        exclude_LOD = NULL, from_source = F, replace_old = T)
    Output
      Error : Missing argument in `SUM()`
      

---

    Code
      class(self$new_variable(name = "tmp", J = "SUM()", I = NULL, by = NULL,
        include_LOD = NULL, exclude_LOD = NULL, from_source = F, replace_old = T))
    Output
      [1] "error_message" "glue"          "character"    

---

    Code
      self$new_variable(name = "tmp", J = "c", I = NULL, by = NULL, include_LOD = NULL,
        exclude_LOD = NULL, from_source = F, replace_old = T)
    Output
      Variable/s `c` not found.

