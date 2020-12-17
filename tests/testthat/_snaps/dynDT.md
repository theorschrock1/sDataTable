# dynDT

    Code
      dynDT(x = mtcars)
    Output
      <dynDataTable>
        Public:
          add_variable_from_source: function (name) 
          build_exclude_formula: function (var_id, exclude_LOD) 
          build_formulas: function (var_id) 
          build_I_expr: function (I) 
          build_query_expr: function (J, BY = NULL, filter = NULL) 
          calculated_field: function (name, J, by = NULL, I = NULL) 
          check_for_self_referencing: function (name, J, by = NULL, I = NULL, from_source = F, include_LOD = NULL, 
          clone: function (deep = FALSE) 
          current_query_groups: active binding
          current_query_prereqs: active binding
          data: active binding
          data_expr: active binding
          data_setnames: function (old_name, new_name) 
          date_vars: active binding
          dimension_vars: active binding
          eval_pointers: function (x) 
          eval_variable_pointers: function (x, index = NULL, self_expr) 
          find_query_prerequite_vars: function (J, eval_pointers = T) 
          find_symbols: function (x = self$tmp_formula, index = NULL, self_expr = expr(self$tmp_formula), 
          fixed_variable_name: active binding
          func: R6ClassGenerator
          gen_requisites_variable_fn: function (var_id) 
          generate_fn: function (slot = c("I", "J", "by"), fn_type = c("query", "formula"), 
          generate_variable_id: function (variable_name, id = create_unique_id(length = 10)) 
          get_attrs: function (attrs = NULL, vars = NULL, .if = NULL, .in = NULL) 
          get_fixed_variables: function (vars = NULL) 
          get_name_fns: function (x) 
          get_variable_id: function (var_name, stop_if_not_found = T, return_matched = F) 
          id_table: active binding
          identity_vars: active binding
          initialize: function (data = NULL, vars = NULL, var_names_fn = NULL) 
          measure_vars: active binding
          new_source_variable_placeholder: function (name, data_type_fn) 
          new_variable: function (name, J, by = NULL, I = NULL, from_source = F, include_LOD = NULL, 
          next_level: function (exp_in, index) 
          parse_DT_list: function (x) 
          parse_formula_parts: function (name, J, by, I, exclude_LOD = NULL, include_LOD = NULL, 
          print_data: active binding
          query: function (vars, var_groups = NULL, filter = NULL) 
          query2: function (I = NULL, J, by = NULL) 
          remove_I_and_BY_from_DT_query: function (query) 
          rename_variable: function (old_name, new_name) 
          replace_symbol: function (symbol, self_expr, replace_with = "hidden_J") 
          replace_symbols_with_formula_pointers: function (formula) 
          replace_symbols_with_name_pointers: function (formula) 
          replace_symbols_with_pointer_type: function (formula, pointer) 
          set_data: function (data) 
          set_names_expr: active binding
          test_formula: function (test_formula, from_source = F, has_fixed_calculation, 
          test_J_formula: function (J_parsed, include_LOD = NULL) 
          tmp_expr: NULL
          tmp_formula: call
          transform: function (I = NULL, J, by = NULL) 
          var_count: active binding
          variable_names: active binding
          vars: list
        Private:
          .base_data_expr: call
          .current_query_groups: NULL
          .current_query_prereqs: NULL
          .data: DataTable, R6
          .id_table: data.table, data.frame
          .set_names: NULL
          .var_count: 11

