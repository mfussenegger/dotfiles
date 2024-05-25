(variable_declaration
  (assignment_statement
    (variable_list
        name: (identifier) @tabstop)
    ))

(table_constructor
  (field
    name: _
    value: _ @tabstop))


(function_call
  name: _
  arguments: (arguments (_) @tabstop))
