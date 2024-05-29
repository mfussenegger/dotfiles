(method_declaration
  name: (identifier) @tabstop
  parameters: (formal_parameters) @tabstop)


[
 (string_literal)
 (decimal_integer_literal)
 (true)
 (false)
] @_literal

(method_invocation
  object: (identifier) @_object (#eq? @_object "Map")
  name: (identifier) @_name (#eq? @_name "of")
  arguments: (argument_list
    (_)
    .
    (_literal) @tabstop
    .
  )
)

(method_invocation
  object: (identifier) @_object (#eq? @_object "Map")
  name: (identifier) @_name (#eq? @_name "of")
  arguments: (argument_list
    (_)
    .
    (_literal) @tabstop
    .
    (_)
    .
    (_literal) @tabstop
    .
  )
)

(method_invocation
  object: (identifier) @_object (#eq? @_object "Map")
  name: (identifier) @_name (#eq? @_name "of")
  arguments: (argument_list
    (_)
    .
    (_literal) @tabstop
    .
    (_)
    .
    (_literal) @tabstop
    .
    (_)
    .
    (_literal) @tabstop
    .
  )
)
