mod common;
use common::*;


test_dir!(
    operator;
    add_num_nil,
    comparison,
    add_nil_nil,
    less_or_equal_nonnum_num,
    add_string_nil,
    divide,
    subtract_nonnum_num,
    greater_or_equal_nonnum_num,
    greater_or_equal_num_nonnum,
    multiply_num_nonnum,
    subtract_num_nonnum,
    subtract,
    equals_class,
    greater_num_nonnum,
    greater_nonnum_num,
    equals_method,
    not_class,
    less_nonnum_num,
    negate_nonnum,
    add_bool_num,
    less_num_nonnum,
    less_or_equal_num_nonnum,
    not_equals,
    negate,
    divide_num_nonnum,
    add_bool_nil,
    equals,
    add_bool_string,
    multiply,
    multiply_nonnum_num,
    add,
    not,
    divide_nonnum_num,
);


test_dir!(
    class;
    inherit_self,
    local_inherit_other,
    local_inherit_self,
    reference_self,
    local_reference_self,
    empty,
    inherited_method,
);


test_dir!(
    function;
    recursion,
    nested_call_with_arguments,
    print,
    parameters,
    too_many_parameters,
    missing_comma_in_parameters,
    mutual_recursion,
    local_mutual_recursion,
    empty_body,
    local_recursion,
    too_many_arguments,
    missing_arguments,
    body_must_be_block,
    extra_arguments,
);


test_dir!(
    return;
    after_while,
    in_method,
    return_nil_if_no_value,
    after_else,
    at_top_level,
    in_function,
    after_if,
);


test_dir!(
    block;
    scope,
    empty,
);


test_dir!(
    if;
    var_in_else,
    truth,
    else,
    dangling_else,
    class_in_else,
    var_in_then,
    fun_in_else,
    fun_in_then,
    class_in_then,
    if,
);


test_dir!(
    logical_operator;
    or_truth,
    or,
    and,
    and_truth,
);


test_dir!(
    for;
    closure_in_body,
    statement_condition,
    syntax,
    fun_in_body,
    class_in_body,
    return_inside,
    statement_initializer,
    statement_increment,
    return_closure,
    var_in_body,
    scope,
);


test_dir!(
    comments;
    line_at_eof,
    only_line_comment_and_line,
    unicode,
    only_line_comment,
);


test_dir!(
    assignment;
    associativity,
    to_this,
    undefined,
    syntax,
    prefix_operator,
    grouping,
    local,
    infix_operator,
    global,
);


test_dir!(
    nil;
    literal,
);


test_dir!(
    while;
    closure_in_body,
    syntax,
    fun_in_body,
    class_in_body,
    return_inside,
    return_closure,
    var_in_body,
);


test_dir!(
    variable;
    use_nil_as_var,
    shadow_local,
    use_global_in_initializer,
    undefined_global,
    in_nested_block,
    duplicate_local,
    local_from_method,
    uninitialized,
    use_false_as_var,
    duplicate_parameter,
    collide_with_parameter,
    redeclare_global,
    scope_reuse_in_different_blocks,
    early_bound,
    undefined_local,
    redefine_global,
    use_local_in_initializer,
    shadow_global,
    unreached_undefined,
    shadow_and_local,
    use_this_as_var,
    in_middle_of_block,
);


test_dir!(
    regression;
    _40,
    _394,
);


test_dir!(
    bool;
    equality,
    not,
);


test_dir!(
    print;
    missing_argument,
);


test_dir!(
    closure;
    reference_closure_multiple_times,
    nested_closure,
    reuse_closure_slot,
    close_over_later_variable,
    assign_to_closure,
    close_over_method_parameter,
    shadow_closure_with_local,
    closed_closure_in_function,
    open_closure_in_function,
    unused_later_closure,
    assign_to_shadowed_later,
    close_over_function_parameter,
    unused_closure,
);


test_dir!(
    field;
    get_on_class,
    get_on_function,
    get_on_num,
    set_on_string,
    call_nonfunction_field,
    get_on_nil,
    undefined,
    set_on_num,
    set_on_function,
    on_instance,
    set_on_nil,
    get_on_bool,
    many,
    set_on_class,
    get_and_set_method,
    method,
    method_binds_this,
    set_on_bool,
    get_on_string,
    set_evaluation_order,
    call_function_field,
);


test_dir!(
    call;
    object,
    string,
    num,
    bool,
    nil,
);


test_dir!(
    number;
    leading_dot,
    decimal_point_at_eof,
    literals,
    nan_equality,
    trailing_dot,
);


test_dir!(
    string;
    multiline,
    error_after_multiline,
    literals,
    unterminated,
);


test_dir!(
    empty_file,
    precedence,
    unexpected_character,
);
