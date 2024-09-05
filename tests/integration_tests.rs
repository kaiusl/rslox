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
    comments;
    line_at_eof,
    only_line_comment_and_line,
    unicode,
    only_line_comment,
);

test_dir!(
    nil;
    literal,
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

test_dir!(empty_file, precedence, unexpected_character,);
