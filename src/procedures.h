#pragma once

#include "object.h"

extern mpfr_prec_t const MPC_PREC;

/* Equivalence predicates */
Object scm_eqv_p(Object const args);
Object scm_eq_p(Object const args);

/* Numbers */
Object scm_number_p(Object const args);
Object scm_complex_p(Object const args);
Object scm_real_p(Object const args);
Object scm_rational_p(Object const args);
Object scm_integer_p(Object const args);
Object scm_exact_p(Object const args);
Object scm_inexact_p(Object const args);
Object scm_exact_integer_p(Object const args);
Object scm_finite_p(Object const args);
Object scm_infinite_p(Object const args);
Object scm_nan_p(Object const args);
Object scm_math_equal_p(Object const args);
Object scm_zero_p(Object const args);
Object scm_positive_p(Object const args);
Object scm_negative_p(Object const args);

Object scm_add(Object const args);
Object scm_mul(Object const args);
Object scm_sub(Object const args);
Object scm_div(Object const args);
Object scm_abs(Object const args);
Object scm_numerator(Object const args);
Object scm_denominator(Object const args);
Object scm_floor(Object const args);
Object scm_ceiling(Object const args);
Object scm_truncate(Object const args);
Object scm_exp(Object const args);
Object scm_log(Object const args);
Object scm_sin(Object const args);
Object scm_cos(Object const args);
Object scm_tan(Object const args);
Object scm_asin(Object const args);
Object scm_acos(Object const args);
Object scm_atan(Object const args);
Object scm_square(Object const args);
Object scm_sqrt(Object const args);
Object scm_exact_integer_sqrt(Object const args);
Object scm_expt(Object const args);
Object scm_make_rectangular(Object const args);
Object scm_make_polar(Object const args);
Object scm_real_part(Object const args);
Object scm_imag_part(Object const args);
Object scm_magnitude(Object const args);
Object scm_angle(Object const args);
Object scm_inexact(Object const args);
Object scm_exact(Object const args);

Object scm_number_tostring(Object const args);
Object scm_string_tonumber(Object const args);
/* Numbers end */

/* Booleans */
Object scm_not(Object const args);
Object scm_boolean_p(Object const args);
Object scm_boolean_eq_p(Object const args);
/* Booleans end */

/* Pairs and lists */
Object scm_pair_p(Object const args);
Object scm_cons(Object const args);
Object scm_car(Object const args);
Object scm_cdr(Object const args);
Object scm_set_car(Object const args);
Object scm_set_cdr(Object const args);
Object scm_null_p(Object const args);
Object scm_list(Object const args);
/* Pairs and lists end */
/* Symbols */
Object scm_symbol_p(Object const args);
Object scm_symbol_tostring(Object const args);
/* Characters */
Object scm_char_p(Object const args);
Object scm_char_alphabetic_p(Object const args);
Object scm_char_numeric_p(Object const args);
Object scm_char_whitespace_p(Object const args);
Object scm_char_upper_case_p(Object const args);
Object scm_char_lower_case_p(Object const args);
Object scm_digit_value(Object const args);
Object scm_char_tointeger(Object const args);
Object scm_integer_tochar(Object const args);
Object scm_char_upcase(Object const args);
Object scm_char_downcase(Object const args);
Object scm_char_foldcase(Object const args);
/* Strings */
Object scm_string_p(Object const args);
Object scm_make_string(Object const args);
Object scm_string(Object const args);
Object scm_string_length(Object const args);
Object scm_string_ref(Object const args);
Object scm_string_set(Object const args);
/* Vectors */
Object scm_vector_p(Object const args);
Object scm_vector(Object const args);
Object scm_vector_length(Object const args);
Object scm_vector_ref(Object const args);
Object scm_vector_set(Object const args);
/* Bytevectors */
Object scm_bytevector_p(Object const args);
Object scm_bytevector(Object const args);
Object scm_bytevector_length(Object const args);
Object scm_bytevector_ueight_ref(Object const args);
Object scm_bytevector_ueight_set(Object const args);
Object scm_utfeight_tostring(Object const args);
Object scm_string_toutfeight(Object const args);
/* Control features */
Object scm_procedure_p(Object const args);
/* Exceptions */
Object scm_error_implementation_defined_object(Object const args);
Object scm_error_object_p(Object const args);
Object scm_error_object_message(Object const args);
Object scm_error_object_irritants(Object const args);
Object scm_read_error_p(Object const args);
Object scm_file_error_p(Object const args);
/* Input and output */
Object scm_input_port_p(Object const args);
Object scm_output_port_p(Object const args);
Object scm_textual_port_p(Object const args);
Object scm_binary_port_p(Object const args);
Object scm_input_port_open_p(Object const args);
Object scm_output_port_open_p(Object const args);
Object scm_open_input_file(Object const args);
Object scm_open_binary_input_file(Object const args);
Object scm_open_output_file(Object const args);
Object scm_open_binary_output_file(Object const args);
Object scm_close_port(Object const args);
Object scm_read(Object const args);
Object scm_read_char(Object const args);
Object scm_eof_object_p(Object const args);
Object scm_eof_object(Object const args);
Object scm_write(Object const args);
Object scm_write_shared(Object const args);
Object scm_write_simple(Object const args);
Object scm_display(Object const args);
/* System interface */
Object scm_file_exists_p(Object const args);
Object scm_primitive_delete_file(Object const args);
Object scm_emergency_exit(Object const args);
