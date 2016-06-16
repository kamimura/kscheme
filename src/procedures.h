#pragma once

#include "object.h"

extern mpfr_prec_t const MPC_PREC;

/* Equivalence predicates */
Object scm_eqv_p(Object args);
Object scm_eq_p(Object args);

/* Numbers */
Object scm_number_p(Object args);
Object scm_complex_p(Object args);
Object scm_rational_p(Object args);
Object scm_exact_p(Object args);
Object scm_inexact_p(Object args);
Object scm_finite_p(Object args);
Object scm_infinite_p(Object args);
Object scm_nan_p(Object args);
Object scm_zero_p(Object args);
Object scm_positive_p(Object args);
Object scm_negative_p(Object args);

Object scm_add(Object args);
Object scm_mul(Object args);
Object scm_sub(Object args);
Object scm_div(Object args);
Object scm_numerator(Object args);
Object scm_denominator(Object args);
Object scm_floor(Object args);
Object scm_ceiling(Object args);
Object scm_truncate(Object args);
Object scm_sqrt(Object args);
/* Numbers end */

/* Booleans */
/* Booleans */

/* Pairs and lists */
Object scm_pair_p(Object args);
Object scm_cons(Object args);
Object scm_car(Object args);
Object scm_cdr(Object args);
Object scm_set_car(Object args);
Object scm_set_cdr(Object args);
Object scm_list(Object args);
/* Pairs and lists end */
/* Symbols */
Object scm_symbol_p(Object args);
/* Characters */
Object scm_char_p(Object args);
Object scm_char_alphabetic_p(Object args);
Object scm_char_numeric_p(Object args);
Object scm_char_whitespace_p(Object args);
Object scm_char_upper_case_p(Object args);
Object scm_char_lower_case_p(Object args);
Object scm_digit_value(Object args);
Object scm_char_tointeger(Object args);
Object scm_integer_tochar(Object args);
Object scm_char_upcase(Object args);
Object scm_char_downcase(Object args);
Object scm_char_foldcase(Object args);
/* Strings */
Object scm_string_p(Object args);
Object scm_make_string(Object args);
Object scm_string(Object args);
Object scm_string_length(Object args);
Object scm_string_ref(Object args);
Object scm_string_set(Object args);
/* Vectors */
Object scm_vector_p(Object args);
Object scm_vector(Object args);
Object scm_vector_length(Object args);
Object scm_vector_ref(Object args);
Object scm_vector_set(Object args);
/* Bytevectors */
Object scm_bytevector_p(Object args);
Object scm_bytevector(Object args);
Object scm_bytevector_length(Object args);
Object scm_bytevector_ueight_ref(Object args);
Object scm_bytevector_ueight_set(Object args);
Object scm_utfeight_string(Object args);
Object scm_string_utfeight(Object args);
/* Control features */
Object scm_procedure_p(Object args);
/* Exceptions */
Object scm_error_implementation_defined_object(Object args);
Object scm_error_object_p(Object args);
Object scm_error_object_message(Object args);
Object scm_error_object_irritants(Object args);
Object scm_file_error_p(Object args);
/* Input and output */
Object scm_input_port_p(Object args);
Object scm_output_port_p(Object args);
Object scm_textual_port_p(Object args);
Object scm_binary_port_p(Object args);
Object scm_input_port_open_p(Object args);
Object scm_output_port_open_p(Object args);
Object scm_open_input_file(Object args);
Object scm_open_binary_input_file(Object args);
Object scm_open_output_file(Object args);
Object scm_open_binary_output_file(Object args);
Object scm_close_port(Object args);
Object scm_read(Object args);
Object scm_read_char(Object args);
Object scm_eof_object_p(Object args);
Object scm_eof_object(Object args);
Object scm_write(Object args);
Object scm_write_shared(Object args);
Object scm_write_simple(Object args);
Object scm_display(Object args);
/* System interface */
Object scm_file_exists_p(Object args);
Object scm_primitive_delete_file(Object args);
Object scm_emergency_exit(Object args);
