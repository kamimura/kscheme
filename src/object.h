#pragma once

#include <stdlib.h>
#include <stdio.h>
typedef enum type {
  EMPTY,
  PAIR,
  IDENTIFIER,
  TRUE_TYPE,
  FALSE_TYPE,
  NUMBERZ,
  NUMBERQ,
  NUMBERR,
  NUMBERC,
  CHARACTER,
  STRING_EMPTY,
  STRING,
  VECTOR,
  BYTEVECTOR,
  QUOTE,
  LAMBDA,
  IF,
  SET,
  DEFINE,
  BEGIN_TYPE,
  PRIMITIVE_PROCEDURE,
  PROCEDURE,
  PRIMITIVE_PROCEDURE_APPLY,
  PRIMITIVE_PROCEDURE_CALL_WITH_CC,
  CONTINUATION,
  PRIMITIVE_PROCEDURE_RAISE,
  PRIMITIVE_PROCEDURE_RAISE_CONTINUABLE,
  IMPLEMENTATION_DEFINED_OBJECT,
  PORT_INPUT_TEXT,
  PORT_INPUT_BINARY,
  PORT_OUTPUT_TEXT,
  PORT_OUTPUT_BINARY,
  EOF_OBJ,
  FILE_ERROR,
  AND,
  OR,
  MULTIPLE,
  MULTIPLE_ZERO,
  NONE,
  UNSPECIFIED,
  WRONG_NUMBER_OF_ARGUMENTS,
  WRONG_TYPE_ARGUMENT,
  MOVED,
  EXPT_ERROR,
  EXACT_ERROR,
} Type;

#include <gmp.h>
#include <mpfr.h>
#include <mpc.h>
#include <glib.h>
typedef struct Object {
  Type type;
  union {
    void *cont;
    size_t index;
    char *identifier;
    mpz_t numberz;
    mpq_t numberq;
    mpfr_t numberr;
    mpc_t numberc;
    gunichar character;
    size_t vector_length;
    size_t bytevector_length;
    struct Object (*proc)(struct Object);
    FILE *port;
    char *message;
  };
} Object;

extern Object kread_obj;
extern Object const empty;
extern Object const none;
extern Object const unspecified;
extern Object const true_obj;
extern Object const false_obj;
extern Object const string_empty;

#include "memory.h"
#include "pair.h"
#include "identifier.h"
#include "numberz.h"
#include "numberq.h"
#include "numberr.h"
#include "numberc.h"
#include "character.h"

Object object_copy(Object obj);
void object_free(Object *obj_ptr);

void object_write(FILE *stream, Object obj);
void object_write_shared(FILE *stream, Object obj);
void object_write_simple(FILE *stream, Object obj);

void object_display(FILE *stream, Object obj);
