#include "object.h"

Object kread_obj;
Object const empty = {.type = EMPTY};
Object const none = {.type = NONE};
Object const unspecified = {.type = UNSPECIFIED};
Object const true_obj = {.type = TRUE_TYPE};
Object const false_obj = {.type = FALSE_TYPE};
Object const string_empty = {.type = STRING_EMPTY};

#include <string.h> // strdup
Object object_copy(Object obj) {
  switch (obj.type) {
  case NUMBERZ:
    return numberz_copy(obj);
  case NUMBERQ: {
    Object out = {.type = NUMBERQ};
    mpq_init(out.numberq);
    mpq_set(out.numberq, obj.numberq);
    return out;
  }
  case NUMBERR: {
    Object out = {.type = NUMBERR};
    mpfr_init_set(out.numberr, obj.numberr, MPFR_RNDN);
    return out;
  }
  case NUMBERC: {
    Object out = {.type = NUMBERC};
    mpc_init2(out.numberc, MPC_PREC);
    mpc_set(out.numberc, obj.numberc, MPC_RNDNN);
    return out;
  }
  case FILE_ERROR: {
    Object out = {.type = obj.type, .message = strdup(obj.message)};
    return out;
  }
  case CHARACTER:
  case STRING_EMPTY:
  case STRING:
  case VECTOR:
  case BYTEVECTOR:
  case EMPTY:
  case PAIR:
  case IDENTIFIER:
  case TRUE_TYPE:
  case FALSE_TYPE:
  case QUOTE:
  case LAMBDA:
  case IF:
  case SET:
  case DEFINE:
  case BEGIN_TYPE:
  case AND:
  case OR:
  case PRIMITIVE_PROCEDURE:
  case PRIMITIVE_PROCEDURE_APPLY:
  case PRIMITIVE_PROCEDURE_CALL_WITH_CC:
  case CONTINUATION:
  case PRIMITIVE_PROCEDURE_RAISE:
  case PRIMITIVE_PROCEDURE_RAISE_CONTINUABLE:

  case PROCEDURE:
  case IMPLEMENTATION_DEFINED_OBJECT:
  case PORT_INPUT_TEXT:
  case PORT_INPUT_BINARY:
  case PORT_OUTPUT_TEXT:
  case PORT_OUTPUT_BINARY:

  case EOF_OBJ:
  case UNSPECIFIED:
    return obj;

  case NONE:
    return obj;
  }
  return none;
}
void object_free(Object *obj_ptr) {
  switch (obj_ptr->type) {
  case NUMBERZ:
    numberz_free(obj_ptr->numberz);
    break;
  case NUMBERQ:
    mpq_clear(obj_ptr->numberq);
    break;
  case NUMBERR:
    mpfr_clear(obj_ptr->numberr);
    break;
  case NUMBERC:
    mpc_clear(obj_ptr->numberc);
    break;
  case FILE_ERROR:
    free(obj_ptr->message);
    break;
  case PORT_INPUT_TEXT:
  case PORT_OUTPUT_TEXT:
  case PORT_INPUT_BINARY:
  case PORT_OUTPUT_BINARY:
  case CHARACTER:
  case STRING_EMPTY:
  case STRING:
  case VECTOR:
  case BYTEVECTOR:
  case EMPTY:
  case PAIR:
  case IDENTIFIER:
  case TRUE_TYPE:
  case FALSE_TYPE:
  case QUOTE:
  case LAMBDA:
  case IF:
  case SET:
  case DEFINE:
  case BEGIN_TYPE:
  case AND:
  case OR:
  case PRIMITIVE_PROCEDURE:
  case PRIMITIVE_PROCEDURE_APPLY:
  case PRIMITIVE_PROCEDURE_CALL_WITH_CC:
  case CONTINUATION:
  case PRIMITIVE_PROCEDURE_RAISE:
  case PRIMITIVE_PROCEDURE_RAISE_CONTINUABLE:

  case PROCEDURE:
  case EOF_OBJ:
  case IMPLEMENTATION_DEFINED_OBJECT:
  case UNSPECIFIED:
    break;
  case NONE: {
    /* fprintf(stderr, "ksi error object_free NONE\n"); */
    /* exit(1); */
    break;
  }
  }
  obj_ptr->type = NONE;
}
#include <stdbool.h>
void object_write(FILE *stream, Object obj) {
  switch (obj.type) {
  case NUMBERZ:
    mpz_out_str(stream, 10, obj.numberz);
    break;
  case NUMBERQ:
    mpq_out_str(stream, 10, obj.numberq);
    break;
  case NUMBERR: {
    if (mpfr_zero_p(obj.numberr)) {
      fprintf(stream, "0.0");
    } else {
      mpfr_out_str(stream, 10, 0, obj.numberr, MPFR_RNDN);
    }
    break;
  }
  case NUMBERC: {
    if (mpfr_zero_p(mpc_realref(obj.numberc))) {
      fprintf(stream, "0.0");
    } else {
      mpfr_out_str(stream, 10, 0, mpc_realref(obj.numberc), MPFR_RNDN);
    }
    if (mpfr_zero_p(mpc_imagref(obj.numberc))) {
      break;
    }
    if (mpfr_sgn(mpc_imagref(obj.numberc)) >= 0) {
      fprintf(stream, "+");
    }
    mpfr_out_str(stream, 10, 0, mpc_imagref(obj.numberc), MPFR_RNDN);
    fprintf(stream, "i");
    break;
  }
  case CHARACTER: {
    fprintf(stream, "#\\");
    switch (obj.character) {
    case 0x7:
      fprintf(stream, "alarm");
      break;
    case 0x8:
      fprintf(stream, "backspace");
      break;
    case 0x7f:
      fprintf(stream, "delete");
      break;
    case 0x1b:
      fprintf(stream, "escape");
      break;
    case 0xa:
      fprintf(stream, "newline");
      break;
    case 0x0:
      fprintf(stream, "null");
      break;
    case 0xd:
      fprintf(stream, "return");
      break;
    case ' ':
      fprintf(stream, "space");
      break;
    case 0x9:
      fprintf(stream, "tab");
      break;
    default: {
      char outbuf[7];
      gint len = g_unichar_to_utf8(obj.character, outbuf);
      outbuf[len] = '\0';
      fprintf(stream, "%s", outbuf);
      break;
    }
    }
    break;
  }
  case STRING_EMPTY:
    fprintf(stream, "\"\"");
    break;
  case STRING: {
    fprintf(stream, "\"");
    for (Object s = obj; s.type != STRING_EMPTY; s = string_cdrref(s)) {
      gunichar c = string_carref(s).character;
      switch (c) {
      case 0:
        fprintf(stream, "\\x0000;");
        break;
      case 7:
        fprintf(stream, "\\a");
        break;
      case 8:
        fprintf(stream, "\\b");
        break;
      case 0xa:
        fprintf(stream, "\\n");
        break;
      case 0xd:
        fprintf(stream, "\\r");
        break;
      case 0x22:
        fprintf(stream, "\\\"");
        break;
      case 0x5c:
        fprintf(stream, "\\\\");
        break;
      case 0x7c:
        fprintf(stream, "\\|");
        break;
      default: {
        char outbuf[7];
        gint len = g_unichar_to_utf8(c, outbuf);
        outbuf[len] = '\0';
        fprintf(stream, "%s", outbuf);
        break;
      }
      }
    }
    fprintf(stream, "\"");
    break;
  }
  case VECTOR: {
    fprintf(stream, "#(");
    size_t len = cdrs[obj.index].vector_length;
    for (size_t i = obj.index; i < len + obj.index; i++) {
      if (i != obj.index) {
        fprintf(stream, " ");
      }
      object_write(stream, cars[i]);
    }
    fprintf(stream, ")");
    break;
  }
  case BYTEVECTOR: {
    fprintf(stream, "#u8(");
    size_t len = cdrs[obj.index].bytevector_length;
    for (size_t i = obj.index; i < len + obj.index; i++) {
      if (i != obj.index) {
        fprintf(stream, " ");
      }
      mpz_out_str(stream, 10, cars[i].numberz);
      /* gmp_fprintf(stream, "#x%ZX", cars[i].numberz); */
    }
    fprintf(stream, ")");
    break;
  }
  case IDENTIFIER:
    fprintf(stream, "%s", obj.identifier);
    break;
  case EMPTY:
    fprintf(stream, "()");
    break;
  case PAIR: {
    pair_write(stream, obj);
    break;
  }
  case TRUE_TYPE:
    fprintf(stream, "#t");
    break;
  case FALSE_TYPE:
    fprintf(stream, "#f");
    break;
  case QUOTE:
    fprintf(stream, "#<syntax quote>");
    break;
  case LAMBDA:
    fprintf(stream, "#<syntax lambda>");
    break;
  case IF:
    fprintf(stream, "#<syntax if>");
    break;
  case SET:
    fprintf(stream, "#<syntax set!>");
    break;
  case DEFINE:
    fprintf(stream, "#<syntax define>");
    break;
  case BEGIN_TYPE:
    fprintf(stream, "#<syntax begin>");
    break;
  case AND:
    fprintf(stream, "#<syntax and>");
    break;
  case OR:
    fprintf(stream, "#<syntax or>");
    break;
  case PRIMITIVE_PROCEDURE:
  case PRIMITIVE_PROCEDURE_APPLY:
  case PRIMITIVE_PROCEDURE_CALL_WITH_CC:
  case PRIMITIVE_PROCEDURE_RAISE:
  case PRIMITIVE_PROCEDURE_RAISE_CONTINUABLE:
    fprintf(stream, "#<primitive-procedure>");
    break;
  case CONTINUATION: {
    fprintf(stream, "#<continuation>");
    break;
  }
  case PROCEDURE:
    fprintf(stream, "#<compound-procedure ");
    object_write(stream, carref(cdrref(obj)));
    fprintf(stream, " ");
    object_write(stream, cdrref(cdrref(obj)));
    fprintf(stream, ">");
    break;
  case IMPLEMENTATION_DEFINED_OBJECT: {
    fprintf(stream, "#<error ");
    object_write(stream, carref(obj));
    fprintf(stream, " ");
    object_write(stream, cdrref(obj));
    fprintf(stream, ">");
    break;
  }
  case PORT_INPUT_TEXT: {
    fprintf(stream, "#<text-input-port");
    if (port_carref(obj).port == NULL) {
      fprintf(stream, "(closed)");
    }
    fprintf(stream, " ");
    object_write(stream, port_cdrref(obj));
    fprintf(stream, ">");
    break;
  }
  case PORT_INPUT_BINARY: {
    fprintf(stream, "#<binary-input-port ");
    if (port_carref(obj).port == NULL) {
      fprintf(stream, "(closed) ");
    }
    fprintf(stream, " ");
    object_write(stream, port_cdrref(obj));
    fprintf(stream, ">");
    break;
  }
  case PORT_OUTPUT_TEXT: {
    fprintf(stream, "#<text-output-port ");
    if (port_carref(obj).port == NULL) {
      fprintf(stream, "(closed) ");
    }
    fprintf(stream, " ");
    object_write(stream, port_cdrref(obj));
    fprintf(stream, ">");
    break;
  }
  case PORT_OUTPUT_BINARY: {
    fprintf(stream, "#<binary-output-port ");
    if (port_carref(obj).port == NULL) {
      fprintf(stream, "(closed) ");
    }
    fprintf(stream, " ");
    object_write(stream, port_cdrref(obj));
    fprintf(stream, ">");
    break;
  }
  case EOF_OBJ:
    fprintf(stream, "#<eof>");
    break;
  case FILE_ERROR:
    fprintf(stream, "#<file-error: %s>", obj.message);
    break;
  case UNSPECIFIED:
    fprintf(stream, "#<unspecified>");
    break;
  case NONE: {
    fprintf(stderr, "ksi error ksi object_write NONE VECTOR_NULL\n");
    exit(1);
    break;
  }
  }
}
void object_write_shared(FILE *stream, Object obj) {
  switch (obj.type) {
  case NUMBERZ:
  case NUMBERQ:
  case NUMBERR:
  case NUMBERC:
  case CHARACTER:
  case STRING_EMPTY:
  case STRING:
  case IDENTIFIER:
  case EMPTY:
  case TRUE_TYPE:
  case FALSE_TYPE:
  case QUOTE:
  case LAMBDA:
  case IF:
  case SET:
  case DEFINE:
  case BEGIN_TYPE:
  case AND:
  case OR:
  case PRIMITIVE_PROCEDURE:
  case PRIMITIVE_PROCEDURE_APPLY:
  case PRIMITIVE_PROCEDURE_CALL_WITH_CC:
  case PRIMITIVE_PROCEDURE_RAISE:
  case PRIMITIVE_PROCEDURE_RAISE_CONTINUABLE:
  case CONTINUATION:
  case PROCEDURE:
  case IMPLEMENTATION_DEFINED_OBJECT:
  case PORT_INPUT_TEXT:
  case PORT_INPUT_BINARY:
  case PORT_OUTPUT_TEXT:
  case PORT_OUTPUT_BINARY:
  case EOF_OBJ:
  case FILE_ERROR:
  case UNSPECIFIED:
  case NONE:
    object_write(stream, obj);
    break;
  case VECTOR: {
    fprintf(stream, "#(");
    size_t len = cdrs[obj.index].vector_length;
    for (size_t i = obj.index; i < len + obj.index; i++) {
      if (i != obj.index) {
        fprintf(stream, " ");
      }
      object_write(stream, cars[i]);
    }
    fprintf(stream, ")");
    break;
  }
  case BYTEVECTOR: {
    fprintf(stream, "#u8(");
    size_t len = cdrs[obj.index].bytevector_length;
    for (size_t i = obj.index; i < len + obj.index; i++) {
      if (i != obj.index) {
        fprintf(stream, " ");
      }
      mpz_out_str(stream, 10, cars[i].numberz);
      /* gmp_fprintf(stream, "#x%ZX", cars[i].numberz); */
    }
    fprintf(stream, ")");
    break;
  }
  case PAIR:
    pair_write_shared(stream, obj);
    break;
  }
}

void object_write_simple(FILE *stream, Object obj) {
  switch (obj.type) {
  case NUMBERZ:
  case NUMBERQ:
  case NUMBERR:
  case NUMBERC:
  case CHARACTER:
  case STRING_EMPTY:
  case STRING:
  case IDENTIFIER:
  case EMPTY:
  case TRUE_TYPE:
  case FALSE_TYPE:
  case QUOTE:
  case LAMBDA:
  case IF:
  case SET:
  case DEFINE:
  case BEGIN_TYPE:
  case AND:
  case OR:
  case PRIMITIVE_PROCEDURE:
  case PRIMITIVE_PROCEDURE_APPLY:
  case PRIMITIVE_PROCEDURE_CALL_WITH_CC:
  case PRIMITIVE_PROCEDURE_RAISE:
  case PRIMITIVE_PROCEDURE_RAISE_CONTINUABLE:
  case CONTINUATION:
  case PROCEDURE:
  case IMPLEMENTATION_DEFINED_OBJECT:
  case PORT_INPUT_TEXT:
  case PORT_INPUT_BINARY:
  case PORT_OUTPUT_TEXT:
  case PORT_OUTPUT_BINARY:
  case EOF_OBJ:
  case FILE_ERROR:
  case UNSPECIFIED:
  case NONE:
    object_write(stream, obj);
    break;
  case VECTOR: {
    fprintf(stream, "#(");
    size_t len = cdrs[obj.index].vector_length;
    for (size_t i = obj.index; i < len + obj.index; i++) {
      if (i != obj.index) {
        fprintf(stream, " ");
      }
      object_write(stream, cars[i]);
    }
    fprintf(stream, ")");
    break;
  }
  case BYTEVECTOR: {
    fprintf(stream, "#u8(");
    size_t len = cdrs[obj.index].bytevector_length;
    for (size_t i = obj.index; i < len + obj.index; i++) {
      if (i != obj.index) {
        fprintf(stream, " ");
      }
      mpz_out_str(stream, 10, cars[i].numberz);
      /* gmp_fprintf(stream, "#x%ZX", cars[i].numberz); */
    }
    fprintf(stream, ")");
    break;
  }
  case PAIR:
    pair_write_simple(stream, obj);
    break;
  }
}

void object_display(FILE *stream, Object obj) {
  switch (obj.type) {
  case CHARACTER: {
    switch (obj.character) {
    case 0x7:
    case 0x8:
    case 0x7f:
    case 0x1b:
    case 0xa:
    case 0x0:
    case 0xd:
    case ' ':
    case 0x9:
      fprintf(stream, "%c", obj.character);
      break;
    default: {
      char outbuf[7];
      gint len = g_unichar_to_utf8(obj.character, outbuf);
      outbuf[len] = '\0';
      fprintf(stream, "%s", outbuf);
      break;
    }
    }
    break;
  }
  case STRING_EMPTY:
    break;
  case STRING: {
    for (Object s = obj; s.type != STRING_EMPTY; s = string_cdrref(s)) {
      Object c = string_carref(s);
      object_display(stream, c);
    }
    break;
  }
  case NONE: {
    fprintf(stderr, "ksi error ksi object_write NONE VECTOR_NULL\n");
    exit(1);
    break;
  }
  default: { object_write(stream, obj); }
  }
}
