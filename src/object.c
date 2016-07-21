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
  case READ_ERROR:
  case FILE_ERROR: {
    Object out = {.type = obj.type, .message = strdup(obj.message)};
    return out;
  }
  default:
    return obj;
  }
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
  case READ_ERROR:
  case FILE_ERROR:
    free(obj_ptr->message);
    break;
  default:
    break;
  }
  *obj_ptr = none;
}
#include <stdbool.h>
static void error(char const *msg) {
  fprintf(stderr, "kscheme error: %s\n", msg);
  exit(1);
}
#include <ctype.h> // isprint
void object_write(FILE *stream, Object obj) {
  switch (obj.type) {
  case NUMBERZ:
    mpz_out_str(stream, 10, obj.numberz);
    break;
  case NUMBERQ:
    mpq_canonicalize(obj.numberq);
    mpq_out_str(stream, 10, obj.numberq);
    break;
  case NUMBERR: {
    if (mpfr_nan_p(obj.numberr)) {
      fprintf(stream, "+nan.0");
    } else if (mpfr_inf_p(obj.numberr)) {
      if (mpfr_sgn(obj.numberr) >= 0) {
        fprintf(stream, "+");
      } else {
        fprintf(stream, "-");
      }
      fprintf(stream, "inf.0");
    } else {
      mpfr_fprintf(stream, "%.16Rg", obj.numberr);
      mpfr_trunc(opfr, obj.numberr);
      mpfr_sub(opfr, obj.numberr, opfr, MPFR_RNDN);
      if (mpfr_zero_p(opfr)) {
        fprintf(stream, ".0");
      }
    }
    break;
  }
  case NUMBERC: {
    if (mpfr_nan_p(mpc_realref(obj.numberc))) {
      fprintf(stream, "+nan.0");
    } else if (mpfr_inf_p(mpc_realref(obj.numberc))) {
      if (mpfr_sgn(mpc_realref(obj.numberc)) >= 0) {
        fprintf(stream, "+");
      } else {
        fprintf(stream, "-");
      }
      fprintf(stream, "inf.0");
    } else {
      mpfr_fprintf(stream, "%.16Rg", mpc_realref(obj.numberc));
      mpfr_trunc(opfr, mpc_realref(obj.numberc));
      mpfr_sub(opfr, mpc_realref(obj.numberc), opfr, MPFR_RNDN);
      if (mpfr_zero_p(opfr)) {
        fprintf(stream, ".0");
      }
    }
    if (mpfr_zero_p(mpc_imagref(obj.numberc))) {
      break;
    }
    if (mpfr_nan_p(mpc_imagref(obj.numberc))) {
      fprintf(stream, "+nan.0");
    } else {
      if (mpfr_sgn(mpc_imagref(obj.numberc)) >= 0) {
        fprintf(stream, "+");
      }
      mpfr_fprintf(stream, "%.16Rg", mpc_imagref(obj.numberc));
      if (mpfr_inf_p(mpc_imagref(obj.numberc))) {
        fprintf(stream, ".0");
      } else {
        mpfr_trunc(opfr, mpc_imagref(obj.numberc));
        mpfr_sub(opfr, mpc_imagref(obj.numberc), opfr, MPFR_RNDN);
        if (mpfr_zero_p(opfr)) {
          fprintf(stream, ".0");
        }
      }
    }
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
      if (!g_unichar_isprint(obj.character)) {
        fprintf(stream, "x%x", obj.character);
        break;
      }
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
        if (!g_unichar_isprint(c)) {
          fprintf(stream, "\\x%x;", c);
          break;
        }
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
  case STRING_IMMUTABLE: {
    fprintf(stream, "\"");
    for (size_t i = 0; obj.string_immutable[i] != '\0'; i++) {
      switch (obj.string_immutable[i]) {
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
      default:
        fprintf(stream, "%c", obj.string_immutable[i]);
      }
    }
    fprintf(stream, "\"");
    break;
  }
  case STRING_IMMUTABLE_VERTICAL: {
    fprintf(stream, "\"");
    char *p = obj.string_immutable_vertical;
    for (gunichar c = g_utf8_get_char(p); c != '\0'; c = g_utf8_get_char(p)) {
      p = g_utf8_next_char(p);
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
      default:
        if (g_unichar_isprint(c)) {
          char outbuf[6];
          gint len = g_unichar_to_utf8(c, outbuf);
          outbuf[len] = '\0';
          fprintf(stream, "%s", outbuf);
        } else {
          fprintf(stream, "\\x%x;", c);
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
  case IDENTIFIER_VERTICAL: {
    fprintf(stream, "|");
    char *p = obj.identifier_vertical;
    for (gunichar c = g_utf8_get_char(p); c != '\0'; c = g_utf8_get_char(p)) {
      p = g_utf8_next_char(p);
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
      default:
        if (g_unichar_isprint(c)) {
          char outbuf[6];
          gint len = g_unichar_to_utf8(c, outbuf);
          outbuf[len] = '\0';
          fprintf(stream, "%s", outbuf);
        } else {
          fprintf(stream, "\\x%x;", c);
        }
      }
    }
    fprintf(stream, "|");
    break;
  }
  case EMPTY:
    fprintf(stream, "()");
    break;
  case PAIR: {
    if (env.index == obj.index) {
      fprintf(stream, "#<environment interaction-environment>");
    } else {
      pair_write(stream, obj);
    }
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
  case DELAY:
    fprintf(stream, "#<syntax delay>");
    break;
  case DELAY_FORCE:
    fprintf(stream, "#<syntax delay-force>");
    break;
  case PROMISE: {
    fprintf(stream, "#<promise ");
    if (carref(obj).type == TRUE_TYPE) {
      fprintf(stream, "(forced)>");
    } else {
      object_write(stream, carref(cdrref(obj)));
      /* object_write(stream, cdrref(obj)); */
      fprintf(stream, ">");
    }
    break;
  }
  case PRIMITIVE_PROCEDURE:
  case PRIMITIVE_PROCEDURE_MAKE_PROMISE:
  case PRIMITIVE_PROCEDURE_FORCE:
  case PRIMITIVE_PROCEDURE_APPLY:
  case PRIMITIVE_PROCEDURE_EVAL:
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
    for (Object t = obj; t.type != EMPTY; t = cdrref(t)) {
      object_display(stream, carref(t));
      fprintf(stream, " ");
    }
    break;
  }
  case PORT_INPUT_TEXT: {
    fprintf(stream, "#<textual-input-port");
    FILE *f = carref(obj).port;
    if (f == NULL) {
      fprintf(stream, "(closed)");
    }
    if (f == stdin) {
      fprintf(stream, " (standard input)>");
    } else {
      fprintf(stream, " ");
      object_write(stream, cdrref(obj));
      fprintf(stream, ">");
    }
    break;
  }
  case PORT_INPUT_BINARY: {
    fprintf(stream, "#<binary-input-port");
    if (carref(obj).port == NULL) {
      fprintf(stream, "(closed)");
    }
    fprintf(stream, " ");
    object_write(stream, cdrref(obj));
    fprintf(stream, ">");
    break;
  }
  case PORT_OUTPUT_TEXT: {
    fprintf(stream, "#<textual-output-port");
    FILE *f = carref(obj).port;
    if (f == NULL) {
      fprintf(stream, "(closed)");
    }
    if (f == stdout) {
      fprintf(stream, " (standard output)>");
    } else if (f == stderr) {
      fprintf(stream, " (standard error)>");
    } else {
      fprintf(stream, " ");
      object_write(stream, cdrref(obj));
      fprintf(stream, ">");
    }
    break;
  }
  case PORT_OUTPUT_BINARY: {
    fprintf(stream, "#<binary-output-port");
    if (carref(obj).port == NULL) {
      fprintf(stream, "(closed)");
    }
    fprintf(stream, " ");
    object_write(stream, cdrref(obj));
    fprintf(stream, ">");
    break;
  }
  case PORT_INPUT_TEXT_STRING: {
    fprintf(stream, "#<textual-input-port");
    if (carref(obj).port == NULL) {
      fprintf(stream, "(closed)");
    }
    fprintf(stream, "(string)>");
    break;
  }
  case PORT_OUTPUT_TEXT_STRING: {
    fprintf(stream, "#<textual-output-port");
    if (carref(obj).port == NULL) {
      fprintf(stream, "(closed)");
    }
    fprintf(stream, "(string)");
    fprintf(stream, ">");
    break;
  }
  case PORT_INPUT_BINARY_BYTEVECTOR: {
    fprintf(stream, "#<binary-input-port");
    if (carref(obj).port == NULL) {
      fprintf(stream, "(closed)");
    }
    fprintf(stream, "(bytevector)");
    fprintf(stream, ">");
    break;
  }
  case PORT_OUTPUT_BINARY_BYTEVECTOR: {
    fprintf(stream, "#<binary-output-port");
    if (carref(obj).port == NULL) {
      fprintf(stream, "(closed)");
    }
    fprintf(stream, "(bytevector)");
    fprintf(stream, ">");
    break;
  }
  case EOF_OBJ:
    fprintf(stream, "#<eof>");
    break;
  case READ_ERROR:
    fprintf(stream, "#<read-error: %s>", obj.message);
    break;
  case FILE_ERROR:
    fprintf(stream, "#<file-error: %s>", obj.message);
    break;
  case UNSPECIFIED:
    fprintf(stream, "#<unspecified>");
    break;
  case MULTIPLE: {
    bool flag = false;
    for (Object o = obj; o.type != EMPTY; o = cdrref(o)) {
      if (flag) {
        fprintf(stream, " ");
      } else {
        flag = true;
      }
      object_write(stream, carref(o));
    }
    break;
  }
  case MULTIPLE_ZERO:
    break;
  case NONE:
    error("object_write");
  default:
    break;
  }
}
void object_write_shared(FILE *stream, Object obj) {
  switch (obj.type) {
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
  case NONE:
    error("object_write_shared");
  default:
    object_write(stream, obj);
    break;
  }
}

void object_write_simple(FILE *stream, Object obj) {
  switch (obj.type) {
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
  case NONE:
    error("object_write_simple");
  default:
    object_write(stream, obj);
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
  case STRING_IMMUTABLE: {
    fprintf(stream, "%s", obj.string_immutable);
    break;
  }
  case STRING_IMMUTABLE_VERTICAL: {
    for (size_t i = 1; i != '|'; i++) {
      fprintf(stream, "%c", obj.string_immutable_vertical[i]);
    }
    break;
  }
  case NONE:
    error("object_display");
  default:
    object_write(stream, obj);
    break;
  }
}

Object value(Object const obj) {
  if (obj.type != MULTIPLE) {
    return obj;
  }
  Object o;
  for (o = carref(obj); o.type == MULTIPLE; o = carref(o)) {
    ;
  }
  return o;
}
