/** \file  */
#include "memory.h"

Object memory_from[2][MEMORY_SIZE];
Object memory_to[2][MEMORY_SIZE];
Object *cars;
Object *cdrs;
Object *new_cars;
Object *new_cdrs;

static size_t free_index = 0;
static Object oldcr = {.type = NONE};
static Object new_obj = {.type = NONE};

Object root;
Object expr, env, val, cont, proc, argl, unev;
Object stack;
Object global;

static void gc_before(Object obj) {
  puts("GC");
  root = empty;
  root = cons(unev, root);
  root = cons(argl, root);
  root = cons(proc, root);
  root = cons(cont, root);
  root = cons(val, root);
  root = cons(env, root);
  root = cons(expr, root);
  root = cons(stack, root);
  root = cons(global, root);
  root = cons(obj, root);
  puts("gc_before");
}
static void gc_after(Object *ptr) {
  *ptr = car(root);
  root = cdrref(root);
  global = carref(root);
  root = cdrref(root);
  stack = carref(root);
  root = cdrref(root);
  expr = car(root);
  root = cdrref(root);
  env = carref(root);
  root = cdrref(root);
  val = car(root);
  root = cdrref(root);
  cont = carref(root);
  root = cdrref(root);
  proc = carref(root);
  root = cdrref(root);
  argl = carref(root);
  root = cdrref(root);
  unev = car(root);
  puts("gc_after");
}

void gc() {
  free_index = 0;
  size_t scan_index = 0;
  Object old_obj = root;
  void *relocate_continue = &&reassign_root;
  goto relocate_old_result_in_new;
reassign_root:
  root = new_obj;
  goto gc_loop;
gc_loop:
  if (scan_index == free_index) {
    goto gc_flip;
  }
  old_obj = new_cars[scan_index];
  relocate_continue = &&update_car;
  goto relocate_old_result_in_new;
update_car:
  new_cars[scan_index] = new_obj;
  old_obj = new_cdrs[scan_index];
  relocate_continue = &&update_cdr;
  goto relocate_old_result_in_new;
update_cdr:
  new_cdrs[scan_index] = new_obj;
  scan_index++;
  goto gc_loop;
relocate_old_result_in_new:
  switch (old_obj.type) {
  case PAIR:
  case STRING:
  case PROCEDURE:
  case CONTINUATION:
  case IMPLEMENTATION_DEFINED_OBJECT:
  case PORT_INPUT_TEXT:
  case PORT_INPUT_BINARY:
  case PORT_OUTPUT_TEXT:
  case PORT_OUTPUT_BINARY:
  case MULTIPLE: {
    goto pair;
  }
  case VECTOR:
  case BYTEVECTOR: {
  }
  default: {
    new_obj = old_obj;
    goto *relocate_continue;
  }
  }
pair:
  oldcr = cars[old_obj.index];
  if (oldcr.type == MOVED) {
    goto already_moved;
  }
  cars[old_obj.index].type = MOVED;
  new_obj = (Object){.type = old_obj.type, .index = free_index};
  free_index++;
  object_free(&new_cars[new_obj.index]);
  new_cars[new_obj.index] = oldcr;
  object_free(&new_cdrs[new_obj.index]);
  new_cdrs[new_obj.index] = cdrs[old_obj.index];
  cdrs[old_obj.index] = new_obj;
  goto *relocate_continue;

already_moved:
  new_obj = cdrs[old_obj.index];
  goto *relocate_continue;

gc_flip:;
  if (free_index == (MEMORY_SIZE - REGISTER_COUNT)) {
    fprintf(stderr, "Insufficient memory.\n");
    exit(1);
  }
  Object *temp = cdrs;
  cdrs = new_cdrs;
  new_cdrs = temp;
  temp = cars;
  cars = new_cars;
  new_cars = temp;
}
Object cons(Object obj1, Object obj2) {
  Object out = {.type = PAIR, .index = free_index};
  object_free(&cars[free_index]);
  object_free(&cdrs[free_index]);
  cars[free_index] = obj1;
  cdrs[free_index] = obj2;
  free_index++;
  if (free_index == MEMORY_SIZE - REGISTER_COUNT) {
    gc_before(out);
    gc();
    gc_after(&out);
  }
  return out;
}
Object car(Object obj) {
  switch (obj.type) {
  case PAIR:
  case STRING:
  case PROCEDURE:
  case CONTINUATION:
  case IMPLEMENTATION_DEFINED_OBJECT:
  case PORT_INPUT_TEXT:
  case PORT_INPUT_BINARY:
  case PORT_OUTPUT_TEXT:
  case PORT_OUTPUT_BINARY:
  case MULTIPLE: {
    return object_copy(cars[obj.index]);
  }
  default:
    fprintf(stderr, "kscheme error: memory car -- %d\n", obj.type);
    exit(1);
  }
}
Object cdr(Object obj) {
  switch (obj.type) {
  case PAIR:
  case STRING:
  case PROCEDURE:
  case CONTINUATION:
  case IMPLEMENTATION_DEFINED_OBJECT:
  case PORT_INPUT_TEXT:
  case PORT_INPUT_BINARY:
  case PORT_OUTPUT_TEXT:
  case PORT_OUTPUT_BINARY:
  case MULTIPLE: {
    return object_copy(cdrs[obj.index]);
  }
  default:
    fprintf(stderr, "kscheme error: memory cdr -- %d\n", obj.type);
    exit(1);
  }
}
Object carref(Object obj) {
  switch (obj.type) {
  case PAIR:
  case STRING:
  case PROCEDURE:
  case CONTINUATION:
  case IMPLEMENTATION_DEFINED_OBJECT:
  case PORT_INPUT_TEXT:
  case PORT_INPUT_BINARY:
  case PORT_OUTPUT_TEXT:
  case PORT_OUTPUT_BINARY:
  case MULTIPLE: {
    return cars[obj.index];
  }
  default:
    fprintf(stderr, "kscheme error: memory carref -- type(%d)", obj.type);
    exit(1);
  }
}
Object cdrref(Object obj) {
  switch (obj.type) {
  case PAIR:
  case STRING:
  case PROCEDURE:
  case CONTINUATION:
  case IMPLEMENTATION_DEFINED_OBJECT:
  case PORT_INPUT_TEXT:
  case PORT_INPUT_BINARY:
  case PORT_OUTPUT_TEXT:
  case PORT_OUTPUT_BINARY:
  case MULTIPLE: {
    return cdrs[obj.index];
  }
  default:
    fprintf(stderr, "kscheme error: memory carref -- type(%d)", obj.type);
    exit(1);
  }
}
void save(Object obj) { stack = cons(obj, stack); }
void restore(Object *ptr) {
  *ptr = car(stack);
  stack = cdrref(stack);
}

Object string_cons(Object obj1, Object obj2) {
  Object out = {.type = STRING, .index = free_index};
  object_free(&cars[free_index]);
  object_free(&cdrs[free_index]);
  cars[free_index] = obj1;
  cdrs[free_index] = obj2;
  free_index++;
  if (free_index == MEMORY_SIZE - REGISTER_COUNT) {
    gc_before(out);
    gc();
    gc_after(&out);
  }
  return out;
}

Object string_carref(Object obj) { return cars[obj.index]; }
Object string_cdrref(Object obj) { return cdrs[obj.index]; }

Object list2vector(Object obj) {
  size_t len = 0;
  for (Object t = obj; t.type != EMPTY; t = cdrref(t), len++) {
    ;
  }
  Object out = empty;
  if (free_index + len - 1 >= MEMORY_SIZE) {
    gc_before(out);
    gc();
    gc_after(&out);
    if (free_index + len - 1 >= MEMORY_SIZE) {
      fprintf(stderr, "Insufficient memory.\n");
      exit(1);
    }
  }
  out = (Object){.type = VECTOR, .index = free_index};
  object_free(&cdrs[free_index]);
  cdrs[free_index] = (Object){.vector_length = len};
  for (Object t = obj; t.type != EMPTY; t = cdrref(t), free_index++) {
    object_free(&cars[free_index]);
    cars[free_index] = car(t);
  }
  if (free_index == MEMORY_SIZE - REGISTER_COUNT) {
    gc_before(out);
    gc();
    gc_after(&out);
  }
  return out;
}

Object list2bytevector(Object obj) {
  size_t len = 0;
  for (Object t = obj; t.type != EMPTY; t = cdrref(t), len++) {
    ;
  }
  Object out = empty;
  if (free_index + len - 1 >= MEMORY_SIZE) {
    gc_before(out);
    gc();
    gc_after(&out);
    if (free_index + len - 1 >= MEMORY_SIZE) {
      fprintf(stderr, "Insufficient memory.\n");
      exit(1);
    }
  }
  out = (Object){.type = BYTEVECTOR, .index = free_index};
  object_free(&cdrs[free_index]);
  cdrs[free_index] = (Object){.bytevector_length = len};
  for (Object t = obj; t.type != EMPTY; t = cdrref(t), free_index++) {
    object_free(&cars[free_index]);
    cars[free_index] = car(t);
  }
  if (free_index == MEMORY_SIZE - REGISTER_COUNT) {
    gc_before(out);
    gc();
    gc_after(&out);
  }
  return out;
}

Object continuation_cons(Object obj1, Object obj2) {
  Object out = {.type = CONTINUATION, .index = free_index};
  object_free(&cars[free_index]);
  object_free(&cdrs[free_index]);
  cars[free_index] = obj1;
  cdrs[free_index] = obj2;
  free_index++;
  if (free_index == MEMORY_SIZE - REGISTER_COUNT) {
    gc_before(out);
    gc();
    gc_after(&out);
  }
  return out;
}
Object continuation_carref(Object obj) { return cars[obj.index]; }
Object continuation_cdrref(Object obj) { return cdrs[obj.index]; }
Object implementation_defined_object_carref(Object obj) {
  return cars[obj.index];
}
Object implementation_defined_object_cdrref(Object obj) {
  return cdrs[obj.index];
}
Object port_carref(Object obj) { return cars[obj.index]; }
Object port_cdrref(Object obj) { return cdrs[obj.index]; }
