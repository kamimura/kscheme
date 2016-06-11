/** \file  */
#include "memory.h"

Object memory_from[2][MEMORY_SIZE];
Object memory_to[2][MEMORY_SIZE];
Object *cars;
Object *cdrs;
Object *new_cars;
Object *new_cdrs;

size_t free_index = 0;
void gc() {
  puts("gc start");
  exit(0);
  free_index = 0;
  size_t scan_index = 0;
  Object old;

  void *relocate_continue = &&reassign_root;

  goto relocate_old_result_in_new;
reassign_root:

  goto gc_loop;
gc_loop:
  if (scan_index == free_index) {
    goto gc_flip;
  }

  relocate_continue = &&update_car;
  goto relocate_old_result_in_new;

update_car:

  relocate_continue = &&update_cdr;
  goto relocate_old_result_in_new;

update_cdr:

  scan_index++;
  goto gc_loop;

relocate_old_result_in_new:
  if (old.type == PAIR || old.type == PROCEDURE)
    goto pair;
  goto *relocate_continue;
pair:

  if (1)
    goto already_moved;
  goto *relocate_continue;

already_moved:
  goto *relocate_continue;

gc_flip:;
}
Object cons(Object obj1, Object obj2) {
  Object out = {.type = PAIR, .index = free_index};
  object_free(&cars[free_index]);
  object_free(&cdrs[free_index]);
  cars[free_index] = obj1;
  cdrs[free_index] = obj2;
  free_index++;
  if (free_index == MEMORY_SIZE) {
    gc();
  }
  return out;
}
Object car(Object obj) { return object_copy(cars[obj.index]); }
Object cdr(Object obj) { return object_copy(cdrs[obj.index]); }
Object carref(Object obj) { return cars[obj.index]; }
Object cdrref(Object obj) { return cdrs[obj.index]; }

void assign(Object *ptr1, Object *ptr2) {
  object_free(ptr1);
  *ptr1 = *ptr2;
  ptr2->type = NONE;
}

Object string_cons(Object obj1, Object obj2) {
  Object out = {.type = STRING, .index = free_index};
  object_free(&cars[free_index]);
  object_free(&cdrs[free_index]);
  cars[free_index] = obj1;
  cdrs[free_index] = obj2;
  free_index++;
  if (free_index == MEMORY_SIZE) {
    gc();
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
  if (free_index + len - 1 >= MEMORY_SIZE) {
    gc();
    if (free_index + len - 1 >= MEMORY_SIZE) {
      fprintf(stderr, "Insufficient memory.\n");
      exit(1);
    }
  }
  Object out = {.type = VECTOR, .index = free_index};
  object_free(&cdrs[free_index]);
  cdrs[free_index] = (Object){.vector_length = len};
  for (Object t = obj; t.type != EMPTY; t = cdrref(t), free_index++) {
    object_free(&cars[free_index]);
    cars[free_index] = car(t);
  }
  if (free_index == MEMORY_SIZE) {
    gc();
  }
  return out;
}

Object list2bytevector(Object obj) {
  size_t len = 0;
  for (Object t = obj; t.type != EMPTY; t = cdrref(t), len++) {
    ;
  }
  if (free_index + len - 1 >= MEMORY_SIZE) {
    gc();
    if (free_index + len - 1 >= MEMORY_SIZE) {
      fprintf(stderr, "Insufficient memory.\n");
      exit(1);
    }
  }
  Object out = {.type = BYTEVECTOR, .index = free_index};
  object_free(&cdrs[free_index]);
  cdrs[free_index] = (Object){.bytevector_length = len};
  for (Object t = obj; t.type != EMPTY; t = cdrref(t), free_index++) {
    object_free(&cars[free_index]);
    cars[free_index] = car(t);
  }
  if (free_index == MEMORY_SIZE) {
    gc();
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
  if (free_index == MEMORY_SIZE) {
    gc();
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
