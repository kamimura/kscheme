#include "pair.h"

gint cmp_func(gconstpointer a_ptr, gconstpointer b_ptr) {
  size_t a = (size_t)a_ptr;
  size_t b = (size_t)b_ptr;
  return a < b ? -1 : a_ptr == b_ptr ? 0 : 1;
}
int tree_value = 1;
gboolean traverse_func(gpointer key, gpointer value, gpointer tree) {
  g_tree_insert(tree, key, &tree_value);
  return FALSE;
}
GTree *tree_copy(GTree *tree) {
  GTree *out = g_tree_new(cmp_func);
  g_tree_foreach(tree, traverse_func, out);
  return out;
}
void pair_get_cycles(Object obj, GTree *tree, uint8_t *cycles) {
  if (obj.type != PAIR) {
    return;
  }
  if (cycles[obj.index] == 1) {
    return;
  }
  gpointer *p = g_tree_lookup(tree, (gpointer)(obj.index));
  if (p != NULL) {
    cycles[obj.index] = 1;
    return;
  }
  g_tree_insert(tree, (gpointer)(obj.index), &tree_value);
  Object o1 = carref(obj);
  Object o2 = cdrref(obj);
  GTree *new_tree = tree_copy(tree);
  pair_get_cycles(o1, tree, cycles);
  g_tree_destroy(tree);
  pair_get_cycles(o2, new_tree, cycles);
  g_tree_destroy(new_tree);
}
void pair_write_cycle(FILE *stream, Object obj, uint8_t *cycles,
                      size_t *labels_no, size_t *label_no_ptr) {
  if (obj.type != PAIR) {
    object_write(stream, obj);
    return;
  }
  if (cycles[obj.index] == 2) {
    fprintf(stream, "#%ld#", labels_no[obj.index]);
    return;
  }
  if (cycles[obj.index] == 1) {
    fprintf(stream, "#%ld=", *label_no_ptr);
    cycles[obj.index] = 2;
    labels_no[obj.index] = *label_no_ptr;
    *label_no_ptr += 1;
  }
  fprintf(stream, "(");
  pair_write_cycle(stream, carref(obj), cycles, labels_no, label_no_ptr);
  for (Object t = cdrref(obj); t.type != EMPTY; t = cdrref(t)) {
    fprintf(stream, " ");
    if (t.type != PAIR) {
      fprintf(stream, ". ");
      object_write(stream, t);
      break;
    }
    if (cycles[t.index] == 2) {
      fprintf(stream, ". ");
      fprintf(stream, "#%ld#", labels_no[t.index]);
      break;
    }
    if (cycles[t.index] == 1) {
      fprintf(stream, ". ");
      pair_write_cycle(stream, t, cycles, labels_no, label_no_ptr);
      break;
    }
    pair_write_cycle(stream, carref(t), cycles, labels_no, label_no_ptr);
  }
  fprintf(stream, ")");
}
void pair_write(FILE *stream, Object const obj) {
  uint8_t *cycles = calloc(MEMORY_SIZE, sizeof(uint8_t));
  GTree *tree = g_tree_new(cmp_func);
  pair_get_cycles(obj, tree, cycles);
  g_tree_destroy(tree);
  size_t *labels_no = calloc(MEMORY_SIZE, sizeof(size_t));
  size_t label_no = 0;
  pair_write_cycle(stream, obj, cycles, labels_no, &label_no);
  free(cycles);
  free(labels_no);
}
void pair_get_shared(Object obj, uint8_t *shared) {
  shared[obj.index] = 1;
  Object o = carref(obj);
  if (o.type == PAIR) {
    if (shared[o.index] == 0) {
      shared[o.index] = 1;
      pair_get_shared(o, shared);
    } else if (shared[o.index] == 1) {
      shared[o.index] = 2;
    }
  }
  o = cdrref(obj);
  if (o.type == PAIR) {
    if (shared[o.index] == 0) {
      shared[o.index] = 1;
      pair_get_shared(o, shared);
    } else {
      shared[o.index] = 2;
    }
  }
}
void pair_shared_write(FILE *stream, Object obj, uint8_t *shared,
                       size_t *labels_no, size_t *label_no_ptr) {
  if (obj.type != PAIR) {
    object_write(stream, obj);
    return;
  }
  if (shared[obj.index] == 3) {
    fprintf(stream, "#%ld#", labels_no[obj.index]);
    return;
  }
  if (shared[obj.index] == 2) {
    fprintf(stream, "#%ld=", *label_no_ptr);
    shared[obj.index] = 3;
    labels_no[obj.index] = *label_no_ptr;
    *label_no_ptr += 1;
  }
  fprintf(stream, "(");
  pair_shared_write(stream, carref(obj), shared, labels_no, label_no_ptr);
  for (Object t = cdrref(obj); t.type != EMPTY; t = cdrref(t)) {
    fprintf(stream, " ");
    if (t.type != PAIR) {
      fprintf(stream, ". ");
      object_write(stream, t);
      break;
    }
    if (shared[t.index] == 3) {
      fprintf(stream, ". ");
      fprintf(stream, "#%ld#", labels_no[t.index]);
      break;
    }
    if (shared[t.index] == 2) {
      fprintf(stream, ". ");
      pair_shared_write(stream, t, shared, labels_no, label_no_ptr);
      break;
    }
    pair_shared_write(stream, carref(t), shared, labels_no, label_no_ptr);
  }
  fprintf(stream, ")");
}

void pair_write_shared(FILE *stream, Object const obj) {
  uint8_t *shared = calloc(MEMORY_SIZE, sizeof(uint8_t));
  pair_get_shared(obj, shared);
  size_t *labels_no = calloc(MEMORY_SIZE, sizeof(size_t));
  size_t label_no = 0;
  pair_shared_write(stream, obj, shared, labels_no, &label_no);
  free(shared);
  free(labels_no);
}

void pair_write_simple(FILE *stream, Object const obj) {
  fprintf(stream, "(");
  object_write(stream, carref(obj));
  for (Object t = cdrref(obj); t.type != EMPTY; t = cdrref(t)) {
    fprintf(stream, " ");
    if (t.type != PAIR) {
      fprintf(stream, ". ");
      object_write(stream, t);
      break;
    }
    object_write(stream, carref(t));
  }
  fprintf(stream, ")");
}
