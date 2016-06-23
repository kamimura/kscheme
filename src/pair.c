#include "pair.h"

gint cmp_func(gconstpointer a_ptr, gconstpointer b_ptr) {
  size_t a = (size_t)a_ptr;
  size_t b = (size_t)b_ptr;
  return a < b ? -1 : a_ptr == b_ptr ? 0 : 1;
}
static int tree_value = 1;
gboolean traverse_func(gpointer key, gpointer value, gpointer tree) {
  g_tree_insert(tree, key, &tree_value);
  return FALSE;
}
GTree *tree_copy(GTree *tree) {
  GTree *out = g_tree_new(cmp_func);
  g_tree_foreach(tree, traverse_func, out);
  return out;
}
typedef struct {
  Object o;
  GTree *t;
} ObjTree;

void pair_get_cycles(Object const obj, uint8_t *cycles) {
  GTree *tree = g_tree_new(cmp_func);
  g_tree_insert(tree, (gpointer)obj.index, &tree_value);
  GQueue *queue = g_queue_new();
  Object o1 = carref(obj);
  ObjTree ot1 = {.o = o1, .t = tree};
  GTree *tree2 = tree_copy(tree);
  Object o2 = cdrref(obj);
  ObjTree ot2 = {.o = o2, .t = tree2};
  g_queue_push_head(queue, &ot2);
  ObjTree *ot_ptr = &ot1;
  while (ot_ptr != NULL) {
    if (ot_ptr->o.type == PAIR) {
      if (cycles[ot_ptr->o.index] != 1) {
        gpointer *p = g_tree_lookup(ot_ptr->t, (gpointer)(ot_ptr->o.index));
        if (p != NULL) {
          cycles[ot_ptr->o.index] = 1;
          g_tree_destroy(ot_ptr->t);
          ot_ptr = g_queue_pop_tail(queue);
        } else {
          g_tree_insert(ot_ptr->t, (gpointer)obj.index, &tree_value);
          Object o1 = carref(ot_ptr->o);
          Object o2 = cdrref(ot_ptr->o);
          GTree *tree2 = tree_copy(ot_ptr->t);
          ObjTree ot = {.o = o1, .t = ot_ptr->t};
          ot_ptr = &ot;
          ObjTree ot2 = {.o = o2, .t = tree2};
          g_queue_push_head(queue, &ot2);
        }
      } else {
        g_tree_destroy(ot_ptr->t);
        ot_ptr = g_queue_pop_tail(queue);
      }
    } else {
      g_tree_destroy(ot_ptr->t);
      ot_ptr = g_queue_pop_tail(queue);
    }
  }
}
static void pair_write_cycle(FILE *stream, Object obj, uint8_t *cycles,
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
#include <errno.h>
#include <string.h>
void pair_write(FILE *stream, Object const obj) {
  uint8_t *cycles = calloc(MEMORY_SIZE, sizeof(uint8_t));
  if (cycles == NULL) {
    fprintf(stderr, "%s\n", strerror(errno));
    exit(1);
  }
  pair_get_cycles(obj, cycles);
  size_t *labels_no = calloc(MEMORY_SIZE, sizeof(size_t));
  if (labels_no == NULL) {
    fprintf(stderr, "%s\n", strerror(errno));
    exit(1);
  }
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
  if (shared == NULL) {
    fprintf(stderr, "%s\n", strerror(errno));
    exit(1);
  }
  pair_get_shared(obj, shared);
  size_t *labels_no = calloc(MEMORY_SIZE, sizeof(size_t));
  if (labels_no == NULL) {
    fprintf(stderr, "%s\n", strerror(errno));
    exit(1);
  }
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
