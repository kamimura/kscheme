#include "identifier.h"

#include <glib.h>
#include <string.h>

Object quote_sym;

static GHashTable *symtab = NULL;
Object identifier_new(char const *str) {
  if (symtab == NULL) {
    symtab = g_hash_table_new(g_str_hash, g_str_equal);
  }
  char *s = g_hash_table_lookup(symtab, str);
  if (s == NULL) {
    s = strdup(str);
    g_hash_table_insert(symtab, s, s);
  }
  return (Object){.type = IDENTIFIER, .identifier = s};
}

static GHashTable *symtab_vertical = NULL;
Object identifier_vertical_new(char const *str) {
  if (symtab_vertical == NULL) {
    symtab_vertical = g_hash_table_new(g_str_hash, g_str_equal);
  }
  char *s = g_hash_table_lookup(symtab_vertical, str);
  if (s == NULL) {
    s = strdup(str);
    g_hash_table_insert(symtab_vertical, s, s);
  }
  return (Object){.type = IDENTIFIER_VERTICAL, .identifier_vertical = s};
}
size_t identifier_vertical_i = 0;
char *identifier_vertical_str = NULL;
void identifier_vertical_func(gpointer data, gpointer user_data) {
  identifier_vertical_str[identifier_vertical_i] = GPOINTER_TO_UINT(data);
  identifier_vertical_i++;
}
