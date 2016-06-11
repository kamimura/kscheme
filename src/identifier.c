#include "identifier.h"

#include <glib.h>
#include <string.h>

Object quote_sym;

GHashTable *symtab = NULL;
Object identifier_new(char *str) {
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
