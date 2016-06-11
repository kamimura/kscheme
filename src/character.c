/** \file  */
#include "character.h"

Object character_new(char const *str) {
  return (Object){.type = CHARACTER, .character = g_utf8_get_char(str)};
}
