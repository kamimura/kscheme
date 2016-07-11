#pragma once

#include "object.h"

/* lex, procedures */
extern mpz_t opz;
extern mpz_t opz1;
extern mpq_t opq;
extern mpq_t opq1;
extern mpf_t opf;
extern mpf_t opf1;
extern mpfr_t real;
extern mpfr_t imag;
extern mpfr_t opfr;
extern mpfr_t opfr1;
extern mpfr_t opfr2;
extern mpfr_t opfr3;
extern mpfr_t opfr4;
extern mpfr_t argopfr;
extern mpfr_t sopfr;
extern mpfr_t copfr;
extern mpc_t opc;
extern mpc_t opc1;
extern mpc_t opc2;
extern mpc_t opc3;
/* lex, procedures, end */

extern mpfr_prec_t const MPC_PREC;

Object numberc_new_r_istr(mpfr_t const real, char *imag, int base);
