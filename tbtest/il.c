#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

uint8_t lbuf[72];
uint8_t cp = 0;
char pgm[72 * 256];
uint8_t pgp = 0;
int16_t vars[26];
uint8_t pc = 0;

int16_t aestk[256];
uint8_t aesp = 0;
uint16_t sbrstk[256];
uint8_t sbrsp = 0;

inline void drop_ae() {
  aesp--;
}

inline void drop_sbr() {
  sbrsp--;
}

inline void dup_ae() {
  aestk[aesp + 1] = aestk[aesp];
  aesp++;
}

inline void dup_sbr() {
  sbrstk[sbrsp + 1] = sbrstk[sbrsp];
  sbrsp++;
}

inline void add() {
  int16_t tmp = aestk[aesp] + aestk[aesp - 1];
  drop_ae();
  aestk[aesp] = tmp;
}

inline void sub() {
  int16_t tmp = aestk[aesp] - aestk[aesp - 1];
  drop_ae();
  aestk[aesp] = tmp;
}

inline void neg_ae() {
  aestk[aesp] = -(aestk[aesp]);
}

inline void mul() {
  int16_t tmp = aestk[aesp] * aestk[aesp - 1];
  drop_ae();
  aestk[aesp] = tmp;
}

inline void div() {
  int16_t tmp = aestk[aesp] / aestk[aesp - 1];
  drop_ae();
  aestk[aesp] = tmp;
}

inline void store() {
  vars[aestk[aesp - 1]] = aestk[aesp];
  drop_ae();
  drop_ae();
}

inline void lit_ae(int16_t n) {
  aestk[++aesp] = num;
}

inline void prn() {
  printf("%d", aestk[aesp]);
  drop_ae();
}

inline void ind() {
  aestk[aesp] = vars[aestk[aesp]];
}

inline void nline() {
  printf("\n");
}

inline void prs() {
  while (pgm[pgp] != '\n' && pgm[pgp] != '"') {
    printf("%c", pgm[pgp++]);
  }
  pgp++;
}

inline void tstv(int8_t offset) {
  if (pgm[pgp] >= 'A' && pgm[pgp] <= 'Z') {
    aestk[++aesp] = pgm[pgp] - 'A';
    ilpc++;
  } else {
    ilpc += offset;
  }
}

inline void tstn(int8_t offset) {
  int i = 0;
  for (i = pgp; pgm[i] != ' ' && pgm[i] != '\n'; i++)
