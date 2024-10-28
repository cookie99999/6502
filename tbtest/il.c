#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

char lbuf[72];
uint8_t cp = 0;
char pgm[72 * 256];
uint16_t pgp = 0;
int16_t vars[26];
uint16_t ilpc = 0;
uint8_t curline = 0;

int16_t aestk[256];
uint8_t aesp = 0;
uint16_t sbrstk[256];
uint8_t sbrsp = 0;

void dump_ae() {
  printf("AESTK:\n");
  for (int i = 0; i <= aesp; i++) {
    printf("%d", aestk[i]);
    if (i == aesp) {
      printf("<-- AESP = %d\n", aesp);
      break;
    }
    printf("\n");
  }
}

void dump_sbr() {
  printf("SBRSTK:\n");
  for (int i = 0; i <= sbrsp; i++) {
    printf("%d ", sbrstk[i]);
    if (i == sbrsp) {
      printf("<-- SBRSP = %d\n");
      break;
    }
    if (i % 3 == 0)
      printf("\n");
  }
}

void dump_vars() {
  printf("Variables:\n");
  for (int i = 0; i < 26; i++) {
    printf("%c = %d\n", 'A' + i, vars[i]);
  }
}

void drop_ae() {
  aesp--;
}

void drop_sbr() {
  sbrsp--;
}

void swap_ae() {
  int16_t tmp = aestk[aesp - 1];
  aestk[aesp - 1] = aestk[aesp - 2];
  aestk[aesp - 2] = tmp;
}

void swap_sbr() {
  uint16_t tmp = sbrstk[sbrsp - 1];
  sbrstk[sbrsp - 1] = sbrstk[sbrsp - 2];
  sbrstk[sbrsp - 2] = tmp;
}

void dup_ae() {
  aestk[aesp] = aestk[aesp - 1];
  aesp++;
}

void dup_sbr() {
  sbrstk[sbrsp] = sbrstk[sbrsp - 1];
  sbrsp++;
}

void add() {
  int16_t tmp = aestk[aesp - 1] + aestk[aesp - 2];
  drop_ae();
  aestk[aesp - 1] = tmp;
}

void sub() {
  int16_t tmp = aestk[aesp - 1] - aestk[aesp - 2];
  drop_ae();
  aestk[aesp - 1] = tmp;
}

void neg() {
  aestk[aesp - 1] = -(aestk[aesp - 1]);
}

void mul() {
  int16_t tmp = aestk[aesp - 1] * aestk[aesp - 2];
  drop_ae();
  aestk[aesp - 1] = tmp;
}

void divd() {
  int16_t tmp = aestk[aesp - 1] / aestk[aesp - 2];
  drop_ae();
  aestk[aesp - 1] = tmp;
}

void store() {
  vars[aestk[aesp - 2]] = aestk[aesp - 1];
  drop_ae();
  drop_ae();
}

void lit(int16_t n) {
  aestk[aesp++] = n;
}

void prn() {
  printf("%d", aestk[aesp - 1]);
  drop_ae();
}

void ind() {
  aestk[aesp - 1] = vars[aestk[aesp - 1]];
}

void nline() {
  printf("\n");
}

void prs() {
  while (pgm[pgp] != '\n' && pgm[pgp] != '"') {
    printf("%c", pgm[pgp++]);
  }
  pgp++;
}

void tstv(int8_t offset) {
  if (pgm[pgp] >= 'A' && pgm[pgp] <= 'Z') {
    aestk[aesp++] = pgm[pgp] - 'A';
    ilpc++;
  } else {
    ilpc += offset;
  }
}

void tstn(int8_t offset) {
  int i = pgp;
  if (pgm[i] == '-')
    i++;
  for (; pgm[i] != ' ' && pgm[i] != '\n'; i++) {
    if (pgm[i] < '0' || pgm[i] > '9') {
      ilpc += offset;
      return;
    }
  }

  aestk[aesp++] = atoi(&pgm[pgp]);
  ilpc++;
}

void tst(int8_t offset, char *string) {
  int i = 0;
  while (string[i] != '\0') {
    if (string[i++] != pgm[pgp++]) {
      ilpc += offset;
      return;
    }
  }
  ilpc++;
}

void call(uint16_t addr) {
  sbrstk[sbrsp++] = ilpc;
  ilpc = addr;
}

void rtn() {
  ilpc = sbrstk[--sbrsp];
}

void done(uint16_t addr) {
  while(pgm[pgp++] == ' ')
    ;
  if (pgm[pgp] != '\n')
    ; //error
  ilpc = addr;
}

void jmp(uint16_t addr) {
  ilpc = addr;
}

void sav() {
  sbrstk[sbrsp++] = curline;
  //todo overflow check
}

void rstr() {
  curline = sbrstk[--sbrsp];
}

void xfer() {
  if (aestk[aesp - 1] < 0x100 && aestk[aesp - 1] >= 0) {
    curline = aestk[--aesp];
    //check if exists and position pgp
  }
  //else err
}

void nxt() {
}

enum comparisons {
  CMP_EQ, CMP_GT, CMP_LT, CMP_NE, CMP_GTE, CMP_LTE
};

void cmpr() {
  int failed = 0;
  switch (aestk[aesp - 2]) {
  case CMP_EQ:
    failed = !(aestk[aesp - 1] == aestk[aesp - 3]);
    break;
  case CMP_GT:
    failed = !(aestk[aesp - 1] > aestk[aesp - 3]);
    break;
  case CMP_LT:
    failed = !(aestk[aesp - 1] < aestk[aesp - 3]);
    break;
  case CMP_NE:
    failed = !(aestk[aesp - 1] != aestk[aesp - 3]);
    break;
  case CMP_GTE:
    failed = !(aestk[aesp - 1] >= aestk[aesp - 3]);
    break;
  case CMP_LTE:
    failed = !(aestk[aesp - 1] <= aestk[aesp - 3]);
    break;
  default:
    failed = 1;
    printf("cmpr oops\n");
    break;
  }
  drop_ae();
  drop_ae();
  drop_ae();
  if (failed) {
    nxt();
    return;
  }
}

void innum() {
  int16_t buf;
  int stat;
  do {
    printf("? ");
    fflush(stdin);
    stat = scanf("%hd", &buf);
  } while (stat != 1 || stat == EOF);
  aestk[aesp++] = buf;
}

void lst() {
  printf("%s\n", pgm);
}

void init() {
  aesp = sbrsp = pgp = curline = 0;
  memset(pgm, '\0', 72 * 256);
  memset(lbuf, '\0', 72);
}

//1....... ctrl flow

//10...... absolute
//10000000 0x80 jmp 3 bytes
//10000001 0x81 call 3 bytes
//10000010 0x82 rtn
//10000011 0x83 fin
//10000100 0x84 err 3 bytes
//10000101 0x85 init
//10000110 0x86 nxt 3 bytes
//10000111 0x87 done 3 bytes
//10001000 0x88 xinit

//11...... relative
//11000000 0xc0 tst 2 bytes plus strlen
//11000001 0xc1 tstv 2 bytes
//11000010 0xc2 tstn 2 bytes
//11000011 0xc3 tstl 2 bytes

//0....... stack/io/basic line
//00000000 0x00 reserved
//00000001 0x01 prs
//00000010 0x02 prn
//00000011 0x03 spc
//00000100 0x04 nline
//00000101 0x05 xfer
//00000110 0x06 sav
//00000111 0x07 rstr
//00001000 0x08 cmpr
//00001001 0x09 innum
//00001010 0x0a reserved
//00001011 0x0b lit 3 bytes
//00001100 0x0c add
//00001101 0x0d sub
//00001110 0x0e neg
//00001111 0x0f mul
//00010000 0x10 div
//00010001 0x11 store
//00010010 0x12 ind
//00010011 0x13 lst
//00010100 0x14 getline
//00010101 0x15 insrt

enum il_instructions {
  IL_JMP = 0x80, IL_CALL, IL_RTN, IL_FIN, IL_ERR, IL_INIT, IL_NXT,
  IL_DONE, IL_XINIT, IL_TST = 0xc0, IL_TSTV, IL_TSTN, IL_TSTL,
  IL_PRS = 0x01, IL_PRN, IL_SPC, IL_NLINE, IL_XFER, IL_SAV, IL_RSTR,
  IL_CMPR, IL_INNUM, IL_LIT = 0x0b, IL_ADD, IL_SUB, IL_NEG, IL_MUL,
  IL_DIV, IL_STORE, IL_IND, IL_LST, IL_GETLINE, IL_INSRT
};

void main() {
  init();
  lit(3);
  innum();
  innum();
  divd();
  store();
  lit(3);
  ind();
  prn();
}
