#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

char lbuf[72];
uint8_t cp = 0;
char pgm[72 * 256] = "100 Success\n200 poo\n";
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
    printf("%d", sbrstk[i]);
    if (i == sbrsp) {
      printf("<-- SBRSP = %d\n", sbrsp);
      break;
    }
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
    ilpc += 2;
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

  aestk[aesp++] = strtol(&pgm[pgp], NULL, 10);
  pgp = i;
  ilpc += 2;
}

void tst(int8_t offset, char *string) {
  int i = 0;
  while (pgm[pgp] == ' ')
    pgp++;
  while (string[i] != '\0') {
    if (string[i++] != pgm[pgp++]) {
      ilpc += offset;
      pgp -= i;
      return;
    }
  }
  ilpc += 2 + strlen(string) + 1; //1 for null terminator
}

void tstl(int8_t offset) {
  printf("implement tstl please\n");
}

void err(uint16_t addr) {
  printf("ERR @ %d\n", curline);
  ilpc = addr;
}

void xinit() {
  printf("implement xinit please\n");
}

void spc() {
  printf(" ");
}

void getln() {
  printf(":");
  fgets(lbuf, sizeof lbuf, stdin);
  printf("you entered %s", lbuf);
}

/* pgm layout
   line number (ascii saves time potentially but putting it into a byte saves some space)
   line text ending with \n
   zero terminator
   72 bytes max line
   256 lines max pgm
 */

int findline(int l) {
  //given line number n return the index into pgm where that line starts
  int i = 0;
  char *end;
  for (;;) {
    int l2 = strtol(pgm + i, &end, 10);
    if (end == pgm + i) {
      //err, stored line has no number
      printf("findline oops: no line num in stored line at index %d\n", i);
    }
    if (l2 == l) {
      //match
      return i;
    } //else no match
    while (pgm[i++] != '\n') {
      if (i >= sizeof pgm) {
	//end of pgm with no match
	return -1;
      }
    } //continue to next line
    if (pgm[i] == '\0') //only needed because pgm can end with \n in which case it will try to keep going
      return -1;
  }
}

void insrt() {
  //get line number from lbuf
  //see if line already exists in pgm
  //delete it if so
  //insert

  char *end;
  int l = strtol(lbuf, &end, 10);
  if (end == lbuf) {
    //err, entered line has no number (this should run it in immediate actually i think)
    printf("no num\n");
    return;
  }
  printf("the line entered is %d\n", l);
  int il = findline(l);
  if (il < 0) {
    printf("no matching line found\n");
    //insert it after the next lowest line
  } else {//we must delete existing line and then insert
    printf("line already exists, must delete\n");
  }
}

void call(uint16_t addr) {
  sbrstk[sbrsp++] = ilpc + 3;
  ilpc = addr;
}

void rtn() {
  ilpc = sbrstk[--sbrsp];
}

void done(uint16_t addr) {
  while(pgm[pgp++] == ' ')
    ;
  if (pgm[pgp] != '\n')
    err(addr);
}

void jmp(uint16_t addr) {
  ilpc = addr;
}

void fin(uint16_t addr) {
  jmp(addr);
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

void nxt(uint16_t addr) {
  printf("please implement nxt\n");
  if (curline == 0) {
    ilpc = addr;
    return;
  } else {
    //select next line and interpret
  }
}

enum comparisons {
  CMP_EQ, CMP_GT, CMP_LT, CMP_NE, CMP_GTE, CMP_LTE
};

void cmpr(uint16_t addr) {
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
    nxt(addr);
    return;
  }
  ilpc += 2;
}

void innum() {
  int16_t buf;
  char *stat;
  char *end;
  do {
    printf("? ");
    stat = fgets(lbuf, sizeof lbuf, stdin);
    buf = strtol(lbuf, &end, 10);
  } while (end == lbuf || stat == NULL);
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

//uint8_t ilpgm[] = {IL_INIT, IL_LIT, 0x03, 0x00, IL_INNUM, IL_INNUM, IL_DIV, IL_STORE,
//		   IL_LIT, 0x03, 0x00, IL_IND, IL_PRN};
uint8_t ilpgm[] = {IL_GETLINE, IL_INSRT};

void step_il() {
  uint8_t instr = ilpgm[ilpc];

  if (instr & 0x80) { //ctrl flow
    if (instr & 0x40) { //relative
      int8_t offset = ilpgm[ilpc + 1];
      switch (instr) {
      case IL_TST:
	tst(offset, &ilpgm[ilpc + 2]);
	break;
      case IL_TSTV:
	tstv(offset);
	break;
      case IL_TSTN:
	tstn(offset);
	break;
      case IL_TSTL:
	tstl(offset);
	break;
      default:
	printf("ctrl flow rel oops\n");
	ilpc++;
	break;
      }
      return;
    } else { //absolute
      uint16_t addr = (uint16_t)((ilpgm[ilpc + 1]) | (ilpgm[ilpc + 2] << 8));
      switch (instr) {
      case IL_JMP:
	jmp(addr);
	break;
      case IL_CALL:
	call(addr);
	break;
      case IL_RTN:
	rtn();
	break;
      case IL_FIN:
	jmp(addr); //design note says fin just jumps to line collect routine
	break;
      case IL_ERR:
	err(addr);
	break;
      case IL_INIT:
	init();
	ilpc++;
	break;
      case IL_NXT:
	nxt(addr);
	break;
      case IL_DONE:
	done(addr);
	break;
      case IL_XINIT:
	xinit();
	ilpc++;
	break;
      default:
	printf("ctrl flow abs oops\n");
	ilpc++;
	break;
      }
    }
  } else { //stack/io/basic line
    int16_t n;
    uint16_t addr = (uint16_t)((ilpgm[ilpc + 1]) | (ilpgm[ilpc + 2] << 8));
    switch (instr) {
    case IL_PRS:
      prs();
      break;
    case IL_PRN:
      prn();
      break;
    case IL_SPC:
      spc();
      break;
    case IL_NLINE:
      nline();
      break;
    case IL_XFER:
      xfer();
      break;
    case IL_SAV:
      sav();
      break;
    case IL_RSTR:
      rstr();
      break;
    case IL_CMPR:
      cmpr(addr);
      break;
    case IL_INNUM:
      innum();
      break;
    case IL_LIT:
      n = (int16_t)((ilpgm[ilpc + 1]) | (ilpgm[ilpc + 2] << 8));
      lit(n);
      ilpc += 2;
      break;
    case IL_ADD:
      add();
      break;
    case IL_SUB:
      sub();
      break;
    case IL_NEG:
      neg();
      break;
    case IL_MUL:
      mul();
      break;
    case IL_DIV:
      divd();
      break;
    case IL_STORE:
      store();
      break;
    case IL_IND:
      ind();
      break;
    case IL_LST:
      lst();
      break;
    case IL_GETLINE:
      getln();
      break;
    case IL_INSRT:
      insrt();
      break;
    default:
      printf("stack/io/basic line oops\n");
      break;
    }
    ilpc++;
  }
}

void main() {
  while (ilpc <= 1) {
    step_il();
  }
  printf("index of line 100 = %d\nindex of line 105 = %d\n", findline(100), findline(105));
}
