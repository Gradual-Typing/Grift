#include "runtime.h"
#include "constants.h"

int64_t read_int(){
  int64_t i;
  if(1 != scanf("%" PRId64, &i)){
    fputs("Error in read_int\n", stderr);
    exit(-1);
  }
  return i;
}

double read_float(){
  double d;
  if(1 != scanf("%lf", &d)){
    fputs("Error in read_float\n", stderr);
    exit(-1);
  }
  return d;
}

int64_t read_bool(){
  char c;
  if (1 != scanf(" #%c", &c)) {
    fputs("Error in read_bool\n", stderr);
    exit(-1);
  } else if ( c == 't') {
    return TRUE_IMDT;
  } else if ( c == 'f') {
    return FALSE_IMDT;
  }

  fputs("Error in read_bool: didn't get #t or #f\n", stderr);
  exit(-1);
  
}

// Based on rackets version of print_char
// char are the unicode code points
void print_ascii_char(int64_t ch){
  char *str, buf[32]; 
  if (0 <= ch && ch <= 127) {
    switch ( ch ) {
    case '\0': 
      str = "#\\nul";
      break;
    case '\n':
      str = "#\\newline";
      break;
    case '\t':
      str = "#\\tab";
      break;
    case 0xb:
      str = "#\\vtab";
      break;
    case ' ':
      str = "#\\space";
      break;
    case '\r':
      str = "#\\return";
      break;
    case '\f':
      str = "#\\page";
      break;
    case '\b':
      str = "#\\backspace";
      break;
    case 0x7f:
      str = "#\\rubout";
      break;
    default:
      // a small range of homoiconic characters
      if (ch >= 32  && ch <= 126) { 
        buf[0] = '#';
        buf[1] = '\\';
        buf[2] = ch;  
        buf[3] = 0;
      } else {
        sprintf(buf, "#\\u%.4X", (uint32_t) ch);
      }
      str = buf;
      break;
    }
    fputs(str, stdout);
  } else {
    fputs("print_ascii_string:"
          " code point is outside of ascii range",
          stderr);
  }
}

void display_ascii_char(int64_t ch){
  putc(ch, stdout);
}

char read_byte(){
  int c = getchar();
  if (EOF == c){
    fputs("read_byte : unable to read byte", stderr);
  }
  return (char)c;
}
  
int64_t read_ascii_char(){
  char c = read_byte();
  if (!(0 <= c && c <= 127)){
    fputs("read_asci_char: invalid code_point\n", stdout);
  }
  return c;
}


