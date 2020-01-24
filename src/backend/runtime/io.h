/* 
read_int
runtime function for read-int primitive : (-> Int)
reads a integer from the stdin port.
*/
int64_t read_int();

/* 
read_float
runtime function for read-float primitive : (-> Float)
reads a floating point number from the stdin port.
*/
double read_float();

/*
print_ascii_char
prints out the character literal for a character
*/
void print_ascii_char(int64_t);

/*
display_char
print a character to stdout
*/
void display_ascii_char(int64_t);

int64_t read_ascii_char();

double neg_float(double n);

/* 
read_bool
runtime function for read-bool primitive : (-> Bool)
reads a #t or #f from the stdin port.
*/
int64_t read_bool();


