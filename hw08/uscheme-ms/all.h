/* {\Tt all.h} for \uschemeplus 267a */
#include <assert.h>
#include <ctype.h>
#include <inttypes.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/* type definitions for \uschemeplus (generated by a script) */
typedef struct Lambda Lambda; 
typedef struct Value Value;
typedef enum {
    NIL, BOOLV, NUM, SYM, PAIR, CLOSURE, PRIMITIVE, FORWARD, INVALID
} Valuealt;

/* type definitions for \uschemeplus (generated by a script) */
typedef struct Def *Def;
typedef enum { VAL, EXP, DEFINE, DEFS } Defalt; 
typedef struct XDef *XDef;
typedef enum { DEF, USE, TEST } XDefalt; 
typedef struct UnitTest *UnitTest;
typedef enum { CHECK_EXPECT, CHECK_ASSERT, CHECK_ERROR } UnitTestalt;

typedef struct Exp *Exp;
typedef enum {
    LITERAL, VAR, SET, IFX, WHILEX, BEGIN, LETX, LAMBDAX, APPLY, BREAKX,
    CONTINUEX, RETURNX, THROW, TRY_CATCH, HOLE, WHILE_RUNNING_BODY,
    CALLENV, LETXENV
} Expalt;

/* type definitions for \uschemeplus 267b */
typedef struct Stack *Stack;
typedef struct Frame Frame;
/* type definitions for \uschemeplus 319a */
typedef Value *Register;  /* pointer to a local variable or a parameter
                             of a C function that could allocate */
typedef struct Registerlist *Registerlist;   /* list of Register */
typedef struct UnitTestlistlist *UnitTestlistlist;
                                               /* list of UnitTestlist (list) */
/* type definitions for \uscheme 147b */
typedef enum Letkeyword { LET, LETSTAR, LETREC } Letkeyword;
typedef struct UnitTestlist  *UnitTestlist;  // list of UnitTest 
typedef struct Explist  *Explist;            // list of Exp 
typedef struct Deflist  *Deflist;            // list of Def    /*OMIT*/
/* type definitions for \uscheme 147d */
typedef struct Valuelist *Valuelist;     // list of Value
typedef Value (Primitive)(Exp e, int tag, Valuelist vs);
/* type definitions for \uscheme 159b */
typedef struct Env *Env;
/* type definitions for \uscheme 172b */
enum {
  #define xx(NAME, TAG, FUNCTION) TAG,
  #include "prim.h"
  #undef xx
  UNUSED_TAG
};
/* shared type definitions 35d */
typedef struct Name *Name;
typedef struct Namelist *Namelist;   // list of Name
/* shared type definitions 38a */
typedef struct XDefstream *XDefstream;
/* shared type definitions 38d */
typedef enum Prompts { NO_PROMPTS, STD_PROMPTS } Prompts;
/* shared type definitions 39b */
typedef enum Echo { NO_ECHOES, ECHOES } Echo;
/* shared type definitions 43b */
typedef struct Sourceloc *Sourceloc;
/* shared type definitions 43d */
typedef enum ErrorFormat { WITH_LOCATIONS, WITHOUT_LOCATIONS } ErrorFormat;
/* shared type definitions 945b */
typedef struct ParserState *ParserState;
typedef struct ParsingContext *ParsingContext;
/* shared type definitions 946a */
typedef enum ParserResult {
  PARSED,            /* some input was parsed without any errors */
  INPUT_EXHAUSTED,   /* there aren't enough inputs */
  INPUT_LEFTOVER,    /* there are too many inputs */
  BAD_INPUT,         /* an input wasn't what it should have been */
  STOP_PARSING       /* all the inputs have been parsed; it's time to stop */
} ParserResult;
/* shared type definitions 946b */
typedef ParserResult (*ShiftFun)(ParserState);
/* shared type definitions 950c */
typedef struct ParserRow *ParserTable;
/* shared type definitions 958a */
enum Sugar {
  CAND, COR,    /* short-circuit Boolean operators */

  WHILESTAR, DO_WHILE, FOR,     /* bonus loop forms */

  WHEN, UNLESS,       /* single-sided conditionals */

  RECORD,             /* record-type definition */

  COND                /* McCarthy's conditional from Lisp */

};
/* shared type definitions (generated by a script) */
typedef struct Par *Par;
typedef enum { ATOM, LIST } Paralt; 
/* shared type definitions 914a */
typedef struct Linestream *Linestream;
/* shared type definitions 917c */
typedef struct Parlist *Parlist; /* list of Par */
/* shared type definitions 917d */
typedef struct Parstream *Parstream;
/* shared type definitions 924b */
typedef struct Printbuf *Printbuf;
/* shared type definitions 926d */
/* definition of [[va_list_box]] 927a */
typedef struct va_list_box {
  va_list ap;
} va_list_box;
typedef void Printer(FILE *output, va_list_box *args);
/* shared type definitions 1034d */
typedef enum TestResult { TEST_PASSED, TEST_FAILED } TestResult;

/* structure definitions for \uschemeplus (generated by a script) */
struct Lambda { Namelist formals; Exp body; }; 
struct Value {
    Valuealt alt;
    union {
        bool boolv;
        int num;
        Name sym;
        struct { Value *car; Value *cdr; } pair;
        struct { Lambda lambda; Env env; } closure;
        struct { int tag; Primitive *function; } primitive;
        Value *forward;
        const char *invalid;
    } u;
};

/* structure definitions for \uschemeplus (generated by a script) */
struct Def {
    Defalt alt;
    union {
        struct { Name name; Exp exp; } val;
        Exp exp;
        struct { Name name; Lambda lambda; } define;
        Deflist defs;
    } u;
};

struct XDef {
    XDefalt alt; union { Def def; Name use; UnitTest test; } u;
};

struct UnitTest {
    UnitTestalt alt;
    union {
        struct { Exp check; Exp expect; } check_expect;
        Exp check_assert;
        Exp check_error;
    } u;
};

struct Exp {
    Expalt alt;
    union {
        Value literal;
        Name var;
        struct { Name name; Exp exp; } set;
        struct { Exp cond; Exp truex; Exp falsex; } ifx;
        struct { Exp cond; Exp body; } whilex;
        Explist begin;
        struct { Letkeyword let; Namelist xs; Explist es; Exp body; } letx;
        Lambda lambdax;
        struct { Exp fn; Explist actuals; } apply;
        Exp returnx;
        Exp throw;
        struct { Exp body; Exp handler; } try_catch;
        Env callenv;
        Env letxenv;
    } u;
};

/* structure definitions for \uschemeplus (generated by a script) */
struct Parlist {
   Par hd;
   struct Parlist *tl;
};

struct Namelist {
   Name hd;
   struct Namelist *tl;
};

struct UnitTestlist {
   UnitTest hd;
   struct UnitTestlist *tl;
};

struct Explist {
   Exp hd;
   struct Explist *tl;
};

struct Deflist {
   Def    /*OMIT*/ hd;
   struct Deflist *tl;
};

struct Valuelist {
   Value hd;
   struct Valuelist *tl;
};

struct Registerlist {
   Register hd;
   struct Registerlist *tl;
};

struct UnitTestlistlist {
   UnitTestlist hd;
   struct UnitTestlistlist *tl;
};

/* structure definitions for \uschemeplus 267c */
struct Frame {
    struct Exp context;     // mutated in place during evaluation
    Exp syntax;             // when not NULL, kept pristine for error messages
};
/* structure definitions for \uschemeplus 323c */
struct Env {
    Name name;
    Value *loc;
    Env tl;
};
/* structure definitions for \uscheme 1047b */
struct Component {
    Exp exp;
    Explist exps;
    Name name;
    Namelist names;
    Value value;
    /* fields of \uscheme\ [[Component]] added in exercises 1049c */
    /* if implementing COND, add a question-answer field here */
    /* fields of \uscheme\ [[Component]] added in exercises 1312h */
    // for COND:
    struct qa_pairs { Explist questions; Explist answers; } qa_pairs;
};
/* shared structure definitions 945a */
#define MAXCOMPS 4 /* max # of components in any syntactic form */
struct ParserState {
    int nparsed;           /* number of components parsed so far */
    struct Component components[MAXCOMPS];  /* those components */
    Parlist input;         /* the part of the input not yet parsed */

    struct ParsingContext {   /* context of this parse */
        Par par;       /* the original thing we are parsing */
        struct Sourceloc {
            int line;                /* current line number */
            const char *sourcename;  /* where the line came from */
        } *source;
        Name name;     /* a keyword, or name of a function being defined */
    } context;
};
/* shared structure definitions 949d */
struct ParserRow {
    const char *keyword;
    int code;
    ShiftFun *shifts;  /* points to array of shift functions */
};
/* shared structure definitions (generated by a script) */
struct Par { Paralt alt; union { Name atom; Parlist list; } u; }; 
/* shared structure definitions 915a */
struct Linestream {
    char *buf;               /* holds the last line read */
    int bufsize;                /* size of buf */

    struct Sourceloc source; /* where the last line came from */
    FILE *fin;               /* non-NULL if filelines */
    const char *s;           /* non-NULL if stringlines */
};

/* function prototypes for \uschemeplus 1094a */
void cyclecheck(Value *l);
/* function prototypes for \uschemeplus (generated by a script) */
Lambda mkLambda(Namelist formals, Exp body);
Value mkNil(void);
Value mkBoolv(bool boolv);
Value mkNum(int num);
Value mkSym(Name sym);
Value mkPair(Value *car, Value *cdr);
Value mkClosure(Lambda lambda, Env env);
Value mkPrimitive(int tag, Primitive *function);
Value mkForward(Value *forward);
Value mkInvalid(const char *invalid);
/* function prototypes for \uschemeplus (generated by a script) */
Def mkVal(Name name, Exp exp);
Def mkExp(Exp exp);
Def mkDefine(Name name, Lambda lambda);
Def mkDefs(Deflist defs);
struct Def mkValStruct(Name name, Exp exp);
struct Def mkExpStruct(Exp exp);
struct Def mkDefineStruct(Name name, Lambda lambda);
struct Def mkDefsStruct(Deflist defs);
XDef mkDef(Def def);
XDef mkUse(Name use);
XDef mkTest(UnitTest test);
struct XDef mkDefStruct(Def def);
struct XDef mkUseStruct(Name use);
struct XDef mkTestStruct(UnitTest test);
UnitTest mkCheckExpect(Exp check, Exp expect);
UnitTest mkCheckAssert(Exp check_assert);
UnitTest mkCheckError(Exp check_error);
struct UnitTest mkCheckExpectStruct(Exp check, Exp expect);
struct UnitTest mkCheckAssertStruct(Exp check_assert);
struct UnitTest mkCheckErrorStruct(Exp check_error);
Exp mkLiteral(Value literal);
Exp mkVar(Name var);
Exp mkSet(Name name, Exp exp);
Exp mkIfx(Exp cond, Exp truex, Exp falsex);
Exp mkWhilex(Exp cond, Exp body);
Exp mkBegin(Explist begin);
Exp mkLetx(Letkeyword let, Namelist xs, Explist es, Exp body);
Exp mkLambdax(Lambda lambdax);
Exp mkApply(Exp fn, Explist actuals);
Exp mkBreakx(void);
Exp mkContinuex(void);
Exp mkReturnx(Exp returnx);
Exp mkThrow(Exp throw);
Exp mkTryCatch(Exp body, Exp handler);
Exp mkHole(void);
Exp mkWhileRunningBody(void);
Exp mkCallenv(Env callenv);
Exp mkLetxenv(Env letxenv);
struct Exp mkLiteralStruct(Value literal);
struct Exp mkVarStruct(Name var);
struct Exp mkSetStruct(Name name, Exp exp);
struct Exp mkIfxStruct(Exp cond, Exp truex, Exp falsex);
struct Exp mkWhilexStruct(Exp cond, Exp body);
struct Exp mkBeginStruct(Explist begin);
struct Exp mkLetxStruct(Letkeyword let, Namelist xs, Explist es, Exp body);
struct Exp mkLambdaxStruct(Lambda lambdax);
struct Exp mkApplyStruct(Exp fn, Explist actuals);
struct Exp mkBreakxStruct(void);
struct Exp mkContinuexStruct(void);
struct Exp mkReturnxStruct(Exp returnx);
struct Exp mkThrowStruct(Exp throw);
struct Exp mkTryCatchStruct(Exp body, Exp handler);
struct Exp mkHoleStruct(void);
struct Exp mkWhileRunningBodyStruct(void);
struct Exp mkCallenvStruct(Env callenv);
struct Exp mkLetxenvStruct(Env letxenv);
/* function prototypes for \uschemeplus (generated by a script) */
int     lengthPL(Parlist ps);
Par     nthPL   (Parlist ps, unsigned n);
Parlist mkPL    (Par p, Parlist ps);
Parlist popPL   (Parlist ps);
Printer printparlist;

int      lengthNL(Namelist ns);
Name     nthNL   (Namelist ns, unsigned n);
Namelist mkNL    (Name n, Namelist ns);
Namelist popNL   (Namelist ns);
Printer  printnamelist;

int          lengthUL(UnitTestlist us);
UnitTest     nthUL   (UnitTestlist us, unsigned n);
UnitTestlist mkUL    (UnitTest u, UnitTestlist us);
UnitTestlist popUL   (UnitTestlist us);
Printer      printunittestlist;

int     lengthEL(Explist es);
Exp     nthEL   (Explist es, unsigned n);
Explist mkEL    (Exp e, Explist es);
Explist popEL   (Explist es);
Printer printexplist;

int     lengthDL(Deflist ds);
Def    /*OMIT*/ nthDL   (Deflist ds, unsigned n);
Deflist mkDL    (Def    /*OMIT*/ d, Deflist ds);
Deflist popDL   (Deflist ds);
Printer printdeflist;

int       lengthVL(Valuelist vs);
Value     nthVL   (Valuelist vs, unsigned n);
Valuelist mkVL    (Value v, Valuelist vs);
Valuelist popVL   (Valuelist vs);
Printer   printvaluelist;

int          lengthRL(Registerlist rs);
Register     nthRL   (Registerlist rs, unsigned n);
Registerlist mkRL    (Register r, Registerlist rs);
Registerlist popRL   (Registerlist rs);
Printer      printregisterlist;

int              lengthULL(UnitTestlistlist uss);
UnitTestlist     nthULL   (UnitTestlistlist uss, unsigned n);
UnitTestlistlist mkULL    (UnitTestlist us, UnitTestlistlist uss);
UnitTestlistlist popULL   (UnitTestlistlist uss);
Printer          printunittestlistlist;

/* function prototypes for \uschemeplus 267d */
Stack  emptystack  (void);
Exp    pushcontext (struct Exp e, Stack s);
void   popframe    (Stack s);
void   clearstack  (Stack s);
/* function prototypes for \uschemeplus 268a */
Frame *topframe (Stack s);  // NULL if empty
/* function prototypes for \uschemeplus 268b */
void   pushenv_opt (Env env, Expalt context, Stack s);  // may optimize
/* function prototypes for \uschemeplus 268e */
void stack_trace_init(int *countp);  // how many steps to show
void stack_trace_current_expression(Exp e,   Env rho, Stack s);
void stack_trace_current_value     (Value v, Env rho, Stack s);
/* function prototypes for \uschemeplus 268f */
Value getoption(Name name, Env env, Value defaultval);
/* function prototypes for \uschemeplus 269a */
Value validate(Value v);
/* function prototypes for \uschemeplus 275a */
Exp transition_explist(Explist es, Value v); // pointer to static memory
/* function prototypes for \uschemeplus 275b */
Exp head_replaced_with_hole(Explist es);
                                        // shares memory with transition_explist
/* function prototypes for \uschemeplus 275c */
Explist copyEL(Explist es);
void    freeEL(Explist es);
/* function prototypes for \uschemeplus 276a */
Valuelist asLiterals(Explist es);
Value     asLiteral (Exp e);
/* function prototypes for \uschemeplus 276b */
void freeVL(Valuelist vs);
/* function prototypes for \uschemeplus 1071c */
void printstack   (FILE *, va_list_box*);
void printoneframe(FILE *, va_list_box*);
void printframe   (FILE *, Frame *fr);
void printnoenv   (FILE *, va_list_box*);
/* function prototypes for \uschemeplus 320b */
#ifndef DEBUG_GC_REGISTERS    /*OMIT*/
void pushreg(Value *reg);
void popreg (Value *reg);
#endif  /*OMIT*/
/* function prototypes for \uschemeplus 320c */
void pushregs(Valuelist regs);
void popregs (Valuelist regs);
/* function prototypes for \uschemeplus 337a */
void gc_debug_post_acquire(Value *mem, unsigned nvalues); 
/* function prototypes for \uschemeplus 337b */
void gc_debug_pre_release(Value *mem, unsigned nvalues); 
/* function prototypes for \uschemeplus 337c */
void gc_debug_pre_allocate(Value *mem); 
/* function prototypes for \uschemeplus 337d */
void gc_debug_post_reclaim(Value *mem); 
/* function prototypes for \uschemeplus 337e */
void gc_debug_post_reclaim_block(Value *mem, unsigned nvalues); 
/* function prototypes for \uschemeplus 337f */
Value validate(Value v);
/* function prototypes for \uschemeplus 337g */
void gcprint (const char *fmt, ...);  /* print GC debugging info */
void gcprintf(const char *fmt, ...);
/* function prototypes for \uschemeplus 337h */
void gc_debug_init(void);
/* function prototypes for \uscheme 1087b */
int gammadesired(int defaultval, int minimum);
/* function prototypes for \uscheme 159c */
Value *find(Name name, Env env);
/* function prototypes for \uscheme 160 */
Env bindalloc    (Name name,   Value v,      Env env);
Env bindalloclist(Namelist xs, Valuelist vs, Env env);
/* function prototypes for \uscheme 161a */
Value *allocate(Value v);
/* function prototypes for \uscheme 161b */
void initallocate(Env *globals);
/* function prototypes for \uscheme 161c */
Value truev, falsev;
/* function prototypes for \uscheme 161d */
void initvalue(void);
/* function prototypes for \uscheme 161e */
bool istrue(Value v);
/* function prototypes for \uscheme 161f */
Value unspecified(void);
/* function prototypes for \uscheme 162a */
Value eval   (Exp e, Env rho);
Env   evaldef(Def d, Env rho, Echo echo);
/* function prototypes for \uscheme 162b */
void readevalprint(XDefstream xdefs, Env *envp, Echo echo);
/* function prototypes for \uscheme 162c */
void addprimitives(Env *envp);
/* function prototypes for \uscheme 163a */
void printenv    (FILE *output, va_list_box*);
void printvalue  (FILE *output, va_list_box*);
void printexp    (FILE *output, va_list_box*);
void printdef    (FILE *output, va_list_box*);
void printlambda (FILE *output, va_list_box*);
/* function prototypes for \uscheme 171c */
void process_tests(UnitTestlist tests, Env rho);
/* function prototypes for \uscheme 174c */
Value cons(Value v, Value w);
Value equalatoms(Value v, Value w);
/* function prototypes for \uscheme ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) */
Exp desugarLetStar(Namelist xs, Explist es, Exp body);
Exp desugarLet    (Namelist xs, Explist es, Exp body);
/* function prototypes for \uscheme 1050e */
Value parsesx(Par p, Sourceloc source);
struct Component parseletbindings(ParsingContext context, Parlist input);
/* function prototypes for \uscheme 1059b */
int number_of_good_tests(UnitTestlist tests, Env rho);
/* function prototypes for \uscheme 1059d */
TestResult test_result(UnitTest t, Env rho);
/* function prototypes for \uscheme 1061g */
bool equalpairs(Value v, Value w);
/* function prototypes for \uscheme 1063a */
Name namecat(Name n1, Name n2);
/* function prototypes for \uscheme 320d */
Value *allocloc(void);
/* function prototypes for \uscheme 320e */
void initallocate(Env *globals);
/* function prototypes for \uscheme 1311f */
Exp desugarAnd(Explist args);
/* function prototypes for \uscheme 1312a */
Namelist freevars(Exp e, Namelist bound, Namelist free);
/* function prototypes for \uscheme 1312e */
Exp desugarOr(Explist args);
/* function prototypes for \uscheme 1313b */
Exp desugarCond(Explist questions, Explist answers);
/* function prototypes for \uscheme ((answers)) 1313j */
void writeqapairs(ParsingContext context, struct qa_pairs *pairs, Parlist input)
                                                                               ;
/* function prototypes for \uscheme 1314g */
Deflist desugarRecord(Name recname, Namelist fieldnames);
/* shared function prototypes 35e */
Name strtoname(const char *s);
const char *nametostr(Name x);
/* shared function prototypes 38b */
XDef getxdef(XDefstream xdefs);
/* shared function prototypes 38c */
XDefstream stringxdefs(const char *stringname, const char *input);
XDefstream filexdefs  (const char *filename, FILE *input, Prompts prompts);
/* shared function prototypes 42a */
void print (const char *fmt, ...);  // print to standard output
void fprint(FILE *output, const char *fmt, ...);  // print to given file
/* shared function prototypes 42b */
void installprinter(unsigned char c, Printer *take_and_print);
/* shared function prototypes 43a */
void runerror(const char *fmt, ...);
extern jmp_buf errorjmp;        // longjmp here on error
/* shared function prototypes 43c */
void synerror(Sourceloc src, const char *fmt, ...);
/* shared function prototypes 44a */
void set_toplevel_error_format(ErrorFormat format);
/* shared function prototypes 44b */
void checkargc(Exp e, int expected, int actual);
/* shared function prototypes 44c */
Name duplicatename(Namelist names);
/* shared function prototypes 940a */
Exp  parseexp (Par p, Sourceloc source);
XDef parsexdef(Par p, Sourceloc source);
/* shared function prototypes 940b */
Exp exp_of_atom(Name atom);
/* shared function prototypes 943b */
Exp  reduce_to_exp (int alt, struct Component *components);
XDef reduce_to_xdef(int alt, struct Component *components);
/* shared function prototypes 945d */
struct ParserState mkParserState(Par p, Sourceloc source);
/* shared function prototypes 946c */
ParserResult sExp     (ParserState state);  /* shift 1 input into Exp */
ParserResult sExps    (ParserState state);  /* shift all inputs into Explist */
ParserResult sName    (ParserState state);  /* shift 1 input into Name */
ParserResult sNamelist(ParserState state);  /* shift 1 input into Namelist */
/* shared function prototypes 946e */
void halfshift(ParserState state); /* advance input, check for room in output */
/* shared function prototypes 947c */
Explist parseexplist(Parlist p, Sourceloc source);
/* shared function prototypes 947e */
Name parsename(Par p, ParsingContext context);
/* shared function prototypes 948d */
ParserResult stop(ParserState state);
/* shared function prototypes 948f */
ParserResult setcontextname(ParserState state);
/* shared function prototypes 949c */
ParserResult sLocals(ParserState state);  // shift locals if (locals x y z ...)
/* shared function prototypes 950b */
void rowparse(struct ParserRow *table, ParserState s);
void usage_error(int alt, ParserResult r, ParsingContext context);
/* shared function prototypes 950e */
struct ParserRow *tableparse(ParserState state, ParserTable t);
/* shared function prototypes 953d */
ParserResult use_exp_parser(ParserState state);
/* shared function prototypes 957c */
int code_of_name(Name n);
/* shared function prototypes 957d */
void check_exp_duplicates(Sourceloc source, Exp e);
void check_def_duplicates(Sourceloc source, Def d);
/* shared function prototypes (generated by a script) */
Par mkAtom(Name atom);
Par mkList(Parlist list);
struct Par mkAtomStruct(Name atom);
struct Par mkListStruct(Parlist list);
/* shared function prototypes 914b */
char *getline_(Linestream r, const char *prompt);
/* shared function prototypes 914c */
Linestream stringlines(const char *stringname, const char *s);
Linestream filelines  (const char *filename,   FILE *fin);
/* shared function prototypes 917e */
Parstream parstream(Linestream lines, Prompts prompts);
Par       getpar   (Parstream r);
Sourceloc parsource(Parstream pars);
/* shared function prototypes 918a */
extern bool read_tick_as_quote;
/* shared function prototypes 924c */
Printbuf printbuf(void);
void bufreset(Printbuf);
char *bufcopy(Printbuf);
void bufput(Printbuf, char);
/* shared function prototypes 926b */
void print (const char *fmt, ...);                /* print to standard output */
void fprint(FILE *output, const char *fmt, ...);  /* print to given file */
/* shared function prototypes 926c */
void installprinter(unsigned char specifier, Printer *take_and_print);
/* shared function prototypes 927b */
void vprint(FILE *output, const char *fmt, va_list_box *box);
/* shared function prototypes 929b */
Printer printpercent, printstring, printdecimal, printchar, printname;
/* shared function prototypes 929g */
Printer printpar;
/* shared function prototypes 930b */
typedef enum ErrorMode { NORMAL, TESTING } ErrorMode;
void set_error_mode(ErrorMode mode);
extern jmp_buf testjmp;         /* longjmp here if error occurs during a test */
/* shared function prototypes 934a */
extern int checkoverflow(int limit);
/* shared function prototypes 934d */
extern void checkarith(char operation, int32_t n, int32_t m, int precision);
/* shared function prototypes 936a */
void fprint_utf8(FILE *output, unsigned code_point);
void print_utf8 (unsigned u);
/* shared function prototypes 1034b */
void report_test_results(int npassed, int ntests);
/* shared function prototypes 1037d */
Printer printexp, printdef, printvalue, printfun;
/* shared function prototypes 172a */
Primitive arith, binary, unary;
/* shared function prototypes 1048a */
ParserResult sSexp    (ParserState state);
ParserResult sBindings(ParserState state);
/* shared function prototypes ((answers)) 1313h */
ParserResult sQas(ParserState state);

/* global variables for \uschemeplus 268c */
extern int high_stack_mark;
/* global variables for \uschemeplus 268d */
extern int optimize_tail_calls;
extern int show_high_stack_mark;

/* macro definitions used in parsing 944a */
#define ANEXP(ALT)  (  0+(ALT))
#define ADEF(ALT)   (100+(ALT))
#define ATEST(ALT)  (200+(ALT))
#define ANXDEF(ALT) (300+(ALT))
#define ALET(ALT)   (400+(ALT))
#define SUGAR(CODE) (500+(CODE))
#define LATER       1000
#define EXERCISE    1001
/* declarations of global variables used in lexical analysis and parsing 951b */
extern struct ParserRow exptable[];
extern struct ParserRow xdeftable[];
/* declarations of global variables used in lexical analysis and parsing 955b */
extern struct Usage {
    int code;
                         /* codes for form in reduce_to_exp or reduce_to_xdef */
    const char *expected;  /* shows the expected usage of the identified form */
} usage_table[];
/* declarations of global variables used in lexical analysis and parsing ((answers)) 1314d */
extern ShiftFun recordshifts[];
/* {\Tt all.h} for \uschemeplus 320a */
/* structure definitions used in garbage collection 319b */
struct Roots {
    struct {
        Env *user;              // global variables from the user's program 
        struct {
            UnitTestlistlist pending_tests; // unit tests waiting to be run
        } internal;             // the interpreter's internal variables
    } globals;                  // all the global variables
    Stack stack;
                           // the uscheme+ stack, with all parameters and locals
    Registerlist registers;    // pointers to 'machine registers'
};
/* global variables used in garbage collection 1088c */
extern int gc_uses_mark_bits;
/* global variables used in garbage collection 319c */
extern struct Roots roots;
