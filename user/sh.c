#include "kernel/types.h"
#include "user/user.h"
#include "kernel/fcntl.h"

typedef struct {
  enum type_enum{
    INT,
    FLOAT,
    STRING, // TODO: add primitive procedure of string
    PROC,
    SYMBOL, // TODO: handle multiple layers of quotation
    PAIR,
    ENV, // not use ENV and FRAME to simplify handling environment, but may need in defensive programming
    FRAME
  } type;
  void* x;
} free_type;

typedef enum {false, true} bool;

bool eq(free_type x, free_type y) {
  if (x.type != y.type) return false;
  if (x.type == INT) return (uint64)*(uint64*)x.x == (uint64)*(uint64*)y.x;
  if (x.type == FLOAT) return (double)*(double*)x.x == (double)*(double*)y.x;
  if (x.type == STRING || x.type == PROC || x.type == SYMBOL) return strcmp(x.x, y.x) == 0;
  return x.x == y.x;
}

bool is_false(free_type x) { return *(bool*)x.x == false; }
bool is_true(free_type x) { return !is_false(x); }

free_type null = (free_type) {.type = SYMBOL, .x = (void*)0 };
free_type free_type_true = (free_type) {.type = SYMBOL, .x = "true" };
free_type free_type_false = (free_type) {.type = SYMBOL, .x = "false" };
free_type ok = (free_type) {.type = SYMBOL, .x = "ok"};

bool is_free_type_false(free_type x) { return eq(x, free_type_false); }
bool is_free_type_true(free_type x) { return !is_free_type_false(x); }

bool is_null(free_type x) { return x.x == (void*)0; }

free_type car(free_type);
free_type cdr(free_type);
free_type proc_para(free_type);
free_type proc_body(free_type);

void __display(free_type x) {
  if (x.type == INT) printf("%d", *(uint64*)x.x);
  else if (x.type == FLOAT) printf("lf", *(double*)x.x);
  else if (x.type == STRING) printf("\"%s\"", (char*)x.x);
  else if (x.type == PROC) printf("#<procedure:%s>", (char*)x.x);
  else if (x.type == SYMBOL) printf("\'%s", (char*)x.x);
  else if (x.type == PAIR) {
    // printf("%d %s\n", car(x).type, car(x).x);
    if (eq(car(x), (free_type) {.type = SYMBOL, .x = "procedure"})) {
      printf("<procedure> ");
      __display(proc_para(x));
      printf("\n");
      __display(proc_body(x));
      printf("\n");
      printf("<procedure env>\n");
      // procedure env has procedure itself, and this cause endless loop
    }
    else {
      if (!is_null(x)) printf("("), __display(car(x));
      free_type i;
      for (i = cdr(x); i.type == PAIR; i = cdr(i))
        printf(" "), __display(car(i));
      if (!is_null(i)) printf(" . "), __display(i);
      printf(")");
    }
  }
  else printf("%d %d", x.type, x.x);
}

void display(free_type x) {
  __display(x);
  printf("\n");
}

#define log(x) printf(__FUNCTION__), printf(": "), display(x);

free_type error(char* s, free_type x) {
  printf(s);
  printf("\n");
  display(x);
  return x;
} 

typedef struct { free_type x; free_type y; } pair;

free_type cons(free_type x, free_type y) {
  free_type a;
  a.type = PAIR;
  a.x = (pair*)malloc(sizeof(pair));
  ((pair*)a.x) -> x = x;
  ((pair*)a.x) -> y = y;
  return a;
}

free_type car(free_type a) {
  if (a.type != PAIR) error("Expect a pair -- CAR", a);
  return ((pair*)a.x) -> x;
}

free_type cdr(free_type a) {
  if (a.type != PAIR) error("Expect a pair -- CDR", a);
  return ((pair*)a.x) -> y;
}

free_type caar(free_type a) { return car(car(a)); }
free_type cadr(free_type a) { return car(cdr(a)); }
free_type cdar(free_type a) { return cdr(car(a)); }
free_type cddr(free_type a) { return cdr(cdr(a)); }
free_type caadr(free_type a) { return car(car(cdr(a))); }
free_type caddr(free_type a) { return car(cdr(cdr(a))); }
free_type cadar(free_type a) { return car(cdr(car(a))); }
free_type cdadr(free_type a) { return cdr(car(cdr(a))); }
free_type cadddr(free_type a) { return car(cdr(cdr(cdr(a)))); }

void set_car(free_type a, free_type b) {
  if (a.type != PAIR) error("Expect a pair -- SET_CAR", a);
  ((pair*)a.x) -> x = b;
}

void set_cdr(free_type a, free_type b) {
  if (a.type != PAIR) error("Expect a pair -- SET_CDR", a);
  ((pair*)a.x) -> y = b;
}

// eval without assignment, begin and cond
bool is_selfeval(free_type);
bool is_var(free_type);
bool is_quot(free_type);
bool is_def(free_type);
bool is_if(free_type);
bool is_lambda(free_type);
bool is_app(free_type);
free_type find_var(free_type, free_type);
free_type text_of_quot(free_type, free_type);
free_type eval_def(free_type, free_type);
free_type eval_if(free_type, free_type);
free_type make_proc(free_type, free_type, free_type);
free_type lambda_para(free_type);
free_type lambda_body(free_type);
free_type apply(free_type, free_type, free_type);
free_type op(free_type);
free_type values(free_type, free_type);

free_type eval(free_type exp, free_type env) {
  log(exp);
  log(env);
  if (is_selfeval(exp)) return exp;
  if (is_var(exp)) return find_var(exp, env);
  if (is_quot(exp)) return text_of_quot(exp, env);
  if (is_def(exp)) return eval_def(exp, env);
  if (is_if(exp)) return eval_if(exp, env);
  if (is_lambda(exp)) return make_proc(lambda_para(exp), lambda_body(exp), env);
  if (is_app(exp)) return apply(eval(op(exp), env), values(cdr(exp), env), env);
  return error("Unknown expression type -- EVAL", exp);
}

bool is_primitive(free_type);
free_type apply_primitive(free_type, free_type);
bool is_compound(free_type);
free_type eval_seq(free_type, free_type);
free_type proc_body(free_type);
free_type proc_para(free_type);
free_type extend_env(free_type, free_type, free_type);
free_type proc_env(free_type);

free_type apply(free_type proc, free_type arg, free_type env) {
  log(proc); log(arg);
  if (is_primitive(proc)) return apply_primitive(proc, arg);
  if (is_compound(proc)) return eval_seq(proc_body(proc), extend_env(proc_para(proc), arg, proc_env(proc)));
  return error("Unknown procedure type -- APPLY", proc);
}

bool is_selfeval(free_type exp) {
  return exp.type == INT || exp.type == FLOAT || exp.type == STRING;
}

bool is_symbol(free_type exp) {
  return exp.type == SYMBOL;
}
bool is_var(free_type exp) { return is_symbol(exp); }

bool is_pair(free_type x) { return x.type == PAIR; }

bool is_tagged_list(free_type exp, char* s) {
  if (!is_pair(exp)) return false;
  return is_symbol(car(exp)) && strcmp(s, (char*) car(exp).x) == 0; 
}

bool is_quot(free_type exp) { return is_tagged_list(exp, "quote"); }
bool is_def(free_type exp) { return is_tagged_list(exp, "define"); }
bool is_if(free_type exp) { return is_tagged_list(exp, "if"); }
bool is_lambda(free_type exp) { return is_tagged_list(exp, "lambda"); }

bool is_app(free_type exp) {
  return exp.type == PAIR;
}

free_type text_of_quot(free_type exp, free_type env) { return cadr(exp); }

free_type eval_if(free_type exp, free_type env) {
  if (is_true(eval(cadr(exp), env))) return eval(caddr(env), env);
  else return eval(cadddr(exp), env);
}

bool is_empty_env(free_type env) {
  return env.type == ENV && is_null(env);
}

free_type first_frame(free_type env) {
  return car(env);
}

free_type enclose_env(free_type env) {
  return cdr(env);
}

free_type frame_vars(free_type frame) {
  return car(frame);
}

free_type frame_vals(free_type frame) {
  return cdr(frame);
}

free_type scan_for_find(free_type, free_type, free_type, free_type);

free_type env_loop_for_find(free_type var, free_type env) {
  if (is_empty_env(env)) return error("Unbound variable -- FIND_VAR", var);
  return scan_for_find(frame_vars(first_frame(env)), frame_vals(first_frame(env)), var, env);
}
free_type scan_for_find(free_type vars, free_type vals, free_type var, free_type env) {
  // log(vars); log(vals); log(env);
  if (is_null(vars)) return env_loop_for_find(var, enclose_env(env));
  else if (eq(car(vars), var)) return car(vals);
  else return scan_for_find(cdr(vars), cdr(vals), var, env);
}
free_type find_var(free_type var, free_type env) { return env_loop_for_find(var, env); }

void scan_for_set(free_type, free_type, free_type, free_type, free_type);

void env_loop_for_set(free_type var, free_type val, free_type env) {
  if (is_empty_env(env)) error("Unbound variable -- FIND_VAR", var);
  scan_for_set(frame_vars(first_frame(env)), frame_vals(first_frame(env)), var, val, env);
}
void scan_for_set(free_type vars, free_type vals, free_type var, free_type val, free_type env) {
  if (is_null(vars)) env_loop_for_set(var, val, enclose_env(env));
  else if (eq(car(vars), var)) set_car(vals, val);
  else scan_for_set(cdr(vars), cdr(vals), var, val, env);
}
void set_var(free_type var, free_type val, free_type env) { env_loop_for_set(var, val, env);}

void scan_for_define(free_type vars, free_type vals, free_type var, free_type val, free_type frame) {
  if (is_null(vars)) {
    set_car(frame, cons(var, car(frame)));
    set_cdr(frame, cons(val, cdr(frame)));
  }
  else if (eq(car(vars), var)) set_car(vals, val);
  else scan_for_define(cdr(vars), cdr(vals), var, val, frame);
}
void define_var(free_type var, free_type val, free_type env) {
  scan_for_define(frame_vars(first_frame(env)), frame_vals(first_frame(env)), var, val, first_frame(env));
}

free_type lambda_para(free_type exp) { return cadr(exp); }
free_type lambda_body(free_type exp) { return cddr(exp); }
free_type make_lambda(free_type arg, free_type proc) {
  return cons((free_type) { .type = SYMBOL, .x = "lambda"}, cons(arg, proc));
}

free_type def_var(free_type exp) {
  if (is_symbol(cadr(exp))) return cadr(exp);
  return caadr(exp);
}
free_type def_val(free_type exp) {
  if (is_symbol(cadr(exp))) return caddr(exp);
  return make_lambda(cdadr(exp), cddr(exp));
}

free_type eval_def(free_type exp, free_type env) {
  define_var(def_var(exp), eval(def_val(exp), env), env);
  return ok;
}

free_type op(free_type exp) { return car(exp); }
free_type values(free_type exp, free_type env) {
  if (is_null(exp)) return null;
  return cons(eval(car(exp), env), values(cdr(exp), env));
}

free_type make_proc(free_type param, free_type body, free_type env) {
  return cons((free_type) {.type = SYMBOL, .x = "procedure"}, cons(param, cons(body, cons(env, null))));
}

free_type proc_para(free_type proc) { return cadr(proc); }
free_type proc_body(free_type proc) { return caddr(proc); }
free_type proc_env(free_type proc) { return cadddr(proc); }

free_type eval_seq(free_type seq, free_type env) {
  if (is_null(cdr(seq))) return eval(car(seq), env);
  else {
    eval(car(seq), env);
    return eval_seq(cdr(seq), env);
  }
}

free_type extend_env(free_type vars, free_type vals, free_type env) {
  return cons(cons(vars, vals), env);
}
bool is_primitive(free_type proc) {
  return proc.type == PROC && 
    (strcmp(proc.x, "+") == 0
  || strcmp(proc.x, "-") == 0
  || strcmp(proc.x, "*") == 0
  || strcmp(proc.x, "/") == 0
  || strcmp(proc.x, "%") == 0
  || strcmp(proc.x, "<") == 0
  || strcmp(proc.x, ">") == 0
  || strcmp(proc.x, "=") == 0
  || strcmp(proc.x, "cons") == 0
  || strcmp(proc.x, "car") == 0
  || strcmp(proc.x, "cdr") == 0
  || strcmp(proc.x, "and") == 0
  || strcmp(proc.x, "or") == 0
  || strcmp(proc.x, "not") == 0);
}
bool is_compound(free_type proc) {
  return is_tagged_list(proc, "procedure");
}

double convert_to_double(free_type x) {
  if (x.type == FLOAT) return *(double*)x.x;
  else if (x.type == INT) return (double)*(uint64*)x.x;
  else return error("Wrong type -- CONVERT_TO_DOUBLE", x), 0;
}

uint64 convert_to_int(free_type x) {
  if (x.type == INT) return *(uint64*)x.x;
  else if (x.type == FLOAT) return (uint64)*(double*)x.x;
  else return error("Wrong type -- CONVERT_TO_DOUBLE", x), 0.0;
}

free_type binocular(free_type proc, free_type first, free_type second) {
  free_type ret;
  if ((first.type == FLOAT) || (second.type == FLOAT)) ret.type = FLOAT;
  else if ((first.type == INT) && (second.type == INT)) ret.type = INT;
  else error("Expect a number -- BINOCULAR", first);
  if (ret.type == FLOAT) {
    ret.x = (double*)malloc(sizeof(double));
    if (strcmp(proc.x, "+") == 0)
      *(double*)ret.x = convert_to_double(first) + convert_to_double(second);
    else if (strcmp(proc.x, "-") == 0)
      *(double*)ret.x = convert_to_double(first) - convert_to_double(second);
    else if (strcmp(proc.x, "*") == 0)
      *(double*)ret.x = convert_to_double(first) * convert_to_double(second);
    else if (strcmp(proc.x, "/") == 0)
      *(double*)ret.x = convert_to_double(first) / convert_to_double(second);
    else if (strcmp(proc.x, ">") == 0)
      *(double*)ret.x = convert_to_double(first) > convert_to_double(second);
    else if (strcmp(proc.x, "<") == 0)
      *(double*)ret.x = convert_to_double(first) < convert_to_double(second);
    else if (strcmp(proc.x, "=") == 0)
      *(double*)ret.x = convert_to_double(first) == convert_to_double(second);
  }
  else if (ret.type == INT) {
    ret.x = (uint64*)malloc(sizeof(uint64));
    if (strcmp(proc.x, "+") == 0)
      *(uint64*)ret.x = convert_to_int(first) + convert_to_int(second);
    else if (strcmp(proc.x, "-") == 0)
      *(uint64*)ret.x = convert_to_int(first) - convert_to_int(second);
    else if (strcmp(proc.x, "*") == 0)
      *(uint64*)ret.x = convert_to_int(first) * convert_to_int(second);
    else if (strcmp(proc.x, "/") == 0)
      *(uint64*)ret.x = convert_to_int(first) / convert_to_int(second);
    else if (strcmp(proc.x, ">") == 0)
      *(uint64*)ret.x = convert_to_int(first) > convert_to_int(second);
    else if (strcmp(proc.x, "<") == 0)
      *(uint64*)ret.x = convert_to_int(first) < convert_to_int(second);
    else if (strcmp(proc.x, "=") == 0)
      *(uint64*)ret.x = convert_to_int(first) == convert_to_int(second);
    else if (strcmp(proc.x, "%") == 0)
      *(uint64*)ret.x = convert_to_int(first) % convert_to_int(second);
  }
  return ret;
}

free_type apply_primitive(free_type proc, free_type arg) {
  if (!is_primitive(proc)) error("Expect a primitive procedure -- APPLY_PRIMITIVE", proc);
  if (strcmp(proc.x, "+") == 0
  ||  strcmp(proc.x, "-") == 0
  ||  strcmp(proc.x, "*") == 0
  ||  strcmp(proc.x, "/") == 0
  ||  strcmp(proc.x, "%") == 0
  ||  strcmp(proc.x, ">") == 0
  ||  strcmp(proc.x, "<") == 0
  ||  strcmp(proc.x, "=") == 0) {
    if (is_null(cdr(arg))) return car(arg);
    return apply_primitive(proc, cons(binocular(proc, car(arg), cadr(arg)), cddr(arg)));
  }
  if (strcmp(proc.x, "cons") == 0) {
    return cons(car(arg), cadr(arg));
  }
  if (strcmp(proc.x, "car") == 0) {
    return caar(arg);
  }
  if (strcmp(proc.x, "cdr") == 0) {
    return cdar(arg);
  }
  if (strcmp(proc.x, "and") == 0) {
    for (free_type i = arg; !is_null(i); i = cdr(i))
      if (is_free_type_false(car(i))) return free_type_false;
    return free_type_true;
  }
  if (strcmp(proc.x, "or") == 0) {
    for (free_type i = arg; !is_null(i); i = cdr(i))
      if (is_free_type_true(car(i))) return free_type_true;
    return free_type_false;
  }
  if (strcmp(proc.x, "not") == 0) {
    return is_free_type_false(car(arg)) ?  free_type_true : free_type_false;
  }
  error("Except primitive procedure -- APPLY_PRIMITIVE", proc);
  return null;
}


bool is_left_bracket(char a) { return a == '('; }
bool is_right_bracket(char a) { return a == ')'; }
bool is_bracket(char a) { return is_left_bracket(a) || is_right_bracket(a); }
bool is_number_char(char a) { return ('0' <= a && a <= '9') || a == '.'; }
bool is_symbol_char(char a) { return '!' <= a && a <= '~' && is_bracket(a) == false; }
bool is_selfeval_char(char a) { return is_number_char(a) || is_symbol_char(a); }
bool is_blank(char a) { return a == '\r' || a == '\n' || a == '\t' || a == ' ' || a == 127; }

free_type parse_apply(char*, char*);

free_type parse_eval(char* s, char* s_end) {
  printf("parse_eval: ");
  for (char* i = s; i <= s_end; i++) printf("%c", *i);
  printf("\n");
  free_type ret;
  while (is_blank(*s) && s <= s_end) s++;
  if (s > s_end) return error("Blank string -- PARSE_EVAL", null), null;
  if (is_bracket(*s)) {
    return parse_apply(s, s_end);
  }
  else if (is_selfeval_char(*s)) {
    bool is_symbol = false, is_float = false;
    for (char* i = s; i <= s_end; i++) {
      if (is_number_char(*i) == false) {
        is_symbol = true;
        break;
      }
      else if (*i == '.') {
        is_float = true;
      }
    }
    if (is_symbol == true) {
      ret.type = SYMBOL;
      ret.x = (char*)malloc((s_end - s + 2) * sizeof(char));
      char* j = s, *k = ret.x;
      while (!is_symbol_char(*j)) j++, k++;
      while (j <= s_end && is_symbol_char(*j))
        *k = *j, j++, k++;
      *k = 0;
    }
    else {
      if (is_float) {
        ret.type = FLOAT;
        ret.x = (double*)malloc(sizeof(double));
        *((double*)ret.x) = 0.0;
        char* i = s;
        while (!is_number_char(*i)) i++;
        for (; is_number_char(*i); i++) {
          *((double*)ret.x) = *((double*)ret.x) * 10 + (*i - '0');
        }
        if (*i == '.') {
          double decimal_part = 0.0;
          i = s_end;
          while (!is_number_char(*i)) i--;
          for (; is_number_char(*i); i--)
            decimal_part = decimal_part / 10.0 + (*i - '0');
          *((double*)ret.x) = *((double*)ret.x) + decimal_part / 10.0;
        }
      }
      else {
        ret.type = INT;
        ret.x = (uint64*)malloc(sizeof(uint64));
        *((uint64*)ret.x) = 0;
        char* i = s;
        while (!is_number_char(*i)) i++;
        for (; is_number_char(*i); i++)
          *((uint64*)ret.x) = *((uint64*)ret.x) * 10 + (*i - '0');
      }
    }
  }
  else {
    error("Unknown char -- PARSE_EVAL", null);
    for (char* i = s; i <= s_end; i++) printf("%c", *i);
    printf("\n");
  }
  return ret;
}

free_type parse_apply(char* s, char* s_end) {
  printf("parse_apply: ");
  for (char* i = s; i <= s_end; i++) printf("%c", *i);
  printf("\n");
  free_type ret = null;
  bool met_bracket = false;
  bool passing_selfeval = false;
  uint cnt_bracket = 0;
  char* i = s_end;
  char* last_i = s_end;
  while (met_bracket == false || cnt_bracket != 0) {
    if (is_selfeval_char(*i)) {
      if (passing_selfeval == false && cnt_bracket == 1) last_i = i;
      passing_selfeval = true;
      if (cnt_bracket == 1 && (i == s || !is_selfeval_char(*(i - 1))))
        ret = cons(parse_eval(i, last_i), ret);
    }
    else if (is_left_bracket(*i)) {
      cnt_bracket--, met_bracket = true;
      if (cnt_bracket == 1)
        ret = cons(parse_eval(i, last_i), ret);
    }
    else if (is_right_bracket(*i)) {
      if (passing_selfeval == false && cnt_bracket == 1) last_i = i;
      cnt_bracket++, met_bracket = true;
    }
    else if(is_blank(*i)){
      passing_selfeval = false;
    }
    else {
      error("Unknown char -- PARSE_APPLY", null);
      for (char* i = s; i <= s_end; i++) printf("%c", *i);
      printf("\n");
    }
    i--;
  }
  return ret;
}

free_type empty_env = (free_type) {.type = ENV, .x = (void*)0 };
free_type global_env;

void setup_env() {
  global_env = extend_env(
    cons((free_type) {.type = SYMBOL, .x = "+"},
    cons((free_type) {.type = SYMBOL, .x = "-"},
    cons((free_type) {.type = SYMBOL, .x = "*"},
    cons((free_type) {.type = SYMBOL, .x = "/"},
    cons((free_type) {.type = SYMBOL, .x = "%"},
    cons((free_type) {.type = SYMBOL, .x = ">"},
    cons((free_type) {.type = SYMBOL, .x = "<"},
    cons((free_type) {.type = SYMBOL, .x = "="},
    cons((free_type) {.type = SYMBOL, .x = "cons"},
    cons((free_type) {.type = SYMBOL, .x = "car"},
    cons((free_type) {.type = SYMBOL, .x = "cdr"},
    cons((free_type) {.type = SYMBOL, .x = "and"},
    cons((free_type) {.type = SYMBOL, .x = "or"},
    cons((free_type) {.type = SYMBOL, .x = "not"}, null)))))))))))))),
    cons((free_type) {.type = PROC, .x = "+"},
    cons((free_type) {.type = PROC, .x = "-"},
    cons((free_type) {.type = PROC, .x = "*"},
    cons((free_type) {.type = PROC, .x = "/"},
    cons((free_type) {.type = PROC, .x = "%"},
    cons((free_type) {.type = PROC, .x = ">"},
    cons((free_type) {.type = PROC, .x = "<"},
    cons((free_type) {.type = PROC, .x = "="},
    cons((free_type) {.type = PROC, .x = "cons"},
    cons((free_type) {.type = PROC, .x = "car"},
    cons((free_type) {.type = PROC, .x = "cdr"},
    cons((free_type) {.type = PROC, .x = "and"},
    cons((free_type) {.type = PROC, .x = "or"},
    cons((free_type) {.type = PROC, .x = "not"}, null)))))))))))))),
    empty_env
  );
}

int getcmd(char *buf, int nbuf)
{
  write(2, "> ", 2);
  memset(buf, 0, nbuf);
  gets(buf, nbuf);
  if(buf[0] == 0) // EOF
    return -1;
  return 0;
}

int main(void)
{
  static char buf[100];
  int fd;

  setup_env();
  
  // Ensure that three file descriptors are open.
  while((fd = open("console", O_RDWR)) >= 0){
    if(fd >= 3){
      close(fd);
      break;
    }
  }

  // Read and run input commands.
  while(getcmd(buf, sizeof(buf)) >= 0){
    int pid;
    if((pid = fork()) == 0) {
      log(parse_eval(buf, buf + strlen(buf) - 1));
      display(eval(parse_eval(buf, buf + strlen(buf) - 1), global_env));
    }
    else if (pid == -1) error("Fork error -- MAIN", null);
    wait(0);
  }
  exit(0);
}
