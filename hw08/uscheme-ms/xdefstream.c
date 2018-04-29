#include "all.h"
/* xdefstream.c 923e */
struct XDefstream {
    Parstream pars;                  /* where input comes from */
};
/* xdefstream.c 923f */
XDefstream xdefstream(Parstream pars) {
    XDefstream xdefs = malloc(sizeof(*xdefs));
    assert(xdefs);
    assert(pars);
    xdefs->pars = pars;
    return xdefs;
}
/* xdefstream.c 923g */
XDefstream filexdefs(const char *filename, FILE *input, Prompts prompts) {
    return xdefstream(parstream(filelines(filename, input), prompts));
}
XDefstream stringxdefs(const char *stringname, const char *input) {
    return xdefstream(parstream(stringlines(stringname, input), NO_PROMPTS));
}
/* xdefstream.c 924a */
XDef getxdef(XDefstream xdr) {
    Par p = getpar(xdr->pars);
    if (p == NULL) 
        return NULL;
    else
        return parsexdef(p, parsource(xdr->pars));
}
