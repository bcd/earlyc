#
/* C compiler

Copyright 1972 Bell Telephone Laboratories, Inc. 

*/

#include "c0h.c"

/* BCD: 'isn' is the ID of the next auto-gen label (e.g. L1: etc.)
 * No check for overflow on 'line', it's not gonna happen. */
int	isn 1;
int	peeksym -1;
int	line 1;
int	debug 0;
int	dimp	0;

struct kwtab {
	char	*kwname;
	int	kwval;
} kwtab[]
{
	"int",		INT,
	"char",		CHAR,
	"float",	FLOAT,
	"double",	DOUBLE,
	"struct",	STRUCT,
	"auto",		AUTO,
	"extern",	EXTERN,
	"static",	STATIC,
	"register",	REG,
	"goto",		GOTO,
	"return",	RETURN,
	"if",		IF,
	"while",	WHILE,
	"else",		ELSE,
	"switch",	SWITCH,
	"case",		CASE,
	"break",	BREAK,
	"continue",	CONTIN,
	"do",		DO,
	"default",	DEFAULT,
	"for",		FOR,
	"sizeof",	SIZEOF,
	0,		0,
};

main(argc, argv)
char *argv[];
{
	extern fin, fout;
	/* BCD: In v5, space for trees is allocated on the stack of main; this indicates a
	 * machine with a decent amount of memory.  In v3, tree space was initialized
	 * to 0, which overwrote the beginning of the program, an 'init' function.
	 * v2 did the same overwriting thing, but used a symbol name to indicate
	 * where that could happen, indicating this was before memory management
	 * hardware existed.  sbrk() is used to expand the heap as is traditional
	 * today.  Hardware at the time would have supported virtual memory.
	 *
	 * v5 also uses numerous #define macros, which are lacking in earlier versions,
	 * making this one the oldest & easiest to understand. */
	int treespace[ossiz];
	register char *sp, *np;
	register struct kwtab *ip;

	if(argc<4) {
		error("Arg count");
		exit(1);
	}
	/* BCD: arguments are source file, intermediate tree file, and
	   intermediate strings file. */
	if((fin=open(argv[1],0))<0) {
		error("Can't find %s", argv[1]);
		exit(1);
	}
	if (fcreat(argv[2], &fout)<0 || fcreat(argv[3], binbuf)<0) {
		error("Can't create temp");
		exit(1);
	}
	if (argc>4)
		proflg++;
	xdflg++;
	/* BCD: Put the keywords into the symbol table, so one lookup
	 * will handle user identifiers or keywords.  They are tagged
	 * as keyword class, KEYWC. */
	for (ip=kwtab; (np = ip->kwname); ip++) {
		for (sp = symbuf; sp<symbuf+ncps;)
			if ((*sp++ = *np++) == '\0')
				np--;
		np = lookup();
		np->hclass = KEYWC;
		np->htype = ip->kwval;
	}
	xdflg = 0;
	treebase = treespace+10;
	putw(treebase, binbuf);
	while(!eof) {
		extdef(); /* BCD: read a definition */
		blkend(); /* BCD: end a definition */
	}
	flush();
	fflush(binbuf);
	exit(nerror!=0);
}

/* BCD: Returns symtab entry for a name 'symbuf', or create it if
 * it doesn't exist.  This first computes a hash on the name
 * to determine where in the table to look first. */
struct hshtab *lookup()
{
	int ihash;
	register struct hshtab *rp;
	register char *sp, *np;

	ihash = 0;
	for (sp=symbuf; sp<symbuf+ncps;)
		ihash =+ *sp++;
	rp = &hshtab[ihash%hshsiz];
	while (*(np = rp->name)) {
		for (sp=symbuf; sp<symbuf+ncps;)
			if (*np++ != *sp++) /* BCD: note, no strcmp */
				goto no;
		return(rp);
	no:
		if (++rp >= &hshtab[hshsiz])
			rp = hshtab;
	}
	if(++hshused >= hshsiz) {
		error("Symbol table overflow");
		exit(1);
	}
	rp->hclass = 0;
	rp->htype = 0;
	rp->hoffset = 0;
	rp->dimp = 0;
	rp->hflag = xdflg;
	sp = symbuf;
	for (np=rp->name; sp<symbuf+ncps;)
		*np++ = *sp++;
	return(rp);
}

/* BCD: The scanner; returns one token/symbol at a time.  No lex() yet :-).
 * peeksym can be pushed to peek ahead at the next symbol.
 * peekc similarly allows to peek ahead at the next character. */
symbol() {
	register c;
	register char *sp;

	if (peeksym>=0) {
		c = peeksym;
		peeksym = -1;
		if (c==NAME)
			mosflg = 0;
		return(c);
	}
	if (peekc) {
		c = peekc;
		peekc = 0;
	} else
		if (eof)
			return(EOF);
		else
			c = getchar();
loop:
	switch(ctab[c]) {

	case INSERT:		/* ignore newlines */
		/* BCD: This is ASCII character 0x01 (SOH - Start of Heading) */
		inhdr = 1;
		c = getchar();
		goto loop;

	case NEWLN:
		if (!inhdr)
			line++;
		inhdr = 0;

	case SPACE:
		c = getchar();
		goto loop;

	case EOF:
		eof++;
		return(0);

	case PLUS:
		return(subseq(c,PLUS,INCBEF));

	case MINUS:
		return(subseq(c,subseq('>',MINUS,ARROW),DECBEF));

	case ASSIGN:
		if (subseq(' ',0,1)) return(ASSIGN);
		c = symbol();
		/* BCD: Parse =+ here, instead of +=, etc.  It's easier to parse the '='
		 * and then modify it based on the next character.  Note this can cause
		 * ambiguity when = is followed by a unary minus/star/address of, and
		 * there's even a warning for that happening. */
		if (c>=PLUS && c<=EXOR) {
			if (peekc==0)
				peekc = getchar();
			if (ctab[peekc] != SPACE 
			 && (c==MINUS || c==AND || c==TIMES)) {
				error("Warning: assignment operator assumed");
				nerror--;
			}
			return(c+30);
		}
		if (c==ASSIGN)
			return(EQUAL);
		peeksym = c;
		return(ASSIGN);

	case LESS:
		if (subseq(c,0,1)) return(LSHIFT);
		return(subseq('=',LESS,LESSEQ));

	case GREAT:
		if (subseq(c,0,1)) return(RSHIFT);
		return(subseq('=',GREAT,GREATEQ));

	case EXCLA:
		return(subseq('=',EXCLA,NEQUAL));

	case DIVIDE:
		if (subseq('*',1,0))
			return(DIVIDE);
com:
		c = getchar();
com1:
		switch(c) {
		case '\0':
			eof++;
			error("Nonterminated comment");
			return(0);
		case '\n':
			if (!inhdr)
				line++;
			inhdr = 0;
			goto com;
		case 001:		/* SOH, insert marker */
			inhdr++;
		default:
			goto com;
		case '*':
			c = getchar();
			if (c!='/')
				goto com1;
		}
		c = getchar();
		goto loop;

	case PERIOD:
	case DIGIT:
		peekc = c;
		/* BCD: No supported for hex integers yet, only decimal or octal.
		 * getnum is not defined here, it must be an assembler routine.  Returns
		 * FCON when it parses a float/double; in that case, fcval holds the
		 * 4 words of the value, and cval is set here to a label that acts as a
		 * pointer to it. */
		if ((c=getnum(c=='0'?8:10)) == FCON)
			cval = isn++;
		return(c);

	case DQUOTE:
		return(getstr());

	case SQUOTE:
		return(getcc());

	case LETTER:
		/* BCD: Capture identifier into symbuf */
		sp = symbuf;
		if (mosflg) {
			*sp++ = '.';
			mosflg = 0;
		}
		while(ctab[c]==LETTER || ctab[c]==DIGIT) {
			if (sp<symbuf+ncps) *sp++ = c;
			c = getchar();
		}
		/* BCD: Ensure symbuf is NUL-padded for identifiers less than 8 characters.
		 * Note, an 8-char identifier is not null terminated at all. */
		while(sp<symbuf+ncps)
			*sp++ = '\0';
		peekc = c;
		csym = lookup();
		/* BCD: Return SIZEOF for the sizeof keyword, KEYW for all other keywords,
		 * or NAME for a user defined. */
		if (csym->hclass==KEYWC) {
			if (csym->htype==SIZEOF)
				return(SIZEOF);
			cval = csym->htype;
			return(KEYW);
		}
		return(NAME);

	case AND:
		return(subseq('&', AND, LOGAND));

	case OR:
		return(subseq('|', OR, LOGOR));

	case UNKN:
		error("Unknown character");
		c = getchar();
		goto loop;

	}
	return(ctab[c]);
}

/* BCD: Commonly used in the lexical analyzer, enough to warrant a subroutine.
 * If next character is 'c', then return 'b', else return 'a'.
 * Used when two lexical tokens share a prefix, e.g. + and ++.
 * The order of a, b is not what I would have expected; it is opposite of the
 * ?: operator.
 */
subseq(c,a,b) {
	if (!peekc)
		peekc = getchar();
	if (peekc != c)
		return(a);
	peekc = 0;
	return(b);
}

getstr() {
	register int c;
	register char *t, *d;

	nchstr = 1;
	t = ".text";
	d = ".data";
	printf("%s\nL%d:.byte ", (strflg?t:d), cval=isn++);
	while((c=mapch('"')) >= 0) {
		printf("%o,", c);
		nchstr++;
	}
	printf("0\n.even\n%s\n", (strflg?d:t));
	return(STRING);
}

getcc()
{
	register int c, cc;
	register char *ccp;

	cval = 0;
	ccp = &cval;
	cc = 0;
	/* BCD: Note that multiple characters can be embedded in a character
	 * constant, e.g. 'xy'.  This is permitted up to 'ncpw' (number of
	 * characters per word). */
	while((c=mapch('\'')) >= 0)
		if(cc++ < ncpw)
			*ccp++ = c;
	if(cc>ncpw)
		error("Long character constant");
	return(CON);
}

/* BCD: Parses a character constant that may contain an escape sequence.
 * 'ac' is the terminating character (single or double quote), which
 * causes this to return -1. */
mapch(ac)
{
	register int a, c, n;
	static mpeek;

	c = ac;
	if (mpeek) {
		a = mpeek;
		mpeek = 0;
	} else
		a = getchar();
loop:
	if (a==c)
		return(-1);
	switch(a) {

	case '\n':
	case '\0':
		error("Nonterminated string");
		peekc = a;
		return(-1);

	case '\\':
		switch (a=getchar()) {

		case 't':
			return('\t');

		case 'n':
			return('\n');

		case 'b':
			return('\b');

		case '0': case '1': case '2': case '3':
		case '4': case '5': case '6': case '7':
			n = 0;
			c = 0;
			while (++c<=3 && '0'<=a && a<='7') {
				n =<< 3;
				n =+ a-'0';
				a = getchar();
			}
			mpeek = a;
			return(n);

		case 'r':
			return('\r');

		case '\n':
			if (!inhdr)
				line++;
			inhdr = 0;
			a = getchar();
			goto loop;
		}
	}
	return(a);
}

/* BCD: Parses an expression.  Calls build() to push trees onto the
 * expression stack. */
tree()
{
#define	SEOF	200
#define	SSIZE	20
	int *op, opst[SSIZE], *pp, prst[SSIZE];
	register int andflg, o;
	register struct hshtab *cs;
	int p, ps, os, *np;

	osleft = ossiz;
	space = treebase;
	op = opst;
	pp = prst;
	cp = cmst;
	*op = SEOF;
	*pp = 06;
	andflg = 0;

advanc:
	switch (o=symbol()) {

	/* BCD: Operands are pushed onto the cm stack (cp).  andflg is set
	 * to 1 indicating that the next token cannot also be an operand. */
	case NAME:
		cs = csym;
		if (cs->hclass==0 && cs->htype==0)
			if(nextchar()=='(') {
				/* set function */
				cs->hclass = EXTERN;
				cs->htype = FUNC;
			} else if (initflg)
				cs->hclass = EXTERN;
			else {
				/* set label */
				cs->htype = ARRAY;
				if (cs->hoffset==0)
					cs->hoffset = isn++;
			}
		*cp++ = block(2,NAME,cs->htype,cs->hdimp,
		    cs->hclass,0);
		if (cs->hclass==EXTERN) {
			np = cs->name;
			for (o=0; o<4; o++) {
				pblock(*np);
				if (((*np++)&~0177) == 0)
					break;
			}
		} else
			pblock(cs->hoffset);
		goto tand;

	case FCON:
		if (!initflg)
			printf(".data\nL%d:%o;%o;%o;%o\n.text\n",cval,fcval);

	case CON:
	case SFCON:
		/* BCD: note that the scanner does not return typed constants - only integers
		 * and doubles. */
		*cp++ = block(1,o,(o==CON?INT:DOUBLE),0,cval);
		goto tand;

	/* fake a static char array */
	case STRING:
		*cp++ = block(3, NAME, ARRAY+CHAR,0,STATIC,0,cval);

tand:
		if(cp>=cmst+cmsiz) {
			error("Expression overflow");
			exit(1);
		}
		if (andflg)
			goto syntax;
		andflg = 1;
		goto advanc;

	case INCBEF:
	case DECBEF:
		/* BCD: Converts preincrement to postincrement */
		if (andflg)
			o =+ 2;
		goto oponst;

	case COMPL:
	case EXCLA:
	case SIZEOF:
		/* BCD: None of these are postfix operators */
		if (andflg)
			goto syntax;
		goto oponst;

	case MINUS:
		if (!andflg)  {
			/* BCD: Convert to prefix operator NEG */
			if ((peeksym=symbol())==FCON) {
				fcval = - fcval;
				goto advanc;
			}
			if (peeksym==SFCON) {
				fcval = - fcval;
				cval =^ 0100000;
				goto advanc;
			}
			o = NEG;
		}
		andflg = 0;
		goto oponst;

	case AND:
	case TIMES:
		/* BCD: As prefix operator, convert AND to AMPER and TIMES to STAR. */
		if (andflg)
			andflg = 0; else
			if(o==AND)
				o = AMPER;
			else
				o = STAR;
		goto oponst;

	case LPARN:
		if (andflg) {
			/* BCD: As postfix operator, convert to function call */
			o = symbol();
			if (o==RPARN)
				o = MCALL;
			else {
				peeksym = o;
				o = CALL;
				andflg = 0;
			}
		}
		goto oponst;

	case RBRACK:
	case RPARN:
		if (!andflg)
			goto syntax;
		goto oponst;

	case DOT:
	case ARROW:
		/* BCD: Inform symbol table lookup that next name should be member
		 * of structure. */
		mosflg++;
		break;

	}
	/* binaries */
	if (!andflg)
		goto syntax;
	andflg = 0;

	/* BCD: I'm guessing this means "operator on stack".  Compare priority
	 * of current symbol versus the top of stack symbol. */
oponst:
	/* BCD: p is the priority of the operator (see c04.c).
	 * COMMA is normally 07 and COLON is 14, but this is lowered
	 * when initflg is set, when parsing a constant initializer.  The
	 * comma is then used to separate the initializers of an array.  Colon? */
	p = (opdope[o]>>9) & 077;
	if ((o==COMMA || o==COLON) && initflg)
		p = 05;
opon1:
	/* BCD: pp is the priority stack pointer.  Initial value on stack is 06. */
	ps = *pp;
	if (p>ps || p==ps && (opdope[o]&RASSOC)!=0) {
		switch (o) {

		case INCAFT:
		case DECAFT:
			p = 37;
			break;
		case LPARN:
		case LBRACK:
		case CALL:
			p = 04;
		}
		if (op >= &opst[SSIZE-1]) {
			error("expression overflow");
			exit(1);
		}
		/* BCD: Push operator/priority onto their stacks */
		*++op = o;
		*++pp = p;
		goto advanc;
	}

	/* BCD: Or, pop from stack */
	--pp;
	switch (os = *op--) {

	case SEOF:
		peeksym = o;
		build(0);		/* flush conversions */
		return(*--cp);

	case CALL:
		if (o!=RPARN)
			goto syntax;
		build(os);
		goto advanc;

	case MCALL:
		*cp++ = block(0,0,0,0);	/* 0 arg call */
		/* BCD: MCALL is always turned into CALL eventually. */
		os = CALL;
		goto fbuild;

	case LPARN:
		if (o!=RPARN)
			goto syntax;
		goto advanc;

	case LBRACK:
		if (o!=RBRACK)
			goto syntax;
		build(LBRACK);
		goto advanc;
	}
fbuild:
	build(os);
	goto opon1;

syntax:
	error("Expression syntax");
	errflush(o);
	return(0);
}

/* BCD: Parse a list of declarators/names.  askw and tkw give
 * the storage/type keyword, respectively.  offset is the
 * "address" within the storage area for the first declaration
 * to be assigned.  elsize is nonzero for structs and give
 * their total size.
 * Returns the new offset. */
declare(askw, tkw, offset, elsize)
{
	register int o;
	register int skw;

	skw = askw;
	do {
		offset =+ decl1(skw, tkw, offset, elsize);
		if (xdflg && skw!=MOS)
			return;
	} while ((o=symbol()) == COMMA);
	if (o==SEMI || o==RPARN && skw==ARG1)
		return(offset);
	decsyn(o);
}

/* BCD: Parse a single declarator.  Called from declare() above in
 * a loop. */
decl1(askw, tkw, offset, elsize)
{
	int t1, chkoff;
	register int type, skw;
	register struct hshtab *dsym;

	skw = askw;
	chkoff = 0;
	mosflg = skw==MOS;
	if ((peeksym=symbol())==SEMI || peeksym==RPARN)
		return(0);
	/* BCD: Read the name and any array/pointer notation attached to it.
	 * t1 returns the type info, minus the base type in tkw above. */
	if ((t1=getype()) < 0)
		goto syntax;

	/* BCD: Merge t1 and tkw into a combined type value. */
	type = 0;
	do
		type = type<<2 | (t1 & 030);
	while (((t1=>>2) & 030)!=0);
	type =| tkw;

	/* BCD: Move defsym into local 'dsym', since in a structure type parse,
	 * defsym may change on recursive calls. */
	dsym = defsym;
	if (!(dsym->hclass==0
	   || (skw==ARG && dsym->hclass==ARG1)
	   || (skw==EXTERN && dsym->hclass==EXTERN && dsym->htype==type)))
		if (skw==MOS && dsym->hclass==MOS && dsym->htype==type)
			chkoff = 1;
		else {
			redec();
			goto syntax;
		}
	dsym->htype = type;
	if (skw)
		dsym->hclass = skw;
	if (skw==ARG1) {
		if (paraml==0)
			paraml = dsym;
		else
			parame->hoffset = dsym;
		parame = dsym;
	}
	if (elsize && ((type&07)==RSTRUCT || (type&07)==STRUCT)) {
		/* BCD: Calculate the size of the struct, save that into the next
		 * free entry in 'dimtab', then put the dimtab pointer into 'lenp'. */
		dsym->lenp = dimp;
		chkdim();
		dimtab[dimp++] = elsize;
	}
	elsize = 0;
	if (skw==MOS) {
		elsize = length(dsym);
		if ((offset&1)!=0 && elsize!=1) {
			offset++;
			elsize++;
		}
		if (chkoff && dsym->hoffset != offset)
			redec();
		dsym->hoffset = offset;
	}
	if ((dsym->htype&030)==FUNC) {
		if (dsym->hclass!=EXTERN && dsym->hclass!=AUTO)
			error("Bad function");
		dsym->hclass = EXTERN;
	}

	/* BCD: Set the symbol's hoffset.
	 * For locals, increment 'autolen' which is the total size of all locals.
	 * For statics, use an autogenerated symbol from 'isn' for its address; the
	 *    linker will assign the address.
	 * For register variables, use 'regvar', which is a register number.
	 *    Floats/structs/functions/arrays not allowed.  regvar<3 implies that
	 *    only 3 register variables are allowed (r2, r3, and r4).
	 */
	if (dsym->hclass==AUTO) {
		autolen =+ rlength(dsym);
		dsym->hoffset = -autolen;
	} else if (dsym->hclass==STATIC) {
		dsym->hoffset = isn;
		printf(".bss\nL%d:.=.+%o\n.text\n", isn++, rlength(dsym));
	} else if (dsym->hclass==REG) {
		if ((type&07)>CHAR && (type&030)==0
		 || (type&030)>PTR || regvar<3)
			error("Bad register %o", type);
		dsym->hoffset = --regvar;
	}
syntax:
	return(elsize);
}

/* BCD: Read name plus optional preceding stars and/or succeeding array
 * brackets: the declarator.  Returns a type value if any array/pointer
 * options are included, else 0.  Also sets 'defsym' to the symbol for
 * the name. */
getype()
{
	register int o, type;
	register struct hshtab *ds;

	switch(o=symbol()) {

	case TIMES:
		return(getype()<<2 | PTR);

	case LPARN:
		type = getype();
		if ((o=symbol()) != RPARN)
			goto syntax;
		goto getf;

	case NAME:
		defsym = ds = csym;
		type = 0;
		ds->ssp = dimp;
	getf:
		switch(o=symbol()) {

		case LPARN:
			if (xdflg) {
				xdflg = 0;
				ds = defsym;
				declare(ARG1, 0, 0, 0);
				defsym = ds;
				xdflg++;
			} else
				if ((o=symbol()) != RPARN)
					goto syntax;
			/* BCD: For the type "function returning T", and the like, the
			 * T is always shifted via "type<<2", as the lower 2-bits cover
			 * the simple types: int, char, float, and double.  Struct/rstruct
			 * are not allowed to be returned from functions with the simple
			 * calling conventions implemented at this point, so they are not
			 * considered. */
			type = type<<2 | FUNC;
			goto getf;

		case LBRACK:
			if ((o=symbol()) != RBRACK) {
				/* BCD: Handle array subscripts in declarations, e.g. int x[10].
				 * 'dimtab' is a global table of all dimension constants used in
				 * the program.  This is a space saving technique to avoid reserving
				 * an entire word in each symbol table entry. */
				peeksym = o;
				cval = conexp();
				/* BCD: For multidimension arrays, update the earlier sizes too. */
				for (o=ds->ssp&0377; o<dimp; o++)
					dimtab[o] =* cval;
				dimtab[dimp++] = cval;
				if ((o=symbol())!=RBRACK)
					goto syntax;
			} else
				/* BCD: The size of an unspecified array is 1. */
				dimtab[dimp++] = 1;
			type = type<<2 | ARRAY;
			goto getf;
		}
		peeksym = o;
		return(type);
	}
syntax:
	decsyn(o);
	return(-1);
}

decsyn(o)
{
	error("Declaration syntax");
	errflush(o);
}

redec()
{
	error("%.8s redeclared", defsym->name);
}

