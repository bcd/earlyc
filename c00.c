#
/* C compiler
 *
 *
 *
 * Called from cc:
 *   c0 source temp1 temp2 [ profileflag ]
 * temp1 contains some ascii text and the binary expression
 * trees.  Each tree is introduced by the # character.
 * Strings are put on temp2, which cc tacks onto
 * temp1 for assembly.
 */

#include "c0h.c"

int	isn	1;
int	stflg	1;
int	peeksym	-1;
int	line	1;
int	debug	0;
int	dimp	0;
struct	tname	funcblk { NAME, 0, 0, REG, 0, 0 };
int	*treespace { osspace };

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
	/* BCD: 'long' is new in v6. */
	"long",		LONG,
	/* BCD: v7 will add unsigned, union, and short. */
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
	/* BCD: v7 will also add typedef and enum here. */
	0,		0,
};

main(argc, argv)
char *argv[];
{
	extern fin;
	register char *sp;
	register i;
	register struct kwtab *ip;

	if(argc<3) {
		error("Arg count");
		exit(1);
	}
	if((fin=open(argv[1],0))<0) {
		error("Can't find %s", argv[1]);
		exit(1);
	}
	if (fcreat(argv[2], obuf)<0 || fcreat(argv[3], sbuf)<0) {
		error("Can't create temp");
		exit(1);
	}
	if (argc>4)
		proflg++;
	/*
	 * The hash table locations of the keywords
	 * are marked; if an identifier hashes to one of
	 * these locations, it is looked up in in the keyword
	 * table first.
	 */
	for (ip=kwtab; (sp = ip->kwname); ip++) {
		i = 0;
		while (*sp)
			i =+ *sp++;
		hshtab[i%hshsiz].hflag = FKEYW;
	}
	while(!eof) {
		extdef();
		blkend();
	}
	outcode("B", EOF);
	strflg++;
	outcode("B", EOF);
	fflush(obuf);
	fflush(sbuf);
	exit(nerror!=0);
}

/*
 * Look up the identifier in symbuf in the symbol table.
 * If it hashes to the same spot as a keyword, try the keyword table
 * first.  An initial "." is ignored in the hash.
 * Return is a ptr to the symbol table entry.
 */
lookup()
{
	int ihash;
	register struct hshtab *rp;
	register char *sp, *np;

	ihash = 0;
	sp = symbuf;
	if (*sp=='.')
		sp++;
	while (sp<symbuf+ncps)
		ihash =+ *sp++;
	rp = &hshtab[ihash%hshsiz];
	if (rp->hflag&FKEYW)
		if (findkw())
			return(KEYW);
	while (*(np = rp->name)) {
		for (sp=symbuf; sp<symbuf+ncps;)
			if (*np++ != *sp++)
				goto no;
		csym = rp;
		return(NAME);
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
	rp->hflag =| xdflg;
	sp = symbuf;
	for (np=rp->name; sp<symbuf+ncps;)
		*np++ = *sp++;
	csym = rp;
	return(NAME);
}

/*
 * Search the keyword table.
 * Ignore initial "." to avoid member-of-structure
 * problems.
 */
findkw()
{
	register struct kwtab *kp;
	register char *p1, *p2;
	char *wp;

	wp = symbuf;
	/* BCD: Skip over leading dots in symbol names; this would be removed in v7. */
	if (*wp=='.')
		wp++;
	for (kp=kwtab; (p2 = kp->kwname); kp++) {
		p1 = wp;
		while (*p1 == *p2++)
			if (*p1++ == '\0') {
				cval = kp->kwval;
				return(1);
			}
	}
	return(0);
}


/*
 * Return the next symbol from the input.
 * peeksym is a pushed-back symbol, peekc is a pushed-back
 * character (after peeksym).
 * mosflg means that the next symbol, if an identifier,
 * is a member of structure or a structure tag, and it
 * gets a "." prepended to it to distinguish
 * it from other identifiers.
 */
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
			if (spnextchar() != ' '
			 && (c==MINUS || c==AND || c==TIMES)) {
				error("Warning: assignment operator assumed");
				nerror--;
			}
			return(c+ASPLUS-PLUS);
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
		while ((c = spnextchar()) != EOF) {
			peekc = 0;
			if (c=='*') {
				if (spnextchar() == '/') {
					peekc = 0;
					c = getchar();
					goto loop;
				}
			}
		}
		eof++;
			error("Nonterminated comment");
			return(0);

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
		if ((c=lookup())==KEYW && cval==SIZEOF)
			c = SIZEOF;
		return(c);

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

/*
 * If the next input character is c, return a and advance.
 * Otherwise push back the character and return a.
 */
subseq(c,a,b)
{
	if (spnextchar() != c)
		return(a);
	peekc = 0;
	return(b);
}

/*
 * Read a double-quoted string, placing it on the
 * string buffer.
 */
getstr()
{
	register int c;
	register char *sp;

	nchstr = 1;
	sp = savstr;
	while((c=mapch('"')) >= 0) {
		nchstr++;
		if (sp >= &savstr[STRSIZ]) {
			sp = savstr;
			error("String too long");
		}
		*sp++ = c;
	}
	strptr = sp;
	cval = isn++;
	return(STRING);
}

/*
 * Write out a string, either in-line
 * or in the string temp file labelled by
 * lab.
 */
putstr(lab)
{
	register char *sp;

	if (lab) {
		strflg++;
		outcode("BNB", LABEL, lab, BDATA);
	} else
		outcode("B", BDATA);
	for (sp = savstr; sp<strptr; )
		outcode("1N", *sp++ & 0377);
	outcode("100");
	strflg = 0;
}

/*
 * read a single-quoted character constant.
 * The routine is sensitive to the layout of
 * characters in a word.
 */
getcc()
{
	register int c, cc;
	register char *ccp;

	cval = 0;
	ccp = &cval;
	cc = 0;
	/* BCD: Note that multiple characters can be embedded in a character
	 * constant, e.g. 'xy'.  This is permitted up to NCPW (number of
	 * characters per word). */
	while((c=mapch('\'')) >= 0)
		if(cc++ < NCPW)
			*ccp++ = c;
	if(cc>NCPW)
		error("Long character constant");
	return(CON);
}

/*
 * Read a character in a string or character constant,
 * detecting the end of the string.
 * It implements the escape sequences.
 */
mapch(ac)
{
	register int a, c, n;
	static mpeek;

	c = ac;
	if (a = mpeek)
		mpeek = 0;
	else
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

		/* BCD: \f (form feed) and \v (vertical tab) added in v7. */

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

/*
 * Read an expression and return a pointer to its tree.
 * It's the classical bottom-up, priority-driven scheme.
 * The initflg prevents the parse from going past
 * "," or ":" because those delimitesrs are special
 * in initializer (and some other) expressions.
 */
tree()
{
#define	SEOF	200
#define	SSIZE	20
	int *op, opst[SSIZE], *pp, prst[SSIZE];
	register int andflg, o;
	register struct hshtab *cs;
	int p, ps, os;

	space = treespace;
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
		*cp++ = copname(cs);
		goto tand;

	case FCON:
		if (!initflg)
			outcode("BBNB1N1N1N1N0B", DATA,LABEL,
			    cval, WDATA, fcval, PROG);

	case CON:
	case SFCON:
		*cp++ = block(1,o,(o==CON?INT:DOUBLE),0,cval);
		goto tand;

	/* fake a static char array */
	case STRING:
		putstr(cval);
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

oponst:
	p = (opdope[o]>>9) & 077;
	if ((o==COMMA || o==COLON) && initflg)
		p = 05;
opon1:
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
		*++op = o;
		*++pp = p;
		goto advanc;
	}
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
		os = CALL;
		/* BCD: v6 changes a goto to a break, which works just the same */
		break;

	case INCBEF:
	case INCAFT:
	case DECBEF:
	case DECAFT:
		*cp++ = block(1, CON, INT, 0, 1);
		break;

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
	build(os);
	goto opon1;

syntax:
	error("Expression syntax");
	errflush(o);
	return(0);
}

/*
 * Generate a tree node for a name.
 * All the relevant info from the symbol table is
 * copied out, including the name if it's an external.
 * This is because the symbol table is gone in the next
 * pass, so a ptr isn't sufficient.
 */
copname(acs)
struct hshtab *acs;
{
	register struct hshtab *cs;
	register struct tname *tp;
	register char *cp1;
	int i;
	char *cp2;

	cs = acs;
	tp = gblock(sizeof(*tp)/NCPW);
	tp->op = NAME;
	tp->type = cs->htype;
	tp->dimp = cs->hdimp;
	if ((tp->class = cs->hclass)==0)
		tp->class = STATIC;
	tp->offset = 0;
	tp->nloc = cs->hoffset;
	if (cs->hclass==EXTERN) {
		gblock((ncps-NCPW)/NCPW);
		cp1 = tp->nname;
		cp2 = cs->name;
		i = ncps;
		do {
			*cp1++ = *cp2++;
		} while (--i);
	}
	if (cs->hflag&FFIELD)
		tp->class = FMOS;
	return(tp);
}
