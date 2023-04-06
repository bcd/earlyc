/* C compiler

Copyright 1972 Bell Telephone Laboratories, Inc. 

*/

/* BCD: Tree allocation begins at ospace.  Up to 250 words can be
 * used.  This deliberately overwrites the init() and main()
 * functions after they are not needed anymore -- at least, up to
 * the point of the main loop. */
ossiz 250;
ospace() {}	/* fake */

/* BCD: init() populates all keywords in the symbol table.
 * The first word is later renamed to "hclass", and is set to 1
 * which is KEYWC, indicating a keyword.  The second word is the
 * keyword number, later renamed to "htype".
 *
 * When the code overwriting was eliminated in v5, this code was
 * merged back into main() itself. */
init(s, t)
char s[]; {
	extern lookup, symbuf, namsiz;
	char symbuf[], sp[];
	int np[], i;

	i = namsiz;
	sp = symbuf;
	while(i--)
		if ((*sp++ = *s++)=='\0') --s;
	np = lookup();
	*np++ = 1;
	*np = t;
}

main(argc, argv)
int argv[]; {
	extern init, flush;
	extern extdef, eof, open, creat;
	extern fout, fin, error, exit, nerror, tmpfil;

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
	/* BCD: creat was changed to fcreat in v5. */
	if((fout=creat(argv[2], 017))<0) {
		error("Can't create %s", argv[2]);
		exit(1);
	}
	tmpfil = argv[3];
	init("int", 0);
	init("char", 1);
	init("float", 2);
	init("double", 3);
/*	init("long", 4);  */
	init("auto", 5);
	init("extern", 6);
	init("static", 7);
	init("goto", 10);
	init("return", 11);
	init("if", 12);
	init("while", 13);
	init("else", 14);
	init("switch", 15);
	init("case", 16);
	init("break", 17);
	init("continue", 18);
	init("do", 19);
	init("default", 20);
	while(!eof) {
		extdef();
		blkend();
	}
	flush();
	flshw();
	exit(nerror!=0);
}

/* BCD: Returns symtab entry for a name 'symbuf', or create it if
 * it doesn't exist.  This first computes a hash on the name
 * to determine where in the table to look first. */
lookup() {
	extern hshtab[], hshsiz, pssiz, symbuf[];
	extern hshlen, hshused, exit, error, nwps;
	auto i, j, np[], sp[], rp[];

	i = 0;
	sp = symbuf;
	j = nwps;
	while(j--)
		i =+ *sp++;
	if (i<0) i = -i;
	i =% hshsiz;
	i =* pssiz;
	while(*(np = &hshtab[i+4])) {
		sp = symbuf;
		j = nwps;
		while(j--)
			if (*np++ != *sp++) goto no;
		return(&hshtab[i]);
no:		if ((i =+ pssiz) >= hshlen) i = 0;
	}
	if(hshused++ > hshsiz) {
		error("Symbol table overflow");
		exit(1);
	}
	rp = np = &hshtab[i];
	sp = symbuf;
	j = 4;
	while(j--)
		*np++ = 0;
	j = nwps;
	while(j--)
		*np++ = *sp++;
	return(rp);
}

/* BCD: The scanner; returns one token/symbol at a time.  No lex() yet :-).
 * peeksym can be pushed to peek ahead at the next symbol.
 * peekc similarly allows to peek ahead at the next character. */
symbol() {
	extern peeksym, peekc, eof, getchar, subseq, error, line;
	extern csym[], getstr, symbuf, namsiz, lookup[], ctab, cval;
	auto b, c;
	char symbuf[], sp[], ctab[];

	if (peeksym>=0) {
		c = peeksym;
		peeksym = -1;
		return(c);
	}
	if (peekc) {
		c = peekc;
		peekc = 0;
	} else
		if (eof)
			return(0); else
			c = getchar();
loop:
	switch(ctab[c]) {

	case 125:	/* newline */
		line++;

	case 126:	/* white space */
		c = getchar();
		goto loop;

	case 0:		/* EOF */
		eof++;
		return(0);

	case 40:	/* + */
		return(subseq(c,40,30));

	case 41:	/* - */
		return(subseq(c,41,31));

	case 80:	/* = */
		if (subseq(' ',0,1)) return(80);
		c = symbol();
		/* BCD: Parse =+ here, instead of +=, etc.  It's easier to parse the '='
		 * and then modify it based on the next character.  Note this can cause
		 * ambiguity when = is followed by a unary minus/star/address of, and
		 * there's even a warning for that happening. */
		if (c>=40 & c<=49)
			return(c+30);
		if (c==80)
			return(60);
		peeksym = c;
		return(80);

	case 63:	/* < */
		if (subseq(c,0,1)) return(46);
		return(subseq('=',63,62));

	case 65:	/* > */
		if (subseq(c,0,1)) return(45);
		return(subseq('=',65,64));

	case 34:	/* ! */
		return(subseq('=',34,61));

	case 43:	/* / */
		if (subseq('*',1,0))
			return(43);
com:
		c = getchar();
com1:
		/* BCD: In v5, below becomes a switch statement. */
		if (c=='\0') {
			eof++;
			error("Nonterminated comment");
			return(0);
		}
		if (c=='\n')
			line++;
		if (c!='*')
			goto com;
		c = getchar();
		if (c!='/')
			goto com1;
		c = getchar();
		goto loop;

	case 124:	/* number */
		/* BCD: No supported for hex integers yet, only decimal or octal.
		 * In v5, most of this is moved to a separate function, getnum().  Returns
		 * FCON when it parses a float/double; in that case, fcval holds the
		 * 4 words of the value, and cval is set here to a label that acts as a
		 * pointer to it. */
		cval = 0;
		if (c=='0')
			b = 8; else
			b = 10;
		while(ctab[c]==124) {
			cval = cval*b + c -'0';
			c = getchar();
		}
		peekc = c;
		return(21);

	case 122:	/* " */
		return(getstr());

	case 121:	/* ' */
		return(getcc());

	case 123:	/* letter */
		/* BCD: Capture identifier into symbuf */
		sp = symbuf;
		while(ctab[c]==123 | ctab[c]==124) {
			/* BCD: In v5, namsiz renamed to ncps */
			if (sp<symbuf+namsiz) *sp++ = c;
			c = getchar();
		}
		/* BCD: Ensure symbuf is NUL-padded for identifiers less than 8 characters.
		 * Note, an 8-char identifier is not null terminated at all. */
		while(sp<symbuf+namsiz)
			*sp++ = '\0';
		peekc = c;
		csym = lookup();
		if (csym[0]==1) {	/* keyword */
			/* BCD: Later, sizeof() was added and checked here. */
			cval = csym[1];
			return(19);
		}
		return(20);

		/* BCD: Later, checks for & vs. && and | vs. || were added here. */

	case 127:	/* unknown */
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
	extern getchar, peekc;

	if (!peekc)
		peekc = getchar();
	if (peekc != c)
		return(a);
	peekc = 0;
	return(b);
}
getstr() {
	extern isn, cval;
	auto c;

	printf(".data;L%d:.byte ", cval=isn++);
	while((c=mapch('"')) >= 0)
		printf("%o,", c);
	printf("0;.even;.text\n");
	return(22);
}

getcc()
{
	extern cval, ncpw;
	auto c, cc;
	char cp[];

	cval = 0;
	/* BCD: Below, cp later renamed to ccp to avoid confusion with other meaning of cp
	 * throughout the compiler. */
	cp = &cval;
	cc = 0;
	/* BCD: Note that multiple characters can be embedded in a character
	 * constant, e.g. 'xy'.  This is permitted up to 'ncpw' (number of
	 * characters per word). */
	while((c=mapch('\'')) >= 0)
		if(cc++ < ncpw)
			*cp++ = c;
	if(cc>ncpw)
		error("Long character constant");
	return(21);
}

/* BCD: Parses a character constant that may contain an escape sequence.
 * 'c' is the terminating character (single or double quote), which
 * causes this to return -1. */
mapch(c)
{
	extern peekc, line;
	auto a;

	if((a=getchar())==c)
		return(-1);
	switch(a) {

	case '\n':
	case 0:
		error("Nonterminated string");
		peekc = a;
		return(-1);

	case '\\':
		switch (a=getchar()) {

		case 't':
			return('\t');

		case 'n':
			return('\n');

		case '0':
			/* BCD: Later, support for any octal constant here would be added. */
			return('\0');

		case 'r':
			return('\r');

		case '\n':
			line++;
			return('\n');
		}

	}
	return(a);
}

/* BCD: Parses an expression.  Calls build() to push trees onto the
 * expression stack. */
tree() {
	extern symbol, block, csym[], ctyp, isn,
		peeksym, opdope[], build, error, cp[], cmst[],
		space, ospace, cval, ossiz, exit, errflush, cmsiz;

	auto op[], opst[20], pp[], prst[20], andflg, o,
		p, ps, os;

	space = ospace;
	op = opst;
	pp = prst;
	cp = cmst;
	*op = 200;		/* stack EOF */
	*pp = 06;
	andflg = 0;

advanc:
	switch (o=symbol()) {

	/* name */
	case 20:
		if (*csym==0)
			if((peeksym=symbol())==6)
				*csym = 6;	/* extern */
			else {
				if(csym[2]==0)	/* unseen so far */
					csym[2] = isn++;
			}
			if(*csym==6)	/* extern */
			    *cp++ = block(5,20,csym[1],0,*csym,
					csym[4],csym[5],csym[6],csym[7]);
			else
			    *cp++ = block(2,20,csym[1],0,*csym,csym[2]);
		goto tand;

	/* short constant */
	case 21:
	case21:
		*cp++ = block(1,21,ctyp,0,cval);
		goto tand;

	/* string constant */
	case 22:
		*cp++ = block(1,22,17,0,cval);

tand:
		if(cp>=cmst+cmsiz) {
			error("Expression overflow");
			exit(1);
		}
		if (andflg)
			goto syntax;
		andflg = 1;
		goto advanc;

	/* ++, -- */
	case 30:
	case 31:
		if (andflg)
			o =+ 2;
		goto oponst;

	/* ! */
	case 34:
		/* BCD: complement (~) and sizeof() were added here later. */
		if (andflg)
			goto syntax;
		goto oponst;

	/* - */
	case 41:
		if (!andflg) {
			peeksym = symbol();
			if (peeksym==21) {
				peeksym = -1;
				cval = -cval;
				goto case21;
			}
			o = 37;
		}
		andflg = 0;
		goto oponst;

	/* & */
	/* * */
	case 47:
	case 42:
		if (andflg)
			andflg = 0; else
			if(o==47)
				o = 35;
			else
				o = 36;
		goto oponst;

	/* ( */
	case 6:
		if (andflg) {
			o = symbol();
			if (o==7)
				o = 101; else {
				peeksym = o;
				o = 100;
				andflg = 0;
			}
		}
	goto oponst;

	/* ) */
	/* ] */
	case 5:
	case 7:
		if (!andflg)
			goto syntax;
		goto oponst;
	}

	/* binaries */
	if (!andflg)
		goto syntax;
	andflg = 0;

	/* BCD: I'm guessing this means "operator on stack".  Compare priority
	 * of current symbol versus the top of stack symbol. */
oponst:
	/* BCD: p is the priority of the operator (see c0t.s).
	 * COMMA is normally 07 and COLON is 14, but this is lowered
	 * when initflg is set, when parsing a constant initializer.  The
	 * comma is then used to separate the initializers of an array.  Colon? */
	p = (opdope[o]>>9) & 077;
opon1:
	/* BCD: pp is the priority stack pointer.  Initial value on stack is 06. */
	ps = *pp;
	if (p>ps | p==ps & (opdope[o]&0200)!=0) { /* right-assoc */
putin:
		switch (o) {

		case 6: /* ( */
		case 4: /* [ */
		case 100: /* call */
			p = 04;
		}
		if(op>=opst+20) {		/* opstack size */
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

	/* EOF */
	case 200:
		peeksym = o;
		return(*--cp);

	/* call */
	case 100:
		if (o!=7)
			goto syntax;
		build(os);
		goto advanc;

	/* mcall */
	case 101:
		*cp++ = 0;		/* 0 arg call */
		os = 100;
		goto fbuild;

	/* ( */
	case 6:
		if (o!=7)
			goto syntax;
		goto advanc;

	/* [ */
	case 4:
		if (o!=5)
			goto syntax;
		build(4);
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

declare(kw) {
	extern csym[], symbol, paraml[], parame[];
	extern error, cval, errflush, peeksym, exit;
	int t[], n, o;

	while((o=symbol())==20) {		/* name */
		if(kw>=5) {			/* type or sort? */
			if(*csym>0)
				error("%p redeclared", csym[4]);
			*csym = kw;
		} else {
			if ((csym[1]&017)!=0)
				error("%p redeclared", &csym[4]);
			csym[1] =| csym[1]&0760 | kw;
			if (*csym==0)
				*csym = -2;
		}
		while((o=symbol())==4) {	/* [ */
			if((o=symbol())==21) {	/* const */
				if(csym[1]>=020)
					error("Bad vector");
				csym[3] = cval;
				o = symbol();
			}
			if (o!=5)		/* ] */
				goto syntax;
			csym[1] =+ 020;
		}
		if(kw==8)  {		/* parameter */
			*csym = -1;
			if (paraml==0)
				paraml = csym;
			else
				*parame = csym;
			parame = csym;
		}
		if (o!=9)	/* , */
			break;
	}
	if(o==1 & kw!=8 | o==7 & kw==8)
		return;
syntax:
	error("Declaration syntax");
	errflush(o);
}

/* storage */

regtab 0;
efftab 1;
cctab 2;
sptab 3;
symbuf[4];
pssiz 8;
namsiz 8;
nwps 4;
hshused 0;
hshsiz 100;
hshlen 800;	/* 8*hshsiz */
hshtab[800];
space 0;
cp 0;
cmsiz 40;
cmst[40];
ctyp 0;
isn 1;
swsiz 120;
swtab[120];
swp 0;
contlab 0;
brklab 0;
deflab 0;
nreg 4;
maprel[] 60,61,64,65,62,63,68,69,66,67;
nauto 0;
stack 0;
peeksym 0177777;
peekc 0;
eof 0;
line 1;
csym 0;
cval 0;
ncpw 2;
nerror 0;
paraml;
parame;
tmpfil;

