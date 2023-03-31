#
/* C compiler

Copyright 1972 Bell Telephone Laboratories, Inc. 

*/

#include "c0h.c"

/* BCD: Parse an external definition.
 * Calls decl1() to parse the declarator, and then either
 * cfunc() for a function definition or cinit() for a data
 * definition.  An "extern" will call neither and simply put
 * the name into the symbol table. */
extdef()
{
	register o, width;
	int type, elsize, nel;
	char *cs;
	register struct hshtab *ds;

	if(((o=symbol())==EOF) || o==SEMI)
		return;
	/* BCD: Default type is set to INT.  Default symbol table flag
	 * is FNDEL, which means the symbol should be deleted from the
	 * table when the current block goes out of scope. */
	type = 0;
	xdflg = FNDEL;
    xxx:
	if (o==KEYW) {
		if (cval==EXTERN) {
			/* BCD: "extern foo;" is parsed the same as "foo;". */
			o = symbol();
			goto xxx;
		}
		/* BCD: Acceptable keywords here are all type-related
		 * (int, char, float, double, and struct). */
		if ((type=cval)>STRUCT)
			goto syntax;	/* not type */
		elsize = 0;
		if (type==STRUCT) {
			elsize = strdec(&type, 0);
			if ((peeksym=symbol())!=KEYW)
				blkhed();
		}
	} else {
		/* BCD: Only other token acceptable here is a name.
		 * Push it back until needed. */
		if (o!=NAME)
			goto syntax;
		peeksym = o;
	}

	/* BCD: Now that 'type' is set, parse a list of declarators. */
	do {
		defsym = 0;
		strflg = 1;
		decl1(EXTERN, type&07, 0, elsize);
		if ((ds=defsym)==0)
			return;
		cs = ds->name;
		funcsym = ds;
		ds->hflag = FNDEL;
		printf(".globl	_%.8s\n", cs);
		xdflg = 0;
		type = ds->type;
		if ((type&030)==FUNC) {
			/* BCD: Handle a function declaration.  The declarator "name()"
			 * can be followed by a left brace (for compound statement)
			 * or keyword (for single statement) to make it a definition.
			 * Else, it remains undefined/external. */
			if ((peeksym=symbol())==LBRACE || peeksym==KEYW)
				cfunc(cs);
			return;
		}

		/* BCD: Calculate total number of elements for arrays, including
		 * multidimensional ones. */
		nel = 1;
		while ((ds->type&030)==ARRAY) {
			nel = dimtab[ds->ssp&0377];
			ds->type = decref(ds->type);
		}

		/* BCD: Calculate the (simple type) width.  For structures,
		 * 'nel' will be the total length in bytes, and width is set to 0.
		 * Note width is already guaranteed to be word-aligned, so the
		 * division below will not truncate. */
		width = length(ds);
		if (ds->type==STRUCT) {
			nel =* width/2;
			width = 0;
		}
		ds->type = type;
		/* BCD: Parse the rest of the data declaration */
		cinit(cs, type, nel, width);
	} while ((o=symbol())==COMMA);
	if (o==SEMI)
		return;
syntax:
	error("External definition syntax");
	errflush(o);
	statement(0);
}

cfunc(cs)
char *cs;
{
	register savdimp;

	strflg = 0;
	savdimp = dimp;
	printf(".text\n_%.8s:\n", cs);
	declist(ARG);
	/* BCD: regvar is the minimum register number reserved by the implementation.
	 * r5 is the frame pointer, r6 the stack pointer, and r7 the program counter
	 * on the PDP-11. */
	regvar = 5;
	retlab = isn++;
	if ((peeksym = symbol()) != LBRACE)
		error("Compound statement required");
	statement(1);
	printf("L%d:jmp	rretrn\n", retlab);
	dimp = savdimp;
}

cinit(cs, type, nel, awidth)
char *cs;
{
	register o, ninit, width;

	/* BCD: Emit a .comm statement to declare an uninitialized variable (BSS).
	 * Note the '=' sign is not required in initializers yet. */
	if ((width = awidth) == 0)
		width = 2;
	if ((peeksym=symbol())==COMMA || peeksym==SEMI) {
		printf(".comm	_%.8s,%o\n", cs, (nel*width+1)&~01);
		return;
	}

	/* BCD: Emit 1 or more initial values; require {} around struct/array initializers. */
	ninit = 0;
	printf(".data\n_%.8s=.\n", cs);
	if ((o=symbol())==LBRACE) {
		do
			ninit = cinit1(cs, type, awidth, ninit);
		while ((o=symbol())==COMMA);
		if (o!=RBRACE)
			peeksym = o;
	} else {
		peeksym = o;
		ninit = cinit1(cs, type, awidth, 0);
	}
	/* BCD: If fewer initializers are provided than the struct/array size, then pad rest. */
	if (ninit<nel)
		printf(".=.+%o\n", (nel-ninit)*width);
	else
		nel = ninit;
	if (nel>1 && (type&030)!=ARRAY && (type&07)!=STRUCT)
		error("Too many initializers");
	/* BCD: Force padding to an even address, if the object has an odd size. */
	if (((nel&width)&01) != 0)
		printf(".even\n");
}

/* BCD: Emit assembler for a simple initializer.  Handle constant
 * string, char, int, float, and double. */
cinit1(cs, type, awidth, ninit)
char *cs;
{
	float sf;
	register struct tnode *s;
	register width;

	if ((peeksym=symbol())==STRING && type==ARRAY+CHAR) {
		peeksym = -1;
		if (ninit)
			bxdec();
		printf(".text\n_%.8s=L%d\n", cs, cval);
		return((nchstr+1) & ~01);
	}
	if (peeksym==RBRACE)
		return(ninit);
	initflg++;
	s = tree();
	if ((width = awidth) == 0)
		width = length(s);
	initflg = 0;
	switch(width) {

	case 1:
		printf(".byte ");
		if (s->op != CON)
			bxdec();

	case 2:
		if (s->op==CON) {
			printf("%o\n", s->value);
			break;
		}
		rcexpr(block(1,INIT,0,0,s), regtab);
		break;

	case 4:
		sf = fcval;
		printf("%o;%o\n", sf);
		goto flt;

	case 8:
		printf("%o;%o;%o;%o\n", fcval);
		if (awidth==0)
			ninit =+ 3;
	flt:
		if (s->op==FCON || s->op==SFCON)
			break;

	default:
		bxdec();

	}
	return(++ninit);
}

bxdec()
{
	error("Inconsistent external initialization");
}

/* BCD: Parse statement.  d is 1 when a function block is expected. */
statement(d)
{
	register o, o1, o2;
	int o3, o4;
	struct tnode *np;

stmt:
	switch(o=symbol()) {

	case EOF:
		error("Unexpected EOF");
	case SEMI:
	case RBRACE: /* BCD: A stray right brace is just ignored, also see below */
		return;

	case LBRACE:
		if (d) {
			o2 = blkhed() - 4;
			/* BCD: Call profiler if enabled */
			if (proflg)
				o = "jsr\tr5,mrsave;0f;%o\n.bss\n0:.=.+2\n.text\n";
			else
				o = "jsr	r5,rsave; %o\n";
			printf(o, o2);
		}
		while (!eof) {
			if ((o=symbol())==RBRACE)
				return;
			peeksym = o;
			statement(0);
		}
		error("Missing '}'");
		return;

	case KEYW:
		switch(cval) {

		case GOTO:
			/* BCD: If the argument is just a name/label, then use 'simplegoto' to
			 * emit a jump to it.  Otherwise the argument can be any expression,
			 * which is evaluated to an address... this is 'computed goto'. */
			if (o1 = simplegoto())
				branch(o1);
			else 
				dogoto();
			goto semi;

		case RETURN:
			doret();
			goto semi;

		case IF:
			np = pexpr();
			o2 = 0;

			/* BCD: Optimize certain if statements */
			if ((o1=symbol())==KEYW) switch (cval) {
			case GOTO:
				if (o2=simplegoto())
					goto simpif;
				cbranch(np, o2=isn++, 0);
				dogoto();
				label(o2);
				goto hardif;

			case RETURN:
				if (nextchar()==';') {
					o2 = retlab;
					goto simpif;
				}
				cbranch(np, o1=isn++, 0);
				doret();
				label(o1);
				o2++;
				goto hardif;

			case BREAK:
				o2 = brklab;
				goto simpif;

			case CONTIN:
				o2 = contlab;
			simpif:
				/* BCD: When the if statement is a simple jump, either by void return,
				 * goto, break, or continue, just use 'cbranch' to evalate and
				 * conditionally branch.  Notice the fallthrough... */
				chconbrk(o2);
				cbranch(np, o2, 1);

				/* BCD: When the if statement is a computed goto or value return, the
				 * code was already emitted above, and this does lookahead for an 'else'
				 * block */
			hardif:
				if ((o=symbol())!=SEMI)
					goto syntax;
				if ((o1=symbol())==KEYW && cval==ELSE) 
					goto stmt;
				peeksym = o1;
				return;
			}

			/* Handle if statements that are general statements */
			peeksym = o1;
			cbranch(np, o1=isn++, 0);
			statement(0);
			if ((o=symbol())==KEYW && cval==ELSE) {
				o2 = isn++;
				branch(o2);
				label(o1);
				statement(0);
				label(o2);
				return;
			}
			peeksym = o;
			label(o1);
			return;

		case WHILE:
			o1 = contlab;
			o2 = brklab;
			label(contlab = isn++);
			cbranch(pexpr(), brklab=isn++, 0);
			statement(0);
			branch(contlab);
			label(brklab);
			contlab = o1;
			brklab = o2;
			return;

		case BREAK:
			chconbrk(brklab);
			branch(brklab);
			goto semi;

		case CONTIN:
			chconbrk(contlab);
			branch(contlab);
			goto semi;

		case DO:
			o1 = contlab;
			o2 = brklab;
			contlab = isn++;
			brklab = isn++;
			label(o3 = isn++);
			statement(0);
			label(contlab);
			contlab = o1;
			if ((o=symbol())==KEYW && cval==WHILE) {
				cbranch(tree(), o3, 1);
				label(brklab);
				brklab = o2;
				goto semi;
			}
			goto syntax;

		case CASE:
			o1 = conexp();
			if ((o=symbol())!=COLON)
				goto syntax;
			if (swp==0) {
				error("Case not in switch");
				goto stmt;
			}
			if(swp>=swtab+swsiz) {
				error("Switch table overflow");
			} else {
				swp->swlab = isn;
				(swp++)->swval = o1;
				label(isn++);
			}
			goto stmt;

		case SWITCH:
			o1 = brklab;
			brklab = isn++;
			np = pexpr();
			chkw(np);
			rcexpr(block(1,RFORCE,0,0,np), regtab);
			pswitch();
			brklab = o1;
			return;

		case DEFAULT:
			if (swp==0)
				error("Default not in switch");
			if ((o=symbol())!=COLON)
				goto syntax;
			label(deflab = isn++);
			goto stmt;

		case FOR:
			o1 = contlab;
			o2 = brklab;
			contlab = isn++;
			brklab = isn++;
			if (o=forstmt())
				goto syntax;
			label(brklab);
			contlab = o1;
			brklab = o2;
			return;
		}

		error("Unknown keyword");
		goto syntax;

	case NAME:
		if (nextchar()==':') {
			/* BCD: Parse the label statement.  Note its symbol table entry is marked
			 * as a static array, but its hoffset stores a label number.  The check for
			 * RBRACE is weird. */
			peekc = 0;
			o1 = csym;
			if (o1->hclass>0) {
				error("Redefinition");
				goto stmt;
			}
			o1->hclass = STATIC;
			o1->htype = ARRAY;
			if (o1->hoffset==0)
				o1->hoffset = isn++;
			label(o1->hoffset);
			if ((peeksym=symbol())==RBRACE)
				return;
			goto stmt;
		}
	}

	peeksym = o;
	rcexpr(tree(), efftab);

semi:
	if ((o=symbol())==SEMI)
		return;
syntax:
	error("Statement syntax");
	errflush(o);
	goto stmt;
}

#define	forsps	150

forstmt()
{
	int l, savxpr[forsps];
	int *st, *ss;
	register int *sp1, *sp2, o;

	if ((o=symbol()) != LPARN)
		return(o);
	if ((o=symbol()) != SEMI) {		/* init part */
		peeksym = o;
		rcexpr(tree(), efftab);
		if ((o=symbol()) != SEMI)
			return(o);
	}
	label(contlab);
	if ((o=symbol()) != SEMI) {		/* test part */
		peeksym = o;
		rcexpr(block(1,CBRANCH,tree(),brklab,0), cctab);
		if ((o=symbol()) != SEMI)
			return(o);
	}
	if ((peeksym=symbol()) == RPARN) {	/* incr part */
		peeksym = -1;
		statement(0);
		branch(contlab);
		return(0);
	}
	l = contlab;
	contlab = isn++;
	st = tree();
	if ((o=symbol()) != RPARN)
		return(o);
	ss = space;
	if (space-treebase > forsps) {
		error("Expression too large");
		space = &treebase[forsps];
	}
	sp2 = savxpr;
	for (sp1=treebase; sp1<space;)
		*sp2++ = *sp1++;
	statement(0);
	space = ss;
	sp2 = savxpr;
	for (sp1=treebase; sp1<space;)
		*sp1++ = *sp2++;
	label(contlab);
	rcexpr(st, efftab);
	branch(l);
	return(0);
}

/* BCD: Read a parenthesized expression */
pexpr()
{
	register o, t;

	if ((o=symbol())!=LPARN)
		goto syntax;
	t = tree();
	if ((o=symbol())!=RPARN)
		goto syntax;
	return(t);
syntax:
	error("Statement syntax");
	errflush(o);
	return(0);
}

pswitch()
{
	int *sswp, swlab;
	register int *swb, *wswp, dl;

	swb = sswp = swp;
	if (swp==0)
		swb = swp = swtab;
	branch(swlab=isn++);
	dl = deflab;
	deflab = 0;
	statement(0);
	branch(brklab);
	label(swlab);
	putchar('#');		/* switch is pseudo-expression */
	label(brklab);
	if (!deflab) {
		deflab = isn++;
		label(deflab);
	}
	wswp = swp;
	/* BCD: switch also handled by C01 */
	putw(wswp-swb, binbuf);
	putw(deflab, binbuf);
	putw(4, binbuf);	/* table 4 is switch */
	putw(line, binbuf);
	while (swb < wswp)
		putw(*swb++, binbuf);
	deflab = dl;
	swp = sswp;
}

blkhed()
{
	register pl;
	register struct hshtab *cs;

	autolen = 6;
	declist(0);
	pl = 4;
	while(paraml) {
		parame->hoffset = 0;
		cs = paraml;
		paraml = paraml->hoffset;
		if (cs->htype==FLOAT)
			cs->htype = DOUBLE;
		cs->hoffset = pl;
		cs->hclass = AUTO;
		if ((cs->htype&030) == ARRAY) {
			cs->htype =- 020;	/* set ptr */
			cs->ssp++;		/* pop dims */
		}
		pl =+ rlength(cs);
	}
	for (cs=hshtab; cs<hshtab+hshsiz; cs++) {
		if (cs->name[0] == '\0')
			continue;
		/* check tagged structure */
		if (cs->hclass>KEYWC && (cs->htype&07)==RSTRUCT) {
			/* BCD: Detect references to struct */
			cs->lenp = dimtab[cs->lenp&0377]->lenp;
			cs->htype = cs->htype&~07 | STRUCT;
		}
		if (cs->hclass == STRTAG && dimtab[cs->lenp&0377]==0)
			error("Undefined structure: %.8s", cs->name);
		if (cs->hclass == ARG)
			error("Not an argument: %.8s", cs->name);
	}
	osleft = ossiz;
	space = treebase;
	rcexpr(block(1, SETREG, regvar), regtab);
	return(autolen);
}

blkend() {
	register struct hshtab *cs;

	/* BCD: Remove all local symbols from the hshtab.  (e.g. labels). */
	for (cs=hshtab; cs<hshtab+hshsiz; cs++) {
		if (cs->name[0]) {
			if (cs->hclass==0)
				error("%.8s undefined", cs->name);
			if((cs->hflag&FNDEL)==0) {
				cs->name[0] = '\0';
				hshused--;
			}
		}
	}
}

errflush(ao)
{
	register o;

	/* BCD: When an error occurs, skip all symbols until the next
	 * semicolon or brace. */
	o = ao;
	while(o>RBRACE)	/* ; { } */
		o = symbol();
	peeksym  = o;
}

/* BCD: Parse a list of storage class/type keywords.  skwd below is either 0
 * (for a top-level declaration, MOS (for a * struct definition, or ARG (for
 * a function's argument list).  This is the 'default' storage class.
 * Returns the total amount of space required for all declarations of this
 * type (multiple declarators/names may follow). */
declist(skwd)
{
	int o, elsize, ndec;
	register offset, tkw, skw;

	offset = 0;
loop:
	ndec = 0;
	tkw = -1;
	skw = skwd;
	elsize = 0;
	/* BCD: Parse all keywords of the type definition.  Note that storage classes
	 * and type names can appear in either order.  skw will be set to the storage
	 * keyword and tkw to the type keyword. */
	while ((o=symbol())==KEYW) switch (cval) {
	case AUTO:
	case STATIC:
	case EXTERN:
	case REG:
		if (skw)
			error("Conflict in storage class");
		skw = cval;
		ndec++;
		if (tkw<0)
			continue;
		goto list;

	case STRUCT:
		o = cval;
		elsize = strdec(&o, skw==MOS);
		cval = o;
	case INT:
	case CHAR:
	case FLOAT:
	case DOUBLE:
		ndec++;
		if (tkw>=0)
			error("Type clash");
		tkw = cval;
		if (skw==0)
			continue;
		goto list;

	default:
		goto brk1;
	}
  brk1:
	peeksym = o;
	if (ndec==0)
		return(offset);
list:
	/* BCD: Default storage class is AUTO; default type is INT */
	if (tkw<0)
		tkw = INT;
	if (skw==0)
		skw = AUTO;
	offset = declare(skw, tkw, offset, elsize);
	goto loop;
}

/* BCD: Parse a structure declaration.  mosf is the 'middle of struct' flag;
 * this is true when the declaration appears inside a struct definition. */
strdec(tkwp, mosf)
int *tkwp;
{
	register elsize, o;
	register struct hshtab *ssym;
	struct hshtab *ds;

	mosflg = 1;
	ssym = 0;
	if ((o=symbol())==NAME) {
		ssym = csym;
		if (ssym->hclass==0) {
			ssym->hclass = STRTAG;
			ssym->lenp = dimp;
			chkdim();
			dimtab[dimp++] = 0;
		}
		if (ssym->hclass != STRTAG)
			redec();
		mosflg = mosf;
		o = symbol();
	}
	mosflg = 0;
	if (o != LBRACE) {
		/* BCD: Handle 'struct foo' that isn't followed by a left brace,
		 * i.e. it isn't defined the structure members.  The type is
		 * RSTRUCT, for "reference to struct".  The tag must already be
		 * defined. */
		if (ssym==0) {
		syntax:
			decsyn(o);
			return(0);
		}
		if (ssym->hclass!=STRTAG)
			error("Bad structure name");
		if ((elsize = dimtab[ssym->lenp&0377])==0) {
			*tkwp = RSTRUCT;
			elsize = ssym;
		}
		peeksym = o;
	} else {
		ds = defsym;
		mosflg = 0;
		/* BCD: Parse all of the member declarations.  Align the
		 * total to a word boundary.  Enter this size into 'dimtab'. */
		elsize = declist(MOS);
		if (elsize&01)
			elsize++;
		defsym = ds;
		if ((o = symbol()) != RBRACE)
			goto syntax;
		if (ssym) {
			if (dimtab[ssym->lenp&0377])
				error("%.8s redeclared", ssym->name);
			dimtab[ssym->lenp&0377] = elsize;
		}
	}
	return(elsize);
}

chkdim()
{
	if (dimp >= dimsiz) {
		error("Dimension/struct table overflow");
		exit(1);
	}
}
