#
/*

	    	C compiler, part 2

	Copyright 1972 Bell Telephone Laboratories, Inc.

*/

#include "c1h.c"

/* BCD: maprel changes the operator from (A OP B) when switched to (B OP A).
 * Thus, less than/greater than are swapped, but equal and not equal
 * are left alone. */
char	maprel[] {	EQUAL, NEQUAL, GREATEQ, GREAT, LESSEQ,
			LESS, GREATQP, GREATP, LESSEQP, LESSP
};

/* BCD: notrel just negates a relational operator */
char	notrel[] {	NEQUAL, EQUAL, GREAT, GREATEQ, LESS,
			LESSEQ, GREATP, GREATQP, LESSP, LESSEQP
};

struct tconst czero { CON, INT, 0, 0};
struct tconst cone  { CON, INT, 0, 1};
struct tconst fczero { SFCON, DOUBLE, 0, 0 };

struct table *tabtab[]
{
	regtab,
	efftab,
	cctab,
	sptab,
	0
};

int	nreg	3;
int	isn	10000;
int	namsiz	8; /* BCD: not used anywhere, same as ncps in pass 1 */
int	*treebase;
struct tnode	*xdel[2];

main(argc, argv)
char *argv[];
{
	int treespace[ossiz];
	struct table *table;
	register *sp, c, *tree;
	extern fout;

	if (argc<4) {
		error("Arg count");
		exit(1);
	}
	if(fopen(argv[1], ascbuf)<0 || fopen(argv[2], binbuf)<0){
		error("Missing temp file");
		exit(1);
	}
	/* BCD: below, creat() had the mode argument added since V3 */
	if ((fout = creat(argv[3], 0666)) < 0) {
		error("Can't create %s", argv[3]);
		exit(1);
	}
	treebase = getw(binbuf);
	if (treebase < treespace) {
		error("Tree space botch");
		exit(1);
	}
	spacemax = &treespace[ossiz];
	/* BCD: Read intermediate form from pass 1. */
	while ((c=getc(ascbuf)) > 0) {
		if(c=='#') {
			/* BCD: Hash signals a tree to be evaluated.
			 * tree is the tree itself
			 * table dictates how to compile it.
			 * line is for debugging.
			 */
			sp = treebase;
			c = getw(binbuf);
			tree = getw(binbuf);
			table = tabtab[getw(binbuf)];
			line = getw(binbuf);
			while(--c >= 0)
				*sp++ = getw(binbuf);
			if (table==0)		/* is switch */
				pswitch(treebase, sp, tree);
			else {
				spacep = sp;
				tree = optim(tree); /* BCD: first optimize the tree */
				nstack = 0;
				rcexpr(tree, table, 0); /* BCD: then evaluate it */
			}
		} else /* BCD: Any other char is emitted as-is to assembler. */
			putchar(c);
	}
	if (nfloat)
		printf(".globl	fltused\n");
	flush();
	exit(nerror!=0);
}

char *match(atree, table, nrleft)
struct tnode *atree;
struct table *table;
{
	int op, d1, d2, t1, t2, dope;
	struct tnode *p2;
	register struct tnode *p1, *tree;
	register struct optab *opt;

	if ((tree=atree)==0)
		return(0);
	if (table==lsptab)
		table = sptab;
	op = tree->op;
	dope = opdope[op];
	if ((dope&LEAF) == 0)
		p1 = tree->tr1;
	else
		p1 = tree;
	t1 = p1->type;
	d1 = dcalc(p1, nrleft);
	if ((dope&BINARY)!=0) {
		p2 = tree->tr2;
		t2 = p2->type;
		d2 = dcalc(p2, nrleft);
	}
	/* BCD: First scan the table for the matching opcode; then scan all
	 * entries in the subtable (tabp) sequentially, looking for the first
	 * match.  The match criteria is given by 4 chars, 2 describing the
	 * left operand and 2 for the right (if BINARY).  The structure for
	 * each is similar.  One byte stores the maximum degree of difficulty;
	 * if this is exceeded, then matching fails.  The other, tabtypN,
	 * is passed with the subtree to notcompat(), which if true will also
	 * fail to match.
	 */
	for (; table->op!=op; table++)
		if (table->op==0)
			return(0);
	for (opt = table->tabp; opt->tabdeg1!=0; opt++) {
		if (d1 > (opt->tabdeg1&077)
		 || (opt->tabdeg1 >= 0100 && (p1->op != STAR)))
			continue;
		if (notcompat(p1, opt->tabtyp1, op)) {
			continue;
		}
		if ((opdope[op]&BINARY)!=0 && p2!=0) {
			if (d2 > (opt->tabdeg2&077)
			 || (opt->tabdeg2 >= 0100) && (p2->op != STAR) )
				continue;
			if (notcompat(p2,opt->tabtyp2, 0))
				continue;
		}
		return(opt);
	}
	return(0);
}

/* BCD: The entry point into the real code generator.
 * atree - tree expression to be evaluated
 * atable - this controls how code is emitted:
 *    regtab - most applicable, puts its result in a register
 *    cctab - used only to set the condition codes.  Used when the
 *       expression is inside a conditional statement (if/while/etc.)
 *    sptab - emit result onto the stack.  Note: this may be PDP-11 specific.
 *       Some architectures do not permit writing a value directly to stack
 *       without need of a register.
 *    efftab - evaluate for side effects only; used for expression statements.
 * reg - regno where the result should be placed.
 *
 * Returns the register number where the result was actually placed.
 * Note, many calls to rcexpr() do not check the return value.
 */
rcexpr(atree, atable, reg)
struct tnode *atree;
struct table *atable;
{
	register r;
	int modf, nargs;
	/* BCD: Note that 'atree' and 'atable' are on the stack, so register
	 * values are declared here since they are frequently used.  This is
	 * not automatically done for you. */
	register struct tnode *tree;
	register struct table *table;

	table = atable;
	if((tree=atree)==0)
		return(0);

	/* BCD: The easier cases are checked first, which don't involve
	 * arithmetic/logical operations. */
	switch (tree->op)  {

	case SETREG:
		nreg = tree->type-1;
		return;

	case CBRANCH:
		cbranch(tree->btree, tree->lbl, tree->cond, 0);
		return(0);

	case INIT:
		if (tree->tr1->op == AMPER)
			tree->tr1 = tree->tr1->tr1;
		if (tree->tr1->op!=NAME && tree->tr1->op!=CON)
			error("Illegal initialization");
		else
			cexpr(tree, regtab, nreg);
		return(0);

	case EXCLA:
		/* BCD: Move the negation of !(A OP B) into OP. */
		if ((opdope[tree->tr1->op] & RELAT) != 0) {
			tree = tree->tr1;
			tree->op = notrel[tree->op - EQUAL];
		}
		break;

	case RFORCE:
		/* BCD: RFORCE just forces its operand into R0.  Used for return
		 * values and the switch statement operand, since those are
		 * implemented by some hardcoded assembler (see 'pswitch'). */
		if((r=rcexpr(tree->tr1, table, reg)) != 0)
			printf("mov%c	r%d,r0\n", isfloat(tree->tr1), r);
		return(0);

	case COMMA:
		/* BCD: Evaluate the first operand of comma for side-effects, then proceed
		 * with just operand 2 */
		rcexpr(tree->tr1, efftab, reg);
		tree = tree->tr2;
		break;

	case CALL:
		/* BCD: CALL tree is used in pass 1.  This is converted to CALL1 (for
		 * externs) or CALL2 (for well known functions) for pass 2. */
		r = 0;
		nargs = 0;
		modf = 0;
		if (tree->tr1->op!=NAME) {	/* get nargs right */
			nargs++;
			nstack++;
		}
		/* BCD: The argument(s) to a function call are the same as a comma
		 * expression, evaluated right to left.  Each argument is pushed
		 * onto the stack via comarg().  Then the call itself is emitted
		 * with a recursive call to cexpr().  Last, popstk() is used to
		 * clear the stack arguments. */
		tree = tree->tr2;
		if(tree->op) {
			while (tree->op==COMMA) {
				r =+ comarg(tree->tr2, &modf);
				tree = tree->tr1;
				nargs++;
			}
			r =+ comarg(tree, &modf);
			nargs++;
		}
		tree = atree;
		tree->op = CALL2;
		if (modf && tree->tr1->op==NAME && tree->tr1->class==EXTERN)
			tree->op = CALL1;
		cexpr(tree, regtab, reg);
		popstk(r);
		nstack =- nargs;
		if (table==efftab || table==regtab)
			return(0);
		r = 0;

		/* BCD: For cctab, 'fixup' will emit the 'tst' instruction afterwards */
		xdel[0] = 0;
		xdel[1] = 0;
		goto fixup;

	case TIMES:
	case DIVIDE:
	case ASTIMES:
	case ASDIV:
		/* BCD: Convert multiply/divide of powers of 2 to shifts. */
		pow2(tree);
	}

	/* BCD: From the Tour:
	 * rcexpr itself picks off some special cases, then calls cexpr to do the real work.
	 * The table scanning really starts here.  reorder() does some specific optimizations. */

	modf = 100;
	tree = reorder(tree, reg, &modf, 0);
	if (modf!=100)
		tree = optim(tree);
	if (table==efftab && tree->op==NAME)
		return(reg);
	if ((r=cexpr(tree, table, reg))>=0)
		return(r);

	/* BCD: If table is not regtab, and it couldn't be evaluated, then
	 * fall back to regtab and try again...  */
	if (table!=regtab)  {
		if((r=cexpr(tree, regtab, reg))>=0) {
	fixup:
		/* BCD: If that succeeded, need to fixup the result. */
			modf = isfloat(tree);
			if (table==sptab || table==lsptab) {
				/* BCD: For sptab, push register onto stack */
				printf("mov%c	r%d,%c(sp)\n", modf, r,
					table==sptab? '-':0);
				nstack++;
			}
			atree = xdel[1];
			xdel[1] = 0;
			if (xdel[0]) {
				tree = xdel[0];
				xdel[0] = 0;
				rcexpr(tree, efftab, 0);
			}
			if (atree)
				rcexpr(atree, efftab, 0);
			if (table==cctab) /* BCD: For cctab, emit tst instruction */
				printf("tst%c	r%d\n", modf, r);
			return(r);
		}
	}
	error("No match for op %d", tree->op);
	return(reg);
}
struct table *cregtab;

/* BCD: From the Tour:
 * cexpr tries to find an entry applicable to the given tree in the given table, and
 * returns -1 if no such entry is found, letting rcexpr try again with a different
 * table.  A successful match yields a string containing both literal characters
 * which are written out and pseudo-operations, or macros, which are expanded.
 * BCD: The return value when matching returns a register number >=0. */
cexpr(atree, table, areg) /* BCD: areg short for "argument register" */
struct tnode *atree;
struct table *table;
{
	int c, r;
	register struct tnode *p, *p1, *tree;
	struct table *ctable;
	struct tnode *p2;
	char *string;
	int reg, reg1, rreg, flag, opd;
	char *opt;
	struct tnode *del[2];

	tree = atree;
	del[0] = 0;
	del[1] = 0;
	reg = areg;
	p1 = tree->tr2;
	c = tree->op;
	opd = opdope[c];

	/* BCD: Some special cases first, where the emitted code requires some conditional
	 * branch statements.  When a conditional must be converted into a value, 0 or 1,
	 * use predeclared tree nodes that store those. */

	if ((opd&RELAT||c==LOGAND||c==LOGOR||c==EXCLA) && table!=cctab) {
		cbranch(tree, c=isn++, 1, reg);
		rcexpr(&czero, table, reg);
		branch(isn, 0);
		label(c);
		rcexpr(&cone, table, reg);
		label(isn++);
		return(reg);
	}
	if(c==QUEST) {
		if (table==cctab)
			return(-1);
		cbranch(tree->tr1, c=isn++, 0, reg);
		flag = nstack;
		rreg = rcexpr(p1->tr1, table, reg);
		nstack = flag;
		branch(r=isn++, 0);
		label(c);
		reg = rcexpr(p1->tr2, table, rreg);
		/* BCD: Below, new from V3->V5: rcexpr may return in a different reg
		 * than requested, so check for that. */
		if (rreg!=reg)
			printf("mov%c	r%d,r%d\n",
			    isfloat(tree),reg,rreg);
		reg = rreg;
		label(r);
		goto retrn;
	}
	reg = oddreg(tree, reg);
	reg1 = reg+1;
	if ((r = chkleaf(tree, table, reg)) >= 0)
		return(r);
	r = 0;
	if (table==cctab || table==cregtab)
		r++;

	/* BCD: From the Tour:
	 * Only three features of the operands are used in deciding whether a match has
	 * occurred.  They are: 1. Is the type of the operand compatible with that demanded?
	 * 2. Is the `degree of difficulty' (in a sense described below) compatible?
	 * 3. The table may demand that the operand have a `*' (indirection operator) as its
	 * highest operator. */

	for (;;) {
		flag = 0;
		if ((opd & LEAF) == 0)
			p1 = tree->tr1 = reorder(tree->tr1, areg, &flag,
			    r?0: &del[0]);
		p2 = 0;
		if (opd&BINARY)
			p2 = tree->tr2 = reorder(tree->tr2, areg, &flag,
			    r?0: &del[1]);
		if (flag==0)
			break;
		if (flag > 1 && (opd&RELAT) && p2->op==CON
		 && p2->value==0 && r && opdope[p1->op]&LEAF
		 && del[0]==0 && del[1]==0)
			goto retrn;
		tree = optim(tree);
	}

	if ((tree->op==PLUS||tree->op==ASPLUS) &&
	    p2->op == CON && p2->value == -1) {
		p2->value = 1;
		tree->op++;		/* +, =+ to -, =- */
	}

	/* BCD: Call match() to find the first option that matches the tree.
	 */
	if (table==cregtab)
		table = regtab;
	if (table!=cctab || c==INCAFT || c==DECAFT
	 || (opt = match(tree, efftab, nreg-reg)) == 0)
		if ((opt=match(tree, table, nreg-reg))==0) {
			xdel[0] = del[0];
			xdel[1] = del[1];
			return(-1); /* BCD: no match found */
		}

	/* BCD: If a match is found, perform the expansion indicated by
	 * tabstring. */
	string = opt->tabstring;
loop:
	if ((c = *string++) & 0200) {
		/* BCD: when bit 8 of a 7-bit ASCII character is set, use that
		 * to insert a tab onto the output. */
		c =& 0177;
		putchar('\t');
	}
	switch (c) {

	case '\0':
	retrn:
		/* BCD: Evaluate any delayed expressions that were generated here. */
		if (del[0])
			rcexpr(del[0], efftab, 0);
		if (del[1])
			rcexpr(del[1], efftab, 0);
		if (!isfloat(tree))
			if (tree->op==DIVIDE || tree->op==ASDIV)
				reg--;
		return(reg); /* BCD: success! */

	/* BCD: The values in the comments are what you put into the .s table files.
	 * The cvopt program then converts these into a more compact form, using the
	 * codes that are in the case statements.  This situation existed even in V2. */

	/* A1 */
	case 'A':
		p = p1;
		goto adr;

	/* A2 */
	case 'B':
		p = p2;
		goto adr;

	/* A */
	case 'O':
		p = tree;
	adr:
		c = 0;
		if (*string=='\'') {
			c++;
			string++;
		}
		pname(p, c); /* BCD: print operand (either tree, or its left/right operands */
		goto loop;

	/* I */
	case 'M':
		if ((c = *string)=='\'')
			string++;
		else
			c = 0;
		prins(tree->op, c); /* BCD: print operation mnemonic from tree op */
		goto loop;

	/* B1 */
	case 'C':
		if ((opd&LEAF) != 0)
			p = tree;
		else
			p = p1;
		goto pbyte;

	/* BF */
	case 'P':
		p = tree;
		goto pb1;

	/* B2 */
	case 'D':
		p = p2;
	pbyte:
		if (p->type==CHAR)
			putchar('b');
	pb1:
		if (isfloat(p))
			putchar('f');
		goto loop;

	/* BE */
	case 'L':
		if (p1->type==CHAR || p2->type==CHAR)
			putchar('b');
		p = tree;
		goto pb1;

	/* C1 */
	case 'E':
		p = p1->tr1;
		goto Yconst;

	/* C2 */
	case 'F':
		p = p2->tr1;
	Yconst:
		printf("%o", p); /* BCD: print octal constant in left/right operand */
		goto loop;

	/* F */
	case 'G':
		p = p1;
		flag = 01;
		goto subtre;

	/* S */
	case 'K':
		p = p2;
		flag = 02;
		goto subtre;

	/* H */
	case 'H':
		p = tree;
		flag = 04;

	subtre:
		ctable = regtab;
		c = *string++ - 'A';
		if ((c&02)!=0)
			ctable = sptab;
		if ((c&04)!=0)
			ctable = cctab;
		if ((flag&01) && ctable==regtab && (c&01)==0
		  && (tree->op==DIVIDE||tree->op==MOD
		   || tree->op==ASDIV||tree->op==ASMOD))
			ctable = cregtab;
		if ((c&01)!=0) {
			p = p->tr1;
			if(collcon(p) && ctable!=sptab) {
				if (p->op==STAR)
					p = p->tr1;
				p = p->tr1;
			}
		}
		if (table==lsptab && ctable==sptab)
			ctable = lsptab;
		if (c&010)
			r = reg1;
		else
			if (opdope[p->op]&LEAF || p->degree < 2)
				r = reg;
			else
				r = areg;
		rreg = rcexpr(p, ctable, r);
		if (ctable!=regtab && ctable!=cregtab)
			goto loop;
		if (c&010)
			reg1 = rreg;
		else if (rreg!=reg)
			if (oddreg(tree, 0)==0 && (flag&04 ||
			      flag&01
			  && xdcalc(p2, nreg-rreg-1) <= (opt->tabdeg2&077)
			 ||   flag&02
			  && xdcalc(p1,nreg-rreg-1) <= (opt->tabdeg1&077))) {
				reg = rreg;
				reg1 = rreg+1;
			} else
				printf("mov%c\tr%d,r%d\n",
				    isfloat(tree), rreg, reg);
		goto loop;

	/* R */
	case 'I': /* BCD: use "reg" */
		r = reg;
		if (*string=='-') {
			string++;
			r--; /* BCD: R- will allocate two registers for multiply/divide */
		}
		goto preg;

	/* R1 */
	case 'J': /* BCD: use "reg1" computed earlier from F/S/H macro */
		r = reg1;
	preg:
		if (r>nreg)
			error("Register overflow: simplify expression");
		printf("r%d", r);
		goto loop;

	case '-':		/* check -(sp) */
		if (*string=='(') {
			nstack++;
			if (table!=lsptab)
				putchar('-');
			goto loop;
		}
		break;

	case ')':		/* check (sp)+ */
		putchar(')');
		if (*string=='+')
			nstack--;
		goto loop;

	/* #1 */
	case '#': /* BCD: generic argument 1, could be many things */
		p = p1->tr1;
		goto nmbr;

	/* #2 */
	case '"': /* BCD: generic argument 2 */
		p = p2->tr1;

	nmbr:
		if(collcon(p)) {
			if (p->op==STAR) {
				printf("*");
				p = p->tr1;
			}
			if ((p = p->tr2)->op == CON) {
				if (p->value)
					psoct(p->value);
			} else if (p->op==AMPER)
				pname(p->tr1, 0);
		}
		goto loop;

	/* V */
	case 'V': /* BCD: change relational op when order of operands is swapped (cctab) */
		tree->op = maprel[tree->op - EQUAL];
		goto loop;

	case '^':		/* for ++ --, tr2 is length */
		printf("%o", tree->tr2);
		goto loop;

	case 'T':		/* "tst R" if 1st op not in cctab */
		if (dcalc(p1, 5)>12 && !match(p1, cctab, 10))
			printf("tst	r%d\n", reg);
		goto loop;
	}
	putchar(c);
	goto loop;
}

/* BCD: From the Tour:
 * Reordering is a ... sort of optimization.
 * Many cases which it detects are useful mainly with
 * register variables.
 *
 * The first idea is when compiling "r = x+y", where r is a 
 * register target, this can be rewritten as "r=x; r += y"
 * to avoid a move instruction into r at the end.
 *
 * The second idea is similar for register targets inside
 * subexpressions.
 *
 * There is a third idea here as well, TODO.
 */
reorder(ap, reg, afp, delp)
struct tnode *ap;
int *afp, *delp;
{
	register struct tnode *p, *p1;
	register int *fp;

	p = ap;
	if (opdope[p->op]&LEAF)
		return(p);
	fp = afp;
	p1 = p->tr1;
	if (p->op==STAR || p->op==PLUS) {
		p->tr1 = reorder(p1, reg, fp, delp);
		if (p->op==PLUS)
			p->tr2 = reorder(p->tr2, reg, fp, delp);
		if (*fp)
			*fp = 1;
		return(p);
	}
	if (p1->op==NAME) switch(p->op) {
		case ASLSH:
		case ASRSH:
		case ASSIGN:
			if (p1->class != REG || isfloat(p->tr2))
				break;
			if (p->op==ASSIGN) switch (p->tr2->op) {
			case TIMES:
			case DIVIDE:
				if (!ispow2(p->tr2))
					break;
				pow2(p->tr2);
			case PLUS:
			case MINUS:
			case AND:
			case OR:
			case EXOR:
			case LSHIFT:
			case RSHIFT:
				p1 = p->tr2->tr2;
				if (xdcalc(p1) > 12
				 || p1->op==NAME
				 &&(p1->nloc==p->tr1->nloc
				  || p1->regno==p->tr1->nloc))
					return(p);
				p1 = p->tr2;
				p->tr2 = p1->tr1;
				if (p1->tr1->op!=NAME
				 || p1->tr1->class!=REG
				 || p1->tr1->nloc!=p->tr1->nloc)
					rcexpr(p, efftab, reg);
				p->tr2 = p1->tr2;
				/* BCD: Convert x=x OP y to x OP= y. */
				p->op = p1->op + ASPLUS - PLUS;
				(*fp) = 2;
				return(p);
			}
			goto OK;

		case INCAFT:
		case DECAFT:
			if (delp && *fp < 100) {
				if (p1->op==NAME && p1->class==REG)
					p1 = block(3, p1->op,p1->type,p1->elsize,
					  p1->tr1,p1->offset,p1->nloc);
				*delp = p;
				*fp = 1;
				return(p1);
			}
			break;

		case ASTIMES:
		case ASDIV:
			if (!ispow2(p))
				break;
		case ASPLUS:
		case ASMINUS:
		case ASSAND:
		case ASOR:
		case ASXOR:
		case DECBEF:
		case INCBEF:
		OK:
			if (*fp >= 100)
				break;
			rcexpr(p, delp?efftab:cctab, reg);
			(*fp) = 2;
			return(p1);
	}
	return(p);
}

chkleaf(atree, table, reg)
struct tnode *atree;
{
	struct tnode lbuf;
	register struct tnode *tree;

	tree = atree;
	if (tree->op!=STAR && dcalc(tree, nreg-reg) > 12)
		return(-1);
	lbuf.op = LOAD;
	lbuf.type = tree->type;
	lbuf.degree = tree->degree;
	lbuf.tr1 = tree;
	return(rcexpr(&lbuf, table, reg));
}

comarg(atree, flagp)
int *flagp;
{
	static rathole;
	register struct tnode *tree;
	struct tnode *pmp;
	register retval;

	pmp = 0;
	tree = reorder(atree, 0, &rathole, &pmp);
	/* BCD: structs can't be passed on the stack yet */
	if (tree->type==STRUCT)
		error("Illegal structure");
	if (nstack || isfloat(tree)) {
		rcexpr(tree, sptab, 0);
		retval = arlength(tree->type);
	} else {
		(*flagp)++;
		rcexpr(tree, lsptab, 0);
		retval = 0;
	}
	if (pmp)
		rcexpr(pmp, efftab, 0);
	return(retval);
}
