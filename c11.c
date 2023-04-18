#
/*
 *  C compiler
 */

#include "c1h.c"

/* BCD: Many helper routines here */

max(a, b)
{
	if (a>b)
		return(a);
	return(b);
}

/* BCD: From the Tour:
 * The `degree of difficulty' computed is actually finer than the mere number of
 * registers required; a constant is considered simpler than the address of a static
 * or external, which is simpler than reference to a variable.
 */
degree(at)
struct tnode *at;
{
	register struct tnode *t;

	if ((t=at)==0 || t->op==0)
		return(0);
	if (t->op==CON || t->op==SFCON)
		return(-3);
	if (t->op == AMPER)
		return(-2);
	if ((opdope[t->op] & LEAF) != 0) {
		if (t->type==CHAR || t->type==FLOAT)
			return(1);
		return(0);
	}
	return(t->degree);
}

/* BCD: Print a tree node, assembler-friendly */
pname(ap, flag)
struct tnode *ap;
{
	register i;
	register struct tnode *p;

	p = ap;
loop:
	switch(p->op) {

	case SFCON:
	case CON:
		printf("$%o", p->value);
		return;

	case FCON:
		printf("L%d", p->value);
		return;

	case NAME:
		if (i = p->offset) {
			psoct(i);
			if (p->class!=OFFS)
				putchar('+');
		}
		switch(p->class) {

		case SOFFS:
		case XOFFS:
			pbase(p);

		case OFFS:
			printf("(r%d)", p->regno);
			return;

		case EXTERN:
		case STATIC:
			pbase(p);
			return;

		case REG:
			printf("r%d", p->nloc);
			return;

		}
		error("Compiler error: pname");
		return;

	case AMPER:
		putchar('$');
		p = p->tr1;
		goto loop;

	case AUTOI:
		printf("(r%d)%c", p->nloc, flag?0:'+');
		return;

	case AUTOD:
		printf("%c(r%d)", flag?0:'-', p->nloc);
		return;

	case STAR:
		p = p->tr1;
		putchar('*');
		goto loop;

	}
	error("pname called illegally");
}

pbase(ap)
struct tnode *ap;
{
	register struct tnode *p;

	p = ap;
	if (p->class==SOFFS || p->class==STATIC)
		printf("L%d", p->nloc);
	else
		printf("_%.8s", &(p->nloc));
}

/* BCD: Yet another variation of dcalc(), which penalizes some
 * byte-mode instructions further. */
xdcalc(ap, nrleft)
struct tnode *ap;
{
	register struct tnode *p;
	register d;

	p = ap;
	d = dcalc(p, nrleft);
	if (d<20 && p->type==CHAR) {
		if (nrleft>=1)
			d = 20;
		else
			d = 24;
	}
	return(d);
}


/* BCD: dcalc() is a layer above degree(), as it is not just based on Sethi-
 * Ullman numbering, but other factors of difficulty.  Floating-point and
 * forms of indirection are penalized further.  Also, if the estimated number
 * of registers needed is bigger than the number available, penalize more.
 * Not sure what the values here actually mean, but it is probably related
 * to PDP-11 cycle counts. */
dcalc(ap, nrleft)
struct tnode *ap;
{
	register struct tnode *p, *p1;

	if ((p=ap)==0)
		return(0);
	switch (p->op) {

	case NAME:
		if (p->class==REG)
			return(9);

	case AMPER:
	case FCON:
	case AUTOI:
	case AUTOD:
		return(12);

	case CON:
	case SFCON:
		return(p->value==0? 4:(p->value==1?5:8));

	case STAR:
		p1 = p->tr1;
		if (p1->op==NAME || p1->op==CON)
			return(12);
	}
	return(p->degree<=nrleft? 20: 24);
}

/* BCD: The main routine for checking if a tree 'ap' has incompatible
 * type with the spec given in 'ast'.  op is the parent operator.
 * This routine could use some preprocessor love.
 * See "types" in c1h.c for the bit definitions, also remember that
 * composite types have multiple values shifted up.
 */
notcompat(ap, ast, op)
struct tnode *ap;
{
	register at, st;
	register struct tnode *p;

	p = ap;
	at = p->type;
	st = ast;

	/* BCD: TODO: check comments below, I think they are all backwards */
	/* BCD: For matching purpose, a struct is treated as an int. */
	if ((at&07)==STRUCT)
		at =& 077770;	/* map to int */

	/* BCD: spec '0' means matches only word types */
	if (st==0)		/* word, byte */
		return(at>1 & at<=07);

	/* BCD: spec '1' means anything but int */
	if (st==1)		/* word */
		return(at>0 & at<=07);

	st =- 2;
	if ((at&077740) != 0)
		at = 020;
	if ((at&077770) != 0)
		at = at&07 | 020;

	/* BCD: spec '2' means don't match float or double */
	if (st==2 && at==3)
		at = 2;
	if (p->op==NAME && p->class==REG && op==ASSIGN && st==CHAR)
		return(0);
	return(st != at);
}

/* BCD: Print instruction name from instab */
prins(op, c) {
	register struct instab *insp;
	register char *ip;

	for (insp=instab; insp->op != 0; insp++) {
		if (insp->op == op) {
			ip = c? insp->str2: insp->str1;
			if (ip==0)
				break;
			printf("%s", ip);
			return;
		}
	}
	error("No match' for op %d", op);
}

collcon(ap)
struct tnode *ap;
{
	register op;
	register struct tnode *p;

	p = ap;
	if (p->op==STAR)
		p = p->tr1;
	if (p->op==PLUS) {
		op = p->tr2->op;
		if (op==CON || op==AMPER)
			return(1);
	}
	return(0);
}

/* BCD: Returns a character to be appended to an instruction mnemonic.
 * Both 'float' and 'double' types will return 'f', else a zero character
 * (which doesn't print anything). */
isfloat(at)
struct tnode *at;
{
	register struct tnode *t;

	t = at;
	if ((opdope[t->op]&RELAT)!=0)
		t = t->tr1;
	if (t->type==FLOAT || t->type==DOUBLE) {
		nfloat = 1;
		return('f');
	}
	return(0);
}

oddreg(t, areg)
struct tnode *t;
{
	register reg;

	reg = areg;
	if (!isfloat(t))
		switch(t->op) {
		case DIVIDE:
		case MOD:
		case ASDIV:
		case ASMOD:
			reg++;

		case TIMES:
		case ASTIMES:
			return(reg|1);
		}
	return(reg);
}

/* BCD: get argument length of a simple type as it appears on the stack. */
arlength(t)
{
	if (t>=PTR)
		return(2);
	switch(t) {

	case INT:
	case CHAR:
		return(2);

	/* BCD: long will be added in v6.  unsigned added in v7. */

	case FLOAT:
	case DOUBLE:
		return(8);
	}
	return(1024);
}

/* BCD: Special handling to emit switch statements.
 * Considerable effort is done here to make switches efficient.  No doubt,
 * the compiler itself depended on these techniques; observe the number
 * of switch statements that are present in the source code here.
 * This is a great example of abstraction being used even in C itself; the
 * implementation of the "switch" is no concern to the programmer.
 * Note in v6, the canned assembler was moved to separate string constants, to
 * make the core logic easier to follow. */
pswitch(afp, alp, deflab)
struct swtab *afp, *alp;
{
	int tlab, ncase, i, j, tabs, worst, best, range;
	register struct swtab *swp, *fp, *lp;
	int poctab[swsiz];

	fp = afp;
	lp = alp;
	/* BCD: Handle edge case when there is only case label, which is the
	 * default. */
	if (fp==lp) {
		printf("jbr	L%d\n", deflab);
		return;
	}
	tlab = isn++;
	if (sort(fp, lp))
		return;
	ncase = lp-fp;
	lp--;

	/* BCD: The table ranges from 'fp' (first value) to 'lp' (last value), inclusive. */
	range = lp->swval - fp->swval;

	/* direct switch */
	if (range>0 && range <= 3*ncase) {
		/* BCD: The canonical implementation of a jump table.  The minimum
		 * value is subtracted off, to yield an index.  If the expression is
		 * higher than any case value, then jump to the default.  Else,
		 * scale the index by a pointer size (2x) and jump indirect.
		 * This implementation works best when the case value are dense over
		 * the range.
		 *
		 * If the range is too large, then this is skipped, to avoid making a
		 * big jump table, when it would mostly have default entries.  It must
		 * be at least 1/3 full of non-default entries. */
		if (fp->swval)
			printf("sub	$%o,r0\n", fp->swval);
		printf("cmp	r0,$%o\n", range);
		printf("jhi	L%d\n", deflab);
		printf("asl	r0\n");
		printf("jmp	*L%d(r0)\n.data\nL%d:", isn, isn);
		isn++;
		for (i=fp->swval; i<=lp->swval; i++) {
			if (i==fp->swval) {
				printf("L%d\n", fp->swlab);
				fp++;
			} else
				printf("L%d\n", deflab);
		}
		goto esw;
	}
	/* simple switch */
	if (ncase<8) {
		/* BCD: Alternate implementation as a variable length of array of
		 * value/label pairs.  At most 8 of these also to save space.  The range
		 * does not have to be continuous though.  Uses indirect addressing.
		 * The range is irrelevant here. */
		i = isn++;
		j = isn++;
		printf("mov	$L%d,r1\n", i);
		printf("mov	r0,L%d\n", j);
		printf("L%d:cmp	r0,(r1)+\n", isn);
		printf("jne	L%d\n", isn++);
		printf("jmp	*L%d-L%d(r1)\n", j, i);
		printf(".data\nL%d:", i);
		for (; fp<=lp; fp++)
			printf("%o\n", fp->swval);
		printf("L%d:..\n", j);
		for (fp = afp; fp<=lp; fp++)
			printf("L%d\n", fp->swlab);
		printf("L%d\n", deflab);
		goto esw;
	}
	/* hash switch */
	best = 077777;
	for (i=ncase/4; i<=ncase/2; i++) {
		for (j=0; j<i; j++)
			poctab[j] = 0;
		for (swp=fp; swp<=lp; swp++)
			poctab[lrem(0, swp->swval, i)]++;
		worst = 0;
		for (j=0; j<i; j++)
			if (poctab[j]>worst)
				worst = poctab[j];
		if (i*worst < best) {
			tabs = i;
			best = i*worst;
		}
	}

	/* BCD: The large switch implementation is a beast.  First it calls a
	 * helper assembly function called 'hsw' to hash the argument in r0.
	 * The return address is put into r2, which allows hsw to see the
	 * top table size in the immediate that follows.
	 * The return is a hash value, which is used  to do a doubly indirect jump.
	 * That is, the hash indexes the first table, emitted by the first for
	 * loop below, to get the address of a second table.  That contains the
	 * actual jump targets.
	 *
	 * The two-level table structure essentially allows for a sparse set of
	 * values to be stored more efficiently.
	 *
	 * The size of the first table is variable; see for loop above that tries
	 * multiple sizes to see which one leads to the most efficient use.
	 *
	 * ldiv/lrem are library functions, not defined in this code.
	 */
	printf("jsr	r2,hsw; %o; L%d\n", tabs, isn);
	printf("jmp	*L%d-L%d(r1)\n", isn+tabs+1, isn+1);
	printf(".data\nL%d:", isn++);
	for (i=0; i<=tabs; i++)
		printf("L%d\n", isn+i);
	for (i=0; i<tabs; i++) {
		printf("L%d:..\n", isn++);
		for (swp=fp; swp<=lp; swp++)
			if (lrem(0, swp->swval, tabs) == i)
				printf("%o\n", ldiv(0, swp->swval, tabs));
	}
	printf("L%d:", isn++);
	for (i=0; i<tabs; i++) {
		printf("L%d\n", deflab);
		for (swp=fp; swp<=lp; swp++)
			if (lrem(0, swp->swval, tabs) == i)
				printf("L%d\n", swp->swlab);
	}
esw:
	printf(".text\n");
}

/* BCD: Sort the array of switch statement values (the 'case's).
 * Note that duplicate entries are detected and cause a compile error. */
sort(afp, alp)
struct swtab *afp, *alp;
{
	register struct swtab *cp, *fp, *lp;
	int intch, t;

	fp = afp;
	lp = alp;
	while (fp < --lp) {
		intch = 0;
		for (cp=fp; cp<lp; cp++) {
			if (cp->swval == cp[1].swval) {
				error("Duplicate case (%d)", cp->swval);
				return(1);
			}
			if (cp->swval > cp[1].swval) {
				intch++;
				t = cp->swval;
				cp->swval = cp[1].swval;
				cp[1].swval = t;
				t = cp->swlab;
				cp->swlab = cp[1].swlab;
				cp[1].swlab = t;
			}
		}
		if (intch==0)
			break;
	}
	return(0);
}

/* BCD: Return tree value if it is an integer constant power of 2. */
ispow2(atree)
{
	register int d;
	register struct tnode *tree;

	tree = atree;
	if (!isfloat(tree) && tree->tr2->op==CON) {
		d = tree->tr2->value;
		if (d>1 && (d&(d-1))==0)
			return(d);
	}
	return(0);
}

/* BCD: Convert multiply/divide into equivalent shift when the
 * operand is a constant integer power of two */
pow2(atree)
struct tnode *atree;
{
	register int d, i;
	register struct tnode *tree;

	tree = atree;
	if (d = ispow2(tree)) {
		for (i=0; (d=>>1)!=0; i++);
		tree->tr2->value = i;
		d = tree->op;
		tree->op = d==TIMES? LSHIFT:
			  (d==DIVIDE? RSHIFT:
			  (d==ASTIMES? ASLSH: ASRSH));
	}
}

/* BCD: Expand a conditional branch on a complex expression.
 * atree is any expression tree, which is taken in boolean context.
 * cond is 0 or 1, and says whether to branch if the expression is
 * false or true.  alabel is the jump target.
 * Note that this involves emitting branch instructions, as well as
 * evaluating expressions.
 */
cbranch(atree, albl, cond, areg)
struct tnode *atree;
{
	int l1, op;
	register lbl, reg;
	register struct tnode *tree;

	lbl = albl;
	reg = areg;
	if ((tree=atree)==0)
		return;
	switch(tree->op) {

	/* BCD: Implement short circuit evaluation for && and ||.
	 * When expression is !x, then just invert cond.
	 * For if (x, y), evaluate x for side effects only, then test y.
	 * cond is passed into the assembler function, which can simply
	 * use the negated opcode, like notrel[]. */
	case LOGAND:
		if (cond) {
			cbranch(tree->tr1, l1=isn++, 0, reg);
			cbranch(tree->tr2, lbl, 1, reg);
			label(l1);
		} else {
			cbranch(tree->tr1, lbl, 0, reg);
			cbranch(tree->tr2, lbl, 0, reg);
		}
		return;

	case LOGOR:
		if (cond) {
			cbranch(tree->tr1, lbl, 1, reg);
			cbranch(tree->tr2, lbl, 1, reg);
		} else {
			cbranch(tree->tr1, l1=isn++, 1, reg);
			cbranch(tree->tr2, lbl, 0, reg);
			label(l1);
		}
		return;

	case EXCLA:
		cbranch(tree->tr1, lbl, !cond, reg);
		return;

	case COMMA:
		rcexpr(tree->tr1, efftab, reg);
		tree = tree->tr2;
		break;
	}
	rcexpr(tree, cctab, reg);
	op = tree->op;
	if ((opdope[op]&RELAT)==0)
		op = NEQUAL;
 	else if (tree->tr2->op==CON && tree->tr2->value==0)
		op =+ 200;		/* special for ptr tests */
	if (isfloat(tree))
		printf("cfcc\n");
	branch(lbl, op, !cond);
}

/* BCD: print branch instruction */
branch(lbl, aop, c)
{
	register op;

	if(op=aop)
		prins(op,c);
	else
		printf("jbr");
	printf("\tL%d\n", lbl);
}

/* BCD: print label */
label(l)
{
	printf("L%d:", l);
}

/* BCD: Pop stack arguments after a function call.  The 2-byte and
 * 4-byte versions probably run faster on the PDP-11.  The tst/cmp
 * are not used later, they are just done to avoid any other side
 * effects. */
popstk(a)
{
	switch(a) {

	case 0:
		return;

	case 2:
		printf("tst	(sp)+\n");
		return;

	case 4:
		printf("cmp	(sp)+,(sp)+\n");
		return;
	}
	printf("add	$%o,sp\n", a);
}

error(s, p1, p2, p3, p4, p5, p6)
{
	register f;
	extern fout;

	nerror++;
	flush();
	f = fout;
	fout = 1;
	printf("%d: ", line);
	printf(s, p1, p2, p3, p4, p5, p6);
	putchar('\n');
	flush();
	fout = f;
}

/* BCD: Print a signed octal */
psoct(an)
{
	register int n, sign;

	sign = 0;
	if ((n = an) < 0) {
		n = -n;
		sign = '-';
	}
	printf("%c%o", sign, n);
}
