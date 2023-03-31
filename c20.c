#
/*
 *	 C object code improver
 *	  Copyright 1974 Bell Telephone Laboratories, Incorporated
 */

#include "c2h.c"

struct optab optab[] {
	"jbr",	JBR,
	"jeq",	CBR | JEQ<<8,
	"jne",	CBR | JNE<<8,
	"jle",	CBR | JLE<<8,
	"jge",	CBR | JGE<<8,
	"jlt",	CBR | JLT<<8,
	"jgt",	CBR | JGT<<8,
	"jlo",	CBR | JLO<<8,
	"jhi",	CBR | JHI<<8,
	"jlos",	CBR | JLOS<<8,
	"jhis",	CBR | JHIS<<8,
	"jmp",	JMP,
	".globl",EROU,
	"mov",	MOV,
	"clr",	CLR,
	"com",	COM,
	"inc",	INC,
	"dec",	DEC,
	"neg",	NEG,
	"tst",	TST,
	"asr",	ASR,
	"asl",	ASL,
	"sxt",	SXT,
	"cmp",	CMP,
	"add",	ADD,
	"sub",	SUB,
	"bit",	BIT,
	"bic",	BIC,
	"bis",	BIS,
	"mul",	MUL,
	"ash",	ASH,
	"xor",	XOR,
	".text",TEXT,
	".data",DATA,
	".bss",	BSS,
	".even",EVEN,
	"movf",	MOVF,
	"movof",MOVOF,
	"movfo",MOVFO,
	"addf",	ADDF,
	"subf",	SUBF,
	"divf",	DIVF,
	"mulf",	MULF,
	"clrf",	CLRF,
	"cmpf",	CMPF,
	"negf",	NEGF,
	"tstf",	TSTF,
	"cfcc",	CFCC,
	"sob",	SOB,
	"jsr",	JSR,
	".end",	END,
	0,	0};

/* BCD: revbr equivalent to maprel in the compiler proper */
char	revbr[] { JNE, JEQ, JGT, JLT, JGE, JLE, JHIS, JLOS, JHI, JLO };

int	isn	20000;

main(argc, argv)
char **argv;
{
	register int niter, maxiter, isend;
	extern end;
	extern fin, fout;
	int nflag;

	if (argc>1 && argv[1][0]=='+') {
		argc--;
		argv++;
		debug++;
	}
	if (argc>1 && argv[1][0]=='-') {
		argc--;
		argv++;
		nflag++;
	}
	if (argc>1) {
		if ((fin = open(argv[1], 0)) < 0) {
			printf("C2: can't find %s\n", argv[1]);
			exit(1);
		}
	} else
		fin = dup(0);
	if (argc>2) {
		if ((fout = creat(argv[2], 0666)) < 0) {
			fout = 1;
			printf("C2: can't create %s\n", argv[2]);
			exit(1);
		}
	} else
		fout = dup(1);
	lasta = firstr = lastr = sbrk(2);
	maxiter = 0;
	opsetup();

	/* BCD: The main loop of C2:
	 * Read each line of assembler source, parsing it to a "struct node" which identifies
	 * it as a label or an operation (see table above), and add that onto the scan list.
	 */
	do {
		isend = input();
		movedat();
		niter = 0;
		do {
			refcount(); /* BCD: Link labels/instructions */
			do {
				iterate(); /* BCD: Loop through core optimizations as long as they apply */
				clearreg();
				niter++;
			} while (nchange);
			comjump();
			rmove(); /* BCD: Delete redundant moves */
		} while (nchange || jumpsw());
		addsob(); /* BCD: check if SOB instruction can be inserted here */
		output(); /* BCD: dump the instruction window */
		if (niter > maxiter)
			maxiter = niter; /* BCD: diagnostic to count max # of iterations of the inner loop */
		lasta = firstr;
	} while (isend);
	flush();
	fout = 2;
	if (nflag) {
		printf("%d iterations\n", maxiter);
		printf("%d jumps to jumps\n", nbrbr);
		printf("%d inst. after jumps\n", iaftbr);
		printf("%d jumps to .+2\n", njp1);
		printf("%d redundant labels\n", nrlab);
		printf("%d cross-jumps\n", nxjump);
		printf("%d code motions\n", ncmot); /* BCD: see codemove() */
		printf("%d branches reversed\n", nrevbr); /* BCD: see jumpsw() */
		printf("%d redundant moves\n", redunm); /* BCD: see rmove() */
		printf("%d simplified addresses\n", nsaddr); /* BCD: see repladdr() */
		printf("%d loops inverted\n", loopiv); /* BCD: see codemove() */
		printf("%d redundant jumps\n", nredunj);
		printf("%d common seqs before jmp's\n", ncomj); /* BCD: see backjmp() */
		printf("%d skips over jumps\n", nskip);
		printf("%d sob's added\n", nsob); /* BCD: replace with SOB instruction */
		printf("%d redundant tst's\n", nrtst); /* BCD: see rmove() */
		printf("%dK core\n", ((lastr+01777)>>10)&077);
		flush();
	}
}

input()
{
	register struct node *p, *lastp;
	register int op;

	lastp = &first;
	for (;;) {
		op = getline();
		switch (op.op) {
	
		case LABEL:
			/* BCD: Here it appears that sizeof() only accepts a variable and not a type */
			p = alloc(sizeof first);
			if (line[0] == 'L') {
				p->combop = LABEL;
				p->labno = getnum(line+1);
				p->code = 0;
			} else {
				p->combop = DLABEL;
				p->labno = 0;
				p->code = copy(line);
			}
			break;
	
		case JBR:
		case CBR:
		case JMP:
		case JSW:
			p = alloc(sizeof first);
			p->combop = op;
			if (*curlp=='L' && (p->labno = getnum(curlp+1)))
				p->code = 0;
			else {
				p->labno = 0;
				p->code = copy(curlp);
			}
			break;

		default:
			p = alloc(sizeof first);
			p->combop = op;
			p->labno = 0;
			p->code = copy(curlp);
			break;

		}

		/* BCD: The next line is chained onto a linked list with previous nodes.
		 * See 'first' and 'lastp'. */
		p->forw = 0;
		p->back = lastp;
		lastp->forw = p;
		lastp = p;
		p->ref = 0;

		/* BCD: EROU means ".globl" was seen, and that returns to the caller, so the
		 * optimizer is working on one function at a time. */
		if (op==EROU)
			return(1);
		if (op==END)
			return(0);
	}
}

getline()
{
	register char *lp;
	register c;

	lp = line;
	while (c = getchar()) {
		if (c==':') {
			*lp++ = 0;
			return(LABEL);
		}
		if (c=='\n') {
			*lp++ = 0;
			return(oplook());
		}
		*lp++ = c;
	}
	*lp++ = 0;
	return(END);
}

getnum(ap)
char *ap;
{
	register char *p;
	register n, c;

	p = ap;
	n = 0;
	while ((c = *p++) >= '0' && c <= '9')
		n = n*10 + c - '0';
	if (*--p != 0)
		return(0);
	return(n);
}

/* BCD: Output all the instructions on the scan list. */
output()
{
	register struct node *t;
	register struct optab *op;
	register int byte;

	t = &first;
	while (t = t->forw) switch (t->op) {

	case END:
		return;

	case LABEL:
		printf("L%d:", t->labno);
		continue;

	case DLABEL:
		printf("%s:", t->code);
		continue;

	default:
		if ((byte = t->subop) == BYTE)
			t->subop = 0;
		for (op = optab; op->opstring!=0; op++) 
			if (op->opcode == t->combop) {
				printf("%s", op->opstring);
				if (byte==BYTE)
					printf("b");
				break;
			}
		if (t->code)
			printf("\t%s\n", t->code);
		else if (t->op==JBR || t->op==CBR)
			printf("\tL%d\n", t->labno);
		else
			printf("\n");
		continue;

	case JSW:
		printf("L%d\n", t->labno);
		continue;

	case SOB:
		printf("sob	%s,L%d\n", t->code, t->labno);
		continue;

	case 0:
		if (t->code)
			printf("%s", t->code);
		printf("\n");
		continue;
	}
}

copy(ap)
char *ap;
{
	register char *p, *np;
	char *onp;
	register n;
	int na;

	na = nargs();
	p = ap;
	n = 0;
	if (*p==0)
		return(0);
	do
		n++;
	while (*p++);
	if (na>1) {
		p = (&ap)[1];
		while (*p++)
			n++;
	}
	onp = np = alloc(n);
	p = ap;
	while (*np++ = *p++);
	if (na>1) {
		p = (&ap)[1];
		np--;
		while (*np++ = *p++);
	}
	return(onp);
}

opsetup()
{
	register struct optab *optp, **ophp;
	register int *p;

	for (optp = optab; p = optp->opstring; optp++) {
		ophp = &ophash[((p[0]+p[1].lbyte)&077777) % OPHS];
		while (*ophp++)
			if (ophp > &ophash[OPHS])
				ophp = ophash;
		*--ophp = optp;
	}
}

oplook()
{
	register struct optab *optp;
	register char *lp, *op;
	static char tmpop[32];
	struct optab **ophp;

	op = tmpop;
	for (lp = line; *lp && *lp!=' ' && *lp!='\t';)
		*op++ = *lp++;
	*op++ = 0;
	while (*lp=='\t' || *lp==' ')
		lp++;
	curlp = lp;
	ophp = &ophash[((tmpop[0].int+tmpop[2])&077777) % OPHS];
	while (optp = *ophp) {
		op = optp->opstring;
		lp = tmpop;
		while (*lp == *op++)
			if (*lp++ == 0)
				return(optp->opcode);
		if (*lp++=='b' && *lp++==0 && *--op==0)
			return(optp->opcode + (BYTE<<8));
		ophp++;
		if (ophp >= &ophash[OPHS])
			ophp = ophash;
	}
	if (line[0]=='L') {
		lp = &line[1];
		while (*lp)
			if (*lp<'0' || *lp++>'9')
				return(0);
		curlp = line;
		return(JSW);
	}
	curlp = line;
	return(0);
}

/* BCD: Link instructions with a label operand and the labels themselves. */
refcount()
{
	register struct node *p, *lp;
	static struct node *labhash[LABHS];
	register struct node **hp;

	for (hp = labhash; hp < &labhash[LABHS];)
		*hp++ = 0;
	for (p = first.forw; p!=0; p = p->forw)
		if (p->op==LABEL) {
			labhash[p->labno % LABHS] = p;
			p->refc = 0;
		}
	for (p = first.forw; p!=0; p = p->forw) {
		if (p->op==JBR || p->op==CBR || p->op==JSW) {
			p->ref = 0;
			lp = labhash[p->labno % LABHS];
			if (lp==0 || p->labno!=lp->labno)
			for (lp = first.forw; lp!=0; lp = lp->forw) {
				if (lp->op==LABEL && p->labno==lp->labno)
					break;
			}
			if (lp) {
				hp = nonlab(lp)->back;
				if (hp!=lp) {
					p->labno = hp->labno;
					lp = hp;
				}
				p->ref = lp;
				lp->refc++;
			}
		}
	}
	for (p = first.forw; p!=0; p = p->forw)
		if (p->op==LABEL && p->refc==0
		 && (lp = nonlab(p))->op && lp->op!=JSW)
			decref(p);
}

/* BCD: Main optimization routine.  Called in a loop since one optimization may
 * enable other one.  'nchange' is incremented anytime something changed,
 * telling the caller to iterate() one more time afterwards.  Interestingly,
 * there is no safety maximum here. */
iterate()
{
	register struct node *p, *rp, *p1;

	nchange = 0;
	for (p = first.forw; p!=0; p = p->forw) {
		if ((p->op==JBR||p->op==CBR||p->op==JSW) && p->ref) {
			rp = nonlab(p->ref);
			if (rp->op==JBR && rp->labno) {
				/* BCD: Simplify branch to branch */
				nbrbr++;
				p->labno = rp->labno;
				decref(p->ref);
				rp->ref->refc++;
				p->ref = rp->ref;
				nchange++;
			}
		}
		if (p->op==CBR && (p1 = p->forw)->op==JBR) {
			/* BCD: Skip over jump */
			rp = p->ref;
			do
				rp = rp->back;
			while (rp->op==LABEL);
			if (rp==p1) {
				decref(p->ref);
				p->ref = p1->ref;
				p->labno = p1->labno;
				p1->forw->back = p;
				p->forw = p1->forw;
				p->subop = revbr[p->subop];
				nchange++;
				nskip++;
			}
		}
		if (p->op==JBR || p->op==JMP) {
			/* BCD: Remove unreachable instructions after jump */
			while (p->forw && p->forw->op!=LABEL
				&& p->forw->op!=EROU && p->forw->op!=END) {
				nchange++;
				iaftbr++;
				if (p->forw->ref)
					decref(p->forw->ref);
				p->forw = p->forw->forw;
				p->forw->back = p;
			}
			rp = p->forw;
			while (rp && rp->op==LABEL) {
				if (p->ref == rp) {
					/* BCD: Also remove jump to self, which is a no-op. */
					p->back->forw = p->forw;
					p->forw->back = p->back;
					p = p->back;
					decref(rp);
					nchange++;
					njp1++;
					break;
				}
				rp = rp->forw;
			}
			xjump(p);
			p = codemove(p);
		}
	}
}

/* BCD: Check for cross jumps (???) */
xjump(ap)
{
	register int *p1, *p2, *p3;
	int nxj;

	nxj = 0;
	p1 = ap;
	if ((p2 = p1->ref) == 0)
		return(0);
	for (;;) {
		while ((p1 = p1->back) && p1->op==LABEL);
		while ((p2 = p2->back) && p2->op==LABEL);
		if (!equop(p1, p2))
			return(nxj);
		p3 = insertl(p2);
		p1->combop = JBR;
		p1->ref = p3;
		p1->labno = p3->labno;
		p1->code = 0;
		nxj++;
		nxjump++;
		nchange++;
	}
}

insertl(ap)
struct node *ap;
{
	register struct node *lp, *op;

	op = ap;
	if (op->op == LABEL) {
		op->refc++;
		return(op);
	}
	if (op->back->op == LABEL) {
		op = op->back;
		op->refc++;
		return(op);
	}
	lp = alloc(sizeof first);
	lp->combop = LABEL;
	lp->labno = isn++;
	lp->ref = 0;
	lp->code = 0;
	lp->refc = 1;
	lp->back = op->back;
	lp->forw = op;
	op->back->forw = lp;
	op->back = lp;
	return(lp);
}

codemove(ap)
struct node *ap;
{
	register struct node *p1, *p2, *p3;
	struct node *t, *tl;
	int n;

	p1 = ap;
	if (p1->op!=JBR || (p2 = p1->ref)==0)
		return(p1);
	while (p2->op == LABEL)
		if ((p2 = p2->back) == 0)
			return(p1);
	if (p2->op!=JBR && p2->op!=JMP)
		goto ivloop;
	p2 = p2->forw;
	p3 = p1->ref;
	while (p3) {
		if (p3->op==JBR || p3->op==JMP) {
			if (p1==p3)
				return(p1);
			ncmot++; /* BCD: Code motion */
			nchange++;
			p1->back->forw = p2;
			p1->forw->back = p3;
			p2->back->forw = p3->forw;
			p3->forw->back = p2->back;
			p2->back = p1->back;
			p3->forw = p1->forw;
			decref(p1->ref);
			return(p2);
		} else
			p3 = p3->forw;
	}
	return(p1);
ivloop:
	if (p1->forw->op!=LABEL)
		return(p1);
	p3 = p2 = p2->forw;
	n = 16;
	do {
		if ((p3 = p3->forw) == 0 || p3==p1 || --n==0)
			return(p1);
	} while (p3->op!=CBR || p3->labno!=p1->forw->labno);
	do 
		if ((p1 = p1->back) == 0)
			return(ap);
	while (p1!=p3);
	p1 = ap;
	tl = insertl(p1);
	p3->subop = revbr[p3->subop];
	decref(p3->ref);
	p2->back->forw = p1;
	p3->forw->back = p1;
	p1->back->forw = p2;
	p1->forw->back = p3;
	t = p1->back;
	p1->back = p2->back;
	p2->back = t;
	t = p1->forw;
	p1->forw = p3->forw;
	p3->forw = t;
	p2 = insertl(p1->forw);
	p3->labno = p2->labno;
	p3->ref = p2;
	decref(tl);
	if (tl->refc<=0)
		nrlab--;
	loopiv++; /* BCD: Loop inversion */
	nchange++;
	return(p3);
}

comjump()
{
	register struct node *p1, *p2, *p3;

	for (p1 = first.forw; p1!=0; p1 = p1->forw)
		if (p1->op==JBR && (p2 = p1->ref) && p2->refc > 1)
			for (p3 = p1->forw; p3!=0; p3 = p3->forw)
				if (p3->op==JBR && p3->ref == p2)
					backjmp(p1, p3);
}

backjmp(ap1, ap2)
struct node *ap1, *ap2;
{
	register struct node *p1, *p2, *p3;

	p1 = ap1;
	p2 = ap2;
	for(;;) {
		while ((p1 = p1->back) && p1->op==LABEL);
		p2 = p2->back;
		if (equop(p1, p2)) {
			/* BCD: common sequence before jmp.  My guess is this looks for
			 * an insn that is executed on the true and false paths of a
			 * conditional branch, and moves it before the branch unconditionally. */
			p3 = insertl(p1);
			p2->back->forw = p2->forw;
			p2->forw->back = p2->back;
			p2 = p2->forw;
			decref(p2->ref);
			p2->labno = p3->labno;
			p2->ref = p3;
			nchange++;
			ncomj++;
		} else
			return;
	}
}
