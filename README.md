# narcapi
*narcapi* is a compiler for compiling abstract equational protocol narrations into concrete processes in the applied pi calculus (specifically in [ProVerif](http://prosecco.gforge.inria.fr/personal/bblanche/proverif/)'s untyped syntax).

It is an extension to Sébastien Briais and Uwe Nestmann's concept of knowledge and the associated compiler [spyer](http://sbriais.free.fr/tools/spyer/), that enables specification of arbitrary cryptographic primitives through equational theory.

###### Example
A simple handshake protocol using non-deterministic encryption. (it conviniently also shows all the syntactical constructs of the input language)
```
dec/2; enc/3
dec(enc(x,y,z),y) = x

A,B know (A,B)
A,B share k
private m; A knows m
A generates nA
B generates nB

A->B: enc(m, k, nA)
B->A: enc((m,m), k, nB)
```
Note: Function declarations (as in the first line) are optional.

When compiled using *narcapi*, the following applied pi process is generated.
```
(* Equational theory *)
fun dec/2.
fun enc/3.
equation dec(enc(x, y, z), y) = x.

(* Protocol *)
let B =
  new nB;
  in(B, x_3);
  if dec(x_3, k) = dec(x_3, k) then    <- Well-formed test (Can B decrypt the message sent by A?)
  out(A, enc((dec(x_3, k), dec(x_3, k)), k, nB)); 0.

let A =
  new nA;
  out(B, enc(m, k, nA));
  in(A, x_4);
  if dec(x_4, k) = (m, m) then    <- Consistency check (Was B able to decrypt and encrypt m?)
  if (m, m) = (m, m) then 0.

process
  new k;
  new m;
  !(B | A)
```
#### Usage
```bash
narcapi INPUTPATH [> OUTPUTPATH]
```

Where INPUTPATH is the path/filename of an equational protocol narration and OUTPUTPATH is the path/filename for the resulting applied pi process.

###### Example
```bash
narcapi examples/diffiehellman.epn > examples/diffiehellman.pv
```

#### Compiling the source code
Download and install the [OCaml](https://ocaml.org/) compiler for your platform.

An executable can then be compiled using the following command:
```bash
ocamlc helper.ml tree.ml parser.mli parser.ml lexer.ml knowledge.ml appliedpi.ml main.ml
```

Alternatively, the program can be run as a [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop) (read-eval-print loop) by starting an OCaml top-level, in a folder containing the source files, using the ```ocaml``` command. The *.ocamlinit*-file will automatically include the required dependancies for the OCaml top-level.

#### Input syntax
The syntax of equational protocol narrations is defined below as the non-terminal *P*:
```
M,N ::= a | A | ( [M[,N]*] ) | f( [M[,N]*] )
E ::= A -> B : M
D ::= A[,B]* know[s] M | A[,B]* share M | A generates n | private M
T ::= M EOL* = EOL* N | f/i
S ::= ; | EOL
P3 ::= E S+ P3 | E
P2 ::= D S+ P2 | D | P3
P1 ::= T S+ P1 | T | P2
P ::= S* [P1 S*] EOF
```
- *a* is a name.
- *A* and *B* are agent names.
- *f* is a function symbol.
- *i* is an integer.

Note: In the REPL, "EOF" is replaced with "."
