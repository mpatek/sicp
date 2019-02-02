Eva Lu Ator is correct. 

Let's take an example.

Imagine R1 can vary between (9, 11)
R2 between (8, 12)

If R1 = 9 and R2 = 8 then parallel resistence is 72/17 or about 4.235
If R1 = 11 and R2 = 12 then parallel resistence is 132/23 or about 5.739

Both formula 1 and 2 give us the same values in these cases, and these are the 'true' lower/upper bounds for these intervals. The tighter bounds returned by the `par2` procedure exactly match the bounds found in the above investigation. The bounds returned by `par1` are in fact looser than they need to be.

Imagine some algebraic expression which results in the interval E. We could always obtain an algebraically equivalent expression E' by multiplying and dividing by some interval A: E' -> A * E / A -> E * (A / A). The resulting expression E' will have a width greater than E. In fact, formula 1 for parallel resistance is what you get by multiplying and dividing fomula 2 by an interval. In this case, F2 -> F1 * (R1R2 / R1R2). And you could always introduce some arbitrary interval by which to multiply and divide, thus arbitrarily increasing the error bounds without reason.
