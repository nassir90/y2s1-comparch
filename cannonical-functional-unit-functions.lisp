;; Numbers are in the form FSSSC, where F represents
;; whether shifter output or alu output is being used.
(("00000" "F=A" mov-a)
 ("00001" "F=A+1" inc)
 ("00010" "F=A+B" add)
 ("00011" "F=A+B+1" add-x)
 ("00100" "F=A+NOT(B)" sub-x)
 ("00101" "F=A+NOT(B)+1" sub)
 ("00110" "F=A-1" dec)
 ("00111" "F=A" mov-a-x)
 ("01000" "F=A&B" and)
 ("01010" "F=A|B" or)
 ("01100" "F=A^B" xor)
 ("01110" "F=NOT(A)" inv)
 ("10000" "F=B" mov-b)
 ("10100" "F=srB" lsr)
 ("11000" "F=slB" lsl))
