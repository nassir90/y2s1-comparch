`((21364466 10 0 :header "A: Simply adding 10 to my student number.")
  (21364466 ,(neg 21364466) 0 :header "B: Subtracting my student number from my student number.")
  (#b11111111111111111111111111111111 #b00000000000000000000000000000000 1 :header "C: Worst case propagation delay.")
  (#b10111111111111111111111111111111 #b10000000000000000000000000000000 1 :header "D: Demonstration of overflow and carry flag being set.")
  (21364466 ,(- (expt 2 31) 21364465) 0 :header "E: Adding a number to my student number such as to cause an overflow.")
  (21364466 ,(- (expt 2 32) 21364465) 0 :header "F: Adding a number to my student number such as to cause a carry."))
