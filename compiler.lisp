(load "ripple.lisp")

(defvar functional-unit-functions (with-open-file (s "functional-unit-functions.lisp") (read s)))
(defvar function-mappings (with-open-file (s "micro-operations-to-fs.lisp") (read s)))
(defvar cannonical-function-mappings (with-open-file (s "cannonical-functional-unit-functions.lisp") (read s)))

(defun desugar (instruction)
  (case (car instruction)
    (nop (append '(mov-a r0 r0 r0) (cdr instruction)))
    (nop-x (append '(mov-a) (cdr instruction)))
    (b (desugar (append '(nop :mc :control :na) (cdr instruction))))
    (end (desugar '(b fetch :ms always)))
    (t instruction)))

(defun decode-function (function)
  (cond ((stringp function) function)
        (t (cadr (assoc function function-mappings)))))

(defun decode-jump-condition (condition)
  (format nil "\"~3,'0b\""
          (case condition
            (never 0)
            (always 1)
            (carry 2)
            (overflow 3)
            (zero 4)
            (negative 5)
            (not-carry 6)
            (not-zero 7))))

(defun decode-register (register &optional (size 4))
  (format nil "\"~v,'0b\"" size
          (cond ((numberp register) register)
                ((eq register '_) 0) 
                (t (with-input-from-string (s (subseq (symbol-name register) 1)) (read s))))))

(defun decode-address-source (source)
  (decode-bit (case source
                (:register-file 0)
                (:program-counter 1))))

(defun concatenate-vectors (into vectors)
  (if vectors (concatenate-vectors (concatenate 'string into "&" (car vectors)) (cdr vectors)) into))

(defun decode-bit (bit)
  (format nil "\"~d\"" (if (numberp bit) bit (if bit 1 0))))

(defun decode-next-address (offset)
  (if (numberp offset) (format nil "\"~17,'0b\"" offset) offset))

(defun decode-ir-source (ir-source)
  (case ir-source
    (:control "\"0\"")
    (:opcode "\"1\"")))

(defun sa-from-immediate (immediate)
  (cond ((stringp immediate) (subseq immediate 0 5))
        ((numberp immediate) (irep (ash immediate -5) 5))))

(defun sb-from-immediate (immediate)
  (cond ((stringp immediate) (subseq immediate 5))
        ((numberp immediate) (irep (logand immediate #b11111) 5))))

(defun parse-instruction (instruction &key labels)
  (case (car instruction)
    (:branch (let ((immediate-value (if (symbolp (nth 3 instruction))
                                        (cdr (assoc (nth 3 instruction) labels))
                                        (nth 3 instruction))))
                   (parse-instruction
                    (list (nth 1 instruction)
                          (nth 2 instruction)
                          (sa-from-immediate immediate-value)
                          (sb-from-immediate immediate-value))
                    :labels labels)))
    (:immediate (parse-instruction
                   (list (nth 1 instruction) ;op
                         (nth 2 instruction) ;dr
                         (nth 3 instruction) ;sa
                         (sb-from-immediate (nth 4 instruction)))
                   :labels labels))
    (:word (decode-register (nth 1 instruction) 32))
    (t (let ((opcode (nth 0 instruction)))
         (concatenate-vectors (decode-register (cond ((symbolp opcode) (cdr (assoc opcode labels)))
                                                         ((numberp opcode) opcode)
                                                         (t 0))
                                                   17) ; Opcode
                                  (list (decode-register (or (nth 1 instruction) 0) 5) ; DR
                                       (decode-register (or (nth 2 instruction) 0) 5) ; SA
                                        (decode-register (or (nth 3 instruction) 0) 5))))))) ; SB)

(defun parse-control-word (instruction)
  (let ((na (decode-next-address (getf instruction :na 0))) ; na
        (ms (decode-jump-condition (getf instruction :ms 'never))) ; ms
        (mc (decode-ir-source (getf instruction :mc :control))) ; mc
        (il (decode-bit (getf instruction :il 0))) ; il
        (pc-increment (decode-bit (getf instruction :pi 0))) ; pi
        (pc-load (decode-bit (getf instruction :pl 0))) ; pl
        (td (decode-register (nth 1 instruction))) ; td
        (ta (decode-register (nth 2 instruction))) ; ta
        (tb (decode-register (nth 3 instruction))) ; tb
        (mb (decode-bit (getf instruction :mb 0))) ; mb
        (fs (decode-function (nth 0 instruction)))
        (md (decode-bit (getf instruction :md 0))) ; md
        (rw (decode-bit (getf instruction :rw 0))) ; rw
        (mm (decode-address-source (getf instruction :mm :register-file))) ; mm
        (mw (decode-bit (getf instruction :mw 0))) ; mw
        (rv (decode-bit (getf instruction :rv 0))) ; rv
        (rc (decode-bit (getf instruction :rc 0))) ; rc
        (rn (decode-bit (getf instruction :rn 0))) ; rn
        (rz (decode-bit (getf instruction :rz 0))) ; rz
        (fl (decode-bit (getf instruction :fl 0)))) ; fl
    (concatenate-vectors na (list ms mc il pc-increment pc-load td ta tb mb fs md rw mm mw rv rc rn rz fl))))

(defun pass-1 (raw-code &key labels code (address 0))
  (if raw-code
      (let ((current-instruction (car raw-code)))
        (if (eq 'label (car current-instruction))
            (pass-1 (cdr raw-code)
                    :labels (cons `(,(cadr current-instruction) . ,address) labels)
                    :code code
                    :address address)
            (pass-1 (cdr raw-code)
                    :labels labels
                    :code (cons current-instruction code)
                    :address (1+ address))))
      (list (cons 'labels labels)
            (cons 'code (reverse code)))))

(defun pass-2-microcode (microcode &key labels code)
  (if microcode
      (let ((current-instruction (desugar (car microcode)))
            (microcode (cdr microcode))
            (next-address (getf (desugar (car microcode)) :na)))
        (if (and next-address (symbolp next-address))
            (setf (getf current-instruction :na) (cdr (assoc next-address labels))))
        (pass-2-microcode microcode
                          :labels labels
                          :code (cons (parse-control-word current-instruction) code)))
      (reverse code)))


(defun pass-2-assembly (assembly &key labels code (address 0) control-labels)
  "Replace all labels with the correct relative displacements.
For any instruction whose first argument is :branch and whose fourth
argument is a symbol, the symbol will be resolved to the correct
displacement.

    (:branch branch r0 add)"
  (if assembly
      (let ((current-instruction (car assembly))
            (determiner (caar assembly))
            (possible-label (nth 3 (car assembly))))
        (when (and (eq determiner :branch) (symbolp possible-label))
          (setf (nth 3 (car assembly)) (- (cdr (assoc possible-label labels)) address 1)))
        (pass-2-assembly (cdr assembly)
                         :labels labels
                         :control-labels control-labels
                         :code (cons (parse-instruction
                                      current-instruction
                                      :labels control-labels)
                                     code)
                         :address (1+ address)))
      (reverse code)))

(defun compile-microcode-and-assembly (microcode assembly
                                       &key (microcode-start 66) (assembly-start 6))
  (let ((microcode-1 (pass-1 microcode :address microcode-start)))
    (let ((microcode-labels (cdr (assoc 'labels microcode-1)))
          (microcode-code (cdr (assoc 'code microcode-1))))
      (let ((assembly-1 (pass-1 assembly :address assembly-start)))
        (let ((assembly-labels (cdr (assoc 'labels assembly-1)))
              (assembly-code (cdr (assoc 'code assembly-1))))
          (values (pass-2-microcode microcode-code
                                    :labels microcode-labels)
                  (pass-2-assembly assembly-code
                                   :labels assembly-labels
                                   :control-labels microcode-labels
                                   :address assembly-start)))))))

(defvar assembly '((label start)
                   (:immediate ldr= r0 _  14)
                   (:immediate ldr= r1 _  14)
                   (:immediate ldr= r2 _  14)
                   (:immediate ldr= r3 _  14)
                   (:immediate ldr= r4 _  14)
                   (:immediate ldr= r5 _  14)
                   (:immediate ldr= r6 _  14)
                   (:immediate ldr= r7 _  14)
                   (:immediate ldr= r8 _  14)
                   (:immediate ldr= r9 _  14)
                   (:immediate ldr= r10 _ 14)
                   
                   (:immediate ldr= r0 _ 20)
                   (:immediate ldr= r1 _ 31)
                   (:immediate ldr= r5 _ 10)
                   (add r3 r0 r1) ; R3 <= R0 + R1 = 51
                   (str _ r5 r3) ; M[R5] <- R3
                   (ldr r10 r5 _) ; R10 <- M[R5] = 51
                   (sub r10 r10 r5) ; R10 <- R10 - R5 = 41
                   (inc r10 r10 _) ; R10 <- R10 + 1 = 42
                   (:immediate lsl r10 r10 2) ; R10 <- R10 LSL #2 = 168
                   (add-x r10 r10 r1) ; R10 <- R10 + R1 + 1 = 200
                   (mov-a-x r7 r10) ; R7 <- R10 = 200
                   (dec r7 r7 _) ; R7 <- R7 - 1 = 199
                   (and r7 r7 r10) ; R7 <- R7 AND R10 = 192
                   (or r7 r7 r0) ; R7 <- R7 OR R9 = 212
                   (xor r7 r7 r7) ; R7 <- R7 XOR R7 = 0
                   (:branch beq _ close-call)
                   (:word #xDEADBEEF)
                   (label close-call)
                   (inv r7 r3 _) ; R7 <- NOT(R3) = -52
                   (mov-b r8 _ r7); R8 <- R7 = -52
                   (:immediate lsr r8 r8 31) ; R8 <- R8 LSR 31 = 1
                   (add r4 r8 r8) ; R4 <- R8 + R8 = 2
                   (sub-x r4 r4 r8) ; R4 <- R4 + NOT(R8) = 0
                   (:branch beq _ start)))

(defvar microcode `((label fetch)
                    ;; Load instruction register and increment PC (afterwards)
                    (nop :pi 1 :il 1 :mc :control :rw 0 :mw 0 :mm :program-counter)
                    (nop :mc :opcode :ms always)

                    ,@(loop for function in cannonical-function-mappings append
                            `((label ,(nth 2 function))
                              (,(nth 2 function) u0 u0 u0 :rw 1 :fl 1)
                              (end)))

                    (label lsl)
                    (mov-b t1 _ u0 :mb 1 :fl 1 :rw 1)
                    (mov-a t2 u0 _ :rw 1)
                    (label lsl-continue)
                    (b lsl-end :ms zero)
                    (lsl t2 _ t2 :rw 1)
                    (dec t1 t1 _ :fl 1 :rw 1)
                    (b lsl-continue :ms always)
                    (label lsl-end)
                    (mov-a u0 t2 _ :rw 1)
                    (end)

                    (label lsr)
                    (mov-b t1 _ u0 :mb 1 :fl 1 :rw 1)
                    (mov-a t2 u0 _ :rw 1)
                    (label lsr-continue)
                    (b lsr-end :ms zero)
                    (lsr t2 _ t2 :rw 1)
                    (dec t1 t1 _ :fl 1 :rw 1)
                    (b lsr-continue :ms always)
                    (label lsr-end)
                    (mov-a u0 t2 _ :rw 1)
                    (end)

                    (label ldr)
                    (nop :rw 1 :md 1)
                    (end)
                    
                    (label ldr=)
                    (mov-b r0 r0 r0 :mb 1 :rw 1 :mw 0)
                    (end)

                    (label str)
                    ;; Take the value from the A port as the address.
                    ;; Take the value form the B port as the value to load.
                    (nop :mb 0 :mw 1 :mm :register-file)
                    (end)

                    (label beq)
                    (b beq-not-equal :ms not-zero)
                    ;; Apply a diplacement, since were the zero flag unset,
                    ;; We would have jumped to beq-not-equal
                    (nop :pl 1)
                    (label beq-not-equal)
                    (end)))
