(defvar function-mappings (with-open-file (s "micro-operations-to-fs.lisp") (read s)))

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
  (format
   nil
   "\"~v,'0b\""
   size
   (if (numberp register)
       register
       (with-input-from-string (s (subseq (symbol-name register) 1)) (read s)))))

(defun decode-address-source (source)
  (decode-bit (case source
                (register-file 1)
                (instruction-register 0))))

(defun concatenate-vectors (into vectors)
  (if vectors (concatenate-vectors (concatenate 'string into "&" (car vectors)) (cdr vectors)) into))

(defun decode-bit (bit)
  (format nil "\"~d\"" (if (numberp bit) bit (if bit 1 0))))

(defun decode-jump-offset (offset)
  (format nil "\"~17,'0b\"" offset))

(defun parse-control-word (control-parameters)
  (let ((next-address (decode-jump-offset (getf control-parameters :jump-offset 0))) ; na
        (jump-condition (decode-jump-condition (getf control-parameters :jump-condition 'never))) ; mux s
        (execute-next-instruction (decode-bit (getf control-parameters :finish 0))) ; mux c
        (load-next-instruction (decode-bit (getf control-parameters :load-next-instruction 0))) ; il
        (increment-pc (decode-bit (getf control-parameters :increment-pc 0))) ; pi
        (load-pc (decode-bit (getf control-parameters :load-pc 0))) ; pl
        ;; Bruh
        (td (decode-register (nth 1 control-parameters))) ; td
        (ta (decode-register (nth 2 control-parameters))) ; ta
        (tb (decode-register (nth 3 control-parameters))) ; tb
        (mb (decode-bit (getf control-parameters :mb 0))) ; mb
        (fs (cadr (assoc (nth 0 control-parameters) function-mappings)))
        (md (decode-bit (getf control-parameters :md 0))) ; md
        (rw (decode-bit (getf control-parameters :rw 0))) ; rw
        (address-source (decode-address-source (getf control-parameters :rw 'register-file))) ; mm
        (mw (decode-bit (getf control-parameters :mw 0))) ; mw
        (rv (decode-bit (getf control-parameters :rv 0))) ; rv
        (rc (decode-bit (getf control-parameters :rc 0))) ; rc
        (rn (decode-bit (getf control-parameters :rn 0))) ; rn
        (rz (decode-bit (getf control-parameters :rz 0))) ; rz
        (fl (decode-bit (getf control-parameters :fl 0)))) ; fl
    (concatenate-vectors next-address (list jump-condition execute-next-instruction load-next-instruction increment-pc load-pc td ta tb mb fs md rw address-source mw rv rc rn rz fl))))

(defun parse-opcode (opcode)
  (if (numberp opcode)
      (format nil "\"~17,'0b\"" opcode)
      (case opcode
        (add "\"00000000000000000\""))))
  

(defun parse-instruction (instruction)
  (concatenate-vectors
     (parse-opcode (nth 0 instruction)) ; Opcode
     (list (decode-register (nth 1 instruction) 5) ; DR
           (decode-register (nth 2 instruction) 5) ; SA
           (decode-register (nth 3 instruction) 5)))) ; SB
  
