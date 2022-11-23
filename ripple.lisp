(defclass ripple-section ()
  ((sum :initarg :sum)
   (a :initarg :a)
   (b :initarg :b)
   (c :initarg :c)))

(defun number-to-list (number)
  (reverse (loop for c across (format nil "~32,'0b" number)
                 collect (with-input-from-string
                             (*standard-input* (make-string 1 :initial-element c))
                           (read)))))

(defun ripple-add (a b c)
  (ripple-inner (number-to-list (if (< a 0) (+ (expt 2 31) (- a)) a))
                (number-to-list (if (< b 0) (+ (expt 2 31) (- b)) b))
                c))

(defun ripple-inner (a b c &optional sections)
  (let* ((a-1 (pop a)) (b-1 (pop b)) (sum (+ a-1 b-1 c)))
    (push `(:a ,a-1 :b ,b-1 :c ,c :sum ,(logand 1 sum)) sections)
    (if a
        (ripple-inner a b (ash (logand 2 sum) -1) sections)
        (append `((:a 0 :b 0 :c ,(ash (logand 2 sum) -1) :sum 0)) sections))))
                         
(defun ripple-unzip (sections)
  (car (loop for section in sections
             collect (getf section :a) into a
             collect (getf section :b) into b
             collect (getf section :c) into c
             collect (getf section :sum) into sum
             collect (list c a b sum))))

(defun neg (n &optional (base 32))
  (1+ (logxor (1- (expt 2 base)) n)))

(defun uneg (n &optional (base 32))
  (- (expt 2 base) n))

(defun srep (n &optional (base 32))
  (if (>= n (expt 2 (1- base)))
      (- (uneg n))
      n))
