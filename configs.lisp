(defun parse-config-list ())

(defun parse-config (rest config &aux (current (car config)) (next (cadr config)))
  ;; Parse current, push to config
  ;; Invoke parse bindings
    

    

  (defun parse-bindings (rest config &aux (current (car rest)) (next (cadr rest)))
    ;; Parse each binding
    ;; For each binding, re-invoke self with () (cdr config)
  (cond ((not (stringp current)) (append current (parse-bindings next)))
        (current (parse-config 
