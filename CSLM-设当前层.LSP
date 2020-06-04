(defun cslm ( / lm_oe lm_oc en ln)
  (defun lm_er (s)
    (if (/= s "Function cancelled")
      (if (=s "quit / exit abort")
        (princ)
        (princ (strcat "\nError: " s))
      )
    )
    (eval(read U:E))
    (if lm_oe
      (setq *error* lm_oe)
    )
    (if lm_oc (setvar "cmdecho" lm_oc))
    (princ)
  )
  (if *error*
    (setq lm_oe *error* *error* lm_er)
    (setq *error* lm_er)
  )
  (setq U:G "(command \"undo\" \"group\")"
        U:E "(command \"undo\" \"en\")"
  )
  (setq lm_oc (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (eval(read U:G))
  (princ (strcat "\n极速设层,(c) 1997.1.4 by 陈松 -- 南昌. "))
  (lm_ol)
  (if lm_oe (setq *error* lm_oe))
  (eval(read U:E))
  (if lm_oc (setvar "cmdecho" lm_oc))
  (princ)
)

;;; The lm_ol() definition
;;;
(defun lm_ol()
  (while (null en)
    (setq en (entsel "\n请选择要设为当前层层上的目标: "))
  )
  (setq ln (cdr (assoc 8 (entget (car en)))))
  (command "layer" "s" ln "")
)
;;;
;;; The C: function definition
;;;
(defun c:lm     () (cslm))
(princ "\n\tC:CSLM 已经加载. 键入 LM 执行命令.")
(princ)

