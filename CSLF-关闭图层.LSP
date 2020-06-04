(defun cslf ( / lf_oe lf_oc  en cl ln opt)
  (defun lf_er (s)
    (if (/= s "Function cancelled")
      (if (=s "quit / exit abort")
        (princ)
        (princ (strcat "\nError: " s))
      )
    )
    (eval(read U:E))
    (if lf_oe
      (setq *error* lf_oe)
    )
    (if lf_oc (setvar "cmdecho" lf_oc))
    (princ)
  )
  (if *error*
    (setq lf_oe *error* *error* lf_er)
    (setq *error* lf_er)
  )
  (setq U:G "(command \"undo\" \"group\")"
        U:E "(command \"undo\" \"en\")"
  )
  (setq lf_oc (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (eval(read U:G))
  (princ (strcat "\n极速关层,(c) 1997.1.4 by 陈松 -- 南昌. "))
  (lf_ol)
  (if lf_oe (setq *error* lf_oe))
  (eval(read U:E))
  (if lf_oc (setvar "cmdecho" lf_oc))
  (princ)
)

;;; The lf_ol() definition
;;;
(defun lf_ol()
  (setq cl (getvar "clayer"))
  (setq en (entsel "\n请选择要关闭层层上的目标: "))
  (if (not (null en))
    (progn
      (setq ln (cdr (assoc 8 (entget (car en)))))
      (initget "Freeze Off")
      (setq opt (getkword "\nF冻结/<O关闭>: "))
      (if opt nil (setq opt "Off"))
      (if (and (= opt "Freeze") (= ln cl))
        (princ "\n不能冻结当前层 ")
        (progn
          (if (and (= opt "Off") (= ln cl))
            (command "layer" opt ln "y" "")
            (command "layer" opt ln "")
          )
        )
      )
    )
    (princ "\n没有选到目标 ")
  )
)
;;;
;;; The C: function definition
;;;
(defun c:lf     () (cslf))
(princ "\n\tC:CSlf 已经加载. 键入 LF 执行命令.")
(princ)
