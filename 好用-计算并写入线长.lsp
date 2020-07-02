;计算并写入线长
(DEFUN C:tjxc ()
     (setvar "cmdecho" 0) ;指令执行过程不响应
     (PRINC "\n计算并写入线长功能")
  (setq tot_len 0)  
 (princ "\n-->请选取要计算长度的线条.....")
  (setq ss (ssget))
  (while (> (sslength ss) 0)
    (setq en (ssname ss 0))
    (setq ed (entget en))
    (setq e_type (cdr (assoc '0 ed)))
    (cond
      ((= e_type "LINE") (add_lines))
      ((= e_type "ARC") (add_arcs))
      ((= e_type "POLYLINE") (add_poly))
      ((= e_type "LWPOLYLINE") (add_poly))
      ((or
	 (/= e_type "LINE")
	 (/= e_type "ARC")
	 (/= e_type "POLYLINE")
       )
       (ssdel en ss)
      )
    )
  )
  (prompt (strcat "\n-->线的总长度为: " (rtos tot_len 2 1)))
  (setq txtPt (getpoint "\n-->请指定线长统计的写入点: "))
  (command "._text" "middle" txtpt)
  (if (= 0.0 (cdr (assoc 40 (tblsearch "style" (getvar "textstyle")))))
	   (command "" "" (rtos tot_len 2 1))
	   (command "" (rtos tot_len 2 1))
	)
     (PRINC "\n线长统计写入完成！")(PRINC))

(defun add_lines ()
  (setq pt1 (cdr (assoc '10 ed)))
  (setq pt2 (cdr (assoc '11 ed)))
  (setq line_len (distance pt1 pt2))
  (setq tot_len (+ tot_len line_len))
  (ssdel en ss)
)

(defun add_arcs ()
  (SETQ CEN (CDR (ASSOC '10 Ed))
	RAD (CDR (ASSOC '40 Ed))
	DIA (* RAD 2.0)
	CIRCUM (* (* RAD PI) 2.0)
	S_ANG (CDR (ASSOC '50 Ed))
	E_ANG (CDR (ASSOC '51 Ed))
  )
  (IF (< E_ANG S_ANG)
    (SETQ E_ANG (+ E_ANG (* PI 2.0)))
  )
  (SETQ
	N_ANG (- E_ANG S_ANG)
	N_ANG_1 (* (/ N_ANG PI) 180.0)
	PART_CIRC (/ N_ANG_1 360.0)  
	A_LEN (* PART_CIRC CIRCUM)
  )
  (setq tot_len (+ tot_len a_len))
  (PRIN1)
  (SSDEL EN SS)
)

(defun add_poly ()
  (command "area" "e" en)
  (setq tot_len (+ tot_len (getvar "perimeter")))
  (ssdel en ss)
)
