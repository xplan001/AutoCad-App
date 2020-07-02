;;;====================打断命令小改造 BY wowan1314 ==================;;;
;;;功能:如果选择打断第二点时右键或空格则打断于点
;;;     其他同CAD的打断命令   2013.7.8
(DEFUN C:BR (/ ENT ERR P1 P2 oldcm *ERROR*)
  (DEFUN *ERROR* (X)
    (IF	ENT
      (REDRAW (CAR ENT) 4)
    )
    (if	oldCM
      (setvar "cmdecho" oldCM)
    )
    (PRINC X)
  )
  (setvar 'errno 0)
  (setq ENT (entsel "选择要打断的对象:"))
  (setq err (getvar "errno"))
  (while (= err 7)
    (setvar 'errno 0)
    (setq ent (entsel))
    (setq err (getvar "errno"))
  )
  (setq oldCM (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (IF ENT
    (PROGN
      (SETQ P1 (OSNAP (CADR ENT) "NEA"))
      (REDRAW (CAR ENT) 3)
      (INITGET "F f")
      (SETQ P2 (GETPOINT
		 "\n 指定打断的第二点 或第一点<F> 或打断于点<右键>:"
	       )
      )
      (IF P2
	(IF (OR (= P2 "F") (= P2 "f"))
	  (PROGN
	    (princ "\n指定打断的第一点:")
	    (VL-CMDF ".BREAK" ENT "F" Pause)
	    (while (/= 0 (getvar "cmdactive"))
	      (princ "\n指定打断的第二点:")
	      (VL-CMDF Pause)
	    )
	  )
	  (VL-CMDF ".BREAK" ENT "F" P1 P2)
	)
	(VL-CMDF ".BREAK" ENT "F" P1 "@")
      )
    )
  )
  (PRINC)
)
