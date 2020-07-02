;;; ------------------------------------------------------------------
;;;  2.6 LayerFreezeM 冻结物体所在的层（多选）
(defun C:gb (/ ES EN EL A)
	(princ "Selected Entity(s) Layers Freezed.")
	(setq ES (ssget)
				A 0
				EN ""
				EL nil
				FL nil
	)
	(while (/= EN nil)
		(setq EN (ssname ES A)
					EL (cons EN EL)
					A (1+ A)
		)
	)
	(setq EL (cdr EL)
				FL (cdr (assoc '8 (entget (car EL))))
				EL (cdr EL)
	)
	(repeat (- A 2)
		(setq EN (cdr (assoc '8 (entget (car EL))))
					FL (strcat EN "," FL)
					EL (cdr EL)
		)
	)
	(command "LAYER" "F" (eval FL) "")
	(princ)
)

;;;  2.7 LayerThawAll 解冻所有的层
(Defun C:qx ()
	(COMMAND "LAYER" "THAW" "*" "")
	(PRINC)
)