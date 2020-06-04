;转换坐标系的小工具
;大家经常需要画与一条斜线垂直的直线，这个小工具起到这个作用。
;FA ，选择一条直线。如果是多段线的话，就需要在多段线上再选择两点。
;AF，恢复直角坐标系。
;非常有用的工具。今天刚完善好。
;;改变坐标系统，与所选物体垂直
(defun c:fa (/ ent1 pt_START pt_END)

  (setq ent1 (entsel "\n请选择对象："))

  (cond

    ((= ENT1 NIL) (CHSNAP))

    (
     (= (cdr (assoc 0 (entget (car ent1)))) "LWPOLYLINE")
     (CHSNAP)
    )

    (
     (=
       (cdr (assoc 0 (entget (car ent1))))
       "LINE"
     )
     (progn

       (setq pt_START (cdr (assoc 10 (entget (car ent1)))))

       (setq pt_END (cdr (assoc 11 (entget (car ent1)))))

       (setvar "snapang" (angle pt_START pt_END))
     )
    )
  )
  (setvar "orthomode" 1)
  (princ)

)


(DEFUN
	  CHSNAP
		(/ PT1 PT2)

  (setq pt1 (getpoint "\n请选择第一点："))

  (setq pt2 (getpoint "\n请选择第二点："))

  (setvar "snapang" (angle pt1 pt2))

)
(defun c:af() 

(setvar "snapang"0) 


) 