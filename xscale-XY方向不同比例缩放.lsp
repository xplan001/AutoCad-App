;x,y方向不同比例缩放
; ***  XSCALE   [Version 1.0] 6/22/2005  ***
;
;Copyleft Gu Wenwei
;
; ***************************************
; ****  Author:  Apooollo            ****
; ****                               ****
; ****  Wuxi Jiangsu China           ****
; ***************************************
;
;
; This program takes selected objects, defines an anonymous block,
; then inserts the block at the original location, Scale by X,Y


(defun C:XSCALE(/ bp ss xscal yscal entL)

  (defun errexit (s)
    (princ "\nError:  ")
    (princ s)
    (restore)
  )

  (defun restore ()
    (setvar "CMDECHO" (car oldvar))
    (setq *error* olderr)
    (princ)
  )
  
(defun MAKEUNBLOCK (ss ip / tmp errexit mbx BLAYER)

  (setq T (not nil))
  (setq olderr  *error*
        *error* errexit
  )
  (setq oldvar 
    (list 
      (getvar "CMDECHO") 
    )
  )
  (setvar "CMDECHO" 0)
  (terpri)
  (if BLAYER  
    (command "._LAYER"
      (if (tblsearch "LAYER" BLAYER) "_S" "_M")
      BLAYER
      ""
    )
  )
  (if (and ip ss)
    (progn
      (entmake (list 
        (cons '0 "BLOCK") 
        (cons '2 "*U") 
        (cons '70 1) 
        (cons '10 ip)
      ))
      (setq cnt (sslength ss))
      (while (>= (setq cnt (1- cnt)) 0)
        (setq tmp (ssname ss cnt))
        (entmake (setq el (entget tmp)))
        (if (> (cdr (assoc 66 el)) 0)
          (while
            (/= "SEQEND"
              (cdr
                (assoc 0
                  (entmake (setq el (entget (entnext (cdr (assoc -1 el))))))
                )
              )
            )
          )
        )
        (entdel tmp)
      ) 
      (setq tmp (entmake (list (cons '0 "ENDBLK"))))
      (entmake (list 
        (cons '0 "INSERT") 
        (cons '2 tmp) 
        (cons '10 ip)
      ))
    )
  )  
  (restore)
)

  (setq ss (ssget))    ;;; 选择缩放实体
  (if ss
    (progn
      (setvar "cmdecho" 0)
      (setq bp (getpoint "缩放基准点 (<0,0,0>): "))
      (if (not bp) (setq bp (list 0 0 0)))
      (setq xscal (getreal "X向比例因子 <1>: "))
      (if (not xscal) (setq xscal 1))
      (setq yscal (getreal "Y向比例因子 <1>: "))
      (if (not yscal) (setq yscal 1))
      (MAKEUNBLOCK ss bp)
      (setq entL (entget (entLast))
	  entL (subst (cons 41 xscal) (assoc 41 entL) entL)
	  entL (subst (cons 42 yscal) (assoc 42 entL) entL)
      )
      (entmod entL)
      (command "_explode" "l" "")
    )
  )
  (princ "X,Y不同比例缩放, 命令:XSCALE")
)