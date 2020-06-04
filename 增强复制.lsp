
;;;********************************************************图形矫正程序-jz
(defun c:cc (/ p1 p2 s e cn)
 ;__________________ 
  (defun ttt (ss n / m)
    (setq ee e
	  ns (ssadd)
    )
    (while (setq ee (entnext ee))
      (setq ns (ssadd ee ns))
    )
    (command "erase" ns "")
    (command "copy" ss "" "m" "non" p1)
    (setq m 0)
    (repeat (atoi n)
      (setq m (1+ m))
      (cond
	((= "/" (substr n (strlen n)))
	 (command
	   "non"
	   (mapcar '(lambda (x y) (+ x (* m (/ (- y x) (atof n)))))
		   p1
		   p2
	   )
	 )
	)
	(t
	 (command "non"
		  (mapcar '(lambda (x y) (+ x (* m (- y x)))) p1 p2)
	 )
	)
      )
    )
    (command)
  )
 ;__________________ 
    (princ "\n选择要复制的物体:")
  (setq s (ssget))
  (setq p1 (getpoint "\n复制的起点:"))
  (setq p2 (getpoint p1 "\n复制的终点:"))
  (setq e (entlast))
  (command "copy" s "" "non" p1 "non" p2)
  (while (/= 0
	     (atof (setq cn (getstring "\n份数(以 / 结束为等分):")))
	 )
    (ttt s cn)
  )
  (princ)
)



(defun c:c1 (/ p1 p2 s e cn a1 d1 ns cnn)
 ;__________________ 
  (defun ttt (ss n / m)
    (setq ee e
	  ns (ssadd)
    )
    (while (setq ee (entnext ee))
      (setq ns (ssadd ee ns))
    )
    (command "erase" ns "")
    (command "copy" ss "" "m" "non" p1)
    (if	(member (substr n (strlen n)) '("/" "*"))
      (progn
	(setq m 0)
	(repeat	(atoi n)
	  (setq m (1+ m))
	  (cond
	    ((= "/" (substr n (strlen n)))
	     (command
	       "non"
	       (mapcar '(lambda (x y) (+ x (* m (/ (- y x) (atof n)))))
		       p1
		       p2
	       )
	     )
	    )
	    ((= "*" (substr n (strlen n)))
	     (command "non"
		      (mapcar '(lambda (x y) (+ x (* m (- y x)))) p1 p2)
	     )
	    )
	  )
	)
      )
      (command "non" (setq p2 (polar p1 a1 (atof n))))
    )
    (command)
  )
 ;__________________ 
  (princ "\n选择要复制的物体:")
  (setq s (ssget))
  (setq p1 (getpoint "\n复制的起点:"))
  (command "undo" "be" "line" p1 p1 "")
  (setq e (entlast))
  (command "copy" s "" "non" p1 pause)
  (setq	p2 (getvar "lastpoint")
	a1 (angle p1 p2)
	d1 (distance p1 p2)
  )
  (setq cn "1*")
  (while cn
    (ttt s cn)
    (initget 128)
    (princ
      "\n输入坐标=复制终点                         输入数值=修改间距 "
    )
    (princ
      "\n输入数值n并以 / 结束=间距内等分n次复制    输入数值n并以 * 结束=按间距复制n次 "
    )
    (setq cnn (getpoint "\n请按提示输入<退出>:"))
    (if	(= 'LIST (type cnn))
      (setq p2 cnn
	    a1 (angle p1 p2)
	    d1 (distance p1 p2)
      )
      (setq cn cnn)
    )
  )
  (entdel e)
  (command "undo" "e")
  (princ)
)


(defun c:c2 (/ p1 p2 s e cn)
 ;__________________
  (defun ttt (ss n / m)
    (setq ee e
	  ns (ssadd)
    )
    (while (setq ee (entnext ee))
      (setq ns (ssadd ee ns))
    )
    (command "erase" ns "")
    (command "copy" ss "" "m" "non" p1)
    (setq m 0)
    (repeat (atoi n)
      (setq m (1+ m))
      (cond
	((= "/" (substr n (strlen n)))
	 (command
	   "non"
	   (mapcar '(lambda (x y) (+ x (* m (/ (- y x) (atof n)))))
		   p1
		   p2
	   )
	 )
	)
	(t
	 (command "non"
		  (mapcar '(lambda (x y) (+ x (* m (- y x)))) p1 p2)
	 )
	)
      )
    )
    (command)
  )
 ;__________________
  (princ "\n选择要复制的物体:")
  (setq s (ssget))
  (setq p1 (getpoint "\n复制的起点:"))
  (setvar "lastpoint" p1)
					;(setq p2 (getpoint p1 "\n复制的终点:"))
  (setq e (entlast))
  (command "copy" s "" "non" p1 pause)
  (if (not (equal p1 (setq p2 (getvar "lastpoint"))))
    (while (/= 0
	       (atof (setq cn (getstring "\n份数(以 / 结束为等分):")))
	   )
      (ttt s cn)
    )
  )
  (princ)
)


;;;|增强拷贝
(defun c:c3 (/ getpt getpt1 ss ptx pty db n x y gtin)
  (setq	getpt1 (acet-ss-drag-move
		 (setq ss (ssget))
		 (setq getpt (getpoint "\n&点取基点:"))
		 1
	       )
  )
  (setq	ptx (- (car getpt1) (car getpt))
	pty (- (cadr getpt1) (cadr getpt))
	y   0
  )
  (vl-cmdf ".copy" ss "" getpt getpt1)
  (while (setq gtin (- (getint "\n重复次数:") 1))
    (vl-cmdf ".undo" "e")
    (if	(/= y 0)
      (vl-cmdf ".u")
    )
    (setq n  1
	  x  0
	  db nil
    )
    (if	(/= y 0)
      (vl-cmdf ".u")
    )
    (vl-cmdf ".undo" "be")
    (repeat gtin
      (setq db (cons (list (+ (* n ptx) (car getpt1))
			   (+ (* n pty) (cadr getpt1))
			   0.0
		     )
		     db
	       )
      )
      (setq n (1+ n))
    )
    (repeat (length db)
      (vl-cmdf ".copy" ss "" getpt (nth x (reverse db)))
      (setq x (1+ x))
    )
    (vl-cmdf ".undo" "e")
    (vl-cmdf ".undo" "be")
    (setq y (1+ y))
  )
  (princ)
)



