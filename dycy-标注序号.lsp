;;用于标注序号的程序
;; 希望能够对大家的绘图工作有所帮助
;;用于标注序号的LISP程序
;;序号可以自动累计标注
;;必须在图形中设置6图层用于标注
;;系统中应该有"gbeitc","gbcbig"字体，如果没有请予以更改
(defun C:DYCY();;;(/ r p1 p2 p3 p4 x y a x1 y1 n nn)
  (defun wz_sz() ;;;文字设置
  (if (or (=(setq sty (tblsearch "STYLE" "standard")) nil);"汉字HZ字型是否存在"
      (= (cdr (assoc 40 sty)) 0);字高是否为定值
      (/= (getvar "TEXTSTYLE") "standard");当前字型是否为"hz"
  )
  (command "style" "standard" "gbeitc,gbcbig" (* mm 10) 1 0 "n" "n");设置字型"
  )
)
(defun tc_sz()
  (if (or (= (setq lay (tblsearch "layer" "6")) nil) ;;;6图层是否存在
          (/= (cdr (assoc 2 lay)) "6");当前图层是否为6图层
      )
         (progn    ;;;上述条件成立新建图层
;;LCA - COMMENT: The LAYER command has new options.
            (command "layer" "new" "6" "c" "white" "" "ltype" "continuous" "" "");设置"dim图层"
            (command "linetype" "s" "continuous" "") ;;;;设置线型为连续线型
            (command "layer" "s" "6" "c" "white" "" "")
          )
         (progn ;;;判断条件不成立设置图层
            (command "layer" "s" "6" "c" "white" "" "")
            (command "linetype" "s" "continuous" "")
          )
  )
)
;;;;主程序开始  
  (setq olderr  *error*    ;;;保存初始设置
     *error*  yyerr
     oldcmd (getvar "cmdecho")
     oldos (getvar "osmode")
  )
  (setvar "cmdecho" 0)    ;命令不回显
  (setvar "osmode" 0);;;取消捉
  (setq nn 1)
  (if (= mm nil)
    (progn
       (setq mm (getreal "\n 输入绘图比例1:mm<1:1>"))
         (if (= mm nil)(setq mm 1))
    )
  )
  (wz_sz)
  (tc_sz)
  ;;;;(setq mmm (* 10 mm))
  (setvar "ORTHOMODE" 0)
  (setq p1 1)
  (while p1
        (setq p1 (getpoint "选定标注零件插入点 <停止标注>:"))
 (if p1 ;;*
    (progn
;;;  (setq p1 (getpoint "指定件号点："))
  (setq p2 (getpoint p1 "指定标注点："))
  (if (= no nil)
  (setq no (getint "起始序号<1>"))
  )
  (if (= no nil) (setq no 1))
  (setvar "ORTHOMODE" 1)
  (setq p4 (getpoint p2 "指定标注方向点："))
  (setq xx (- (car p4) (car p2)))
  (setq yy (- (cadr p4) (cadr p2))) 
  (setq r (* 8 mm))
  (setq n (getint "\n零件数量N＝?<1>"))
  (if (= n nil) (setq n 1))
  ;(setq n (- n 1));;********
  (setq x (-(car p2)(car p1)))
  (setq y (-(cadr p2)(cadr p1)))
  (setq a (atan (/(abs y)(abs x))))
  (setq y1 (* (sin a) r))
  (setq x1 (* (cos a) r))
        (cond ((and (>= x 0)(>= y 0))
	       (setq p3 (list (+(car p2) x1)(+ (cadr p2) y1)))
	       )
	      ((and (< x 0)(> y 0))
	       (setq p3 (list (-(car p2) x1)(+ (cadr p2) y1)))
	       )
	      ((and (< x 0)(< y 0))
	       (setq p3 (list (-(car p2) x1)(- (cadr p2) y1)))
	       )
	      ((and (> x 0)(< y 0))
	       (setq p3 (list (+(car p2) x1)(- (cadr p2) y1)))
	       )
	 )
  (command "color" "7")
  (command "donut" 0 0.8 p1 "")
  (command "line" p1 p2 "")
  (if (or (<= n 1) (and (> x 0)(< y 0)) (and (> x 0)(= yy 0)) (and (< x 0)(< y 0)(= xx 0)))
    (progn
         (command "circle" p3 (* 8 mm) "")
;;;  (if (and (< x 0)(> n 0))
;;;  (setq no1 (+ no n 1))
;;;  (setq p3 (polar p3 pi (* n 2 r)))
         (command "text" "j" "m" p3 "0" no);**********
         (setq no (+ 1 no))
      )
    )
;;;(if (<= n 0) (command "text" "j" "m" p3 "0" no));**********
;;;  (setq no (+ 1 no))
  (setq x (-(car p4)(car p2)))
  (setq y (-(cadr p4)(cadr p2)))
    (if (and (= x 0) (> y 0))
		  (progn
		    (setq x1 0)
		    (setq y1 (* 2 r))
		   )
    )
    (if (and (= x 0) (< y 0))
		  (progn
		    (setq x1 0)
		    (setq y1 (* 2 (- 0 r)))
		   )
    )
   (if (and (= y 0) (> x 0))
		  (progn
		    (setq y1 0)
		    (setq x1 (* 2 r))
		   )
		 
    )
   (if (and (= y 0) (< x 0))
		  (progn
		    (setq y1 0)
		    (setq x1 (* 2 (- 0 r)))
		   )
		 
    )


  
    (if (and (< x 0)(> n 0))
	  (setq p3 (polar p3 pi (* 2 n r)))
    )
    (if (and (> y 0)(> n 0))
	  (setq p3 (polar p3 (* pi 0.5) (* 2 n r)))
    )
 (if (or (<= n 1) (> x 0) (and (> x 0)(= yy 0)) (and (< x 0)(< y 0)(= xx 0))) (setq n (- n 1)))
 (if (or (and (< y 0) (= xx 0)) (and (< y 0) (= xx 0))) (setq n (- n 1)));;88888888888
    (setq nn0 1)	     
	(while (<= nn0 n)
     (cond ((and (< x 0)(> n 1))
	      (progn
	          (setq p3 (list (- (car p3) x1)(+ (cadr p3) y1)))
	  (command "circle" p3 (* 8 mm) "")
	  (command "text" "j" "m" p3 "0" no )
          (setq no (+ 1 no))
	  (setq nn0 (+ nn0 1))
		)
	    )
            ((and (> y 0) (> n 1))
	        (progn
        	  (setq p3 (list (+ (car p3) x1)(- (cadr p3) y1)))
	  (command "circle" p3 (* 8 mm) "")
	  (command "text" "j" "m" p3 "0" no )
          (setq no (+ 1 no))
	  (setq nn0 (+ nn0 1))
		  )
	     )
	    ((OR (> x 0) (< y 0))
	         (progn
	          (setq p3 (list (+ (car p3) x1)(+ (cadr p3) y1)))
	  (command "circle" p3 (* 8 mm) "")
	  (command "text" "j" "m" p3 "0" no )
          (setq no (+ 1 no))
	  (setq nn0 (+ nn0 1))
         	)
	     )
	  )
   )
      (setvar "ORTHOMODE" 0)
    )
   )
    )
    (command "redraw")
(setq *error* olderr)   ;;;恢复原设置
    (SETVAR "CMDECHO" OLDCMD)
    (setvar "osmode" oldos)
    (princ)
)