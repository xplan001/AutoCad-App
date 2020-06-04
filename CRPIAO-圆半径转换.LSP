;本段程序可将选中的圆一次性转为你所需要的半径。

;如果有什么BUG，请联系我：piaoyj@szmedi.com.cn
(Defun c:cr () 
 (command "redraw")
 (command "undo" "begin")
 (setq pbx (getvar "pickbox"))
 (setvar "pickbox" 3)
 (setq ucsfl (getvar "ucsfollow"))
 (setvar "ucsfollow" 0)
 (command "ucs" "world")			;转到世界坐标系

 ( setq R (getreal "\ninput R "))	；输入圆半径
 (setq fil (ssget))			；选择圆
 (setq len (sslength fil))		；确定选中圆的个数
 (setq n 0)

 (while (<= n (- len 1))
  (progn 
   (setq e (ssname fil n))
   (setq ed (entget e))
   (setq b nil) (setq c nil)
   (if (/= "CIRCLE" (cdr (assoc 0 ed)))
    (setq n (+ 1 n))
    (progn 
     (setq R0 (cdr (assoc 40 ed)))	;old circle's radius
     (setq pt0 (cdr (assoc 10 ed)))	;old circle's center
     (setq ed (subst (cons 40 R) (assoc 40 ed) ed))
     (setq n (+ 1 n))
     (entmod ed)
(command "zoom" "c" pt0 "15")		;可调整该数字以适应不同半径的圆
     (setq ang 0)
     (while (< ang (* 2 pi))
      (setq pt1 (polar pt0 ang R0))
      (setq ang (+ ang (/ pi 30)))
      (setq pt2 (osnap pt1 "endp"))
      (setq b (cons pt2 b))
     )

(setq ln (length b))
(setq lnn (- ln 1))
(setq i 0)
(setq pt (nth i b))
(while (<= i lnn)
  (setq pt (nth i b))
  (setq j (+ i 1))
  (setq jt (nth j b))
  (while (and (not (equal pt jt)) (< j lnn))
    (setq qt (nth j b))
    (setq j (+ 1 j))
    (setq jt (nth j b))
  )
  (if (not (equal pt jt)) (setq c (cons pt c)))
  (setq i (+ 1 i))
)
  (setq k 0)
  (setq L (length c))
;*************************************

  (while (<= k (- L 1))
    (setq pt (nth k c))
       (if (> R0 R)
          (command "extend" e "" pt "")
          (command "trim" e "" pt "")
       )

    (setq k (+ 1 k))
   )；while结束

;*************************************
  )
 )
    )
   )
 (command "ucs" "p")			;返回用户坐标系
 (setvar "ucsfollow" ucsfl)
 (setvar "pickbox" pbx)
 (command "undo" "end")
(princ)      
  
)


