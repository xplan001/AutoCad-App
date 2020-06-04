;;;本文件与bgao.lsp相比，可在用户坐标系下标示选取点的绝对坐标。
;技巧：（按一下F8键试试，你可能会有一点惊喜哟） 
;如果有什么BUG，请联系我：piaoyj@szmedi.com.cn
;启动命令为zb
(defun c:bz(/ CHO os p0 pxx pyy px py pp entl a b pa ppp paa pbb le len sc alph alf p1 p2 p3 ent pd p11 p12 p21)

 (command "undo" "begin")
 (setq CHO (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)

 ;(command "style" "txtp" "txt,hztxt" "" "" "" "" "" "")



(while (equal h1 nil)
  (setq h1 (getreal "\n 请输入字体高度:"))
;(command "LAYER" "Make" "gaobz" "")
 )   
(setq os (getvar "osmode"))
   
(setvar "osmode" 37)


(setq p0 (getpoint "\n 请选择插入点:"))

(setq pxx (car p0))
(setq pyy (nth 1 p0))
(setq px (rtos pxx 2 3))
(setq py (rtos pyy 2 3))
(setvar "osmode" 0)
(setq pp (getpoint "\n 请选择引出点:"))	;引出点pp
(command "line" p0 pp nill)

(command "line" "" pause "")


(setq entl (entget (entlast)))
(setq a (assoc 10 entl))
(setq b (assoc 11 entl))
(setq pa (cdr a))		;引出点pa
(setq ppp (cdr b))		;引出点pb
(setq paa (trans pa 0 1))
(setq pbb (trans ppp 0 1))		;引出点的用户坐标
(setq le (distance paa pbb))		;两点间距离	
(setq len (* 4.3 h1))
(setq sc (/ len le))
(if (< le len)
  (progn 
   (command "scale" (entlast) "" paa sc "")
   )
)
(setq alph (angle paa pbb))		;引出两点的弧度alph 
(setq alf (* 180.0 (/ alph pi)))	;求出角度值alf
(setq p1 (polar paa alph (* 0.4 h1)))		
(setq p2 (polar p1 alph (* 4 h1)))
(setq p3 (polar p2 alph 1000))
(setq ent (entlast))
(command "break" ent p2 p3)		;将多余的线剪掉

(if (>= alf 105) 
(if (>= alf 255)
 (progn
  (setq pd p1)
  (setq alfa alf)
  (setq p11 (polar pd (+ 1.5708 alph) (* 0.3 h1)))
  (setq p12 (polar pd (+ 4.7124 alph) (* 1.3 h1)));定出文本起点
  )
 (progn
  (setq p21 (polar p2 (+ pi alph) h1))	;
  (setq p11 (polar p21 (- alph 1.5708) (* 0.3 h1)))
  (setq p12 (polar p21 (+ alph 1.5708) (* 1.3 h1)));定出文本起点
  (setq alfa (+ alf 180))
  )
  )
				;当alf大于105时
 (progn
  (setq pd p1)
  (setq alfa alf)
  (setq p11 (polar pd (+ 1.5708 alph) (* 0.3 h1)))
  (setq p12 (polar pd (+ 4.7124 alph) (* 1.3 h1)));定出文本起点
  )
)

(command "text" p11 h1 alfa "DADA")
(command "text" p12 h1 alfa "ADAD")

(setvar "osmode" os)			;返回原捕捉模式
 (command "undo" "end")
 (setvar "CMDECHO" CHO)
 (princ)


)