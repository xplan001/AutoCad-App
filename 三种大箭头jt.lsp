(defun C:jt()
  (prompt "\n绘制箭头")
  (setvar "cmdecho" 0)
  (setq oldmode (getvar "osmode"))
  (setvar "osmode" 0)  ;关闭扑捉
  
  (initget "A B C")
  (setq enda (getkword "\n直箭头A/弯箭头B/大弯箭头<C> <A>")
        enda (if enda enda "A"))
         
  (while (setq p1 (getpoint "\n箭头的尖端位置:"))
        (setq p2 (getpoint p1 "\n箭头的另一端:")
              dd (distance p1 p2))
  (prompt (rtos dd 2 4))
  (setq w (* dd 1.2)
        an (angle p1 p2)
        p3 (polar p2 (+ an (* pi 0.5)) (/ w 2.0))
        p4 (polar p2 (+ an (* pi 1.5)) (/ w 2.0)))
  
  (if (= enda "A")
  (progn
  (command "solid" p1 p3 p1 p4 ""
           "pline" p2 "w" (* w 0.4) (* w 0.4) (polar p2 an (getdist p2)) "")
   )
   )
   (if (= enda "B")
   (command "pline" p1 "w" "0" w p2 "w" (* w 0.4) (* w 0.4) "a" pause "")
   )
   (if (= enda "C")
   (command "pline" p1 "w" "0" w p2 "w" (* w 0.4) (* w 1.2) "a" pause "")
    )
    (if (= p1 nil) (exit))
    )
  (setvar "osmode" oldmode)
  (prin1)
)

(princ "\n本程序采摘自www.lisp123.com更多内容敬请期待！")
(princ "\n本程序命令：JT")
(princ)