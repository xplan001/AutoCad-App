(defun C:ets(/ s1 ename ti% p11 p10 f en p11n p10n fn fn1)
    (princ "ET-Supper Made by LeaLeung.\n")	;超级ET程式制造者：黎亮!
    (princ "Select Text to change:\n")
    (setq 
        s1
        (ssget '((0 . "TEXT")))		;构造针对TEXT的选择集
    )

    (setq ti% 0)
    (if (/= s1 nil)
     (progn
       (while 
        (<= 
                ti% 
                (- (sslength s1) 1)
        )
        (setq ename(ssname s1 ti%))
        (setq p11 nil)			;将p11置空
        (setq e (entget ename))		;取实体表e
        (setq p11 (assoc  11 e))	;取对齐点
             (setq p10 (assoc 10 e))	;取其始点表
             (setq f (assoc 10 e))	;如上
             (setq en e)
             (if (= (cdr f) 0) (setq p11n (cons 11 (cdr p10))))

             (if (/= (cdr f ) 0)
               (progn
                 (setq p10n (cons 10 (cdr p11)));将对齐点位构成其始点表赋给p10n
                 (setq en (subst p10n p10 en))	;用新的其始点位更换旧的其始 点表
               )
              )
              (setq fn 0)
              (setq fn (cons 72 fn))		;(72 . 0) 为左齐
              (setq fn1 (assoc 72 en))		;取旧的对齐方式
              (setq e (subst fn fn1 e))		;更换旧的对齐方式为左齐
              (entmod e)
              (setq ti%(+ 1 ti%))
         )  ;end while
      ) ;endprogn s1/=nil
    )  ;endif  s1 /= nil
    (setq s1 nil)
    (princ "Made by LeaLeung")	;LeaLeung 就是我 黎亮!
)
(princ "执行ets命令即可将你选择文字的对齐方式改为左对齐")
(princ )