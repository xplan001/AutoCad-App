

(defun c:lgb (/)
  (setq ssPj (ssget '((0 . "TEXT"))))
  (setq NssPj (sslength ssPj))
  (setq ssBm (ssadd))
  (setq ssWl (ssadd))
  (setq ssCol (ssadd))
  (setq TEMP 0)
  (repeat NssPj
    (setq ENT0 (ssname ssPj TEMP))
    (setq TEMP (+ TEMP 1))
    (setq ENTFULL (entget ENT0))
    (setq Ently (cdr (assoc '8 ENTFULL)))
    (cond
      ((= Ently "21000") (ssadd ENT0 ssBm))
      ((= Ently "24000") (ssadd ENT0 ssWl))
      ((= Ently "31000") (ssadd ENT0 ssCol))
      (t nil)
    )
  )
;;;上面的程序把选择集按图层分成了三块：梁配筋、墙配筋和柱配筋
  (setq NssPj (sslength ssBm))
  (setq ssBmGj (ssadd))
  (setq ssBmZj (ssadd))
  (setq ssBmNj (ssadd))
  (setq TEMP 0)
  (repeat NssPj
    (setq ENT0 (ssname ssBm TEMP))
    (setq TEMP (+ TEMP 1))
    (setq ENTFULL (entget ENT0))
    (setq Ently (cdr (assoc '1 ENTFULL)))
    (setq Ently (substr Ently 1 1))
    (cond
      ((= Ently "G") (ssadd ENT0 ssBmGj))
      ((= Ently "V") (ssadd ENT0 ssBmNj))
      (t (ssadd ENT0 ssBmZj))
    )
  )
;;;上面的程序把梁配筋分成了箍筋、纵筋、和抗扭筋三个选择集


  
  (setq NssBG (sslength ssBmGj))
  (setq N1 0)
  (repeat (- NssBG 1)
    (setq ENT0 (ssname ssBmGj N1))
    (setq N1 (+ N1 1))
    (setq ENTFULL (entget ENT0))
    (setq Ently (cdr (assoc '1 ENTFULL)))
    (setq ins0 (cdr (assoc '10 ENTFULL)))
    (cond
      ((= Ently " ") nil)
      (t
       (setq LenEnt0 (strlen Ently))
       (setq Nrpt1 1)
       (repeat LenEnt0
	 (setq StrXG (substr Ently Nrpt1 1))
	 (if (= StrXG "-")
	   (setq Flag1 Nrpt1)
	 )
	 (setq Nrpt1 (+ Nrpt1 1))
       )
       (setq StrGJ11 (substr Ently 2 (- Flag1 2)))
       (setq StrGJ21 (substr Ently (+ Flag1 1) (- LenEnt0 Flag1)))
       (setq StrGJ11 (atof StrGJ11))
       (setq StrGJ21 (atof StrGJ21))
       (setq N2 N1)
       (repeat (- NssBG N1)
	 (setq ENT1 (ssname ssBmGj N2))
	 (setq N2 (+ N2 1))
	 (setq ENTFULL1 (entget ENT1))
	 (setq Ently1 (cdr (assoc '1 ENTFULL1)))
	 (setq ins1 (cdr (assoc '10 ENTFULL1)))
	 (cond
	   ((= Ently1 " ") nil)
	   (t
	    (setq distToPt (distance ins0 ins1))
	    (cond
	      ((> distToPt 100.0) nil)
	      (t
	       (setq LenEnt0 (strlen Ently1))
	       (setq Nrpt1 1)
	       (repeat LenEnt0
		 (setq StrXG (substr Ently1 Nrpt1 1))
		 (if (= StrXG "-")
		   (setq Flag1 Nrpt1)
		 )
		 (setq Nrpt1 (+ Nrpt1 1))
	       )
	       (setq StrGJ12 (substr Ently1 2 (- Flag1 2)))
	       (setq
		 StrGJ22 (substr Ently1 (+ Flag1 1) (- LenEnt0 Flag1))
	       )
	       (setq StrGJ12 (atof StrGJ12))
	       (setq StrGJ22 (atof StrGJ22))
	       (if (> StrGJ12 StrGJ11)
		 (setq StrGJ11 StrGJ12)
	       )
	       (if (> StrGJ22 StrGJ21)
		 (setq StrGJ21 StrGJ22)
	       )
	       (setq stnr1 (assoc 1 ENTFULL1))
	       (setq stnr2 (cons 1 " "))
	       (setq ENTFULL1 (subst stnr2 stnr1 ENTFULL1))
	       (entmod ENTFULL1)
	       (setq stnr11 (assoc 1 ENTFULL))
	       (setq stnr21 (cons 1
				  (strcat "G"
					  (rtos StrGJ11 2 1)
					  "-"
					  (rtos StrGJ21 2 1)
				  )
			    )
	       )
	       (setq ENTFULL (subst stnr21 stnr11 ENTFULL))
	       (entmod ENTFULL)
	      )
	    )
	   )
	 )
       )
      )
    )
  )
;;;上面的程序处理了梁箍筋



  
  (setq NssBG (sslength ssBmNj))
  (setq N1 0)
  (repeat (- NssBG 1)
    (setq ENT0 (ssname ssBmNj N1))
    (setq N1 (+ N1 1))
    (setq ENTFULL (entget ENT0))
    (setq Ently (cdr (assoc '1 ENTFULL)))
    (setq ins0 (cdr (assoc '10 ENTFULL)))
    (cond
      ((= Ently " ") nil)
      (t
       (setq LenEnt0 (strlen Ently))
       (setq Nrpt1 1)
       (repeat LenEnt0
	 (setq StrXG (substr Ently Nrpt1 1))
	 (if (= StrXG "-")
	   (setq Flag1 Nrpt1)
	 )
	 (setq Nrpt1 (+ Nrpt1 1))
       )
       (setq StrGJ11 (substr Ently 3 (- Flag1 3)))
       (setq StrGJ21 (substr Ently (+ Flag1 1) (- LenEnt0 Flag1)))
       (setq StrGJ11 (atoi StrGJ11))
       (setq StrGJ21 (atof StrGJ21))
       (setq N2 N1)
       (repeat (- NssBG N1)
	 (setq ENT1 (ssname ssBmNj N2))
	 (setq N2 (+ N2 1))
	 (setq ENTFULL1 (entget ENT1))
	 (setq Ently1 (cdr (assoc '1 ENTFULL1)))
	 (setq ins1 (cdr (assoc '10 ENTFULL1)))
	 (cond
	   ((= Ently1 " ") nil)
	   (t
	    (setq distToPt (distance ins0 ins1))
	    (cond
	      ((> distToPt 100.0) nil)
	      (t
	       (setq LenEnt0 (strlen Ently1))
	       (setq Nrpt1 1)
	       (repeat LenEnt0
		 (setq StrXG (substr Ently1 Nrpt1 1))
		 (if (= StrXG "-")
		   (setq Flag1 Nrpt1)
		 )
		 (setq Nrpt1 (+ Nrpt1 1))
	       )
	       (setq StrGJ12 (substr Ently1 3 (- Flag1 3)))
	       (setq
		 StrGJ22 (substr Ently1 (+ Flag1 1) (- LenEnt0 Flag1))
	       )
	       (setq StrGJ12 (atoi StrGJ12))
	       (setq StrGJ22 (atof StrGJ22))
	       (if (> StrGJ12 StrGJ11)
		 (setq StrGJ11 StrGJ12)
	       )
	       (if (> StrGJ22 StrGJ21)
		 (setq StrGJ21 StrGJ22)
	       )
	       (setq stnr1 (assoc 1 ENTFULL1))
	       (setq stnr2 (cons 1 " "))
	       (setq ENTFULL1 (subst stnr2 stnr1 ENTFULL1))
	       (entmod ENTFULL1)
	       (setq stnr11 (assoc 1 ENTFULL))
	       (setq stnr21 (cons 1
				  (strcat "VT"
					  (itoa StrGJ11)
					  "-"
					  (rtos StrGJ21 2 1)
				  )
			    )
	       )
	       (setq ENTFULL (subst stnr21 stnr11 ENTFULL))
	       (entmod ENTFULL)
	      )
	    )
	   )
	 )
       )
      )
    )
  )
;上面程序处理了梁抗扭钢筋



  
  (setq NssBG (sslength ssBmZj))
  (setq N1 0)
  (repeat (- NssBG 1)
    (setq ENT0 (ssname ssBmZj N1))
    (setq N1 (+ N1 1))
    (setq ENTFULL (entget ENT0))
    (setq Ently (cdr (assoc '1 ENTFULL)))
    (setq ins0 (cdr (assoc '10 ENTFULL)))
    (cond
      ((= Ently " ") nil)
      (t
       (setq LenEnt0 (strlen Ently))
       (setq Nrpt1 1)
       (setq Flag1 0)
       (setq Flag2 0)
       (repeat LenEnt0
	 (setq StrXG (substr Ently Nrpt1 1))
	 (if (= StrXG "-")
	   (if (= Flag1 0) (setq Flag1 Nrpt1) (setq Flag2 Nrpt1)) 	   
	 )
	 (setq Nrpt1 (+ Nrpt1 1))
       )
       (setq StrGJ11 (substr Ently 1 (- Flag1 1)))
       (setq StrGJ21 (substr Ently (+ Flag1 1) (- Flag2 Flag1 1)))
       (setq StrGJ31 (substr Ently (+ Flag2 1) (- LenEnt0 Flag2)))
       (setq StrGJ11 (atoi StrGJ11))
       (setq StrGJ21 (atoi StrGJ21))
       (setq StrGJ31 (atoi StrGJ31))       
       (setq N2 N1)
       (repeat (- NssBG N1)
	 (setq ENT1 (ssname ssBmZj N2))
	 (setq N2 (+ N2 1))
	 (setq ENTFULL1 (entget ENT1))
	 (setq Ently1 (cdr (assoc '1 ENTFULL1)))
	 (setq ins1 (cdr (assoc '10 ENTFULL1)))
	 (cond
	   ((= Ently1 " ") nil)
	   (t
	    (setq distToPt (distance ins0 ins1))
	    (cond
	      ((> distToPt 100.0) nil)
	      (t
	       (setq LenEnt0 (strlen Ently1))
	       (setq Nrpt1 1)
	       (setq Flag1 0)
	       (setq Flag2 0)	       
	       (repeat LenEnt0
		 (setq StrXG (substr Ently1 Nrpt1 1))
		 (if (= StrXG "-")
		   (if (= Flag1 0) (setq Flag1 Nrpt1) (setq Flag2 Nrpt1)) 		  
		 )
		 (setq Nrpt1 (+ Nrpt1 1))
	       )
	       (setq StrGJ12 (substr Ently1 1 (- Flag1 1)))
	       (setq
		 StrGJ22 (substr Ently1 (+ Flag1 1) (- Flag2 Flag1 1))
	       )
	       (setq StrGJ32 (substr Ently1 (+ Flag2 1) (- LenEnt0 Flag2)))
	       (setq StrGJ12 (atoi StrGJ12))
	       (setq StrGJ22 (atoi StrGJ22))
	       (setq StrGJ32 (atoi StrGJ32))	       
	       (if (> StrGJ12 StrGJ11)
		 (setq StrGJ11 StrGJ12)
	       )
	       (if (> StrGJ22 StrGJ21)
		 (setq StrGJ21 StrGJ22)
	       )
	       (if (> StrGJ32 StrGJ31)
		 (setq StrGJ31 StrGJ32)
	       )
	       (setq stnr1 (assoc 1 ENTFULL1))
	       (setq stnr2 (cons 1 " "))
	       (setq ENTFULL1 (subst stnr2 stnr1 ENTFULL1))
	       (entmod ENTFULL1)
	       (setq stnr11 (assoc 1 ENTFULL))
	       (setq stnr21 (cons 1
				  (strcat
				          (itoa StrGJ11)
					  "-"
					  (itoa StrGJ21)
					  "-"
					  (itoa StrGJ31)
				  )
			    )
	       )
	       (setq ENTFULL (subst stnr21 stnr11 ENTFULL))
	       (entmod ENTFULL)
	      )
	    )
	   )
	 )
       )
      )
    )
  )
;;;上面的程序处理了梁纵筋  





 






  



  
  


)





