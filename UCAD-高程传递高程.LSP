;*************************** Match Elevation ***********************************
;在两条或多条线间传递“高程”
(defun c:Mel1()
 (setq a (ssget) n (sslength a) i 0)  
 (while (< i n)
    (setq ed (entget (ssname a i)) na (cdr (assoc 0 ed)))
    ;-----------------Change elevation----------------------------
    (if (= na "LWPOLYLINE") 
        (progn    
          (setq elt (cdr (assoc 38 ed))) 
          (if (= i 0) (progn (setq elt0 elt) (princ (strcat "\n第1条线高程为" (rtos elt0 2 3)))))
          (if (/= i 0) (setq ed (subst (cons 38 elt0) (assoc 38 ed) ed)))
          (princ (strcat "\n第" (itoa (+ i 1)) "条线(Lwpoline)高程从 " (rtos elt 2 3) " 到 " (rtos elt0 2 3) ", 已完成!"))
        );p
    );i
    (if (= na "LINE")
        (progn
          (setq pt10 (assoc 10 ed) pt11 (assoc 11 ed) elt (cadddr pt10))  
          (if (= i 0) (progn (setq elt0 elt) (princ (strcat "\n第1条线高程为" (rtos elt0 2 3)))))
          (if (/= i 0) 
              (setq pt10 (subst elt0 (last pt10) pt10) pt11 (subst elt0 (last pt11) pt11)
                      ed (subst pt10 (assoc 10 ed) ed) ed (subst pt11 (assoc 11 ed) ed)
              )
          );i         
          (princ (strcat "\n第" (itoa (+ i 1)) "条线(Line)高程从 " (rtos elt 2 3) " 到 " (rtos (+ elt0 (* i dte)) 2 3) ", 已完成!"))
        );p
    );i
   ;-----------------Change color-------------------------------- 
    (if (or (= na "LWPOLYLINE") (= na "LINE"))
     (if (/= (assoc 62 ed) nil) 
         (progn
            (setq ysh (cdr (assoc 62 ed)))
            (if (= i 0) (setq ysh0 ysh))
            (if (/= i 0)(setq ed (subst (cons 62 ysh0) (assoc 62 ed) ed)))
         );p
        (progn
            (setq ysh (cdr (assoc 62 (tblsearch "LAYER" (cdr (assoc 8 ed))))))
            (if (= i 0) (setq ysh0 ysh))
            (if (/= i 0) (setq ed (append ed (list (cons 62 ysh0)))))
        );p
     );i
   );i       
   (entmod ed)  
   (setq i (1+ i))
 );w
 (princ)
)
;======================================================================
;在两条或多条线间按等高差传递“高程”
(defun c:Mel2()
 (setq a (ssget) n (sslength a) i 0)
 (setq dte (getreal "地形线间等高距<1.0 m>:"))
 (if (= dte nil) (setq dte 1.0))
 (setq c1 (getvar "USERS1"))
 (while (< i n)
    (setq en0 (ssname a i))
    (setq ed (entget en0) na (cdr (assoc 0 ed)))

    ;-----------------Change elevation----------------------------
    (if (= na "LWPOLYLINE") (progn    
          (setq elt (cdr (assoc 38 ed))) 
          (if (= i 0) (progn (setq elt0 elt) (princ (strcat "\n第1条线高程为" (rtos elt0 2 3)))))
          (if (/= i 0) (setq ed (subst (cons 38 (+ elt0 (* i dte))) (assoc 38 ed) ed)))
          (princ (strcat "\n第" (itoa (+ i 1)) "条线(Lwpoline)高程从 " (rtos elt 2 3) " 到 " (rtos (+ elt0 (* i dte)) 2 3) ", 已完成!"))
        );p
    );i
    (if (= na "LINE")
        (progn
          (setq pt10 (assoc 10 ed) pt11 (assoc 11 ed) elt (cadddr pt10))  
          (if (= i 0) (progn (setq elt0 elt) (princ (strcat "\n第1条线高程为" (rtos elt0 2 3)))))
          (if (/= i 0) 
              (setq pt10 (subst (+ elt0 (* i dte)) (last pt10) pt10) pt11 (subst (+ elt0 (* i dte)) (last pt11) pt11)
                      ed (subst pt10 (assoc 10 ed) ed) ed (subst pt11 (assoc 11 ed) ed)
              )
          )          
          (princ (strcat "\n第" (itoa (+ i 1)) "条线(Line)高程从 " (rtos elt 2 3) " 到 " (rtos (+ elt0 (* i dte)) 2 3) ", 已完成!"))
        );p
    );i
   ;-----------------Change color--------------------------------  
    (if (or (= na "LWPOLYLINE") (= na "LINE"))   
     (if (/= (cdr (assoc 5 ed)) c1)
      (if (/= (assoc 62 ed) nil)
          (setq ysh0 (cdr (assoc 62 ed))
                  ed (subst (cons 62 (if (< ysh0 5) (+ ysh0 5)(- ysh0 5))) (assoc 62 ed) ed)
          )
          (setq ysh0 (cdr (assoc 62 (tblsearch "LAYER" (cdr (assoc 8 ed)))))
                  ed (append ed (list (cons 62 (if (< ysh0 5) (+ ysh0 5)(- ysh0 5)))))
          )
      );i   
     );i
    );i
    (if (= i (- n 1)) (setvar "USERS1" (cdr (assoc 5 ed))))    
    (entmod ed)  
    (setq i (1+ i))
  );w
 (princ)
)
;========================================================================
;当你选择了一任意高程(KER块)后，如果再选其他点（数目任
;意），本命令'pel'能自动标出相对于已选任意点的高程。
(defun c:Pel()
 (setvar "cmdecho" 0)
 (setq ed (entget (ssname (ssget) 0)) pt (cdr (assoc 10 ed)))
 (setq fn (entnext (CDR (Assoc -1 ed))))
 (setq pt0 (trans (list (car pt) (cadr pt) (caddr pt)) 0 1))
 (setq kk T)
 (while kk
  (setq ed (EntGet fn))
  (if (= (cdr (assoc 0 ed)) "ATTRIB") (setq ele (cdr (assoc 1 ed))))
  (If (= (CDR (Assoc 0 ed)) "SEQEND")
                        (Setq kk Nil)
                        (Progn
    			(setq fn (EntNext fn))
                        ;(setq ed (entget fn))
    			)
  )
 )  
 (setq ele (atof ele))
 (setq k (getreal "Given the Proportional Factor<1.0>:"))
 (if (= k nil) (setq k 1.0))
 (setq scale (/ (getvar "userr2") 6.0))
 (setq eg0 (cadr pt0))
 (setq pp pt0)
 (while (/= pp nil)
  (setq pt (getpoint "Please Given Point:\n"))
  (setq eg (cadr pt))
  (if (/= pt nil) (setq ev (+ ele (/ (- eg eg0) k))))
  (if (/= pt nil) (setq ev (rtos ev 2 3)))
  (if (/= pt nil) (command "insert" "ker" pt scale scale 0 ev))
  (setq pp pt)
 )
)
;************************************************************************
(defun c:P01()
  (setvar "cmdecho" 0) 
  (setq pt1 (getpoint "\n 请输入一个角点:"))
  (setq pt2 (getcorner pt1 "\n 请输入另一角点:"))
  (princ "\n 请等候....")
  (setq a (ssget "C" pt1 pt2))
  (setq n (sslength a))
  (setq i 0)
  (while (< i n)
   (setq en (ssname a i))
   (setq ed (entget en))
   (setq ef (assoc 0 ed))
   (setq na (cdr ef))
   (if (or (= na "POLYLINE")(= na "LINE")) (progn
       (command"pedit" en "" "")
       )
   )
   (setq i (+ i 1))   
 )
 (princ "你所选择范围内的实体己转变成多义线!")
 (princ)
)
(defun c:P02()
  (setvar "cmdecho" 0)
  (setq pt1 (getpoint "\n 请输入一个角点:"))
  (setq pt2 (getcorner pt1 "\n 请输入另一角点:"))
  (princ "\n 请等候....")
  (setq a (ssget "C" pt1 pt2))
  (setq n (sslength a))
  (setq i 0)
  (while (< i n)
   (setq en (ssname a i))
   (setq ed (entget en))
   (setq na (cdr (assoc 0 ed)))
   (setq nb (cdr (assoc 62 ed)))
   (if (and (= na "LWPOLYLINE")(or (= nb 2) (= nb 3))) (progn
        (command"pedit" (ssname a i) "j" "all" "" "")            
       )
   )
   (setq i (+ i 1))
  )
  (princ "多义线连接己完成!")
  (princ)
)

;本命令删出窗选范围内的非3DFACE实体
(defun c:P03()
  (setvar "cmdecho" 0) 
  (setq pt1 (getpoint "\n 请输入一个角点:"))
  (setq pt2 (getcorner pt1 "\n 请输入另一角点:"))
  (setq a (ssget "C" pt1 pt2))
  (setq n (sslength a))
  (setq i 0 ii 0)
  (while (< i n)
   (setq en (ssname a i))
   (setq ed (entget en))   
   (setq ef (cdr (assoc 0 ed)))
   (if (or (/= ef "3DFACE") (= ef "LWPOLYLINE"))
       (progn
        (command"erase" en "")
        (setq ii (+ ii 1))
       )       
   )
   (setq i (+ i 1))
   (princ)
  )
  (setq sm (strcat "\n 你选择的范围内有" (itoa ii) "个非3DFACE实体被删出!"))
  (princ sm)
  (princ)
)


;***************************** Match Properties *******************************
;在两个或多个文本实体间传递字高
(defun c:MPh()
 (setq a (ssget))
 (setq ed0 (entget (ssname a 0)) na (cdr (assoc 0 ed0)))
 (if (= na "TEXT") (progn (setq ew0 (cdr (assoc 40 ed0))) (princ "")))
 (setq n (sslength a) i 1)
 (while (< i n)
      (setq ed (entget (ssname a i)) na (cdr (assoc 0 ed)))
      (if (= na "TEXT") 
        (progn             
          (setq ed (subst (cons 40 ew0) (assoc 40 ed) ed))
        );p
      );i
  (entmod ed)
  (setq i (1+ i))
  );w
  (princ)
 )
;======================================================================
;在两个或多个实体间传递“层”
(defun c:Mpl()
 (setq a (ssget))
 (setq ed0 (entget (ssname a 0)))
 (setq ef0 (assoc 8 ed0))
 (setq eg0 (cdr ef0))
 (princ "\n" )
 (setq n (sslength a) i 1)
 (repeat (- n 1)
  (setq en1 (ssname a i))
  (setq i (+ i 1))
  (setq ed1 (entget en1))
  (setq ef1 (assoc 8 ed1))
  (setq eg1 (cdr ef1)) 
  (setq ed1 (subst (cons 8 eg0) ef1 ed1))   
  (entmod ed1)
  (princ)
 )
)
;======================================================================
;在两个或多个文本实体间传递文本内容
(defun c:MPt()
 (setq a (ssget))
 (setq ed0 (entget (ssname a 0)) na (cdr (assoc 0 ed0)))
 (if (= na "TEXT") (progn (setq ew0 (cdr (assoc 1 ed0))) (princ "")))
 (setq n (sslength a) i 1)
 (while (< i n)
      (setq ed (entget (ssname a i)) na (cdr (assoc 0 ed)))
      (if (= na "TEXT") 
        (progn             
          (setq ed (subst (cons 1 ew0) (assoc 1 ed) ed))
        );p
      );i
  (entmod ed)
  (setq i (1+ i))
  );w
  (princ)
)
;======================================================================
;在两个或多个实体间传递多义线宽
(defun c:MPw()
 (setq a (ssget))
 (setq ed0 (entget (ssname a 0)) na (cdr (assoc 0 ed0)))
 (if (= na "LWPOLYLINE") (progn (setq ew0 (cdr (assoc 43 ed0))) (princ "")))
 (setq n (sslength a) i 1)
 (while (< i n)
      (setq ed (entget (ssname a i)) na (cdr (assoc 0 ed)))
      (if (= na "LWPOLYLINE") 
        (progn             
          (setq ed (subst (cons 43 ew0) (assoc 43 ed) ed))             
        );p
      );i
  (entmod ed)
  (setq i (1+ i))
  );w
  (princ)
 )

;************************************************************************
;It change '引4+7845.125' to '引47+845.125'.
(defun c:Pf()
 (princ "Please choose the objects that you have to change:\n")
 (setq a (ssget))
 (setq n (sslength a) i 0)
 (repeat n
  (setq en (ssname a i))
  (setq ed (entget en))
  (setq ef (assoc 1 ed))
  (setq eg (cdr ef))
  (setq nn (strlen eg))
  (setq aa (substr eg (- nn 6) 7))
  (setq ab (substr eg 1 3))
  (setq ac (substr eg 4 1))
  (setq ad (substr eg 5 1))
  (setq na (strcat ab ad ac aa))
  (setq ed (subst (cons 1 na) ef ed))
  (entmod ed)
  (setq i (+ i 1))  
 )
 (princ)
)

;************************************************************************

(defun c:Pth() 
 (setq aa (getstring "\n  请给出需要替换的文本值:"))
 (setq bb (getstring "\n  请给出用来替换的文本值:"))
 (setq pt1 (getpoint "\n 请给出被替换文本所在范围的一个角点:"))
 (setq pt2 (getcorner pt1 "\n 请给出被替换文本所在范围的另一个角点:"))
 (setq a (ssget "C" pt1 pt2))
 ;(setq a (ssget "x" (list (cons 0 "text"))))
 (setq n (sslength a))
 (setq i 0)
 (while (< i n)
        (setq en (ssname a i))
        (setq ed (entget en))
        (setq ef (assoc 1 ed))
        (setq na (cdr ef) nb (cdr (assoc 0 ed)))
        (if (and (= na aa)(= nb "TEXT")) (progn 
         (setq ed (subst (cons 1 bb) ef ed))         
         (entmod ed)
         )
        )
        (setq i (+ i 1))                
 )
 (princ "\n 替换己完成!")
 (princ)
)
;**********************************标注多义线尺寸、高程**************************************
(defun c:zd()
  (setvar "cmdecho" 0)
  (setq os (getvar "osmode"))
  (if (/= os 0) (setvar "osmode" 0))
  (princ "请选择需要标注尺寸、高程的多义线:\n")  
  (setq a (ssget)) 
  (setq con1 (getstring "需要标注高程吗?<N>:\n"))  
  (if (or (= con1 "Y")(= con1 "y")) (setq ele (getreal "请输入起始点高程:\n")))
  (setq dip (* -2 (getvar "dimexe")))
  (setq nf (entget (ssname a 0)))
  (setq pt0 (trans (cdr (assoc 10 nf)) 0 1))
  (if (or (= con1 "Y") (= con1 "y")) (command "insert" "ker" pt0 0.5 0.5 0 (rtos ele 2 3))) 
  (setq n (length nf) zl 0 zc 0)  
  (command"layer" "s" "text" "")
  (setq i 0 ii 0)
  (repeat n
          (setq ne (car (nth i nf)))
          (if (= ne 10) 
              (progn
              (setq pt1 (trans (cdr (nth i nf)) 0 1))              
              (if (and (/= ii 0) (/= pt0 pt1))
                (progn                                
                (setq len (distance pt0 pt1))
                (setq ang (angle pt0 pt1))
                (if (or (= con1 "Y") (= con1 "y")) 
                    (progn
                    (setq ele (+ ele (* len (sin ang))))
                    (setq ev (rtos ele 2 3))                     
                    )
                )
                (setq pt2 (polar pt0 (- ang 1.5707963) dip))               
                (if (/= len 0.0) (progn
                (command"dimaligned" pt0 pt1 pt2)                
                (if (or (= con1 "Y") (= con1 "y")) (progn                     
                    (if (and (>= ang 3.14159)(< ang 6.28319))(command "insert" "kel" pt1 0.5 0.5 0 ev))
                    (if (and (>= ang 0.00000)(< ang 3.14159))(command "insert" "ker" pt1 0.5 0.5 0 ev))
                    )
                )
                )
                )
                (setq pt0 pt1)
                ) 
              )
              (setq ii (+ ii 1))
              )        
           )
           (setq i (+ i 1))         
  )
  (command"layer" "s" "0" "")
  (setvar "osmode" os)
  (princ)
)
;************************************************************************
(defun c:Ucad()
 (princ "    在两个或多个多义线实体间传递'高程'，请用'Mel1'命令!\n")
 (princ "    在两个或多个多义线实体间按等高差传递'高程',请用'Mel2'命令!\n")
 (princ "    标注相对于已知点的高程，请用'Pel'命令!\n")
 (princ "    在两个或多个文本实体间传递'字高'，请用'Mph'命令!\n")
 (princ "    在两个或多个实体间传递'层'，请用'Mpl'命令!\n")
 (princ "    替换文本内容, 请用'Mpt'命令!\n")
 (princ "    在多义线间传递线宽, 请用'Mpw'命令!\n")
 (princ "    如需标注多义线各段长度、各点高程，请用'Zd'命令!\n")
 (princ)
)
(princ "Ucad.lsp已成功安装,键入Ucad可得到命令使用提示!\n")