;           =======================================
;           |               层操作                |
;           |      Ver: 1.2   作者: 李新村        |  ;(setq en (ssget)) (setq en (ssname ss 0)) (setq a (entget en))
;           |     2003.08.25(XIU)                 |
             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;

(defun TCGJ(OPT1 / n la ss len n m a) 
   (setvar "cmdecho" 0)
   (setvar "expert"  0)
   (initget "E T F U L O ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (IF (= "" opt1 )
    (setq OPT1 (getkword "\n删除E/全部打开T/关闭所选O/关闭F/全部解锁U/<锁定L>: "))
   )
  (if ( OR (= OPT1 "L")
           (= OPT1 NIL)
      )
   (progn
      (prompt "\n请选择不被锁定的层上的物体!")
      (setq ss (ssget))
      (if (/= nil ss)
       (progn
            (setq en (ssname ss 0))
            (setq a (entget en))
            (setq la (cdr(assoc 8 a)))
            (command "layer" "lo" "*" "")
            (command "layer" "s" la "")
            (setq len (sslength ss))
            (setq n 1)
            (while (<= n len)
              (setq en (ssname ss (1- n)))
              (setq a (entget en))
              (setq la (cdr(assoc 8 a)))
              (command "layer" "u" la "")
              (setq n (1+ n))
           );END WHILE
        );END PROGN 
      );END IF
     (prompt "\n未选中的图元已被关闭!")     
    );END PROGN
 );END IF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (if (= OPT1 "F")
      (progn
        (prompt "\n请选择不被关闭的层上的物体!")
        (setq ss (ssget))
        (if (/= nil SS)
           (progn
             (setq en (ssname ss 0))
             (setq a (entget en))
             (setq la (cdr(assoc 8 a)))
             (command "layer" "s" la "")
             (command "layer" "OFF" "*" "y" "")
             (setq len (sslength ss))
             (setq n 1)
             (while (<= n len)
                (setq en (ssname ss (1- n)))
                (setq a (entget en))
                (setq la (cdr(assoc 8 a)))
                (command "layer" "ON" la "")
                (setq n (1+ n))
             )
           )
        )
        (prompt "\n未选中的图元已被锁!")
     )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (if (= OPT1 "O")
   (progn
     (setq m 1)
     (while (= m 1)
       (setq m 0) 
       (prompt "\n请选择要被关闭的层上的物体!")
       (setq ss (ssget))   
       (if (/= nil ss)
         (progn
            (setq en (ssname ss 0))
            (setq a (entget en))
            (setq la (cdr(assoc 8 a)))
            (setq len (sslength ss))
            (setq n 1)
            (while (<= n len)
              (setq en (ssname ss (1- n)))
              (setq a (entget en))
              (setq la (cdr(assoc 8 a)))
              (cond 
                  ((= la (getvar "CLAYER"))
                   (command "layer" "OFF" la "YES" "")
                  )
                  ((/= la (getvar "CLAYER"))
                   (command "layer" "OFF" la "")
                  )
              )
              (setq n (1+ n))
           )
	  ；(prompt "\n没选中的图元!")
	   (prompt "\n选中的图元已被关闭!")
        )
     )
     (setq m 1)
   )   
  )
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (if (= OPT1 "E")
    (progn
       
      (progn
        (setq en (car (entsel "\n在层上选一个物体 : ")))
        (if (/= en nil) (setq la (assoc 8 (entget en))))
      )
  

     (if (/= nil en)
       (progn
          (setq ss (ssget "X" (list la)))
          (setq len (sslength ss))
          (setq n 1)
          (while (<= n len)
            (setq en1 (ssname ss (1- n)))
            (setq a (entget en1))
            (setq a (cdr (assoc -1 a)))
            (redraw a 3)
            (setq n (1+ n))
          )
          (initget "Y N")
          (setq a (getkword "\nNo/<Yes>: "))
          (if (or (= a "Y") (= a nil))
            (progn
             (command "erase" ss "")
             (princ (strcat "有" (itoa len) "个图元被删除 !"))
            )
          )
          (redraw)
        )
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (if (= OPT1 "T")
    (progn
      (command "layer" "ON" "*" "")
      (prompt "\n所有的层已打开!")
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (if (= OPT1 "U")
    (progn
      (command "layer" "U" "*" "")
    (prompt "\n所有的层已解锁!")
    )
  )
   (princ)  
   (setvar "cmdecho" 1)
   (setvar "expert"  1)
);END Defun TCGJ

;;main TCGJ
(defun c:TCGJ(/ opt1)
   (TCGJ "") 
)

;;main 1
(defun c:4(/ opt1)
   (TCGJ "L") 
)

(defun c:5(/ opt1)
   (TCGJ "U") 
)

(defun c:3(/ opt1)
   (TCGJ "F") 
)

(defun c:1(/ opt1)
   (TCGJ "T") 
)
(defun c:2(/ opt1)
   (TCGJ "O") 
)