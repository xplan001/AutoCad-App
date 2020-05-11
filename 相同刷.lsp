;;; 　　　         《相同刷》v2.0
;;; ====================================================================
;;; 功能：如源对象为文字或属性，则目标文字、属性、块内文字内容刷成同内容
;;;       如源对象为天正文字，则刷目标天正文字内容相同，(不支持天正多行文字)
;;;       如源对象为圆，则目标圆大小刷成源圆的大小，
;;;       如源对象为块，则目标块刷成源块一样，
;;;       如源对象为多段线，则目标线、圆、圆弧、多段线等刷成同线宽
;;;       如源对象为线、尺寸、填充、圆弧，则目标特性匹配
;;; 使用：命令：xts，选择一个源对象，程序自动判断，再选择集
;;; 作者：langjs           qq:59509100         日期:2013年6月
(defun c:xts (/ #errxts $orr buk en1 ent i name name1 snap ss tp ty uu)
  (defun #errxts (s)         ; 出错处理程序
    (redraw name 4)
    (setvar "nomutt" 0)
    (setvar "PICKBOX" buk)
    (setvar "osmode" snap)
    (command ".UNDO" "E")
    (setq *error* $orr)
    (princ)
  )
  (setq $orr *error*)
  (setq *error* #errxts)
  (vl-load-com)          ; 主程序开始
  (setvar "cmdecho" 0)
  (command ".UNDO" "BE")
  (setq snap (getvar "osmode"))
  (setvar "osmode" 0)
  (setq buk (getvar "PICKBOX"))
  (setvar "PEDITACCEPT" 1)        ; 下面程序选择合适的源对象，如没选到重新选
  (while (not (and
  (setq name1 (nentsel "\n选择源对象:"))
  (setq name (car name1))
  (setq ent (entget name))
  (setq ty (cdr (assoc 0 ent)))
  (member ty '("TEXT" "MTEXT"
    "LWPOLYLINE" "CIRCLE"
    "INSERT" "LINE"
    "ARC" "HATCH"
    "DIMENSION" "ATTRIB"
    "TCH_ARROW" "TCH_TEXT"
    "TCH_DRAWINGNAME" "TCH_MULTILEADER"
    "TCH_ELEVATION"
   )
  )
       )
  )
    (if (= 52 (getvar "errno"))
      (vl-exit-with-error "")
    )
  )           ; 下面程序加了一个判断，如果源对象选择的是块，且不是属性或者块内文字，则认为选择的是块
  (if (and
(not (member ty '("TEXT" "MTEXT"
        "ATTRIB"
       )
      )
)
(= (type (car (last name1))) 'ename)
(= (cdr (assoc 0 (entget (car (last name1))))) "INSERT")
      )
    (setq name (car (last name1))
   ent (entget name)
   ty (cdr (assoc 0 ent))
    )
  )
  (redraw name 3)
  (setvar "nomutt" 1)
  (setvar "PICKBOX" (fix (+ 1 (* 1.2 buk))))
  (cond           ; 1、 如果源对象是文字、天正文字或者块内文字或者属性，则执行。。。
    ((member ty '("TEXT" "MTEXT"
       "ATTRIB" "TCH_TEXT"
       "TCH_ARROW" "TCH_DRAWINGNAME"
       "TCH_MULTILEADER" "TCH_ELEVATION"
      )
     )
      (setq uu (cdr (assoc 1 ent)))
      (princ (strcat "\n选择目标对象:<文字相同>  T = " "\"" uu "\""))
      (while t
(setq ss (ssget ":S" '((0 . "TEXT,MTEXT,INSERT,TCH_ARROW,TCH_TEXT,TCH_DRAWINGNAME,TCH_MULTILEADER,TCH_ELEVATION"))))
(if (= (caar (setq name1 (ssnamex ss 0))) 1) ; 如果目标文字是单选块内文字或者属性或普通文字，则执行。。。
   (progn
     (setq ent (ssname ss 0)
    en1 (car (nentselp (trans (cadr (last (car name1))) 0 1)))
    tp (cdr (assoc 0 (entget en1)))
     )
     (cond
       ((member tp '("TEXT" "MTEXT"
   "ATTRIB"
         )
        )
  (vla-put-textstring (vlax-ename->vla-object en1) uu)
  (entupd en1)
  (entupd ent)
       )
       ((member tp '("TCH_TEXT" "TCH_ELEVATION"
   "TCH_ARROW"
         )
        )
  (vlax-put-property (vlax-ename->vla-object en1) 'text uu)
       )
       ((= tp "TCH_DRAWINGNAME")
  (vlax-put-property (vlax-ename->vla-object en1) 'nametext uu)
       )
       ((= tp "TCH_MULTILEADER")
  (vlax-put-property (vlax-ename->vla-object en1) 'uptext uu)
       )
     )
   )
   (progn         ; 如果目标文字多选的是普通文字，则循环更新文字内容
     (repeat (setq i (sslength ss))
       (setq ent (entget (setq en1 (ssname ss (setq i (1- i))))))
       (setq tp (cdr (assoc 0 ent)))
       (cond
  ((member tp '("TEXT" "MTEXT"))
    (entmod (subst
       (cons 1 uu)
       (assoc 1 ent)
       ent
     )
    )
  )
  ((member tp '("TCH_TEXT" "TCH_ELEVATION"
     "TCH_ARROW"
    )
   )
    (vlax-put-property (vlax-ename->vla-object en1) 'text uu)
  )
  ((= tp "TCH_DRAWINGNAME")
    (vlax-put-property (vlax-ename->vla-object en1) 'nametext uu)
  )
  ((= tp "TCH_MULTILEADER")
    (vlax-put-property (vlax-ename->vla-object en1) 'uptext uu)
  )
       )
     )
   )
)
      )
    )
    ((member ty '("CIRCLE" "ARC"))     ; 3、 如果源对象是圆，则循环更新目标圆的直径
      (setq uu (cdr (assoc 40 ent)))
      (princ (strcat "\n选择目标对象:<半径相同>  R = " (rtos uu 2 2)))
      (repeat (setq i (sslength (setq ss (ssget '((0 . "CIRCLE,ARC"))))))
(setq ent (entget (ssname ss (setq i (1- i)))))
(entmod (subst
    (cons 40 uu)
    (assoc 40 ent)
    ent
  )
)
      )
    )
    ((= ty "INSERT")         ; 4、 如果源对象是块，则拷贝源块到目标块的位置，删除目标块
      (princ " \n选择目标对象:<块相同>")
      (setq uu (cdr (assoc 10 ent)))
      (repeat (setq i (sslength (setq ss (ssget '((0 . "INSERT"))))))
(setq ent (entget (ssname ss (setq i (1- i)))))
(command "COPY" name "" uu (cdr (assoc 10 ent)))
      )
      (command "ERASE" ss "")
    )
    ((= ty "LWPOLYLINE")        ; 5、 如果源对象是多义线，则转化目标对象的线宽
      (if (not (setq uu (cdr (assoc 43 ent))))
(setq uu (cdr (assoc 40 ent)))
      )
      (princ (strcat "\n选择目标对象:<线宽相同>   W = " (rtos uu 2 2)))
      (repeat (setq i (sslength (setq ss (ssget '((0 . "LINE,ARC,POLYLINE,LWPOLYLINE,CIRCLE"))))))
(setq name1 (ssname ss (setq i (1- i)))
       tp (cdr (assoc 0 (setq ent (entget name1))))
)
(cond
   ((member tp '("LINE" "ARC"))
     (command "pedit" name1 "w" uu "x")
   )
   ((member tp '("POLYLINE" "LWPOLYLINE"))
     (command "pedit" name1 "w" uu "x")
   )
   ((= tp "CIRCLE")
     (command "donut" (- (* (cdr (assoc 40 ent)) 2) uu) (+ (* (cdr (assoc 40 ent)) 2) uu) (cdr (assoc 10 ent)) "")
     (entdel name1)
   )
)
      )
    )           ; 6、其他的一些情况，则调用特性匹配命令
    ((member ty '("LINE" "HATCH"
       "DIMENSION"
      )
     )
      (princ "\n选择目标对象:<特性匹配>")
      (command "matchprop" name (ssget (list (cons 0 ty))) "")
    )
  )
  (redraw name 4)
  (setvar "nomutt" 0)
  (setvar "PICKBOX" buk)
  (setvar "osmode" snap)
  (command ".UNDO" "E")
  (setq *error* $orr)
  (princ)
)