(defun c:HH()
(alert"
\nＣＬ连续测量距离  ＣＤ线段求和  ＮＢ块统计  ＡＡＭ面积求和  
\nＣＬＬ测量长度  HＭＪ填充面积  ＡＡ０面积  ＪＳ根据喷头数给出管径 
\nＤＮ标管径  ＴＴ合并文字  ＴＴＴ合并单行文本  ＴＬ字按线对齐 ＪＵ文本对齐  
\nＸＴ分解文字  ＤＸ改大小写  ＤＤＴ打断插文字  ＴＣＭ单行字合拼多行          
\nＣＭ沿某方向多重复制  ＡＥ自动删除  ＬＡＥ删除冻结的图层  ＺＥ全图缩放  
\nＺ０Z轴归零 ＡＮ旋转绘图角度  ＬＯＣＫＵＰ加密  ＢＳ多块同时缩放  
\nＬＬＡ列出所有图层  ＣV改图层  Ｗ１２３显示+解锁+解冻全部层 ＡＴ加前后缀 
\nＣＢ改成自选颜色  ＬＬＩ只显示被选对象所在层  ＬＬ将所选对象的层变为当前层  
\nＬＪ解锁图层  ＬＫ快速改对象的层  Ｗ１/Ｗ２/Ｗ３显示/解冻/解锁全部层
\nＱ１/Ｑ２/Ｑ３关闭/冻结/锁定所选对象所在的层  LK快速改对象的层 ＷＲ文本提取 
\n变线条颜色1红/2黄/3绿/4青/5蓝/6紫/7白/8灰/123墨绿/21橙  ＴＣ写带圆圈数字          
\nＣ１～９改颜色 ＢG(灰)ＢＷ(白)ＢＫ(黑)改底色 ＪH加宽命令  ＤＬ改成点划线  
\nＸＸ改成虚线  ＢＰＬ变多义线  ＸH线宽刷  ＣＰ圆变多边形 ＣＲ改多圆半径  
\nＰＮ改线弧圆宽度ＬＰＮ按层改线弧圆宽度 HＢ填充基点  ＺＰ上一个视图
\nＴＲＤ剪切标注边界  ＪＴ箭头  ＡＤＤ文字求和   Ｃ１～４增强拷贝 
\nＬＰ直线变多义线  HＴ描图  ＷＴ风管弯头
"))
;****************************************************WT风管弯头
;输入两条直线(LINE1 LINE2)和一个管径值(R)，获得一个半径为R的并与LINE1、LINE2相切圆的圆心
;圆心在LINE1与LINE2的夹角平分线上,画一直线将圆心与两直线交点连接
(defun LIANG (r line1 line2 / ln1 ln2 P1 P2 P3 P4 PIK1 PIK2 P5 P6 ANGA ANGB ANGC angd l cen_pt
		pt_l1 pt_l2)
  (setq ln1 (entget (car line1)))
  (setq ln2 (entget (car line2)))
  (setq P1 (cdr (assoc 10 LN1))
        P2 (cdr (assoc 11 LN1))
      PIK1 (osnap (cadr line1) "near")
   )
   (setq p3 (cdr (assoc 10 LN2))
      P4 (cdr (assoc 11 LN2))
      PIK2 (osnap (cadr line2) "near")
   )
; 取交点和角度
   (setq P5 (inters P1 P2 P3 P4 nil)
      ANGA (angle P5 PIK1)
      ANGB (angle P5 PIK2)
   )
   (if (> ANGA ANGB)
      (setq ANGC (/ (+ (- (* 2 pi) ANGA) ANGB) 2))
      (setq ANGC (/ (- ANGB ANGA) 2))
   )
  (setq ANGD (+ angc anga))
  (setq p6 (polar p5 angd r))
  (setq p6 (inters pik1 pik2 p5 p6 nil))
  (setq l (/ r (abs (sin angc))))
  (setq cen_pt (polar p5 (angle p5 p6) l ))
  (SETQ PT_L1 (POLAR P5 ANGA (/ r (abs (/ (sin angc) (cos angc))))))
  (SETQ PT_L2 (POLAR P5 ANGB (/ r (abs (/ (sin angc) (cos angc))))))
  (SETQ LIS (LIST CEN_PT PT_L1 PT_L2 P5))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN C:WT(/ r a b las cen_pt pt1 pt2 jd r1 r2 ang1 ang2 ang3 p1 p2 p3 p4 p5 p6)
;输入管径和选取两条管线
  (setq txt1 "\n请输入风管管径：")
  (setq txt2 "\n请选取第一条风管管道中心线：")
  (setq txt3 "\n请选取第二条风管管道中心线：")
  (while
    (eq (setq r(getreal txt1)) nil)
    (setq txt1 "\n风管管径不能为空值，请重新输入风管管径："))
  (while
    (eq (setq a(entsel txt2)) nil)
    (setq txt2 "\n您没有选取风管中心线，请重新选取第一条管道中心线："))
  (while
    (eq (setq b(entsel txt3)) nil)
    (setq txt3 "\n您没有选取风管中心线，请重新选取第二条管道中心线："))
;调用子程序求出四个坐标点的表
  (setq las (liang r a b))
;提取四个点的坐标
  (SETQ cen_Pt (car las))
  (setq pt1 (cadr las))
  (setq pt2 (caddr las))
  (SETQ JD (LAST  LAS))
;定义弯头的半径
  (SETQ R1 (* R 0.5))
  (SETQ R2 (* R 1.5))
;定义弯头几个角点相对于圆心点的角度
  (SETQ ANG1 (ANGLE CEN_PT PT1))
  (SETQ ANG2 (ANGLE CEN_PT PT2))
  (SETQ ANG3 (ANGLE CEN_PT JD))
;定义弯头的各个角点
  (SETQ P1 (POLAR CEN_PT ANG1 R1)
	P2 (POLAR CEN_PT ANG2 R1)
	P3 (POLAR CEN_PT ANG3 R1)
	P4 (POLAR CEN_PT ANG1 R2)
	P5 (POLAR CEN_PT ANG2 R2)
	P6 (POLAR CEN_PT ANG3 R2))
;画出弧线
  (command "layer" "m" "1" "")
  (COMMAND "LINE" P1 P4 "")
  (COMMAND "LINE" P2 P5 "")
  (command "layer" "m" "2" "")
  (COMMAND "ARC" P1 P3 P2)
  (COMMAND "ARC" P4 P6 P5)
;  (COMMAND "ARC" PT1 (POLAR CEN_PT ANG3 R) PT2)
;OK!!!OK!!!!OK!OK!OK!
  (prin1)
)
;****************************************************HT描图
(defun c:ht	(/ p1 p2 lst setucs)
  (defun err-new (msg)

    (command "._redraw")
    (princ msg)
    (ended)
  )
;;;________________________________
  (defun ended (/ tmp)
    (setq *error* err-old)
    (if	setucs
      (command "_.ucs" "p")
    )
  )
  (setq err-old *error*)
  (setq *error* err-new)
  (princ "\nWRITEN BY WKAI , XDCAD.NET , 20040611")
  (setvar "ORTHOMODE" 0)
  (setvar "cmdecho" 1)
  
  (if (setq p1 (getpoint "\n指定第一点:"))
    (if	(setq p2 (getpoint p1 "\n指定下一点:"))      
      (progn
	(setq p1 (trans p1 1 0))
	(setq p2 (trans p2 1 0))
	(entmake (list (cons 0 "LINE") (cons 10 P1) (cons 11 P2)))
	(command "_.ucs" "ob" (entlast))
	(setq setucs t)
	(entdel (entlast))
	(setvar "ORTHOMODE" 1)
	(command "_.pline" (trans p1 0 1) (trans p2 0 1))
	(while (/= (getvar "cmdactive") 0)
	  (command pause)
	)
	(command)
      )
    )
  )
  (ended)
)
;****************************************************ＬＰ直线变多义线
(defun l_to_pl (ssa / ss1 lines ent n)
  (defun l_to_pl:main (/ ent ss1 n)
  (setq ent (ssname lines 0))
  (if (member (cdr (assoc 0 (entget ent)))
	      '("POLYLINE" "LWPOLYLINE")
      )
    (if	(/= 1 (logand (cdr (assoc 70 (entget ent))) 1))
      (command "._pedit" ent "j" lines "" "")
    )
    (command "._pedit" ent "y" "j" lines "" "")
  )
  (setq lines (ssdel ent lines))
  (setq ss1 (ssadd))
  (repeat (setq n (sslength lines))
    (setq ent (ssname lines (setq n (1- n))))
    (if	(entget ent)
      (setq ss1 (ssadd ent ss1))
    )
  )
  (setq lines ss1)
  (while (> (sslength lines) 0) (l_to_pl:main))
)
  (command "._undo" "be")
  (setq ss1 (ssadd))
  (repeat (setq n (sslength ssa))
    (setq ent (ssname ssa (setq n (1- n))))
    (if	(and (entget ent)
	     (member (cdr (assoc 0 (entget ent)))
		     '("LINE" "ARC" "POLYLINE" "LWPOLYLINE")
	     )
	)
      (setq ss1 (ssadd ent ss1))
    )
  )
  (setq lines ss1)
  (l_to_pl:main)
  (command "._undo" "e")
)

(defun c:lp (/ a)
  (setq a (ssget ))
  (l_to_pl a)
;;;(l_to_pl lines)
;;;(while (> (sslength (setq lines (l_to_pl lines))) 0) (progn))
)
;****************************************************ＪＴ箭头
(defun c:jt (/ pt1 pt2 pt3 len oplw)
(setq oplw (getvar "plinewid"))
(and
(setq pt1 (getpoint "尖点: "))
(setq pt2 (getpoint pt1 "脖点: "))
(progn
(setq len (distance pt1 pt2))
(command "pline" pt1 "w" 0 (* 1.8 len) pt2)
(command "w" (* 0.4 len) (* 1.3 len) "a")
(princ "\n结束点: ")
(command pause "")
)
)
(setvar "plinewid" oplw)
)
;****************************************************ＴＣＭ单行字合拼多行
(defun c:tcm (/ dxf ss index ent mtext)
  (defun dxf (tag obj) (cdr (assoc tag obj)))
  (cond
    ((not (setq reftext (car (entsel "Pick reference text"))))
       (princ "Nothing selected"))
    ((not (= (dxf 0 (setq reftext (entget reftext))) "TEXT"))
       (princ "Not a text"))
    ((not (setq ss (ssget)))
       (princ "Nothing selected"))
    (T
     (setq index 0.0
           mtext '((0 . "MTEXT") (100 . "AcDbEntity") (100 . "AcDbMText"))
           mtext (append mtext 
                         (list (assoc 8 reftext) (assoc 10 reftext)
                               (assoc 7 reftext) (assoc 40 reftext)
                               (cons 41 (abs (- (caar (textbox reftext))
                                                (caadr (textbox reftext)))))
                               (cons 3 (strcat (dxf 1 reftext) "\\P"))))
     )
     (entdel (dxf -1 reftext))
     (repeat (sslength ss)
       (cond ((not
                (= (dxf 0 (setq ent (entget (ssname ss index)))) "TEXT")
              )
                (princ "Non-text ignored")
             )
             (T (setq mtext (append mtext 
                               (list (cons 3 (strcat (dxf 1 ent) "\\P")))))
                (entdel (dxf -1 ent))
             )
       )
       (setq index (1+ index))
     )
     (entmake (append mtext '((1 . " "))))
    )
  )
  (princ)
)
;****************************************************ＡＤＤ文字求和
;|(xtcal)= 文本计数求和;---------------------------------------------陌生人.2004.1
;v1.1 2004.1.对mtext的bug修正。消除重复符号;支持-.5写法,排除"写.法" ".." "+-";
功能：对选集中文本进行所有数字计算,支持一个text,mtext中有多个数字字符串,支持字符串中小数,负数:
返回: 有数字，数字相加后写文本，并返回求和数值(非字符串).无有效数字返回nil.
|;
(defun c:add ( / ss filter mspace n e str asclst strs add pt txt txth)
  (defun *error* (msg) (if ss (x_draw ss 4)) (setq *error* oerr))
  (princ "\n选择要计算的文本(支持*TEXT选择集):")
  (setq oerr *error*
	ss (ssget '((0 . "*TEXT")))
	filter "0123456789.-+"
	mspace (vla-get-modelspace(vla-get-activedocument (vlax-get-acad-object)))
	str nil strs nil)
  (if ss
    (repeat (setq n (sslength ss))
      (x_draw ss 3)
      (setq n (1- n)
	    e (ssname ss n)
	    str (vla-get-textstring(vlax-ename->vla-object e))
	    strs (strcat (if strs strs " ") (x_txt2 str) " ")) ;;排除mtext bug.v1.1-2004.1
      )
    )
  (if (and ss (/= "" strs))
    (progn
      (setq add (eval (read (strcat "(+ " strs ")")))) 
      (princ "\n文本数字和为: ")(princ add)
      (if (setq pt (getpoint "\n标注位置<重新计算>:"))
	  (progn
            (setq prec (getint "\n精度(小数位数):")
		  txt (rtos add 2 prec)
		  txth (getdist "\n字高:"))
	    (vla-addtext mspace txt (vlax-3D-point pt) txth)
	    (x_draw ss 4)
	    (princ) add)
	  (progn (if ss (x_draw ss 4))(xtcal))  ;多次<重新计算>可以作为一个简易统计查看器.
      )
    )
    (progn (princ "\n!空选集或文本中无有效数字!\n") nil)
  )
)
;;
(defun x_draw (ss key / n e)
  (if (= 'PICKSET (type ss))
    (repeat (setq n (sslength ss))
      (setq n (1- n)
	    e (ssname ss n))
      (redraw e key)
    )
  )
)
;;
(defun x_txt2 (str / i key1 key2 str1)
(setq i 1 key2 nil)
(repeat (strlen str)
(cond
((= "{\\f" (substr str i 3)) (setq i (+ 3 i) key1 1 key2 1)) 
((and key1 (= "}" (substr str i 1))) (setq key1 nil key2 nil))
((and key1 (= ";" (substr str i 1))) (setq key2 nil))
((not key2)
(setq st (substr str i 1)
str1 (strcat (if (not str1) "" str1) 
(cond
((= "." st)(if (wcmatch (substr str (1+ i) 1) "#") st " "))
((member st '("+" "-")) (if (wcmatch (substr str (1+ i) 1) "#,'.") st " "))
(T (if (wcmatch filter (strcat "*" st "*")) st " ")) 
)
))
)
)
(setq i (1+ i))
)
(setq str str1)
)


;****************************************************ＪＵ文本对齐
(defun newerr (s)
  (if (= s "Function cancelled")
    (progn
      (setq *error* olderr)
      (if oldsnp
	(setvar "osmode" oldsnp)
      )
      (if oldzin
	(setvar "dimzin" oldzin)
      )
    )
  )
  (princ)
)

(defun ju_defun	()			;定义函数
  (defun ju_txt	(j72 j73)              ;Text角点
    (if	(/= j73 0)
      (if (= j73 1)
	(setq j73 (/ h -3))
	(setq j73 (* (abs (- j73 1)) 0.5 h))
      )
    )
    (trans (list
	     (- (+ (* lx (cos alf) j72 0.5) (car pt0)) (* j73 (sin alf)))
	     (+ (* lx (sin alf) j72 0.5) (cadr pt0) (* j73 (cos alf)))
	     (caddr pt0)
	   )
	   0
	   1
    )
  )

  (defun ju_set	(se)			;将文本实体选择集转换为含实体名的表
    (setq l1   (sslength se)
	  i    0
	  set0 nil
    )
    (repeat l1
      (setq e0	 (ssname se i)
	    set0 (cons e0 set0)
	    i	 (1+ i)
      )
    )
  )

  (defun ju_m_pt (sign axis tname)	;文本边缘x、y值，sign——'MIN、'MAX，axis——'CAR、'CADR...
    (setq en  (entget tname)
	  h   (cdr (assoc 40 en))
	  alf (cdr (assoc 50 en))
	  obl (cdr (assoc 51 en))
	  pt0 (cdr (assoc 10 en))
	  lx  (- (caadr (textbox en))
		 (* h (/ (sin obl) (cos obl)))
	      )
    )
    (setq ptlist (list (ju_txt 0 0)
		       (ju_txt 0 3)
		       (ju_txt 2 0)
		       (ju_txt 2 3)
		 )
    )
    (apply sign (mapcar axis ptlist))
  )
  
;;|                                     ;R14时行首去掉一个分号
  (defun ju_sorten (sign axis se)	;实体按x、y排序       R2k使用
    (if	(= sign 'min)
      (setq mc <)
      (setq mc >)
    )
    (setq ss (vl-sort se
		      (function
			(lambda	(e1 e2)
			  (mc (ju_m_pt sign axis e1) (ju_m_pt sign axis e2))
			)
		      )
	     )
    )
  )
;;|;

;|                                      ;R14时行首加一个分号
  (defun ju_sorten (sign axis se)	;实体按x、y排序       R14使用
    (setq ss   nil
	  sexy (mapcar
		 '(lambda (x)
		    (ju_m_pt sign axis x)
		  )
		 se
	       )
    )
    (repeat (length se)
      (setq mc	  (apply 'min sexy)
	    ii	  0
	    i	  -1
	    list1 nil
	    list0 nil
      )
      (while (= ii 0)
	(setq i	  (1+ i)
	      sei (nth i se)
	      xy  (ju_m_pt sign axis sei)
	)
	(if (= mc xy)
	  (setq	ss (cons sei ss)
		ii 1
	  )
	  (setq	list1 (cons (nth i sexy) list1)
		list0 (cons sei list0)
	  )
	)
      )
      (setq sexy (append (reverse list1) (cdr (member mc sexy)))
	    se	 (append (reverse list0) (cdr (member sei se)))
      )
    )
    (if	(= sign 'min)
      (setq ss (reverse ss))
      (setq ss ss)
    )
  )
;;|;

  (defun ju_row	()			;将实体分行
    (ju_sorten 'max 'cadr set0)
    (setq row  1
	  set1 (cons (cons (car ss) row) nil)
    )
    (mapcar
      '(lambda (x)
	 (if (<	(ju_m_pt 'max 'cadr x)
		(ju_m_pt 'min 'cadr (caar set1))
	     )
	   (setq row (1+ row))
	 )
	 (setq set1 (cons (cons x row) set1))
       )
      (cdr ss)
    )
    (setq set1 (reverse set1))
  )

  (defun ju_col	()			;将实体分列
    (ju_sorten 'min 'car set0)
    (setq col  1
	  set2 (cons (cons (car ss) col) nil)
    )
    (mapcar
      '(lambda (x)
	 (setq ym nil)
	 (mapcar
	   '(lambda(y)
	      (if (= (cdr y) col)
		(setq ym (cons (ju_m_pt 'max 'car (car y)) ym))
	      )
	    )
	   set2
	 )
	 (if (>	(ju_m_pt 'min 'car x)
		(apply 'max ym)
	     )
	   (setq col (1+ col))
	 )
	 (setq set2 (cons (cons x col) set2))
       )
      (cdr ss)
    )
    (setq set2 (reverse set2))
  )

  (defun ju_rc ()			;判断行列模式
    (if	(and (/= row 1) (/= col 1))
      (setq rc "阵列")
      (if (/= col 1)
	(setq rc "行")
	(setq rc "列")
      )
    )
  )

  (defun ju_dist (/ dis)		;输入行列间距
    (initget 128)
    (setq disr (getpoint "\n指定单位单元或输入行间距<自动>: "))
    (if	disr
      (if (= (type disr) 'LIST)
	(progn
	  (initget 1)
	  (setq dis (getcorner disr "\n指定对角点: "))
	  (setq	disc (rtos (abs (- (car dis) (car disr))) 2 2)
		disr (rtos (abs (- (cadr dis) (cadr disr))) 2 2)
	  )
	  (if (= (distof disr) 0.0)
	    (setq disr "自动")
	  )
	  (if (= (distof disc) 0.0)
	    (setq disc "自动")
	  )
	)
	(if (= (type disr) 'STR)
	  (if (setq dis (distof disr))
	    (if	(> dis 0.0)
	      (progn
		(initget 6)
		(setq disc (getdist "\n输入列间距<自动>: "))
		(if (= disc nil)
		  (setq disc "自动")
		  (setq disc (rtos disc 2 2))
		)
	      )
	      (progn
		(princ "\n需要正数值或两个二维角点。")
		(ju_dist)
	      )
	    )
	    (progn
	      (princ "\n需要正数值或两个二维角点。")
	      (ju_dist)
	    )
	  )
	  (progn
	    (princ "\n需要正数值或两个二维角点。")
	    (ju_dist)
	  )
	)
      )
      (progn
	(setq disr "自动")
	(initget 6)
	(setq disc (getdist "\n输入列间距<自动>: "))
	(if (= disc nil)
	  (setq disc "自动")
	  (setq disc (rtos disc 2 2))
	)
      )
    )
  )

  (defun ju_vset ()			;计算默认值
    (setq jus  "右"
	  ju72 2
	  pre  "不统一"
	  sta  "不合计"
	  disr "自动"
	  disc "自动"
    )
    (if	(= rc "列")
      (setq ali	 "右"
	    ju72 2
      )
      (setq ali	 "基线"
	    ju73 0
      )
    )
  )

  (defun ju_mktext (str pt10 j72 j73 j50 / sty) ;make_text
    (entmake
      (list
	'(0 . "TEXT")
	(cons 1 str)
	(cons 10 pt10)
	(cons 11 pt10)
	(cons 7 (setq sty (getvar "textstyle")))
	(cons 40 (getvar "textsize"))
	(assoc 41 (tblsearch "style" sty))
	(cons 50 j50)
	(cons 51 (cdr (assoc 50 (tblsearch "style" sty))))
	'(71 . 0)
	(cons 72 j72)
	(cons 73 j73)
      )
    )
  )


  (defun ju_out	()
    (if	(= rc "阵列")
      (ju_array)
      (ju_column_row)
    )
  )

  (defun ju_column_row ()		;行、列模式运行结果
    (setq ptx (car pt1)
	  pty (cadr pt1)
	  num nil
    )
    (mapcar
      '(lambda (x)
	 (setq e0 (entget x))
;|       (if (= rc "列")
 	   (setq ju73 (cdr (assoc 73 e0)))
	   (setq ju72 (cdr (assoc 72 e0)))
         )
|;
	 (if (not ju72)
	   (setq ju72 (cdr (assoc 72 (entget (caar set2)))))
	 )
	 (if (not ju73)
	   (setq ju73 (cdr (assoc 73 (entget (caar set1)))))
	 )
	 (if (= ju72 4)
	   (setq ju72 1)
	   (if (or (= ju72 5) (= ju72 6))
	     (setq ju72 2)
	   )
	 )
         (setq en  (entget x)
	       h   (cdr (assoc 40 en))
	       alf (cdr (assoc 50 en))
	       obl (cdr (assoc 51 en))
	       pt0 (cdr (assoc 10 en))
	       lx  (- (caadr (textbox en))
		      (* h (/ (sin obl) (cos obl)))
		   )
	       pti (ju_txt ju72 ju73)
	 )
	 (if (= rc "列")
	   (setq pti (trans (list ptx (cadr pti) (caddr pti)) 1 0))
	   (setq pti (trans (list (car pti) pty (caddr pti)) 1 0))
	 )
	 (setq e0 (subst (cons 10 pti) (assoc 10 e0) e0)
	       e0 (subst (cons 11 pti) (assoc 11 e0) e0)
	       e0 (subst (cons 72 ju72) (assoc 72 e0) e0)
	       e0 (subst (cons 73 ju73) (assoc 73 e0) e0)
	 )
	 (if (/= pre "不统一")
	   (progn
	     (setq etx (cdr (assoc 1 e0)))
	     (if (distof etx)
	       (setq etx (rtos (distof etx) 2 (atoi pre))
		     num (cons (atof etx) num)
		     e0	 (subst (cons 1 etx) (assoc 1 e0) e0)
	       )
	     )
	   )
	   (progn
	     (setq etx (cdr (assoc 1 e0)))
	     (if (distof etx)
	       (setq num (cons (atof etx) num))
	     )
	   )
	 )
	 (entmod e0)
       )
      set0
    )
    (if	num
      (setq num (apply '+ num))
    )
    (if	(and num (/= sta "不合计"))
      (progn
	(initget 1)
	(setq pt1 (getpoint "\n合计数字位置: "))
	(if (= rc "列")
	  (setq	alf (cdr (assoc 50 (entget (caar set1))))
		pt1 (trans (list ptx (cadr pt1) (caddr pti)) 1 0)
	  )
	  (setq	alf (cdr (assoc 50 (entget (caar set2))))
		pt1 (trans (list (car pt1) pty (caddr pti)) 1 0)
	  )
	)
	(if (= pre "不统一")
	  (progn
	    (setvar "dimzin" 8)
	    (ju_mktext (rtos num 2) pt1 ju72 ju73 alf)
	    (setvar "dimzin" oldzin)
	  )
	  (ju_mktext (rtos num 2 (atoi pre)) pt1 ju72 ju73 alf)
	)
      )
      (if (not num)
	(princ "\n无数值。")
      )
    )
  )

  (defun ju_in ()			;运行前处理
    (cond
      ((or (= rc "列") (= rc "行"))
       (princ (strcat "\n当前为\""   rc		    "\"模式; 对齐方式:"
		      ali	     "; 小数位数:"  pre
		      "; 文本合计:"  sta	    "。"
		     )
       )
       (initget 1 "Mode Precision Align Stat")
       (setq pt1 (getpoint "\n模式M/对齐方式A/小数位数P/合计S/<对齐基准线>: "
		 )
       )
      )
      ((and (= rc "阵列") (= disr "自动") (= disc "自动"))
       (princ (strcat "\n当前为\"阵列\"模式; 行对齐方式:"   ali
		      "; 列对齐方式:"	 jus		    "; 小数位数:"
		      pre		 ";\n行距:"	    disr
		      "; 列距:"		 disc		    "; 文本合计:"
		      sta		 "。"
		     )
       )
       (initget "Mode Precision Align Stat Distance Justify")
       (setq pt1
	      (getkword
		"\n模式M/行对齐方式A/列对齐方式J/小数位数P/行列间距D/合计S/<回车确认>: "
	      )
       )
      )
      (t
       (princ (strcat "\n当前为\"阵列\"模式; 行对齐方式:"   ali
		      "; 列对齐方式:"	 jus		    "; 小数位数:"
		      pre		 ";\n行距:"	    disr
		      "; 列距:"		 disc		    "; 文本合计:"
		      sta		 "。"
		     )
       )
       (initget 1 "Mode Precision Align Stat Distance Justify")
       (setq pt1
	      (getpoint
		(strcat
		  "\n模式M/行对齐方式A/列对齐方式J/小数位数P/行列间距D/合计S/<任意基准点>: "
		)
	      )
       )
      )
    )
    (cond
      ((= pt1 "Mode")
       (initget "Row Column Array")
       (setq rc (getkword "\n模式: 行R/列C/<阵列A>: "))
       (if (= rc "Row")
	 (setq rc "行")
	 (if (= rc "Column")
	   (setq rc "列")
	   (setq rc "阵列")
	 )
       )
       (ju_in)
      )
      ((= pt1 "Align")
       (if (= rc "列")
	 (progn
	   (initget "Left Center Right")
	   (setq ali (getkword "\n对齐方式: 左L/中C/<右R>: "))
	   (if (= ali "Left")
	     (setq ju72	0
		   ali	"左"
	     )
	     (if (= ali "Center")
	       (setq ju72 1
		     ali  "中"
	       )
	       (setq ju72 2
		     ali  "右"
	       )
	     )
	   )
	 )
	 (progn
	   (initget "Top Middle Base")
	   (setq ju73 (getkword "\n对齐方式: 上T/中M/<基线B>: "))
	   (if (= ju73 "Top")
	     (setq ju73	3
		   ali	"上"
	     )
	     (if (= ju73 "Middle")
	       (setq ju73 2
		     ali  "中"
	       )
	       (setq ju73 0
		     ali  "基线"
	       )
	     )
	   )
	 )
       )
       (ju_in)
      )
      ((= pt1 "Justify")
       (initget "Left Center Right")
       (setq ju72 (getkword "\n对齐方式: 左L/中C/<右R>："))
       (if (= ju72 "Left")
	 (setq ju72 0
	       jus  "左"
	 )
	 (if (= ju72 "Center")
	   (setq ju72 1
		 jus  "中"
	   )
	   (setq ju72 2
		 jus  "右"
	   )
	 )
       )
       (ju_in)
      )
      ((= pt1 "Precision")
       (initget 4)
       (setq pre (getint "\n小数位数<回车不统一>: "))
       (if (= pre nil)
	 (setq pre "不统一")
	 (setq pre (itoa pre))
       )
       (ju_in)
      )
      ((= pt1 "Stat")
       (if (= rc "阵列")
	 (progn
	   (initget "Row Column All")
	   (setq
	     sta (getkword "\n对齐方式: 合计行R/合计列C/全部合计A/<不合计>: "
		 )
	   )
	   (if (= sta "Row")
	     (setq sta "合计行")
	     (if (= sta "Column")
	       (setq sta "合计列")
	       (if (= sta "All")
		 (setq sta "全部合计")
		 (setq sta "不合计")
	       )
	     )
	   )
	 )
;|       (progn
	   (initget "Total")
	   (setq sta (getkword "\n合计T/<不合计>: "))
	   (if (= sta "Total")
	     (setq sta "合计")
	     (setq sta "不合计")
	   )
	 )
|;
	 (if (= sta "合计")
	   (setq sta "不合计")
	   (setq sta "合计")
	 )
       )
       (ju_in)
      )
      ((= pt1 "Distance")
       (ju_dist)
       (ju_in)
      )
      (t nil)
    )
  )

  (defun ju_stas (ss xy n ;|ju72 ju73 sta pre|;) ;构造输出点及合计结果表
    (setq ss0 nil
	  i   0
    )
    (repeat n
      (setq i	 (+ 1 i)
	    num	 nil
	    ptxy nil
      )
      (mapcar
	'(lambda (x)
	   (if (= (cdr x) i)
             (setq en	(entget (car x))
		   h	(cdr (assoc 40 en))
		   alf	(cdr (assoc 50 en))
		   obl	(cdr (assoc 51 en))
		   pt0	(cdr (assoc 10 en))
		   lx	(- (caadr (textbox en))
			   (* h (/ (sin obl) (cos obl)))
			)
		   ptxy	(cons (nth xy (ju_txt ju72 ju73)) ptxy)
	     )
	   )
	 )
	ss
      )
      (cond
	((and (= ju72 0) (= xy 0))
	 (setq ptxy (apply 'MIN ptxy))
	)
	((and (= ju72 2) (= xy 0))
	 (setq ptxy (apply 'MAX ptxy))
	)
	(t (setq ptxy (/ (apply '+ ptxy) (length ptxy))))
      )
      (mapcar
	'(lambda (x)
	   (if (= (cdr x) i)
	     (progn
	       (setq en	 (entget (car x))
		     etx (cdr (assoc 1 en))
	       )
	       (if (and (/= sta "不合计") (distof etx))
		 (if (/= pre "不统一")
		   (setq etx (rtos (distof etx) 2 (atoi pre))
			 num (cons (atof etx) num)
		   )
		   (setq num (cons (atof etx) num))
		 )
	       )
	     )
	   )
	 )
	ss
      )
      (if num
	(setq num (apply '+ num))
      )
      (setq ss0 (cons (list ptxy num) ss0))
    )
    (reverse ss0)
  )

  (defun ju_array ()			;阵列模式输出结果
    (setq b1 (ju_stas set1 1 row)
	  b2 (ju_stas set2 0 col)
    )
    (if	pt1
      (progn
	(if (/= disr "自动")
	  (setq	b1 (mapcar
		     '(lambda (x)
			(setq tmp (- (cadr pt1) (car x)))
			(if (< (/ tmp (distof disr)) 0)
			  (setq tmp (fix (- (/ tmp (distof disr)) 0.500001)))
			  (setq tmp (fix (+ (/ tmp (distof disr)) 0.499999)))
			)
			(setq tmp (- (cadr pt1) (* tmp (distof disr))))
			(list tmp (cadr x))
		      )
		     b1
		   )
	  )
	)
	(if (/= disc "自动")
	  (setq	b2 (mapcar
		     '(lambda (x)
			(setq tmp (- (car pt1) (car x)))
			(if (< (/ tmp (distof disc)) 0)
			  (setq tmp (fix (- (/ tmp (distof disc)) 0.500001)))
			  (setq tmp (fix (+ (/ tmp (distof disc)) 0.499999)))
			)
			(setq tmp (- (car pt1) (* tmp (distof disc))))
			(list tmp (cadr x))
		      )
		     b2
		   )
	  )
	)
      )
    )
    (mapcar
      '(lambda (x)
	 (setq e0  (entget x)
	       ix  (- (cdr (assoc x set1)) 1)
	       iy  (- (cdr (assoc x set2)) 1)
	       pti (trans (list	(car (nth iy b2))
				(car (nth ix b1))
				(nth 3 (assoc 10 e0))
			  )
			  1
			  0
		   )
	       e0  (subst (cons 10 pti) (assoc 10 e0) e0)
	       e0  (subst (cons 11 pti) (assoc 11 e0) e0)
	       e0  (subst (cons 72 ju72) (assoc 72 e0) e0)
	       e0  (subst (cons 73 ju73) (assoc 73 e0) e0)
	 )
	 (if (/= pre "不统一")
	   (progn
	     (setq etx (cdr (assoc 1 e0)))
	     (if (distof etx)
	       (setq etx (rtos (distof etx) 2 (atoi pre))
		     e0	 (subst (cons 1 etx) (assoc 1 e0) e0)
	       )
	     )
	   )
	 )
	 (entmod e0)
       )
      set0
    )
    (if	(= sta "合计行")
      (ju_sta b1 0 "\n行合计数字位置: ")
      (if (= sta "合计列")
	(ju_sta b2 1 "\n列合计数字位置: ")
	(if (= sta "全部合计")
	  (progn
	    (ju_sta b1 0 "\n行合计数字位置: ")
	    (ju_sta b2 1 "\n列合计数字位置: ")
	  )
	)
      )
    )
  )

  (defun ju_sta	(ss xy msg)		;合计数值输出
    (setq alf (cdr (assoc 50 (entget (caar set1)))))
    (initget 1)
    (setq ptx (getpoint msg))
    (mapcar
      '(lambda (x)
	 (if (= xy 0)
	   (setq pt1 (trans (list (car ptx) (car x) (caddr ptx)) 1 0))
	   (setq pt1 (trans (list (car x) (cadr ptx) (caddr ptx)) 1 0))
	 )
	 (if (setq num (cadr x))
	   (if (= pre "不统一")
	     (progn
	       (setvar "dimzin" 8)
	       (ju_mktext (rtos num 2) pt1 ju72 ju73 alf)
	       (setvar "dimzin" oldzin)
	     )
	     (ju_mktext (rtos num 2 (atoi pre)) pt1 ju72 ju73 alf)
	   )
	   (princ "\n无数值。")
	 )
       )
      ss
    )
  )
)

(defun c:ju (/ alf     ali     axis    b1      b2      col     dis     disc
	       disr    e0      e00     ei      en      ent     etx     h
	       i       ii      ix      iy      j72     j73     ju72    ju73
	       jus     l1      list0   list1   lx      mc      num     obl
	       olderr  oldsnp  oldzin  pre     pt_list pt0     pt1     pti
	       ptx     ptxy    pty     rc      row     s       se      se3
	       se4     set0    set1    set2    si      sign    ss      ss0
	       sta     tname   tmp     x       x0      xi      xy      ym
	       ju_array	       ju_col  ju_column_row   ju_dist ju_in   ju_m_pt
	       ju_mktext       ju_sorten       ju_out  ju_rc   ju_row  ju_set
	       ju_sta  ju_stas ju_txt  ju_vset
	      )
  (command "color" (getvar "cecolor"))
  (setq olderr *error*)
  (setq oldsnp (getvar "osmode"))
  (setq oldzin (getvar "dimzin"))
  (setvar "osmode" 0)
  (setvar "dimzin" 0)
  (setq *error* newerr)
  (princ "\n选取文本: ")
  (setq se1 (ssget '((0 . "TEXT"))))
  (if se1
    (progn
      (princ "\n请稍候...\n")
      (ju_defun)
      (ju_set se1)
      (ju_row)
      (princ "...\n")
      (ju_col)
      (ju_rc)
      (ju_vset)
      (ju_in)
      (ju_out)
    )
    (princ "\n空选择集。")
  )
  (if oldsnp
    (setvar "osmode" oldsnp)
  )
  (if oldzin
    (setvar "dimzin" oldzin)
  )
  (setq *error* olderr)
  (princ)
)



;****************************************************ＴＲＤ剪切标注边界
(defun c:trd ( / pt1 pt2 ss i ent entl p10 p13 p14 ptt np14 np13)
(princ "\ndmtr2=====dim trim 剪齐dim边界线--v2 -----------lxx.2002")
(command "_.undo" "be" ^c )
;;;
(defun *error* (msg) (print msg)(command  "_.undo" "e" ^c)(setq *error* nil))
;;;
(setq pt1 (getpoint "\n定义修剪界线 (只处理相交的dim),起点:")
      pt2 (getpoint pt1 "\n终点:")
      ss (ssget "f" (list pt1 pt2) '((0 . "DIMENSION")) )
      pt1 (trans pt1 1 0)
      pt2 (trans pt2 1 0)
      i 0
)
(repeat (sslength ss)
 (setq ent (ssname ss i)
      entl (entget ent)
      p10 (cdr (assoc 10 entl))
      p13 (cdr (assoc 13 entl))
      p14 (cdr (assoc 14 entl))
    ;;ptt (cdr (assoc 11 entl))
      i (1+ i)
      pt1 (polar pt1 (angle pt1 pt2) (/ (distance pt1 pt2) 2) ) 
      pt2 (polar pt1 (+ (/ PI 2) (angle p10 p14)) 100)
      np14 (inters pt1 pt2 p14 p10 nil)
 )
 (if (not(member '(100 . "AcDbRotatedDimension") entl)) 
     (setq np13 (polar np14 (angle p14 p13) (distance p14 p13)))
     (setq np13 (inters pt1 pt2 p13 (polar p13 (angle p14 p10) 100) nil))  
 )
 (setq entl (subst (cons 13 np13) (assoc 13 entl) entl)
       entl (subst (cons 14 np14) (assoc 14 entl) entl)
 )
  (entmod entl)
);end repeat
(command "_.undo" "e" ^c)
(setq *error* nil)
(princ)
)

;****************************************************WR文本提取
(defun wr_defun	()
  (defun wr_set	(se)			;将文本实体选择集转换为含实体名的表
    (setq l1   (sslength se)
	  i    0
	  set0 nil
    )
    (repeat l1
      (setq e0	 (ssname se i)
	    set0 (cons e0 set0)
	    i	 (1+ i)
      )
    )
  )

  (defun wr_sorten (se axis)		;实体按x、y排序
    (setq ss   nil
	  sexy (mapcar
		 '(lambda (x)
		    (axis (trans (cdr (assoc 10 (entget x))) 0 1))
		  )
		 se
	       )
    )
    (repeat (length se)
      (setq mc	  (apply 'max sexy) 
	    ii	  0
	    i	  -1
	    list1 nil
	    list0 nil
      )
      (while (= ii 0)
	(setq i	  (1+ i)
	      sei (nth i se)
	      xy  (axis (trans (cdr (assoc 10 (entget sei))) 0 1))
	)
	(if (= mc xy)
	  (setq	ss (cons sei ss)
		ii 1
	  )
	  (setq	list1 (cons (nth i sexy) list1)
		list0 (cons sei list0)
	  )
	)
      )
      (setq sexy (append (reverse list1) (cdr (member mc sexy)))
	    se	 (append (reverse list0) (cdr (member sei se)))
      )
    )
    (setq ss (reverse ss))
  )

  (defun wr_row	()			;将实体分行
    (wr_sorten set0 cadr)
    (setq row  1
	  set1 (cons (cons (car ss) row) nil)
    )
    (mapcar
      '(lambda (x)
	 (if (<	(+ (cadr (trans (cdr (assoc 10 (entget x))) 0 1))
		   (cdr (assoc 40 (entget x)))
		)
		(cadr (trans (cdr (assoc 10 (entget (caar set1)))) 0 1))
	     )
	   (setq row (1+ row))
	 )
	 (setq set1 (cons (cons x row) set1))
       )
      (cdr ss)
    )
    (setq set1 (reverse set1))
  )

  (defun wr_col	()			;将实体分列
    (setq ss (reverse (wr_sorten set0 car)))
    (setq col  1
	  set2 (cons (cons (car ss) col) nil)
    )
    (mapcar
      '(lambda (x)
	 (setq ym nil)
	 (mapcar
	   '(lambda (y)
	      (if (= (cdr y) col)
		(setq tmp (entget (car y))
		      ym  (cons
			    (+ (car (trans (cdr (assoc 10 tmp)) 0 1))
			       (caadr (textbox tmp))
			    )
			    ym
			  )
		)
	      )
	    )
	   set2
	 )
	 (if (>	(car (trans (cdr (assoc 10 (entget x))) 0 1))
		(apply 'max ym)
	     )
	   (setq col (1+ col))
	 )
	 (setq set2 (cons (cons x col) set2))
       )
      (cdr ss)
    )
    (setq set2 (reverse set2))
  )

  (defun wr_main ()
    (while (not fn)
      (setq fn (getfiled "文本文件" "CADText" "txt" 13))
    )
    (if	(findfile fn)
      (progn
	(initget "Add Write")
	(setq fs (getkword "\n覆盖W/<附加A>: "))
	(if (= fs "Write")
	  (setq fs (open fn "w"))
	  (setq fs (open fn "a"))
	)
      )
      (setq fs (open fn "a"))
    )
    (initget "Normal Tab")
    (setq wmode (getkword "\n制表模式T/<普通文本N>: "))
    (if (= wmode nil)
      (setq wmode "Normal")
    )
    (wr_set se1)
    (wr_row)
    (wr_col)
    (setq ij 0)
    (repeat row
      (setq l1 nil
	    ij (1+ ij)
      )
      (mapcar
	'(lambda (x)
	   (if (= (cdr x) ij)
	     (setq l1 (cons (car x) l1))
	   )
	 )
	set1
      )
      (setq l1 (reverse (wr_sorten l1 car))
	    j1 1
      )
      (mapcar
	'(lambda (x)
	   (setq j2 (cdr (assoc x set2)))
	   (if (= wmode "Tab")
	     (progn
	       (repeat (- j2 j1)
		 (princ "\t" fs)
	       )
	       (princ (cdr (assoc 1 (entget x))) fs)
	     )
	     (progn
	       (repeat (- j2 j1 1)
		 (princ " " fs)
	       )
	       (princ (cdr (assoc 1 (entget x))) fs)
	     )
	   )
	   (setq j1 j2)
	 )
	l1
      )
      (princ "\n" fs)
    )
    (close fs)
  )
)

(defun c:wr (/	     axis    col     e0	     fn	     fs	     i	     ii
	     ij	     j1	     j2	     l1	     list0   list1   mc	     olderr
	     oldsnp  oldzin  row     se	     set0    set1    set2    ss
	     text_name	     tmp     wmode   x	     y	     ym	     wr_col
	     wr_main wr_m_pt wr_row  wr_sorten	     wr_set  na
	    )
  (command "color" (getvar "cecolor"))
  (princ "\n选取文本: ")
  (setq se1 (ssget '((0 . "TEXT"))))
  (if se1
    (progn
      (wr_defun)
      (wr_main)
    )
    (princ "\n空选择集。")
  )
  (princ)
)
;****************************************************AT加前后缀
(DEFUN C:at ()
 (setq qh (getint "\n1--加前缀,2--加后缀,<1>"))
 (if (= qh nil)(setq qh 1))
 (princ "\nselect object:")
 (setq s (ssget))
 (setq str (getstring "\n输入要加的字:"))
 (setq n (sslength s))
 (setq k 0 )
 (while (< k n) 
      (setq name (ssname s k))
      (setq a (entget name))
      (setq t1 (assoc '0 a))
      (setq t1 (cdr t1))
      (if (= t1 "TEXT") (PROGN
        (setq h (assoc '1 a))
	(setq hh (cdr h))
        (if (= qh 1)(setq  str1 (strcat str hh)))
	(if (/= qh 1)(setq str1 (strcat hh str)))
	(setq h1 (cons 1 str1))
        ;(if (= str "") (setq h1 h))
        (setq a (subst h1 h a))
        (entmod a)
        ))
      (setq k (+ k 1))
 )
)
;****************************************************ＴＣ写带圆圈数字
(defun c:tc ()
(setq stanum (getint "\n Input the begin integer<1>: "))
(if (= stanum nil) (setq stanum 1) ) 
(setq num1 (getint "\Input the times<10>: "))
(if (= num1 nil) (setq num1 10) ) 
(setq texthigh (getreal "\Input the high of the text<2.5>: "))
(if (= texthigh nil) (setq texthigh 2.5) )
(setq r1 (getreal "\nInput the radius <0.85>: "))
(if (= r1 nil) (setq r1 0.85) ) 
(command "style" "standard" "romans.shx,ehzdx.shx" "0" "0.7" "0" "n" "n" "n")
(repeat num1
(setq pt (getpoint "\nSpecify the point: "))
(command "text" "j" "m" pt texthigh "0" stanum)
(command "circle" pt (* texthigh r1))
(setq stanum (1+ stanum))
)
(princ "\n Welcome to use the program again!")
(princ "\n Copyright by HongQuan.\n")
(princ "\n TianJin Urban Construction Design Courtyard!\n")
(princ)
) 
;****************************************************ZP上一个视图
(defun c:zp (/ oldcmdecho vplist curcvport nr vpss ms en x)
  (setq oldcmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (setq vplist (mapcar 'car (vports)))
  (setq curcvport (getvar "cvport"))
  (if (= (getvar "tilemode") 0)
    (progn
      (if (= (setq ms (getvar "cvport")) 1)
        (command "._mspace")
      )
      (setq vpss (ssget "_x"
                        (list '(-4 . "<AND")
                              '(0 . "VIEWPORT")
                              (cons 410 (getvar "ctab"))
                              '(-4 . "<NOT")
                              '(69 . 1)
                              '(-4 . "NOT>")
                              '(-4 . "AND>")
                        )
                 )
      )
      (setq nr 0)
      (if vpss                          ; in case there are no viewports
        (repeat (sslength vpss)
          (setq en (entget (ssname vpss nr)))
          (if (and (= 0 (logand 1 (cdr (assoc 90 en))))
                                        ; not perspective
                   (< 0 (cdr (assoc 68 en))) ; on and active
                   (/= 16384 (logand 16384 (cdr (assoc 90 en))))
                                        ; not locked
              )
            (progn
              (setvar "cvport" (cdr (assoc 69 en)))
              (command "._zoom" "_p")
            )
          )
          (setq nr (+ 1 nr))
        )
      )
      (if (= ms 1) (command "._pspace"))
    )
    (foreach x vplist
      (setvar "cvport" x)
      (command "._zoom" "_p")
    )
  )
  (setvar "cvport" curcvport)
  (setvar "cmdecho" oldcmdecho)
  (princ)
)
;****************************************************ZE全图缩放
(defun c:ze (/ oldcmdecho vplist curcvport nr vpss ms en x)
  (setq oldcmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (setq vplist (mapcar 'car (vports)))
  (setq curcvport (getvar "cvport"))
  (if (= (getvar "tilemode") 0)
    (progn
      (if (= (setq ms (getvar "cvport")) 1)
        (command "._mspace")
      )
      (setq vpss (ssget "_x"
                        (list '(-4 . "<AND")
                              '(0 . "VIEWPORT")
                              (cons 410 (getvar "ctab"))
                              '(-4 . "<NOT")
                              '(69 . 1)
                              '(-4 . "NOT>")
                              '(-4 . "AND>")
                        )
                 )
      )
      (setq nr 0)
      (if vpss                          ; in case there are no viewports
        (repeat (sslength vpss)
          (setq en (entget (ssname vpss nr)))
          (if (and (= 0 (logand 1 (cdr (assoc 90 en))))
                                        ; not perspective
                   (< 0 (cdr (assoc 68 en))) ; on and active
                   (/= 16384 (logand 16384 (cdr (assoc 90 en))))
                                        ; not locked
              )
            (progn
              (setvar "cvport" (cdr (assoc 69 en)))
              (command "._zoom" "_e")
            )
          )
          (setq nr (+ 1 nr))
        )
      )
      (if (= ms 1) (command "._pspace"))
    )
    (foreach x vplist
      (setvar "cvport" x)
      (command "._zoom" "_e")
    )
  )
  (setvar "cvport" curcvport)
  (setvar "cmdecho" oldcmdecho)
  (princ)
)
;****************************************************XH线宽刷
(defun sz_s()
  (setq wd1 "\n当前匹配属性：" wd0 "不匹配属性：")
  (if (= sz_color 1) (setq wd1 (strcat wd1 " 颜色")) (setq wd0 (strcat wd0 " 颜色")))
  (if (= sz_layer 1) (setq wd1 (strcat wd1 " 图层")) (setq wd0 (strcat wd0 " 图层")))
  (if (= sz_ltype 1) (setq wd1 (strcat wd1 " 线形")) (setq wd0 (strcat wd0 " 线形")))
  (if (= sz_ltsca 1) (setq wd1 (strcat wd1 " 线形比例")) (setq wd0 (strcat wd0 " 线形比例")))
  (if (= sz_asywh 1) (setq wd1 (strcat wd1 " 变宽线形")) (setq wd0 (strcat wd0 " 变宽线形")))
  (if (= wd1 "\n当前匹配属性：") (setq wd1 "\n当前匹配属性：宽度")(setq wd1 (strcat wd1 "。"))) 
  (if (= wd0 "不匹配属性：") (setq wd0 "") (setq wd0 (strcat wd0 "。")))
  (princ (strcat "\n***" wd1 wd0))
  (initget "1 2 3 4 5")
  (setq setk(getkword "\n改变：颜色1/图层2/线形3/线形比例4/变宽线形5/<确认>："))
  (cond
    ((= setk "1") (if (= sz_color 1) (setq sz_color 0) (setq sz_color 1)) (sz_s))
    ((= setk "2") (if (= sz_layer 1) (setq sz_layer 0) (setq sz_layer 1)) (sz_s))
    ((= setk "3") (if (= sz_ltype 1) (setq sz_ltype 0) (setq sz_ltype 1)) (sz_s))
    ((= setk "4") (if (= sz_ltsca 1) (setq sz_ltsca 0) (setq sz_ltsca 1)) (sz_s))
    ((= setk "5") (if (= sz_asywh 1) (setq sz_asywh 0) (setq sz_asywh 1)) (sz_s))
    (t nil)
  )
)

(defun sz_b(num enum um / j b0 bj)
  (if (and (assoc num ei) enum)
    (setq ei (subst enum (assoc num ei) ei))
    (if enum
      (setq ei (reverse (cons enum (reverse ei))))
      (if (assoc num ei)
        (progn
          (setq j -1
                b0 nil
          )
          (repeat (length ei)
            (setq j (1+ j)
                  bj(nth j ei)
            )
            (if (/= bj (assoc num ei))
	      (setq b0 (cons bj b0))
	      (if um (setq b0 (cons (cons num um) b0)))
            )
          )
	  (setq ei (reverse b0))
        )
      )
    )
  )
)

(defun sz_main()
  (setq len (sslength ss1)
	i   -1
	ent0(entget (car ent1))
	e62 (assoc 62 ent0)
	e8  (assoc 8 ent0)
	e6  (assoc 6 ent0)
	e48 (assoc 48 ent0)
	e43 (assoc 43 ent0)
	e90 (cdr (assoc 90 ent0))
  )
  (if (>= e90 3)
    (progn
      (repeat 3 (setq ent0 (subst (cons 11 (cdr (assoc 10 ent0))) (assoc 10 ent0) ent0)))
      (setq ent0 (reverse ent0)
            ent0 (reverse (cons (car ent0) (cdr (member (assoc 11 ent0) ent0))))
	    ent0 (subst '(90 . 2) (assoc 90 ent0) ent0)
      )
      (repeat 2 (setq ent0 (subst (cons 10 (cdr (assoc 11 ent0))) (assoc 11 ent0) ent0)))
    )
  )
  (repeat len
    (setq i (1+ i)
	  eni (ssname ss1 i)
	  ei  (entget eni)
    )
    (cond
      ((= (cdr (assoc 0 ei)) "LWPOLYLINE")
       (sz_lwp)
      )
      ((= (cdr (assoc 0 ei)) "LINE")
       (sz_line)
      )
      ((= (cdr (assoc 0 ei)) "ARC")
       (sz_arc)
      )
      ((= (cdr (assoc 0 ei)) "CIRCLE")
       (sz_circle)
      )
      (t nil)
    )
  )
)

(defun sz_lwp()
  (if (= sz_color 1) (sz_b 62 e62 256))
  (if (= sz_layer 1) (setq ei (subst e8 (assoc 8 ei) ei)))
  (if (= sz_ltype 1) (sz_b 6 e6 "BYLAYER"))
  (if (= sz_ltsca 1) (sz_b 48 e48 1.0))
  (if (assoc 43 ei)
    (setq ei (subst e43 (assoc 43 ei) ei))
    (if (= sz_asywh 1)
      (setq ei (reverse (cons e43 (reverse ei))))
    )
  )
  (entmod ei)
  (entupd eni)
)

(defun sz_line(/ ei62 ei8 ei6 ei48 ei10 ei11)
  (setq ei62 (assoc 62 ei)
        ei8  (assoc 8 ei)
        ei6  (assoc 6 ei)
        ei48 (assoc 48 ei)
        ei10 (assoc 10 ei)
        ei11 (assoc 11 ei)
  )
  (setq ei (subst (assoc 5 ei) (assoc 5 ent0) ent0)
	ei (subst (cons 70 0) (assoc 70 ei) ei)
  )
  (if (= sz_color 0) (sz_b 62 ei62 256))
  (if (= sz_layer 0) (setq ei (subst ei8 (assoc 8 ei) ei)))
  (if (= sz_ltype 0) (sz_b 6 ei6 "BYLAYER"))
  (if (= sz_ltsca 0) (sz_b 48 ei48 1.0))
  (setq ei (subst (cons 12 (cdr ei10)) (assoc 10 ei) ei)
	ei (subst (cons 10 (cdr ei11)) (assoc 10 ei) ei)
	ei (subst ei10 (assoc 12 ei) ei)
  )
  (repeat 2 (setq ei (subst (cons 42 0.0) (assoc 42 ei) ei)))
  (entdel eni)
  (entmake ei)
)

(defun sz_circle(/ ei62 ei8 ei6 ei48 ei10 ei40 pt1 pt2)
  (setq ei62 (assoc 62 ei)
        ei8  (assoc 8 ei)
        ei6  (assoc 6 ei)
        ei48 (assoc 48 ei)
        ei10 (cdr (assoc 10 ei))
        ei40 (cdr (assoc 40 ei))
	pt1  (cons (- (car ei10) ei40) (cdr ei10))
	pt2  (cons (+ (car ei10) ei40) (cdr ei10))
  )
  (setq ei (subst (assoc 5 ei) (assoc 5 ent0) ent0)
	ei (subst (cons 70 1) (assoc 70 ei) ei)
  )
  (if (= sz_color 0) (sz_b 62 ei62 256))
  (if (= sz_layer 0) (setq ei (subst ei8 (assoc 8 ei) ei)))
  (if (= sz_ltype 0) (sz_b 6 ei6 "BYLAYER"))
  (if (= sz_ltsca 0) (sz_b 48 ei48 1.0))
  (setq ei (subst (cons 12 pt1) (assoc 10 ei) ei)
	ei (subst (cons 10 pt2) (assoc 10 ei) ei)
	ei (subst (cons 10 pt1) (assoc 12 ei) ei)
  )
  (repeat 2 (setq ei (subst (cons 42 1.0) (assoc 42 ei) ei)))
  (entdel eni)
  (entmake ei)
)

(defun sz_arc(/ ei62 ei8 ei6 ei10 ei48 ei40 ei50 ei51 pt1 pt2 lpt alf e42)
  (setq ei62 (assoc 62 ei)
        ei8  (assoc 8 ei)
        ei6  (assoc 6 ei)
        ei48 (assoc 48 ei)
        ei10 (cdr (assoc 10 ei))
        ei40 (cdr (assoc 40 ei))
        ei50 (cdr (assoc 50 ei))
        ei51 (cdr (assoc 51 ei))
	pt1  (list (+ (car ei10) (* ei40 (cos ei50)))
		   (+ (cadr ei10) (* ei40 (sin ei50))) (caddr ei10))
	pt2  (list (+ (car ei10) (* ei40 (cos ei51)))
		   (+ (cadr ei10) (* ei40 (sin ei51))) (caddr ei10))
	lpt  (/ (distance pt1 pt2) 2.0)
	alf  (- ei51 ei50)
  )
  (setq ei (subst (assoc 5 ei) (assoc 5 ent0) ent0)
	ei (subst (cons 70 0) (assoc 70 ei) ei)
  )
  (if (= sz_color 0) (sz_b 62 ei62 256))
  (if (= sz_layer 0) (setq ei (subst ei8 (assoc 8 ei) ei)))
  (if (= sz_ltype 0) (sz_b 6 ei6 "BYLAYER"))
  (if (= sz_ltsca 0) (sz_b 48 ei48 1.0))
  (setq ei (subst (cons 12 pt1) (assoc 10 ei) ei)
	ei (subst (cons 10 pt2) (assoc 10 ei) ei)
	ei (subst (cons 10 pt1) (assoc 12 ei) ei)
  )
  (if (or (> alf pi) (and (< alf 0) (> alf (- pi))))
    (setq e42 (/ (+ ei40 (sqrt (- (* ei40 ei40) (* lpt lpt)))) lpt)
	  ei  (subst (cons 42 e42) (assoc 42 ei) ei)
    )
    (setq e42 (/ (- ei40 (sqrt (- (* ei40 ei40) (* lpt lpt)))) lpt)
	  ei  (subst (cons 42 e42) (assoc 42 ei) ei)
    )
  )
  (entdel eni)
  (entmake ei)
)

(defun sz_ss()
  (setq wd1 "\n当前匹配属性：" wd0 "不匹配属性：")
  (if (= sz_color 1) (setq wd1 (strcat wd1 " 颜色")) (setq wd0 (strcat wd0 " 颜色")))
  (if (= sz_layer 1) (setq wd1 (strcat wd1 " 图层")) (setq wd0 (strcat wd0 " 图层")))
  (if (= sz_ltype 1) (setq wd1 (strcat wd1 " 线形")) (setq wd0 (strcat wd0 " 线形")))
  (if (= sz_ltsca 1) (setq wd1 (strcat wd1 " 线形比例")) (setq wd0 (strcat wd0 " 线形比例")))
  (if (= sz_asywh 1) (setq wd1 (strcat wd1 " 变宽线形")) (setq wd0 (strcat wd0 " 变宽线形")))
  (if (= wd1 "\n当前匹配属性：") (setq wd1 "\n当前匹配属性：宽度")(setq wd1 (strcat wd1 "。"))) 
  (if (= wd0 "不匹配属性：") (setq wd0 "") (setq wd0 (strcat wd0 "。")))
  (princ (strcat "\n***" wd1 wd0))
  (initget "Setting  ")
  (setq ent1(entsel "\n设置匹配属性S/<拾取参考宽度多义线：>"))
  (if (= ent1 "Setting")
    (progn
      (sz_s)
      (sz_ss)
    )
    (progn
      (if ent1
        (if (/= (type ent1) 'STR)
          (progn
            (if (/= (cdr (assoc 0 (entget (car ent1)))) "LWPOLYLINE") (sz_ss))
            (if (not (assoc 43 (entget (car ent1)))) (sz_ss))
          )
        )
        (sz_ss) 
      )
    )
  )
)

(defun c:xh(/ ent1 ent0 wd0 wd1 setk ss1 len i e62 e8 e6 e48 e90 e43 ei eni)
  (command "color" (getvar "cecolor"))
  (if (not sz_color) (setq sz_color 1))
  (if (not sz_layer) (setq sz_layer 1))
  (if (not sz_ltype) (setq sz_ltype 1))
  (if (not sz_ltsca) (setq sz_ltsca 1))
  (if (not sz_asywh) (setq sz_asywh 0))
  (sz_ss)
  (if (/= (type ent1) 'STR)
    (progn
      (princ "\n拾取欲变宽度的线实体：")
      (setq ss1 (ssget '((-4 . "<OR")(0 . "CIRCLE")(0 . "LINE")(0 . "ARC")(0 . "LWPOLYLINE")(-4 . "OR>"))))  
      (if ss1
        (sz_main)
      )
    )
  )
  (princ)
)
;****************************************************BPL变多义线
(defun c:bpl () (c:pljoinfuzz)) ; this line can be commented out if there is an existing command called jf
(defun c:pljoinfuzz (/ ss1 entLine objType oldcmdecho oldpeditaccept fuzz okObjects)
  (setq oldcmdecho (getvar "cmdecho"))
  (setq oldpeditaccept (getvar "PEDITACCEPT"))
  (setvar "cmdecho" 0)
  (setq A2k4 (>= (substr (getvar "ACADVER") 1 2) "16"))
  (if A2k4 (setvar "PEDITACCEPT" 0))
  (setq	okObjects '((0 . "LINE,ARC,POLYLINE,LWPOLYLINE")))
  (princ "\nSelect object to join: ")
  (setq ss1 (ssget okObjects))
  (setq fuzz (getdist "\nFuzz distance <0>: "))
  (if (= fuzz nil) (setq fuzz 0))
  (if (/= ss1 nil)
      (progn
	(setq objType (cdr (assoc 0 (entget (setq entLine (ssname ss1 0))))))
	(if (= (sslength ss1) 1) (setq ss1 (ssget "X" okObjects)))
	(if (member objType '("LINE" "ARC"))
	  (command "_.pedit" "_M" ss1 "" "_Y" "_J" "_J" "_B" fuzz "")
	  (command "_.pedit" "_M" ss1 "" "_J" "_J" "_B" fuzz "")
	)
      )
  )
  (setvar "cmdecho" oldcmdecho)
  (if A2k4 (setvar "PEDITACCEPT" oldpeditaccept))
  (princ)
)
;****************************************************CLL测量长度
(defun c:cll (/ en curve len)
  (if (setq en (entsel))
    (progn
      (setq curve (vlax-ename->vla-object (car en)))
      (if (vl-catch-all-error-p
            (setq len (vl-catch-all-apply
                        'vlax-curve-getDistAtParam
                        (list curve
                              (vl-catch-all-apply
                                'vlax-curve-getEndParam
                                (list curve)
                              )
                        )
                      )
            )
          )
        nil
        len
      )
    )
  )
)
;****************************************************LLA列出所有图层
(defun ax:layer-lw-list (/ layer lw lst)
  (vlax-for layer (vla-get-Layers
                    (vla-get-ActiveDocument
                      (vlax-get-acad-object)
                    )
                  )
    (setq lw (vla-get-lineweight layer))
    (if (= lw -3)
      (setq lw 0.25 lwt "Default")
      (setq lw (/ lw 100.0) lwt (strcat (rtos lw 2 2) " mm"))
    )
    (setq lst (cons
                (list
                  (vla-get-name layer)
                  lw
                  lwt
                ) lst))
  )
  (vl-sort lst
           (function (lambda (e1 e2)
                       (< (strcase (car e1)) (strcase (car e2)))
                     )
           )
  ) 
)

(defun c:lla (/ p row y ts xd plinewidold)
  (setq p (getpoint "Specify top left point of list: "))
  (setq ts (getvar "textsize"))
  (setq y (cadr p))
  (setq xd (* ts 15)) ; dist between columns
  (setq plinewidold (getvar "PLINEWID"))
  (if p
    (foreach row (ax:layer-lw-list)
      (command "text" p "" "" (car row))
      (setvar "PLINEWID" (* (/ ts 2.11) (cadr row)))
      (command "pline"
               (list (+ (car p) (* 0.9 xd)) (+ (cadr p) (/ ts 2.0)) (caddr p))
               (list (+ (car p) (* 0.98 xd)) (+ (cadr p) (/ ts 2.0)) (caddr p))
               ""
      )
      (command "text" (list (+ (car p) xd) (cadr p) (caddr p)) "" "" (caddr row))
      (setq y (- y (* ts 1.66667)))
      (setq p (list (car p) y (caddr p)))
    )
  )
  (setvar "PLINEWID" plinewidold)
  (princ)
)
;****************************************************HMJ填充面积
(defun c:hmj () (hatchb nil)) ; this line can be commented out if there is an existing command called hb
(defun c:hbl () (hatchb T)) ; this line can be commented out if there is an existing command called hbl
(defun c:hatchb () (hatchb nil))
(defun hatchb (hl  /     es    blay  ed1   ed2   loops1      bptf  part
             et    noe   plist ic    bul   nr    ang1  ang2  obj *ModelSpace* *PaperSpace*
             space cw errexit undox olderr oldcmdecho ss1 lastent en1 en2 ss lwp
             list->variantArray 3dPoint->2dPoint A2k ent i ss2
             knot-list controlpoint-list kn cn pos xv bot area hst noarea
            )
 (setq A2k (>= (substr (getvar "ACADVER") 1 2) "15"))
 (if A2k
   (progn
     (defun list->variantArray (ptsList / arraySpace sArray)
       (setq arraySpace
	      (vlax-make-safearray
		vlax-vbdouble
		(cons 0 (- (length ptsList) 1))
	      )
       )
       (setq sArray (vlax-safearray-fill arraySpace ptsList))
       (vlax-make-variant sArray)
     )
     (defun areaOfObject (en / curve area)
       (if en
	 (if A2k
	   (progn
	     (setq curve (vlax-ename->vla-object en))
	     (if
	       (vl-catch-all-error-p
		 (setq
		   area
		    (vl-catch-all-apply 'vlax-curve-getArea (list curve))
		 )
	       )
		nil
		area
	     )
	   )
	   (progn
	     (command "._area" "_O" en)
	     (getvar "area")
	   )
	 )
       )
     )
   )
 )
 (if A2k
  (defun 3dPoint->2dPoint (3dpt)
    (list (float (car 3dpt)) (float (cadr 3dpt)))
  )
 )

  (defun errexit (s)
    (princ "\nError:  ")
    (princ s)
    (restore)
  )

  (defun undox ()
    (command "._ucs" "_p")
    (command "._undo" "_E")
    (setvar "cmdecho" oldcmdecho)
    (setq *error* olderr)
    (princ)
  )

  (setq olderr  *error*
        restore undox
        *error* errexit
  )
  (setq oldcmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (command "._UNDO" "_BE")
  (if A2k (progn
    (vl-load-com)
    (setq *ModelSpace* (vla-get-ModelSpace
                         (vla-get-ActiveDocument (vlax-get-acad-object))
                       )
          *PaperSpace* (vla-get-PaperSpace
                         (vla-get-ActiveDocument (vlax-get-acad-object))
                       )
    ))
  )


; Remove for testing purpose
; (setq A2k nil)
  
  (if (/= (setq ss2 (ssget '((0 . "HATCH")))) nil)
   (progn
    (setq i 0)
    (setq area 0)
    (setq bMoreLoops nil)
    (while (setq ent (ssname ss2 i))
      (setq noarea nil)
      (setq ed1 (entget ent))
      (setq layer (cdr (assoc 8 ed1)))
      (if (not (equal (assoc 210 ed1) '(210 0.0 0.0 1.0))) (princ "\nHatch not in WCS!"))
      (setq xv (cdr (assoc 210 ed1)))
      (command "._ucs" "_w")
      (setq loops1 (cdr (assoc 91 ed1))) ; number of boundary paths (loops)
      (if (and A2k (= (strcase (cdr (assoc 410 ed1))) "MODEL"))
        (setq space *ModelSpace*)
        (setq space *PaperSpace*)
      )
      (repeat loops1
        (setq ed1 (member (assoc 92 ed1) ed1))
        (setq bptf (cdr (car ed1))) ; boundary path type flag
        (setq ic (cdr (assoc 73 ed1))) ; is closed
        (setq noe (cdr (assoc 93 ed1))) ; number of edges
	(setq bot (cdr (assoc 92 ed1))) ; boundary type
	(setq hst (cdr (assoc 75 ed1))) ; hatch style
        (setq ed1 (member (assoc 72 ed1) ed1))
        (setq bul (cdr (car ed1))) ; bulge
        (setq plist nil)
        (setq blist nil)
        (cond
          ((> (boole 1 bptf 2) 0) ; polyline
           (repeat noe
             (setq ed1 (member (assoc 10 (cdr ed1)) ed1))
             (setq plist (append plist (list (cdr (assoc 10 ed1)))))
             (setq blist (append blist
                                 (if (> bul 0)
                                   (list (cdr (assoc 42 ed1)))
                                   nil
                                 )
                         )
             )
           )
           (if A2k (progn
             (setq polypoints
                    (apply 'append
                           (mapcar '3dPoint->2dPoint plist)
                    )
             )
             (setq VLADataPts (list->variantArray polypoints))
             (setq obj (vla-addLightweightPolyline space VLADataPts))
             (setq nr 0)
             (repeat (length blist)
               (if (/= (nth nr blist) 0)
                 (vla-setBulge obj nr (nth nr blist))
               )
               (setq nr (1+ nr))
             )
             (if (= ic 1)
               (vla-put-closed obj T)
             )
	     (if hl (vla-put-layer obj layer))
            )
            (progn
	      (setq ne (append (list '(0 . "POLYLINE")) (list (cons 66 1))))
	      (if (= ic 1) (setq ne (append ne (list (cons 70 1)))))
	      (if hl (setq ne (append ne (list (cons 8 layer)))))
              (entmake ne)
              (setq nr 0)
              (repeat (length plist)
                (if (= bul 0)
                  (entmake (list (cons 0 "VERTEX")
                                 (cons 10 (nth nr plist))
                           )
                  )
                  (entmake (list (cons 0 "VERTEX")
                                 (cons 10 (nth nr plist))
                                 (cons 42 (nth nr blist))
                           )
                  )
                )
                (setq nr (1+ nr))
              )
              (entmake '((0 . "SEQEND")))
            )
           )
          )
          (t ; not polyline
           (setq lastent (entlast))
           (setq lwp T)
           (repeat noe
             (setq et (cdr (assoc 72 ed1)))
             (cond
               ((= et 1) ; line
                (setq ed1 (member (assoc 10 (cdr ed1)) ed1))
                (if A2k
		  (progn
                    (vla-AddLine
                      space
                      (vlax-3d-point (cdr (assoc 10 ed1)))
                      (vlax-3d-point (cdr (assoc 11 ed1)))
                    )
		    (if hl (vla-put-layer obj layer))
		  )
		  (progn
		    (setq ne (append (list (cons 0 "LINE"))
                        (list (list 10 (cadr (assoc 10 ed1)) (caddr (assoc 10 ed1)) 0))
                        (list (list 11 (cadr (assoc 11 ed1)) (caddr (assoc 11 ed1)) 0))
		      ;  (cons 210 xv)
                      )
                    )
		    (if hl (setq ne (append ne (list (cons 8 layer)))))
                    (entmake ne)
		  )
                )
                (setq ed1 (cddr ed1))
               )
               ((= et 2) ; circular arc
                 (setq ed1 (member (assoc 10 (cdr ed1)) ed1))
                 (setq ang1 (cdr (assoc 50 ed1)))
                 (setq ang2 (cdr (assoc 51 ed1)))
                 (setq cw (cdr (assoc 73 ed1)))
                 (if (and (equal ang1 0 0.00001) (equal ang2 6.28319 0.00001))
                   (progn
                     (if A2k
		       (progn
                         (vla-AddCircle
                           space
                           (vlax-3d-point (cdr (assoc 10 ed1)))
                           (cdr (assoc 40 ed1))
                         )
		         (if hl (vla-put-layer obj layer))
		       )
		       (progn
			 (setq ne (append
				      (list (cons 0 "CIRCLE"))
				      (list (cons 8 layer))
                                      (list (assoc 10 ed1))
                                      (list (assoc 40 ed1))
                                )
                         )
			 (if hl (setq ne (append ne (list (cons 8 layer)))))
                         (entmake ne)
		       )
                     )
                     (setq lwp nil)
                   )
                   (if A2k
		     (progn
                       (vla-AddArc
                         space
                         (vlax-3d-point (cdr (assoc 10 ed1)))
                         (cdr (assoc 40 ed1))
                         (if (= cw 0)
                           (- 0 ang2)
                           ang1
                         )
                         (if (= cw 0)
                           (- 0 ang1)
                           ang2
                         )
		       )
		       (if hl (vla-put-layer obj layer))
                     )
		     (progn
		       (setq ne (append (list (cons 0 "ARC"))
                                    (list (assoc 10 ed1))
                                    (list (assoc 40 ed1))
                                    (list (cons 50
                                          (if (= cw 0)
                                            (- 0 ang2)
                                            ang1
                                          )
                                    ))
                                    (list (cons 51
                                          (if (= cw 0)
                                            (- 0 ang1)
                                            ang2
                                          )
                                    ))
                              )
		       )
		       (if hl (setq ne (append ne (list (cons 8 layer)))))
                       (entmake ne)
		     )
                   )
                 )
                 (setq ed1 (cddddr ed1))
               )
               ((= et 3) ; elliptic arc
                (setq ed1 (member (assoc 10 (cdr ed1)) ed1))
                (setq ang1 (cdr (assoc 50 ed1)))
                (setq ang2 (cdr (assoc 51 ed1)))
                (setq cw (cdr (assoc 73 ed1)))
                (if A2k (progn
                  (setq obj (vla-AddEllipse
                              space
                              (vlax-3d-point (cdr (assoc 10 ed1)))
                              (vlax-3d-point (cdr (assoc 11 ed1)))
                              (cdr (assoc 40 ed1))
                            )
                  )
                  (vla-put-startangle obj (if (= cw 0) (- 0 ang2) ang1))
                  (vla-put-endangle obj (if (= cw 0) (- 0 ang1) ang2))
		  (if hl (vla-put-layer obj layer))
                 )
		 (progn
                   (princ "\nElliptic arc not supported!")
		   (setq noarea T)
		 )
                )
                (setq lwp nil)
               )
               ((= et 4) ; spline
                (setq ed1 (member (assoc 94 (cdr ed1)) ed1))
                (setq knot-list nil)
                (setq controlpoint-list nil)
		(setq kn (cdr (assoc 95 ed1)))
                (setq cn (cdr (assoc 96 ed1)))
                (setq pos (vl-position (assoc 40 ed1) ed1))
                (repeat kn
                  (setq knot-list (cons (cons 40 (cdr (nth pos ed1))) knot-list))
                  (setq pos (1+ pos))
                )
                (setq pos (vl-position (assoc 10 ed1) ed1))
                (repeat cn
                  (setq controlpoint-list (cons (cons 10 (cdr (nth pos ed1))) controlpoint-list))
                  (setq pos (1+ pos))
                )
                (setq knot-list (reverse knot-list))
                (setq controlpoint-list (reverse controlpoint-list))
		(setq ne (append
		               (list '(0 . "SPLINE"))
                               (list (cons 100 "AcDbEntity"))
                               (list (cons 100 "AcDbSpline"))
                               (list (cons 70 (+ 1 8 (* 2 (cdr (assoc 74 ed1))) (* 4 (cdr (assoc 73 ed1))))))
                               (list (cons 71 (cdr (assoc 94 ed1))))
                               (list (cons 72 kn))
                               (list (cons 73 cn))
                               knot-list
                               controlpoint-list
                      )
		)
		(if hl (setq ne (append ne (cons 8 layer))))
                (entmake ne)
		(setq ed1 (member (assoc 10 ed1) ed1))
                (setq lwp nil)
               )
             ) ; end cond
           ) ; end repeat noe
           (if lwp (progn
             (setq en1 (entnext lastent))
             (setq ss (ssadd))
             (ssadd en1 ss)
             (while (setq en2 (entnext en1))
               (ssadd en2 ss)
               (setq en1 en2)
             )
	     (if (= (getvar "peditaccept") 1)
               (command "_.pedit" (entlast) "_J" ss "" "")
	       (command "_.pedit" (entlast) "_Y" "_J" ss "" "")
	     )
          ))

          ) ; end t
        ) ; end cond
;	Tries to get the area on islands but it's not clear how to know if an island is filled or not
;	and if it should be substracted or added to the total area.
;	(if (or (= bot 0) (= (boole 1 bot 1) 1)) (setq area (+ area (areaOfObject (entlast)))))
;	(if (and (/= hst 1) (/= bot 0) (= (boole 1 bot 1) 0)) (setq area (- area (areaOfObject (entlast)))))
;	(princ "\n") (princ bot) (princ "\n") (princ hst) (princ "\n")
;	(princ (areaOfObject (entlast)))
      ) ; end repeat loops1
      (if (and (= noarea nil) (= loops1 1)) (setq area (+ area (areaOfObject (entlast)))) (setq bMoreLoops T))
      (setq i (1+ i))
    )
   )
  )
  (if (and area (not bMoreLoops)) (progn
    (princ "\nTotal Area = ")
    (princ area)
  ))
  (restore)
  (princ)
)
;****************************************************BG(灰)BW(白)BK(黑)改底色
; Set the background in model and paper space to grey
(defun c:BG ()
  (vl-load-com)
  (setq disp (vla-get-display (vla-get-preferences (vlax-get-acad-object))))
  (setq drafting (vla-get-drafting (vla-get-preferences (vlax-get-acad-object))))
  (vla-put-GraphicsWinModelBackgrndColor disp 5987163)
  (vla-put-GraphicsWinLayoutBackgrndColor disp 5987163)
  (vla-put-LayoutCrosshairColor disp 16777215)
  (vla-put-ModelCrosshairColor disp 16777215)
  (vla-put-AutoTrackingVecColor disp 16777215)
  (vla-put-AutoSnapMarkerColor drafting 2)
  (princ)
)

; Set the background in model and paper space to white
(defun c:BW ()
  (vl-load-com)
  (setq disp (vla-get-display (vla-get-preferences (vlax-get-acad-object))))
  (setq drafting (vla-get-drafting (vla-get-preferences (vlax-get-acad-object))))
  (vla-put-GraphicsWinModelBackgrndColor disp 16777215)
  (vla-put-GraphicsWinLayoutBackgrndColor disp 16777215)
  (vla-put-LayoutCrosshairColor disp 0)
  (vla-put-ModelCrosshairColor disp 0)
  (vla-put-AutoTrackingVecColor disp 0)
  (vla-put-AutoSnapMarkerColor drafting 6)
  (princ)
)

; Set the background in model and paper space to black
(defun c:Bk ()
  (vl-load-com)
  (setq disp (vla-get-display (vla-get-preferences (vlax-get-acad-object))))
  (setq drafting (vla-get-drafting (vla-get-preferences (vlax-get-acad-object))))
  (vla-put-GraphicsWinModelBackgrndColor disp 0)
  (vla-put-GraphicsWinLayoutBackgrndColor disp 0)
  (vla-put-LayoutCrosshairColor disp 16777215)
  (vla-put-ModelCrosshairColor disp 16777215)
  (vla-put-AutoTrackingVecColor disp 16777215)
  (vla-put-AutoSnapMarkerColor drafting 2)
  (princ)
)

; Background toggle between black and white
(defun c:bgt ()
  (vl-load-com)
  (setq	disp (vla-get-display (vla-get-preferences (vlax-get-acad-object))))
  (setq	drafting (vla-get-drafting (vla-get-preferences (vlax-get-acad-object))))
  (if (= (vlax-variant-value
	   (vlax-variant-change-type
	     (vla-get-graphicswinmodelbackgrndcolor disp)
	     vlax-vblong
	   )
	 )
	 0
      )
    (c:bgwhite)
    (c:bgblack)
  )
  (princ)
)

(princ)
;****************************************************AAO面积
(defun c:aao (/ en curve area)
  (if (setq en (entsel))
    (progn
      (setq curve (vlax-ename->vla-object (car en)))
      (if
        (vl-catch-all-error-p
          (setq
            area (vl-catch-all-apply 'vlax-curve-getArea (list curve))
          )
        )
         nil
         area
      )
    )
  )
)




;****************************************************C1～4增强拷贝
(defun c:c1 (/ p1 p2 s e cn)
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



(defun c:c2 (/ p1 p2 s e cn a1 d1 ns cnn)
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


(defun c:c3 (/ p1 p2 s e cn)
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
(defun c:c4 (/ getpt getpt1 ss ptx pty db n x y gtin)
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
;****************************************************DN标管径
;;****此程序功能是对管道线进行标注，其符号为DN。
(defun dxf (code elist)
  (cdr (assoc code elist))
)

(defun c:dn (/ cmd_old os_old ss ss1 ss2 pt pt0 pt1 pt2 ang dn dn0 bdn_er bdn_oe)
  (setq cmd_old (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (setq os_old (getvar "osmode"))
  (setvar "osmode" 0)

  (defun bdn_er	(s)			; If an error (such as ESC) occurs
					; while this command is active...
    (if	(/= msg "功能取消")
      (if (= msg "退出 / 中止")
	(princ)
	(princ (strcat "\n功能取消!"))
      )
    )
    (eval (read U:E))
    (if	bdn_oe				; If an old error routine exists
      (setq *error* bdn_oe)		; then, reset it
    )
    (if	temp
      (redraw temp 1)
    )
    (princ)
  )
  (if *error*				; Set our new error handler
    (setq bdn_oe  *error*
	  *error* bdn_er
    )
    (setq *error* bdn_er)
  )

  ;; Set undo groups and ends with (eval(read U:G)) or (eval(read U:E))
  (setq	U:G "(command \"undo\" \"group\")"
	U:E "(command \"undo\" \"en\")"
  )
(while
  (setq ss (entsel "\n请拾取需标注管径的管道<回车退出>:"))
  (menucmd "P0=acad.p02")
  (menucmd "P0=*")
  (setq ss1 (entget (car ss)))
  (setq ss2 (dxf 0 ss1))
  (setq pt (car (cdr ss)))
  (setq pt (osnap pt "NEA"))
  (cond
    ((= ss2 "LINE")

     (setq pt1 (dxf 10 ss1)
	   pt2 (dxf 11 ss1)
     )
      (setq ang0 (angle pt1 pt2))
      (if
          (and (> ang0 (* PI 0.5)) (<= ang0 (* PI 1.5)))
          (setq ang0 (+ ang0 PI))
      )
      (setq ang (+ ang0 (* PI 0.5)))
      (setq pt0 (polar pt ang (* (getvar "textsize") 10)))
      (setq pt (inters pt1 pt2 pt0 pt nil))
      (setq pt (polar pt ang (* (getvar "textsize") 0.35)))       
      (setq dn0 "20")
      (setq dn (getstring (strcat "\n请输入该管道管径<" dn0 ">:")))
    (if
      (= dn "")
      (setq dn dn0)
      (setq dn0 dn)
    )
      (setq dn (strcat "DN" dn))
      (command "_.text" "C" pt (getvar "textsize") (angtos ang0 0 3) dn)
  )					;cond1


  ((= ss2 "LWPOLYLINE")
        (setq ss1 (member (assoc 10 ss1) ss1))
        (setq pt1 (dxf 10 ss1))

        (setq r 1)
    (while r
        (setq ss1 (cdr ss1)
              ss1 (member (assoc 10 ss1) ss1)
              pt2 (dxf 10 ss1)
        )


        (setq dt1 (distance pt1 pt)
              dt2 (distance pt pt2)
              dt1 (+ dt1 dt2)
              dt1 (rtos dt1 2 1)
              dt1 (distof dt1 2)
              dt2 (distance pt1 pt2)
              dt2 (rtos dt2 2 1)
              dt2 (distof dt2 2)
        )

       (if (= dt1 dt2)
           (setq r nil)
           (setq pt1 pt2)
       )
     )    ;end while
          (setq ang0 (angle pt1 pt2))
      (if
          (and (> ang0 (* PI 0.5)) (<= ang0 (* PI 1.5)))
          (setq ang0 (+ ang0 PI))
      )
      (setq ang (+ ang0 (* PI 0.5)))
      (setq pt0 (polar pt ang (* (getvar "textsize") 2)))
      (setq pt (inters pt1 pt2 pt0 pt nil))
      (setq pt (polar pt ang (* (getvar "textsize") 0.4)))       
      (setq dn0 "20")
      (setq dn (getstring (strcat "\n请输入该管道管径<" dn0 ">:")))
    (if
      (= dn "")
      (setq dn dn0)
      (setq dn0 dn)
    )
      (setq dn (strcat "DN" dn))
      (command "_.text" "C" pt (getvar "textsize") (angtos ang0 0 3) dn)

  )					;cond2
  (T
      (alert "\n所选图元不能进行管径标注!重新选取")
  )					;T
)					;cond
)                                       ;while
(setvar "cmdecho" cmd_old)
(setvar "osmode" os_old)
(princ)
);end of defun
;****************************************************JS根据喷头数给出管径
(defun c:js(/ dptll xptll dptge xptge ll ls ll2)

     (setq dptll 250 xptll 80)
     (princ "输入大喷头个数:")
     (setq dptge (getreal))
     (princ "输入小喷头个数:")
     (setq xptge (getreal))
     (setq ll (+ (* dptge dptll)(* xptge xptll)))
     (setq ll2 (/ ll 60))
     (setq ls 5.0)
     (princ "\n流量:")(princ ll2)(princ "L/s.")
     (if (< ll 81)
        (princ "\n管径:DN25")
        (if (< ll 168)
            (princ "\n管径:DN32")
            (if (< ll 264)
                (princ "\n管径:DN40")
                (if (< ll 480)
                    (princ "\n管径:DN50")
                    (if (< ll 1056)
                        (princ "\n管径:DN70")
                        (if (< ll 1488)
                            (princ "\n管径:DN80")
                            (if (< ll 1800)
                                (princ "\n管径:DN100")
                                (princ "\n流量大于30L/s,管径为DN100!")
                            )
                        )
                     )
                 )
              )
          )
       )
       (princ)
)
;****************************************************LAE删除冻结的图层
(defun c:lae (/ la)
  (defun layer-del (layer / e d c f)
    (if (setq e (tblobjname "layer" layer))
      (progn
        (setq
          d   (entget e)
          c   (cdr (assoc 62 d))
          f   (cdr (assoc 70 d))
          del nil
        )
        (if (minusp c)
                                        ; layer is off, force abs of color
          (progn (setq del T)
                 (setq d (subst (cons 62 (abs c)) (assoc 62 d) d))
          )
        )
        (if (eq 1 (logand 1 f))
                                        ; layer is frozen, mask off 1
          (progn (setq del T)
                 (setq f (boole 6 f 1))
          )
        )
        (if (eq 4 (logand 4 f))
                                        ; layer is locked, mask off 4
          (setq f (boole 6 f 4))
        )
                                        ; did we change the flag value?
        (if (not (eq f (cdr (assoc 70 d))))
          (setq d (subst (cons 70 f) (assoc 70 d) d))
        )
                                        ; did we change the dxf data at all?
        (if (not (equal d (entget e)))
          (entmod d)
        )
        (if del
          (progn
            (setq ss  (ssget "X" (list (cons 8 layer)))
                  doc (vla-get-activedocument (vlax-get-acad-object))
                  c   -1
            )
            (vla-put-activeLayer
              doc
              (vla-item (vla-get-layers doc) "0")
            )
            (if ss
              (repeat (sslength ss)
                (vla-erase
                  (vlax-ename->vla-object (ssname ss (setq c (1+ c))))
                )
              )
            )
;;; purge the layer
            (vl-catch-all-apply
              'vla-delete
              (list (vla-item (vla-get-layers doc) layer))
            )
;;; if not purged freeze it again
            (if (setq e (tblobjname "layer" layer))
              (command "._layer" "_f" layer "")
            )
          )
        )
      )
    )
  )
  (vlax-for la (vla-get-layers
                 (vla-get-activedocument (vlax-get-acad-object))
               )
    (layer-del (vla-get-name la))
  )
)
;****************************************************HB填充基点
(defun c:hb (/ oldos oldsn oldcmdecho i ent)
  (setq oldos (getvar "osmode"))
  (setq oldsn (getvar "snapbase"))
  (setq oldcmdecho (getvar "cmdecho"))
  (setvar "osmode" 47)
  (princ "\nSelect point you wish Hatch(s) to start from...")
  (command "._snapbase" pause)
  (princ "\nSelect Hatch(s) to adjust snapbase")
  (if (not (setq ss (ssget)))
    (alert "\n No Entities selected..... Please try again.")
    (progn
      (setq i 0)
      (while (setq ent (ssname ss i))
	(command "._hatchedit" ent "" "" "" "")
	(setq i (1+ i))
      )
    )
  )
  (setvar "snapbase" oldsn)
  (setvar "osmode" oldos)
  (setvar "cmdecho" oldcmdecho)
  (princ)
)
;****************************************************CL连续测量距离
 accdist.lsp
(defun c:cl (/ errexit undox restore *error* p1 p2 sum)
  (defun errexit (s)
    (princ)
    (restore)
  )

  (defun undox ()
    (redraw)
    (setq *error* olderr)
    (princ)
  )

  (setq olderr  *error*
        restore undox
        *error* errexit
  )
  (setq p1  (getpoint "\nSpecify first point: ")
        p2  "First"
        sum 0
  )
  (while (and p1 p2)
    (if (= p2 "First")
      (progn
        (initget 32)
        (setq p2 (getpoint "\nSpecify next point: " p1))
      )
      (progn
        (initget 32 "First")
        (setq p2 (getpoint "\nSpecify next point or [First]: " p1))
      )
    )
    (cond
      ((not p2))
      ((= p2 "First")
       (setq p1 (getpoint "\nSpecify first point: "))
      )
      (t
       (grdraw p1 p2 -1 1)
       (setq sum (+ sum (distance p1 p2))
             p1  p2
       )
      )
    )
  )
  (princ "\nAccumulated distance = ")
  (princ sum)
  (restore)
)

(defun c:accdist1 (/ p1 p2 sum)
  (setq sum 0)
  (setq p1 (getpoint "\nSpecify first point: "))
  (while (and p1
              (not (initget 32))
              (setq p2 (getpoint "\nSpecify next point: " p1))
         )
    (grdraw p1 p2 -1 1)
    (setq sum (+ sum (distance p1 p2)))
    (setq p1 p2)
  )
  (redraw)
  (princ "\nAccumulated distance = ")
  (princ sum)
  (princ)
)

(defun c:accdist2 (/ p1 p2 sum)
  (setq sum 0)
  (while
    (and (setq p1 (getpoint "\nSpecify first point: "))
         (not (initget 32))
         (setq p2 (getpoint "\nSpecify second point: " p1))
    )
     (setq sum (+ sum (distance p1 p2)))
  )
  (princ "\nAccumulated distance = ")
  (princ sum)
  (princ)
)
;****************************************************JH加宽命令
(defun c:jh( / ss width i l ent ed etype co)
  (prompt "\n选择图素变宽小程序 V1.0 1996.4.24")
  (setq co (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (command "undo" "mark")
  (setq width (getreal "\n线宽:"))
  (setq ss (ssget))
  (if ss
    (progn 
      (setq i (sslength ss))
      (setq l 0)
      (repeat i
        (setq ent (ssname ss l))
        (setq ed (entget ent))
        (setq etype (cdr (assoc 0 ed)))
        (if (= etype "POLYLINE")
          (command "pedit" ent "w" width "x")
        )  
        (if (or (= etype "LINE") (= etype "ARC"))
          (command "pedit" ent "y" "w" width "x")
        )
        (if (= etype "CIRCLE")
          (progn
            (setq center (cdr (assoc 10 ed)))
            (setq wd (* (cdr (assoc 40 ed)) 2.0))
            (setq nd (- wd (* width 2.0)))
            (entdel ent)
            (command "donut" nd wd center "")
          )
        )
        (setq l (+ l 1))
      )
    )
  )
  (setq ss nil)
  (command "undo" "end")
  (setvar "cmdecho" co)
  (princ)
)
;****************************************************CD测量长度
(defun c:cd() 
(setq os (getvar "osmode")) 
(setvar "osmode" 0) 
(setq sum 0 i 0) 
(setq ss (ssget)) 
(repeat (sslength ss) 
(setq en (ssname ss i)) 
(command "lengthen" en "") 
(setq l (getvar "perimeter")) 
(setq sum (+ sum l) 
i (+ i 1)) 
) 
(setvar "osmode" os) 
  sum 
)
;****************************************************AAM面积求和
(defun c:aam (/ olderr oldcmdecho errexit undox restore ss1 nr en tot_area)
(defun errexit (s)
(restore)
)

(defun undox ()
(command "._undo" "_E")
(setvar "cmdecho" oldcmdecho)
(setq *error* olderr)
(princ)
)

(setq olderr *error*
restore undox
*error* errexit
)
(setq oldcmdecho (getvar "cmdecho"))
(setvar "cmdecho" 0)
(command "._UNDO" "_BE")
(if (setq ss1 (ssget '((-4 . "<OR")
(0 . "POLYLINE")
(0 . "LWPOLYLINE")
(0 . "CIRCLE")
(0 . "ELLIPSE")
(0 . "SPLINE")
(0 . "REGION")
(-4 . "OR>")
)
)
)
(progn
(setq nr 0)
(setq tot_area 0.0)
(setq en (ssname ss1 nr))
(while en
(command "._area" "_O" en)
(setq tot_area (+ tot_area (getvar "area")))
(setq nr (1+ nr))
(setq en (ssname ss1 nr))
)
(princ "\n面积之和 = ")
(princ tot_area)
)
)
(restore)
)
;****************************************************DDT打断插文字
(defun c:ddt ()
 (setq th (getdist "\请输入文字高度:"))
 (setq r(* th 1.25))
 (setq t (getstring "请输入要插入的文字:"))
 (setq h1 (entsel))
 (setq h2 (getpoint"\n选择插入点:"))
 (while h2
 (command "circle" h2 r)
 (setq na (entlast))
 (command "trim" na "" h1"")
 (command "text""J""M" h2 th""t)
 (command "erase" na"")
 (setq h1 (entsel))
 (setq h2 (getpoint"\n选择插入点:"))
  )
 )
;****************************************************CP圆变多边形
(defun c:cp (/ en n)
  (setvar "cmdecho" 0)
  (setq en (entsel "请选择一个圆"))
  (setq en_data (entget (car en)))
  (setq cen (cdr (assoc 10 en_data)))
  (setq r (cdr (assoc 40 en_data)))
  (setq n (getint "\n请输入正多边形的边数:"))
  (initget "I C")
  (setq	a (getkword "\n输入选项 [内接于圆(I)/外切于圆(C)] <C>:"))
  (if (= a "I")
    (progn
      (command "polygon" n cen "i" r)
    )
    (progn
      (command "polygon" n cen "c" r)
    )
  )
  (command "ERASE" en "")
  (princ)
)
;****************************************************CR改多圆半径
(defun c:cr() 
    (setq cm0(getvar "cmdecho")) 
    (setvar "cmdecho" 0) 
    (princ "\n \n \n") 
        (setq r(getdist "请输入半径或<直接回车单个修改>:")) 
    (if r (progn 
    (setq ss(ssget)) 
    (while ss 
    (setq ll(sslength ss)) 
    (setq ll0 -1) 
    (repeat ll 
        (setq ll0(+ ll0 1)) 
        (setq si(ssname ss ll0)) 
        (setq cc( entget si)) 
        (setq ty(cdr (assoc 0 cc))) 
        (if (or (= ty "CIRCLE") (= ty "ARC")) 
        (progn 
        (setq r0(cdr (assoc 40 cc))) 
        (setq cc(subst (cons 40 r)(assoc 40 cc)cc)) 
        (entmod cc)(entupd si) 
        )) 
     ) 
     (setq ss(ssget)) 
     )) 
     (progn 
    (setq si(entsel "\n选择圆或弧：")) 
    (while si 
        (setq cc(entget (car si))) 
        (setq ty(cdr (assoc 0 cc))) 
        (if (or (= ty "CIRCLE") (= ty "ARC")) 
        (progn 
            (setq nm(cdr (assoc -1 cc))) 
            (setq r0(cdr (assoc 40 cc))) 
            (princ r0)(setq r(getdist "->")) 
            (if r (progn 
            (setq cc(subst (cons 40 r)(assoc 40 cc)cc)) 
            (entmod cc)(entupd nm))) 
        )) 
        (setq si(entsel "\n选择圆或弧：")) 
    ) 
     )) 
     (setvar "cmdecho" cm0) 
)
;****************************************************CM沿某方向多重复制
(defun C:CM ()
(setq A nil)
(setq OM (getvar "OSMODE"))
(setvar "OSMODE" 33)
(setq PNT1 (getpoint "\n方向起点： "))
(setq PNT2 (getpoint "\n方向终点： " PNT1))(terpri)
(initget 1 "M E N")
(prompt "\n选择复制方式： ")
(setq CTYPE
(getkword "[最大间距(M)/精确间距(E)/数量(N)]： "))
(if (= CTYPE "M")
(setq SP (getdist "\n最大对象间距： ")))
(if (= CTYPE "E")
(setq SP (getdist "\n精确对象间距： ")))
(if (= CTYPE "N")
(setq SP (getreal "\n对象数量： ")))
(setq DIST (distance PNT1 PNT2))
(setq ANG (angle PNT1 PNT2))
(setq TEMP1 (/ DIST SP))
(setq TEMP2 (fix (/ DIST SP)))
(setq INC1 SP)
(setq INC2 (/ DIST (+ 1 (fix (/ DIST SP)))))
(setq INC3 (/ DIST (- SP 1)))
(if (= TEMP1 TEMP2) (setq INC INC1) (setq INC INC2))
(if (= CTYPE "E") (setq INC INC1) (setq INC INC))
(if (= CTYPE "N") (setq INC INC3) (setq INC INC))
(setq TMS (FIX (+ 0.00001 (/ DIST INC))))
(setvar "OSMODE" 0)
(setq A (ssget))
(setq INCR 0)
(repeat TMS
(setq INCR (+ INCR INC))
(setq NEWPT (polar PNT1 ANG INCR))
(command "copy" A "" PNT1 NEWPT)
)
(setvar "OSMODE" OM)
(setq A nil)
)
;****************************************************LPN按层改线弧圆宽度
(defun c:lpn(/ s1 s2 wl n s3 s4 stt)
 (setvar "cmdecho" 0)
 (setq wl (getreal "\n输入线宽："))
 (setq stt (getstring "\n输入需改线宽的层名:"))
 (setq s1 (ssget "X" (list (cons 8 stt))))
  (if s1
  (progn
    (setq n 0)
    (repeat (sslength s1)
      (setq s2 (ssname s1 n))
      (if (= wl 0)      
      (if (or (= "POLYLINE" (cdr (assoc 0 (entget s2))))
               (= "LWPOLYLINE" (cdr (assoc 0 (entget s2))))  )
         (command ^"explode" s2))
      (progn
       (if (= "LINE" (cdr (assoc 0 (entget s2))))
         (command ^"pedit" s2 "y" "w" wl ""))
       (if (= "ARC" (cdr (assoc 0 (entget s2))))
         (command ^"pedit" s2 "y" "w" wl ""))
       (if (or (= "POLYLINE" (cdr (assoc 0 (entget s2))))
               (= "LWPOLYLINE" (cdr (assoc 0 (entget s2))))  )
         (command ^"pedit" s2 "w" wl ""))
      )
      );endif
      (setq n (1+ n))
    )
   )
 )
 (setvar "cmdecho" 1)
)
;****************************************************PN改线弧圆宽度
(defun C:pn (/ p l n e q w a m b layer0 color0 linetype0 layer1 color1 linetype1 rad-out rad-in)
  (setq oldblp (getvar "blipmode")
        oldech (getvar "cmdecho")
        olderr *error*
        linetype1 (getvar "celtype")
        layer1 (getvar "clayer")
        color1 (getvar "cecolor")
  )
  (setvar "blipmode" 0) 
  (setvar "cmdecho" 0)
  (defun *error* (msg)
    (princ "\n") 
    (princ msg)
    (setvar "blipmode" oldblp)
    (setvar "cmdecho" oldech)
    (setq *error* olderr)
    (princ)
  )  
  (prompt "\n请选择要改变宽度的线,弧,圆及多义线.")
  (setq p (ssget))
  (setq w (getreal "\n请输入宽度<50>:"))
  (if (not w) (setq w 50))
  (setq l 0 m 0 n (sslength p))
  (while (< l n)
    (setq q (ssname p l))
    (setq ent (entget q))
    (setq b (cdr (assoc 0 ent)))
    (if (member b '("LINE" "ARC"))
      (progn 
        (command "PEDIT" q "y" "w" w "x") 
        (setq m (+ 1 m))
      ) 
    )
    (if (= "LWPOLYLINE" b)
      (progn 
        (command "PEDIT" q "w" w "x") 
        (setq m (+ 1 m))
      ) 
    )
    (if (= "CIRCLE" b)
      (progn 
        (if (assoc 6 ent) (setq linetype0 (cdr (assoc 6 ent))) (setq linetype0 "bylayer"))
        (setq layer0 (cdr (assoc 8 ent)))
        (if (assoc 62 ent) (setq color0 (cdr (assoc 62 ent))) (setq color0 "bylayer"))
        (setq center0 (cdr (assoc 10 ent)))
        (setq radius0 (cdr (assoc 40 ent)))
        (setq diameter0 (* 2 radius0))
        (entdel q)
        (command "color" color0)
        (command "layer" "s" layer0 "")
        (command "linetype" "s" linetype0 "")
        (if (> w diameter0)
          (progn 
            (princ "\n\t 因线宽大于圆的直径，故将该圆填充")
            (princ)
            (setq rad-out (* 2 radius0)
                  rad-in 0
            )
          )
        )
        (if (<= w diameter0)
          (progn 
            (setq rad-out (+ (* 2 radius0) w) 
                  rad-in (- (* 2 radius0) w)
            )
          )
        )
        (command "donut" rad-in rad-out center0 "")
        (setq m (+ 1 m))
      )
    ) 
    (setq l (+ 1 l))
  )
  (if (= 0 m)
    (progn 
     (princ "\n\t  没有任何线,弧,圆及多义线被选中")
      (princ)
    )
  )
  (setvar "blipmode" oldblp)
  (setvar "cmdecho" oldech)
  (setq *error* olderr)
  (command "color" color1)
  (command "layer" "s" layer1 "")
  (command "linetype" "s" linetype1 "")
  (princ)
)
(princ)
;****************************************************改颜色
;自动删除--------------------------------------------------------- AE
(defun c:ae()
  (command "erase" "single" "auto")
)

;改成红色--------------------------------------------------------- 1
(defun c:1(/ ent)
  (setq ent (ssget))
  (command "change" ent "" "p" "c" "1" "")
)
;改成黄色--------------------------------------------------------- 2
(defun c:2(/ ent)
  (setq ent (ssget))
  (command "change" ent "" "p" "c" "2" "")
)
;改成绿色--------------------------------------------------------- 3
(defun c:3(/ ent)
  (setq ent (ssget))
  (command "change" ent "" "p" "c" "3" "")
)
;改成蓝色--------------------------------------------------------- 4
(defun c:4(/ ent)
  (setq ent (ssget))
  (command "change" ent "" "p" "c" "4" "")
)
;改成橙色--------------------------------------------------------- 21
(defun c:21(/ ent)
  (setq ent (ssget))
  (command "change" ent "" "p" "c" "21" "")
)
;改成紫色--------------------------------------------------------- 6
(defun c:6(/ ent)
  (setq ent (ssget))
  (command "change" ent "" "p" "c" "6" "")
)
;改成白色--------------------------------------------------------- 7
(defun c:7(/ ent)
  (setq ent (ssget))
  (command "change" ent "" "p" "c" "7" "")
)
;改成灰色-------------------------------------------------------8
(defun c:8(/ ent)
  (setq ent (ssget))
  (command "change" ent "" "p" "c" "8" "")
)
;改成墨绿色-------------------------------------------------------123
 (defun c:123(/ ent)
   (setq ent(ssget))
   (command "change" ent "" "p" "c" "123" "")

)
;改成自选颜色----------------------------------------------------cb
 (defun c:cb(/ ent)
   (setq ent(ssget))
   (command "change" ent "" "p" "c" pause "")
)
;改成虚线------------------------------------------------------- XX
(defun c:XX(/ ent)
  (setq ent (ssget))
  (command "change" ent "" "p" "lt" "hidden2" "")
  (command "ltscale")
)
;改成点划线----------------------------------------------------- DL
(defun c:DL(/ ent)
  (setq ent (ssget))
  (command "change" ent "" "p" "lt" "dashdot" "")
  (command "ltscale")
)
;改图层----------------------------------------------------- cv
(defun c:cv(/ ent)
  (setq ent (ssget))
  (command "change" ent "" "p" "la" pause "")
)
;****************************************************AN旋转绘图角度
(defun c:an()  (command "snapang"))
(defun c:0()   (command "snapang" "0"))
(defun c:30()  (command "snapang" "30"))
(defun c:45()  (command "snapang" "45"))
(defun c:60()  (command "snapang" "60"))
;****************************************************Z0轴归零
(defun c:z0()
   (setvar "cmdecho" 0)
   (setvar "blipmode" 0)
   (graphscr)
   (prompt "Z向归零:") (terpri)

   (princ "请选择要归零的实体") 
   (setq s (ssget))
   (setq len (sslength s))
   (setq index 0)

   (repeat len
      (setq a (entget (ssname s index)))

      (setq b10 (assoc 10 a))
      (setq b11 (assoc 11 a))

      (setq x10 (cadr b10))
      (setq y10 (caddr b10))

      (setq x11 (cadr b11))
      (setq y11 (caddr b11))

      (setq b101 (cons 10 (list x10 y10 0)))
      (setq b111 (cons 11 (list x11 y11 0)))

      (setq a (subst b101 b10 a))
      (entmod a)
      (setq a (subst b111 b11 a))
      (entmod a)

      (setq index (+ index 1))
   )
   (princ "成功")
   (princ)
)
;****************************************************undolock加密
(defun lockerror (msg)
  (if (/= msg "Function cancelled")
    (princ
      (strcat "\nError: " msg " [" (itoa (getvar "ERRNO")) "]")
    )
    (princ)
  )
  (command "UNDO" "End")
  (Abort "\n加密操作被放弃！")
  (setq *error* olderr)
  (princ)
)

(defun Abort (msg)
  (setvar "filedia" fdia)
  (setvar "cmddia" cdia)
  (setvar "cmdecho" cmd)
  (alert msg)
)
;;Exit

(defun getlayers ()
  (setq lyr (tblnext "layer" t))
  (setq laylist "")
  (while lyr
    (if	(or (and (= (cdr (assoc 62 lyr)) 8)
		 (not (wcmatch (cdr (assoc 2 lyr)) "*|*"))
	    )
	    (and (= (cdr (assoc 62 lyr)) 9)
		 (not (wcmatch (cdr (assoc 2 lyr)) "*|*"))
	    )
	    (and (= (cdr (assoc 62 lyr)) 251)
		 (not (wcmatch (cdr (assoc 2 lyr)) "*|*"))
	    )
	    (and (= (cdr (assoc 62 lyr)) 252)
		 (not (wcmatch (cdr (assoc 2 lyr)) "*|*"))
	    )
	    (and (= (cdr (assoc 62 lyr)) 253)
		 (not (wcmatch (cdr (assoc 2 lyr)) "*|*"))
	    )
	    (and (= (cdr (assoc 62 lyr)) 254)
		 (not (wcmatch (cdr (assoc 2 lyr)) "*|*"))
	    )
	    (and (= (cdr (assoc 62 lyr)) 255)
		 (not (wcmatch (cdr (assoc 2 lyr)) "*|*"))
	    )
	)
      (if (equal laylist "")
	(setq laylist (strcat laylist (cdr (assoc 2 lyr))))
	(setq laylist (strcat laylist "," (cdr (assoc 2 lyr))))
      )
    )
    (setq lyr (tblnext "layer"))
  )
  laylist
)

(defun backblk (layoutName Mins)
  (if layoutName
    (cond
      ((= layoutName "14MS")
       (setq blist (list '(-4 . "<NOT")
			 '(-4 . "<OR")
			 '(67 . 1)
			 '(0 . "SOLID")
			 '(2 . "SOLID")
			 '(-4 . "OR>")
			 '(-4 . "NOT>")
			 '(-4 . "<OR")
			 (cons 8 (getlayers))
			 '(62 . 8)
			 '(62 . 9)
			 '(62 . 251)
			 '(62 . 252)
			 '(62 . 253)
			 '(62 . 254)
			 '(62 . 255)
			 '(-4 . "OR>")
		   )
       )
      )
      ((= layoutName "14PS")
       (setq blist (list '(67 . 1)
			 '(-4 . "<NOT")
			 '(-4 . "<OR")
			 '(0 . "SOLID")
			 '(2 . "SOLID")
			 '(0 . "VIEWPORT")
			 '(-4 . "OR>")
			 '(-4 . "NOT>")
			 '(-4 . "<OR")
			 (cons 8 (getlayers))
			 '(62 . 8)
			 '(62 . 9)
			 '(62 . 251)
			 '(62 . 252)
			 '(62 . 253)
			 '(62 . 254)
			 '(62 . 255)
			 '(-4 . "OR>")
		   )
       )
      )
      (T
       (setq blist (list (cons 410 layoutName)
			 '(-4 . "<NOT")
			 '(-4 . "<OR")
			 '(0 . "SOLID")
			 '(2 . "SOLID")
			 '(0 . "VIEWPORT")
			 '(-4 . "OR>")
			 '(-4 . "NOT>")
			 '(-4 . "<OR")
			 (cons 8 (getlayers))
			 '(62 . 8)
			 '(62 . 9)
			 '(62 . 251)
			 '(62 . 252)
			 '(62 . 253)
			 '(62 . 254)
			 '(62 . 255)
			 '(-4 . "OR>")
		   )
       )
      )
    )
    (setq blist	(list '(-4 . "<NOT")
		      '(-4 . "<OR")
		      '(0 . "SOLID")
		      '(2 . "SOLID")
		      '(0 . "VIEWPORT")
		      '(-4 . "OR>")
		      '(-4 . "NOT>")
		      '(-4 . "<OR")
		      (cons 8 (getlayers))
		      '(62 . 8)
		      '(62 . 9)
		      '(62 . 251)
		      '(62 . 252)
		      '(62 . 253)
		      '(62 . 254)
		      '(62 . 255)
		      '(-4 . "OR>")
		)
    )
  )
  (setq ssetb (ssget "X" blist))
  (setq viewsset (ssget "X" '((0 . "VIEWPORT"))))
  (if viewsset
    (progn
      (setq n 0)
      (repeat (sslength viewsset)
	(if (setq clipent (assoc 340 (entget (ssname viewsset n))))
	  (ssdel (cdr clipent) ssetb)
	)
	(setq n (1+ n))
      )
    )
  )
  (if ssetb
    (progn
      (setq pt (list 0.0 0.0))
      (entmake ;;write block header
	       (list '(0 . "BLOCK")
		     '(2 . "*anon")
		     '(70 . 1)
		     (cons '10 pt)
	       )
      )
      (setq a 0)
      (repeat (sslength ssetb)
	(setq ent2 (entmake (entget (setq ent (ssname ssetb a)))))
	(if (null ent2)
	  (princ (entget (setq ent (ssname ssetb a))))
	)
	(if (assoc 66 (entget ent))
	  (progn
	    (setq subent (entnext ent))
	    (while (/= (cdr (assoc 0 (entget subent))) "SEQEND")
	      (entmake (entget subent))
	      (setq subent (entnext subent))
	    )
	    (setq ent3 (entmake (entget subent)))
	    (if	(null ent3)
	      (princ (entget subent))
	    )
	  )
	)
	(entdel ent)
	(setq a (1+ a))
	(c:spin "Making Block of background colours..")
      )
      (setq nameb (entmake '((0 . "endblk"))))
      (princ "\n  Inserting...\n")
      (if Mins
	(entmake
	  (list	'(0 . "INSERT")
		(CONS '100 "AcDbMInsertBlock")
		(CONS '70 2)
		(CONS '71 2)
		(cons '2 nameb)
		(cons '10 pt)
	  )
	)
	(entmake
	  (list	'(0 . "INSERT")
		(cons '2 nameb)
		(cons '10 pt)
	  )
	)
      )
      (setq bc (entlast))
      (setq bac "back")
      (command "_.draworder" bc "" (strcat "_" bac))
      (setq ssetb nil)
      (setq viewsset nil)
    )
  )
  (princ)
)

(defun solidblk	(layoutName Mins)
  (if layoutName
    (cond
      ((= layoutName "14MS")
       (setq slist (list '(-4 . "<NOT")		     '(67 . 1)
			 '(-4 . "NOT>")		     '(-4 . "<OR")
			 '(0 . "SOLID")		     '(2 . "SOLID")
			 '(-4 . "OR>")
			)
       )
      )
      ((= layoutName "14PS")
       (setq slist (list '(67 . 1)
			 '(-4 . "<OR")
			 '(0 . "SOLID")
			 '(2 . "SOLID")
			 '(-4 . "OR>")
		   )
       )
      )
      (T
       (setq slist (list (cons 410 layoutName)
			 '(-4 . "<OR")
			 '(0 . "SOLID")
			 '(2 . "SOLID")
			 '(-4 . "OR>")
		   )
       )
      )
    )
    (setq slist	(list '(-4 . "<OR")
		      '(0 . "SOLID")
		      '(2 . "SOLID")
		      '(-4 . "OR>")
		)
    )
  )
  (setq ssets (ssget "X" slist))
  (if ssets
    (progn
      (setq pt (list 0.0 0.0))
      (entmake ;;write block header
	       (list '(0 . "BLOCK")
		     '(2 . "*anon")
		     '(70 . 1)
		     (cons '10 pt)
	       )
      )
      (setq a 0)
      (repeat (sslength ssets)
	(setq ent2 (entmake (entget (setq ent (ssname ssets a)))))
	(if (null ent2)
	  (princ (entget (setq ent (ssname ssets a))))
	)
	(if (assoc 66 (entget ent))
	  (progn
	    ;;add sub-entities until seqend is found
	    (setq subent (entnext ent))
	    (while (/= (cdr (assoc 0 (entget subent))) "SEQEND")
	      (entmake (entget subent))
	      (setq subent (entnext subent))
	    )
	    (setq ent3 (entmake (entget subent)))
	    (if	(null ent3)
	      (princ (entget subent))
	    )
	  )
	)
	(entdel ent)
	(setq a (1+ a))
	(c:spin "Making Block of solids..")
      )
      (setq names (entmake '((0 . "endblk"))))
      (princ "\n  Inserting...\n")
      (if Mins
	(entmake
	  (list	'(0 . "INSERT")
		(CONS '100 "AcDbMInsertBlock")
		(CONS '70 2)
		(CONS '71 2)
		(cons '2 names)
		(cons '10 pt)
	  )
	)
	(entmake
	  (list	'(0 . "INSERT")
		(cons '2 names)
		(cons '10 pt)
	  )
	)
      )
      (setq so (entlast))
      (setq ba "back")
      (command "_.draworder" so "" (strcat "_" ba))
      (setq ssets nil)
    )
  )
  (princ)
)

(defun anonBlock (layoutName Mins)
  (if layoutName
    (cond
      ((= layoutName "14MS")
       (setq alist (list '(-4 . "<NOT")
			 '(-4 . "<OR")
			 '(67 . 1)
			 '(0 . "ACAD_PROXY_ENTITY")
			 '(0 . "AEC_*")
			 '(0 . "AECS_*")
			 '(0 . "RTEXT")
			 '(0 . "WIPEOUT")
			 ;;'(8 . "LAYCFG")
			 '
			  (0 . "SOLID")
			 '(2 . "SOLID")
			 (cons 8 (getlayers))
			 '(62 . 8)
			 '(62 . 9)
			 '(62 . 251)
			 '(62 . 252)
			 '(62 . 253)
			 '(62 . 254)
			 '(62 . 255)
			 '(-4 . "OR>")
			 '(-4 . "NOT>")
		   )
       )
      )
      ((= layoutName "14PS")
       (setq alist (list '(67 . 1)
			 '(-4 . "<NOT")
			 '(-4 . "<OR")
			 '(0 . "VIEWPORT")
			 '(0 . "ACAD_PROXY_ENTITY")
			 '(0 . "AEC_*")
			 '(0 . "AECS_*")
			 '(0 . "RTEXT")
			 '(0 . "WIPEOUT")
			 ;;'(8 . "LAYCFG")
			 '
			  (0 . "SOLID")
			 '(2 . "SOLID")
			 (cons 8 (getlayers))
			 '(62 . 8)
			 '(62 . 9)
			 '(62 . 251)
			 '(62 . 252)
			 '(62 . 253)
			 '(62 . 254)
			 '(62 . 255)
			 '(-4 . "OR>")
			 '(-4 . "NOT>")
		   )
       )
      )
      (T
       (setq alist (list (cons 410 layoutName)
			 '(-4 . "<NOT")
			 '(-4 . "<OR")
			 ;;'(8 . "LAYCFG")
			 '
			  (0 . "VIEWPORT")
			 '(0 . "ACAD_PROXY_ENTITY")
			 '(0 . "AECC_*")
			 '(0 . "AEC_*")
			 '(0 . "AECS_*")
			 '(0 . "RTEXT")
			 '(0 . "WIPEOUT")
			 '(0 . "SOLID")
			 '(2 . "SOLID")
			 (cons 8 (getlayers))
			 '(62 . 8)
			 '(62 . 9)
			 '(62 . 251)
			 '(62 . 252)
			 '(62 . 253)
			 '(62 . 254)
			 '(62 . 255)
			 '(-4 . "OR>")
			 '(-4 . "NOT>")
		   )
       )
      )
    )
    (setq alist	(list '(-4 . "<NOT")
		      '(-4 . "<OR")
		      ;;'(8 . "LAYCFG")
		      '
		       (0 . "VIEWPORT")
		      '(0 . "ACAD_PROXY_ENTITY")
		      '(0 . "AECC_*")
		      '(0 . "AEC_*")
		      '(0 . "AECS_*")
		      '(0 . "RTEXT")
		      '(0 . "WIPEOUT")
		      '(0 . "SOLID")
		      '(2 . "SOLID")
		      (cons 8 (getlayers))
		      '(62 . 8)
		      '(62 . 9)
		      '(62 . 251)
		      '(62 . 252)
		      '(62 . 253)
		      '(62 . 254)
		      '(62 . 255)
		      '(-4 . "OR>")
		      '(-4 . "NOT>")
		)
    )
  )
  (setq sset (ssget "X" alist))
  (setq viewsset (ssget "X" '((0 . "VIEWPORT"))))
  (if viewsset
    (progn
      (setq n 0)
      (repeat (sslength viewsset)
	(if (setq clipent (assoc 340 (entget (ssname viewsset n))))
	  (ssdel (cdr clipent) sset)
	)
	(setq n (1+ n))
      )
    )
  )
  (if sset
    (progn
      (setq pt (list 0.0 0.0))
      (entmake ;;write block header
	       (list '(0 . "BLOCK")
		     '(2 . "*anon")
		     '(70 . 1)
		     (cons '10 pt)
	       )
      )
      (setq a 0)
      (repeat (sslength sset)
	(setq ent2 (entmake (entget (setq ent (ssname sset a)))))
	(if (null ent2)
	  (princ (entget (setq ent (ssname sset a))))
	)
	(if (assoc 66 (entget ent))
	  (progn
	    ;;add sub-entities until seqend is found
	    (setq subent (entnext ent))
	    (while (/= (cdr (assoc 0 (entget subent))) "SEQEND")
	      (entmake (entget subent))
	      (setq subent (entnext subent))
	    )
	    (setq ent3 (entmake (entget subent)))
	    (if	(null ent3)
	      (princ (entget subent))
	    )
	  )
	)
	(entdel ent)
	(setq a (1+ a))
	(c:spin "Making Block..")
      )
      (setq name (entmake '((0 . "endblk"))))
      (princ "\n  Inserting Block..\n")
      (if Mins
	;;Minsert block reference at insertion point
	(entmake
	  (list	'(0 . "INSERT")
		(CONS '100 "AcDbMInsertBlock")
		(CONS '70 2)
		(CONS '71 2)
		(cons '2 name)
		(cons '10 pt)
	  )
	)
	(entmake
	  (list	'(0 . "INSERT")
		(cons '2 name)
		(cons '10 pt)
	  )
	)
      )
      (setq sset nil)
      (setq viewsset nil)
    )
    (if	layoutName
      (princ (strcat "\nNo entities to lock in " layoutName))
    )
  )
  (princ)
)

(defun Finish (vers)
  (setvar "clayer" cla)
  (setvar "tilemode" space)
  (if (= vers 2)
    (command "-layer" "state" "restore" "lockup" "" "")
  )
  (command "-layer" "lock" "*" "")
  (setvar "proxyshow" 1)
  (command "regen")
  (cond
    ((= cont "Yes")
     (alert
       "\nPaper space only has been locked.
                                \nTo lock model space, run Lockup
                                \nagain and do NOT skip to paper space."
     )
    )
    ((= answer2 "Model")
     (alert "\nAll selected entities have been locked.")
    )
    ((= answer2 nil)
     (alert "\nAll selected entities have been locked.")
    )
  )
  (setq	cont nil
	answer2	nil
  )
  (princ "\n加密完成. ")
  (princ)
)
(defun goLock14PS ()
  (setvar "tilemode" 0)
  (proxy)
  (anonBlock "14PS" nil)		; make anon insert - on paper space
  (backblk "14PS" nil)			; make anon insert - on paper space
  (solidBlk "14PS" nil)			; make anon insert - on paper space
  (anonBlock "14PS" T)			; make anon minsert - on paper space
  (command "zoom" "extents")
  (prompt "\n  Paper Space has been locked.")
  (Finish 0)
)

(defun goLockPS	(vers)
  (if (= vers 0)
    (goLock14PS)
    (progn
      (princ "\nType in Layout Name to make current: ")
      (command "layout" "set" pause)	;type in whatever layout to set current
      (while (> (getvar "cmdactive") 0) (command pause))
      (proxy)
      (anonBlock (getvar "CTAB") nil)	; make anon insert in named layout
      (backblk (getvar "CTAB") nil)	; make anon insert in named layout
      (solidblk (getvar "CTAB") nil)	; make anon insert in named layout
      (anonBlock (getvar "CTAB") T)	; make anon minsert in named layout
      (command "zoom" "extents")
      (initget "Yes No")
      (prompt
	(strcat "\n  Layout " (getvar "ctab") " has been locked.")
      )
      (setq answer
	     (getkword "\nAre there more layouts to lock? Y/<N>: ")
      )
      (cond
	((or (null answer) (= answer "No"))
	 (Finish vers)
	)
	((= answer "Yes")
	 (goLockPS vers)
	)
	(T nil)
      )
    )
  )
)

(defun goLock (vers)
  (setvar "tilemode" 1)
  (if (= vers 2)
    (command "-layer" "state" "save" "lockup" "" "" "")
  )
  (command "-layer" "thaw" "*" "on" "*" "unlock" "*" "")
  (command "zoom" "extents")
  (proxy)
  (if (/= vers 0)
    (progn
      (anonBlock "Model" nil)		; make anon insert in model space
      (backblk "Model" nil)		; make anon insert in model space
      (solidblk "Model" nil)		; make anon insert in model space
      (anonBlock "Model" T)		; make anon minsert in model space
    )
    (progn
      (anonBlock "14MS" nil)
      (backblk "14MS" nil)
      (solidblk "14MS" nil)
      (anonBlock "14MS" T)
    )
  )
  (prompt "\n  Model Space has been locked.")
  (initget "Yes No")
  (setq	answer
	 (getkword "\nDo you want to lock Paper Space? Y/<N>: ")
  )
  (cond
    ((or (null answer) (= answer "No")) (Finish vers))
    ((= answer "Yes") (goLockPS vers))
    (T nil)
  )
)

(defun states ()
  (if (= vers 2)
    (command "-layer" "state" "save" "lockup" "" "" "")
  )
  (command "-layer" "thaw" "*" "on" "*" "unlock" "*" "")
  (command "graphscr")
  (command "zoom" "extents")
  (goLockps vers)
)

(defun continue	()
  (initget "Yes No")
  (setq	cont (getkword
	       "\nModel Space will not be locked! Continue? Y/<N>: "
	     )
  )
  (cond	((= cont "Yes") (states))
	((= cont "No") (skip))
	((= cont nil) (skip))
  )
)

(defun skip ()
  (initget "Skip Model")
  (setq	answer2
	 (getkword
	   "\nStart in Model Space or Skip to Paper Space? Skip/<Model>:"
	 )
  )
  (cond	((= answer2 "Skip") (continue))
	((= answer2 "Model") (goLock vers))
	((= answer2 nil) (goLock vers))
  )
)

(defun 14or2k (/ answer)
  (initget "14 2000 2000i")
  (setq	answer
	 (getkword
	   "\nWhat version of AutoCAD are you in? 14/2000<2000i>: "
	 )
  )
  (cond
    ((= answer "14") (setq vers 0))
    ((= answer "2000") (setq vers 1))
    ((= answer "2000i") (setq vers 2))
    ((= answer nil) (setq vers 2))
  )
  (skip)
)

(defun goexp ()
  (progn
    (repeat (sslength sset)
      (command "_explode" (ssname sset CNT))
      (setq CNT (1+ CNT))
      (c:spin "Exploding..")
    )
    (alert (strcat "\n    " (itoa CNT) " Entities Exploded."))
  )
  (setq sset nil)
  (princ)
)

(defun xpproxy (/ xpl)
  (alert
    "\n     Proxy Entities have been found.
    If they are not exploded, they will
  be omitted from the lockup process."
  )
  (initget "Yes No")
  (setq xpl (getkword "\nExplode Proxy Entities? Y/<N>: "))
  (if (or (= xpl "No") (= xpl nil))
    (princ)
  )
  (if (= xpl "Yes")
    (goexp)
  )
  (princ)
)

(defun goerase ()
  (progn
    (repeat (sslength wsset)
      (entdel (ssname wsset WCNT))
      (setq WCNT (1+ WCNT))
      (c:spin "Erasing..")
    )
    (alert (strcat "\n    " (itoa WCNT) " Wipeouts Erased."))
  )
  (setq wsset nil)
  (princ)
)

(defun goaskerase (/ del)
  (alert
    "\n     Wipeouts have been found."
  )
  (initget "Yes No")
  (setq del (getkword "\nErase Wipeouts? Y/<N>: "))
  (if (or (= del "No") (= del nil))
    (princ)
  )
  (if (= del "Yes")
    (goerase)
  )
  (princ)
)

(defun gowipeout (/ where wlist)
  (setq where (getvar "tilemode"))
  (setq cs 67)
  (if (= where 0)
    (setq sp 1)
  )
  (if (= where 1)
    (setq sp 0)
  )
  (setq	wlist (list (cons cs sp)
		    '(0 . "wipeout")
	      )
  )
  (setq WCNT 0)
  (setq wsset (ssget "x" wlist))
  (if (= wsset nil)
    (princ)
  )
  (if (not (= wsset nil))
    (goaskerase)
  )
  (princ)
)

(defun proxy (/ where plist)
  (setq where (getvar "tilemode"))
  (if (= where 0)
    (setq plist	'((-4 . "<NOT")
		  (67 . 0)
		  (-4 . "NOT>")
		  (-4 . "<OR")
		  (0 . "ACAD_PROXY_ENTITY")
		  (0 . "AECC_*")
		  (0 . "AEC_*")
		  (0 . "AECS_*")
		  (0 . "RTEXT")
		  (-4 . "OR>")
		 )
    )
  )
  (if (= where 1)
    (setq plist	'((-4 . "<NOT")
		  (67 . 1)
		  (-4 . "NOT>")
		  (-4 . "<OR")
		  (0 . "ACAD_PROXY_ENTITY")
		  (0 . "AECC_*")
		  (0 . "AEC_*")
		  (0 . "AECS_*")
		  (0 . "RTEXT")
		  (-4 . "OR>")
		 )
    )
  )
  (setq CNT 0)
  (setq sset (ssget "x" plist))
  (if (= sset nil)
    (princ)
  )
  (if (not (= sset nil))
    (xpproxy)
  )
  (gowipeout)
  (princ)
)

(defun c:undolock ()
  ;;Undo and Reset variables
  (setvar "cmdecho" 0)
  (princ "\nPlease wait while Lockup is undone.")
  (command "undo" "end")
  (command "undo" "back")
  (setvar "cmdecho" 1)
  (setvar "filedia" 1)
  (setvar "cmddia" 1)
  (setvar "clayer" cla)
  (princ "\nLockup has been undone.")
  (princ)
)

(defun c:look (/ alist CNT sset)
  (setq	alist '((-4 . "<OR")
		(0 . "ACAD_PROXY_ENTITY")
		(0 . "AECC_*")
		(0 . "AEC_*")
		(0 . "AECS_*")
		(0 . "RTEXT")
		(0 . "WIPEOUT")
		(-4 . "OR>")
	       )
  )
  (setq CNT 0)
  (if alist
    (progn
      (setq sset (ssget "X" alist))
      (if sset
	(repeat	(sslength sset)
	  (setq CNT (1+ CNT))
	)
      )
      (if (= CNT 1)
	(alert (strcat "\n        " (itoa CNT) " Entity found."))
      )
      (if (> CNT 1)
	(alert (strcat "\n       " (itoa CNT) " Entities found."))
      )
    )
  )
  (if (= sset nil)
    (alert "\nNo Entities were found.")
  )
  (princ)
)

(defun c:spin (wh)
  (prompt (strcat "\r  "
		  wh
		  (cond	((= sp "|") (setq sp "/"))
			((= sp "/") (setq sp "-"))
			((= sp "-") (setq sp "\\"))
			(T (setq sp "|"))
		  )
	  )
  )
  (princ)
)

(defun C:Lockup	(/ start answer)
  (setq	fdia	(getvar "filedia")
	cdia	(getvar "cmddia")
	cmd	(getvar "cmdecho")
	cla	(getvar "clayer")
	space	(getvar "tilemode")
	olderr	*error*
	*error*	lockerror
	cont	nil
	answer2	nil
  )
  (setvar "cmdecho" 0)
  (command "UNDO" "Begin")
  (setvar "filedia" 0)
  (setvar "cmddia" 0)
  (command "undo" "mark")
  (command "-layer" "make" "LOCKUP" "")
  (command "color" "bylayer")
  (setvar "proxyshow" 0)
  (command "regen")
  (initget "Yes No")
  (setq	answer
	 (getkword
	   "\n请确认作好了图纸备份！继续加密? Y/<N>: "
	 )
  )
  (cond
    ((or (= answer "No") (null answer))
     (Alert "LOCKUP aborted!")
    )
    ((= answer "Yes") (14or2k))
  )
  (command "UNDO" "End")
  (setq *error* olderr)
  (setvar "filedia" fdia)
  (setvar "cmddia" cdia)
  (setvar "cmdecho" cmd)
  (princ)
)
(princ)
;****************************************************BS多块同时缩放
(defun c:bs ()
  (command "_.undo" "_begin")
  (setq	old_err	*error*
	*error*	Sb_err
  )
  (setq blkname (getstring "\n请输入需缩放的块名称:"))
  (initget 7)
  (setq blkfactor (getreal "\n请输入缩放倍数:"))
  (setq blksset (ssget (list (cons 0 "INSERT") (cons 2 blkname))))
  (setq blksscnt (sslength blksset))
  (setq donecount 0)
  (while (> blksscnt 0)
    (setq temp (ssname blksset (setq blksscnt (1- blksscnt))))
    (setq templist (entget temp))
    (setq blkbasept (cdr (assoc 10 templist)))
    (command "scale" temp "" blkbasept blkfactor ^c)
    (setq donecount (1+ donecount))
  )
  (princ (strcat "\n完成缩放 "
		 (itoa donecount)
		 " 个名称为"
		 "\""
		 blkname
		 "\""
		 "的块."
	 )
  )
  (command "_.undo" "_end")
)

(defun Bs_err (s)
  (princ "\n命令中止!")
  (setq *error* old_err)
  (princ)
)

(princ)
;****************************************************NB块统计
(defun c:nb ()
 (setq st t)
 (while st
 (while  (not (setq st (entsel "\n选择需要统计的块:"))))
            (if  (= (cdr (assoc '0 (entget (car st)))) "INSERT")
                 (progn
                 (setq blockname (cdr (assoc '2 (entget (car st)))))
                 (setq st nil)
                 )
                 (princ "\n未选择到块!")
            )               
 )
 
 (princ (strcat "\n选择块" blockname "<全选>:"))
 (setq ss (ssget))
 (if (= ss nil) (setq ss (ssget "x")))
 (setq n 0 m 0)
 (while (and ss (< n (sslength ss)))
           (setq ssn (ssname ss n))
           (if (= (cdr (assoc '0 (entget ssn))) "INSERT")
               (progn
              (setq blockname1 (cdr (assoc '2 (entget ssn))))            
              (if (= blockname blockname1)
                  (setq m (+ m 1))
              )
              )
            )
            (setq n (+ n 1))
 )
 (alert  (strcat  blockname ":" (rtos m 2 0) "个"))
 (setq pt (getpoint "\n给定输出的点位<不输出>:"))
 (if pt
     (command "text" pt (getvar "textsize") "0"   (strcat  blockname " " (rtos m 2 0) "个"))
 ) 
 )
;****************************************************TTT合并单行文本
(defun update (mode txt el1 / ent el1)
  (setq ent (subst (cons mode txt) (assoc mode el1) el1))
  (entmod ent)
)
(defun C:ttt(/ ent1 el1 e1 txt1 ent2 el2 e2 txt2 txt tst ent)
(setvar "CMDECHO" 0)
(setq tst T)
(setq ent1 (car (entsel "\n请选择基准文本: ")))
(if (/= ent1 nil)
  (progn
    (setq el1 (entget ent1))
    (setq e1 (cdr (assoc -1 el1)))
    (if (= "TEXT" (cdr (assoc 0 el1)))
      (progn
        (while tst
          (setq txt1 (cdr (assoc 1 (entget e1))))
          (setq ent2 (car (entsel "\n请选择加入文本: ")))
          (if (/= ent2 nil)
            (progn
              (setq el2 (entget ent2))
              (setq e2 (cdr (assoc -1 el2)))
              (if (= "TEXT" (cdr (assoc 0 el2)))
                (progn
                  (setq txt2 (cdr (assoc 1 el2))) 
                    (command "erase" e2 "")
                    (setq txt (strcat txt1 txt2))
                    (update 1 txt el1)
                )
                (princ "\n你选择的不是单行文本 !")  
              )
            )
            (setq tst nil)
          )
        )
      )
      (princ "\n你选择的基准文本不是单行文本!")  
    )
  )
)
(redraw)
(princ)
)
;****************************************************TT合并文字
(defun c:tt()
  (command "osnap" "off")
  (setq kg1 (getint"\n合并字符间空格数0~10<0>:"))
  (if (= kg1 nil)(setq kg11 ""))
  (if (= kg1 0)(setq kg11 ""))
  (if (= kg1 2)(setq kg11 "  "))
  (if (= kg1 3)(setq kg11 "   "))
  (if (= kg1 4)(setq kg11 "    ")) 
  (if (= kg1 5)(setq kg11 "     "))
  (if (= kg1 6)(setq kg11 "      "))
  (if (= kg1 7)(setq kg11 "       "))
  (if (= kg1 8)(setq kg11 "        ")) 
  (if (= kg1 9)(setq kg11 "         "))    
  (if (= kg1 10)(setq kg11 "          "))
  (setq zzz "")
  (princ "\n选择字符串:")
  (setq s (ssget))
  (setq n (sslength s))
  (setq k 0 )(setq cgm 0)
  (setq fxx nil)
  (setq fyy nil)
  (setq fzz nil)
  (setq pxx1 nil)
  (setq pyy1 nil)

  (while (< k n)
        (setq name (ssname s k))
        (setq a (entget name))
        (if (= k 0) (progn
            (setq b (assoc '0 a))
            (setq b (cdr b))
            (setq h0 (assoc '40 a))
            (setq h0 (cdr h0))
            (setq ag1 (assoc '50 a))
            (setq ag1 (cdr ag1))
	    (setq ag1 (* ag1 180) ag1 (/ ag1 pi))
            ))
        (setq nam1 (assoc '-1 a))
        (setq nam1 (cdr nam1))
        (setq xxx (assoc '10 a))
        (setq xy (cdr xxx))
        (setq xx (car xy) yy (cdr xy) yy (car yy))
        (setq tx1 (assoc '1 a))
        (setq tx1 (cdr tx1))
        (setq k (+ k 1))

        (setq lxx (list xx tx1))
        (setq lyy (list yy tx1))
        (setq lxx (list lxx))
        (setq lyy (list lyy))
        (setq fxx (append fxx lxx))
        (setq fyy (append fyy lyy))
        (setq pxx (list xx) pyy (list yy))
        (setq pxx1 (append pxx1 pxx) pyy1 (append pyy1 pyy))
        (entdel nam1)
    )
    (setq pxx1 (vl-sort pxx1 '<))
    (setq pyy1 (vl-sort pyy1 '<))
    (setq px (car pxx1) py (car pyy1))
    (setq p1 (list px py))
    (if (= ag1 0)(progn
        (setq fzz (vl-sort fxx
                  (function (lambda (e1 e2)
                            (< (car e1) (car e2))))))
	))
    (if (> ag1 0)(progn
        (setq fzz (vl-sort fyy
                  (function (lambda (e1 e2)
                            (< (car e1) (car e2))))))
	))



		    
        (setq nn 0)
        (while (< nn k)
	     (setq zz1 (car fzz))
	     (setq zz1 (cdr zz1) zz1 (car zz1))
	     (setq zzz (strcat zzz kg11 zz1))
	     (setq fzz (cdr fzz))
	     (setq nn (+ nn 1))
        )
  (command "text" p1 h0 ag1 zzz)
  (command "osnap" "int,mid,nea,cen,per,tan")
)
;****************************************************TL字按线对齐
(setq *Nblock* 0)
(defun GetNestEntity(ELst / ent1 ss1 pt lst lst1 Obj lst2 ) 
  (setq ent1 (car ELst)
	pt   (cadr ELst)
	lst1 (entget ent1)
        Obj  (cdr (assoc 0 lst1))
	ss1  nil
	;n    0
  );;;end setq
  (if (or (wcmatch Obj "INSERT") (wcmatch Obj "LWPOLYLINE") (wcmatch Obj "POLYLINE"))
    (progn
      (command "_.explode" ent1)
      (setq *Nblock* (1+ *Nblock*))
      (if (setq ss1 (ssget pt))
        (setq lst2 (list (ssname ss1 0) pt)
              lst (GetNestEntity lst2))
	(progn
	  (command "_.undo" *Nblock*)
	  (setq *Nblock* 0)
	  (exit)
	)  
      ) 	
      (list (car lst) (1+ (cadr lst)))
    )
    (list ent1 0)
   
  );;;end if
  
 )
;;;////////////////////////
(defun C:TL(/ lst n ent txt objtype errhandler olderr elst lst1)
  (setq *Nblock* 0)
;;;//////////////////////
(defun errhandler(s)
  (if (/= s "Function cancelled")
       (if (= s "quit / exit abort")
           (princ)
           (princ (strcat "\nError: " s))
        )
   );;;end if
  ;(if (> n 0) (command "_.undo" n))
  (if olderr (setq *error* olderr))
  ;(princ ent)(princ)
  (if ent (command "_.erase" ent ""))
  (command "_.undo" "end")
  ;(if (> n 0) (command "_.undo" n))
);;;end defun 
;;;///////////////////////////
  (command "_.undo" "begin")
  (if *error*
    (setq olderr *error* *error* errhandler)
    (setq *error* errhandler))
  (setvar "cmdecho" 0)
  (setvar "errno" 7)
  (while (= (getvar "errno") 7)
    (setq lst (entsel "\nSelect Object [Line Or Arc]:"))
    (if lst
     (progn
       (setq lst1 (GetNestEntity lst)
             n    (last lst1)
	     ent  (car lst1)
	     elst (entget ent)
	     objtype (cdr (assoc 0 elst))
	     *Nblock* 0)
       (if (> n 0) (command "_.undo" n))
       (entmake elst)
       (setq ent (entlast))
       (if (or (wcmatch objtype "LINE")
	       (wcmatch objtype "ARC"))
	       ;(wcmatch objtype "CIRCLE"))
	(progn 
         (redraw ent 3)
         (setvar "errno" 7)
         (while (= (getvar "errno") 7)
          (setq txt (entsel "\nSelect text:"))
	  (if txt
	   (progn
	    (setq txt (car txt))
	    (if (wcmatch (cdr (assoc 0 (entget txt))) "TEXT")
	     (progn 
              (cond
	       ((wcmatch (cdr (assoc 0 (entget ent))) "LINE") (TextAlignToLine ent txt))
	       ((wcmatch (cdr (assoc 0 (entget ent))) "ARC") (TextAlignToArc ent txt))
	       ((wcmatch (cdr (assoc 0 (entget ent))) "CIRCLE") (alert "You pick a Circle"))
              );;;end cond
	      ;(setvar "errno" 0)
	    );;;end progn  
	   );;;end if
	  );;;progn 
          );;;end if
         );;;end while
         (setvar "errno" 0)
	 (entdel ent)
       )
       (progn
	 (command "_.erase" ent "")
         (setvar "errno" 7)
       );;;end progn
      );;;end if
     );;;end progn 
    );;;end if
  );;;end while
  (command "_.undo" "end")
  (princ)
)
;;;//////////////////
(defun TextAlignToLine(Line Text / LineTable PointStart PointEnd LineAngle TextTable)
  (setq LineTable  (entget Line)
	PointStart (cdr (assoc '10 LineTable))
	PointEnd   (cdr (assoc '11 LineTable))
        LineAngle  (angle PointStart PointEnd)
  )
   (if (or (> (* pi 1.5) LineAngle (* pi 0.5)) (= LineAngle (* pi 1.5)))
    (setq LineAngle (- LineAngle pi))
   );;;end if
  (setq TextTable (entget Text)
	TextTable (subst (cons '50 LineAngle) (assoc '50 TextTable) TextTable))
  (entmod TextTable)
  (setvar "errno" 7)
)
;;;///////////////////////
(defun TextAlignToArc(Arc Text / ArcTable Centerpoint TextTable TextBpt ang)
  (setq ArcTable    (entget Arc)
	Centerpoint (cdr (assoc 10 ArcTable))
	TextTable   (entget Text)
	TextBpt     (cdr (assoc 10 TextTable))
	ang         (+ (angle Centerpoint TextBpt) (/ pi 2))
  )
  ;(if (> ang (* 2 pi)) (setq ang (- ang (* 2 pi))))
  (if (or (> (* pi 1.5) ang (* pi 0.5)) (= ang (* pi 1.5)))
    (setq ang (- ang pi))
   );;;end if
  ;(command "_.line" Centerpoint TextBpt "")(princ)
  (setq TextTable  (subst (cons '50 ang) (assoc '50 TextTable) TextTable))
  (entmod TextTable)
  ;(alert "You pick an Arc")
  (setvar "errno" 7)
)
;****************************************************XT炸开文字
(Defun C:XT (/ lvl lul lvp lvs lss ViewPL)
(SetQ lvs (GetVar "viewsize")
lss (GetVar "screensize")
)
(SetVar "cmdecho" 0)
(Defun ViewPL ( / vi vw vh vc)
(setq vi (* lvs (/ (Car lss) (Cadr lss)))
vc (GetVar "viewctr")
vw (list (- (car vc) (* 0.5 vi))
(- (cadr vc) (* 0.5 lvs))
)
vh (list (+ (car vc) (* 0.5 vi))
(+ (cadr vc) (* 0.5 lvs))
)
)
(List vw vh)
)
(PrinC "\n要分解的文字行: ")
(SetQ ltl (SSGet)
lvl (ViewPL)
lul (List (Caar lvl) (Cadadr lvl))
lvp (GetVar "viewctr")
)
(Command "mirror" ltl "" lvp "@0,1" "y" 
"wmfout" "textb" ltl ""
"erase" ltl ""
"wmfin" "textb" lul "2" "" ""
"mirror" (EntLast) "" lvp "@0,1" "y"
"explode" (EntLast) 
"erase" (ssget "p") "R" "W"
(polar (car lvl) (* 0.25 Pi)
(Max (Abs (/ lvs (Cadr lss))) 
(Abs (/ (* lvs 
(/ (Car lss) (Cadr lss))
) 
(Car lss)
)
)
)
) 
(cadr lvl)
""
)
(SetVar "cmdecho" 1)(PrinC)
)
;****************************************************DX改大小写
(defun c:dx ( / oldblp oldech olderr p dx L )
  (setq oldblp (getvar "blipmode")
        oldech (getvar "cmdecho")
        olderr *error*
  )
  (setvar "blipmode" 0)
  (setvar "cmdecho" 0)
  (defun *error* (msg)
    (princ "\n")
    (princ msg)
    (setvar "blipmode" oldblp)
    (setvar "cmdecho" oldech)
    (setq *error* olderr)
    (princ)
  )
  (prompt "\n请选择要改变的字符串.")
  (setq P (ssget))
  (initget 1 "D X")
  (setq dx (getkword"\n改成: [大写(D)/小写(X)]"))
  (setq L 0 m 0 n (sslength p))
  (while (< L n)
    (setq q (ssname p l))
    (if (= "TEXT" (cdr (assoc 0 (setq e (entget (ssname p l))))))
      (progn
        (if (= "X" dx)
          (progn
            (setq w1 (strcase (cdr (setq b (assoc 1 e))) T))
            (setq e (subst (cons 1 w1) b e))
            (entmod e)
            (setq m (+ 1 m))
          )
        )
        (if (= "D" dx)
          (progn
            (setq w1 (strcase (cdr (setq b (assoc 1 e)))))
            (setq e (subst (cons 1 w1) b e))
            (entmod e)
            (setq m (+ 1 m))
          )
        )
      )
    )
    (setq l (+ 1 l))
  )
  (if (= 0 m)
    (progn
      (princ "\n\t  没有任何被选中")
      (princ)
    )
  )
  (setvar "blipmode" oldblp)
  (setvar "cmdecho" oldech)
  (setq *error* olderr)
  (princ)
)
;****************************************************LL将所选对象的层变为当前层
(DEFUN C:LL( / e n)
(setq e (car (entsel "请选择对象，该对象所在层将变为当前层:")))
(if e (progn 
(setq e (entget e))
(setq n (cdr (assoc 8 e)))
(command"layer" "set" n "")
))
)
;****************************************************LLI只显示被选对象所在层
(DEFUN C:lli (/ ES EN EL A)
 (princ "请选择对象，未被选中的对象所在的层将被关闭")
 (setq ES (ssget) A 0 EN "" EL nil FL nil)
 (while (/= EN nil)
 (setq EN (ssname ES A) EL (cons EN EL) A (1+ A)))
 (setq EL (cdr EL) FL (cdr (assoc ' 8 (entget (car EL)))) EL (cdr EL))
 (repeat (- A 2)
 (setq EN (cdr (assoc ' 8 (entget (car EL))))
  FL (strcat EN "," FL) EL (cdr EL)) )
 (command "LAYER" "off" "*" "y" "on" (eval FL) "")
(princ))
;****************************************************LK快速改对象的层
(DEFUN C:LK()
(princ "请选择要改变层的对象\n")
(setq ss (ssget))
(if (and ss (> (sslength ss) 0))
(progn 
(setq ent (entsel "\n请选择目标层上的对象:"))
(if ent (setq la (cdr(assoc 8 (entget (car ent)))))
(setq la (getvar "clayer"))
)
(command ".chprop" ss "" "layer" la "")
)
)
(princ)
)
;****************************************************LJ解锁图层
(defun C:LJ (/ ES EN EL A)
       (princ "请选择要解锁的图层上的对象")
       (setq ES (ssget) A 0 EN "" EL nil FL nil)
       (while (/= EN nil)
       (setq EN (ssname ES A) EL (cons EN EL) A (1+ A)))
       (setq EL (cdr EL) FL (cdr (assoc ' 8 (entget (car EL)))) EL (cdr EL))
       (repeat (- A 2)
       (setq EN (cdr (assoc ' 8 (entget (car EL))))
       FL (strcat EN "," FL) EL (cdr EL)) )
       (command "LAYER" "U" (eval FL) "")
(princ))
;****************************************************Q1关闭所选物体所在的层
(DEFUN  C:Q1 ()
  (setvar "cmdecho" 0)
  (prompt"\n请选择要关闭的图层上的对象")
  (setq ss (ssget))
  (if (and ss (sslength ss) 0)
    (progn 
     (setq ct 0 len (sslength ss) cl (getvar "clayer"))
     (command ".layer")
     (while (< ct len)
         (setq la (cdr (assoc 8 (entget (ssname ss ct)))))
         (if (/= cl la)(command "off" la)
                       (progn (prompt "\n你选择的层:")
                              (prompt la)
                              (prompt " 是当前层，不能关闭")
                       )  ;end of progn
         )                ;end of if
         (if (= old nil)(setq OLD la)(setq OLD (strcat OLD "," la)))
         (setq ct (1+ ct))
       )                  ;end of while
       (command"")
     )                  ;end of progn
 )                      ;end of if
 (princ)
 (setvar "cmdecho" 0) (prin1)
)
;****************************************************Q2冻结所选物体所在的层
(defun C:Q2 (/ ES EN EL A)
 (princ "请选择要冻结的图层上的对象.")
 (setq ES (ssget) A 0 EN "" EL nil FL nil)
 (while (/= EN nil)
 (setq EN (ssname ES A) EL (cons EN EL) A (1+ A)))
 (setq EL (cdr EL) FL (cdr (assoc ' 8 (entget (car EL)))) EL (cdr EL))
 (repeat (- A 2)
 (setq EN (cdr (assoc ' 8 (entget (car EL))))
  FL (strcat EN "," FL) EL (cdr EL)) )
 (command "LAYER" "F" (eval FL) "")
(princ))
;****************************************************Q3锁定所选物体所在的层
(defun C:Q3 (/ ES EN EL A)
 (princ "请选择要加锁的图层上的对象.")
 (setq ES (ssget) A 0 EN "" EL nil FL nil)
 (while (/= EN nil)
 (setq EN (ssname ES A) EL (cons EN EL) A (1+ A)))
 (setq EL (cdr EL) FL (cdr (assoc ' 8 (entget (car EL)))) EL (cdr EL))
 (repeat (- A 2)
 (setq EN (cdr (assoc ' 8 (entget (car EL))))
  FL (strcat EN "," FL) EL (cdr EL)) )
 (command "LAYER" "LO" (eval FL) "")
(princ))
;****************************************************W1显示全部层
(DEFUN C:W1 ()
       (command "layer" "on" "*" "")
(princ))
;****************************************************W2解冻全部层
(DEFUN C:W2 ()
        (COMMAND "LAYER" "THAW" "*" "")
    (PRINC)
)
;****************************************************W3解锁全部层
(DEFUN C:W3 ()
        (COMMAND "LAYER" "U" "*" "")
    (PRINC)
)
;****************************************************123显示+解锁+解冻全部层
(DEFUN C:W123 ()
        (command "layer" "on" "*" "")
        (COMMAND "LAYER" "THAW" "*" "")
        (COMMAND "LAYER" "U" "*" "")
    (PRINC)
)
(princ "***请输入 HH 查看命令列表***")
