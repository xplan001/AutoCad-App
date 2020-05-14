
;;;变实线
(DEFUN C:Con ()
        (SETVAR "CMDECHO" 0)
        (PRINC "SELECT Obj. be ContinuouS: ")
        (SETQ SS (SSGET))
        (COMMAND "CHPROP" SS "" "LT" "CONTINUOUS" "") (SETVAR "CMDECHO" 1) (PRINC)) 


;;;变点化线
(DEFUN C:Cen ()
        (SETVAR "CMDECHO" 0)
        (PRINC "SELECT Obj. be CEnter: ")
        (SETQ SS (SSGET))
        (COMMAND "CHPROP" SS "" "LT" "CENTER" "") (SETVAR "CMDECHO" 1) (PRINC)) 
;;;变虚线
(DEFUN C:Das ()
        (princ "select object...\n")
        (setq ss (ssget))
        (command "chprop" ss "" "lt" "dashed" "") (princ)) 

;;;双向偏移
(defun c:db( / en m n pon ename et ppp ouse )
  (setvar "cmdecho" 1)
  (COMMAND "OFFSET" PAUSE "")
  (prompt "\n Select Objects to Offset: ")
  (setq en (ssget))
  (setq m (sslength en ))
  (setq n 0)
  (setvar "cmdecho" 0)
  (repeat m 
    (setq ename (ssname en n))
    (setq et  (entget ename))
    (setq ppp (trans (cdr (assoc 10 et)) 0 1))
    (setq ouse (list ename ppp))
    (setq pon  (trans (list (+ (car ppp) 1001) (- (cadr ppp) 1000) ) 0 1 ))
    (command "offset" "" ouse pon "")
    (setq ouse (list (entlast) (trans (cdr (assoc 10 (entget (entlast)))) 0 1)))
    (command "offset" (* (getvar "offsetdist") 2 )  ouse ppp "")
    (command "offset" (/ (getvar "offsetdist") 2 ) "")
    (setq n (+ n 1))
  )
)


;;;多重复制
(defun c:c()
(setq o1 (ssget))
(command "copy" o1 "" "m"))
;;;缩小一半
(defun c:k()
(command "_.scale" (ssget) "" (getpoint "\n选择基点进行0.5倍缩放:") 0.5 "")
)
;;;放大一倍
(defun c:kK()
(command "_.scale" (ssget) "" (getpoint "\n选择基点进行0.5倍缩放:") 2.0 "")
)
;;;移动上一个选择集
(defun c:dd() (princ "\n移动上一个选择集")(command "_.move" "p" ""))
(defun c:cd() (princ "\n复制上一个选择集")(command "_.copy" "p" ""))
(defun c:ed() (princ "\n删除上一个选择集")(command "_.erase" "p" ""))

;;;交点打断
(defun c:br ()
(command) 
(princ "\n选择要切断的物体:")
(while (setq mid (entsel)) 
(setq old-osmode (getvar "osmode"))
(command "_.break"
   mid
   "F"
   (setq pt1 (getpoint "\n输入切断点:"))
   (progn (setvar "osmode" 0) pt1)
)
(setvar "osmode" old-osmode)
)
)

;;;图中实体所见即所得
(defun c:zz (/		       YJWSH_CENTERPOINT
		YJWSH_CHANGSCALE       YJWSH_CHANGSCALE_H
		YJWSH_PLOTSCALE	       YJWSH_SCREENHIGN
		YJWSH_SCREENSIZE_Y     YJWSH_VIEWCTR
		YJWSH_VIEWCTR_NEW      YJWSH_VIEWSIZE
	       )
  (princ
    (strcat "\n*** 图中实体所见即所得软件V050308 ***")
  ) ;_ 结束princ
  (princ (strcat "\n       [它山之石图形工作室作品]"))
  (princ)

  (setvar "cmdecho" 0)
  (princ (strcat "\n请选择视图中心点<退出>"))
  (if (setq YJWSh_centerpoint
	     (getpoint)
      ) ;_ 结束setq
    (progn
;;;以下得到当前视口中视图的中心点
      (setq YJWSh_viewctr (getvar "viewctr"))
;;;以下将新得到的视图的中心点移到原来的中心点
      (command "pan" YJWSh_centerpoint YJWSh_viewctr)
;;;以下得到当前视口的视图高度
      (setq YJWSh_viewsize (getvar "viewsize"))
;;;以下得到以像素为单位存储当前视口的大小（X 和 Y 值）
      (setq YJWSh_screensize_y (cadr (getvar "screensize")))
;;;240(毫米)为用尺子量出的17寸显示器的可视高度，分辩率设为1024X768。
      (setq YJWSh_screenhign (* 240 (/ YJWSh_screensize_y 768)))
;;;以下得到出图比例,也可以通过DIMSCALE变量来设定
      (setq YJWSh_plotscale 100)

      (setq YJWSh_changscale_h
	     (/	(/ YJWSh_viewsize YJWSh_screenhign)
		YJWSh_plotscale
	     ) ;_ 结束/
      ) ;_ 结束setq
      (setq YJWSh_changscale (strcat (rtos YJWSh_changscale_h 2 8) "x"))
      (command "zoom" YJWSh_changscale)
    ) ;_ 结束progn
  ) ;_ 结束if
  (princ
    (strcat "\n*** 图中实体所见即所得软件V050308 ***")
  ) ;_ 结束princ
  (princ (strcat "\n       [它山之石图形工作室作品]"))
  (princ)
) ;_ 结束defun


;;;刷文字
(defun c:aa ( / content name_old text_new)  
(repeat 1000
(setq content (cdr(assoc 1 (entget(car(entsel"\n 请选择源文字:"))))))
(setq name_old(car(entsel"\n 请选择目标文字:")))
(setq text_new(entget name_old))
(setq text_new(subst (cons '1 content) (assoc 1 text_new) text_new))
(entmake text_new)
(entdel name_old)
(princ)
)
)

;;;介入当前涂层
(defun c:cv()
(setq cmd(getvar "cmdecho"))
(setvar "cmdecho" 0)
(setq ss(entsel "\nSelect Dimension:"))
(setq la(cdr(assoc 8(entget(car ss)))))
(setq lb(cdr(assoc 3(entget(car ss)))))
(setvar "clayer" la)
(command "dimstyle" "r" lb)
(setvar "cmdecho" 0)
)


;;;超级剪切
(defun c:J (/ PT0 PTLIST PTLIST0 ss CMDECHO OSMODE)
  (setq	cmdecho	(getvar "cmdecho")
	osmode	(getvar "osmode")
  )
  (while (and (setq ptlist0 (getpoint_list))
	      (> (length ptlist0) 1)
	 )
    (setvar "cmdecho" 0)
    (setvar "osmode" 0)
    (setq pt0	 (car ptlist0)
	  ptlist (cdr ptlist0)
    )
    (command "trim" "")
    (foreach pt	ptlist
      (command "f" pt0 pt "")
      (setq pt0 pt)
    )
    (command "")
    (if	(setq ss (ssget "f" ptlist0))
      (command "erase" ss "")
    )
    (setvar "osmode" osmode)
    (setvar "cmdecho" cmdecho)
  )
  (princ)
)
(defun getpoint_list ( / DIS OUT PT)
  (setq pt (getpoint "\n开始："))
  (princ "\n按任意键完成:")
  (if (= (type pt) 'LIST)
    (progn
      (setq out (list pt)
	    dis (* 0.01 (getvar "viewsize")))
      (while (= 5 (car (setq pt (grread t 4 0))))
	(setq pt (cadr pt))
         (if (> (distance pt (car out)) dis)
	   (progn
	     (grdraw pt (car out) 1)
	     (setq out (cons pt out))
	     )
	   )
	)
      )
    )
  (redraw)
  (reverse out)
  )



;;;选择易
(defun c:ss ( / slent f dcl_name lst2 lst1 color tmp index_value flag entl lst3 lst4 code ktmp klst lst4
	        hand fjflt filter kl_pre ;;原为全局变量，样板实体空选时按上次过滤表进行选择，现用ss_saved_lst保存
	        ss fjlst ssl attdis strtmp) ;;ss_saved_lst作为全局变量保存上次选择的变量值，格式：(hand fjflt filter kl_pre)
  (setq attdis "Y" kl_pre (last ss_saved_lst) slent "N")
  (while (= slent "N")
      (initget "N")
      (setq slent (entsel (strcat "\n请选择样板实体(N-关闭块属性显示,当前状态：" (if (= "Y" attdis) "打开" "关闭") "):" )))
      (if (= slent "N") (setq attdis "N") (setq slent (car slent)))
  );;while
 (if slent  ;;;----------------------1
   (progn  ;;;-----------------------1
       (setq fjflt nil filter nil entl (entget slent))     
       (setq lst2 '( 
("通用" ((0 "实体类型") (6 "实体线型") (8 "所在图层") (48 "线型比例") (62 "实体颜色" ((256 "随层") (0 "随块") (1 "红色") (2 "黄色") (3 "绿色") 
         (4 "青色") (5 "蓝色") (6 "紫色") (7 "黑白"))) (370 "实体线宽")))
("ARC" ((-4 "圆弧") (10 "圆心坐标") (40 "圆弧半径") (39 "实体厚度") (50 "起点角度") (51 "终点角度")) ("FJ" ("FJ1" "圆弧长度" (len slent))))
("CIRCLE" ((-4 "圆形") (10 "圆心坐标") (40 "圆形半径") (39 "实体厚度")))
("SOLID" ((-4 "SOLID") (39 "实体厚度")))
("POINT" ((-4 "点") (10 "点的位置") (39 "实体厚度") (50 "旋转角度")))
("LINE" ((-4 "直线段") (10 "起点坐标") (11 "终点坐标") (39 "实体厚度")) ("FJ" ("FJ1" "线段长度" (len slent)) 
         ("FJ2" "线段角度" (REM (ATOF (ANGTOS (ANGLE (DXF 10 slent) (DXF 11 slent)))) 180))))
("ELLIPSE" ((-4 "椭圆") (10 "椭圆中心") (11 "长轴端点") (40 "长短轴比") (41 "开始参数") (42 "结束参数")))
("INSERT" ((-4 "图块") (10 "图块位置") (2 "图块名称") (41 "X 轴比例") (42 "Y 轴比例") (43 "Z 轴比例") (50 "旋转角度"))) 
           ;("FJ" ("FJ1" "属性标志" (car (attstr slent))) ("FJ2" "属性数值" (cadr (attstr slent)))))
("LWPOLYLINE" ((-4 "轻多义线") (38 "复线标高") (43 "固定宽度") (90 "顶点个数") (39 "复线厚度") (70 "是否闭合" ((0 "不闭合") (1 "闭合")))) 
               ("FJ" ("FJ1" "曲线长度" (len slent))))
("POLYLINE" ((-4 "重多义线") (70 "是否闭合" ((0 "不闭合") (1 "闭合")))) ("FJ" ("FJ1" "曲线长度" (len slent ))))
("HATCH" ((-4 "图案填充") (2 "填充图案") (41 "填充比例") (52 "填充角度") (71 "边界关联" ((0 "不关联") (1 "关联"))) 
          (76 "图案类型" ((0 "用户定义") (1 "预定义") (2 "自定义")))))
("TEXT" ((-4 "文字") (1 "文字内容") (7 "文字样式") (10 "插入位置") (40 "文字高度") (41 "宽度系数") (50 "旋转角度") (51 "倾斜角度") 
         (71 "文字镜像" ((0 "默认") (2 "文字反向") (4 "文字倒置") (6 "反向倒置"))) 
         (72 "水平对齐" ((0 "左对齐") (1 "居中对齐") (2 "右对齐") (3 "对齐") (4 "中间") (5 "拟合"))) 
         (73 "垂直对齐" ((0 "基线对齐") (1 "底端对齐") (2 "居中对齐") (3 "顶端对齐")))) ("FJ" ("FJ1" "文字数值" (ATOF (DXF 1 slent)))))
("ATTDEF" ((-4 "属性定义") (2 "属性标记") (7 "字型样式") (10 "插入位置") (40 "文字高度") (50 "旋转角度") (51 "倾斜角度") 
           (71 "文字镜像" ((0 "默认") (2 "文字反向") (4 "文字倒置") (6 "反向倒置"))) 
           (72 "水平对齐" ((0 "左对齐") (1 "居中对齐") (2 "右对齐") (3 "对齐") (4 "中间") (5 "拟合"))) 
           (73 "垂直对齐" ((0 "基线对齐") (1 "底端对齐") (2 "居中对齐") (3 "顶端对齐")))) ("FJ" ("FJ1" "标记数值" (ATOF (DXF 2 slent)))))
("MTEXT" ((-4 "多行文字") (10 "插入位置") (1 "文字内容") (7 "文字样式") (40 "文字高度") (50 "旋转角度")))
("SPLINE" ((-4 "样条曲线") (70 "曲线标志") (71 "曲线阶数") (72 "节点数量") (73 "控制点数") (74 "拟合点数") 
           (42 "节点公差") (43 "控点公差") (44 "拟合公差")) ("FJ" ("FJ1" "曲线长度" (len slent))))
("DIMENSION" ((-4 "尺寸标注") (1 "标注文字") (42 "测量值") (3 "标注样式") (70 "标注类型" ((32 "水平垂直") (33 "对齐标注") (34 "角度标注") 
              (35 "直径标注") (36 "半径标注") (37 "三点角度") (38 "坐标标注")))))
) 
);;setq lst2 
  (if (and (= attdis "Y") (= "INSERT" (dxf 0 slent))) (kldc_1)) ;;对块实体，增加属性过滤表，slent及lst2作为全局变量传递
  (setq lst3 (car (dxf "通用" lst2)) 
    lst5 (dxf (dxf 0 entl) lst2) 
    lst4 (car lst5) 
    lst5 (cadr lst5) 
  ) 
  (foreach tmp lst3 
    (if (and (not (dxf (car tmp) entl)) (/= (car tmp) 62)) (setq lst3 (vl-remove tmp lst3))) 
  );;foreach 
  (setq dcl_name (strcat (getenv "temp") "\\sel" ".dcl") 
        f (OPEN dcl_name "w")) 
  (write-line "sl:dialog{label=\"我的选择易--By 小菜\";" f) 
  (write-line ":column{" f) 
  (write-line ":boxed_column{label=\"过滤条件\";" f) 
  (write-line ":boxed_column{label=\"通用\";" f) 
  (foreach tmp lst3 
     (write-line ":row{fixed_width=true;" f) 
     (write-line (strcat ":toggle{key=\"" (itoa (car tmp)) "\";label=\"" (cadr tmp) "\";width=12;}") f) 
     (write-line (strcat ":popup_list{edit_width=5;key=\"pop" (itoa (car tmp)) "\";}") f) 
     (setq ktmp (list (strcat "pop" (itoa (car tmp))) (itoa (car tmp)))) 
    (if  (/= 62 (car tmp)) 
      (progn 
      (setq ktmp (write f ktmp (car tmp) (vl-princ-to-string (dxf (car tmp) entl)) "txt" "16")) 
      (if (= 48 (car tmp)) 
        (setq ktmp (write f ktmp 48 "容差" "txta" "7")) 
      );;if 
       );;progn 
       (progn 
      (setq color (dxf 62 entl))  (if (not color) (setq color 256)) 
     (setq ktmp (write f ktmp 62 (itoa color) "txt" "16")) 
         (write-line (strcat ":edit_box{value=\"" (vl-princ-to-string (car (dxf color (caddr tmp)))) "\";edit_width=7 ;allow_accept=true;}") f) 
    );progn 
      );;if 
     (write-line "}" f) 
    (setq klst (cons (reverse ktmp) klst)) 
  );;foreach 
  (write-line "}" f) 
  (write-line (strcat ":boxed_column{label=\"" (vl-princ-to-string (car (dxf -4 lst4))) "\";") f) 
  (setq lst4 (cdr lst4)) ;;去掉前面的-4组码 
  (foreach tmp lst4 
    (setq code (car tmp) ktmp nil) 
    (if (dxf code entl) (progn 
    (write-line ":row{fixed_width=true;" f) 
    (setq ktmp (list (strcat "pop" (itoa code)) (itoa code))) 
    (write-line (strcat ":toggle{key=\"" (itoa code) "\";label=\"" (vl-princ-to-string (cadr tmp)) "\";width=12;}") f) 
    (write-line (strcat ":popup_list{edit_width=5;key=\"pop" (itoa code) "\";}") f) 
    (cond ((or (= code 10) (= code 11)) 
       (setq ktmp (write f ktmp code (vl-princ-to-string (car (dxf code entl))) "txt_x" "6.5")) 
       (setq ktmp (write f ktmp code (vl-princ-to-string (cadr (dxf code entl))) "txt_y" "6")) 
       (setq ktmp (write f ktmp code (vl-princ-to-string (caddr (dxf code entl))) "txt_z" "7")) 
      ) 
      ((member code '(1 2 3 7 90 38 39 40 41 42 43 44 50 51 52 70 71 72 73 74 76)) 
       (setq strtmp (vl-princ-to-string (dxf code entl)))
       (if (= code 1) 
	   (foreach tmp '("\r\n" "\\P" "\\")
               (while (vl-string-search tmp strtmp) (setq strtmp (vl-string-subst " " tmp strtmp)))
           );;foreach ;;;;消除acad2005中的mtext中的换行符(shift+enter)导致对话框不正常
	);;end if code=1
       (setq ktmp (write f ktmp code strtmp "txt" "16")) ;原strtmp=(vl-princ-to-string (dxf code entl))
         (cond ((member code '(38 39 40 41 42 43 44 50 51 52)) 
            (setq ktmp (write f ktmp code "容差" "txta" "7")) 
               ) 
           ((member code '(70 71 72 73 74 76)) 
            (if (car (dxf (dxf code entl) (cadr (dxf code lst4))))
(write-line (strcat ":edit_box{value=\"" (vl-princ-to-string (car (dxf (dxf code entl) (cadr (dxf code lst4))))) "\";edit_width=7;allow_accept=true;}") f) 
            );if 
           ) 
         );;cond 
      ) 
    );;cond 
    (write-line "}" f) 
    ));;progn & if 
    (if ktmp (setq klst (cons (reverse ktmp) klst))) 
  );;foreach 
  (write-line "}" f) 
  (if lst5 (progn (setq lst5 (cdr lst5) ) ;;去掉lst5第一个元素"FJ" 
    (write-line ":boxed_column{label=\"附加过滤\";" f) 
    (foreach tmp lst5 
       (write-line ":row{fixed_width=true;" f) 
       (write-line (strcat ":toggle{key=\"" (car tmp) "\";label=\"" (cadr tmp) "\";width=12;}") f) 
       (write-line (strcat ":popup_list{edit_width=5;key=\"pop" (car tmp) "\";}") f) 
       (setq ktmp (list (strcat "pop" (car tmp)) (car tmp))) 
       (setq ktmp (write f ktmp (car tmp) (vl-princ-to-string (eval (caddr tmp))) "txt" "16")) 
       (setq ktmp (write f ktmp (car tmp) "容差" "txta" "7")) 
       (setq fjlst (cons (reverse ktmp) fjlst))  ;;fjlst是附加过滤条件的变量表 
       (write-line "}" f) 
    );;foreach 
    (write-line "}" f) 
  ));if lst5 
  (write-line "}:row{:boxed_radio_row{label=\"过滤范围\";" f) 
  (write-line ":radio_button{label=\"手选\";key=\"hand\";value=\"1\";}" f) 
  (write-line ":radio_button{label=\"预选\";key=\"pre\";}" f) 
  (write-line ":radio_button{label=\"全图\";key=\"all\";}" f) 
  (write-line "}}:row{ok_cancel;}}}" f) 
  (close f) 
  (setq klst (reverse klst)) 
  (setq index_value (load_dialog dcl_name));_加载dcl文件 
  (new_dialog "sl" index_value);_开始新对话框 
  (foreach tmp klst  ;;klst为变量表，第三项开始含有变量名及初始值 
    ;;如：'(("0" "pop0" ("txt0" "INSERT")) ("8" "pop8" ("txt8" "_消防报警")) ("62" "pop62" ("txt62" "256")) 
    ;;("10" "pop10" ("txt_x10" "3431.58") ("txt_y10" "-17355.0") ("txt_z10" "0.0")) ("2" "pop2" ("txt2" "RXF008")) 
    ;;("41" "pop41" ("txt41" "-64.0") ("txta41" "容?.. 
    (cond ((member (car tmp) '("0" "1" "2" "3" "6" "7" "8")) (show_list (cadr tmp) '("=" "<>"))) 
      ((member (car tmp) '("10" "11" "38" "39" "40" "41" "42" "43" "44" "48" "50" "51" "52")) (show_list (cadr tmp) '("=" "<" ">" "<=" ">=" "<>"))) 
      ((member (car tmp) '("62" "70" "71" "72" "73" "74" "76" "90")) (show_list (cadr tmp) '("=" "<" ">" "<=" ">=" "<>" "&" "&="))) 
    );;cond 
  );;foreach 显示下拉选单信息 
  (if fjlst (foreach tmp fjlst (show_list (cadr tmp) '("=" "<" ">" "<=" ">=" "<>"))));;;;end if fjlst;显示附加过滤下拉选单信息 
        ;;;;fjlst是附加过滤条件的变量表,如：'(("FJ3" "popFJ3" ("txtFJ3" "0.0") ("txtaFJ3" "容差")) ("FJ2" "popFJ2" ("txtFJ2" "0.0") 
                                            ;;("txtaFJ2" "容差")) ("FJ1" "popFJ1" ("txtFJ1" "0.0") ("txtaFJ1" "容差"))) 
  (if kl_pre 
    (foreach tmp (cdr kl_pre) 
        (if (= (dxf 0 entl) (car kl_pre)) 
            (set_tile (car tmp) "1") 
            (if (member (car tmp) '("0" "6" "8" "48" "62" "370")) 
            (set_tile (car tmp) "1") 
        );;end if 
    );;end if 
    );;foreach 
  ) ;;把上次选中的复选框设为选中状态 
  (action_tile "accept" "(get_filter) (done_dialog 1)") 
  (setq flag (start_dialog)) 
  (unload_dialog index_value)
);;end progn ;;;-----------------------1
   (setq hand (car ss_saved_lst)
	 fjflt (cadr ss_saved_lst)
	 filter (caddr ss_saved_lst)
   );;setq
);;end if;;;;--------------------------1
  (if filter (progn (princ "\n使用过滤器：") 
           (princ filter) 
           (cond ((= hand "1") (setq ss (ssget filter))) 
                 ((= hand "2") (setq ss (ssget "p" filter))) 
                 ((= hand "3") (setq ss (ssget "x" filter))) 
           );;cond 
  ));;end if filter 
  (if (and (setq ssl (chsget ss)) fjflt) 
      (foreach slent ssl (if (not (eval fjflt)) (setq ss (ssdel slent ss)))) 
  );;end if
(setq ss_saved_lst (list hand fjflt filter kl_pre)) ;;保存至全局变量
(if ss (progn
	 (princ (strcat "\n共选中了" (itoa (sslength ss)) "个实体。"))
	 (if (= 0 (getvar "cmdactive")) (command "select" ss "" "pselect" ss ""))
       );;progn
       (princ "\n共选中了0个实体。")
);;if
  ss
);;defun 
(defun get_filter ( / tmp pop txt txt1 rc txt2 txt3 pop_1 pop_2 pop_3) 
  (cond ((= "1" (get_tile "hand")) (setq hand "1"))
	((= "1" (get_tile "pre")) (setq hand "2"))
        ((= "1" (get_tile "all")) (setq hand "3"))
  );;cond
  (foreach tmp klst (if (/= "1" (get_tile (car tmp))) (setq klst (vl-remove tmp klst)))) 
  (foreach tmp fjlst (if (/= "1" (get_tile (car tmp))) (setq fjlst (vl-remove tmp fjlst)))) 
  (setq kl_pre (append (list (dxf 0 entl)) klst fjlst)) ;;附加过滤选中的项下次使用也成为缺省选中 
  (foreach tmp klst 
    (setq pop (get_tile (cadr tmp))) 
    (cond ((member (car tmp) '("0" "1" "2" "3" "6" "7" "8"))
	   (setq txt (get_tile (caaddr tmp)) 
                 txt1 (cadr (caddr tmp)))
	   (if (= txt txt1) (setq txt (dxf (read (car tmp)) entl))) ;;如果(car tmp)对应的值未被用户修改过，取回原来的值 
           (cond ((= pop "0") ;(setq txt (get_tile (caaddr tmp)) 
                    (setq filter (append (cons '(-4 . "<OR") (cons (cons (read (car tmp)) txt) '((-4 . "OR>")))) filter) 
                    );;setq 
                 ) 
                ((= pop "1") ; (setq txt (get_tile (caaddr tmp)) 
                     (setq filter (append (cons '(-4 . "<NOT") (cons (cons (read (car tmp)) txt) '((-4 . "NOT>")))) filter) 
                     );;setq 
                ) 
            );;cond 
          );;end member 
      ((member (car tmp) '("62" "70" "71" "72" "73" "74" "76" "90")) 
          (setq txt (get_tile (caaddr tmp)) 
                        filter (append 
                                   (cons (cons -4 (nth (read pop) '("=" "<" ">" "<=" ">=" "<>" "&" "&="))) (list (cons (read (car tmp)) (read txt)))) 
                                  filter 
                          );;append 
              );;setq 
      ) 
      ((member (car tmp) '("38" "39" "40" "41" "42" "43" "44" "48" "50" "51" "52")) 
           (setq txt (get_tile (caaddr tmp)) 
             txt1 (cadr (caddr tmp)) 
             rc (read (get_tile (car (last tmp)))) 
        );;setq 
        (if (/= txt txt1) (setq txt (atof txt)) (setq txt (dxf (read (car tmp)) entl))) ;;如果(car tmp)对应的值未被用户修改过，取回原来的实数数值 
                (if (and (or (= (type rc) 'REAL) (= (type rc) 'INT)) (= pop "0")) ;;如果设置了容差，且为数值型，过滤条件为"="时要处理容差 
               (setq filter (append ;;处理容差 
                                        (cons '(-4 . "<=") (list (cons (read (car tmp)) (+ txt (abs rc))))) 
                       (cons '(-4 . ">=") (list (cons (read (car tmp)) (- txt (abs rc))))) 
                                       filter 
                                    );;append 
                );;setq 
                    (setq filter (append    ;不处理容差 
                                   (cons (cons -4 (nth (read pop) '("=" "<" ">" "<=" ">=" "<>"))) (list (cons (read (car tmp)) txt))) 
                                  filter 
                          );;append 
                    );;setq 
        );;end of if 容差 
      ) 
      ((member (car tmp) '("10" "11")) 
       (setq txt1 (get_tile (caaddr tmp)) 
         txt2 (get_tile (car (cadddr tmp))) 
         txt3 (get_tile (car (last tmp))) 
         pop_1 (nth (read pop) '("=" "<" ">" "<=" ">=" "<>"))
	 pop_2 pop_1
	 pop_3 pop_1
       );;setq
       (if (= txt1 "") (setq pop_1 "*"))
       (if (= txt2 "") (setq pop_2 "*"))
       (if (= txt3 "") (setq pop_3 "*"))
       (if (/= txt1 (cadr (caddr tmp))) (setq txt1 (atof txt1)) (setq txt1 (car (dxf (read (car tmp)) entl))));;如果坐标对应的值未被用户修改过，取回原来的实数数值 
       (if (/= txt2 (cadr (cadddr tmp))) (setq txt2 (atof txt2)) (setq txt2 (cadr (dxf (read (car tmp)) entl)))) 
       (if (/= txt3 (cadr (last tmp))) (setq txt3 (atof txt3)) (setq txt3 (caddr (dxf (read (car tmp)) entl)))) 
       (setq filter (append 
                (cons (cons -4 (strcat pop_1 "," pop_2 "," pop_3)) (list (cons (read (car tmp)) (list txt1 txt2 txt3)))) 
                filter   
            ) 
        ) 
      );;end of member (car tmp) '("10" "11") 
    );;cond 
  );;foreach tmp klst 
  (if fjlst (progn 
          (if (null filter) (setq filter (list (assoc 0 entl))))  ;;如果仅选中的附加条件，则将filter设为样板实体的类别 
          (setq fjflt '(and)) 
          (foreach tmp fjlst 
        (setq pop (get_tile (cadr tmp)) 
              txt (get_tile (caaddr tmp)) 
              txt1 (cadr (caddr tmp)) 
              rc (read (get_tile (car (last tmp)))) 
        );;setq 
        (if (/= txt txt1)
	       (if (/= "INSERT" (dxf 0 slent)) (setq txt (atof txt)));图块实体的附加过滤为字符型，其余为数值型
	       (setq txt (eval (cadr (dxf (car tmp) lst5))))
	) ;;如果(car tmp)对应的值未被用户修改过，取回原来的实数数值 
                (if (and (or (= (type rc) 'REAL) (= (type rc) 'INT)) (= pop "0")) ;;如果设置了容差，且为数值型，过滤条件为"="时要处理容差 
               (setq fjflt (append ;;处理容差 
                               fjflt 
                       (list (list 'and 
                          (list '<= (cadr (dxf (car tmp) lst5)) (+ txt (abs rc))) 
                          (list '>= (cadr (dxf (car tmp) lst5)) (- txt (abs rc))) 
                       )) 
                                    );;append 
                );;setq 
                    (setq fjflt (append    ;不处理容差 
                                    fjflt 
                    (list (list (read (nth (read pop) '("=" "<" ">" "<=" ">=" "<>"))) (cadr (dxf (car tmp) lst5)) txt)) 
                          );;append 
                    );;setq 
        );;end of if 容差 
          );;foreach fjlst 
  ));;end if fjlst 
);;defun 
(defun show_list ( key lst) 
  (start_list key) 
  (mapcar 'add_list lst) 
  (end_list) 
);;defun 
(defun write (f ktmp code value txt width / tmp) 
   (setq tmp (strcat txt (vl-princ-to-string code))) 
   (write-line (strcat ":edit_box{value=\"" value "\";key=\"" tmp "\";edit_width=" width ";allow_accept=true;}") f) 
   (setq ktmp (cons (list tmp value) ktmp)) 
);;defun 
(defun dxf ( i ent) 
    (if (= (type ent) 'ENAME) 
      (setq ent (entget ent)) 
    ) 
    (cdr (assoc i ent)) 
);;defun
(defun chsget ( c01 / c02 c03 c04 c05) 
   (if c01 (progn 
           (setq c02 0 c03 (sslength c01)) 
           (while (< c02 c03) 
              (setq c04 (ssname c01 c02) 
                    c02 (1+ c02)) 
              (setq c05 (cons c04 c05)) 
           )  ;end of while 
           )  ;end of progn 
    )  ;end of if 
    c05 
)  ;end of defun
(defun len (ent)
  (if (= (type ent) 'ENAME) (setq ent (vlax-ename->vla-object ent)))
  (if (wcmatch (vla-get-ObjectName ent) "AcDbPolyline,AcDbEllipse,AcDbCircle,AcDbArc,AcDbLine,AcDb2dPolyline,AcDbSpline")
      (vlax-curve-getdistatparam ent (vlax-curve-getendparam ent))
  );;if
);;defun
(defun VxGetAtts (Obj) 
(if (= (type Obj) 'ENAME) (setq Obj (vlax-ename->vla-object Obj)))
  (if (= (vla-get-ObjectName obj) "AcDbBlockReference")
  (mapcar 
    '(lambda (Att) 
       (cons (vla-get-TagString Att) 
         (vla-get-TextString Att) 
       ) 
     ) 
    (vlax-invoke Obj "GetAttributes") 
  )
    )
) 
(defun KLDC_1 (/ attl alen lval ltag aflst aa cc a11 a12 a13)
 (setq attl (VxGetAtts slent))
 (if attl (progn
   (setq alen (length attl))
   (while (> alen 0)
    (setq a11 (list (cons 'nth (cons (- alen 1) '((VxGetAtts slent)))))
          a12 (cons 'if (list '(VxGetAtts slent) (cons 'cdr a11)))
          a13 (cons 'if (list '(VxGetAtts slent) (cons 'car a11)))
          lval (list  (strcat "FJ" (rtos (* 2 alen) 2 0))       '"属性数值" a12)
          ltag (list  (strcat "FJ" (rtos (- (* 2 alen) 1) 2 0)) '"属性标志" a13)
          aflst (append (list lval ltag) aflst)
          alen (1- alen))
   );;end while
   (setq aa (assoc "INSERT" lst2)
         cc (list (car aa) (cadr aa) (append '(FJ) (reverse aflst)))
         lst2 (subst cc aa lst2))
  );;progn
 );;if
);;end defun
(if (not (member "acopm.arx" (arx))) (arxload "acopm.arx")) 
(princ)

;;;改块颜色
(defun c:se( / blk blkref blocks doc ent name ss n clo)
 (vl-load-com)
  (princ "\n选要随层的块: ")
(setq ss (ssget '((0 . "INSERT")))
      n	 (sslength ss)
)
  (while (and (setq BLK     (ssname ss (setq n (1- n))))
	  (setq BLKREF  (vlax-ename->vla-object BLK))
	  (not(and(/= (vla-get-objectname BLKREF) "AcDbBlockReference")
	      (princ"\n不是块:"))
	   )
	      (setq clo (acad_colordlg 7))

	  (setq name(vla-get-name BLKREF))
      )
    (progn
          (command"undo""group")
          (setq DOC     (vla-get-activedocument (vlax-get-acad-object))
                BLOCKS  (vla-get-blocks doc)
	        blk     (vla-item BLOCKS name)
          )
           (vlax-for ENT blk
             (vla-put-layer ent "0")
             (vla-put-color ent clo)
           )
        (vla-regen doc acActiveViewport)
        (vlax-release-object blk)
        (vlax-release-object BLOCKS)
        (vlax-release-object DOC)
        (command"undo""end")
        
     )
  )
  (princ"\nUndo后请regen.")
(princ))
