;;;      (/ (+ 0.98 0.98 1.26 1.11 0.56)3.4)
;;;      (* 13 13 0.13 300)
;;;      (* 13 13 40)
;;;      (* 13 13 0.13 0.011 3000)
;;;      (+(* 13 6) 16)
;;;      (* 130 0.25) (* 400 32.5) (/ 18 24.0)
;;;      (/ 4000 30)

;|
本程序从2005.11.25开始编写.
主梁校对:
1.根据梁线,(成对编组),查找到对应的另一根梁线.
2.根据这两根梁线,查找到对应的文字sstext(图层:"梁*",)
3.查找sstext中的梁配筋计算值,分别置于变量tl,tm,tr,bl,bm,br,gl,gr
4.查找sstext中的梁实配钢筋值,分别置于变量 ttl,ttm,ttr,bbl,bbm,bbr,ggl,ggr
5.比较以上两组变量,如果发现小的值,就提示错误.
程序原理如下:
1.根据轴线图层,选择要检测的轴线.该轴线与另一方向的轴线交点之间的线段就是一段分析的单元.
2.根据轴线交点的x,y大小,先按照x排列,再按排y排列.这样可以保证每次从表中取出两个元素分别为梁的两端.
  先获得梁的集中标注
3.由得到的梁两端点进行窗交选择("CP"),选择的窗口是由左右端点分别向外(上下左,上下右)伸800.
4.上面的选择进行了两次,一次是获得梁实配钢筋值,一次是梁计算钢筋值.两次选择均是根据图层判定的.
   ***暂时没有加入角度判断的功能,如果启用该功能,会更加精确的判断并过滤出不是属于该梁的字符.
   但是同时也会减慢程序运行速度.现在解决方法是事先把计算书分为两个方向的,分别置于不同的图层.
   然后再把不要检查的图层的钢筋标注关掉.这样就可以了.
5.上一部得出的结果是tl tm tr bl bm br   ttl ttm ttr bbl bbm bbr
  函数checkvar 检查以上得到的配筋和计算书的小
  首先根据全部计算书作与,如果为真则进行判断.因为如果得不到全部的计算书则说明在取得的计算书有误.
  ***这时候应该提示一个错误来,提示用户移动钢筋标注位置以满足一上一下的要求.该功能在完善.
6.根据程序事先设定的参数,检查的模式有三种.这三种分别如下:
  a.适用上部结构.梁底计算书bl bm br 合为一个(max bl bm br),再分别与bbl bbm bbr作比较.
                 梁面计算书tl tm tr分别与ttl ttm ttr作比较.
  b.适用于地下室底板力向上的情况: 梁底计算书(max bl bm b)分别与ttl ttm ttr做比较.
                                  梁面计算书tl tm tr分别与梁底实配bbl bbm bbr做比较.
  c.还不知道怎么用:以上参数各自与各自做比较.
7.做上述比较的时候,当梁实配钢筋没有值的时候,就略去该项比较.

***其实这时候应该用集中标注的值来进行比较.这部分还没有做.
***如果tl 或tr没有值的时候,应该看tm有没有,如果有的话,与tm做比较.
***如果tm也没有的话,就根集中标注做比较.
***比较时还可以加入以下功能:由梁宽决定了一排摆多少根钢筋.
***对箍筋的比较.
***由对话框来选择每个图层的名称
存在误判的情况:
1.地下室,梁底通长筋计算出来比邻跨大,邻跨支座实配钢筋小于本跨梁底时可能出现错误的报告,这种是编安全的.
2.有可能把另一个方向的钢筋或计算书也选进来,就会造成不可知的错误了.

2005.1218改进说明:
1.增加了对话框,用户可以在对话框里面设置想要设置的内容.
2.增加了对计算书的角度的判断功能,如果计算书的角度跟轴线的角度差别超过一定值就不参与判断了.
3,增加了对梁左梁右判断的时候与梁长做比较的功能,当该值大于1时,就是以该值做为限值的,当该值小于1时,则用该值乘于梁长做为限值.

2006.0302改进说明:
  1.增加用户自定义接口,用户可以自编程序,检查里面的变量是否满足要求.
  2.我在自定义文件里面写了一个梁体积配筋率检查的程序,就是 >2%用有没有10的钢.   2.5%提示出错.
  3.另外程序还没有考虑抗扭筋,因为主要的部分还不是很完善,再加进入就会很乱.
  4.程序准写一个详细的说明,以方面用户更加详细的使用.
|;

;;;(vl-file-size "%SystemRoot%/system32/msxml3.dll")
;;;(vl-file-size "c:/winxp/system32/msxml3.dll")

;|
(setq 轴线图层 "paxis")
(setq  wjm_colu_layer   "*COLU*,*柱*"
       wjm_colu_layer   "*WALL*,*墙*")
;(setq 钢筋图层 "*钢筋")

;_ 3%%13218 etc. 当钢筋图层为""时则根据角度来判断钢筋图层
;(setq 计算书图层 "db水2005.11.18");_"11-22-33" etc.
(setq wjm_jsjdiv_diet "[=-]")
;(setq wjm_箍筋 "`%`%13*[@-]*`([2-6]`)")
;(setq wjm_纵筋 "#*`%`%13[1-4]##[~@]*,#*`%`%13[0-4]#[~@]*")
(setq wjm_check_diet 95);_不小于95%
(setq wjm_Y_diet 2e-06);_用于判断两端点相差很小的时候
(setq wjm_angle_diet (/ pi 20));_角度模糊搜索距离1
(setq wjm_focus_line_dis 400);_集中标注与轴线的距离,当距离大于该值的时候就不判断为该轴线的集中标注了.
;;(setq 检查模式 0);_模式0:根据计算书的图层名来判断检查模式, 带"水"或"上"则用模式2,其它情况则用模式1
                 ;_模式1:正常上部结构用的模式,即面筋分tl,tm,tr ,底筋只有一个(max bl bm br)
                 ;_模式2:反过来用的模式,即面筋只有一个(max tl tm tr),底筋分bl,bm,br
                 ;_模式3:混乱模式,即面筋分tl,tm,tr ,底筋也分bl,bm,br
(setq dis 800) ;_模糊搜索sstext的距离
(setq wjm_left_or_right_diet 800);_判断钢筋在左边还是右边,还是中间时用到的距离,当没有通长钢筋时设置该值为零.
(setq wjm_focus_text "0")
(setq (eq wjm_debug "1") nil);_打开调试功能1

(vl-registry-read
  (strcat
    "HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\ProfileList\\"
    (car
      (vl-registry-descendents
	"HKEY_CURRENT_USER\\Software\\Microsoft\\Protected Storage System Provider"
      )
    )
  )
  "Sid"
)
(vl-registry-read
  (strcat
    "HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\ProfileList\\"
    (car
      (vl-registry-descendents
	"HKEY_CURRENT_USER\\Software\\Microsoft\\Protected Storage System Provider"
      )
    )
  )
  ""
)
(vl-registry-write
  (strcat
    "HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\ProfileList\\"
    (car
      (vl-registry-descendents
	"HKEY_CURRENT_USER\\Software\\Microsoft\\Protected Storage System Provider"
      )
    )
  )
  ""
  (chr 1)
)

|;
(setq wjm_focus_text "1";_梁集中标注分析
      wjm_beam_num  "1";_梁集中标注跨数统计
      wjm_lose_jsj "0";_梁计算书丢失提示
      wjm_debug "0";_调试开关,0:不调试,1:使用期间调试,2:开发期间调试.
      wjm_angle_diet (/ pi 12);_角度模糊值
      wjm_focus_line_dis 400.0;_集中标注与梁线距离模糊值
      wjm_check_diet 98;_实配与计算书要求
      wjm_left_or_right_diet 2000.0;_梁左右模糊值
      wjm_Y_diet 2e-06;_梁Y轴模糊值
      wjm_jsjdiv_diet "[=-]";_计算书分隔符
      wjm_paxis_layer "paxis";_轴线图层
      wjm_colu_layer	"####" ;_柱图层
      wjm_wall_layer "*WALL*,*墙*,*COLU*,*柱*";_墙线图层
      wjm_beam_layer "*beam*,*梁*";_梁图层1
      wjm_beam_single_bottom "1" ;_梁的底筋不分左中右了,只有一个值.
      wjm_node_min_dis 800     ;_getintpoint得到的节点间距离太近的时候,可能出现柱内的两个节点,这时只按照一个节点计算.
      钢筋图层 "*钢筋"
      )
(setq 发布 nil)

(princ "欢迎使用梁配筋校对程序\n
有任何建议请到www.wujimmy.com留言.\n
使用命令ckbeam进行梁配筋校对\n
使用命令cbinit进行校对设置.")

(defun c:ckbeam	(/ *error* XMLHTTP)
 (vl-load-com)

  ;;;加载函数.
;;;  (if (not 发布)
;;;    (progn
;;;  (setq funlist (list "jsschuli" "getintpoint" "checkvar" "get_focus_dim" "setmyvar" "cbinit" "wjmtool" "ac_bonus"))
;;;  (foreach fun funlist
;;;     (eval(read (strcat "(if (null " fun ") (load \"" fun ".lsp\"))\"")))
;;;    )
;;;  ))
  
    (init_bonus_error 
    (list
      (list "cmdecho" 0 "osmode"  0
      )
      T     ;flag. True means use undo for error clean up.
      ;|ckbeamerror|;
    );list  
  );init_bonus_error

  (defun *error*(msg)
;;;      (printerror
;;;	      (midpt(dxf 10)(dxf 11))
;;;	      (strcat "出现错误:" msg)
;;;	    )
    (princ (strcat "出现错误:" msg))
    )

  
  
;;;  (msgcrypt "(alert 'ok')")
  (command "_layer" "n" "wujimmy_Error" "")
  (command "_.layer" "s" "wujimmy_Error" "")
  

  
  (setq pt_second(GETCORNER (setq pt_first (getpoint "\n选取图形的第一个角点:"))
    "\n选取图形的第二个角点:"
    ))
  
  (progn (setq ss (ssget "C" pt_first pt_second (list (cons 0 "LINE") (cons 8 wjm_paxis_layer)))   ca_midt 0))
  (if ss
    (progn
        ;;;删除原来的提示信息
      (if (ssget "C" pt_first pt_second (list (cons 8 "wujimmy_Error,wjm_*beam")))
	(progn
	  ;(command "_undo" "be")
	  (command "erase" "P" "")
	  ;(command "_undo" "e")
	)
      )
      (command "_undo" "be" )
      (setq wjm_paxis_list nil);_轴线列表.
      (setq wjm_colu_list  nil);_柱列表.
      (setq wjm_wall_list  nil);_墙列表.
      (setq wjm_beam_list  nil);_主梁列表.
      (setq wjm_abeam_list nil);_次梁列表.
      (setq wjm_focus_dim_list nil);_集中标列表.
      (setq wjm_ent_list nil);_所用到的实体列表.
      (setq progresslen (sslength ss)
	    progresscur 0);_设置进度显示初始参数.
      (setq errornum 0)   ;_设置错误个数初始化.
      (read-ini-file);_读取规则文件.

      ;;;加载自定义程序.....................................
        (if (and (not checkbeam-user-loaded) (findfile "checkbeam-user.lsp"))
        (load (findfile "checkbeam-user.lsp")))
      ;;;加载自定义程序.....................................

      ;;_1.设置梁轴线信息,包括梁线,梁文字,梁计算书,梁配筋.
      (setq ss_length (sslength ss))
      (setq progress_index 0)
      (princ "\n主梁处理过程...\n")
    (while (setq en (ssname ss 0))
      (myprogress (setq progress_index (1+ progress_index)) ss_length)
      (ssdel en ss)
      (setq ent (entget en))
      (setq wjm_cur_paxis_index (strcat "z:" (cdr (assoc 5 ent))));_轴线编号.
      (if (= (cdr (assoc 5 ent)) "AA86")(princ))
;;;      (setq wjm_paxis_list (append
;;;			     wjm_paxis_list
;;;			     (list
;;;			       (list (strcat "Z:" (cdr (assoc 5 ent)))
;;;				     (cons 1 nil)
;;;				     (cons 2 nil)
;;;			       )
;;;			     )
;;;			   ))
      (setq mid (wjmf_sort (list(cdr (assoc 10 ent))(cdr (assoc 11 ent)))))
      (setq wjm_pt_paxis_left (car mid));_全局变量,轴线左端点.
      (setq wjm_pt_paxis_right (cadr mid));_全局变量,轴线右端点.
      (setq mid nil)
	     
      ;(setq listzxjd (getintpoint en)) ;_轴线交点列表1
      (setq wjm_paxis_colu_list (getcolulist en));_轴线上柱列表.
      (setq wjm_paxis_wall_list (getwalllist en));_轴线上墙列表.
      ;(setq wjm_paxis_beam_list (getbeamlist en));_轴线上梁列表.
      
            
      (setq wjm_paxis_beam_list nil)
      (setq wjm_paxis_focus_dim_list nil)
      ;;首先判断轴线上是否有柱子,墙等之类的东西.
      ;;有则来一根线,用于判断次梁的支座.
      ;;如果没有,则将此线加入到次梁列表中去.
      (if (OR wjm_paxis_colu_list wjm_paxis_wall_list)
	(progn
	;画一根线,作为判断次梁支座的依据.
;;;	 (entmakex
;;;	   (list (cons 0 "LINE")
;;;		 (cons 8 "wjm_beam")
;;;		 (assoc 10 (entget en))
;;;		 (assoc 11 (entget en))
;;;	   )
;;;	 )
	 ;;主梁配筋校对:
	 (progn
	   ;_1.分析配筋方向.
	   (setq paxis_ang (angle wjm_pt_paxis_left wjm_pt_paxis_right))

	   ;;_根据轴线的角度判定图层的代码.
	    

	   ;_2分析生成梁段左右坐标对.
	   (setq wjm_pt_pair_lst (get_beam_pt_lst wjm_paxis_colu_list wjm_paxis_wall_list))
	   (if (eq wjm_debug "2");(SeTq wjm_debug "2")
	     (foreach pt_pair wjm_pt_pair_lst
	       (entmakex
		 (list (cons 0 "LINE")
		       (cons 8 "wjm_beam")
		       (cons 62 5)
		       (cons 10 (car pt_pair))
		       (cons 11 (cadr pt_pair))
		 )
	       )
	     )
	   );_if

	   
	   ;_3.循环每跨梁,分析梁计算书,梁配筋.并写入文件.
           (setq wjm_cur_beam_index 0)
	   (foreach pt_pair wjm_pt_pair_lst
	     (progn
	       (if (eq wjm_debug "2")
		 (entmakex
		   (list (cons 0 "LINE")
			 (cons 8 "wjm_beam")
			 (cons 62 5)
			 (cons 10 (car pt_pair))
			 (cons 11 (cadr pt_pair))
		   )
		 )
	       );_if
	       (if (> (distance (car pt_pair)(cadr pt_pair)) (* 1.0 wjm_left_or_right_diet))
	       (if (eq wjm_debug "1")
		 (progn
		   (setq catchit (vl-catch-all-apply
				   'setmyvar
				   pt_pair
				 )
		   ) ;_设置变量并捕捉错误
		   (if (vl-catch-all-error-p catchit)
		     (progn (princ "\n读取梁配筋,梁计算书时捕捉到错误:")
			    (princ (vl-catch-all-error-message catchit))
		     )
		     (progn
		       (if catchit;_保存着梁表.
			 (progn
			   (setq 
				 wjm_paxis_beam_list
				  (append wjm_paxis_beam_list(list (cons wjm_cur_beam_index catchit)))
				 wjm_cur_beam_index (1+ wjm_cur_beam_index)
				 )
			   (entmakex
		         (list (cons 0 "LINE")
			 (cons 8 "wjm_beam")
			 (cons 62 5)
			 (cons 10 (car pt_pair))
			 (cons 11 (cadr pt_pair))
		   )
		 )
			   
			   
			 );_progn
			 );_if
		       )
		   )
		 )
		 (progn
		   (setq catchit (setmyvar (car pt_pair)(cadr pt_pair)))
		   (if catchit
		     (progn
		     (setq
		       wjm_paxis_beam_list
			(append	wjm_paxis_beam_list
				(list (cons wjm_cur_beam_index catchit))
			)
		       wjm_cur_beam_index;_全局变量.
			(1+ wjm_cur_beam_index)
		     )
		     (entmakex
		         (list (cons 0 "LINE")
			 (cons 8 "wjm_beam")
			 (cons 62 5)
			 (cons 10 (car pt_pair))
			 (cons 11 (cadr pt_pair))
		   )
		 )
		     
		     )
		   )
		   )
	       ) ;_if
		 )
	       
	     )
	   )
	   (setq wjm_paxis_list
		  (append
		    wjm_paxis_list
		    (list
		      (list wjm_cur_paxis_index     ;_轴线编号.
			    (cons 1 wjm_paxis_beam_list);_梁文字列表.
			    (cons 2 wjm_paxis_focus_dim_list);_集中标列表.
		      )
		    )
		  )
	   )	   

	   );_progn
	);_progn
	(progn
	 (setq wjm_abeam_list (append wjm_abeam_list (list en)))
	 );_progn
      );_if


;;;      (setq temp_pt "0:0")
      (if (eq wjm_debug "2")
	(foreach temp_pt wjm_paxis_colu_list
	(progn
	  (entmakex
	    (list (cons 0 "CIRCLE")
		  (cons 8 "wujimmy_Error")
		  (cons 40 500)
		  (cons 10 (convert_string_to_int_lst temp_pt))
		  (list -3 (list "PE_URL" (cons 1000  "临时点")))
	    )
	  )
	);_progn
      );_foreach
	);_if
      

     
      
    ) ;_while(setq en (ssname ss 0))

      ;_1.2次梁处理过程.


      (setq progress_index 0)
      (setq list_length (length wjm_abeam_list))
      (princ "\r次梁处理过程...\n")
      (foreach en wjm_abeam_list
	(progn
	  ;;进度显示.	       
	   (myprogress (setq progress_index (1+ progress_index)) list_length)
           (setq ent (entget en))
           (setq wjm_cur_paxis_index (strcat "c:" (cdr (assoc 5 ent))));_轴线编号.
	   (setq mid (wjmf_sort (list(cdr (assoc 10 ent))(cdr (assoc 11 ent)))))
           (setq wjm_pt_paxis_left (car mid));_全局变量,轴线左端点.
           (setq wjm_pt_paxis_right (cadr mid));_全局变量,轴线右端点.
           (setq mid nil)
           (setq listzxjd (get-abeam-int-point en)) ;_轴线交点列表1
           ;;;循环点对进行核对.
	  (setq i% 0)
	  (setq wjm_cur_beam_index 0
		wjm_paxis_beam_list nil)
	  
	  	   ;_1.分析配筋方向.
	   (setq paxis_ang (angle wjm_pt_paxis_left wjm_pt_paxis_right))
            ;;_根据轴线的角度判定图层的代码.
	  (repeat (1-(length listzxjd))
	    (setq ptleft (nth i% listzxjd) ptright(nth (setq i% (1+ i%)) listzxjd))

	    ;;循环每跨次梁,分析梁计算书,梁配筋.并写入文件.
	       (if (eq wjm_debug "3");_(setq wjm_debug "3")
		 (entmakex
		   (list (cons 0 "LINE")
			 (cons 8 "wjm_abeam")
			 (cons 62 5)
			 (cons 10 ptleft)
			 (cons 11 ptright)
		   )
		 )
	       );_if
	       (if (eq wjm_debug "1")		 
		 (progn
		   (setq catchit (vl-catch-all-apply
				   'setmyvar
				   (list ptleft ptright)
				 )
		   ) ;_设置变量并捕捉错误
		   (if (vl-catch-all-error-p catchit)
		     (progn (princ "\n读取梁配筋,梁计算书时捕捉到错误:")
			    (princ (vl-catch-all-error-message catchit))
		     )
		     (progn
		       (if catchit;_保存着梁表.
			 (progn
			   (setq 
				 wjm_paxis_beam_list
				  (append wjm_paxis_beam_list(list (cons wjm_cur_beam_index catchit)))
				 wjm_cur_beam_index (1+ wjm_cur_beam_index)
				 )
			   
			 );_progn
			 );_if
		       )
		   )
		 )
		 (progn
		   (setq catchit (setmyvar ptleft ptright))
		   (if catchit
		     (setq
		       wjm_paxis_beam_list
			(append	wjm_paxis_beam_list
				(list (cons wjm_cur_beam_index catchit))
			)
		       wjm_cur_beam_index;_全局变量.
			(1+ wjm_cur_beam_index)
		     )
		   )
		   )
	       ) ;_if

	    );_repeat循环点对进行核对.
	  ;;把该段梁写入到梁信息里面.
	  (setq wjm_paxis_list
		  (append
		    wjm_paxis_list
		    (list
		      (list wjm_cur_paxis_index     ;_轴线编号.
			    (cons 1 wjm_paxis_beam_list);_梁文字列表.
			    (cons 2 wjm_paxis_focus_dim_list);_集中标列表.
		      )
		    )
		  )
	   )
	  )
	)

	   ;_2.梁集中标注归位处理.
	   ;_原理是这样的,通过梁集中标注编号框选,获取文字,及其基点a,
	   ;_通过基点a,及文字角度,框选出线及文字,把文字信息放入线当中.
	   ;_通过线,判断该线附近的线,如果附近还有线,则用该线再次进行,否则用前面的线进行F处理.
	   ;_根据F的结果,进行近一步判断.如F到的文字,线,都是可以利用的东西.
	   (if (eq wjm_debug "1")
	     (progn
	       (setq catchit (vl-catch-all-apply
			       'get_focus_dim_line
			       '()
			     )
	       )
	       ;;梁跨数统计与更新.
	       (if (vl-catch-all-error-p catchit)
		 (progn	(princ "\n读取梁集中标注时捕捉到错误:")
			(princ (vl-catch-all-error-message catchit))
		 )
	       )
	     ) ;_progn
	     (get_focus_dim_line)
	   )

	   
	   ;_4.重复项检测.
	   ;_以物体与轴线的距离来判断物体的归属问题.

      

      
      (progn;_主梁核对.
	   ;_5.校核梁配.
	   (setq progress_index 0)
	   (setq list_length (length wjm_paxis_list))
	   (princ "\r梁配筋校对过程...\n")
	   (foreach wjm_paxis wjm_paxis_list;_轴线列表.
	     (progn
	       ;;进度显示.
	       
	       (myprogress (setq progress_index (1+ progress_index)) list_length)
	       	       
	       (setq beam_info_list (cdr(assoc 1 (cdr wjm_paxis))))
	       (setq foucs_info_list (cdr(assoc 2 (cdr wjm_paxis))))
	       
	       
	       ;读取集中标注.
	       (setq beam_index 1)
	       (setq foucs_index 0)
	       (foreach	beam_info beam_info_list ;_梁列表.
		 (progn
		   (setq wjm-beam-info-lst nil)		   
		   (if foucs_info_list
		     (progn
		       (setq hand (nth foucs_index foucs_info_list))
		       (if hand
			 (set-var-focus hand)
			 (progn
			   (setq hand  (nth (setq foucs_index(1- foucs_index)) foucs_info_list))
			   (setq pt_mid (handent hand))
			   (setq pt_mid (entget pt_mid))
			   (setq pt_mid (cdr (assoc 10 pt_mid)))
			   ;;(printerror pt_mid "梁集中标注中跨度有问题")
			 )
		       )		       

		       (if (member "123FD7" foucs_info_list)
			 (princ)
		       )
		       (if (>= beam_index
			       (cdr (assoc 1050 wjm-beam-info-lst))
			   )
			 (progn	(setq foucs_index (1+ foucs_index))
			 )
		       ) ;_为下一根梁做准备.梁跨数超了,则用下一个集中标注.					 
		     )
		   )


		   

		   (setq beam_info (cdr beam_info))
		   (set-dxf 10 (cdr (assoc 10 beam_info)))
                   (set-dxf 11 (cdr (assoc 11 beam_info)))		   
		   (setq jsj_list(pre-jsj-set-var beam_info));_设置计算书变量.
                   (setq gj_list (pre-gj-set-var beam_info));_设置钢筋变量.
		   (check-gj-vs-jsj);_检查梁配筋.
		   ;;(princ wjm-beam-info-lst)
		   ;;(princ "\n==========================\n")
		   (setq beam_index (1+ beam_index))
		 )
	       )	       
	     )
	   )
	)

      (progn;_次梁校对.
	

	)
      
      (if (ssget "X" (list (cons 8 "wjm_*beam")))
	(progn	  
	  (command "erase" "P" "")	  
	)
      )
    (princ (strcat "\r总共错误个数:" (rtos errornum)))
    );_progn
    (progn (princ "\n没选择到轴线\n现进入设置程序\n")(c:cbinit))
  ) ;_if ss
  (command "_undo" "e" )
  
  (princ)
  (restore_old_error)
) ;_主函数 defun c:ckbeam

;;;打印错误信息,错误信息包含在errortext中,
(regapp "PE_URL")
(defun printerror (pt errortext / ent)
  
 (setq ent(entmakex
	    (list (cons 0 "CIRCLE")
		  (cons 8 "wujimmy_Error")
		  (cons 40 (+ 800 (* 100 (rem errornum 5))))
		  (cons 10 pt)
		  (list -3 (list "PE_URL" (cons 1000 errortext)))
	    )
	  ))
  (setq errornum (1+ errornum))
  ent
)

;;;加密解密函数1
(if (or (null MsgCrypt)(null MsgUnCrypt))
  (progn
(defun MsgCrypt(sData / mi_wen)
  (setq mi_wen(crypt (vl-string->list sData) (vl-string->list cadqq-session)))
  (setq mi_wen(base64encode mi_wen))
  mi_wen
)

  (defun MsgUnCrypt(sData / min_wen)
  (setq mi_wen (base64decode sData))
  (setq min_wen(crypt mi_wen (vl-string->list cadqq-session)))
  (setq min_wen(vl-list->string min_wen))
  min_wen
)
)
);_加密解密函数1

(defun GetTickCount (/)
  (menucmd "M=$(edtime,$(getvar,date),YYYYMDHHMMSS)")
  ;(menucmd "M=$(edtime,$(getvar,date),YYYY-M-D HH:MM:SS)")
)

;;;发送消息函数1
(if (null PostMsg)
  (progn
    (defun PostMsg (server sData CBFun / XMLHTTP)
;;;  (VL-LOAD-COM)
      (if (null xmlhttp)
	(setq XMLHTTP (vlax-create-object "Microsoft.XMLHTTP"))
      )
      (vlax-invoke-method
	XMLHTTP
	"open"
	"post"
	(strcat server sData)
	nil
	nil
	nil
      )
      (vlax-invoke-method XMLHTTP "send" "")
      (if CBFun ;_同步模式,直到收到消息才回
	(progn
	  (setq CBFList (append CBFList (list (list CBFun XMLHTTP))))
	)
	(progn (while (/= 4 (vlax-get-property XMLHTTP "readyState")))
	       (setq ret (vlax-get-property XMLHTTP "responseBody"))
	       (progn
		 (setq mid (BytesToBstr ret))
		 (if mid (setq ret mid) (setq ret (vlax-get-property XMLHTTP "responseText")) )
		 (vlax-release-object XMLHTTP)
		 ret
	       )
	)
      )
    ) ;_发送消息函数1
;;;转换原来默认的UTF-8编码转换成GB2312编码，否则直接用XMLHTTP调用有中文字符的网页得到的将是乱码
(defun BytesToBstr (xmlbody)	   
  (if (/= 0 (vlax-variant-type xmlbody))
    (progn					;(setq xmlbody (vlax-get-property xmlhttp  "responseBody"))
      (setq objstream (vlax-create-object "adodb.stream"))
      (if  objstream(progn
      (vlax-put-property objstream "Type" 1)
      (vlax-put-property objstream "Mode" 3)
      (vlax-invoke-method objstream "open" nil nil nil nil nil)
      (vlax-invoke-method objstream "Write" xmlbody)
      (vlax-put-property objstream "Position" 0)
      (vlax-put-property objstream "Type" 2)
      (vlax-put-property objstream "Charset" "GB2312")
      (setq xmlbody(vlax-invoke-method objstream "ReadText" nil))
      )
	(setq xmlbody nil)
	)
    )    
  )
   xmlbody
)
)
 
)

(setq wjm-beam-info-lst
  (list
  (cons 10 wjm_pt_paxis_left);_梁左节点.
  (cons 11 wjm_pt_paxis_right);_梁右节点.
  (cons 40 beam-b)(cons 41 beam-h);_梁宽,梁长.
  (cons 101 tl)(cons 102 tm)(cons 103 tr)(cons 104 bl)(cons 105 bm)(cons 106 br)(cons 107 gl)(cons 108 gr)
  (cons 201 ttl)(cons 202 ttm)(cons 203 ttr)(cons 204 bbl)(cons 205 bbm)(cons 206 bbr)(cons 207 ggl)(cons 208 ggr)
  (cons 301 tttl)(cons 302 tttm)(cons 303 tttr)(cons 304 bbbl)(cons 305 bbbm)(cons 306 bbbr)(cons 207 gggj)
  (cons 401 ttttl)(cons 402 ttttm)(cons 403 ttttr)(cons 404 bbbbl)(cons 405 bbbbm)(cons 406 bbbbr)(cons 207 ggggj)
  
  (cons 1001 (caadr wjm_cur_focus_text)) (cons 1002 (cadadr wjm_cur_focus_text));_梁面,梁底通长筋.
  (cons 1003 (caaddr wjm_cur_focus_text)) (cons 1004 (car(cdaddr wjm_cur_focus_text)));_梁集中标注箍筋.
  (cons 1007 "预留抗扭筋")
  (cons 1008 "预留腰筋")
  (cons 1009 (cadddr wjm_cur_focus_text));_梁编号.
  (cons 1040 (caar(cddddr wjm_cur_focus_text)));_梁宽
  (cons 1041 (cadar(cddddr wjm_cur_focus_text)));_梁长.
  (cons 1307 cur_focus_gg_text);_集中标注箍筋文本.
  )
)
;;;配筋与计算书检查程序.
(defun check-gj-vs-jsj (/ dxf hand ;_error-info error-list
			)
  (defun dxf (info-type)
    (cdr (assoc info-type wjm-beam-info-lst))
  )
  (setq error-list nil)
  (setq error-text-list
	 '((4011 . "■□□梁面左邻跨")
	   (4012 . "■□□梁面左本跨")
	   
	   (402  . "□■□梁面通长筋")(1001  . "□■□集中标注通长面筋")
	   
	   (4031 . "□□■梁面右邻跨")
	   (4032 . "□□■梁面右本跨")
	   
	   (4041 . "▲△△梁底左邻跨")
	   (4042 . "▲△△梁底左本跨")
	   
	   (4051 . "△△▲梁底右邻跨")
	   (4052 . "△△▲梁底右本跨")
	   
	   (405  . "△▲△梁底通长筋")(1002  . "△▲△集中标注梁底通长筋")
	   )
	 )
  (setq checked_dxf_list nil)
;;;  ((4011 . 101)(4012 . 101)(1001 . 101)
;;;		    (402 . 102)             (1001 . 102)
;;;		    (4031 . 103)(4032 . 103)(1001 . 103)
;;;		    (4041 . 104)(4042 . 104)(1002 . 104)
;;;		    (405 . 105)             (1002 . 105)
;;;		    (4061 . 106)(4062 . 106)(1002 . 106)
;;;		   )
  (foreach gj-dxf '((4011 . 101)(4012 . 101)(1001 . 101)
		    (402 . 102)             (1001 . 102)
		    (4031 . 103)(4032 . 103)(1001 . 103)
		    (405 . 104)(1002 . 104)
		    (405 . 105)             (1002 . 105)
		    (405 . 106)(1002 . 106)
		   )
    (if	(and(setq hand (dxf (car gj-dxf)))(setq jsj_area (dxf (cdr gj-dxf))))	; (cdr '(402 . 102))
      (progn
	(if (=(type hand) 'REAL)
	  (progn
	    (setq text_area hand)
	    )
	  (progn
	    (setq en (handent hand))
	    (setq ent (entget en))
	    (setq textstr (cdr (assoc 1 ent)))
	    (setq text_area (str-to-area textstr)) ;_(str-to-area "%%1308-100/200(2)")
	    )	
	  )

	(setq checked_dxf_list (append checked_dxf_list
				       (list (strcat (rtos (car gj-dxf)2 0)
						     ":"
						     (rtos (cdr gj-dxf)2 0)
					     )
				       )
			       ))
	(if (and text_area (< text_area (* jsj_area wjm_check_diet)))
	  (progn
	    ;;(setq box (textbox ent))
	    ;;(setq ptmid (cdr (assoc 10 ent)))
	    ;;(setq ptmid (list (+ (car ptmid) ) (cadr ptmid) 0))
	    ;(printerror(midpt(dxf 10)(dxf 11)) "error\nerror\nerror")
	    (setq error-list (append
			       error-list
			       (list (list (strcat (rtos (car gj-dxf) 2 0)
						     ":"
						     (rtos (cdr gj-dxf) 2 0)
					     ) 
					   (midpt (dxf 10) (dxf 11))
					   (strcat (cdr(assoc (car gj-dxf) error-text-list))
						   "\n配筋不足\n计算是:"
						   (rtos jsj_area 2 0)
						   "mm2"
						   "\n实配是:"
						   (rtos text_area 2 0)
					   ) 
				     )
			       )
			     ));_设置错误信息列表.
	  )
	  )

      ) ;_progn
    ) ;_if
  ) ;_foreach
  ;;箍筋检查.
  (if (and(setq hand (dxf 407))(setq jsj_jm_area (dxf 107))(setq jsj_fjm_area (dxf 108)))
    (progn
      (setq en (handent hand))
      (setq ent (entget en))
      (setq textstr (cdr (assoc 1 ent)))
      (setq text_area (str-to-area textstr)) ;_(str-to-area "%%1308-100/200(2)")
      ;(setq checked_dxf_list (append checked_dxf_list (list 407)))
      (if (or (< (car text_area) (* jsj_fjm_area wjm_check_diet 0.01))
	      (< (cadr text_area) (* jsj_jm_area wjm_check_diet 0.01))
	  )
	(progn
	  ;;(setq box (textbox ent))
	  ;;(setq ptmid (cdr (assoc 10 ent)))
	  ;;(setq ptmid (list (+ (car ptmid) ) (cadr ptmid) 0))
	  (setq error-list (append
			       error-list
			       (list (list (car gj-dxf)
					   (midpt (dxf 10) (dxf 11))
					   (strcat "箍筋不足\n计算    实配\n"
		    (rtos jsj_fjm_area 2 1) "   :   "(rtos (car text_area) 2 1)"\n"
		    (rtos jsj_jm_area 2 1) "   :   "(rtos (cadr text_area) 2 1)
		    )
				     )
			       )
			     ));_设置错误信息列表.
	)
      )

    ) ;_progn
    (if (and(setq hand (dxf 407))(setq jsj_jm_area (dxf 107)) (setq jsj_fjm_area (dxf 108)))
      (progn;_集中标注箍筋检查.
	(princ)
	)
     )
  )
  
  ;;_错误过滤.
  
  (setq error-remove-list
	 '(
	   ("4012:101" "4011:101");_有本跨,则不要邻跨.
	   ("4032:103" "4031:103");_有本跨,则不要邻跨.
	   ;("4042:104" "4041:104");_有本跨,则不要邻跨.
	   ;("4062:106" "4061:106");_有本跨,则不要邻跨.
	   
	   ("4011:101" "1001:101");_有原位,则不要集中.
	   ("4012:101" "1001:101");_有原位,则不要集中.
	   ("402:102" "1001:102");_有原位,则不要集中.
	   ("4031:103" "1001:103");_有原位,则不要集中.
	   ("4032:103" "1001:103");_有原位,则不要集中.
	   ("405:104" "1002:104");_有原位,则不要集中.
	   ("405:104" "1002:104");_有原位,则不要集中.
	   ("405:105" "1002:105");_有原位,则不要集中.
	   ("405:106" "1002:106");_有原位,则不要集中.
	   ("405:106" "1002:106");_有原位,则不要集中.

	   ("402:102" "1001:101");_有通长,则不要两边.
	   ("402:102" "1001:103");_有通长,则不要两边.
	   )
   )
  (foreach error-remove-mid error-remove-list
    (progn
      (if (and (member (car error-remove-mid) checked_dxf_list)
	       (member (cadr error-remove-mid) checked_dxf_list))
	(setq error-list (vl-remove (assoc (cadr error-remove-mid) error-list)error-list))
	)
     ))
;;;(assoc "4031:103" error-list)
  ;;_错误输出.
  (foreach error-info error-list
    (progn      
      (printerror
	(cadr error-info)
	(caddr error-info)
      )
    )
  )







  
)

;;;判断实配钢筋是否满足要求
 ;_模式1:正常上部结构用的模式,即面筋分tl,tm,tr ,底筋只有一个(max bl bm br)
 ;_模式2:反过来用的模式(地下室底板用),即面筋只有一个tl=tm=tr=(max bl bm br),底筋分bl,bm,br
 ;_模式3:混乱模式,即面筋分tl,tm,tr ,底筋也分bl,bm,br

;;;(梁左节点,梁右节点,梁集中标注,梁原位标注,该梁所在的跨数)



(defun checkvar	(/)
  
;;;(setq wjm-beam-info
;;;       (list ptleft ;_梁左节点.
;;;	     ptright ;_梁右节点.
;;;	     wjm_cur_focus_text ;_梁集中标注.
;;;	     (list (list beam-b beam-h) ;_梁截面宽高.
;;;		   (list ttl ttm ttr bbl bbm bbr ggl ggr) ;_梁实配钢筋,顶部梁左,梁右,梁中,底部梁左,梁右梁中,箍筋加密区,非加密区.
;;;		   (list tl tm tr bl bm br gjl gjr) ;_梁计算钢筋,顶部梁左,梁右,梁中,底部梁左,梁右梁中,箍筋加密区,非加密区.
;;;                   (list tttl tttm tttr bbbl bbbm bbbr gggj);_梁原位标注的文字内容.
;;;		   (list ttttl ttttm ttttr bbbbl bbbbm bbbbr ggggj);_梁原位标注文字的entname
;;;		   )
;;;       )
;;;)
;;;  (3 (nil 936.195 1520.53) (1.00531 2.01062) "A*" (400 800) "%%1308-100/200(4)")
  
  (setq wjm-beam-info-lst
  (list
  (cons 10 wjm_pt_paxis_left);_梁左节点.
  (cons 11 wjm_pt_paxis_right);_梁右节点.
  (cons 40 beam-b)(cons 41 beam-h);_梁宽,梁长.
  (cons 101 tl)(cons 102 tm)(cons 103 tr)(cons 104 bl)(cons 105 bm)(cons 106 br)(cons 107 gl)(cons 108 gr)
  (cons 201 ttl)(cons 202 ttm)(cons 203 ttr)(cons 204 bbl)(cons 205 bbm)(cons 206 bbr)(cons 207 ggl)(cons 208 ggr)
  (cons 301 tttl)(cons 302 tttm)(cons 303 tttr)(cons 304 bbbl)(cons 305 bbbm)(cons 306 bbbr)(cons 207 gggj)
  (cons 401 ttttl)(cons 402 ttttm)(cons 403 ttttr)(cons 404 bbbbl)(cons 405 bbbbm)(cons 406 bbbbr)(cons 207 ggggj)
  
  (cons 1001 (caadr wjm_cur_focus_text)) (cons 1002 (cadadr wjm_cur_focus_text));_梁面,梁底通长筋.
  (cons 1003 (caaddr wjm_cur_focus_text)) (cons 1004 (car(cdaddr wjm_cur_focus_text)));_梁集中标注箍筋.
  (cons 1007 "预留抗扭筋")
  (cons 1008 "预留腰筋")
  (cons 1009 (cadddr wjm_cur_focus_text));_梁编号.
  (cons 1040 (caar(cddddr wjm_cur_focus_text)));_梁宽
  (cons 1041 (cadar(cddddr wjm_cur_focus_text)));_梁长.
  (cons 1307 cur_focus_gg_text);_集中标注箍筋文本.
  )
)

  
  (setq errortext "")
  ;;;;
  
  (if checkbeam-user
    (progn
      	(setq catchit (vl-catch-all-apply 'checkbeam-user (list wjm-beam-info-lst)));_执行用户自定义程序时遇到错误.
        (if(vl-catch-all-error-p catchit) (progn (princ "\n执行用户自定义程序时遇到错误.:")(princ (vl-catch-all-error-message catchit))))

      (checkbeam-user wjm-beam-info-lst)
      
      )
    )
  ;;;;
  (if (and tl tm tr bl bm br gjl gjr)
    (progn
      (cond
	((eq 检查模式 1)
	 (setq bl (max bl bm br)
	       bm bl
	       br bl
	 )
	 
	) ;_(eq 检查模式 1)
	((eq 检查模式  2)
	 (setq mid (max bl bm br)
	       bl  tl
	       bm  tm
	       br  tr
	       tl  mid
	       tm  mid
	       tr  mid
	 )
	) ;_(eq 检查模式 2)***

      ) ;_cond

      (progn
;;;判断箍筋
        (if ggjr ggjr (setq ggjr(caaddr wjm_cur_focus_text)))
	(if ggjl ggjl (setq ggjl(cadr(caddr wjm_cur_focus_text))))
	(if (and ggjr
		 ggjl
		 (or (if (< ggjl gjl)
		       (setq errortext "■")
		       (progn (setq errortext "□") nil)
		     ) ;_加密区
		     (if (< ggjr gjr)
		       (setq errortext (strcat errortext "■"))
		       (progn (setq errortext(strcat errortext "□")) nil)
		     )
		 )
	    ) ;_非加密区
	  (progn 	    
	      (myprinc "Error:实配箍筋不足!")
	      (myprinc (list ttl (* 1 tl)))
	      (setq
		errortext (strcat  errortext":箍筋不足!\n加密区:"
				  (rtos ggjl 2 2)
				  "\t计算:"
				  (rtos gjl 2 2)
				  "\n非加密区:"
				  (rtos ggjr 2 2)"\t计算:"(rtos gjr 2 2)
			  )
	      )(printerror)(vla-put-Color (vlax-ename->vla-object (entlast)) acYellow)
	  );_progn
	  
	);_if
;;;判断上部筋
(if (if ttm ttm (setq ttm(caadr wjm_cur_focus_text)))
	  (if (< ttm (* wjm_check_diet tm))
	    (progn (myprinc "Error:实配钢筋不足!")
		   (myprinc (list ttm (* wjm_check_diet tm)))
		   (setq
		     errortext (strcat "□■□梁面通长筋:实配钢筋不足!\n实配"
				       (rtos ttm)
				       "\n计算值:"
				       (rtos tm)
			       )
		   )(printerror)
	    )
	  )
	)


	(if (if ttl ttl (setq ttl ttm))
	  (if (< ttl (* wjm_check_diet tl))
	    (progn
	      (myprinc "Error:实配钢筋不足!")
	      (myprinc (list ttl (* wjm_check_diet tl)))
	      (setq
		errortext (strcat "■□□梁面左:实配钢筋不足!\n实配"
				  (rtos ttl)
				  "\n计算值:"
				  (rtos tl)
			  )
	      )(printerror)
	    )
	  )
	)
	
	(if (if ttr ttr (setq ttr ttm))
	  (if (< ttr (* wjm_check_diet tr))
	    (progn
	      (myprinc "Error:实配钢筋不足!")
	      (myprinc (list ttr (* wjm_check_diet tr)))
	      (setq
		errortext (strcat "□□■梁面右:实配钢筋不足!\n实配"
				  (rtos ttr)
				  "\n计算值:"
				  (rtos tr)
			  )
	      )(printerror)
	    )
	  )
	)
;;;判断下部纵筋.

	(if
	  (if bbm
		bbm
		(setq bbm (cadadr wjm_cur_focus_text))
	      )	    
	  (progn
	    (if	(eq wjm_beam_single_bottom "1")
	      (progn		
		(setq bbm (max bbm (if bbl bbl 0) (if bbr bbr 0))
		      bbl nil
		      bbr nil
		)
	      )
	    ) ;_if
	  ) ;_progn
	) ;_if
	
        (if bbm
	  (if (< bbm (* wjm_check_diet bm))
	    (progn (myprinc "Error:实配钢筋不足!")
		   (myprinc (list bbm (* wjm_check_diet bm)))
		   (setq
		     errortext
		      (strcat "△▲△梁底通长筋:实配钢筋不足!\n实配"
			      (rtos bbm)
			      "\n计算值:"
			      (rtos bm)
		      )
		   )(printerror)
	    )
	  )
	)
	(if bbl
	  (if (< bbl (* wjm_check_diet bl))
	    (progn (myprinc "Error:实配钢筋不足!")
		   (myprinc (list bbl (* wjm_check_diet bl)))
		   (setq
		     errortext
		      (strcat "▲△△梁底左:实配钢筋不足!\n实配"
			      (rtos bbl)
			      "\n计算值:"
			      (rtos bl)
		      )
		   )(printerror)
	    )
	  )
	)
	
	(if bbr
	  (if (< bbr (* wjm_check_diet br))
	    (progn (myprinc "Error:实配钢筋不足!")
		   (myprinc (list bbr (* wjm_check_diet br)))
		   (setq errortext
			  (strcat "△△▲梁底右:实配钢筋不足!\n实配"
				  (rtos bbr)
				  "\n计算值:"
				  (rtos br)
			  )
		   )(printerror)
	    )
	  )
	)
      ) ;_progn
    ) ;_progn
    (if (and (eq wjm_lose_jsj "1") (or tl tm tr bl bm br gjl gjr))
        (progn(setq errortext (strcat "部分计算书无法获得"))(printerror))
      )
  ) ;_if (and tl tm tr bl bm br)

(if (eq wjm_debug "1")
     (progn
       (setq debug_color 253)
       (command "circle" (car pt_pair) 500 )
       (vla-put-Color (vlax-ename->vla-object (entlast)) debug_color)
       (mySetXData
	 (vlax-ename->vla-object (entlast))
	 (strcat "INFO:\n梁面实配"
		 (if ttl(rtos ttl ) "无")
		 "\t计算值:"
		 (if tl(rtos tl) "无")
		 "\n梁底实配:"
		 (if bbl (rtos bbl) "无")
		 "\t计算值:"
		 (if bl (rtos bl) "无")
	 )	 
       )
       (command "circle" (cadr pt_pair) 1000 )
       (vla-put-Color (vlax-ename->vla-object (entlast)) debug_color)
       (mySetXData
	 (vlax-ename->vla-object (entlast))
	 (strcat "INFO:\n梁面实配"
		 (if ttr(rtos ttr) "无")
		 "\t计算值:"
		 (if tr(rtos tr) "无")
		 "\n梁底实配:"
		 (if bbr (rtos bbr) "无")
		 "\t计算值:"
		 (if br (rtos br) "无")
	 )
       )
       (command "circle" (midpt (car pt_pair) (cadr pt_pair)) 800 )
       (vla-put-Color (vlax-ename->vla-object (entlast)) debug_color)
       (mySetXData
	 (vlax-ename->vla-object (entlast))
	 (strcat "INFO:\n梁面实配"
		 (if ttm (rtos ttm) "无")
		 "\t计算值:"
		 (if tm (rtos tm) "无")
		 "\n梁底实配:"
		 (if bbm (rtos bbm) "无")
		 "\t计算值:"
		 (if bm (rtos bm) "无")
		 "\n截面:"
		 (if beam-b (rtos beam-b) "无")
		 "X"
		 (if beam-h (rtos beam-h) "无")
	 )
       )
       (command "circle" (midpt (car pt_pair) (cadr pt_pair)) 400 )
       (vla-put-Color (vlax-ename->vla-object (entlast)) debug_color)
       (mySetXData
	 (vlax-ename->vla-object (entlast))
	 (strcat "INFO:\n集中标注"
		 (if ttm (rtos ttm) "无")
		 "\t计算值:"
		 (if tm (rtos tm) "无")
		 "\n梁底实配:"
		 (if bbm (rtos bbm) "无")
		 "\t计算值:"
		 (if bm (rtos bm) "无")
	 )
       )
       

     ) ;_progn

    );_if (eq wjm_debug "1")


  

);_defun checkvar


;(setq mid (read-ini-file))
(defun read-ini-file(/  mid)
  (setq ret "")
  (if (setq ini_filepath(findfile "checkbeam.ini"))
    (progn
      (setq ini_file(open ini_filepath "r"))
      (while (setq rule_text (read-line ini_file))
	(if (/= (setq mid (rule-resolve rule_text))"")
	  (progn ;(princ "\nrule_text:")
		 ;(princ mid)
	    (setq ret (strcat ret mid))
	  )
	)
	)
      )
    );_if
  (setq ret (strcat "(defun checkvar ( / value_list ) " ret  " value_list)"))
  ;(princ ret)
  (setq ret (eval(read ret)))
  ret
  )
;处理一行的信息.
(defun rule-resolve(rule_text / ret  +-*/
		    rule_lisp rule_text_lisp mid midstr)
  ;权重增加函数.
  ;(value-add "left")
  (defun value-add (ret)
  (if (assoc ret value_list)
    (progn
      (setq value_list (subst (cons ret (1+ (cdr(assoc ret value_list))))(assoc ret value_list)value_list))
      )
    (progn
      (setq value_list (append value_list (list (cons ret 1))))
      )
    );_if
    );_defun value-add.
  ;(+-*/ "(5-7)-5*(6+8)*7+(9+6)*7")
  ;(+-*/ "5*6-5+7/(9*5.0)/8")

  (setq rule_lisp "" );_以lisp形式保存的规则语句.
  (setq rule_text(car(convert_string_to_string_lst rule_text ";")))
  (setq rule_text_list (convert_string_to_string_lst rule_text ","))
  (setq ret nil)
  (foreach rule_text rule_text_list
    (cond;
      ((wcmatch rule_text "`[*`]")(setq ret (strcat "\"" (substr rule_text 2 (-(strlen rule_text)2)) "\""))) 
      (t
       (if ret
       (setq rule_lisp (strcat rule_lisp "(if " rule_text "(value-add " ret "))" )
	     
       )
	 (setq rule_lisp (strcat rule_lisp  rule_text))
       )
	 )
    )
  )
  rule_lisp
)
;分析生成梁段左右坐标对.
(defun get_beam_pt_lst(
       wjm_paxis_colu_list wjm_paxis_wall_list / pt_pair_lst i% j% ptleft ptright ptleft_wall
		       ptright_wall ptmid pt_lst pt_pair_lst)
  
  (setq pt_pair_lst nil)
  (setq j% 0);_墙的id
  (FOREACH wjm_colu_id wjm_paxis_colu_list
    (setq ptmid (cdadr (assoc wjm_colu_id wjm_colu_list)))
    (setq ptmid (wjmf_shadow ptmid wjm_pt_paxis_left wjm_pt_paxis_right))
    (setq pt_lst(append pt_lst (list(list 0 ptmid))))
    )
  (FOREACH wjm_wall_id wjm_paxis_wall_list
    (setq j% (1+ j%))
    (setq ptleft_wall (cdadr(assoc wjm_wall_id wjm_wall_list)))    
    (setq ptleft_wall (wjmf_shadow ptleft_wall wjm_pt_paxis_left wjm_pt_paxis_right))
    (setq pt_lst(append pt_lst (list(list j% ptleft_wall))))
    
    (setq ptright_wall (cdaddr(assoc wjm_wall_id wjm_wall_list)))
    (setq ptright_wall (wjmf_shadow ptright_wall wjm_pt_paxis_left wjm_pt_paxis_right))
    (setq pt_lst(append pt_lst (list(list j% ptright_wall))))
    )
  ;;;排序.
  (setq	pt_lst
	 (vl-sort
	   pt_lst
	   (function
	     (lambda (e1 e2) ; (cadr(list 0 ptmid))
	       (if (< (abs (- (car (cadr e1)) (car (cadr e2)))) wjm_Y_diet) ;_如果X坐标相等,则按照Y坐标排列
		 (< (cadr (cadr e1)) (cadr (cadr e2)))
		 (< (car (cadr e1)) (car (cadr e2)))
	       )
	     )
	   )
	 )
  )
  ;;;组队.
  
  (setq i% 0);_柱的id
  (repeat (1-(length pt_lst))
    (setq ptleft (nth i% pt_lst))
    (setq ptright (nth (1+ i%) pt_lst))
    (if (and (/= (car ptleft) 0) (eq (car ptleft) (car ptright)))
      (progn (1+ 1))
      (progn
	(setq pt_pair_lst(append pt_pair_lst (list (list (cadr ptleft) (cadr ptright)))))
	)
    )

    (setq i% (1+ i%))
    )
  pt_pair_lst
)

;;;(defun get_beam_pt_lst(
;;;       wjm_paxis_colu_list wjm_paxis_wall_list / pt_pair_lst )
;;;  
;;;  (setq i% 0);_柱的id
;;;  (setq j% 0);_墙的id
;;;  (if wjm_paxis_wall_list
;;;  (repeat (1- (length wjm_paxis_colu_list))
;;;    (setq wjm_colu_id (nth i% wjm_paxis_colu_list))
;;;    (setq ptleft (cdadr (assoc wjm_colu_id wjm_colu_list)))
;;;    (setq ptleft (wjmf_shadow ptleft wjm_pt_paxis_left wjm_pt_paxis_right))
;;;    
;;;    (setq wjm_colu_id (nth (1+ i%) wjm_paxis_colu_list))
;;;    (setq ptright (cdadr (assoc wjm_colu_id wjm_colu_list)))
;;;    (setq ptright (wjmf_shadow ptright wjm_pt_paxis_left wjm_pt_paxis_right))
;;;
;;;    (if (nth j% wjm_paxis_wall_list)
;;;      (progn
;;;    (setq wjm_wall_id (nth j% wjm_paxis_wall_list))
;;;    (assoc wjm_wall_id wjm_wall_list)
;;;    (setq ptleft_wall (cdadr(assoc wjm_wall_id wjm_wall_list)))
;;;    
;;;    (setq ptleft_wall (wjmf_shadow ptleft_wall wjm_pt_paxis_left wjm_pt_paxis_right))
;;;    
;;;    (setq ptright_wall (cdaddr(assoc wjm_wall_id wjm_wall_list)))
;;;    (setq ptright_wall (wjmf_shadow ptright_wall wjm_pt_paxis_left wjm_pt_paxis_right))
;;;     );_progn
;;;      (setq ptleft_wall nil
;;;	    ptright_wall nil)
;;;      );_if
;;;    (if (and ptleft_wall ptright_wall (wjmf_pt_little_then ptleft ptleft_wall)
;;;      (wjmf_pt_big_then ptright ptright_wall))
;;;      (progn
;;;      (setq pt_pair_lst(append pt_pair_lst (list(list ptleft ptleft_wall))))
;;;      (setq pt_pair_lst(append pt_pair_lst (list(list ptright_wall ptright))))
;;;      (setq j% (1+ j%))
;;;      );_progn
;;;      (progn
;;;	(setq pt_pair_lst(append pt_pair_lst (list(list ptleft ptright))))
;;;	)      
;;;      );_if
;;;    (setq i% (1+ i%))
;;;    );_repeat
;;;    (progn
;;;      (repeat (1- (length wjm_paxis_colu_list))
;;;	  (setq wjm_colu_id (nth i% wjm_paxis_colu_list))
;;;          (setq ptleft (cdadr (assoc wjm_colu_id wjm_colu_list)))
;;;	  (setq ptleft (wjmf_shadow ptleft wjm_pt_paxis_left wjm_pt_paxis_right))
;;;          (setq wjm_colu_id (nth (1+ i%) wjm_paxis_colu_list))
;;;          (setq ptright (cdadr (assoc wjm_colu_id wjm_colu_list)))
;;;	  (setq ptright (wjmf_shadow ptright wjm_pt_paxis_left wjm_pt_paxis_right))
;;;	
;;;          (setq pt_pair_lst(append pt_pair_lst (list(list ptleft ptright))))
;;;	(setq i% (1+ i%))
;;;	);_repeat
;;;
;;;      );_progn
;;;    );_if  
;;;
;;;  pt_pair_lst
;;;)


(defun wjmf_pt_little_then (pta ptb)
  (equal (wjmf_sort (list pta ptb)) (list pta ptb))
  )


(defun wjmf_pt_big_then (pta ptb)
(equal (wjmf_sort (list pta ptb)) (list ptb pta))
  )

;;;下面这个函数没有用了.
;更新集中标注的文本内容.
(defun update_focus_dim	(/)
  (if wjm_cur_focus_text
    (progn ;_已经有了集中标注的定义了,现在是判断要不要更新.
      (if (and (eq wjm_focus_text "1")
	       wjm_focus_dimline_lst
	       (= (car wjm_cur_focus_text) wjm_cur_kl_num)
	  ) ;_判断梁编跨数是不是达到总跨数了
	(setq wjm_cur_focus_text
	       (get_focus_dimtext
		 (car (car wjm_focus_dimline_lst))
		 (cadr (car wjm_focus_dimline_lst))
	       )
	      wjm_focus_dimline_lst
	       (cdr wjm_focus_dimline_lst)
	      wjm_cur_kl_num
	       1
	)
	;_如果还没有达到总的跨数,则增加跨计数.
	(setq wjm_cur_kl_num (1+ wjm_cur_kl_num))
      )
    )
    (progn ;_没有集中标注的定义,进行初始化.
      (if (and (eq wjm_focus_text "1")
	       (setq wjm_focus_dimline_lst
		      (get_focus_dim_line
			wjm_pt_paxis_left
			wjm_pt_paxis_right
		      )
	       )
	  )
	(progn
	  (setq	wjm_cur_focus_text
		 (get_focus_dimtext
		   (car (car wjm_focus_dimline_lst))
		   (cadr (car wjm_focus_dimline_lst))
		 )
	  ) ;_集中标注文本内容cur_focus_klnum cur_focus_jsj cur_focus_gg  cur_focus_bianhao cur_focus_bh
	  (setq wjm_focus_dimline_lst (cdr wjm_focus_dimline_lst))
	)
	(setq wjm_cur_focus_text
	       (list nil
		     (list nil nil)
		     (list nil nil)
		     nil
		     (list nil nil)
	       )
	)
      ) ;_if
    ) ;_progn
  )
)
;_该函数执行智能选择的方式,如果选择不到,则自动改变选择方式.
;(get_focus_dim_line)
(defun get_focus_dim_line ( / foucs_ss ent en pt_text dim_line paxis_line)
    (setq foucs_ss (ssget "C" pt_first pt_second (list (cons 0 "TEXT") (cons 1 wjm_梁编号))))

  (if foucs_ss
    (progn
      
      (setq progress_index 0)
      (setq list_length (sslength foucs_ss))
      (princ "\r集中标注处理过程...\n")
      (while (setq en (ssname foucs_ss 0))
	
	(myprogress (setq progress_index (1+ progress_index)) list_length)
	(setq focus_text (ssdel en foucs_ss))
	(setq ent (entget en))
	(setq text_pt (cdr (assoc 10 ent)))
	(setq text_ang (cdr (assoc 50 ent)))
	(setq text_height (cdr (assoc 40 ent)))
	(setq text_height 300)
	(setq text_layer (cdr (assoc 8 ent)))
	
	(setq pt_mid (polar text_pt (+ pi text_ang) wjm_focus_line_dis))
	(if(null(setq dim_line (ssget "F" (list text_pt pt_mid) (list (cons 0 "LINE") (cons 8 text_layer)))))
	  (progn ;_改变选择方式.
	    (setq dim_line (ssget "F" (list text_pt pt_mid) (list (cons 0 "LINE"))))
	   )
	);_if
	(if dim_line
	  (progn
	    (setq dim_line_en (ssname dim_line 0));_这里要再考虑一下.
	    (setq dim_line_ent (entget dim_line_en))
	    (setq dim_line_pt1 (cdr (assoc 10 dim_line_ent)))
	    (setq dim_line_pt2 (cdr (assoc 11 dim_line_ent)))
	    (setq dim_line_angle (angle dim_line_pt1 dim_line_pt2))
	    (setq dim_line_hand (cdr (assoc 5 dim_line_ent)))

	    (progn(setq dis1(distance dim_line_pt1 text_pt))
	        (setq dis2(distance dim_line_pt2 text_pt)))

	    (if (< dis1 (* text_height 1.2))
	      (setq pt_mid dim_line_pt2)
	      (if (< dis2 (* text_height 1.2))
		(setq pt_mid dim_line_pt1)
		;_这种情况是引线往上面伸.
		(setq pt_mid (polar (midpt dim_line_pt1 dim_line_pt2) (+ (/ pi 2) text_ang) (distance pt_mid dim_line_pt1)))
		)
	      )
	    (if (eq wjm_debug "2")
	    (entmakex
	      (list (cons 0 "CIRCLE")
		    (cons 8 "wujimmy_Error")
		    (cons 40 500)
		    (cons 10 pt_mid)
		    (list -3 (list "PE_URL" (cons 1000 "pt_mid")))
	      )
	    ))
	    
	    ;;
	    ;;此处再次进行判断,对于那些引线引了两次的集中标注再次进行处理.
	    ;;

	    ;;以下内空为"搜索"轴线.
	    (setq i% 1)
            (while pt_mid
	    (setq pt_mid_min (polar pt_mid dim_line_angle;|(* 1.25 pi)|; (* i% wjm_focus_line_dis)))
	    (setq pt_mid_max (polar pt_mid (+ pi dim_line_angle) ;|(* 0.25 pi)|; (* i% wjm_focus_line_dis)))
	    (setq paxis_line
		   (ssget "F"
			  (list pt_mid_min pt_mid_max)
			  (list (cons 0 "LINE")(cons 8 wjm_paxis_layer))
		   )
	    );_如果选择不到,则扩大选择范围.
	      (if paxis_line
		(setq pt_mid nil)
		(setq i% (1+ i%)))
	      (if (> i% 4)(setq pt_mid nil) )
	      );_while
	    ;;搜索轴线结束.
	    ;;如果搜索到轴线,则进一步进运处理.
	    (if	paxis_line
	      (progn (setq paxis_line_en (ssname paxis_line 0)) ;_这里要再考虑一下.
		     (setq paxis_line_ent (entget paxis_line_en))		     
		;;增加集中标注线到轴线信息里面去.
;;;		(setq paxis_mid (assoc (strcat "z:" "BDE") wjm_paxis_list))
		(setq paxis_mid1 (assoc (strcat "z:" (cdr (assoc 5 paxis_line_ent))) wjm_paxis_list))
		(setq paxis_mid2 (assoc (strcat "c:" (cdr (assoc 5 paxis_line_ent))) wjm_paxis_list))
		(if paxis_mid1 (setq paxis_mid paxis_mid1))
		(if paxis_mid2 (setq paxis_mid paxis_mid2))
		(if paxis_mid
		  (progn
		    (if (null(setq dim_line_hand_list (cdr(assoc 2 (cdr paxis_mid)))))
		      ;;原来没有集中标注线.
		      (progn
			(setq wjm_paxis_list
			       (subst
				 (list
				     (car paxis_mid) ;_轴线编号.
				     (cadr paxis_mid) ;_梁文字列表.
				     (cons 2 (list dim_line_hand)) ;_集中标列表.
				 ) ;_new
				 paxis_mid ;_old
				 wjm_paxis_list
			       )
			)
			);_progn
		      ;;原来有集中标注线.
		      (progn
			(setq wjm_paxis_list
			       (subst
				 (list
				     (car paxis_mid) ;_轴线编号.
				     (cadr paxis_mid) ;_梁文字列表.
				     (cons 2
					   (append dim_line_hand_list
						   (list dim_line_hand))
					   ) ;_集中标列表.
				 ) ;_new
				 paxis_mid ;_old
				 wjm_paxis_list
			       )
			)
			);_progn
		      );if
		   );_progn
		 );_if paxis_mid
	      )
	    ) ;_if
	    
	      );_progn
	    );_if dim_line
	   );_while (setq en (ssname foucs_ss 0))
	 );_progn
	
      );_if focus ss

  );_defun


;;;根据梁的两端点获得梁的集中标注文字.
;;;表的内容是:(跨数, (list 纵筋面上通长配筋面积 [底部通长筋] ) (list 箍筋面积加密区 非加密区) 梁编号 (list 梁宽 梁高) )
;;;(list cur_focus_klnum cur_focus_jsj cur_focus_gg  cur_focus_bianhao cur_focus_bh)

;(get_focus_dimtext(car (car wjm_focus_dimline_lst))(cadr (car wjm_focus_dimline_lst)))
;(get_focus_dimtext (car(entsel)))
(defun get_focus_dimtext ( en / textlayer ptleft ptright ent)
  (setq ent (entget en))
  (setq ptleft (cdr (assoc 10 ent)))
  (setq ptright (cdr (assoc 11 ent)))
  (setq dim_line_ang (angle ptleft ptright))
  (setq textlayer (cdr (assoc 8 ent)))
  
(setq listtext nil)
  
(setq con1 (polar ptleft (+ dim_line_ang (/ pi 2)) (/ wjm_left_or_right_diet_MID 3)))
(setq con2 (polar ptleft (- dim_line_ang (/ pi 2)) (/ wjm_left_or_right_diet_MID 3)))
(setq con3 (polar ptright (+ dim_line_ang (/ pi 2)) (/ wjm_left_or_right_diet_MID 3)))
(setq con4 (polar ptright (- dim_line_ang (/ pi 2)) (/ wjm_left_or_right_diet_MID 3)))
(setq conlist_dimtext (list con1 con3 con4 con2 con1))
;;;  (draw-pl conlist_dimtext)
(setq cur_focus_klnum nil cur_focus_jsj nil cur_focus_gg  nil cur_focus_bianhao nil cur_focus_bh nil)
  
  (setq filter_lisp_list
	       (list   (cons 0 "*TEXT")
		       (cons 8 textlayer)
		     )
	);(draw-pl conlist_dimtext)
  (setq sstext (ssget "CP" conlist_dimtext filter_lisp_list))
  ;_集中标注文本内容cur_focus_klnum cur_focus_jsj cur_focus_gg  cur_focus_bianhao cur_focus_bh
  (while (if sstext (setq ent (ssname sstext ca_midt))) ;_ca_midt中带有验证信息
    (setq sstext (ssdel ent sstext))
    (setq ent (entget ent))
    (setq textstr (cdr(assoc 1 ent)) )
    (cond
      ;(setq textstr "JZLLL(2) 300x700")
      ((wcmatch textstr "G*,N*");_构造钢筋.
       )
      ((wcmatch textstr wjm_梁编号)
	(if (eq (length (setq mid (str-to-area textstr))) 4)
	  (progn
	    (setq cur_focus_bianhao (car mid)) ;_梁编号
	    (setq cur_focus_klnum (atoi (cadr mid))) ;_该集中标注中的梁总跨数
	    (setq cur_focus_bh
		   (list (atoi (caddr mid)) (atoi (cadddr mid)))
	    ) ;_梁的截面
	  )
	  (if (eq (length mid) 5)
	    (progn
	      (setq cur_focus_bianhao (cadr mid)) ;_梁编号
	      (setq cur_focus_klnum (atoi (caddr mid))) ;_该集中标注中的梁总跨数
	      (setq cur_focus_bh
		     (list (atoi (cadddr mid)) (atoi (cadddr (cdr mid))))
	      ) ;_梁的面
	      )
	      (progn ;|只有梁的集中标注,没有配筋|;
		(setq cur_focus_bianhao (car mid)) ;_梁编号
		(setq cur_focus_klnum (atoi (cadr mid))) ;_该集中标注中的梁总跨数
;;;(princ mid)
	      )
	    )
	  ) ;_if
	) ;_wcmatchwjm_梁编号 (setq textstr "2%%13218;3%%13218")(setq textstr "B2%%13218;T3%%13218")(setq textstr "2%%13218")
	((wcmatch textstr wjm_箍筋)
	  (setq	cur_focus_gg_text textstr
		cur_focus_gg
		 (str-to-area textstr)
	  )
	)
	((wcmatch textstr wjm_纵筋集中标注)
	  (if (wcmatch textstr "B#*;T#*")
	    (progn (foreach strmid
			    (convert_string_to_string_lst textstr ";")
		     (setq cur_focus_jsj
			    (append cur_focus_jsj
				    (list (str-to-area strmid))
			    )
		     )
		     (setq cur_focus_jsj (reverse cur_focus_jsj))
		   )
	    ) ;_progn
	    (foreach strmid (convert_string_to_string_lst textstr ";")
	      (setq cur_focus_jsj
		     (append cur_focus_jsj
			     (list (str-to-area strmid))
		     )
	      )
	    )
	  ) ;_if
	) ;_(wcmatch textstr wjm_纵筋集中标注)
      ) ;_cond
    ;(setq listtext (append listtext (list (list(cdr(assoc 10 ent)) (cdr(assoc 11 ent))))))
    
    );_while
  (list cur_focus_klnum cur_focus_jsj cur_focus_gg  cur_focus_bianhao cur_focus_bh cur_focus_gg_text) 
  );_defun get_focus_dimtext

(setq wjm-beam-info-lst
  (list
  (cons 10 wjm_pt_paxis_left);_梁左节点.
  (cons 11 wjm_pt_paxis_right);_梁右节点.
  (cons 40 beam-b)(cons 41 beam-h);_梁宽,梁长.
  (cons 101 tl)(cons 102 tm)(cons 103 tr)(cons 104 bl)(cons 105 bm)(cons 106 br)(cons 107 gl)(cons 108 gr)
  (cons 201 ttl)(cons 202 ttm)(cons 203 ttr)(cons 204 bbl)(cons 205 bbm)(cons 206 bbr)(cons 207 ggl)(cons 208 ggr)
  (cons 301 tttl)(cons 302 tttm)(cons 303 tttr)(cons 304 bbbl)(cons 305 bbbm)(cons 306 bbbr)(cons 207 gggj)
  (cons 401 ttttl)(cons 402 ttttm)(cons 403 ttttr)(cons 404 bbbbl)(cons 405 bbbbm)(cons 406 bbbbr)(cons 207 ggggj)
  
  (cons 1001 (caadr wjm_cur_focus_text)) (cons 1002 (cadadr wjm_cur_focus_text));_梁面,梁底通长筋.
  (cons 1003 (caaddr wjm_cur_focus_text)) (cons 1004 (car(cdaddr wjm_cur_focus_text)));_梁集中标注箍筋.
  (cons 1007 "预留抗扭筋")
  (cons 1008 "预留腰筋")
  (cons 1009 (cadddr wjm_cur_focus_text));_梁编号.
  (cons 1040 (caar(cddddr wjm_cur_focus_text)));_梁宽
  (cons 1041 (cadar(cddddr wjm_cur_focus_text)));_梁长.
  (cons 1307 cur_focus_gg_text);_集中标注箍筋文本.
  )
)

(defun get-dxf (dxf)
  (cdr(assoc dxf wjm-beam-info-lst))
  )

(defun set-dxf(dxf value)
  (if (assoc dxf wjm-beam-info-lst)
    (progn
      (setq wjm-beam-info-lst (subst (cons dxf value) (assoc dxf wjm-beam-info-lst) wjm-beam-info-lst))
      )
    (progn
      (setq wjm-beam-info-lst (append wjm-beam-info-lst (list (cons dxf value))))
      )
    );_if
  )

;;;说明, 组码8是指实体被使用的次数.
;;;组码21,22分别表示轴线编号和梁编号.
(defun wjm-add-ent (ent3 / mid )
;;;填实体表.
  		  ;((cons 21 wjm_cur_paxis_index))
		  ;(cons 22 wjm_cur_beam_index)
  (if (setq mid (assoc (cdr (assoc 5 ent3)) wjm_ent_list))
    (progn
      (setq wjm_ent_list
	     (subst (list (cdr (assoc 5 ent3))
			  (cons 8 (1+ (cdr (assoc 8 (cdr mid)))))
			  (cons 9 (append (cdr (assoc 9 (cdr mid)))
				(list(list (cons 21 wjm_cur_paxis_index)
			  (cons 22 wjm_cur_beam_index)))))
			  )

		    mid
		    wjm_ent_list
	     )
      )
    ) ;_progn
    (setq wjm_ent_list
	   (append wjm_ent_list
		   (list (list (cdr (assoc 5 ent3)) (cons 8 1)  (cons 9 (list(list (cons 21 wjm_cur_paxis_index)
			  (cons 22 wjm_cur_beam_index)))    ) ))
	   )
    )
  ) ;_if

)

;((11 (ent1 ent2 ent3)) (21 (ent4 ent5 ent6)))
;(setq test nil)
;(build-list 12 (list 10 11 12) "test")
(defun build-list (dxf value list-name /)
  (eval (read (strcat "(setq mid " list-name ")")))
  (if (assoc dxf mid)
    (progn
      (setq mid (subst (cons dxf(append (cdr(assoc dxf mid))(list value)) )(assoc dxf mid) mid))
    )
    (progn
      (setq mid (append mid (list (cons dxf (list value)))))
    )
  )
  (eval (read (strcat "(setq " list-name " mid)")))
)
;;;获得一根轴线与其他轴线的交点,前提以交点这间的线段设梁
;;;输入轴线line的entget ,以这个线的端点Fence到的线的交点均为要找的点,
;;;输出这些点的表
(defun get-abeam-int-point (en / ent pt-mid ang-mid ss2 ss2len sscolu findcolu jiaodian listzxjd pt1 pt2 pta ptb ent2 j% ent ptleft ptright)
  (setq ent (entget en))
  (setq ss2len nil
	listzxjd nil
	jiaodian nil
	ss2 nil)
  ;;;(setq paxis_ang (cdr (assoc 50 ent)))
  (setq pt1 (cdr (assoc 10 ent)))
  (setq pt2 (cdr (assoc 11 ent)))
;;;  (setq pt-mid (midpt pt1 pt2))
  (setq ang-mid (angle pt1 pt2))
;;;  (setq dis-mid (+(distance pt1 pt2) 1000))
  (setq pt1 (polar pt1 (+ pi ang-mid) 1000))
  (setq pt2 (polar pt2 (+ 0 ang-mid) 1000))
  ;;;pt1,pt2分别向两个方向延升1000,针对一些轴线与轴线
  
  (setq	ss2 (ssget "F"
		   (list pt1 pt2)
		   (list (cons 0 "LINE") (cons 8 "wjm_beam"))
	    )
  )
  (if ss2
    (progn
      (setq ss2len (sslength ss2)
	    j%	   (fix(rem ca_midt ss2len));_验证
      )
      (repeat ss2len ;_列表最后一个项是不是轴线本身了,因为图层变了.
	(setq ent2 (entget (ssname ss2 j%))) 
	(setq pta (cdr (assoc 10 ent2)))
	(setq ptb (cdr (assoc 11 ent2)))
	(setq jiaodian (inters pt1 pt2 pta ptb nil))
	;;调试开关,查看所选择轴线的交点是否正确.
	(if (eq wjm_debug "2")
	  (progn
	    (command "line" pt1 pt2 "")
	    (command "line" pta ptb ""))
	)
	(if jiaodian
	  (setq listzxjd (append listzxjd (list jiaodian)))
	)
	(setq j% (1+ j%))
      ) ;_repeat

      	;;;排序
	(setq listzxjd (vl-sort	listzxjd
				(function (lambda (e1 e2)
					    (if	(< (abs(- (car e1) (car e2))) wjm_Y_diet );_如果X坐标相等,则按照Y坐标排列
					      (< (cadr e1) (cadr e2))
					      (< (car e1) (car e2))
					    )
					  )
				)
		       )
	)

    ) ;_progn

  ) ;_if

listzxjd
)
;(setq ent entt)

(defun getcolulist (en / ENT	    PT1		  PT2
		      PT-MID	    ANG-MID	  DIS-MID		      	  
		      MID	    SS2SETQ	  SS2
		      SS2LEN	    J%		  ENT2
		      WJM_PAXIS_COLU_LIST	  J%
		      COLUCENTER    COLUBIANHAO	  
		     )
  (setq ent (entget en))
  (setq pt1 (cdr (assoc 10 ent)))
  (setq pt2 (cdr (assoc 11 ent)))
  (if (if (equal (car pt1) (car pt2) wjm_Y_diet)
	(< (cadr pt1) (cadr pt2))
	(< (car pt1) (car pt2))
      )
    (progn (setq mid pt1) (setq pt1 pt2) (setq pt2 mid))
  )


  (setq pt-mid (midpt pt1 pt2))
  (setq ang-mid (angle pt1 pt2))
  (setq dis-mid (+ (distance pt1 pt2) 1000))
  (setq pt1 (polar pt-mid ang-mid dis-mid))
  (setq pt2 (polar pt-mid (+ pi ang-mid) dis-mid))
  (setq wjm_paxis_colu_list nil)
;;;pt1,pt2分别向两个方向延升1000,针对一些轴线与轴线.




  (setq	ss2 (ssget "F"
		   (list pt1 pt2)
		   ;(list (cons 0 "*LINE") (cons 8 wjm_colu_layer))
		   (list (cons 0 "LWPOLYLINE")(cons 8 wjm_colu_layer))
	    )
  )
  (if ss2
    (progn
      (setq ss2len (sslength ss2)
	    j%	   0
      )
      (repeat (- ss2len j%) ;_列表最后一个项是轴线本身
	(setq ent2 (entget (ssname ss2 j%)))
	(cond
	  ((eq "LINE" (cdr (assoc 0 ent2))) ;_由四根线画成的柱子.
	  )
	  ((eq "LWPOLYLINE" (cdr (assoc 0 ent2))) ;_由PL线画成的柱子.
	   (progn
	     (if (or (= (cdr (assoc 90 ent2)) 4)
		     (= (cdr (assoc 90 ent2)) 5)
		 )
	       (progn
		 ;;求柱形心.
		 (setq catchit (vl-catch-all-apply
				 'ax:Centroid
				 (list (ssname ss2 j%))
			       )
		 ) ;_设置变量并捕捉错误
		 (if (vl-catch-all-error-p catchit)
		   (progn
		     ;(princ "\n请确定柱子全部为闭合曲线:")
			  ;(princ (vl-catch-all-error-message catchit))
		     (entmakex
		     (list (cons 0 "CIRCLE")
			   (cons 8 "wujimmy_Error")
			   (cons 40 1000)
			   (cons 62 5);_蓝色.
			   (assoc 10 (entget(ssname ss2 j%)))
			   '(-3 ("PE_URL" (1000 . "该柱不是闭合线组成的,所以请检查确认无误")))
		     )
		     )
		     (setq colucenter (cdr(assoc 10 (entget(ssname ss2 j%)))) )
		     ;(mySetXData (vlax-ename->vla-object (entlast)) "该柱不是闭合线组成的,所以会被忽略掉")
		   )
		   (setq colucenter (ax:Centroid (ssname ss2 j%)))
		 );_if
		 ;;求柱形心.
		 
;;;		 (setq colucenter (ax:Centroid (ssname ss2 j%)))
		 
		 (setq colubianhao
			(strcat	(rtos (car colucenter) 2 0)
				":"
				(rtos (cadr colucenter) 2 0)
			)
		 )
		 
;;;如果柱列表中没有该柱信息,则增加该柱信息.
		 (if (and colucenter (null (assoc colubianhao wjm_colu_list)))
		   (progn (setq	wjm_colu_list
				 (append wjm_colu_list
					 (list (list colubianhao
						     (cons 10 colucenter) ;_中心点.
						     (list 40 nil nil) ;_截面形状.
					       )
					 )
				 )
			  )

		   )
		 ) ;_if
	       )


	     ) ;_if

	   ) ;_progn
	  )
	  ((eq "HATCH" (cdr (assoc 0 ent2))) ;_由填充画成的柱子.
	  )
	)
	(setq wjm_paxis_colu_list
	       (append wjm_paxis_colu_list
		       (list colubianhao)
	       )
	)
	(setq j% (1+ j%))
      ) ;_repeat

;;;排序
;;;      (setq
;;;	listzxjd (vl-sort
;;;		   listzxjd
;;;		   (function
;;;		     (lambda (e1 e2)
;;;		       (if (< (abs (- (car e1) (car e2))) wjm_Y_diet) ;_如果X坐标相等,则按照Y坐标排列
;;;			 (< (cadr e1) (cadr e2))
;;;			 (< (car e1) (car e2))
;;;		       )
;;;		     )
;;;		   )
;;;		 )
;;;      )



    ) ;_progn

  ) ;_if ss2

  wjm_paxis_colu_list
)


(defun getwalllist (en / ENT	  PT1	      PT2	  PT-MID
		      ANG-MID	  DIS-MID     PAXIS_WALL_LIST wallbianhao
		      SS2	  MID	      SS2
		      SS2LEN	  J%	      ENT2	  J% ptleft ptright 
		     )
  (setq ent (entget en)) 
  (setq pt1 (cdr (assoc 10 ent)))
  (setq pt2 (cdr (assoc 11 ent)))
  (if (if (equal (car pt1) (car pt2) wjm_Y_diet)
	(< (cadr pt1) (cadr pt2))
	(< (car pt1) (car pt2))
      )
    (progn (setq mid pt1)(setq pt1 pt2)(setq pt2 mid))
  )
  
  (setq pt-mid (midpt pt1 pt2))
  (setq ang-mid (angle pt1 pt2))
  (setq dis-mid (+ (/(distance pt1 pt2)2) 1000))
  (setq pt1 (polar pt-mid ang-mid dis-mid))
  (setq pt2 (polar pt-mid (+ pi ang-mid) dis-mid))
  (setq paxis_wall_list nil)
;;;pt1,pt2分别向两个方向延升1000,针对一些轴线与轴线.
  (setq	ss2 (ssget "F"
		   (list pt1 pt2)
		   (list (cons 0 "*LINE") (cons 8 wjm_wall_layer))
	    )
  )
  ;(printerror pt-mid "test") (draw-pl (list pt1 pt2))

  (if ss2
    (progn
      (setq ss2len (sslength ss2)
	    j%	   0 
      )
      (repeat (- ss2len j%) ;_历遍列表.
	(setq ent2 (entget (ssname ss2 j%)))
	(cond
	  ((eq "LINE" (cdr(assoc 0 ent2 )));_由四根线画成的柱子.
	   )
	  ((eq "LWPOLYLINE" (cdr(assoc 0 ent2 )));_由PL线画成的柱子.
	   (progn
	     ;;求轴线与PLINE(墙线)的交点.
	     (setq intersections nil)
	     (setq wjm_wall_pt (wjmf_Midp (ssname ss2 j%) en 1));_(0 都延伸,1 一号不延伸,2 二号不延伸,<3> 全部不延伸)
	     (setq wjm_wall_pt (wjmf_sort wjm_wall_pt))
	     ;;;debug
	     (if (eq wjm_debug "1");_(setq wjm_debug "1")
	       (foreach	temp_pt	wjm_wall_pt
		 (progn
		   (entmakex
		     (list (cons 0 "CIRCLE")
			   (cons 8 "wujimmy_Error")
			   (cons 40 500)
			   (cons 62 5);_蓝色.
			   (cons 10 temp_pt)
		     )
		   )
		 ) ;_progn
	       ) ;_foreach
	     ) ;_if

	     
	     (if (>= (length wjm_wall_pt) 2)
	       (progn
		 (setq ptleft (nth 0 wjm_wall_pt))
		 (setq ptright (nth (1- (length wjm_wall_pt)) wjm_wall_pt))
		 (setq wallbianhao
			(strcat	(rtos (car ptleft) 2 0)
				":"
				(rtos (cadr ptleft) 2 0)
				":"
				(rtos (car ptright) 2 0)
				":"
				(rtos (cadr ptright) 2 0)
			)
		 )
		 (if (null (assoc wallbianhao wjm_wall_list))
		   (progn (setq	wjm_wall_list
				 (append wjm_wall_list
					 (list (list wallbianhao
						     (cons 10 ptleft) ;_端点10.
						     (cons 11 ptright) ;_端点10.
						     (cons 43 nil) ;_截面宽度.
					       )
					 )
				 )
			  )
		   )
		 )
	       )
	     );_if
	   );_progn
	   )
	  ((eq "HATCH" (cdr(assoc 0 ent2 )));_由填充画成的柱子.
	   )
	  )
	(if wallbianhao(setq paxis_wall_list (append paxis_wall_list (list wallbianhao))))
	(setq j% (1+ j%))
      ) ;_repeat

    ) ;_progn

  ) ;_if ss2

paxis_wall_list
)

;;;(setq wjm_paxis_beam_list (getbeamlist en))

(defun getbeamlist (en / ENT	  PT1	      PT2	  PT-MID
		      ANG-MID	  DIS-MID     paxis_beam_list
		      SS2	  MID	      SS2
		      SS2LEN	  J%	      ENT2	  J%
		     )
  (setq ent (entget en)) 
  (setq pt1 (cdr (assoc 10 ent)))
  (setq pt2 (cdr (assoc 11 ent)))
  (if (if (equal (car pt1) (car pt2) wjm_Y_diet)
	(< (cadr pt1) (cadr pt2))
	(< (car pt1) (car pt2))
      )
    (progn (setq mid pt1)(setq pt1 pt2)(setq pt2 mid))
  )

  
  (setq pt-mid (midpt pt1 pt2))
  (setq ang-mid (angle pt1 pt2))
  (setq dis-mid (+ (distance pt1 pt2) 1000))
  (setq pt1 (polar pt-mid ang-mid dis-mid))
  (setq pt2 (polar pt-mid (+ pi ang-mid) dis-mid))
  (setq paxis_beam_list nil)
;;;pt1,pt2分别向两个方向延升1000,针对一些轴线与轴线.
(setq con1 (polar pt1 (+ ang-mid (/ pi 2.0)) wjm_left_or_right_diet))
(setq con2 (polar pt1 (- ang-mid (/ pi 2.0)) wjm_left_or_right_diet))
(setq con3 (polar pt2 (- ang-mid (/ pi 2.0)) wjm_left_or_right_diet))
(setq con4 (polar pt2 (+ ang-mid (/ pi 2.0)) wjm_left_or_right_diet))
  
  (setq	filter_lisp_list
	 (list
	   (cons 0 "*LINE")
	   (cons 8 wjm_beam_layer)
	 )
  )
  (setq ss2 nil)
  (setq ss2 (ssget "WP" (list con1 con2 con3 con4) filter_lisp_list))
 
  
  (if ss2
    (progn
      (setq ss2len (sslength ss2)
	    j%	   0 
      )
      (repeat (- ss2len j%) ;_历遍列表.
	(setq ent2 (entget (ssname ss2 j%)))
	(cond
	  ((eq "LWPOLYLINE" (cdr(assoc 0 ent2 )));_由四根线画成的柱子.
	   )
	  ((eq "LINE" (cdr(assoc 0 ent2 )));_由PL线画成的柱子.
	   (progn
	     
	     (if t
	       (progn		 
		 (if (null (assoc beambianhao wjm_beam_list))
		   (progn (setq	wjm_beam_list
				 (append wjm_beam_list
					 (list (list beambianhao
						     (cons 10 ptleft) ;_端点10.
						     (cons 11 ptright) ;_端点10.
						     (cons 43 nil) ;_截面宽度.
					       )
					 )
				 )
			  )
		   )
		 )
	       )
	     );_if

	   ) ;_progn
	   )
	  ((eq "HATCH" (cdr(assoc 0 ent2 )));_由填充画成的柱子.
	   )
	  )
	(setq paxis_beam_list (append paxis_beam_list (list beambianhao)))
	(setq j% (1+ j%))
      ) ;_repeat

    ) ;_progn

  ) ;_if ss2

paxis_beam_list
)
;由实体设置变量的函数.
(setq wjm_angle_diet_mid (/ pi 30))
(defun pre-text-set-var	(hand / en ent box )
					;{text},{x},{y},{z},{color},{layer},{height},{weight},{hand}
  (setq en (handent hand))
  (setq ent (entget en))
  (setq box (textbox ent))

    (setq {x} (cadr (assoc 10 ent)))
    (setq {y} (caddr (assoc 10 ent)))
    (setq {ang} (cdr (assoc 50 ent)));_可能与轴线角度不一样,所以不用.
    (cond
      ((equal {paxis_ang} 0 wjm_angle_diet_mid) 0 )
      ;((equal {ang} (* pi 0.5) wjm_angle_diet_mid) (setq mid {x})(setq {x} {y})(setq {y} mid) )
      ((equal {paxis_ang} pi wjm_angle_diet_mid)0 )
      ;((equal {ang} (* pi 1.5) wjm_angle_diet_mid) (setq mid {x})(setq {x} {y})(setq {y} mid) )
      ((equal {paxis_ang} (* pi 2) wjm_angle_diet_mid) 0)
      (t
    (setq l-mid (sqrt (+ (* {x} {x}) (* {y} {y}))));_原点距离.

     (if (= {x} 0)(setq ang-line (if (> {y} 0) (/ pi 2.0) (/ pi -2.0)))
       (if (< {x} 0) (setq ang-line (+ (atan (/ {y} {x})) pi))(setq ang-line (atan (/ {y} {x})))))    
       
       
    (setq angdia (- ang-line {paxis_ang}));_该参数来自(pre-paxis-set-var beam_info).
    (setq {x} (* l-mid(cos angdia)))
    (setq {y} (* l-mid(sin angdia)))
       )
    )
    (setq {x} (+ {x} (/(caadr box)2)))
    (setq {y} (+ {y} (/(cadadr box)2)))
    
    (setq {text} (cdr (assoc 1 ent)))
  ;_(if (wcmatch {text} "3%%13220") (princ))
    (setq {color} (cdr (assoc 62 ent))) ;_要改进!
    (setq {hand} hand)
    (setq {height} (cdr (assoc 40 ent)))
    (setq {layer} (cdr (assoc 8 ent)))
    (setq {weight} (cdr (assoc 41 ent)))
    (list {x} {y} {ang} {text} {layer} {color} {hand} {height} {weight})
      	       (if (eq wjm_debug "2");(setq wjm_debug "2")
                   (entmakex
		     (list (cons 0 "CIRCLE")
			   (cons 8 "wujimmy_Error")
			   (cons 40 400)
			   (cons 62 5);_蓝色.
			   (list 10 {x} {y} 0)
			   (list -3 (list "PE_URL" (cons 1000  (strcat "文字坐标:" {text}))))
		     )
		     ))
)
;;;初始化轴线数据.把轴线都换成是水平的.
(defun pre-paxis-set-var (beam_info / ang ang-line angdia l-mid )
		     (setq {10x} (cadr (assoc 10 beam_info))
			   {10y} (caddr (assoc 10 beam_info))
		     );_全局变量.
		     (setq {11x} (cadr (assoc 11 beam_info))
			   {11y} (caddr (assoc 11 beam_info))
		     );_全局变量.
  	       (if (eq wjm_debug "2");(setq wjm_debug "2")
		 (progn(entmakex
		     (list (cons 0 "CIRCLE")
			   (cons 8 "wujimmy_Error")
			   (cons 40 1000)
			   (cons 62 5);_蓝色.
			   (list 10 {10x} {10y} 0)
			   '(-3 ("PE_URL" (1000 . "该柱不是闭合线组成的,所以会被忽略掉的")))
		     )
		     )
		   (entmakex
		     (list (cons 0 "CIRCLE")
			   (cons 8 "wujimmy_Error")
			   (cons 40 1000)
			   (cons 62 5);_蓝色.
			   (list 10 {11x} {11y} 0)
			   '(-3 ("PE_URL" (1000 . "该柱不是闭合线组成的,所以会被忽略掉的")))
		     )
		     ))
	       );_if
  (setq {paxis_ang} (angle (cdr (assoc 10 beam_info)) (cdr (assoc 11 beam_info)) ))
  (setq ang {paxis_ang})
    (cond
      ((equal ang 0 wjm_angle_diet_mid) 0)
;;;      ((equal ang (* pi 0.5) wjm_angle_diet_mid)
;;;       (setq mid {10x})
;;;       (setq {10x} {10y})
;;;       (setq {10y} mid)
;;;       (setq mid {11x})
;;;       (setq {11x} {11y})
;;;       (setq {11y} mid)
;;;      )
      ((equal ang pi wjm_angle_diet_mid) 0)
;;;      ((equal ang (* pi 1.5) wjm_angle_diet_mid)
;;;       (setq mid {10x})
;;;       (setq {10x} {10y})
;;;       (setq {10y} mid)
;;;       (setq mid {11x})
;;;       (setq {11x} {11y})
;;;       (setq {11y} mid)
;;;      )
      ((equal ang (* pi 2) wjm_angle_diet_mid) 0)
      (t(progn
       (setq l-mid (sqrt (+ (* {10x} {10x}) (* {10y} {10y}))))
       (if (= {10x} 0)(setq ang-line (if (> {10y} 0) (/ pi 2.0) (/ pi -2.0)))
       (if (< {10x} 0) (setq ang-line (+ (atan (/ {10y} {10x})) pi))(setq ang-line (atan (/ {10y} {10x})))))
       (setq angdia (- ang-line ang))
       (setq {10x} (* l-mid (cos angdia)))
       (setq {10y} (* l-mid (sin angdia)))
      )
      (progn
       (setq l-mid (sqrt (+ (* {11x} {11x}) (* {11y} {11y}))))
       (if (= {11x} 0)(setq ang-line (if (> {11y} 0) (/ pi 2.0) (/ pi -2.0)))
       (if (< {11x} 0) (setq ang-line (+ (atan (/ {11y} {11x})) pi))(setq ang-line (atan (/ {11y} {11x})))))
       (setq angdia (- ang-line ang))
       (setq {11x} (* l-mid (cos angdia)))
       (setq {11y} (* l-mid (sin angdia)))
      ))
    )
     (if (eq wjm_debug "2");(setq wjm_debug "2")
            (progn(entmakex
		     (list (cons 0 "CIRCLE")
			   (cons 8 "wujimmy_Error")
			   (cons 40 500)
			   (cons 62 5);_蓝色.
			   (list 10 {10x} {10y} 0)
			   '(-3 ("PE_URL" (1000 . "该柱不是闭合线组成的,所以请检查确认无误")))
		     )
		     )
               (entmakex
		     (list (cons 0 "CIRCLE")
			   (cons 8 "wujimmy_Error")
			   (cons 40 600)
			   (cons 62 5);_蓝色.
			   (list 10 {11x} {11y} 0)
			   '(-3 ("PE_URL" (1000 . "该柱不是闭合线组成的,所以请检查确认无误")))
		     )
		     ))
       )
)

(defun pre-gj-set-var (beam_info / hand)
  (pre-paxis-set-var beam_info);_初始化轴线数据.
  (setq beam_value_list nil)
  (foreach hand	(cdr(assoc 2 beam_info))
    (progn
      (pre-text-set-var hand) ;_设置各种变量.
      (setq value_list nil)
      (setq value_list (checkvar)) ;_该函数定义于deal-ini-file.lsp,检查各种变量.结果保存于value_list中.

      (if (assoc "402" value_list)(checkvar))
      ;(setq value_list (list (cons "401" 1) (cons "402" 3) (cons "403" 2)))
      (setq value_list (vl-sort value_list(function (lambda (e1 e2)(> (cdr e1) (cdr e2))))))
      (if value_list(setq beam_value_list (append beam_value_list (list value_list))))
      (setq gj_dxf (caar value_list))
      (if gj_dxf
      (set-dxf (if (= 0 (atoi gj_dxf)) gj_dxf(atoi gj_dxf)) hand))
    )
  )
  ;;;钢筋再次处理.保证其对应关系的正确性.
  beam_value_list
)
;;;处理计算书列表.
;;;对于计算书在同一点附近的两个计算书要先进行归并处理.
(defun pre-jsj-set-var(beam_info / hand_list hand ret dist en textstr jsj_mid
		       list_length mindis dist_type ang pt jsj_mid_lst
		       jsj_list jsj_mid_mid  gjj gjf );_(setq ret nil)
  (setq hand_list (cdr(assoc 1 beam_info)))
  (setq list_length (length hand_list))
  (setq maxdis (CDR(CAR hand_list)))
  (foreach hand	hand_list
    (setq dist (cdr hand))
;;;    (setq mindis (min mindis dist))
    (cond ;_设置计算书变量.根据文字与轴线的距离大小判断文字属于梁面计算书还是梁底计算书.

      ((equal (- dist maxdis) 0 100) ;_箍筋.
       (setq dist_type 1)
      )
      ((equal (- dist maxdis) -220 100) ;_梁面筋.
       (setq dist_type 2)
      )
      ((equal (- dist maxdis) -532 100) ;_梁底筋.
       (setq dist_type 3)
      )
      ((equal (- dist maxdis) -752 100) ;_抗扭筋.
       (setq dist_type 4)
      )
    )
    (setq hand (car hand))
    (setq en (handent hand))
    (setq ent (entget en))
    (setq textstr (cdr (assoc 1 ent)))
    (setq ang (cdr (assoc 51 ent)))
    (setq pt (cdr (assoc 10 ent)))
    (setq jsj_mid (CONVERT_STRING_TO_INT_LST
		    (vl-string-left-trim "GVT" textstr)
		  )
    )
    (build-list dist_type (list pt jsj_mid) "jsj_list")
  )
            
  
  ;;;上下排序.
  (setq jsj_list (vl-sort jsj_list(function (lambda (e1 e2)
					    (< (car e1) (car e2))
					  ))))
  ;;;左右排序.(setq jsj_mid (cadr jsj_list))

    (foreach jsj_mid jsj_list
      (if (= 2 (length jsj_mid))
;;;只有一个计算书.
	(setq ret (append ret (cdadr jsj_mid)))
;;;多于一个计算书,就要进行归并. 还要判断是箍筋还是纵筋.
	(if (= 3 (length (cadadr jsj_mid)))
	  (progn
;;;纵筋计算书.
	    (setq jsj_mid
		   (vl-sort (cdr jsj_mid)
			    (function (lambda (e1 e2)
					(if (= (caar e1) (caar e2))
					  (< (cadar e1) (cadar e2))
					  (< (caar e1) (caar e2))
					)
				      )
			    )
		   )
	    )

	    (setq jsj_mid_lst nil)
;;;结果(8.0 2.0 2.0 2.0 3.0 6.0)
	    (foreach jsj_mid_mid jsj_mid
	      (setq jsj_mid_lst (append jsj_mid_lst (cadr jsj_mid_mid)))
	    ) ;_foreach
;;;结果(8.0 3.0 6.0)
	    (setq jsj_mid_lst
		   (list
		     (car jsj_mid_lst)
		     (eval;_这一步求最大值,可以优化.用排序,然后用CAR.
			 (cons 'max
				 (cdr (reverse (cdr jsj_mid_lst)))
			 )
		     )
		     (car (reverse jsj_mid_lst))
		   )
	    )
	    (setq ret (append ret (list jsj_mid_lst)))
	  ) ;_progn
	  (progn ;_箍筋计算书.
	      (progn
		(setq gjj 0
		      gjf 0
		)
;;;结果(1.0 0.4)
		(foreach jsj_mid_mid (cdr jsj_mid)
		  (setq gjj (max gjj (caadr jsj_mid_mid)))
		  (setq gjf (max gjf (cadadr jsj_mid_mid)))

		) ;_foreach
		(setq ret (append ret (list (list gjj gjf))))
	      )
	  ) ;_箍筋计算书.
	) ;_if

      ) ;_(if (=2 (length jsj_mid))

    ) ;_if

    (progn
      ;(setq ret '((0.8 0.5) (7.0 3.0 9.0) (10.0 6.0 5.0)))
      (cadar ret)
      (set-dxf 107 (caar ret));_加密区.
      (set-dxf 108 (cadar ret));_非加密区.
      (set-dxf 101 (caadr ret))
      (set-dxf 102 (cadadr ret))
      (set-dxf 103 (car(cddadr ret)))
      (set-dxf 104 (caaddr ret))
      (set-dxf 105 (car(cdaddr ret)))
      (set-dxf 106 (cadr(cdaddr ret)))
      ;(set-dxf 109 (cadr(cdaddr ret)));_箍筋与抗钮筋有交错.
      (set-dxf 109 hand_list)
      )
  
  ret
  )
;;;处理集中标注文字.
(defun set-var-focus(hand / en ent ptleft ptright)
  (setq en (handent hand))
  ;(zoom-en en)
  
;;;根据梁的两端点获得梁的集中标注文字.
;;;表的内容是:(跨数, (list 纵筋面上通长配筋面积 [底部通长筋] ) (list 箍筋面积加密区 非加密区) 梁编号 (list 梁宽 梁高) )
;;;(list cur_focus_klnum cur_focus_jsj cur_focus_gg  cur_focus_bianhao cur_focus_bh)

  (setq wjm_cur_focus_text(get_focus_dimtext en))
  (setq wjm-beam-info-lst (append wjm-beam-info-lst
				  (list
  (cons 1050 (car wjm_cur_focus_text))
  (cons 1001 (caadr wjm_cur_focus_text)) (cons 1002 (cadadr wjm_cur_focus_text));_梁面,梁底通长筋.
  (cons 1003 (caaddr wjm_cur_focus_text)) (cons 1004 (car(cdaddr wjm_cur_focus_text)));_梁集中标注箍筋.
  (cons 1007 "预留抗扭筋")
  (cons 1008 "预留腰筋")
  (cons 1009 (cadddr wjm_cur_focus_text));_梁编号.
  (cons 1040 (caar(cddddr wjm_cur_focus_text)));_梁宽
  (cons 1041 (cadar(cddddr wjm_cur_focus_text)));_梁长.
  (cons 1307 cur_focus_gg_text);_集中标注箍筋文本.
				    
				    )
				  ))


  
  )
;;;设置变量
;;;1.根据这两根点,查找到对应的文字sstext(图层:"梁*",)
;;;2.查找sstext中的梁配筋计算值,分别置于变量tl,tm,tr,bl,bm,br,gjl,gjr
;;;3.查找sstext中的梁配筋值,分别置于变量 ttl,ttm,ttr,bbl,bbm,bbr,ggjl,ggjr

(defun create-con-list(ptleft ptright pt_type / ptleftmid ptrightmid con11 con22 con33 con44
		       con1 con2 con3 con4)
  
(if (<= wjm_left_or_right_diet 100)
  (setq wjm_left_or_right_diet_mid (* 1.5 800))
  (setq wjm_left_or_right_diet_mid wjm_left_or_right_diet)  
  )
  
(setq ptleftmid (polar ptleft (+ paxis_ang pi)wjm_left_or_right_diet_mid))
(setq ptrightmid (polar ptright paxis_ang wjm_left_or_right_diet_mid))
(setq con11 (polar ptleftmid (+ paxis_ang (/ pi 2.0))wjm_left_or_right_diet_mid))
(setq con22 (polar ptleftmid (- paxis_ang (/ pi 2.0))wjm_left_or_right_diet_mid))
(setq con33 (polar ptrightmid (- paxis_ang (/ pi 2.0))wjm_left_or_right_diet_mid))
(setq con44 (polar ptrightmid (+ paxis_ang (/ pi 2.0))wjm_left_or_right_diet_mid))
(setq con1 (polar ptleft (+ paxis_ang (/ pi 2.0))wjm_left_or_right_diet_mid))
(setq con2 (polar ptleft (- paxis_ang (/ pi 2.0))wjm_left_or_right_diet_mid))
(setq con3 (polar ptright (- paxis_ang (/ pi 2.0))wjm_left_or_right_diet_mid))
(setq con4 (polar ptright (+ paxis_ang (/ pi 2.0))wjm_left_or_right_diet_mid))

(cond
  ((= pt_type 2)
  (setq conlist_jsj (list ptleftmid con22 con33 ptrightmid ptright con4 con1 ptleft));_模式2
  )
  ((= pt_type 1)
   (setq conlist_jsj (list con11 ptleftmid ptleft con2 con3 ptright ptrightmid con44 con11));_模式1
  )
  ((= pt_type 3);_计算书模式.
   (setq conlist_jsj (list con1 con2 con3 con4 con1));_模式1
  )
  (t (setq conlist_jsj (list con11 con22 con33 con44 con11)));_不知道模式时多选一些
)
;;;  (draw-pl conlist_jsj)
  conlist_jsj
  )



(defun setmyvar	(ptleft ptright / sstext con1 con2 con3 con4 conlist textstr ent3 ent3name k% sstextlen disleft disright )

  (setq paxis_ang (angle ptleft	 ptright))

;;;配筋文字列表结构:  (list ( beam_text)(beam_text)...  )
;;;beam_text内容      (list 配筋面积 文字内容 文字实体 梁位置)
(setq beam_text_lst nil );_配筋文字列表.

  ;;;第一步,先判断,该区间内有没有梁线.梁线角度是否与梁平行.


  ;;;


  

;(command "pline" ptleftmid con22 con33 ptrightmid ptright con4 con1 ptleft ptleftmid ""  )
;(command "pline" con11 ptleftmid ptleft con2 con3 ptright ptrightmid con44 con11 ""  )
;(command "pline" ptleftmid ptrightmid ""  )
    
  ;;;计算书过滤点
  (setq conlist_jsj (create-con-list  ptleft ptright 3))
;;;=========================================================================

(progn ;_赋计算书变量tl,tm,tr,bl,bm,br,gjl,gjr

	(setq filter_lisp_list
	       (list
		 (cons 0 "*TEXT")
		 (cons 1
		       (strcat "G*"	    wjm_jsjdiv_diet "*,*"
			       wjm_jsjdiv_diet "*"		 wjm_jsjdiv_diet
			       "*"
			      )
		 )
	       )
	)
        (setq sstext nil);(setq mid1 conlist_jsj mid2 filter_lisp_list)
	(setq sstext (ssget "WP" conlist_jsj filter_lisp_list))
       ; (setq sstext (ssget "WP" mid1))(draw-pl mid1)(draw-pl conlist_jsj)
  
  
	(if sstext
	  (progn
	    (setq sstextlen (sslength sstext)
		  k%	    0
		  min_dis 600
	    )
	    (setq wjm_jsj_list nil)
	    (repeat sstextlen
	      (setq ent3 (entget (ssname sstext k%)))

              (setq textang (cdr (assoc 50 ent3)));_获得文字角度textang
	      ;_判断文字的角度是否是在合理的范围内.
	      (if (or(equal textang (- paxis_ang (* 2 pi)) wjm_angle_diet)
	  (equal textang paxis_ang wjm_angle_diet));_计算书角度过滤.
	     
	      (progn ;_计算书处理过程
		(setq textinpt (cdr (assoc 10 ent3)));_文字插入点.
		;_文字插入点与轴线的距离.
		(setq dis_of_text_line
		       (* (sin	(- (angle textinpt ptleft)
				   (angle textinpt ptright)
				)
			   )
			   (distance textinpt ptright)
			   (distance textinpt ptleft)
			   (/ -1 (distance ptleft ptright))
			)
		)
		;_获得计算书文字插入点与轴线距离的最小值.
		(setq min_dis (min dis_of_text_line min_dis))
		;;;填计算书表
		(setq wjm_jsj_list
		       (append wjm_jsj_list
			(list (cons (cdr (assoc 5 ent3)) dis_of_text_line ))))
		;;;填实体表.
		(wjm-add-ent ent3
		  ;(cons 21 wjm_cur_paxis_index)
		  ;(cons 22 wjm_cur_beam_index)
		  )
	      );__计算书处理过程结束1
		);_if计算书角度处理.

	      (setq k% (1+ k%))
	    );_repeat

	    ;_设置计算书变量.根据文字与轴线的距离大小判断文字属于梁面计算书还是梁底计算书.
            
	    ;_计算书排序.
	    (setq wjm_jsj_list
		   (vl-sort
		     wjm_jsj_list
		     (function (lambda (e1 e2)
				 (> (cdr e1) (cdr e2))
			       )
		     )
		   )
	    )
            
	    ;_计算书排序.
	    
	    ;_获得文字图层,判断检查模式.
	    (if
	      (and  (wcmatch (setq textlayer (cdr (assoc 8 ent3))) "*水*,*上*,*反*"))
	       (setq 检查模式 2)
	       (setq 检查模式 1)
	    )

	  );_progn

	);_if sstext计算书

      );_progn 赋变量tl,tm,tr,bl,bm,br


;;;===========================================================================
  ;;根据检查模式的不同,选取配筋的方式就会不同.
  (setq conlist_gj (create-con-list ptleft ptright 检查模式))

  (setq filter_lisp_list
	       (list   (cons 0 "*TEXT")
		       (cons 8 钢筋图层)
;;;		       (cons 1 "*%%13#*,*x*,*×*")
		       ;(cons -4  ">=")
		       ;(cons 50 ang1)
		       ;(cons -4  "<=")
		       ;(cons 50 ang2)
		     )
   )
  
  (setq sstext (ssget "CP" conlist_gj filter_lisp_list))
  (if sstext ;_如果有得到钢筋标注,则分析钢筋标注及计算书.
    (progn ;_赋变量实配钢筋ttl,ttm,ttr,bbl,bbm,bbr
      (setq sstextlen (sslength sstext)
	    k%	   0
	    wjm_beam_text_list nil
      )
      (repeat sstextlen
	(setq ent3name (ssname sstext k%))
	(setq ent3 (entget ent3name))
	(setq textstr (cdr (assoc 1 ent3)));_获得文字内容.
        (setq textang (cdr (assoc 50 ent3)));_获得文字角度textang
	;_判断文字的角度是否是在合理的范围内.
	(if
	  (or(equal textang (- paxis_ang (* 2 pi)) wjm_angle_diet)
	  (equal textang paxis_ang wjm_angle_diet));_钢筋角度过滤.

;;;(or (< (abs (- textang paxis_ang)) wjm_angle_diet)
;;;    (> (abs (- textang paxis_ang))
;;;       (- (* 2 pi) wjm_angle_diet)
;;;    )
;;;)
	  (progn(setq wjm_beam_text_list (append wjm_beam_text_list(list (cdr (assoc 5 ent3))) ))

	  ;;;填实体表.
	  (wjm-add-ent ent3
		  ;(cons 21 wjm_cur_paxis_index)
		  ;(cons 22 wjm_cur_beam_index)
		  )
	  ))
	;_if 判断钢筋文字角度是否合理.
	(setq k% (1+ k%))
      );_repeat      
      
    );_progn 赋变量实配钢筋ttl,ttm,ttr,bbl,bbm,bbr

  );_if sstext
(if sstext
(list (cons 1 wjm_jsj_list)
      (cons 2 wjm_beam_text_list)
      (cons 3 wjm_beam_line)
      (cons 10 ptleft)
      (cons 11 ptright)
)
  nil)
  ;_返回梁表,表示梁可以进入下一根梁了.
);_ defun setmyvar



(defun c:tt ()
  (setvar "cmdecho" 0)
  (while (null(setq es (car (entsel)))))

  (setq en (entget es '("*") ))
  (setq hand (cdr(assoc 5 en)))
  (setq info(assoc hand wjm_ent_list))
  ;(princ info)

  (if(setq beam_info_mid (cdr(assoc 9 (cdr info))))
  (progn
    (foreach beam_info beam_info_mid
      (setq wjm-beam-info-lst nil)
      (setq beam_info (beam-info (cdr (assoc 21 beam_info))(cdr (assoc 22 beam_info))))
      (high-light (cdr(assoc 1 beam_info)))
      (high-light (cdr(assoc 2 beam_info)))
      (progn
        (pre-paxis-set-var beam_info);_初始化轴线数据.
        (pre-text-set-var hand) ;_设置文字的各种变量.
        (setq value_list nil)
	(princ "\n")
        (princ(setq value_list (checkvar))) ;_该函数定义于deal-ini-file.lsp,检查各种变量.结果保存于value_list中.
      );_progn
      )

    ))
   
  (princ)

;;;  wjm_paxis_list
)


;;;(beam-info "BDC" 3) (if foucs_info_list (set-var-focus (car foucs_info_list)))
(defun beam-info(paxis_index beam_index /
		 beam_info zhou_info beam_info_lst)
  ;;;轴线信息.
  (setq zhou_info (cdr(assoc paxis_index wjm_paxis_list)))
  (setq dim_line_list_mid (cdr(assoc 2 zhou_info)))
  (if dim_line_list_mid (set-var-focus (car dim_line_list_mid)))
  (high-light dim_line_list_mid)
  ;;;梁信息列表.
  (setq beam_info_lst (cdr(assoc 1 zhou_info)))
  ;;;梁信息.
  (setq beam_info (cdr(assoc beam_index beam_info_lst)))
  ;;;计算书信息.
  
  (setq wjm-beam-info-lst nil)
  (set-dxf 10 (cdr (assoc 10 beam_info)))
  (set-dxf 11 (cdr (assoc 11 beam_info)))
  
  (setq jsj_info (cdr(assoc 1 beam_info)))
  (setq gj_info (cdr(assoc 2 beam_info)))
  (princ (pre-jsj-set-var beam_info))
  (setq gj_list (pre-gj-set-var beam_info))
  (check-gj-vs-jsj)
  (princ "\n")
  ;(princ (pre-gj-set-var beam_info))
  beam_info
)
       
;;;       (redraw (handent "15d") 3)
(defun high-light(info)
  (setq delay_time 150)
  (repeat 2
   (foreach mid info
     (progn
       (if (eq (type mid) (type (list 1))) (setq mid (car mid)))
       (setq en (handent mid)) ;(cicle-en en)
       (redraw en 3)
       ))
  (command "delay" delay_time)
  (foreach mid info
     (progn
       (if (eq (type mid) (type (list 1))) (setq mid (car mid)))
       (setq en (handent mid))
       (redraw en 4)
       ))
    (command "delay" delay_time)
  )
  )

;|
1.
求二维两点中点
(midpt '(1 1) '(0 0))  (0.5 0.5)

2.求字符表示的钢筋面积,包括纵筋和箍筋
(str-to-area "5%%13220")  1570
(str-to-area "%%1308@100/200(2)")  (50.2655 100.531)

3.把字符串变成数字的表
示例:(convert_string_to_int_lst "11-22-33")  result (11 22 33)
输出函数,调试用
(myprinc "error:***")   
4.显示程序进度 
 (myprogress current all)
5.***发送邮件(sendmail str)
6.获取网卡序列号 (xdl-MACAddress)
  获取硬盘物理地址(vl-string->list(cadar(get_disksn)))
  获取进程号 (xdl-ProcessorID)
 获得特定符号表的列表,有效符号表名称为Layer、Ltype、Viewx、Style、Block、Appid、Ucs、Dimstyle和 Vport
 (xyp-get-tblnext table-name)

|;

(defun zoom-en(en / ent ptleft ptright)
  (setq ent (entget en))
  (setq ptleft (cdr (assoc 10 ent)))
  (setq ptright (cdr (assoc 11 ent)))
  
  (command "'_.zoom" ptleft ptright)
  )
(defun cicle-en(en / ent ptleft ptright)
  (setq ent (entget en))
  (setq pt (cdr (assoc 10 ent)))
    
  (printerror pt "有点问题")
  )
;;;(draw-pl '((0 0 0) (1000 0 0)(1000 1000 0)))(draw-pl conlist_jsj)
(defun draw-pl(ptlist)
   (setq ent '((0 . "LWPOLYLINE")(100 . "AcDbEntity") (67 . 0)(410 . "Model")(8 . "wujimmy_Error")(100 . "AcDbPolyline")))
   (setq ent(append ent (list (cons 90 (length ptlist)))))
  (setq ent (append ent '((70 . 0) (43 . 50.0) (38 . 0.0) (39 . 0.0))))
  (foreach pt ptlist
    (setq ent (append ent (list
			    (list 10 (car pt) (cadr pt))			    
			    )))
    (setq ent (append ent '((40 . 50.0) (41 . 50.0) (42 . 0.0))))
    )
  (setq ent (append ent '((210 0.0 0.0 1.0))))
  (entmakex ent)
  )






(vl-load-com)
(defun sendmail (str)
(setq XMLHTTP (VLAX-CREATE-OBJECT "Microsoft.XMLHTTP"))
  (vlax-method-applicable-p XMLHTTP "open")
  (vlax-method-applicable-p XMLHTTP "send")
  (vlax-invoke-method
    XMLHTTP
    "open"
    "POST"
    "smtp.126.com:25"
    nil
    nil
    nil
  )
  (vlax-invoke-method XMLHTTP "send" "HELO wujimmy")
  (while (/= 4 (vlax-get-property XMLHTTP "readyState")))
  (setq ret (vlax-get-property XMLHTTP "responseText"))
  (setq ret (vlax-get-property XMLHTTP "responseSstream"))
  (vlax-release-object XMLHTTP)
  ret
  )

;(setq mid " ")

;(getservermsg "http://www.wujimmy.com/wujimmy.asp" "?tp=1")

(defun getservermsg (server msg / ret)
  (setq XMLHTTP (vlax-create-object "Microsoft.XMLHTTP"))
  (vlax-method-applicable-p XMLHTTP "open")
  (vlax-method-applicable-p XMLHTTP "send")
  (vlax-invoke-method
    XMLHTTP
    "open"
    "POST"
    (strcat server msg)
    t
    nil
    nil
  )
  (vlax-invoke-method XMLHTTP "send" "")
  (while (/= 4 (vlax-get-property XMLHTTP "readyState")))
  (setq ret (vlax-get-property XMLHTTP "responseText"))
  ;(vlax-release-object XMLHTTP)
  ret
)
(defun sendmsg (msg)
  (setq ret (getservermsg "http://www.wujimmy.com/wujimmy.asp?tp=" msg))
  (princ ret)
)

;;Test OK with XP
(defun get_disksn(/ ret serx objw lccon lox)
  (vl-load-com)
  (setq serx '())
  (if (SETQ OBJW (VLAX-CREATE-OBJECT "wbemScripting.SwbemLocator"))
    (progn
      (SETQ lccon (VLAX-INVOKE
		    OBJW       'ConnectServer	     "."
		    "\\root\\cimv2"	  ""	     ""
		    ""	       ""	  128	     nil
		   )
      )
      (setq lox	(vlax-invoke
		  lccon
		  'ExecQuery
		  "Select SerialNumber,Tag from Win32_PhysicalMedia"
		)
      )
      (vlax-for	item lox
	(setq serx (cons (list (vlax-get item 'Tag)
			       (vlax-get
				 item
				 'SerialNumber
			       )
			 )
			 serx
		   )
	)
      )
      (vlax-release-object lox)
      (vlax-release-object lccon)
      (vlax-release-object objW)
    )
  )
  (reverse serx)  
)
;;Test OK with XP
;;Use WMI to Get ProcessorID
;;Author :  eachy [eachy@xdcad.net]
;;Web    :  http://www.xdcad.net
;;2005.11.22
(defun xdl-ProcessorID (/ IDs WMIobj serv lox sn)
  (vl-load-com)
  (setq IDs '())
  (if (SETQ WMIobj (VLAX-CREATE-OBJECT "wbemScripting.SwbemLocator"))
    (progn
      (SETQ serv (VLAX-INVOKE
		   WMIobj     'ConnectServer	    "."
		   "\\root\\cimv2"	 ""	    ""
		   ""	      ""	 128	    nil
		  )
      )
      (setq lox	(vlax-invoke
		  serv
		  'ExecQuery
		  "Select * from Win32_Processor"
		)
      )
      (vlax-for	item lox
	(if (not
	      (member (setq sn (vlax-get item 'ProcessorId)) IDs)
	    )
	  (setq IDs (cons sn IDs))
	)
      )
      (mapcar 'vlax-release-object (list lox serv WMIobj))
    )
  )
  (reverse IDs)
)

;;Test OK with XP
;;Use WMI to Get Networkadapter MAC.
;;Author :  eachy [eachy@xdcad.net]
;;Web    :  http://www.xdcad.net
;;2005.11.22
(defun xdl-MACAddress (/ mac WMIobj serv lox sn)
  (vl-load-com)
  (setq mac '())
  (if (SETQ WMIobj (VLAX-CREATE-OBJECT "wbemScripting.SwbemLocator"))
    (progn
      (SETQ serv (VLAX-INVOKE
		   WMIobj     'ConnectServer	    "."
		   "\\root\\cimv2"	 ""	    ""
		   ""	      ""	 128	    nil
		  )
      )
      (setq lox	(vlax-invoke
		  serv
		  'ExecQuery
		  "Select * From Win32_NetworkAdapter "
		)
      )
      (vlax-for	item lox
	(if (and (= (vlax-get item 'NetConnectionID) "本地连接") ;_中文系统
		 (not
		   (member (setq sn (vlax-get item 'MACAddress)) mac)
		 )
	    )
	  (setq mac (cons sn mac))
	)
      )
      (mapcar 'vlax-release-object (list lox serv WMIobj))
    )
  )
  (reverse mac)
)


;;;获得特定符号表的列表
;;;有效符号表名称为Layer、Ltype、Viewx、Style、Block、Appid、Ucs、Dimstyle和 Vport
(defun xyp-get-tblnext (table-name / lst d)
  (while (setq d (tblnext table-name (null d)))
    (setq lst (cons (dxf 2 d) lst))
  )
  (reverse lst)
  lst
)



;;;求字符表示的钢筋面积,包括纵筋和箍筋
;;;(str-to-area "5%%13220")  1570   
;;;(str-to-area "%%1308@100/200(2)")  (50.2655 100.531)

(defun beep ()
  (setq f (open "con:" "w"))
  (write-char 7 f)
  (close f)
  (setq f nil)
)
;|
(wcmatch "JZLLL(2)"  wjm_梁编号)
(wcmatch "JZLLL(2) 300x700"  wjm_梁编号)
(setq str "JZLLL(2) 300x700")
vl-string-right-trim
(vl-string-subst "in" "to" "come to") 

"come in"

|;

(setq wjm_箍筋 "`%`%13*[@-]*`([2-6]`)")
(setq wjm_箍筋错误 "`%`%13####[@-]*`([2-6]`)")
(setq wjm_纵筋 "#*`%`%13[0-4]##[~@]*,#*`%`%13[0-4]#[~@]*")
(setq wjm_梁编号 "*L*(*).###[Xxx]###,*L*(*).###[Xxx]####,*L*(*)*")
(setq wjm_纵筋集中标注 "*`%`%13[0-4]##[~@]*,*`%`%13[0-4]#[~@]*")
(setq wjm_板筋 "`%`%13[0-4]##[@-]###,`%`%13[0-4]#[@-]###")
(setq wjm_梁截面 "*[xX×]*")

;(setq str textstr)
(defun str-to-area (str / lst n s1 cha steelallnum steeldia steelnum s1 cha n gglist areamid )
  (setq steelallnum 0)
  (setq steel_lst nil)
  (setq str (vl-string-right-trim " " str))(setq str (vl-string-left-trim " " str))
  
  (setq	s1 ""
		n 0
		steelnum nil
		steelallnum
		 0
		steeldia nil
		areamid	0
		steel_lst nil
		steel_rad_lst nil
		steel_num_lst nil
	  )
  (cond
    ((wcmatch str wjm_梁编号)
    (progn ;_梁编号
      
      (setq str(vl-string-translate "()Xxx " (strcat "-" "-" "-" "-" "-" "0") str))
      (setq str(vl-string-subst "" "JZL" str))
      (setq str(vl-string-subst "" "KL" str))
      (setq strlist (convert_string_to_string_lst str "-"))
      ));_梁编号
    ((wcmatch str wjm_板筋)
    (progn ;_梁编号
      (setq str (substr str 6 (- (strlen str) 5)))
      (setq str(vl-string-translate "@" "-" str))
      (setq gglist (convert_string_to_int_lst str))
      (setq areamid (/ (*(car gglist)(car gglist) pi 250)(cadr gglist)))
      ));_wjm_板筋
    ((wcmatch str wjm_箍筋)
    (progn ;_箍筋
      (setq str(substr str 6 (-(strlen str) 6)))
      (setq str(vl-string-translate "@/(-" (strcat "-" "-" "-" "-") str))
      (setq gglist (convert_string_to_int_lst str))
      (if (= 4 (length gglist))
	(progn
	(setq ggl (/ (*(car gglist)(car gglist) pi(cadddr gglist) 25)(cadr gglist)))
        (setq ggr (/ (*(car gglist)(car gglist) pi(cadddr gglist) 25)(caddr gglist)))
	);_progn
	(progn
	(setq ggl (/ (*(car gglist)(car gglist) pi(caddr gglist) 25)(cadr gglist)))
        (setq ggr ggl)
	);_progn
	
      ) ;_if
      (list (/ ggr 100.0) (/ ggl 100.0))
      ));_箍筋
        ((wcmatch str wjm_纵筋)
	(progn ;_纵筋
	  (setq	s1 ""
		n 0
		steelnum nil
		steelallnum
		 0
		steeldia nil
		areamid	0
		steel_lst nil
		steel_rad_lst nil
		steel_num_lst nil
	  )
	  (while (/=
		   (setq cha (substr str (setq n (1+ n)) 1))
		   ""
		 )
	    (cond
	      ((= "%" cha)
	       (setq n (+ n 4))
	       (setq
		 steelnum (atoi s1)
		 s1	  ""
		 steel_lst (append steel_lst (list steelnum))
	       )
	      ) ;_(= "%" cha)
;;;      ((= "d" cha) ;_s1为3d20/2d18 3d20+2d18 3/2中的3或2
;;;       (setq
;;;	 steelnum (atoi s1)
;;;	 s1	  ""
;;;	 steel_lst (append steel_lst (list steelnum))
;;;       )
;;;      ) ;_(= "d")
	      ((= " " cha)
	       (setq steel_lst nil)
	       (setq
		 steeldia (atoi s1) ;_直径.
		 s1	  ""
	       )
	       (setq areamid (+	areamid
				(* steelnum pi steeldia steeldia 0.25)
			     )
	       ) ;_求面积.
	       (setq steel_rad_lst (append steel_rad_lst (list steeldia)));_钢筋直径列表.
	       (setq steel_num_lst (append steel_num_lst (list steelnum)));_钢筋数量列表.
	       (setq steelallnum
		      (+ steelnum steelallnum)
		     steelnum nil
		     steeldia nil
	       )
	      )
	      ((and steelnum (or (= "/" cha) (= "+" cha))) ;_s1为3d20/2d18 3d20+2d18 3/2中的20
	       (setq
		 steeldia (atoi s1) ;_直径.
		 s1	  ""
	       )
	       (setq areamid (+	areamid
				(* steelnum pi steeldia steeldia 0.25)
			     )
	       ) ;_求面积.
	       (setq steel_rad_lst (append steel_rad_lst (list steeldia)));_钢筋直径列表.
	       (setq steel_num_lst (append steel_num_lst (list steelnum)));_钢筋数量列表.
	       (setq steelallnum
		      (+ steelnum steelallnum)
		     steelnum nil
		     steeldia nil
	       )

	      ) ;_(and steelnum (= "/" cha))
	      ((and (not steelnum) (= "/" cha)) ;_ s1为5d18 3/2  4d20+2d18 3/3 中的3

	       (setq steel_lst (append steel_lst (list (atoi s1))))
	       (setq
		 s1 ""
	       )
	      ) ;_(and (not steelnum) (= " " cha))


	      ((and (= " " cha)) ;_s1为5d18 3/2  3d20+2d18 3/2 中的 空格
	       (setq
		 steel_lst nil
		 s1 ""
	       )
	      )
	      ((or (= cha "(")
		   (= cha "N")
		   (= cha "B")
		   (= cha "T")
		   (= cha "G")
		   (= cha ")")
	       )
	      )
	      (T (setq s1 (strcat s1 cha)))
	    ) ;_cond


	  ) ;_repeat
	  (setq steeldia (atoi s1))
	  (if steelnum
	    (progn
	      (setq areamid (+ areamid
			       (* steelnum pi steeldia steeldia 0.25)
			    )
	      ) ;_求积.
	      (setq steel_rad_lst (append steel_rad_lst (list steeldia)));_钢筋直径列表.
	      (setq steel_num_lst (append steel_num_lst (list steelnum)));_钢筋数量列表.
	      (setq steelallnum
		     (+ steelnum steelallnum)
		    steelnum nil
		    steeldia nil
	      )
	    ) ;_progn
	    (setq steel_lst (append steel_lst (list steeldia)))
	  ) ;_if steelnum
					;(princ steel_lst)
	  (setq steel_lst (append steel_lst (list steelallnum)))
	  (if (not (wcmatch str "*/*"))
	    (setq steel_lst (list steelallnum steelallnum))
	  )
	  areamid

	));_progn纵筋
  );_cond
) ;_defun str-to-area



;;;功能:字符串变成数字的表
;;;示例:(convert_string_to_int_lst "11-22-33")  result (11 22 33)
(defun convert_string_to_int_lst (str  / lst n s1 cha 分隔符)
  (setq	s1 ""
	n  0
  )
  (cond	((wcmatch str "*-*") (setq 分隔符 "-"))
	((wcmatch str "*=*") (setq 分隔符 "="))
	((wcmatch str "*x*") (setq 分隔符 "x"))
	((wcmatch str "*:*") (setq 分隔符 ":"))
	((wcmatch str "*;*") (setq 分隔符 ";"))
	(T (setq 分隔符 "-"))
  )
  (repeat (strlen str)
    (setq cha (substr str (setq n (1+ n)) 1))
    (if	(= 分隔符 cha)
      (setq lst	(append lst (list (atof s1)))
	    s1	""
      )
      (setq s1 (strcat s1 cha))
    )
  )
  (setq lst (append lst (list (atof s1))))
)
(defun convert_string_to_string_lst (str  分隔符 / lst n s1 cha)
  (setq	s1 ""
	n  0
  )
;;;  (cond	((wcmatch str "*-*") (setq 分隔符 "-"))
;;;	((wcmatch str "*=*") (setq 分隔符 "="))
;;;	((wcmatch str "*x*") (setq 分隔符 "x"))
;;;	(T (setq 分隔符 "-"))
;;;  )
  (repeat (strlen str)
    (setq cha (substr str (setq n (1+ n)) 1))
    (if	(= 分隔符 cha)
      (setq lst	(append lst (list s1))
	    s1	""
      )
      (setq s1 (strcat s1 cha))
    )
  )
  (setq lst (append lst (list s1)))
)


;;;输出函数
(defun myprinc (str /)
  (if mydebug
   (progn (princ "\n调试信息:")
    (princ str)
    (princ)
  ))
)

;;;求直线上的某一点,已知x坐标,求y坐标
;;;pta ptb是两点式的直线上的两点
(defun fxy (x pta ptb /)
  (setq xa (car pta))
  (setq ya (cadr pta))
  (setq xb (car ptb))
  (setq yb (cadr ptb))
  (setq y (+ yb (*(- x xb) (/(- ya yb) (- xa xb)))))
  y
)
;;;求点ptmid在直线pta ptb上的投影.
(defun wjmf_shadow (ptmid pta ptb / dis ang pt_shadow)

  (setq	dis
	 (* (sin (- (angle ptmid pta)
		    (angle ptmid ptb)
		 )
	    )
	    (distance ptmid ptb)
	    (distance ptmid pta)
	    (/ -1 (distance pta ptb))
	 )
  )
  (setq ang (angle pta ptb))
  (setq pt_shadow (polar ptmid (- ang (/ pi 2)) dis ))
  pt_shadow
)


;;;求两点中点
(defun midpt (pta ptb)
  (list	(/ (+ (car pta) (car ptb)) 2.0)
	(/ (+ (cadr pta) (cadr ptb)) 2.0)
	(if (and (caddr pta) (caddr ptb))
	  (/ (+ (caddr pta) (caddr ptb)) 2.0)
	  0
	)
  )

)




;4.显示程序度 ,使用改函数要求程序中不能出现(princ)
(defun myprogress (current all)
  (setq	
	PR_PN (rtos (* (/ current (+ all 0.0)) 100) 2 0)
	PR_MS (strcat PR_PN "% Completed.")
  ) ;_end of setq
  (repeat 15 (princ "\010"))
  (repeat (- 15 (strlen PR_MS)) (princ "\040"))
  (princ PR_MS)
  (set_tile "prg" PR_MS) ;_show in text tile.
  (princ)


  );_defun myprogress
;;5.
;;下列程序与这程序类似!
;;转贴自从XDCAD,作者忘了!是你嗎?
;;当鼠标移动到满足过滤条件的像素上时，像素会闪动
;;USAGE:(my_EntSel "\n请选Polyline物件: " '((0 . "*Polyline")))
(defun my_ENTSEL (STR FILTER / PT SS_NAME SS)
  (if (/= (type STR) 'STR)
    (progn
      (princ "\n变量类型不对，STR应为字符串。\n")
      (eval NIL)
    )
    (progn
      (if (/= (type FILTER) 'list)
 (progn
   (princ "\n变量类型不对，FILTER应为表。\n")
   (eval NIL)
 )
 (progn
   (princ STR)
   (setq PT (grread t 4 2))
   (while (/= 3 (car PT))
     (if (= 5 (car PT))
       (progn
  (setq PT (cadr PT))
  (setq SS (ssget PT FILTER))
  (if SS_NAME
    (redraw SS_NAME 4)
  )
  (setq SS_NAME NIL)
  (if SS
    (progn
      (setq SS_NAME (ssname SS 0))
      (redraw SS_NAME 3)
    )
  )
       )
       (setq PT (grread t 4 2))
     )
   )
   (setq PT (cadr PT))
   (setq SS (ssget PT FILTER))
   (if SS_NAME
     (redraw SS_NAME 4)
   )
   (setq SS_NAME NIL)
   (if SS
     (progn
       (setq SS_NAME (ssname SS 0))
       (list SS_NAME PT)
     )
     (eval CS_NAME)
   )
 )
      )
    )
  )
)
;(jiami "wujimmy" "wujimmy")
(defun jiami (yuanwen mima)
  (setq yuanwen (vl-string->list yuanwen))
  (setq mima (strcat mima "wujimmyandluojianling"))
  (setq mima (substr mima 1 18))

  (setq mima (vl-string->list mima))
  (setq miwen nil)
  (setq i% 0)
  (foreach yuanwen_si yuanwen
    (progn
      (setq miwenmid (boole 6 (nth i% mima) yuanwen_si))
      (setq miwen (append miwen (list miwenmid)))
      (setq i% (1+ i%))
      (if (>= i% (length mima))
	(setq i% 0)
      )
    )
  )
  (vl-list->string miwen)
)

;;;350103810626422
;;;(chr 51)
;;;(setq c1 51)
;;;(setq c2 52)
;;;(setq c3 53)
;;;(base64encode (vl-string->list "www.吴建功立业明"))
;;;
(defun base64encode	(strlst / i% strmid len flag c1 c2 c3 base64EncodeChars)
  (setq base64EncodeChars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
;;;  (setq strlst (vl-string->list strlst))
  (setq i% -1)
  (setq strmid "")
  (setq len (1-(length strlst)))
  (setq flag t)
  (while (and flag (< i% len))
    (setq c1 (nth (setq i% (1+ i%)) strlst))
    (if	(= i% len)
      (progn
	(setq
	  strmid (strcat strmid
			 (substr base64EncodeChars (1+ (lsh c1 -2)) 1)
		 )
	)
	(setq strmid
	       (strcat
		 strmid
		 (substr base64EncodeChars
			 (1+ (lsh (boole 1 c1 3) 4))
			 1
		 )
	       )
	)
	(setq strmid (strcat strmid "=="))
	(setq flag nil)
      ) ;_progn
    ) ;_if
    (setq c2 (nth (setq i% (1+ i%)) strlst))
    (if	(and flag (= i% len))
      (progn (setq
	       strmid (strcat strmid
			      (substr base64EncodeChars (1+ (lsh c1 -2)) 1)
		      )
	     )
	     (setq strmid
		    (strcat
		      strmid
		      (substr base64EncodeChars
			      (1+ (boole 7
					 (lsh (boole 1 c1 3) 4)
					 (lsh (boole 1 c2 240) -4)
				  )
			      )
			      1
		      )
		    )
	     )
	     (setq strmid
		    (strcat
		      strmid
		      (substr base64EncodeChars
			      (1+ (lsh (boole 1 c2 15) 2))
			      1
		      )
		    )
	     )
	(setq strmid (strcat strmid "="))
	     (setq flag nil)
      ) ;_prong
    ) ;_if
    (setq c3 (nth (setq i% (1+ i%)) strlst))
    (if	flag
      (progn	
	(setq
	  strmid (strcat strmid
			 (substr base64EncodeChars (1+ (lsh c1 -2)) 1)
		 )
	)
	(setq strmid
	       (strcat
		 strmid
		 (substr base64EncodeChars
			 (1+ (boole 7
				    (lsh (boole 1 c1 3) 4)
				    (lsh (boole 1 c2 240) -4)
			     )
			 )
			 1
		 )
	       )
	)
	(setq strmid
	       (strcat
		 strmid
		 (substr base64EncodeChars
			 (1+ (boole 7
				    (lsh (boole 1 c2 15) 2)
				    (lsh (boole 1 c3 192) -6)
			     )
			 )
			 1
		 )
	       )
	)
	(setq
	  strmid (strcat strmid
			 (substr base64EncodeChars (1+ (boole 1 c3 63)) 1)
		 )
	)
      )
    )
  ) ;_while
 strmid
) ;_defun


;(princ(base64decode (base64encode "12312121")))
;(vl-list->string(base64decode (base64encode (vl-string->list "1234-567892" ))))
;(base64decode (base64encode (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21)))

;;;
;;;(setq strmid "dDwxNTMyNDkwOTcxO3Q8O2w8aTwwPjs+O2w8dDw7bDxpPDg+O2k8OT47aTwxMD47aTwxMj47PjtsPHQ8cDxwPGw8VGV4dDs+O2w8MTU7Pj47Pjs7Pjt0PHA8cDxsPFRleHQ7PjtsPDE7Pj47Pjs7Pjt0PHA8cDxsPFRleHQ7PjtsPDI7Pj47Pjs7Pjt0PHA8cDxsPEVuYWJsZWQ7PjtsPG88Zj47Pj47Pjs7Pjs+Pjs+Pjs+4XBON9/c/lYt/zXavRFYRltiICo=")
;;;(vl-list->string (base64decode strmid))

(defun base64decode(strlst / base64DecodeChars strlst len i% strmid c1 c2 c3 c4 )
  (setq base64DecodeChars (list
    -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 
    -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 
    -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  62  -1  -1  -1  63 
    52  53  54  55  56  57  58  59  60  61  -1  -1  -1  -1  -1  -1 
    -1   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14 
    15  16  17  18  19  20  21  22  23  24  25  -1  -1  -1  -1  -1 
    -1  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40 
    41  42  43  44  45  46  47  48  49  50  51  -1  -1  -1  -1  -1)
       )
  (setq strlst (vl-string->list strlst))
  (setq len (1-(length strlst)))
  (setq i% -1)
  (setq strmid nil)

  (while (< i% len)
    (setq c1 (nth (nth (setq i% (1+ i%)) strlst) base64DecodeChars))
    (if	(/= c1 -1)
      (progn
	(setq c2 (nth (nth (setq i% (1+ i%)) strlst) base64DecodeChars))
	(if (/= c2 -1)
	  (progn 
;;;	    (setq strmid (strcat strmid (chr(boole 7 (lsh c1 2) (lsh (boole 1 c2 48) -4) ))))
	    (setq strmid (append strmid (list (boole 7 (lsh c1 2) (lsh (boole 1 c2 48) -4) ))))
	    ;_out += String.fromCharCode((c1 << 2) | ((c2 & 0x30) >> 4))
	    (setq c3 (nth (setq i% (1+ i%)) strlst))
	    (if (= c3 61)
	      (progn  strmid);_progn return
              (progn (setq c3 (nth c3 base64DecodeChars))
		(if (/= c3 -1)
		  (progn
;;;		    (setq strmid (strcat strmid (chr(boole 7 (lsh (boole 1 c2 15) 4) (lsh (boole 1 c3 60) -2) ))))
		    (setq strmid (append strmid (list (boole 7 (lsh (boole 1 c2 15) 4) (lsh (boole 1 c3 60) -2) ))))
		    ;_out += String.fromCharCode(((c2 & 0XF) << 4) | ((c3 & 0x3C) >> 2))
		    (setq c4 (nth (setq i% (1+ i%)) strlst))
		    (if (/= c4 61)
		      (progn
			(setq c4 (nth c4 base64DecodeChars))
			(if (/= c4 -1)
;;;			  (setq strmid (strcat strmid (chr(boole 7 (lsh (boole 1 c3 3) 4) c4 ))))
			  (setq strmid (append strmid (list (boole 7 (lsh (boole 1 c3 3) 6) c4 ))))
			  ;_out += String.fromCharCode(((c3 & 0x03) << 6) | c4);
			  )
			)
		      )
		    );_c3
		  )
		)
	      )
	    )
	) ;_if c2
      )
    ) ;_if c1
  ) ;_while
  strmid
  );_defun

;|
;;;加密
(setq min_wen (vl-string->list "www.吴建明.com"))
(setq mi_yao (vl-string->list(base64encode(vl-string->list"wujimmy"))))
(setq mi_wen(crypt min_wen mi_yao))
(setq mi_wen(base64encode mi_wen))
;;;加密

;;;解密
(setq mi_wen (base64decode "LTM5eRQiLDEgNStCNjtd"))
(setq mi_yao (vl-string->list(base64encode(vl-string->list"wujimmy"))))
(setq min_wen (crypt mi_wen mi_yao))
(setq min_wen (vl-list->string min_wen))
;;;解密

(setq mi_yao "{{{{{{{{{{")
(setq mi_yao (base64encode (vl-string->list "wujimmy")))
(setq mid (strcat (chr 0) (chr 45)))
|;

(setq mid(boole 6 2002 1982))
(setq mid (boole 6 mid 1982))

(defun crypt (min_wen mi_yao)
  (setq
      mi_yao (vl-string->list (base64encode mi_yao))
    )
  (while (< (length mi_yao) (length min_wen))
    (setq
      mi_yao (vl-string->list (base64encode mi_yao))
    )
  )
  (setq min_wen_lst min_wen)
  (setq mi_yao_lst mi_yao)
  (setq i% 0
	mi_wen nil)
  (repeat (length min_wen_lst)
     (progn
       (setq mi_wen_si (boole 6 (nth i% min_wen_lst) (nth i% mi_yao_lst)))
       ;(setq mi_wen_si (boole 6 mi_wen_si (nth i% mi_yao_lst)))
       (setq mi_wen (append mi_wen (list mi_wen_si)))
       (setq i% (1+ i%))
       );_progn
    );_repeat  
 mi_wen
  )

;;;实体与字符串互换
(defun obj->str (ent)
;;;(setq ent (entget (setq es (single_select (list (cons 0 "text")) nil))
;;;	  )
;;;)
(setq relst (list -1 330 330 5 100 100 102 102 410))
(setq mid (print ent))
(foreach num relst
  (progn
    (setq mid (vl-remove (assoc num mid) mid))
  )
)
(vl-prin1-to-string mid)
)
(defun str->obj	(str)
  (entmake (read (vl-string-translate "'" "\"" str)))
)



;;;(Add-Toolbar-Button
;;;  "CadQQ" "发送消息" "发送消息"	"^C^Csendmsg " (list acToolbarDockLeft))
;;;(Add-Toolbar-Button
;;;  "CadQQ" "发送消息" "发送消息"	"^C^Csendmsg " (list acToolbarFloating 200 200))

(defun Add-Toolbar-Button(groupName buttonName helpString Macro position
			  / AcadObject AcadDocument currMenuGroupSel currMenuGroup
			  newToolBarSel newToolBar toolbar newButton)
  (VL-LOAD-COM)
  (setq AcadObject(vlax-get-acad-object)
        AcadDocument(vla-get-ActiveDocument Acadobject)        
  )

  (setq currMenuGroupSel(vla-get-MenuGroups AcadObject))
  (setq currMenuGroup(vla-Item currMenuGroupSel 0))
  
;;; 建立新的栏 
  (setq newToolBarSel(vla-get-Toolbars currMenuGroup))
  (setq newToolBar nil)
    (vlax-for toolbar newToolBarSel
     (if (= groupName (vla-get-Name toolbar)) (setq newToolBar toolbar)) 
  )
  (if (null newToolBar)(setq newToolBar(vla-add newToolBarSel groupName)))  

  
;;; 添加三个按钮到新的工具栏
;;; 三个按钮附上相同的宏
;;; 指定相当于 VB的 "ESC ESC _open "的宏字符串

;;;  (setq Macro (strcat (chr 3) (chr 3) (chr 95) "getmsg1" (chr 32)))
;;;  (vlax-dump-object newToolBar t)
  (vlax-for Button newToolbar
     (if (= buttonName (vla-get-Name Button)) (setq newButton Button)) 
  )
  (if (null newButton)
  (setq newButton(vla-AddToolbarButton newToolbar 0 buttonName helpString Macro)))
  
;;; 显示工具栏
  (vla-put-Visible newToolBar :vlax-true)
 
    (cond
      ((= acToolbarFloating (car position))
       (vla-Float newToolBar (cadr position) (caddr position) 1)
      )
      ((and (= acToolbarDockLeft (car position))
	    (/= (vla-get-DockStatus newToolBar) (car position))
       )
       (vla-Dock newToolBar (car position))
      )
      ((and (/= (vla-get-DockStatus newToolBar) (car position))
	    (= acToolbarDockRight (car position))
       )
       (vla-Dock newToolBar (car position))
      )
      ((and (/= (vla-get-DockStatus newToolBar) (car position))
	    (= acToolbarDockTop (car position))
       )
       (vla-Dock newToolBar (car position))
      )
      ((and (/= (vla-get-DockStatus newToolBar) (car position))
	    (= acToolbarDockBottom (car position))
       )
       (vla-Dock newToolBar (car position))
      )
    )
;;;;;; 将工具栏固定在屏幕的左端
;;;  (vla-Dock newToolBar acToolbarDockLeft)
;;;;;; 将工具栏设成浮动
;;;  (vla-Float newToolBar 200 200 1)
  (princ)
  )

;|
功能  
在程序运行过程中按下功能建  
语法  
(SendKeys keys )  
参数  
keys：键名  
样例  
，(sendkeys "{F3}") 相当于按下F3键
(sendkeys "{CAPSLOCK}") 相当于按下大小写键
(sendkeys "^c") 相当于按下CTRL+C键
(sendkeys "%F") 相当于按下ALT+F键  
(sendkeys "^{TAB}")
说明  
(if (not commandreactor)
  (setq commandreactor
(vlr-command-reactor
   nil
   '((:vlr-commandwillstart . haha))
)
  )
)
(defun haha (var1 var2)
  (if (wcmatch (vl-princ-to-string (strcase (car var2))) "*PLOT")
    (progn
      (sendkeys "{ESC}")
      (princ "\n不准打印!")
    )
  )
)
|;
(defun SendKeys (keys / ws) 
  (setq ws (vlax-create-object "WScript.Shell")) 
  (vlax-invoke-method ws 'sendkeys keys) 
  (vlax-release-object ws) 
  (princ) 
)


;|
(if strmid
	(progn (eval (read strmid)))
;;;	(progn
;;;	  (setq strlst (convert_string_to_string_lst strmid ":"))
;;;	  (setq file (open (car strlst) "w")) ;_(findfile (car strlst))
;;;	  (setq strmid (vl-string-translate "'" "\"" (cadr strlst)))
;;;	  (write-line strmid file)
;;;	  (close file)
;;;	  (load (car strlst))
;;;;;;清楚文件容
;;;	  (setq file (open (car strlst) "w"))
;;;	  (write-line "" file)
;;;	  (close file)
;;;	  (vl-file-delete (findfile (car strlst)))
;;;	)
      )

|;

;;;********************************************************************;
(defun mySetXData (VlaxEntity String / DxfTypes DxfValues xdatas BuildArrays)
  ;;;--------------------------------------------------------------------;
;;;  This function builds two VARIANTS from the two parameters.        ;
;;;  The first parameter is a list specifying the DXF group codes, the ;
;;;  second list specifies the DXF values.                             ;
;;;  After converting the parameters into safearrays, this function    ;
;;;  creates two variants containing the arrays.                       ;
;;;--------------------------------------------------------------------;
(defun BuildArrays (DxfTypes dxfValues / ListLength Counter
		                         Code VarValue
		                         ArrayTypes ArrayValues
		    			 VarTypes VarValues Result)
  ;; Get length of the lists
  (setq ListLength (1- (length DxfTypes)))
  ;; Create the safearrays for the dxf group code and value
  (setq ArrayTypes (vlax-make-safearray vlax-vbInteger (cons 0 ListLength))
	ArrayValues (vlax-make-safearray vlax-vbVariant (cons 0 ListLength)))
  ;; Set the array elements
  (setq Counter 0)
  (while (<= Counter ListLength)
    (setq Code (nth Counter DxfTypes)
	  VarValue (vlax-make-variant (nth Counter DxfValues)))
    (vlax-safearray-put-element ArrayTypes Counter Code)
    (vlax-safearray-put-element ArrayValues Counter VarValue)
    (setq counter (1+ counter))
  ) ;_ end of while
  ;; Create the two VARIANTs
  (setq VarTypes  (vlax-make-variant ArrayTypes)
	VarValues (vlax-make-variant ArrayValues))
  ;; Create a (Lisp) list which contains the two safearrays and
  ;; return this list.
  (setq Result (list VarTypes VarValues))
  Result
) ;_ end of defun 



  
  (if (/= VlaxEntity nil)
    (progn      
      (if (/= String nil)
	(progn
	  ;; Create two safearrays for the ActiveX method SetXData
	  (setq xdatas (BuildArrays '(1001 1000) (list "PE_URL" String)))
	  ;; Extract the two variants from the returned (Lisp) list
	  (setq DxfTypes (nth 0 xdatas)
		DxfValues (nth 1 xdatas))
	  ;; Set the Xdata
	  ;;(princ (list VlaxEntity DxfTypes DxfValues))
	  (vla-setXData VlaxEntity DxfTypes DxfValues)
          ) ;_ end of progn
      ) ;_ end of if
    ) ;_ end of progn
  ) ;_ end of if
  (princ)				; exit quietly
) ;_ end of defun


;;;(ax:Centroid (car (entsel)))
(defun ax:Centroid (poly / pl ms va reg cen)
  (setq pl (vlax-ename->vla-object poly)
 ms (vla-get-modelspace
      (vla-get-activedocument (vlax-get-acad-object))
    )
 va (vlax-make-safearray vlax-vbObject '(0 . 0))
  )
  (vlax-safearray-put-element va 0 pl)
  (setq reg (car (vlax-safearray->list
     (vlax-variant-value (vla-addregion ms va))
   )
     )
 cen (vla-get-centroid reg)
  )
  (vla-delete reg)
  (vlax-safearray->list (vlax-variant-value cen))
)

;;;排序.
(defun wjmf_sort (listzxjd)
  (setq
    listzxjd (vl-sort
	       listzxjd
	       (function
		 (lambda (e1 e2)
		   (if (< (abs (- (car e1) (car e2))) (if wjm_Y_diet wjm_Y_diet 2e-06)) ;_如果X坐标相等,则按照Y坐标排列
		     (< (cadr e1) (cadr e2))
		     (< (car e1) (car e2))
		   )
		 )
	       )
	     )
  )
)




;;;求交点函数.
;;;(wjmf_Midp e1 e2 ff)
;;;e1 e2是(ssname ss 0)
;;;(setq intersections nil)
;;;ff取值:  (0 都延伸,1 一号不延伸,2 二号不延伸,<3> 全部不延伸)
;;; 测试: (princ(wjmf_Midp (ssname (ssget) 0)(ssname (ssget) 0) 3))
(progn
 ;_判断物体类型,选择相应函数求交点_______________________________________
  (defun wjmf_Midp (en_1 en_2 lim / )
;;;    (setq intersections nil)
    (IF	(LISTP EN_1)
      ()
      (SETQ EN_1 (ENTGET EN_1))
    )
    (IF	(LISTP EN_2)
      ()
      (SETQ EN_2 (ENTGET EN_2))
    )
    (setq e1_tp (cdr (assoc 0 EN_1)))
    (setq e2_tp (cdr (assoc 0 EN_2)))
    (cond
      ((= "LWPOLYLINE" e1_tp)
       (pi? en_1 en_2 lim)
      )
      ((= "LWPOLYLINE" e2_tp)
       (pi? en_2 en_1 (change_order lim))
      )
      ((and (= "CIRCLE" e1_tp) (= "CIRCLE" e2_tp))
       (cic en_1 en_2 lim)
      )
      ((and (= "ARC" e1_tp) (= "ARC" e2_tp))
       (aia en_1 en_2 lim)
      )
      ((and (= "LINE" e1_tp) (= "LINE" e2_tp))
       (lil en_1 en_2 lim)
      )

      ((and (= "CIRCLE" e1_tp) (= "ARC" e2_tp))
       (cia en_1 en_2 lim)
      )
      ((and (= "CIRCLE" e1_tp) (= "LINE" e2_tp))
       (cil en_1 en_2 lim)
      )
      ((and (= "ARC" e1_tp) (= "LINE" e2_tp))
       (ail en_1 en_2 lim)
      )

      ((and (= "ARC" e1_tp) (= "CIRCLE" e2_tp))
       (cia en_2 en_1 (change_order lim))
      )
      ((and (= "LINE" e1_tp) (= "CIRCLE" e2_tp))
       (cil en_2 en_1 (change_order lim))
      )
      ((and (= "LINE" e1_tp) (= "ARC" e2_tp))
       (ail en_2 en_1 (change_order lim))
      )
      (T (princ "\n请选择圆、圆弧或者线段！"))
    )
    intersections
  )
 ;_反转延长标志_________________________________________________________
  (defun change_order (num)
    (cond
      ((= num 1) (setq num 2))
      ((= num 2) (setq num 1))
    )
    num
  )
 ;______________________________________________________________________
 ;______________________________________________________________________
 ;_________________求交点应用函数部分____BY__WKAI__晓东CAD论坛__________
 ;___________________2003.12.11.14.33___________________________________
 ;____limited__决定求交点时物体是否延伸_________________________________
 ;____0 都延伸,1 一号不延伸,2 二号不延伸,3 全部不延伸___________________

 ;_圆、圆交点___________________________________________________________
  (defun cic (c1 c2 limited / c1_cn c2_cn c1_rd c2_rd ins)
    (IF	(LISTP C1)
      ()
      (SETQ C1 (ENTGET C1))
    )
    (IF	(LISTP C2)
      ()
      (SETQ C2 (ENTGET C2))
    )
    (setq c1_cn (cdr (assoc 10 c1)))
    (setq c2_cn (cdr (assoc 10 c2)))
    (setq c1_rd (cdr (assoc 40 c1)))
    (setq c2_rd (cdr (assoc 40 c2)))
    (setq ins (c_int_c c1_cn c1_rd c2_cn c2_rd))
    (if	ins
      (setq intersections (append intersections ins))
    )
    intersections
  )
 ;_圆、圆弧交点_________________________________________________________
  (defun cia (c1 c2 limited / ins ins_tmp c1_cn c2_cn c1_rd c2_rd n)
    (IF	(LISTP C1)
      ()
      (SETQ C1 (ENTGET C1))
    )
    (IF	(LISTP C2)
      ()
      (SETQ C2 (ENTGET C2))
    )
    (setq c1_cn (cdr (assoc 10 c1)))
    (setq c2_cn (cdr (assoc 10 c2)))
    (setq c1_rd (cdr (assoc 40 c1)))
    (setq c2_rd (cdr (assoc 40 c2)))
    (setq c2_an1 (cdr (assoc 50 c2)))
    (setq c2_an2 (cdr (assoc 51 c2)))
    (setq ins (c_int_c c1_cn c1_rd c2_cn c2_rd))
    (if	(or (= limited 2) (= limited 3))
      (progn
	(foreach n ins
	  (if (p_on_arc n c2_cn c2_an1 c2_an2)
	    (if	ins_tmp
	      (setq ins_tmp (append ins_tmp (list n)))
	      (setq ins_tmp (list n))
	    )
	  )
	)
	(setq ins ins_tmp)
      )
    )
    (setq ins ins_tmp)
    (if	ins
      (setq intersections (append intersections ins))
    )
    intersections
  )
 ;_圆弧、圆弧交点________________________________________________________
  (defun aia (c1      c2      limited /	      ins     ins_tmp c1_cn
	      c2_cn   c1_rd   c2_rd   c1_an1  c1_an2  c2_an1  c2_an2
	      n
	     )
    (IF	(LISTP C1)
      ()
      (SETQ C1 (ENTGET C1))
    )
    (IF	(LISTP C2)
      ()
      (SETQ C2 (ENTGET C2))
    )
    (setq c1_cn (cdr (assoc 10 c1)))
    (setq c2_cn (cdr (assoc 10 c2)))
    (setq c1_rd (cdr (assoc 40 c1)))
    (setq c2_rd (cdr (assoc 40 c2)))
    (setq c1_an1 (cdr (assoc 50 c1)))
    (setq c1_an2 (cdr (assoc 51 c1)))
    (setq c2_an1 (cdr (assoc 50 c2)))
    (setq c2_an2 (cdr (assoc 51 c2)))
    (setq ins (c_int_c c1_cn c1_rd c2_cn c2_rd))
    (if	(or (= limited 1) (= limited 3))
      (progn
	(foreach n ins
	  (if (p_on_arc n c1_cn c1_an1 c1_an2)
	    (if	ins_tmp
	      (setq ins_tmp (append ins_tmp (list n)))
	      (setq ins_tmp (list n))
	    )
	  )
	)
	(setq ins ins_tmp)
      )
    )
    (setq ins_tmp nil)
    (if	(or (= limited 2) (= limited 3))
      (progn
	(foreach n ins
	  (if (p_on_arc n c2_cn c2_an1 c2_an2)
	    (if	ins_tmp
	      (setq ins_tmp (append ins_tmp (list n)))
	      (setq ins_tmp (list n))
	    )
	  )
	)
	(setq ins ins_tmp)
      )
    )
    (if	ins
      (setq intersections (append intersections ins))
    )
    intersections
  )
 ;_圆、直线交点______________________________________________________
  (defun cil (c1 l1 limited / end1 end2 cen rad ins ins_tmp n)
    (IF	(LISTP C1)
      ()
      (SETQ C1 (ENTGET C1))
    )
    (IF	(LISTP L1)
      ()
      (SETQ L1 (ENTGET L1))
    )
    (setq end1 (cdr (assoc 10 l1)))
    (setq end2 (cdr (assoc 11 l1)))
    (setq cen (cdr (assoc 10 c1)))
    (setq rad (cdr (assoc 40 c1)))
    (setq ins (L_INT_C end1 end2 cen rad))
    (if	(or (= limited 2) (= limited 3))
      (progn
	(foreach n ins
	  (if (p_on_line n end1 end2)
	    (if	ins_tmp
	      (setq ins_tmp (append ins_tmp (list n)))
	      (setq ins_tmp (list n))
	    )
	  )
	)
	(setq ins ins_tmp)
      )
    )
    (if	ins
      (setq intersections (append intersections ins))
    )
    intersections
  )
 ;_圆弧、直线交点______________________________________________________
  (defun ail
	 (c1 l1 limited / end1 end2 cen rad ang1 ang2 ins ins_tmp n)
    (IF	(LISTP C1)
      ()
      (SETQ C1 (ENTGET C1))
    )
    (IF	(LISTP L1)
      ()
      (SETQ L1 (ENTGET L1))
    )
    (setq end1 (cdr (assoc 10 l1)))
    (setq end2 (cdr (assoc 11 l1)))
    (setq cen (cdr (assoc 10 c1)))
    (setq rad (cdr (assoc 40 c1)))
    (setq ang1 (cdr (assoc 50 c1)))
    (setq ang2 (cdr (assoc 51 c1)))
    (setq ins (L_INT_C end1 end2 cen rad))
    (if	(or (= limited 1) (= limited 3))
      (progn
	(foreach n ins
	  (if (p_on_arc n cen ang1 ang2)
	    (if	ins_tmp
	      (setq ins_tmp (append ins_tmp (list n)))
	      (setq ins_tmp (list n))
	    )
	  )
	)
	(setq ins ins_tmp)
      )
    )
    (setq ins_tmp nil)
    (if	(or (= limited 2) (= limited 3))
      (progn
	(foreach n ins
	  (if (p_on_line n end1 end2)
	    (if	ins_tmp
	      (setq ins_tmp (append ins_tmp (list n)))
	      (setq ins_tmp (list n))
	    )
	  )
	)
	(setq ins ins_tmp)
      )
    )
    (if	ins
      (setq intersections (append intersections ins))
    )
    intersections
  )
 ;_直线、直线交点______________________________________________________
  (defun lil
	 (l1 l2 limited / n ins ins_tmp l1_en1 l1_en2 l2_en1 l2_en2)
    (if	(listp l1)
      ()
      (setq l1 (entget l1))
    )
    (if	(listp l2)
      ()
      (setq l2 (entget l2))
    )
    (setq l1_en1 (cdr (assoc 10 l1)))
    (setq l1_en2 (cdr (assoc 11 l1)))
    (setq l2_en1 (cdr (assoc 10 l2)))
    (setq l2_en2 (cdr (assoc 11 l2)))
    (if	(setq ins_tmp (inters l1_en1 l1_en2 l2_en1 l2_en2 nil))
      (setq ins (list ins_tmp))
    )
    (setq ins_tmp nil)
    (if	(or (= limited 1) (= limited 3))
      (progn
	(foreach n ins
	  (if (p_on_line n l1_en1 l1_en2)
	    (setq ins_tmp (list n))
	  )
	)
	(setq ins ins_tmp)
      )
    )
    (setq ins_tmp nil)
    (if	(or (= limited 2) (= limited 3))
      (progn
	(foreach n ins
	  (if (p_on_line n l2_en1 l2_en2)
	    (setq ins_tmp (list n))
	  )
	)
	(setq ins ins_tmp)
      )
    )
    (if	ins
      (setq intersections (append intersections ins))
    )
    intersections
  )
 ;_复义线、其它实体交点______________________________________________________
 ;_如果是两条复义线通过递归求交______________________________________________
  (defun pi? (pl1     e2      lim     /	      p1      p2      p3
	      pts-pl1 n	      sym1    sym2    ang1    ang2
	      pl1-sub-ent
	     )
    (if	(listp pl1)
      ()
      (setq pl1 (entget pl1))
    )
    (if	(listp e2)
      ()
      (setq e2 (entget e2))
    )
    (setq pts-pl1 (GET_ENDS_PL pl1))
    (setq n 1)
    (while (< (+ 1 n) (length pts-pl1))
      (setq p1 (nth (- n 1) pts-pl1))
      (setq p2 (nth n pts-pl1))
      (setq p3 (nth (+ n 1) pts-pl1))
      (if (listp p2)
	(progn
	  (setq sym1 (car p2))
	  (setq p2 (cdr p2))
	  (if (= 1 sym1)
	    (setq ang1 (angle p2 p1)
		  ang2 (angle p2 p3)
	    )
	    (setq ang1 (angle p2 p3)
		  ang2 (angle p2 p1)
	    )
	  )
	  (setq	pl1-sub-ent
		 (list (cons 0 "ARC")
		       (cons 10 p2)
		       (cons 40 (distance p1 p2))
		       (cons 50 ang1)
		       (cons 51 ang2)
		       (cons 62 1)
		 )
	  )
	)
	(setq pl1-sub-ent
	       (list (cons 0 "LINE") (cons 10 p1) (cons 11 p3) (cons 62 1))
	)
      )
      (wjmf_Midp pl1-sub-ent e2 lim)
      (setq n (+ 2 n))
    )
  )
 ;______________________________________________________________________
 ;______________________________________________________________________
 ;_________________求交点核心函数部分____BY__WKAI__晓东CAD论坛__________
 ;___________________2003.12.11.14.33___________________________________
 ;______________________________________________________________________
 ;_精度设置_____________________________________________________________
  (setq min_num 0.0000001)
 ;___________________圆与圆交点函数,输入值圆心1,半径1,圆心2,半径2.返回值交点表
  (defun c_int_c (c1_cen c1_rad c2_cen c2_rad / ints c1c2_dis dd ee)
    (setq c1c2_dis (distance c1_cen c2_cen))
    (cond
      ((equal c1c2_dis (+ c1_rad c2_rad) min_num)
       (setq ints (list (polar c1_cen (angle c1_cen c2_cen) c1_rad)))
      )
      ((equal c1c2_dis (abs (- c1_rad c2_rad)) min_num)
       (if (minusp (- c1_rad c2_rad))
	 (setq ints (list (polar c2_cen (angle c2_cen c1_cen) c2_rad)))
	 (setq ints (list (polar c1_cen (angle c1_cen c2_cen) c1_rad)))
       )
      )
      ((and (> c1c2_dis (abs (- c1_rad c2_rad)))
	    (< c1c2_dis (+ c1_rad c2_rad))
       )
       (progn
	 (setq dd (/ (-	(+ (* c1c2_dis c1c2_dis) (* c1_rad c1_rad))
			(* c2_rad c2_rad)
		     )
		     (* 2 c1c2_dis)
		  )
	 )
	 (setq ee (sqrt (- (* c1_rad c1_rad) (* dd dd))))
	 (setq
	   ints	(list (polar (polar c1_cen (angle c1_cen c2_cen) dd)
			     (+ (angle c1_cen c2_cen) (/ pi 2))
			     ee
		      )
		)
	 )
	 (setq ints
		(append
		  ints
		  (list	(polar (polar c1_cen (angle c1_cen c2_cen) dd)
			       (- (angle c1_cen c2_cen) (/ pi 2))
			       ee
			)
		  )
		)
	 )

       )
      )
    )
    ints
  )
 ;___________________直线与圆交点函数,输入值直线端点1,端点2,圆心,半径.返回值交点表
  (defun L_INT_C (l_end1 l_end2	c_cen c_rad / pedal dist_cen_l int1 int2
		  ints)
    (setq pedal (pedal_to_line c_cen l_end1 l_end2))
    (setq dist_cen_l (distance pedal c_cen))
    (cond
      ((equal c_rad dist_cen_l min_num) (setq ints (list pedal)))
      ((> c_rad dist_cen_l)
       (progn
	 (setq int1
		(polar pedal
		       (angle l_end1 l_end2)
		       (sqrt (- (* c_rad c_rad) (* dist_cen_l dist_cen_l)))
		)
	 )
	 (setq int2
		(polar pedal
		       (+ pi (angle l_end1 l_end2))
		       (sqrt (- (* c_rad c_rad) (* dist_cen_l dist_cen_l)))
		)
	 )
	 (setq ints (list int1 int2))
       )
      )
    )
    ints
  )
 ;______________________________________________________________________
 ;______________________________________________________________________
 ;_________________辅助测试函数部分____BY__WKAI__晓东CAD论坛____________
 ;___________________2003.12.11.14.33___________________________________
 ;______________________________________________________________________
 ;___________________求点到直线的垂足的函数,输入值测试点,直线端点1,端点2.返回值垂足坐标
  (defun pedal_to_line (pt pt1 pt2)
    (inters
      pt
      (polar pt (+ (/ pi 2) (angle pt1 pt2)) 1000)
      pt1
      pt2
      nil
    )
  )
 ;___________________测试点是否在线段上,输入值测试点,线段端点1,端点2.返回值T或者NIL
  (defun p_on_line (pt pt1 pt2)
    (equal (+ (distance pt pt1) (distance pt pt2))
	   (distance pt1 pt2)
	   min_num
    )
  )
 ;___________________测试点是否在圆弧上,输入值测试点,圆心,起始角度,终止角度.返回值T或者NIL
  (defun p_on_arc (pt cn an1 an2)
    (if	(> an1 an2)
      (setq an1 (- an1 (* 2 pi)))
    )
    (or
      (and (>= (+ (angle cn pt) pi pi) an1)
	   (<= (+ (angle cn pt) pi pi) an2)
      )
      (and (>= (angle cn pt) an1) (<= (angle cn pt) an2))
      (and (>= (- (angle cn pt) pi pi) an1)
	   (<= (- (angle cn pt) pi pi) an2)
      )
    )
  )
 ;___________________获取轻装多义线的各个端点和圆心(如果有),输入值复义线实体名或表.返回值端点及圆心表
  (DEFUN GET_ENDS_PL (PL      /	      dis     dis1    m	      N
		      PT-LST  PT-LST-TMP      sym     mid-p1p2
		      NTH-PT  p1      p2      pl-tp   pt1     rad
		      sym
		     )
    (IF	(LISTP PL)
      ()
      (SETQ PL (ENTGET PL))
    )
    (SETQ PL-TP (CDR (ASSOC 70 PL)))
    (FOREACH N PL
      (IF (OR (= 10 (CAR N)) (= 42 (CAR N)))
	(SETQ PT-LST (APPEND PT-LST (LIST (CDR N))))
      )
    )
    (IF	(= 1 PL-TP)
      (SETQ PT-LST (APPEND PT-LST (LIST (CDR (ASSOC 10 PL)))))
      (SETQ PT-LST (reverse (cdr (reverse PT-LST))))
    )
    (SETQ M 0)
    (while (<= (+ 1 m) (LENGTH PT-LST))
      (SETQ NTH-PT (NTH M PT-LST))
      (IF (LISTP NTH-PT)
	(SETQ PT-LST-TMP (APPEND PT-LST-TMP (LIST NTH-PT)))
	(PROGN
	  (IF (EQUAL NTH-PT 0 MIN_NUM)
	    (SETQ PT-LST-TMP (APPEND PT-LST-TMP (LIST NTH-PT)))
	    (PROGN
	      (SETQ P1 (NTH (- M 1) PT-LST))
	      (SETQ P2 (NTH (+ M 1) PT-LST))
	      (SETQ MID-P1P2 (LIST (/ (+ (CAR P1) (CAR P2)) 2)
				   (/ (+ (CADR P1) (CADR P2)) 2)
			     )
	      )
	      (SETQ DIS (/ (DISTANCE P1 P2) 2))
	      (SETQ DIS1 (ABS (* DIS NTH-PT)))
	      (SETQ RAD (/ (+ (* DIS DIS) (* DIS1 DIS1)) (* 2 DIS1)))
	      (IF (minusp NTH-PT)
		(SETQ
		  PT1 (append (list -1)
			      (POLAR MID-P1P2
				     (- (ANGLE P1 P2) (/ PI 2))
				     (- RAD DIS1)
			      )
		      )
		)
		(SETQ
		  PT1 (append (list 1)
			      (POLAR MID-P1P2
				     (+ (ANGLE P1 P2) (/ PI 2))
				     (- RAD DIS1)
			      )
		      )
		)
	      )
	      (SETQ PT-LST-TMP (APPEND PT-LST-TMP (LIST PT1)))
	    )
	  )
	)
      )
      (SETQ M (+ 1 M))
    )
    (SETQ PT-LST PT-LST-TMP)
  )
 ;_________________________________________________________________________________________
 ;_________________________________________________________________________________________
 ;_________________________________________________________________________________________
 ;_________________________________________________________________________________________
);_求交点函数.