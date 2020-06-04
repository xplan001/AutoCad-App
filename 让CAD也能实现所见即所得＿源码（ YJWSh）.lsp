(defun c:YJWSh (/		       YJWSH_CENTERPOINT
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
;;;以下得到调整系数并调整视图大小
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
