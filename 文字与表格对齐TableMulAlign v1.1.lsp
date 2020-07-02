;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;表格多行文字对齐[左中/中间/右中]TableMulAlign
;;v1.0 基本完成 by woyb 20131212
;;v1.1 优化共框判断的代码，文字中心点y值大于框的y2值时，结束当前共框判断 by woyb 20131215
(defun c:BGWZDQ (/ txtss ty lst lst1 lst2 lsti len 
				txt txtpt n i leni pxi pyi txtent pt0
				box pts pt1 pt2 px1 py1 px2 py2
			)
	(vl-load-com)
	(princ "\n选择要对齐的文本对象: ")
	(setq txtss (ssget '((0 . "TEXT"))))
	(initget "s d f")
	(setq ty (getkword "\n左中[s]/中间[d]/右中[f]: <d>"))
	(if (not ty) (setq ty "d"))
	(WYB-undob)
	;建文字列
	(setq lst '() lst1 '() lst2 '() lsti '())
	(setq len (sslength txtss));文字总数
	(repeat (setq n len)
		(setq txt (ssname txtss (setq n (1- n))))
		(setq txtpt (WYB-GetBoxCenter txt))
		(setq lst1 (cons (list txtpt txt) lst1))
	)
	(setq lst1
		(vl-sort lst1
			(function
				(lambda
					(e1 e2) 
					(< (cadr (car e1)) (cadr (car e2)))
				)
			)
		)
	)
	(setq leni 0) ;文字计数
	;共框判断
	(while (/= leni len)
		(setq txt (cadr (car lst1)));首个对象处理
		(setq txtpt (car (car lst1)))
		(setq lst1 (cdr lst1))
		(vl-cmdf "boundary" "a" "o" "p" "" txtpt "")
		(setq box (entlast));表格框
		(setq pts (WYB-GetBox box))
		(entdel box)
		(setq pt1 (car pts) pt2 (cadr pts));表格框的点坐标
		(setq px1 (car pt1) py1 (cadr pt1))
		(setq px2 (car pt2) py2 (cadr pt2))
		(setq lsti (cons (list px1 py1 px2 py2) lsti)) ;框坐标入共框列
		(setq lsti (cons txt lsti)) ;文字入共框列
		(setq leni (1+ leni))
		(while (/= lst1 nil)
			(setq txt (cadr (car lst1)));第二个对象处理
			(setq txtpt (car (car lst1)))
			(setq pxi (car txtpt) pyi (cadr txtpt));文字中心点坐标
			(if (< pyi py2)
				(progn ;y共框
					(if (< px1 pxi px2)
						(progn ;共框，入共框列
							(setq lsti (cons txt lsti))
							(setq leni (1+ leni))
						)
						(setq lst2 (cons (list txtpt txt) lst2)) ;不共框，入临时列
					)
					(setq lst1 (cdr lst1))
				)
				(progn
					(setq lst1 (reverse lst1))
					(setq lst2 (append lst1 lst2))
					(setq lst1 '())
				)
			)
		)
		(setq lsti (reverse lsti))
		(setq lst2 (reverse lst2))
		(setq lst (cons lsti lst)) ;共框列入列合集
		(setq lst1 lst2) ;非共框文字列返回
		(setq lst2 '())
		(setq lsti '())
	)
	(repeat (length lst) ;列集合循环
		(setq lsti (car lst))
		(setq lst (cdr lst))
		(setq pts (car lsti))
		(setq lsti (cdr lsti))
		(setq px1 (nth 0 pts) py1 (nth 1 pts))
		(setq px2 (nth 2 pts) py2 (nth 3 pts))
		(setq n (length lsti) i 0)
		(setq py (/ (- py2 py1) n))
		(repeat n ;共框列循环
			(setq tx (car lsti))
			(setq lsti (cdr lsti))
			(setq txtent (entget tx))
			(cond
				((= ty "s");左中
					(progn
						(setq txtent (subst (cons 72 0) (assoc 72 txtent) txtent))
						(setq txtent (subst (cons 73 2) (assoc 73 txtent) txtent))
						(setq pt0 (list (+ px1 1) (+ (* py (+ i 0.5)) py1) 0))
						(setq i (1+ i))
						(setq txtent (subst (cons 11 pt0) (assoc 11 txtent) txtent))
					)
				)
				((= ty "d");中间
					(progn
						(setq txtent (subst (cons 72 4) (assoc 72 txtent) txtent))
						(setq txtent (subst (cons 73 0) (assoc 73 txtent) txtent))
						(setq pt0 (list (* (+ px1 px2) 0.5) (+ (* py (+ i 0.5)) py1) 0))
						(setq i (1+ i))
						(setq txtent (subst (cons 11 pt0) (assoc 11 txtent) txtent))
					)
				)
				((= ty "f");右中
					(progn
						(setq txtent (subst (cons 72 2) (assoc 72 txtent) txtent))
						(setq txtent (subst (cons 73 2) (assoc 73 txtent) txtent))
						(setq pt0 (list (- px2 1) (+ (* py (+ i 0.5)) py1) 0))
						(setq i (1+ i))
						(setq txtent (subst (cons 11 pt0) (assoc 11 txtent) txtent))
					)
				)
			)
			(entmod txtent)
		)
	)
	(WYB-undoe)
	(princ "\n操作完成")
	(princ)
)


;;;;;;;;;;;;;;;;;;;;
;; 获取对象正中点
;;(WYB-GetBoxCenter 对象)
(defun WYB-GetBoxCenter (e / obj minpoint maxpoint)
    (if (= 'ENAME (type e))
        (setq obj (vlax-ename->vla-object e)) ;转换图元名
        (setq obj e)
    )
    (vla-GetBoundingBox obj 'minpoint 'maxpoint) ;取得包容图元的最大点和最小点
    (setq minpoint (vlax-safearray->list minpoint)) ;把变体数据转化为表
    (setq maxpoint (vlax-safearray->list maxpoint)) ;把变体数据转化为表
    (setq p (mapcar '+ minpoint maxpoint))
    (mapcar '(lambda (x) (* 0.5 x)) p)
)
;;;;;;;;;;;;;;;;;;;;
;;取得对象外矩形框
;;By Longxin 明经通道 2005.06
;;(WYB-GetBox 对象)
;;返回: ((x1 y1 z1)_min (x2 y2 z2)_max)
(defun WYB-GetBox (e / obj minpoint maxpoint)
    (if (= 'ENAME (type e))
        (setq obj (vlax-ename->vla-object e)) ;转换图元名
        (setq obj e)
    )
    (vla-GetBoundingBox obj 'minpoint 'maxpoint) ;取得包容图元的最大点和最小点
    (setq minpoint (vlax-safearray->list minpoint)) ;把变体数据转化为表
    (setq maxpoint (vlax-safearray->list maxpoint)) ;把变体数据转化为表
    (setq obj (list minpoint maxpoint))
)
;;;;;;;;;;;;;;;;;;;;
;;关命令响应，开始undo
;;(WYB-undob)
(defun WYB-undob()
    (setvar "cmdecho" 0)
    (command ".undo" "be")
)
;;;;;;;;;;;;;;;;;;;;
;;开启命令相应，结束undo
;;(WYB-undoe)
(defun WYB-undoe()
    (command ".undo" "e")
    (setvar "cmdecho" 1)
)