;;;;;;;物体（文字、曲线、块）与线平齐;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;BY yjr111 2011-10-25;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:dq(/ e1 e2 ee eee s11  p11 ang1 ang2 an lst11 JIAODU1  JIAODU2 JIAODU3)
  (vl-load-com)
  (setvar "cmdecho" 0)
  (setq e1 (car(setq ee(nentselp"\n 请选择要对齐的物体："))))
  (setq s11 (entget e1))
  
  (if (wcmatch(cdr(assoc 0 s11))"*TEXT")
    (progn
      (setq p11 (cdr(assoc 10 s11)))
      (setq ang1 (cdr(assoc 50 s11))))
    (progn
  (setq lst11(nentselp (setq p11(cadr ee))))
  (qvxianjiaodu lst11 )
  (setq ang1 an))
    )
  (SETQ JIAODU1 (* (/ ang1 PI)180))
  (setq an nil)
  (setq e2 (car(setq eee(nentselp"\n 请选择物体要对齐的曲线"))))
  (setq lst11(nentselp (cadr eee)))
  (qvxianjiaodu lst11 )
  (setq ang2 an) 
  (SETQ JIAODU2 (* (/ ang2 PI)180))
  (setq an nil)
 ;;;;;;;;;;;;;;旋转平移;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
    
    (IF (> (LENGTH ee)2) (PROGN (SETQ e1 (CAAR (REVERSE ee)))
	(setq p11 (cdr(assoc 10 (ENTGET e1)))
	jiaodu3 (*(/(cdr(assoc 50 (ENTGET e1)))PI)180))))
  (setq jiaodu (- jiaodu2 jiaodu1))
  
  (cond
         ((and(and e1 e2)(> (LENGTH ee)2))
	   (command "_.rotate" e1 "" p11 (- jiaodu jiaodu3))  
	   (command  "_.MOVE" e1 ""  p11  pause))
	
      (t(command "_.rotate" e1 "" p11   jiaodu )  
	   (command  "_.MOVE" e1 ""  p11  pause))
	
  )
    
  (princ)

)



;;;;;;;;;;;;;引用highflybir的块切线的大部分代码,qvxianjiaodu部分有修改，在此致谢！;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun qvxianjiaodu (lst11 /  Bs en LST11 m pp  P1 P2 P3 P4 P5 P6 vt rt D L S)
  (defun CheckIsCurve(en / dxf typ)					;检查是否是曲线
    (and
      en								;存在实体
      (setq dxf (entget en))						;DXF码
      (setq typ (cdr (assoc 0 dxf)))					;图元类型
      (or (member typ '("ELLIPSE" "CIRCLE" "ARC" "RAY"))		
          (wcmatch typ "*LINE")
      )
    )
  )
    
  (if lst11      (progn                                                 ;;;原程序为while p0
    
    (setq en (car lst11))						;光标处图元
    (if (CheckIsCurve en)
      (setq P12 (cadr lst11)						;光标点
	    m  (caddr lst11)						;变换矩阵
	    Bs (cadddr lst11)						;块参照列表(可能有嵌套)
	    P1 (TransNested P12 Bs 1 2) 				;把点变换到图块坐标系
	    P1 (vlax-curve-getclosestpointto en P1)			;得到最近点
	    pp (vlax-curve-getParamAtPoint en P1)			;得到这点参数
	    vt (vlax-curve-getFirstDeriv en pp)				;得到切线
  	    an (angle '(0 0 0) vt)				        ;切线角
	    
	    
      )
      (princ "\n你没点中或者此处不是曲线类物体!")
    )
  ))
  (princ)
)

;;; 矢量的点积                                                          
;;; VXV Returns the dot product of 2 vectors                            
(defun vxv (v1 v2)
  (apply '+ (mapcar '* v1 v2))
)

;;; 矢量转置                                                            
;;; TRP Transpose a matrix -Doug Wilson-                                
(defun trp (m)
  (apply 'mapcar (cons 'list m))
)

;;; 矢量的矩阵变换                                                      
;;; MXV Apply a transformation matrix to a vector -Vladimir Nesterovsky-
(defun mxv (m v)
  (mapcar (function (lambda (r) (vxv r v))) m)
)

;;; 点到矩阵的变换
(defun mxp (m p)
  (reverse (cdr (reverse (mxv m (append p '(1.0))))))
)

;;; 矩阵相乘                                                            
;;; MXM Multiply two matrices -Vladimir Nesterovsky-                    
(defun mxm (m q)
  (mapcar (function (lambda (r) (mxv (trp q) r))) m)
)

;;;位移变换                                                             
(defun translation (mat vec)
  (mapcar (function
	    (lambda (x y)
	      (list (car x) (cadr x) (caddr x) (+ (cadddr x) y))
	    )
	  )
	  mat
	  vec
  )
)

;;;两矢量的叉积                                                         
;;; Vector Cross Product - Lee Mac					
;;; Args: u,v - vectors in R^3						
(defun v^v ( u v )
  (list
    (- (* (cadr u) (caddr v)) (* (cadr v) (caddr u)))
    (- (* (car  v) (caddr u)) (* (car  u) (caddr v)))
    (- (* (car  u) (cadr  v)) (* (car  v) (cadr  u)))
  )
)
;; TransNested (gile)
;; Translates a point coordinates from WCS or UCS to RCS -coordinates system of a
;; reference (xref or block) whatever its nested level-
;;
;; Arguments
;; pt : the point to translate
;; rlst : the parents entities list from the deepest nested to the one inserted in
;;        current space -same as (last (nentsel)) or (last (nentselp))
;; from to : as with trans function: 0 for WCS, 1 for current UCS, 2 for RCS

(defun TransNested (pt rlst from to)
  (and (= 1 from) (setq pt (trans pt 1 0)))
  (and (= 2 to) (setq rlst (reverse rlst)))
  (and (or (= 2 from) (= 2 to))
       (while rlst
	(setq geom (if	(= 2 to)
		      (RevRefGeom (car rlst))
		      (RefGeom (car rlst))
		    )
	       rlst (cdr rlst)
	       pt   (mapcar '+ (mxv (car geom) pt) (cadr geom))
	)
       )
  )
  (if (= 1 to)
    (trans pt 0 1)
    pt
  )
)

;; RefGeom (gile)
;; Returns a list which first item is a 3x3 transformation matrix (rotation,
;; scales, normal) and second item the object insertion point in its parent
;; (xref, bloc or space)
;;
;; Argument : an ename

(defun RefGeom (ename / elst ang norm mat)
  (setq	elst (entget ename)
	ang  (cdr (assoc 50 elst))
	norm (cdr (assoc 210 elst))
  )
  (list
    (setq mat
	   (mxm
	     (mapcar (function (lambda (v) (trans v 0 norm T)))
		     '((1.0 0.0 0.0) (0.0 1.0 0.0) (0.0 0.0 1.0))
	     )
	     (mxm
	       (list (list (cos ang) (- (sin ang)) 0.0)
		     (list (sin ang) (cos ang) 0.0)
		     '(0.0 0.0 1.0)
	       )
	       (list (list (cdr (assoc 41 elst)) 0.0 0.0)
		     (list 0.0 (cdr (assoc 42 elst)) 0.0)
		     (list 0.0 0.0 (cdr (assoc 43 elst)))
	       )
	     )
	   )
    )
    (mapcar
      '-
      (trans (cdr (assoc 10 elst)) norm 0)
      (mxv mat
	   (cdr (assoc 10 (tblsearch "BLOCK" (cdr (assoc 2 elst)))))
      )
    )
  )
)

;; RevRefGeom (gile)
;; RefGeom inverse function

(defun RevRefGeom (ename / entData ang norm mat)
  (setq	entData	(entget ename)
	ang	(- (cdr (assoc 50 entData)))
	norm	(cdr (assoc 210 entData))
  )
  (list
    (setq mat
	   (mxm
	     (list (list (/ 1 (cdr (assoc 41 entData))) 0.0 0.0)
		   (list 0.0 (/ 1 (cdr (assoc 42 entData))) 0.0)
		   (list 0.0 0.0 (/ 1 (cdr (assoc 43 entData))))
	     )
	     (mxm
	       (list (list (cos ang) (- (sin ang)) 0.0)
		     (list (sin ang) (cos ang) 0.0)
		     '(0.0 0.0 1.0)
	       )
	       (mapcar (function (lambda (v) (trans v norm 0 T)))
		       '((1.0 0.0 0.0) (0.0 1.0 0.0) (0.0 0.0 1.0))
	       )
	     )
	   )
    )
    (mapcar '-
	    (cdr (assoc 10 (tblsearch "BLOCK" (cdr (assoc 2 entData)))))
	    (mxv mat (trans (cdr (assoc 10 entData)) norm 0))
    )
  )
)


