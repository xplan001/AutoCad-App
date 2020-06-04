
;;;功能：多义线添加顶点 
;;;用法：(Add_Vertex <多义线ename>  <点pt> <凸度bulge>) 
;;;如果点pt在多义线上，则添加该点作为多义线的一个顶点，并保持该点处的曲率不变； 
;;;如果点pt不在多义线上，则添加该点作为多义线的最后一个顶点，并使新加子段的凸度为bulge。 
(defun Add_Vertex (ename pt bulge / obj n dm d1 d2 pcen v plist) 
  (setq obj (vlax-ename->vla-object ename)) 
  (if (vlax-curve-getParamAtPoint obj pt) 
    (progn 
  (setq n (fix (vlax-curve-getParamAtPoint obj pt))) 
  (setq dm (vlax-curve-getDistAtPoint obj pt)) 
  (setq d1 (vlax-curve-getDistAtParam obj n)) 
  (setq d2 (vlax-curve-getDistAtParam obj (1+ n))) 
  (setq v (vlax-curve-getsecondderiv obj n)) 
  (if (zerop (vla-getbulge obj n)) 
    (vla-AddVertex obj (1+ n) (ax:2DPoint pt)) 
    (progn 
      (if (> (vla-getbulge obj n) 0) 
    (setq pcen (mapcar '+ (vlax-safearray->list (vlax-variant-value (vla-get-Coordinate obj n))) v)) 
    (setq pcen (mapcar '- (vlax-safearray->list (vlax-variant-value (vla-get-Coordinate obj n))) v)) 
      ) 
      (setq ang2m 
        (* 0.25 
           (- (angle pcen (vlax-safearray->list (vlax-variant-value(vla-get-Coordinate obj (1+ n))))) 
          (angle pcen pt) 
           ) 
        ) 
        angm1 
        (* 0.25 
           (- (angle pcen pt) 
          (angle pcen (vlax-safearray->list (vlax-variant-value(vla-get-Coordinate obj n)))) 
           ) 
        ) 
      ) 
      (vla-AddVertex obj (1+ n) (ax:2DPoint pt)) 
      (vla-SetBulge obj n (/ (sin angm1) (cos angm1))) 
      (vla-SetBulge obj (1+ n) (/ (sin ang2m) (cos ang2m))) 
    ) 
  ) 
  ) 
  (progn 
    (setq plist    (vlax-safearray->list 
          (vlax-variant-value 
            (vla-get-coordinates obj) 
          ) 
        ) 
    ) 
    (vla-AddVertex obj (/ (length plist) 2) (ax:2DPoint pt)) 
    (vla-SetBulge obj (1- (/ (length plist) 2)) bulge) 
    ) 
  ) 
  (vla-update obj) 
  (princ) 
) 

(defun ax:2DPoint (pt) 
  (vlax-make-variant 
    (vlax-safearray-fill 
      (vlax-make-safearray vlax-vbdouble '(0 . 1)) 
      (list (car pt) (cadr pt)) 
    ) 
  ) 
)
