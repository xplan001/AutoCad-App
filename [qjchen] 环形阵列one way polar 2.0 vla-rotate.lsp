;;; ========================================================================
;;; the following code are writen by CHEN QING JUN                         ;
;;; Civil engineering Department, South China University of Technology     ;
;;; Purpose: To dynamic copy Object in polar                               ;
;;; Version v 1.0 2011.03.16                                               ;
;;; Http://chenqj.blogspot.com                                             ;
;;; ========================================================================

;;; =======================================================================;
;;; The main function                                                      ;
;;; =======================================================================;
(vl-load-com)
(defun c:HXFZ( / ang angnow gr oang p0 px px1 ss ss1)
  (setq	ss (std-sslist (ssget))
        p0 (getpoint "\nP0,the center :") px (getpoint p0 "\nThe angle start Point:")
        px1 (getpoint p0 "\nThe angle end Point:")
        ang (- (angle p0 px1)(setq oang  (angle p0 px)))
  )
  (prompt "\nThe rotation point:")
  (while (= (car (setq gr (grread nil 5 0))) 5)
    (if	ss1 (mapcar 'vla-delete ss1))
    (redraw)
    (setq angnow (- (angle p0 (cadr gr)) oang))
    (if (and (< ang 0)(> angnow 0)) (setq angnow (- angnow (* 2 pi))))
    (if (and (> ang 0)(< angnow 0)) (setq angnow (+ (* 2 pi) angnow)))
    (setq ss1 (q:ss:dyngenpolar ss (fix (/ angnow ang)) p0 ang))
    (q:grdraw:arc p0 (/ (getvar "viewsize") 4.0) oang angnow)
  )
  (princ)
)

;;; =======================================================================;
;;; by qjchen, grdraw circle arc                                           ;
;;; =======================================================================;
(defun q:grdraw:arc(cen r ang angadd / angdiv n)
 (grdraw cen (polar cen ang r) 3 1)
 (grdraw cen (polar cen (+ ang angadd) r) 3 1)
 (setq n 100 angdiv (/ angadd n))
 (repeat n
   (grdraw (polar cen ang r)(polar cen (setq ang (+ ang angdiv)) r) 1 1)
 )
)


;;; =======================================================================;
;;; by qjchen, copy ss according to the direction and vector               ;
;;; =======================================================================;
(defun q:ss:dyngenpolar (sslst n cen ang / i obj1 ss xobj)
  (setq ss nil)
  (foreach x sslst
    (setq xobj (vlax-ename->vla-object x) i 1)
    (repeat n
      (setq ss (cons (setq obj1 (vla-copy xobj)) ss))
      (Vla-rotate obj1 (vlax-3d-point cen) (* ang i))  
      (setq i (1+ i))
    )
  )
  ss
)

;;; =======================================================================;
;;; selection to list, by Reini Urban                                      ;
;;; =======================================================================;
(defun std-sslist  (ss / n lst)
  (if (eq 'pickset (type ss))
    (repeat (setq n (fix (sslength ss))) ; fixed
      (setq lst (cons (ssname ss (setq n (1- n))) lst))))
)

(princ "by qjchen@gmail.com, To dynamic rotate copy object, the command is Test")



