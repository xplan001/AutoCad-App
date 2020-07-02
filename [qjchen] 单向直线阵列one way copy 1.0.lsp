;;; ========================================================================
;;; the following code are writen by qjchen                                ;
;;; Purpose: To dynamic copy Object in one way                             ;
;;; Thanks to: lushui2 (The original idea Author)                          ;
;;;            Andera (He post a very cool Dynamic Array rountine)         ;
;;;                  at http://www.theswamp.org/index.php?topic=26633.5    ;
;;; Version v 1.0 2011.03.15                                               ;
;;; Http://chenqj.blogspot.com                                             ;
;;; ========================================================================

;;; =======================================================================;
;;; The main function                                                      ;
;;; =======================================================================;
(vl-load-com)
(defun c:CC( / dir gr nx p0 px pxv ss ss1 vecx)
  (setq	ss (std-sslist (ssget))
        p0 (getpoint "\nP0:") px (getpoint p0 "\nPx:")
        vecx (mapcar '- px p0)
  )
  (prompt "\nThe end point:")
  (while (= (car (setq gr (grread nil 5 0))) 5)
    (if	ss1 (q:ss:del ss1))
    (redraw)
    (setq pxv (mapcar '- (inters (cadr gr) (polar (cadr gr) (+ (/ pi 2.0) (angle px p0)) 1.0) p0 px nil) p0))
    (if (< (setq nx  (fix (/ (caddr (trans pxv 0 vecx)) (caddr (trans vecx 0 vecx))))) 0)
           (setq dir -1 nx (- nx)) (setq dir 1))
    (setq ss1 (q:ss:dyngen ss nx vecx dir))
    (grdraw p0 (mapcar '+ p0 pxv) 3 1)
  )
  (princ)
)

;;; =======================================================================;
;;; by qjchen, copy ss according to the direction and vector               ;
;;; =======================================================================;
(defun q:ss:dyngen (sslst n v dir / i matlist obj1 ss transmat xobj)
  (setq ss (ssadd))
  (foreach x sslst
    (setq xobj (vlax-ename->vla-object x) i 1)
    (repeat n
      (setq obj1 (vla-copy xobj)
            matList (list (list 1 0 0 (* i (car v) dir)) (list 0 1 0 (* i (cadr v) dir)) '(0 0 1 0) '(0 0 0 1)) 
            transmat (vlax-tmatrix matlist))
      (vla-transformby obj1 transMat)
      (ssadd (vlax-vla-object->ename obj1) ss)
      (setq i (1+ i))
    )
  )
  ss
)

;;; =======================================================================;
;;; by qjchen, entdel ss                                                   ;
;;; =======================================================================;
;; (setq a (ssget))                                                        ;
;; (q:ss:del a)                                                            ;
;;; =======================================================================;
(defun q:ss:del	(ss / i)
  (setq i 0)
  (repeat (sslength ss)
    (entdel (ssname ss i))
    (setq i (1+ i))
  )
)
;;; =======================================================================;
;;; by qjchen, 2 ss add                                                    ;
;;; =======================================================================;
(defun q:ss:add	(ss1 ss2 / i)
  (setq i -1)
  (repeat (sslength ss2)
    (setq i (1+ i))
    (setq ss1 (ssadd (ssname ss2 i) ss1))
  )
  ss1
)
;;; =======================================================================;
;;; selection to list, by Reini Urban                                      ;
;;; =======================================================================;
(defun std-sslist  (ss / n lst)
  (if	(eq 'pickset (type ss))
    (repeat (setq n (fix (sslength ss))) ; fixed
      (setq lst (cons (ssname ss (setq n (1- n))) lst))))
)


(princ "by qjchen@gmail.com, To dynamic Array object, the command is Test")



