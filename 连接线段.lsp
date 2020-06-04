;连接线段或圆弧 
(defun c:JOIN (/ OBJ1 OBJ2 wmend2) 
  (defun wmend2    (e0 e1 / mm mma    2pi p1 p2 p3 p4    q1 q2 qa1 qa2 r    a1 a2 d 
         d1 tfla tfs) 
    (if    (not (equal (car e0) (car e1))) 
      (progn 
    (setq mm  0.1 
          mma -0.1 
          2pi (+ pi pi) 
    ) 
    (xdrx_setenttodb (car e0)) 
    (if (setq p1  (xdrx_getentdxf 10) 
          tfla (= "LINE" (xdrx_getentdxf 0)) 
        ) 
      (setq p2 (xdrx_getentdxf 11)) 
      (setq    r  (xdrx_getentdxf 40) 
        a1 (xdrx_getentdxf 50) 
        a2 (xdrx_getentdxf 51) 
      ) 
    ) 
    (xdrx_setenttodb (car e1)) 
    (cond 
      ((and    tfla 
        (= "LINE" (xdrx_getentdxf 0)) 
        (< (abs    (xdrx_p2ldist 
              (setq    q1 (xdrx_getentdxf 10) 
                q2 (xdrx_getentdxf 11) 
              ) 
              p1 
              p2 
            ) 
          ) 
          mm 
        ) 
        (< (abs (xdrx_p2ldist q1 p1 p2)) mm) 
      ) 
      (setq d 0) 
      (mapcar '(lambda (pt1 pt2) 
              (if (> (setq d1 (distance pt1 pt2)) d) 
            (setq d     d1 
                  p3 pt1 
                  p4 pt2 
            ) 
              ) 
            ) 
          (list p1 p1 p1 p2 p2 q1) 
          (list q1 q2 p2 q1 q2 q2) 
      ) 
      (entdel (car e0)) 
      (xdrx_modent 10 p3 11 p4) 
      ) 
      ((and    (not tfla) 
        (= "ARC" (xdrx_getentdxf 0)) 
        (equal (xdrx_getentdxf 10) p1 mm) 
        (equal (xdrx_getentdxf 40) r mm) 
      ) 
      (setq a  (angle p1 (last e1)) 
         qa1 (- a (xdrx_getentdxf 50)) 
         qa2 (- (xdrx_getentdxf 51) a) 
         tfs (>    (if (> qa1 mma) 
              qa1 
              (+ 2pi qa1) 
            ) 
            (if (> qa2 mma) 
              qa2 
              (+ 2pi qa2) 
            ) 
            ) 
      ) 
      (entdel (car e0)) 
      (if tfs 
        (xdrx_modent 51 a2) 
        (xdrx_modent 50 a1) 
      ) 
      ) 
      ((command ".fillet" "r" 0 ".fillet" e0 e1)) 
    ) 
      ) 
    ) 
  ) 
  (if (and (setq OBJ1 (xdrx_entsel 
            "\n请拾取第一根线(LINE)或弧(ARC) <退出>:" 
            '((0 . "line,arc")) 
              ) 
      ) 
      (setq OBJ2 
          (xdrx_entsel 
            "\n再拾取第二根线(LINE)或弧(ARC)进行连接 <退出>: " 
            '((0 . "line,arc")) 
          ) 
      ) 
      ) 
    (progn 
      (xdrx_begin) 
      (wmend2 OBJ1 OBJ2) 
      (xdrx_end) 
    ) 
  ) 
  (princ) 
) 
