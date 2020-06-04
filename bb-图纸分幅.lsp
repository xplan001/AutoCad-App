;;一个做块程序，程序可以将一张大图分成若干张小图：
;;要求：
;;1。必须在e:建立一个plot目录，且plot目录为空
;;2。第一个选择目标必须选一个图号（字符大于3），以后的图号程序可自动处理。
(defun c:bb()
  (command "osnap" "off")
 (princ "\n选择图号")
 (setq s (ssget))
  (setq th 0)
  (setq xxx 0)
      (setq name (ssname s 0))
      (setq a (entget name))
      (setq t1 (assoc '0 a))
      (setq t1 (cdr t1))
      (if (= t1 "TEXT") (PROGN
          (setq tx (assoc '1 a))
          (setq tx (cdr tx))
          (setq llen (strlen tx))
          (setq llen (- llen 2))
          (setq tx1 (substr tx 1 llen))
          ))
  (while (/= xxx nil)
    (if (< th 10)(setq thh (strcat "0" (rtos th 2 0))))
    (if (>= th 10)(setq thh (rtos th 2 0)))
    (setq name1 (strcat tx1 thh))
    (setq th (+ th 1))
    (setq p1 (getpoint"\n 输入第一点:"))
    (setq xxx p1)
    (if (/= p1 nil)(progn
        (setq p2 (getpoint"\n 输入第二点:"))
        (setq mmm (strcat "e:/" "plot" "/" name1))
        (command "-wblock" mmm "" p1 "w" p1 p2 "")
        (command "oops")
        )
  )
 )
 )

