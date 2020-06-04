;;;sap.lsp v0.6  by nixnauy@gmail.com 20130715
;;;读取s2k文件，在autocad中绘制节点和杆件，并标上各自编号

(defun xx(s1)  ;;;用空格把一行字符串s1分成多个字符串，并由它们组成b1,各字符串中的“=”替换成空格。
   (setq n (strlen s1))
   (setq s2 "" b1 '() i 1)
   (while (<= i n)
     (setq a (substr s1 i 1))
     (cond ((= a " ")  (if (/= s2 "")(setq b1 (cons s2 b1) s2 "")));;;如果a是空格且s2存在则入表b1
           (T (setq s2 (strcat s2 (if (= a "=")(setq a " ")(setq b a)))))   
     )
     (setq i (+ i 1))
   )
   (setq b1 (cons s2 b1))
);;; end defun xx

(defun zz (s1) ;;;用xx函数将s1变成字符串表。然后将表中的每个字符串再变成表。
  (setq b2 '())
  (foreach a (xx s1)(setq b2 (cons (reverse (xx a)) b2)))
)


(defun C:sap ()
 (setq joints-list '() frames-list '())
 (setq f1 (open (getfiled "file" "" "s2k" 8) "r"));;;打开待处理的SAP文件。
   (while   (setq r1 (read-line f1))  ;;;到文件末尾时退出;
      
    (if (wcmatch r1 "*JOINT COORDINATES*")   ;;;如果是节点数据
        (while (> (strlen (setq r2 (read-line f1))) 2) ;;;是空行则结束;
            (setq r3 (zz r2) 
                  id (atoi (cadr (assoc "Joint" r3)))
                  x1 (atof (cadr (assoc "GlobalX" r3)))
                  y1 (atof (cadr (assoc "GlobalY" r3)))
                  z1 (atof (cadr (assoc "GlobalZ" r3)))
                  p1 (list x1 y1 z1)
             )
            (setq joints-list (cons (cons id p1) joints-list))     ;;;生成节点总表
               (command "color" "red")
               (command "point" p1)
               (command "text" p1 "10" "0" id) ;;;写节点号
         );;; end while 
     );;; end if
    (setq joints-list (reverse joints-list))
          
    (if (wcmatch r1 "*CONNECTIVITY - FRAME*")   ;;;如果是杆件数据
         (while (> (strlen (setq r2 (read-line f1))) 2) ;;;如果不是空行;
             (setq r3 (zz r2)
                   fid (atoi (cadr (assoc "Frame" r3)))
                   iid (atoi (cadr (assoc "JointI" r3)))
                   jid (atoi (cadr (assoc "JointJ" r3)))
                   i1 (cdr (assoc iid joints-list))  ;;;杆件始端坐标
                   j1 (cdr (assoc jid joints-list))  ;;;杆件末端坐标
                   x1 (atof (cadr (assoc "CentroidX" r3)))
                   y1 (atof (cadr (assoc "CentroidY" r3)))
                   z1 (atof (cadr (assoc "CentroidZ" r3)))
                   p2 (list x1 y1 z1) ;;;杆件中心点坐标
              )
            (setq frames-list (cons (list fid iid jid) frames-list))     ;;;生成杆件总表
                (command "color" "green")
                (command "line" i1 j1 "")
                (command "text" p2 "10" (* (/ 180 3.14)(angle i1 j1)) fid) ;;;写杆件号
          );;; end while 
     );;;end if
    (setq frames-list (reverse frames-list))
   
   );;; end while
  (close f1)
  (command "zoom" "e")
) ;;;  end defun C:sap