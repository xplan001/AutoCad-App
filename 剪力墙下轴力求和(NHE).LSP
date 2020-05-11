;--------------------------------------+
;文件名：NHE.lsp(命令NHE）           +
;剪力墙下轴力求和（JCCAD生成的基础数据）
;copyright by pwf 2011.8.16
;宁波大学建筑设计院
;--------------------------------------+
(defun clmerr(msg)
       (setq *error* m:err m:err nil)
       (princ)
)
;----------------------------------
;系统变量初始化
(defun inivar()
   (setq oer *error* *error* clmerr)
   (setq cmdold (getvar "CMDECHO")
         osmold (getvar "OSMODE")
         bliold (getvar "BLIPMODE")
   ) ;setq end
   ;(setq oldlay (getvar "clayer"))  ;获得当前层的层名
   (setvar "CMDECHO" 0)
   (setvar "OSMODE" 0)
   (setvar "BLIPMODE" 0)
   (princ)
) ;defun end

;系统变量还原
(defun resvar()
   (setvar "CMDECHO"  cmdold)
   (setvar "OSMODE"   osmold)
   (setvar "BLIPMODE" bliold)
   ;(command "LAYER" "SET" oldlay "")
   (setq *error* oer)
   (princ)
) ;defun end
;----------------------------------

(defun c:Nhe( / sset sum first str1 c i str)
(inivar) ;初始化系统变量

  (princ "选择墙下需求和的轴力：")

  (setq	sset (ssget
	       '((0 . "TEXT"))
	     )
  )
  (setq	sum 0
	i 0
        c 0
  )
  (setq first (entget (ssname sset 0)))

 (repeat (sslength sset)

   (progn

         (setq str1 (cdr (assoc 1 (entget (ssname sset i)))))

          (if (= (substr str1 1 1) "N");判断是否"N=123,M=456"格式
            (progn
              (setq sum (+ sum (abs (atoi (NZH1 STR1)))))
              (setq c (+ c 1))
            );end progn
           );END IF  
         
        (setq i (+ 1 i))
   )  
   ) 
 (setq str (strcat "\n墙下轴力求和共有" (itoa c) "个,轴力总值为" (itoa sum)))  
  (princ str)
  (princ)

(resvar) ;还原系统变量 
)
;---------------------------------------------

;-----子程序--------------------------
;读取字符STR1= "N=123,M=456"中的123数值
(defun nzh1 (str1 / a r)
  (setq r 3)
  (while (/= a ",")
   (setq a (substr str1 r 1))
   (setq r (+ 1 r))
   );end while
  (substr str1 3 (- r 2))
);end defun