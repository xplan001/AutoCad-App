(defun C:ets(/ s1 ename ti% p11 p10 f en p11n p10n fn fn1)
    (princ "ET-Supper Made by LeaLeung.\n")	;����ET��ʽ�����ߣ�����!
    (princ "Select Text to change:\n")
    (setq 
        s1
        (ssget '((0 . "TEXT")))		;�������TEXT��ѡ��
    )

    (setq ti% 0)
    (if (/= s1 nil)
     (progn
       (while 
        (<= 
                ti% 
                (- (sslength s1) 1)
        )
        (setq ename(ssname s1 ti%))
        (setq p11 nil)			;��p11�ÿ�
        (setq e (entget ename))		;ȡʵ���e
        (setq p11 (assoc  11 e))	;ȡ�����
             (setq p10 (assoc 10 e))	;ȡ��ʼ���
             (setq f (assoc 10 e))	;����
             (setq en e)
             (if (= (cdr f) 0) (setq p11n (cons 11 (cdr p10))))

             (if (/= (cdr f ) 0)
               (progn
                 (setq p10n (cons 10 (cdr p11)));�������λ������ʼ�������p10n
                 (setq en (subst p10n p10 en))	;���µ���ʼ��λ�����ɵ���ʼ ���
               )
              )
              (setq fn 0)
              (setq fn (cons 72 fn))		;(72 . 0) Ϊ����
              (setq fn1 (assoc 72 en))		;ȡ�ɵĶ��뷽ʽ
              (setq e (subst fn fn1 e))		;�����ɵĶ��뷽ʽΪ����
              (entmod e)
              (setq ti%(+ 1 ti%))
         )  ;end while
      ) ;endprogn s1/=nil
    )  ;endif  s1 /= nil
    (setq s1 nil)
    (princ "Made by LeaLeung")	;LeaLeung ������ ����!
)
(princ "ִ��ets����ɽ���ѡ�����ֵĶ��뷽ʽ��Ϊ�����")
(princ )