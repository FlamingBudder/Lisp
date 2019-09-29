(defparameter *chinese*
  '(
    (paragraph (sentence "." sentence "." sentence*))
    (sentence action description)
    (sentence* () (sentence "." sentence*) (sentence "." sentence*) (sentence "." sentence*) (sentence "." sentence*) (sentence "." sentence*))
    (action (noun* verb noun*))
    (description (noun* degree* adj))
    (noun* () noun noun noun noun)
    (degree* () degree degree)

    
    (noun ren2 wo3 ta1 ni3 shi4 jia1 fang2 jia1 (peng2 you3) (bei1 zi3) (shou3 ji1) shu1 zhi3 hua1 (yin1 yue4) bi3 (lou2 ti1) (yi3 zi1) (zhao4 pian1) zhong1)
    (degree (jue2 bu4) bu4 ye3 hen3 (fei1 chang2) jue2 (te4 bie2) chao1)
    (adj da4 hui4 hao3 gao1 lao3 hong2 lan2 lv4 huang2 bai2 hei1 ai3 (hao3 chi1) jv2 ying4 xi4 mian2 shuai4 mei3 (cong1 ming2) ben4)
    (verb shi4 you3 lai2 dao4 shuo1 chu1 yao4 hui4 na2 neng2 na2 la1 shua1 dan2 du2 xie3 kai1 guan1 zou3 pao3 tiao4 wen2 ting1 chang2 xi1 chui1 da3 fa1)
   )
  )

(defvar grammar *chinese*)

(setf grammar *chinese*)

(defun mappend (f list)
  (apply #'append (mapcar f list)))

(defun evaluate (part)
  (rest (assoc part grammar)))

(defun random-elt (list)
  (elt list (random (length list))))

(defun generate (part)
  (if (listp part)
      (mappend #'generate part)
      (let ((choices (evaluate part)))
        (if (null choices)
            (list part)
            (generate (random-elt choices))))))
