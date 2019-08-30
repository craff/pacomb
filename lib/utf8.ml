type context =
  | ASCII
  | UTF8
  | CJK_UTF8
let width ?(context= UTF8)  c =
  let n = Uchar.to_int c in
  if context = CJK_UTF8
  then
    (if n < 9450
     then
       (if n < 70197
        then
          (if n < 119155
           then
             (if n < 127989
              then
                (if n < 129399
                 then
                   (if n < 173824
                    then
                      (if n < 195102
                       then
                         (if n < 917760
                          then
                            (if n < 983040
                             then
                               (if n < 1048574
                                then (if n < 1048576 then 1 else 2)
                                else 1)
                             else if n < 918000 then 2 else 1)
                          else
                            if n < 917506
                            then
                              (if n < 917536
                               then (if n < 917632 then 0 else 1)
                               else 0)
                            else if n < 917505 then 1 else 0)
                       else
                         if n < 178208
                         then
                           (if n < 183984
                            then
                              (if n < 191457
                               then (if n < 194560 then 1 else 2)
                               else 1)
                            else if n < 183970 then 2 else 1)
                         else
                           if n < 177984
                           then (if n < 178206 then 2 else 1)
                           else if n < 177973 then 2 else 1)
                    else
                      if n < 129648
                      then
                        (if n < 129667
                         then
                           (if n < 129686
                            then
                              (if n < 131072
                               then (if n < 173783 then 2 else 1)
                               else 2)
                            else if n < 129680 then 1 else 2)
                         else
                           if n < 129656
                           then
                             (if n < 129659
                              then (if n < 129664 then 1 else 2)
                              else 1)
                           else if n < 129652 then 2 else 1)
                      else
                        if n < 129451
                        then
                          (if n < 129483
                           then
                             (if n < 129485
                              then (if n < 129536 then 2 else 1)
                              else 2)
                           else if n < 129454 then 1 else 2)
                        else
                          if n < 129443
                          then (if n < 129445 then 1 else 2)
                          else if n < 129402 then 1 else 2)
                 else
                   if n < 128507
                   then
                     (if n < 128726
                      then
                        (if n < 128992
                         then
                           (if n < 129293
                            then
                              (if n < 129394
                               then (if n < 129395 then 1 else 2)
                               else 1)
                            else if n < 129004 then 2 else 1)
                         else
                           if n < 128749
                           then
                             (if n < 128756
                              then (if n < 128763 then 2 else 1)
                              else 2)
                           else if n < 128747 then 1 else 2)
                      else
                        if n < 128716
                        then
                          (if n < 128720
                           then
                             (if n < 128723
                              then (if n < 128725 then 1 else 2)
                              else 1)
                           else if n < 128717 then 2 else 1)
                        else
                          if n < 128640
                          then (if n < 128710 then 2 else 1)
                          else if n < 128592 then 2 else 1)
                   else
                     if n < 128331
                     then
                       (if n < 128379
                        then
                          (if n < 128407
                           then
                             (if n < 128420
                              then (if n < 128421 then 2 else 1)
                              else 2)
                           else if n < 128405 then 1 else 2)
                        else
                          if n < 128336
                          then
                            (if n < 128360
                             then (if n < 128378 then 1 else 2)
                             else 1)
                          else if n < 128335 then 2 else 1)
                     else
                       if n < 128065
                       then
                         (if n < 128253
                          then
                            (if n < 128255
                             then (if n < 128318 then 2 else 1)
                             else 2)
                          else if n < 128066 then 1 else 2)
                       else
                         if n < 128063
                         then (if n < 128064 then 1 else 2)
                         else if n < 127992 then 1 else 2)
              else
                if n < 125259
                then
                  (if n < 127568
                   then
                     (if n < 127869
                      then
                        (if n < 127951
                         then
                           (if n < 127968
                            then
                              (if n < 127985
                               then (if n < 127988 then 1 else 2)
                               else 1)
                            else if n < 127956 then 2 else 1)
                         else
                           if n < 127892
                           then
                             (if n < 127904
                              then (if n < 127947 then 2 else 1)
                              else 2)
                           else if n < 127870 then 1 else 2)
                      else
                        if n < 127744
                        then
                          (if n < 127789
                           then
                             (if n < 127798
                              then (if n < 127799 then 1 else 2)
                              else 1)
                           else if n < 127777 then 2 else 1)
                        else
                          if n < 127584
                          then (if n < 127590 then 2 else 1)
                          else if n < 127570 then 2 else 1)
                   else
                     if n < 127280
                     then
                       (if n < 127491
                        then
                          (if n < 127548
                           then
                             (if n < 127552
                              then (if n < 127561 then 2 else 1)
                              else 2)
                           else if n < 127504 then 1 else 2)
                        else
                          if n < 127344
                          then
                            (if n < 127405
                             then (if n < 127488 then 1 else 2)
                             else 1)
                          else if n < 127338 then 2 else 1)
                     else
                       if n < 127184
                       then
                         (if n < 127243
                          then
                            (if n < 127248
                             then (if n < 127278 then 2 else 1)
                             else 2)
                          else if n < 127232 then 1 else 2)
                       else
                         if n < 126981
                         then (if n < 127183 then 1 else 2)
                         else if n < 126980 then 1 else 2)
                else
                  if n < 121505
                  then
                    (if n < 122917
                     then
                       (if n < 123628
                        then
                          (if n < 125136
                           then
                             (if n < 125143
                              then (if n < 125252 then 1 else 0)
                              else 1)
                           else if n < 123632 then 0 else 1)
                        else
                          if n < 122923
                          then
                            (if n < 123184
                             then (if n < 123191 then 0 else 1)
                             else 0)
                          else if n < 122918 then 1 else 0)
                     else
                       if n < 122888
                       then
                         (if n < 122907
                          then
                            (if n < 122914
                             then (if n < 122915 then 1 else 0)
                             else 1)
                          else if n < 122905 then 0 else 1)
                       else
                         if n < 122880
                         then (if n < 122887 then 0 else 1)
                         else if n < 121520 then 0 else 1)
                  else
                    if n < 121399
                    then
                      (if n < 121462
                       then
                         (if n < 121477
                          then
                            (if n < 121499
                             then (if n < 121504 then 0 else 1)
                             else 0)
                          else if n < 121476 then 1 else 0)
                       else
                         if n < 121453
                         then (if n < 121461 then 1 else 0)
                         else if n < 121403 then 1 else 0)
                    else
                      if n < 119210
                      then
                        (if n < 119362
                         then
                           (if n < 119365
                            then (if n < 121344 then 1 else 0)
                            else 1)
                         else if n < 119214 then 0 else 1)
                      else
                        if n < 119173
                        then (if n < 119180 then 0 else 1)
                        else if n < 119171 then 0 else 1)
           else
             if n < 72243
             then
               (if n < 73104
                then
                  (if n < 94180
                   then
                     (if n < 110948
                      then
                        (if n < 113823
                         then
                           (if n < 113828
                            then
                              (if n < 119143
                               then (if n < 119146 then 0 else 1)
                               else 0)
                            else if n < 113824 then 1 else 0)
                         else
                           if n < 110960
                           then
                             (if n < 111356
                              then (if n < 113821 then 1 else 0)
                              else 1)
                           else if n < 110952 then 2 else 1)
                      else
                        if n < 101107
                        then
                          (if n < 110879
                           then
                             (if n < 110928
                              then (if n < 110931 then 2 else 1)
                              else 2)
                           else if n < 110592 then 1 else 2)
                        else
                          if n < 100344
                          then (if n < 100352 then 1 else 2)
                          else if n < 94208 then 1 else 2)
                   else
                     if n < 78905
                     then
                       (if n < 94031
                        then
                          (if n < 94095
                           then
                             (if n < 94099
                              then (if n < 94176 then 1 else 2)
                              else 1)
                           else if n < 94032 then 0 else 1)
                        else
                          if n < 92917
                          then
                            (if n < 92976
                             then (if n < 92983 then 0 else 1)
                             else 0)
                          else if n < 92912 then 1 else 0)
                     else
                       if n < 73111
                       then
                         (if n < 73459
                          then
                            (if n < 73461
                             then (if n < 78896 then 1 else 0)
                             else 1)
                          else if n < 73112 then 0 else 1)
                       else
                         if n < 73109
                         then (if n < 73110 then 0 else 1)
                         else if n < 73106 then 0 else 1)
                else
                  if n < 72768
                  then
                    (if n < 73009
                     then
                       (if n < 73022
                        then
                          (if n < 73030
                           then
                             (if n < 73031
                              then (if n < 73032 then 0 else 1)
                              else 0)
                           else if n < 73023 then 1 else 0)
                        else
                          if n < 73018
                          then
                            (if n < 73019
                             then (if n < 73020 then 1 else 0)
                             else 1)
                          else if n < 73015 then 0 else 1)
                     else
                       if n < 72881
                       then
                         (if n < 72884
                          then
                            (if n < 72885
                             then (if n < 72887 then 0 else 1)
                             else 0)
                          else if n < 72882 then 1 else 0)
                       else
                         if n < 72872
                         then (if n < 72874 then 1 else 0)
                         else if n < 72850 then 1 else 0)
                  else
                    if n < 72284
                    then
                      (if n < 72752
                       then
                         (if n < 72760
                          then
                            (if n < 72766
                             then (if n < 72767 then 1 else 0)
                             else 1)
                          else if n < 72759 then 0 else 1)
                       else
                         if n < 72343
                         then
                           (if n < 72344
                            then (if n < 72346 then 0 else 1)
                            else 0)
                         else if n < 72330 then 1 else 0)
                    else
                      if n < 72263
                      then
                        (if n < 72273
                         then
                           (if n < 72279
                            then (if n < 72281 then 1 else 0)
                            else 1)
                         else if n < 72264 then 0 else 1)
                      else
                        if n < 72251
                        then (if n < 72255 then 0 else 1)
                        else if n < 72249 then 0 else 1)
             else
               if n < 71100
               then
                 (if n < 71352
                  then
                    (if n < 71737
                     then
                       (if n < 72156
                        then
                          (if n < 72161
                           then
                             (if n < 72193
                              then (if n < 72203 then 0 else 1)
                              else 0)
                           else if n < 72160 then 1 else 0)
                        else
                          if n < 72148
                          then
                            (if n < 72152
                             then (if n < 72154 then 1 else 0)
                             else 1)
                          else if n < 71739 then 0 else 1)
                     else
                       if n < 71462
                       then
                         (if n < 71468
                          then
                            (if n < 71727
                             then (if n < 71736 then 0 else 1)
                             else 0)
                          else if n < 71463 then 1 else 0)
                       else
                         if n < 71456
                         then (if n < 71458 then 1 else 0)
                         else if n < 71453 then 1 else 0)
                  else
                    if n < 71230
                    then
                      (if n < 71341
                       then
                         (if n < 71344
                          then
                            (if n < 71350
                             then (if n < 71351 then 1 else 0)
                             else 1)
                          else if n < 71342 then 0 else 1)
                       else
                         if n < 71233
                         then
                           (if n < 71339
                            then (if n < 71340 then 0 else 1)
                            else 0)
                         else if n < 71231 then 1 else 0)
                    else
                      if n < 71132
                      then
                        (if n < 71219
                         then
                           (if n < 71227
                            then (if n < 71229 then 1 else 0)
                            else 1)
                         else if n < 71134 then 0 else 1)
                      else
                        if n < 71103
                        then (if n < 71105 then 0 else 1)
                        else if n < 71102 then 0 else 1)
               else
                 if n < 70517
                 then
                   (if n < 70835
                    then
                      (if n < 70849
                       then
                         (if n < 70852
                          then
                            (if n < 71090
                             then (if n < 71094 then 0 else 1)
                             else 0)
                          else if n < 70850 then 1 else 0)
                       else
                         if n < 70842
                         then
                           (if n < 70843
                            then (if n < 70847 then 1 else 0)
                            else 1)
                         else if n < 70841 then 0 else 1)
                    else
                      if n < 70725
                      then
                        (if n < 70727
                         then
                           (if n < 70750
                            then (if n < 70751 then 0 else 1)
                            else 0)
                         else if n < 70726 then 1 else 0)
                      else
                        if n < 70720
                        then (if n < 70722 then 1 else 0)
                        else if n < 70712 then 1 else 0)
                 else
                   if n < 70400
                   then
                     (if n < 70464
                      then
                        (if n < 70502
                         then
                           (if n < 70509
                            then (if n < 70512 then 1 else 0)
                            else 1)
                         else if n < 70465 then 0 else 1)
                      else
                        if n < 70459
                        then (if n < 70461 then 0 else 1)
                        else if n < 70402 then 0 else 1)
                   else
                     if n < 70207
                     then
                       (if n < 70368
                        then
                          (if n < 70371
                           then (if n < 70379 then 0 else 1)
                           else 0)
                        else if n < 70367 then 1 else 0)
                     else
                       if n < 70200
                       then (if n < 70206 then 1 else 0)
                       else if n < 70198 then 1 else 0)
        else
          if n < 42607
          then
            (if n < 64218
             then
               (if n < 68160
                then
                  (if n < 69837
                   then
                     (if n < 70004
                      then
                        (if n < 70089
                         then
                           (if n < 70191
                            then
                              (if n < 70194
                               then (if n < 70196 then 1 else 0)
                               else 1)
                            else if n < 70093 then 0 else 1)
                         else
                           if n < 70018
                           then
                             (if n < 70070
                              then (if n < 70079 then 0 else 1)
                              else 0)
                           else if n < 70016 then 1 else 0)
                      else
                        if n < 69927
                        then
                          (if n < 69933
                           then
                             (if n < 69941
                              then (if n < 70003 then 1 else 0)
                              else 1)
                           else if n < 69932 then 0 else 1)
                        else
                          if n < 69888
                          then (if n < 69891 then 0 else 1)
                          else if n < 69838 then 0 else 1)
                   else
                     if n < 69688
                     then
                       (if n < 69815
                        then
                          (if n < 69819
                           then
                             (if n < 69821
                              then (if n < 69822 then 0 else 1)
                              else 0)
                           else if n < 69817 then 1 else 0)
                        else
                          if n < 69759
                          then
                            (if n < 69762
                             then (if n < 69811 then 1 else 0)
                             else 1)
                          else if n < 69703 then 0 else 1)
                     else
                       if n < 68904
                       then
                         (if n < 69457
                          then
                            (if n < 69633
                             then (if n < 69634 then 0 else 1)
                             else 0)
                          else if n < 69446 then 1 else 0)
                       else
                         if n < 68327
                         then (if n < 68900 then 1 else 0)
                         else if n < 68325 then 1 else 0)
                else
                  if n < 65529
                  then
                    (if n < 66427
                     then
                       (if n < 68108
                        then
                          (if n < 68152
                           then
                             (if n < 68155
                              then (if n < 68159 then 1 else 0)
                              else 1)
                           else if n < 68112 then 0 else 1)
                        else
                          if n < 68100
                          then
                            (if n < 68101
                             then (if n < 68103 then 0 else 1)
                             else 0)
                          else if n < 68097 then 1 else 0)
                     else
                       if n < 66045
                       then
                         (if n < 66272
                          then
                            (if n < 66273
                             then (if n < 66422 then 1 else 0)
                             else 1)
                          else if n < 66046 then 0 else 1)
                       else
                         if n < 65533
                         then (if n < 65534 then 0 else 1)
                         else if n < 65532 then 2 else 1)
                  else
                    if n < 65108
                    then
                      (if n < 65280
                       then
                         (if n < 65377
                          then
                            (if n < 65504
                             then (if n < 65511 then 0 else 1)
                             else 2)
                          else if n < 65281 then 1 else 2)
                       else
                         if n < 65128
                         then
                           (if n < 65132
                            then (if n < 65279 then 1 else 0)
                            else 1)
                         else if n < 65127 then 2 else 1)
                    else
                      if n < 65040
                      then
                        (if n < 65056
                         then
                           (if n < 65072
                            then (if n < 65107 then 2 else 1)
                            else 2)
                         else if n < 65050 then 0 else 1)
                      else
                        if n < 64287
                        then (if n < 65024 then 2 else 0)
                        else if n < 64286 then 1 else 0)
             else
               if n < 43494
               then
                 (if n < 43710
                  then
                    (if n < 44006
                     then
                       (if n < 44032
                        then
                          (if n < 57344
                           then
                             (if n < 64110
                              then (if n < 64112 then 1 else 2)
                              else 1)
                           else if n < 55204 then 2 else 1)
                        else
                          if n < 44009
                          then
                            (if n < 44013
                             then (if n < 44014 then 2 else 1)
                             else 0)
                          else if n < 44008 then 1 else 0)
                     else
                       if n < 43756
                       then
                         (if n < 43766
                          then
                            (if n < 43767
                             then (if n < 44005 then 1 else 0)
                             else 1)
                          else if n < 43758 then 0 else 1)
                       else
                         if n < 43713
                         then (if n < 43714 then 0 else 1)
                         else if n < 43712 then 0 else 1)
                  else
                    if n < 43596
                    then
                      (if n < 43697
                       then
                         (if n < 43701
                          then
                            (if n < 43703
                             then (if n < 43705 then 0 else 1)
                             else 0)
                          else if n < 43698 then 1 else 0)
                       else
                         if n < 43644
                         then
                           (if n < 43645
                            then (if n < 43696 then 1 else 0)
                            else 1)
                         else if n < 43597 then 0 else 1)
                    else
                      if n < 43571
                      then
                        (if n < 43575
                         then
                           (if n < 43587
                            then (if n < 43588 then 0 else 1)
                            else 0)
                         else if n < 43573 then 1 else 0)
                      else
                        if n < 43567
                        then (if n < 43569 then 1 else 0)
                        else if n < 43561 then 1 else 0)
               else
                 if n < 43232
                 then
                   (if n < 43389
                    then
                      (if n < 43446
                       then
                         (if n < 43452
                          then
                            (if n < 43454
                             then (if n < 43493 then 1 else 0)
                             else 1)
                          else if n < 43450 then 0 else 1)
                       else
                         if n < 43395
                         then
                           (if n < 43443
                            then (if n < 43444 then 0 else 1)
                            else 0)
                         else if n < 43392 then 1 else 0)
                    else
                      if n < 43302
                      then
                        (if n < 43335
                         then
                           (if n < 43346
                            then (if n < 43360 then 1 else 2)
                            else 1)
                         else if n < 43310 then 0 else 1)
                      else
                        if n < 43263
                        then (if n < 43264 then 0 else 1)
                        else if n < 43250 then 0 else 1)
                 else
                   if n < 43011
                   then
                     (if n < 43020
                      then
                        (if n < 43047
                         then
                           (if n < 43204
                            then (if n < 43206 then 0 else 1)
                            else 0)
                         else if n < 43045 then 1 else 0)
                      else
                        if n < 43015
                        then (if n < 43019 then 1 else 0)
                        else if n < 43014 then 1 else 0)
                   else
                     if n < 42654
                     then
                       (if n < 42736
                        then
                          (if n < 42738
                           then (if n < 43010 then 1 else 0)
                           else 1)
                        else if n < 42656 then 0 else 1)
                     else
                       if n < 42612
                       then (if n < 42622 then 0 else 1)
                       else if n < 42611 then 0 else 1)
          else
            if n < 9955
            then
              (if n < 11648
               then
                 (if n < 12544
                  then
                    (if n < 12784
                     then
                       (if n < 40944
                        then
                          (if n < 42125
                           then
                             (if n < 42128
                              then (if n < 42183 then 0 else 1)
                              else 2)
                           else if n < 40960 then 1 else 2)
                        else
                          if n < 12832
                          then
                            (if n < 19894
                             then (if n < 19968 then 1 else 2)
                             else 1)
                          else if n < 12831 then 2 else 1)
                     else
                       if n < 12687
                       then
                         (if n < 12731
                          then
                            (if n < 12736
                             then (if n < 12772 then 2 else 1)
                             else 2)
                          else if n < 12688 then 1 else 2)
                       else
                         if n < 12592
                         then (if n < 12593 then 1 else 2)
                         else if n < 12549 then 1 else 2)
                  else
                    if n < 12272
                    then
                      (if n < 12351
                       then
                         (if n < 12439
                          then
                            (if n < 12441
                             then (if n < 12443 then 1 else 2)
                             else 0)
                          else if n < 12353 then 1 else 2)
                       else
                         if n < 12288
                         then
                           (if n < 12330
                            then (if n < 12334 then 1 else 2)
                            else 0)
                         else if n < 12284 then 2 else 1)
                    else
                      if n < 11930
                      then
                        (if n < 12020
                         then
                           (if n < 12032
                            then (if n < 12246 then 2 else 1)
                            else 2)
                         else if n < 11931 then 1 else 2)
                      else
                        if n < 11776
                        then (if n < 11904 then 1 else 2)
                        else if n < 11744 then 1 else 0)
               else
                 if n < 10071
                 then
                   (if n < 10176
                    then
                      (if n < 11093
                       then
                         (if n < 11503
                          then
                            (if n < 11506
                             then (if n < 11647 then 1 else 0)
                             else 1)
                          else if n < 11098 then 0 else 1)
                       else
                         if n < 11037
                         then
                           (if n < 11088
                            then (if n < 11089 then 2 else 1)
                            else 2)
                         else if n < 11035 then 1 else 2)
                    else
                      if n < 10133
                      then
                        (if n < 10160
                         then
                           (if n < 10161
                            then (if n < 10175 then 1 else 2)
                            else 1)
                         else if n < 10136 then 2 else 1)
                      else
                        if n < 10102
                        then (if n < 10112 then 2 else 1)
                        else if n < 10072 then 2 else 1)
                 else
                   if n < 10025
                   then
                     (if n < 10061
                      then
                        (if n < 10063
                         then
                           (if n < 10067
                            then (if n < 10070 then 2 else 1)
                            else 2)
                         else if n < 10062 then 1 else 2)
                      else
                        if n < 10046
                        then (if n < 10060 then 1 else 2)
                        else if n < 10045 then 1 else 2)
                   else
                     if n < 9989
                     then
                       (if n < 9994
                        then
                          (if n < 9996
                           then (if n < 10024 then 1 else 2)
                           else 1)
                        else if n < 9990 then 2 else 1)
                     else
                       if n < 9960
                       then (if n < 9984 then 2 else 1)
                       else if n < 9956 then 2 else 1)
            else
              if n < 9742
              then
                (if n < 9835
                 then
                   (if n < 9886
                    then
                      (if n < 9900
                       then
                         (if n < 9920
                          then
                            (if n < 9924
                             then (if n < 9954 then 2 else 1)
                             else 2)
                          else if n < 9917 then 1 else 2)
                       else
                         if n < 9889
                         then
                           (if n < 9890
                            then (if n < 9898 then 1 else 2)
                            else 1)
                         else if n < 9888 then 2 else 1)
                    else
                      if n < 9840
                      then
                        (if n < 9856
                         then
                           (if n < 9875
                            then (if n < 9876 then 2 else 1)
                            else 2)
                         else if n < 9855 then 1 else 2)
                      else
                        if n < 9838
                        then (if n < 9839 then 1 else 2)
                        else if n < 9836 then 1 else 2)
                 else
                   if n < 9793
                   then
                     (if n < 9824
                      then
                        (if n < 9827
                         then
                           (if n < 9830
                            then (if n < 9831 then 1 else 2)
                            else 1)
                         else if n < 9826 then 2 else 1)
                      else
                        if n < 9795
                        then
                          (if n < 9800
                           then (if n < 9812 then 2 else 1)
                           else 2)
                        else if n < 9794 then 1 else 2)
                   else
                     if n < 9756
                     then
                       (if n < 9758
                        then
                          (if n < 9759
                           then (if n < 9792 then 1 else 2)
                           else 1)
                        else if n < 9757 then 2 else 1)
                     else
                       if n < 9748
                       then (if n < 9750 then 2 else 1)
                       else if n < 9744 then 2 else 1)
              else
                if n < 9662
                then
                  (if n < 9698
                   then
                     (if n < 9727
                      then
                        (if n < 9735
                         then
                           (if n < 9737
                            then (if n < 9738 then 2 else 1)
                            else 2)
                         else if n < 9733 then 1 else 2)
                      else
                        if n < 9711
                        then
                          (if n < 9712
                           then (if n < 9725 then 1 else 2)
                           else 1)
                        else if n < 9702 then 2 else 1)
                   else
                     if n < 9673
                     then
                       (if n < 9676
                        then
                          (if n < 9678
                           then (if n < 9682 then 2 else 1)
                           else 2)
                        else if n < 9675 then 1 else 2)
                     else
                       if n < 9666
                       then (if n < 9670 then 1 else 2)
                       else if n < 9664 then 1 else 2)
                else
                  if n < 9632
                  then
                    (if n < 9650
                     then
                       (if n < 9654
                        then
                          (if n < 9656
                           then (if n < 9660 then 1 else 2)
                           else 1)
                        else if n < 9652 then 2 else 1)
                     else
                       if n < 9635
                       then (if n < 9642 then 2 else 1)
                       else if n < 9634 then 2 else 1)
                  else
                    if n < 9588
                    then
                      (if n < 9616
                       then
                         (if n < 9618 then (if n < 9622 then 2 else 1) else 2)
                       else if n < 9600 then 1 else 2)
                    else
                      if n < 9548
                      then (if n < 9552 then 1 else 2)
                      else if n < 9451 then 1 else 2)
     else
       if n < 3636
       then
         (if n < 7413
          then
            (if n < 8634
             then
               (if n < 8777
                then
                  (if n < 8857
                   then
                     (if n < 8988
                      then
                        (if n < 9200
                         then
                           (if n < 9203
                            then
                              (if n < 9204
                               then (if n < 9312 then 1 else 2)
                               else 1)
                            else if n < 9201 then 2 else 1)
                         else
                           if n < 9003
                           then
                             (if n < 9193
                              then (if n < 9197 then 2 else 1)
                              else 2)
                           else if n < 9001 then 1 else 2)
                      else
                        if n < 8895
                        then
                          (if n < 8978
                           then
                             (if n < 8979
                              then (if n < 8986 then 1 else 2)
                              else 1)
                           else if n < 8896 then 2 else 1)
                        else
                          if n < 8869
                          then (if n < 8870 then 2 else 1)
                          else if n < 8858 then 2 else 1)
                   else
                     if n < 8810
                     then
                       (if n < 8836
                        then
                          (if n < 8840
                           then
                             (if n < 8853
                              then (if n < 8854 then 2 else 1)
                              else 2)
                           else if n < 8838 then 1 else 2)
                        else
                          if n < 8814
                          then
                            (if n < 8816
                             then (if n < 8834 then 1 else 2)
                             else 1)
                          else if n < 8812 then 2 else 1)
                     else
                       if n < 8787
                       then
                         (if n < 8802
                          then
                            (if n < 8804
                             then (if n < 8808 then 2 else 1)
                             else 2)
                          else if n < 8800 then 1 else 2)
                       else
                         if n < 8781
                         then (if n < 8786 then 1 else 2)
                         else if n < 8780 then 1 else 2)
                else
                  if n < 8725
                  then
                    (if n < 8742
                     then
                       (if n < 8756
                        then
                          (if n < 8764
                           then
                             (if n < 8766
                              then (if n < 8776 then 1 else 2)
                              else 1)
                           else if n < 8760 then 2 else 1)
                        else
                          if n < 8749
                          then
                            (if n < 8750
                             then (if n < 8751 then 2 else 1)
                             else 2)
                          else if n < 8743 then 1 else 2)
                     else
                       if n < 8733
                       then
                         (if n < 8739
                          then
                            (if n < 8740
                             then (if n < 8741 then 1 else 2)
                             else 1)
                          else if n < 8737 then 2 else 1)
                       else
                         if n < 8730
                         then (if n < 8731 then 2 else 1)
                         else if n < 8726 then 2 else 1)
                  else
                    if n < 8706
                    then
                      (if n < 8716
                       then
                         (if n < 8720
                          then
                            (if n < 8721
                             then (if n < 8722 then 2 else 1)
                             else 2)
                          else if n < 8719 then 1 else 2)
                       else
                         if n < 8711
                         then
                           (if n < 8713
                            then (if n < 8715 then 1 else 2)
                            else 1)
                         else if n < 8708 then 2 else 1)
                    else
                      if n < 8661
                      then
                        (if n < 8680
                         then
                           (if n < 8704
                            then (if n < 8705 then 2 else 1)
                            else 2)
                         else if n < 8679 then 1 else 2)
                      else
                        if n < 8659
                        then (if n < 8660 then 1 else 2)
                        else if n < 8658 then 1 else 2)
             else
               if n < 8309
               then
                 (if n < 8481
                  then
                    (if n < 8543
                     then
                       (if n < 8585
                        then
                          (if n < 8592
                           then
                             (if n < 8602
                              then (if n < 8632 then 1 else 2)
                              else 1)
                           else if n < 8586 then 2 else 1)
                        else
                          if n < 8556
                          then
                            (if n < 8560
                             then (if n < 8570 then 2 else 1)
                             else 2)
                          else if n < 8544 then 1 else 2)
                     else
                       if n < 8491
                       then
                         (if n < 8531
                          then
                            (if n < 8533
                             then (if n < 8539 then 1 else 2)
                             else 1)
                          else if n < 8492 then 2 else 1)
                       else
                         if n < 8486
                         then (if n < 8487 then 2 else 1)
                         else if n < 8483 then 2 else 1)
                  else
                    if n < 8451
                    then
                      (if n < 8458
                       then
                         (if n < 8468
                          then
                            (if n < 8470
                             then (if n < 8471 then 2 else 1)
                             else 2)
                          else if n < 8467 then 1 else 2)
                       else
                         if n < 8453
                         then
                           (if n < 8454
                            then (if n < 8457 then 1 else 2)
                            else 1)
                         else if n < 8452 then 2 else 1)
                    else
                      if n < 8325
                      then
                        (if n < 8365
                         then
                           (if n < 8400
                            then (if n < 8433 then 2 else 1)
                            else 0)
                         else if n < 8364 then 1 else 2)
                      else
                        if n < 8320
                        then (if n < 8321 then 1 else 2)
                        else if n < 8319 then 1 else 2)
               else
                 if n < 8228
                 then
                   (if n < 8246
                    then
                      (if n < 8288
                       then
                         (if n < 8294
                          then
                            (if n < 8304
                             then (if n < 8308 then 1 else 2)
                             else 1)
                          else if n < 8293 then 0 else 1)
                       else
                         if n < 8252
                         then
                           (if n < 8254
                            then (if n < 8255 then 0 else 1)
                            else 2)
                         else if n < 8251 then 1 else 2)
                    else
                      if n < 8240
                      then
                        (if n < 8242
                         then
                           (if n < 8244
                            then (if n < 8245 then 1 else 2)
                            else 1)
                         else if n < 8241 then 2 else 1)
                      else
                        if n < 8234
                        then (if n < 8239 then 2 else 1)
                        else if n < 8232 then 0 else 1)
                 else
                   if n < 8209
                   then
                     (if n < 8218
                      then
                        (if n < 8222
                         then
                           (if n < 8224
                            then (if n < 8227 then 2 else 1)
                            else 2)
                         else if n < 8220 then 1 else 2)
                      else
                        if n < 8215
                        then (if n < 8216 then 1 else 2)
                        else if n < 8211 then 1 else 2)
                   else
                     if n < 7674
                     then
                       (if n < 7680
                        then
                          (if n < 8203
                           then (if n < 8208 then 1 else 2)
                           else 0)
                        else if n < 7675 then 1 else 0)
                     else
                       if n < 7418
                       then (if n < 7616 then 1 else 0)
                       else if n < 7416 then 1 else 0)
          else
            if n < 6155
            then
              (if n < 6965
               then
                 (if n < 7144
                  then
                    (if n < 7224
                     then
                       (if n < 7394
                        then
                          (if n < 7405
                           then
                             (if n < 7406
                              then (if n < 7412 then 1 else 0)
                              else 1)
                           else if n < 7401 then 0 else 1)
                        else
                          if n < 7379
                          then
                            (if n < 7380
                             then (if n < 7393 then 0 else 1)
                             else 0)
                          else if n < 7376 then 1 else 0)
                     else
                       if n < 7151
                       then
                         (if n < 7212
                          then
                            (if n < 7220
                             then (if n < 7222 then 1 else 0)
                             else 1)
                          else if n < 7154 then 0 else 1)
                       else
                         if n < 7149
                         then (if n < 7150 then 0 else 1)
                         else if n < 7146 then 0 else 1)
                  else
                    if n < 7040
                    then
                      (if n < 7082
                       then
                         (if n < 7086
                          then
                            (if n < 7142
                             then (if n < 7143 then 0 else 1)
                             else 0)
                          else if n < 7083 then 1 else 0)
                       else
                         if n < 7074
                         then
                           (if n < 7078
                            then (if n < 7080 then 1 else 0)
                            else 1)
                         else if n < 7042 then 0 else 1)
                    else
                      if n < 6973
                      then
                        (if n < 6979
                         then
                           (if n < 7019
                            then (if n < 7028 then 0 else 1)
                            else 0)
                         else if n < 6978 then 1 else 0)
                      else
                        if n < 6971
                        then (if n < 6972 then 1 else 0)
                        else if n < 6966 then 1 else 0)
               else
                 if n < 6742
                 then
                   (if n < 6765
                    then
                      (if n < 6832
                       then
                         (if n < 6912
                          then
                            (if n < 6916
                             then (if n < 6964 then 1 else 0)
                             else 1)
                          else if n < 6847 then 0 else 1)
                       else
                         if n < 6781
                         then
                           (if n < 6783
                            then (if n < 6784 then 0 else 1)
                            else 0)
                         else if n < 6771 then 1 else 0)
                    else
                      if n < 6752
                      then
                        (if n < 6754
                         then
                           (if n < 6755
                            then (if n < 6757 then 1 else 0)
                            else 1)
                         else if n < 6753 then 0 else 1)
                      else
                        if n < 6744
                        then (if n < 6751 then 0 else 1)
                        else if n < 6743 then 0 else 1)
                 else
                   if n < 6441
                   then
                     (if n < 6460
                      then
                        (if n < 6681
                         then
                           (if n < 6683
                            then (if n < 6684 then 0 else 1)
                            else 0)
                         else if n < 6679 then 1 else 0)
                      else
                        if n < 6451
                        then (if n < 6457 then 1 else 0)
                        else if n < 6450 then 1 else 0)
                   else
                     if n < 6313
                     then
                       (if n < 6432
                        then
                          (if n < 6435
                           then (if n < 6439 then 1 else 0)
                           else 1)
                        else if n < 6314 then 0 else 1)
                     else
                       if n < 6277
                       then (if n < 6279 then 0 else 1)
                       else if n < 6159 then 0 else 1)
            else
              if n < 4159
              then
                (if n < 4960
                 then
                   (if n < 6068
                    then
                      (if n < 6087
                       then
                         (if n < 6100
                          then
                            (if n < 6109
                             then (if n < 6110 then 0 else 1)
                             else 0)
                          else if n < 6089 then 1 else 0)
                       else
                         if n < 6071
                         then
                           (if n < 6078
                            then (if n < 6086 then 1 else 0)
                            else 1)
                         else if n < 6070 then 0 else 1)
                    else
                      if n < 5941
                      then
                        (if n < 5972
                         then
                           (if n < 6002
                            then (if n < 6004 then 0 else 1)
                            else 0)
                         else if n < 5970 then 1 else 0)
                      else
                        if n < 5909
                        then (if n < 5938 then 1 else 0)
                        else if n < 5906 then 1 else 0)
                 else
                   if n < 4229
                   then
                     (if n < 4254
                      then
                        (if n < 4448
                         then
                           (if n < 4608
                            then (if n < 4957 then 1 else 0)
                            else 1)
                         else if n < 4352 then 0 else 2)
                      else
                        if n < 4237
                        then
                          (if n < 4238
                           then (if n < 4253 then 1 else 0)
                           else 1)
                        else if n < 4231 then 0 else 1)
                   else
                     if n < 4193
                     then
                       (if n < 4213
                        then
                          (if n < 4226
                           then (if n < 4227 then 0 else 1)
                           else 0)
                        else if n < 4209 then 1 else 0)
                     else
                       if n < 4186
                       then (if n < 4190 then 1 else 0)
                       else if n < 4184 then 1 else 0)
              else
                if n < 3953
                then
                  (if n < 4029
                   then
                     (if n < 4146
                      then
                        (if n < 4153
                         then
                           (if n < 4155
                            then (if n < 4157 then 1 else 0)
                            else 1)
                         else if n < 4152 then 0 else 1)
                      else
                        if n < 4039
                        then
                          (if n < 4141
                           then (if n < 4145 then 0 else 1)
                           else 0)
                        else if n < 4038 then 1 else 0)
                   else
                     if n < 3974
                     then
                       (if n < 3981
                        then
                          (if n < 3992
                           then (if n < 3993 then 1 else 0)
                           else 1)
                        else if n < 3976 then 0 else 1)
                     else
                       if n < 3968
                       then (if n < 3973 then 0 else 1)
                       else if n < 3967 then 0 else 1)
                else
                  if n < 3790
                  then
                    (if n < 3894
                     then
                       (if n < 3896
                        then
                          (if n < 3897
                           then (if n < 3898 then 0 else 1)
                           else 0)
                        else if n < 3895 then 1 else 0)
                     else
                       if n < 3866
                       then (if n < 3893 then 1 else 0)
                       else if n < 3864 then 1 else 0)
                  else
                    if n < 3761
                    then
                      (if n < 3764
                       then
                         (if n < 3773 then (if n < 3784 then 1 else 0) else 1)
                       else if n < 3762 then 0 else 1)
                    else
                      if n < 3655
                      then (if n < 3663 then 0 else 1)
                      else if n < 3643 then 0 else 1)
       else
         if n < 1649
         then
           (if n < 2748
            then
              (if n < 3142
               then
                 (if n < 3300
                  then
                    (if n < 3426
                     then
                       (if n < 3541
                        then
                          (if n < 3543
                           then
                             (if n < 3633
                              then (if n < 3634 then 0 else 1)
                              else 0)
                           else if n < 3542 then 1 else 0)
                        else
                          if n < 3530
                          then
                            (if n < 3531
                             then (if n < 3538 then 1 else 0)
                             else 1)
                          else if n < 3428 then 0 else 1)
                     else
                       if n < 3389
                       then
                         (if n < 3397
                          then
                            (if n < 3405
                             then (if n < 3406 then 0 else 1)
                             else 0)
                          else if n < 3393 then 1 else 0)
                       else
                         if n < 3330
                         then (if n < 3387 then 1 else 0)
                         else if n < 3328 then 1 else 0)
                  else
                    if n < 3202
                    then
                      (if n < 3270
                       then
                         (if n < 3276
                          then
                            (if n < 3278
                             then (if n < 3298 then 1 else 0)
                             else 1)
                          else if n < 3271 then 0 else 1)
                       else
                         if n < 3261
                         then
                           (if n < 3263
                            then (if n < 3264 then 0 else 1)
                            else 0)
                         else if n < 3260 then 1 else 0)
                    else
                      if n < 3157
                      then
                        (if n < 3170
                         then
                           (if n < 3172
                            then (if n < 3201 then 1 else 0)
                            else 1)
                         else if n < 3159 then 0 else 1)
                      else
                        if n < 3146
                        then (if n < 3150 then 0 else 1)
                        else if n < 3145 then 0 else 1)
               else
                 if n < 2885
                 then
                   (if n < 3008
                    then
                      (if n < 3073
                       then
                         (if n < 3077
                          then
                            (if n < 3134
                             then (if n < 3137 then 0 else 1)
                             else 0)
                          else if n < 3076 then 1 else 0)
                       else
                         if n < 3021
                         then
                           (if n < 3022
                            then (if n < 3072 then 1 else 0)
                            else 1)
                         else if n < 3009 then 0 else 1)
                    else
                      if n < 2903
                      then
                        (if n < 2916
                         then
                           (if n < 2946
                            then (if n < 2947 then 0 else 1)
                            else 0)
                         else if n < 2914 then 1 else 0)
                      else
                        if n < 2894
                        then (if n < 2902 then 1 else 0)
                        else if n < 2893 then 1 else 0)
                 else
                   if n < 2788
                   then
                     (if n < 2876
                      then
                        (if n < 2879
                         then
                           (if n < 2880
                            then (if n < 2881 then 1 else 0)
                            else 1)
                         else if n < 2877 then 0 else 1)
                      else
                        if n < 2816
                        then
                          (if n < 2817
                           then (if n < 2818 then 0 else 1)
                           else 0)
                        else if n < 2810 then 1 else 0)
                   else
                     if n < 2759
                     then
                       (if n < 2765
                        then
                          (if n < 2766
                           then (if n < 2786 then 1 else 0)
                           else 1)
                        else if n < 2761 then 0 else 1)
                     else
                       if n < 2753
                       then (if n < 2758 then 0 else 1)
                       else if n < 2749 then 0 else 1)
            else
              if n < 2369
              then
                (if n < 2559
                 then
                   (if n < 2635
                    then
                      (if n < 2674
                       then
                         (if n < 2678
                          then
                            (if n < 2689
                             then (if n < 2691 then 0 else 1)
                             else 0)
                          else if n < 2677 then 1 else 0)
                       else
                         if n < 2641
                         then
                           (if n < 2642
                            then (if n < 2672 then 1 else 0)
                            else 1)
                         else if n < 2638 then 0 else 1)
                    else
                      if n < 2621
                      then
                        (if n < 2627
                         then
                           (if n < 2631
                            then (if n < 2633 then 0 else 1)
                            else 0)
                         else if n < 2625 then 1 else 0)
                      else
                        if n < 2563
                        then (if n < 2620 then 1 else 0)
                        else if n < 2561 then 1 else 0)
                 else
                   if n < 2434
                   then
                     (if n < 2509
                      then
                        (if n < 2530
                         then
                           (if n < 2532
                            then (if n < 2558 then 1 else 0)
                            else 1)
                         else if n < 2510 then 0 else 1)
                      else
                        if n < 2493
                        then
                          (if n < 2497
                           then (if n < 2501 then 0 else 1)
                           else 0)
                        else if n < 2492 then 1 else 0)
                   else
                     if n < 2385
                     then
                       (if n < 2402
                        then
                          (if n < 2404
                           then (if n < 2433 then 1 else 0)
                           else 1)
                        else if n < 2392 then 0 else 1)
                     else
                       if n < 2381
                       then (if n < 2382 then 0 else 1)
                       else if n < 2377 then 0 else 1)
              else
                if n < 2036
                then
                  (if n < 2089
                   then
                     (if n < 2307
                      then
                        (if n < 2363
                         then
                           (if n < 2364
                            then (if n < 2365 then 0 else 1)
                            else 0)
                         else if n < 2362 then 1 else 0)
                      else
                        if n < 2137
                        then
                          (if n < 2140
                           then (if n < 2259 then 1 else 0)
                           else 1)
                        else if n < 2094 then 0 else 1)
                   else
                     if n < 2074
                     then
                       (if n < 2084
                        then
                          (if n < 2085
                           then (if n < 2088 then 0 else 1)
                           else 0)
                        else if n < 2075 then 1 else 0)
                     else
                       if n < 2046
                       then (if n < 2070 then 1 else 0)
                       else if n < 2045 then 1 else 0)
                else
                  if n < 1807
                  then
                    (if n < 1840
                     then
                       (if n < 1958
                        then
                          (if n < 1969
                           then (if n < 2027 then 1 else 0)
                           else 1)
                        else if n < 1867 then 0 else 1)
                     else
                       if n < 1809
                       then (if n < 1810 then 0 else 1)
                       else if n < 1808 then 0 else 1)
                  else
                    if n < 1765
                    then
                      (if n < 1769
                       then
                         (if n < 1770 then (if n < 1774 then 0 else 1) else 0)
                       else if n < 1767 then 1 else 0)
                    else
                      if n < 1758
                      then (if n < 1759 then 1 else 0)
                      else if n < 1750 then 1 else 0)
         else
           if n < 363
           then
             (if n < 736
              then
                (if n < 1425
                 then
                   (if n < 1480
                    then
                      (if n < 1564
                       then
                         (if n < 1611
                          then
                            (if n < 1632
                             then (if n < 1648 then 1 else 0)
                             else 1)
                          else if n < 1565 then 0 else 1)
                       else
                         if n < 1542
                         then
                           (if n < 1552
                            then (if n < 1563 then 0 else 1)
                            else 0)
                         else if n < 1536 then 1 else 0)
                    else
                      if n < 1473
                      then
                        (if n < 1476
                         then
                           (if n < 1478
                            then (if n < 1479 then 1 else 0)
                            else 1)
                         else if n < 1475 then 0 else 1)
                      else
                        if n < 1471
                        then (if n < 1472 then 0 else 1)
                        else if n < 1470 then 0 else 1)
                 else
                   if n < 963
                   then
                     (if n < 1104
                      then
                        (if n < 1106
                         then
                           (if n < 1155
                            then (if n < 1162 then 0 else 1)
                            else 0)
                         else if n < 1105 then 1 else 2)
                      else
                        if n < 1025
                        then
                          (if n < 1026
                           then (if n < 1040 then 1 else 2)
                           else 1)
                        else if n < 970 then 2 else 1)
                   else
                     if n < 930
                     then
                       (if n < 938
                        then
                          (if n < 945 then (if n < 962 then 2 else 1) else 2)
                        else if n < 931 then 1 else 2)
                     else
                       if n < 880
                       then (if n < 913 then 1 else 2)
                       else if n < 768 then 1 else 0)
              else
                if n < 593
                then
                  (if n < 716
                   then
                     (if n < 728
                      then
                        (if n < 733
                         then
                           (if n < 734 then (if n < 735 then 1 else 2) else 1)
                         else if n < 732 then 2 else 1)
                      else
                        if n < 718
                        then
                          (if n < 720 then (if n < 721 then 2 else 1) else 2)
                        else if n < 717 then 1 else 2)
                   else
                     if n < 708
                     then
                       (if n < 711
                        then
                          (if n < 712 then (if n < 713 then 1 else 2) else 1)
                        else if n < 709 then 2 else 1)
                     else
                       if n < 609
                       then (if n < 610 then 2 else 1)
                       else if n < 594 then 2 else 1)
                else
                  if n < 469
                  then
                    (if n < 473
                     then
                       (if n < 475
                        then
                          (if n < 476 then (if n < 477 then 2 else 1) else 2)
                        else if n < 474 then 1 else 2)
                     else
                       if n < 471
                       then (if n < 472 then 1 else 2)
                       else if n < 470 then 1 else 2)
                  else
                    if n < 464
                    then
                      (if n < 466
                       then
                         (if n < 467 then (if n < 468 then 1 else 2) else 1)
                       else if n < 465 then 2 else 1)
                    else
                      if n < 462
                      then (if n < 463 then 2 else 1)
                      else if n < 364 then 2 else 1)
           else
             if n < 240
             then
               (if n < 296
                then
                  (if n < 324
                   then
                     (if n < 334
                      then
                        (if n < 340
                         then
                           (if n < 358 then (if n < 360 then 2 else 1) else 2)
                         else if n < 338 then 1 else 2)
                      else
                        if n < 328
                        then
                          (if n < 332 then (if n < 333 then 1 else 2) else 1)
                        else if n < 325 then 2 else 1)
                   else
                     if n < 308
                     then
                       (if n < 313
                        then
                          (if n < 319 then (if n < 323 then 2 else 1) else 2)
                        else if n < 312 then 1 else 2)
                     else
                       if n < 300
                       then (if n < 305 then 1 else 2)
                       else if n < 299 then 1 else 2)
                else
                  if n < 255
                  then
                    (if n < 275
                     then
                       (if n < 283
                        then
                          (if n < 284 then (if n < 294 then 1 else 2) else 1)
                        else if n < 276 then 2 else 1)
                     else
                       if n < 258
                       then
                         (if n < 273 then (if n < 274 then 2 else 1) else 2)
                       else if n < 257 then 1 else 2)
                  else
                    if n < 247
                    then
                      (if n < 252
                       then
                         (if n < 253 then (if n < 254 then 1 else 2) else 1)
                       else if n < 251 then 2 else 1)
                    else
                      if n < 242
                      then (if n < 244 then 2 else 1)
                      else if n < 241 then 2 else 1)
             else
               if n < 181
               then
                 (if n < 215
                  then
                    (if n < 231
                     then
                       (if n < 235
                        then
                          (if n < 236 then (if n < 238 then 2 else 1) else 2)
                        else if n < 232 then 1 else 2)
                     else
                       if n < 222
                       then
                         (if n < 226 then (if n < 230 then 1 else 2) else 1)
                       else if n < 217 then 2 else 1)
                  else
                    if n < 192
                    then
                      (if n < 199
                       then
                         (if n < 208 then (if n < 209 then 2 else 1) else 2)
                       else if n < 198 then 1 else 2)
                    else
                      if n < 187
                      then (if n < 188 then 1 else 2)
                      else if n < 182 then 1 else 2)
               else
                 if n < 165
                 then
                   (if n < 171
                    then
                      (if n < 174
                       then
                         (if n < 175 then (if n < 176 then 1 else 2) else 1)
                       else if n < 173 then 2 else 0)
                    else
                      if n < 169
                      then (if n < 170 then 1 else 2)
                      else if n < 167 then 1 else 2)
                 else
                   if n < 127
                   then
                     (if n < 161
                      then
                        (if n < 162 then (if n < 164 then 1 else 2) else 1)
                      else if n < 160 then 2 else 1)
                   else
                     if n < 1
                     then (if n < 32 then (-1) else 1)
                     else if n < 0 then (-1) else 0)
  else
    if n < 12872
    then
      (if n < 72152
       then
         (if n < 123184
          then
            (if n < 128405
             then
               (if n < 129454
                then
                  (if n < 173824
                   then
                     (if n < 191457
                      then
                        (if n < 917506
                         then
                           (if n < 917632
                            then (if n < 917760 then 1 else 0)
                            else if n < 917536 then 1 else 0)
                         else
                           if n < 195102
                           then (if n < 917505 then 1 else 0)
                           else if n < 194560 then 1 else 2)
                      else
                        if n < 178206
                        then
                          (if n < 183970
                           then (if n < 183984 then 1 else 2)
                           else if n < 178208 then 1 else 2)
                        else
                          if n < 177973
                          then (if n < 177984 then 1 else 2)
                          else 1)
                   else
                     if n < 129659
                     then
                       (if n < 129680
                        then
                          (if n < 131072
                           then (if n < 173783 then 2 else 1)
                           else if n < 129686 then 2 else 1)
                        else
                          if n < 129664
                          then (if n < 129667 then 2 else 1)
                          else 2)
                     else
                       if n < 129536
                       then
                         (if n < 129652
                          then (if n < 129656 then 1 else 2)
                          else if n < 129648 then 1 else 2)
                       else
                         if n < 129483
                         then (if n < 129485 then 1 else 2)
                         else 1)
                else
                  if n < 128747
                  then
                    (if n < 129394
                     then
                       (if n < 129402
                        then
                          (if n < 129445
                           then (if n < 129451 then 2 else 1)
                           else if n < 129443 then 2 else 1)
                        else
                          if n < 129395
                          then (if n < 129399 then 2 else 1)
                          else 2)
                     else
                       if n < 128763
                       then
                         (if n < 129004
                          then (if n < 129293 then 1 else 2)
                          else if n < 128992 then 1 else 2)
                       else
                         if n < 128749
                         then (if n < 128756 then 1 else 2)
                         else 1)
                  else
                    if n < 128710
                    then
                      (if n < 128720
                       then
                         (if n < 128725
                          then (if n < 128726 then 2 else 1)
                          else if n < 128723 then 2 else 1)
                       else
                         if n < 128716
                         then (if n < 128717 then 2 else 1)
                         else 2)
                    else
                      if n < 128421
                      then
                        (if n < 128592
                         then (if n < 128640 then 1 else 2)
                         else if n < 128507 then 1 else 2)
                      else
                        if n < 128407
                        then (if n < 128420 then 1 else 2)
                        else 1)
             else
               if n < 127789
               then
                 (if n < 127992
                  then
                    (if n < 128318
                     then
                       (if n < 128336
                        then
                          (if n < 128378
                           then (if n < 128379 then 2 else 1)
                           else if n < 128360 then 2 else 1)
                        else
                          if n < 128331
                          then (if n < 128335 then 2 else 1)
                          else 2)
                     else
                       if n < 128065
                       then
                         (if n < 128253
                          then (if n < 128255 then 1 else 2)
                          else if n < 128066 then 1 else 2)
                       else
                         if n < 128063
                         then (if n < 128064 then 1 else 2)
                         else 1)
                  else
                    if n < 127947
                    then
                      (if n < 127968
                       then
                         (if n < 127988
                          then (if n < 127989 then 2 else 1)
                          else if n < 127985 then 2 else 1)
                       else
                         if n < 127951
                         then (if n < 127956 then 2 else 1)
                         else 2)
                    else
                      if n < 127869
                      then
                        (if n < 127892
                         then (if n < 127904 then 1 else 2)
                         else if n < 127870 then 1 else 2)
                      else
                        if n < 127798
                        then (if n < 127799 then 1 else 2)
                        else 1)
               else
                 if n < 127377
                 then
                   (if n < 127561
                    then
                      (if n < 127584
                       then
                         (if n < 127744
                          then (if n < 127777 then 2 else 1)
                          else if n < 127590 then 2 else 1)
                       else
                         if n < 127568
                         then (if n < 127570 then 2 else 1)
                         else 2)
                    else
                      if n < 127491
                      then
                        (if n < 127548
                         then (if n < 127552 then 1 else 2)
                         else if n < 127504 then 1 else 2)
                      else
                        if n < 127387
                        then (if n < 127488 then 1 else 2)
                        else 1)
                 else
                   if n < 125259
                   then
                     (if n < 127183
                      then
                        (if n < 127374
                         then (if n < 127375 then 2 else 1)
                         else if n < 127184 then 2 else 1)
                      else
                        if n < 126980
                        then (if n < 126981 then 2 else 1)
                        else 2)
                   else
                     if n < 123632
                     then
                       (if n < 125143
                        then (if n < 125252 then 1 else 0)
                        else if n < 125136 then 1 else 0)
                     else
                       if n < 123191
                       then (if n < 123628 then 1 else 0)
                       else 1)
          else
            if n < 92917
            then
              (if n < 119171
               then
                 (if n < 121477
                  then
                    (if n < 122888
                     then
                       (if n < 122915
                        then
                          (if n < 122918
                           then (if n < 122923 then 0 else 1)
                           else if n < 122917 then 0 else 1)
                        else
                          if n < 122907
                          then (if n < 122914 then 0 else 1)
                          else if n < 122905 then 0 else 1)
                     else
                       if n < 121505
                       then
                         (if n < 122880
                          then (if n < 122887 then 0 else 1)
                          else if n < 121520 then 0 else 1)
                       else
                         if n < 121499
                         then (if n < 121504 then 0 else 1)
                         else 0)
                  else
                    if n < 121344
                    then
                      (if n < 121453
                       then
                         (if n < 121462
                          then (if n < 121476 then 1 else 0)
                          else if n < 121461 then 1 else 0)
                       else
                         if n < 121399
                         then (if n < 121403 then 1 else 0)
                         else 1)
                    else
                      if n < 119210
                      then
                        (if n < 119362
                         then (if n < 119365 then 0 else 1)
                         else if n < 119214 then 0 else 1)
                      else
                        if n < 119173
                        then (if n < 119180 then 0 else 1)
                        else 0)
               else
                 if n < 110879
                 then
                   (if n < 113821
                    then
                      (if n < 113828
                       then
                         (if n < 119146
                          then (if n < 119155 then 1 else 0)
                          else if n < 119143 then 1 else 0)
                       else
                         if n < 113823
                         then (if n < 113824 then 1 else 0)
                         else 1)
                    else
                      if n < 110948
                      then
                        (if n < 110960
                         then (if n < 111356 then 0 else 1)
                         else if n < 110952 then 2 else 1)
                      else
                        if n < 110928
                        then (if n < 110931 then 2 else 1)
                        else 2)
                 else
                   if n < 94176
                   then
                     (if n < 100344
                      then
                        (if n < 101107
                         then (if n < 110592 then 1 else 2)
                         else if n < 100352 then 1 else 2)
                      else
                        if n < 94180 then (if n < 94208 then 1 else 2) else 1)
                   else
                     if n < 94031
                     then
                       (if n < 94095
                        then (if n < 94099 then 2 else 1)
                        else if n < 94032 then 0 else 1)
                     else
                       if n < 92976 then (if n < 92983 then 0 else 1) else 0)
            else
              if n < 72872
              then
                (if n < 73030
                 then
                   (if n < 73111
                    then
                      (if n < 73461
                       then
                         (if n < 78905
                          then (if n < 92912 then 1 else 0)
                          else if n < 78896 then 1 else 0)
                       else
                         if n < 73112
                         then (if n < 73459 then 1 else 0)
                         else 1)
                    else
                      if n < 73104
                      then
                        (if n < 73109
                         then (if n < 73110 then 0 else 1)
                         else if n < 73106 then 0 else 1)
                      else
                        if n < 73031 then (if n < 73032 then 0 else 1) else 0)
                 else
                   if n < 73009
                   then
                     (if n < 73019
                      then
                        (if n < 73022
                         then (if n < 73023 then 1 else 0)
                         else if n < 73020 then 1 else 0)
                      else
                        if n < 73015 then (if n < 73018 then 1 else 0) else 1)
                   else
                     if n < 72882
                     then
                       (if n < 72885
                        then (if n < 72887 then 0 else 1)
                        else if n < 72884 then 0 else 1)
                     else
                       if n < 72874 then (if n < 72881 then 0 else 1) else 0)
              else
                if n < 72279
                then
                  (if n < 72752
                   then
                     (if n < 72766
                      then
                        (if n < 72768
                         then (if n < 72850 then 1 else 0)
                         else if n < 72767 then 1 else 0)
                      else
                        if n < 72759 then (if n < 72760 then 1 else 0) else 1)
                   else
                     if n < 72330
                     then
                       (if n < 72344
                        then (if n < 72346 then 0 else 1)
                        else if n < 72343 then 0 else 1)
                     else
                       if n < 72281 then (if n < 72284 then 0 else 1) else 0)
                else
                  if n < 72243
                  then
                    (if n < 72255
                     then
                       (if n < 72264
                        then (if n < 72273 then 1 else 0)
                        else if n < 72263 then 1 else 0)
                     else
                       if n < 72249 then (if n < 72251 then 1 else 0) else 1)
                  else
                    if n < 72160
                    then
                      (if n < 72193
                       then (if n < 72203 then 0 else 1)
                       else if n < 72161 then 0 else 1)
                    else
                      if n < 72154 then (if n < 72156 then 0 else 1) else 0)
       else
         if n < 68101
         then
           (if n < 70459
            then
              (if n < 71103
               then
                 (if n < 71344
                  then
                    (if n < 71462
                     then
                       (if n < 71736
                        then
                          (if n < 71739
                           then (if n < 72148 then 1 else 0)
                           else if n < 71737 then 1 else 0)
                        else
                          if n < 71468
                          then (if n < 71727 then 1 else 0)
                          else if n < 71463 then 1 else 0)
                     else
                       if n < 71352
                       then
                         (if n < 71456
                          then (if n < 71458 then 1 else 0)
                          else if n < 71453 then 1 else 0)
                       else
                         if n < 71350
                         then (if n < 71351 then 1 else 0)
                         else 1)
                  else
                    if n < 71230
                    then
                      (if n < 71339
                       then
                         (if n < 71341
                          then (if n < 71342 then 0 else 1)
                          else if n < 71340 then 0 else 1)
                       else
                         if n < 71231
                         then (if n < 71233 then 0 else 1)
                         else 0)
                    else
                      if n < 71134
                      then
                        (if n < 71227
                         then (if n < 71229 then 1 else 0)
                         else if n < 71219 then 1 else 0)
                      else
                        if n < 71105 then (if n < 71132 then 1 else 0) else 1)
               else
                 if n < 70750
                 then
                   (if n < 70849
                    then
                      (if n < 71090
                       then
                         (if n < 71100
                          then (if n < 71102 then 0 else 1)
                          else if n < 71094 then 0 else 1)
                       else
                         if n < 70850
                         then (if n < 70852 then 0 else 1)
                         else 0)
                    else
                      if n < 70841
                      then
                        (if n < 70843
                         then (if n < 70847 then 1 else 0)
                         else if n < 70842 then 1 else 0)
                      else
                        if n < 70751 then (if n < 70835 then 1 else 0) else 1)
                 else
                   if n < 70517
                   then
                     (if n < 70722
                      then
                        (if n < 70726
                         then (if n < 70727 then 0 else 1)
                         else if n < 70725 then 0 else 1)
                      else
                        if n < 70712 then (if n < 70720 then 0 else 1) else 0)
                   else
                     if n < 70465
                     then
                       (if n < 70509
                        then (if n < 70512 then 1 else 0)
                        else if n < 70502 then 1 else 0)
                     else
                       if n < 70461 then (if n < 70464 then 1 else 0) else 1)
            else
              if n < 69888
              then
                (if n < 70191
                 then
                   (if n < 70207
                    then
                      (if n < 70371
                       then
                         (if n < 70400
                          then (if n < 70402 then 0 else 1)
                          else if n < 70379 then 0 else 1)
                       else
                         if n < 70367
                         then (if n < 70368 then 0 else 1)
                         else 0)
                    else
                      if n < 70197
                      then
                        (if n < 70200
                         then (if n < 70206 then 1 else 0)
                         else if n < 70198 then 1 else 0)
                      else
                        if n < 70194 then (if n < 70196 then 1 else 0) else 1)
                 else
                   if n < 70004
                   then
                     (if n < 70070
                      then
                        (if n < 70089
                         then (if n < 70093 then 0 else 1)
                         else if n < 70079 then 0 else 1)
                      else
                        if n < 70016 then (if n < 70018 then 0 else 1) else 0)
                   else
                     if n < 69932
                     then
                       (if n < 69941
                        then (if n < 70003 then 1 else 0)
                        else if n < 69933 then 1 else 0)
                     else
                       if n < 69891 then (if n < 69927 then 1 else 0) else 1)
              else
                if n < 69633
                then
                  (if n < 69815
                   then
                     (if n < 69821
                      then
                        (if n < 69837
                         then (if n < 69838 then 0 else 1)
                         else if n < 69822 then 0 else 1)
                      else
                        if n < 69817 then (if n < 69819 then 0 else 1) else 0)
                   else
                     if n < 69703
                     then
                       (if n < 69762
                        then (if n < 69811 then 1 else 0)
                        else if n < 69759 then 1 else 0)
                     else
                       if n < 69634 then (if n < 69688 then 1 else 0) else 1)
                else
                  if n < 68160
                  then
                    (if n < 68900
                     then
                       (if n < 69446
                        then (if n < 69457 then 0 else 1)
                        else if n < 68904 then 0 else 1)
                     else
                       if n < 68325 then (if n < 68327 then 0 else 1) else 0)
                  else
                    if n < 68112
                    then
                      (if n < 68155
                       then (if n < 68159 then 1 else 0)
                       else if n < 68152 then 1 else 0)
                    else
                      if n < 68103 then (if n < 68108 then 1 else 0) else 1)
         else
           if n < 43597
           then
             (if n < 64218
              then
                (if n < 65280
                 then
                   (if n < 66045
                    then
                      (if n < 66422
                       then
                         (if n < 68097
                          then (if n < 68100 then 0 else 1)
                          else if n < 66427 then 0 else 1)
                       else
                         if n < 66272
                         then (if n < 66273 then 0 else 1)
                         else if n < 66046 then 0 else 1)
                    else
                      if n < 65504
                      then
                        (if n < 65529
                         then (if n < 65532 then 0 else 1)
                         else if n < 65511 then 0 else 1)
                      else
                        if n < 65281 then (if n < 65377 then 2 else 1) else 2)
                 else
                   if n < 65072
                   then
                     (if n < 65127
                      then
                        (if n < 65132
                         then (if n < 65279 then 1 else 0)
                         else if n < 65128 then 1 else 2)
                      else
                        if n < 65107 then (if n < 65108 then 1 else 2) else 1)
                   else
                     if n < 65024
                     then
                       (if n < 65050
                        then (if n < 65056 then 2 else 0)
                        else if n < 65040 then 1 else 2)
                     else
                       if n < 64286 then (if n < 64287 then 0 else 1) else 0)
              else
                if n < 43758
                then
                  (if n < 44013
                   then
                     (if n < 55204
                      then
                        (if n < 64110
                         then (if n < 64112 then 1 else 2)
                         else if n < 63744 then 1 else 2)
                      else
                        if n < 44014 then (if n < 44032 then 1 else 2) else 1)
                   else
                     if n < 44005
                     then
                       (if n < 44008
                        then (if n < 44009 then 0 else 1)
                        else if n < 44006 then 0 else 1)
                     else
                       if n < 43766 then (if n < 43767 then 0 else 1) else 0)
                else
                  if n < 43703
                  then
                    (if n < 43712
                     then
                       (if n < 43714
                        then (if n < 43756 then 1 else 0)
                        else if n < 43713 then 1 else 0)
                     else
                       if n < 43705 then (if n < 43710 then 1 else 0) else 1)
                  else
                    if n < 43696
                    then
                      (if n < 43698
                       then (if n < 43701 then 0 else 1)
                       else if n < 43697 then 0 else 1)
                    else
                      if n < 43644 then (if n < 43645 then 0 else 1) else 0)
           else
             if n < 43250
             then
               (if n < 43450
                then
                  (if n < 43569
                   then
                     (if n < 43575
                      then
                        (if n < 43588
                         then (if n < 43596 then 1 else 0)
                         else if n < 43587 then 1 else 0)
                      else
                        if n < 43571 then (if n < 43573 then 1 else 0) else 1)
                   else
                     if n < 43493
                     then
                       (if n < 43561
                        then (if n < 43567 then 0 else 1)
                        else if n < 43494 then 0 else 1)
                     else
                       if n < 43452 then (if n < 43454 then 0 else 1) else 0)
                else
                  if n < 43360
                  then
                    (if n < 43395
                     then
                       (if n < 43444
                        then (if n < 43446 then 1 else 0)
                        else if n < 43443 then 1 else 0)
                     else
                       if n < 43389 then (if n < 43392 then 1 else 0) else 1)
                  else
                    if n < 43302
                    then
                      (if n < 43335
                       then (if n < 43346 then 2 else 1)
                       else if n < 43310 then 0 else 1)
                    else
                      if n < 43263 then (if n < 43264 then 0 else 1) else 0)
             else
               if n < 42656
               then
                 (if n < 43019
                  then
                    (if n < 43047
                     then
                       (if n < 43206
                        then (if n < 43232 then 1 else 0)
                        else if n < 43204 then 1 else 0)
                     else
                       if n < 43020 then (if n < 43045 then 1 else 0) else 1)
                  else
                    if n < 43010
                    then
                      (if n < 43014
                       then (if n < 43015 then 0 else 1)
                       else if n < 43011 then 0 else 1)
                    else
                      if n < 42736 then (if n < 42738 then 0 else 1) else 0)
               else
                 if n < 42128
                 then
                   (if n < 42611
                    then
                      (if n < 42622
                       then (if n < 42654 then 1 else 0)
                       else if n < 42612 then 1 else 0)
                    else
                      if n < 42183 then (if n < 42607 then 1 else 0) else 1)
                 else
                   if n < 19968
                   then
                     (if n < 40960
                      then (if n < 42125 then 2 else 1)
                      else if n < 40944 then 2 else 1)
                   else if n < 12880 then (if n < 19894 then 2 else 1) else 2)
    else
      if n < 4213
      then
        (if n < 7680
         then
           (if n < 9996
            then
              (if n < 11776
               then
                 (if n < 12439
                  then
                    (if n < 12687
                     then
                       (if n < 12772
                        then
                          (if n < 12831
                           then (if n < 12832 then 1 else 2)
                           else if n < 12784 then 1 else 2)
                        else
                          if n < 12731
                          then (if n < 12736 then 1 else 2)
                          else if n < 12688 then 1 else 2)
                     else
                       if n < 12544
                       then
                         (if n < 12592
                          then (if n < 12593 then 1 else 2)
                          else if n < 12549 then 1 else 2)
                       else
                         if n < 12441
                         then (if n < 12443 then 1 else 2)
                         else 0)
                  else
                    if n < 12272
                    then
                      (if n < 12330
                       then
                         (if n < 12351
                          then (if n < 12353 then 1 else 2)
                          else if n < 12334 then 1 else 2)
                       else
                         if n < 12284
                         then (if n < 12288 then 0 else 2)
                         else 1)
                    else
                      if n < 11931
                      then
                        (if n < 12032
                         then (if n < 12246 then 2 else 1)
                         else if n < 12020 then 2 else 1)
                      else
                        if n < 11904 then (if n < 11930 then 2 else 1) else 2)
               else
                 if n < 10161
                 then
                   (if n < 11093
                    then
                      (if n < 11506
                       then
                         (if n < 11648
                          then (if n < 11744 then 1 else 0)
                          else if n < 11647 then 1 else 0)
                       else
                         if n < 11094
                         then (if n < 11503 then 1 else 0)
                         else 1)
                    else
                      if n < 11035
                      then
                        (if n < 11088
                         then (if n < 11089 then 2 else 1)
                         else if n < 11037 then 2 else 1)
                      else
                        if n < 10175 then (if n < 10176 then 2 else 1) else 2)
                 else
                   if n < 10067
                   then
                     (if n < 10072
                      then
                        (if n < 10136
                         then (if n < 10160 then 1 else 2)
                         else if n < 10133 then 1 else 2)
                      else
                        if n < 10070 then (if n < 10071 then 1 else 2) else 1)
                   else
                     if n < 10060
                     then
                       (if n < 10062
                        then (if n < 10063 then 2 else 1)
                        else if n < 10061 then 2 else 1)
                     else
                       if n < 10024 then (if n < 10025 then 2 else 1) else 2)
            else
              if n < 9856
              then
                (if n < 9941
                 then
                   (if n < 9978
                    then
                      (if n < 9982
                       then
                         (if n < 9990
                          then (if n < 9994 then 1 else 2)
                          else if n < 9989 then 1 else 2)
                       else
                         if n < 9979 then (if n < 9981 then 1 else 2) else 1)
                    else
                      if n < 9970
                      then
                        (if n < 9973
                         then (if n < 9974 then 2 else 1)
                         else if n < 9972 then 2 else 1)
                      else
                        if n < 9962 then (if n < 9963 then 2 else 1) else 2)
                 else
                   if n < 9917
                   then
                     (if n < 9926
                      then
                        (if n < 9935
                         then (if n < 9940 then 1 else 2)
                         else if n < 9934 then 1 else 2)
                      else
                        if n < 9919 then (if n < 9924 then 1 else 2) else 1)
                   else
                     if n < 9889
                     then
                       (if n < 9898
                        then (if n < 9900 then 2 else 1)
                        else if n < 9890 then 2 else 1)
                     else if n < 9875 then (if n < 9876 then 2 else 1) else 2)
              else
                if n < 9003
                then
                  (if n < 9725
                   then
                     (if n < 9750
                      then
                        (if n < 9812
                         then (if n < 9855 then 1 else 2)
                         else if n < 9800 then 1 else 2)
                      else
                        if n < 9727 then (if n < 9748 then 1 else 2) else 1)
                   else
                     if n < 9200
                     then
                       (if n < 9203
                        then (if n < 9204 then 2 else 1)
                        else if n < 9201 then 2 else 1)
                     else if n < 9193 then (if n < 9197 then 2 else 1) else 2)
                else
                  if n < 8294
                  then
                    (if n < 8433
                     then
                       (if n < 8988
                        then (if n < 9001 then 1 else 2)
                        else if n < 8986 then 1 else 2)
                     else if n < 8304 then (if n < 8400 then 1 else 0) else 1)
                  else
                    if n < 8234
                    then
                      (if n < 8288
                       then (if n < 8293 then 0 else 1)
                       else if n < 8239 then 0 else 1)
                    else if n < 8203 then (if n < 8208 then 0 else 1) else 0)
         else
           if n < 6754
           then
             (if n < 7083
              then
                (if n < 7376
                 then
                   (if n < 7406
                    then
                      (if n < 7418
                       then
                         (if n < 7674
                          then (if n < 7675 then 1 else 0)
                          else if n < 7616 then 1 else 0)
                       else
                         if n < 7413
                         then (if n < 7416 then 1 else 0)
                         else if n < 7412 then 1 else 0)
                    else
                      if n < 7393
                      then
                        (if n < 7401
                         then (if n < 7405 then 1 else 0)
                         else if n < 7394 then 1 else 0)
                      else
                        if n < 7379 then (if n < 7380 then 1 else 0) else 1)
                 else
                   if n < 7150
                   then
                     (if n < 7212
                      then
                        (if n < 7222
                         then (if n < 7224 then 0 else 1)
                         else if n < 7220 then 0 else 1)
                      else
                        if n < 7151 then (if n < 7154 then 0 else 1) else 0)
                   else
                     if n < 7143
                     then
                       (if n < 7146
                        then (if n < 7149 then 1 else 0)
                        else if n < 7144 then 1 else 0)
                     else if n < 7086 then (if n < 7142 then 1 else 0) else 1)
              else
                if n < 6966
                then
                  (if n < 7028
                   then
                     (if n < 7074
                      then
                        (if n < 7080
                         then (if n < 7082 then 0 else 1)
                         else if n < 7078 then 0 else 1)
                      else
                        if n < 7040 then (if n < 7042 then 0 else 1) else 0)
                   else
                     if n < 6973
                     then
                       (if n < 6979
                        then (if n < 7019 then 1 else 0)
                        else if n < 6978 then 1 else 0)
                     else if n < 6971 then (if n < 6972 then 1 else 0) else 1)
                else
                  if n < 6784
                  then
                    (if n < 6912
                     then
                       (if n < 6964
                        then (if n < 6965 then 0 else 1)
                        else if n < 6916 then 0 else 1)
                     else if n < 6832 then (if n < 6847 then 0 else 1) else 0)
                  else
                    if n < 6765
                    then
                      (if n < 6781
                       then (if n < 6783 then 1 else 0)
                       else if n < 6771 then 1 else 0)
                    else if n < 6755 then (if n < 6757 then 1 else 0) else 1)
           else
             if n < 6089
             then
               (if n < 6450
                then
                  (if n < 6684
                   then
                     (if n < 6744
                      then
                        (if n < 6752
                         then (if n < 6753 then 0 else 1)
                         else if n < 6751 then 0 else 1)
                      else
                        if n < 6742 then (if n < 6743 then 0 else 1) else 0)
                   else
                     if n < 6460
                     then
                       (if n < 6681
                        then (if n < 6683 then 1 else 0)
                        else if n < 6679 then 1 else 0)
                     else if n < 6451 then (if n < 6457 then 1 else 0) else 1)
                else
                  if n < 6279
                  then
                    (if n < 6432
                     then
                       (if n < 6439
                        then (if n < 6441 then 0 else 1)
                        else if n < 6435 then 0 else 1)
                     else if n < 6313 then (if n < 6314 then 0 else 1) else 0)
                  else
                    if n < 6110
                    then
                      (if n < 6159
                       then (if n < 6277 then 1 else 0)
                       else if n < 6155 then 1 else 0)
                    else if n < 6100 then (if n < 6109 then 1 else 0) else 1)
             else
               if n < 5906
               then
                 (if n < 6004
                  then
                    (if n < 6071
                     then
                       (if n < 6086
                        then (if n < 6087 then 0 else 1)
                        else if n < 6078 then 0 else 1)
                     else if n < 6068 then (if n < 6070 then 0 else 1) else 0)
                  else
                    if n < 5941
                    then
                      (if n < 5972
                       then (if n < 6002 then 1 else 0)
                       else if n < 5970 then 1 else 0)
                    else if n < 5909 then (if n < 5938 then 1 else 0) else 1)
               else
                 if n < 4253
                 then
                   (if n < 4448
                    then
                      (if n < 4957
                       then (if n < 4960 then 0 else 1)
                       else if n < 4608 then 0 else 1)
                    else if n < 4254 then (if n < 4352 then 0 else 2) else 1)
                 else
                   if n < 4229
                   then
                     (if n < 4237
                      then (if n < 4238 then 0 else 1)
                      else if n < 4231 then 0 else 1)
                   else if n < 4226 then (if n < 4227 then 0 else 1) else 0)
      else
        if n < 2765
        then
          (if n < 3393
           then
             (if n < 3895
              then
                (if n < 4038
                 then
                   (if n < 4155
                    then
                      (if n < 4186
                       then
                         (if n < 4193
                          then (if n < 4209 then 1 else 0)
                          else if n < 4190 then 1 else 0)
                       else
                         if n < 4159
                         then (if n < 4184 then 1 else 0)
                         else if n < 4157 then 1 else 0)
                    else
                      if n < 4145
                      then
                        (if n < 4152
                         then (if n < 4153 then 1 else 0)
                         else if n < 4146 then 1 else 0)
                      else
                        if n < 4039 then (if n < 4141 then 1 else 0) else 1)
                 else
                   if n < 3973
                   then
                     (if n < 3981
                      then
                        (if n < 3993
                         then (if n < 4029 then 0 else 1)
                         else if n < 3992 then 0 else 1)
                      else
                        if n < 3974 then (if n < 3976 then 0 else 1) else 0)
                   else
                     if n < 3898
                     then
                       (if n < 3967
                        then (if n < 3968 then 1 else 0)
                        else if n < 3953 then 1 else 0)
                     else if n < 3896 then (if n < 3897 then 1 else 0) else 1)
              else
                if n < 3636
                then
                  (if n < 3773
                   then
                     (if n < 3864
                      then
                        (if n < 3893
                         then (if n < 3894 then 0 else 1)
                         else if n < 3866 then 0 else 1)
                      else
                        if n < 3784 then (if n < 3790 then 0 else 1) else 0)
                   else
                     if n < 3663
                     then
                       (if n < 3762
                        then (if n < 3764 then 1 else 0)
                        else if n < 3761 then 1 else 0)
                     else if n < 3643 then (if n < 3655 then 1 else 0) else 1)
                else
                  if n < 3531
                  then
                    (if n < 3542
                     then
                       (if n < 3633
                        then (if n < 3634 then 0 else 1)
                        else if n < 3543 then 0 else 1)
                     else if n < 3538 then (if n < 3541 then 0 else 1) else 0)
                  else
                    if n < 3406
                    then
                      (if n < 3428
                       then (if n < 3530 then 1 else 0)
                       else if n < 3426 then 1 else 0)
                    else if n < 3397 then (if n < 3405 then 1 else 0) else 1)
           else
             if n < 3076
             then
               (if n < 3260
                then
                  (if n < 3278
                   then
                     (if n < 3328
                      then
                        (if n < 3387
                         then (if n < 3389 then 0 else 1)
                         else if n < 3330 then 0 else 1)
                      else
                        if n < 3298 then (if n < 3300 then 0 else 1) else 0)
                   else
                     if n < 3264
                     then
                       (if n < 3271
                        then (if n < 3276 then 1 else 0)
                        else if n < 3270 then 1 else 0)
                     else if n < 3261 then (if n < 3263 then 1 else 0) else 1)
                else
                  if n < 3150
                  then
                    (if n < 3170
                     then
                       (if n < 3201
                        then (if n < 3202 then 0 else 1)
                        else if n < 3172 then 0 else 1)
                     else if n < 3157 then (if n < 3159 then 0 else 1) else 0)
                  else
                    if n < 3137
                    then
                      (if n < 3145
                       then (if n < 3146 then 1 else 0)
                       else if n < 3142 then 1 else 0)
                    else if n < 3077 then (if n < 3134 then 1 else 0) else 1)
             else
               if n < 2893
               then
                 (if n < 2947
                  then
                    (if n < 3021
                     then
                       (if n < 3072
                        then (if n < 3073 then 0 else 1)
                        else if n < 3022 then 0 else 1)
                     else if n < 3008 then (if n < 3009 then 0 else 1) else 0)
                  else
                    if n < 2903
                    then
                      (if n < 2916
                       then (if n < 2946 then 1 else 0)
                       else if n < 2914 then 1 else 0)
                    else if n < 2894 then (if n < 2902 then 1 else 0) else 1)
               else
                 if n < 2818
                 then
                   (if n < 2879
                    then
                      (if n < 2881
                       then (if n < 2885 then 0 else 1)
                       else if n < 2880 then 0 else 1)
                    else if n < 2876 then (if n < 2877 then 0 else 1) else 0)
                 else
                   if n < 2788
                   then
                     (if n < 2816
                      then (if n < 2817 then 1 else 0)
                      else if n < 2810 then 1 else 0)
                   else if n < 2766 then (if n < 2786 then 1 else 0) else 1)
        else
          if n < 2085
          then
            (if n < 2530
             then
               (if n < 2641
                then
                  (if n < 2691
                   then
                     (if n < 2753
                      then
                        (if n < 2759
                         then (if n < 2761 then 0 else 1)
                         else if n < 2758 then 0 else 1)
                      else
                        if n < 2748 then (if n < 2749 then 0 else 1) else 0)
                   else
                     if n < 2674
                     then
                       (if n < 2678
                        then (if n < 2689 then 1 else 0)
                        else if n < 2677 then 1 else 0)
                     else if n < 2642 then (if n < 2672 then 1 else 0) else 1)
                else
                  if n < 2621
                  then
                    (if n < 2631
                     then
                       (if n < 2635
                        then (if n < 2638 then 0 else 1)
                        else if n < 2633 then 0 else 1)
                     else if n < 2625 then (if n < 2627 then 0 else 1) else 0)
                  else
                    if n < 2559
                    then
                      (if n < 2563
                       then (if n < 2620 then 1 else 0)
                       else if n < 2561 then 1 else 0)
                    else if n < 2532 then (if n < 2558 then 1 else 0) else 1)
             else
               if n < 2381
               then
                 (if n < 2434
                  then
                    (if n < 2497
                     then
                       (if n < 2509
                        then (if n < 2510 then 0 else 1)
                        else if n < 2501 then 0 else 1)
                     else if n < 2492 then (if n < 2493 then 0 else 1) else 0)
                  else
                    if n < 2392
                    then
                      (if n < 2404
                       then (if n < 2433 then 1 else 0)
                       else if n < 2402 then 1 else 0)
                    else if n < 2382 then (if n < 2385 then 1 else 0) else 1)
               else
                 if n < 2307
                 then
                   (if n < 2364
                    then
                      (if n < 2369
                       then (if n < 2377 then 0 else 1)
                       else if n < 2365 then 0 else 1)
                    else if n < 2362 then (if n < 2363 then 0 else 1) else 0)
                 else
                   if n < 2094
                   then
                     (if n < 2140
                      then (if n < 2259 then 1 else 0)
                      else if n < 2137 then 1 else 0)
                   else if n < 2088 then (if n < 2089 then 1 else 0) else 1)
          else
            if n < 1611
            then
              (if n < 1809
               then
                 (if n < 2036
                  then
                    (if n < 2070
                     then
                       (if n < 2075
                        then (if n < 2084 then 0 else 1)
                        else if n < 2074 then 0 else 1)
                     else if n < 2045 then (if n < 2046 then 0 else 1) else 0)
                  else
                    if n < 1867
                    then
                      (if n < 1969
                       then (if n < 2027 then 1 else 0)
                       else if n < 1958 then 1 else 0)
                    else if n < 1810 then (if n < 1840 then 1 else 0) else 1)
               else
                 if n < 1765
                 then
                   (if n < 1770
                    then
                      (if n < 1807
                       then (if n < 1808 then 0 else 1)
                       else if n < 1774 then 0 else 1)
                    else if n < 1767 then (if n < 1769 then 0 else 1) else 0)
                 else
                   if n < 1649
                   then
                     (if n < 1758
                      then (if n < 1759 then 1 else 0)
                      else if n < 1750 then 1 else 0)
                   else if n < 1632 then (if n < 1648 then 1 else 0) else 1)
            else
              if n < 1471
              then
                (if n < 1480
                 then
                   (if n < 1552
                    then
                      (if n < 1564
                       then (if n < 1565 then 0 else 1)
                       else if n < 1563 then 0 else 1)
                    else if n < 1536 then (if n < 1542 then 0 else 1) else 0)
                 else
                   if n < 1475
                   then
                     (if n < 1478
                      then (if n < 1479 then 1 else 0)
                      else if n < 1476 then 1 else 0)
                   else if n < 1472 then (if n < 1473 then 1 else 0) else 1)
              else
                if n < 174
                then
                  (if n < 1155
                   then
                     (if n < 1425
                      then (if n < 1470 then 0 else 1)
                      else if n < 1162 then 0 else 1)
                   else if n < 768 then (if n < 880 then 0 else 1) else 0)
                else
                  if n < 32
                  then
                    (if n < 160
                     then (if n < 173 then 1 else 0)
                     else if n < 127 then 1 else (-1))
                  else if n < 0 then (if n < 1 then 1 else (-1)) else 0
type grapheme_break_property =
  | Other
  | CR
  | LF
  | Prepend
  | Control
  | Extend
  | SpacingMark
  | L
  | V
  | T
  | LV
  | LVT
  | ZWJ
  | RegionalIndicator
  | ExtPict
let gbp c =
  let n = Uchar.to_int c in
  if n < 47869
  then
    (if n < 54197
     then
       (if n < 71342
        then
          (if n < 121399
           then
             (if n < 127514
              then
                (if n < 129340
                 then
                   (if n < 129632
                    then
                      (if n < 129667
                       then
                         (if n < 917506
                          then
                            (if n < 917632
                             then (if n < 917760 then Other else Extend)
                             else if n < 917536 then Other else Extend)
                          else
                            if n < 129686
                            then (if n < 917505 then Other else Control)
                            else if n < 129680 then Other else ExtPict)
                       else
                         if n < 129652
                         then
                           (if n < 129659
                            then (if n < 129664 then Other else ExtPict)
                            else if n < 129656 then Other else ExtPict)
                         else
                           if n < 129646
                           then (if n < 129648 then Other else ExtPict)
                           else Other)
                    else
                      if n < 129443
                      then
                        (if n < 129454
                         then
                           (if n < 129485
                            then (if n < 129620 then ExtPict else Other)
                            else if n < 129483 then ExtPict else Other)
                         else
                           if n < 129445
                           then (if n < 129451 then ExtPict else Other)
                           else ExtPict)
                      else
                        if n < 129394
                        then
                          (if n < 129399
                           then (if n < 129402 then Other else ExtPict)
                           else if n < 129395 then Other else ExtPict)
                        else
                          if n < 129350
                          then (if n < 129351 then Other else ExtPict)
                          else Other)
                 else
                   if n < 128326
                   then
                     (if n < 128763
                      then
                        (if n < 128992
                         then
                           (if n < 129293
                            then (if n < 129339 then ExtPict else Other)
                            else if n < 129004 then ExtPict else Other)
                         else
                           if n < 128981
                           then (if n < 128985 then ExtPict else Other)
                           else ExtPict)
                      else
                        if n < 128726
                        then
                          (if n < 128749
                           then (if n < 128752 then Other else ExtPict)
                           else if n < 128736 then Other else ExtPict)
                        else
                          if n < 128592
                          then (if n < 128640 then Other else ExtPict)
                          else Other)
                   else
                     if n < 127570
                     then
                       (if n < 127744
                        then
                          (if n < 128000
                           then (if n < 128318 then ExtPict else Other)
                           else if n < 127995 then ExtPict else Extend)
                        else
                          if n < 127584
                          then (if n < 127590 then ExtPict else Other)
                          else ExtPict)
                     else
                       if n < 127536
                       then
                         (if n < 127547
                          then (if n < 127568 then Other else ExtPict)
                          else if n < 127538 then Other else ExtPict)
                       else
                         if n < 127515
                         then (if n < 127535 then Other else ExtPict)
                         else Other)
              else
                if n < 125259
                then
                  (if n < 127280
                   then
                     (if n < 127374
                      then
                        (if n < 127462
                         then
                           (if n < 127489
                            then (if n < 127491 then ExtPict else Other)
                            else if n < 127488 then ExtPict else Other)
                         else
                           if n < 127377
                           then (if n < 127387 then RegionalIndicator
                                 else Other)
                           else if n < 127375 then ExtPict else Other)
                      else
                        if n < 127344
                        then
                          (if n < 127358
                           then (if n < 127360 then ExtPict else Other)
                           else if n < 127346 then ExtPict else Other)
                        else
                          if n < 127340
                          then (if n < 127341 then ExtPict else Other)
                          else ExtPict)
                   else
                     if n < 127153
                     then
                       (if n < 127184
                        then
                          (if n < 127222
                           then (if n < 127279 then Other else ExtPict)
                           else if n < 127185 then Other else ExtPict)
                        else
                          if n < 127168
                          then (if n < 127169 then Other else ExtPict)
                          else Other)
                     else
                       if n < 127024
                       then
                         (if n < 127136
                          then (if n < 127151 then ExtPict else Other)
                          else if n < 127124 then ExtPict else Other)
                       else
                         if n < 126976
                         then (if n < 127020 then ExtPict else Other)
                         else ExtPict)
                else
                  if n < 122905
                  then
                    (if n < 123184
                     then
                       (if n < 123632
                        then
                          (if n < 125143
                           then (if n < 125252 then Other else Extend)
                           else if n < 125136 then Other else Extend)
                        else
                          if n < 123191
                          then (if n < 123628 then Other else Extend)
                          else Other)
                     else
                       if n < 122915
                       then
                         (if n < 122918
                          then (if n < 122923 then Extend else Other)
                          else if n < 122917 then Extend else Other)
                       else
                         if n < 122907
                         then (if n < 122914 then Extend else Other)
                         else Extend)
                  else
                    if n < 121499
                    then
                      (if n < 121520
                       then
                         (if n < 122887
                          then (if n < 122888 then Other else Extend)
                          else if n < 122880 then Other else Extend)
                       else
                         if n < 121504
                         then (if n < 121505 then Other else Extend)
                         else Other)
                    else
                      if n < 121461
                      then
                        (if n < 121476
                         then (if n < 121477 then Extend else Other)
                         else if n < 121462 then Extend else Other)
                      else
                        if n < 121403
                        then (if n < 121453 then Extend else Other)
                        else Extend)
           else
             if n < 72884
             then
               (if n < 92917
                then
                  (if n < 119142
                   then
                     (if n < 119171
                      then
                        (if n < 119214
                         then
                           (if n < 119365
                            then (if n < 121344 then Other else Extend)
                            else if n < 119362 then Other else Extend)
                         else
                           if n < 119180
                           then (if n < 119210 then Other else Extend)
                           else if n < 119173 then Other else Extend)
                      else
                        if n < 119149
                        then
                          (if n < 119155
                           then (if n < 119163 then Other else Extend)
                           else if n < 119150 then Control else Extend)
                        else
                          if n < 119143
                          then (if n < 119146 then SpacingMark else Other)
                          else Extend)
                   else
                     if n < 94095
                     then
                       (if n < 113823
                        then
                          (if n < 113828
                           then (if n < 119141 then SpacingMark else Extend)
                           else if n < 113824 then Other else Control)
                        else
                          if n < 94099
                          then (if n < 113821 then Other else Extend)
                          else Other)
                     else
                       if n < 94031
                       then
                         (if n < 94033
                          then (if n < 94088 then Extend else Other)
                          else if n < 94032 then SpacingMark else Other)
                       else
                         if n < 92976
                         then (if n < 92983 then Extend else Other)
                         else Extend)
                else
                  if n < 73103
                  then
                    (if n < 73112
                     then
                       (if n < 73463
                        then
                          (if n < 78905
                           then (if n < 92912 then Other else Extend)
                           else if n < 78896 then Other else Control)
                        else
                          if n < 73459
                          then (if n < 73461 then Other else SpacingMark)
                          else Extend)
                     else
                       if n < 73107
                       then
                         (if n < 73110
                          then (if n < 73111 then Other else Extend)
                          else if n < 73109 then SpacingMark else Extend)
                       else
                         if n < 73104
                         then (if n < 73106 then SpacingMark else Other)
                         else Extend)
                  else
                    if n < 73020
                    then
                      (if n < 73030
                       then
                         (if n < 73032
                          then (if n < 73098 then Other else SpacingMark)
                          else if n < 73031 then Other else Extend)
                       else
                         if n < 73022
                         then (if n < 73023 then Prepend else Extend)
                         else Other)
                    else
                      if n < 73009
                      then
                        (if n < 73018
                         then (if n < 73019 then Extend else Other)
                         else if n < 73015 then Extend else Other)
                      else
                        if n < 72885
                        then (if n < 72887 then Extend else Other)
                        else Extend)
             else
               if n < 72249
               then
                 (if n < 72346
                  then
                    (if n < 72768
                     then
                       (if n < 72873
                        then
                          (if n < 72881
                           then (if n < 72882 then SpacingMark else Extend)
                           else if n < 72874 then SpacingMark else Extend)
                        else
                          if n < 72850
                          then (if n < 72872 then SpacingMark else Other)
                          else Extend)
                     else
                       if n < 72759
                       then
                         (if n < 72766
                          then (if n < 72767 then Other else Extend)
                          else if n < 72760 then SpacingMark else Extend)
                       else
                         if n < 72751
                         then (if n < 72752 then Other else Extend)
                         else SpacingMark)
                  else
                    if n < 72279
                    then
                      (if n < 72324
                       then
                         (if n < 72343
                          then (if n < 72344 then Other else Extend)
                          else if n < 72330 then SpacingMark else Extend)
                       else
                         if n < 72281
                         then (if n < 72284 then Prepend else Other)
                         else Extend)
                    else
                      if n < 72255
                      then
                        (if n < 72264
                         then (if n < 72273 then SpacingMark else Extend)
                         else if n < 72263 then Other else Extend)
                      else
                        if n < 72250
                        then (if n < 72251 then Other else Extend)
                        else Prepend)
               else
                 if n < 71737
                 then
                   (if n < 72160
                    then
                      (if n < 72165
                       then
                         (if n < 72203
                          then (if n < 72243 then SpacingMark else Extend)
                          else if n < 72193 then Other else Extend)
                       else
                         if n < 72161
                         then (if n < 72164 then Other else SpacingMark)
                         else Other)
                    else
                      if n < 72148
                      then
                        (if n < 72154
                         then (if n < 72156 then Extend else SpacingMark)
                         else if n < 72152 then Extend else Other)
                      else
                        if n < 71739
                        then (if n < 72145 then Extend else SpacingMark)
                        else Other)
                 else
                   if n < 71458
                   then
                     (if n < 71468
                      then
                        (if n < 71727
                         then (if n < 71736 then Extend else SpacingMark)
                         else if n < 71724 then Extend else SpacingMark)
                      else
                        if n < 71462
                        then (if n < 71463 then Other else Extend)
                        else SpacingMark)
                   else
                     if n < 71351
                     then
                       (if n < 71453
                        then (if n < 71456 then Extend else SpacingMark)
                        else if n < 71352 then Extend else Other)
                     else
                       if n < 71344
                       then (if n < 71350 then Extend else SpacingMark)
                       else Extend)
        else
          if n < 69635
          then
            (if n < 70464
             then
               (if n < 70842
                then
                  (if n < 71102
                   then
                     (if n < 71227
                      then
                        (if n < 71233
                         then
                           (if n < 71340
                            then (if n < 71341 then SpacingMark else Extend)
                            else if n < 71339 then SpacingMark else Extend)
                         else
                           if n < 71230
                           then (if n < 71231 then Other else Extend)
                           else if n < 71229 then SpacingMark else Extend)
                      else
                        if n < 71132
                        then
                          (if n < 71216
                           then (if n < 71219 then SpacingMark else Extend)
                           else if n < 71134 then SpacingMark else Other)
                        else
                          if n < 71103
                          then (if n < 71105 then Extend else Other)
                          else Extend)
                   else
                     if n < 70852
                     then
                       (if n < 71090
                        then
                          (if n < 71096
                           then (if n < 71100 then SpacingMark else Extend)
                           else if n < 71094 then SpacingMark else Other)
                        else
                          if n < 71087
                          then (if n < 71088 then Extend else SpacingMark)
                          else Extend)
                     else
                       if n < 70846
                       then
                         (if n < 70849
                          then (if n < 70850 then Other else Extend)
                          else if n < 70847 then SpacingMark else Extend)
                       else
                         if n < 70843
                         then (if n < 70845 then SpacingMark else Extend)
                         else SpacingMark)
                else
                  if n < 70517
                  then
                    (if n < 70727
                     then
                       (if n < 70832
                        then
                          (if n < 70835
                           then (if n < 70841 then Extend else SpacingMark)
                           else if n < 70833 then Extend else SpacingMark)
                        else
                          if n < 70750
                          then (if n < 70751 then Extend else Other)
                          else Extend)
                     else
                       if n < 70720
                       then
                         (if n < 70725
                          then (if n < 70726 then Other else Extend)
                          else if n < 70722 then SpacingMark else Extend)
                       else
                         if n < 70709
                         then (if n < 70712 then SpacingMark else Extend)
                         else SpacingMark)
                  else
                    if n < 70487
                    then
                      (if n < 70500
                       then
                         (if n < 70509
                          then (if n < 70512 then Other else Extend)
                          else if n < 70502 then Other else Extend)
                       else
                         if n < 70488
                         then (if n < 70498 then Other else SpacingMark)
                         else Other)
                    else
                      if n < 70471
                      then
                        (if n < 70475
                         then (if n < 70478 then Extend else Other)
                         else if n < 70473 then SpacingMark else Other)
                      else
                        if n < 70465
                        then (if n < 70469 then SpacingMark else Other)
                        else SpacingMark)
             else
               if n < 70067
               then
                 (if n < 70200
                  then
                    (if n < 70400
                     then
                       (if n < 70459
                        then
                          (if n < 70462
                           then (if n < 70463 then Extend else SpacingMark)
                           else if n < 70461 then Extend else Other)
                        else
                          if n < 70402
                          then (if n < 70404 then Extend else Other)
                          else SpacingMark)
                     else
                       if n < 70367
                       then
                         (if n < 70371
                          then (if n < 70379 then Extend else Other)
                          else if n < 70368 then Extend else SpacingMark)
                       else
                         if n < 70206
                         then (if n < 70207 then Extend else Other)
                         else Extend)
                  else
                    if n < 70093
                    then
                      (if n < 70194
                       then
                         (if n < 70197
                          then (if n < 70198 then Other else Extend)
                          else if n < 70196 then SpacingMark else Extend)
                       else
                         if n < 70188
                         then (if n < 70191 then SpacingMark else Extend)
                         else SpacingMark)
                    else
                      if n < 70081
                      then
                        (if n < 70084
                         then (if n < 70089 then Other else Extend)
                         else if n < 70082 then Other else Prepend)
                      else
                        if n < 70070
                        then (if n < 70079 then Other else SpacingMark)
                        else Extend)
               else
                 if n < 69838
                 then
                   (if n < 69957
                    then
                      (if n < 70004
                       then
                         (if n < 70018
                          then (if n < 70019 then SpacingMark else Other)
                          else if n < 70016 then SpacingMark else Extend)
                       else
                         if n < 69959
                         then (if n < 70003 then Other else Extend)
                         else Other)
                    else
                      if n < 69927
                      then
                        (if n < 69933
                         then (if n < 69941 then SpacingMark else Other)
                         else if n < 69932 then Extend else SpacingMark)
                      else
                        if n < 69888
                        then (if n < 69891 then Extend else Other)
                        else Extend)
                 else
                   if n < 69811
                   then
                     (if n < 69819
                      then
                        (if n < 69822
                         then (if n < 69837 then Other else Prepend)
                         else if n < 69821 then Other else Prepend)
                      else
                        if n < 69815
                        then (if n < 69817 then Other else Extend)
                        else SpacingMark)
                   else
                     if n < 69759
                     then
                       (if n < 69763
                        then (if n < 69808 then Extend else SpacingMark)
                        else if n < 69762 then Other else SpacingMark)
                     else
                       if n < 69688
                       then (if n < 69703 then Extend else Other)
                       else Extend)
          else
            if n < 54981
            then
              (if n < 65438
               then
                 (if n < 68108
                  then
                    (if n < 68327
                     then
                       (if n < 69457
                        then
                          (if n < 69633
                           then (if n < 69634 then Other else SpacingMark)
                           else if n < 69632 then Extend else SpacingMark)
                        else
                          if n < 68904
                          then (if n < 69446 then Other else Extend)
                          else if n < 68900 then Other else Extend)
                     else
                       if n < 68155
                       then
                         (if n < 68160
                          then (if n < 68325 then Other else Extend)
                          else if n < 68159 then Other else Extend)
                       else
                         if n < 68112
                         then (if n < 68152 then Other else Extend)
                         else Other)
                  else
                    if n < 66273
                    then
                      (if n < 68097
                       then
                         (if n < 68101
                          then (if n < 68103 then Extend else Other)
                          else if n < 68100 then Extend else Other)
                       else
                         if n < 66422
                         then (if n < 66427 then Extend else Other)
                         else Extend)
                    else
                      if n < 65532
                      then
                        (if n < 66046
                         then (if n < 66272 then Other else Extend)
                         else if n < 66045 then Other else Extend)
                      else
                        if n < 65440
                        then (if n < 65529 then Other else Control)
                        else Other)
               else
                 if n < 55177
                 then
                   (if n < 64287
                    then
                      (if n < 65056
                       then
                         (if n < 65279
                          then (if n < 65280 then Extend else Other)
                          else if n < 65072 then Control else Other)
                       else
                         if n < 65024
                         then (if n < 65040 then Extend else Other)
                         else Extend)
                    else
                      if n < 55239
                      then
                        (if n < 55292
                         then (if n < 64286 then Other else Extend)
                         else if n < 55243 then Other else T)
                      else
                        if n < 55204
                        then (if n < 55216 then Other else V)
                        else Other)
                 else
                   if n < 55092
                   then
                     (if n < 55121
                      then
                        (if n < 55149
                         then (if n < 55176 then LVT else LV)
                         else if n < 55148 then LVT else LV)
                      else
                        if n < 55093
                        then (if n < 55120 then LVT else LV)
                        else LVT)
                   else
                     if n < 55036
                     then
                       (if n < 55064
                        then (if n < 55065 then LV else LVT)
                        else if n < 55037 then LV else LVT)
                     else
                       if n < 55008
                       then (if n < 55009 then LV else LVT)
                       else LV)
            else
              if n < 54589
              then
                (if n < 54785
                 then
                   (if n < 54896
                    then
                      (if n < 54925
                       then
                         (if n < 54953
                          then (if n < 54980 then LVT else LV)
                          else if n < 54952 then LVT else LV)
                       else
                         if n < 54897
                         then (if n < 54924 then LVT else LV)
                         else LVT)
                    else
                      if n < 54840
                      then
                        (if n < 54868
                         then (if n < 54869 then LV else LVT)
                         else if n < 54841 then LV else LVT)
                      else
                        if n < 54812
                        then (if n < 54813 then LV else LVT)
                        else LV)
                 else
                   if n < 54700
                   then
                     (if n < 54729
                      then
                        (if n < 54757
                         then (if n < 54784 then LVT else LV)
                         else if n < 54756 then LVT else LV)
                      else
                        if n < 54701
                        then (if n < 54728 then LVT else LV)
                        else LVT)
                   else
                     if n < 54644
                     then
                       (if n < 54672
                        then (if n < 54673 then LV else LVT)
                        else if n < 54645 then LV else LVT)
                     else
                       if n < 54616
                       then (if n < 54617 then LV else LVT)
                       else LV)
              else
                if n < 54393
                then
                  (if n < 54504
                   then
                     (if n < 54533
                      then
                        (if n < 54561
                         then (if n < 54588 then LVT else LV)
                         else if n < 54560 then LVT else LV)
                      else
                        if n < 54505
                        then (if n < 54532 then LVT else LV)
                        else LVT)
                   else
                     if n < 54448
                     then
                       (if n < 54476
                        then (if n < 54477 then LV else LVT)
                        else if n < 54449 then LV else LVT)
                     else
                       if n < 54420
                       then (if n < 54421 then LV else LVT)
                       else LV)
                else
                  if n < 54308
                  then
                    (if n < 54337
                     then
                       (if n < 54365
                        then (if n < 54392 then LVT else LV)
                        else if n < 54364 then LVT else LV)
                     else
                       if n < 54309
                       then (if n < 54336 then LVT else LV)
                       else LVT)
                  else
                    if n < 54252
                    then
                      (if n < 54280
                       then (if n < 54281 then LV else LVT)
                       else if n < 54253 then LV else LVT)
                    else
                      if n < 54224
                      then (if n < 54225 then LV else LVT)
                      else LV)
     else
       if n < 51033
       then
         (if n < 52628
          then
            (if n < 53412
             then
               (if n < 53804
                then
                  (if n < 54000
                   then
                     (if n < 54085
                      then
                        (if n < 54141
                         then
                           (if n < 54169
                            then (if n < 54196 then LVT else LV)
                            else if n < 54168 then LVT else LV)
                         else
                           if n < 54113
                           then (if n < 54140 then LVT else LV)
                           else if n < 54112 then LVT else LV)
                      else
                        if n < 54029
                        then
                          (if n < 54057
                           then (if n < 54084 then LVT else LV)
                           else if n < 54056 then LVT else LV)
                        else
                          if n < 54001
                          then (if n < 54028 then LVT else LV)
                          else LVT)
                   else
                     if n < 53889
                     then
                       (if n < 53944
                        then
                          (if n < 53972
                           then (if n < 53973 then LV else LVT)
                           else if n < 53945 then LV else LVT)
                        else
                          if n < 53916
                          then (if n < 53917 then LV else LVT)
                          else LV)
                     else
                       if n < 53833
                       then
                         (if n < 53861
                          then (if n < 53888 then LVT else LV)
                          else if n < 53860 then LVT else LV)
                       else
                         if n < 53805
                         then (if n < 53832 then LVT else LV)
                         else LVT)
                else
                  if n < 53608
                  then
                    (if n < 53693
                     then
                       (if n < 53748
                        then
                          (if n < 53776
                           then (if n < 53777 then LV else LVT)
                           else if n < 53749 then LV else LVT)
                        else
                          if n < 53720
                          then (if n < 53721 then LV else LVT)
                          else LV)
                     else
                       if n < 53637
                       then
                         (if n < 53665
                          then (if n < 53692 then LVT else LV)
                          else if n < 53664 then LVT else LV)
                       else
                         if n < 53609
                         then (if n < 53636 then LVT else LV)
                         else LVT)
                  else
                    if n < 53497
                    then
                      (if n < 53552
                       then
                         (if n < 53580
                          then (if n < 53581 then LV else LVT)
                          else if n < 53553 then LV else LVT)
                       else
                         if n < 53524
                         then (if n < 53525 then LV else LVT)
                         else LV)
                    else
                      if n < 53441
                      then
                        (if n < 53469
                         then (if n < 53496 then LVT else LV)
                         else if n < 53468 then LVT else LV)
                      else
                        if n < 53413
                        then (if n < 53440 then LVT else LV)
                        else LVT)
             else
               if n < 53020
               then
                 (if n < 53216
                  then
                    (if n < 53301
                     then
                       (if n < 53356
                        then
                          (if n < 53384
                           then (if n < 53385 then LV else LVT)
                           else if n < 53357 then LV else LVT)
                        else
                          if n < 53328
                          then (if n < 53329 then LV else LVT)
                          else LV)
                     else
                       if n < 53245
                       then
                         (if n < 53273
                          then (if n < 53300 then LVT else LV)
                          else if n < 53272 then LVT else LV)
                       else
                         if n < 53217
                         then (if n < 53244 then LVT else LV)
                         else LVT)
                  else
                    if n < 53105
                    then
                      (if n < 53160
                       then
                         (if n < 53188
                          then (if n < 53189 then LV else LVT)
                          else if n < 53161 then LV else LVT)
                       else
                         if n < 53132
                         then (if n < 53133 then LV else LVT)
                         else LV)
                    else
                      if n < 53049
                      then
                        (if n < 53077
                         then (if n < 53104 then LVT else LV)
                         else if n < 53076 then LVT else LV)
                      else
                        if n < 53021
                        then (if n < 53048 then LVT else LV)
                        else LVT)
               else
                 if n < 52824
                 then
                   (if n < 52909
                    then
                      (if n < 52964
                       then
                         (if n < 52992
                          then (if n < 52993 then LV else LVT)
                          else if n < 52965 then LV else LVT)
                       else
                         if n < 52936
                         then (if n < 52937 then LV else LVT)
                         else LV)
                    else
                      if n < 52853
                      then
                        (if n < 52881
                         then (if n < 52908 then LVT else LV)
                         else if n < 52880 then LVT else LV)
                      else
                        if n < 52825
                        then (if n < 52852 then LVT else LV)
                        else LVT)
                 else
                   if n < 52713
                   then
                     (if n < 52768
                      then
                        (if n < 52796
                         then (if n < 52797 then LV else LVT)
                         else if n < 52769 then LV else LVT)
                      else
                        if n < 52740
                        then (if n < 52741 then LV else LVT)
                        else LV)
                   else
                     if n < 52657
                     then
                       (if n < 52685
                        then (if n < 52712 then LVT else LV)
                        else if n < 52684 then LVT else LV)
                     else
                       if n < 52629
                       then (if n < 52656 then LVT else LV)
                       else LVT)
          else
            if n < 51817
            then
              (if n < 52209
               then
                 (if n < 52405
                  then
                    (if n < 52516
                     then
                       (if n < 52572
                        then
                          (if n < 52600
                           then (if n < 52601 then LV else LVT)
                           else if n < 52573 then LV else LVT)
                        else
                          if n < 52544
                          then (if n < 52545 then LV else LVT)
                          else if n < 52517 then LV else LVT)
                     else
                       if n < 52460
                       then
                         (if n < 52488
                          then (if n < 52489 then LV else LVT)
                          else if n < 52461 then LV else LVT)
                       else
                         if n < 52432
                         then (if n < 52433 then LV else LVT)
                         else LV)
                  else
                    if n < 52320
                    then
                      (if n < 52349
                       then
                         (if n < 52377
                          then (if n < 52404 then LVT else LV)
                          else if n < 52376 then LVT else LV)
                       else
                         if n < 52321
                         then (if n < 52348 then LVT else LV)
                         else LVT)
                    else
                      if n < 52264
                      then
                        (if n < 52292
                         then (if n < 52293 then LV else LVT)
                         else if n < 52265 then LV else LVT)
                      else
                        if n < 52236
                        then (if n < 52237 then LV else LVT)
                        else LV)
               else
                 if n < 52013
                 then
                   (if n < 52124
                    then
                      (if n < 52153
                       then
                         (if n < 52181
                          then (if n < 52208 then LVT else LV)
                          else if n < 52180 then LVT else LV)
                       else
                         if n < 52125
                         then (if n < 52152 then LVT else LV)
                         else LVT)
                    else
                      if n < 52068
                      then
                        (if n < 52096
                         then (if n < 52097 then LV else LVT)
                         else if n < 52069 then LV else LVT)
                      else
                        if n < 52040
                        then (if n < 52041 then LV else LVT)
                        else LV)
                 else
                   if n < 51928
                   then
                     (if n < 51957
                      then
                        (if n < 51985
                         then (if n < 52012 then LVT else LV)
                         else if n < 51984 then LVT else LV)
                      else
                        if n < 51929
                        then (if n < 51956 then LVT else LV)
                        else LVT)
                   else
                     if n < 51872
                     then
                       (if n < 51900
                        then (if n < 51901 then LV else LVT)
                        else if n < 51873 then LV else LVT)
                     else
                       if n < 51844
                       then (if n < 51845 then LV else LVT)
                       else LV)
            else
              if n < 51425
              then
                (if n < 51621
                 then
                   (if n < 51732
                    then
                      (if n < 51761
                       then
                         (if n < 51789
                          then (if n < 51816 then LVT else LV)
                          else if n < 51788 then LVT else LV)
                       else
                         if n < 51733
                         then (if n < 51760 then LVT else LV)
                         else LVT)
                    else
                      if n < 51676
                      then
                        (if n < 51704
                         then (if n < 51705 then LV else LVT)
                         else if n < 51677 then LV else LVT)
                      else
                        if n < 51648
                        then (if n < 51649 then LV else LVT)
                        else LV)
                 else
                   if n < 51536
                   then
                     (if n < 51565
                      then
                        (if n < 51593
                         then (if n < 51620 then LVT else LV)
                         else if n < 51592 then LVT else LV)
                      else
                        if n < 51537
                        then (if n < 51564 then LVT else LV)
                        else LVT)
                   else
                     if n < 51480
                     then
                       (if n < 51508
                        then (if n < 51509 then LV else LVT)
                        else if n < 51481 then LV else LVT)
                     else
                       if n < 51452
                       then (if n < 51453 then LV else LVT)
                       else LV)
              else
                if n < 51229
                then
                  (if n < 51340
                   then
                     (if n < 51369
                      then
                        (if n < 51397
                         then (if n < 51424 then LVT else LV)
                         else if n < 51396 then LVT else LV)
                      else
                        if n < 51341
                        then (if n < 51368 then LVT else LV)
                        else LVT)
                   else
                     if n < 51284
                     then
                       (if n < 51312
                        then (if n < 51313 then LV else LVT)
                        else if n < 51285 then LV else LVT)
                     else
                       if n < 51256
                       then (if n < 51257 then LV else LVT)
                       else LV)
                else
                  if n < 51144
                  then
                    (if n < 51173
                     then
                       (if n < 51201
                        then (if n < 51228 then LVT else LV)
                        else if n < 51200 then LVT else LV)
                     else
                       if n < 51145
                       then (if n < 51172 then LVT else LV)
                       else LVT)
                  else
                    if n < 51088
                    then
                      (if n < 51116
                       then (if n < 51117 then LV else LVT)
                       else if n < 51089 then LV else LVT)
                    else
                      if n < 51060
                      then (if n < 51061 then LV else LVT)
                      else LV)
       else
         if n < 49464
         then
           (if n < 50248
            then
              (if n < 50640
               then
                 (if n < 50836
                  then
                    (if n < 50921
                     then
                       (if n < 50977
                        then
                          (if n < 51005
                           then (if n < 51032 then LVT else LV)
                           else if n < 51004 then LVT else LV)
                        else
                          if n < 50949
                          then (if n < 50976 then LVT else LV)
                          else if n < 50948 then LVT else LV)
                     else
                       if n < 50865
                       then
                         (if n < 50893
                          then (if n < 50920 then LVT else LV)
                          else if n < 50892 then LVT else LV)
                       else
                         if n < 50837
                         then (if n < 50864 then LVT else LV)
                         else LVT)
                  else
                    if n < 50725
                    then
                      (if n < 50780
                       then
                         (if n < 50808
                          then (if n < 50809 then LV else LVT)
                          else if n < 50781 then LV else LVT)
                       else
                         if n < 50752
                         then (if n < 50753 then LV else LVT)
                         else LV)
                    else
                      if n < 50669
                      then
                        (if n < 50697
                         then (if n < 50724 then LVT else LV)
                         else if n < 50696 then LVT else LV)
                      else
                        if n < 50641
                        then (if n < 50668 then LVT else LV)
                        else LVT)
               else
                 if n < 50444
                 then
                   (if n < 50529
                    then
                      (if n < 50584
                       then
                         (if n < 50612
                          then (if n < 50613 then LV else LVT)
                          else if n < 50585 then LV else LVT)
                       else
                         if n < 50556
                         then (if n < 50557 then LV else LVT)
                         else LV)
                    else
                      if n < 50473
                      then
                        (if n < 50501
                         then (if n < 50528 then LVT else LV)
                         else if n < 50500 then LVT else LV)
                      else
                        if n < 50445
                        then (if n < 50472 then LVT else LV)
                        else LVT)
                 else
                   if n < 50333
                   then
                     (if n < 50388
                      then
                        (if n < 50416
                         then (if n < 50417 then LV else LVT)
                         else if n < 50389 then LV else LVT)
                      else
                        if n < 50360
                        then (if n < 50361 then LV else LVT)
                        else LV)
                   else
                     if n < 50277
                     then
                       (if n < 50305
                        then (if n < 50332 then LVT else LV)
                        else if n < 50304 then LVT else LV)
                     else
                       if n < 50249
                       then (if n < 50276 then LVT else LV)
                       else LVT)
            else
              if n < 49856
              then
                (if n < 50052
                 then
                   (if n < 50137
                    then
                      (if n < 50192
                       then
                         (if n < 50220
                          then (if n < 50221 then LV else LVT)
                          else if n < 50193 then LV else LVT)
                       else
                         if n < 50164
                         then (if n < 50165 then LV else LVT)
                         else LV)
                    else
                      if n < 50081
                      then
                        (if n < 50109
                         then (if n < 50136 then LVT else LV)
                         else if n < 50108 then LVT else LV)
                      else
                        if n < 50053
                        then (if n < 50080 then LVT else LV)
                        else LVT)
                 else
                   if n < 49941
                   then
                     (if n < 49996
                      then
                        (if n < 50024
                         then (if n < 50025 then LV else LVT)
                         else if n < 49997 then LV else LVT)
                      else
                        if n < 49968
                        then (if n < 49969 then LV else LVT)
                        else LV)
                   else
                     if n < 49885
                     then
                       (if n < 49913
                        then (if n < 49940 then LVT else LV)
                        else if n < 49912 then LVT else LV)
                     else
                       if n < 49857
                       then (if n < 49884 then LVT else LV)
                       else LVT)
              else
                if n < 49660
                then
                  (if n < 49745
                   then
                     (if n < 49800
                      then
                        (if n < 49828
                         then (if n < 49829 then LV else LVT)
                         else if n < 49801 then LV else LVT)
                      else
                        if n < 49772
                        then (if n < 49773 then LV else LVT)
                        else LV)
                   else
                     if n < 49689
                     then
                       (if n < 49717
                        then (if n < 49744 then LVT else LV)
                        else if n < 49716 then LVT else LV)
                     else
                       if n < 49661
                       then (if n < 49688 then LVT else LV)
                       else LVT)
                else
                  if n < 49549
                  then
                    (if n < 49604
                     then
                       (if n < 49632
                        then (if n < 49633 then LV else LVT)
                        else if n < 49605 then LV else LVT)
                     else
                       if n < 49576
                       then (if n < 49577 then LV else LVT)
                       else LV)
                  else
                    if n < 49493
                    then
                      (if n < 49521
                       then (if n < 49548 then LVT else LV)
                       else if n < 49520 then LVT else LV)
                    else
                      if n < 49465
                      then (if n < 49492 then LVT else LV)
                      else LVT)
         else
           if n < 48653
           then
             (if n < 49045
              then
                (if n < 49241
                 then
                   (if n < 49352
                    then
                      (if n < 49408
                       then
                         (if n < 49436
                          then (if n < 49437 then LV else LVT)
                          else if n < 49409 then LV else LVT)
                       else
                         if n < 49380
                         then (if n < 49381 then LV else LVT)
                         else if n < 49353 then LV else LVT)
                    else
                      if n < 49296
                      then
                        (if n < 49324
                         then (if n < 49325 then LV else LVT)
                         else if n < 49297 then LV else LVT)
                      else
                        if n < 49268
                        then (if n < 49269 then LV else LVT)
                        else LV)
                 else
                   if n < 49156
                   then
                     (if n < 49185
                      then
                        (if n < 49213
                         then (if n < 49240 then LVT else LV)
                         else if n < 49212 then LVT else LV)
                      else
                        if n < 49157
                        then (if n < 49184 then LVT else LV)
                        else LVT)
                   else
                     if n < 49100
                     then
                       (if n < 49128
                        then (if n < 49129 then LV else LVT)
                        else if n < 49101 then LV else LVT)
                     else
                       if n < 49072
                       then (if n < 49073 then LV else LVT)
                       else LV)
              else
                if n < 48849
                then
                  (if n < 48960
                   then
                     (if n < 48989
                      then
                        (if n < 49017
                         then (if n < 49044 then LVT else LV)
                         else if n < 49016 then LVT else LV)
                      else
                        if n < 48961
                        then (if n < 48988 then LVT else LV)
                        else LVT)
                   else
                     if n < 48904
                     then
                       (if n < 48932
                        then (if n < 48933 then LV else LVT)
                        else if n < 48905 then LV else LVT)
                     else
                       if n < 48876
                       then (if n < 48877 then LV else LVT)
                       else LV)
                else
                  if n < 48764
                  then
                    (if n < 48793
                     then
                       (if n < 48821
                        then (if n < 48848 then LVT else LV)
                        else if n < 48820 then LVT else LV)
                     else
                       if n < 48765
                       then (if n < 48792 then LVT else LV)
                       else LVT)
                  else
                    if n < 48708
                    then
                      (if n < 48736
                       then (if n < 48737 then LV else LVT)
                       else if n < 48709 then LV else LVT)
                    else
                      if n < 48680
                      then (if n < 48681 then LV else LVT)
                      else LV)
           else
             if n < 48261
             then
               (if n < 48457
                then
                  (if n < 48568
                   then
                     (if n < 48597
                      then
                        (if n < 48625
                         then (if n < 48652 then LVT else LV)
                         else if n < 48624 then LVT else LV)
                      else
                        if n < 48569
                        then (if n < 48596 then LVT else LV)
                        else LVT)
                   else
                     if n < 48512
                     then
                       (if n < 48540
                        then (if n < 48541 then LV else LVT)
                        else if n < 48513 then LV else LVT)
                     else
                       if n < 48484
                       then (if n < 48485 then LV else LVT)
                       else LV)
                else
                  if n < 48372
                  then
                    (if n < 48401
                     then
                       (if n < 48429
                        then (if n < 48456 then LVT else LV)
                        else if n < 48428 then LVT else LV)
                     else
                       if n < 48373
                       then (if n < 48400 then LVT else LV)
                       else LVT)
                  else
                    if n < 48316
                    then
                      (if n < 48344
                       then (if n < 48345 then LV else LVT)
                       else if n < 48317 then LV else LVT)
                    else
                      if n < 48288
                      then (if n < 48289 then LV else LVT)
                      else LV)
             else
               if n < 48065
               then
                 (if n < 48176
                  then
                    (if n < 48205
                     then
                       (if n < 48233
                        then (if n < 48260 then LVT else LV)
                        else if n < 48232 then LVT else LV)
                     else
                       if n < 48177
                       then (if n < 48204 then LVT else LV)
                       else LVT)
                  else
                    if n < 48120
                    then
                      (if n < 48148
                       then (if n < 48149 then LV else LVT)
                       else if n < 48121 then LV else LVT)
                    else
                      if n < 48092
                      then (if n < 48093 then LV else LVT)
                      else LV)
               else
                 if n < 47980
                 then
                   (if n < 48009
                    then
                      (if n < 48037
                       then (if n < 48064 then LVT else LV)
                       else if n < 48036 then LVT else LV)
                    else
                      if n < 47981
                      then (if n < 48008 then LVT else LV)
                      else LVT)
                 else
                   if n < 47924
                   then
                     (if n < 47952
                      then (if n < 47953 then LV else LVT)
                      else if n < 47925 then LV else LVT)
                   else
                     if n < 47896
                     then (if n < 47897 then LV else LVT)
                     else LV)
  else
    if n < 8986
    then
      (if n < 44704
       then
         (if n < 46273
          then
            (if n < 47084
             then
               (if n < 47476
                then
                  (if n < 47672
                   then
                     (if n < 47757
                      then
                        (if n < 47813
                         then
                           (if n < 47841
                            then (if n < 47868 then LVT else LV)
                            else if n < 47840 then LVT else LV)
                         else
                           if n < 47785
                           then (if n < 47812 then LVT else LV)
                           else if n < 47784 then LVT else LV)
                      else
                        if n < 47701
                        then
                          (if n < 47729
                           then (if n < 47756 then LVT else LV)
                           else if n < 47728 then LVT else LV)
                        else
                          if n < 47673
                          then (if n < 47700 then LVT else LV)
                          else LVT)
                   else
                     if n < 47561
                     then
                       (if n < 47616
                        then
                          (if n < 47644
                           then (if n < 47645 then LV else LVT)
                           else if n < 47617 then LV else LVT)
                        else
                          if n < 47588
                          then (if n < 47589 then LV else LVT)
                          else LV)
                     else
                       if n < 47505
                       then
                         (if n < 47533
                          then (if n < 47560 then LVT else LV)
                          else if n < 47532 then LVT else LV)
                       else
                         if n < 47477
                         then (if n < 47504 then LVT else LV)
                         else LVT)
                else
                  if n < 47280
                  then
                    (if n < 47365
                     then
                       (if n < 47420
                        then
                          (if n < 47448
                           then (if n < 47449 then LV else LVT)
                           else if n < 47421 then LV else LVT)
                        else
                          if n < 47392
                          then (if n < 47393 then LV else LVT)
                          else LV)
                     else
                       if n < 47309
                       then
                         (if n < 47337
                          then (if n < 47364 then LVT else LV)
                          else if n < 47336 then LVT else LV)
                       else
                         if n < 47281
                         then (if n < 47308 then LVT else LV)
                         else LVT)
                  else
                    if n < 47169
                    then
                      (if n < 47224
                       then
                         (if n < 47252
                          then (if n < 47253 then LV else LVT)
                          else if n < 47225 then LV else LVT)
                       else
                         if n < 47196
                         then (if n < 47197 then LV else LVT)
                         else LV)
                    else
                      if n < 47113
                      then
                        (if n < 47141
                         then (if n < 47168 then LVT else LV)
                         else if n < 47140 then LVT else LV)
                      else
                        if n < 47085
                        then (if n < 47112 then LVT else LV)
                        else LVT)
             else
               if n < 46665
               then
                 (if n < 46861
                  then
                    (if n < 46972
                     then
                       (if n < 47028
                        then
                          (if n < 47056
                           then (if n < 47057 then LV else LVT)
                           else if n < 47029 then LV else LVT)
                        else
                          if n < 47000
                          then (if n < 47001 then LV else LVT)
                          else if n < 46973 then LV else LVT)
                     else
                       if n < 46916
                       then
                         (if n < 46944
                          then (if n < 46945 then LV else LVT)
                          else if n < 46917 then LV else LVT)
                       else
                         if n < 46888
                         then (if n < 46889 then LV else LVT)
                         else LV)
                  else
                    if n < 46776
                    then
                      (if n < 46805
                       then
                         (if n < 46833
                          then (if n < 46860 then LVT else LV)
                          else if n < 46832 then LVT else LV)
                       else
                         if n < 46777
                         then (if n < 46804 then LVT else LV)
                         else LVT)
                    else
                      if n < 46720
                      then
                        (if n < 46748
                         then (if n < 46749 then LV else LVT)
                         else if n < 46721 then LV else LVT)
                      else
                        if n < 46692
                        then (if n < 46693 then LV else LVT)
                        else LV)
               else
                 if n < 46469
                 then
                   (if n < 46580
                    then
                      (if n < 46609
                       then
                         (if n < 46637
                          then (if n < 46664 then LVT else LV)
                          else if n < 46636 then LVT else LV)
                       else
                         if n < 46581
                         then (if n < 46608 then LVT else LV)
                         else LVT)
                    else
                      if n < 46524
                      then
                        (if n < 46552
                         then (if n < 46553 then LV else LVT)
                         else if n < 46525 then LV else LVT)
                      else
                        if n < 46496
                        then (if n < 46497 then LV else LVT)
                        else LV)
                 else
                   if n < 46384
                   then
                     (if n < 46413
                      then
                        (if n < 46441
                         then (if n < 46468 then LVT else LV)
                         else if n < 46440 then LVT else LV)
                      else
                        if n < 46385
                        then (if n < 46412 then LVT else LV)
                        else LVT)
                   else
                     if n < 46328
                     then
                       (if n < 46356
                        then (if n < 46357 then LV else LVT)
                        else if n < 46329 then LV else LVT)
                     else
                       if n < 46300
                       then (if n < 46301 then LV else LVT)
                       else LV)
          else
            if n < 45488
            then
              (if n < 45880
               then
                 (if n < 46076
                  then
                    (if n < 46161
                     then
                       (if n < 46217
                        then
                          (if n < 46245
                           then (if n < 46272 then LVT else LV)
                           else if n < 46244 then LVT else LV)
                        else
                          if n < 46189
                          then (if n < 46216 then LVT else LV)
                          else if n < 46188 then LVT else LV)
                     else
                       if n < 46105
                       then
                         (if n < 46133
                          then (if n < 46160 then LVT else LV)
                          else if n < 46132 then LVT else LV)
                       else
                         if n < 46077
                         then (if n < 46104 then LVT else LV)
                         else LVT)
                  else
                    if n < 45965
                    then
                      (if n < 46020
                       then
                         (if n < 46048
                          then (if n < 46049 then LV else LVT)
                          else if n < 46021 then LV else LVT)
                       else
                         if n < 45992
                         then (if n < 45993 then LV else LVT)
                         else LV)
                    else
                      if n < 45909
                      then
                        (if n < 45937
                         then (if n < 45964 then LVT else LV)
                         else if n < 45936 then LVT else LV)
                      else
                        if n < 45881
                        then (if n < 45908 then LVT else LV)
                        else LVT)
               else
                 if n < 45684
                 then
                   (if n < 45769
                    then
                      (if n < 45824
                       then
                         (if n < 45852
                          then (if n < 45853 then LV else LVT)
                          else if n < 45825 then LV else LVT)
                       else
                         if n < 45796
                         then (if n < 45797 then LV else LVT)
                         else LV)
                    else
                      if n < 45713
                      then
                        (if n < 45741
                         then (if n < 45768 then LVT else LV)
                         else if n < 45740 then LVT else LV)
                      else
                        if n < 45685
                        then (if n < 45712 then LVT else LV)
                        else LVT)
                 else
                   if n < 45573
                   then
                     (if n < 45628
                      then
                        (if n < 45656
                         then (if n < 45657 then LV else LVT)
                         else if n < 45629 then LV else LVT)
                      else
                        if n < 45600
                        then (if n < 45601 then LV else LVT)
                        else LV)
                   else
                     if n < 45517
                     then
                       (if n < 45545
                        then (if n < 45572 then LVT else LV)
                        else if n < 45544 then LVT else LV)
                     else
                       if n < 45489
                       then (if n < 45516 then LVT else LV)
                       else LVT)
            else
              if n < 45096
              then
                (if n < 45292
                 then
                   (if n < 45377
                    then
                      (if n < 45432
                       then
                         (if n < 45460
                          then (if n < 45461 then LV else LVT)
                          else if n < 45433 then LV else LVT)
                       else
                         if n < 45404
                         then (if n < 45405 then LV else LVT)
                         else LV)
                    else
                      if n < 45321
                      then
                        (if n < 45349
                         then (if n < 45376 then LVT else LV)
                         else if n < 45348 then LVT else LV)
                      else
                        if n < 45293
                        then (if n < 45320 then LVT else LV)
                        else LVT)
                 else
                   if n < 45181
                   then
                     (if n < 45236
                      then
                        (if n < 45264
                         then (if n < 45265 then LV else LVT)
                         else if n < 45237 then LV else LVT)
                      else
                        if n < 45208
                        then (if n < 45209 then LV else LVT)
                        else LV)
                   else
                     if n < 45125
                     then
                       (if n < 45153
                        then (if n < 45180 then LVT else LV)
                        else if n < 45152 then LVT else LV)
                     else
                       if n < 45097
                       then (if n < 45124 then LVT else LV)
                       else LVT)
              else
                if n < 44900
                then
                  (if n < 44985
                   then
                     (if n < 45040
                      then
                        (if n < 45068
                         then (if n < 45069 then LV else LVT)
                         else if n < 45041 then LV else LVT)
                      else
                        if n < 45012
                        then (if n < 45013 then LV else LVT)
                        else LV)
                   else
                     if n < 44929
                     then
                       (if n < 44957
                        then (if n < 44984 then LVT else LV)
                        else if n < 44956 then LVT else LV)
                     else
                       if n < 44901
                       then (if n < 44928 then LVT else LV)
                       else LVT)
                else
                  if n < 44789
                  then
                    (if n < 44844
                     then
                       (if n < 44872
                        then (if n < 44873 then LV else LVT)
                        else if n < 44845 then LV else LVT)
                     else
                       if n < 44816
                       then (if n < 44817 then LV else LVT)
                       else LV)
                  else
                    if n < 44733
                    then
                      (if n < 44761
                       then (if n < 44788 then LVT else LV)
                       else if n < 44760 then LVT else LV)
                    else
                      if n < 44705
                      then (if n < 44732 then LVT else LV)
                      else LVT)
       else
         if n < 43188
         then
           (if n < 44003
            then
              (if n < 44285
               then
                 (if n < 44481
                  then
                    (if n < 44592
                     then
                       (if n < 44648
                        then
                          (if n < 44676
                           then (if n < 44677 then LV else LVT)
                           else if n < 44649 then LV else LVT)
                        else
                          if n < 44620
                          then (if n < 44621 then LV else LVT)
                          else if n < 44593 then LV else LVT)
                     else
                       if n < 44536
                       then
                         (if n < 44564
                          then (if n < 44565 then LV else LVT)
                          else if n < 44537 then LV else LVT)
                       else
                         if n < 44508
                         then (if n < 44509 then LV else LVT)
                         else LV)
                  else
                    if n < 44396
                    then
                      (if n < 44425
                       then
                         (if n < 44453
                          then (if n < 44480 then LVT else LV)
                          else if n < 44452 then LVT else LV)
                       else
                         if n < 44397
                         then (if n < 44424 then LVT else LV)
                         else LVT)
                    else
                      if n < 44340
                      then
                        (if n < 44368
                         then (if n < 44369 then LV else LVT)
                         else if n < 44341 then LV else LVT)
                      else
                        if n < 44312
                        then (if n < 44313 then LV else LVT)
                        else LV)
               else
                 if n < 44089
                 then
                   (if n < 44200
                    then
                      (if n < 44229
                       then
                         (if n < 44257
                          then (if n < 44284 then LVT else LV)
                          else if n < 44256 then LVT else LV)
                       else
                         if n < 44201
                         then (if n < 44228 then LVT else LV)
                         else LVT)
                    else
                      if n < 44144
                      then
                        (if n < 44172
                         then (if n < 44173 then LV else LVT)
                         else if n < 44145 then LV else LVT)
                      else
                        if n < 44116
                        then (if n < 44117 then LV else LVT)
                        else LV)
                 else
                   if n < 44013
                   then
                     (if n < 44033
                      then
                        (if n < 44061
                         then (if n < 44088 then LVT else LV)
                         else if n < 44060 then LVT else LV)
                      else
                        if n < 44014
                        then (if n < 44032 then LVT else LV)
                        else Other)
                   else
                     if n < 44008
                     then
                       (if n < 44011
                        then (if n < 44012 then Extend else SpacingMark)
                        else if n < 44009 then Other else SpacingMark)
                     else
                       if n < 44005
                       then (if n < 44006 then Extend else SpacingMark)
                       else Extend)
            else
              if n < 43569
              then
                (if n < 43701
                 then
                   (if n < 43755
                    then
                      (if n < 43760
                       then
                         (if n < 43766
                          then (if n < 43767 then SpacingMark else Other)
                          else if n < 43765 then Extend else SpacingMark)
                       else
                         if n < 43756
                         then (if n < 43758 then Other else SpacingMark)
                         else Extend)
                    else
                      if n < 43710
                      then
                        (if n < 43713
                         then (if n < 43714 then SpacingMark else Other)
                         else if n < 43712 then Extend else Other)
                      else
                        if n < 43703
                        then (if n < 43705 then Extend else Other)
                        else Extend)
                 else
                   if n < 43597
                   then
                     (if n < 43645
                      then
                        (if n < 43697
                         then (if n < 43698 then Other else Extend)
                         else if n < 43696 then Other else Extend)
                      else
                        if n < 43598
                        then (if n < 43644 then Other else Extend)
                        else Other)
                   else
                     if n < 43575
                     then
                       (if n < 43588
                        then (if n < 43596 then SpacingMark else Extend)
                        else if n < 43587 then Other else Extend)
                     else
                       if n < 43571
                       then (if n < 43573 then Other else Extend)
                       else SpacingMark)
              else
                if n < 43392
                then
                  (if n < 43452
                   then
                     (if n < 43493
                      then
                        (if n < 43561
                         then (if n < 43567 then Extend else SpacingMark)
                         else if n < 43494 then Extend else Other)
                      else
                        if n < 43454
                        then (if n < 43457 then Extend else Other)
                        else SpacingMark)
                   else
                     if n < 43443
                     then
                       (if n < 43446
                        then (if n < 43450 then Extend else SpacingMark)
                        else if n < 43444 then Extend else SpacingMark)
                     else
                       if n < 43395
                       then (if n < 43396 then Extend else Other)
                       else SpacingMark)
                else
                  if n < 43302
                  then
                    (if n < 43346
                     then
                       (if n < 43360
                        then (if n < 43389 then Extend else Other)
                        else if n < 43348 then L else Other)
                     else
                       if n < 43310
                       then (if n < 43335 then SpacingMark else Extend)
                       else Other)
                  else
                    if n < 43232
                    then
                      (if n < 43263
                       then (if n < 43264 then Extend else Other)
                       else if n < 43250 then Extend else Other)
                    else
                      if n < 43204
                      then (if n < 43206 then Extend else Other)
                      else Extend)
         else
           if n < 10083
           then
             (if n < 12337
              then
                (if n < 42656
                 then
                   (if n < 43019
                    then
                      (if n < 43047
                       then
                         (if n < 43136
                          then (if n < 43138 then SpacingMark else Other)
                          else if n < 43048 then SpacingMark else Other)
                       else
                         if n < 43043
                         then (if n < 43045 then SpacingMark else Extend)
                         else if n < 43020 then SpacingMark else Other)
                    else
                      if n < 43010
                      then
                        (if n < 43014
                         then (if n < 43015 then Extend else Other)
                         else if n < 43011 then Extend else Other)
                      else
                        if n < 42736
                        then (if n < 42738 then Extend else Other)
                        else Extend)
                 else
                   if n < 12953
                   then
                     (if n < 42611
                      then
                        (if n < 42622
                         then (if n < 42654 then Other else Extend)
                         else if n < 42612 then Other else Extend)
                      else
                        if n < 12954
                        then (if n < 42607 then Other else Extend)
                        else Other)
                   else
                     if n < 12441
                     then
                       (if n < 12951
                        then (if n < 12952 then ExtPict else Other)
                        else if n < 12443 then ExtPict else Other)
                     else
                       if n < 12349
                       then (if n < 12350 then Extend else Other)
                       else ExtPict)
              else
                if n < 11035
                then
                  (if n < 11506
                   then
                     (if n < 11744
                      then
                        (if n < 12330
                         then (if n < 12336 then Other else ExtPict)
                         else if n < 11776 then Extend else Other)
                      else
                        if n < 11647
                        then (if n < 11648 then Extend else Other)
                        else Extend)
                   else
                     if n < 11089
                     then
                       (if n < 11094
                        then (if n < 11503 then Other else Extend)
                        else if n < 11093 then Other else ExtPict)
                     else
                       if n < 11037
                       then (if n < 11088 then Other else ExtPict)
                       else Other)
                else
                  if n < 10161
                  then
                    (if n < 10548
                     then
                       (if n < 11013
                        then (if n < 11016 then ExtPict else Other)
                        else if n < 10550 then ExtPict else Other)
                     else
                       if n < 10175
                       then (if n < 10176 then ExtPict else Other)
                       else ExtPict)
                  else
                    if n < 10136
                    then
                      (if n < 10146
                       then (if n < 10160 then Other else ExtPict)
                       else if n < 10145 then Other else ExtPict)
                    else
                      if n < 10088
                      then (if n < 10133 then Other else ExtPict)
                      else Other)
           else
             if n < 9872
             then
               (if n < 10035
                then
                  (if n < 10061
                   then
                     (if n < 10067
                      then
                        (if n < 10071
                         then (if n < 10072 then ExtPict else Other)
                         else if n < 10070 then ExtPict else Other)
                      else
                        if n < 10062
                        then (if n < 10063 then ExtPict else Other)
                        else ExtPict)
                   else
                     if n < 10053
                     then
                       (if n < 10056
                        then (if n < 10060 then Other else ExtPict)
                        else if n < 10055 then Other else ExtPict)
                     else
                       if n < 10037
                       then (if n < 10052 then Other else ExtPict)
                       else Other)
                else
                  if n < 10007
                  then
                    (if n < 10017
                     then
                       (if n < 10024
                        then (if n < 10025 then ExtPict else Other)
                        else if n < 10018 then ExtPict else Other)
                     else
                       if n < 10013
                       then (if n < 10014 then ExtPict else Other)
                       else ExtPict)
                  else
                    if n < 10003
                    then
                      (if n < 10005
                       then (if n < 10006 then Other else ExtPict)
                       else if n < 10004 then Other else ExtPict)
                    else
                      if n < 9990
                      then (if n < 9992 then Other else ExtPict)
                      else Other)
             else
               if n < 9642
               then
                 (if n < 9727
                  then
                    (if n < 9735
                     then
                       (if n < 9748
                        then (if n < 9862 then ExtPict else Other)
                        else if n < 9747 then ExtPict else Other)
                     else
                       if n < 9728
                       then (if n < 9734 then ExtPict else Other)
                       else ExtPict)
                  else
                    if n < 9655
                    then
                      (if n < 9665
                       then (if n < 9723 then Other else ExtPict)
                       else if n < 9664 then Other else ExtPict)
                    else
                      if n < 9644
                      then (if n < 9654 then Other else ExtPict)
                      else Other)
               else
                 if n < 9168
                 then
                   (if n < 9208
                    then
                      (if n < 9410
                       then (if n < 9411 then ExtPict else Other)
                       else if n < 9211 then ExtPict else Other)
                    else
                      if n < 9193
                      then (if n < 9204 then ExtPict else Other)
                      else ExtPict)
                 else
                   if n < 9001
                   then
                     (if n < 9097
                      then (if n < 9167 then Other else ExtPict)
                      else if n < 9096 then Other else ExtPict)
                   else
                     if n < 8988
                     then (if n < 9000 then Other else ExtPict)
                     else Other)
    else
      if n < 3397
      then
        (if n < 6277
         then
           (if n < 7086
            then
              (if n < 7616
               then
                 (if n < 8266
                  then
                    (if n < 8482
                     then
                       (if n < 8596
                        then
                          (if n < 8617
                           then (if n < 8619 then ExtPict else Other)
                           else if n < 8602 then ExtPict else Other)
                        else
                          if n < 8505
                          then (if n < 8506 then ExtPict else Other)
                          else if n < 8483 then ExtPict else Other)
                     else
                       if n < 8294
                       then
                         (if n < 8400
                          then (if n < 8433 then ExtPict else Other)
                          else if n < 8304 then Extend else Other)
                       else
                         if n < 8288
                         then (if n < 8293 then Control else Other)
                         else Control)
                  else
                    if n < 8206
                    then
                      (if n < 8239
                       then
                         (if n < 8253
                          then (if n < 8265 then Other else ExtPict)
                          else if n < 8252 then Other else ExtPict)
                       else
                         if n < 8208
                         then (if n < 8232 then Other else Control)
                         else Other)
                    else
                      if n < 7680
                      then
                        (if n < 8204
                         then (if n < 8205 then Control else ZWJ)
                         else if n < 8203 then Extend else Control)
                      else
                        if n < 7674
                        then (if n < 7675 then Other else Extend)
                        else Other)
               else
                 if n < 7224
                 then
                   (if n < 7405
                    then
                      (if n < 7413
                       then
                         (if n < 7416
                          then (if n < 7418 then Extend else Other)
                          else if n < 7415 then Extend else SpacingMark)
                       else
                         if n < 7406
                         then (if n < 7412 then Other else Extend)
                         else Other)
                    else
                      if n < 7380
                      then
                        (if n < 7394
                         then (if n < 7401 then Extend else Other)
                         else if n < 7393 then Extend else SpacingMark)
                      else
                        if n < 7376
                        then (if n < 7379 then Extend else Other)
                        else Extend)
                 else
                   if n < 7151
                   then
                     (if n < 7204
                      then
                        (if n < 7220
                         then (if n < 7222 then Other else Extend)
                         else if n < 7212 then SpacingMark else Extend)
                      else
                        if n < 7154
                        then (if n < 7156 then SpacingMark else Other)
                        else SpacingMark)
                   else
                     if n < 7144
                     then
                       (if n < 7149
                        then (if n < 7150 then Extend else SpacingMark)
                        else if n < 7146 then Extend else SpacingMark)
                     else
                       if n < 7142
                       then (if n < 7143 then Extend else SpacingMark)
                       else Extend)
            else
              if n < 6765
              then
                (if n < 6978
                 then
                   (if n < 7043
                    then
                      (if n < 7078
                       then
                         (if n < 7082
                          then (if n < 7083 then Other else Extend)
                          else if n < 7080 then SpacingMark else Extend)
                       else
                         if n < 7073
                         then (if n < 7074 then SpacingMark else Extend)
                         else SpacingMark)
                    else
                      if n < 7019
                      then
                        (if n < 7040
                         then (if n < 7042 then Other else SpacingMark)
                         else if n < 7028 then Extend else Other)
                      else
                        if n < 6979
                        then (if n < 6981 then Extend else Other)
                        else SpacingMark)
                 else
                   if n < 6912
                   then
                     (if n < 6964
                      then
                        (if n < 6972
                         then (if n < 6973 then Extend else SpacingMark)
                         else if n < 6971 then Extend else SpacingMark)
                      else
                        if n < 6916
                        then (if n < 6917 then Extend else Other)
                        else SpacingMark)
                   else
                     if n < 6783
                     then
                       (if n < 6832
                        then (if n < 6847 then Extend else Other)
                        else if n < 6784 then Extend else Other)
                     else
                       if n < 6771
                       then (if n < 6781 then Extend else Other)
                       else Extend)
              else
                if n < 6679
                then
                  (if n < 6744
                   then
                     (if n < 6753
                      then
                        (if n < 6755
                         then (if n < 6757 then SpacingMark else Extend)
                         else if n < 6754 then Other else Extend)
                      else
                        if n < 6751
                        then (if n < 6752 then Other else Extend)
                        else Other)
                   else
                     if n < 6684
                     then
                       (if n < 6742
                        then (if n < 6743 then Extend else SpacingMark)
                        else if n < 6741 then Extend else SpacingMark)
                     else
                       if n < 6681
                       then (if n < 6683 then Other else Extend)
                       else SpacingMark)
                else
                  if n < 6441
                  then
                    (if n < 6450
                     then
                       (if n < 6457
                        then (if n < 6460 then Extend else Other)
                        else if n < 6451 then Extend else SpacingMark)
                     else
                       if n < 6444
                       then (if n < 6448 then Extend else SpacingMark)
                       else Other)
                  else
                    if n < 6314
                    then
                      (if n < 6435
                       then (if n < 6439 then SpacingMark else Extend)
                       else if n < 6432 then SpacingMark else Extend)
                    else
                      if n < 6279
                      then (if n < 6313 then Other else Extend)
                      else Other)
         else
           if n < 3981
           then
             (if n < 4253
              then
                (if n < 6002
                 then
                   (if n < 6087
                    then
                      (if n < 6110
                       then
                         (if n < 6158
                          then (if n < 6159 then Extend else Other)
                          else if n < 6155 then Control else Extend)
                       else
                         if n < 6100
                         then (if n < 6109 then Other else Extend)
                         else if n < 6089 then Other else Extend)
                    else
                      if n < 6070
                      then
                        (if n < 6078
                         then (if n < 6086 then SpacingMark else Extend)
                         else if n < 6071 then SpacingMark else Extend)
                      else
                        if n < 6004
                        then (if n < 6068 then SpacingMark else Extend)
                        else Other)
                 else
                   if n < 4960
                   then
                     (if n < 5938
                      then
                        (if n < 5970
                         then (if n < 5972 then Extend else Other)
                         else if n < 5941 then Extend else Other)
                      else
                        if n < 5906
                        then (if n < 5909 then Extend else Other)
                        else Extend)
                   else
                     if n < 4448
                     then
                       (if n < 4608
                        then (if n < 4957 then Other else Extend)
                        else if n < 4520 then Other else T)
                     else
                       if n < 4254
                       then (if n < 4352 then V else L)
                       else Other)
              else
                if n < 4182
                then
                  (if n < 4226
                   then
                     (if n < 4229
                      then
                        (if n < 4237
                         then (if n < 4238 then Extend else Other)
                         else if n < 4231 then Extend else Other)
                      else
                        if n < 4227
                        then (if n < 4228 then Extend else SpacingMark)
                        else Other)
                   else
                     if n < 4190
                     then
                       (if n < 4209
                        then (if n < 4213 then Extend else Other)
                        else if n < 4193 then Extend else Other)
                     else
                       if n < 4184
                       then (if n < 4186 then Extend else Other)
                       else Extend)
                else
                  if n < 4145
                  then
                    (if n < 4153
                     then
                       (if n < 4157
                        then (if n < 4159 then SpacingMark else Other)
                        else if n < 4155 then Extend else SpacingMark)
                     else
                       if n < 4146
                       then (if n < 4152 then Extend else Other)
                       else Extend)
                  else
                    if n < 4029
                    then
                      (if n < 4039
                       then (if n < 4141 then SpacingMark else Extend)
                       else if n < 4038 then Other else Extend)
                    else
                      if n < 3992
                      then (if n < 3993 then Other else Extend)
                      else Other)
           else
             if n < 3635
             then
               (if n < 3893
                then
                  (if n < 3904
                   then
                     (if n < 3968
                      then
                        (if n < 3974
                         then (if n < 3976 then Extend else Other)
                         else if n < 3973 then Extend else Other)
                      else
                        if n < 3953
                        then (if n < 3967 then Extend else SpacingMark)
                        else Extend)
                   else
                     if n < 3896
                     then
                       (if n < 3898
                        then (if n < 3902 then Other else SpacingMark)
                        else if n < 3897 then Other else Extend)
                     else
                       if n < 3894
                       then (if n < 3895 then Other else Extend)
                       else Other)
                else
                  if n < 3763
                  then
                    (if n < 3784
                     then
                       (if n < 3864
                        then (if n < 3866 then Extend else Other)
                        else if n < 3790 then Extend else Other)
                     else
                       if n < 3764
                       then (if n < 3773 then Extend else Other)
                       else Extend)
                  else
                    if n < 3655
                    then
                      (if n < 3761
                       then (if n < 3762 then SpacingMark else Other)
                       else if n < 3663 then Extend else Other)
                    else
                      if n < 3636
                      then (if n < 3643 then Extend else Other)
                      else Extend)
             else
               if n < 3531
               then
                 (if n < 3544
                  then
                    (if n < 3570
                     then
                       (if n < 3633
                        then (if n < 3634 then SpacingMark else Other)
                        else if n < 3572 then Extend else Other)
                     else
                       if n < 3551
                       then (if n < 3552 then SpacingMark else Other)
                       else Extend)
                  else
                    if n < 3538
                    then
                      (if n < 3542
                       then (if n < 3543 then SpacingMark else Other)
                       else if n < 3541 then Extend else Other)
                    else
                      if n < 3535
                      then (if n < 3536 then Extend else SpacingMark)
                      else Extend)
               else
                 if n < 3415
                 then
                   (if n < 3428
                    then
                      (if n < 3460
                       then (if n < 3530 then Other else Extend)
                       else if n < 3458 then Other else SpacingMark)
                    else
                      if n < 3416
                      then (if n < 3426 then Other else Extend)
                      else Other)
                 else
                   if n < 3402
                   then
                     (if n < 3406
                      then (if n < 3407 then Extend else Other)
                      else if n < 3405 then Prepend else Extend)
                   else
                     if n < 3398
                     then (if n < 3401 then SpacingMark else Other)
                     else SpacingMark)
      else
        if n < 2559
        then
          (if n < 2947
           then
             (if n < 3201
              then
                (if n < 3274
                 then
                   (if n < 3328
                    then
                      (if n < 3389
                       then
                         (if n < 3391
                          then (if n < 3393 then Other else Extend)
                          else if n < 3390 then SpacingMark else Extend)
                       else
                         if n < 3332
                         then (if n < 3387 then Other else Extend)
                         else if n < 3330 then Other else SpacingMark)
                    else
                      if n < 3285
                      then
                        (if n < 3298
                         then (if n < 3300 then Extend else Other)
                         else if n < 3287 then Extend else Other)
                      else
                        if n < 3276
                        then (if n < 3278 then Extend else Other)
                        else Extend)
                 else
                   if n < 3264
                   then
                     (if n < 3269
                      then
                        (if n < 3271
                         then (if n < 3273 then SpacingMark else Other)
                         else if n < 3270 then SpacingMark else Extend)
                      else
                        if n < 3266
                        then (if n < 3267 then Other else SpacingMark)
                        else Extend)
                   else
                     if n < 3260
                     then
                       (if n < 3262
                        then (if n < 3263 then SpacingMark else Extend)
                        else if n < 3261 then SpacingMark else Other)
                     else
                       if n < 3202
                       then (if n < 3204 then Extend else Other)
                       else SpacingMark)
              else
                if n < 3073
                then
                  (if n < 3145
                   then
                     (if n < 3157
                      then
                        (if n < 3170
                         then (if n < 3172 then Extend else Other)
                         else if n < 3159 then Extend else Other)
                      else
                        if n < 3146
                        then (if n < 3150 then Extend else Other)
                        else Extend)
                   else
                     if n < 3134
                     then
                       (if n < 3141
                        then (if n < 3142 then Other else Extend)
                        else if n < 3137 then Other else SpacingMark)
                     else
                       if n < 3076
                       then (if n < 3077 then Extend else Other)
                       else Extend)
                else
                  if n < 3017
                  then
                    (if n < 3022
                     then
                       (if n < 3032
                        then (if n < 3072 then SpacingMark else Extend)
                        else if n < 3031 then Other else Extend)
                     else
                       if n < 3018
                       then (if n < 3021 then Other else Extend)
                       else SpacingMark)
                  else
                    if n < 3008
                    then
                      (if n < 3011
                       then (if n < 3014 then Other else SpacingMark)
                       else if n < 3009 then Other else SpacingMark)
                    else
                      if n < 3006
                      then (if n < 3007 then Extend else SpacingMark)
                      else Extend)
           else
             if n < 2761
             then
               (if n < 2878
                then
                  (if n < 2893
                   then
                     (if n < 2904
                      then
                        (if n < 2916
                         then (if n < 2946 then Other else Extend)
                         else if n < 2914 then Other else Extend)
                      else
                        if n < 2894
                        then (if n < 2902 then Other else Extend)
                        else Other)
                   else
                     if n < 2885
                     then
                       (if n < 2889
                        then (if n < 2891 then Extend else SpacingMark)
                        else if n < 2887 then Other else SpacingMark)
                     else
                       if n < 2880
                       then (if n < 2881 then Other else Extend)
                       else SpacingMark)
                else
                  if n < 2810
                  then
                    (if n < 2818
                     then
                       (if n < 2876
                        then (if n < 2877 then Extend else Other)
                        else if n < 2820 then Extend else Other)
                     else
                       if n < 2816
                       then (if n < 2817 then SpacingMark else Extend)
                       else Other)
                  else
                    if n < 2765
                    then
                      (if n < 2786
                       then (if n < 2788 then Extend else Other)
                       else if n < 2766 then Extend else Other)
                    else
                      if n < 2762
                      then (if n < 2763 then Extend else SpacingMark)
                      else Other)
             else
               if n < 2642
               then
                 (if n < 2692
                  then
                    (if n < 2750
                     then
                       (if n < 2758
                        then (if n < 2759 then SpacingMark else Extend)
                        else if n < 2753 then Other else Extend)
                     else
                       if n < 2748
                       then (if n < 2749 then SpacingMark else Other)
                       else Extend)
                  else
                    if n < 2677
                    then
                      (if n < 2689
                       then (if n < 2691 then Other else SpacingMark)
                       else if n < 2678 then Extend else Other)
                    else
                      if n < 2672
                      then (if n < 2674 then Extend else Other)
                      else Extend)
               else
                 if n < 2625
                 then
                   (if n < 2633
                    then
                      (if n < 2638
                       then (if n < 2641 then Other else Extend)
                       else if n < 2635 then Other else Extend)
                    else
                      if n < 2627
                      then (if n < 2631 then Other else Extend)
                      else Other)
                 else
                   if n < 2564
                   then
                     (if n < 2621
                      then (if n < 2622 then Extend else SpacingMark)
                      else if n < 2620 then Other else Extend)
                   else
                     if n < 2561
                     then (if n < 2563 then Other else SpacingMark)
                     else Extend)
        else
          if n < 1810
          then
            (if n < 2366
             then
               (if n < 2493
                then
                  (if n < 2507
                   then
                     (if n < 2520
                      then
                        (if n < 2532
                         then (if n < 2558 then Other else Extend)
                         else if n < 2530 then Other else Extend)
                      else
                        if n < 2510
                        then (if n < 2519 then Other else Extend)
                        else if n < 2509 then Other else Extend)
                   else
                     if n < 2497
                     then
                       (if n < 2503
                        then (if n < 2505 then SpacingMark else Other)
                        else if n < 2501 then SpacingMark else Other)
                     else
                       if n < 2494
                       then (if n < 2495 then Extend else SpacingMark)
                       else Extend)
                else
                  if n < 2392
                  then
                    (if n < 2433
                     then
                       (if n < 2436
                        then (if n < 2492 then Other else Extend)
                        else if n < 2434 then Other else SpacingMark)
                     else
                       if n < 2402
                       then (if n < 2404 then Extend else Other)
                       else Extend)
                  else
                    if n < 2381
                    then
                      (if n < 2384
                       then (if n < 2385 then Other else Extend)
                       else if n < 2382 then Other else SpacingMark)
                    else
                      if n < 2369
                      then (if n < 2377 then Extend else SpacingMark)
                      else Extend)
             else
               if n < 2088
               then
                 (if n < 2275
                  then
                    (if n < 2362
                     then
                       (if n < 2364
                        then (if n < 2365 then SpacingMark else Other)
                        else if n < 2363 then Extend else SpacingMark)
                     else
                       if n < 2307
                       then (if n < 2308 then Extend else Other)
                       else SpacingMark)
                  else
                    if n < 2137
                    then
                      (if n < 2259
                       then (if n < 2274 then Extend else Prepend)
                       else if n < 2140 then Extend else Other)
                    else
                      if n < 2089
                      then (if n < 2094 then Extend else Other)
                      else Extend)
               else
                 if n < 2045
                 then
                   (if n < 2074
                    then
                      (if n < 2084
                       then (if n < 2085 then Other else Extend)
                       else if n < 2075 then Other else Extend)
                    else
                      if n < 2046
                      then (if n < 2070 then Other else Extend)
                      else Other)
                 else
                   if n < 1958
                   then
                     (if n < 2027
                      then (if n < 2036 then Extend else Other)
                      else if n < 1969 then Extend else Other)
                   else
                     if n < 1840
                     then (if n < 1867 then Extend else Other)
                     else Extend)
          else
            if n < 1473
            then
              (if n < 1648
               then
                 (if n < 1767
                  then
                    (if n < 1774
                     then
                       (if n < 1808
                        then (if n < 1809 then Other else Extend)
                        else if n < 1807 then Other else Prepend)
                     else
                       if n < 1769
                       then (if n < 1770 then Other else Extend)
                       else Other)
                  else
                    if n < 1757
                    then
                      (if n < 1759
                       then (if n < 1765 then Extend else Other)
                       else if n < 1758 then Extend else Other)
                    else
                      if n < 1649
                      then (if n < 1750 then Prepend else Extend)
                      else Other)
               else
                 if n < 1542
                 then
                   (if n < 1564
                    then
                      (if n < 1611
                       then (if n < 1632 then Extend else Other)
                       else if n < 1565 then Extend else Other)
                    else
                      if n < 1552
                      then (if n < 1563 then Control else Other)
                      else Extend)
                 else
                   if n < 1478
                   then
                     (if n < 1480
                      then (if n < 1536 then Other else Prepend)
                      else if n < 1479 then Other else Extend)
                   else
                     if n < 1475
                     then (if n < 1476 then Other else Extend)
                     else Other)
            else
              if n < 160
              then
                (if n < 880
                 then
                   (if n < 1425
                    then
                      (if n < 1471
                       then (if n < 1472 then Extend else Other)
                       else if n < 1470 then Extend else Other)
                    else
                      if n < 1155
                      then (if n < 1162 then Extend else Other)
                      else Extend)
                 else
                   if n < 173
                   then
                     (if n < 175
                      then (if n < 768 then Other else Extend)
                      else if n < 174 then Other else ExtPict)
                   else
                     if n < 169
                     then (if n < 170 then Control else Other)
                     else ExtPict)
              else
                if n < 35
                then
                  (if n < 43
                   then
                     (if n < 58
                      then (if n < 127 then Other else Control)
                      else if n < 48 then Other else ExtPict)
                   else
                     if n < 36
                     then (if n < 42 then Other else ExtPict)
                     else Other)
                else
                  if n < 11
                  then
                    (if n < 14
                     then (if n < 32 then ExtPict else Other)
                     else if n < 13 then Control else CR)
                  else
                    if n < 0
                    then (if n < 10 then Control else LF)
                    else Control

type previous_chars =
  EvenRegionalIndicator | ExtPictExtendStar | NoPrevious

(* [break_between previous c1 c2] returns true if and only if a grapheme break
   is between c1 and c2.
   previous must tel if there is an even number of RI before c1, if
   c1 is a RI. It should tell if there is a pattern
   ExtPict Extend* before c1 if c1 is ZWJ *)

let break_between previous bp1 bp2 =
  match (bp1,bp2) with
  | (CR, LF)                 -> false (* rule 3.0 *)
  | ((Control | CR | LF), _) -> true  (* rule 4.0 *)
  | (_, (Control | CR | LF)) -> true  (* rule 5.0 *)
  | (L, (L | V | LV | LVT))  -> false (* rule 6.0 *)
  | ((LV | V), (V | T))      -> false (* rule 7.0 *)
  | ((LVT | T), T)           -> false (* rule 8.0 *)
  | (_, (Extend | ZWJ))      -> false (* rule 9.0 *)
  | (_, SpacingMark)         -> false (* rule 9.1 *)
  | (Prepend, _)             -> false (* rule 9.2 *)
  | (ZWJ, ExtPict) when previous = ExtPictExtendStar
                             -> false (* rule 11.0 *)
  | (RegionalIndicator, RegionalIndicator) when previous = EvenRegionalIndicator
                             -> false (* rule 12.0 and 13.0, assuming *)

  | _                        -> true  (* rule 999.0 *)

(*
 * Decode a UTF8 character at a given position in a string.
 * Arguments:
 *   s : the string,
 *   i : index where to look.
 * Returns a couple (c, l) where c is the code of the character and l is the
 * number of bytes read.
 * Raise invalid_arg if s.[i] is not a valid first byte for a UTF8 character.
 * No checks are run on the hypothetical second, third and fourth byte (i.e.
 * length of s is not checked, and shape 0x10xxxxxx of byte is not checked).
 *)
let decode : string -> int -> (Uchar.t * int) = fun s i ->
  let cc = Char.code s.[i] in
  if cc lsr 7 = 0 then
    (Uchar.of_int cc, 1)
  else if (cc lsr 6) land 1 = 0 then
    raise (invalid_arg "UTF8.decode")
  else if (cc lsr 5) land 1 = 0 then
    let i0 = (cc land 0b00011111) lsl 6 in
    let i1 = (Char.code s.[i+1]) land 0b00111111 in
    (Uchar.of_int (i0 lor i1), 2)
  else if (cc lsr 4) land 1 = 0 then
    let i0 = (cc land 0b00001111) lsl 12 in
    let i1 = ((Char.code s.[i+1]) land 0b00111111) lsl 6 in
    let i2 = (Char.code s.[i+2])  land 0b00111111 in
    (Uchar.of_int (i0 lor i1 lor i2), 3)
  else if (cc lsr 3) land 1 = 0 then
    let i0 = (cc land 0b00000111) lsl 18 in
    let i1 = ((Char.code s.[i+1]) land 0b00111111) lsl 12 in
    let i2 = ((Char.code s.[i+2]) land 0b00111111) lsl 6 in
    let i3 = (Char.code s.[i+3])  land 0b00111111 in
    (Uchar.of_int (i0 lor i1 lor i2 lor i3), 4)
  else
    raise (invalid_arg "UTF8.decode")

let look : string -> int -> Uchar.t = fun s i ->
  fst (decode s i)

let next : string -> int -> int = fun s i ->
  let (_, sz) = decode s i in
  i + sz

let prev : string -> int -> int = fun s i ->
  let ps = List.filter (fun i -> i >= 0) [i-1; i-2; i-3; i-4] in
  let rec try_until_found l =
    match l with
    | []    -> assert false
    | p::ps -> try
               let (_, sz) = decode s p in
               if p + sz <> i then assert false;
               p
             with
               Invalid_argument _ -> try_until_found ps
  in try_until_found ps

let grapheme_break : string -> int -> bool = fun s pos ->
  if pos = 0 || pos >= String.length s then true else
    let i0 = prev s pos in
    let c1 = look s i0 and c2 = look s pos in
    let bp1 = gbp c1 and bp2 = gbp c2 in
    let rec previous_ri acc i0 bp1 =
      match bp1 with
      | RegionalIndicator ->
         begin
           if i0 = 0 then not acc else
             let i0 = prev s i0 in
             let c1 = look s i0 in
             let bp1 = gbp c1 in
             previous_ri (not acc) i0 bp1
         end
      | _ -> acc
    in
    let rec previous_pict i0 bp1 =
      match bp1 with
      | Extend ->
         begin
           if i0 = 0 then false else
             let i0 = prev s i0 in
             let c1 = look s i0 in
             let bp1 = gbp c1 in
             previous_pict i0 bp1
         end
      | ExtPict -> true
      | _       -> false
    in
    let previous =
      match bp1 with
      | ZWJ ->
         if previous_pict i0 Extend then ExtPictExtendStar
         else NoPrevious
      | RegionalIndicator ->
         if previous_ri false i0 RegionalIndicator then EvenRegionalIndicator
         else NoPrevious
      | _ -> NoPrevious
    in
    break_between previous bp1 bp2

let next_grapheme : string -> int -> int = fun s pos ->
  let npos = ref pos in
  try
    while !npos < String.length s do
      npos := next s !npos;
      if grapheme_break s !npos then raise Exit
    done;
    raise Not_found (* end of string *)
  with
    Exit -> !npos

let prev_grapheme : string -> int -> int = fun s pos ->
  let npos = ref pos in
  try
    while !npos > 0 do
      npos := prev s !npos;
      if grapheme_break s !npos then raise Exit
    done;
    0
  with
    Exit -> !npos

let fold_grapheme : (string -> 'a -> 'a) -> 'a -> string -> 'a =
  fun fn acc s ->
  let pos = ref 0 in
  let res = ref acc in
  while !pos < String.length s do
    let npos = next_grapheme s !pos in
    let s = String.sub s !pos (npos - !pos) in
    pos := npos;
    res := fn s !res;
  done;
  !res
