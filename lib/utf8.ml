type context =
  ASCII | UTF8 | CJK_UTF8

let width ?(context=UTF8) c =
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
