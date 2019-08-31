type context =
  | ASCII
  | UTF8
  | CJK_UTF8
let width ?(context= UTF8)  c =
  let n = Uchar.to_int c in
  if context = CJK_UTF8
  then
    (if n <= 9450
     then
       (if n <= 3642
        then
          (if n <= 1749
           then
             (if n <= 363
              then
                (if n <= 240
                 then
                   (if n <= 181
                    then
                      (if n <= 166
                       then
                         (if n <= 159
                          then
                            (if n <= 31
                             then (if n <= 0 then 0 else (-1))
                             else if n <= 126 then 1 else (-1))
                          else
                            if n <= 161
                            then (if n <= 160 then 1 else 2)
                            else
                              if n <= 163
                              then 1
                              else if n <= 164 then 2 else 1)
                       else
                         if n <= 172
                         then
                           (if n <= 169
                            then (if n <= 168 then 2 else 1)
                            else if n <= 170 then 2 else 1)
                         else
                           if n <= 174
                           then (if n <= 173 then 0 else 2)
                           else
                             if n <= 175
                             then 1
                             else if n <= 180 then 2 else 1)
                    else
                      if n <= 216
                      then
                        (if n <= 197
                         then
                           (if n <= 187
                            then (if n <= 186 then 2 else 1)
                            else if n <= 191 then 2 else 1)
                         else
                           if n <= 207
                           then (if n <= 198 then 2 else 1)
                           else
                             if n <= 208
                             then 2
                             else if n <= 214 then 1 else 2)
                      else
                        if n <= 231
                        then
                          (if n <= 225
                           then (if n <= 221 then 1 else 2)
                           else
                             if n <= 229
                             then 1
                             else if n <= 230 then 2 else 1)
                        else
                          if n <= 235
                          then (if n <= 234 then 2 else 1)
                          else
                            if n <= 237 then 2 else if n <= 239 then 1 else 2)
                 else
                   if n <= 298
                   then
                     (if n <= 256
                      then
                        (if n <= 250
                         then
                           (if n <= 243
                            then (if n <= 241 then 1 else 2)
                            else if n <= 246 then 1 else 2)
                         else
                           if n <= 252
                           then (if n <= 251 then 1 else 2)
                           else
                             if n <= 253
                             then 1
                             else if n <= 254 then 2 else 1)
                      else
                        if n <= 275
                        then
                          (if n <= 272
                           then (if n <= 257 then 2 else 1)
                           else
                             if n <= 273
                             then 2
                             else if n <= 274 then 1 else 2)
                        else
                          if n <= 283
                          then (if n <= 282 then 1 else 2)
                          else
                            if n <= 293 then 1 else if n <= 295 then 2 else 1)
                   else
                     if n <= 324
                     then
                       (if n <= 311
                        then
                          (if n <= 304
                           then (if n <= 299 then 2 else 1)
                           else if n <= 307 then 2 else 1)
                        else
                          if n <= 318
                          then (if n <= 312 then 2 else 1)
                          else
                            if n <= 322 then 2 else if n <= 323 then 1 else 2)
                     else
                       if n <= 337
                       then
                         (if n <= 331
                          then (if n <= 327 then 1 else 2)
                          else
                            if n <= 332 then 1 else if n <= 333 then 2 else 1)
                       else
                         if n <= 357
                         then (if n <= 339 then 2 else 1)
                         else
                           if n <= 359 then 2 else if n <= 362 then 1 else 2)
              else
                if n <= 767
                then
                  (if n <= 593
                   then
                     (if n <= 469
                      then
                        (if n <= 464
                         then
                           (if n <= 462
                            then (if n <= 461 then 1 else 2)
                            else if n <= 463 then 1 else 2)
                         else
                           if n <= 466
                           then (if n <= 465 then 1 else 2)
                           else
                             if n <= 467
                             then 1
                             else if n <= 468 then 2 else 1)
                      else
                        if n <= 473
                        then
                          (if n <= 471
                           then (if n <= 470 then 2 else 1)
                           else if n <= 472 then 2 else 1)
                        else
                          if n <= 475
                          then (if n <= 474 then 2 else 1)
                          else
                            if n <= 476 then 2 else if n <= 592 then 1 else 2)
                   else
                     if n <= 716
                     then
                       (if n <= 708
                        then
                          (if n <= 609
                           then (if n <= 608 then 1 else 2)
                           else if n <= 707 then 1 else 2)
                        else
                          if n <= 711
                          then (if n <= 710 then 1 else 2)
                          else
                            if n <= 712 then 1 else if n <= 715 then 2 else 1)
                     else
                       if n <= 731
                       then
                         (if n <= 719
                          then (if n <= 717 then 2 else 1)
                          else
                            if n <= 720 then 2 else if n <= 727 then 1 else 2)
                       else
                         if n <= 733
                         then (if n <= 732 then 1 else 2)
                         else
                           if n <= 734 then 1 else if n <= 735 then 2 else 1)
                else
                  if n <= 1469
                  then
                    (if n <= 969
                     then
                       (if n <= 930
                        then
                          (if n <= 912
                           then (if n <= 879 then 0 else 1)
                           else if n <= 929 then 2 else 1)
                        else
                          if n <= 944
                          then (if n <= 937 then 2 else 1)
                          else
                            if n <= 961 then 2 else if n <= 962 then 1 else 2)
                     else
                       if n <= 1104
                       then
                         (if n <= 1025
                          then (if n <= 1024 then 1 else 2)
                          else
                            if n <= 1039
                            then 1
                            else if n <= 1103 then 2 else 1)
                       else
                         if n <= 1154
                         then (if n <= 1105 then 2 else 1)
                         else
                           if n <= 1161
                           then 0
                           else if n <= 1424 then 1 else 0)
                  else
                    if n <= 1535
                    then
                      (if n <= 1474
                       then
                         (if n <= 1471
                          then (if n <= 1470 then 1 else 0)
                          else if n <= 1472 then 1 else 0)
                       else
                         if n <= 1477
                         then (if n <= 1475 then 1 else 0)
                         else
                           if n <= 1478
                           then 1
                           else if n <= 1479 then 0 else 1)
                    else
                      if n <= 1564
                      then
                        (if n <= 1551
                         then (if n <= 1541 then 0 else 1)
                         else
                           if n <= 1562
                           then 0
                           else if n <= 1563 then 1 else 0)
                      else
                        if n <= 1631
                        then (if n <= 1610 then 1 else 0)
                        else
                          if n <= 1647 then 1 else if n <= 1648 then 0 else 1)
           else
             if n <= 2748
             then
               (if n <= 2376
                then
                  (if n <= 2044
                   then
                     (if n <= 1807
                      then
                        (if n <= 1766
                         then
                           (if n <= 1758
                            then (if n <= 1757 then 0 else 1)
                            else if n <= 1764 then 0 else 1)
                         else
                           if n <= 1769
                           then (if n <= 1768 then 0 else 1)
                           else
                             if n <= 1773
                             then 0
                             else if n <= 1806 then 1 else 0)
                      else
                        if n <= 1866
                        then
                          (if n <= 1809
                           then (if n <= 1808 then 1 else 0)
                           else if n <= 1839 then 1 else 0)
                        else
                          if n <= 1968
                          then (if n <= 1957 then 1 else 0)
                          else
                            if n <= 2026
                            then 1
                            else if n <= 2035 then 0 else 1)
                   else
                     if n <= 2093
                     then
                       (if n <= 2074
                        then
                          (if n <= 2069
                           then (if n <= 2045 then 0 else 1)
                           else if n <= 2073 then 0 else 1)
                        else
                          if n <= 2084
                          then (if n <= 2083 then 0 else 1)
                          else
                            if n <= 2087
                            then 0
                            else if n <= 2088 then 1 else 0)
                     else
                       if n <= 2361
                       then
                         (if n <= 2139
                          then (if n <= 2136 then 1 else 0)
                          else
                            if n <= 2258
                            then 1
                            else if n <= 2306 then 0 else 1)
                       else
                         if n <= 2363
                         then (if n <= 2362 then 0 else 1)
                         else
                           if n <= 2364
                           then 0
                           else if n <= 2368 then 1 else 0)
                else
                  if n <= 2560
                  then
                    (if n <= 2491
                     then
                       (if n <= 2391
                        then
                          (if n <= 2381
                           then (if n <= 2380 then 1 else 0)
                           else if n <= 2384 then 1 else 0)
                        else
                          if n <= 2403
                          then (if n <= 2401 then 1 else 0)
                          else
                            if n <= 2432
                            then 1
                            else if n <= 2433 then 0 else 1)
                     else
                       if n <= 2509
                       then
                         (if n <= 2496
                          then (if n <= 2492 then 0 else 1)
                          else
                            if n <= 2500
                            then 0
                            else if n <= 2508 then 1 else 0)
                       else
                         if n <= 2531
                         then (if n <= 2529 then 1 else 0)
                         else
                           if n <= 2557
                           then 1
                           else if n <= 2558 then 0 else 1)
                  else
                    if n <= 2637
                    then
                      (if n <= 2624
                       then
                         (if n <= 2619
                          then (if n <= 2562 then 0 else 1)
                          else if n <= 2620 then 0 else 1)
                       else
                         if n <= 2630
                         then (if n <= 2626 then 0 else 1)
                         else
                           if n <= 2632
                           then 0
                           else if n <= 2634 then 1 else 0)
                    else
                      if n <= 2676
                      then
                        (if n <= 2641
                         then (if n <= 2640 then 1 else 0)
                         else
                           if n <= 2671
                           then 1
                           else if n <= 2673 then 0 else 1)
                      else
                        if n <= 2688
                        then (if n <= 2677 then 0 else 1)
                        else
                          if n <= 2690 then 0 else if n <= 2747 then 1 else 0)
             else
               if n <= 3144
               then
                 (if n <= 2892
                  then
                    (if n <= 2809
                     then
                       (if n <= 2760
                        then
                          (if n <= 2757
                           then (if n <= 2752 then 1 else 0)
                           else if n <= 2758 then 1 else 0)
                        else
                          if n <= 2765
                          then (if n <= 2764 then 1 else 0)
                          else
                            if n <= 2785
                            then 1
                            else if n <= 2787 then 0 else 1)
                     else
                       if n <= 2876
                       then
                         (if n <= 2816
                          then (if n <= 2815 then 0 else 1)
                          else
                            if n <= 2817
                            then 0
                            else if n <= 2875 then 1 else 0)
                       else
                         if n <= 2879
                         then (if n <= 2878 then 1 else 0)
                         else
                           if n <= 2880
                           then 1
                           else if n <= 2884 then 0 else 1)
                  else
                    if n <= 3008
                    then
                      (if n <= 2913
                       then
                         (if n <= 2901
                          then (if n <= 2893 then 0 else 1)
                          else if n <= 2902 then 0 else 1)
                       else
                         if n <= 2945
                         then (if n <= 2915 then 0 else 1)
                         else
                           if n <= 2946
                           then 0
                           else if n <= 3007 then 1 else 0)
                    else
                      if n <= 3075
                      then
                        (if n <= 3021
                         then (if n <= 3020 then 1 else 0)
                         else
                           if n <= 3071
                           then 1
                           else if n <= 3072 then 0 else 1)
                      else
                        if n <= 3133
                        then (if n <= 3076 then 0 else 1)
                        else
                          if n <= 3136 then 0 else if n <= 3141 then 1 else 0)
               else
                 if n <= 3327
                 then
                   (if n <= 3259
                    then
                      (if n <= 3158
                       then
                         (if n <= 3149
                          then (if n <= 3145 then 1 else 0)
                          else if n <= 3156 then 1 else 0)
                       else
                         if n <= 3171
                         then (if n <= 3169 then 1 else 0)
                         else
                           if n <= 3200
                           then 1
                           else if n <= 3201 then 0 else 1)
                    else
                      if n <= 3270
                      then
                        (if n <= 3262
                         then (if n <= 3260 then 0 else 1)
                         else
                           if n <= 3263
                           then 0
                           else if n <= 3269 then 1 else 0)
                      else
                        if n <= 3277
                        then (if n <= 3275 then 1 else 0)
                        else
                          if n <= 3297 then 1 else if n <= 3299 then 0 else 1)
                 else
                   if n <= 3427
                   then
                     (if n <= 3392
                      then
                        (if n <= 3386
                         then (if n <= 3329 then 0 else 1)
                         else if n <= 3388 then 0 else 1)
                      else
                        if n <= 3404
                        then (if n <= 3396 then 0 else 1)
                        else
                          if n <= 3405 then 0 else if n <= 3425 then 1 else 0)
                   else
                     if n <= 3541
                     then
                       (if n <= 3530
                        then (if n <= 3529 then 1 else 0)
                        else
                          if n <= 3537 then 1 else if n <= 3540 then 0 else 1)
                     else
                       if n <= 3632
                       then (if n <= 3542 then 0 else 1)
                       else
                         if n <= 3633 then 0 else if n <= 3635 then 1 else 0)
        else
          if n <= 7415
          then
            (if n <= 6158
             then
               (if n <= 4183
                then
                  (if n <= 3966
                   then
                     (if n <= 3863
                      then
                        (if n <= 3761
                         then
                           (if n <= 3662
                            then (if n <= 3654 then 1 else 0)
                            else if n <= 3760 then 1 else 0)
                         else
                           if n <= 3772
                           then (if n <= 3763 then 1 else 0)
                           else
                             if n <= 3783
                             then 1
                             else if n <= 3789 then 0 else 1)
                      else
                        if n <= 3894
                        then
                          (if n <= 3892
                           then (if n <= 3865 then 0 else 1)
                           else if n <= 3893 then 0 else 1)
                        else
                          if n <= 3896
                          then (if n <= 3895 then 0 else 1)
                          else
                            if n <= 3897
                            then 0
                            else if n <= 3952 then 1 else 0)
                   else
                     if n <= 4037
                     then
                       (if n <= 3975
                        then
                          (if n <= 3972
                           then (if n <= 3967 then 1 else 0)
                           else if n <= 3973 then 1 else 0)
                        else
                          if n <= 3991
                          then (if n <= 3980 then 1 else 0)
                          else
                            if n <= 3992
                            then 1
                            else if n <= 4028 then 0 else 1)
                     else
                       if n <= 4151
                       then
                         (if n <= 4140
                          then (if n <= 4038 then 0 else 1)
                          else
                            if n <= 4144
                            then 0
                            else if n <= 4145 then 1 else 0)
                       else
                         if n <= 4154
                         then (if n <= 4152 then 1 else 0)
                         else
                           if n <= 4156
                           then 1
                           else if n <= 4158 then 0 else 1)
                else
                  if n <= 5905
                  then
                    (if n <= 4230
                     then
                       (if n <= 4208
                        then
                          (if n <= 4189
                           then (if n <= 4185 then 0 else 1)
                           else if n <= 4192 then 0 else 1)
                        else
                          if n <= 4225
                          then (if n <= 4212 then 0 else 1)
                          else
                            if n <= 4226
                            then 0
                            else if n <= 4228 then 1 else 0)
                     else
                       if n <= 4351
                       then
                         (if n <= 4237
                          then (if n <= 4236 then 1 else 0)
                          else
                            if n <= 4252
                            then 1
                            else if n <= 4253 then 0 else 1)
                       else
                         if n <= 4607
                         then (if n <= 4447 then 2 else 0)
                         else
                           if n <= 4956
                           then 1
                           else if n <= 4959 then 0 else 1)
                  else
                    if n <= 6069
                    then
                      (if n <= 5969
                       then
                         (if n <= 5937
                          then (if n <= 5908 then 0 else 1)
                          else if n <= 5940 then 0 else 1)
                       else
                         if n <= 6001
                         then (if n <= 5971 then 0 else 1)
                         else
                           if n <= 6003
                           then 0
                           else if n <= 6067 then 1 else 0)
                    else
                      if n <= 6088
                      then
                        (if n <= 6077
                         then (if n <= 6070 then 1 else 0)
                         else
                           if n <= 6085
                           then 1
                           else if n <= 6086 then 0 else 1)
                      else
                        if n <= 6108
                        then (if n <= 6099 then 0 else 1)
                        else
                          if n <= 6109 then 0 else if n <= 6154 then 1 else 0)
             else
               if n <= 6965
               then
                 (if n <= 6742
                  then
                    (if n <= 6449
                     then
                       (if n <= 6313
                        then
                          (if n <= 6278
                           then (if n <= 6276 then 1 else 0)
                           else if n <= 6312 then 1 else 0)
                        else
                          if n <= 6434
                          then (if n <= 6431 then 1 else 0)
                          else
                            if n <= 6438
                            then 1
                            else if n <= 6440 then 0 else 1)
                     else
                       if n <= 6678
                       then
                         (if n <= 6456
                          then (if n <= 6450 then 0 else 1)
                          else if n <= 6459 then 0 else 1)
                       else
                         if n <= 6682
                         then (if n <= 6680 then 0 else 1)
                         else
                           if n <= 6683
                           then 0
                           else if n <= 6741 then 1 else 0)
                  else
                    if n <= 6770
                    then
                      (if n <= 6752
                       then
                         (if n <= 6750
                          then (if n <= 6743 then 1 else 0)
                          else if n <= 6751 then 1 else 0)
                       else
                         if n <= 6754
                         then (if n <= 6753 then 1 else 0)
                         else
                           if n <= 6756
                           then 1
                           else if n <= 6764 then 0 else 1)
                    else
                      if n <= 6846
                      then
                        (if n <= 6782
                         then (if n <= 6780 then 0 else 1)
                         else
                           if n <= 6783
                           then 0
                           else if n <= 6831 then 1 else 0)
                      else
                        if n <= 6915
                        then (if n <= 6911 then 1 else 0)
                        else
                          if n <= 6963 then 1 else if n <= 6964 then 0 else 1)
               else
                 if n <= 7145
                 then
                   (if n <= 7041
                    then
                      (if n <= 6977
                       then
                         (if n <= 6971
                          then (if n <= 6970 then 0 else 1)
                          else if n <= 6972 then 0 else 1)
                       else
                         if n <= 7018
                         then (if n <= 6978 then 0 else 1)
                         else
                           if n <= 7027
                           then 0
                           else if n <= 7039 then 1 else 0)
                    else
                      if n <= 7082
                      then
                        (if n <= 7077
                         then (if n <= 7073 then 1 else 0)
                         else
                           if n <= 7079
                           then 1
                           else if n <= 7081 then 0 else 1)
                      else
                        if n <= 7141
                        then (if n <= 7085 then 0 else 1)
                        else
                          if n <= 7142 then 0 else if n <= 7143 then 1 else 0)
                 else
                   if n <= 7375
                   then
                     (if n <= 7153
                      then
                        (if n <= 7149
                         then (if n <= 7148 then 1 else 0)
                         else if n <= 7150 then 1 else 0)
                      else
                        if n <= 7219
                        then (if n <= 7211 then 1 else 0)
                        else
                          if n <= 7221 then 1 else if n <= 7223 then 0 else 1)
                   else
                     if n <= 7400
                     then
                       (if n <= 7379
                        then (if n <= 7378 then 0 else 1)
                        else
                          if n <= 7392 then 0 else if n <= 7393 then 1 else 0)
                     else
                       if n <= 7405
                       then (if n <= 7404 then 1 else 0)
                       else
                         if n <= 7411 then 1 else if n <= 7412 then 0 else 1)
          else
            if n <= 8657
            then
              (if n <= 8318
               then
                 (if n <= 8231
                  then
                    (if n <= 8210
                     then
                       (if n <= 7674
                        then
                          (if n <= 7615
                           then (if n <= 7417 then 0 else 1)
                           else if n <= 7673 then 0 else 1)
                        else
                          if n <= 8202
                          then (if n <= 7679 then 0 else 1)
                          else
                            if n <= 8207
                            then 0
                            else if n <= 8208 then 2 else 1)
                     else
                       if n <= 8219
                       then
                         (if n <= 8215
                          then (if n <= 8214 then 2 else 1)
                          else if n <= 8217 then 2 else 1)
                       else
                         if n <= 8223
                         then (if n <= 8221 then 2 else 1)
                         else
                           if n <= 8226
                           then 2
                           else if n <= 8227 then 1 else 2)
                  else
                    if n <= 8250
                    then
                      (if n <= 8240
                       then
                         (if n <= 8238
                          then (if n <= 8233 then 1 else 0)
                          else if n <= 8239 then 1 else 2)
                       else
                         if n <= 8243
                         then (if n <= 8241 then 1 else 2)
                         else
                           if n <= 8244
                           then 1
                           else if n <= 8245 then 2 else 1)
                    else
                      if n <= 8292
                      then
                        (if n <= 8253
                         then (if n <= 8251 then 2 else 1)
                         else
                           if n <= 8254
                           then 2
                           else if n <= 8287 then 1 else 0)
                      else
                        if n <= 8303
                        then (if n <= 8293 then 1 else 0)
                        else
                          if n <= 8307 then 1 else if n <= 8308 then 2 else 1)
               else
                 if n <= 8482
                 then
                   (if n <= 8451
                    then
                      (if n <= 8363
                       then
                         (if n <= 8320
                          then (if n <= 8319 then 2 else 1)
                          else if n <= 8324 then 2 else 1)
                       else
                         if n <= 8399
                         then (if n <= 8364 then 2 else 1)
                         else
                           if n <= 8432
                           then 0
                           else if n <= 8450 then 1 else 2)
                    else
                      if n <= 8466
                      then
                        (if n <= 8453
                         then (if n <= 8452 then 1 else 2)
                         else
                           if n <= 8456
                           then 1
                           else if n <= 8457 then 2 else 1)
                      else
                        if n <= 8469
                        then (if n <= 8467 then 2 else 1)
                        else
                          if n <= 8470 then 2 else if n <= 8480 then 1 else 2)
                 else
                   if n <= 8543
                   then
                     (if n <= 8491
                      then
                        (if n <= 8486
                         then (if n <= 8485 then 1 else 2)
                         else if n <= 8490 then 1 else 2)
                      else
                        if n <= 8532
                        then (if n <= 8530 then 1 else 2)
                        else
                          if n <= 8538 then 1 else if n <= 8542 then 2 else 1)
                   else
                     if n <= 8585
                     then
                       (if n <= 8559
                        then (if n <= 8555 then 2 else 1)
                        else
                          if n <= 8569 then 2 else if n <= 8584 then 1 else 2)
                     else
                       if n <= 8601
                       then (if n <= 8591 then 1 else 2)
                       else
                         if n <= 8631 then 1 else if n <= 8633 then 2 else 1)
            else
              if n <= 8779
              then
                (if n <= 8725
                 then
                   (if n <= 8707
                    then
                      (if n <= 8678
                       then
                         (if n <= 8659
                          then (if n <= 8658 then 2 else 1)
                          else if n <= 8660 then 2 else 1)
                       else
                         if n <= 8703
                         then (if n <= 8679 then 2 else 1)
                         else
                           if n <= 8704
                           then 2
                           else if n <= 8705 then 1 else 2)
                    else
                      if n <= 8718
                      then
                        (if n <= 8712
                         then (if n <= 8710 then 1 else 2)
                         else
                           if n <= 8714
                           then 1
                           else if n <= 8715 then 2 else 1)
                      else
                        if n <= 8720
                        then (if n <= 8719 then 2 else 1)
                        else
                          if n <= 8721 then 2 else if n <= 8724 then 1 else 2)
                 else
                   if n <= 8742
                   then
                     (if n <= 8736
                      then
                        (if n <= 8730
                         then (if n <= 8729 then 1 else 2)
                         else if n <= 8732 then 1 else 2)
                      else
                        if n <= 8739
                        then (if n <= 8738 then 1 else 2)
                        else
                          if n <= 8740 then 1 else if n <= 8741 then 2 else 1)
                   else
                     if n <= 8759
                     then
                       (if n <= 8749
                        then (if n <= 8748 then 2 else 1)
                        else
                          if n <= 8750 then 2 else if n <= 8755 then 1 else 2)
                     else
                       if n <= 8765
                       then (if n <= 8763 then 1 else 2)
                       else
                         if n <= 8775 then 1 else if n <= 8776 then 2 else 1)
              else
                if n <= 8857
                then
                  (if n <= 8811
                   then
                     (if n <= 8799
                      then
                        (if n <= 8785
                         then (if n <= 8780 then 2 else 1)
                         else if n <= 8786 then 2 else 1)
                      else
                        if n <= 8803
                        then (if n <= 8801 then 2 else 1)
                        else
                          if n <= 8807 then 2 else if n <= 8809 then 1 else 2)
                   else
                     if n <= 8837
                     then
                       (if n <= 8815
                        then (if n <= 8813 then 1 else 2)
                        else
                          if n <= 8833 then 1 else if n <= 8835 then 2 else 1)
                     else
                       if n <= 8852
                       then (if n <= 8839 then 2 else 1)
                       else
                         if n <= 8853 then 2 else if n <= 8856 then 1 else 2)
                else
                  if n <= 9000
                  then
                    (if n <= 8895
                     then
                       (if n <= 8869
                        then (if n <= 8868 then 1 else 2)
                        else if n <= 8894 then 1 else 2)
                     else
                       if n <= 8978
                       then (if n <= 8977 then 1 else 2)
                       else
                         if n <= 8985 then 1 else if n <= 8987 then 2 else 1)
                  else
                    if n <= 9200
                    then
                      (if n <= 9192
                       then (if n <= 9002 then 2 else 1)
                       else
                         if n <= 9196 then 2 else if n <= 9199 then 1 else 2)
                    else
                      if n <= 9203
                      then (if n <= 9202 then 1 else 2)
                      else
                        if n <= 9311 then 1 else if n <= 9449 then 2 else 1)
     else
       if n <= 70197
       then
         (if n <= 42610
          then
            (if n <= 9955
             then
               (if n <= 9743
                then
                  (if n <= 9663
                   then
                     (if n <= 9633
                      then
                        (if n <= 9599
                         then
                           (if n <= 9551
                            then (if n <= 9547 then 2 else 1)
                            else if n <= 9587 then 2 else 1)
                         else
                           if n <= 9617
                           then (if n <= 9615 then 2 else 1)
                           else
                             if n <= 9621
                             then 2
                             else if n <= 9631 then 1 else 2)
                      else
                        if n <= 9651
                        then
                          (if n <= 9641
                           then (if n <= 9634 then 1 else 2)
                           else if n <= 9649 then 1 else 2)
                        else
                          if n <= 9655
                          then (if n <= 9653 then 1 else 2)
                          else
                            if n <= 9659
                            then 1
                            else if n <= 9661 then 2 else 1)
                   else
                     if n <= 9701
                     then
                       (if n <= 9674
                        then
                          (if n <= 9669
                           then (if n <= 9665 then 2 else 1)
                           else if n <= 9672 then 2 else 1)
                        else
                          if n <= 9677
                          then (if n <= 9675 then 2 else 1)
                          else
                            if n <= 9681
                            then 2
                            else if n <= 9697 then 1 else 2)
                     else
                       if n <= 9732
                       then
                         (if n <= 9711
                          then (if n <= 9710 then 1 else 2)
                          else
                            if n <= 9724
                            then 1
                            else if n <= 9726 then 2 else 1)
                       else
                         if n <= 9736
                         then (if n <= 9734 then 2 else 1)
                         else
                           if n <= 9737
                           then 2
                           else if n <= 9741 then 1 else 2)
                else
                  if n <= 9835
                  then
                    (if n <= 9793
                     then
                       (if n <= 9756
                        then
                          (if n <= 9749
                           then (if n <= 9747 then 1 else 2)
                           else if n <= 9755 then 1 else 2)
                        else
                          if n <= 9758
                          then (if n <= 9757 then 1 else 2)
                          else
                            if n <= 9791
                            then 1
                            else if n <= 9792 then 2 else 1)
                     else
                       if n <= 9825
                       then
                         (if n <= 9799
                          then (if n <= 9794 then 2 else 1)
                          else
                            if n <= 9811
                            then 2
                            else if n <= 9823 then 1 else 2)
                       else
                         if n <= 9829
                         then (if n <= 9826 then 1 else 2)
                         else
                           if n <= 9830
                           then 1
                           else if n <= 9834 then 2 else 1)
                  else
                    if n <= 9887
                    then
                      (if n <= 9854
                       then
                         (if n <= 9838
                          then (if n <= 9837 then 2 else 1)
                          else if n <= 9839 then 2 else 1)
                       else
                         if n <= 9874
                         then (if n <= 9855 then 2 else 1)
                         else
                           if n <= 9875
                           then 2
                           else if n <= 9885 then 1 else 2)
                    else
                      if n <= 9916
                      then
                        (if n <= 9889
                         then (if n <= 9888 then 1 else 2)
                         else
                           if n <= 9897
                           then 1
                           else if n <= 9899 then 2 else 1)
                      else
                        if n <= 9923
                        then (if n <= 9919 then 2 else 1)
                        else
                          if n <= 9953 then 2 else if n <= 9954 then 1 else 2)
             else
               if n <= 11743
               then
                 (if n <= 10071
                  then
                    (if n <= 10044
                     then
                       (if n <= 9989
                        then
                          (if n <= 9983
                           then (if n <= 9959 then 1 else 2)
                           else if n <= 9988 then 1 else 2)
                        else
                          if n <= 9995
                          then (if n <= 9993 then 1 else 2)
                          else
                            if n <= 10023
                            then 1
                            else if n <= 10024 then 2 else 1)
                     else
                       if n <= 10061
                       then
                         (if n <= 10059
                          then (if n <= 10045 then 2 else 1)
                          else if n <= 10060 then 2 else 1)
                       else
                         if n <= 10066
                         then (if n <= 10062 then 2 else 1)
                         else
                           if n <= 10069
                           then 2
                           else if n <= 10070 then 1 else 2)
                  else
                    if n <= 11034
                    then
                      (if n <= 10135
                       then
                         (if n <= 10111
                          then (if n <= 10101 then 1 else 2)
                          else if n <= 10132 then 1 else 2)
                       else
                         if n <= 10160
                         then (if n <= 10159 then 1 else 2)
                         else
                           if n <= 10174
                           then 1
                           else if n <= 10175 then 2 else 1)
                    else
                      if n <= 11097
                      then
                        (if n <= 11087
                         then (if n <= 11036 then 2 else 1)
                         else
                           if n <= 11088
                           then 2
                           else if n <= 11092 then 1 else 2)
                      else
                        if n <= 11505
                        then (if n <= 11502 then 1 else 0)
                        else
                          if n <= 11646
                          then 1
                          else if n <= 11647 then 0 else 1)
               else
                 if n <= 12548
                 then
                   (if n <= 12283
                    then
                      (if n <= 11930
                       then
                         (if n <= 11903
                          then (if n <= 11775 then 0 else 1)
                          else if n <= 11929 then 2 else 1)
                       else
                         if n <= 12031
                         then (if n <= 12019 then 2 else 1)
                         else
                           if n <= 12245
                           then 2
                           else if n <= 12271 then 1 else 2)
                    else
                      if n <= 12352
                      then
                        (if n <= 12329
                         then (if n <= 12287 then 1 else 2)
                         else
                           if n <= 12333
                           then 0
                           else if n <= 12350 then 2 else 1)
                      else
                        if n <= 12440
                        then (if n <= 12438 then 2 else 1)
                        else
                          if n <= 12442
                          then 0
                          else if n <= 12543 then 2 else 1)
                 else
                   if n <= 12830
                   then
                     (if n <= 12687
                      then
                        (if n <= 12592
                         then (if n <= 12591 then 2 else 1)
                         else if n <= 12686 then 2 else 1)
                      else
                        if n <= 12735
                        then (if n <= 12730 then 2 else 1)
                        else
                          if n <= 12771
                          then 2
                          else if n <= 12783 then 1 else 2)
                   else
                     if n <= 40959
                     then
                       (if n <= 19893
                        then (if n <= 12831 then 1 else 2)
                        else
                          if n <= 19967
                          then 1
                          else if n <= 40943 then 2 else 1)
                     else
                       if n <= 42127
                       then (if n <= 42124 then 2 else 1)
                       else
                         if n <= 42182
                         then 2
                         else if n <= 42606 then 1 else 0)
          else
            if n <= 64285
            then
              (if n <= 43560
               then
                 (if n <= 43249
                  then
                    (if n <= 43013
                     then
                       (if n <= 42655
                        then
                          (if n <= 42621
                           then (if n <= 42611 then 1 else 0)
                           else if n <= 42653 then 1 else 0)
                        else
                          if n <= 42737
                          then (if n <= 42735 then 1 else 0)
                          else
                            if n <= 43009
                            then 1
                            else if n <= 43010 then 0 else 1)
                     else
                       if n <= 43044
                       then
                         (if n <= 43018
                          then (if n <= 43014 then 0 else 1)
                          else if n <= 43019 then 0 else 1)
                       else
                         if n <= 43203
                         then (if n <= 43046 then 0 else 1)
                         else
                           if n <= 43205
                           then 0
                           else if n <= 43231 then 1 else 0)
                  else
                    if n <= 43391
                    then
                      (if n <= 43309
                       then
                         (if n <= 43263
                          then (if n <= 43262 then 1 else 0)
                          else if n <= 43301 then 1 else 0)
                       else
                         if n <= 43345
                         then (if n <= 43334 then 1 else 0)
                         else
                           if n <= 43359
                           then 1
                           else if n <= 43388 then 2 else 1)
                    else
                      if n <= 43449
                      then
                        (if n <= 43442
                         then (if n <= 43394 then 0 else 1)
                         else
                           if n <= 43443
                           then 0
                           else if n <= 43445 then 1 else 0)
                      else
                        if n <= 43453
                        then (if n <= 43451 then 1 else 0)
                        else
                          if n <= 43492
                          then 1
                          else if n <= 43493 then 0 else 1)
               else
                 if n <= 43711
                 then
                   (if n <= 43596
                    then
                      (if n <= 43572
                       then
                         (if n <= 43568
                          then (if n <= 43566 then 0 else 1)
                          else if n <= 43570 then 0 else 1)
                       else
                         if n <= 43586
                         then (if n <= 43574 then 0 else 1)
                         else
                           if n <= 43587
                           then 0
                           else if n <= 43595 then 1 else 0)
                    else
                      if n <= 43697
                      then
                        (if n <= 43644
                         then (if n <= 43643 then 1 else 0)
                         else
                           if n <= 43695
                           then 1
                           else if n <= 43696 then 0 else 1)
                      else
                        if n <= 43702
                        then (if n <= 43700 then 0 else 1)
                        else
                          if n <= 43704
                          then 0
                          else if n <= 43709 then 1 else 0)
                 else
                   if n <= 44007
                   then
                     (if n <= 43757
                      then
                        (if n <= 43713
                         then (if n <= 43712 then 1 else 0)
                         else if n <= 43755 then 1 else 0)
                      else
                        if n <= 43766
                        then (if n <= 43765 then 1 else 0)
                        else
                          if n <= 44004
                          then 1
                          else if n <= 44005 then 0 else 1)
                   else
                     if n <= 55203
                     then
                       (if n <= 44012
                        then (if n <= 44008 then 0 else 1)
                        else
                          if n <= 44013
                          then 0
                          else if n <= 44031 then 1 else 2)
                     else
                       if n <= 64109
                       then (if n <= 55295 then 1 else 2)
                       else
                         if n <= 64111
                         then 1
                         else if n <= 64217 then 2 else 1)
            else
              if n <= 68324
              then
                (if n <= 65531
                 then
                   (if n <= 65126
                    then
                      (if n <= 65049
                       then
                         (if n <= 65023
                          then (if n <= 64286 then 0 else 1)
                          else if n <= 65039 then 0 else 2)
                       else
                         if n <= 65071
                         then (if n <= 65055 then 1 else 0)
                         else
                           if n <= 65106
                           then 2
                           else if n <= 65107 then 1 else 2)
                    else
                      if n <= 65280
                      then
                        (if n <= 65131
                         then (if n <= 65127 then 1 else 2)
                         else
                           if n <= 65278
                           then 1
                           else if n <= 65279 then 0 else 1)
                      else
                        if n <= 65503
                        then (if n <= 65376 then 2 else 1)
                        else
                          if n <= 65510
                          then 2
                          else if n <= 65528 then 1 else 0)
                 else
                   if n <= 68096
                   then
                     (if n <= 66045
                      then
                        (if n <= 65533
                         then (if n <= 65532 then 1 else 2)
                         else if n <= 66044 then 1 else 0)
                      else
                        if n <= 66272
                        then (if n <= 66271 then 1 else 0)
                        else
                          if n <= 66421
                          then 1
                          else if n <= 66426 then 0 else 1)
                   else
                     if n <= 68111
                     then
                       (if n <= 68100
                        then (if n <= 68099 then 0 else 1)
                        else
                          if n <= 68102
                          then 0
                          else if n <= 68107 then 1 else 0)
                     else
                       if n <= 68154
                       then (if n <= 68151 then 1 else 0)
                       else
                         if n <= 68158
                         then 1
                         else if n <= 68159 then 0 else 1)
              else
                if n <= 69837
                then
                  (if n <= 69702
                   then
                     (if n <= 69445
                      then
                        (if n <= 68899
                         then (if n <= 68326 then 0 else 1)
                         else if n <= 68903 then 0 else 1)
                      else
                        if n <= 69632
                        then (if n <= 69456 then 0 else 1)
                        else
                          if n <= 69633
                          then 0
                          else if n <= 69687 then 1 else 0)
                   else
                     if n <= 69816
                     then
                       (if n <= 69761
                        then (if n <= 69758 then 1 else 0)
                        else
                          if n <= 69810
                          then 1
                          else if n <= 69814 then 0 else 1)
                     else
                       if n <= 69820
                       then (if n <= 69818 then 0 else 1)
                       else
                         if n <= 69821
                         then 0
                         else if n <= 69836 then 1 else 0)
                else
                  if n <= 70015
                  then
                    (if n <= 69931
                     then
                       (if n <= 69890
                        then (if n <= 69887 then 1 else 0)
                        else if n <= 69926 then 1 else 0)
                     else
                       if n <= 69940
                       then (if n <= 69932 then 1 else 0)
                       else
                         if n <= 70002
                         then 1
                         else if n <= 70003 then 0 else 1)
                  else
                    if n <= 70092
                    then
                      (if n <= 70069
                       then (if n <= 70017 then 0 else 1)
                       else
                         if n <= 70078
                         then 0
                         else if n <= 70088 then 1 else 0)
                    else
                      if n <= 70193
                      then (if n <= 70190 then 1 else 0)
                      else
                        if n <= 70195 then 1 else if n <= 70196 then 0 else 1)
       else
         if n <= 119170
         then
           (if n <= 72248
            then
              (if n <= 71101
               then
                 (if n <= 70711
                  then
                    (if n <= 70401
                     then
                       (if n <= 70366
                        then
                          (if n <= 70205
                           then (if n <= 70199 then 0 else 1)
                           else if n <= 70206 then 0 else 1)
                        else
                          if n <= 70370
                          then (if n <= 70367 then 0 else 1)
                          else
                            if n <= 70378
                            then 0
                            else if n <= 70399 then 1 else 0)
                     else
                       if n <= 70464
                       then
                         (if n <= 70460
                          then (if n <= 70458 then 1 else 0)
                          else if n <= 70463 then 1 else 0)
                       else
                         if n <= 70508
                         then (if n <= 70501 then 1 else 0)
                         else
                           if n <= 70511
                           then 1
                           else if n <= 70516 then 0 else 1)
                  else
                    if n <= 70840
                    then
                      (if n <= 70725
                       then
                         (if n <= 70721
                          then (if n <= 70719 then 0 else 1)
                          else if n <= 70724 then 0 else 1)
                       else
                         if n <= 70749
                         then (if n <= 70726 then 0 else 1)
                         else
                           if n <= 70750
                           then 0
                           else if n <= 70834 then 1 else 0)
                    else
                      if n <= 70849
                      then
                        (if n <= 70842
                         then (if n <= 70841 then 1 else 0)
                         else
                           if n <= 70846
                           then 1
                           else if n <= 70848 then 0 else 1)
                      else
                        if n <= 71089
                        then (if n <= 70851 then 0 else 1)
                        else
                          if n <= 71093
                          then 0
                          else if n <= 71099 then 1 else 0)
               else
                 if n <= 71452
                 then
                   (if n <= 71230
                    then
                      (if n <= 71133
                       then
                         (if n <= 71104
                          then (if n <= 71102 then 1 else 0)
                          else if n <= 71131 then 1 else 0)
                       else
                         if n <= 71226
                         then (if n <= 71218 then 1 else 0)
                         else
                           if n <= 71228
                           then 1
                           else if n <= 71229 then 0 else 1)
                    else
                      if n <= 71341
                      then
                        (if n <= 71338
                         then (if n <= 71232 then 0 else 1)
                         else
                           if n <= 71339
                           then 0
                           else if n <= 71340 then 1 else 0)
                      else
                        if n <= 71349
                        then (if n <= 71343 then 1 else 0)
                        else
                          if n <= 71350
                          then 1
                          else if n <= 71351 then 0 else 1)
                 else
                   if n <= 71738
                   then
                     (if n <= 71462
                      then
                        (if n <= 71457
                         then (if n <= 71455 then 0 else 1)
                         else if n <= 71461 then 0 else 1)
                      else
                        if n <= 71726
                        then (if n <= 71467 then 0 else 1)
                        else
                          if n <= 71735
                          then 0
                          else if n <= 71736 then 1 else 0)
                   else
                     if n <= 72159
                     then
                       (if n <= 72151
                        then (if n <= 72147 then 1 else 0)
                        else
                          if n <= 72153
                          then 1
                          else if n <= 72155 then 0 else 1)
                     else
                       if n <= 72192
                       then (if n <= 72160 then 0 else 1)
                       else
                         if n <= 72202
                         then 0
                         else if n <= 72242 then 1 else 0)
            else
              if n <= 73105
              then
                (if n <= 72849
                 then
                   (if n <= 72329
                    then
                      (if n <= 72263
                       then
                         (if n <= 72254
                          then (if n <= 72250 then 1 else 0)
                          else if n <= 72262 then 1 else 0)
                       else
                         if n <= 72278
                         then (if n <= 72272 then 1 else 0)
                         else
                           if n <= 72280
                           then 1
                           else if n <= 72283 then 0 else 1)
                    else
                      if n <= 72758
                      then
                        (if n <= 72343
                         then (if n <= 72342 then 0 else 1)
                         else
                           if n <= 72345
                           then 0
                           else if n <= 72751 then 1 else 0)
                      else
                        if n <= 72765
                        then (if n <= 72759 then 1 else 0)
                        else
                          if n <= 72766
                          then 1
                          else if n <= 72767 then 0 else 1)
                 else
                   if n <= 73014
                   then
                     (if n <= 72881
                      then
                        (if n <= 72873
                         then (if n <= 72871 then 0 else 1)
                         else if n <= 72880 then 0 else 1)
                      else
                        if n <= 72884
                        then (if n <= 72883 then 0 else 1)
                        else
                          if n <= 72886
                          then 0
                          else if n <= 73008 then 1 else 0)
                   else
                     if n <= 73022
                     then
                       (if n <= 73018
                        then (if n <= 73017 then 1 else 0)
                        else
                          if n <= 73019
                          then 1
                          else if n <= 73021 then 0 else 1)
                     else
                       if n <= 73030
                       then (if n <= 73029 then 0 else 1)
                       else
                         if n <= 73031
                         then 0
                         else if n <= 73103 then 1 else 0)
              else
                if n <= 94207
                then
                  (if n <= 92911
                   then
                     (if n <= 73111
                      then
                        (if n <= 73109
                         then (if n <= 73108 then 1 else 0)
                         else if n <= 73110 then 1 else 0)
                      else
                        if n <= 73460
                        then (if n <= 73458 then 1 else 0)
                        else
                          if n <= 78895
                          then 1
                          else if n <= 78904 then 0 else 1)
                   else
                     if n <= 94031
                     then
                       (if n <= 92975
                        then (if n <= 92916 then 0 else 1)
                        else
                          if n <= 92982
                          then 0
                          else if n <= 94030 then 1 else 0)
                     else
                       if n <= 94098
                       then (if n <= 94094 then 1 else 0)
                       else
                         if n <= 94175
                         then 1
                         else if n <= 94179 then 2 else 1)
                else
                  if n <= 110951
                  then
                    (if n <= 110591
                     then
                       (if n <= 100351
                        then (if n <= 100343 then 2 else 1)
                        else if n <= 101106 then 2 else 1)
                     else
                       if n <= 110927
                       then (if n <= 110878 then 2 else 1)
                       else
                         if n <= 110930
                         then 2
                         else if n <= 110947 then 1 else 2)
                  else
                    if n <= 113823
                    then
                      (if n <= 111355
                       then (if n <= 110959 then 1 else 2)
                       else
                         if n <= 113820
                         then 1
                         else if n <= 113822 then 0 else 1)
                    else
                      if n <= 119142
                      then (if n <= 113827 then 0 else 1)
                      else
                        if n <= 119145
                        then 0
                        else if n <= 119154 then 1 else 0)
         else
           if n <= 127991
           then
             (if n <= 126979
              then
                (if n <= 121519
                 then
                   (if n <= 121402
                    then
                      (if n <= 119213
                       then
                         (if n <= 119179
                          then (if n <= 119172 then 1 else 0)
                          else if n <= 119209 then 1 else 0)
                       else
                         if n <= 119364
                         then (if n <= 119361 then 1 else 0)
                         else
                           if n <= 121343
                           then 1
                           else if n <= 121398 then 0 else 1)
                    else
                      if n <= 121475
                      then
                        (if n <= 121460
                         then (if n <= 121452 then 0 else 1)
                         else if n <= 121461 then 0 else 1)
                      else
                        if n <= 121498
                        then (if n <= 121476 then 0 else 1)
                        else
                          if n <= 121503
                          then 0
                          else if n <= 121504 then 1 else 0)
                 else
                   if n <= 122917
                   then
                     (if n <= 122904
                      then
                        (if n <= 122886
                         then (if n <= 122879 then 1 else 0)
                         else if n <= 122887 then 1 else 0)
                      else
                        if n <= 122913
                        then (if n <= 122906 then 1 else 0)
                        else
                          if n <= 122914
                          then 1
                          else if n <= 122916 then 0 else 1)
                   else
                     if n <= 123631
                     then
                       (if n <= 123183
                        then (if n <= 122922 then 0 else 1)
                        else
                          if n <= 123190
                          then 0
                          else if n <= 123627 then 1 else 0)
                     else
                       if n <= 125142
                       then (if n <= 125135 then 1 else 0)
                       else
                         if n <= 125251
                         then 1
                         else if n <= 125258 then 0 else 1)
              else
                if n <= 127569
                then
                  (if n <= 127337
                   then
                     (if n <= 127231
                      then
                        (if n <= 127182
                         then (if n <= 126980 then 2 else 1)
                         else if n <= 127183 then 2 else 1)
                      else
                        if n <= 127247
                        then (if n <= 127242 then 2 else 1)
                        else
                          if n <= 127277
                          then 2
                          else if n <= 127279 then 1 else 2)
                   else
                     if n <= 127503
                     then
                       (if n <= 127404
                        then (if n <= 127343 then 1 else 2)
                        else
                          if n <= 127487
                          then 1
                          else if n <= 127490 then 2 else 1)
                     else
                       if n <= 127551
                       then (if n <= 127547 then 2 else 1)
                       else
                         if n <= 127560
                         then 2
                         else if n <= 127567 then 1 else 2)
                else
                  if n <= 127869
                  then
                    (if n <= 127776
                     then
                       (if n <= 127589
                        then (if n <= 127583 then 1 else 2)
                        else if n <= 127743 then 1 else 2)
                     else
                       if n <= 127797
                       then (if n <= 127788 then 1 else 2)
                       else
                         if n <= 127798
                         then 1
                         else if n <= 127868 then 2 else 1)
                  else
                    if n <= 127955
                    then
                      (if n <= 127903
                       then (if n <= 127891 then 2 else 1)
                       else
                         if n <= 127946
                         then 2
                         else if n <= 127950 then 1 else 2)
                    else
                      if n <= 127984
                      then (if n <= 127967 then 1 else 2)
                      else
                        if n <= 127987
                        then 1
                        else if n <= 127988 then 2 else 1)
           else
             if n <= 129401
             then
               (if n <= 128591
                then
                  (if n <= 128334
                   then
                     (if n <= 128065
                      then
                        (if n <= 128063
                         then (if n <= 128062 then 2 else 1)
                         else if n <= 128064 then 2 else 1)
                      else
                        if n <= 128254
                        then (if n <= 128252 then 2 else 1)
                        else
                          if n <= 128317
                          then 2
                          else if n <= 128330 then 1 else 2)
                   else
                     if n <= 128404
                     then
                       (if n <= 128359
                        then (if n <= 128335 then 1 else 2)
                        else
                          if n <= 128377
                          then 1
                          else if n <= 128378 then 2 else 1)
                     else
                       if n <= 128419
                       then (if n <= 128406 then 2 else 1)
                       else
                         if n <= 128420
                         then 2
                         else if n <= 128506 then 1 else 2)
                else
                  if n <= 128746
                  then
                    (if n <= 128716
                     then
                       (if n <= 128709
                        then (if n <= 128639 then 1 else 2)
                        else if n <= 128715 then 1 else 2)
                     else
                       if n <= 128722
                       then (if n <= 128719 then 1 else 2)
                       else
                         if n <= 128724
                         then 1
                         else if n <= 128725 then 2 else 1)
                  else
                    if n <= 129003
                    then
                      (if n <= 128755
                       then (if n <= 128748 then 2 else 1)
                       else
                         if n <= 128762
                         then 2
                         else if n <= 128991 then 1 else 2)
                    else
                      if n <= 129393
                      then (if n <= 129292 then 1 else 2)
                      else
                        if n <= 129394
                        then 1
                        else if n <= 129398 then 2 else 1)
             else
               if n <= 177972
               then
                 (if n <= 129651
                  then
                    (if n <= 129453
                     then
                       (if n <= 129444
                        then (if n <= 129442 then 2 else 1)
                        else if n <= 129450 then 2 else 1)
                     else
                       if n <= 129484
                       then (if n <= 129482 then 2 else 1)
                       else
                         if n <= 129535
                         then 2
                         else if n <= 129647 then 1 else 2)
                  else
                    if n <= 129679
                    then
                      (if n <= 129658
                       then (if n <= 129655 then 1 else 2)
                       else
                         if n <= 129663
                         then 1
                         else if n <= 129666 then 2 else 1)
                    else
                      if n <= 131071
                      then (if n <= 129685 then 2 else 1)
                      else
                        if n <= 173782
                        then 2
                        else if n <= 173823 then 1 else 2)
               else
                 if n <= 917504
                 then
                   (if n <= 183969
                    then
                      (if n <= 178205
                       then (if n <= 177983 then 1 else 2)
                       else if n <= 178207 then 1 else 2)
                    else
                      if n <= 191456
                      then (if n <= 183983 then 1 else 2)
                      else
                        if n <= 194559
                        then 1
                        else if n <= 195101 then 2 else 1)
                 else
                   if n <= 917999
                   then
                     (if n <= 917535
                      then (if n <= 917505 then 0 else 1)
                      else
                        if n <= 917631
                        then 0
                        else if n <= 917759 then 1 else 0)
                   else
                     if n <= 1048573
                     then (if n <= 983039 then 1 else 2)
                     else
                       if n <= 1048575
                       then 1
                       else if n <= 1114109 then 2 else 1)
  else
    if n <= 12879
    then
      (if n <= 4225
       then
         (if n <= 2765
          then
            (if n <= 2087
             then
               (if n <= 1631
                then
                  (if n <= 1471
                   then
                     (if n <= 767
                      then
                        (if n <= 126
                         then
                           (if n <= 0 then 0 else if n <= 31 then (-1) else 1)
                         else
                           if n <= 172
                           then (if n <= 159 then (-1) else 1)
                           else if n <= 173 then 0 else 1)
                      else
                        if n <= 1161
                        then
                          (if n <= 879 then 0 else if n <= 1154 then 1 else 0)
                        else
                          if n <= 1469
                          then (if n <= 1424 then 1 else 0)
                          else if n <= 1470 then 1 else 0)
                   else
                     if n <= 1535
                     then
                       (if n <= 1475
                        then
                          (if n <= 1472
                           then 1
                           else if n <= 1474 then 0 else 1)
                        else
                          if n <= 1478
                          then (if n <= 1477 then 0 else 1)
                          else if n <= 1479 then 0 else 1)
                     else
                       if n <= 1562
                       then
                         (if n <= 1541 then 0 else if n <= 1551 then 1 else 0)
                       else
                         if n <= 1564
                         then (if n <= 1563 then 1 else 0)
                         else if n <= 1610 then 1 else 0)
                else
                  if n <= 1809
                  then
                    (if n <= 1766
                     then
                       (if n <= 1749
                        then
                          (if n <= 1647
                           then 1
                           else if n <= 1648 then 0 else 1)
                        else
                          if n <= 1758
                          then (if n <= 1757 then 0 else 1)
                          else if n <= 1764 then 0 else 1)
                     else
                       if n <= 1773
                       then
                         (if n <= 1768 then 0 else if n <= 1769 then 1 else 0)
                       else
                         if n <= 1807
                         then (if n <= 1806 then 1 else 0)
                         else if n <= 1808 then 1 else 0)
                  else
                    if n <= 2044
                    then
                      (if n <= 1957
                       then
                         (if n <= 1839 then 1 else if n <= 1866 then 0 else 1)
                       else
                         if n <= 2026
                         then (if n <= 1968 then 0 else 1)
                         else if n <= 2035 then 0 else 1)
                    else
                      if n <= 2073
                      then
                        (if n <= 2045 then 0 else if n <= 2069 then 1 else 0)
                      else
                        if n <= 2083
                        then (if n <= 2074 then 1 else 0)
                        else if n <= 2084 then 1 else 0)
             else
               if n <= 2531
               then
                 (if n <= 2381
                  then
                    (if n <= 2361
                     then
                       (if n <= 2136
                        then
                          (if n <= 2088
                           then 1
                           else if n <= 2093 then 0 else 1)
                        else
                          if n <= 2258
                          then (if n <= 2139 then 0 else 1)
                          else if n <= 2306 then 0 else 1)
                     else
                       if n <= 2364
                       then
                         (if n <= 2362 then 0 else if n <= 2363 then 1 else 0)
                       else
                         if n <= 2376
                         then (if n <= 2368 then 1 else 0)
                         else if n <= 2380 then 1 else 0)
                  else
                    if n <= 2491
                    then
                      (if n <= 2401
                       then
                         (if n <= 2384 then 1 else if n <= 2391 then 0 else 1)
                       else
                         if n <= 2432
                         then (if n <= 2403 then 0 else 1)
                         else if n <= 2433 then 0 else 1)
                    else
                      if n <= 2500
                      then
                        (if n <= 2492 then 0 else if n <= 2496 then 1 else 0)
                      else
                        if n <= 2509
                        then (if n <= 2508 then 1 else 0)
                        else if n <= 2529 then 1 else 0)
               else
                 if n <= 2641
                 then
                   (if n <= 2624
                    then
                      (if n <= 2560
                       then
                         (if n <= 2557 then 1 else if n <= 2558 then 0 else 1)
                       else
                         if n <= 2619
                         then (if n <= 2562 then 0 else 1)
                         else if n <= 2620 then 0 else 1)
                    else
                      if n <= 2632
                      then
                        (if n <= 2626 then 0 else if n <= 2630 then 1 else 0)
                      else
                        if n <= 2637
                        then (if n <= 2634 then 1 else 0)
                        else if n <= 2640 then 1 else 0)
                 else
                   if n <= 2747
                   then
                     (if n <= 2676
                      then
                        (if n <= 2671 then 1 else if n <= 2673 then 0 else 1)
                      else
                        if n <= 2688
                        then (if n <= 2677 then 0 else 1)
                        else if n <= 2690 then 0 else 1)
                   else
                     if n <= 2757
                     then
                       (if n <= 2748 then 0 else if n <= 2752 then 1 else 0)
                     else
                       if n <= 2760
                       then (if n <= 2758 then 1 else 0)
                       else if n <= 2764 then 1 else 0)
          else
            if n <= 3396
            then
              (if n <= 3076
               then
                 (if n <= 2893
                  then
                    (if n <= 2875
                     then
                       (if n <= 2809
                        then
                          (if n <= 2785
                           then 1
                           else if n <= 2787 then 0 else 1)
                        else
                          if n <= 2816
                          then (if n <= 2815 then 0 else 1)
                          else if n <= 2817 then 0 else 1)
                     else
                       if n <= 2879
                       then
                         (if n <= 2876 then 0 else if n <= 2878 then 1 else 0)
                       else
                         if n <= 2884
                         then (if n <= 2880 then 1 else 0)
                         else if n <= 2892 then 1 else 0)
                  else
                    if n <= 3007
                    then
                      (if n <= 2913
                       then
                         (if n <= 2901 then 1 else if n <= 2902 then 0 else 1)
                       else
                         if n <= 2945
                         then (if n <= 2915 then 0 else 1)
                         else if n <= 2946 then 0 else 1)
                    else
                      if n <= 3021
                      then
                        (if n <= 3008 then 0 else if n <= 3020 then 1 else 0)
                      else
                        if n <= 3072
                        then (if n <= 3071 then 1 else 0)
                        else if n <= 3075 then 1 else 0)
               else
                 if n <= 3260
                 then
                   (if n <= 3156
                    then
                      (if n <= 3141
                       then
                         (if n <= 3133 then 1 else if n <= 3136 then 0 else 1)
                       else
                         if n <= 3145
                         then (if n <= 3144 then 0 else 1)
                         else if n <= 3149 then 0 else 1)
                    else
                      if n <= 3171
                      then
                        (if n <= 3158 then 0 else if n <= 3169 then 1 else 0)
                      else
                        if n <= 3201
                        then (if n <= 3200 then 1 else 0)
                        else if n <= 3259 then 1 else 0)
                 else
                   if n <= 3297
                   then
                     (if n <= 3269
                      then
                        (if n <= 3262 then 1 else if n <= 3263 then 0 else 1)
                      else
                        if n <= 3275
                        then (if n <= 3270 then 0 else 1)
                        else if n <= 3277 then 0 else 1)
                   else
                     if n <= 3329
                     then
                       (if n <= 3299 then 0 else if n <= 3327 then 1 else 0)
                     else
                       if n <= 3388
                       then (if n <= 3386 then 1 else 0)
                       else if n <= 3392 then 1 else 0)
            else
              if n <= 3895
              then
                (if n <= 3642
                 then
                   (if n <= 3537
                    then
                      (if n <= 3425
                       then
                         (if n <= 3404 then 1 else if n <= 3405 then 0 else 1)
                       else
                         if n <= 3529
                         then (if n <= 3427 then 0 else 1)
                         else if n <= 3530 then 0 else 1)
                    else
                      if n <= 3542
                      then
                        (if n <= 3540 then 0 else if n <= 3541 then 1 else 0)
                      else
                        if n <= 3633
                        then (if n <= 3632 then 1 else 0)
                        else if n <= 3635 then 1 else 0)
                 else
                   if n <= 3783
                   then
                     (if n <= 3760
                      then
                        (if n <= 3654 then 1 else if n <= 3662 then 0 else 1)
                      else
                        if n <= 3763
                        then (if n <= 3761 then 0 else 1)
                        else if n <= 3772 then 0 else 1)
                   else
                     if n <= 3865
                     then
                       (if n <= 3789 then 0 else if n <= 3863 then 1 else 0)
                     else
                       if n <= 3893
                       then (if n <= 3892 then 1 else 0)
                       else if n <= 3894 then 1 else 0)
              else
                if n <= 4038
                then
                  (if n <= 3973
                   then
                     (if n <= 3952
                      then
                        (if n <= 3896 then 1 else if n <= 3897 then 0 else 1)
                      else
                        if n <= 3967
                        then (if n <= 3966 then 0 else 1)
                        else if n <= 3972 then 0 else 1)
                   else
                     if n <= 3991
                     then
                       (if n <= 3975 then 0 else if n <= 3980 then 1 else 0)
                     else
                       if n <= 4028
                       then (if n <= 3992 then 1 else 0)
                       else if n <= 4037 then 1 else 0)
                else
                  if n <= 4156
                  then
                    (if n <= 4145
                     then
                       (if n <= 4140 then 1 else if n <= 4144 then 0 else 1)
                     else
                       if n <= 4152
                       then (if n <= 4151 then 0 else 1)
                       else if n <= 4154 then 0 else 1)
                  else
                    if n <= 4189
                    then
                      (if n <= 4183
                       then (if n <= 4158 then 0 else 1)
                       else if n <= 4185 then 0 else 1)
                    else
                      if n <= 4208
                      then (if n <= 4192 then 0 else 1)
                      else if n <= 4212 then 0 else 1)
       else
         if n <= 8202
         then
           (if n <= 6754
            then
              (if n <= 6099
               then
                 (if n <= 5908
                  then
                    (if n <= 4253
                     then
                       (if n <= 4230
                        then
                          (if n <= 4226
                           then 0
                           else if n <= 4228 then 1 else 0)
                        else
                          if n <= 4237
                          then (if n <= 4236 then 1 else 0)
                          else if n <= 4252 then 1 else 0)
                     else
                       if n <= 4607
                       then
                         (if n <= 4351 then 1 else if n <= 4447 then 2 else 0)
                       else
                         if n <= 4959
                         then (if n <= 4956 then 1 else 0)
                         else if n <= 5905 then 1 else 0)
                  else
                    if n <= 6067
                    then
                      (if n <= 5969
                       then
                         (if n <= 5937 then 1 else if n <= 5940 then 0 else 1)
                       else
                         if n <= 6001
                         then (if n <= 5971 then 0 else 1)
                         else if n <= 6003 then 0 else 1)
                    else
                      if n <= 6077
                      then
                        (if n <= 6069 then 0 else if n <= 6070 then 1 else 0)
                      else
                        if n <= 6086
                        then (if n <= 6085 then 1 else 0)
                        else if n <= 6088 then 1 else 0)
               else
                 if n <= 6450
                 then
                   (if n <= 6312
                    then
                      (if n <= 6154
                       then
                         (if n <= 6108 then 1 else if n <= 6109 then 0 else 1)
                       else
                         if n <= 6276
                         then (if n <= 6158 then 0 else 1)
                         else if n <= 6278 then 0 else 1)
                    else
                      if n <= 6434
                      then
                        (if n <= 6313 then 0 else if n <= 6431 then 1 else 0)
                      else
                        if n <= 6440
                        then (if n <= 6438 then 1 else 0)
                        else if n <= 6449 then 1 else 0)
                 else
                   if n <= 6741
                   then
                     (if n <= 6678
                      then
                        (if n <= 6456 then 1 else if n <= 6459 then 0 else 1)
                      else
                        if n <= 6682
                        then (if n <= 6680 then 0 else 1)
                        else if n <= 6683 then 0 else 1)
                   else
                     if n <= 6750
                     then
                       (if n <= 6742 then 0 else if n <= 6743 then 1 else 0)
                     else
                       if n <= 6752
                       then (if n <= 6751 then 1 else 0)
                       else if n <= 6753 then 1 else 0)
            else
              if n <= 7085
              then
                (if n <= 6970
                 then
                   (if n <= 6831
                    then
                      (if n <= 6770
                       then
                         (if n <= 6756 then 1 else if n <= 6764 then 0 else 1)
                       else
                         if n <= 6782
                         then (if n <= 6780 then 0 else 1)
                         else if n <= 6783 then 0 else 1)
                    else
                      if n <= 6915
                      then
                        (if n <= 6846 then 0 else if n <= 6911 then 1 else 0)
                      else
                        if n <= 6964
                        then (if n <= 6963 then 1 else 0)
                        else if n <= 6965 then 1 else 0)
                 else
                   if n <= 7039
                   then
                     (if n <= 6977
                      then
                        (if n <= 6971 then 1 else if n <= 6972 then 0 else 1)
                      else
                        if n <= 7018
                        then (if n <= 6978 then 0 else 1)
                        else if n <= 7027 then 0 else 1)
                   else
                     if n <= 7077
                     then
                       (if n <= 7041 then 0 else if n <= 7073 then 1 else 0)
                     else
                       if n <= 7081
                       then (if n <= 7079 then 1 else 0)
                       else if n <= 7082 then 1 else 0)
              else
                if n <= 7378
                then
                  (if n <= 7150
                   then
                     (if n <= 7143
                      then
                        (if n <= 7141 then 1 else if n <= 7142 then 0 else 1)
                      else
                        if n <= 7148
                        then (if n <= 7145 then 0 else 1)
                        else if n <= 7149 then 0 else 1)
                   else
                     if n <= 7219
                     then
                       (if n <= 7153 then 0 else if n <= 7211 then 1 else 0)
                     else
                       if n <= 7223
                       then (if n <= 7221 then 1 else 0)
                       else if n <= 7375 then 1 else 0)
                else
                  if n <= 7411
                  then
                    (if n <= 7393
                     then
                       (if n <= 7379 then 1 else if n <= 7392 then 0 else 1)
                     else
                       if n <= 7404
                       then (if n <= 7400 then 0 else 1)
                       else if n <= 7405 then 0 else 1)
                  else
                    if n <= 7615
                    then
                      (if n <= 7415
                       then (if n <= 7412 then 0 else 1)
                       else if n <= 7417 then 0 else 1)
                    else
                      if n <= 7674
                      then (if n <= 7673 then 0 else 1)
                      else if n <= 7679 then 0 else 1)
         else
           if n <= 10023
           then
             (if n <= 9874
              then
                (if n <= 9192
                 then
                   (if n <= 8303
                    then
                      (if n <= 8238
                       then
                         (if n <= 8207 then 0 else if n <= 8233 then 1 else 0)
                       else
                         if n <= 8292
                         then (if n <= 8287 then 1 else 0)
                         else if n <= 8293 then 1 else 0)
                    else
                      if n <= 8985
                      then
                        (if n <= 8399 then 1 else if n <= 8432 then 0 else 1)
                      else
                        if n <= 9000
                        then (if n <= 8987 then 2 else 1)
                        else if n <= 9002 then 2 else 1)
                 else
                   if n <= 9726
                   then
                     (if n <= 9200
                      then
                        (if n <= 9196 then 2 else if n <= 9199 then 1 else 2)
                      else
                        if n <= 9203
                        then (if n <= 9202 then 1 else 2)
                        else if n <= 9724 then 1 else 2)
                   else
                     if n <= 9799
                     then
                       (if n <= 9747 then 1 else if n <= 9749 then 2 else 1)
                     else
                       if n <= 9854
                       then (if n <= 9811 then 2 else 1)
                       else if n <= 9855 then 2 else 1)
              else
                if n <= 9961
                then
                  (if n <= 9918
                   then
                     (if n <= 9889
                      then
                        (if n <= 9875 then 2 else if n <= 9888 then 1 else 2)
                      else
                        if n <= 9899
                        then (if n <= 9897 then 1 else 2)
                        else if n <= 9916 then 1 else 2)
                   else
                     if n <= 9933
                     then
                       (if n <= 9923 then 1 else if n <= 9925 then 2 else 1)
                     else
                       if n <= 9939
                       then (if n <= 9934 then 2 else 1)
                       else if n <= 9940 then 2 else 1)
                else
                  if n <= 9978
                  then
                    (if n <= 9971
                     then
                       (if n <= 9962 then 2 else if n <= 9969 then 1 else 2)
                     else
                       if n <= 9973
                       then (if n <= 9972 then 1 else 2)
                       else if n <= 9977 then 1 else 2)
                  else
                    if n <= 9988
                    then
                      (if n <= 9980 then 1 else if n <= 9981 then 2 else 1)
                    else
                      if n <= 9993
                      then (if n <= 9989 then 2 else 1)
                      else if n <= 9995 then 2 else 1)
           else
             if n <= 11903
             then
               (if n <= 10174
                then
                  (if n <= 10069
                   then
                     (if n <= 10060
                      then
                        (if n <= 10024
                         then 2
                         else if n <= 10059 then 1 else 2)
                      else
                        if n <= 10062
                        then (if n <= 10061 then 1 else 2)
                        else if n <= 10066 then 1 else 2)
                   else
                     if n <= 10132
                     then
                       (if n <= 10070 then 1 else if n <= 10071 then 2 else 1)
                     else
                       if n <= 10159
                       then (if n <= 10135 then 2 else 1)
                       else if n <= 10160 then 2 else 1)
                else
                  if n <= 11093
                  then
                    (if n <= 11036
                     then
                       (if n <= 10175 then 2 else if n <= 11034 then 1 else 2)
                     else
                       if n <= 11088
                       then (if n <= 11087 then 1 else 2)
                       else if n <= 11092 then 1 else 2)
                  else
                    if n <= 11646
                    then
                      (if n <= 11502 then 1 else if n <= 11505 then 0 else 1)
                    else
                      if n <= 11743
                      then (if n <= 11647 then 0 else 1)
                      else if n <= 11775 then 0 else 1)
             else
               if n <= 12440
               then
                 (if n <= 12283
                  then
                    (if n <= 12019
                     then
                       (if n <= 11929 then 2 else if n <= 11930 then 1 else 2)
                     else
                       if n <= 12245
                       then (if n <= 12031 then 1 else 2)
                       else if n <= 12271 then 1 else 2)
                  else
                    if n <= 12333
                    then
                      (if n <= 12287 then 1 else if n <= 12329 then 2 else 0)
                    else
                      if n <= 12352
                      then (if n <= 12350 then 2 else 1)
                      else if n <= 12438 then 2 else 1)
               else
                 if n <= 12687
                 then
                   (if n <= 12548
                    then
                      (if n <= 12442 then 0 else if n <= 12543 then 2 else 1)
                    else
                      if n <= 12592
                      then (if n <= 12591 then 2 else 1)
                      else if n <= 12686 then 2 else 1)
                 else
                   if n <= 12783
                   then
                     (if n <= 12735
                      then (if n <= 12730 then 2 else 1)
                      else if n <= 12771 then 2 else 1)
                   else
                     if n <= 12831
                     then (if n <= 12830 then 2 else 1)
                     else if n <= 12871 then 2 else 1)
    else
      if n <= 72153
      then
        (if n <= 68102
         then
           (if n <= 43643
            then
              (if n <= 43262
               then
                 (if n <= 42735
                  then
                    (if n <= 42182
                     then
                       (if n <= 40943
                        then
                          (if n <= 19893
                           then 2
                           else if n <= 19967 then 1 else 2)
                        else
                          if n <= 42124
                          then (if n <= 40959 then 1 else 2)
                          else if n <= 42127 then 1 else 2)
                     else
                       if n <= 42611
                       then
                         (if n <= 42606
                          then 1
                          else if n <= 42610 then 0 else 1)
                       else
                         if n <= 42653
                         then (if n <= 42621 then 0 else 1)
                         else if n <= 42655 then 0 else 1)
                  else
                    if n <= 43019
                    then
                      (if n <= 43010
                       then
                         (if n <= 42737
                          then 0
                          else if n <= 43009 then 1 else 0)
                       else
                         if n <= 43014
                         then (if n <= 43013 then 1 else 0)
                         else if n <= 43018 then 1 else 0)
                    else
                      if n <= 43203
                      then
                        (if n <= 43044
                         then 1
                         else if n <= 43046 then 0 else 1)
                      else
                        if n <= 43231
                        then (if n <= 43205 then 0 else 1)
                        else if n <= 43249 then 0 else 1)
               else
                 if n <= 43451
                 then
                   (if n <= 43388
                    then
                      (if n <= 43309
                       then
                         (if n <= 43263
                          then 0
                          else if n <= 43301 then 1 else 0)
                       else
                         if n <= 43345
                         then (if n <= 43334 then 1 else 0)
                         else if n <= 43359 then 1 else 2)
                    else
                      if n <= 43442
                      then
                        (if n <= 43391
                         then 1
                         else if n <= 43394 then 0 else 1)
                      else
                        if n <= 43445
                        then (if n <= 43443 then 0 else 1)
                        else if n <= 43449 then 0 else 1)
                 else
                   if n <= 43570
                   then
                     (if n <= 43493
                      then
                        (if n <= 43453
                         then 0
                         else if n <= 43492 then 1 else 0)
                      else
                        if n <= 43566
                        then (if n <= 43560 then 1 else 0)
                        else if n <= 43568 then 1 else 0)
                   else
                     if n <= 43586
                     then
                       (if n <= 43572 then 1 else if n <= 43574 then 0 else 1)
                     else
                       if n <= 43595
                       then (if n <= 43587 then 0 else 1)
                       else if n <= 43596 then 0 else 1)
            else
              if n <= 64285
              then
                (if n <= 43765
                 then
                   (if n <= 43704
                    then
                      (if n <= 43696
                       then
                         (if n <= 43644
                          then 0
                          else if n <= 43695 then 1 else 0)
                       else
                         if n <= 43700
                         then (if n <= 43697 then 1 else 0)
                         else if n <= 43702 then 1 else 0)
                    else
                      if n <= 43712
                      then
                        (if n <= 43709
                         then 1
                         else if n <= 43711 then 0 else 1)
                      else
                        if n <= 43755
                        then (if n <= 43713 then 0 else 1)
                        else if n <= 43757 then 0 else 1)
                 else
                   if n <= 44013
                   then
                     (if n <= 44005
                      then
                        (if n <= 43766
                         then 0
                         else if n <= 44004 then 1 else 0)
                      else
                        if n <= 44008
                        then (if n <= 44007 then 1 else 0)
                        else if n <= 44012 then 1 else 0)
                   else
                     if n <= 63743
                     then
                       (if n <= 44031 then 1 else if n <= 55203 then 2 else 1)
                     else
                       if n <= 64111
                       then (if n <= 64109 then 2 else 1)
                       else if n <= 64217 then 2 else 1)
              else
                if n <= 65280
                then
                  (if n <= 65106
                   then
                     (if n <= 65039
                      then
                        (if n <= 64286
                         then 0
                         else if n <= 65023 then 1 else 0)
                      else
                        if n <= 65055
                        then (if n <= 65049 then 2 else 1)
                        else if n <= 65071 then 0 else 2)
                   else
                     if n <= 65127
                     then
                       (if n <= 65107 then 1 else if n <= 65126 then 2 else 1)
                     else
                       if n <= 65278
                       then (if n <= 65131 then 2 else 1)
                       else if n <= 65279 then 0 else 1)
                else
                  if n <= 66045
                  then
                    (if n <= 65510
                     then
                       (if n <= 65376 then 2 else if n <= 65503 then 1 else 2)
                     else
                       if n <= 65531
                       then (if n <= 65528 then 1 else 0)
                       else if n <= 66044 then 1 else 0)
                  else
                    if n <= 66426
                    then
                      (if n <= 66272
                       then (if n <= 66271 then 1 else 0)
                       else if n <= 66421 then 1 else 0)
                    else
                      if n <= 68099
                      then (if n <= 68096 then 1 else 0)
                      else if n <= 68100 then 1 else 0)
         else
           if n <= 70460
           then
             (if n <= 69890
              then
                (if n <= 69633
                 then
                   (if n <= 68324
                    then
                      (if n <= 68151
                       then
                         (if n <= 68107
                          then 1
                          else if n <= 68111 then 0 else 1)
                       else
                         if n <= 68158
                         then (if n <= 68154 then 0 else 1)
                         else if n <= 68159 then 0 else 1)
                    else
                      if n <= 68903
                      then
                        (if n <= 68326
                         then 0
                         else if n <= 68899 then 1 else 0)
                      else
                        if n <= 69456
                        then (if n <= 69445 then 1 else 0)
                        else if n <= 69632 then 1 else 0)
                 else
                   if n <= 69816
                   then
                     (if n <= 69758
                      then
                        (if n <= 69687
                         then 1
                         else if n <= 69702 then 0 else 1)
                      else
                        if n <= 69810
                        then (if n <= 69761 then 0 else 1)
                        else if n <= 69814 then 0 else 1)
                   else
                     if n <= 69821
                     then
                       (if n <= 69818 then 0 else if n <= 69820 then 1 else 0)
                     else
                       if n <= 69837
                       then (if n <= 69836 then 1 else 0)
                       else if n <= 69887 then 1 else 0)
              else
                if n <= 70193
                then
                  (if n <= 70015
                   then
                     (if n <= 69932
                      then
                        (if n <= 69926
                         then 1
                         else if n <= 69931 then 0 else 1)
                      else
                        if n <= 70002
                        then (if n <= 69940 then 0 else 1)
                        else if n <= 70003 then 0 else 1)
                   else
                     if n <= 70078
                     then
                       (if n <= 70017 then 0 else if n <= 70069 then 1 else 0)
                     else
                       if n <= 70092
                       then (if n <= 70088 then 1 else 0)
                       else if n <= 70190 then 1 else 0)
                else
                  if n <= 70366
                  then
                    (if n <= 70197
                     then
                       (if n <= 70195 then 1 else if n <= 70196 then 0 else 1)
                     else
                       if n <= 70205
                       then (if n <= 70199 then 0 else 1)
                       else if n <= 70206 then 0 else 1)
                  else
                    if n <= 70378
                    then
                      (if n <= 70367 then 0 else if n <= 70370 then 1 else 0)
                    else
                      if n <= 70401
                      then (if n <= 70399 then 1 else 0)
                      else if n <= 70458 then 1 else 0)
           else
             if n <= 71104
             then
               (if n <= 70750
                then
                  (if n <= 70711
                   then
                     (if n <= 70501
                      then
                        (if n <= 70463
                         then 1
                         else if n <= 70464 then 0 else 1)
                      else
                        if n <= 70511
                        then (if n <= 70508 then 0 else 1)
                        else if n <= 70516 then 0 else 1)
                   else
                     if n <= 70724
                     then
                       (if n <= 70719 then 0 else if n <= 70721 then 1 else 0)
                     else
                       if n <= 70726
                       then (if n <= 70725 then 1 else 0)
                       else if n <= 70749 then 1 else 0)
                else
                  if n <= 70849
                  then
                    (if n <= 70841
                     then
                       (if n <= 70834 then 1 else if n <= 70840 then 0 else 1)
                     else
                       if n <= 70846
                       then (if n <= 70842 then 0 else 1)
                       else if n <= 70848 then 0 else 1)
                  else
                    if n <= 71093
                    then
                      (if n <= 70851 then 0 else if n <= 71089 then 1 else 0)
                    else
                      if n <= 71101
                      then (if n <= 71099 then 1 else 0)
                      else if n <= 71102 then 1 else 0)
             else
               if n <= 71349
               then
                 (if n <= 71230
                  then
                    (if n <= 71218
                     then
                       (if n <= 71131 then 1 else if n <= 71133 then 0 else 1)
                     else
                       if n <= 71228
                       then (if n <= 71226 then 0 else 1)
                       else if n <= 71229 then 0 else 1)
                  else
                    if n <= 71339
                    then
                      (if n <= 71232 then 0 else if n <= 71338 then 1 else 0)
                    else
                      if n <= 71341
                      then (if n <= 71340 then 1 else 0)
                      else if n <= 71343 then 1 else 0)
               else
                 if n <= 71462
                 then
                   (if n <= 71452
                    then
                      (if n <= 71350 then 1 else if n <= 71351 then 0 else 1)
                    else
                      if n <= 71457
                      then (if n <= 71455 then 0 else 1)
                      else if n <= 71461 then 0 else 1)
                 else
                   if n <= 71736
                   then
                     (if n <= 71726
                      then (if n <= 71467 then 0 else 1)
                      else if n <= 71735 then 0 else 1)
                   else
                     if n <= 72147
                     then (if n <= 71738 then 0 else 1)
                     else if n <= 72151 then 0 else 1)
      else
        if n <= 123190
        then
          (if n <= 92975
           then
             (if n <= 72873
              then
                (if n <= 72280
                 then
                   (if n <= 72248
                    then
                      (if n <= 72160
                       then
                         (if n <= 72155
                          then 0
                          else if n <= 72159 then 1 else 0)
                       else
                         if n <= 72202
                         then (if n <= 72192 then 1 else 0)
                         else if n <= 72242 then 1 else 0)
                    else
                      if n <= 72262
                      then
                        (if n <= 72250
                         then 1
                         else if n <= 72254 then 0 else 1)
                      else
                        if n <= 72272
                        then (if n <= 72263 then 0 else 1)
                        else if n <= 72278 then 0 else 1)
                 else
                   if n <= 72758
                   then
                     (if n <= 72342
                      then
                        (if n <= 72283
                         then 0
                         else if n <= 72329 then 1 else 0)
                      else
                        if n <= 72345
                        then (if n <= 72343 then 1 else 0)
                        else if n <= 72751 then 1 else 0)
                   else
                     if n <= 72766
                     then
                       (if n <= 72759 then 1 else if n <= 72765 then 0 else 1)
                     else
                       if n <= 72849
                       then (if n <= 72767 then 0 else 1)
                       else if n <= 72871 then 0 else 1)
              else
                if n <= 73030
                then
                  (if n <= 73014
                   then
                     (if n <= 72883
                      then
                        (if n <= 72880
                         then 0
                         else if n <= 72881 then 1 else 0)
                      else
                        if n <= 72886
                        then (if n <= 72884 then 1 else 0)
                        else if n <= 73008 then 1 else 0)
                   else
                     if n <= 73019
                     then
                       (if n <= 73017 then 1 else if n <= 73018 then 0 else 1)
                     else
                       if n <= 73022
                       then (if n <= 73021 then 0 else 1)
                       else if n <= 73029 then 0 else 1)
                else
                  if n <= 73111
                  then
                    (if n <= 73105
                     then
                       (if n <= 73031 then 0 else if n <= 73103 then 1 else 0)
                     else
                       if n <= 73109
                       then (if n <= 73108 then 1 else 0)
                       else if n <= 73110 then 1 else 0)
                  else
                    if n <= 78895
                    then
                      (if n <= 73458 then 1 else if n <= 73460 then 0 else 1)
                    else
                      if n <= 92911
                      then (if n <= 78904 then 0 else 1)
                      else if n <= 92916 then 0 else 1)
           else
             if n <= 119172
             then
               (if n <= 110927
                then
                  (if n <= 94179
                   then
                     (if n <= 94031
                      then
                        (if n <= 92982
                         then 0
                         else if n <= 94030 then 1 else 0)
                      else
                        if n <= 94098
                        then (if n <= 94094 then 1 else 0)
                        else if n <= 94175 then 1 else 2)
                   else
                     if n <= 100351
                     then
                       (if n <= 94207
                        then 1
                        else if n <= 100343 then 2 else 1)
                     else
                       if n <= 110591
                       then (if n <= 101106 then 2 else 1)
                       else if n <= 110878 then 2 else 1)
                else
                  if n <= 113822
                  then
                    (if n <= 110951
                     then
                       (if n <= 110930
                        then 2
                        else if n <= 110947 then 1 else 2)
                     else
                       if n <= 111355
                       then (if n <= 110959 then 1 else 2)
                       else if n <= 113820 then 1 else 0)
                  else
                    if n <= 119142
                    then
                      (if n <= 113823
                       then 1
                       else if n <= 113827 then 0 else 1)
                    else
                      if n <= 119154
                      then (if n <= 119145 then 0 else 1)
                      else if n <= 119170 then 0 else 1)
             else
               if n <= 121498
               then
                 (if n <= 121398
                  then
                    (if n <= 119213
                     then
                       (if n <= 119179
                        then 0
                        else if n <= 119209 then 1 else 0)
                     else
                       if n <= 119364
                       then (if n <= 119361 then 1 else 0)
                       else if n <= 121343 then 1 else 0)
                  else
                    if n <= 121460
                    then
                      (if n <= 121402
                       then 1
                       else if n <= 121452 then 0 else 1)
                    else
                      if n <= 121475
                      then (if n <= 121461 then 0 else 1)
                      else if n <= 121476 then 0 else 1)
               else
                 if n <= 122904
                 then
                   (if n <= 121519
                    then
                      (if n <= 121503
                       then 0
                       else if n <= 121504 then 1 else 0)
                    else
                      if n <= 122886
                      then (if n <= 122879 then 1 else 0)
                      else if n <= 122887 then 1 else 0)
                 else
                   if n <= 122916
                   then
                     (if n <= 122913
                      then (if n <= 122906 then 1 else 0)
                      else if n <= 122914 then 1 else 0)
                   else
                     if n <= 122922
                     then (if n <= 122917 then 1 else 0)
                     else if n <= 123183 then 1 else 0)
        else
          if n <= 128406
          then
            (if n <= 127797
             then
               (if n <= 127386
                then
                  (if n <= 126979
                   then
                     (if n <= 125135
                      then
                        (if n <= 123627
                         then 1
                         else if n <= 123631 then 0 else 1)
                      else
                        if n <= 125251
                        then (if n <= 125142 then 0 else 1)
                        else if n <= 125258 then 0 else 1)
                   else
                     if n <= 127183
                     then
                       (if n <= 126980
                        then 2
                        else if n <= 127182 then 1 else 2)
                     else
                       if n <= 127374
                       then (if n <= 127373 then 1 else 2)
                       else if n <= 127376 then 1 else 2)
                else
                  if n <= 127567
                  then
                    (if n <= 127503
                     then
                       (if n <= 127487
                        then 1
                        else if n <= 127490 then 2 else 1)
                     else
                       if n <= 127551
                       then (if n <= 127547 then 2 else 1)
                       else if n <= 127560 then 2 else 1)
                  else
                    if n <= 127589
                    then
                      (if n <= 127569
                       then 2
                       else if n <= 127583 then 1 else 2)
                    else
                      if n <= 127776
                      then (if n <= 127743 then 1 else 2)
                      else if n <= 127788 then 1 else 2)
             else
               if n <= 128062
               then
                 (if n <= 127950
                  then
                    (if n <= 127869
                     then
                       (if n <= 127798
                        then 1
                        else if n <= 127868 then 2 else 1)
                     else
                       if n <= 127903
                       then (if n <= 127891 then 2 else 1)
                       else if n <= 127946 then 2 else 1)
                  else
                    if n <= 127984
                    then
                      (if n <= 127955
                       then 2
                       else if n <= 127967 then 1 else 2)
                    else
                      if n <= 127988
                      then (if n <= 127987 then 1 else 2)
                      else if n <= 127991 then 1 else 2)
               else
                 if n <= 128330
                 then
                   (if n <= 128065
                    then
                      (if n <= 128063
                       then 1
                       else if n <= 128064 then 2 else 1)
                    else
                      if n <= 128254
                      then (if n <= 128252 then 2 else 1)
                      else if n <= 128317 then 2 else 1)
                 else
                   if n <= 128359
                   then
                     (if n <= 128334 then 2 else if n <= 128335 then 1 else 2)
                   else
                     if n <= 128378
                     then (if n <= 128377 then 1 else 2)
                     else if n <= 128404 then 1 else 2)
          else
            if n <= 129482
            then
              (if n <= 128748
               then
                 (if n <= 128715
                  then
                    (if n <= 128506
                     then
                       (if n <= 128419
                        then 1
                        else if n <= 128420 then 2 else 1)
                     else
                       if n <= 128639
                       then (if n <= 128591 then 2 else 1)
                       else if n <= 128709 then 2 else 1)
                  else
                    if n <= 128722
                    then
                      (if n <= 128716
                       then 2
                       else if n <= 128719 then 1 else 2)
                    else
                      if n <= 128725
                      then (if n <= 128724 then 1 else 2)
                      else if n <= 128746 then 1 else 2)
               else
                 if n <= 129394
                 then
                   (if n <= 128991
                    then
                      (if n <= 128755
                       then 1
                       else if n <= 128762 then 2 else 1)
                    else
                      if n <= 129292
                      then (if n <= 129003 then 2 else 1)
                      else if n <= 129393 then 2 else 1)
                 else
                   if n <= 129442
                   then
                     (if n <= 129398 then 2 else if n <= 129401 then 1 else 2)
                   else
                     if n <= 129450
                     then (if n <= 129444 then 1 else 2)
                     else if n <= 129453 then 1 else 2)
            else
              if n <= 177972
              then
                (if n <= 129663
                 then
                   (if n <= 129647
                    then
                      (if n <= 129484
                       then 1
                       else if n <= 129535 then 2 else 1)
                    else
                      if n <= 129655
                      then (if n <= 129651 then 2 else 1)
                      else if n <= 129658 then 2 else 1)
                 else
                   if n <= 129685
                   then
                     (if n <= 129666 then 2 else if n <= 129679 then 1 else 2)
                   else
                     if n <= 173782
                     then (if n <= 131071 then 1 else 2)
                     else if n <= 173823 then 1 else 2)
              else
                if n <= 194559
                then
                  (if n <= 178207
                   then
                     (if n <= 177983 then 1 else if n <= 178205 then 2 else 1)
                   else
                     if n <= 183983
                     then (if n <= 183969 then 2 else 1)
                     else if n <= 191456 then 2 else 1)
                else
                  if n <= 917535
                  then
                    (if n <= 917504
                     then (if n <= 195101 then 2 else 1)
                     else if n <= 917505 then 0 else 1)
                  else
                    if n <= 917759
                    then (if n <= 917631 then 0 else 1)
                    else if n <= 917999 then 0 else 1
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
  if n <= 47895
  then
    (if n <= 8987
     then
       (if n <= 3397
        then
          (if n <= 2560
           then
             (if n <= 1839
              then
                (if n <= 1474
                 then
                   (if n <= 168
                    then
                      (if n <= 35
                       then
                         (if n <= 12
                          then
                            (if n <= 9
                             then Control
                             else if n <= 10 then LF else Control)
                          else
                            if n <= 31
                            then (if n <= 13 then CR else Control)
                            else if n <= 34 then Other else ExtPict)
                       else
                         if n <= 47
                         then
                           (if n <= 41
                            then Other
                            else if n <= 42 then ExtPict else Other)
                         else
                           if n <= 126
                           then (if n <= 57 then ExtPict else Other)
                           else if n <= 159 then Control else Other)
                    else
                      if n <= 1154
                      then
                        (if n <= 173
                         then
                           (if n <= 169
                            then ExtPict
                            else if n <= 172 then Other else Control)
                         else
                           if n <= 767
                           then (if n <= 174 then ExtPict else Other)
                           else if n <= 879 then Extend else Other)
                      else
                        if n <= 1469
                        then
                          (if n <= 1161
                           then Extend
                           else if n <= 1424 then Other else Extend)
                        else
                          if n <= 1471
                          then (if n <= 1470 then Other else Extend)
                          else if n <= 1472 then Other else Extend)
                 else
                   if n <= 1648
                   then
                     (if n <= 1551
                      then
                        (if n <= 1478
                         then
                           (if n <= 1475
                            then Other
                            else if n <= 1477 then Extend else Other)
                         else
                           if n <= 1535
                           then (if n <= 1479 then Extend else Other)
                           else if n <= 1541 then Prepend else Other)
                      else
                        if n <= 1564
                        then
                          (if n <= 1562
                           then Extend
                           else if n <= 1563 then Other else Control)
                        else
                          if n <= 1631
                          then (if n <= 1610 then Other else Extend)
                          else if n <= 1647 then Other else Extend)
                   else
                     if n <= 1768
                     then
                       (if n <= 1757
                        then
                          (if n <= 1749
                           then Other
                           else if n <= 1756 then Extend else Prepend)
                        else
                          if n <= 1764
                          then (if n <= 1758 then Other else Extend)
                          else if n <= 1766 then Other else Extend)
                     else
                       if n <= 1806
                       then
                         (if n <= 1769
                          then Other
                          else if n <= 1773 then Extend else Other)
                       else
                         if n <= 1808
                         then (if n <= 1807 then Prepend else Other)
                         else if n <= 1809 then Extend else Other)
              else
                if n <= 2368
                then
                  (if n <= 2088
                   then
                     (if n <= 2045
                      then
                        (if n <= 1968
                         then
                           (if n <= 1866
                            then Extend
                            else if n <= 1957 then Other else Extend)
                         else
                           if n <= 2035
                           then (if n <= 2026 then Other else Extend)
                           else if n <= 2044 then Other else Extend)
                      else
                        if n <= 2074
                        then
                          (if n <= 2069
                           then Other
                           else if n <= 2073 then Extend else Other)
                        else
                          if n <= 2084
                          then (if n <= 2083 then Extend else Other)
                          else if n <= 2087 then Extend else Other)
                   else
                     if n <= 2306
                     then
                       (if n <= 2139
                        then
                          (if n <= 2093
                           then Extend
                           else if n <= 2136 then Other else Extend)
                        else
                          if n <= 2273
                          then (if n <= 2258 then Other else Extend)
                          else if n <= 2274 then Prepend else Extend)
                     else
                       if n <= 2362
                       then
                         (if n <= 2307
                          then SpacingMark
                          else if n <= 2361 then Other else Extend)
                       else
                         if n <= 2364
                         then (if n <= 2363 then SpacingMark else Extend)
                         else if n <= 2365 then Other else SpacingMark)
                else
                  if n <= 2493
                  then
                    (if n <= 2401
                     then
                       (if n <= 2381
                        then
                          (if n <= 2376
                           then Extend
                           else if n <= 2380 then SpacingMark else Extend)
                        else
                          if n <= 2384
                          then (if n <= 2383 then SpacingMark else Other)
                          else if n <= 2391 then Extend else Other)
                     else
                       if n <= 2433
                       then
                         (if n <= 2403
                          then Extend
                          else if n <= 2432 then Other else Extend)
                       else
                         if n <= 2491
                         then (if n <= 2435 then SpacingMark else Other)
                         else if n <= 2492 then Extend else Other)
                  else
                    if n <= 2508
                    then
                      (if n <= 2500
                       then
                         (if n <= 2494
                          then Extend
                          else if n <= 2496 then SpacingMark else Extend)
                       else
                         if n <= 2504
                         then (if n <= 2502 then Other else SpacingMark)
                         else if n <= 2506 then Other else SpacingMark)
                    else
                      if n <= 2529
                      then
                        (if n <= 2518
                         then (if n <= 2509 then Extend else Other)
                         else if n <= 2519 then Extend else Other)
                      else
                        if n <= 2557
                        then (if n <= 2531 then Extend else Other)
                        else if n <= 2558 then Extend else Other)
           else
             if n <= 3005
             then
               (if n <= 2761
                then
                  (if n <= 2671
                   then
                     (if n <= 2626
                      then
                        (if n <= 2619
                         then
                           (if n <= 2562
                            then Extend
                            else if n <= 2563 then SpacingMark else Other)
                         else
                           if n <= 2621
                           then (if n <= 2620 then Extend else Other)
                           else if n <= 2624 then SpacingMark else Extend)
                      else
                        if n <= 2634
                        then
                          (if n <= 2630
                           then Other
                           else if n <= 2632 then Extend else Other)
                        else
                          if n <= 2640
                          then (if n <= 2637 then Extend else Other)
                          else if n <= 2641 then Extend else Other)
                   else
                     if n <= 2747
                     then
                       (if n <= 2677
                        then
                          (if n <= 2673
                           then Extend
                           else if n <= 2676 then Other else Extend)
                        else
                          if n <= 2690
                          then (if n <= 2688 then Other else Extend)
                          else if n <= 2691 then SpacingMark else Other)
                     else
                       if n <= 2752
                       then
                         (if n <= 2748
                          then Extend
                          else if n <= 2749 then Other else SpacingMark)
                       else
                         if n <= 2758
                         then (if n <= 2757 then Extend else Other)
                         else if n <= 2760 then Extend else SpacingMark)
                else
                  if n <= 2879
                  then
                    (if n <= 2815
                     then
                       (if n <= 2765
                        then
                          (if n <= 2762
                           then Other
                           else if n <= 2764 then SpacingMark else Extend)
                        else
                          if n <= 2787
                          then (if n <= 2785 then Other else Extend)
                          else if n <= 2809 then Other else Extend)
                     else
                       if n <= 2819
                       then
                         (if n <= 2816
                          then Other
                          else if n <= 2817 then Extend else SpacingMark)
                       else
                         if n <= 2876
                         then (if n <= 2875 then Other else Extend)
                         else if n <= 2877 then Other else Extend)
                  else
                    if n <= 2893
                    then
                      (if n <= 2886
                       then
                         (if n <= 2880
                          then SpacingMark
                          else if n <= 2884 then Extend else Other)
                       else
                         if n <= 2890
                         then (if n <= 2888 then SpacingMark else Other)
                         else if n <= 2892 then SpacingMark else Extend)
                    else
                      if n <= 2913
                      then
                        (if n <= 2901
                         then Other
                         else if n <= 2903 then Extend else Other)
                      else
                        if n <= 2945
                        then (if n <= 2915 then Extend else Other)
                        else if n <= 2946 then Extend else Other)
             else
               if n <= 3201
               then
                 (if n <= 3075
                  then
                    (if n <= 3017
                     then
                       (if n <= 3008
                        then
                          (if n <= 3006
                           then Extend
                           else if n <= 3007 then SpacingMark else Extend)
                        else
                          if n <= 3013
                          then (if n <= 3010 then SpacingMark else Other)
                          else if n <= 3016 then SpacingMark else Other)
                     else
                       if n <= 3030
                       then
                         (if n <= 3020
                          then SpacingMark
                          else if n <= 3021 then Extend else Other)
                       else
                         if n <= 3071
                         then (if n <= 3031 then Extend else Other)
                         else if n <= 3072 then Extend else SpacingMark)
                  else
                    if n <= 3145
                    then
                      (if n <= 3136
                       then
                         (if n <= 3076
                          then Extend
                          else if n <= 3133 then Other else Extend)
                       else
                         if n <= 3141
                         then (if n <= 3140 then SpacingMark else Other)
                         else if n <= 3144 then Extend else Other)
                    else
                      if n <= 3158
                      then
                        (if n <= 3149
                         then Extend
                         else if n <= 3156 then Other else Extend)
                      else
                        if n <= 3171
                        then (if n <= 3169 then Other else Extend)
                        else if n <= 3200 then Other else Extend)
               else
                 if n <= 3275
                 then
                   (if n <= 3265
                    then
                      (if n <= 3260
                       then
                         (if n <= 3203
                          then SpacingMark
                          else if n <= 3259 then Other else Extend)
                       else
                         if n <= 3262
                         then (if n <= 3261 then Other else SpacingMark)
                         else if n <= 3263 then Extend else SpacingMark)
                    else
                      if n <= 3269
                      then
                        (if n <= 3266
                         then Extend
                         else if n <= 3268 then SpacingMark else Other)
                      else
                        if n <= 3272
                        then (if n <= 3270 then Extend else SpacingMark)
                        else if n <= 3273 then Other else SpacingMark)
                 else
                   if n <= 3329
                   then
                     (if n <= 3286
                      then
                        (if n <= 3277
                         then Extend
                         else if n <= 3284 then Other else Extend)
                      else
                        if n <= 3299
                        then (if n <= 3297 then Other else Extend)
                        else if n <= 3327 then Other else Extend)
                   else
                     if n <= 3389
                     then
                       (if n <= 3386
                        then (if n <= 3331 then SpacingMark else Other)
                        else if n <= 3388 then Extend else Other)
                     else
                       if n <= 3392
                       then (if n <= 3390 then Extend else SpacingMark)
                       else if n <= 3396 then Extend else Other)
        else
          if n <= 6278
          then
            (if n <= 3991
             then
               (if n <= 3635
                then
                  (if n <= 3534
                   then
                     (if n <= 3415
                      then
                        (if n <= 3404
                         then
                           (if n <= 3400
                            then SpacingMark
                            else if n <= 3401 then Other else SpacingMark)
                         else
                           if n <= 3406
                           then (if n <= 3405 then Extend else Prepend)
                           else if n <= 3414 then Other else Extend)
                      else
                        if n <= 3457
                        then
                          (if n <= 3425
                           then Other
                           else if n <= 3427 then Extend else Other)
                        else
                          if n <= 3529
                          then (if n <= 3459 then SpacingMark else Other)
                          else if n <= 3530 then Extend else Other)
                   else
                     if n <= 3550
                     then
                       (if n <= 3540
                        then
                          (if n <= 3535
                           then Extend
                           else if n <= 3537 then SpacingMark else Extend)
                        else
                          if n <= 3542
                          then (if n <= 3541 then Other else Extend)
                          else if n <= 3543 then Other else SpacingMark)
                     else
                       if n <= 3571
                       then
                         (if n <= 3551
                          then Extend
                          else if n <= 3569 then Other else SpacingMark)
                       else
                         if n <= 3633
                         then (if n <= 3632 then Other else Extend)
                         else if n <= 3634 then Other else SpacingMark)
                else
                  if n <= 3893
                  then
                    (if n <= 3763
                     then
                       (if n <= 3662
                        then
                          (if n <= 3642
                           then Extend
                           else if n <= 3654 then Other else Extend)
                        else
                          if n <= 3761
                          then (if n <= 3760 then Other else Extend)
                          else if n <= 3762 then Other else SpacingMark)
                     else
                       if n <= 3789
                       then
                         (if n <= 3772
                          then Extend
                          else if n <= 3783 then Other else Extend)
                       else
                         if n <= 3865
                         then (if n <= 3863 then Other else Extend)
                         else if n <= 3892 then Other else Extend)
                  else
                    if n <= 3952
                    then
                      (if n <= 3896
                       then
                         (if n <= 3894
                          then Other
                          else if n <= 3895 then Extend else Other)
                       else
                         if n <= 3901
                         then (if n <= 3897 then Extend else Other)
                         else if n <= 3903 then SpacingMark else Other)
                    else
                      if n <= 3972
                      then
                        (if n <= 3966
                         then Extend
                         else if n <= 3967 then SpacingMark else Extend)
                      else
                        if n <= 3975
                        then (if n <= 3973 then Other else Extend)
                        else if n <= 3980 then Other else Extend)
             else
               if n <= 4253
               then
                 (if n <= 4183
                  then
                    (if n <= 4145
                     then
                       (if n <= 4037
                        then
                          (if n <= 3992
                           then Other
                           else if n <= 4028 then Extend else Other)
                        else
                          if n <= 4140
                          then (if n <= 4038 then Extend else Other)
                          else if n <= 4144 then Extend else SpacingMark)
                     else
                       if n <= 4154
                       then
                         (if n <= 4151
                          then Extend
                          else if n <= 4152 then Other else Extend)
                       else
                         if n <= 4158
                         then (if n <= 4156 then SpacingMark else Extend)
                         else if n <= 4181 then Other else SpacingMark)
                  else
                    if n <= 4226
                    then
                      (if n <= 4192
                       then
                         (if n <= 4185
                          then Extend
                          else if n <= 4189 then Other else Extend)
                       else
                         if n <= 4212
                         then (if n <= 4208 then Other else Extend)
                         else if n <= 4225 then Other else Extend)
                    else
                      if n <= 4230
                      then
                        (if n <= 4227
                         then Other
                         else if n <= 4228 then SpacingMark else Extend)
                      else
                        if n <= 4237
                        then (if n <= 4236 then Other else Extend)
                        else if n <= 4252 then Other else Extend)
               else
                 if n <= 6003
                 then
                   (if n <= 5905
                    then
                      (if n <= 4519
                       then
                         (if n <= 4351
                          then Other
                          else if n <= 4447 then L else V)
                       else
                         if n <= 4956
                         then (if n <= 4607 then T else Other)
                         else if n <= 4959 then Extend else Other)
                    else
                      if n <= 5940
                      then
                        (if n <= 5908
                         then Extend
                         else if n <= 5937 then Other else Extend)
                      else
                        if n <= 5971
                        then (if n <= 5969 then Other else Extend)
                        else if n <= 6001 then Other else Extend)
                 else
                   if n <= 6088
                   then
                     (if n <= 6070
                      then
                        (if n <= 6067
                         then Other
                         else if n <= 6069 then Extend else SpacingMark)
                      else
                        if n <= 6085
                        then (if n <= 6077 then Extend else SpacingMark)
                        else if n <= 6086 then Extend else SpacingMark)
                   else
                     if n <= 6154
                     then
                       (if n <= 6108
                        then (if n <= 6099 then Extend else Other)
                        else if n <= 6109 then Extend else Other)
                     else
                       if n <= 6158
                       then (if n <= 6157 then Extend else Control)
                       else if n <= 6276 then Other else Extend)
          else
            if n <= 7141
            then
              (if n <= 6770
               then
                 (if n <= 6680
                  then
                    (if n <= 6443
                     then
                       (if n <= 6431
                        then
                          (if n <= 6312
                           then Other
                           else if n <= 6313 then Extend else Other)
                        else
                          if n <= 6438
                          then (if n <= 6434 then Extend else SpacingMark)
                          else if n <= 6440 then Extend else SpacingMark)
                     else
                       if n <= 6450
                       then
                         (if n <= 6447
                          then Other
                          else if n <= 6449 then SpacingMark else Extend)
                       else
                         if n <= 6459
                         then (if n <= 6456 then SpacingMark else Extend)
                         else if n <= 6678 then Other else Extend)
                  else
                    if n <= 6750
                    then
                      (if n <= 6740
                       then
                         (if n <= 6682
                          then SpacingMark
                          else if n <= 6683 then Extend else Other)
                       else
                         if n <= 6742
                         then (if n <= 6741 then SpacingMark else Extend)
                         else if n <= 6743 then SpacingMark else Extend)
                    else
                      if n <= 6753
                      then
                        (if n <= 6751
                         then Other
                         else if n <= 6752 then Extend else Other)
                      else
                        if n <= 6756
                        then (if n <= 6754 then Extend else Other)
                        else if n <= 6764 then Extend else SpacingMark)
               else
                 if n <= 6978
                 then
                   (if n <= 6915
                    then
                      (if n <= 6783
                       then
                         (if n <= 6780
                          then Extend
                          else if n <= 6782 then Other else Extend)
                       else
                         if n <= 6846
                         then (if n <= 6831 then Other else Extend)
                         else if n <= 6911 then Other else Extend)
                    else
                      if n <= 6970
                      then
                        (if n <= 6916
                         then SpacingMark
                         else if n <= 6963 then Other else Extend)
                      else
                        if n <= 6972
                        then (if n <= 6971 then SpacingMark else Extend)
                        else if n <= 6977 then SpacingMark else Extend)
                 else
                   if n <= 7072
                   then
                     (if n <= 7027
                      then
                        (if n <= 6980
                         then SpacingMark
                         else if n <= 7018 then Other else Extend)
                      else
                        if n <= 7041
                        then (if n <= 7039 then Other else Extend)
                        else if n <= 7042 then SpacingMark else Other)
                   else
                     if n <= 7079
                     then
                       (if n <= 7073
                        then SpacingMark
                        else if n <= 7077 then Extend else SpacingMark)
                     else
                       if n <= 7082
                       then (if n <= 7081 then Extend else SpacingMark)
                       else if n <= 7085 then Extend else Other)
            else
              if n <= 7673
              then
                (if n <= 7375
                 then
                   (if n <= 7153
                    then
                      (if n <= 7145
                       then
                         (if n <= 7142
                          then Extend
                          else if n <= 7143 then SpacingMark else Extend)
                       else
                         if n <= 7149
                         then (if n <= 7148 then SpacingMark else Extend)
                         else if n <= 7150 then SpacingMark else Extend)
                    else
                      if n <= 7211
                      then
                        (if n <= 7155
                         then SpacingMark
                         else if n <= 7203 then Other else SpacingMark)
                      else
                        if n <= 7221
                        then (if n <= 7219 then Extend else SpacingMark)
                        else if n <= 7223 then Extend else Other)
                 else
                   if n <= 7405
                   then
                     (if n <= 7392
                      then
                        (if n <= 7378
                         then Extend
                         else if n <= 7379 then Other else Extend)
                      else
                        if n <= 7400
                        then (if n <= 7393 then SpacingMark else Extend)
                        else if n <= 7404 then Other else Extend)
                   else
                     if n <= 7414
                     then
                       (if n <= 7411
                        then Other
                        else if n <= 7412 then Extend else Other)
                     else
                       if n <= 7417
                       then (if n <= 7415 then SpacingMark else Extend)
                       else if n <= 7615 then Other else Extend)
              else
                if n <= 8287
                then
                  (if n <= 8207
                   then
                     (if n <= 8202
                      then
                        (if n <= 7674
                         then Other
                         else if n <= 7679 then Extend else Other)
                      else
                        if n <= 8204
                        then (if n <= 8203 then Control else Extend)
                        else if n <= 8205 then ZWJ else Control)
                   else
                     if n <= 8251
                     then
                       (if n <= 8231
                        then Other
                        else if n <= 8238 then Control else Other)
                     else
                       if n <= 8264
                       then (if n <= 8252 then ExtPict else Other)
                       else if n <= 8265 then ExtPict else Other)
                else
                  if n <= 8482
                  then
                    (if n <= 8303
                     then
                       (if n <= 8292
                        then Control
                        else if n <= 8293 then Other else Control)
                     else
                       if n <= 8432
                       then (if n <= 8399 then Other else Extend)
                       else if n <= 8481 then Other else ExtPict)
                  else
                    if n <= 8601
                    then
                      (if n <= 8505
                       then (if n <= 8504 then Other else ExtPict)
                       else if n <= 8595 then Other else ExtPict)
                    else
                      if n <= 8618
                      then (if n <= 8616 then Other else ExtPict)
                      else if n <= 8985 then Other else ExtPict)
     else
       if n <= 44704
       then
         (if n <= 43203
          then
            (if n <= 10087
             then
               (if n <= 9989
                then
                  (if n <= 9643
                   then
                     (if n <= 9192
                      then
                        (if n <= 9095
                         then
                           (if n <= 8999
                            then Other
                            else if n <= 9000 then ExtPict else Other)
                         else
                           if n <= 9166
                           then (if n <= 9096 then ExtPict else Other)
                           else if n <= 9167 then ExtPict else Other)
                      else
                        if n <= 9210
                        then
                          (if n <= 9203
                           then ExtPict
                           else if n <= 9207 then Other else ExtPict)
                        else
                          if n <= 9410
                          then (if n <= 9409 then Other else ExtPict)
                          else if n <= 9641 then Other else ExtPict)
                   else
                     if n <= 9727
                     then
                       (if n <= 9663
                        then
                          (if n <= 9653
                           then Other
                           else if n <= 9654 then ExtPict else Other)
                        else
                          if n <= 9722
                          then (if n <= 9664 then ExtPict else Other)
                          else if n <= 9726 then ExtPict else Other)
                     else
                       if n <= 9746
                       then
                         (if n <= 9733
                          then ExtPict
                          else if n <= 9734 then Other else ExtPict)
                       else
                         if n <= 9861
                         then (if n <= 9747 then Other else ExtPict)
                         else if n <= 9871 then Other else ExtPict)
                else
                  if n <= 10036
                  then
                    (if n <= 10012
                     then
                       (if n <= 10003
                        then
                          (if n <= 9991
                           then Other
                           else if n <= 10002 then ExtPict else Other)
                        else
                          if n <= 10005
                          then (if n <= 10004 then ExtPict else Other)
                          else if n <= 10006 then ExtPict else Other)
                     else
                       if n <= 10017
                       then
                         (if n <= 10013
                          then ExtPict
                          else if n <= 10016 then Other else ExtPict)
                       else
                         if n <= 10024
                         then (if n <= 10023 then Other else ExtPict)
                         else if n <= 10034 then Other else ExtPict)
                  else
                    if n <= 10061
                    then
                      (if n <= 10054
                       then
                         (if n <= 10051
                          then Other
                          else if n <= 10052 then ExtPict else Other)
                       else
                         if n <= 10059
                         then (if n <= 10055 then ExtPict else Other)
                         else if n <= 10060 then ExtPict else Other)
                    else
                      if n <= 10069
                      then
                        (if n <= 10062
                         then ExtPict
                         else if n <= 10066 then Other else ExtPict)
                      else
                        if n <= 10071
                        then (if n <= 10070 then Other else ExtPict)
                        else if n <= 10082 then Other else ExtPict)
             else
               if n <= 12348
               then
                 (if n <= 11036
                  then
                    (if n <= 10174
                     then
                       (if n <= 10144
                        then
                          (if n <= 10132
                           then Other
                           else if n <= 10135 then ExtPict else Other)
                        else
                          if n <= 10159
                          then (if n <= 10145 then ExtPict else Other)
                          else if n <= 10160 then ExtPict else Other)
                     else
                       if n <= 10549
                       then
                         (if n <= 10175
                          then ExtPict
                          else if n <= 10547 then Other else ExtPict)
                       else
                         if n <= 11015
                         then (if n <= 11012 then Other else ExtPict)
                         else if n <= 11034 then Other else ExtPict)
                  else
                    if n <= 11646
                    then
                      (if n <= 11092
                       then
                         (if n <= 11087
                          then Other
                          else if n <= 11088 then ExtPict else Other)
                       else
                         if n <= 11502
                         then (if n <= 11093 then ExtPict else Other)
                         else if n <= 11505 then Extend else Other)
                    else
                      if n <= 11775
                      then
                        (if n <= 11647
                         then Extend
                         else if n <= 11743 then Other else Extend)
                      else
                        if n <= 12335
                        then (if n <= 12329 then Other else Extend)
                        else if n <= 12336 then ExtPict else Other)
               else
                 if n <= 42735
                 then
                   (if n <= 12953
                    then
                      (if n <= 12442
                       then
                         (if n <= 12349
                          then ExtPict
                          else if n <= 12440 then Other else Extend)
                       else
                         if n <= 12951
                         then (if n <= 12950 then Other else ExtPict)
                         else if n <= 12952 then Other else ExtPict)
                    else
                      if n <= 42611
                      then
                        (if n <= 42606
                         then Other
                         else if n <= 42610 then Extend else Other)
                      else
                        if n <= 42653
                        then (if n <= 42621 then Extend else Other)
                        else if n <= 42655 then Extend else Other)
                 else
                   if n <= 43019
                   then
                     (if n <= 43010
                      then
                        (if n <= 42737
                         then Extend
                         else if n <= 43009 then Other else Extend)
                      else
                        if n <= 43014
                        then (if n <= 43013 then Other else Extend)
                        else if n <= 43018 then Other else Extend)
                   else
                     if n <= 43047
                     then
                       (if n <= 43044
                        then (if n <= 43042 then Other else SpacingMark)
                        else if n <= 43046 then Extend else SpacingMark)
                     else
                       if n <= 43137
                       then (if n <= 43135 then Other else SpacingMark)
                       else if n <= 43187 then Other else SpacingMark)
          else
            if n <= 44004
            then
              (if n <= 43570
               then
                 (if n <= 43394
                  then
                    (if n <= 43309
                     then
                       (if n <= 43249
                        then
                          (if n <= 43205
                           then Extend
                           else if n <= 43231 then Other else Extend)
                        else
                          if n <= 43263
                          then (if n <= 43262 then Other else Extend)
                          else if n <= 43301 then Other else Extend)
                     else
                       if n <= 43347
                       then
                         (if n <= 43334
                          then Other
                          else if n <= 43345 then Extend else SpacingMark)
                       else
                         if n <= 43388
                         then (if n <= 43359 then Other else L)
                         else if n <= 43391 then Other else Extend)
                  else
                    if n <= 43453
                    then
                      (if n <= 43443
                       then
                         (if n <= 43395
                          then SpacingMark
                          else if n <= 43442 then Other else Extend)
                       else
                         if n <= 43449
                         then (if n <= 43445 then SpacingMark else Extend)
                         else if n <= 43451 then SpacingMark else Extend)
                    else
                      if n <= 43493
                      then
                        (if n <= 43456
                         then SpacingMark
                         else if n <= 43492 then Other else Extend)
                      else
                        if n <= 43566
                        then (if n <= 43560 then Other else Extend)
                        else if n <= 43568 then SpacingMark else Extend)
               else
                 if n <= 43702
                 then
                   (if n <= 43597
                    then
                      (if n <= 43586
                       then
                         (if n <= 43572
                          then SpacingMark
                          else if n <= 43574 then Extend else Other)
                       else
                         if n <= 43595
                         then (if n <= 43587 then Extend else Other)
                         else if n <= 43596 then Extend else SpacingMark)
                    else
                      if n <= 43695
                      then
                        (if n <= 43643
                         then Other
                         else if n <= 43644 then Extend else Other)
                      else
                        if n <= 43697
                        then (if n <= 43696 then Extend else Other)
                        else if n <= 43700 then Extend else Other)
                 else
                   if n <= 43755
                   then
                     (if n <= 43711
                      then
                        (if n <= 43704
                         then Extend
                         else if n <= 43709 then Other else Extend)
                      else
                        if n <= 43713
                        then (if n <= 43712 then Other else Extend)
                        else if n <= 43754 then Other else SpacingMark)
                   else
                     if n <= 43764
                     then
                       (if n <= 43757
                        then Extend
                        else if n <= 43759 then SpacingMark else Other)
                     else
                       if n <= 43766
                       then (if n <= 43765 then SpacingMark else Extend)
                       else if n <= 44002 then Other else SpacingMark)
            else
              if n <= 44311
              then
                (if n <= 44115
                 then
                   (if n <= 44013
                    then
                      (if n <= 44008
                       then
                         (if n <= 44005
                          then Extend
                          else if n <= 44007 then SpacingMark else Extend)
                       else
                         if n <= 44011
                         then (if n <= 44010 then SpacingMark else Other)
                         else if n <= 44012 then SpacingMark else Extend)
                    else
                      if n <= 44059
                      then
                        (if n <= 44031
                         then Other
                         else if n <= 44032 then LV else LVT)
                      else
                        if n <= 44087
                        then (if n <= 44060 then LV else LVT)
                        else if n <= 44088 then LV else LVT)
                 else
                   if n <= 44200
                   then
                     (if n <= 44144
                      then
                        (if n <= 44116
                         then LV
                         else if n <= 44143 then LVT else LV)
                      else
                        if n <= 44172
                        then (if n <= 44171 then LVT else LV)
                        else if n <= 44199 then LVT else LV)
                   else
                     if n <= 44255
                     then
                       (if n <= 44227
                        then LVT
                        else if n <= 44228 then LV else LVT)
                     else
                       if n <= 44283
                       then (if n <= 44256 then LV else LVT)
                       else if n <= 44284 then LV else LVT)
              else
                if n <= 44507
                then
                  (if n <= 44396
                   then
                     (if n <= 44340
                      then
                        (if n <= 44312
                         then LV
                         else if n <= 44339 then LVT else LV)
                      else
                        if n <= 44368
                        then (if n <= 44367 then LVT else LV)
                        else if n <= 44395 then LVT else LV)
                   else
                     if n <= 44451
                     then
                       (if n <= 44423
                        then LVT
                        else if n <= 44424 then LV else LVT)
                     else
                       if n <= 44479
                       then (if n <= 44452 then LV else LVT)
                       else if n <= 44480 then LV else LVT)
                else
                  if n <= 44592
                  then
                    (if n <= 44536
                     then
                       (if n <= 44508
                        then LV
                        else if n <= 44535 then LVT else LV)
                     else
                       if n <= 44564
                       then (if n <= 44563 then LVT else LV)
                       else if n <= 44591 then LVT else LV)
                  else
                    if n <= 44648
                    then
                      (if n <= 44620
                       then (if n <= 44619 then LVT else LV)
                       else if n <= 44647 then LVT else LV)
                    else
                      if n <= 44676
                      then (if n <= 44675 then LVT else LV)
                      else if n <= 44703 then LVT else LV)
       else
         if n <= 46299
         then
           (if n <= 45488
            then
              (if n <= 45096
               then
                 (if n <= 44900
                  then
                    (if n <= 44815
                     then
                       (if n <= 44759
                        then
                          (if n <= 44731
                           then LVT
                           else if n <= 44732 then LV else LVT)
                        else
                          if n <= 44787
                          then (if n <= 44760 then LV else LVT)
                          else if n <= 44788 then LV else LVT)
                     else
                       if n <= 44844
                       then
                         (if n <= 44816
                          then LV
                          else if n <= 44843 then LVT else LV)
                       else
                         if n <= 44872
                         then (if n <= 44871 then LVT else LV)
                         else if n <= 44899 then LVT else LV)
                  else
                    if n <= 45011
                    then
                      (if n <= 44955
                       then
                         (if n <= 44927
                          then LVT
                          else if n <= 44928 then LV else LVT)
                       else
                         if n <= 44983
                         then (if n <= 44956 then LV else LVT)
                         else if n <= 44984 then LV else LVT)
                    else
                      if n <= 45040
                      then
                        (if n <= 45012
                         then LV
                         else if n <= 45039 then LVT else LV)
                      else
                        if n <= 45068
                        then (if n <= 45067 then LVT else LV)
                        else if n <= 45095 then LVT else LV)
               else
                 if n <= 45292
                 then
                   (if n <= 45207
                    then
                      (if n <= 45151
                       then
                         (if n <= 45123
                          then LVT
                          else if n <= 45124 then LV else LVT)
                       else
                         if n <= 45179
                         then (if n <= 45152 then LV else LVT)
                         else if n <= 45180 then LV else LVT)
                    else
                      if n <= 45236
                      then
                        (if n <= 45208
                         then LV
                         else if n <= 45235 then LVT else LV)
                      else
                        if n <= 45264
                        then (if n <= 45263 then LVT else LV)
                        else if n <= 45291 then LVT else LV)
                 else
                   if n <= 45403
                   then
                     (if n <= 45347
                      then
                        (if n <= 45319
                         then LVT
                         else if n <= 45320 then LV else LVT)
                      else
                        if n <= 45375
                        then (if n <= 45348 then LV else LVT)
                        else if n <= 45376 then LV else LVT)
                   else
                     if n <= 45432
                     then
                       (if n <= 45404
                        then LV
                        else if n <= 45431 then LVT else LV)
                     else
                       if n <= 45460
                       then (if n <= 45459 then LVT else LV)
                       else if n <= 45487 then LVT else LV)
            else
              if n <= 45880
              then
                (if n <= 45684
                 then
                   (if n <= 45599
                    then
                      (if n <= 45543
                       then
                         (if n <= 45515
                          then LVT
                          else if n <= 45516 then LV else LVT)
                       else
                         if n <= 45571
                         then (if n <= 45544 then LV else LVT)
                         else if n <= 45572 then LV else LVT)
                    else
                      if n <= 45628
                      then
                        (if n <= 45600
                         then LV
                         else if n <= 45627 then LVT else LV)
                      else
                        if n <= 45656
                        then (if n <= 45655 then LVT else LV)
                        else if n <= 45683 then LVT else LV)
                 else
                   if n <= 45795
                   then
                     (if n <= 45739
                      then
                        (if n <= 45711
                         then LVT
                         else if n <= 45712 then LV else LVT)
                      else
                        if n <= 45767
                        then (if n <= 45740 then LV else LVT)
                        else if n <= 45768 then LV else LVT)
                   else
                     if n <= 45824
                     then
                       (if n <= 45796
                        then LV
                        else if n <= 45823 then LVT else LV)
                     else
                       if n <= 45852
                       then (if n <= 45851 then LVT else LV)
                       else if n <= 45879 then LVT else LV)
              else
                if n <= 46076
                then
                  (if n <= 45991
                   then
                     (if n <= 45935
                      then
                        (if n <= 45907
                         then LVT
                         else if n <= 45908 then LV else LVT)
                      else
                        if n <= 45963
                        then (if n <= 45936 then LV else LVT)
                        else if n <= 45964 then LV else LVT)
                   else
                     if n <= 46020
                     then
                       (if n <= 45992
                        then LV
                        else if n <= 46019 then LVT else LV)
                     else
                       if n <= 46048
                       then (if n <= 46047 then LVT else LV)
                       else if n <= 46075 then LVT else LV)
                else
                  if n <= 46187
                  then
                    (if n <= 46131
                     then
                       (if n <= 46103
                        then LVT
                        else if n <= 46104 then LV else LVT)
                     else
                       if n <= 46159
                       then (if n <= 46132 then LV else LVT)
                       else if n <= 46160 then LV else LVT)
                  else
                    if n <= 46243
                    then
                      (if n <= 46215
                       then (if n <= 46188 then LV else LVT)
                       else if n <= 46216 then LV else LVT)
                    else
                      if n <= 46271
                      then (if n <= 46244 then LV else LVT)
                      else if n <= 46272 then LV else LVT)
         else
           if n <= 47084
           then
             (if n <= 46691
              then
                (if n <= 46495
                 then
                   (if n <= 46384
                    then
                      (if n <= 46328
                       then
                         (if n <= 46300
                          then LV
                          else if n <= 46327 then LVT else LV)
                       else
                         if n <= 46356
                         then (if n <= 46355 then LVT else LV)
                         else if n <= 46383 then LVT else LV)
                    else
                      if n <= 46439
                      then
                        (if n <= 46411
                         then LVT
                         else if n <= 46412 then LV else LVT)
                      else
                        if n <= 46467
                        then (if n <= 46440 then LV else LVT)
                        else if n <= 46468 then LV else LVT)
                 else
                   if n <= 46580
                   then
                     (if n <= 46524
                      then
                        (if n <= 46496
                         then LV
                         else if n <= 46523 then LVT else LV)
                      else
                        if n <= 46552
                        then (if n <= 46551 then LVT else LV)
                        else if n <= 46579 then LVT else LV)
                   else
                     if n <= 46635
                     then
                       (if n <= 46607
                        then LVT
                        else if n <= 46608 then LV else LVT)
                     else
                       if n <= 46663
                       then (if n <= 46636 then LV else LVT)
                       else if n <= 46664 then LV else LVT)
              else
                if n <= 46887
                then
                  (if n <= 46776
                   then
                     (if n <= 46720
                      then
                        (if n <= 46692
                         then LV
                         else if n <= 46719 then LVT else LV)
                      else
                        if n <= 46748
                        then (if n <= 46747 then LVT else LV)
                        else if n <= 46775 then LVT else LV)
                   else
                     if n <= 46831
                     then
                       (if n <= 46803
                        then LVT
                        else if n <= 46804 then LV else LVT)
                     else
                       if n <= 46859
                       then (if n <= 46832 then LV else LVT)
                       else if n <= 46860 then LV else LVT)
                else
                  if n <= 46972
                  then
                    (if n <= 46916
                     then
                       (if n <= 46888
                        then LV
                        else if n <= 46915 then LVT else LV)
                     else
                       if n <= 46944
                       then (if n <= 46943 then LVT else LV)
                       else if n <= 46971 then LVT else LV)
                  else
                    if n <= 47028
                    then
                      (if n <= 47000
                       then (if n <= 46999 then LVT else LV)
                       else if n <= 47027 then LVT else LV)
                    else
                      if n <= 47056
                      then (if n <= 47055 then LVT else LV)
                      else if n <= 47083 then LVT else LV)
           else
             if n <= 47476
             then
               (if n <= 47280
                then
                  (if n <= 47195
                   then
                     (if n <= 47139
                      then
                        (if n <= 47111
                         then LVT
                         else if n <= 47112 then LV else LVT)
                      else
                        if n <= 47167
                        then (if n <= 47140 then LV else LVT)
                        else if n <= 47168 then LV else LVT)
                   else
                     if n <= 47224
                     then
                       (if n <= 47196
                        then LV
                        else if n <= 47223 then LVT else LV)
                     else
                       if n <= 47252
                       then (if n <= 47251 then LVT else LV)
                       else if n <= 47279 then LVT else LV)
                else
                  if n <= 47391
                  then
                    (if n <= 47335
                     then
                       (if n <= 47307
                        then LVT
                        else if n <= 47308 then LV else LVT)
                     else
                       if n <= 47363
                       then (if n <= 47336 then LV else LVT)
                       else if n <= 47364 then LV else LVT)
                  else
                    if n <= 47420
                    then
                      (if n <= 47392
                       then LV
                       else if n <= 47419 then LVT else LV)
                    else
                      if n <= 47448
                      then (if n <= 47447 then LVT else LV)
                      else if n <= 47475 then LVT else LV)
             else
               if n <= 47672
               then
                 (if n <= 47587
                  then
                    (if n <= 47531
                     then
                       (if n <= 47503
                        then LVT
                        else if n <= 47504 then LV else LVT)
                     else
                       if n <= 47559
                       then (if n <= 47532 then LV else LVT)
                       else if n <= 47560 then LV else LVT)
                  else
                    if n <= 47616
                    then
                      (if n <= 47588
                       then LV
                       else if n <= 47615 then LVT else LV)
                    else
                      if n <= 47644
                      then (if n <= 47643 then LVT else LV)
                      else if n <= 47671 then LVT else LV)
               else
                 if n <= 47783
                 then
                   (if n <= 47727
                    then
                      (if n <= 47699
                       then LVT
                       else if n <= 47700 then LV else LVT)
                    else
                      if n <= 47755
                      then (if n <= 47728 then LV else LVT)
                      else if n <= 47756 then LV else LVT)
                 else
                   if n <= 47839
                   then
                     (if n <= 47811
                      then (if n <= 47784 then LV else LVT)
                      else if n <= 47812 then LV else LVT)
                   else
                     if n <= 47867
                     then (if n <= 47840 then LV else LVT)
                     else if n <= 47868 then LV else LVT)
  else
    if n <= 54223
    then
      (if n <= 51059
       then
         (if n <= 49464
          then
            (if n <= 48679
             then
               (if n <= 48287
                then
                  (if n <= 48091
                   then
                     (if n <= 47980
                      then
                        (if n <= 47924
                         then
                           (if n <= 47896
                            then LV
                            else if n <= 47923 then LVT else LV)
                         else
                           if n <= 47952
                           then (if n <= 47951 then LVT else LV)
                           else if n <= 47979 then LVT else LV)
                      else
                        if n <= 48035
                        then
                          (if n <= 48007
                           then LVT
                           else if n <= 48008 then LV else LVT)
                        else
                          if n <= 48063
                          then (if n <= 48036 then LV else LVT)
                          else if n <= 48064 then LV else LVT)
                   else
                     if n <= 48176
                     then
                       (if n <= 48120
                        then
                          (if n <= 48092
                           then LV
                           else if n <= 48119 then LVT else LV)
                        else
                          if n <= 48148
                          then (if n <= 48147 then LVT else LV)
                          else if n <= 48175 then LVT else LV)
                     else
                       if n <= 48231
                       then
                         (if n <= 48203
                          then LVT
                          else if n <= 48204 then LV else LVT)
                       else
                         if n <= 48259
                         then (if n <= 48232 then LV else LVT)
                         else if n <= 48260 then LV else LVT)
                else
                  if n <= 48483
                  then
                    (if n <= 48372
                     then
                       (if n <= 48316
                        then
                          (if n <= 48288
                           then LV
                           else if n <= 48315 then LVT else LV)
                        else
                          if n <= 48344
                          then (if n <= 48343 then LVT else LV)
                          else if n <= 48371 then LVT else LV)
                     else
                       if n <= 48427
                       then
                         (if n <= 48399
                          then LVT
                          else if n <= 48400 then LV else LVT)
                       else
                         if n <= 48455
                         then (if n <= 48428 then LV else LVT)
                         else if n <= 48456 then LV else LVT)
                  else
                    if n <= 48568
                    then
                      (if n <= 48512
                       then
                         (if n <= 48484
                          then LV
                          else if n <= 48511 then LVT else LV)
                       else
                         if n <= 48540
                         then (if n <= 48539 then LVT else LV)
                         else if n <= 48567 then LVT else LV)
                    else
                      if n <= 48623
                      then
                        (if n <= 48595
                         then LVT
                         else if n <= 48596 then LV else LVT)
                      else
                        if n <= 48651
                        then (if n <= 48624 then LV else LVT)
                        else if n <= 48652 then LV else LVT)
             else
               if n <= 49071
               then
                 (if n <= 48875
                  then
                    (if n <= 48764
                     then
                       (if n <= 48708
                        then
                          (if n <= 48680
                           then LV
                           else if n <= 48707 then LVT else LV)
                        else
                          if n <= 48736
                          then (if n <= 48735 then LVT else LV)
                          else if n <= 48763 then LVT else LV)
                     else
                       if n <= 48819
                       then
                         (if n <= 48791
                          then LVT
                          else if n <= 48792 then LV else LVT)
                       else
                         if n <= 48847
                         then (if n <= 48820 then LV else LVT)
                         else if n <= 48848 then LV else LVT)
                  else
                    if n <= 48960
                    then
                      (if n <= 48904
                       then
                         (if n <= 48876
                          then LV
                          else if n <= 48903 then LVT else LV)
                       else
                         if n <= 48932
                         then (if n <= 48931 then LVT else LV)
                         else if n <= 48959 then LVT else LV)
                    else
                      if n <= 49015
                      then
                        (if n <= 48987
                         then LVT
                         else if n <= 48988 then LV else LVT)
                      else
                        if n <= 49043
                        then (if n <= 49016 then LV else LVT)
                        else if n <= 49044 then LV else LVT)
               else
                 if n <= 49267
                 then
                   (if n <= 49156
                    then
                      (if n <= 49100
                       then
                         (if n <= 49072
                          then LV
                          else if n <= 49099 then LVT else LV)
                       else
                         if n <= 49128
                         then (if n <= 49127 then LVT else LV)
                         else if n <= 49155 then LVT else LV)
                    else
                      if n <= 49211
                      then
                        (if n <= 49183
                         then LVT
                         else if n <= 49184 then LV else LVT)
                      else
                        if n <= 49239
                        then (if n <= 49212 then LV else LVT)
                        else if n <= 49240 then LV else LVT)
                 else
                   if n <= 49352
                   then
                     (if n <= 49296
                      then
                        (if n <= 49268
                         then LV
                         else if n <= 49295 then LVT else LV)
                      else
                        if n <= 49324
                        then (if n <= 49323 then LVT else LV)
                        else if n <= 49351 then LVT else LV)
                   else
                     if n <= 49408
                     then
                       (if n <= 49380
                        then (if n <= 49379 then LVT else LV)
                        else if n <= 49407 then LVT else LV)
                     else
                       if n <= 49436
                       then (if n <= 49435 then LVT else LV)
                       else if n <= 49463 then LVT else LV)
          else
            if n <= 50248
            then
              (if n <= 49856
               then
                 (if n <= 49660
                  then
                    (if n <= 49575
                     then
                       (if n <= 49519
                        then
                          (if n <= 49491
                           then LVT
                           else if n <= 49492 then LV else LVT)
                        else
                          if n <= 49547
                          then (if n <= 49520 then LV else LVT)
                          else if n <= 49548 then LV else LVT)
                     else
                       if n <= 49604
                       then
                         (if n <= 49576
                          then LV
                          else if n <= 49603 then LVT else LV)
                       else
                         if n <= 49632
                         then (if n <= 49631 then LVT else LV)
                         else if n <= 49659 then LVT else LV)
                  else
                    if n <= 49771
                    then
                      (if n <= 49715
                       then
                         (if n <= 49687
                          then LVT
                          else if n <= 49688 then LV else LVT)
                       else
                         if n <= 49743
                         then (if n <= 49716 then LV else LVT)
                         else if n <= 49744 then LV else LVT)
                    else
                      if n <= 49800
                      then
                        (if n <= 49772
                         then LV
                         else if n <= 49799 then LVT else LV)
                      else
                        if n <= 49828
                        then (if n <= 49827 then LVT else LV)
                        else if n <= 49855 then LVT else LV)
               else
                 if n <= 50052
                 then
                   (if n <= 49967
                    then
                      (if n <= 49911
                       then
                         (if n <= 49883
                          then LVT
                          else if n <= 49884 then LV else LVT)
                       else
                         if n <= 49939
                         then (if n <= 49912 then LV else LVT)
                         else if n <= 49940 then LV else LVT)
                    else
                      if n <= 49996
                      then
                        (if n <= 49968
                         then LV
                         else if n <= 49995 then LVT else LV)
                      else
                        if n <= 50024
                        then (if n <= 50023 then LVT else LV)
                        else if n <= 50051 then LVT else LV)
                 else
                   if n <= 50163
                   then
                     (if n <= 50107
                      then
                        (if n <= 50079
                         then LVT
                         else if n <= 50080 then LV else LVT)
                      else
                        if n <= 50135
                        then (if n <= 50108 then LV else LVT)
                        else if n <= 50136 then LV else LVT)
                   else
                     if n <= 50192
                     then
                       (if n <= 50164
                        then LV
                        else if n <= 50191 then LVT else LV)
                     else
                       if n <= 50220
                       then (if n <= 50219 then LVT else LV)
                       else if n <= 50247 then LVT else LV)
            else
              if n <= 50640
              then
                (if n <= 50444
                 then
                   (if n <= 50359
                    then
                      (if n <= 50303
                       then
                         (if n <= 50275
                          then LVT
                          else if n <= 50276 then LV else LVT)
                       else
                         if n <= 50331
                         then (if n <= 50304 then LV else LVT)
                         else if n <= 50332 then LV else LVT)
                    else
                      if n <= 50388
                      then
                        (if n <= 50360
                         then LV
                         else if n <= 50387 then LVT else LV)
                      else
                        if n <= 50416
                        then (if n <= 50415 then LVT else LV)
                        else if n <= 50443 then LVT else LV)
                 else
                   if n <= 50555
                   then
                     (if n <= 50499
                      then
                        (if n <= 50471
                         then LVT
                         else if n <= 50472 then LV else LVT)
                      else
                        if n <= 50527
                        then (if n <= 50500 then LV else LVT)
                        else if n <= 50528 then LV else LVT)
                   else
                     if n <= 50584
                     then
                       (if n <= 50556
                        then LV
                        else if n <= 50583 then LVT else LV)
                     else
                       if n <= 50612
                       then (if n <= 50611 then LVT else LV)
                       else if n <= 50639 then LVT else LV)
              else
                if n <= 50836
                then
                  (if n <= 50751
                   then
                     (if n <= 50695
                      then
                        (if n <= 50667
                         then LVT
                         else if n <= 50668 then LV else LVT)
                      else
                        if n <= 50723
                        then (if n <= 50696 then LV else LVT)
                        else if n <= 50724 then LV else LVT)
                   else
                     if n <= 50780
                     then
                       (if n <= 50752
                        then LV
                        else if n <= 50779 then LVT else LV)
                     else
                       if n <= 50808
                       then (if n <= 50807 then LVT else LV)
                       else if n <= 50835 then LVT else LV)
                else
                  if n <= 50947
                  then
                    (if n <= 50891
                     then
                       (if n <= 50863
                        then LVT
                        else if n <= 50864 then LV else LVT)
                     else
                       if n <= 50919
                       then (if n <= 50892 then LV else LVT)
                       else if n <= 50920 then LV else LVT)
                  else
                    if n <= 51003
                    then
                      (if n <= 50975
                       then (if n <= 50948 then LV else LVT)
                       else if n <= 50976 then LV else LVT)
                    else
                      if n <= 51031
                      then (if n <= 51004 then LV else LVT)
                      else if n <= 51032 then LV else LVT)
       else
         if n <= 52628
         then
           (if n <= 51843
            then
              (if n <= 51451
               then
                 (if n <= 51255
                  then
                    (if n <= 51144
                     then
                       (if n <= 51088
                        then
                          (if n <= 51060
                           then LV
                           else if n <= 51087 then LVT else LV)
                        else
                          if n <= 51116
                          then (if n <= 51115 then LVT else LV)
                          else if n <= 51143 then LVT else LV)
                     else
                       if n <= 51199
                       then
                         (if n <= 51171
                          then LVT
                          else if n <= 51172 then LV else LVT)
                       else
                         if n <= 51227
                         then (if n <= 51200 then LV else LVT)
                         else if n <= 51228 then LV else LVT)
                  else
                    if n <= 51340
                    then
                      (if n <= 51284
                       then
                         (if n <= 51256
                          then LV
                          else if n <= 51283 then LVT else LV)
                       else
                         if n <= 51312
                         then (if n <= 51311 then LVT else LV)
                         else if n <= 51339 then LVT else LV)
                    else
                      if n <= 51395
                      then
                        (if n <= 51367
                         then LVT
                         else if n <= 51368 then LV else LVT)
                      else
                        if n <= 51423
                        then (if n <= 51396 then LV else LVT)
                        else if n <= 51424 then LV else LVT)
               else
                 if n <= 51647
                 then
                   (if n <= 51536
                    then
                      (if n <= 51480
                       then
                         (if n <= 51452
                          then LV
                          else if n <= 51479 then LVT else LV)
                       else
                         if n <= 51508
                         then (if n <= 51507 then LVT else LV)
                         else if n <= 51535 then LVT else LV)
                    else
                      if n <= 51591
                      then
                        (if n <= 51563
                         then LVT
                         else if n <= 51564 then LV else LVT)
                      else
                        if n <= 51619
                        then (if n <= 51592 then LV else LVT)
                        else if n <= 51620 then LV else LVT)
                 else
                   if n <= 51732
                   then
                     (if n <= 51676
                      then
                        (if n <= 51648
                         then LV
                         else if n <= 51675 then LVT else LV)
                      else
                        if n <= 51704
                        then (if n <= 51703 then LVT else LV)
                        else if n <= 51731 then LVT else LV)
                   else
                     if n <= 51787
                     then
                       (if n <= 51759
                        then LVT
                        else if n <= 51760 then LV else LVT)
                     else
                       if n <= 51815
                       then (if n <= 51788 then LV else LVT)
                       else if n <= 51816 then LV else LVT)
            else
              if n <= 52235
              then
                (if n <= 52039
                 then
                   (if n <= 51928
                    then
                      (if n <= 51872
                       then
                         (if n <= 51844
                          then LV
                          else if n <= 51871 then LVT else LV)
                       else
                         if n <= 51900
                         then (if n <= 51899 then LVT else LV)
                         else if n <= 51927 then LVT else LV)
                    else
                      if n <= 51983
                      then
                        (if n <= 51955
                         then LVT
                         else if n <= 51956 then LV else LVT)
                      else
                        if n <= 52011
                        then (if n <= 51984 then LV else LVT)
                        else if n <= 52012 then LV else LVT)
                 else
                   if n <= 52124
                   then
                     (if n <= 52068
                      then
                        (if n <= 52040
                         then LV
                         else if n <= 52067 then LVT else LV)
                      else
                        if n <= 52096
                        then (if n <= 52095 then LVT else LV)
                        else if n <= 52123 then LVT else LV)
                   else
                     if n <= 52179
                     then
                       (if n <= 52151
                        then LVT
                        else if n <= 52152 then LV else LVT)
                     else
                       if n <= 52207
                       then (if n <= 52180 then LV else LVT)
                       else if n <= 52208 then LV else LVT)
              else
                if n <= 52431
                then
                  (if n <= 52320
                   then
                     (if n <= 52264
                      then
                        (if n <= 52236
                         then LV
                         else if n <= 52263 then LVT else LV)
                      else
                        if n <= 52292
                        then (if n <= 52291 then LVT else LV)
                        else if n <= 52319 then LVT else LV)
                   else
                     if n <= 52375
                     then
                       (if n <= 52347
                        then LVT
                        else if n <= 52348 then LV else LVT)
                     else
                       if n <= 52403
                       then (if n <= 52376 then LV else LVT)
                       else if n <= 52404 then LV else LVT)
                else
                  if n <= 52516
                  then
                    (if n <= 52460
                     then
                       (if n <= 52432
                        then LV
                        else if n <= 52459 then LVT else LV)
                     else
                       if n <= 52488
                       then (if n <= 52487 then LVT else LV)
                       else if n <= 52515 then LVT else LV)
                  else
                    if n <= 52572
                    then
                      (if n <= 52544
                       then (if n <= 52543 then LVT else LV)
                       else if n <= 52571 then LVT else LV)
                    else
                      if n <= 52600
                      then (if n <= 52599 then LVT else LV)
                      else if n <= 52627 then LVT else LV)
         else
           if n <= 53412
           then
             (if n <= 53020
              then
                (if n <= 52824
                 then
                   (if n <= 52739
                    then
                      (if n <= 52683
                       then
                         (if n <= 52655
                          then LVT
                          else if n <= 52656 then LV else LVT)
                       else
                         if n <= 52711
                         then (if n <= 52684 then LV else LVT)
                         else if n <= 52712 then LV else LVT)
                    else
                      if n <= 52768
                      then
                        (if n <= 52740
                         then LV
                         else if n <= 52767 then LVT else LV)
                      else
                        if n <= 52796
                        then (if n <= 52795 then LVT else LV)
                        else if n <= 52823 then LVT else LV)
                 else
                   if n <= 52935
                   then
                     (if n <= 52879
                      then
                        (if n <= 52851
                         then LVT
                         else if n <= 52852 then LV else LVT)
                      else
                        if n <= 52907
                        then (if n <= 52880 then LV else LVT)
                        else if n <= 52908 then LV else LVT)
                   else
                     if n <= 52964
                     then
                       (if n <= 52936
                        then LV
                        else if n <= 52963 then LVT else LV)
                     else
                       if n <= 52992
                       then (if n <= 52991 then LVT else LV)
                       else if n <= 53019 then LVT else LV)
              else
                if n <= 53216
                then
                  (if n <= 53131
                   then
                     (if n <= 53075
                      then
                        (if n <= 53047
                         then LVT
                         else if n <= 53048 then LV else LVT)
                      else
                        if n <= 53103
                        then (if n <= 53076 then LV else LVT)
                        else if n <= 53104 then LV else LVT)
                   else
                     if n <= 53160
                     then
                       (if n <= 53132
                        then LV
                        else if n <= 53159 then LVT else LV)
                     else
                       if n <= 53188
                       then (if n <= 53187 then LVT else LV)
                       else if n <= 53215 then LVT else LV)
                else
                  if n <= 53327
                  then
                    (if n <= 53271
                     then
                       (if n <= 53243
                        then LVT
                        else if n <= 53244 then LV else LVT)
                     else
                       if n <= 53299
                       then (if n <= 53272 then LV else LVT)
                       else if n <= 53300 then LV else LVT)
                  else
                    if n <= 53356
                    then
                      (if n <= 53328
                       then LV
                       else if n <= 53355 then LVT else LV)
                    else
                      if n <= 53384
                      then (if n <= 53383 then LVT else LV)
                      else if n <= 53411 then LVT else LV)
           else
             if n <= 53804
             then
               (if n <= 53608
                then
                  (if n <= 53523
                   then
                     (if n <= 53467
                      then
                        (if n <= 53439
                         then LVT
                         else if n <= 53440 then LV else LVT)
                      else
                        if n <= 53495
                        then (if n <= 53468 then LV else LVT)
                        else if n <= 53496 then LV else LVT)
                   else
                     if n <= 53552
                     then
                       (if n <= 53524
                        then LV
                        else if n <= 53551 then LVT else LV)
                     else
                       if n <= 53580
                       then (if n <= 53579 then LVT else LV)
                       else if n <= 53607 then LVT else LV)
                else
                  if n <= 53719
                  then
                    (if n <= 53663
                     then
                       (if n <= 53635
                        then LVT
                        else if n <= 53636 then LV else LVT)
                     else
                       if n <= 53691
                       then (if n <= 53664 then LV else LVT)
                       else if n <= 53692 then LV else LVT)
                  else
                    if n <= 53748
                    then
                      (if n <= 53720
                       then LV
                       else if n <= 53747 then LVT else LV)
                    else
                      if n <= 53776
                      then (if n <= 53775 then LVT else LV)
                      else if n <= 53803 then LVT else LV)
             else
               if n <= 54000
               then
                 (if n <= 53915
                  then
                    (if n <= 53859
                     then
                       (if n <= 53831
                        then LVT
                        else if n <= 53832 then LV else LVT)
                     else
                       if n <= 53887
                       then (if n <= 53860 then LV else LVT)
                       else if n <= 53888 then LV else LVT)
                  else
                    if n <= 53944
                    then
                      (if n <= 53916
                       then LV
                       else if n <= 53943 then LVT else LV)
                    else
                      if n <= 53972
                      then (if n <= 53971 then LVT else LV)
                      else if n <= 53999 then LVT else LV)
               else
                 if n <= 54111
                 then
                   (if n <= 54055
                    then
                      (if n <= 54027
                       then LVT
                       else if n <= 54028 then LV else LVT)
                    else
                      if n <= 54083
                      then (if n <= 54056 then LV else LVT)
                      else if n <= 54084 then LV else LVT)
                 else
                   if n <= 54167
                   then
                     (if n <= 54139
                      then (if n <= 54112 then LV else LVT)
                      else if n <= 54140 then LV else LVT)
                   else
                     if n <= 54195
                     then (if n <= 54168 then LV else LVT)
                     else if n <= 54196 then LV else LVT)
    else
      if n <= 71343
      then
        (if n <= 69687
         then
           (if n <= 55007
            then
              (if n <= 54615
               then
                 (if n <= 54419
                  then
                    (if n <= 54308
                     then
                       (if n <= 54252
                        then
                          (if n <= 54224
                           then LV
                           else if n <= 54251 then LVT else LV)
                        else
                          if n <= 54280
                          then (if n <= 54279 then LVT else LV)
                          else if n <= 54307 then LVT else LV)
                     else
                       if n <= 54363
                       then
                         (if n <= 54335
                          then LVT
                          else if n <= 54336 then LV else LVT)
                       else
                         if n <= 54391
                         then (if n <= 54364 then LV else LVT)
                         else if n <= 54392 then LV else LVT)
                  else
                    if n <= 54504
                    then
                      (if n <= 54448
                       then
                         (if n <= 54420
                          then LV
                          else if n <= 54447 then LVT else LV)
                       else
                         if n <= 54476
                         then (if n <= 54475 then LVT else LV)
                         else if n <= 54503 then LVT else LV)
                    else
                      if n <= 54559
                      then
                        (if n <= 54531
                         then LVT
                         else if n <= 54532 then LV else LVT)
                      else
                        if n <= 54587
                        then (if n <= 54560 then LV else LVT)
                        else if n <= 54588 then LV else LVT)
               else
                 if n <= 54811
                 then
                   (if n <= 54700
                    then
                      (if n <= 54644
                       then
                         (if n <= 54616
                          then LV
                          else if n <= 54643 then LVT else LV)
                       else
                         if n <= 54672
                         then (if n <= 54671 then LVT else LV)
                         else if n <= 54699 then LVT else LV)
                    else
                      if n <= 54755
                      then
                        (if n <= 54727
                         then LVT
                         else if n <= 54728 then LV else LVT)
                      else
                        if n <= 54783
                        then (if n <= 54756 then LV else LVT)
                        else if n <= 54784 then LV else LVT)
                 else
                   if n <= 54896
                   then
                     (if n <= 54840
                      then
                        (if n <= 54812
                         then LV
                         else if n <= 54839 then LVT else LV)
                      else
                        if n <= 54868
                        then (if n <= 54867 then LVT else LV)
                        else if n <= 54895 then LVT else LV)
                   else
                     if n <= 54951
                     then
                       (if n <= 54923
                        then LVT
                        else if n <= 54924 then LV else LVT)
                     else
                       if n <= 54979
                       then (if n <= 54952 then LV else LVT)
                       else if n <= 54980 then LV else LVT)
            else
              if n <= 65439
              then
                (if n <= 55203
                 then
                   (if n <= 55092
                    then
                      (if n <= 55036
                       then
                         (if n <= 55008
                          then LV
                          else if n <= 55035 then LVT else LV)
                       else
                         if n <= 55064
                         then (if n <= 55063 then LVT else LV)
                         else if n <= 55091 then LVT else LV)
                    else
                      if n <= 55147
                      then
                        (if n <= 55119
                         then LVT
                         else if n <= 55120 then LV else LVT)
                      else
                        if n <= 55175
                        then (if n <= 55148 then LV else LVT)
                        else if n <= 55176 then LV else LVT)
                 else
                   if n <= 65023
                   then
                     (if n <= 55242
                      then
                        (if n <= 55215
                         then Other
                         else if n <= 55238 then V else Other)
                      else
                        if n <= 64285
                        then (if n <= 55291 then T else Other)
                        else if n <= 64286 then Extend else Other)
                   else
                     if n <= 65071
                     then
                       (if n <= 65039
                        then Extend
                        else if n <= 65055 then Other else Extend)
                     else
                       if n <= 65279
                       then (if n <= 65278 then Other else Control)
                       else if n <= 65437 then Other else Extend)
              else
                if n <= 68111
                then
                  (if n <= 66421
                   then
                     (if n <= 66044
                      then
                        (if n <= 65528
                         then Other
                         else if n <= 65531 then Control else Other)
                      else
                        if n <= 66271
                        then (if n <= 66045 then Extend else Other)
                        else if n <= 66272 then Extend else Other)
                   else
                     if n <= 68099
                     then
                       (if n <= 66426
                        then Extend
                        else if n <= 68096 then Other else Extend)
                     else
                       if n <= 68102
                       then (if n <= 68100 then Other else Extend)
                       else if n <= 68107 then Other else Extend)
                else
                  if n <= 68899
                  then
                    (if n <= 68158
                     then
                       (if n <= 68151
                        then Other
                        else if n <= 68154 then Extend else Other)
                     else
                       if n <= 68324
                       then (if n <= 68159 then Extend else Other)
                       else if n <= 68326 then Extend else Other)
                  else
                    if n <= 69631
                    then
                      (if n <= 69445
                       then (if n <= 68903 then Extend else Other)
                       else if n <= 69456 then Extend else Other)
                    else
                      if n <= 69633
                      then (if n <= 69632 then SpacingMark else Extend)
                      else if n <= 69634 then SpacingMark else Other)
         else
           if n <= 70464
           then
             (if n <= 70069
              then
                (if n <= 69887
                 then
                   (if n <= 69814
                    then
                      (if n <= 69761
                       then
                         (if n <= 69702
                          then Extend
                          else if n <= 69758 then Other else Extend)
                       else
                         if n <= 69807
                         then (if n <= 69762 then SpacingMark else Other)
                         else if n <= 69810 then SpacingMark else Extend)
                    else
                      if n <= 69820
                      then
                        (if n <= 69816
                         then SpacingMark
                         else if n <= 69818 then Extend else Other)
                      else
                        if n <= 69836
                        then (if n <= 69821 then Prepend else Other)
                        else if n <= 69837 then Prepend else Other)
                 else
                   if n <= 69958
                   then
                     (if n <= 69931
                      then
                        (if n <= 69890
                         then Extend
                         else if n <= 69926 then Other else Extend)
                      else
                        if n <= 69940
                        then (if n <= 69932 then SpacingMark else Extend)
                        else if n <= 69956 then Other else SpacingMark)
                   else
                     if n <= 70015
                     then
                       (if n <= 70002
                        then Other
                        else if n <= 70003 then Extend else Other)
                     else
                       if n <= 70018
                       then (if n <= 70017 then Extend else SpacingMark)
                       else if n <= 70066 then Other else SpacingMark)
              else
                if n <= 70205
                then
                  (if n <= 70187
                   then
                     (if n <= 70081
                      then
                        (if n <= 70078
                         then Extend
                         else if n <= 70080 then SpacingMark else Other)
                      else
                        if n <= 70088
                        then (if n <= 70083 then Prepend else Other)
                        else if n <= 70092 then Extend else Other)
                   else
                     if n <= 70195
                     then
                       (if n <= 70190
                        then SpacingMark
                        else if n <= 70193 then Extend else SpacingMark)
                     else
                       if n <= 70197
                       then (if n <= 70196 then Extend else SpacingMark)
                       else if n <= 70199 then Extend else Other)
                else
                  if n <= 70401
                  then
                    (if n <= 70367
                     then
                       (if n <= 70206
                        then Extend
                        else if n <= 70366 then Other else Extend)
                     else
                       if n <= 70378
                       then (if n <= 70370 then SpacingMark else Extend)
                       else if n <= 70399 then Other else Extend)
                  else
                    if n <= 70460
                    then
                      (if n <= 70403
                       then SpacingMark
                       else if n <= 70458 then Other else Extend)
                    else
                      if n <= 70462
                      then (if n <= 70461 then Other else Extend)
                      else if n <= 70463 then SpacingMark else Extend)
           else
             if n <= 70842
             then
               (if n <= 70708
                then
                  (if n <= 70487
                   then
                     (if n <= 70472
                      then
                        (if n <= 70468
                         then SpacingMark
                         else if n <= 70470 then Other else SpacingMark)
                      else
                        if n <= 70477
                        then (if n <= 70474 then Other else SpacingMark)
                        else if n <= 70486 then Other else Extend)
                   else
                     if n <= 70501
                     then
                       (if n <= 70497
                        then Other
                        else if n <= 70499 then SpacingMark else Other)
                     else
                       if n <= 70511
                       then (if n <= 70508 then Extend else Other)
                       else if n <= 70516 then Extend else Other)
                else
                  if n <= 70749
                  then
                    (if n <= 70721
                     then
                       (if n <= 70711
                        then SpacingMark
                        else if n <= 70719 then Extend else SpacingMark)
                     else
                       if n <= 70725
                       then (if n <= 70724 then Extend else SpacingMark)
                       else if n <= 70726 then Extend else Other)
                  else
                    if n <= 70832
                    then
                      (if n <= 70750
                       then Extend
                       else if n <= 70831 then Other else Extend)
                    else
                      if n <= 70840
                      then (if n <= 70834 then SpacingMark else Extend)
                      else if n <= 70841 then SpacingMark else Extend)
             else
               if n <= 71102
               then
                 (if n <= 71086
                  then
                    (if n <= 70846
                     then
                       (if n <= 70844
                        then SpacingMark
                        else if n <= 70845 then Extend else SpacingMark)
                     else
                       if n <= 70849
                       then (if n <= 70848 then Extend else SpacingMark)
                       else if n <= 70851 then Extend else Other)
                  else
                    if n <= 71093
                    then
                      (if n <= 71087
                       then Extend
                       else if n <= 71089 then SpacingMark else Extend)
                    else
                      if n <= 71099
                      then (if n <= 71095 then Other else SpacingMark)
                      else if n <= 71101 then Extend else SpacingMark)
               else
                 if n <= 71228
                 then
                   (if n <= 71133
                    then
                      (if n <= 71104
                       then Extend
                       else if n <= 71131 then Other else Extend)
                    else
                      if n <= 71218
                      then (if n <= 71215 then Other else SpacingMark)
                      else if n <= 71226 then Extend else SpacingMark)
                 else
                   if n <= 71338
                   then
                     (if n <= 71230
                      then (if n <= 71229 then Extend else SpacingMark)
                      else if n <= 71232 then Extend else Other)
                   else
                     if n <= 71340
                     then (if n <= 71339 then Extend else SpacingMark)
                     else if n <= 71341 then Extend else SpacingMark)
      else
        if n <= 121402
        then
          (if n <= 72884
           then
             (if n <= 72249
              then
                (if n <= 71738
                 then
                   (if n <= 71461
                    then
                      (if n <= 71351
                       then
                         (if n <= 71349
                          then Extend
                          else if n <= 71350 then SpacingMark else Extend)
                       else
                         if n <= 71455
                         then (if n <= 71452 then Other else Extend)
                         else if n <= 71457 then SpacingMark else Extend)
                    else
                      if n <= 71723
                      then
                        (if n <= 71462
                         then SpacingMark
                         else if n <= 71467 then Extend else Other)
                      else
                        if n <= 71735
                        then (if n <= 71726 then SpacingMark else Extend)
                        else if n <= 71736 then SpacingMark else Extend)
                 else
                   if n <= 72160
                   then
                     (if n <= 72151
                      then
                        (if n <= 72144
                         then Other
                         else if n <= 72147 then SpacingMark else Extend)
                      else
                        if n <= 72155
                        then (if n <= 72153 then Other else Extend)
                        else if n <= 72159 then SpacingMark else Extend)
                   else
                     if n <= 72192
                     then
                       (if n <= 72163
                        then Other
                        else if n <= 72164 then SpacingMark else Other)
                     else
                       if n <= 72242
                       then (if n <= 72202 then Extend else Other)
                       else if n <= 72248 then Extend else SpacingMark)
              else
                if n <= 72750
                then
                  (if n <= 72280
                   then
                     (if n <= 72262
                      then
                        (if n <= 72250
                         then Prepend
                         else if n <= 72254 then Extend else Other)
                      else
                        if n <= 72272
                        then (if n <= 72263 then Extend else Other)
                        else if n <= 72278 then Extend else SpacingMark)
                   else
                     if n <= 72329
                     then
                       (if n <= 72283
                        then Extend
                        else if n <= 72323 then Other else Prepend)
                     else
                       if n <= 72343
                       then (if n <= 72342 then Extend else SpacingMark)
                       else if n <= 72345 then Extend else Other)
                else
                  if n <= 72849
                  then
                    (if n <= 72759
                     then
                       (if n <= 72751
                        then SpacingMark
                        else if n <= 72758 then Extend else Other)
                     else
                       if n <= 72766
                       then (if n <= 72765 then Extend else SpacingMark)
                       else if n <= 72767 then Extend else Other)
                  else
                    if n <= 72873
                    then
                      (if n <= 72871
                       then Extend
                       else if n <= 72872 then Other else SpacingMark)
                    else
                      if n <= 72881
                      then (if n <= 72880 then Extend else SpacingMark)
                      else if n <= 72883 then Extend else SpacingMark)
           else
             if n <= 92975
             then
               (if n <= 73103
                then
                  (if n <= 73021
                   then
                     (if n <= 73014
                      then
                        (if n <= 72886
                         then Extend
                         else if n <= 73008 then Other else Extend)
                      else
                        if n <= 73018
                        then (if n <= 73017 then Other else Extend)
                        else if n <= 73019 then Other else Extend)
                   else
                     if n <= 73030
                     then
                       (if n <= 73022
                        then Other
                        else if n <= 73029 then Extend else Prepend)
                     else
                       if n <= 73097
                       then (if n <= 73031 then Extend else Other)
                       else if n <= 73102 then SpacingMark else Other)
                else
                  if n <= 73458
                  then
                    (if n <= 73108
                     then
                       (if n <= 73105
                        then Extend
                        else if n <= 73106 then Other else SpacingMark)
                     else
                       if n <= 73110
                       then (if n <= 73109 then Extend else SpacingMark)
                       else if n <= 73111 then Extend else Other)
                  else
                    if n <= 78895
                    then
                      (if n <= 73460
                       then Extend
                       else if n <= 73462 then SpacingMark else Other)
                    else
                      if n <= 92911
                      then (if n <= 78904 then Control else Other)
                      else if n <= 92916 then Extend else Other)
             else
               if n <= 119142
               then
                 (if n <= 94098
                  then
                    (if n <= 94031
                     then
                       (if n <= 92982
                        then Extend
                        else if n <= 94030 then Other else Extend)
                     else
                       if n <= 94087
                       then (if n <= 94032 then Other else SpacingMark)
                       else if n <= 94094 then Other else Extend)
                  else
                    if n <= 113823
                    then
                      (if n <= 113820
                       then Other
                       else if n <= 113822 then Extend else Other)
                    else
                      if n <= 119140
                      then (if n <= 113827 then Control else Other)
                      else if n <= 119141 then Extend else SpacingMark)
               else
                 if n <= 119172
                 then
                   (if n <= 119149
                    then
                      (if n <= 119145
                       then Extend
                       else if n <= 119148 then Other else SpacingMark)
                    else
                      if n <= 119162
                      then (if n <= 119154 then Extend else Control)
                      else if n <= 119170 then Extend else Other)
                 else
                   if n <= 119361
                   then
                     (if n <= 119209
                      then (if n <= 119179 then Extend else Other)
                      else if n <= 119213 then Extend else Other)
                   else
                     if n <= 121343
                     then (if n <= 119364 then Extend else Other)
                     else if n <= 121398 then Extend else Other)
        else
          if n <= 127514
          then
            (if n <= 126975
             then
               (if n <= 122906
                then
                  (if n <= 121503
                   then
                     (if n <= 121461
                      then
                        (if n <= 121452
                         then Extend
                         else if n <= 121460 then Other else Extend)
                      else
                        if n <= 121476
                        then (if n <= 121475 then Other else Extend)
                        else if n <= 121498 then Other else Extend)
                   else
                     if n <= 122879
                     then
                       (if n <= 121504
                        then Other
                        else if n <= 121519 then Extend else Other)
                     else
                       if n <= 122887
                       then (if n <= 122886 then Extend else Other)
                       else if n <= 122904 then Extend else Other)
                else
                  if n <= 123190
                  then
                    (if n <= 122916
                     then
                       (if n <= 122913
                        then Extend
                        else if n <= 122914 then Other else Extend)
                     else
                       if n <= 122922
                       then (if n <= 122917 then Other else Extend)
                       else if n <= 123183 then Other else Extend)
                  else
                    if n <= 125135
                    then
                      (if n <= 123627
                       then Other
                       else if n <= 123631 then Extend else Other)
                    else
                      if n <= 125251
                      then (if n <= 125142 then Extend else Other)
                      else if n <= 125258 then Extend else Other)
             else
               if n <= 127339
               then
                 (if n <= 127167
                  then
                    (if n <= 127123
                     then
                       (if n <= 127019
                        then ExtPict
                        else if n <= 127023 then Other else ExtPict)
                     else
                       if n <= 127150
                       then (if n <= 127135 then Other else ExtPict)
                       else if n <= 127152 then Other else ExtPict)
                  else
                    if n <= 127184
                    then
                      (if n <= 127168
                       then Other
                       else if n <= 127183 then ExtPict else Other)
                    else
                      if n <= 127278
                      then (if n <= 127221 then ExtPict else Other)
                      else if n <= 127279 then ExtPict else Other)
               else
                 if n <= 127374
                 then
                   (if n <= 127345
                    then
                      (if n <= 127340
                       then ExtPict
                       else if n <= 127343 then Other else ExtPict)
                    else
                      if n <= 127359
                      then (if n <= 127357 then Other else ExtPict)
                      else if n <= 127373 then Other else ExtPict)
                 else
                   if n <= 127487
                   then
                     (if n <= 127386
                      then (if n <= 127376 then Other else ExtPict)
                      else if n <= 127461 then Other else RegionalIndicator)
                   else
                     if n <= 127490
                     then (if n <= 127488 then Other else ExtPict)
                     else if n <= 127513 then Other else ExtPict)
          else
            if n <= 129349
            then
              (if n <= 128591
               then
                 (if n <= 127583
                  then
                    (if n <= 127537
                     then
                       (if n <= 127534
                        then Other
                        else if n <= 127535 then ExtPict else Other)
                     else
                       if n <= 127567
                       then (if n <= 127546 then ExtPict else Other)
                       else if n <= 127569 then ExtPict else Other)
                  else
                    if n <= 127994
                    then
                      (if n <= 127589
                       then ExtPict
                       else if n <= 127743 then Other else ExtPict)
                    else
                      if n <= 128317
                      then (if n <= 127999 then Extend else ExtPict)
                      else if n <= 128325 then Other else ExtPict)
               else
                 if n <= 128980
                 then
                   (if n <= 128735
                    then
                      (if n <= 128639
                       then Other
                       else if n <= 128725 then ExtPict else Other)
                    else
                      if n <= 128751
                      then (if n <= 128748 then ExtPict else Other)
                      else if n <= 128762 then ExtPict else Other)
                 else
                   if n <= 129003
                   then
                     (if n <= 128984
                      then ExtPict
                      else if n <= 128991 then Other else ExtPict)
                   else
                     if n <= 129338
                     then (if n <= 129292 then Other else ExtPict)
                     else if n <= 129339 then Other else ExtPict)
            else
              if n <= 129645
              then
                (if n <= 129444
                 then
                   (if n <= 129394
                    then
                      (if n <= 129350
                       then Other
                       else if n <= 129393 then ExtPict else Other)
                    else
                      if n <= 129401
                      then (if n <= 129398 then ExtPict else Other)
                      else if n <= 129442 then ExtPict else Other)
                 else
                   if n <= 129482
                   then
                     (if n <= 129450
                      then ExtPict
                      else if n <= 129453 then Other else ExtPict)
                   else
                     if n <= 129619
                     then (if n <= 129484 then Other else ExtPict)
                     else if n <= 129631 then Other else ExtPict)
              else
                if n <= 129679
                then
                  (if n <= 129655
                   then
                     (if n <= 129647
                      then Other
                      else if n <= 129651 then ExtPict else Other)
                   else
                     if n <= 129663
                     then (if n <= 129658 then ExtPict else Other)
                     else if n <= 129666 then ExtPict else Other)
                else
                  if n <= 917535
                  then
                    (if n <= 917504
                     then (if n <= 129685 then ExtPict else Other)
                     else if n <= 917505 then Control else Other)
                  else
                    if n <= 917759
                    then (if n <= 917631 then Extend else Other)
                    else if n <= 917999 then Extend else Other
type previous_chars =
  | EvenRegionalIndicator
  | ExtPictExtendStar
  | NoPrevious
let break_between previous bp1 bp2 =
  match (bp1, bp2) with
  | (CR, LF) -> false
  | ((Control|CR|LF), _) -> true
  | (_, (Control|CR|LF)) -> true
  | (L, (L|V|LV|LVT)) -> false
  | ((LV|V), (V|T)) -> false
  | ((LVT|T), T) -> false
  | (_, (Extend|ZWJ)) -> false
  | (_, SpacingMark) -> false
  | (Prepend, _) -> false
  | (ZWJ, ExtPict) when previous = ExtPictExtendStar -> false
  | (RegionalIndicator, RegionalIndicator) when
      previous = EvenRegionalIndicator -> false
  | _ -> true
let encode : Uchar.t -> string =
  fun i ->
    let i = Uchar.to_int i in
    if i < 0
    then raise (invalid_arg "UF8.encode")
    else
      if i <= 0x7F
      then String.make 1 (char_of_int i)
      else
        if i <= 0x077F
        then
          (let c0 = char_of_int (((i lsr 6) land 0b00011111) lor 0b11000000) in
           let c1 = char_of_int ((i land 0b00111111) lor 0b10000000) in
           let s = Bytes.create 2 in
           Bytes.set s 0 c0; Bytes.set s 1 c1; Bytes.to_string s)
        else
          if i <= 0xFFFF
          then
            (let c0 =
               char_of_int (((i lsr 12) land 0b00001111) lor 0b11100000) in
             let c1 =
               char_of_int (((i lsr 6) land 0b00111111) lor 0b10000000) in
             let c2 = char_of_int ((i land 0b00111111) lor 0b10000000) in
             let s = Bytes.create 3 in
             Bytes.set s 0 c0;
             Bytes.set s 1 c1;
             Bytes.set s 2 c2;
             Bytes.to_string s)
          else
            if i <= 0x10FFFF
            then
              (let c0 =
                 char_of_int (((i lsr 18) land 0b00000111) lor 0b11110000) in
               let c1 =
                 char_of_int (((i lsr 12) land 0b00111111) lor 0b10000000) in
               let c2 =
                 char_of_int (((i lsr 6) land 0b00111111) lor 0b10000000) in
               let c3 = char_of_int ((i land 0b00111111) lor 0b10000000) in
               let s = Bytes.create 4 in
               Bytes.set s 0 c0;
               Bytes.set s 1 c1;
               Bytes.set s 2 c2;
               Bytes.set s 3 c3;
               Bytes.to_string s)
            else raise (invalid_arg "UTF8.encode")
let decode : string -> int -> (Uchar.t * int) =
  fun s ->
    fun i ->
      let cc = Char.code (s.[i]) in
      if (cc lsr 7) = 0
      then ((Uchar.of_int cc), 1)
      else
        if ((cc lsr 6) land 1) = 0
        then raise (invalid_arg "UTF8.decode")
        else
          if ((cc lsr 5) land 1) = 0
          then
            (let i0 = (cc land 0b00011111) lsl 6 in
             let i1 = (Char.code (s.[i + 1])) land 0b00111111 in
             ((Uchar.of_int (i0 lor i1)), 2))
          else
            if ((cc lsr 4) land 1) = 0
            then
              (let i0 = (cc land 0b00001111) lsl 12 in
               let i1 = ((Char.code (s.[i + 1])) land 0b00111111) lsl 6 in
               let i2 = (Char.code (s.[i + 2])) land 0b00111111 in
               ((Uchar.of_int ((i0 lor i1) lor i2)), 3))
            else
              if ((cc lsr 3) land 1) = 0
              then
                (let i0 = (cc land 0b00000111) lsl 18 in
                 let i1 = ((Char.code (s.[i + 1])) land 0b00111111) lsl 12 in
                 let i2 = ((Char.code (s.[i + 2])) land 0b00111111) lsl 6 in
                 let i3 = (Char.code (s.[i + 3])) land 0b00111111 in
                 ((Uchar.of_int (((i0 lor i1) lor i2) lor i3)), 4))
              else raise (invalid_arg "UTF8.decode")
let look : string -> int -> Uchar.t = fun s -> fun i -> fst (decode s i)
let next : string -> int -> int =
  fun s -> fun i -> let (_, sz) = decode s i in i + sz
let prev : string -> int -> int =
  fun s ->
    fun i ->
      let ps = List.filter (fun i -> i >= 0) [i - 1; i - 2; i - 3; i - 4] in
      let rec try_until_found l =
        match l with
        | [] -> assert false
        | p::ps ->
            (try
               let (_, sz) = decode s p in
               if (p + sz) <> i then assert false; p
             with | Invalid_argument _ -> try_until_found ps) in
      try_until_found ps
let fold : ('a -> Uchar.t -> 'a) -> 'a -> string -> 'a =
  fun f ->
    fun ini ->
      fun s ->
        if s = ""
        then ini
        else
          (let l = String.length s in
           let rec fold' acc i =
             if i > l
             then raise (invalid_arg "UTF.fold")
             else
               if i = l
               then acc
               else (let (u, l) = decode s i in fold' (f acc u) (i + l)) in
           fold' ini 0)
let of_list : Uchar.t list -> string =
  fun l -> String.concat "" (List.map encode l)
let to_list : string -> Uchar.t list =
  fun s -> List.rev (fold (fun acc -> fun x -> x :: acc) [] s)
let grapheme_break : string -> int -> bool =
  fun s ->
    fun pos ->
      let rec previous_ri acc i0 bp1 =
        match bp1 with
        | RegionalIndicator ->
            if i0 = 0
            then not acc
            else
              (let i0 = prev s i0 in
               let c1 = look s i0 in
               let bp1 = gbp c1 in previous_ri (not acc) i0 bp1)
        | _ -> acc in
      let rec previous_pict i0 bp1 =
        match bp1 with
        | Extend ->
            if i0 = 0
            then false
            else
              (let i0 = prev s i0 in
               let c1 = look s i0 in let bp1 = gbp c1 in previous_pict i0 bp1)
        | ExtPict -> true
        | _ -> false in
      if (pos = 0) || (pos >= (String.length s))
      then true
      else
        (let i0 = prev s pos in
         let c1 = look s i0
         and c2 = look s pos in
         let bp1 = gbp c1
         and bp2 = gbp c2 in
         let previous =
           match bp1 with
           | ZWJ ->
               if previous_pict i0 Extend
               then ExtPictExtendStar
               else NoPrevious
           | RegionalIndicator ->
               if previous_ri false i0 RegionalIndicator
               then EvenRegionalIndicator
               else NoPrevious
           | _ -> NoPrevious in
         break_between previous bp1 bp2)
let grapheme_break_after : Uchar.t list -> Uchar.t -> bool =
  fun s ->
    fun c2 ->
      let rec previous_ri s acc bp1 =
        match (bp1, s) with
        | (RegionalIndicator, []) -> not acc
        | (RegionalIndicator, c1::s) ->
            let bp1 = gbp c1 in previous_ri s (not acc) bp1
        | _ -> acc in
      let rec previous_pict s bp1 =
        match (bp1, s) with
        | (ExtPict, _) -> true
        | (Extend, c1::s) -> let bp1 = gbp c1 in previous_pict s bp1
        | _ -> false in
      match s with
      | [] -> true
      | c1::s ->
          let bp1 = gbp c1
          and bp2 = gbp c2 in
           (let previous =
              match bp1 with
              | ZWJ ->
                  if previous_pict s Extend
                  then ExtPictExtendStar
                  else NoPrevious
              | RegionalIndicator ->
                  if previous_ri s false RegionalIndicator
                  then EvenRegionalIndicator
                  else NoPrevious
              | _ -> NoPrevious in
            break_between previous bp1 bp2)
let next_grapheme : string -> int -> int =
  fun s ->
    fun pos ->
      let npos = ref pos in
      try
        while (!npos) < (String.length s) do
          (npos := (next s (!npos));
           if grapheme_break s (!npos) then raise Exit)
          done;
        raise Not_found
      with | Exit -> !npos
let prev_grapheme : string -> int -> int =
  fun s ->
    fun pos ->
      let npos = ref pos in
      try
        while (!npos) > 0 do
          (npos := (prev s (!npos));
           if grapheme_break s (!npos) then raise Exit)
          done;
        0
      with | Exit -> !npos
let fold_grapheme : ('a -> string -> 'a) -> 'a -> string -> 'a =
  fun fn ->
    fun acc ->
      fun s ->
        let pos = ref 0 in
        let res = ref acc in
        while (!pos) < (String.length s) do
          (let npos = next_grapheme s (!pos) in
           let s = String.sub s (!pos) (npos - (!pos)) in
           pos := npos; res := (fn (!res) s))
          done;
        !res

let length : string -> int = fun s ->
  fold (fun i _ -> i + 1) 0 s

let sub s start len =
  let slen = String.length s in
  let rec find pos num =
    if num < 0 then invalid_arg "Utf8.sub (negative index)";
    if num = 0 then pos else
      begin
        if pos >= slen then invalid_arg "Utf8.sub (char out of bound)";
        find (next s pos) (num-1)
      end
  in
  let start = find 0 start in
  let len   = (find start len) - start in
  String.sub s start len
