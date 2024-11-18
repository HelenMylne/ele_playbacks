#### information        ####
# produce credible interval values for reporting credible intervals instead of mean ± SD

#### MBM ####
load('movement_direction/movement_binomial_agecontrasts.RData')

## calculate mean contrast values for age categories
mean(age1_vs_age2)                               # NEW: -0.006555659, OLD: -0.006308783
mean(age2_vs_age3)                               # NEW: -0.002336704, OLD: -0.002412628
mean(age3_vs_age4)                               # NEW: -0.003489837, OLD: -0.003272488
mean(age1_vs_age4)                               # NEW: -0.009799535, OLD: -0.009458391

## calculate credible intervals for age categories
quantile(age1_vs_age2, prob = c(0.025, 0.975))   # -0.0288651744, -0.0006480073
quantile(age2_vs_age3, prob = c(0.025, 0.975))   # -0.0108177108, -0.0001555761
quantile(age3_vs_age4, prob = c(0.025, 0.975))   # -0.0174373216, -0.0002005946
quantile(age1_vs_age4, prob = c(0.025, 0.975))   #  -0.043381256,  -0.001304859
print('MBM age values calculated')

## calculate mean contrast values for stimulus types
mean(ctd_vs_lion)                                #  0.01209256
mean(ctd_vs_human)                               #  0.01158384
mean(lion_vs_human)                              # -0.0005087185

## calculate credible intervals for stimulus types
quantile(ctd_vs_lion, prob = c(0.025, 0.975))    # 0.0006750686, 0.0596907419
quantile(ctd_vs_human, prob = c(0.025, 0.975))   # 0.0004724996, 0.0591628562
quantile(lion_vs_human, prob = c(0.025, 0.975))  #  -0.02034395,   0.01749618
print('MBM stimulus values calculated')

#### MOM1 ####
rm(list = ls()) ; gc()
load('movement_direction/movement_ordinal_model1_agecontrasts.RData')

## calculate mean contrast values for all age categories
mean(alt_vs_org_awaydirect[,which(age_move_org$f_age_num != 4)])     #  0.0001895325
mean(alt_vs_org_awayangle[ ,which(age_move_org$f_age_num != 4)])     #  0.0003270455
mean(alt_vs_org_neither[   ,which(age_move_org$f_age_num != 4)])     # -8.505571e-06
mean(alt_vs_org_twdsangle[ ,which(age_move_org$f_age_num != 4)])     #  0.0001317071
mean(alt_vs_org_twdsdirect[,which(age_move_org$f_age_num != 4)])     # -0.0006397795

## calculate credible interval of contrast values for all age categories
quantile(alt_vs_org_awaydirect[,which(age_move_org$f_age_num != 4)], prob = c(0.025, 0.975))     # -0.01014897  0.01133813
quantile(alt_vs_org_awayangle[ ,which(age_move_org$f_age_num != 4)], prob = c(0.025, 0.975))     # -0.01153416  0.01715046
quantile(alt_vs_org_neither[   ,which(age_move_org$f_age_num != 4)], prob = c(0.025, 0.975))     # -0.01368334  0.01391876
quantile(alt_vs_org_twdsangle[ ,which(age_move_org$f_age_num != 4)], prob = c(0.025, 0.975))     # -0.01642975  0.01385185
quantile(alt_vs_org_twdsdirect[,which(age_move_org$f_age_num != 4)], prob = c(0.025, 0.975))     # -0.01304913  0.01087674

## calculate contrast values for age category -- move away directly
mean(age1v2_ad) #   0.001370572
mean(age2v3_ad) #   0.002269465
mean(age3v4_ad) #   -0.00192397
mean(age1v4_ad) #   0.002257025

## calculate credible intervals for age category -- move away directly
quantile(age1v2_ad, prob = c(0.025, 0.975)) #  -0.005936094,  0.013262680
quantile(age2v3_ad, prob = c(0.025, 0.975)) # -0.0008714738, 0.0144946310
quantile(age3v4_ad, prob = c(0.025, 0.975)) #  -0.013422460,  0.004190756
quantile(age1v4_ad, prob = c(0.025, 0.975)) #  -0.007260302,  0.019292256

## calculate contrast values for age category -- move away at an angle
mean(age1v2_aa) #  0.0005025695
mean(age2v3_aa) #  0.0004826847
mean(age3v4_aa) #  0.0001518902
mean(age1v4_aa) # -0.0005059808

## calculate credible intervals for age category -- move away at an angle
quantile(age1v2_aa, prob = c(0.025, 0.975)) #  -0.01052428,  0.02249888
quantile(age2v3_aa, prob = c(0.025, 0.975)) # -0.008292568, 0.025590032
quantile(age3v4_aa, prob = c(0.025, 0.975)) #  -0.02305091,  0.01010697
quantile(age1v4_aa, prob = c(0.025, 0.975)) #  -0.01334550,  0.01065824

## calculate contrast values for age category -- move neither towards or away
mean(age1v2_n)  # -0.0003872208
mean(age2v3_n)  #  0.0001858154
mean(age3v4_n)  # -0.0001105394
mean(age1v4_n)  # -0.0004216679

## calculate credible intervals for age category -- move neither towards or away
quantile(age1v2_n, prob = c(0.025, 0.975)) # -0.01686655, 0.01340039
quantile(age2v3_n, prob = c(0.025, 0.975)) # -0.01282690, 0.01477349
quantile(age3v4_n, prob = c(0.025, 0.975)) # -0.01394023, 0.01309663
quantile(age1v4_n, prob = c(0.025, 0.975)) # -0.02175597, 0.01760540

## calculate contrast values for age category -- approach at an angle
mean(age1v2_ta) #  8.109591e-05
mean(age2v3_ta) # -9.345049e-05
mean(age3v4_ta) #   0.000345428
mean(age1v4_ta) #   0.001262607

## calculate credible intervals for age category -- approach at an angle
quantile(age1v2_ta, prob = c(0.025, 0.975)) #  -0.01990183,  0.01369365
quantile(age2v3_ta, prob = c(0.025, 0.975)) #  -0.02435127,  0.01063774
quantile(age3v4_ta, prob = c(0.025, 0.975)) #  -0.01061294,  0.02273476
quantile(age1v4_ta, prob = c(0.025, 0.975)) # -0.006971138, 0.014348712

## calculate contrast values for age category -- approach directly
mean(age1v2_td) #  -0.001567017
mean(age2v3_td) #  -0.002844514
mean(age3v4_td) #   0.001537191
mean(age1v4_td) #  -0.002591983

## calculate credible intervals for age category -- approach directly
quantile(age1v2_td, prob = c(0.025, 0.975)) #  -0.017306992, 0.007788484
quantile(age2v3_td, prob = c(0.025, 0.975)) # -0.0155776844, 0.0009619818
quantile(age3v4_td, prob = c(0.025, 0.975)) #  -0.004569964, 0.014707340
quantile(age1v4_td, prob = c(0.025, 0.975)) #  -0.025286890, 0.008092307
print('MOM1 complete')

#### MOM2 ####
rm(list = ls()) ; gc()
load('movement_direction/moving_ordinal_2bda_stimuluscontrasts.RData')

## produce values for reporting
mean(ctd_vs_lion)   # -3.355116e-17
mean(ctd_vs_human)  # -3.203541e-17
mean(lion_vs_human) # -2.453751e-17
print('MOM2 stimuli complete')

rm(list = ls()) ; gc()
load('movement_direction/moving_ordinal_2bda_agecontrasts.RData')

## calculate mean contrast values for all age categories
mean(alt_vs_org_awaydirect[,which(age_move_org$f_age_num != 4)])     #  0.0001874596
mean(alt_vs_org_awayangle[ ,which(age_move_org$f_age_num != 4)])     #  0.0003329584
mean(alt_vs_org_neither[   ,which(age_move_org$f_age_num != 4)])     # -2.34897e-05
mean(alt_vs_org_twdsangle[ ,which(age_move_org$f_age_num != 4)])     #  0.0001419014
mean(alt_vs_org_twdsdirect[,which(age_move_org$f_age_num != 4)])     # -0.0006388297

## calculate credible interval of contrast values for all age categories
quantile(alt_vs_org_awaydirect[,which(age_move_org$f_age_num != 4)], prob = c(0.025, 0.975))     # -0.01038989  0.01146946
quantile(alt_vs_org_awayangle[ ,which(age_move_org$f_age_num != 4)], prob = c(0.025, 0.975))     # -0.01176603  0.01680378
quantile(alt_vs_org_neither[   ,which(age_move_org$f_age_num != 4)], prob = c(0.025, 0.975))     # -0.01431158  0.01417861
quantile(alt_vs_org_twdsangle[ ,which(age_move_org$f_age_num != 4)], prob = c(0.025, 0.975))     # -0.01631930  0.01406369
quantile(alt_vs_org_twdsdirect[,which(age_move_org$f_age_num != 4)], prob = c(0.025, 0.975))     # -0.01323257  0.01134963

## calculate contrast values for age category -- move away directly
mean(age1v2_ad) #   0.001598347
mean(age2v3_ad) #   0.002304834
mean(age3v4_ad) #  -0.002004788
mean(age1v4_ad) #   0.002430653

## calculate credible intervals for age category -- move away directly
quantile(age1v2_ad, prob = c(0.025, 0.975)) # -0.005527075  0.014176454
quantile(age2v3_ad, prob = c(0.025, 0.975)) # -0.0006061172  0.0146521139
quantile(age3v4_ad, prob = c(0.025, 0.975)) # -0.013834509  0.003949645
quantile(age1v4_ad, prob = c(0.025, 0.975)) # -0.00627880  0.02026156

## calculate contrast values for age category -- move away at an angle
mean(age1v2_aa) #  0.0006133569
mean(age2v3_aa) #  0.0004832358
mean(age3v4_aa) #  0.0001421945
mean(age1v4_aa) # -0.0005143003

## calculate credible intervals for age category -- move away at an angle
quantile(age1v2_aa, prob = c(0.025, 0.975)) # -0.01055368  0.02577884
quantile(age2v3_aa, prob = c(0.025, 0.975)) # -0.008332905  0.026218885
quantile(age3v4_aa, prob = c(0.025, 0.975)) # -0.02377856  0.01021288
quantile(age1v4_aa, prob = c(0.025, 0.975)) # -0.01392762  0.01059961

## calculate contrast values for age category -- move neither towards or away
mean(age1v2_n)  #  -0.000426104
mean(age2v3_n)  #   0.000202071
mean(age3v4_n)  # -0.0001491432
mean(age1v4_n)  # -0.0005120494

## calculate credible intervals for age category -- move neither towards or away
quantile(age1v2_n, prob = c(0.025, 0.975)) # -0.01779582  0.01398801
quantile(age2v3_n, prob = c(0.025, 0.975)) # -0.01296856  0.01515363
quantile(age3v4_n, prob = c(0.025, 0.975)) # -0.01478822  0.01339399
quantile(age1v4_n, prob = c(0.025, 0.975)) # -0.02261908  0.01788545

## calculate contrast values for age category -- approach at an angle
mean(age1v2_ta) #  2.209731e-05
mean(age2v3_ta) # -9.723911e-05
mean(age3v4_ta) #  0.0003817826
mean(age1v4_ta) #   0.001429702

## calculate credible intervals for age category -- approach at an angle
quantile(age1v2_ta, prob = c(0.025, 0.975)) # -0.02406911  0.01364865
quantile(age2v3_ta, prob = c(0.025, 0.975)) # -0.02454164  0.01082393
quantile(age3v4_ta, prob = c(0.025, 0.975)) # -0.01073476  0.02362665
quantile(age1v4_ta, prob = c(0.025, 0.975)) # -0.006466362  0.015029122

## calculate contrast values for age category -- approach directly
mean(age1v2_td) #  -0.001807697
mean(age2v3_td) #  -0.002892902
mean(age3v4_td) #   0.001629954
mean(age1v4_td) #  -0.002834005

## calculate credible intervals for age category -- approach directly
quantile(age1v2_td, prob = c(0.025, 0.975)) # -0.017882509  0.007184391
quantile(age2v3_td, prob = c(0.025, 0.975)) # -0.0158049666  0.0006234332
quantile(age3v4_td, prob = c(0.025, 0.975)) # -0.004147326  0.015474759
quantile(age1v4_td, prob = c(0.025, 0.975)) # -0.02619027  0.00719154
print('MOM2 ages complete')

#### LOM1 ####
rm(list = ls()) ; gc()
load('looking_direction/looking_ordinal_model1_stimuluscontrasts.RData')

## calculate mean value for ctd vs lion
mean(ctd_vs_lion_away)   # -0.0004967693
mean(ctd_vs_lion_side)   # 0.0001507964
mean(ctd_vs_lion_twds)   # 0.0003459729

## calculate credible interval for ctd vs lion
quantile(ctd_vs_lion_away, prob = c(0.025,0.975))   # -0.004288301  0.003072626
quantile(ctd_vs_lion_side, prob = c(0.025,0.975))   # -0.004299423  0.004352422
quantile(ctd_vs_lion_twds, prob = c(0.025,0.975))   # -0.002623217  0.004182680

## calculate mean value for ctd vs human
mean(ctd_vs_human_away)  # 0.000475123
mean(ctd_vs_human_side)  # -0.0001427692
mean(ctd_vs_human_twds)  # -0.0003323538

## calculate credible interval for ctd vs human
quantile(ctd_vs_human_away, prob = c(0.025,0.975))   # -0.002088805  0.003853334
quantile(ctd_vs_human_side, prob = c(0.025,0.975))   # -0.003541611  0.003854272
quantile(ctd_vs_human_twds, prob = c(0.025,0.975))   # -0.003840682  0.001796441

## calculate mean value for lion vs human
mean(lion_vs_human_away) # 0.0009718923
mean(lion_vs_human_side) # -0.0002935656
mean(lion_vs_human_twds) # -0.0006783267

## calculate credible interval for lion vs human
quantile(lion_vs_human_away, prob = c(0.025,0.975))   # -0.002604983  0.005465318
quantile(lion_vs_human_side, prob = c(0.025,0.975))   # -0.005098691  0.005484458
quantile(lion_vs_human_twds, prob = c(0.025,0.975))   # -0.005570919  0.002112949
print('LOM1 stimulus type complete')

rm(list = ls()) ; gc()
load('looking_direction/looking_ordinal_model1_agecontrasts.RData')

## calculate mean values -- all
mean(alt_vs_org_away) #  0.0003579506
mean(alt_vs_org_side) # -4.175601e-05
mean(alt_vs_org_twds) # -0.0003161946

## calculate credible intervals -- all
quantile(alt_vs_org_away, prob = c(0.025,0.975)) # -0.004550505   0.006176830
quantile(alt_vs_org_side, prob = c(0.025,0.975)) # -0.006013325   0.007034322
quantile(alt_vs_org_twds, prob = c(0.025,0.975)) # -0.006122688   0.004022211

## calculate mean contrast values -- look away
mean(away_12) # -0.0007637813
mean(away_23) #  0.0006043386
mean(away_34) #  0.0003146462
mean(away_14) #  0.0007367962

## calculate mean contrast values -- look away
quantile(away_12, prob = c(0.025,0.975)) # -0.007710064  0.005613353
quantile(away_23, prob = c(0.025,0.975)) # -0.003112422  0.007094089
quantile(away_34, prob = c(0.025,0.975)) # -0.004945460  0.005539726
quantile(away_14, prob = c(0.025,0.975)) # -0.007386693  0.009214541

## calculate mean contrast values -- side on
mean(side_12) #  0.0001926507
mean(side_23) #  -5.44491e-05
mean(side_34) #  -6.96339e-05
mean(side_14) # -0.0001332215

## calculate mean contrast values -- side on
quantile(side_12, prob = c(0.025,0.975)) # -0.008808403  0.008317878
quantile(side_23, prob = c(0.025,0.975)) # -0.005986493  0.006578004
quantile(side_34, prob = c(0.025,0.975)) # -0.005535588  0.007022446
quantile(side_14, prob = c(0.025,0.975)) # -0.009683622  0.008986158

## calculate mean contrast values -- look at
mean(twds_12) #  0.0005711306
mean(twds_23) # -0.0005498895
mean(twds_34) # -0.0002450123
mean(twds_14) # -0.0006035747

## calculate mean contrast values -- look at
quantile(twds_12, prob = c(0.025,0.975)) # -0.004552770  0.008620793
quantile(twds_23, prob = c(0.025,0.975)) # -0.007020755  0.002696368
quantile(twds_34, prob = c(0.025,0.975)) # -0.005577821  0.004418952
quantile(twds_14, prob = c(0.025,0.975)) # -0.008940465  0.005420819
print('LOM1 ages complete')

#### LOM2 ####
rm(list = ls()) ; gc()
load('looking_direction/looking_ordinal_model2bda_agecontrasts.RData')

## calculate mean contrast values for all age categories
mean(alt_vs_org_away[,which(age_look_org$f_age_num != 4)])   #   0.000321588
mean(alt_vs_org_side[,which(age_look_org$f_age_num != 4)])   # -2.660703e-05
mean(alt_vs_org_twds[,which(age_look_org$f_age_num != 4)])   #  -0.000294981

## calculate credible interval of contrast values for all age categories
quantile(alt_vs_org_away[,which(age_look_org$f_age_num != 4)], prob = c(0.025,0.975))   # -0.004514154  0.005901860
quantile(alt_vs_org_side[,which(age_look_org$f_age_num != 4)], prob = c(0.025,0.975))   # -0.005868090  0.006849674
quantile(alt_vs_org_twds[,which(age_look_org$f_age_num != 4)], prob = c(0.025,0.975))   # -0.005925277  0.003966568

## calculate mean contrast values -- look away
mean(age1v2_away) # -0.0008361118
mean(age2v3_away) #  0.0005874849
mean(age3v4_away) #  0.0002658452
mean(age1v4_away) #  0.0005675312

## calculate mean contrast values -- look away
quantile(age1v2_away, prob = c(0.025,0.975)) # -0.007804199  0.005332523
quantile(age2v3_away, prob = c(0.025,0.975)) # -0.003073279  0.007156208
quantile(age3v4_away, prob = c(0.025,0.975)) # -0.004732203  0.005100754
quantile(age1v4_away, prob = c(0.025,0.975)) # -0.007446372  0.008925398

## calculate mean contrast values -- side on
mean(age1v2_side) #  0.0001951941
mean(age2v3_side) # -3.917711e-05
mean(age3v4_side) # -5.245311e-05
mean(age1v4_side) # -9.192425e-05

## calculate mean contrast values -- side on
quantile(age1v2_side, prob = c(0.025,0.975)) # -0.009154130  0.008000188
quantile(age2v3_side, prob = c(0.025,0.975)) # -0.006035804  0.006574300
quantile(age3v4_side, prob = c(0.025,0.975)) # -0.005196361  0.006673408
quantile(age1v4_side, prob = c(0.025,0.975)) # -0.009467578  0.008588611

## calculate mean contrast values -- look at
mean(age1v2_twds) #  0.0006409177
mean(age2v3_twds) # -0.0005483078
mean(age3v4_twds) # -0.0002133921
mean(age1v4_twds) # -0.0004756069

## calculate mean contrast values -- look at
quantile(age1v2_twds, prob = c(0.025,0.975)) # -0.004427701  0.008973402
quantile(age2v3_twds, prob = c(0.025,0.975)) # -0.007023819  0.002668920
quantile(age3v4_twds, prob = c(0.025,0.975)) # -0.005302574  0.004245412
quantile(age1v4_twds, prob = c(0.025,0.975)) # -0.008458264  0.005685631
print('LOM2 complete')

#### NBM ####
rm(list = ls()) ; gc()
load('nearest_neighbour/neighbour_binomial_stimuluscontrasts.RData')

## produce values for reporting
mean(ctd_vs_lion)    # 0.0009149998
mean(ctd_vs_human)   # 0.0002481317
mean(lion_vs_human)  # -0.0006668681

rm(list = ls()) ; gc()
load('nearest_neighbour/neighbour_binomial_agecontrasts.RData')

## save contrasts
save.image('nearest_neighbour/neighbour_binomial_agecontrasts.RData')

## produce mean values for reporting -- increase focal age from 1, target = 1
mean(contrast_11v21)  # -0.0003636851
mean(contrast_11v31)  #  0.002193879
mean(contrast_11v41)  #  0.002554144

## produce credible interval values for reporting -- increase focal age from 1, target = 1
quantile(contrast_11v21, prob = c(0.025,0.975))  # -0.004146166  0.002854362
quantile(contrast_11v31, prob = c(0.025,0.975))  # -0.001306883  0.012072590
quantile(contrast_11v41, prob = c(0.025,0.975))  # -0.001827909  0.014049780

## produce mean values for reporting -- increase focal age from 1, target = 2
mean(contrast_12v22)  #  0.0006762435
mean(contrast_12v32)  #  0.0001698291
mean(contrast_12v42)  #  0.0004824931

## produce credible interval values for reporting -- increase focal age from 1, target = 2
quantile(contrast_12v22, prob = c(0.025,0.975))  # -0.002510585  0.004950938
quantile(contrast_12v32, prob = c(0.025,0.975))  # -0.003363597  0.003967548
quantile(contrast_12v42, prob = c(0.025,0.975))  # -0.003189126  0.004905612

## produce mean values for reporting -- increase focal age from 1, target = 3
mean(contrast_13v23)  # -0.003204741
mean(contrast_13v33)  # -0.003147912
mean(contrast_13v43)  # -0.002534239

## produce credible interval values for reporting -- increase focal age from 1, target = 3
quantile(contrast_13v23, prob = c(0.025,0.975))  # -0.0201954687  0.0005220252
quantile(contrast_13v33, prob = c(0.025,0.975))  # -0.0200416712  0.0006477576
quantile(contrast_13v43, prob = c(0.025,0.975))  # -0.018604790  0.001342357

## produce mean values for reporting -- increase focal age from 1, target = 4
mean(contrast_14v24)  # -0.002325231
mean(contrast_14v34)  # -0.00181654
mean(contrast_14v44)  # -0.002628319

## produce credible interval values for reporting -- increase focal age from 1, target = 4
quantile(contrast_14v24, prob = c(0.025,0.975))  # -0.01591046  0.00210529
quantile(contrast_14v34, prob = c(0.025,0.975))  # -0.01482124  0.00288720
quantile(contrast_14v44, prob = c(0.025,0.975))  # -0.01685424  0.00213087

## produce mean values for reporting -- increase focal age from 2, target = 1
mean(contrast_21v31)  #  0.002557564
mean(contrast_21v41)  #  0.002917829

## produce credible interval values for reporting -- increase focal age from 2, target = 1
quantile(contrast_21v31, prob = c(0.025,0.975))  # -0.001187354  0.012742312
quantile(contrast_21v41, prob = c(0.025,0.975))  # -0.001021151  0.014735428

## produce mean values for reporting -- increase focal age from 2, target = 2
mean(contrast_22v32)  # -0.0005064144
mean(contrast_22v42)  # -0.0001937504

## produce credible interval values for reporting -- increase focal age from 2, target = 2
quantile(contrast_22v32, prob = c(0.025,0.975))  # -0.003091884  0.001256055
quantile(contrast_22v42, prob = c(0.025,0.975))  # -0.002792905  0.002082361

## produce mean values for reporting -- increase focal age from 2, target = 3
mean(contrast_23v33)  #  5.68287e-05
mean(contrast_23v43)  #  0.0006705026

## produce credible interval values for reporting -- increase focal age from 2, target = 3
quantile(contrast_23v33, prob = c(0.025,0.975))  # -0.001866292  0.002085309
quantile(contrast_23v43, prob = c(0.025,0.975))  # -0.000996925  0.004156143

## produce mean values for reporting -- increase focal age from 2, target = 4
mean(contrast_24v34)  #  0.0005086905
mean(contrast_24v44)  # -0.0003030881

## produce credible interval values for reporting -- increase focal age from 2, target = 4
quantile(contrast_24v34, prob = c(0.025,0.975))  # -0.001762194  0.003905255
quantile(contrast_24v44, prob = c(0.025,0.975))  # -0.004140168  0.002992363

## produce mean values for reporting -- increase focal age from 3
mean(contrast_31v41)  #  0.0003602652
mean(contrast_32v42)  #  0.000312664
mean(contrast_33v43)  #  0.0006136739
mean(contrast_34v44)  # -0.0008117786

## produce credible interval values for reporting -- increase focal age from 3
quantile(contrast_31v41, prob = c(0.025,0.975))  # -0.007162807  0.009471052
quantile(contrast_32v42, prob = c(0.025,0.975))  # -0.001790487  0.002987172
quantile(contrast_33v43, prob = c(0.025,0.975))  # -0.001160324  0.004124394
quantile(contrast_34v44, prob = c(0.025,0.975))  # -0.004798068  0.001617911
print('increase focal interval complete')

## produce mean values for reporting -- increase target age, focal = 1
mean(contrast_11v12)  # -0.0001713666
mean(contrast_11v13)  #  0.004169344
mean(contrast_11v14)  #  0.003050537
mean(contrast_12v13)  #  0.00434071
mean(contrast_12v14)  #  0.003221904
mean(contrast_13v14)  # -0.001118807

## produce credible interval values for reporting -- increase target age, focal = 1
quantile(contrast_11v12, prob = c(0.025,0.975))  #  -0.004190519   0.003423550
quantile(contrast_11v13, prob = c(0.025,0.975))  # -2.131456e-05  2.259680e-02
quantile(contrast_11v14, prob = c(0.025,0.975))  #   -0.00125202    0.01716440
quantile(contrast_12v13, prob = c(0.025,0.975))  #  8.150329e-07  2.277936e-02
quantile(contrast_12v14, prob = c(0.025,0.975))  #  -0.001283573   0.017738829
quantile(contrast_13v14, prob = c(0.025,0.975))  #  -0.015913495   0.008567241

## produce mean values for reporting -- increase target age, focal = 2
mean(contrast_21v22)  #  0.0008685619
mean(contrast_21v23)  #  0.001328288
mean(contrast_21v24)  #  0.001088992
mean(contrast_22v23)  #  0.0004597256
mean(contrast_22v24)  #  0.0002204296
mean(contrast_23v24)  # -0.000239296

## produce credible interval values for reporting -- increase target age, focal = 2
quantile(contrast_21v22, prob = c(0.025,0.975))  # -0.001977431  0.004909770
quantile(contrast_21v23, prob = c(0.025,0.975))  # -0.001072769  0.005872008
quantile(contrast_21v24, prob = c(0.025,0.975))  # -0.001752873  0.005529423
quantile(contrast_22v23, prob = c(0.025,0.975))  # -0.001187772  0.003152336
quantile(contrast_22v24, prob = c(0.025,0.975))  # -0.002036916  0.002963579
quantile(contrast_23v24, prob = c(0.025,0.975))  # -0.002845948  0.001834309

## produce mean values for reporting -- increase target age, focal = 3
mean(contrast_31v32)  # -0.002195416
mean(contrast_31v33)  # -0.001172448
mean(contrast_31v34)  # -0.0009598818
mean(contrast_32v33)  #  0.001022969
mean(contrast_32v34)  #  0.001235534
mean(contrast_33v34)  #  0.0002125658

## produce credible interval values for reporting -- increase target age, focal = 3
quantile(contrast_31v32, prob = c(0.025,0.975))  # -0.012089666  0.001222292
quantile(contrast_31v33, prob = c(0.025,0.975))  # -0.009711406  0.002688899
quantile(contrast_31v34, prob = c(0.025,0.975))  # -0.009346602  0.003395367
quantile(contrast_32v33, prob = c(0.025,0.975))  # -0.0003891057  0.0042899204
quantile(contrast_32v34, prob = c(0.025,0.975))  # -0.0002341442  0.0051664962
quantile(contrast_33v34, prob = c(0.025,0.975))  # -0.002012479  0.003052807

## produce mean values for reporting -- increase target age, focal = 4
mean(contrast_41v42)  # -0.002243017
mean(contrast_41v43)  # -0.0009190388
mean(contrast_41v44)  # -0.002131926
mean(contrast_42v43)  #  0.001323979
mean(contrast_42v44)  #  0.0001110918
mean(contrast_43v44)  # -0.001212887

## produce credible interval values for reporting -- increase target age, focal = 4
quantile(contrast_41v42, prob = c(0.025,0.975))  #   -0.01331430    0.00191518
quantile(contrast_41v43, prob = c(0.025,0.975))  #  -0.010103446   0.004071682
quantile(contrast_41v44, prob = c(0.025,0.975))  #   -0.01328095    0.00253407
quantile(contrast_42v43, prob = c(0.025,0.975))  # -0.0004213015  0.0058727647
quantile(contrast_42v44, prob = c(0.025,0.975))  #  -0.003158156   0.003773460
quantile(contrast_43v44, prob = c(0.025,0.975))  #  -0.006111186   0.001101227
print('increase target interval complete')

print('NBM complete')