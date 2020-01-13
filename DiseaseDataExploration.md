ExploratoryPrevSev
================
LRA
1/13/2020

## Exploratory analysis of wasting disease prevalence and severity from 2019 intertidal surveys

These plots show WD prevalence and severity based on a subset of the
Eelisa output. Once the full output is available, I will update the
plots and
analysis.

### Summary table of transect-level prevalence and severity by tidal height within region

    ## # A tibble: 12 x 6
    ## # Groups:   Region [?]
    ##    Region TidalHeight   prev     sev lesion count
    ##    <ord>  <chr>        <dbl>   <dbl>  <dbl> <int>
    ##  1 AK     L           0.518  0.0740  108.     340
    ##  2 AK     U           0.588  0.0756   88.7    337
    ##  3 BC     L           0.202  0.0432   53.2    129
    ##  4 BC     U           0.107  0.0304   28.3    121
    ##  5 WA     L           0.689  0.0790  350.     244
    ##  6 WA     U           0.582  0.0891  281.     225
    ##  7 OR     L           0.0922 0.00274   5.07   293
    ##  8 OR     U           0.188  0.0204   21.1    266
    ##  9 BB     L           0.418  0.0470  115.     261
    ## 10 BB     U           0.410  0.0689  136.     268
    ## 11 SD     L           0.412  0.0898   14.9    277
    ## 12 SD     U           0.371  0.0549    7.43   283

### Plot of transect-level prevalence

<<<<<<< HEAD
=======
``` r
ggplot(data=disease_most_summ,aes(x=Region,y=prev,fill=TidalHeight))+geom_boxplot()+
  stat_summary(fun.y=mean,geom="point",shape=4,size=4,position=position_dodge(width=0.75))+
  geom_text(data=disease_most_summ2,aes(x=Region,y=1.1,label=count),position=position_dodge(width=0.75))+
  scale_y_continuous(limits=c(0,1.12),breaks=c(0,0.25,0.5,0.75,1.0))+
  scale_fill_manual(values=c("royal blue","light blue"))+
  ylab("Wasting Disease Prevalence")+
  theme_bw()+
  theme()
```

>>>>>>> 8d28d1020ea87037e145994836b1a7b117515702
![](DiseaseDataExploration_files/figure-gfm/prevalence-1.png)<!-- -->

This plot shows the distributions of transect-level prevalence by tidal
height in each region. The black bar in the box-plot indicates the
median prevalence, the X indicates the mean. Numbers at the top of the
plot indicate the number of measurements for each box-plot. Note that
the total number of shoots per tidal height should be 300 for BC, WA,
OR, and SD and 360 for AK and BB. More measurements will be added when
the complete Eelisa output is available.

Interesting pattern: AK and WA have mean prevalence above 50%, BB and SD
have prevalence between 25 and 50% and BC and OR have mean prevalence
<<<<<<< HEAD
below 25%. A lot of variability, especially in BB.

### Plot of transect-level severity

=======
below 25%. A lot of variability, especially in
BB.

### Plot of transect-level severity

``` r
ggplot(data=disease_most_summ,aes(x=Region,y=sev,fill=TidalHeight))+geom_boxplot()+
  stat_summary(fun.y=mean,geom="point",shape=4,size=4,position=position_dodge(width=0.75))+
  geom_text(data=disease_most_summ2,aes(x=Region,y=0.52,label=count),position=position_dodge(width=0.75))+
  scale_y_continuous(limits=c(0,0.52),breaks=c(0,0.1,0.2,0.3,0.4,0.5))+
  scale_fill_manual(values=c("royal blue","light blue"))+
  ylab("Wasting Disease Severity")+
  theme_bw()+
  theme()
```

>>>>>>> 8d28d1020ea87037e145994836b1a7b117515702
![](DiseaseDataExploration_files/figure-gfm/severity-1.png)<!-- -->

Similar to above, this plot shows transect-level severity by tidal
height within region. The black bar in the box-plot indicates the median
prevalence, the X indicates the mean. Numbers at the top of the plot
indicate the number of measurements for each box-plot. Note that the
total number of shoots per tidal height should be 300 for BC, WA, OR,
and SD and 360 for AK and BB. More measurements will be added when the
complete Eelisa output is available.

The regions are less differentiated, but we see a similar pattern -
lowest severity at BC and OR, highest at AK and WA.
