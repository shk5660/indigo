---
title: "Image to Palette with R"
layout: post
date: 2018-01-11 02:51
tag:
- R
- imager
- palette
- vis
category: blog
author: sang-hyeon
description: Palette production project with R
---

## Intro
-----

------------------------------------------------------------------------

최근 **Color Quantization**에 관련된 흥미로운 게시물들을 읽었다. Color Quantization이라는 개념은 보기와는 다르게 생각보다 간단한 개념이다. 우리는 실생활 속에서 항상 색상 양자화를 하며 살아가고 있다. 하나의 이미지(우리가 일상 속에서 마주치는 수 많은 View들)는 엄청나게 많은 종류의 색깔을 가지고 있다. 하지만 우리가 직관적으로 이를 인식할 때에, 그 많은 색깔을 하나하나 전부 받아들이지 않는다.

![](http://static.pexels.com/photos/515560/pexels-photo-515560.jpeg)

예를 들어, 노을이 지는 풍경을 바라볼 때에 노을의 그라데이션 속에 있는 모든 색깔을 하나하나 인지하며, '아, 정말 아름답다!'라고 생각하지 않는다는 것이다. 주홍빛, 조금 더 옅은, 조금 더 진한 주홍빛, 그리고 불타는 노을과 맞닿아 있는 약간은 덜 익은 밤하늘의 색. 이렇게 색깔을 어느정도 범주화하여 받아들인다. 아래 게시물들은 R을 이용하여 색상을 양자화 시켜보는 방법을 담고 있다. 글들을 읽던 중, 양자화 방법을 조금 더 응용하여 재미있는 실험을 하고 싶어졌다.

> **About Color Quantization**
>
> -   <https://www.r-bloggers.com/color-quantization-in-r/>
> -   <http://lumiamitie.github.io/r/imager-color-quantization/>

나는 한옥에 대해 조사하고 발표 하려고 한다. 이 때에 프레젠테이션 템플릿에 한옥의 아름다운 색깔을 담을 수 있다면, 보다 매력적이고 전달력있는 발표가 될 것이다! 이 때 프레젠테이션에 사용할 색깔을 한옥 사진으로부터 뽑아내려고 한다.

<br /><br />

## 1. Quantizing Image Using K-Means
---------------------------------

먼저 팔레트화 시킬 한옥 이미지를 불러오자.

``` r
# Required Packages
library(imager) 
library(tidyverse)
library(gridExtra)

hanok <- load.image('hanok_img.jpg')
```

![](assets/post_images/2018-01-10-image-to-palette/hanok_img.jpg)

다음으로 K-means를 이용하여 RGB 값들을 K개의 대표 색상으로 Clustering하고, PCA를 이용하여 2차원에 표현할 수 있도록 하는 정보들을 담은 데이터 프레임을 생성한다.

``` r
img_quantize <- function(img, level){

  img_df <- img %>%
    as.data.frame(wide = 'c') %>%
    tbl_df()

  img_cluster <- img_df %>%
    select(-x, -y) %>%
    kmeans(level, algorithm = 'Lloyd', iter.max = 100)

  cluster_colors <- img_cluster$centers %>%
    tbl_df() %>%
    mutate(label = as.character(1:nrow(img_cluster$centers)))

  img_df_cluster <- img_df %>%
    mutate(label = as.character(img_cluster$cluster)) %>%
    left_join(cluster_colors, by = 'label')

  img_PCA <- img_df_cluster %>%
    select(3:5) %>%
    prcomp(center = TRUE, scale = TRUE)

  img_df_cluster <- img_df_cluster %>%
    mutate(
      u = img_PCA$x[,1],
      v = img_PCA$x[,2]
    )

  return(img_df_cluster)
}
```

``` r
img_quantize(hanok, 6) # 6개의 색으로 Clustering
```

    ## # A tibble: 498,200 x 11
    ##        x     y c.1.x c.2.x c.3.x label c.1.y c.2.y c.3.y     u     v
    ##    <int> <int> <dbl> <dbl> <dbl> <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
    ##  1     1     1 0.898 0.910 0.929 1     0.907 0.898 0.886 -2.28 0.165
    ##  2     2     1 0.898 0.910 0.929 1     0.907 0.898 0.886 -2.28 0.165
    ##  3     3     1 0.898 0.910 0.929 1     0.907 0.898 0.886 -2.28 0.165
    ##  4     4     1 0.898 0.910 0.929 1     0.907 0.898 0.886 -2.28 0.165
    ##  5     5     1 0.898 0.910 0.929 1     0.907 0.898 0.886 -2.28 0.165
    ##  6     6     1 0.898 0.910 0.929 1     0.907 0.898 0.886 -2.28 0.165
    ##  7     7     1 0.898 0.910 0.929 1     0.907 0.898 0.886 -2.28 0.165
    ##  8     8     1 0.898 0.910 0.929 1     0.907 0.898 0.886 -2.28 0.165
    ##  9     9     1 0.898 0.910 0.929 1     0.907 0.898 0.886 -2.28 0.165
    ## 10    10     1 0.898 0.910 0.929 1     0.907 0.898 0.886 -2.28 0.165
    ## # ... with 498,190 more rows

위 데이터프레임에서 x가 들어간 3개의 열은 Clustering 전의 RGB 값이고, y가 들어간 3개의 열은 Clustering 후의 RGB 대표값이다.(Cluster의 RGB 평균 값)

<br /><br />

## 2. Visualization
----------------

------------------------------------------------------------------------

위에서 K개로 군집화 된 색상을 한 눈에 볼 수 있는 여러가지 시각화 함수를 만들어 보았다. 먼저 양자화 후의 이미지를 보여주는 함수다.

``` r
# After Image
converted_img <- function(img_df_cluster, img){

  back_to_img <- img_df_cluster %>%
    select(x, y, c.1.y, c.2.y, c.3.y) %>%
    gather(key = 'cc', value = 'value', starts_with('c.')) %>%
    mutate(cc = gsub('c\\.', '', cc)) %>%
    mutate(cc = as.numeric(gsub('\\.y', '', cc)))

  img_cluster_result <- back_to_img %>%
    as.cimg(dim = dim(img))

  rg <- grid::rasterGrob(img_cluster_result, interpolate = TRUE)

  qplot(1:10, 1:10, geom = 'blank') +
    annotation_custom(rg, xmin = -Inf, xmax = Inf,
                      ymin = -Inf, ymax = Inf) +
    geom_point(alpha = 0) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0),
      axis.title = element_blank(),
      axis.text  = element_blank(),
      axis.ticks = element_blank()
    )
}
```

`converted_img` 함수는 만들기가 참 어려웠다. 왜냐하면 후에 `girdExtra` 패키지를 이용하여 만들어진 플롯들을 한 번에 표현할 계획이었는데, `grid.arrange` 함수 안에 `plot` 객체를 넣을 수 없기 때문이다. 따라서 `ggplot2`로 그래프를 그려야했다.

막대 그래프와 PCA를 통해 2차원으로 줄여진 RGB 값의 산점도이다. 결과로 어떤 그림이 나오는지는 이후에 한번에 보도록 하자.

``` r
# Bar Graph
converted_bar <- function(img_df_cluster) {
  img_df_cluster %>%
    ggplot(aes(reorder(label, c.1.y),
               fill = rgb(c.1.y,
                          c.2.y,
                          c.3.y))) +
    geom_bar() +
    scale_fill_identity() +
    theme_light() +
    theme(
      axis.title = element_blank(),
      axis.text  = element_blank(),
      axis.ticks = element_blank()
    )
}
```

``` r
# PCA Scatter Plot
converted_scatter <- function(img_df_cluster){
  img_df_cluster %>%
    sample_n(size = 0.1 * nrow(img_df_cluster)) %>%
    ggplot(aes(x = u,
               y = v,
               col = rgb(c.1.y,
                         c.2.y,
                         c.3.y))) +
    geom_point(size = 1.5) +
    scale_color_identity() +
    theme_light() +
    theme(
      axis.title = element_blank(),
      axis.text  = element_blank(),
      axis.ticks = element_blank()
    )
}
```

시각화 함수들을 정의하긴 했지만, 일일이 그래프를 그리며 관찰하기엔 너무 번거롭다. 또한 Cluster의 수를 몇개로 설정하느냐에 따라 다른 팔레트가 만들어 질 것이다. 때문에 우리의 목표는 K level에 따라 어떤 팔레트가 등장하는 지 한 눈에 살펴보고, **가장 마음에 드는 K level을 선택**할 수 있게 하는 것이다.

먼저 한 Level에 대해 위 세 개의 그래프를 그려주는 함수를 정의하자.

``` r
vis <- function(img, level){
  img_quantized <- img %>% img_quantize(level)

  vis1 <- converted_img(img_quantized, img)
  vis2 <- converted_bar(img_quantized)
  vis3 <- converted_scatter(img_quantized)

  grid.arrange(vis1, vis2, vis3, ncol = 1, nrow = 3)
}
```

다음으로 여러 개의 K level별로 비교하는 함수를 정의한다.

``` r
vis_compare <- function(img, levels){
  plot_list <- lapply(levels, function(x) vis(img, x))

  col <- length(levels)

  grid.arrange(grobs = lapply(plot_list, grid::grobTree), ncol = col)
}
```

드디어 결과물을 볼 차례다! K를 4, 6, 8로 설정해보자.

``` r
vis_compare(hanok, levels = c(4, 6, 8))
```

<img src="_posts/posting_files/figure-markdown_github/2018-01-10-image-to-palette/unnamed-chunk-10-4.png" style="display: block; margin: auto;" /> 

K level에 따라 각각 다른 대표 색상들이 뽑혔다. 난 개인적으로 이 중 K = 8 일때의 색상 값들이 제일 마음에 든다. 나무의 색깔들이 잘 표현되어 있는 듯 하다. 이제 선택한 8개의 색상에 대한 팔레트를 만들어 볼 차례다.

<br /><br />

## 3. Make Palette
---------------

------------------------------------------------------------------------

팔레트를 만들 때 실제로 필요한 것은 내가 선택한 8개의 색깔에 대한 Hex 값들이다. K level을 설정하여 Hex 값을 반환하는 함수를 생성하자.

``` r
img2hex <- function(image, k = 4){

  image_df <- image %>%
    as.data.frame(wide = 'c') %>%
    tbl_df()

  image_cluster <- image_df %>%
    select(-x, -y) %>%
    kmeans(k)

  cluster_colors <- image_cluster$centers %>%
    tbl_df() %>%
    mutate(label = as.character(1:nrow(image_cluster$centers)))

  cluster_colors_df <- as.data.frame(cluster_colors[, 1:3])

  cluster_colors_df_order <- cluster_colors_df[order(cluster_colors_df[ ,1]), ]
  
  r <- cluster_colors_df_order[, 1] * 255
  g <- cluster_colors_df_order[, 2] * 255
  b <- cluster_colors_df_order[, 3] * 255

  rgb2hex <- function(r,g,b) rgb(r, g, b, maxColorValue = 255)
  hex <- rgb2hex(r, g, b)

  return(hex)
}
```

``` r
hex_hanok <- img2hex(hanok, k = 8)
hex_hanok
```

    ## [1] "#291917" "#374141" "#636D65" "#80411D" "#98958B" "#C2914B" "#C5C2BE"
    ## [8] "#E7E5E2"

실제로 사용할 수 있는 Hex 값의 모음이 만들어졌다! 여기까지의 결과물을 통해서도 어느정도 사용할만한 팔레트를 만들었지만, 조금 더 발전시켜 보도록 하자.

아래의 함수는 [Wes Anderson Palette](https://github.com/karthik/wesanderson)를 참고하여 구현하였다.

``` r
hex2pal <- function (hex, n, type = c("discrete", "continuous")) {
  type <- match.arg(type)
  pal <- hex
  if (is.null(pal))
    stop("Palette not found.")
  if (missing(n)) {
    n <- length(pal)
  }
  if (type == "discrete" && n > length(pal)) {
    stop("Number of requested colors greater than what palette can offer")
  }
  out <- switch(type, continuous = colorRampPalette(pal)(n),
                discrete = pal[1:n])
  structure(out, class = "palette", name = deparse(substitute(hex)))
}

print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 3, family = "serif")
}
```

``` r
pal_hanok <- hex2pal(hex_hanok, n = 8, type = 'discrete')
pal_hanok
```

<img src="_posts/posting_files/figure-markdown_github/2018-01-10-image-to-palette/unnamed-chunk-14-1.png" style="display: block; margin: auto;" /> 

나는 이 팔레트에서 마음에 드는 5가지의 색깔만 뽑아쓰고 싶다.

``` r
pal_hanok <- pal_hanok[c(1, 4, 6, 2, 5)]
pal_hanok
```

    ## [1] "#291917" "#80411D" "#C2914B" "#374141" "#98958B"

이제 만들어진 팔레트를 이용해서 실제로 그래프를 한번 그려보자!(데이터는 편의상 한옥과 1도 관계없는 `diamonds`를 사용하였다!)

``` r
ggplot(diamonds, aes(cut, fill = cut)) +
  geom_bar() +
  scale_fill_manual(values = pal_hanok)
```

<img src="_posts/posting_files/figure-markdown_github/2018-01-10-image-to-palette/unnamed-chunk-16-1.png" style="display: block; margin: auto;" /> 

한옥의 아름다움이 고스란히 담긴 훌륭한(??) 그래프가 완성되었다!

<br /><br />

## Outro
-----

------------------------------------------------------------------------

지금까지의 내용들을 학회 컨퍼런스에서 발표하며, 있어보이기 위해(ㅎ) 패키지화를 시켰다.(솔직히 패키지라고 말하기 부끄럽다.) [바로 여기](https://github.com/shk5660/img2pal)에 올려놓았고, `devtools::install_github('shk5660/img2pal)`을 실행해서 설치 할 수 있다.

하지만 만들어놓고 보니 정말이지 패키지라고 부르기 부끄러운, 조악한 퀄리티를 가지고 있다. 이번 겨울동안 다시 코드들을 사용하기 편하게 재정비하고, 조금 덜 부끄러운 패키지로 업그레이드 시켜보려 한다.

그리고 마지막으로 좋은 사이트를 하나 소개하려한다. [이 사이트](https://www.canva.com/color-palette/)는 사진에서 팔레트를 추출해주는 웹페이지이다. 매우 빠른데다가 예쁘게 잘 뽑아준다!! 몇일동안 개고생한 나에게 심심한 위로를 건낸다.
