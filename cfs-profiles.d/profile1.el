;;; 设置默认字体列表，按`C-c C-c'测试字体显示效果。
;;; 另外，你可以使用命令: `describe-char' 来了解光标处字符使用什么字体。
;;; 也可以运行`(print (font-family-list))'来获得当前可用的字体的名称列表
(setq cfs--custom-set-fonts-names
      '(
        ("PragmataPro" "DejaVu Sans Mono" "Consolas" "Inconsolata" "Monaco" "Menlof" "Droid Sans Mono Pro" "Droid Sans Mono" "Source Code Pro" "Lucida Console" "Envy Code R" "Andale Mono" "Lucida Sans Typewriter" "monoOne" "Lucida Typewriter" "Panic Sans" "Bitstream Vera Sans Mono" "HyperFont" "PT Mono" "Ti92Pluspc" "Excalibur Monospace" "Courier New" "Courier" "Cousine" "Fira Mono" "Lekton" "Ubuntu Mono" "Liberation Mono" "M+ 1mn" "BPmono" "Free Mono" "Anonymous Pro" "ProFont" "ProFontWindows" "Latin Modern Mono" "Code 2002" "ProggyCleanTT" "ProggyTinyTT")
        ("微软雅黑" "黑体" "文泉驿等宽微米黑" "Microsoft Yahei" "Microsoft_Yahei"  "Hiragino Sans GB" "文泉驿等宽正黑" "文泉驿正黑" "文泉驿点阵正黑" "新宋体" "宋体" "楷体_GB2312" "仿宋_GB2312" "幼圆" "隶书" "方正姚体" "方正舒体" "方正粗圆_GBK" "华文仿宋" "华文中宋" "华文彩云" "华文新魏" "华文细黑" "华文行楷")
        ("HanaMinB" "SimSun-ExtB" "MingLiU-ExtB" "PMingLiU-ExtB" "MingLiU_HKSCS-ExtB")
       ))

;;; 为每个字号(9 10.5 11.5 12.5 14 16 18 20 22)设置中文调整系数，使中英文等宽度。
(setq cfs--custom-set-fonts-scales
       '(1.13 1.07 1.17 1.07 1.07 1.04 1.05 1.03 1.03))
