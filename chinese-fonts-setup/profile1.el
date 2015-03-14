;;; `cfs--custom-set-fonts-names' 列表有3个子列表，第1个为英文字体列表，第2个为中文字体列表，
;;; 第3个列表中的字体用于显示不常用汉字，每一个字体列表中，*第一个* *系统存在* 的字体将被使用。
;;; 将光标移动到上述列表中，按 `C-c C-c' 可以测试字体显示效果。
(setq cfs--custom-set-fonts-names
      '(
        ("PragmataPro" "Monaco" "Consolas" "Menlof" "DejaVu Sans Mono" "Droid Sans Mono Pro" "Droid Sans Mono" "Inconsolata" "Source Code Pro" "Lucida Console" "Envy Code R" "Andale Mono" "Lucida Sans Typewriter" "monoOne" "Lucida Typewriter" "Panic Sans" "Bitstream Vera Sans Mono" "HyperFont" "PT Mono" "Ti92Pluspc" "Excalibur Monospace" "Courier New" "Courier" "Cousine" "Fira Mono" "Lekton" "Ubuntu Mono" "Liberation Mono" "M+ 1mn" "BPmono" "Free Mono" "Anonymous Pro" "ProFont" "ProFontWindows" "Latin Modern Mono" "Code 2002" "ProggyCleanTT" "ProggyTinyTT")
        ("黑体" "文泉驿等宽微米黑" "Microsoft Yahei" "Microsoft_Yahei" "微软雅黑" "Hiragino Sans GB" "文泉驿等宽正黑" "文泉驿正黑" "文泉驿点阵正黑" "新宋体" "宋体" "楷体_GB2312" "仿宋_GB2312" "幼圆" "隶书" "方正姚体" "方正舒体" "方正粗圆_GBK" "华文仿宋" "华文中宋" "华文彩云" "华文新魏" "华文细黑" "华文行楷")
        ("HanaMinB" "SimSun-ExtB" "MingLiU-ExtB" "PMingLiU-ExtB" "MingLiU_HKSCS-ExtB")
       ))

;;; 为每个英文字体字号 (9 10.5 11.5 12.5 14 16 18 20 22) 设置对应的中文字体字号，使中英文等宽。
;;; 将光标移动到 `cfs--custom-set-chinese-fontsizes-steps' 列表中各个数字上：
;;; 1. C-c C-c 查看光标处中文字号的对齐效果。
;;; 2. C-<up> 增大光标处中文字号的大小，同时显示对齐效果。
;;; 3. C-<down> 减小光标处中文字号大小, 同时显示对齐效果。
(setq cfs--custom-set-chinese-fontsizes-steps
       '(10.0 10.5 12.0 12.5 14.5 16.5 18 20.5 22.5))
