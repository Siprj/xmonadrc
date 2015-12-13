Config
    { font = "-*-Fixed-Bold-R-Normal-*-13-*-*-*-*-*-*-*"
    , borderColor = "black"
    , border = TopB
    , bgColor = "black"
    , fgColor = "grey"
    , position = TopW L 100
    , allDesktops = True
    , commands =
        [ Run Network "enp4s0" ["-L","0","-H","32","--normal","green","--high","red"] 10
        , Run Network "wlp7s0" ["-L","0","-H","32","--normal","green","--high","red"] 10
        , Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 10
        , Run Memory ["-t","Mem: <usedratio>%"] 10
        , Run Swap [] 10
        , Run Com "uname" ["-s","-r"] "" 36000
        , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
        , Run Volume "default" "Master" [] 10
        , Run StdinReader
        , Run Kbd [("cz", "cz"), ("us", "us")]
        ]
    , sepChar = "%"
    , alignSep = "}{"
    , template = "%StdinReader% | %default:Master% | %cpu% | %memory% * %swap% | %enp4s0% - %wlp7s0% }{%kbd% <fc=#ee9a00>%date%</fc> | %uname% "
    }
