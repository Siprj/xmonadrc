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
        , Run BatteryP ["BAT1"]
            ["-t", "<watts> (<left>%)"
            , "-L", "10", "-H", "80", "-p", "3"
            , "--", "-O", "<fc=green>On</fc> - ", "-i", ""
            , "-L", "-15", "-H", "-5"
            , "-l", "red", "-m", "blue", "-h", "green"
            ] 600
        ]
    , sepChar = "%"
    , alignSep = "}{"
    , template = "%StdinReader% | %default:Master% | %battery% | %cpu% | %memory% | %enp0s25% - %wlp1s0% }{%kbd% <fc=#ee9a00>%date%</fc> | %uname% "
    }
