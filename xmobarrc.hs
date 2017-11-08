Config
    { font = "-*-Fixed-Bold-R-Normal-*-13-*-*-*-*-*-*-*"
    , borderColor = "black"
    , border = TopB
    , bgColor = "black"
    , fgColor = "grey"
    , position = TopW L 100
    , allDesktops = True
    , commands =
        [ Run DynNetwork
            [ "--template" , "<dev>: <tx>|<rx>"
            , "--Low" , "1000000"
            , "--High" , "2500000"
            , "--low" , "green"
            , "--normal" , "orange"
            , "--high" , "red"
            , "-S" , "True"
            ] 10
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
    , template = "%StdinReader% | %default:Master% | %battery% | %cpu% | %memory% | %dynnetwork% }{%kbd% <fc=#ee9a00>%date%</fc> | %uname% "
    }
