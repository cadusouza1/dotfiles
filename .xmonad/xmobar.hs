import Xmobar

config :: Config
config = defaultConfig {
        font = "xft:FiraCode Nerd Font:weight=regular:pixelsize=13",
        additionalFonts = ["xft:FontAwsome:pixelsize=12"],
        borderColor = "black",
        border = TopB,
        bgColor = "black",
        fgColor = "grey",
        alpha = 255,
        position = Top,
        textOffset = -1,
        iconOffset = -1,
        lowerOnStart = True,
        pickBroadest = False,
        persistent = False,
        hideOnStart = False,
        iconRoot = "/home/work/.xmonad/xmp", -- Default: "."
        allDesktops = True,
        overrideRedirect = True,
        commands = [
            Run $ Cpu [
                "-t",
                "<fn=0>\xf2db</fn> Cpu <total>%",
                "-L","3",
                "-H","80", 
                "--normal","#1bb21b",
                "--high","#ff0000"
            ] 10,
            Run $ Memory [
                "-t",
                "<fn=0>\xf233</fn> Mem <usedratio>%",
                "-L","3",
                "-H","80", 
                "--normal","#ff69b4",
                "--high","#ff0000"
            ] 10,
            Run $ Date "<fn=0>\xf133</fn> %a %_d %b %Y %H:%M" "date" 600,
            Run $ StdinReader
        ],
        sepChar = "%",
        alignSep = "}{",
        template = " <fc=#1bb21b> <fn=0>\xf121</fn> </fc> | %StdinReader% |} { <fc=#1bb21b>%cpu%</fc>| <fc=#ff69b4>%memory%</fc> | <fc=#ee9a00>%date%</fc>"
    }

main :: IO ()
main = xmobar config
