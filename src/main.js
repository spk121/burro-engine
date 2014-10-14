var r = 0, g = 0, b = 0;
var rotation = 0.0, expansion = 1.0;
var test_console = 1;

BgSetBmp16FromFile(0, "splash_bmp16.tga");
BgShow(0);
BgSetRotationCenter(0, 150, 150);
BgScroll(0, 150, 150);

function DoIdle(delta_t) {
    if (test_console == 1) {
        ecma48Test();
        test_console = 0;
    }
    backdropSetColor(r, g, b);
    r += 0.2;
    g += 0.3;
    b += 0.5;
    if (r > 255)
        g = 0;
    if (g > 255)
        g = 0;
    if (b > 255)
        b = 0;
    rotation += 0.001;
    expansion += 0.001;
    if (expansion > 1.5)
        expansion = 0.5;
    BgSetRotationExpansion(0, rotation, expansion);
}

function DoAfterDrawFrame(delta_t) {
}

"ready";

