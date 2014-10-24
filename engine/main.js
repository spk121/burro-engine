var r = 0, g = 0, b = 0;
var rotation = 0.0, expansion = 1.0;
var x = 0, y = 0;
var test_console = 1;

// BgSetBmp16FromFile(0, "splash_bmp16.tga");
// BgShow(0);
// BgSetRotationCenter(0, 150, 150);
// BgScroll(0, 150, 150);

TmxSetMapFromResource("map1.tmx");
bgUpdateFromTmxMap();
bgReset();
bgSetRotationCenter(0,200,200)
bgSetRotationCenter(1,200,200)
bgSetRotationCenter(2,200,200)
bgSetRotationCenter(3,200,200)
bgSetScroll(0,200,200)
bgSetScroll(1,200,200)
bgSetScroll(2,200,200)
bgSetScroll(3,200,200)

objUpdateSpritesheet("sprites.png");
objInit(1, 0, 0, 32, 32, 16, 16, false, false, 0)
objSet(1, 1, 10, 10, 0, 1, 0);
objShow(1)

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
}

function doEvent(event, start, tick)
{
    if (event === 0) {
        y -= 10;
    }
    else if (event == 1) {
        y += 10;
    }
    else if (event == 2) {
        x -= 10;
    }
    else if (event == 3) {
        x += 10;
    }
    objSetPosition(1,x,y);
}

function DoAfterDrawFrame(delta_t) {
    rotation += 0.01;
    expansion += 0.001;
    if (expansion > 1.0)
        expansion = 0.5;
    // bgSetRotationExpansion(0, rotation, expansion);
    // bgSetRotationExpansion(1, rotation, expansion);
    //bgSetRotation(0,rotation)
    //bgSetRotation(1,-rotation)
   // bgSetRotation(2,rotation)
    // bgSetRotation(3,-rotation)
    objSetRotationExpansion(1, rotation, expansion);
}

"ready";

