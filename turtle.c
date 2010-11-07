/* -*- Mode: C; Package: (Turtle C) -*- */

#define ZETA_C_SUPPRESS_AUTO_FLOAT_TO_DOUBLE

lispval window;

struct turtlestate {
     float x, y;
     float hdg;
     enum penstate { up, down } pen;
     enum showstate { hide, show } show;
} turt = { 0.0, 0.0, 0, down };

int turtwidth = 5;
int turtlength = 15;
float pi = 3.14159265359;
float sin(), cos();

struct point { int x, y; } origin;

void init(), draw_turtle(), draw_line(), fd(), bk(), rt(), lt(), pu(), pd();

void
init()
{
     int org_x, org_y;

     if (!window)
     	  window = @tv:(make-window 'window :edges-from :mouse :activate-p t :expose-p t);
     turt.x = turt.y = turt.hdg = 0.0;
     turt.pen = down;
#lisp
     (gl:send |window| :clear-window)
     (gl:setq |org_x| (gl:/ (gl:send |window| :inside-width) 2))
     (gl:setq |org_y| (gl:/ (gl:send |window| :inside-height) 2))
#endlisp
     origin.x = org_x;
     origin.y = org_y;
     draw_turtle();
}

void
draw_turtle()
{
     float radhdg = turt.hdg * (pi / 180.f), coshdg = cos (radhdg),
          sinhdg = sin (radhdg);
	
     draw_line(turt.x + turtwidth * coshdg, turt.y - turtwidth * sinhdg,
	       turt.x + turtlength * sinhdg, turt.y + turtlength * coshdg);
     draw_line(turt.x - turtwidth * coshdg, turt.y + turtwidth * sinhdg,
	       turt.x + turtlength * sinhdg, turt.y + turtlength * coshdg);
     draw_line(turt.x + turtwidth * coshdg, turt.y - turtwidth * sinhdg,
	       turt.x - turtwidth * coshdg, turt.y + turtwidth * sinhdg);
}

void
draw_line(x1, y1, x2, y2)
float x1, y1, x2, y2;
{
     int x1i, y1i, x2i, y2i;
     x1i = x1 + origin.x; y1i = origin.y - y1;
     x2i = x2 + origin.x; y2i = origin.y - y2;
#lisp
     (global:send |window| :draw-line |x1i| |y1i| |x2i| |y2i| tv:alu-xor ())
#endlisp
}

void
fd(n)
     int n;
{
     float newx, newy;

     newx = turt.x + n * sin (turt.hdg * (pi / 180.f));
     newy = turt.y + n * cos (turt.hdg * (pi / 180.f));
     draw_turtle();
     if (turt.pen == down) draw_line(turt.x, turt.y, newx, newy);
     turt.x = newx;
     turt.y = newy;
     draw_turtle();
}

void
bk(n)
     int n;
{
     fd (-n);
}

void
rt(n)
     int n;		/* in degrees. */
{
     draw_turtle();
     turt.hdg += n;
     turt.hdg -= ((int)(turt.hdg / 360)) * 360;
     draw_turtle();
}

void
lt (n)
     int n;
{
     rt(-n);
}

void
pu()
{
     turt.pen = up;
}

void
pd()
{
     turt.pen = down;
}
