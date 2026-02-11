#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

aitermux_anim_print_frame() {
  local frame="${1-}"
  printf '%s' "$frame"
  printf '\033[0m\033[J'
}

# AITermux boot animation #9（致敬黑客帝国·现代霓虹版）：
#   多层代码雨（绿）+ 过载脉冲 + 霓虹故障闪断 + 汇聚解码 -> AI\n'termux
#
# 设计目标：
# - 全屏：默认高度 = 终端行数（不再 lines-2）
# - 纯 ASCII 为主（避免宽字符错位）
# - 特效“更猛”：多层雨 + 霓虹故障 + 汇聚成像

DURATION="${DURATION:-2.7}"
HOLD="${HOLD:-0.7}"
FPS="${FPS:-12}"
SPEED="${SPEED:-1.0}"
COLOR="${COLOR:-1}"
ALTSCR="${ALTSCR:-1}"
WIDTH="${WIDTH:-}"
HEIGHT="${HEIGHT:-}"

usage() {
  cat <<'USAGE'
usage: 9.sh [--duration SEC] [--hold SEC] [--fps N] [--speed X] [--size WxH] [--no-color] [--no-altscr]

ENV:
  DURATION HOLD FPS SPEED COLOR ALTSCR WIDTH HEIGHT

TIPS:
  - 更快： ./9.sh --duration 2.2 --hold 0.5 --fps 12
  - 黑白： ./9.sh --no-color
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --duration|-d) DURATION="$2"; shift 2;;
    --hold) HOLD="$2"; shift 2;;
    --fps|-f) FPS="$2"; shift 2;;
    --speed) SPEED="$2"; shift 2;;
    --size|-s)
      if [[ "${2:-}" =~ ^([0-9]+)x([0-9]+)$ ]]; then
        WIDTH="${BASH_REMATCH[1]}"; HEIGHT="${BASH_REMATCH[2]}"
      fi
      shift 2
      ;;
    --no-color) COLOR=0; shift;;
    --color) COLOR=1; shift;;
    --altscr) ALTSCR=1; shift;;
    --no-altscr) ALTSCR=0; shift;;
    --help|-h) usage; exit 0;;
    *) shift;;
  esac
done

cols_raw=$(tput cols 2>/dev/null || echo 80)
lines_raw=$(tput lines 2>/dev/null || echo 24)

# 避免在最后一列输出触发行包裹/滚屏（Termux 等环境下会导致花屏、行顺序错乱）
cols=$cols_raw
lines=$lines_raw
if (( cols > 2 )); then cols=$((cols-1)); fi

if [[ -z "${WIDTH}" ]]; then WIDTH=$cols; fi
if [[ -z "${HEIGHT}" ]]; then HEIGHT=$lines; fi
if (( WIDTH < 52 )); then WIDTH=52; fi
if (( HEIGHT < 16 )); then HEIGHT=16; fi
if (( WIDTH > cols )); then WIDTH=$cols; fi
if (( HEIGHT > lines )); then HEIGHT=$lines; fi

cleanup() {
  printf "\033[0m\033[?25h\033[?7h"
  if [[ "${ALTSCR}" -eq 1 ]]; then
    printf "\033[?1049l"
  fi
}
trap cleanup EXIT INT TERM

printf "\033[?25l"
if [[ "${ALTSCR}" -eq 1 ]]; then
  printf "\033[?1049h"
fi
printf "\033[?7l"
printf "\033[H\033[2J"

# 产帧/延时分离：awk 只负责画，bash 用 read -t 定时（有 tty 时更准更省电）
DELIM=$'\x1e'
dt="$(awk -v fps="${FPS}" 'BEGIN{fps=int(fps+0); if(fps<8) fps=8; if(fps>30) fps=30; printf "%.6f", 1.0/fps }')"
have_tty=0
if { exec 4</dev/tty; } 2>/dev/null; then
  have_tty=1
fi

exec 3< <(
awk \
  -v W="${WIDTH}" -v H="${HEIGHT}" \
  -v FPS="${FPS}" -v DUR="${DURATION}" -v HOLD="${HOLD}" -v SPEED="${SPEED}" \
  -v COLOR="${COLOR}" \
'function clamp(x,lo,hi){ return x<lo?lo:(x>hi?hi:x) }
function min(a,b){ return a<b?a:b }
function max(a,b){ return a>b?a:b }
function abs(x){ return x<0?-x:x }
function smoothstep(x){ return x<=0?0:(x>=1?1:(x*x*(3-2*x))) }
function fract(x){ return x - int(x) }
function hash01(a,b,c){ return fract(sin(a*12.9898 + b*78.233 + c*37.719 + 0.1234)*43758.5453) }

function ansi_color(ci,   r,g,b,tw){
  # 1 bg, 2 dim green, 3 green, 4 head, 5 cyan, 6 magenta, 7 lime, 8 amber, 9 logo
  if(ci==1){ r=6;   g=14;  b=10 }
  else if(ci==2){ r=28;  g=110; b=55 }
  else if(ci==3){ r=70;  g=255; b=135 }
  else if(ci==4){ r=230; g=255; b=245 }
  else if(ci==5){ r=70;  g=235; b=255 }
  else if(ci==6){ r=255; g=95;  b=220 }
  else if(ci==7){ r=180; g=255; b=120 }
  else if(ci==8){ r=255; g=200; b=90 }
  else if(ci==9){ r=235; g=255; b=240 }
  else { r=245; g=245; b=245 }

  # dynamic shimmer per frame (cheap but very effective)
  tw = 0.85 + 0.15*sin(f*0.17 + ci*1.3)
  if(ci==5 || ci==6 || ci==8) tw = 0.78 + 0.22*sin(f*0.22 + ci*2.1)
  if(ci==9) tw = 0.92 + 0.08*sin(f*0.12)
  tw = tw * (1.0 + 0.35*over)

  r=int(clamp(r*tw,0,255))
  g=int(clamp(g*tw,0,255))
  b=int(clamp(b*tw,0,255))
  return sprintf("\033[38;2;%d;%d;%dm", r,g,b)
}

function idx(x,y){ return y*W+x }
function clearbuf(   i){ for(i=0;i<total;i++){ ch[i]=" "; col[i]=0 } }
function put_raw(i,s,c){
  if(i<0||i>=total) return
  # priority = color index (bigger wins)
  if(c < col[i] && col[i]!=0) return
  ch[i]=s
  col[i]=c
}
function put_xy(x,y,s,c){
  x=int(x); y=int(y)
  if(x<0||x>=W||y<0||y>=H) return
  put_raw(idx(x,y),s,c)
}
function put_str(x,y,s,c,   i){
  for(i=1;i<=length(s);i++) put_xy(x+i-1,y,substr(s,i,1),c)
}
function put_str_clip(x,y,s,c,maxx,   i,n){
  maxx=int(maxx)
  if(maxx < x) return
  n = maxx - x + 1
  if(n <= 0) return
  if(length(s) > n) s = substr(s, 1, n)
  for(i=1;i<=length(s);i++) put_xy(x+i-1,y,substr(s,i,1),c)
}

function put_empty_xy(x,y,s,c,   i){
  x=int(x); y=int(y)
  if(x<0||x>=W||y<0||y>=H) return
  i=idx(x,y)
  if(col[i]!=0) return
  ch[i]=s
  col[i]=c
}

function glow1(x,y,c,   dx,dy,xx,yy,chh){
  for(dy=-1; dy<=1; dy++){
    for(dx=-1; dx<=1; dx++){
      if(dx==0 && dy==0) continue
      xx=x+dx; yy=y+dy
      chh = ((dx*dx + dy*dy)==2 ? "." : ":")
      put_empty_xy(xx,yy,chh,c)
    }
  }
}

function chroma_ghost(x,y,g){
  # chromatic aberration style ghost
  if(over < 0.25) return
  if(((x+y+f)%3)==0){
    put_xy(x+1,y,g,5)
    put_xy(x-1,y,g,6)
  }
}

function dline(x0,y0,x1,y1,s,c,   dx,dy,sx,sy,err,e2){
  x0=int(x0); y0=int(y0); x1=int(x1); y1=int(y1)
  dx=abs(x1-x0); sx=(x0<x1)?1:-1
  dy=-abs(y1-y0); sy=(y0<y1)?1:-1
  err=dx+dy
  while(1){
    put_xy(x0,y0,s,c)
    if(x0==x1 && y0==y1) break
    e2=2*err
    if(e2>=dy){ err+=dy; x0+=sx }
    if(e2<=dx){ err+=dx; y0+=sy }
  }
}

function font_init(){
  gw=5; gh=7; gap=1
  g["A",0]=".###."; g["A",1]="#...#"; g["A",2]="#...#"; g["A",3]="#####"; g["A",4]="#...#"; g["A",5]="#...#"; g["A",6]="#...#"
  g["I",0]="#####"; g["I",1]="..#.."; g["I",2]="..#.."; g["I",3]="..#.."; g["I",4]="..#.."; g["I",5]="..#.."; g["I",6]="#####"
  g["t",0]="..#.."; g["t",1]="..#.."; g["t",2]="#####"; g["t",3]="..#.."; g["t",4]="..#.."; g["t",5]="..#.."; g["t",6]="..##."
  g["e",0]="#####"; g["e",1]="#...."; g["e",2]="####."; g["e",3]="#...."; g["e",4]="#...."; g["e",5]="#...."; g["e",6]="#####"
  g["r",0]="####."; g["r",1]="#...#"; g["r",2]="#...."; g["r",3]="#...."; g["r",4]="#...."; g["r",5]="#...."; g["r",6]="#...."
  g["m",0]="#...#"; g["m",1]="##.##"; g["m",2]="#.#.#"; g["m",3]="#...#"; g["m",4]="#...#"; g["m",5]="#...#"; g["m",6]="#...#"
  g["u",0]="#...#"; g["u",1]="#...#"; g["u",2]="#...#"; g["u",3]="#...#"; g["u",4]="#...#"; g["u",5]="#...#"; g["u",6]=".###."
  g["x",0]="#...#"; g["x",1]=".#.#."; g["x",2]="..#.."; g["x",3]=".#.#."; g["x",4]="#...#"; g["x",5]=".#.#."; g["x",6]="#...#"
}

function logo_init(scale,   line1,line2,w1,w2,ox,oy,ci,chh,row,pat,colx,dx,dy,x,y,i){
  line1="AI"; line2="termux"
  w1=(gw*length(line1) + gap*(length(line1)-1))*scale
  w2=(gw*length(line2) + gap*(length(line2)-1))*scale
  ox=int((W-max(w1,w2))/2)
  oy=int((H-(gh*2*scale + 1))/2)
  lminx=W; lmaxx=0; lminy=H; lmaxy=0

  for(i=0;i<total;i++){ logo[i]=0; lr[i]=0 }

  for(ci=1; ci<=length(line1); ci++){
    chh=substr(line1,ci,1)
    for(row=0; row<gh; row++){
      pat=g[chh,row]
      for(colx=1; colx<=gw; colx++){
        if(substr(pat,colx,1)!="#") continue
        for(dy=0; dy<scale; dy++){
          for(dx=0; dx<scale; dx++){
            x = ox + ((ci-1)*(gw+gap) + (colx-1))*scale + dx
            y = oy + row*scale + dy
            if(x<0||x>=W||y<0||y>=H) continue
            i=idx(x,y)
            logo[i]=1
            lr[i]=rand()
            if(x<lminx) lminx=x
            if(x>lmaxx) lmaxx=x
            if(y<lminy) lminy=y
            if(y>lmaxy) lmaxy=y
          }
        }
      }
    }
  }
  for(ci=1; ci<=length(line2); ci++){
    chh=substr(line2,ci,1)
    for(row=0; row<gh; row++){
      pat=g[chh,row]
      for(colx=1; colx<=gw; colx++){
        if(substr(pat,colx,1)!="#") continue
        for(dy=0; dy<scale; dy++){
          for(dx=0; dx<scale; dx++){
            x = ox + ((ci-1)*(gw+gap) + (colx-1))*scale + dx
            y = oy + (gh*scale + 1) + row*scale + dy
            if(x<0||x>=W||y<0||y>=H) continue
            i=idx(x,y)
            logo[i]=1
            lr[i]=rand()
            if(x<lminx) lminx=x
            if(x>lmaxx) lmaxx=x
            if(y<lminy) lminy=y
            if(y>lmaxy) lmaxy=y
          }
        }
      }
    }
  }
}

function glyph_at(x,y,f,layer,   k,s,n){
  if(layer==2){ s=glyphs2; n=glen2 }
  else if(layer==1){ s=glyphs1; n=glen1 }
  else { s=glyphs0; n=glen0 }
  k = (x*37 + y*17 + f*11 + layer*23) % n
  return substr(s, k+1, 1)
}

function fill_bg(f,   x,y,v,i){
  # faint dust so it is not pure black
  for(y=0;y<H;y++){
    for(x=0;x<W;x++){
      v = (x*17 + y*31 + f*3) % 97
      if(v==0){ i=idx(x,y); ch[i]="."; col[i]=1 }
      else if(v==1){ i=idx(x,y); ch[i]=":"; col[i]=1 }
    }
  }
}

function draw_rain(p,t,f, fade,   x,layer,spd,len,head,yy,seg,c,dist,g,dx,boost,kk){
  for(x=0;x<W;x++){
    layer=cl[x]

    if(layer==2) kk=0.58
    else if(layer==1) kk=0.78
    else kk=1.0

    spd = cs[x] * kk
    boost = (layer==0?1.9:(layer==1?1.4:1.1))
    spd = spd * (1.0 + boost*over)

    len = int(cn[x] * (layer==2?0.75:(layer==1?0.92:1.05)) * (0.78 + 0.90*over))
    if(len<4) len=4
    head = (t*spd + cp[x]) % (H + len) - len

    if(layer==0) dx=0
    else if(layer==1) dx=int(sin(t*0.85 + x*0.16) * (1.4 + 1.2*over))
    else dx=int(sin(t*1.05 + x*0.22) * (2.0 + 1.6*over))

    # fade out near logo during decode
    if(fade < 1.0 && x>=lminx-3 && x<=lmaxx+3){
      if(((x+f)%11) > int(fade*10)) continue
    }

    for(seg=0; seg<len; seg++){
      yy=int(head - seg)
      if(yy<0 || yy>=H) continue
      dist = seg / len

      if(dist < 0.08) c=4
      else if(dist < 0.24) c=3
      else c=2

      # depth dims
      if(layer==2){
        if(c==4) c=3
        else if(c==3) c=2
      }

      # neon accents
      if(cg[x]==1 && dist < 0.50 && ((f+x)%5)==0) c=5
      if(cm[x]==1 && dist < 0.45 && ((f+x)%7)==0) c=6
      if(ca[x]==1 && dist < 0.22 && ((f+x)%6)==0 && over>0.25) c=8

      g = glyph_at(x,yy,f,layer)
      if(seg==0){
        if(((f+x)%13)==0) g="@"
        else if(((f+x)%9)==0) g="$"
        else if(((f+x)%4)==0) g="*"
        if(layer==0) glow1(x+dx,yy,3)
        else glow1(x+dx,yy,2)
        chroma_ghost(x+dx,yy,g)
      }
      put_xy(x+dx,yy,g,c)

      if(seg==1 && over>0.55 && ((f+x)%5)==0){
        put_empty_xy(x+dx+1,yy,".",8)
      }
    }
  }
}

function burst_lines(a,f,   i,tx,ty,c,ex,ey,chh,r,n,ang,rx,ry){
  # neon focus lines (towards the logo box)
  if(a <= 0) return
  for(i=0;i<18;i++){
    if((i%4)==0){
      ex = lminx-2 + int((i*11 + f*5) % (lmaxx-lminx+5))
      ey = ((i%3)==0 ? lminy-2 : lmaxy+2)
    } else {
      ex = ((i%2)==0 ? lminx-2 : lmaxx+2)
      ey = lminy-2 + int((i*7 + f*3) % (lmaxy-lminy+5))
    }
    c = ((i%5)==0 ? 8 : ((i%2)==0 ? 6 : 5))
    chh = ((i%3)==0 ? "." : "-")
    dline(cx,cy,ex,ey,chh,c)
  }

  # ring pulse
  if(a>0.35){
    r = int(min(W,H) * (0.18 + 0.22*a))
    n = 48
    for(i=0;i<n;i++){
      ang = i*6.28318/n
      rx = cx + int(cos(ang)*r)
      ry = cy + int(sin(ang)*r*0.55)
      chh = ((i%4)==0 ? "=" : "-")
      c = ((i%6)==0 ? 8 : 7)
      put_xy(rx,ry,chh,c)
    }
  }
}

function orbit_sparks(a,f,   i,ang,r,x,y,c){
  if(a <= 0) return
  r = min(W,H) * (0.10 + 0.18*a)
  for(i=0;i<10;i++){
    ang = f*0.19 + i*0.628
    x = cx + int(cos(ang)*r)
    y = cy + int(sin(ang)*r*0.55)
    c = (i%3==0 ? 8 : (i%2==0 ? 6 : 5))
    put_xy(x,y,(i%2==0?".":"+"),c)
  }
}

function glitch_scan(f,   y,x,seg,x0,w0,c,i,y0){
  # segmented glitch bars + short vertical slices
  w0 = 12 + (f%10)

  y = (f*3) % H
  for(seg=0; seg<6; seg++){
    x0 = (seg*23 + f*7) % W
    c = (seg%2==0 ? 5 : 6)
    for(x=x0; x<min(W,x0+w0); x++){
      if((x+f)%2==0) put_xy(x,y,"-",c)
      else put_xy(x,y,"=",c)
    }
  }

  y = (f*5+7) % H
  for(seg=0; seg<5; seg++){
    x0 = (seg*29 + f*9) % W
    c = (seg%3==0 ? 8 : 6)
    for(x=x0; x<min(W,x0+w0); x++){
      if((x+seg+f)%3==0) put_xy(x,y,"_",c)
    }
  }

  for(i=0;i<3;i++){
    x0 = (f*13 + i*37) % W
    y0 = (f*9 + i*11) % H
    c = (i==0 ? 5 : (i==1 ? 6 : 8))
    for(y=y0; y<min(H,y0+6); y++){
      if(((y+f)%2)==0) put_xy(x0,y,"|",c)
    }
  }
}

function draw_logo_decode(p,f,   a,scan,x,y,i,gi){
  a = smoothstep(clamp((p-pDecode)/0.20,0,1))
  scan = int(lminy + a*(lmaxy-lminy+1))

  # scanline highlight (neon)
  for(x=0;x<W;x++){
    if((x+f)%3==0) put_xy(x,scan,"=",8)
    else if((x+f)%7==0) put_xy(x,scan,"-",7)
  }

  for(i=0;i<total;i++){
    if(logo[i]!=1) continue
    y=int(i/W)
    if(y > scan) continue
    x=i - y*W
    if(a >= lr[i]){
      put_raw(i, "#", 9)
      if(((i+f)%9)==0) glow1(x,y,7)
    } else {
      if(((i+f)%3)==0){
        gi = glyph_at(x,y,f,0)
        put_raw(i, gi, 3)
      }
    }
  }

  # modern neon frame around logo box
  if(a>0.45){
    for(x=lminx-2; x<=lmaxx+2; x++){
      put_xy(x, lminy-2, ".", 7)
      put_xy(x, lmaxy+2, ".", 7)
    }
    for(y=lminy-2; y<=lmaxy+2; y++){
      put_xy(lminx-2, y, ".", 7)
      put_xy(lmaxx+2, y, ".", 7)
    }
    put_xy(lminx-2, lminy-2, "+", 8)
    put_xy(lmaxx+2, lminy-2, "+", 8)
    put_xy(lminx-2, lmaxy+2, "+", 8)
    put_xy(lmaxx+2, lmaxy+2, "+", 8)
  }
}

function render_frame(   out,y,x,i,cv,lastc,line){
  out="\033[H"
  for(y=0;y<H;y++){
    lastc=-1
    line=""
    for(x=0;x<W;x++){
      i=idx(x,y)
      cv=col[i]
      if(COLOR==1){
        if(cv!=lastc){
          if(cv==0) line=line"\033[0m"
          else line=line ansi_color(cv)
          lastc=cv
        }
      }
      line=line ch[i]
    }
    if(COLOR==1) line=line"\033[0m"
    if(y < H-1) out=out line "\n"
    else out=out line
  }
  return out
}

BEGIN{
  srand(1)
  fps=int(FPS+0); if(fps<8) fps=8; if(fps>30) fps=30
  frames=int((DUR+0)*fps+0.5); if(frames<1) frames=1
  holdFrames=int((HOLD+0)*fps+0.5); if(holdFrames<0) holdFrames=0
  total=W*H
  cx=int(W/2); cy=int(H/2)

  glyphs0="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$#@%&*+-=<>?/|;:[]{}()"
  glen0=length(glyphs0)
  glyphs1="0123456789abcdef"
  glen1=length(glyphs1)
  glyphs2="01"
  glen2=length(glyphs2)

  # column params
  for(x=0;x<W;x++){
    cp[x]=rand()*(H+50)
    cs[x]=8.0 + rand()*18.0
    cn[x]=6 + int(rand()*20)
    r = rand()
    cl[x]=(r<0.52?0:(r<0.84?1:2))       # 3 depth layers
    cg[x]=(rand()<0.065?1:0)            # cyan glitch col
    cm[x]=(rand()<0.045?1:0)            # magenta glitch col
    ca[x]=(rand()<0.030?1:0)            # amber sparks
  }

  font_init()
  scale = (W>=72 && H>=24)?2:1
  logo_init(scale)

  # stages
  pOver=0.52
  pDecode=0.72
  pLock=0.92

  for(f=0; f<frames+holdFrames; f++){
    if(f < frames) u = (frames<=1?1.0:(f/(frames-1.0)))
    else u = 1.0
    p = clamp(u,0,1)
    t = u*(DUR+0) * (0.9 + 0.9*clamp(SPEED+0,0.2,2.4))

    clearbuf()
    fill_bg(f)

    # overclock factor
    over = 0.0
    if(p > pOver) over = smoothstep(clamp((p-pOver)/(pDecode-pOver),0,1))

    # fade rain when decoding logo
    fade = 1.0
    if(p > pDecode) fade = 1.0 - smoothstep(clamp((p-pDecode)/(pLock-pDecode),0,1))

    draw_rain(p,t,f,fade)

    # stronger-than-matrix extras
    if(p > pOver && p < pLock){
      if((f%2)==0) glitch_scan(f)
      if((f%2)==0) burst_lines(over,f)
      if(p < pDecode || (f%3)==0) orbit_sparks(over,f)
    }

    # decode / lock logo (rain gradually disappears)
    if(p >= pDecode){
      draw_logo_decode(p,f)
      if(p > pLock){
        # lock: suppress leftover rain
        for(i=0;i<total;i++){
          if(logo[i]==1) continue
          if(col[i] < 2) continue
          if(((i+f)%5)!=0) { ch[i]=" "; col[i]=0 }
        }
        put_str_clip(int((W-20)/2), int(min(H-2, lmaxy+3)), "~$ AI termux ready", 7, W-1)
      }
    } else {
      # tiny HUD (subtle)
      if(W>=60){
        put_str_clip(2,0,"[matrix] stream online  |  entropy=" int(1000*p), 2, W-1)
      }
    }

    # hold: final frame
    if(f >= frames){
      # ensure logo fully visible
      for(i=0;i<total;i++){
        if(logo[i]==1) put_raw(i,"#",9)
      }
      put_str_clip(int((W-20)/2), int(min(H-2, lmaxy+3)), "~$ AI termux ready", 7, W-1)
    }

    out = render_frame()
    printf "%s%c", out, 30
    fflush()
  }
}
'
)

while IFS= read -r -u 3 -d "${DELIM}" frame; do
  aitermux_anim_print_frame "${frame}" "${HEIGHT}"
  if (( have_tty )); then
    IFS= read -rsn1 -t "${dt}" <&4 || true
  else
    sleep "${dt}"
  fi
done
exec 3<&-
if (( have_tty )); then
  exec 4<&-
fi
