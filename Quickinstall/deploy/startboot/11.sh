#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

SCRIPT_DIR="$(CDPATH='' cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
source "${SCRIPT_DIR}/_render.sh"

# AITermux boot animation #11（致敬 TRON）：
#   霓虹网格世界穿梭 + 光轨/粒子 -> 汇聚解码 -> AI\n'termux
#
# 设计目标：
# - 全屏：默认高度 = 终端行数（不再 lines-2）
# - 纯 ASCII 为主（避免宽字符错位）
# - 5 秒内完成（默认参数）

DURATION="${DURATION:-3.0}"
HOLD="${HOLD:-0.6}"
FPS="${FPS:-14}"
SPEED="${SPEED:-1.0}"
COLOR="${COLOR:-1}"
ALTSCR="${ALTSCR:-1}"
WIDTH="${WIDTH:-}"
HEIGHT="${HEIGHT:-}"

usage() {
  cat <<'USAGE'
usage: 11.sh [--duration SEC] [--hold SEC] [--fps N] [--speed X] [--size WxH] [--no-color] [--no-altscr]

ENV:
  DURATION HOLD FPS SPEED COLOR ALTSCR WIDTH HEIGHT

TIPS:
  - faster:   ./11.sh --duration 2.8 --hold 0.5 --fps 14
  - no color: ./11.sh --no-color
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

DELIM=$'\x1e'
dt="$(awk -v fps="${FPS}" 'BEGIN{fps=int(fps+0); if(fps<10) fps=10; if(fps>60) fps=60; printf "%.6f", 1.0/fps }')"
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
function modf(a,m){ return a - int(a/m)*m }

function ansi_color(ci,m,   r,g,b,tw){
  # 1 dim bg, 2 grid dim, 3 grid, 4 white, 5 magenta, 6 lime, 7 amber, 9 logo
  if(ci==1){ r=8;   g=14;  b=22 }
  else if(ci==2){ r=40;  g=170; b=210 }
  else if(ci==3){ r=80;  g=235; b=255 }
  else if(ci==4){ r=235; g=255; b=255 }
  else if(ci==5){ r=255; g=95;  b=230 }
  else if(ci==6){ r=160; g=255; b=140 }
  else if(ci==7){ r=255; g=205; b=120 }
  else if(ci==9){ r=235; g=255; b=245 }
  else { r=245; g=245; b=245 }

  tw = 0.86 + 0.14*sin(f*0.16 + ci*1.5)
  tw = tw * (0.92 + 0.35*over)
  tw = clamp(tw, 0.55, 1.45)
  m = clamp(m, 0.45, 1.75)

  r=int(clamp(r*tw*m,0,255))
  g=int(clamp(g*tw*m,0,255))
  b=int(clamp(b*tw*m,0,255))
  return sprintf("\033[38;2;%d;%d;%dm", r,g,b)
}

function idx(x,y){ return y*W+x }
function clearbuf(   i){ for(i=0;i<total;i++){ ch[i]=" "; col[i]=0 } }
function put_raw(i,s,c){
  if(i<0||i>=total) return
  if(c < col[i] && col[i]!=0) return
  ch[i]=s
  col[i]=c
}
function put_xy(x,y,s,c){
  x=int(x); y=int(y)
  if(x<0||x>=W||y<0||y>=H) return
  put_raw(idx(x,y),s,c)
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
  # subtle chromatic ghost on very bright points
  if(over < 0.35) return
  if(((x+y+f)%4)==0){
    put_xy(x+1,y,g,5)
    put_xy(x-1,y,g,6)
  }
}
function put_str_clip(x,y,s,c,maxx,   i,n){
  maxx=int(maxx)
  if(maxx < x) return
  n = maxx - x + 1
  if(n <= 0) return
  if(length(s) > n) s = substr(s, 1, n)
  for(i=1;i<=length(s);i++) put_xy(x+i-1,y,substr(s,i,1),c)
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

function glyph_at(x,y,   k){
  k = (x*37 + y*17 + f*11) % glen
  return substr(glyphs, k+1, 1)
}

function bg_stars(p,   i,chh){
  for(i=0;i<starN;i++){
    chh = (fract(starP[i] + f*0.07) < 0.06 ? "*" : starCh[i])
    put_xy(starX[i], starY[i], chh, 1)
  }
}

function portal_ring(a,fade,   n,i,ang,r,x,y,c,chh){
  if(a <= 0) return
  r = min(W,H) * (0.10 + 0.22*a)
  n = 56
  for(i=0;i<n;i++){
    if(fade < 1.0 && ((i+f)%9) > int(fade*8)) continue
    ang = i*6.28318/n + t*(0.25 + 0.65*a)
    x = cx + int(cos(ang)*r)
    y = int(H*0.28) + int(sin(ang)*r*0.45)
    c = (i%12==0 ? 7 : (i%5==0 ? 6 : 5))
    chh = (i%7==0 ? "=" : (i%3==0 ? "-" : "."))
    put_xy(x,y,chh,c)
    if((i%9)==0) glow1(x,y,c)
  }
}

function spark_burst(a,fade,   n,i,x,y,c,chh){
  if(a <= 0) return
  n = int(20 + 80*a)
  for(i=0;i<n;i++){
    if(fade < 1.0 && ((i+f)%11) > int(fade*10)) continue
    x = cx + int((rand()-0.5)*(W*0.90))
    y = int(H*0.28) + int(rand()*(H*0.65))
    if(x<0||x>=W||y<0||y>=H) continue
    c = (i%9==0 ? 7 : (i%2==0 ? 5 : 6))
    chh = (i%5==0 ? "*" : (i%2==0 ? "." : "+"))
    put_xy(x,y,chh,c)
    if((i%8)==0) glow1(x,y,c)
  }
}

function neon_afterglow(   i){
  for(i=0;i<total;i++){
    if(col[i] >= 4) ag[i]=3
    else if(col[i] == 3) ag[i]=max(ag[i],2)
    else if(col[i] == 2) ag[i]=max(ag[i],1)
  }
  for(i=0;i<total;i++){
    if(col[i]!=0) continue
    if(ag[i] <= 0) continue
    if(ag[i]==3){ ch[i]=":"; col[i]=2 }
    else { ch[i]="."; col[i]=1 }
    ag[i]=ag[i]-1
  }
}

function vignette(   x,y,dxn,dyn,r2,i){
  for(y=0;y<H;y++){
    dyn = (y - cy) / max(1.0, (H/2.0))
    for(x=0;x<W;x++){
      dxn = (x - cx) / max(1.0, (W/2.0))
      r2 = dxn*dxn + dyn*dyn
      if(r2 > 1.08){
        i=idx(x,y)
        ch[i]=" "
        col[i]=0
      } else if(r2 > 0.95){
        i=idx(x,y)
        if(col[i] > 0 && ((x+y+f)%4)==0){
          ch[i]="."
          if(col[i] > 1) col[i]=1
        }
      }
    }
  }
}

function draw_grid(p,fade,   hy,k,zspan,gm,i,z,sc,y,hw,xl,xr,c,chh,x,cols,j,base,xb){
  hy = int(H*0.28)
  k = 7.0
  zspan = 30.0
  gm = t*(5.2 + 4.8*over)

  # horizontal lines
  for(i=0;i<18;i++){
    z = modf(i*1.9 + gm, zspan)
    sc = z/(z+k)
    y = hy + int((H-1-hy)*sc)
    if(y<0||y>=H) continue
    hw = int((W/2-2) * min(1.0, sc*1.12 + 0.04))
    xl = cx - hw
    xr = cx + hw
    if(xl<0) xl=0
    if(xr>W-1) xr=W-1
    c = (sc < 0.35 ? 2 : (sc < 0.75 ? 3 : 4))
    chh = ((i%3)==0 ? "=" : "-")
    for(x=xl; x<=xr; x++){
      if(fade < 1.0 && ((x+y+f)%11) > int(fade*10)) continue
      if(((x+i+f)%6)==0) put_xy(x,y,chh,c)
      else if(((x+i+f)%13)==0) put_xy(x,y,".",c)
    }
  }

  # vertical lines
  cols = 12
  for(j=0;j<=cols;j++){
    base = (j - cols/2.0) / (cols/2.0)
    xb = cx + int(base*(W/2-2)*1.08)
    c = (abs(base) > 0.7 ? 2 : 3)
    if(fade < 1.0 && ((j+f)%5) > int(fade*4)) continue
    dline(xb,H-1,cx,hy, "|", c)
    if(((j+f)%4)==0) glow1(xb,int((H+hy)/2),2)
  }

  # horizon pulse
  if(over > 0.15){
    for(x=0;x<W;x++){
      if(((x+f)%5)==0) put_xy(x,hy,"_",2)
    }
  }
}

function light_trails(p,fade,   hy,k,zspan,gm,tr,phase,headZ,s,z,sc,xx,yy,px,py,c,chh,ax){
  hy = int(H*0.28)
  k = 7.0
  zspan = 30.0
  gm = t*(5.2 + 4.8*over)

  for(tr=0; tr<3; tr++){
    phase = 1.7 + tr*3.1
    headZ = modf(gm*(1.10 + 0.08*tr) + tr*11.0, zspan)
    px=-999; py=-999
    for(s=0; s<16; s++){
      z = headZ - s*(1.45 + 0.10*tr)
      if(z < 0) break
      sc = z/(z+k)
      ax = sin(t*(0.60+0.06*tr) + phase + z*0.11)*0.90 + 0.22*sin(t*1.25 + phase*0.7 + z*0.23)
      xx = cx + int(ax*(W/2-3)*sc*1.12)
      yy = hy + int((H-1-hy)*sc)
      if(xx<0||xx>=W||yy<0||yy>=H) continue
      if(fade < 1.0 && ((xx+yy+f+tr*7)%11) > int(fade*10)) continue
      if(s==0){ c=4; chh="@" }
      else if(s<3){
        c=(tr==0?5:(tr==1?6:7))
        chh="+"
      }
      else if(s<8){ c=3; chh="." }
      else { c=2; chh="." }
      put_xy(xx,yy,chh,c)
      if(s<4) glow1(xx,yy,(tr==0?5:(tr==1?6:7)))
      if(s==0) chroma_ghost(xx,yy,chh)
      if(px!=-999) dline(px,py,xx,yy,".",c)
      px=xx; py=yy
    }
  }
}

function draw_logo_decode(p,fade,   a,scan,x,y,i,gi){
  a = smoothstep(clamp((p-pDecode)/0.20,0,1))
  scan = int(lminy + a*(lmaxy-lminy+1))

  for(x=0;x<W;x++){
    if((x+f)%3==0) put_xy(x,scan,"=",7)
    else if((x+f)%7==0) put_xy(x,scan,"-",3)
  }

  for(i=0;i<total;i++){
    if(logo[i]!=1) continue
    y=int(i/W)
    if(y > scan) continue
    x=i - y*W
    if(a >= lr[i]){
      put_raw(i, "#", 9)
      if(((i+f)%9)==0) glow1(x,y,3)
    } else {
      if(fade > 0.2 && ((i+f)%3)==0){
        gi = glyph_at(x,y)
        put_raw(i, gi, 3)
      }
    }
  }

  if(a>0.45){
    for(x=lminx-2; x<=lmaxx+2; x++){
      put_xy(x, lminy-2, ".", 3)
      put_xy(x, lmaxy+2, ".", 3)
    }
    for(y=lminy-2; y<=lmaxy+2; y++){
      put_xy(lminx-2, y, ".", 3)
      put_xy(lmaxx+2, y, ".", 3)
    }
    put_xy(lminx-2, lminy-2, "+", 7)
    put_xy(lmaxx+2, lminy-2, "+", 7)
    put_xy(lminx-2, lmaxy+2, "+", 7)
    put_xy(lmaxx+2, lmaxy+2, "+", 7)
  }
}

function render_frame(   out,y,x,i,cv,lastc,line,m){
  out="\033[H"
  for(y=0;y<H;y++){
    lastc=-1
    line=""
    m = 0.95 - 0.12*(y%2)
    for(x=0;x<W;x++){
      i=idx(x,y)
      cv=col[i]
      if(COLOR==1){
        if(cv!=lastc){
          if(cv==0) line=line"\033[0m"
          else line=line ansi_color(cv,m)
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
  srand(2)
  fps=int(FPS+0); if(fps<10) fps=10; if(fps>60) fps=60
  frames=int((DUR+0)*fps+0.5); if(frames<1) frames=1
  holdFrames=int((HOLD+0)*fps+0.5); if(holdFrames<0) holdFrames=0
  total=W*H
  cx=int(W/2); cy=int(H/2)

  glyphs="abcdefghijklmnopqrstuvwxyz0123456789$#@%&*+-=<>?/|;:[]{}()"
  glen=length(glyphs)

  font_init()
  scale = (W>=72 && H>=24)?2:1
  logo_init(scale)

  # starfield (pre-baked, cheaper than per-cell hashing)
  starN = int(W*H*0.004)
  if(starN < 12) starN=12
  if(starN > 80) starN=80
  for(i=0;i<starN;i++){
    starX[i]=int(rand()*W)
    starY[i]=int(rand()*int(H*0.45))
    starP[i]=rand()
    starCh[i]=(rand()<0.18?"*":".")
  }

  pDecode=0.70
  pLock=0.90

  for(i=0;i<total;i++) ag[i]=0

  for(f=0; f<frames+holdFrames; f++){
    if(f < frames) u = (frames<=1?1.0:(f/(frames-1.0)))
    else u = 1.0
    p = clamp(u,0,1)
    t = u*(DUR+0) * (0.85 + 0.95*clamp(SPEED+0,0.2,2.4))

    over = smoothstep(clamp((p-0.10)/0.55,0,1))

    fade = 1.0
    if(p > pDecode) fade = 1.0 - smoothstep(clamp((p-pDecode)/(pLock-pDecode),0,1))

    clearbuf()
    bg_stars(p)
    draw_grid(p,fade)
    light_trails(p,fade)
    portal_ring(over,fade)
    if(over > 0.25 && (f%2)==0) spark_burst(over,fade)

    if(W>=70 && p < pDecode){
      put_str_clip(2,0,"TRON GRID  |  VECTOR=" int(1000*over) "  |  RUN",2,W-1)
    }

    if(p >= pDecode){
      draw_logo_decode(p,fade)
      if(p > pLock){
        for(i=0;i<total;i++){
          if(logo[i]==1) continue
          if(col[i] < 2) continue
          if(((i+f)%5)!=0) { ch[i]=" "; col[i]=0 }
        }
        put_str_clip(int((W-20)/2), int(min(H-2, lmaxy+3)), "~$ AI termux ready", 6, W-1)
      }
    }

    neon_afterglow()
    vignette()

    if(f >= frames){
      for(i=0;i<total;i++){
        if(logo[i]==1) put_raw(i,"#",9)
      }
      put_str_clip(int((W-20)/2), int(min(H-2, lmaxy+3)), "~$ AI termux ready", 6, W-1)
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
