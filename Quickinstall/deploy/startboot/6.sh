#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

SCRIPT_DIR="$(CDPATH='' cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
source "${SCRIPT_DIR}/_render.sh"

# AITermux boot animation #6（最优解·稳定开屏）：
#   霓虹极光背景（粉/紫/青渐变）+ 终端启动序列（打字/进度条） -> 扫描线生成 AI\n'termux -> hold.
#
# 设计目标：
# - 不依赖终端背景色（用前景字符填满画面，避免“黑底”问题）
# - 不用宽字符中文排版（避免错位）
# - 5 秒内完成（默认参数）

DURATION="${DURATION:-1.7}"
HOLD="${HOLD:-0.6}"
FPS="${FPS:-18}"
SPEED="${SPEED:-1.0}"
COLOR="${COLOR:-1}"
ALTSCR="${ALTSCR:-1}"
WIDTH="${WIDTH:-}"
HEIGHT="${HEIGHT:-}"

usage() {
  cat <<'USAGE'
usage: 6.sh [--duration SEC] [--hold SEC] [--fps N] [--speed X] [--size WxH] [--no-color] [--no-altscr]

ENV:
  DURATION HOLD FPS SPEED COLOR ALTSCR WIDTH HEIGHT

TIPS:
  - faster:   ./6.sh --speed 1.4 --duration 1.4
  - save cpu: ./6.sh --fps 14
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
if (( WIDTH < 40 )); then WIDTH=40; fi
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
function mix(a,b,t){ return a + (b-a)*t }
function smoothstep(x){ return x<=0?0:(x>=1?1:(x*x*(3-2*x))) }

function ansi_color(ci,   r,g,b){
  # 1 dim, 2 cyan, 3 pink, 4 purple, 5 white, 6 green, 7 gold, 9 logo
  if(ci==1){ r=90;  g=95;  b=130 }
  else if(ci==2){ r=80;  g=220; b=255 }
  else if(ci==3){ r=255; g=95;  b=185 }
  else if(ci==4){ r=200; g=120; b=255 }
  else if(ci==5){ r=245; g=245; b=245 }
  else if(ci==6){ r=95;  g=255; b=140 }
  else if(ci==7){ r=255; g=210; b=130 }
  else { r=255; g=120; b=200 }
  return sprintf("\033[38;2;%d;%d;%dm", r,g,b)
}

function idx(x,y){ return y*W+x }
function clearbuf(   i){
  for(i=0;i<total;i++){ ch[i]=" "; col[i]=0 }
}
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
function put_str(x,y,s,c,   i){
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

function draw_glyph(chh, ox, oy, scale, c,   row,colx,dx,dy,pat){
  for(row=0; row<gh; row++){
    pat=g[chh,row]
    if(pat=="") continue
    for(colx=1; colx<=gw; colx++){
      if(substr(pat,colx,1)!="#") continue
      for(dy=0; dy<scale; dy++){
        for(dx=0; dx<scale; dx++){
          put_xy(ox + (colx-1)*scale + dx, oy + row*scale + dy, "█", c)
        }
      }
    }
  }
}

function draw_logo(scale, scan, glow,   line1,line2,w1,w2,ox,oy,ci,chh,x,y){
  line1="AI"; line2="termux"
  w1=(gw*length(line1) + gap*(length(line1)-1))*scale
  w2=(gw*length(line2) + gap*(length(line2)-1))*scale
  ox=int((W-max(w1,w2))/2)
  oy=int((H-(gh*2*scale + 1))/2)

  # glow speckles
  if(glow>0){
    for(y=oy-2; y<=oy+gh*2*scale+3; y++){
      for(x=ox-3; x<=ox+max(w1,w2)+3; x++){
        if((x+y+int(glow*99))%11==0 && glow>0.35) put_xy(x,y,".",1)
      }
    }
  }

  # scanning reveal (only draw pixels above scan line)
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
            if(y <= scan) put_xy(x,y,"█",9)
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
            if(y <= scan) put_xy(x,y,"█",9)
          }
        }
      }
    }
  }
}

function bg_cell(x,y,t,   nx,ny,v,w,a,ci,chh){
  # 用前景字符填满，避免依赖背景色
  nx=(x-(W-1)/2.0)/W
  ny=(y-(H-1)/2.0)/H
  v = 0.0
  v += 0.60*sin(nx*6.0 + t*1.6)
  v += 0.55*sin(ny*4.4 - t*1.2)
  v += 0.45*sin((nx*3.2 + ny*2.5)*3.0 + t*1.1)
  v = (v+1.9)/3.8
  v = clamp(v,0,1)

  # char ramp (always visible; avoid "black background")
  if(v<0.12){ chh="."; a=0.12 }
  else if(v<0.22){ chh=":"; a=0.24 }
  else if(v<0.34){ chh="░"; a=0.38 }
  else if(v<0.48){ chh="▒"; a=0.52 }
  else if(v<0.62){ chh="▓"; a=0.68 }
  else if(v<0.76){ chh="█"; a=0.82 }
  else { chh="█"; a=0.95 }

  # color blend zones: cyan <-> pink <-> purple
  w = 0.5 + 0.5*sin((nx*2.0 - ny*1.2)*3.0 + t*0.9)
  if(w<0.33) ci=2
  else if(w<0.66) ci=3
  else ci=4

  # highlight sparkle (ASCII only for max compatibility)
  if(a>0.65 && ((x*13 + y*7 + int(t*20))%97==0)) { chh="*"; ci=7 }
  put_xy(x,y,chh,ci)
}

function draw_bg(t){
  for(y=0;y<H;y++){
    for(x=0;x<W;x++){
      bg_cell(x,y,t)
    }
  }
}

function draw_terminal(t, u){
  # simple boot lines
  x0=int(W*0.10)
  y0=int(H*0.18)

  cmd="$ aitermux --boot"
  typed=int(clamp(t/0.35,0,1)*length(cmd))
  put_str(x0,y0,substr(cmd,1,typed),5)
  if(int(t*6)%2==0) put_xy(x0+typed,y0,"_",5)

  if(u>0.28){
    put_str(x0,y0+2,"[OK] terminal",6)
    put_str(x0,y0+3,"[OK] tools",6)
    put_str(x0,y0+4,"[OK] ai",6)
  }

  if(u>0.46){
    barw=min(44, W-x0-6); if(barw<18) barw=18
    p=clamp((u-0.46)/0.26,0,1)
    put_str(x0,y0+6,"loading: [",5)
    for(i=0;i<barw;i++){
      put_xy(x0+10+i,y0+6,(i<int(p*barw)?"#":"."),(i<int(p*barw)?7:1))
    }
    put_str(x0+10+barw,y0+6,"]",5)
  }
}

BEGIN{
  pi=3.141592653589793
  srand(1)

  fps=int(FPS+0); if(fps<10) fps=10; if(fps>60) fps=60
  dt=1.0/fps
  frames=int((DUR+0)*fps+0.5); if(frames<1) frames=1
  holdFrames=int((HOLD+0)*fps+0.5); if(holdFrames<0) holdFrames=0

  total=W*H
  font_init()

  for(f=0; f<frames+holdFrames; f++){
    u = (frames<=1?1.0:(f/(frames-1.0)))
    t = u*(DUR+0) * (0.9 + 0.9*clamp(SPEED+0,0.2,2.4))
    clearbuf()

    draw_bg(t)
    draw_terminal(t,u)

    # logo scan-in
    if(u>0.58){
      a=smoothstep(clamp((u-0.58)/0.26,0,1))
      scan=int((H*0.10) + a*(H*0.85))
      scale=(W>=72 && H>=24)?2:1
      draw_logo(scale, scan, a)
      # scan line highlight
      for(x=0;x<W;x++){
        if((x+int(t*20))%7==0) put_xy(x,scan,"-",1)
      }
    }

    # hold: final logo + ready
    if(f>=frames){
      scale=(W>=72 && H>=24)?2:1
      draw_logo(scale, H, 1.0)
      put_str(int((W-20)/2), int(H*0.74), "~$ AI termux ready",6)
    }

    out="\033[H"
    for(y=0;y<H;y++){
      lastc=-1
      line=""
      for(x=0;x<W;x++){
        ii=idx(x,y)
        cv=col[ii]
        if(COLOR==1){
          if(cv!=lastc){
            if(cv==0) line=line"\033[0m"
            else line=line ansi_color(cv)
            lastc=cv
          }
        }
        line=line ch[ii]
      }
      if(COLOR==1) line=line"\033[0m"
      if(y < H-1) out=out line "\n"
      else out=out line
    }
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
