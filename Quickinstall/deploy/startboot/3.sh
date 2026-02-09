#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

SCRIPT_DIR="$(CDPATH='' cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
source "${SCRIPT_DIR}/_render.sh"

# AITermux boot animation #3 (致敬终端 / roguelike):
#   Prompt typing -> classic "loading" -> ASCII roguelike dungeon (@ 收集 AITERMUX) -> logo.

DURATION="${DURATION:-2.2}"
HOLD="${HOLD:-0.8}"
FPS="${FPS:-30}"
SPEED="${SPEED:-1.0}"
COLOR="${COLOR:-1}"
ALTSCR="${ALTSCR:-1}"
WIDTH="${WIDTH:-}"
HEIGHT="${HEIGHT:-}"

usage() {
  cat <<'USAGE'
usage: 3.sh [--duration SEC] [--hold SEC] [--fps N] [--speed X] [--size WxH] [--no-color] [--no-altscr]

ENV:
  DURATION HOLD FPS SPEED COLOR ALTSCR WIDTH HEIGHT

TIPS:
  - faster:   ./3.sh --speed 1.4
  - shorter:  ./3.sh --duration 1.8
  - no hold:  ./3.sh --hold 0
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
if (( WIDTH < 38 )); then WIDTH=38; fi
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
dt="$(awk -v fps="${FPS}" 'BEGIN{fps=int(fps+0); if(fps<12) fps=12; if(fps>60) fps=60; printf "%.6f", 1.0/fps }')"
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

function ansi_color(ci,   r,g,b){
  # 1 prompt user/host, 2 prompt path, 3 cursor, 4 dim text, 5 dungeon wall, 6 dungeon floor, 7 item, 8 player, 9 logo
  if(ci==1){ r=80; g=255; b=140 }
  else if(ci==2){ r=120; g=170; b=255 }
  else if(ci==3){ r=240; g=240; b=240 }
  else if(ci==4){ r=120; g=120; b=120 }
  else if(ci==5){ r=170; g=170; b=170 }
  else if(ci==6){ r=90; g=90; b=90 }
  else if(ci==7){ r=255; g=220; b=120 }
  else if(ci==8){ r=245; g=245; b=245 }
  else { r=235; g=235; b=235 }
  return sprintf("\033[38;2;%d;%d;%dm", r,g,b)
}

function idx(x,y){ return y*W+x }
function clearbuf(   i){
  for(i=0;i<total;i++){ ch[i]=" "; col[i]=0 }
}
function put_raw(i, s, c){
  if(i<0||i>=total) return
  ch[i]=s
  col[i]=c
}
function put_xy(x,y,s,c){
  if(x<0||x>=W||y<0||y>=H) return
  put_raw(idx(x,y), s, c)
}
function put_str(x,y,s,c,   i){
  for(i=1;i<=length(s);i++) put_xy(x+i-1,y,substr(s,i,1),c)
}
function box(x0,y0,w,h,   x,y,x1,y1){
  x1=x0+w-1; y1=y0+h-1
  for(x=x0;x<=x1;x++){ put_xy(x,y0,"-",5); put_xy(x,y1,"-",5) }
  for(y=y0;y<=y1;y++){ put_xy(x0,y,"|",5); put_xy(x1,y,"|",5) }
  put_xy(x0,y0,"+",5); put_xy(x1,y0,"+",5); put_xy(x0,y1,"+",5); put_xy(x1,y1,"+",5)
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
          put_xy(ox + (colx-1)*scale + dx, oy + row*scale + dy, "#", c)
        }
      }
    }
  }
}
function draw_logo(scale,   line1,line2,w1,w2,ox,oy,ci,chh){
  line1="AI"; line2="termux"
  w1=(gw*length(line1) + gap*(length(line1)-1))*scale
  w2=(gw*length(line2) + gap*(length(line2)-1))*scale
  ox=int((W-max(w1,w2))/2)
  oy=int((H-(gh*2*scale + 1))/2)
  for(ci=1; ci<=length(line1); ci++){
    chh=substr(line1,ci,1)
    draw_glyph(chh, ox + int((ci-1)*(gw+gap)*scale), oy, scale, 9)
  }
  for(ci=1; ci<=length(line2); ci++){
    chh=substr(line2,ci,1)
    draw_glyph(chh, ox + int((ci-1)*(gw+gap)*scale), oy + gh*scale + 1, scale, 9)
  }
}

function make_dungeon(){
  dw=min(W-6, 66)
  dh=min(H-9, 18)
  if(dw<30) dw=30
  if(dh<10) dh=10
  dx0=int((W-dw)/2)
  dy0=int((H-dh)/2) + 1

  for(y=0;y<dh;y++){
    for(x=0;x<dw;x++){
      tile[x,y]="."
      # 少量石墙点缀，保持可读性
      if(x>1 && x<dw-2 && y>1 && y<dh-2){
        if(rand()<0.06) tile[x,y]="#"
      }
    }
  }
  # 走廊（保证主路径可走）
  for(x=2;x<dw-2;x++) tile[x,int(dh/2)]="."
  for(y=2;y<dh-2;y++) tile[int(dw/2),y]="."
  tile[2,dh-3]="."

  # items A I t e r m u x (按终端常用 ASCII/大小写)
  nitems=8
  itemch[1]="A"; itemch[2]="I"; itemch[3]="t"; itemch[4]="e"; itemch[5]="r"; itemch[6]="m"; itemch[7]="u"; itemch[8]="x"
  # 分布在四象限，尽量沿十字走廊可到达
  itemx[1]=int(dw/2); itemy[1]=2
  itemx[2]=dw-4;     itemy[2]=int(dh/2)-2
  itemx[3]=dw-6;     itemy[3]=dh-3
  itemx[4]=int(dw/2)+3; itemy[4]=dh-4
  itemx[5]=2;        itemy[5]=dh-4
  itemx[6]=4;        itemy[6]=int(dh/2)+2
  itemx[7]=int(dw/2)-3; itemy[7]=3
  itemx[8]=int(dw/2)+1; itemy[8]=int(dh/2)+1

  # player path waypoints: start -> each item
  nwp=nitems+1
  wpx[1]=2; wpy[1]=dh-3
  for(i=1;i<=nitems;i++){ wpx[i+1]=itemx[i]; wpy[i+1]=itemy[i] }
}

function waypoint_at(p,   seg,local,tlen,acc,k,dx,dy,dist){
  # p: 0..1 along polyline
  tlen=0
  for(k=1;k<nwp;k++){
    dx=wpx[k+1]-wpx[k]; dy=wpy[k+1]-wpy[k]
    dist=sqrt(dx*dx+dy*dy); if(dist<0.0001) dist=0.0001
    seglen[k]=dist
    tlen+=dist
  }
  acc=0
  for(k=1;k<nwp;k++){
    if(p*tlen <= acc+seglen[k] || k==nwp-1){
      local=(p*tlen-acc)/seglen[k]
      px=wpx[k] + (wpx[k+1]-wpx[k])*local
      py=wpy[k] + (wpy[k+1]-wpy[k])*local
      return
    }
    acc+=seglen[k]
  }
  px=wpx[nwp]; py=wpy[nwp]
}

function draw_prompt(t,   prompt,cmd,typed,cur,on,basey){
  basey=int(H*0.45)
  prompt="u0_a485@termux:"
  put_str(int(W*0.12), basey, prompt, 1)
  put_str(int(W*0.12)+length(prompt), basey, "~$ ", 2)
  cmd="./start.sh  # boot"
  typed=int(clamp(t/ptDur,0,1)*length(cmd))
  put_str(int(W*0.12)+length(prompt)+3, basey, substr(cmd,1,typed), 9)
  on=((int(t*5)%2)==0)
  if(on) put_xy(int(W*0.12)+length(prompt)+3+typed, basey, "_", 3)
}

function draw_loading(t,   i,basey,msg,barw,fill,x0){
  basey=int(H*0.40)
  put_str(int(W*0.12), basey-2, "Running boot script...", 4)
  for(i=0;i<5;i++){
    msg=sprintf("[*] loading module %d/%d", i+1, 5)
    if(t > i*(ldDur/5.0)) put_str(int(W*0.12), basey+i, msg, 4)
  }
  barw=min(44, W-int(W*0.24))
  if(barw<18) barw=18
  x0=int(W*0.12)
  put_str(x0, basey+6, "[", 4)
  put_str(x0+barw-1, basey+6, "]", 4)
  fill=int(clamp(t/ldDur,0,1)*(barw-2))
  for(i=0;i<barw-2;i++){
    put_xy(x0+1+i, basey+6, (i<fill?"#":"."), (i<fill?7:4))
  }
}

function draw_dungeon_scene(p,   x,y,ix,iy,inv,got,i,xg,yg,scale,bar){
  # header (roguelike style)
  put_str(dx0, dy0-2, "AITermux (terminal roguelike)", 4)
  inv="Inventory: "
  got=int(clamp(p,0,1)*nitems + 1e-9)
  for(i=1;i<=got;i++) inv=inv itemch[i]
  put_str(dx0, dy0-1, inv, 7)

  # frame
  box(dx0-1, dy0-1, dw+2, dh+2)

  # map tiles
  for(y=0;y<dh;y++){
    for(x=0;x<dw;x++){
      if(tile[x,y]=="#") put_xy(dx0+x, dy0+y, "#", 5)
      else put_xy(dx0+x, dy0+y, ".", 6)
    }
  }

  # items (未收集的显示在地图上)
  for(i=1;i<=nitems;i++){
    if(i<=got) continue
    put_xy(dx0+itemx[i], dy0+itemy[i], itemch[i], 7)
  }

  # player position
  waypoint_at(p)
  xg=int(px+0.5); yg=int(py+0.5)
  put_xy(dx0+xg, dy0+yg, "@", 8)

  # status line (致敬 Rogue 末行)
  bar=sprintf("Level:1  Gold:0  Hp:%d(%d)  Str:16(16)  Arm:4  Exp:1/0", 12+int(2*sin(p*6.28)), 12)
  put_str(dx0, dy0+dh+2, bar, 4)
}

BEGIN{
  srand()
  total=W*H
  fps=int(FPS+0); if(fps<12) fps=12; if(fps>60) fps=60
  dt=1.0/fps
  frames=int((DUR+0)*fps+0.5); if(frames<1) frames=1
  holdFrames=int((HOLD+0)*fps+0.5); if(holdFrames<0) holdFrames=0

  font_init()
  make_dungeon()

  ptDur=(DUR+0)*0.28
  ldDur=(DUR+0)*0.22
  dgDur=(DUR+0) - ptDur - ldDur
  if(dgDur < 0.4){ dgDur=0.4; ldDur=max(0.2,(DUR+0)-ptDur-dgDur) }

  for(f=0; f<frames+holdFrames; f++){
    t=f*dt
    clearbuf()

    if(t < (DUR+0)){
      if(t < ptDur){
        draw_prompt(t)
      } else if(t < ptDur + ldDur){
        draw_prompt(ptDur)
        draw_loading(t-ptDur)
      } else {
        p=(t-ptDur-ldDur)/dgDur
        p=clamp(p,0,1)
        # SPEED 影响移动节奏（不是总时长）
        p=clamp(p*SPEED,0,1)
        draw_dungeon_scene(p)

        # 末段淡入 logo（短促“开机标志”）
        if(p>0.88){
          scale=(W>=70 && H>=24)?2:1
          draw_logo(scale)
        }
      }
    } else {
      # hold: final logo
      scale=(W>=70 && H>=24)?2:1
      draw_logo(scale)
      put_str(int((W-20)/2), int(min(H-2, H*0.72)), "~$ AI termux ready", 1)
    }

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
