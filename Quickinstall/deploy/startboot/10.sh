#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

SCRIPT_DIR="$(CDPATH='' cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
source "${SCRIPT_DIR}/_render.sh"

# AITermux boot animation #10（致敬 CRT / VT100 终端）：
#   暖屏 -> 去磁闪白 -> 扫描线/荧光残影 -> 噪声对焦解码 -> AI\n'termux
#
# 设计目标：
# - 全屏：默认高度 = 终端行数（不再 lines-2）
# - 纯 ASCII 为主（避免宽字符错位）
# - 5 秒内完成（默认参数）

DURATION="${DURATION:-2.9}"
HOLD="${HOLD:-0.6}"
FPS="${FPS:-14}"
SPEED="${SPEED:-1.0}"
COLOR="${COLOR:-1}"
ALTSCR="${ALTSCR:-1}"
WIDTH="${WIDTH:-}"
HEIGHT="${HEIGHT:-}"

usage() {
  cat <<'USAGE'
usage: 10.sh [--duration SEC] [--hold SEC] [--fps N] [--speed X] [--size WxH] [--no-color] [--no-altscr]

ENV:
  DURATION HOLD FPS SPEED COLOR ALTSCR WIDTH HEIGHT

TIPS:
  - faster:   ./10.sh --duration 2.6 --hold 0.5 --fps 14
  - no color: ./10.sh --no-color
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
function hash01(a,b,c){ return fract(sin(a*12.9898 + b*78.233 + c*37.719 + 0.1234)*43758.5453) }

function ansi_color(ci,m,   r,g,b,tw){
  # 1 dim green, 2 green, 3 bright green, 4 phosphor white, 5 flash, 7 amber, 9 logo
  if(ci==1){ r=20;  g=100; b=45 }
  else if(ci==2){ r=45;  g=220; b=95 }
  else if(ci==3){ r=90;  g=255; b=145 }
  else if(ci==4){ r=235; g=255; b=245 }
  else if(ci==5){ r=200; g=240; b=255 }
  else if(ci==7){ r=255; g=210; b=120 }
  else if(ci==9){ r=240; g=255; b=245 }
  else { r=200; g=200; b=200 }

  tw = 0.90 + 0.10*sin(f*0.18 + ci*1.7) + 0.06*sin(f*0.47 + ci*0.9)
  tw = tw * (0.92 + 0.20*power)
  tw = clamp(tw, 0.60, 1.35)
  m = clamp(m, 0.40, 1.60)

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
function put_str_clip(x,y,s,c,maxx,   i,n){
  maxx=int(maxx)
  if(maxx < x) return
  n = maxx - x + 1
  if(n <= 0) return
  if(length(s) > n) s = substr(s, 1, n)
  for(i=1;i<=length(s);i++) put_xy(x+i-1,y,substr(s,i,1),c)
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

function compute_rows(scan,warp,   y,d,alt,gl){
  for(y=0;y<H;y++){
    alt = ((y%2)==0 ? 1.00 : 0.86)
    gl = 1.0 - 0.18*abs(y-scan)
    if(gl<0.55) gl=0.55
    rowMul[y] = alt * (0.72 + 0.55*gl) * (0.65 + 0.55*power)
    d = (y - cy) / max(1,(H-1.0))
    rowShift[y] = int( sin(y*0.33 + t*2.1)*warp + sin(t*1.05)*warp*0.55 + d*d*warp*0.9 )
  }
}

function bg_noise(p,scan,   x,y,r,n,edge,pts,k,chh,c){
  # sparse noise for speed (CRT snow)
  n = 0.30*(1.0 - smoothstep(clamp(p/0.60,0,1))) + 0.02
  pts = int(W*H*n)
  for(k=0;k<pts;k++){
    x = int(rand()*W)
    y = int(rand()*H)
    edge = min(min(x,W-1-x), min(y,H-1-y))
    if(edge < 1 && rand() < 0.75) continue
    r = rand()
    if(r < 0.33){ chh="."; c=1 }
    else if(r < 0.66){ chh=":"; c=1 }
    else { chh="*"; c=2 }
    put_xy(x,y,chh,c)
  }

  # sync roll line (subtle)
  if(p < 0.70){
    y = (scan + int(t*9)) % H
    for(x=0;x<W;x++){
      if((x+f)%8==0) put_xy(x,y,"-",1)
    }
  }
}

function degauss(p,   a,x,y,dx,dy,r,ring,i){
  a = 1.0 - abs(p-0.23)/0.07
  a = clamp(a,0,1)
  a = a*a
  if(a<=0) return
  for(y=0;y<H;y++){
    dy = (y - cy)
    for(x=0;x<W;x++){
      dx = (x - cx)
      r = sqrt(dx*dx + dy*dy)
      ring = int((r + t*40) % 7)
      if(ring==0 && ((x+y+f)%3)==0){
        i=idx(x,y)
        ch[i] = ( (x+y+f)%2==0 ? "=" : "-" )
        col[i] = 5
      } else if(ring==1 && ((x+y+f)%11)==0){
        i=idx(x,y)
        ch[i] = "."
        col[i] = 4
      }
    }
  }
}

function draw_logo_focus(p,scan,   a,x,y,i,gi,c){
  a = smoothstep(clamp((p-pFocus)/(pLock-pFocus),0,1))
  for(i=0;i<total;i++){
    if(logo[i]!=1) continue
    y=int(i/W); x=i - y*W

    if(a >= lr[i]){
      put_raw(i, "#", 9)
      if(((i+f)%7)==0) glow1(x,y,2)
      if(((i+f)%19)==0) put_empty_xy(x,y-1,".",7)
    } else {
      if(((x+y+f)%2)==0){
        gi = glyph_at(x,y)
        c = ((scan - y) > 0 ? 2 : 1)
        put_raw(i, gi, c)
      }
    }
  }

  # scan highlight near reveal frontier
  if(p >= pFocus){
    for(x=lminx-3; x<=lmaxx+3; x++){
      if((x+f)%3==0) put_xy(x,scan,"-",7)
    }
  }
}

function crt_mask(   x,y,dxn,dyn,r2,i){
  # rounded corners / vignette mask
  for(y=0;y<H;y++){
    dyn = (y - cy) / max(1.0, (H/2.0))
    for(x=0;x<W;x++){
      dxn = (x - cx) / max(1.0, (W/2.0))
      r2 = dxn*dxn + dyn*dyn
      i = idx(x,y)
      if(r2 > 1.06){
        ch[i]=" "
        col[i]=0
      } else if(r2 > 0.92){
        # edge falloff
        if(col[i] > 0 && ((x+y+f)%3)==0){
          ch[i]="."
          if(col[i] > 1) col[i]=1
        }
      }
    }
  }
}

function scan_beam(scan,   x){
  # bright beam + faint trailing line
  for(x=0;x<W;x++){
    if((x+f)%2==0) put_xy(x,scan,"=",5)
    else if((x+f)%7==0) put_xy(x,scan,"-",4)
  }
  if(scan+1 < H){
    for(x=0;x<W;x++){
      if((x+f)%5==0) put_empty_xy(x,scan+1,"-",1)
    }
  }
}

function warmup(p,   a,y,x){
  # power-on line: thin bright bar that blooms then fades
  a = 1.0 - abs(p-0.07)/0.06
  a = clamp(a,0,1)
  if(a<=0) return
  y = cy + int(sin(t*10)*1.0)
  for(x=0;x<W;x++){
    if(((x+f)%2)==0) put_xy(x,y,"=",5)
    else put_xy(x,y,"-",4)
  }
  if(a>0.35){
    for(x=0;x<W;x++){
      if(((x+f)%7)==0) put_empty_xy(x,y-1,".",4)
      if(((x+f)%9)==0) put_empty_xy(x,y+1,".",4)
    }
  }
}

function phosphor_afterglow(   i){
  # simple persistence buffer: bright pixels leave a fading tail
  for(i=0;i<total;i++){
    if(col[i] >= 3){
      ag[i]=3
      agc[i]=col[i]
    } else if(col[i] == 2){
      ag[i]=max(ag[i],2)
      agc[i]=max(agc[i],2)
    } else if(col[i] == 1){
      ag[i]=max(ag[i],1)
      agc[i]=max(agc[i],1)
    }
  }
  for(i=0;i<total;i++){
    if(col[i]!=0) continue
    if(ag[i] <= 0) continue
    if(ag[i]==3){ ch[i]=":"; col[i]=2 }
    else if(ag[i]==2){ ch[i]="."; col[i]=1 }
    else { ch[i]="."; col[i]=1 }
    ag[i] = ag[i]-1
  }
}

function render_frame(   out,y,x,sx,ii,cv,lastc,line,m){
  out="\033[H"
  for(y=0;y<H;y++){
    lastc=-1
    line=""
    m = rowMul[y]
    for(x=0;x<W;x++){
      sx = x - rowShift[y]
      if(sx<0 || sx>=W){ cv=0; cc=" " }
      else {
        ii=idx(sx,y)
        cv=col[ii]
        cc=ch[ii]
      }
      if(COLOR==1){
        if(cv!=lastc){
          if(cv==0) line=line"\033[0m"
          else line=line ansi_color(cv,m)
          lastc=cv
        }
      }
      line=line cc
    }
    if(COLOR==1) line=line"\033[0m"
    if(y < H-1) out=out line "\n"
    else out=out line
  }
  return out
}

BEGIN{
  srand(1)
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

  pWarm=0.10
  pFocus=0.48
  pLock=0.82

  for(i=0;i<total;i++){ ag[i]=0; agc[i]=0 }

  for(f=0; f<frames+holdFrames; f++){
    if(f < frames) u = (frames<=1?1.0:(f/(frames-1.0)))
    else u = 1.0
    p = clamp(u,0,1)
    t = u*(DUR+0) * (0.85 + 0.95*clamp(SPEED+0,0.2,2.4))

    power = smoothstep(clamp((p-pWarm)/(pLock-pWarm),0,1))
    warp = 2.6*(1.0-power) + 0.45
    if(p > 0.70) warp = 0.70*(1.0 - smoothstep(clamp((p-0.70)/0.30,0,1))) + 0.25

    scan = int(fract(t*0.62 + 0.13*sin(t*0.9))*H)
    compute_rows(scan, warp)

    clearbuf()
    bg_noise(p,scan)
    degauss(p)
    warmup(p)
    scan_beam(scan)

    # vt100-ish status line
    if(W>=64 && p < pLock){
      put_str_clip(2,0,"VT100  SYNC:" int(100*power) "%  |  PHOSPHOR=ON", 1, W-1)
    }

    # logo focusing / locking
    if(p >= pFocus){
      draw_logo_focus(p,scan)
      if(p > pLock){
        for(i=0;i<total;i++){
          if(logo[i]==1) continue
          if(col[i] < 1) continue
          if(((i+f)%4)!=0) { ch[i]=" "; col[i]=0 }
        }
        put_str_clip(int((W-20)/2), int(min(H-2, lmaxy+3)), "~$ AI termux ready", 3, W-1)
      }
    }

    phosphor_afterglow()
    crt_mask()

    # hold: final steady logo
    if(f >= frames){
      for(i=0;i<total;i++){
        if(logo[i]==1) put_raw(i,"#",9)
      }
      put_str_clip(int((W-20)/2), int(min(H-2, lmaxy+3)), "~$ AI termux ready", 3, W-1)
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
