#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

SCRIPT_DIR="$(CDPATH='' cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
source "${SCRIPT_DIR}/_render.sh"

# AITermux boot animation #1:
#   Tank vs. "AI\ntermux" brick wall -> a few shots -> AI transform -> dash crush -> brick explosion -> logo.

DURATION="${DURATION:-1.9}"
HOLD="${HOLD:-0.7}"
FPS="${FPS:-30}"
SPEED="${SPEED:-1.0}"
COLOR="${COLOR:-1}"
ALTSCR="${ALTSCR:-1}"
WIDTH="${WIDTH:-}"
HEIGHT="${HEIGHT:-}"

usage() {
  cat <<'USAGE'
usage: 1.sh [--duration SEC] [--hold SEC] [--fps N] [--speed X] [--size WxH] [--no-color] [--no-altscr]

ENV:
  DURATION HOLD FPS SPEED COLOR ALTSCR WIDTH HEIGHT

TIPS:
  - faster:   ./1.sh --speed 1.4
  - shorter:  ./1.sh --duration 1.6
  - no hold:  ./1.sh --hold 0
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
if (( WIDTH < 34 )); then WIDTH=34; fi
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
function mix(a,b,t){ return a + (b-a)*t }
function smoothstep(x){ return x<=0?0:(x>=1?1:(x*x*(3-2*x))) }
function fract(x){ return x - int(x) }
function hash01(a,b,c){ return fract(sin(a*12.9898 + b*78.233 + c*37.719 + 0.1234)*43758.5453) }
function gset(ch,row,s){ g[ch,row]=s }

function font_init(){
  # compact 5x7 font (fits small Termux heights reliably)
  gw=5; gh=7; gap=1

  gset("A",0,".###.")
  gset("A",1,"#...#")
  gset("A",2,"#...#")
  gset("A",3,"#####")
  gset("A",4,"#...#")
  gset("A",5,"#...#")
  gset("A",6,"#...#")

  gset("I",0,"#####")
  gset("I",1,"..#..")
  gset("I",2,"..#..")
  gset("I",3,"..#..")
  gset("I",4,"..#..")
  gset("I",5,"..#..")
  gset("I",6,"#####")

  # termux (lowercase, but full-height blocks to avoid "missing bricks" feel)
  gset("t",0,"..#..")
  gset("t",1,"..#..")
  gset("t",2,"#####")
  gset("t",3,"..#..")
  gset("t",4,"..#..")
  gset("t",5,"..#..")
  gset("t",6,"..##.")

  gset("e",0,"#####")
  gset("e",1,"#....")
  gset("e",2,"####.")
  gset("e",3,"#....")
  gset("e",4,"#....")
  gset("e",5,"#....")
  gset("e",6,"#####")

  gset("r",0,"####.")
  gset("r",1,"#...#")
  gset("r",2,"#....")
  gset("r",3,"#....")
  gset("r",4,"#....")
  gset("r",5,"#....")
  gset("r",6,"#....")

  gset("m",0,"#...#")
  gset("m",1,"##.##")
  gset("m",2,"#.#.#")
  gset("m",3,"#...#")
  gset("m",4,"#...#")
  gset("m",5,"#...#")
  gset("m",6,"#...#")

  gset("u",0,"#...#")
  gset("u",1,"#...#")
  gset("u",2,"#...#")
  gset("u",3,"#...#")
  gset("u",4,"#...#")
  gset("u",5,"#...#")
  gset("u",6,".###.")

  gset("x",0,"#...#")
  gset("x",1,".#.#.")
  gset("x",2,"..#..")
  gset("x",3,".#.#.")
  gset("x",4,"#...#")
  gset("x",5,".#.#.")
  gset("x",6,"#...#")
}

function brick_add(ix,iy,typ,   idx){
  if(ix<0||ix>=W||iy<0||iy>=H) return
  idx=iy*W+ix
  b[idx]=1
  bt[idx]=typ
  bxlist[bnlist]=ix
  bylist[bnlist]=iy
  bnlist++
}

function add_text_bricks(line, typ, baseX, baseY, sg,   ci,ch,row,pat,col,dx,dy,x,y){
  for(ci=1; ci<=length(line); ci++){
    ch=substr(line,ci,1)
    for(row=0; row<gh; row++){
      pat=g[ch,row]
      if(pat=="") continue
      for(col=1; col<=gw; col++){
        if(substr(pat,col,1)!="#") continue
        for(dy=0; dy<sg; dy++){
          for(dx=0; dx<sg; dx++){
            x=baseX + ((ci-1)*(gw+gap) + (col-1))*sg + dx
            y=baseY + row*sg + dy
            brick_add(x,y,typ)
          }
        }
      }
    }
  }
}

function logo_add_point(ix,iy,   idx){
  if(ix<0||ix>=W||iy<0||iy>=H) return
  idx=iy*W+ix
  logo[idx]=1
}

function add_text_logo(line, baseX, baseY, sg,   ci,ch,row,pat,col,dx,dy,x,y){
  for(ci=1; ci<=length(line); ci++){
    ch=substr(line,ci,1)
    for(row=0; row<gh; row++){
      pat=g[ch,row]
      if(pat=="") continue
      for(col=1; col<=gw; col++){
        if(substr(pat,col,1)!="#") continue
        for(dy=0; dy<sg; dy++){
          for(dx=0; dx<sg; dx++){
            x=baseX + ((ci-1)*(gw+gap) + (col-1))*sg + dx
            y=baseY + row*sg + dy
            logo_add_point(x,y)
          }
        }
      }
    }
  }
}

function spawn(x,y,cnt,spd,kind,   k,ang,s){
  for(k=0;k<cnt;k++){
    if(pn>=MAXP) return
    ang=rand()*2*pi
    s=spd*(0.35+0.75*rand())
    px[pn]=x+0.5
    py[pn]=y+0.5
    pvx[pn]=cos(ang)*s
    pvy[pn]=sin(ang)*s
    pl[pn]=0.14+0.56*rand()
    pk[pn]=kind
    pn++
  }
}

function put_raw(idx,chv,cv){
  if(idx<0||idx>=total) return
  ch[idx]=chv
  col[idx]=cv
}

function put_xy(x,y,chv,cv,   idx){
  x += OX; y += OY
  if(x<0||x>=W||y<0||y>=H) return
  idx=y*W+x
  put_raw(idx,chv,cv)
}

function ansi_color(ci,   r,g,b){
  # 1 bricks(AI), 2 bricks(termux), 3 tank, 4 tank dark, 5 bullet, 6 sparks warm, 7 sparks cool, 8 logo, 9 stars dim
  if(ci==1){ r=60; g=220; b=255 }
  else if(ci==2){ r=85; g=255; b=125 }
  else if(ci==3){ r=220; g=220; b=220 }
  else if(ci==4){ r=60; g=60; b=60 }
  else if(ci==5){ r=255; g=230; b=120 }
  else if(ci==6){ r=255; g=140; b=60 }
  else if(ci==7){ r=140; g=210; b=255 }
  else if(ci==8){ r=245; g=245; b=245 }
  else { r=90; g=120; b=210 }
  return sprintf("\033[38;2;%d;%d;%dm", r,g,b)
}

function hline(x0,y,x1,chv,cv,   x,tmp){
  if(x0>x1){ tmp=x0; x0=x1; x1=tmp }
  for(x=x0; x<=x1; x++) put_xy(x,y,chv,cv)
}
function vline(x,y0,y1,chv,cv,   y,tmp){
  if(y0>y1){ tmp=y0; y0=y1; y1=tmp }
  for(y=y0; y<=y1; y++) put_xy(x,y,chv,cv)
}
function box(x0,y0,w,h, fillCh, edgeCh, cvFill, cvEdge,   x,y,x1,y1){
  if(w<2 || h<2) return
  x1=x0+w-1; y1=y0+h-1
  # fill
  for(y=y0+1; y<=y1-1; y++){
    for(x=x0+1; x<=x1-1; x++) put_xy(x,y,fillCh,cvFill)
  }
  # edges (thin box drawing)
  put_xy(x0,y0,"┌",cvEdge); put_xy(x1,y0,"┐",cvEdge)
  put_xy(x0,y1,"└",cvEdge); put_xy(x1,y1,"┘",cvEdge)
  hline(x0+1,y0,x1-1,"─",cvEdge)
  hline(x0+1,y1,x1-1,"─",cvEdge)
  vline(x0,y0+1,y1-1,"│",cvEdge)
  vline(x1,y0+1,y1-1,"│",cvEdge)
}

function tank_put(cx,by,dx,dy,chv,cv){
  put_xy(int(cx+0.5)+dx, int(by+0.5)+dy, chv, cv)
}

function draw_tank(cx,baseY,mode,u,muzzle,   by,cvH,cvT,cvG,ff,ai,thr,ang,dx,dy){
  # 动态“图标坦克”：用少量高识别符号构成，并用 f%N 做稳定动画（不使用 rand，避免闪烁）。
  by=int(baseY+0.5)
  cvH=(mode==1?7:3)   # hull
  cvT=(mode==1?7:4)   # tracks / dark
  cvG=7               # glow / AI
  ff=(f%4)

  # tracks (animated tread)
  tank_put(cx,by,-7,0,(ff<2?"▂":"▃"),cvT)
  tank_put(cx,by,-6,0,(ff<2?"▂":"▃"),cvT)
  tank_put(cx,by,-5,0,(ff<2?"▂":"▃"),cvT)
  tank_put(cx,by,-4,0,"▂",cvT)
  tank_put(cx,by,-3,0,"▂",cvT)
  tank_put(cx,by,-2,0,"▂",cvT)
  tank_put(cx,by,-1,0,"▂",cvT)
  tank_put(cx,by, 0,0,"▂",cvT)
  tank_put(cx,by, 1,0,"▂",cvT)
  tank_put(cx,by, 2,0,"▂",cvT)
  tank_put(cx,by, 3,0,"▂",cvT)
  tank_put(cx,by, 4,0,"▂",cvT)
  tank_put(cx,by, 5,0,(ff<2?"▃":"▂"),cvT)
  tank_put(cx,by, 6,0,(ff<2?"▃":"▂"),cvT)
  tank_put(cx,by, 7,0,(ff<2?"▃":"▂"),cvT)

  # wheels (swap glyph for motion illusion)
  tank_put(cx,by,-5,0,(ff%2==0?"o":"O"),cvT)
  tank_put(cx,by, 0,0,(ff%2==0?"o":"O"),cvT)
  tank_put(cx,by, 5,0,(ff%2==0?"o":"O"),cvT)

  # hull (blocky icon)
  tank_put(cx,by,-6,-1,"▛",cvH); tank_put(cx,by,-5,-1,"█",cvH); tank_put(cx,by,-4,-1,"█",cvH)
  tank_put(cx,by,-3,-1,"█",cvH); tank_put(cx,by,-2,-1,"█",cvH); tank_put(cx,by,-1,-1,"█",cvH)
  tank_put(cx,by, 0,-1,"█",cvH); tank_put(cx,by, 1,-1,"█",cvH); tank_put(cx,by, 2,-1,"█",cvH)
  tank_put(cx,by, 3,-1,"█",cvH); tank_put(cx,by, 4,-1,"█",cvH); tank_put(cx,by, 5,-1,"█",cvH)
  tank_put(cx,by, 6,-1,"▜",cvH)

  tank_put(cx,by,-5,-2,"▙",cvH); tank_put(cx,by,-4,-2,"█",cvH); tank_put(cx,by,-3,-2,"█",cvH)
  tank_put(cx,by,-2,-2,"█",cvH); tank_put(cx,by,-1,-2,"█",cvH); tank_put(cx,by, 0,-2,"█",cvH)
  tank_put(cx,by, 1,-2,"█",cvH); tank_put(cx,by, 2,-2,"█",cvH); tank_put(cx,by, 3,-2,"█",cvH)
  tank_put(cx,by, 4,-2,"█",cvH); tank_put(cx,by, 5,-2,"▟",cvH)

  # turret (slight bob for life)
  dy=(ff==1?1:0)
  tank_put(cx,by,-2,-4+dy,"▛",cvH); tank_put(cx,by,-1,-4+dy,"█",cvH); tank_put(cx,by,0,-4+dy,"█",cvH); tank_put(cx,by,1,-4+dy,"▜",cvH)
  tank_put(cx,by,-2,-3+dy,"▙",cvH); tank_put(cx,by,-1,-3+dy,"█",cvH); tank_put(cx,by,0,-3+dy,"█",cvH); tank_put(cx,by,1,-3+dy,"▟",cvH)

  # barrel (up). recoil by muzzle flash
  dx=(muzzle>0?1:0)
  tank_put(cx,by,0+dx,-6,"|",(mode==1?cvG:5))
  tank_put(cx,by,0+dx,-5,"│",(mode==1?cvG:cvT))
  tank_put(cx,by,0+dx,-4,"│",(mode==1?cvG:cvT))

  # muzzle flash
  if(muzzle>0){
    tank_put(cx,by,0+dx,-7,"*",(mode==1?cvG:5))
  }

  # AI overlay (stable threshold morph, no shimmer)
  ai=(mode==1)?u:0
  if(ai>0){
    thr=hash01(int(cx+0.5), by-4, 1); if(thr < ai) tank_put(cx,by,-1,-4+dy,"A",cvG)
    thr=hash01(int(cx+0.5), by-4, 2); if(thr < ai) tank_put(cx,by, 0,-4+dy,"I",cvG)
    # orbit glow dots (deterministic motion)
    for(dx=0; dx<3; dx++){
      ang=t*7.0 + dx*2.1
      tank_put(cx,by, int(cos(ang)*9.0+0.5), int(-2 + sin(ang)*2.0+0.5), ".", cvG)
    }
  }
}

BEGIN{
  pi=3.141592653589793
  srand()

  # drawing glyphs (unicode ok here; we do not substr/length on these)
  BR_IN="█"
  BR_EDGE_AI="▓"
  BR_EDGE_TX="▒"
  BR_IN_AI="█"
  BR_IN_TX="█"
  LOGO_IN="█"
  LOGO_EDGE="▓"
  LOGO_GLOW="."
  BUL="*"

  fps=int(FPS+0); if(fps<12) fps=12; if(fps>60) fps=60
  dt=1.0/fps
  frames=int((DUR+0)*fps+0.5); if(frames<1) frames=1
  holdFrames=int((HOLD+0)*fps+0.5); if(holdFrames<0) holdFrames=0

  total=W*H
  MAXP=9500
  pn=0

  font_init()
  # tank icon constants
  TANK_HW=8
  TANK_H=8
  GUN_TIP_OFF=8.0

  # timeline: shoot -> transform -> charge -> blast -> logo
  tBoost=clamp((DUR+0)*0.45, 0.58, 0.98)
  tCharge=tBoost + 0.22
  tBlast=tCharge + 0.18
  tLogo=tBlast + 0.18
  if(tLogo > (DUR+0) - 0.14){
    shf=tLogo - ((DUR+0) - 0.14)
    tBoost-=shf; tCharge-=shf; tBlast-=shf; tLogo-=shf
    if(tBoost<0.48){ tBoost=0.48; tCharge=tBoost+0.22; tBlast=tCharge+0.18; tLogo=tBlast+0.18 }
  }

  # brick wall: AI newline termux (top)
  bnlist=0
  line1="AI"
  line2="termux"
  w1=length(line1)*gw + (length(line1)-1)*gap
  w2=length(line2)*gw + (length(line2)-1)*gap
  wmax=(w1>w2)?w1:w2
  htot=gh*2 + 2

  sg=int((W-2)/wmax)
  sg2=int((H-1)/htot)
  if(sg2<sg) sg=sg2
  sg=clamp(sg,1,2)

  wallY=0
  x1=int((W - w1*sg)/2)
  y1=wallY
  x2=int((W - w2*sg)/2)
  y2=wallY + (gh+2)*sg

  add_text_bricks(line1, 1, x1, y1, sg)
  add_text_bricks(line2, 2, x2, y2, sg)

  # final logo (center) reveals after the smash (sticky reveal to avoid "incomplete" look)
  for(idx=0; idx<total; idx++){ logo[idx]=0; logoOn[idx]=0 }
  lsg=int((W-2)/wmax)
  lsg2=int((H-1)/htot)
  if(lsg2<lsg) lsg=lsg2
  lsg=clamp(lsg,1,2)
  lx1=int((W - w1*lsg)/2)
  ly1=int((H - (htot*lsg))/2)
  lx2=int((W - w2*lsg)/2)
  ly2=ly1 + (gh+2)*lsg
  add_text_logo(line1, lx1, ly1, lsg)
  add_text_logo(line2, lx2, ly2, lsg)

  # precompute logo edge + glow + stable reveal order (faster + crisper)
  for(idx=0; idx<total; idx++){ logoEdge[idx]=0; logoGlow[idx]=0; logoR[idx]=0; glowR[idx]=0 }
  for(idx=0; idx<total; idx++){
    if(logo[idx]!=1) continue
    logoR[idx]=rand()
    x=int(idx - int(idx/W)*W)
    y=int(idx/W)
    edge=0
    if(x==0 || logo[idx-1]!=1) edge=1
    else if(x==W-1 || logo[idx+1]!=1) edge=1
    else if(y==0 || logo[idx-W]!=1) edge=1
    else if(y==H-1 || logo[idx+W]!=1) edge=1
  if(edge){
      logoEdge[idx]=1
      # 8-neighborhood glow; keep sparse via dedupe
      for(dy=-1; dy<=1; dy++){
        for(dx=-1; dx<=1; dx++){
          if(dx==0 && dy==0) continue
          gx=x+dx; gy=y+dy
          if(gx<0||gx>=W||gy<0||gy>=H) continue
          gidx=gy*W+gx
          if(logo[gidx]==1) continue
          if(logoGlow[gidx]!=1){
            logoGlow[gidx]=1
            glowR[gidx]=rand()
          }
        }
      }
    }
  }

  # choose shot targets from existing bricks (unique-ish columns)
  nShots=6
  t0=0.16
  t1=tBoost-0.10
  if(t1 < t0 + 0.28) t1=t0 + 0.28
  step=(nShots>1)?((t1 - t0)/(nShots-1)):0
  for(i=0;i<nShots;i++){
    shotT[i]=t0 + step*i
    tries=0
    while(tries<70 && bnlist>0){
      j=int(rand()*bnlist)
      tx=bxlist[j]
      if(!(tx in usedX)){
        usedX[tx]=1
        shotX[i]=tx
        break
      }
      tries++
    }
    if(!(i in shotX)) shotX[i]=int(W/2)
  }

  # background stars (store as lists; stable)
  snum=0
  for(y=0;y<H;y++){
    for(x=0;x<W;x++){
      if(rand()<0.012){
        sxlist[snum]=x
        sylist[snum]=y
        snum++
      }
    }
  }

  # tank initial
  # place tank close to bottom; last row reserved for a subtle "ground" line
  tankBaseY=H-2.0
  tankx=W/2.0
  tanky=tankBaseY
  tankMode=0
  muzzle=0.0

  # bullets
  bcount=0
  nextShot=0

  blasted=0
  shake=0

  ramp=" .,:;irsXA253hMHGS#9B&@"
  rlen=length(ramp)

  for(f=0; f<frames+holdFrames; f++){
    t=(f<frames)?(f*dt):(DUR+0)
    stage = (t<tBoost)?0:((t<tCharge)?1:((t<tBlast)?2:3))

    # defaults per frame
    OX=0; OY=0
    uMorph= (t<tBoost)?0:((t<tCharge)?smoothstep(clamp((t - tBoost)/(tCharge - tBoost),0,1)):1)

    # stage 0: shoot
    if(stage==0){
      if(nextShot<nShots) tx=shotX[nextShot]; else tx=W/2
      tankx += (tx - tankx) * (0.26 + 0.10*sin(t*9)) * SPEED
      tankx=clamp(tankx, TANK_HW+2, W-TANK_HW-3)

      if(nextShot<nShots && t>=shotT[nextShot]){
        blx[bcount]=tankx
        bly[bcount]=tanky-GUN_TIP_OFF
        blvy[bcount]=-(38.0 + 10.0*rand()) * SPEED
        bcount++
        muzzle=0.09
        nextShot++
      }
    }

    # stage 1: AI transform
    if(stage==1){
      tankMode=1
      tankx += (W/2 - tankx) * 0.18
      # smooth hover (avoid "flicker-y" jitter)
      tanky = tankBaseY + 0.08*sin(t*16.0)
      # deterministic energy particles (no random flashing)
      if((f%2)==0) spawn(tankx, tanky-2, 1, 14.0*SPEED, 7)
      if((f%5)==0) spawn(tankx + 3*sin(t*11.0), tanky-1 - 2*cos(t*9.0), 1, 18.0*SPEED, 7)
    }

    # stage 2: dash charge
    if(stage==2){
      tankMode=1
      u=smoothstep(clamp((t - tCharge)/(tBlast - tCharge),0,1))
      # aim into the lower "termux" line so the smash feels physically correct
      tanky = mix(tankBaseY, y2 + (gh*sg)*0.65, u)
      tankx += (W/2 - tankx) * 0.18
      # deterministic trail during dash
      spawn(tankx, tanky+2, 1, 20.0*SPEED, 7)
      if((f%2)==0) spawn(tankx, tanky+1, 1, 26.0*SPEED, 7)
    }

    # blast trigger
    if(blasted==0 && t>=tBlast){
      blasted=1
      shake=7
      for(k in b){
        x=int(k - int(k/W)*W)
        y=int(k/W)
        typ=bt[k]
        delete bt[k]
        delete b[k]
        if(rand()<0.60) spawn(x,y, 2, 42.0*SPEED, (typ==1?7:6))
        else if(rand()<0.15) spawn(x,y, 1, 48.0*SPEED, 7)
      }
      for(i=0;i<180;i++) spawn(W/2, (y1+y2)/2, 1, 54.0*SPEED, 7)
      for(i=0;i<bcount;i++) bly[i]=-999
    }

    # update bullets
    for(i=0;i<bcount;i++){
      if(bly[i]<-50) continue
      bly[i] += blvy[i]*dt
      # deterministic tracer dust (avoid random blinking)
      if(((f+i)%3)==0) spawn(blx[i], bly[i], 1, 10.0*SPEED, 6)
      ix=int(blx[i]+0.5); iy=int(bly[i]+0.5)
      if(iy<0){ bly[i]=-999; continue }
      if(ix<0||ix>=W||iy<0||iy>=H) continue
      idx=iy*W+ix
      if((idx in b) && t<tBlast){
        typ=bt[idx]
        delete bt[idx]
        delete b[idx]
        bly[i]=-999
        spawn(ix,iy, 22, 28.0*SPEED, (typ==1?7:6))
      }
    }

    # update particles
    for(i=0;i<pn;i++){
      if(pl[i]<=0) continue
      px[i]+=pvx[i]*dt
      py[i]+=pvy[i]*dt
      pvx[i]*=(1 - 0.72*dt)
      pvy[i]*=(1 - 0.72*dt)
      pvy[i]+=(10.0*dt)
      pl[i]-=dt
    }

    # decay muzzle flash
    if(muzzle>0) muzzle-=dt

    # shake offsets
    if(shake>0){
      OX=int((rand()-0.5)*shake)
      OY=int((rand()-0.5)*shake*0.45)
      shake--
    }

    # clear frame buffers
    for(idx=0; idx<total; idx++){
      ch[idx]=" "
      col[idx]=0
    }

    # scanline / digital noise
    if(COLOR==1 && (f%6)==0){
      # deterministic sweep (no random flicker)
      yy=int(1 + ((f*3) % (H-2)))
      for(x=0;x<W;x++){
        put_xy(x,yy,".",9)
      }
    }

    # starfield
    for(i=0;i<snum;i++){
      put_xy(sxlist[i], sylist[i], ".", 9)
    }

    # ground line (avoid "bottom gap" feeling; stable pattern)
    for(x=0;x<W;x++){
      put_xy(x, H-1, ((x%11)==0?",":((x%5)==0?".":":")), 9)
    }

    # bricks
    for(k in b){
      x=int(k - int(k/W)*W)
      y=int(k/W)
      # edge-aware brick glyphs -> sharper "drawing" than a flat '#'
      edge=0
      if(x==0 || !((k-1) in b)) edge=1
      else if(x==W-1 || !((k+1) in b)) edge=1
      else if(y==0 || !((k-W) in b)) edge=1
      else if(y==H-1 || !((k+W) in b)) edge=1

      if(bt[k]==1){
        # subtle texture (stable, no flicker)
        tex=((x*13 + y*7) % 11)
        put_xy(x,y, (edge?BR_EDGE_AI:(tex==0?"▓":BR_IN_AI)), 1)
      } else {
        tex=((x*9 + y*5) % 13)
        put_xy(x,y, (edge?BR_EDGE_TX:(tex==0?"▒":BR_IN_TX)), 2)
      }
    }

    # particles
    for(i=0;i<pn;i++){
      if(pl[i]<=0) continue
      x=int(px[i]+0.0); y=int(py[i]+0.0)
      if(x<0||x>=W||y<0||y>=H) continue
      v=clamp(pl[i]/0.70,0,1)
      ridx=int(v*(rlen-1))+1
      put_xy(x,y, substr(ramp,ridx,1), (pk[i]==7?7:6))
    }

    # bullets
    for(i=0;i<bcount;i++){
      if(bly[i]<0) continue
      x=int(blx[i]+0.5)
      y=int(bly[i]+0.5)
      put_xy(x,y,BUL,5)
    }

    # final logo reveal (sticky, so it becomes complete)
    if(t>=tLogo){
      a=clamp((t - tLogo)/0.18,0,1)
      a=smoothstep(a)
      # glow first
      for(idx=0; idx<total; idx++){
        if(logoGlow[idx]!=1) continue
        if(a*1.10 >= glowR[idx] && col[idx]==0){
          put_raw(idx, LOGO_GLOW, 7)
        }
      }
      # core/edge with stable reveal + tiny 3D extrusion (bottom-right)
      for(idx=0; idx<total; idx++){
        if(logo[idx]!=1) continue
        if(a >= logoR[idx]){
          logoOn[idx]=1
          put_raw(idx, (logoEdge[idx]==1?LOGO_EDGE:LOGO_IN), 8)
        }
      }
    }

    # tank fade after blast
    tankFade=1.0
    if(t>=tBlast){
      tankFade = 1.0 - smoothstep(clamp((t - tBlast)/0.22,0,1))
      if(rand()<0.60) spawn(tankx, tanky-1, 2, 22.0*SPEED, 7)
    }

    if(tankFade>0.12){
      draw_tank(tankx, tanky, (tankMode==1?1:0), uMorph, (muzzle>0?1:0))
      # transform scanner beam
      if(t>=tBoost && t<tCharge){
        sx=int((tankx - TANK_HW) + uMorph*(2*TANK_HW))
        for(yy=int(tanky-TANK_H); yy<=int(tanky); yy++){
          put_xy(sx,yy,"|",7)
        }
      }
    }

    # hold phase: keep logo alive for a moment (tiny shimmer only)
    if(f>=frames && holdFrames>0){
      # deterministic twinkle (soft, not "flash")
      for(i=0;i<10;i++){
        x=int(lx1 + hash01(i,f,11)*(wmax*lsg))
        y=int(ly1 - 2 + hash01(f,i,13)*(htot*lsg + 4))
        put_xy(x,y, ".", 7)
      }

      msg="~$ AI termux ready"
      my = ly1 + htot*lsg + 1
      if(my < H){
        if(my > H-2) my = H-2
        mx = int((W - length(msg))/2)
        if(mx < 0) mx = 0
        for(j=1; j<=length(msg); j++){
          put_xy(mx+j-1, my, substr(msg,j,1), 7)
        }
      }
    }

    # paint (single buffered output to avoid frame tearing / mis-order)
    out="\033[H"
    for(y=0;y<H;y++){
      lastc=-1
      line=""
      for(x=0;x<W;x++){
        idx=y*W+x
        cv=col[idx]
        if(COLOR==1){
          if(cv!=lastc){
            if(cv==0) line=line"\033[0m"
            else line=line ansi_color(cv)
            lastc=cv
          }
        }
        line=line ch[idx]
      }
      if(COLOR==1) line=line"\033[0m"
      if(y < H-1) out = out line "\n"
      else out = out line
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
