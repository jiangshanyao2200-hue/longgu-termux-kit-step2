#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

SCRIPT_DIR="$(CDPATH='' cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
source "${SCRIPT_DIR}/_render.sh"

# AITermux boot animation #2 (重做版)：
#   AI 数据大爆炸（3D 能量粒子 + 冲击波 + 数据流）→ “涌现智能”（连接/聚合）→ 组装成 AI\n'termux。

DURATION="${DURATION:-2.2}"
HOLD="${HOLD:-0.9}"
FPS="${FPS:-30}"
SPEED="${SPEED:-1.0}"
COLOR="${COLOR:-1}"
ALTSCR="${ALTSCR:-1}"
WIDTH="${WIDTH:-}"
HEIGHT="${HEIGHT:-}"

usage() {
  cat <<'USAGE'
usage: 2.sh [--duration SEC] [--hold SEC] [--fps N] [--speed X] [--size WxH] [--no-color] [--no-altscr]

ENV:
  DURATION HOLD FPS SPEED COLOR ALTSCR WIDTH HEIGHT

TIPS:
  - 更快：     ./2.sh --speed 1.4 --duration 1.9
  - 末尾停留： ./2.sh --hold 1.2
  - 不停留：   ./2.sh --hold 0
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
function min(a,b){ return a<b?a:b }
function max(a,b){ return a>b?a:b }
function abs(x){ return x<0?-x:x }
function mix(a,b,t){ return a + (b-a)*t }
function smoothstep(x){ return x<=0?0:(x>=1?1:(x*x*(3-2*x))) }
function fract(x){ return x - int(x) }
function hash01(a,b,c){ return fract(sin(a*12.9898 + b*78.233 + c*37.719 + 0.1234)*43758.5453) }

function ansi_color(ci,   r,g,b){
  # 1 dim bg, 2 blue, 3 cyan, 4 magenta, 5 purple, 6 green, 7 warm, 8 white, 9 logo
  if(ci==1){ r=80; g=95; b=140 }
  else if(ci==2){ r=90; g=140; b=255 }
  else if(ci==3){ r=60; g=220; b=255 }
  else if(ci==4){ r=255; g=110; b=220 }
  else if(ci==5){ r=180; g=120; b=255 }
  else if(ci==6){ r=95; g=255; b=130 }
  else if(ci==7){ r=255; g=170; b=80 }
  else if(ci==8){ r=245; g=245; b=245 }
  else { r=245; g=245; b=245 }
  return sprintf("\033[38;2;%d;%d;%dm", r,g,b)
}

function idx(x,y){ return y*W+x }
function clearbuf(   i){
  for(i=0;i<total;i++){ ch[i]=" "; col[i]=0 }
}
function put_raw(i, s, c){
  if(i<0||i>=total) return
  # 亮度优先（避免远处覆盖近处）
  if(c < col[i] && col[i]!=0) return
  ch[i]=s
  col[i]=c
}
function put_xy(x,y,s,c){
  x=int(x); y=int(y)
  if(x<0||x>=W||y<0||y>=H) return
  put_raw(idx(x,y), s, c)
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
  gw=7; gh=9; gap=1
  g["A",0]="..###.."; g["A",1]=".#...#."; g["A",2]="#.....#"; g["A",3]="#.....#"; g["A",4]="#######"; g["A",5]="#.....#"; g["A",6]="#.....#"; g["A",7]="#.....#"; g["A",8]="......."
  g["I",0]="#######"; g["I",1]="...#..."; g["I",2]="...#..."; g["I",3]="...#..."; g["I",4]="...#..."; g["I",5]="...#..."; g["I",6]="...#..."; g["I",7]="#######"; g["I",8]="......."
  g["t",0]="...#..."; g["t",1]="...#..."; g["t",2]=".#####."; g["t",3]="...#..."; g["t",4]="...#..."; g["t",5]="...#..."; g["t",6]="...#..#"; g["t",7]="....###"; g["t",8]="......."
  g["e",0]=".#####."; g["e",1]="#......"; g["e",2]="######."; g["e",3]="#......"; g["e",4]="#......"; g["e",5]="#......"; g["e",6]=".#####."; g["e",7]="......."; g["e",8]="......."
  g["r",0]="######."; g["r",1]="#.....#"; g["r",2]="#......"; g["r",3]="#......"; g["r",4]="#......"; g["r",5]="#......"; g["r",6]="#......"; g["r",7]="......."; g["r",8]="......."
  g["m",0]="#.....#"; g["m",1]="##...##"; g["m",2]="#.#.#.#"; g["m",3]="#..#..#"; g["m",4]="#.....#"; g["m",5]="#.....#"; g["m",6]="#.....#"; g["m",7]="......."; g["m",8]="......."
  g["u",0]="#.....#"; g["u",1]="#.....#"; g["u",2]="#.....#"; g["u",3]="#.....#"; g["u",4]="#.....#"; g["u",5]="#.....#"; g["u",6]=".#####."; g["u",7]="......."; g["u",8]="......."
  g["x",0]="#.....#"; g["x",1]=".#...#."; g["x",2]="..#.#.."; g["x",3]="...#..."; g["x",4]="..#.#.."; g["x",5]=".#...#."; g["x",6]="#.....#"; g["x",7]="......."; g["x",8]="......."
}

function build_logo(scale,   line1,line2,w1,w2,ox,oy,ci,chh,row,pat,colx,dx,dy,x,y){
  line1="AI"; line2="termux"
  w1=(gw*length(line1) + gap*(length(line1)-1))*scale
  w2=(gw*length(line2) + gap*(length(line2)-1))*scale
  ox=int((W-max(w1,w2))/2)
  oy=int((H-(gh*2*scale + 1))/2)

  nT=0
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
            tx[nT]=x; ty[nT]=y
            nT++
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
            tx[nT]=x; ty[nT]=y
            nT++
          }
        }
      }
    }
  }
}

function project(x,y,z,   rel){
  rel = z - camz
  if(rel<=near){ sx=-999; sy=-999; depth=0; return }
  sx = cx + (x/rel)*ps
  sy = cy + (y/rel)*ps*asp
  depth = clamp(1.0 - (rel-near)/(far-near), 0, 1)
}

function draw_shockwave(p,   r,th,x,y,i){
  # 一个细环 + 一层扩散点，制造“爆炸波前”
  r = (0.8 + 9.5*p) * (0.55 + 0.55*SPEED)
  for(i=0;i<120;i++){
    th=i*(2*pi/120.0)
    x=cx + cos(th)*r*ps*0.06
    y=cy + sin(th)*r*ps*0.03
    put_xy(x,y,"*", (p<0.25?7:3))
  }
  for(i=0;i<180;i++){
    th=i*(2*pi/180.0)
    x=cx + cos(th)*(r*0.85)*ps*0.06
    y=cy + sin(th)*(r*0.85)*ps*0.03
    if(hash01(i,int(p*100),17) < 0.20) put_xy(x,y,".",2)
  }
}

function draw_implosion(p,   r,th,x,y,i){
  # 爆炸后的“回收波前”：半径收缩 + 细碎数据火花
  p=clamp(p,0,1)
  r = (9.2*(1-p) + 1.4) * (0.55 + 0.50*SPEED)
  for(i=0;i<110;i++){
    th=i*(2*pi/110.0) + p*1.2
    x=cx + cos(th)*r*ps*0.06
    y=cy + sin(th)*r*ps*0.03
    put_xy(x,y,(p<0.55?"+":"."),(p<0.55?3:2))
  }
  for(i=0;i<180;i++){
    th=i*(2*pi/180.0) + p*2.2
    x=cx + cos(th)*(r*0.78)*ps*0.06
    y=cy + sin(th)*(r*0.78)*ps*0.03
    if(hash01(i,33,int(p*180)) < (0.10 + 0.18*(1-p))) put_xy(x,y,".",(p<0.6?7:1))
  }
}

function draw_logo_reveal(tt,   i,x,y,cv,gl,scanY){
  tt=clamp(tt,0,1)
  if(tt<=0) return
  # 扫描线：让“绘制”更像被生成出来
  scanY = int((H*0.20) + tt*(H*0.65))
  for(i=0;i<W;i++){
    if(hash01(i,77,int(tt*120))<0.35) put_xy(i,scanY,"-",1)
  }

  for(i=0;i<nT;i++){
    if(hash01(i,51,7) > tt) continue
    x=tx[i]; y=ty[i]
    gl = 0.35 + 0.65*tt
    cv = (tt<0.35?3:(tt<0.70?5:9))
    put_xy(x,y,"#",cv)
    if(tt>0.55 && hash01(i,19,int(tt*60))<0.035){
      put_xy(x+(hash01(i,5,1)<0.5?-1:1), y, ".", 3)
    }
  }
}

function draw_particles(pBang, pReorg, pAsm,   i,ux,uy,uz,sp,phi,ct,st,rx,ry,rz,cv,chh,gl,trail,ax,ay,bx,by,tid,tt,tx0,ty0,rr,ang,rExp,rCore,step,j,ux2,uy2,uz2,spb,rx2,ry2,rz2,en2,rExp2,rCore2,ang2,curx,cury,curd){
  for(i=0;i<N;i++){
    # 方向（球面分布，确定性）
    phi = 2*pi*hash01(i,1,7)
    ct  = 2*hash01(i,2,9)-1
    st  = sqrt(max(0,1-ct*ct))
    ux = cos(phi)*st
    uy = sin(phi)*st
    uz = ct

    # 速度与“能量”
    sp = 0.55 + 1.25*hash01(i,3,11)
    en = 0.35 + 0.65*hash01(i,4,13)

    # 爆炸/重组/绘制：连续轨迹 + 渐进“锁定到 logo”（无缝）
    rr = smoothstep(clamp(pReorg,0,1))
    tt = smoothstep(clamp(pAsm,0,1))
    rExp = (0.6 + 11.0*min(1,pBang)) * sp
    rCore = (0.85 + 0.65*en)
    ang = phi + (i%97)*0.03 + t*(2.1 + 0.55*SPEED) + rr*(8.0 + 6.0*en) + tt*(3.5+3.0*en)
    r = mix(rExp, rCore, rr)
    rx = cos(ang)*(r*st) + 0.22*sin(t*1.7 + i*0.01)*(1-rr)
    ry = sin(ang)*(r*st) + 0.18*cos(t*1.3 + i*0.011)*(1-rr)
    # z：爆炸时更深，重组时收束到核心层
    rz = mix(1.7 + uz*rExp*0.75, 2.25 + 0.35*uz, rr) + 0.10*sin((i%61)*0.19 + t*4.0)*(1-rr)

    # 渐进锁定到 2D logo 目标：tt 越高越“被绘制”
    if(tt>0){
      tid = i % nT
      tx0 = (tx[tid]-cx)/ps * (far*0.85)
      ty0 = (ty[tid]-cy)/(ps*asp) * (far*0.85)
      rz2 = 2.10 + 0.55*hash01(i,9,5) - 0.95*tt
      rx = mix(rx, tx0, tt)
      ry = mix(ry, ty0, tt)
      rz = mix(rz, rz2, tt)
      rz += 0.12*sin((i%57)*0.22 + t*6.0) * (1-tt)
    }

    project(rx,ry,rz)
    if(sx<-100) continue
    curx=sx; cury=sy; curd=depth

    gl = clamp(0.20 + 0.90*depth,0,1)
    # 色彩：深度/能量/阶段混合
    if(gl>0.82) cv=8
    else if(en>0.78 && gl>0.55) cv=4
    else if(en>0.60) cv=5
    else if(en>0.42) cv=3
    else cv=2
    if((pReorg>0.55 || tt>0.35) && (i%7)==0) cv=6
    chh = (gl>0.78?"*":(gl>0.55?"+":"."))
    if(tt>0.70 && gl>0.70) chh="#"
    put_xy(sx,sy,chh,cv)

    # data trail：爆炸向外拉丝；重组向内收束
    if((pBang>0.18 || rr>0.15 || tt>0.12) && (i%5)==0){
      trail = int(1 + 3*gl*(pBang>0.5?pBang:0.5) + 2*rr + 2*tt)
      for(k=1;k<=trail;k++){
        if(rr<0.22 && tt<0.10){
          put_xy(sx - ux*k*0.8, sy - uy*k*0.4, ".", 1)
        } else {
          # 向屏幕中心“回收”
          put_xy(sx + (cx-sx)*k*0.06, sy + (cy-sy)*k*0.06, ".", 1)
        }
      }
    }

    # 爆炸后网络涌现：连线从“随机”变为“结构化步进”，减少闪烁并体现重组
    if((rr>0.08 || tt>0.15) && (i%13)==0){
      step = int((rr+tt)*5)
      j = (i*73 + 17 + step*101) % N
      if(j==i) j=(i+1)%N
      phi2 = 2*pi*hash01(j,1,7)
      ct2  = 2*hash01(j,2,9)-1
      st2  = sqrt(max(0,1-ct2*ct2))
      ux2 = cos(phi2)*st2
      uy2 = sin(phi2)*st2
      uz2 = ct2
      spb = 0.55 + 1.25*hash01(j,3,11)
      rExp2 = (0.6 + 11.0*min(1,pBang)) * spb
      en2 = 0.35 + 0.65*hash01(j,4,13)
      rCore2 = (0.85 + 0.65*en2)
      ang2 = phi2 + (j%97)*0.03 + t*(2.1 + 0.55*SPEED) + rr*(8.0 + 6.0*en)
      r2 = mix(rExp2, rCore2, rr)
      rx2 = cos(ang2)*(r2*st2)
      ry2 = sin(ang2)*(r2*st2)
      rz2 = mix(1.7 + uz2*rExp2*0.75, 2.25 + 0.35*uz2, rr)
      if(tt>0){
        tid2 = j % nT
        tx1 = (tx[tid2]-cx)/ps * (far*0.85)
        ty1 = (ty[tid2]-cy)/(ps*asp) * (far*0.85)
        rz3 = 2.10 + 0.55*hash01(j,9,5) - 0.95*tt
        rx2 = mix(rx2, tx1, tt)
        ry2 = mix(ry2, ty1, tt)
        rz2 = mix(rz2, rz3, tt)
      }
      ax=curx; ay=cury; ad=curd
      project(rx2,ry2,rz2); bx=sx; by=sy; bd=depth
      if(ax>-100 && bx>-100){
        cc = ((rr+tt)<0.55?2:3)
        dline(ax,ay,bx,by,".",cc)
      }
    }

    # 重组“牵引束”：少量粒子被拉向核心，强化“重组”叙事
    if((rr>0.18 || tt>0.12) && (rr+tt)<0.98 && (i%29)==0){
      dline(curx,cury,cx,cy,".", ((rr+tt)<0.55?2:3))
    }

    # 绘制阶段：把“当前位置”向“目标像素”拉一条短束（看起来像正在生成形状）
    if(tt>0.22 && (i%23)==0){
      tid = i % nT
      project((tx[tid]-cx)/ps*(far*0.85), (ty[tid]-cy)/(ps*asp)*(far*0.85), 2.10); bx=sx; by=sy
      if(bx>-100) dline(curx,cury,bx,by,".", (tt<0.65?3:5))
    }
  }
}

BEGIN{
  pi=3.141592653589793
  srand(1) # 画面尽量确定性，减少“闪烁随机”

  fps=int(FPS+0); if(fps<12) fps=12; if(fps>60) fps=60
  dt=1.0/fps
  frames=int((DUR+0)*fps+0.5); if(frames<1) frames=1
  holdFrames=int((HOLD+0)*fps+0.5); if(holdFrames<0) holdFrames=0

  total=W*H
  cx=(W-1)/2.0
  cy=(H-1)/2.0
  asp=0.58
  ps=min(W,H)*1.05
  near=0.55
  far=18.0
  camz=-0.2

  # 粒子数：根据尺寸自适应（避免大屏太稀/小屏太卡）
  N=int(clamp(W*H/2.4, 900, 2200))

  font_init()
  scale = (W>=72 && H>=24)?2:1
  build_logo(scale)

  tBangEnd=(DUR+0)*0.38
  tReorgEnd=(DUR+0)*0.82
  tAsmStart=(DUR+0)*0.72
  tAsmEnd=(DUR+0)*0.98

  for(f=0; f<frames+holdFrames; f++){
    t=f*dt
    clearbuf()

    # 背景：轻微“量子噪声”星点
    for(i=0;i<int(W*H/70);i++){
      x=int(hash01(i,1,int(t*9))*W)
      y=int(hash01(i,2,int(t*9))*H)
      if(hash01(i,3,int(t*12))<0.12) put_xy(x,y,".",1)
    }

    # 进度映射
    pBang = clamp(t/tBangEnd,0,1)
    pReorg = clamp((t-tBangEnd)/(tReorgEnd-tBangEnd),0,1)
    pAsm  = clamp((t-tAsmStart)/(tAsmEnd-tAsmStart),0,1)

    # camera 轻推近，增强立体（不乱晃）
    camz = -0.38 + 0.30*smoothstep(pReorg) - 0.12*smoothstep(pAsm)

    if(t < (DUR+0)){
      # 核心点：像“智能种子”
      core = 0.5 + 0.5*sin(t*9.0)
      put_xy(cx,cy,"@", (core>0.6?8:7))

      if(pBang<1){
        draw_shockwave(pBang)
        put_str(int(W*0.06), int(H*0.10), "AI data ignition -> big bang", 1)
      } else {
        # 重组波前在绘制期逐渐退场，避免突兀遮挡 logo
        if(pAsm < 0.55){
          draw_implosion(pReorg * (1.0 - 0.75*smoothstep(pAsm/0.55)))
        }
        put_str(int(W*0.06), int(H*0.10), (pAsm<0.35 ? "data storm -> reorganizing intelligence" : "reorg -> drawing AITermux"), 6)
      }

      draw_particles(pBang, pReorg, pAsm)
      draw_logo_reveal(pAsm)
    } else {
      # hold：稳定 logo + 微光噪点
      for(i=0;i<nT;i++){
        put_xy(tx[i],ty[i],"#",9)
        if(hash01(i,4,int(t*30))<0.018) put_xy(tx[i]+(hash01(i,5,1)<0.5?-1:1), ty[i], ".", 3)
      }
      put_str(int((W-20)/2), int(H*0.74), "~$ AI termux ready", 6)
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
