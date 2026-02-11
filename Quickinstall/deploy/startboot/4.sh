#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

aitermux_anim_print_frame() {
  local frame="${1-}"
  printf '%s' "$frame"
  printf '\033[0m\033[J'
}

# AITermux boot animation #4:
#   极致空间感：穿梭 AI 算力“重心矩阵”隧道（3D 线框 + 浮动矩阵块）→减速→抵达 AI termux。

DURATION="${DURATION:-2.4}"
HOLD="${HOLD:-0.9}"
FPS="${FPS:-12}"
SPEED="${SPEED:-1.0}"
COLOR="${COLOR:-1}"
ALTSCR="${ALTSCR:-1}"
WIDTH="${WIDTH:-}"
HEIGHT="${HEIGHT:-}"

usage() {
  cat <<'USAGE'
usage: 4.sh [--duration SEC] [--hold SEC] [--fps N] [--speed X] [--size WxH] [--no-color] [--no-altscr]

ENV:
  DURATION HOLD FPS SPEED COLOR ALTSCR WIDTH HEIGHT

TIPS:
  - 更快：     ./4.sh --speed 1.4 --duration 2.0
  - 末尾停留： ./4.sh --hold 1.2
  - 不停留：   ./4.sh --hold 0
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
function fract(x){ return x - int(x) }
function hash01(a,b,c){ return fract(sin(a*12.9898 + b*78.233 + c*37.719 + 0.1234)*43758.5453) }
function smoothstep(x){ return x<=0?0:(x>=1?1:(x*x*(3-2*x))) }
function fmodp(a,b){ return a - int(a/b)*b } # positive b

function ansi_color(ci,   r,g,b){
  # 1 dim bg, 2 grid dim, 3 grid mid, 4 grid bright, 5 matrix text, 6 star bright, 7 core, 9 logo
  if(ci==1){ r=85; g=95; b=120 }
  else if(ci==2){ r=95; g=120; b=210 }
  else if(ci==3){ r=120; g=180; b=255 }
  else if(ci==4){ r=200; g=240; b=255 }
  else if(ci==5){ r=120; g=255; b=170 }
  else if(ci==6){ r=255; g=230; b=140 }
  else if(ci==7){ r=255; g=160; b=90 }
  else { r=245; g=245; b=245 }
  return sprintf("\033[38;2;%d;%d;%dm", r,g,b)
}

function idx(x,y){ return y*W+x }
function clearbuf(   i){
  for(i=0;i<total;i++){ ch[i]=" "; col[i]=0 }
}
function put_raw(i, s, c){
  if(i<0||i>=total) return
  # 覆盖策略：更亮的颜色优先（避免远处覆盖近处）
  if(c < col[i] && col[i]!=0) return
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
          put_xy(ox + (colx-1)*scale + dx, oy + row*scale + dy, "#", c)
        }
      }
    }
  }
}
function draw_logo(scale, glow,   line1,line2,w1,w2,ox,oy,ci,chh,px,py){
  line1="AI"; line2="termux"
  w1=(gw*length(line1) + gap*(length(line1)-1))*scale
  w2=(gw*length(line2) + gap*(length(line2)-1))*scale
  ox=int((W-max(w1,w2))/2)
  oy=int((H-(gh*2*scale + 1))/2)

  if(glow>0){
    for(py=oy-2; py<=oy+gh*2*scale+2; py++){
      for(px=ox-3; px<=ox+max(w1,w2)+2; px++){
        if(hash01(px,py,int(glow*90)) < 0.018*glow) put_xy(px,py,".",1)
      }
    }
  }
  for(ci=1; ci<=length(line1); ci++){
    chh=substr(line1,ci,1)
    draw_glyph(chh, ox + int((ci-1)*(gw+gap)*scale), oy, scale, 9)
  }
  for(ci=1; ci<=length(line2); ci++){
    chh=substr(line2,ci,1)
    draw_glyph(chh, ox + int((ci-1)*(gw+gap)*scale), oy + gh*scale + 1, scale, 9)
  }
}

function rot_xyz(x,y,z, yaw,pitch,roll,   cy,sy,cp,sp,cr,sr,xx,yy,zz,tx,ty){
  cy=cos(yaw); sy=sin(yaw)
  cp=cos(pitch); sp=sin(pitch)
  cr=cos(roll); sr=sin(roll)
  # yaw (Y轴): (x,z)
  xx=x*cy + z*sy
  zz=-x*sy + z*cy
  yy=y
  # pitch (X轴): (y,z)
  ty=yy*cp - zz*sp
  zz=yy*sp + zz*cp
  yy=ty
  # roll (Z轴): (x,y)
  tx=xx*cr - yy*sr
  yy=xx*sr + yy*cr
  xx=tx
  rx=xx; ry=yy; rz=zz
}

function project(x,y,z,   rel,px,py){
  rel=z - camz
  if(rel <= near) { sx=-999; sy=-999; depth=0; return }
  px = cx + (x/rel)*ps
  py = cy + (y/rel)*ps*asp
  sx=px; sy=py
  depth=clamp(1.0 - (rel-near)/(far-near), 0, 1)
}

function draw_tunnel(scroll, yaw,pitch,roll, inten,   i,z,span,s,ax,ay,bx,by,cx1,cy1,dx1,dy1,zz,cv,chh,ins,j,step,tx0,ty0,tx1,ty1){
  span = slices*dz
  for(i=0;i<slices;i++){
    zz = near + fmodp(i*dz - scroll, span)
    s = baseS
    # 角点（一个正方形截面）
    rot_xyz(-s,-s,zz, yaw,pitch,roll); ax=rx; ay=ry; z1=rz
    rot_xyz( s,-s,zz, yaw,pitch,roll); bx=rx; by=ry; z2=rz
    rot_xyz( s, s,zz, yaw,pitch,roll); cx1=rx; cy1=ry; z3=rz
    rot_xyz(-s, s,zz, yaw,pitch,roll); dx1=rx; dy1=ry; z4=rz

    project(ax,ay,z1); axp=sx; ayp=sy; ad=depth
    project(bx,by,z2); bxp=sx; byp=sy; bd=depth
    project(cx1,cy1,z3); cxp=sx; cyp=sy; cd=depth
    project(dx1,dy1,z4); dxp=sx; dyp=sy; dd=depth

    cv = (inten<0.35?2:(inten<0.7?3:4))
    chh = (inten<0.55?"+":"#")
    # 截面线框
    dline(axp,ayp,bxp,byp,chh,cv)
    dline(bxp,byp,cxp,cyp,chh,cv)
    dline(cxp,cyp,dxp,dyp,chh,cv)
    dline(dxp,dyp,axp,ayp,chh,cv)

    # 内部“矩阵网格”十字
    if(i%2==0){
      dline((axp+cxp)/2,(ayp+cyp)/2,(bxp+dxp)/2,(byp+dyp)/2,"-",2)
      dline((axp+bxp)/2,(ayp+byp)/2,(dxp+cxp)/2,(dyp+cyp)/2,"|",2)
    }
  }
}

function draw_connectors(scroll, yaw,pitch,roll, inten,   i,span,zA,zB,s,ax,ay,bx,by,cx1,cy1,dx1,dy1,z1,z2,z3,z4,axp,ayp,bxp,byp,cxp,cyp,dxp,dyp,axp2,ayp2,bxp2,byp2,cxp2,cyp2,dxp2,dyp2,cv,chh){
  span = slices*dz
  s = baseS
  cv = (inten<0.55?2:3)
  chh = (inten<0.55?".":"/")
  for(i=0;i<slices-1;i++){
    zA = near + fmodp(i*dz - scroll, span)
    zB = near + fmodp((i+1)*dz - scroll, span)

    rot_xyz(-s,-s,zA, yaw,pitch,roll); project(rx,ry,rz); axp=sx; ayp=sy
    rot_xyz(-s,-s,zB, yaw,pitch,roll); project(rx,ry,rz); axp2=sx; ayp2=sy
    dline(axp,ayp,axp2,ayp2,chh,cv)

    rot_xyz( s,-s,zA, yaw,pitch,roll); project(rx,ry,rz); bxp=sx; byp=sy
    rot_xyz( s,-s,zB, yaw,pitch,roll); project(rx,ry,rz); bxp2=sx; byp2=sy
    dline(bxp,byp,bxp2,byp2,chh,cv)

    rot_xyz( s, s,zA, yaw,pitch,roll); project(rx,ry,rz); cxp=sx; cyp=sy
    rot_xyz( s, s,zB, yaw,pitch,roll); project(rx,ry,rz); cxp2=sx; cyp2=sy
    dline(cxp,cyp,cxp2,cyp2,chh,cv)

    rot_xyz(-s, s,zA, yaw,pitch,roll); project(rx,ry,rz); dxp=sx; dyp=sy
    rot_xyz(-s, s,zB, yaw,pitch,roll); project(rx,ry,rz); dxp2=sx; dyp2=sy
    dline(dxp,dyp,dxp2,dyp2,chh,cv)
  }
}

function draw_stars(m, yaw,pitch,roll, inten,   i,x,y,z,zz,c){
  for(i=0;i<nst;i++){
    # 星体沿 z 轴循环，制造“穿梭”感
    zz = stz0[i] - m*stRange*(0.65+0.65*inten)*SPEED
    zz = near + fmodp(zz-near, stRange)

    rot_xyz(stx0[i], sty0[i], zz, yaw*0.35, pitch*0.35, roll*0.15)
    project(rx,ry,rz)
    if(sx<-100) continue
    if(depth>0.75) c=6
    else if(depth>0.45) c=3
    else c=1
    put_xy(sx,sy,(depth>0.75?"*":"."),c)
  }
}

function draw_matrix_blocks(m, yaw,pitch,roll, inten,   i,zz,x,y,base,sc,ox,oy,row,colx,val,txt,cv){
  # 小型矩阵块：在 3D 空间里掠过，像“算力重心”切片
  for(i=0;i<nblk;i++){
    zz = blkz0[i] - m*blkRange*(0.55+0.85*inten)*SPEED
    zz = near + fmodp(zz-near, blkRange)
    rot_xyz(blkx0[i], blky0[i], zz, yaw, pitch, roll)
    project(rx,ry,rz)
    if(sx<-100) continue
    sc = 0.7 + 1.0*depth
    ox=int(sx); oy=int(sy)
    cv = (depth>0.55?5:2)
    if(depth<0.18) continue
    # 外框（3x3）
    put_xy(ox,oy,"[",cv); put_xy(ox+8,oy,"]",cv)
    for(row=1; row<=3; row++){
      txt=""
      for(colx=1; colx<=3; colx++){
        val = int((hash01(i,row,colx+int(m*3)))*90) - 45
        txt = txt sprintf("%3d", val)
      }
      put_str(ox, oy+row, "|" txt "|", cv)
    }
    put_xy(ox,oy+4,"[",cv); put_xy(ox+8,oy+4,"]",cv)
  }
}

function draw_core_overlay(u,   y0,x0,txt){
  # 终端式提示，强化“算力矩阵”叙事
  y0=int(H*0.12)
  x0=int(W*0.10)
  if(u<0.32){
    put_str(x0,y0,"[boot] attaching to AI centroid matrix...",1)
  } else if(u<0.58){
    put_str(x0,y0,"[boot] traversing compute core lattice  (Δt=1/fps)",1)
  } else if(u<0.78){
    put_str(x0,y0,"[boot] warp: prioritize reasoning/IO/memory lanes",1)
  } else {
    put_str(x0,y0,"[boot] arrived: focus locked on AITermux",1)
  }
}

BEGIN{
  pi=3.141592653589793
  srand(1) # 尽量确定性

  total=W*H
  fps=int(FPS+0); if(fps<8) fps=8; if(fps>30) fps=30
  dt=1.0/fps
  frames=int((DUR+0)*fps+0.5); if(frames<1) frames=1
  holdFrames=int((HOLD+0)*fps+0.5); if(holdFrames<0) holdFrames=0

  cx=(W-1)/2.0
  cy=(H-1)/2.0
  asp=0.58
  ps=min(W,H)*1.08

  near=0.55
  far=18.0
  camz=0

  # tunnel config
  slices=14
  dz=1.20
  baseS=2.15

  # stars
  nst=120
  stRange=far-near
  for(i=0;i<nst;i++){
    stx0[i]=(hash01(i,1,7)*2-1)*4.2
    sty0[i]=(hash01(i,2,9)*2-1)*2.6
    stz0[i]=near + hash01(i,3,11)*stRange
  }

  # matrix blocks
  nblk=16
  blkRange=far-near
  for(i=0;i<nblk;i++){
    blkx0[i]=(hash01(i,9,1)*2-1)*3.2
    blky0[i]=(hash01(i,8,2)*2-1)*2.0
    blkz0[i]=near + hash01(i,7,3)*blkRange
  }

  font_init()

  for(f=0; f<frames+holdFrames; f++){
    u = (frames<=1?1.0:(f/(frames-1.0)))
    t = u*(DUR+0)

    clearbuf()

    # 强度：进入 -> 加速 -> warp -> 减速
    inA  = smoothstep(clamp(u/0.20,0,1))
    warp = smoothstep(clamp((u-0.45)/0.18,0,1)) * (1.0 - smoothstep(clamp((u-0.80)/0.18,0,1)))
    outA = smoothstep(clamp((u-0.78)/0.22,0,1))
    inten = clamp(0.25 + 0.55*inA + 0.65*warp, 0, 1)

    # motion 受 SPEED 影响，但过快也会 clamp
    m = clamp(u * (0.85 + 0.85*SPEED), 0, 1)
    camz = m*6.0

    yaw   = 0.32*sin(u*2*pi) + 0.28*warp
    pitch = 0.18*sin(u*2*pi*0.7) + 0.18*warp
    roll  = 0.22*sin(u*2*pi*0.45) + 0.65*warp

    scroll = m*8.0*(0.65+0.95*inten)

    # 背景星体 + 隧道 + 连接器 + 浮动矩阵块
    draw_stars(m, yaw,pitch,roll, inten)
    draw_connectors(scroll, yaw,pitch,roll, inten)
    draw_tunnel(scroll, yaw,pitch,roll, inten)
    draw_matrix_blocks(m, yaw,pitch,roll, inten)
    draw_core_overlay(u)

    # 中央“重心核”脉冲
    if(u>0.50 && u<0.82){
      pulse = (sin((u-0.50)*2*pi*3.0)*0.5+0.5) * warp
      for(i=0;i<120;i++){
        ang = i*(2*pi/120.0)
        r = 1.3 + 0.6*pulse
        x = cx + cos(ang)*r*ps*0.06
        y = cy + sin(ang)*r*ps*0.03
        put_xy(x,y,"o",7)
      }
      put_xy(cx,cy,"@",7)
    }

    # 抵达：logo 淡入并稳定
    if(u>0.80){
      a = smoothstep(clamp((u-0.80)/0.20,0,1))
      scale = (W>=70 && H>=24)?2:1
      draw_logo(scale, a)
      put_str(int((W-24)/2), int(H*0.74), "~$ AI termux ready", 5)
    }

    # hold
    if(f>=frames){
      scale = (W>=70 && H>=24)?2:1
      draw_logo(scale, 0.6)
      put_str(int((W-24)/2), int(H*0.74), "~$ AI termux ready", 5)
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
