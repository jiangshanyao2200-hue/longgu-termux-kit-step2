#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

SCRIPT_DIR="$(CDPATH='' cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
source "${SCRIPT_DIR}/_render.sh"

# AITermux boot animation #5（炫技但有主题：AI 智能涌现）：
#   token 噪声 -> 局部规则自组织（反应扩散）-> 连接/注意力聚焦 -> 无缝“生成”AI\n'termux。
#   纯 bash + awk（兼容 Termux 的 awk：不依赖 gawk 扩展）。

DURATION="${DURATION:-1.9}"
HOLD="${HOLD:-0.4}"
FPS="${FPS:-14}"
SPEED="${SPEED:-1.0}"
COLOR="${COLOR:-1}"
ALTSCR="${ALTSCR:-1}"
WIDTH="${WIDTH:-}"
HEIGHT="${HEIGHT:-}"

usage() {
  cat <<'USAGE'
usage: 5.sh [--duration SEC] [--hold SEC] [--fps N] [--speed X] [--size WxH] [--no-color] [--no-altscr]

ENV:
  DURATION HOLD FPS SPEED COLOR ALTSCR WIDTH HEIGHT

TIPS:
  - 更快：     ./5.sh --speed 1.4 --duration 1.6
  - 省电：     ./5.sh --fps 18
  - 黑白：     ./5.sh --no-color
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
  # 1 dim, 2 blue, 3 cyan, 4 magenta, 5 purple, 6 green, 7 warm, 8 white, 9 logo
  if(ci==1){ r=85; g=95; b=120 }
  else if(ci==2){ r=90; g=140; b=255 }
  else if(ci==3){ r=60; g=220; b=255 }
  else if(ci==4){ r=255; g=110; b=220 }
  else if(ci==5){ r=180; g=120; b=255 }
  else if(ci==6){ r=95; g=255; b=130 }
  else if(ci==7){ r=255; g=170; b=90 }
  else if(ci==8){ r=245; g=245; b=245 }
  else { r=245; g=245; b=245 }
  return sprintf("\033[38;2;%d;%d;%dm", r,g,b)
}

function idx(x,y){ return y*W+x }
function cidx(x,y){ return y*CW+x }

function clearbuf(   i){
  for(i=0;i<total;i++){ ch[i]=" "; col[i]=0 }
}
function put_raw(i,s,c){
  if(i<0||i>=total) return
  # 亮度优先
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
function build_logo(scale,   line1,line2,w1,w2,ox,oy,ci,chh,row,pat,colx,dx,dy,x,y){
  line1="AI"; line2="termux"
  w1=(gw*length(line1) + gap*(length(line1)-1))*scale
  w2=(gw*length(line2) + gap*(length(line2)-1))*scale
  ox=int((W-max(w1,w2))/2)
  oy=int((H-(gh*2*scale + 1))/2)
  nT=0
  minLX=W; minLY=H; maxLX=0; maxLY=0
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
            tgt[idx(x,y)]=1
            if(x<minLX) minLX=x
            if(y<minLY) minLY=y
            if(x>maxLX) maxLX=x
            if(y>maxLY) maxLY=y
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
            tgt[idx(x,y)]=1
            if(x<minLX) minLX=x
            if(y<minLY) minLY=y
            if(x>maxLX) maxLX=x
            if(y>maxLY) maxLY=y
            nT++
          }
        }
      }
    }
  }
}

function char_ramp(v){
  # v: 0..1
  if(v<0.05) return " "
  else if(v<0.10) return "."
  else if(v<0.16) return ":"
  else if(v<0.24) return "-"
  else if(v<0.34) return "*"
  else if(v<0.48) return "o"
  else if(v<0.64) return "O"
  else if(v<0.80) return "#"
  else return "@"
}

function draw_logo_reveal(tt,   i,x,y,cv,scan){
  tt=clamp(tt,0,1)
  if(tt<=0) return
  scan=int((H*0.12) + tt*(H*0.80))
  for(i=0;i<W;i++){
    if(hash01(i,23,int(tt*140))<0.26) put_xy(i,scan,"-",1)
  }
  # 只有在临近收尾时才轻描字形，避免“糊成一团”
  if(tt < 0.94) return
  for(i=0;i<nT;i++){
    if(ty[i] > scan) continue
    if(hash01(i,51,7) > tt) continue
    x=tx[i]; y=ty[i]
    cv = (tt<0.35?3:(tt<0.70?5:9))
    put_xy(x,y,"#",cv)
    if(tt>0.55 && hash01(i,19,int(tt*70))<0.030){
      put_xy(x+(hash01(i,5,1)<0.5?-1:1), y, ".", 3)
    }
  }
}

function clear_rect(x0,y0,x1,y1,   x,y){
  x0=int(x0); y0=int(y0); x1=int(x1); y1=int(y1)
  if(x0<0) x0=0; if(y0<0) y0=0
  if(x1>=W) x1=W-1; if(y1>=H) y1=H-1
  for(y=y0;y<=y1;y++){
    for(x=x0;x<=x1;x++){
      ii=idx(x,y)
      ch[ii]=" "
      col[ii]=0
    }
  }
}

function stamp_logo(glow,   i,x,y){
  # 收尾定稿：强制清晰（避免涌现阶段的纹理把字形糊掉）
  clear_rect(minLX-2, minLY-2, maxLX+2, maxLY+2)
  if(glow>0){
    for(i=0;i<nT;i++){
      x=tx[i]; y=ty[i]
      if(hash01(i,19,int(glow*120))<0.06){
        put_xy(x+1,y,".",3); put_xy(x-1,y,".",3)
        put_xy(x,y+1,".",3); put_xy(x,y-1,".",3)
      }
    }
  }
  for(i=0;i<nT;i++) put_xy(tx[i],ty[i],"#",9)
}

function lap(A, x,y,   c,n,ne,e,se,s,sw,w,nw){
  c=A[cidx(x,y)]
  n =A[cidx(x, (y-1+CH)%CH)]
  s =A[cidx(x, (y+1)%CH)]
  w =A[cidx((x-1+CW)%CW, y)]
  e =A[cidx((x+1)%CW, y)]
  nw=A[cidx((x-1+CW)%CW, (y-1+CH)%CH)]
  ne=A[cidx((x+1)%CW, (y-1+CH)%CH)]
  sw=A[cidx((x-1+CW)%CW, (y+1)%CH)]
  se=A[cidx((x+1)%CW, (y+1)%CH)]
  return -1.0*c + 0.2*(n+s+w+e) + 0.05*(nw+ne+sw+se)
}

function rd_step(F,k,   x,y,i,uu,vv,du,dv,lapU,lapV){
  for(y=0;y<CH;y++){
    for(x=0;x<CW;x++){
      i=cidx(x,y)
      uu=U[i]; vv=V[i]
      lapU=lap(U,x,y); lapV=lap(V,x,y)
      du = Du*lapU - uu*vv*vv + F*(1-uu)
      dv = Dv*lapV + uu*vv*vv - (F+k)*vv
      U2[i]=clamp(uu + du*(dtRD*rdScale), 0, 1)
      V2[i]=clamp(vv + dv*(dtRD*rdScale), 0, 1)
    }
  }
  for(i=0;i<CW*CH;i++){ U[i]=U2[i]; V[i]=V2[i] }
}

function deposit_tokens(str, y, amp,   i,chh,x){
  for(i=1;i<=length(str);i++){
    chh=substr(str,i,1)
    x=i-1
    # 只把 0/1/#/@ 作为 token，注入 V
    if(chh=="0"||chh=="1"||chh=="#"||chh=="@"){
      ci=cidx((x*3 + int(hash01(i,y,int(t*17))*5))%CW, y%CH)
      V[ci]=clamp(V[ci] + amp, 0, 1)
    }
  }
}

BEGIN{
  pi=3.141592653589793
  srand(1)

  fps=int(FPS+0); if(fps<12) fps=12; if(fps>60) fps=60
  dt=1.0/fps
  frames=int((DUR+0)*fps+0.5); if(frames<1) frames=1
  holdFrames=int((HOLD+0)*fps+0.5); if(holdFrames<0) holdFrames=0

  total=W*H
  cx=(W-1)/2.0
  cy=(H-1)/2.0

  # coarse grid (reaction diffusion) - 降低规模，确保真实运行时间 < 5s
  CW=int(clamp(W/3, 22, 54))
  CH=int(clamp(H/3, 10, 30))
  sx=W/CW
  sy=H/CH

  # reaction-diffusion params (Gray-Scott)
  Du=0.16
  Dv=0.08
  dtRD=1.0
  rdScale=1.0

  # init fields
  for(i=0;i<CW*CH;i++){ U[i]=1.0; V[i]=0.0 }
  # seed: a small central patch + faint noise
  for(y=0;y<CH;y++){
    for(x=0;x<CW;x++){
      i=cidx(x,y)
      if(abs(x-CW/2)<2 && abs(y-CH/2)<2) V[i]=0.55
      if(hash01(x,y,7)<0.02) V[i]=max(V[i], 0.35*hash01(x,11,y))
    }
  }

  # particles = tokens / agents（降低数量，减少每帧开销）
  NP=int(clamp(CW*CH*0.85, 260, 620))
  for(i=0;i<NP;i++){
    px[i]=CW/2 + (hash01(i,1,7)*2-1)*(CW*0.18)
    py[i]=CH/2 + (hash01(i,2,9)*2-1)*(CH*0.18)
    vx[i]=(hash01(i,3,11)*2-1)*0.25
    vy[i]=(hash01(i,4,13)*2-1)*0.25
  }

  font_init()
  scale=(W>=72 && H>=24)?2:1
  build_logo(scale)

  # coarse target mask：strict 用于“清晰字形”，soft 用于“引导收敛”
  for(i=0;i<CW*CH;i++){ tgtC1[i]=0; tgtC[i]=0 }
  for(y=0;y<H;y++){
    cyi=int(y/sy); if(cyi<0) cyi=0; if(cyi>=CH) cyi=CH-1
    for(x=0;x<W;x++){
      if(tgt[idx(x,y)]!=1) continue
      cxi=int(x/sx); if(cxi<0) cxi=0; if(cxi>=CW) cxi=CW-1
      tgtC1[cidx(cxi,cyi)]=1
    }
  }
  # soft mask：轻微扩散，避免引导太“硬边”
  for(y=0;y<CH;y++){
    for(x=0;x<CW;x++){
      i=cidx(x,y)
      if(tgtC1[i]==1){ tgtC2[i]=1; continue }
      s=0
      s+=tgtC1[cidx((x+1)%CW,y)]
      s+=tgtC1[cidx((x-1+CW)%CW,y)]
      s+=tgtC1[cidx(x,(y+1)%CH)]
      s+=tgtC1[cidx(x,(y-1+CH)%CH)]
      if(s>0) tgtC2[i]=0.45
      else tgtC2[i]=0
    }
  }
  for(i=0;i<CW*CH;i++) tgtC[i]=max(tgtC1[i], tgtC2[i])

  # 预计算 screen->coarse 映射，减少每帧除法
  for(x=0;x<W;x++){
    mx[x]=int(x/sx); if(mx[x]<0) mx[x]=0; if(mx[x]>=CW) mx[x]=CW-1
  }
  for(y=0;y<H;y++){
    my[y]=int(y/sy); if(my[y]<0) my[y]=0; if(my[y]>=CH) my[y]=CH-1
  }

  for(f=0; f<frames+holdFrames; f++){
    u=(frames<=1?1.0:(f/(frames-1.0)))
    t=u*(DUR+0)
    # 更快：内部演化速度≈3×（不改总时长；仍可用 --speed 额外调）
    spd=clamp((SPEED+0)*3.0, 0.2, 4.0)

    bang=smoothstep(clamp(u/0.22,0,1))
    org =smoothstep(clamp((u-0.18)/0.42,0,1))
    att =smoothstep(clamp((u-0.55)/0.30,0,1))
    # 更早开始“成型”，避免开屏拖
    rev =smoothstep(clamp((u-0.62)/0.30,0,1))
    # 把“涌现”做成由局部规则驱动的阶段
    F = 0.030 + 0.020*(1-bang) + 0.010*(1-att)
    k = 0.055 + 0.010*bang - 0.010*att

    # inject: token stream (left to right), stronger early
    if(u<0.70){
      yinj=int( (CH*0.20) + (CH*0.60)*hash01(int(t*9),3,7) )
      s1="01001101 01000001 01000011 01001000 01001001 01001110 01000101"
      deposit_tokens(s1, yinj, 0.10*(1-rev))
      if(hash01(7,13,int(t*10))<0.55) deposit_tokens("##@#1010@##", (yinj+2)%CH, 0.08*(1-rev))
    }

    # particle deposit + motion (local interactions -> emergence)
    for(i=0;i<NP;i++){
      xi=int(px[i]); yi=int(py[i])
      if(xi<0||xi>=CW||yi<0||yi>=CH){ continue }
      ci=cidx(xi,yi)
      V[ci]=clamp(V[ci] + 0.020*(0.6+0.6*bang), 0, 1)

      # gradient follow (self-organize)
      gx = V[cidx((xi+1)%CW,yi)] - V[cidx((xi-1+CW)%CW,yi)]
      gy = V[cidx(xi,(yi+1)%CH)] - V[cidx(xi,(yi-1+CH)%CH)]

      # swirl around center + gradient = emergent filaments
      sxw = (px[i]-CW/2)
      syw = (py[i]-CH/2)
      sw = 0.004 + 0.010*bang
      vx[i] += gx*(0.18+0.32*org) + (-syw)*sw
      vy[i] += gy*(0.18+0.32*org) + ( sxw)*sw

      # attention: steer towards logo targets (global coherence)
      if(att>0){
        tid=i % nT
        txc = (tx[tid]/W)*CW
        tyc = (ty[tid]/H)*CH
        vx[i] += (txc - px[i])*(0.010 + 0.040*att)
        vy[i] += (tyc - py[i])*(0.010 + 0.040*att)
      }

      # damping + speed clamp
      vx[i] *= (0.86 - 0.10*att)
      vy[i] *= (0.86 - 0.10*att)
      vmag = sqrt(vx[i]*vx[i] + vy[i]*vy[i])
      vmax = 0.75 + 0.90*bang + 0.70*spd
      if(vmag>vmax){
        vx[i] = vx[i]/vmag*vmax
        vy[i] = vy[i]/vmag*vmax
      }

      # integrate
      px[i] += vx[i]*(0.55+0.65*spd)
      py[i] += vy[i]*(0.55+0.65*spd)

      # wrap early, clamp late (to feel like converging)
      if(att<0.55){
        if(px[i]<0) px[i]+=CW; else if(px[i]>=CW) px[i]-=CW
        if(py[i]<0) py[i]+=CH; else if(py[i]>=CH) py[i]-=CH
      } else {
        px[i]=clamp(px[i],0,CW-1)
        py[i]=clamp(py[i],0,CH-1)
      }
    }

    # 涨成 logo：后段把“局部规则”叠加一个很轻的“目标场”，让噪声真的组出 AI termux
    if(rev>0){
      guide = rev*rev
      for(i=0;i<CW*CH;i++){
        target = tgtC[i]
        # 淡出非目标区域，增强对比
        V[i] = clamp(mix(V[i], target, 0.22*guide), 0, 1)
        U[i] = clamp(mix(U[i], 1-target*0.78, 0.12*guide), 0, 1)
      }
    }

    # reaction diffusion iterations (more steps when organizing)
    rdN = (u<0.22?1:(u<0.66?2:1))
    rdScale = 0.75 + 0.55*spd*(0.55+0.45*org)
    for(it=0; it<rdN; it++) rd_step(F,k)

    clearbuf()

    # render base field：先按 coarse cell 计算一次，再映射到 screen（显著提速）
    for(cyi=0; cyi<CH; cyi++){
      for(cxi=0; cxi<CW; cxi++){
        ii=cidx(cxi,cyi)
        vv=V[ii]
        gx = V[cidx((cxi+1)%CW,cyi)] - V[cidx((cxi-1+CW)%CW,cyi)]
        gy = V[cidx(cxi,(cyi+1)%CH)] - V[cidx(cxi,(cyi-1+CH)%CH)]
        edge = abs(gx)+abs(gy)

        v = clamp(vv*1.28,0,1)
        chh = char_ramp(v)
        if(v>0.74) cv=8
        else if(v>0.54) cv=4
        else if(v>0.38) cv=5
        else if(v>0.24) cv=3
        else if(v>0.15) cv=2
        else cv=1

        if(edge>0.30 && v>0.18){
          if(hash01(cxi,cyi,int(t*fps)) < (0.22 + 0.30*org)){
            chh = (hash01(cxi,7,cyi)<0.5?"+":"/")
            cv = (cv<3?3:cv)
          }
        }

        # 让噪声“组成字”：只在 strict mask 上做高对比锐化，避免糊成块
        if(rev>0.22 && tgtC1[ii]==1){
          if(v>0.20){
            cv = (rev<0.55?3:6)
            if(v>0.52) cv=9
            if(v>0.70) chh="#"
          }
        } else if(rev>0.35 && tgtC1[ii]!=1){
          # 非字形区域稍微压暗，凸显轮廓
          if(v<0.55) cv=min(cv,5)
        }

        cch[ii]=chh
        ccv[ii]=cv
      }
    }

    for(y=0;y<H;y++){
      cyi=my[y]
      for(x=0;x<W;x++){
        cxi=mx[x]
        ii=cidx(cxi,cyi)
        put_xy(x,y,cch[ii],ccv[ii])
      }
    }

    # overlay particles + attention links (visible logic)
    for(i=0;i<NP;i++){
      sxp = int(px[i]/CW*W)
      syp = int(py[i]/CH*H)
      if(sxp<0||sxp>=W||syp<0||syp>=H) continue
      pcv = (att>0.65?6:(org>0.55?3:2))
      pch = (bang>0.65?"*":".")
      if(att>0.85) pch="@"
      put_xy(sxp,syp,pch,pcv)

      if(att>0.35 && (i%41)==0){
        tid=i % nT
        txp=tx[tid]; typ=ty[tid]
        dline(sxp,syp,txp,typ,".", (att<0.70?2:6))
      }
    }

    # narrative header (短、清晰)
    if(u<0.26) put_str(int(W*0.06), int(H*0.08), "tokens  ->  noise", 1)
    else if(u<0.58) put_str(int(W*0.06), int(H*0.08), "local rules  ->  patterns", 1)
    else if(u<0.82) put_str(int(W*0.06), int(H*0.08), "attention  ->  coherence", 6)
    else put_str(int(W*0.06), int(H*0.08), "emergence  ->  identity", 6)

    # 收尾：仅在最后一点点时间里轻描轮廓（避免压过“涌现”本体）
    draw_logo_reveal(rev)
    if(rev>0.85) put_str(int((W-20)/2), int(H*0.74), "~$ AI termux ready", 6)

    # hold: stabilize logo
    if(rev>0.86){
      stamp_logo(rev)
    }
    if(f>=frames){
      stamp_logo(1.0)
      put_str(int((W-20)/2), int(H*0.74), "~$ AI termux ready", 6)
    }

    out="\033[H"
    for(y=0;y<H;y++){
      lastc=-1
      rowStr=""
      for(x=0;x<W;x++){
        ii=idx(x,y)
        cv=col[ii]
        if(COLOR==1){
          if(cv!=lastc){
            if(cv==0) rowStr=rowStr"\033[0m"
            else rowStr=rowStr ansi_color(cv)
            lastc=cv
          }
        }
        rowStr=rowStr ch[ii]
      }
      if(COLOR==1) rowStr=rowStr"\033[0m"
      if(y < H-1) out=out rowStr "\n"
      else out=out rowStr
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
