#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

SCRIPT_DIR="$(CDPATH='' cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
source "${SCRIPT_DIR}/_render.sh"

# AITermux boot animation #8（编译器流水线 / 工程感炫技）：
#   TOKENS -> IR graph -> OPT passes -> CODEGEN bytes -> RUN -> 扫描线生成 AI\n'termux
#
# 设计目标：
# - 纯 ASCII（不依赖宽字符、避免错位）
# - 默认 < 5 秒完成
# - 当 AI\n'termux 出现时，其他元素清空（画面语义更聚焦）

DURATION="${DURATION:-1.7}"
HOLD="${HOLD:-0.45}"
FPS="${FPS:-14}"
SPEED="${SPEED:-1.0}"
COLOR="${COLOR:-1}"
ALTSCR="${ALTSCR:-1}"
WIDTH="${WIDTH:-}"
HEIGHT="${HEIGHT:-}"

usage() {
  cat <<'USAGE'
usage: 8.sh [--duration SEC] [--hold SEC] [--fps N] [--speed X] [--size WxH] [--no-color] [--no-altscr]

ENV:
  DURATION HOLD FPS SPEED COLOR ALTSCR WIDTH HEIGHT

TIPS:
  - 更快： ./8.sh --duration 1.4 --hold 0.25 --fps 12
  - 省电： ./8.sh --fps 12 --no-color
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
function mix(a,b,t){ return a + (b-a)*t }
function smoothstep(x){ return x<=0?0:(x>=1?1:(x*x*(3-2*x))) }
function fract(x){ return x - int(x) }
function hash01(a,b,c){ return fract(sin(a*12.9898 + b*78.233 + c*37.719 + 0.1234)*43758.5453) }

function ansi_color(ci,   r,g,b){
  # 1 dim, 2 border, 3 cyan, 4 green, 5 purple, 6 amber, 7 magenta, 8 white, 9 logo
  if(ci==1){ r=90;  g=100; b=125 }
  else if(ci==2){ r=145; g=155; b=185 }
  else if(ci==3){ r=80;  g=230; b=255 }
  else if(ci==4){ r=120; g=255; b=155 }
  else if(ci==5){ r=200; g=130; b=255 }
  else if(ci==6){ r=255; g=190; b=110 }
  else if(ci==7){ r=255; g=110; b=220 }
  else { r=245; g=245; b=245 }
  return sprintf("\033[38;2;%d;%d;%dm", r,g,b)
}

function idx(x,y){ return y*W+x }
function clearbuf(   i){ for(i=0;i<total;i++){ ch[i]=" "; col[i]=0 } }
function put_raw(i,s,c){
  if(i<0||i>=total) return
  if(c < col[i] && col[i]!=0) return
  ch[i]=s; col[i]=c
}
function put_xy(x,y,s,c){
  x=int(x); y=int(y)
  if(x<0||x>=W||y<0||y>=H) return
  put_raw(idx(x,y),s,c)
}
function put_str(x,y,s,c,   i){ for(i=1;i<=length(s);i++) put_xy(x+i-1,y,substr(s,i,1),c) }
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

function box(x0,y0,x1,y1,   x,y){
  put_xy(x0,y0,"+",2); put_xy(x1,y0,"+",2)
  put_xy(x0,y1,"+",2); put_xy(x1,y1,"+",2)
  for(x=x0+1;x<=x1-1;x++){ put_xy(x,y0,"-",2); put_xy(x,y1,"-",2) }
  for(y=y0+1;y<=y1-1;y++){ put_xy(x0,y,"|",2); put_xy(x1,y,"|",2) }
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

function draw_logo(scale, scan,   line1,line2,w1,w2,ox,oy,ci,chh,row,pat,colx,dx,dy,x,y){
  line1="AI"; line2="termux"
  w1=(gw*length(line1) + gap*(length(line1)-1))*scale
  w2=(gw*length(line2) + gap*(length(line2)-1))*scale
  ox=int((W-max(w1,w2))/2)
  oy=int((H-(gh*2*scale + 1))/2)

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
            if(y <= scan) put_xy(x,y,"#",9)
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
            if(y <= scan) put_xy(x,y,"#",9)
          }
        }
      }
    }
  }
}

function fill_bg(t,   x,y,v,u,chh,ci){
  for(y=0;y<H;y++){
    for(x=0;x<W;x++){
      u = hash01(x, y, int(t*9))
      v = (x*11 + y*7 + int(t*17)) % 97
      if(u < 0.03){ chh="."; ci=1 }
      else if(u < 0.045){ chh=":"; ci=1 }
      else if(v==0){ chh=","; ci=1 }
      else { chh=" "; ci=0 }
      if(ci>0) put_xy(x,y,chh,ci)
    }
  }
}

function draw_pipeline(active,   labels,n,i,x,y,seg,arrow){
  labels[1]="LEX"; labels[2]="PARSE"; labels[3]="OPT"; labels[4]="CODEGEN"; labels[5]="RUN"
  n=5
  y=2
  arrow="==>"

  # compute total length
  totalLen=0
  for(i=1;i<=n;i++){
    seg="[" labels[i] "]"
    totalLen += length(seg)
    if(i<n) totalLen += length(arrow)
  }
  x=int((W-totalLen)/2)
  if(x<2) x=2

  for(i=1;i<=n;i++){
    seg="[" labels[i] "]"
    c=1
    if(i==1) c=4
    else if(i==2) c=5
    else if(i==3) c=6
    else if(i==4) c=7
    else c=3

    if(i==active) put_str(x,y,seg,c)
    else put_str(x,y,seg,1)
    x += length(seg)
    if(i<n){
      put_str(x,y,arrow,2)
      x += length(arrow)
    }
  }
}

function draw_progress(p, active,   x,y,w,fill,i,c){
  y=3
  x=3
  w=W-6
  if(w<18) return
  c=(active==1?4:(active==2?5:(active==3?6:(active==4?7:3))))
  put_xy(x-1,y,"[",2); put_xy(x+w,y,"]",2)
  fill=int(p*w+0.5)
  for(i=0;i<w;i++){
    if(i<fill) put_xy(x+i,y,"=",c)
    else put_xy(x+i,y,"-",1)
  }
}

function draw_tokens(p,t,   ns,s,xcol,head,len,i,yc,chh,cq,k,buf,ii){
  ns=int(clamp(W/9,6,16))
  for(s=0;s<ns;s++){
    xcol = 2 + int((s+1)*(W-4)/(ns+1))
    xcol += int(hash01(s, 11, int(t*5))*3) - 1
    xcol = clamp(xcol, 2, W-3)
    head = 5 + int( (t*12 + s*4) % (H-8) )
    len = 6 + int(hash01(s, 7, 3)*7)
    for(i=0;i<len;i++){
      yc=head-i
      if(yc<4 || yc>H-3) continue
      chh = substr(TOKSET, 1 + ((yc+i+s+int(t*20)) % tokLen), 1)
      put_xy(xcol,yc,chh, (i<2?4:(i<5?3:1)) )
    }
  }
  put_str_clip(3,5,"stage: LEX  |  split bytes -> tokens",3,W-4)
  cq=int(clamp(p/s1,0,1)*nTok+0.5)
  buf="TOKENS:"
  for(ii=0; ii<cq; ii++){
    buf = buf " " TOK[ii]
    if(length(buf) > W-6) break
  }
  put_str_clip(3,H-3,buf,4,W-4)
}

function draw_ir(p, t,   a,i,vis,cx,cy,x,y,lbl,c,edges,e,ua,vb,ex,ey){
  a=smoothstep(clamp((p-s1)/(s2-s1),0,1))
  vis=int(a*nN + 0.5)
  put_str_clip(3,5,"stage: PARSE  |  build IR graph (nodes/edges)",3,W-4)

  # edges first (behind)
  for(e=0;e<nE;e++){
    ua=EU[e]; vb=EV[e]
    if(ua>=vis || vb>=vis) continue
    dline(NX[ua], NY[ua], NX[vb], NY[vb], ".", 3)
  }
  # nodes
  for(i=0;i<vis;i++){
    x=NX[i]; y=NY[i]; lbl=NL[i]
    c=5
    put_xy(x-1,y,"[",c); put_xy(x+length(lbl),y,"]",c)
    put_str_clip(x,y,lbl,c,W-3)
  }
}

function draw_opt(p,t,   a,i,keep,e,ua,vb,passCount,pl){
  a=smoothstep(clamp((p-s2)/(s3-s2),0,1))
  put_str_clip(3,5,"stage: OPT  |  passes: DCE, fuse, layout, simplify",3,W-4)

  # start from full IR, then prune edges
  for(e=0;e<nE;e++){
    ua=EU[e]; vb=EV[e]
    keep = (e%2==0 || a < 0.55)
    if(!keep) continue
    dline(NX[ua], NY[ua], NX[vb], NY[vb], (a<0.4?".":"-"), 6)
  }
  for(i=0;i<nN;i++){
    lbl=NL[i]
    put_xy(NX[i]-1,NY[i],"[",6); put_xy(NX[i]+length(lbl),NY[i],"]",6)
    put_str_clip(NX[i],NY[i],lbl, (i==2 && a>0.55?4:8), W-3)
  }

  passCount=int(a*4 + 0.5)
  if(passCount>0) put_str_clip(3,7,"[OK] DCE removed dead ops",4,W-4)
  if(passCount>1) put_str_clip(3,8,"[OK] fuse: matmul+add -> gemm",4,W-4)
  if(passCount>2) put_str_clip(3,9,"[OK] layout: reorder memory",4,W-4)
  if(passCount>3) put_str_clip(3,10,"[OK] simplify constants",4,W-4)
}

function draw_codegen(p,t,   a,lines,base,i,y,off,hex,c){
  a=smoothstep(clamp((p-s3)/(s4-s3),0,1))
  put_str_clip(3,5,"stage: CODEGEN  |  emit bytes + link",3,W-4)
  lines=min(10, H-10)
  base=7
  off=int(t*40) % nHEX
  for(i=0;i<lines;i++){
    y=base+i
    hex=HEX[(off+i)%nHEX]
    c=(i%2==0?7:8)
    put_str_clip(4,y,hex,c,W-5)
  }
  if(a>0.65){
    put_str_clip(3,H-3,"[OK] link: aitermux.bin  |  entry: 0x0000",6,W-4)
  }
}

function draw_run(p,t,   a,sp,idxc){
  a=smoothstep(clamp((p-s4)/(s5-s4),0,1))
  put_str_clip(3,5,"stage: RUN  |  exec: ./aitermux.bin",3,W-4)
  put_str_clip(3,7,"> ./aitermux.bin --boot",8,W-4)
  sp="|/-\\"
  idxc = 1 + (int(t*24) % 4)
  put_str_clip(3,8,"running... " substr(sp,idxc,1) "  (warmup + load models)",4,W-4)
  if(a>0.55){
    put_str_clip(3,10,"[OK] process started  |  handoff to UI",4,W-4)
  }
}

BEGIN{
  fps=int(FPS+0); if(fps<10) fps=10; if(fps>60) fps=60
  dt=1.0/fps
  frames=int((DUR+0)*fps+0.5); if(frames<1) frames=1
  holdFrames=int((HOLD+0)*fps+0.5); if(holdFrames<0) holdFrames=0
  total=W*H
  font_init()

  # token queue
  TOKSET="abcdefghijklmnopqrstuvwxyz0123456789_+-*/=<>[]{}()"
  tokLen=length(TOKSET)
  nTok=0
  TOK[nTok++]="ID(AI)"
  TOK[nTok++]="DOT"
  TOK[nTok++]="ID(termux)"
  TOK[nTok++]="LP"
  TOK[nTok++]="ID(boot)"
  TOK[nTok++]="RP"
  TOK[nTok++]="OP(+)"
  TOK[nTok++]="NUM(1)"
  TOK[nTok++]="EOF"

  # IR graph layout (centered)
  cx=int(W/2); cy=int(H/2)+1
  nN=0
  NL[nN]="embed"; NX[nN]=cx-18; NY[nN]=cy-3; nN++
  NL[nN]="attn";  NX[nN]=cx-6;  NY[nN]=cy-5; nN++
  NL[nN]="resid"; NX[nN]=cx-6;  NY[nN]=cy-1; nN++
  NL[nN]="mlp";   NX[nN]=cx+8;  NY[nN]=cy-3; nN++
  NL[nN]="norm";  NX[nN]=cx+8;  NY[nN]=cy+1; nN++
  NL[nN]="out";   NX[nN]=cx+20; NY[nN]=cy-1; nN++

  nE=0
  EU[nE]=0; EV[nE]=1; nE++
  EU[nE]=1; EV[nE]=2; nE++
  EU[nE]=2; EV[nE]=3; nE++
  EU[nE]=3; EV[nE]=4; nE++
  EU[nE]=4; EV[nE]=5; nE++
  EU[nE]=1; EV[nE]=3; nE++

  # hexdump pool
  nHEX=0
  HEX[nHEX++]="0000: 7f 45 4c 46 02 01 01 00 00 00 00 00 00 00 00 00  |.ELF............|"
  HEX[nHEX++]="0010: 02 00 b7 00 01 00 00 00 78 00 00 00 00 00 00 00  |........x.......|"
  HEX[nHEX++]="0020: 40 00 00 00 00 00 00 00 00 00 00 00 40 00 38 00  |@...........@.8.|"
  HEX[nHEX++]="0030: 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  |................|"
  HEX[nHEX++]="0040: 48 31 c0 48 89 c2 48 89 c6 48 8d 3d 10 00 00 00  |H1.H..H..H.=....|"
  HEX[nHEX++]="0050: b0 3c 0f 05 61 69 74 65 72 6d 75 78 00 00 00 00  |.<..aitermux....|"
  HEX[nHEX++]="0060: 90 90 90 90 90 90 90 90 90 90 90 90 90 90 90 90  |................|"
  HEX[nHEX++]="0070: 00 11 22 33 44 55 66 77 88 99 aa bb cc dd ee ff  |..\"3DUfw........|"
  HEX[nHEX++]="0080: de ad be ef ca fe ba be 13 37 00 00 0b ad f0 0d  |.........7......|"
  HEX[nHEX++]="0090: 6d 6f 64 65 6c 3a 20 61 69 20 62 6f 6f 74 00 00  |model: ai boot..|"
  HEX[nHEX++]="00a0: 70 61 73 73 3a 20 66 75 73 65 20 2b 20 64 63 65  |pass: fuse + dce|"
  HEX[nHEX++]="00b0: 72 75 6e 3a 20 2e 2f 61 69 74 65 72 6d 75 78 00  |run: ./aitermux.|"
  HEX[nHEX++]="00c0: 6f 6b 00 00 00 00 00 00 00 00 00 00 00 00 00 00  |ok..............|"

  for(f=0; f<frames+holdFrames; f++){
    u=(frames<=1?1.0:(f/(frames-1.0)))
    t=u*(DUR+0) * (0.9 + 0.9*clamp(SPEED+0,0.2,2.4))
    p=clamp(u,0,1)

    clearbuf()
    fill_bg(t)

    # stage boundaries (0..1 over main frames)
    s1=0.30; s2=0.55; s3=0.73; s4=0.81; s5=0.90

    # logo stage: clear all other UI
    if(p >= s5){
      a=smoothstep(clamp((p-s5)/(1.0-s5),0,1))
      scan=int((H*0.12) + a*(H*0.86))
      scale=(W>=72 && H>=24)?2:1
      draw_logo(scale, scan)
      for(x=0;x<W;x++){
        if((x+int(t*40))%9==0) put_xy(x,scan,"-",1)
      }
      if(a>0.90) put_str(int((W-20)/2), int(H*0.76), "~$ AI termux ready", 4)
    } else {
      box(0,0,W-1,H-1)
      put_str_clip(2,0," AITermux  compile pipeline ",3,W-3)

      active = (p<s1?1:(p<s2?2:(p<s3?3:(p<s4?4:5))))
      draw_pipeline(active)
      draw_progress(p, active)

      if(p < s1){
        draw_tokens(p,t)
      } else if(p < s2){
        draw_ir(p,t)
      } else if(p < s3){
        draw_opt(p,t)
      } else if(p < s4){
        draw_codegen(p,t)
      } else {
        draw_run(p,t)
      }
    }

    if(f>=frames){
      scale=(W>=72 && H>=24)?2:1
      draw_logo(scale, H)
      put_str(int((W-20)/2), int(H*0.76), "~$ AI termux ready", 4)
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
