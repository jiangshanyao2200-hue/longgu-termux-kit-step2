#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/_render.sh"

ALTSCR=1
COLOR=1
WIDTH=""
HEIGHT=""
FPS="22"
DURATION="2.0"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --size|-s)
      if [[ "${2:-}" =~ ^([0-9]+)x([0-9]+)$ ]]; then
        WIDTH="${BASH_REMATCH[1]}"
        HEIGHT="${BASH_REMATCH[2]}"
      fi
      shift 2
      ;;
    --no-color)
      COLOR=0
      shift
      ;;
    --altscr)
      ALTSCR=1
      shift
      ;;
    --no-altscr)
      ALTSCR=0
      shift
      ;;
    --fps)
      FPS="${2:-22}"
      shift 2
      ;;
    --duration|-d)
      DURATION="${2:-2.0}"
      shift 2
      ;;
    *)
      shift
      ;;
  esac
done

cols="${WIDTH:-$(tput cols 2>/dev/null || echo 80)}"
rows="${HEIGHT:-$(tput lines 2>/dev/null || echo 24)}"
if (( cols > 2 )); then cols=$((cols-1)); fi
if (( rows < 12 )); then rows=12; fi

cleanup() {
  printf '\033[0m\033[?25h\033[?7h'
  if (( ALTSCR )); then
    printf '\033[?1049l'
  fi
}
trap cleanup EXIT INT TERM

printf '\033[?25l\033[?7l'
if (( ALTSCR )); then
  printf '\033[?1049h'
fi

total_frames="$(awk -v d="$DURATION" -v fps="$FPS" 'BEGIN{n=int(d*fps); if(n<1)n=1; print n}')"
sleep_dt="$(awk -v fps="$FPS" 'BEGIN{if(fps<10)fps=10; if(fps>60)fps=60; printf "%.4f", 1/fps}')"
label="Booting AITermux"

for ((f=1; f<=total_frames; f++)); do
  progress="$(awk -v f="$f" -v n="$total_frames" 'BEGIN{p=int((f/n)*100); if(p>100)p=100; print p}')"
  bar_w=$(( cols > 40 ? 32 : cols - 8 ))
  if (( bar_w < 10 )); then bar_w=10; fi
  fill=$(( progress * bar_w / 100 ))
  bar=""
  for ((i=1; i<=bar_w; i++)); do
    if (( i <= fill )); then
      bar+="#"
    else
      bar+="-"
    fi
  done

  cx=$(( (cols - ${#label}) / 2 ))
  bx=$(( (cols - (bar_w + 2)) / 2 ))
  cy=$(( rows / 2 - 1 ))
  py=$(( cy + 2 ))

  frame="\033[H"
  for ((r=1; r<=rows; r++)); do
    line=""
    if (( r == cy )); then
      line="$(printf '%*s%s' "$cx" '' "$label")"
    elif (( r == cy + 1 )); then
      line="$(printf '%*s[%s]' "$bx" '' "$bar")"
    elif (( r == py )); then
      pct="${progress}%"
      px=$(( (cols - ${#pct}) / 2 ))
      line="$(printf '%*s%s' "$px" '' "$pct")"
    fi
    frame+="$line"
    if (( r < rows )); then frame+=$'\n'; fi
  done

  if (( COLOR )); then
    frame="\033[38;2;150;220;255m${frame}\033[0m"
  fi
  aitermux_anim_print_frame "$frame" "$rows"
  sleep "$sleep_dt"
done

sleep 0.12
