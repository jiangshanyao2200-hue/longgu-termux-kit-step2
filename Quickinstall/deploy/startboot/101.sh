#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/_render.sh"

ALTSCR=1
COLOR=1
WIDTH=""
HEIGHT=""
FPS="24"
DURATION="1.8"

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
      FPS="${2:-24}"
      shift 2
      ;;
    --duration|-d)
      DURATION="${2:-1.8}"
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

text="AItermux"
title="Project Ying"
total_frames="$(awk -v d="$DURATION" -v fps="$FPS" 'BEGIN{n=int(d*fps); if(n<1)n=1; print n}')"
sleep_dt="$(awk -v fps="$FPS" 'BEGIN{if(fps<10)fps=10; if(fps>60)fps=60; printf "%.4f", 1/fps}')"

for ((f=1; f<=total_frames; f++)); do
  reveal="$(awk -v f="$f" -v n="$total_frames" -v tlen="${#text}" 'BEGIN{r=int((f/n)*tlen); if(r<1)r=1; if(r>tlen)r=tlen; print r}')"
  show="${text:0:reveal}"
  cx=$(( (cols - ${#show}) / 2 ))
  cy=$(( rows / 2 ))
  tx=$(( (cols - ${#title}) / 2 ))
  ty=$(( cy - 2 ))

  frame="\033[H"
  for ((r=1; r<=rows; r++)); do
    line=""
    if (( r == ty )); then
      line="$(printf '%*s%s' "$tx" '' "$title")"
    elif (( r == cy )); then
      line="$(printf '%*s%s' "$cx" '' "$show")"
    else
      line=""
    fi
    frame+="$line"
    if (( r < rows )); then frame+=$'\n'; fi
  done

  if (( COLOR )); then
    frame="\033[38;2;120;255;170m${frame}\033[0m"
  fi
  aitermux_anim_print_frame "$frame" "$rows"
  sleep "$sleep_dt"
done

sleep 0.12
