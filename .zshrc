export PATH="$PATH:$HOME/.local/bin:$HOME/go/bin"

# ZSH Config
export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="risto"

# DISABLE_AUTO_UPDATE="true"
# DISABLE_UPDATE_PROMPT="true"
# export UPDATE_ZSH_DAYS=13

plugins=(
  git
  dnf
  colored-man-pages
  colorize
  command-not-found
  docker
  encode64
)

source $ZSH/oh-my-zsh.sh

# User Config

# restart gnome process without killing session
alias killgnome="sudo killall -3 gnome-shell"

# dotnet test namespace
tnm () {
    dotnet test --filter "FullyQualifiedName~$1"
}
