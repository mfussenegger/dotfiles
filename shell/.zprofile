PATH=$PATH:/home/$USER/bin
PATH=$PATH:/home/$USER/.local/bin/
PATH=$PATH:/home/$USER/.cargo/bin/
PATH=$PATH:$HOME/.local/share/npm/bin
PATH=$PATH:$HOME/.luarocks/bin:
PATH=$PATH:$HOME/.ghcup/bin:
PATH=$PATH:/.

export JDK11="/usr/lib/jvm/java-11-openjdk/"
export JDK17="$HOME/.m2/jdks/jdk-17.0.14+7/"
export JDK21="$HOME/.m2/jdks/jdk-21.0.6+7/"
export JDK23="$HOME/.m2/jdks/jdk-23.0.2+7/"
export JDK24="$HOME/.m2/jdks/jdk-24+36/"
export JAVA_HOME="$JDK24"
export GRAALVM_HOME="$HOME/.m2/jdks/graalvm-community-openjdk-23.0.2+7.1/"

if [[ -z "$SSH_CLIENT" ]]; then
  export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/gnupg/S.gpg-agent.ssh"
fi

export AMD_VULKAN_ICD=RADV
export MOZ_USE_XINPUT2=1
export MOZ_ENABLE_WAYLAND=1
export MOZ_WEBRENDER=1
export XDG_CURRENT_DESKTOP=sway

export ELECTRON_OZONE_PLATFORM_HINT=wayland
export QT_QPA_PLATFORM=wayland
export QT_QPA_PLATFORMTHEME=qt5ct
export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"

export QT_AUTO_SCREEN_SCALE_FACTOR=1
export QT_ENABLE_HIGHDPI_SCALING=1

export _JAVA_AWT_WM_NONREPARENTING=1

export SDL_VIDEODRIVEVER=wayland

[ -f ~/.zshrc.secret ] && source ~/.zshrc.secret

[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec sway
