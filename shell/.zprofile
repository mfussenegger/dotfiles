PATH=$PATH:/home/$USER/bin
PATH=$PATH:/home/$USER/.local/bin/
PATH=$PATH:/home/$USER/.cargo/bin/
PATH=$PATH:$HOME/.local/share/npm/bin
PATH=$PATH:$HOME/.luarocks/bin:
PATH=$PATH/.

export JDK11="/usr/lib/jvm/java-11-openjdk/"
export JDK17="$HOME/.m2/jdks/jdk-17.0.10+7/"
export JDK21="$HOME/.m2/jdks/jdk-21.0.2+13/"
export JDK22="$HOME/.m2/jdks/jdk-22+36//"
export JAVA_HOME="$JDK22"
export GRAALVM_HOME="$HOME/.m2/jdks/graalvm-community-openjdk-21.0.1+12.1//"

if [[ -z "$SSH_CLIENT" ]]; then
  export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/gnupg/S.gpg-agent.ssh"
fi

export AMD_VULKAN_ICD=RADV
export MOZ_USE_XINPUT2=1
export MOZ_ENABLE_WAYLAND=1
export MOZ_WEBRENDER=1
export XDG_CURRENT_DESKTOP=sway

export QT_QPA_PLATFORM=wayland
export QT_QPA_PLATFORMTHEME=qt5ct
export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"

export SDL_VIDEODRIVEVER=wayland

[ -f ~/.zshrc.secret ] && source ~/.zshrc.secret

[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec sway
