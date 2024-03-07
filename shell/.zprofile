PATH=$PATH:/home/$USER/bin
PATH=$PATH:/home/$USER/.local/bin/
PATH=$PATH:/home/$USER/.cargo/bin/

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
