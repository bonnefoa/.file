if [ -d ~/git-repos/kube-ps1/ ]; then
    source ~/git-repos/kube-ps1/kube-ps1.sh
    KUBE_PS1_SYMBOL_ENABLE=false
    PROMPT='$(kube_ps1)'$PROMPT
fi
