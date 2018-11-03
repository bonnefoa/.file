if [ -d ~/git-repos/kube-ps1/ ]; then
    source ~/git-repos/kube-ps1/kube-ps1.sh
    PROMPT='$(kube_ps1)'$PROMPT
fi

if [ -d ~/git-repos/kubectl-fzf/ ]; then
    source ~/git-repos/kubectl-fzf/kubectl_fzf.plugin.zsh
fi
