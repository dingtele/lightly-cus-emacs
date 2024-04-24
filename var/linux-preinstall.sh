# from windows/user/.wslconf
#input with .. 
# [experimental]
# autoMemoryReclaim = gradual

#!/usr/bin/env bash

#A0-0 for ubuntu, with sudo
#1-0 OS setting: enable systemd to use systemctl to start services
cat >> /etc/wsl.conf <<EOF
[boot]
systemd = true
EOF
exit
#wsl --shutdown
#1-1

#2-0 docker
#2.1-0 uninstall all conflicting packages:
for pkg in docker.io docker-doc docker-compose docker-compose-v2 podman-docker containerd runc; do sudo apt-get remove $pkg; done

#clear docker file space
# rm -rf /var/lib/docker
# rm -rf /var/lib/containerd
#2.1-1

#2.2-0 set up docker's apt repository
# Add Docker's official GPG key:
sudo apt-get update
sudo apt-get install ca-certificates curl
sudo install -m 0755 -d /etc/apt/keyrings
sudo curl -fsSL https://download.docker.com/linux/ubuntu/gpg -o /etc/apt/keyrings/docker.asc
# Add the repository to Apt sources:
echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/ubuntu \
  $(. /etc/os-release && echo "$VERSION_CODENAME") stable" | \
  sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
sudo apt-get update
# install docker packages

#6-0 emacs
#6-1

#7-0 tiny tools: fzf
#7-1




#A0-1


#B0-0 for fedora

#B0-1
