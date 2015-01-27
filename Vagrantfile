# -*- mode: ruby -*-
# vi: set ft=ruby :

# All Vagrant configuration is done below. The "2" in Vagrant.configure
# configures the configuration version (we support older styles for
# backwards compatibility). Please don't change it unless you know what
# you're doing.
Vagrant.configure(2) do |config|
  # The most common configuration options are documented and commented below.
  # For a complete reference, please see the online documentation at
  # https://docs.vagrantup.com.

  # Every Vagrant development environment requires a box. You can search for
  # boxes at https://atlas.hashicorp.com/search.
  config.vm.box = "ubuntu/trusty64"

  # Disable automatic box update checking. If you disable this, then
  # boxes will only be checked for updates when the user runs
  # `vagrant box outdated`. This is not recommended.
  # config.vm.box_check_update = false

  # Create a forwarded port mapping which allows access to a specific port
  # within the machine from a port on the host machine. In the example below,
  # accessing "localhost:8080" will access port 80 on the guest machine.
  # config.vm.network "forwarded_port", guest: 80, host: 8080

  # Create a private network, which allows host-only access to the machine
  # using a specific IP.
  # config.vm.network "private_network", ip: "192.168.33.10"

  # Create a public network, which generally matched to bridged network.
  # Bridged networks make the machine appear as another physical device on
  # your network.
  # config.vm.network "public_network"

  # Share an additional folder to the guest VM. The first argument is
  # the path on the host to the actual folder. The second argument is
  # the path on the guest to mount the folder. And the optional third
  # argument is a set of non-required options.
  # config.vm.synced_folder ".", "/home/vagrant", disabled: false

  # Provider-specific configuration so you can fine-tune various
  # backing providers for Vagrant. These expose provider-specific options.
  # Example for VirtualBox:
  #

  config.vm.provider "virtualbox" do |vb|
    # Display the VirtualBox GUI when booting the machine
    # vb.gui = true
    # v.name = "encore"
    host = RbConfig::CONFIG['host_os']

    if host =~ /darwin/
      cpus = `sysctl -n hw.ncpu`.chomp
    elsif host =~ /linux/
      cpus = `nproc`.chomp
    else # Windows
      cpus = 2
    end

    # Set the number of available CPUs in the VM
    vb.customize ["modifyvm", :id, "--cpus", cpus]

    # Customize the amount of memory on the VM:
    vb.memory = "2048"
  end

  
  #
  # View the documentation for the provider you are using for more
  # information on available options.

  # Define a Vagrant Push strategy for pushing to Atlas. Other push strategies
  # such as FTP and Heroku are also available. See the documentation at
  # https://docs.vagrantup.com/v2/push/atlas.html for more information.
  # config.push.define "atlas" do |push|
  #   push.app = "YOUR_ATLAS_USERNAME/YOUR_APPLICATION_NAME"
  # end

  config.ssh.private_key_path = [ '~/.vagrant.d/insecure_private_key' ]

  config.ssh.forward_agent = true

  # Enable provisioning with a shell script. Additional provisioners such as
  # Puppet, Chef, Ansible, Salt, and Docker are also available. Please see the
  # documentation for more information about their specific syntax and use.

  config.vm.provision "install dependencies", type: "shell", :inline => <<-SHELL
     # Add sources
     sudo add-apt-repository -y ppa:hvr/ghc
     apt-get update

     # Install dependencies
     apt-get install -y clang lldb-3.5 g++ make premake4 zlib1g-dev ghc-7.8.3 cabal-install-1.22 unzip valgrind git emacs vim racket
     ln -s /usr/bin/cabal-1.22 /usr/bin/cabal
     ln -s /usr/bin/lldb-3.5 /usr/bin/lldb
  SHELL

  config.vm.provision "update cabal", type: "shell", privileged: false, :inline => <<-SHELL 
    export PATH=/vagrant/release:/opt/ghc/7.8.3/bin:$PATH
    cabal update && cabal install cabal-install
  SHELL

  config.vm.provision "run test", type: "shell", privileged: false, :inline => <<-SHELL
    export PATH=/vagrant/release:/opt/ghc/7.8.3/bin:$PATH
    cd /vagrant && make clean && make test
  SHELL

  config.vm.provision "update $PATH", type: "shell", privileged: false, inline: "echo export PATH=/vagrant/release:/opt/ghc/7.8.3/bin:$PATH >> .profile"

  config.vm.provision "symlink to shared folder", type: "shell", privileged: false, inline: "ln -s /vagrant/* /home/vagrant/"
end
