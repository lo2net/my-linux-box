#!/bin/bash
export PS1='$PWD>'

# proxy
export http_proxy=http://proxy.zte.com.cn:80
export https_proxy=http://proxy.zte.com.cn:80

# for git
export LC_ALL=zh_CN.UTF-8
export LANG=zh_CN.UTF-8

# for Phabricator check
export PATH=$PATH:"/D/PhabricatorClient/php-5.4.45-nts-Win32-VC9-x86":"/D/PhabricatorClient/arcanist/bin"

# for haskell
#export PATH=$PATH:"/C/Program Files/Haskell Platform/8.2.1/lib/extralibs/bin":"/C/Program Files/Haskell Platform/8.2.1/bin"
export PATH=$PATH:"/C/Users/10034491/AppData/Roaming/local/bin"
# for stack
export STACK_ROOT="d:/home/10034491/.stack_root"

# for vc
export PATH="/D/U31_devtools/devtools_x64/vc/bin":$PATH
export LIB="D:/U31_devtools/devtools_x64/vc/lib"
export INCLUDE="D:/U31_devtools/devtools_x64/vc/include"

# maven
export PATH=$PATH:"/C/Program Files/JetBrains/IntelliJ IDEA 2017.3/plugins/maven/lib/maven3/bin"

# java
export PATH=$PATH:"/C/Program Files/Java/jdk1.8.0_112/bin"
export JAVA_HOME="/C/Program Files/Java/jdk1.8.0_112"

# for u31 compiling
export DEVTOOLS_ROOT="/D/U31_devtools/devtools_x64"
export WIN64=1

echo "------ $HOME/.bash_profile loaded ------"
