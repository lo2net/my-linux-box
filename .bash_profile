#!/bin/bash
export PS1='$PWD>'

# env separator
es=":"
driver_c="c:"
driver_d="d:"

MinGW_setup()
{
    echo " *** MinGW ***"
    echo

    es=":"
    driver_c="/c"
    driver_d="/d"
}
cygwin_setup()
{
    echo "*** cygwin ***\n"

    es=":"
    driver_c="/cygdrive/c"
    driver_d="/cygdrive/d"
}
mksnt_setup()
{
    echo "*** mksnt ***\n"
    export	ROOTDIR=${driver_d}/mksnt
    export	SHELL=$ROOTDIR/sh
    export	MKSINC=$ROOTDIR/inlcude
    export	PATH="${driver_d}/mksnt;$PATH"

    es=";"
    driver_c="c:"
    driver_d="d:"
}

export THIRD_PARTY_ROOT="d:/T3_3rdparty_SDK/for_vc7"
export ACE_ROOT="$THIRD_PARTY_ROOT/ace"
export TAO_ROOT="$THIRD_PARTY_ROOT/ace/TAO"
export SNACC_ROOT="$THIRD_PARTY_ROOT/snacc"
export XERCES_ROOT="$THIRD_PARTY_ROOT/xerces_2_7_0"
export AES_ROOT="$THIRD_PARTY_ROOT/aes"
export BOOST_ROOT="$THIRD_PARTY_ROOT/../boost_1_34_1"
export CPPUNIT_ROOT="$THIRD_PARTY_ROOT/../backup/cppunit"
export UNITEST_ROOT="$THIRD_PARTY_ROOT/../unitest"

platform=`uname -s`
case $platform in
    MINGW32_NT-5.1)
        MinGW_setup;
        ;;
    MINGW32_NT-6.0)
        MinGW_setup;
        ;;
    CYGWIN_NT-5.1)
        cygwin_setup;
        ;;
    Windows_NT)
        mksnt_setup;
        ;;
esac

#############################################################
## VC6
vc6()
{
export PATH="${driver_c}/Program Files/Microsoft Visual Studio/VC98/Bin${es}${driver_c}/Program Files/Microsoft Visual Studio/Common/MSDev98/Bin${es}$PATH"
export INCLUDE="c:/Program Files/Microsoft Visual Studio/VC98/Include"
export LIB="c:/Program Files/Microsoft Visual Studio/VC98/Lib"
}

## VC7
vc7()
{
    export PATH="${driver_d}/Vc7/bin${es}$PATH"
    export INCLUDE="d:/Vc7/include;d:/Vc7/PlatformSDK/Include;d:/Vc7/atlmfc/include"
    export LIB="d:/Vc7/lib;d:/Vc7/PlatformSDK/Lib;d:/Vc7/atlmfc/lib"
}
vc2008()
{
    export PATH="${driver_d}/vc2008/bin${es}$PATH"
    export INCLUDE="d::/vc2008/include;d:/vc2008/PlatformSDK/include;d:/vc2008/atlmfc/include"
    export LIB="d:/vc2008/lib;d;/vc2008/PlatformSDK/lib;d:/vc2008/atlmfc/lib"
}

# for STLport
#export INCLUDE="E:/lib_code_src/C++ Libraries/STL/STLport-4.6.2/stlport;$INCLUDE"
#export LIB="E:/lib_code_src/C++ Libraries/STL/STLport-4.6.2/lib;$LIB"

t3()
{
    echo "T3 development enviroment"

    export ICAN_KERNEL_HOME="d:/views/t3_dev/t3/code/server"
    export ICAN_PLATFORM=nt40
    test -n "$ICAN_KERNEL_HOME" && cd $ICAN_KERNEL_HOME
}

u31()
{
    echo "U31 development enviroment"

    ver="BN_PLATFORM BN_NECOMMON BN_SDH U31_E2E BN_WDM INTERFACE"
    echo "\nPlease select T3_4X_KERNEL_HOME variable:"
    select version in $ver;do
    {
        case $version in
            BN_PLATFORM)
            echo "BN_PLATFORM"
            export T3_4X_KERNEL_HOME=d:/U31_svn/BN_Platform/trunk/code_c
	    export T3_4X_INTERFACE_HOME=d:/U31_svn/BN_Platform/trunk/sdk/interface/sdk-c
            break;
            ;;
            BN_NECOMMON)
            echo "BN_NECOMMON"
            export T3_4X_KERNEL_HOME=d:/U31_svn/BN_NECOMMON/trunk/code_c
	    export T3_4X_INTERFACE_HOME=d:/U31_svn/BN_NECOMMON/trunk/sdk/interface/
            break;
            ;;
            BN_SDH)
            echo "BN_SDH"
            export T3_4X_KERNEL_HOME=d:/U31_svn/BN_SDH/trunk/code_c
	    export T3_4X_INTERFACE_HOME=d:/U31_svn/BN_SDH/trunk/sdk/interface/sdk-c
            break;
            ;;
            U31_E2E)
            echo "U31_E2E"
            export T3_4X_KERNEL_HOME=d:/U31_svn/U31_E2E/trunk/code_c
            break;
            ;;
            BN_WDM)
            echo "BN_WDM"
            export T3_4X_KERNEL_HOME=d:/U31_svn/BN_WDM/trunk/code_c
	    export T3_4X_INTERFACE_HOME=d:/U31_svn/BN_WDM/trunk/sdk/interface/sdk-c
            break;
	    ;;
	    INTERFACE)
	    echo "INTERFACE"
	    export T3_4X_KERNEL_HOME=d:/U31_svn/Interface/trunk/code_c
	    break;
            ;;
            *)
            echo "select again!"
            ;;
        esac
    }
    done

    export ICAN_PLATFORM=nt40
    export T3_4X_OUTPUT_HOME=$T3_4X_KERNEL_HOME/build/output
    test -n "$T3_4X_KERNEL_HOME" && cd $T3_4X_KERNEL_HOME
}

compiler_env="vc6 vc7 vc2008"
select compiler in $compiler_env; do
case $compiler in
	vc6)
    vc6;
    break;
    ;;
    vc7)
    vc7;
    break;
    ;;
    vc2008)
    vc2008;
    break;
    ;;
    *)
    echo "select again!"
    ;;
esac
done

dev_env="T3 U31"
select dev_env in $dev_env; do
case $dev_env in
    T3)
    t3;
    break;
    ;;
    U31)
    u31;
    break;
    ;;
    *)
    echo "select again!"
    ;;
esac
done

# for clang
export PATH="${driver_d}/cygwin/home/fangtao/tools/clang3.3_march2013${es}$PATH"

echo "------ $HOME/.bash_profile loaded ------"
