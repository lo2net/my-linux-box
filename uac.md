* 第一步
LoginActivity::onClick()
    -> getUACConfig()
    -> AuthenticationHelper::getAuthenticationConfig()

https://uac.zte.com.cn/commonmng/config/configInfo.serv
POST => ConfigInputRequest
{
    "verifyCode": "0cc69d7a82ae809aaa6d033b8b849ef6",   --- md5("0uacapp")
    "type": "0",
    "systemCode": "uacapp",
    "patternKey": ""
}
Response:
{
    "other": {
        "uacapp_url_moacore": "https://moa.zte.com.cn/Application/MainFrame/Login.aspx?method=Logout&FromPageUrl=/application/businessapply/AppApply.aspx?applytype=2",
        "uacapp_url_rely_moa": "1"
    },
    "bo": {
        "enMsg": "success",
        "msg": "成功",
        "code": "0000"
    },
    "code": {
        "msg": "操作成功",
        "msgId": "RetCode.Success",
        "code": "0000"
    }
}
成功之后保存:
sharedpreferencesutil.addOrModify("UACConfigSP", "uacapp_url_rely_moa", configinputresponse.getOther().getUacapp_url_rely_moa());
sharedpreferencesutil.addOrModify("UACConfigSP", "applyDeviceAuthenticationUrl", configinputresponse.getOther().getUacapp_url_moacore());
然后调用 loginSSOSecurityAuth()到第二步

* 第二步
loginSSOSecurityAuth()
    -> SSOAuthLoginManager::loginByUserAndPsw()
        保存userId:
        SharedPreferencesUtil.getInstance(this).addOrModify("UACUserInfoSP", "userId", user);
        sharedUtil.addOrModify("userInfoSP", user, "");

    String key = Encrypt.EncryptEmployeeNoAndPasword(user, password);
    
    10034491
    lo2net2015K
    xxxxxxxxxx ---> 最小10字节随机字符串，包含0-9a-zA-Z
    依次排列字符，不足用#填充得到结果 => 1lx0ox02x3nx4ex4tx92x10x#1x#5x#Kx
    DES/CBC/PKCS5Padding加密， KeyValue = "jF0bhel9"; static byte Keys[] = { 18, 52, 86, 120, -112, -85, -51, -17 };  12 34 56 78 90 AB CD EF
    
    login(user, key, "0", "");
    ssoappservice.login(key, "A00233" /*appId*/, "0", baseasynchttpresponsehandler, "");

    按照下面获取地址: SSOAuthConfig.getApiServerFullURLLogin()
        String s11 = (String)map.get("sso_api_server_full_url_login_outer");
        String s14 = (String)map.get("sso_api_server_full_url_login_inner");
    如果返回空,则按照下面规则获取:
        GLOBALARRAY_LOGIN 或者 IS_TEST_ENVIRONMENT 或者 !IS_EXTERNAL_ADDRESS_LOGIN
            map = resourceutil.getResourceHashMap(i);
            String ip = (String)map.get("sso_api_server_ip_outer");  -> mdm.zte.com.cn
            String port = (String)map.get("sso_api_server_port_outer");
            String ip = (String)map.get("sso_api_server_ip_inner");
            String port = (String)map.get("sso_api_server_port_inner");
        否则:
            ip = securityauthutils.getSSOAuthLoginIp();
                return sharedUtil.getString("ssoAuthLoginAddressSP", "ssoAuthLoginIp", "");
            port = securityauthutils.getSSOAuthLoginPort();
                return sharedUtil.getString("ssoAuthLoginAddressSP", "ssoAuthLoginPort", "80");
            http://ip:port/emm/login/service.jssm

    POST => SSOLoginHttpRequest
    {
        "AppId" : "A00233"
        "Br" : "samsung"   --- Build.BRAND
        "CommandName" : "GetSecurityCode"
        "DM" : "SM-A9100"  --- Build.MODEL
        "DN" : "SM-A9100"  --- Build.MODEL
        "DT" : "2"
        "LangId" : "2052"  --- 2052(zh) "1033"(en)
        "MON" : ""
        "Net" :  "WIFI"    --- "no" "2G" "3G" "4G" "WIFI" "unknown"
        "OSVer" : "6.0.1"  --- android.os.Build.VERSION.RELEASE
        "Token" : ?
        "AppVer" :     PackageInfo packageinfo = mContext.getPackageManager().getPackageInfo(mContext.getPackageName(), 0); packageinfo.versionName;
        "EToken" : key
        "LF" : "0"
    }
    返回结构 => SSOLoginHttpResponse
        private String CryPass;
        private String RegRC;
        private String RegRM;
        private String TS;
        private String Token;
        private UserInfo UIF;
    如果返回结果 !"1".equals(getRegRC()) 表示要注册设备? 调用 
        -> authDeviceManager.authDevice(userID, "0", ssologinhttpresponse);
            POST => http://ip:port/emm/device/service.jssm
            CommandName = "UploadDeviceInfo";
            按照下面函数获取 SSOAuthConfig::getApiServerFullURLDevice()
            如果为空则调用正常函数获取,同上面类似内容
            请求结构 SSODeviceHttpRequest:
            SSODeviceHttpRequest ssodevicehttprequest = new SSODeviceHttpRequest(SSOAuthConfig.getAuthServerHttpsFlag(), SSOAuthConfig.getAuthServerIPPort().getIp(SSOAuthConfig.isInnerNet()), SSOAuthConfig.getAuthServerIPPort().getPort(SSOAuthConfig.isInnerNet()));
            String s4 = context.getPackageManager().getPackageInfo(context.getPackageName(), 0).versionName;
            ssodevicehttprequest.setUId(s);
            ssodevicehttprequest.setCurVer(s4);
            ssodevicehttprequest.setAppId(s1);
            ssodevicehttprequest.setTel(null);
            ssodevicehttprequest.setChAdd(s2);
            ssodevicehttprequest.setToken(s3);
            响应结构 SSODeviceHttpResponse:
            
            ->  authenticatedDevice(userAccountCredentsDevice);    
                saveSSOAuthResultData(userID, loginHttpResponse);
    否则  如果 !SSOAuthLoginManager.APP_GET_TOKEN_EXPIRING_LOGIN)
        -> saveSSOAuthResultData(s, ssologinhttpresponse);
            SSOAuthResultData ssoauthresultdata = new SSOAuthResultData();
            Date date = new Date(1000L * Long.valueOf(ssologinhttpresponse.getTS()).longValue() + System.currentTimeMillis());
            ssoauthresultdata.setTimestamp((new SimpleDateFormat(SSOAuthConfig.getAuthDataDateFormart())).format(date));
            ssoauthresultdata.setCryptoPassword(ssologinhttpresponse.getCryPass());
            ssoauthresultdata.setToken(ssologinhttpresponse.getToken());
            ssoauthresultdata.setUserId(s);
            ssoauthresultdata.setUserInfo(ssologinhttpresponse.getUIF());
            boolean flag = saveAuthData(ssoauthresultdata);
            boolean flag1 = saveAuthDataFromUAC(ssoauthresultdata);

    否则 
        -> saveSSOAuthResultData(userID, ssologinhttpresponse);
           上面一样的代码
        -> authLoginCallBack.onLoginSuccess();
            -> authenticatedDevice()
                POST => https://uac.zte.com.cn/commonmng/mobliedevice/mobiledevice.serv
                    userAccountCredentsDevice = new UserAccountCredentsDevice();
                    String s = login_input_jobnum.getText().toString().trim();
                    String s1 = login_input_password.getText().toString().trim();
                    String s2 = (new DeviceUtil(mContext)).getDeviceUniqueId(mContext);
                    userAccountCredentsDevice.setAccount(s);
                    userAccountCredentsDevice.setDeviceNumber(s2);
                    userAccountCredentsDevice.setAppVersion("");
                    userAccountCredentsDevice.setDeviceType("");
                    userAccountCredentsDevice.setSystemVersion("");
                    MobileDeviceInputRequest mobiledeviceinputrequest = new MobileDeviceInputRequest(mContext, true, "uac.zte.com.cn", "443");
                    UserAccountCredentsDeviceOther useraccountcredentsdeviceother = new UserAccountCredentsDeviceOther();
                    String s = (new SimpleDateFormat(UACMobileConstants.getAuthDataDateFormart())).format(Long.valueOf(System.currentTimeMillis())).toString();
                    StringBuilder stringbuilder = new StringBuilder();
                    stringbuilder.append(useraccountcredentsdevice.getAccount()).append(useraccountcredentsdevice.getDeviceNumber()).append(useraccountcredentsdevice.getAppVersion()).append(Build.MODEL).append(useraccountcredentsdevice.getSystemVersion()).append(s);
                    mobiledeviceinputrequest.setAccount(useraccountcredentsdevice.getAccount());
                    mobiledeviceinputrequest.setDeviceNumber(useraccountcredentsdevice.getDeviceNumber());
                    mobiledeviceinputrequest.setAppVersion("");
                    mobiledeviceinputrequest.setDeviceType(Build.MODEL);
                    mobiledeviceinputrequest.setSystemVersion("");
                    mobiledeviceinputrequest.setAppTime(s);
                    mobiledeviceinputrequest.setVerifyCode(MD5Helper.verfiyCode(stringbuilder.toString()));
                    mobiledeviceinputrequest.setOther(useraccountcredentsdeviceother);
                返回结构 MobileDeviceInputResponse
                    如果 mobiledeviceinputresponse.getBo().getCode() == "0000" 则要调用 submitActivationLoging()
                    POST => UserAccountCredentsDeviceRequest
                    https://uac.zte.com.cn/uacmobile/auth/credents/verify.serv
                    userAccountCredentsDevice = new UserAccountCredentsDevice();
                    userAccountCredentsDevice.setAccount(user);
                    userAccountCredentsDevice.setPassWord(password);
                    userAccountCredentsDevice.setCertificate("");
                    s2 = (new DeviceUtil(mContext)).getDeviceUniqueId(mContext);
                    userAccountCredentsDevice.setDeviceNumber(s2);

                    UserAccountCredentsDeviceRequest useraccountcredentsdevicerequest = new UserAccountCredentsDeviceRequest(mContext, true, "uac.zte.com.cn", "443");
                    String s = (new SimpleDateFormat(UACMobileConstants.getAuthDataDateFormart())).format(Long.valueOf(System.currentTimeMillis())).toString();
                    StringBuilder stringbuilder = new StringBuilder();
                    stringbuilder.append(useraccountcredentsdevice.getAccount()).append(useraccountcredentsdevice.getPassWord()).append(useraccountcredentsdevice.getCertificate()).append(useraccountcredentsdevice.getDeviceNumber()).append(Build.MODEL).append(s);
                    useraccountcredentsdevicerequest.setAccount(useraccountcredentsdevice.getAccount());
                    useraccountcredentsdevicerequest.setPassWord(useraccountcredentsdevice.getPassWord());
                    useraccountcredentsdevicerequest.setCertificate(useraccountcredentsdevice.getCertificate());
                    useraccountcredentsdevicerequest.setDeviceNumber(useraccountcredentsdevice.getDeviceNumber());
                    useraccountcredentsdevicerequest.setAppTime(s);
                    useraccountcredentsdevicerequest.setVerifyCode(MD5Helper.verfiyCode(stringbuilder.toString()));
                    返回结构 UserAccountCredentsDeviceResponse
                    UACAuthInfoResultData uacauthinforesultdata = new UACAuthInfoResultData();
                    uacauthinforesultdata.setAccount(s);
                    uacauthinforesultdata.setSeedID(useraccountcredentsdeviceresponse.getOther().getSeedID());
                    uacauthinforesultdata.setServerTime(useraccountcredentsdeviceresponse.getOther().getServerTime());
                    uacauthinforesultdata.setDynRefreshFreq(useraccountcredentsdeviceresponse.getOther().getDynRefreshFreq());
                    Date date = new Date(System.currentTimeMillis());
                    uacauthinforesultdata.setNativeFirstTime((new SimpleDateFormat(UACMobileConstants.getComputerDataDateFormart())).format(date));
                    flag = saveAuthData(uacauthinforesultdata);
                    ---> s = Encrypt.Encrypt(JsonUtil.toJson(uacauthinforesultdata));
                         boolean flag1 = SharedPreferencesUtil.getInstance(mContext).addOrModify("UACSeedcode", "uacSeedcode", s);


* PC 上登陆moa的技巧,需要使用下面列表里面的id才能看到密码输入框
"10114660,10182285,10191531,10003287"
例如: https://moa.zte.com.cn/Application/MainFrame/Login.aspx?user=10003287


* charles抓包结果
URL	http://mdm.zte.com.cn:80/emm/token/service.jssm
{
	"AppVer": "1.1.4",
	"IGUI": "0",
	"UId": "10034491",
	"AppId": "A00233",
	"Br": "samsung",
	"CommandName": "CheckSSOToken",
	"DM": "SM-A9100",
	"DN": "SM-A9100",
	"DT": "2",
	"LangId": "2052",
	"Net": "WIFI",
	"OSVer": "6.0.1",
	"Token": "76fvFjy/wns/NPobXaPzYF50DniTJSAcM3nnkw8XKlcPQbtjU7I+rJzPIS/rGH+ax0r8oaP7NsdKTKtkRGuHyg\u003d\u003d",
	"DId": "9BD02BF38238E5B5F85C022DF6DC244C"
}
{
	"UIF": {
		"UID": "",
		"ENM": "",
		"CNM": "",
		"DPNM": "",
		"PNM": ""
	},
	"RC": "0",
	"RM": "token有效"
}

URL	http://mdm.zte.com.cn:80/emm/appExchange/service.jssm
{
	"AppId": "A00233",
	"CommandName": "GetAppVersion",
	"CurVer": "1.1.4",
	"DM": "SM-A9100",
	"DN": "SM-A9100",
	"DT": "2",
	"LangId": "2052",
	"OSVer": "6.0.1",
	"Token": "76fvFjy/wns/NPobXaPzYF50DniTJSAcM3nnkw8XKlcPQbtjU7I+rJzPIS/rGH+ax0r8oaP7NsdKTKtkRGuHyg\u003d\u003d",
	"UId": "10034491",
	"packageName": "cn.com.zte.app.uac",
	"DId": "9BD02BF38238E5B5F85C022DF6DC244C"
}
{
	"RC": "0",
	"RM": "0",
	"AppId": "A00233",
	"AppNM": "安全令牌",
	"AppRK": "",
	"AppTP": "1",
	"AppAccUrl": "",
	"SchUrl": "",
	"LateVerDesc": "1.新增日志上传功能\n2.设备认证机制优",
	"PKSize": "10",
	"IdeUrl": "com.zte.aaiz",
	"PKNM": "cn.com.zte.app.uac",
	"LateVer": "1.1.2",
	"AppDL": "emm/UpLoadFileJSONService/service.dssm?appId\u003dA00233\u0026versionNo\u003d1.1.2\u0026fileName\u003dUAC.apk"
}

URL	https://uac.zte.com.cn/commonmng/config/configInfo.serv
{
	"patternKey": "",
	"systemCode": "uacapp",
	"type": "0",
	"verifyCode": "0cc69d7a82ae809aaa6d033b8b849ef6",
	"DId": "9BD02BF38238E5B5F85C022DF6DC244C"
}
{
	"code": {
		"code": "0000",
		"msgId": "RetCode.Success",
		"msg": "操作成功"
	},
	"bo": {
		"code": "0000",
		"msg": "成功",
		"enMsg": "success"
	},
	"other": {
		"uacapp_url_rely_moa": "1",
		"uacapp_url_moacore": "https://moa.zte.com.cn/Application/MainFrame/Login.aspx?method=Logout&FromPageUrl=/application/businessapply/AppApply.aspx?applytype=2"
	}
}

URL	https://uac.zte.com.cn/commonmng/mobliedevice/mobiledevice.serv
{
	"account": "10034491",
	"appTime": "2017-06-26 01:27:02.765",
	"appVersion": "1.1.4",
	"deviceNumber": "V02_357755071308934",
	"deviceType": "SM-A9100",
	"other": {},
	"systemVersion": "V6.0.1",
	"verifyCode": "4a49c8d6398b8bb4485999b8f33a613c",
	"DId": "9BD02BF38238E5B5F85C022DF6DC244C"
}
{
	"code": {
		"code": "0000",
		"msgId": "RetCode.Success",
		"msg": "操作成功"
	},
	"bo": {
		"code": "0000",
		"msg": "成功",
		"enMsg": "success"
	},
	"other": null
}

URL	https://uac.zte.com.cn/uacmobile/auth/credents/adjust.serv
{
	"account": "10034491",
	"appTime": "2017-06-26 01:27:41.749",
	"appVersion": "1.1.4",
	"deviceNumber": "V02_357755071308934",
	"deviceType": "SM-A9100",
	"seedCode": "ba669b30622a205a7f4d8018e653dbc11be60ffd5a2f76764a147f9e91a2b343852f993d42f69da4dcc7c23fe7a54939",
	"systemVersion": "V6.0.1",
	"verifyCode": "cbfe70ea7a1ebcc6929092fcb94011df",
	"DId": "9BD02BF38238E5B5F85C022DF6DC244C"
}
{
	"code": {
		"code": "0000",
		"msgId": "RetCode.Success",
		"msg": "操作成功"
	},
	"bo": {
		"code": "0000",
		"msg": "成功",
		"enMsg": "success"
	},
	"other": {
		"seedID": "34254399fcd980345a02d0559b404bd7aa90e2e051f5a5e8cc3324c4ec3ca636d1172b6d845b997ef7f0c79977d002e6",
		"cycleTime": "720",
		"dynRefreshFreq": 60,
		"serverTime": "2017-06-26 01:27:42.487"
	}
}

URL	http://mdm.zte.com.cn:80/emm/uploadLocationMsg/service.jssm
{
	"AppId": "A00233",
	"CommandName": "UploadLocationInfo",
	"DId": "9BD02BF38238E5B5F85C022DF6DC244C",
	"LMsg": {
		"CNT": "中国",
		"City": "深圳市",
		"DMsg": "中国广东省深圳市南山区打石一路在中兴人才公寓附近",
		"LATD": "22.57856",
		"LOTD": "113.944666",
		"Pro": "广东省"
	},
	"MOAInfo": "S001",
	"Token": "76fvFjy/wns/NPobXaPzYF50DniTJSAcM3nnkw8XKlcPQbtjU7I+rJzPIS/rGH+ax0r8oaP7NsdKTKtkRGuHyg\u003d\u003d",
	"Trigger": "T000",
	"UID": "10034491"
}
{
	"RC": "0",
	"RM": ""
}
