function Component()
{
}

Component.prototype.createOperations = function()
{
    try {
        // call the base create operations function
        component.createOperations();
        if (systemInfo.productType === "windows") {
            try {
                var userProfile = installer.environmentVariable("USERPROFILE");
                installer.setValue("UserProfile", userProfile);
                component.addOperation("CreateShortcut", "@TargetDir@\\bin\\MoganResearch.exe", "@UserProfile@\\Desktop\\Mogan Research.lnk");
                component.addOperation("CreateShortcut", "@TargetDir@\\bin\\MoganResearch.exe", "@StartMenuDir@\\Mogan Research.lnk");
            } catch (e) {
                // Do nothing if key doesn't exist
            }
        }

        // return value 3010 means it need a reboot, but in most cases it is not needed for running Qt application
        // return value 5100 means there's a newer version of the runtime already installed
        component.addElevatedOperation("Execute", "{0,3010,1638,5100}", "@TargetDir@\\bin\\vc_redist.x64.exe", "/norestart", "/q");
    } catch (e) {
        print(e);
    }
}
