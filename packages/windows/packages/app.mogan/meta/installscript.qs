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
    } catch (e) {
        print(e);
    }
}
