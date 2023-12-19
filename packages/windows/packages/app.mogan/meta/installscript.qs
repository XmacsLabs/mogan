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
                component.addOperation("CreateShortcut", "@TargetDir@\\bin\\MoganResearch.exe", "@UserProfile@\\Desktop\\MoganResearch.lnk");
            } catch (e) {
                // Do nothing if key doesn't exist
            }
        }
    } catch (e) {
        print(e);
    }
}
