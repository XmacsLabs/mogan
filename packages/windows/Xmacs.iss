; -- Xmacs.iss --

[Setup]
AppName=Mogan
AppVerName=Mogan
DefaultDirName={commonpf64}\Mogan
DefaultGroupName=Mogan

VersionInfoTextVersion=v1.0-alpha4
AppPublisher=XmacsLabs
AppPublisherURL=http://github.com/XmacsLabs/mogan
AppVersion=1.0-alpha4
LicenseFile=LICENSE

UninstallDisplayIcon={app}\TeXmacs.ico
OutputDir=..
OutputBaseFilename=Xmacs-v1.0-alpha4-installer
; SourceDir=../..
ChangesAssociations=yes

WizardImageFile=TeXmacs-large.bmp
WizardImageStretch=no
WizardSmallImageFile=TeXmacs-small.bmp

;PrivilegesRequired=none

CloseApplications=yes

[Registry]
Root: HKCR; Subkey: ".tm"; ValueType: string; ValueName: ""; ValueData: "tmfile"; Flags: uninsdeletevalue
Root: HKCR; Subkey: "tmfile"; ValueType: string; ValueName: ""; ValueData: "text/tm"; Flags: uninsdeletekey
Root: HKCR; Subkey: "tmfile\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\TeXmacs.ico"
Root: HKCR; Subkey: "tmfile\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\bin\Mogan.exe"" ""%1"""

[Files]
Source: *; Excludes: "Xmacs.iss"; DestDir: {app}; Flags: recursesubdirs createallsubdirs ignoreversion
Source: "C:\Program Files (x86)\TeXmacs\bin\gs.exe"; DestDir: {app}/bin/;

[Icons]
Name: "{group}\Mogan"; Filename: "{app}\bin\Mogan.exe"; IconFilename: "{app}\xmacs-64.ico"
Name: "{group}\Uninstall Mogan"; Filename: "{uninstallexe}"
Name: "{commondesktop}\Mogan"; Filename: "{app}\bin\Mogan.exe"; IconFilename: "{app}\xmacs-64.ico"

[UninstallDelete]
Type: files; Name: "{app}\*"
Type: dirifempty; Name: "{app}"

[Code]
procedure UninstallTeXmacs();
var
  FindRec: TFindRec;
  Uninstaller: String;
  FoundInstalled: Boolean;
  ConfirmedUninstall: Boolean;
  UninstallResultCode: integer;
begin
  Uninstaller := '';
  ConfirmedUninstall := false;
  FoundInstalled := FindFirst(ExpandConstant('{app}/unins*.exe'), FindRec);
  if FoundInstalled then
    if MsgBox('Before installing TeXmacs into the destination folder ('
              + ExpandConstant('{app}') + '), it is recommened to uninstall software already present in this folder.'
	      + #13#10 + 'Do you want to perform these uninstallations before installating?', mbConfirmation, MB_YESNO or MB_DEFBUTTON2) = idYes then
    ConfirmedUninstall := true;
  if ConfirmedUninstall then
  repeat
    Uninstaller := FindRec.Name;
    Exec(ExpandConstant('{app}/') + Uninstaller, '', '', SW_SHOW,
         ewWaitUntilTerminated, UninstallResultCode);
  until
    not FindNext(FindRec);
  FindClose(FindRec);
end;
procedure CurStepChanged(CurStep: TSetupStep);
begin
  if (CurStep = ssInstall) then
    UninstallTeXmacs();
end;
