{
 Fluxpas : Simple GUI for XFLUX (f.lux) for Linux OS
 written by vhanla

 CHANGELOG:
 2014-11-17: First basic version

}
unit fluxpas_src;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, LCLIntf, Menus, Process, IniFiles, simpleipc, BaseUnix;
type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnRunXflux: TButton;
    btnExit: TButton;
    btnHide: TButton;
    chkhide: TCheckBox;
    edLocation: TEdit;
    ImageList1: TImageList;
    lblLocation: TLabel;
    lblGetURLLocation: TLabel;
    lblLatitude: TLabel;
    lblLongitude: TLabel;
    lblXfluxStatus: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    PopupMenu1: TPopupMenu;
    SystrayIcon: TTrayIcon;
    Timer1: TTimer;
    procedure btnRunXfluxClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure btnHideClick(Sender: TObject);
    procedure chkhideChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lblGetURLLocationClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure SystrayIconDblClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    procedure KillXflux;
    procedure ReadPreferences;
    procedure SavePreferences;

    procedure InitIPCServer(const IPCID: String);
  public
    { public declarations }
  end;

const
  UNIQUEID = 'fluxpas_gui';
var
  frmMain: TfrmMain;
  fluxPID: String;
  portable: Boolean = True;
  hideonstart: Boolean = False;
  prefFilePath: String;
  xfluxFile: String;

  pIPCserver : TSimpleIPCServer;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.KillXflux;
var
  P: TProcess;
begin
  P := TProcess.Create(nil);
  P.CommandLine:='killall '+xfluxFile;
  P.Options:=P.Options - [poWaitOnExit];
  P.Execute;
  Sleep(250);
end;

procedure TfrmMain.ReadPreferences;
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(prefFilePath);
  try
    edLocation.Text := ini.ReadString('xflux','location','37.3394, -121.8950');
    chkhide.Checked := ini.ReadBool('gui', 'hidden', false);
    hideonstart:=chkhide.Checked;
  finally
    ini.Free;

  end;
end;

procedure TfrmMain.SavePreferences;
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(prefFilePath);
  try
    ini.WriteString('xflux','location',edLocation.Text);
    ini.WriteBool('gui', 'hidden', chkhide.Checked);
  finally
    ini.Free;
  end;
end;

procedure TfrmMain.InitIPCServer(const IPCID: String);
begin
  if not Assigned(pIPCserver) then
  begin
    pIPCserver := TSimpleIPCServer.Create(nil);
    pIPCserver.ServerID:=IPCID;
    pIPCserver.Global:=True;
    pIPCserver.StartServer;
  end;
end;

procedure TfrmMain.btnRunXfluxClick(Sender: TObject);
begin
  MenuItem1Click(Sender);
end;

procedure TfrmMain.btnExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmMain.btnHideClick(Sender: TObject);
begin
  Hide;
end;

procedure TfrmMain.chkhideChange(Sender: TObject);
begin
  SavePreferences;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caNone;
  Hide;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  IPCClient: TSimpleIPCClient;
begin
  // prevent more than one instance of this application
  IPCClient := TSimpleIPCClient.Create(nil);
  IPCClient.ServerID:=UNIQUEID;
  if not IPCClient.ServerRunning then
   InitIPCServer(UNIQUEID)
  else
  begin
   IPCClient.Free;
   FpKill(FpGetpid, 9);
  end;

  {$IFDEF CPU32}
    xfluxFile := 'xflux_32';
  {$ENDIF}
  {$IFDEF CPU64}
    xfluxFile := 'xflux_64';
  {$ENDIF}

  if not FileExists(ExtractFilePath(ParamStr(0)) + xfluxFile) then
  begin
    ShowMessage('Error, '+xfluxFile+' not found!');
    FpKill(FpGetpid, 9);
  end;

  if ExtractFilePath(ParamStr(0)) <> '/usr/bin/' then
  begin
    portable:=True;
    prefFilePath:=ExtractFilePath(ParamStr(0))+'fluxpaspref.ini';
  end
  else
  begin
    portable:=False;
    prefFilePath:='.fluxpaspref.ini';
  end;

  SystrayIcon.Hint:='F.lux GUI';
  SystrayIcon.Show;

  ReadPreferences;

end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  KillXflux;
  pIPCserver.Free;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  if hideonstart then
  begin
    hideonstart:=False;
   btnRunXfluxClick(Sender);
   Hide;
  end;
end;

procedure TfrmMain.lblGetURLLocationClick(Sender: TObject);
begin
  OpenURL('https://justgetflux.com/map.html');
end;

procedure TfrmMain.MenuItem1Click(Sender: TObject);
var
  l,g: Double;
  lst: TStringList;
  pars: String;
  P: TProcess;
begin
  // Evaluate location format : ./xflux -l -15.8402 -g -70.0219 e.g. -15.8402, -70.0219
  lst := TStringList.Create;
  try
    lst.Delimiter:=',';
    lst.DelimitedText:=edLocation.Text;
    if lst.Count = 2 then
    begin
      try
        l := StrToFloat(lst[0]);
        lblLatitude.Caption:='Latitude: '+FloatToStr(l);
        g := StrToFloat(lst[1]);
        lblLongitude.Caption:='Longitude: '+FloatToStr(g);
        if FileExists(ExtractFilePath(ParamStr(0))+xfluxFile) then
        begin
          P := TProcess.Create(nil);
          KillXflux;
          pars := '-l '+lst[0]+' -g '+lst[1];
          P.CommandLine:=xfluxFile+' '+pars;
          P.Options:=P.Options - [poWaitOnExit];
          P.Execute;
          P.Free;
          Sleep(250);
          MenuItem1.Checked:=True;
          SavePreferences;
        end
        else
        ShowMessage('Error, xflux not found in application''s directory!');
      except
        ShowMessage('Error, location format not valid. e.g. -70.000, -15.000');
      end;

    end
    else
    ShowMessage('Error, you should paste a valid location data.');
  finally
    lst.Free;
  end;

end;

procedure TfrmMain.MenuItem2Click(Sender: TObject);
begin
  Show;
end;

procedure TfrmMain.MenuItem4Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmMain.SystrayIconDblClick(Sender: TObject);
begin
  if frmMain.IsVisible then
    Hide
  else
    Show;
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
var
  status: String;
  ls: TStringList;
begin
  RunCommandInDir(ExtractFilePath(ParamStr(0)),'pidof '+xfluxFile,status);
  //RunCommandInDir(ExtractFilePath(ParamStr(0)),'ps aux | grep "'+xfluxFile+' -l"',status);
  if strlen(PChar(Trim(status)))>0 then
  begin
    ls := TStringList.Create;
    try
      ls.Delimiter:=' ';
      ls.DelimitedText:=status;
      lblXfluxStatus.Caption:='xflux status : Enabled - PID '+ls[0];
    finally
      ls.Free;
    end;
  end
  else
  begin
    lblXfluxStatus.Caption:='xflux status : Disabled';
  end;
end;

end.

