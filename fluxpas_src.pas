{
 Fluxpas : Simple GUI for XFLUX (f.lux) for Linux OS
 written by vhanla
}
unit fluxpas_src;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, LCLIntf, Menus, Process;
type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnRunXflux: TButton;
    btnExit: TButton;
    btnHide: TButton;
    edLocation: TEdit;
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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lblGetURLLocationClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure SystrayIconDblClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    procedure KillXflux;
  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;
  fluxPID: String;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.KillXflux;
var
  P: TProcess;
begin
  P := TProcess.Create(nil);
  P.CommandLine:='killall xflux';
  P.Options:=P.Options - [poWaitOnExit];
  P.Execute;
  Sleep(250);
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

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caNone;
  Hide;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  SystrayIcon.Icon.LoadFromFile(ExtractFilePath(ParamStr(0))+'icon.ico');
  SystrayIcon.Hint:='F.lux GUI';
  SystrayIcon.Show;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  KillXflux;
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
        if FileExists(ExtractFilePath(ParamStr(0))+'xflux') then
        begin
          P := TProcess.Create(nil);
          KillXflux;
          pars := '-l '+lst[0]+' -g '+lst[1];
          P.CommandLine:='xflux '+pars;
          P.Options:=P.Options - [poWaitOnExit];
          P.Execute;
          P.Free;
          Sleep(250);
          MenuItem1.Checked:=True;
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
  RunCommandInDir(ExtractFilePath(ParamStr(0)),'pidof xflux',status);
  //RunCommandInDir(ExtractFilePath(ParamStr(0)),'ps aux | grep "xflux -l"',status);
  if strlen(PChar(Trim(status)))>0 then
  begin
    ls := TStringList.Create;
    try
      ls.Delimiter:=' ';
      ls.DelimitedText:=status;
      lblXfluxStatus.Caption:='XFLUX : Enabled - PID '+ls[0];
    finally
      ls.Free;
    end;
  end
  else
  begin
    lblXfluxStatus.Caption:='XFLUX : Disabled';
  end;
end;

end.

