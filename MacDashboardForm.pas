// File: MacDashboardForm.pas

unit MacDashboardForm;

interface

uses
  System.SysUtils, System.Classes, FMX.Forms, FMX.Controls, FMX.StdCtrls,
  FMX.Layouts, FMX.Types, FMX.Memo, GmailIntegration, IniFiles, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    PanelDashboard: TPanel;
    LabelGmail: TLabel;
    ButtonRefreshGmail: TButton;
    LabelXero: TLabel;
    ButtonRefreshXero: TButton;
    LabelNotion: TLabel;
    ButtonRefreshNotion: TButton;
    MemoLog: TMemo;
    procedure ButtonRefreshGmailClick(Sender: TObject);
  private
    FGmailIntegration: TGmailIntegration;
    procedure LoadCredentials;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FGmailIntegration := TGmailIntegration.Create(MemoLog); // Pass MemoLog here
  LoadCredentials;
end;

destructor TForm1.Destroy;
begin
  FGmailIntegration.Free;
  inherited;
end;

procedure TForm1.LoadCredentials;
var
  Ini: TIniFile;
  ConfigPath: string;
begin
  MemoLog.Lines.Add('Loading credentials...');

  {$IFDEF MSWINDOWS}
  ConfigPath := 'Y:\support\macdashboard\config.ini';
  {$ENDIF}
  {$IFDEF MACOS}
  ConfigPath := '/Users/williambuist/Library/CloudStorage/GoogleDrive-william@williambuist.com/My Drive/Business/GitHub/support/config.ini'; // Update this path as needed for Mac
  {$ENDIF}

  Ini := TIniFile.Create(ConfigPath);
  try
    FGmailIntegration.ClientId := Ini.ReadString('Credentials', 'ClientId', '');
    FGmailIntegration.ClientSecret := Ini.ReadString('Credentials', 'ClientSecret', '');
    FGmailIntegration.AccessToken := Ini.ReadString('Credentials', 'AccessToken', '');
    FGmailIntegration.RefreshToken := Ini.ReadString('Credentials', 'RefreshToken', '');
    MemoLog.Lines.Add('ClientId: ' + FGmailIntegration.ClientId);
    MemoLog.Lines.Add('ClientSecret: ' + FGmailIntegration.ClientSecret);
    MemoLog.Lines.Add('AccessToken: ' + FGmailIntegration.AccessToken);
    MemoLog.Lines.Add('RefreshToken: ' + FGmailIntegration.RefreshToken);
    MemoLog.Lines.Add('Credentials loaded.');
  finally
    Ini.Free;
  end;
end;


procedure TForm1.ButtonRefreshGmailClick(Sender: TObject);
var
  InboxCount, UnreadCount: Integer;
begin
  MemoLog.Lines.Add('Fetching Gmail data...');
  try
    InboxCount := FGmailIntegration.GetInboxCount;
    UnreadCount := FGmailIntegration.GetUnreadCount;
    LabelGmail.Text := Format('Inbox: %d, Unread: %d', [InboxCount, UnreadCount]);
    MemoLog.Lines.Add(Format('Inbox: %d, Unread: %d', [InboxCount, UnreadCount]));
  except
    on E: Exception do
    begin
      MemoLog.Lines.Add('Error: ' + E.Message);
    end;
  end;
end;

end.

