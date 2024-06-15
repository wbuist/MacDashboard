unit MacDashboardForm;

interface

uses
  System.SysUtils, System.Classes, FMX.Forms, FMX.Controls, FMX.StdCtrls,
  FMX.Layouts, FMX.Types, FMX.Memo, GmailIntegration, FMX.Memo.Types,
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
    procedure Log(const Msg: string);
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
  Log('Initializing dashboard...');
  try
    FGmailIntegration := TGmailIntegration.Create(MemoLog);
    Log('TGmailIntegration created successfully.');
  except
    on E: Exception do
      Log('Error creating TGmailIntegration: ' + E.Message);
  end;
end;

destructor TForm1.Destroy;
begin
  Log('Destroying TGmailIntegration...');
  FGmailIntegration.Free;
  Log('TGmailIntegration destroyed.');
  inherited;
end;

procedure TForm1.ButtonRefreshGmailClick(Sender: TObject);
var
  InboxCount, UnreadCount: Integer;
begin
  Log('Refreshing Gmail data...');
  try
    InboxCount := FGmailIntegration.GetInboxCount;
    UnreadCount := FGmailIntegration.GetUnreadCount;
    LabelGmail.Text := Format('Inbox: %d, Unread: %d', [InboxCount, UnreadCount]);
    Log(Format('Gmail data updated: Inbox: %d, Unread: %d', [InboxCount, UnreadCount]));
  except
    on E: Exception do
    begin
      Log('Error updating Gmail data: ' + E.Message);
    end;
  end;
end;

procedure TForm1.Log(const Msg: string);
begin
  MemoLog.Lines.Add(Msg);
  {$IFDEF MACOS}
  Writeln(Msg); // Output to the console for debugging on macOS
  {$ENDIF}
end;

end.

