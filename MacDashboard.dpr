program MacDashboard;

uses
  System.StartUpCopy,
  FMX.Forms,
  MacDashboardForm in 'MacDashboardForm.pas' {Form1},
  gmailintegration in 'gmailintegration.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
