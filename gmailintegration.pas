// File: GmailIntegration.pas

unit GmailIntegration;

interface

uses
  System.SysUtils, System.Classes, IdHTTP, IdSSLOpenSSL, IdSSLOpenSSLHeaders, System.JSON, FMX.Memo;

type
  TGmailIntegration = class
  private
    FAccessToken: string;
    FRefreshToken: string;
    FClientId: string;
    FClientSecret: string;
    FHTTP: TIdHTTP;
    FSSLHandler: TIdSSLIOHandlerSocketOpenSSL;
    FMemoLog: TMemo;
    procedure SetAccessToken(const Value: string);
    function ExtractJsonValue(const JsonString, Key: string): string;
    procedure RefreshAccessToken;
  public
    constructor Create(AMemoLog: TMemo);
    destructor Destroy; override;
    function GetInboxCount: Integer;
    function GetUnreadCount: Integer;
    property AccessToken: string read FAccessToken write SetAccessToken;
    property RefreshToken: string read FRefreshToken write FRefreshToken;
    property ClientId: string read FClientId write FClientId;
    property ClientSecret: string read FClientSecret write FClientSecret;
  end;

procedure LoadOpenSSLLibraries;

implementation

procedure LoadOpenSSLLibraries;
begin
  {$IFDEF MSWINDOWS}
  IdOpenSSLSetLibPath('Y:\support\MacDashboard'); // Set this to the path where your DLLs are located
  {$ENDIF}
  {$IFDEF MACOS}
  IdOpenSSLSetLibPath('/opt/homebrew/opt/openssl@1.1/lib');
  {$ENDIF}
  if not IdSSLOpenSSL.LoadOpenSSLLibrary then
    raise Exception.Create('Could not load OpenSSL library.');
end;

{ TGmailIntegration }

constructor TGmailIntegration.Create(AMemoLog: TMemo);
begin
  FMemoLog := AMemoLog;
  LoadOpenSSLLibraries;
  FHTTP := TIdHTTP.Create(nil);
  FSSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FHTTP.IOHandler := FSSLHandler;
end;

destructor TGmailIntegration.Destroy;
begin
  FSSLHandler.Free;
  FHTTP.Free;
  inherited;
end;

procedure TGmailIntegration.SetAccessToken(const Value: string);
begin
  FAccessToken := Value;
end;

procedure TGmailIntegration.RefreshAccessToken;
var
  URL, Response: string;
  Params: TStringList;
  JsonObject: TJSONObject;
begin
  URL := 'https://oauth2.googleapis.com/token';
  Params := TStringList.Create;
  try
    Params.Add('client_id=' + FClientId);
    Params.Add('client_secret=' + FClientSecret);
    Params.Add('refresh_token=' + FRefreshToken);
    Params.Add('grant_type=refresh_token');
    try
      FMemoLog.Lines.Add('Refreshing access token...');
      FMemoLog.Lines.Add('Request Parameters:');
      FMemoLog.Lines.Add('client_id=' + FClientId);
      FMemoLog.Lines.Add('client_secret=' + FClientSecret);
      FMemoLog.Lines.Add('refresh_token=' + FRefreshToken);
      FMemoLog.Lines.Add('grant_type=refresh_token');

      Response := FHTTP.Post(URL, Params);
      FMemoLog.Lines.Add('Response: ' + Response); // Log the raw response

      JsonObject := TJSONObject.ParseJSONValue(Response) as TJSONObject;
      try
        if Assigned(JsonObject) then
        begin
          if JsonObject.GetValue('access_token') <> nil then
          begin
            FAccessToken := JsonObject.GetValue('access_token').Value;
            FMemoLog.Lines.Add('New AccessToken: ' + FAccessToken);
          end
          else if JsonObject.GetValue('error') <> nil then
          begin
            FMemoLog.Lines.Add('Error: ' + JsonObject.GetValue('error').Value);
            FMemoLog.Lines.Add('Error Description: ' + JsonObject.GetValue('error_description').Value);
          end;
        end;
      finally
        JsonObject.Free;
      end;
    except
      on E: Exception do
      begin
        FMemoLog.Lines.Add('Failed to refresh access token: ' + E.Message);
        raise Exception.Create('Failed to refresh access token: ' + E.Message);
      end;
    end;
  finally
    Params.Free;
  end;
end;


function TGmailIntegration.ExtractJsonValue(const JsonString, Key: string): string;
var
  JsonObject: TJSONObject;
  JsonValue: TJSONValue;
begin
  Result := '';
  JsonObject := TJSONObject.ParseJSONValue(JsonString) as TJSONObject;
  if Assigned(JsonObject) then
  try
    JsonValue := JsonObject.GetValue(Key);
    if Assigned(JsonValue) then
      Result := JsonValue.Value;
  finally
    JsonObject.Free;
  end;
end;

function TGmailIntegration.GetInboxCount: Integer;
var
  URL, Response: string;
begin
  URL := 'https://www.googleapis.com/gmail/v1/users/me/messages';
  FHTTP.Request.CustomHeaders.Values['Authorization'] := 'Bearer ' + FAccessToken;
  try
    Response := FHTTP.Get(URL + '?q=in:inbox');
    Result := StrToIntDef(ExtractJsonValue(Response, 'resultSizeEstimate'), 0);
  except
    on E: EIdHTTPProtocolException do
    begin
      if E.ErrorCode = 401 then
      begin
        FMemoLog.Lines.Add('401 Unauthorized - Refreshing access token...');
        RefreshAccessToken;
        FHTTP.Request.CustomHeaders.Values['Authorization'] := 'Bearer ' + FAccessToken;
        Response := FHTTP.Get(URL + '?q=in:inbox');
        Result := StrToIntDef(ExtractJsonValue(Response, 'resultSizeEstimate'), 0);
      end
      else
      begin
        FMemoLog.Lines.Add('HTTP error: ' + E.Message);
        raise Exception.Create('HTTP error: ' + E.Message);
      end;
    end;
  end;
end;

function TGmailIntegration.GetUnreadCount: Integer;
var
  URL, Response: string;
begin
  URL := 'https://www.googleapis.com/gmail/v1/users/me/messages';
  FHTTP.Request.CustomHeaders.Values['Authorization'] := 'Bearer ' + FAccessToken;
  try
    Response := FHTTP.Get(URL + '?q=is:unread');
    Result := StrToIntDef(ExtractJsonValue(Response, 'resultSizeEstimate'), 0);
  except
    on E: EIdHTTPProtocolException do
    begin
      if E.ErrorCode = 401 then
      begin
        FMemoLog.Lines.Add('401 Unauthorized - Refreshing access token...');
        RefreshAccessToken;
        FHTTP.Request.CustomHeaders.Values['Authorization'] := 'Bearer ' + FAccessToken;
        Response := FHTTP.Get(URL + '?q=is:unread');
        Result := StrToIntDef(ExtractJsonValue(Response, 'resultSizeEstimate'), 0);
      end
      else
      begin
        FMemoLog.Lines.Add('HTTP error: ' + E.Message);
        raise Exception.Create('HTTP error: ' + E.Message);
      end;
    end;
  end;
end;

end.

