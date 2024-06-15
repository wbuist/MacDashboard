unit GmailIntegration;

interface

uses
  System.SysUtils, System.Classes, IdHTTP, IdSSLOpenSSL, System.JSON, FMX.Memo, FMX.Forms;

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
    procedure LogMessage(const Msg: string);
    procedure LoadOpenSSLLibraries;
    function LoadLibrary(const LibName: string): HMODULE;
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

implementation

{ TGmailIntegration }

constructor TGmailIntegration.Create(AMemoLog: TMemo);
begin
  FMemoLog := AMemoLog;
  LogMessage('Initializing Gmail integration...');
  try
    LoadOpenSSLLibraries; // Dynamically load SSL libraries
    LogMessage('SSL libraries loaded.');

    FHTTP := TIdHTTP.Create(nil);
    FSSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    FHTTP.IOHandler := FSSLHandler;

    LogMessage('HTTP and SSL handler created successfully.');
  except
    on E: Exception do
    begin
      LogMessage('Error during initialization: ' + E.Message);
      raise;
    end;
  end;
end;

destructor TGmailIntegration.Destroy;
begin
  LogMessage('Destroying TGmailIntegration...');
  FSSLHandler.Free;
  FHTTP.Free;
  LogMessage('TGmailIntegration destroyed.');
  inherited;
end;

procedure TGmailIntegration.LogMessage(const Msg: string);
begin
  FMemoLog.Lines.Add(Msg);
  Application.ProcessMessages;
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
      LogMessage('Refreshing access token...');
      Response := FHTTP.Post(URL, Params);
      LogMessage('Response: ' + Response);

      JsonObject := TJSONObject.ParseJSONValue(Response) as TJSONObject;
      try
        if Assigned(JsonObject) then
        begin
          if JsonObject.GetValue('access_token') <> nil then
          begin
            FAccessToken := JsonObject.GetValue('access_token').Value;
            LogMessage('New AccessToken: ' + FAccessToken);
          end
          else if JsonObject.GetValue('error') <> nil then
          begin
            LogMessage('Error: ' + JsonObject.GetValue('error').Value);
            LogMessage('Error Description: ' + JsonObject.GetValue('error_description').Value);
          end;
        end;
      finally
        JsonObject.Free;
      end;
    except
      on E: Exception do
      begin
        LogMessage('Failed to refresh access token: ' + E.Message);
        raise;
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
    LogMessage('Fetching inbox count...');
    Response := FHTTP.Get(URL + '?q=in:inbox');
    LogMessage('Response: ' + Response);
    Result := StrToIntDef(ExtractJsonValue(Response, 'resultSizeEstimate'), 0);
  except
    on E: EIdHTTPProtocolException do
    begin
      LogMessage('HTTP protocol exception: ' + E.Message);
      if E.ErrorCode = 401 then
      begin
        LogMessage('401 Unauthorized - Refreshing access token...');
        RefreshAccessToken;
        FHTTP.Request.CustomHeaders.Values['Authorization'] := 'Bearer ' + FAccessToken;
        Response := FHTTP.Get(URL + '?q=in:inbox');
        LogMessage('Response after refresh: ' + Response);
        Result := StrToIntDef(ExtractJsonValue(Response, 'resultSizeEstimate'), 0);
      end
      else
        raise;
    end;
    on E: Exception do
    begin
      LogMessage('General error: ' + E.ClassName + ': ' + E.Message);
      raise;
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
    LogMessage('Fetching unread count...');
    Response := FHTTP.Get(URL + '?q=is:unread');
    LogMessage('Response: ' + Response);
    Result := StrToIntDef(ExtractJsonValue(Response, 'resultSizeEstimate'), 0);
  except
    on E: EIdHTTPProtocolException do
    begin
      LogMessage('HTTP protocol exception: ' + E.Message);
      if E.ErrorCode = 401 then
      begin
        LogMessage('401 Unauthorized - Refreshing access token...');
        RefreshAccessToken;
        FHTTP.Request.CustomHeaders.Values['Authorization'] := 'Bearer ' + FAccessToken;
        Response := FHTTP.Get(URL + '?q=is:unread');
        LogMessage('Response after refresh: ' + Response);
        Result := StrToIntDef(ExtractJsonValue(Response, 'resultSizeEstimate'), 0);
      end
      else
        raise;
    end;
    on E: Exception do
    begin
      LogMessage('General error: ' + E.ClassName + ': ' + E.Message);
      raise;
    end;
  end;
end;

function TGmailIntegration.LoadLibrary(const LibName: string): HMODULE;
begin
  Result := System.SysUtils.LoadLibrary(PChar(LibName));
  if Result = 0 then
    LogMessage('Failed to load library: ' + LibName + '. Error: ' + SysErrorMessage(GetLastError))
  else
    LogMessage('Library loaded successfully: ' + LibName);
end;

procedure TGmailIntegration.LoadOpenSSLLibraries;
var
  SSLLib, CryptoLib: HMODULE;
begin
  LogMessage('Setting OpenSSL library path to: /opt/homebrew/opt/openssl@1.1/lib');

  // Use versioned libraries
  SSLLib := LoadLibrary('/opt/homebrew/opt/openssl@1.1/lib/libssl.1.1.dylib');
  if SSLLib = 0 then
    raise Exception.Create('Could not load OpenSSL library: libssl.1.1.dylib');

  CryptoLib := LoadLibrary('/opt/homebrew/opt/openssl@1.1/lib/libcrypto.1.1.dylib');
  if CryptoLib = 0 then
    raise Exception.Create('Could not load OpenSSL library: libcrypto.1.1.dylib');
end;

end.

